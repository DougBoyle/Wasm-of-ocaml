/*
  Same as for reference counting, still keep a 64-bit header for now to track MARKED.
  Very wasteful, could pack into the 'arity' field of objects as long as that bit always cleared
    before making use of it, and mark bit always cleared from all objects after gc pass complete.
  Another improvement, can track the marked/fringe/free list all in the same 64-bit header, then
    possibly just 1 extra block so that we have all of [list ptr, type of list flag, size of block].
  During GC, should set a flag if a block is ever freed/coalesced of the size required.
*/

/*
Stack layout:
[...][number of cells = locals used by function]                                | End of stack
                                               ^ sp global exported by runtime
For one function, locals are stored as [ln, ..., l0]
Hence subtract (index + 1) from sp to get slot to update (must skip over swap slots, not kept on stack)

TODO: For now keep sp just in runtime and export an 'update variable' function taking an index and i32 value.
  May later want to export sp so functions can do this themselves. Then exporting a GLOBAL not a function, so
  will need to change lots of how compilewasm works with assumptions about offsets etc.

  Functions used by compilewasm: create_fun(num slots), update_local(slot, value), exit_fun(num slots)

  TODO: Update_local should really be done by function, not runtime, since offset can be statically calculated

  TODO: Memory and allocator are now much more interlinked, should probably just make into 1 file.
    Since both enforce 8-byte alignment, can ALWAYS MOVE SWEEP TO STACK_LIMIT AS START OF LOWEST BLOCK.
*/

// So that I don't need to look at the free list directly, but can still traverse the heap and avoid freeing
// already free blocks. Use the header for bit 1 = marked and bit 10 = allocated.
// (again, should merge this with other layer)

const headerSize = 8;// just for 64 bit alignment, actually just 1 bit used for now [31 unused, 1 bit, 32 unused]

const STACK_LIMIT = 16384;

class ManagedMemory {
  constructor(memory) {
    this.memory = memory;
    this.uview = new Uint32Array(memory.buffer); // In i32 view, stack top is 2^14 = 16384
    // TODO: Initialise this when runtime instantiated, for calling functions moved into Wasm
    this.runtime = null;
    this.markedSet = []; // fringe left to explore

    // points to a free block at all times, unless the free list is empty
    this.freep = STACK_LIMIT;

    this.memory_used = 0;
    // size of block we are looking for when we decide to run GC
    // TODO: Come up with better conditions for when to run GC
    //   e.g. don't run again if stack pointer has barely moved or very few new objects allocated?
    this.requiredSize = 0;

    this._refreshViews = this._refreshViews.bind(this);
    this.malloc = this.malloc.bind(this);

    // initialise free list to a single block starting after the shadow stack
    this.uview[STACK_LIMIT] = STACK_LIMIT; // only element of cyclic list points to itself
    this.uview[STACK_LIMIT + 1] =  (this.uview.byteLength >> 2) - STACK_LIMIT;
  }

  _refreshViews() {
    this.uview = new Uint32Array(this.memory.buffer);
  }

  setRuntime(runtime) {
    this.runtime = runtime;
  }

  log(v){
    console.log("log sp:", v);
    return v;
  }

  getNext(ptr){
    return this.uview[ptr];
  }
  getSize(ptr){
    return this.uview[ptr + 1];
  }
  getMark(ptr){
    return this.uview[ptr + 2];
  }
  setNext(ptr, val){
    this.uview[ptr] = val;
  }
  setSize(ptr, val){
    this.uview[ptr + 1] = val;
  }
  setMark(ptr, val){
    this.uview[ptr + 2] = val;
  }

  growHeap(units) {
    // convert to number of pages to allocate, rounded up
    let pages = (units*4 + 65535)>>16;
    const ptr = this.memory.buffer.byteLength >> 2;
    // documented error value in
    // https://webassembly.github.io/spec/core/syntax/instructions.html#syntax-instr-memory
    if (this.memory.grow(pages) === -1){
      throw "Could not allocate memory"
    }
    this._refreshViews();
    this.setSize(ptr, pages << 14);
    // TODO: Disable when not debugging
    this.memory_used += pages << 16;
    this.free(ptr );
  }

  malloc(bytes){
    //      console.log("ALLC malloc");
    // round up to align block
    // *2 to put in terms of 32-bit words rather than 64-bit headers
    // TODO: New change, enforce one large 128 bit (16 byte header) so that alignment always works.
    //   Merges the 2 old headers i.e. [next ptr, size, allocation flag, unused] = 4 words
    const units = (((bytes + 16 - 1) >> 4) + 1)*4;
    this.memory_used += units * 4;
    // last block allocated exactly used up the last cell of the free list.
    // need to allocate more memory. Can't rely on 'free' since it expects freep to be defined
    if (this.freep == null){
      this.requiredSize = units;
      this.doGC();
      if (this.requiredSize > 0) { // still not enough memory
        this.growHeap(units);
      }
    }

    // p is the block we are considering allocating in,
    // prev is its predecessor in the free list
    let prev = this.freep;
    let p;
    for (p = this.getNext(prev); ; prev = p, p = this.getNext(p)){
      let size = this.getSize(p);
      if (size >= units) {
        if (size === units) {
          // exact fit, remove from list
          if (this.getNext(p) === p){
            prev = null; // was only element of list, will now set freep = null on line 127
          } else {
            this.setNext(prev, this.getNext(p));
          }
        } else {
          // allocate tail end of free block
          this.setSize(p, this.getSize(p) - units);
          // new block
          p += this.getSize(p);
          this.setSize(p, units);
        }
        this.freep = prev;
        // return a pointer in terms of bytes to the data, not the header
        //        console.log("Allocated:", i32size * p, "size: ", this.getSize(p)*4);
      //  return 4 * (p+2);
        break;
      }
      if (p === this.freep){
        // have searched the whole list
        // do GC and, if that doesn't free up a suitable block, grow memory
        this.requiredSize = units;
        this.doGC();
        if (this.requiredSize > 0) { // still not enough memory
          this.growHeap(units);
        }
        // freep may have changed, so update it and search list again
        p = this.freep;
      }
    }

    this.setMark(p, 2);
    return (p + 4)<<2;
  }

  // takes a byte pointer
  free(blockPtr){
    //  console.log("ALLOC free");
    let sizeFreed = this.getSize(blockPtr);

    // TODO: Remove when not debugging
    this.memory_used -= this.getSize(blockPtr)*4;

    // special case when freep is null. Indicates that the block being freed is only free block in memory
    if (this.freep == null){
      // single element circular list
      this.setNext(blockPtr, blockPtr);
      this.freep = blockPtr;
      if (sizeFreed >= this.requiredSize){
        this.requiredSize = 0;
      }
      return;
    }
    let p;
    // find the free block closest behind the block being freed
    for (p = this.freep; blockPtr < p || blockPtr > this.getNext(p); p = this.getNext(p)){
      if (p >= this.getNext(p) && (blockPtr > p || blockPtr < this.getNext(p))){
        // block to free is at one end of list
        break;
      }
    }
    // join upper block
    // works even if memory has been extended since block was allocated, now pointed to by p
    if (blockPtr + this.getSize(blockPtr) === this.getNext(p)){
      sizeFreed += this.getSize(this.getNext(p));
      this.setSize(blockPtr, this.getSize(blockPtr) + this.getSize(this.getNext(p)));
      this.setNext(blockPtr, this.getNext(this.getNext(p)));
    } else {
      this.setNext(blockPtr, this.getNext(p));
    }
    // join lower block
    if (p + this.getSize(p) === blockPtr){
      sizeFreed += this.getSize(p);
      this.setSize(p, this.getSize(p) + this.getSize(blockPtr));
      this.setNext(p, this.getNext(blockPtr));
    } else {
      this.setNext(p, blockPtr);
    }
    // in case freep was the upper block, so freep would otherwise point to the middle of a block
    this.freep = p;
    if (sizeFreed >= this.requiredSize){
      this.requiredSize = 0;
    }
  }


  markReference(ptr){
    if (ptr & 1 && ptr > 0){ // > 0 avoids treating a negative integer as a pointer
      let raw_ptr = (ptr>>2) - 2; // headersize/4 = 2
      if (!(this.uview[raw_ptr] & 1)){ // not yet marked
     //   console.log("marking:", raw_ptr);
        this.uview[raw_ptr] = 3; // both allocated and marked
        this.markedSet.push(raw_ptr);
    //    console.log("followed ptr", ptr, "pushed:", raw_ptr);
      //  this.marked++;
      }
    }
  }

  // go through stack and set marked flag on all memory objects accessible from the stack.
  // can use sp to avoid searching further than necessary up stack
  // TODO: Root elements aren't padded with headers etc. so need to ensure they never get treated as such
  mark(){
    let stack_top = this.runtime.exports.sp.value >> 2 // shift to get in terms of i32s
    for (let i = 0; i < stack_top; i++){
      this.markReference(this.uview[i]);
    }
    // After identifying root set, do DFS until all reachable objects marked
    while (this.markedSet.length > 0){
      let rawPtr = this.markedSet.pop();
      // mark all of its referenced objects
      let arity = this.uview[rawPtr + 3];
      for (let i = 0; i < arity; i++){
        this.markReference(this.uview[rawPtr + i + 4]);
      }
    }
  }

  // traverse all allocated objects of the heap, adding unmarked ones to the free list.
  // Uses fact that every block allocated has 0b10 in its header, and blocks are always
  // allocated in aligned chunks so can always read the header from the correct position
  sweep(){
    // TODO: Would be better to chain together allocated objects in a list, then free that list
    //   if mark process doesn't move them to a new list
    const heapLimit = this.memory.buffer.byteLength >> 2;
    let blockPtr = STACK_LIMIT; // starting point of scanning heap
    // TODO: Merge layers, using knowledge of lower levels header structure here, so no abstraction achieved
    while (blockPtr < heapLimit){
      let size = this.getSize(blockPtr);
      if (this.uview[blockPtr + 2] === 2){
        // allocated but not marked
        // Can also improve link between sweep, free and malloc, so we don't repeat searching after large enough block freed
        this.setMark(blockPtr, 0); // mark as unallocated
      //  this.marked++;
        this.free(blockPtr);
      } else {
        this.setMark(blockPtr, this.getMark(blockPtr) & 2); // unset marked bit, no effect if free
      }
      blockPtr += size;
    }
  }

  doGC(){
  //  this.marked = 0;
    this.mark(); // well isn't this simple, could probably just write them as a single function instead
   // console.log("live set:", this.marked);
    this.sweep();
  }

  stackLimitExceeded(){
    throw "Maximum stack size exceeded";
  }
}

exports.ManagedMemory = ManagedMemory;
