/*
Stack layout:
[...][number of cells = locals used by function]                                | End of stack
                                               ^ sp global exported by runtime
For one function, locals are stored as [ln, ..., l0]
Hence subtract 4*(index + 1) from sp to get slot to update (skip over swap slots, not kept on stack)

Memory block layout:
[32-bit next ptr, 32-bit size field, 30-bits padding, 2-bit allocated/marked flag, 32-bits padding][data]

Can compress to just 2 words in the future:
[32-bit next ptr, 32-bit size field][data]
Both fields are in terms of 32-bit words, and the size of the data allocated is also padded
to be a multiple of 2 words (8 bytes).
Therefore, last bit of both next and size is 0, can use for allocated/marked flags respectively.
Also, since everything is in terms of 32-bit words rather than bytes, top 2 bits of each field are free too.

Last bit of next ptr = 1 indicates block is allocated
Last bit of size ptr = 1 indicates block is marked
*/


const STACK_LIMIT = 16384 ;

class ManagedMemory {
  constructor(memory) {
    this.memory = memory;
    this.uview = new Uint32Array(memory.buffer); // In i32 view, stack top is 2^14 = 16384
    // TODO: Initialise this when runtime instantiated, for calling functions moved into Wasm
    this.runtime = null;

    // fringe left to explore
    // 0 indicates empty list, since bottom of memory is reserved for stack
    this.markedSet = 0;

    // points to a free block at all times, unless the free list is empty
    this.freep = STACK_LIMIT;

    this.memory_used = 0;
    this.maxMemory = 0;
    // size of block we are looking for when we decide to run GC
    // TODO: Come up with better conditions for when to run GC
    //   e.g. don't run again if stack pointer has barely moved or very few new objects allocated?
    this.requiredSize = 0;

    this._refreshViews = this._refreshViews.bind(this);
    this.malloc = this.malloc.bind(this);

    // initialise free list to a single block starting after the shadow stack
    this.uview[STACK_LIMIT] = STACK_LIMIT; // only element of cyclic list points to itself
    this.uview[STACK_LIMIT + 1] =  (this.uview.byteLength >> 2) - STACK_LIMIT;

    // TODO: Remove all these
    this.numScans = 0;
    this.exact = 0;
    this.fromLarge = 0
    // Shouldn't vary largely between old/new approach
    this.freesDone = 0;
    this.mallocsDone = 0;
    this.gcsDone = 0;

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
    return this.uview[ptr] & (~1);
  }
  isAllocated(ptr){
    return this.uview[ptr] & 1;
  }
  getSize(ptr){
    return this.uview[ptr + 1] & (~1);
  }
  isMarked(ptr){
    return this.uview[ptr + 1] & 1;
  }

  setNext(ptr, val, allocated){
    this.uview[ptr] = val | allocated; // sets allocated bit
  }

  // setSize assumes the block is not marked, we never modify the size once allocated
  setSize(ptr, val){
    this.uview[ptr + 1] = val;
  }
  setMarked(ptr){
    this.uview[ptr + 1] |= 1;
  }
  clearMarked(ptr){
    this.uview[ptr + 1] &= (~1);
  }
  setAllocated(ptr){
    this.uview[ptr] |= 1;
  }

  growHeap(units) {
    console.log("Grow", this.mallocsDone, this.freesDone);
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
    this.free(ptr);
  }

  malloc(bytes){
    this.mallocsDone++;
    //      console.log("ALLC malloc");
    // round up to align block
    // *2 to put in terms of 32-bit words rather than 64-bit headers
    // TODO: Change back to 1
    //   alltrees 5, 12 headers, 1100 mallocs
    const units = (((bytes + 8 - 1) >> 3) + 1)*2;
    this.memory_used += units * 4;
    this.maxMemory = Math.max(this.maxMemory, this.memory_used);
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
      this.numScans++;
      let size = this.getSize(p);
     // console.log("trying prev =", prev, "p =", p, "p size =", size, "p next =", this.getNext(p));
      if (size >= units) {
        if (size === units) {
          this.exact++;
          // exact fit, remove from list
          if (this.getNext(p) === p){
            prev = null; // was only element of list, will now set freep = null on line 127
          } else {
            this.setNext(prev, this.getNext(p), false);
          }
        } else {
          this.fromLarge++;
          // allocate tail end of free block
          this.setSize(p, this.getSize(p) - units);
          // new block
          p += this.getSize(p);
          this.setSize(p, units);
        }
        this.freep = prev;
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
  //      console.log("Updated freep to:", p, "next is:", this.getNext(p));
      }
    }

  //  console.log("malloc:", units, p, "freep is:", this.freep, "after is:", this.getNext(this.freep));
    this.setAllocated(p);
    return (p + 2)<<2;
  }

  // takes a byte pointer
  free(blockPtr){
 //   console.log("freed:", blockPtr);

    this.freesDone++;
    //  console.log("ALLOC free");
    let sizeFreed = this.getSize(blockPtr);

  //  console.log("free:", blockPtr, "size is", sizeFreed);

    // TODO: Remove when not debugging
    this.memory_used -= this.getSize(blockPtr)*4;

    // special case when freep is null. Indicates that the block being freed is only free block in memory
    if (this.freep == null){
      // single element circular list
      this.setNext(blockPtr, blockPtr, false);
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
      this.numScans++;
    }
    // join upper block
    // works even if memory has been extended since block was allocated, now pointed to by p
    // can't happen after grow_memory since getNext(p) must be within old memory
    if (blockPtr + this.getSize(blockPtr) === this.getNext(p)){
      this.merged++;
      sizeFreed += this.getSize(this.getNext(p));
      this.setSize(blockPtr, sizeFreed);
      this.setNext(blockPtr, this.getNext(this.getNext(p)), false);
    } else {
      this.setNext(blockPtr, this.getNext(p), false);
    }
//    console.log("next set to:", this.getNext(blockPtr));
    // join lower block
    if (p + this.getSize(p) === blockPtr){
      this.merged++;
      sizeFreed += this.getSize(p);
      this.setSize(p, sizeFreed);
      // update next in case both sides merged, need a new next ptr
      this.setNext(p, this.getNext(blockPtr), false);
    } else {
      this.setNext(p, blockPtr, false);
    }
    // in case freep was the upper block, so freep would otherwise point to the middle of a block
    this.freep = p;
    if (sizeFreed >= this.requiredSize){
      this.requiredSize = 0;
    }
  }

  // Will only be called at most once per block, since marked flag gets set.
  // pushes onto front of list, so marking phase does DFS
  // TODO: Pointer currently to flag block. Will need to change to point to size when header squashed
  pushMarkedSet(rawPtr){
    // TODO: Use SetNext and SetMark instead
    // both allocated and marked
    this.setMarked(rawPtr);
   // this.uview[rawPtr] = 3;
    // update Next pointer to point to old head of marked list
    this.setNext(rawPtr, this.markedSet, true);
 //   this.uview[rawPtr - 2] = this.markedSet;
    this.markedSet = rawPtr;
  }

  popMarkedSet(){
    let ptr = this.markedSet;
    // follow pointer to next element
    this.markedSet = this.getNext(ptr);
    return ptr;
  }

  markReference(ptr){
    if (ptr & 1){
      let rawPtr = (ptr>>2) - 2; // headersize/4 = 2
      if (!this.isMarked(rawPtr)){ // not yet marked
  //      console.log("marking:", rawPtr, "userptr", ptr);
        this.pushMarkedSet(rawPtr);
      //  this.marked++;
      }
    }
  }

  // go through stack and set marked flag on all memory objects accessible from the stack.
  // can use sp to avoid searching further than necessary up stack
  mark(){
    let stack_top = this.runtime.exports.sp.value >> 2 // shift to get in terms of i32s
  //  console.log("Stack top is:", stack_top);
    for (let i = 0; i < stack_top; i++){
      this.markReference(this.uview[i]);
    }
    // After identifying root set, do DFS until all reachable objects marked
    while (this.markedSet > 1){
      let rawPtr = this.popMarkedSet();
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
    this.merged = 0;
    // TODO: Would be better to chain together allocated objects in a list, then free that list
    //   if mark process doesn't move them to a new list
    const heapLimit = this.memory.buffer.byteLength >> 2;
    let blockPtr = STACK_LIMIT; // starting point of scanning heap
    // TODO: Merge layers, using knowledge of lower levels header structure here, so no abstraction achieved
    while (blockPtr < heapLimit){
      let size = this.getSize(blockPtr);
      if (this.isAllocated(blockPtr)){
        if (this.isMarked(blockPtr)){
          this.clearMarked(blockPtr);
        } else {
          this.free(blockPtr);
        }
      }
      blockPtr += size;
    }
  //  console.log("Merged:", this.merged);
  }

  doGC(){
    this.gcsDone++;
 //    console.log("GC start");
  //  this.marked = 0;
    this.mark(); // well isn't this simple, could probably just write them as a single function instead
   // console.log("live set:", this.marked);
    this.freesDone = 0;
    this.sweep();
//    console.log("GC blocks freed:", this.freesDone);
  }

  stackLimitExceeded(){
    throw "Maximum stack size exceeded";
  }
}

exports.ManagedMemory = ManagedMemory;
