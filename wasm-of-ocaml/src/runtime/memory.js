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

New Layout:
[31-bit next, 1 bit alloc | 31-bit size, 1 bit marked | ... | 31-bit size | 31-bit prev, 1 bit alloc]


TODO: Latest design (making pointer manipulations far easier hopefully):
... | next] <--> [prev | size | ... | size | next] <--> [prev | ...

When a block is allocated:
[next ptr (still to header)/alloc bit | size | ... | size | 31 zeros + alloc bit]
Have 'next' ptr at head when allocated as it is only used for the marked list, so can avoid checking size field

marked flag still goes in first size field, allocated flag goes in both prev and next.
Now prev and next are at switched ends of object and next points to header,
  prev points to trailer (can either point to the trailer or the end of the object).
Therefore, each pointer is literally just jumping over the allocated objects.
MASSIVELY SIMPLIFIES MERGING since blocks outside of 3 maybe being merged don't get adjusted,
  compared to having to change pred pointer on the double successor when the successor is merged.

When moving to out of order list, pointers will no longer just point over the allocated memory
  between blocks, but WON'T NEED TO MODIFY DOUBLE SUCCESSORS etc. still.
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
    // size of block we are looking for when we decide to run GC
    // TODO: Come up with better conditions for when to run GC
    //   e.g. don't run again if stack pointer has barely moved or very few new objects allocated?
    this.requiredSize = 0;

    this._refreshViews = this._refreshViews.bind(this);
    this.malloc = this.malloc.bind(this);

    // initialise free list to a single block starting after the shadow stack
    // TODO: Update initialisation to set block correctly.
    //   When changed to O(1) malloc/free, will no longer need to initialise as circular list
    let mem_top = this.uview.byteLength >> 2;
    // set prev/next/size fields to construct a single block (TODO: Could just call the corresponding functions?)
    //this.uview[STACK_LIMIT] = STACK_LIMIT; // only element of cyclic list points to itself
    //this.uview[STACK_LIMIT + 1] =  (this.uview.byteLength >> 2) - STACK_LIMIT;
    this.setSize(STACK_LIMIT, mem_top - STACK_LIMIT);
    this.setNextTail(mem_top, STACK_LIMIT, false);
    this.setPrev(STACK_LIMIT, mem_top);


    // TODO: Just for debugging
    this.mallocsDone = 0;
    this.gcsDone = 0;
    this.freesDone = 0;
  }

  _refreshViews() {
    this.uview = new Uint32Array(this.memory.buffer);
  }

  setRuntime(runtime) {
    this.runtime = runtime;
  }

  log(v){
    console.log("log:", v);
    return v;
  }


  // Tail suffix on functions indicates they tail a pointer to end on block, not start
  // Header: [prev ptr (and alloc bit), size (and marked bit), ...]
  // Trailer: [..., size, next ptr (and alloc bit)]
  // TODO: Clean up all these functions?

  getNextTail(ptr){
    return this.uview[ptr - 1] & (~1);
  }
  setNextTail(ptr, val, allocated){
    this.uview[ptr - 1] = val | allocated; // sets allocated bit
  }

  // will be needed when I switch to out of order list and O(1) free
  getPrev(ptr){
    return this.uview[ptr] & (~1);
  }
  setPrev(ptr, val){ // Only used when not already allocated, so don't need extra arg
    this.uview[ptr] = val;
  }

  // 2 ways to check allocated depending on if accessing from head or tail
  isAllocated(ptr){
    return this.uview[ptr] & 1;
  }
  isAllocatedTail(ptr){
    return this.uview[ptr - 1] & 1;
  }
  // needs to update both ends so takes head pointer + size
  setAllocated(ptr, size){
    this.uview[ptr] |= 1;
    this.uview[ptr + size - 1] |= 1;
  }

  // 2 ways to get size depending on if accessing from head or tail
  getSize(ptr){
    return this.uview[ptr + 1] & (~1);
  }
  getSizeTail(ptr){
    return this.uview[ptr - 2]; // no marked bit used in tail size field
  }
  // uses size value to also update the tail field
  setSize(ptr, size){
    this.uview[ptr + 1] = size;
    this.uview[ptr + size - 2] = size;
  }

  isMarked(ptr){
    return this.uview[ptr + 1] & 1;
  }
  setMarked(ptr){
    this.uview[ptr + 1] |= 1;
  }
  clearMarked(ptr){
    this.uview[ptr + 1] &= (~1);
  }

  // TODO: Also needs to set Prev pointer to work correctly now?
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
    this.free(ptr);
  }

  malloc(bytes){
    this.mallocsDone++;

    //      console.log("ALLC malloc");
    // round up to align block
    // Now also allocating a trailer
    // TODO: Change back to be just + 2
    const units = (((bytes + 8 - 1) >> 3) + 2)*2;
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
    // TODO: As next pointer now at end of list, must also track size on each iteration too
    let prev = this.freep;
    let prevSize = this.getSize(prev);
    // TODO: Put initalisation back into for loop?
    let p = this.getNextTail(prev + prevSize);
    let pSize = this.getSize(p);
    for ( ; ; prev = p, prevSize = pSize, p = this.getNextTail(p + pSize)){
      // first thing done is to update sizes

      pSize = this.getSize(p);

      if (pSize >= units) {
        if (pSize <= units + 4) {
          // close enough fit that only space for header/trailer would be left, alloc whole  block
          // TODO: Later logic must account for possibility that size != units
          if (this.getNextTail(p + pSize) === p){
            prev = null; // was only element of list, will now set freep = null on line 127
          } else {
            // removing from doubly linked list
            let successor = this.getNextTail(p + pSize);
            this.setNextTail(prev + prevSize, successor, false);
            this.setPrev(successor, prev + prevSize);
          }
        } else {
          // TODO: Now I have trailers, may be easier just to allocate at head?

          let successor = this.getNextTail(p + pSize);
          // allocate tail end of free block, update various fields (have to move trailer)
          let new_size = pSize - units;
          this.setSize(p, new_size);
          // TODO: Generally these should always be seen in pairs,
          //   suggests I should just have a single function to link two points?
          this.setNextTail(p + new_size, successor, false); // creates new trailer
          this.setPrev(successor, p + new_size);
          // new block (next/prev pointers are useless on freshly allocated block)
          p += new_size;
          this.setSize(p, units);
          pSize = units; // use 'pSize' below as the amount allocated
        }
        this.freep = prev;

        // setting next/prev ptrs on allocated block isn't useful here, but do need to set allocated bit
        this.setAllocated(p, pSize);
        // return a pointer in terms of bytes to the data, not the header
        return (p + 2)<<2;
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
        // TODO: Also have to update the size here now p may have changed
        p = this.freep;
        pSize = this.getSize(this.freep);
      }


    }
  }

  // takes a block pointer
  // TODO: Rewrite to no longer scan list (Once changed to out-of-order list just using adjacent pointers)
  // Needs to clear 'allocated' bit in 'prev' ptr when freed
  free(blockPtr){
    // Just for debugging
    this.freesDone++;
    let sizeFreed = this.getSize(blockPtr);

    // TODO: Remove when not debugging
    this.memory_used -= sizeFreed*4;

    // special case when freep is null. Indicates that the block being freed is only free block in memory
    if (this.freep == null){
      // single element circular list
      this.setNextTail(blockPtr + sizeFreed, blockPtr, false);
      this.setPrev(blockPtr, blockPtr + sizeFreed); // TODO: Remove and make non-circular
      this.freep = blockPtr;
      if (sizeFreed >= this.requiredSize){
        this.requiredSize = 0;
      }
      return;
    }

    let p, pSize;
    // find the free block closest behind the block being freed
    for (p = this.freep, pSize = this.getSize(this.freep); // TODO: Can rewrite to avoid finding Next many times?
         blockPtr < p || blockPtr > this.getNextTail(p + pSize); p = this.getNextTail(p + pSize)){

      pSize = this.getSize(p) // TODO: Slightly inefficient calculating twice at start of loop
      if (p >= this.getNextTail(p + pSize) && (blockPtr > p || blockPtr < this.getNextTail(p + pSize))){
        // block to free is at one end of list
        break;
      }
    }

    // TODO: Cleanup and put in loop somewhere
    pSize = this.getSize(p);

    // Merging upper/lower blocks is now trivial due to arrangement of pointers.
    // Effect of merging isn't visible outside successor/predecessor blocks.


    // TODO: Yet another place Next is recalculated
    let successor = this.getNextTail(p + pSize);
    if (blockPtr + sizeFreed === successor){
      // join upper block
      sizeFreed += this.getSize(successor);
      this.setSize(blockPtr, sizeFreed);
    } else {
      // update trailer on block if not merged
      this.setNextTail(blockPtr + sizeFreed, successor, false);
      this.setPrev(successor, blockPtr + sizeFreed);
    }

    if (p + pSize === blockPtr){
      // join lower block
      sizeFreed += pSize;
      this.setSize(p, sizeFreed);
    } else {
      // update header on block if not merged
      this.setNextTail(p + pSize, blockPtr, false);
      this.setPrev(blockPtr, p + pSize);
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
    // TODO: Unfortunate inefficiency that we must now get the size to do marking
  //  this.setNextTail(rawPtr + this.getSize(rawPtr), this.markedSet, true);

    // TODO: Switch 'allocated' option to be on setPrev rather than setNext
    this.setPrev(rawPtr, this.markedSet);
    this.uview[rawPtr] |= 1;

 //   this.uview[rawPtr - 2] = this.markedSet;
    this.markedSet = rawPtr;
  }

  popMarkedSet(){
    let ptr = this.markedSet;
    // follow pointer to next element
 //   this.markedSet = this.uview[ptr - 2];
    // TODO: Unfortunate inefficiency that we must now get the size to do marking
  //  this.markedSet = this.getNextTail(ptr + this.getSize(ptr));
    this.markedSet = this.getPrev(ptr);
    return ptr;
  }

  markReference(ptr){
    if (ptr & 1){
      let rawPtr = (ptr>>2) - 2; // headersize/4 = 2
      if (!this.isMarked(rawPtr)){ // not yet marked
        this.pushMarkedSet(rawPtr);
      //  this.marked++;
      }
    }
  }

  // go through stack and set marked flag on all memory objects accessible from the stack.
  // can use sp to avoid searching further than necessary up stack
  mark(){
    let stack_top = this.runtime.exports.sp.value >> 2 // shift to get in terms of i32s
    for (let i = 0; i < stack_top; i++){
      this.markReference(this.uview[i]);
    }
    // After identifying root set, do DFS until all reachable objects marked
    while (this.markedSet > 1){ // > 1 rather than 0 not strictly necessary, since allocated bit is ignored
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
  }

  doGC(){
  //  console.log("Running GC");
    this.gcsDone++;
  //  this.marked = 0;
    this.mark(); // well isn't this simple, could probably just write them as a single function instead
   // console.log("live set:", this.marked);
    this.sweep();
 //   console.log("GC ran. freed:", this.freesDone, "total mallocs", this.mallocsDone);
    this.freesDone = 0;
  }

  stackLimitExceeded(){
    throw "Maximum stack size exceeded";
  }
}

exports.ManagedMemory = ManagedMemory;
