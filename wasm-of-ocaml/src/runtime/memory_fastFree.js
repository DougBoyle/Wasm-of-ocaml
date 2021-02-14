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

// Moving to unordered list means no longer need to keep list circular (in fact is extra effort to do that)
// (prev pointer needed to allow cutting something out of the list when merging 3 blocks together)

// Now move to multiple entirely separate free lists, separated by the sizes they store.
// For now, just rewrite with this possibility but just have a single list still.
// Any time a block is freed/merged, may now need to work out which list to add/move it to.
// malloc tries each list in turn


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

    // points to a free block at all times, unless the free list is empty
    this.freep = STACK_LIMIT;
    this.setSize(STACK_LIMIT, mem_top - STACK_LIMIT);
    // next and prev will both initially be 0 when fresh memory allocated (no predecessor/successor)
  //  this.setNextTail(mem_top, STACK_LIMIT, false);
  //  this.setPrev(STACK_LIMIT, mem_top);


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

  // joins trailer of one block to header of other (or reverse)
  // checks that 'to' field isn't 0 (end of list) before modifying it
  // both assumed to be free blocks
  linkAfter(fromTrailer, toHeader){
    this.uview[fromTrailer - 1] = toHeader;
    if (toHeader !== 0){
      this.uview[toHeader] = fromTrailer;
    }
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

  // TODO: Needs rewriting once list no longer circular!!
  //   Now require that freep always points to start of free list, else lose track of some free blocks
  //   Will remove some inefficiency of always scanning from freep when binning added
  //   Must update freep if that is the block that gets removed

  // TODO: Inefficient completely repeating after heap grows, should be able to get ptr directly by GC/growHeap/Free
  malloc(bytes){
    this.mallocsDone++;

    // round up to align block
    // Now also allocating a trailer
    const units = (((bytes + 8 - 1) >> 3) + 2)*2;
    this.memory_used += units * 4;

    // last block allocated exactly used up the last cell of the free list
    if (this.freep == 0){
      this.requiredSize = units;
      this.doGC();
      if (this.requiredSize > 0) { // still not enough memory
        this.growHeap(units);
      }
    }

    // Case where very first block can be allocated is handled specially
    // TODO: Will need to generalise this when multiple 'freep' pointers for different size blocks exist
    let current = this.freep;
    let currentSize = this.getSize(current);

    if (currentSize >= units){
      let successor = this.getNextTail(current + currentSize);
      if (currentSize <= units + 4){
        // allocate the whole block
        // successor is the new top of list
        if (successor !== 0){
          this.setPrev(successor, 0); // remove ptr to now allocated block
        }
        this.freep = successor;
      } else {
        // allocate in tail end of this block (TODO: Head would be more efficient?)
        let newSize = currentSize - units;
        this.setSize(current, newSize);
        this.linkAfter(current + newSize, successor); // update trailer location

        // block to return
        current += newSize;
        currentSize = units; // used below
        this.setSize(current, units);
      }
      this.setAllocated(current, currentSize);
      return (current + 2) << 2;
    }

    let prev = current;
    let prevSize = currentSize;

    current = this.getNextTail(prev + prevSize);
    while (current !== 0){ // still some cells to explore


      currentSize = this.getSize(current);
      let successor = this.getNextTail(current + currentSize);
      if (currentSize >= units){
        if (currentSize <= units + 4){
          // allocate whole block, need to cut out of free list
          this.linkAfter(prev + prevSize, successor);
        } else {
          // allocate in tail end of block, must update trailer
          let newSize = currentSize - units;
          this.setSize(current, newSize);
          this.linkAfter(current + newSize, successor); // update trailer location

          // block to return
          current += newSize;
          currentSize = units; // used below
          this.setSize(current, units);

        }
        this.setAllocated(current, currentSize);
        return (current + 2) << 2;
      }
      prev = current; // TODO: MOVE INTO A LOOP
      prevSize = currentSize;
      current = successor;
    }

    // all cells searched, call GC then either grow memory or retry search
    this.requiredSize = units;
    this.doGC();
    if (this.requiredSize > 0) { // still not enough memory
      this.growHeap(units);
    }
    // now that either a sufficient block has been freed up or memory increased to make space, retry
    this.mallocsDone--;
    return this.malloc(bytes);
  }

  free(blockPtr){
    this.freesDone++;
    let sizeFreed = this.getSize(blockPtr);

    // TODO: Remove when not debugging
    this.memory_used -= sizeFreed*4;

    // special case when freep is 0. Indicates that the block being freed is only free block in memory
    if (this.freep === 0){
      // single element, both next and prev are 0
      this.setNextTail(blockPtr + sizeFreed, 0, false);
      this.setPrev(blockPtr, 0);
      this.freep = blockPtr;
      if (sizeFreed >= this.requiredSize){
        this.requiredSize = 0;
      }
      return;
    }

    // see if possible to merge with block above (must check for hitting top of heap)
    // if yes, will temporarily disappear from free list until added back in below
    if (blockPtr + sizeFreed < (this.uview.byteLength >> 2) && !this.isAllocated(blockPtr + sizeFreed)){
      // can merge adjacent block above. Take it out of free list
      let succ = blockPtr + sizeFreed;

      let succSize = this.getSize(succ);

      // blocks either side in free list of block being removed
      let successor = this.getNextTail(succ + succSize);
      let predeccessor = this.getPrev(succ); // if 0, removing front of list so need to update freep

      if (predeccessor === 0){
        this.freep = successor;
        if (successor != 0) { // still something left in list
          this.setPrev(successor, 0);
        }
      } else {
        this.linkAfter(predeccessor, successor);
      }

      sizeFreed += succSize;
      this.setSize(blockPtr, sizeFreed);
    }

    // see if possible to merge with block below (must check for hitting bottom of heap)
    // this section ensures block just freed now becomes part of the free list
    if (blockPtr > STACK_LIMIT && !this.isAllocatedTail(blockPtr)){

      // can merge with adjacent block below
      // blockPtr = start of original block OR tail of block below
      let prevSize = this.getSizeTail(blockPtr);

      let prev = blockPtr - prevSize;

      // need to update successor of prev to point to new trailer position
      let successor = this.getNextTail(blockPtr); // could be 0
      this.linkAfter(blockPtr + sizeFreed, successor);

      sizeFreed += prevSize;
      this.setSize(prev, sizeFreed);
    } else {
      // can't merge into existing block below, so create a new element on the list
      // setting next/prev pointers also marks block as unallocated
      this.setPrev(blockPtr, 0); // front of list
      this.linkAfter(blockPtr + sizeFreed, this.freep);
      this.freep = blockPtr; // new head of list
    }

    if (sizeFreed >= this.requiredSize){
      this.requiredSize = 0;
    }
  }

  // Will only be called at most once per block, since marked flag gets set.
  // pushes onto front of list, so marking phase does DFS
  pushMarkedSet(rawPtr){
    // TODO: Use SetNext and SetMark instead
    // both allocated and marked
    this.setMarked(rawPtr);
    // TODO: Switch 'allocated' option to be on setPrev rather than setNext
    // Once allocated, no use for a doubly-linked list so just use header to build single-linked list
    this.setPrev(rawPtr, this.markedSet | 1); // keep marked as allocated
    this.markedSet = rawPtr;
  }

  popMarkedSet(){
    let ptr = this.markedSet;
    this.markedSet = this.getPrev(ptr);
    return ptr;
  }

  markReference(ptr){
    if (ptr & 1){
      let rawPtr = (ptr>>2) - 2; // headersize/4 = 2
      if (!this.isMarked(rawPtr)){ // not yet marked
        this.pushMarkedSet(rawPtr);
        this.marked++;
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
    // TODO: Would be better to chain together allocated objects in a list, then free from just that list?
    //   Could just use the header field. Would add a slight overhead to malloc, but make sweep faster
    //   if mark process doesn't move them to a new list
    const heapLimit = this.memory.buffer.byteLength >> 2;
    let blockPtr = STACK_LIMIT; // starting point of scanning heap
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
    this.gcsDone++;
 //   this.marked = 0;
    this.mark();
 //   console.log("live set:", this.marked);
    this.sweep();
 //   console.log("GC ran. freed:", this.freesDone, "total mallocs", this.mallocsDone);
    this.freesDone = 0;
  }

  stackLimitExceeded(){
    throw "Maximum stack size exceeded";
  }
}

exports.ManagedMemory = ManagedMemory;
