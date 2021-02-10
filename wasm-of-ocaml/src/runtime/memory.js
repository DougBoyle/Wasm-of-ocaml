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
    this.uview[STACK_LIMIT] = STACK_LIMIT; // only element of cyclic list points to itself
    this.uview[STACK_LIMIT + 1] =  (this.uview.byteLength >> 2) - STACK_LIMIT;


    // TODO: Just for debugging
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

  // TODO: Work out if allocated bit needed. Marked list doesn't need double-linked list?
  setPrev(ptr, size, val){
    // slight overhead in having to work out the size of a block to set its predecessor
    // TODO: Would it be better to put next at end and prev at start?
    //   do we already know the size any time next is accessed?
    this.uview[ptr + size - 1] = val; // for now assume block must be free - allocated = 0
  }
  getPrev(ptr, size){
    return this.uview[ptr + size - 1] & (~1);
  }

  // setSize assumes the block is not marked, we never modify the size once allocated
  setSize(ptr, val){
    this.uview[ptr + 1] = val;
    // Also set value in trailer, 2nd last word of block
    this.uview[ptr + val - 2] = val;
  }
  setMarked(ptr){
    this.uview[ptr + 1] |= 1;
  }
  clearMarked(ptr){
    this.uview[ptr + 1] &= (~1);
  }
  setAllocated(ptr, size){ // size needed to also update trailer
    this.uview[ptr] |= 1;
    this.uview[ptr + size - 1] |= 1;
  }

  // TODO: Also needs to set Prev pointer to work correctly now?
  growHeap(units) {
    console.log("Heap grows");
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
    let prev = this.freep;
    let p;
    for (p = this.getNext(prev); ; prev = p, p = this.getNext(p)){
      let size = this.getSize(p);
      if (size >= units) {
        if (size <= units + 4) {
          // close enough fit that only space for header/trailer would be left, alloc whole  block
          // TODO: Later logic must account for possibility that size != units
          if (this.getNext(p) === p){
            prev = null; // was only element of list, will now set freep = null on line 127
          } else {
            // removing from doubly linked list
            let successor = this.getNext(p);
            this.setNext(prev, successor, false);
            this.setPrev(successor, this.getSize(successor), prev);
          }
        } else {
          // allocate tail end of free block, update various fields (have to move trailer)
          let new_size = this.getSize(p) - units;
          this.setSize(p, new_size);
          this.setPrev(p, new_size, prev); // creates new trailer
          // new block
          p += new_size;
          this.setSize(p, units);
          size = units; // use 'size' below as the amount allocated
        }
        this.freep = prev;

        // setting next/prev ptrs on allocated block isn't useful here, but do need to set allocated bit
        this.setAllocated(p, size);
        // return a pointer in terms of bytes to the data, not the header
        return (p + 2)<<2;
      }
      console.log("malloc loop");
      if (p === this.freep){
        console.log("OOM, try GC or grow");
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
  }

  // takes a block pointer
  // TODO: Rewrite to no longer scan list
  // Needs to clear 'allocated' bit in 'prev' ptr when freed
  free(blockPtr){
    //  console.log("ALLOC free");
    let sizeFreed = this.getSize(blockPtr);

    // TODO: Remove when not debugging
    this.memory_used -= this.getSize(blockPtr)*4;

    // special case when freep is null. Indicates that the block being freed is only free block in memory
    if (this.freep == null){
      // single element circular list
      this.setNext(blockPtr, blockPtr, false);
      this.setPrev(blockPtr, sizeFreed, blockPtr); // TODO: Remove and make non-circular
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

    // Due to complexity of adjusting all the pointers/trailers/sizes and merging blocks,
    // detect and handle each case separately.
    // Doing above then doing below separately causes issues due to some parts getting out of date

    // p and s are the blocks either side of freed block. Could be the same block if only 1 in list
    // TODO: Would having next/prev point to header and trailer respectively make updates easier?
    //   e.g. joining upper block only moves header, and joining lower block only moves trailer,
    //   so could avoid needing to change the pointers on the blocks adjacent to those.
    //   if doing that, may also want to put next ptr in trailer and prev ptr in header??

    // TODO: Make these changes + simplifications before moving to O(1) free
    let s = this.getNext(p);
    let prevSize = this.getSize(p);
    if (blockPtr + sizeFreed === s){
      // Need to join upper block
      if (p + prevSize === blockPtr){
        // Need to join both blocks
        // Possibility that they are now the only blocks in memory, then be careful with pointers
        sizeFreed += prevSize + this.getSize(s);
        this.setSize(p, sizeFreed);
        // TODO: Issue with current design, handle case they are only blocks in mem specially
        let ss = this.getNext(s);
        if (ss === p){
          // only blocks in memory, just coalesce into a single block now
          // overwrite all header/trailer fields
          this.setNext(p, p, false);
          this.setPrev(p, sizeFreed, p);
        } else {
          // at least 1 other block, so don't have to worry about pointers being the same
          this.setPrev(ss, this.getSize(ss), p);
          // need to rewrite header/trailer of p (which used to be the trailer of s)
          this.setNext(p, ss, false);
          this.setPrev(p, sizeFreed, this.getPrev(p, prevSize));
        }
      } else {
        // just join upper block. If this is only block in mem,
        // only next/pred pointers are to it so can safely assign those everywhere.
        // TODO: Prev pointer on block after that needs to be modified since head has moved
        let ss = this.getNext(s);
        this.setPrev(ss, this.getSize(ss), blockPtr);
        // TODO: Issue with current structure.
        //  If s == ss, don't want to point to ss since its header has now also moved (updating trailer fine)
        if (s === ss){
          this.setNext(blockPtr, blockPtr, false);
        } else {
          this.setNext(blockPtr, ss, false);
        }
        sizeFreed += this.getSize(s);
        this.setSize(blockPtr, sizeFreed);

      }
    } else if (p + prevSize === blockPtr){
      // just join lower block. If this is only block in mem,
      // only next/pred pointers are to it so can safely assign those everywhere
      sizeFreed += prevSize;
      // move trailer and adjust size, no other changes to make.
      this.setPrev(p, sizeFreed, this.getPrev(p, prevSize));
      this.setSize(p, sizeFreed);
    } else {
      // No blocks to join, just update list pointers to insert into list
      // overwriting Next and Prev on blockPtr also marks it as unallocated
      this.setNext(blockPtr, s, false);
      this.setPrev(blockPtr, sizeFreed, p);
      this.setNext(p, blockPtr, false);
      this.setPrev(s, this.getSize(s), p);
    }

    // in case freep was the upper block, so freep would otherwise point to the middle of a block
    this.freep = p;
    if (sizeFreed >= this.requiredSize){
      this.requiredSize = 0;
    }
  }

  /*
  // takes a byte pointer
  free(blockPtr){
    //  console.log("ALLOC free");
    let sizeFreed = this.getSize(blockPtr);

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
    }
    // join upper block
    // works even if memory has been extended since block was allocated, now pointed to by p
    // can't happen after grow_memory since getNext(p) must be within old memory
    if (blockPtr + this.getSize(blockPtr) === this.getNext(p)){
      sizeFreed += this.getSize(this.getNext(p));
      this.setSize(blockPtr, this.getSize(blockPtr) + this.getSize(this.getNext(p)));
      this.setNext(blockPtr, this.getNext(this.getNext(p)), false);
    } else {
      this.setNext(blockPtr, this.getNext(p), false);
    }
    // join lower block
    if (p + this.getSize(p) === blockPtr){
      sizeFreed += this.getSize(p);
      this.setSize(p, this.getSize(p) + this.getSize(blockPtr));
    } else {
      this.setNext(p, blockPtr, false);
    }
    // in case freep was the upper block, so freep would otherwise point to the middle of a block
    this.freep = p;
    if (sizeFreed >= this.requiredSize){
      this.requiredSize = 0;
    }
  }*/

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
 //   this.markedSet = this.uview[ptr - 2];
    this.markedSet = this.getNext(ptr);
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
    // TODO: Would be better to chain together allocated objects in a list, then free that list
    //   if mark process doesn't move them to a new list
    const heapLimit = this.memory.buffer.byteLength >> 2;
    let blockPtr = STACK_LIMIT; // starting point of scanning heap
    // TODO: Merge layers, using knowledge of lower levels header structure here, so no abstraction achieved
    while (blockPtr < heapLimit){
      let size = this.getSize(blockPtr);
      if (this.isAllocated(blockPtr)){
        if (this.isMarked(blockPtr)){
          this.setMarked(blockPtr, 0);
        } else {
          this.free(blockPtr);
        }
      }
      blockPtr += size;
    }
  }

  doGC(){
    console.log("Running GC");
    this.gcsDone++;
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
