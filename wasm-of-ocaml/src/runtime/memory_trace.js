// Newest version of memory, but with tracing of heap using for performance measurement

const STACK_LIMIT = 16384 * 2 ;

// Various values tracked as part of performance testing/justifying changes
class ManagedMemory {
  constructor(memory) {
    this.memory = memory;
    this.uview = new Uint32Array(memory.buffer); // In i32 view, stack top is 2^14 = 16384
    this.runtime = null;

    // fringe left to explore
    // 0 indicates empty list, since bottom of memory is reserved for stack
    this.markedSet = 0;

    this.memory_used = 0;
    this.maxMemory = 0;

    // size of block we are looking for when we decide to run GC
    this.requiredSize = 0;

    this._refreshViews = this._refreshViews.bind(this);
    this.malloc = this.malloc.bind(this);

    // stores a list of pointers to free lists of certain sizes.
    // initially, just one pointer
    // TODO: Collect some sort of data on distribution of sizes allocated
    // limit is inclusive i.e. 8 word block goes in first list
    // TODO: Have prev pointer of top of each list be the index of the list it is, so I don't have to search for it?

    this.sizeLimits = [6, 8, 0]; // 0 indicates unlimited
    this.freeLists = [0, 0, STACK_LIMIT]; // free pointers initialised as no very small blocks, 1 large initial block
    this.numFreeLists = 3;

    /*
    this.sizeLimits = [8, 0]; // 0 indicates unlimited
    this.freeLists = [0, STACK_LIMIT]; // free pointers initialised as no very small blocks, 1 large initial block
    this.numFreeLists = 2;
    */

    /*
    this.sizeLimits = [0]; // 0 indicates unlimited
    this.freeLists = [STACK_LIMIT]; // free pointers initialised as no very small blocks, 1 large initial block
    this.numFreeLists = 1;
    */

    // initialise free list to a single block starting after the shadow stack
    // points to a free block at all times, unless the free list is empty
    // next and prev will both initially be 0 when fresh memory allocated (no predecessor/successor)
    let mem_top = this.uview.byteLength >> 2;
    this.setSize(STACK_LIMIT, mem_top - STACK_LIMIT);

    // Just for debugging
 /*   this.mallocsDone = 0;
    this.gcsDone = 0;
    this.freesDone = 0;
    this.freeSequence = []; // updated each time gc called
    this.mallocSequence = [];
    this.gcSequence = []; // record if each GC found a block to free or not

    this.numScans = 0;
    this.fromLarge = 0;
    this.exact = 0;*/
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

  getListIndex(size){
    return Math.min(size/2 - 3, 2);
  }

  // Tail suffix on functions indicates they tail a pointer to end on block, not start
  // Header: [prev ptr (and alloc bit), size (and marked bit), ...]
  // Trailer: [..., size, next ptr (and alloc bit)]
  // TODO: Clean up all these functions?

  getNextTail(ptr){
    return this.uview[ptr - 1] & (~1);
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

    this.memory_used += pages << 16;
    this.free(ptr);
  }

  // cuts a block out of a list, updating the pointer to the free list if necessary
  // don't actually need to pass the block itself, just its prev/next ptrs
  removeFromList(freepIdx, prevTail, nextHead){
    // check both prevTail and nextHead to ensure there's actually something to link to
    if (prevTail != 0){
      this.linkAfter(prevTail, nextHead);
    } else { // head of this free list, need to update pointer
      if (nextHead != 0) {
        this.setPrev(nextHead, prevTail); // will set to 0
      }
      this.freeLists[freepIdx] = nextHead;
    }
  }

  // used by free for cutting adjacent block out of list.
  // only look up freepIdx if necessary.
  // TODO: Worth setting prev to be the freepIdx at the front of the list or not? Seems like no downside
  removeFromUnknownList(blockPtr, prevTail, nextHead){
    // check both prevTail and nextHead to ensure there's actually something to link to
    if (prevTail != 0){
      this.linkAfter(prevTail, nextHead);
    } else { // head of this free list, need to update pointer
      if (nextHead != 0) {
        this.setPrev(nextHead, prevTail); // will set to 0
      }
      // work out which list pointer needs updating
      for (let freepIdx = 0; freepIdx < this.numFreeLists; freepIdx++){
        if (this.freeLists[freepIdx] == blockPtr){
          this.freeLists[freepIdx] = nextHead;
          return;
        }
      }
    }
  }

  // inserts the given block at the front of the appropriate free list
  // overwrites both prev/next to ensure list structure correct
  insertIntoList(blockPtr, size) {
    let freepIdx = this.getListIndex(size);
    this.setPrev(blockPtr, 0); // TODO: Could set to freepIdx?
    this.linkAfter(blockPtr + size, this.freeLists[freepIdx]);
    this.freeLists[freepIdx] = blockPtr;
  }


  // TODO: Inefficient completely repeating after heap grows, should be able to tell it to try last block first?
  //  can probably also optimise when GC finds a potential block to allocate, got next/prev pointers so can allocate it
  malloc(bytes){
 //   this.mallocsDone++;

    // round up to align block
    // Now also allocating a trailer
    // TODO: Checking 4-byte alignment unnecessary due to how malloc gets called?
    // TODO: Can just be +4 >> 2 rather than 8,
    //   ONLY NOW THAT MORE THAN 1 HEADER MUST BE FREE TO SPLIT. Else breaks checks for free blocks?
    const units = (((bytes + 8 - 1) >> 3) + 2) * 2;
    this.memory_used += units * 4;

    // try each free list in turn
    // size limits are 6 and 8, so this calculates correct list to consider first
    for (let freepIdx = this.getListIndex(units); freepIdx < this.numFreeLists; freepIdx++) {
      this.numScans++;

      let freep = this.freeLists[freepIdx];
      // list empty
      if (freep == 0) {continue;}


      // if a block goes below this size, need to move it to a smaller list
      let lowerLimit = (freepIdx > 0) ? this.sizeLimits[freepIdx - 1] : 0;

      // Case where very first block can be allocated is handled specially
      // TODO: Can move within loop? Or is that less efficient?
      let current = freep;
      let currentSize = this.getSize(current);

      if (currentSize >= units) {
        // update now we know we aren't going to call GC
        this.maxMemory = Math.max(this.maxMemory, this.memory_used);
        let successor = this.getNextTail(current + currentSize);
        if (currentSize <= units + 4) {
          // allocate the whole block
          // successor is the new top of list
          if (successor !== 0) {
            this.setPrev(successor, 0); // remove ptr to now allocated block
          }
          this.freeLists[freepIdx] = successor;
        } else {
          // allocate in tail end of this block (TODO: Head would be more efficient?)
          let newSize = currentSize - units;
          this.setSize(current, newSize);

          if (newSize <= lowerLimit){ // need to move to smaller free list
            this.removeFromList(freepIdx, 0, successor); // current = freep so prev is 0
            this.insertIntoList(current, newSize);
          } else {
            this.linkAfter(current + newSize, successor); // update trailer location
          }

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
      while (current !== 0) { // still some cells to explore
      //  this.numScans++;
        // indicates the number of times the required element wasn't found immediately
        currentSize = this.getSize(current);
        let successor = this.getNextTail(current + currentSize);
        if (currentSize >= units) {
          // update now we know we aren't going to call GC
          this.maxMemory = Math.max(this.maxMemory, this.memory_used);
          if (currentSize <= units + 4) {
            // allocate whole block, need to cut out of free list
            this.linkAfter(prev + prevSize, successor);
          } else {
            // allocate in tail end of block, must update trailer
            let newSize = currentSize - units;
            this.setSize(current, newSize);


            if (newSize <= lowerLimit){ // need to move to smaller free list
              this.removeFromList(freepIdx, prev + prevSize, successor); // current = freep so prev is 0
              this.insertIntoList(current, newSize);
            } else {
              this.linkAfter(current + newSize, successor); // update trailer location
            }

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
    }


    // all cells searched, call GC then either grow memory or retry search
    this.requiredSize = units;
    this.doGC();
    // 1 page = 2^14 words. If GC only frees up <256 words, less than 2% of a page free
    // so will probably end up growing heap in the next couple calls anyway.
    // If heap going to grow anyway, better to do immediately than many very small collections
    if (this.requiredSize > 0 || this.dataFreed < 256) { // still not enough memory
  //    this.gcSequence.push(1);
      this.growHeap(units);
    }/* else {
      this.gcSequence.push(0);
    }*/
    // now that either a sufficient block has been freed up or memory increased to make space, retry
  //  this.mallocsDone--;
    this.memory_used -= units * 4;
    return this.malloc(bytes);
  }

  // Since there are multiple lists now, must free/merge blocks before working out list to add to based on size
  free(blockPtr){
//    this.numScans++;
 //   this.freesDone++;
    let sizeFreed = this.getSize(blockPtr);

    // part of optimisation to avoid long tails
    this.dataFreed += sizeFreed;

    this.memory_used -= sizeFreed*4;

    // see if possible to merge with block above (must check for hitting top of heap)
    // if yes, will temporarily disappear from free list until added back in below
    if (blockPtr + sizeFreed < (this.uview.byteLength >> 2) && !this.isAllocated(blockPtr + sizeFreed)){
      // can merge adjacent block above. Take it out of free list
      let succ = blockPtr + sizeFreed;

      let succSize = this.getSize(succ);

      // blocks either side in free list of block being removed
      let successor = this.getNextTail(succ + succSize);
      let predeccessor = this.getPrev(succ); // if 0, removing front of list so need to update freep

      this.removeFromUnknownList(succ, predeccessor, successor);

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

      // previous block has now grown. 2 cases:
      // 1) Still within size range for previous block (i.e. already in large list), can just shift ptrs
      // 2) Now too large for current list, must cut out and insert into new list

      // first determine if we need to move
      /*  Since free lists hold specific sizes, can calculate indices efficiently

      let listIndex = -1;
      for (let freepIdx = 0; freepIdx < this.numFreeLists - 1; freepIdx++) {
        let limit = this.sizeLimits[freepIdx];
        if (limit < prevSize){
          continue; // not this block
        }
        if (limit < prevSize + sizeFreed) {
          // will change block. limit >= old size of prev, but less than new size
          listIndex = freepIdx;
        }
        break;
      }*/
      // TODO: Move to some function perhaps? In case set of lists used changes?
      let oldIdx = this.getListIndex(prevSize);
      let newIdx = this.getListIndex(prevSize + sizeFreed);

      let successor = this.getNextTail(blockPtr); // could be 0

   //   if (listIndex == -1) {
      if (oldIdx === newIdx){
        // just update fields, keep in current position in list

        // need to update successor of prev to point to new trailer position
        this.linkAfter(blockPtr + sizeFreed, successor);
        sizeFreed += prevSize;
        this.setSize(prev, sizeFreed);
      } else {
        // need to move to a new list
        sizeFreed += prevSize;
        this.setSize(prev, sizeFreed);
      //  this.removeFromList(listIndex, this.getPrev(prev), successor);
        this.removeFromList(oldIdx, this.getPrev(prev), successor);
        this.insertIntoList(prev, sizeFreed);
      }
    } else {
      // can't merge into existing block below, so create a new element on the list
      // setting next/prev pointers also marks block as unallocated
      this.insertIntoList(blockPtr, sizeFreed);
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
     //   this.marked++;
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
    // TODO: Determine good choice for this
    // If only a small fraction of memory freed, increase heap as will likely need to anyway
    this.dataFreed = 0;


  //  this.gcsDone++;
  //  this.mallocSequence.push(this.mallocsDone);
  //  this.mallocsDone = 0;
 //   this.marked = 0;
    this.mark();
 //   console.log("live set:", this.marked);
    this.sweep();
 //   console.log("GC ran. freed:", this.freesDone, "total mallocs", this.mallocsDone);
  //  this.freeSequence.push(this.freesDone);
  //  this.freesDone = 0;
  }

  stackLimitExceeded(){
    throw "Maximum stack size exceeded";
  }
}

exports.ManagedMemory = ManagedMemory;
