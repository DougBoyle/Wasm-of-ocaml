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

// Could have 1 list point into the next when it is empty
// Pros: Avoid checking multiple empty lists at start of program, when only block is one large one in last list
// Cons: Freeing (removing/inserting) blocks from lists becomes more complex, have to check size of head of lists
//       to determine if some free list pointers need changing to the new block or not


const STACK_LIMIT = 16384 ;


// Various values tracked as part of performance testing/justifying changes
// TODO: Set of high memory usage tests to justify parameters
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
    this.maxMemory = 0;
    // size of block we are looking for when we decide to run GC
    // TODO: Come up with better conditions for when to run GC
    //   e.g. don't run again if stack pointer has barely moved or very few new objects allocated?
    this.requiredSize = 0;

    this._refreshViews = this._refreshViews.bind(this);
    this.malloc = this.malloc.bind(this);

    // stores a list of pointers to free lists of certain sizes.
    // initially, just one pointer
    // malloc will check EACH LIST IN TURN starting from one of appropriate size, growMemory if none found
    // TODO: Find a good GC benchmark that results in heavily fragmented memory/different sizes allocated?

    // for now 2 lists: first holds elements up to 8-words long
    // technically 6-word allocations are possible for closures with no free vars/data blocks with 0 arity
    // TODO: Collect some sort of data on distribution of sizes allocated
    // limit is inclusive i.e. 8 word block goes in first list
    // TODO: Have prev pointer of top of each list be the index of the list it is, so I don't have to search for it

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
    this.mallocsDone = 0;
    this.gcsDone = 0;
    this.freesDone = 0;
    this.freeSequence = []; // updated each time gc called
    this.mallocSequence = [];
    this.gcSequence = []; // record if each GC found a block to free or not

    this.numScans = 0;
    this.fromLarge = 0;
    this.exact = 0;
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

  // cuts a block out of a list, updating the pointer to the free list if necessary
  // don't actually need to give the block itself, just its prev/next ptrs
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
  insertIntoList(blockPtr, size){
    for (let freepIdx = 0; freepIdx < this.numFreeLists; freepIdx++){
      let sizeLimit = this.sizeLimits[freepIdx];
      if (size > sizeLimit && sizeLimit > 0){
        continue; // not this block
      }
      this.setPrev(blockPtr, 0); // TODO: Could set to freepIdx?
      this.linkAfter(blockPtr + size, this.freeLists[freepIdx]);
      this.freeLists[freepIdx] = blockPtr;
      return;
    }
  }


  // TODO: Inefficient completely repeating after heap grows, should be able to tell it to try last block first?
  //  can probably also optimise when GC finds a potential block to allocate, got next/prev pointers so can allocate it
  malloc(bytes){
    this.mallocsDone++;

    // round up to align block
    // Now also allocating a trailer
    // TODO: Checking 4-byte alignment unnecessary due to how malloc gets called?
    // TODO: Can just be +4 >> 2 rather than 8,
    //   ONLY NOW THAT MORE THAN 1 HEADER MUST BE FREE TO SPLIT. Else breaks checks for free blocks?
    const units = (((bytes + 8 - 1) >> 3) + 2) * 2;
    this.memory_used += units * 4;

    // try each free list in turn TODO: Start at correct size
    // size limits are 6 and 8, so this calculates correct list to consider first
    for (let freepIdx = Math.min(units/2 - 3, 2); freepIdx < this.numFreeLists; freepIdx++) {
      // Not needed due to initial value computed above
      //if (this.sizeLimits[freepIdx] < units && this.sizeLimits[freepIdx] > 0){
      //  continue; // skip to next largest list, no blocks large enough in this list
      //}

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
          this.exact++;
          // allocate the whole block
          // successor is the new top of list
          if (successor !== 0) {
            this.setPrev(successor, 0); // remove ptr to now allocated block
          }
          this.freeLists[freepIdx] = successor;
        } else {
          this.fromLarge++;
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
        this.numScans++;
        // indicates the number of times the required element wasn't found immediately
        currentSize = this.getSize(current);
        let successor = this.getNextTail(current + currentSize);
        if (currentSize >= units) {
          // update now we know we aren't going to call GC
          this.maxMemory = Math.max(this.maxMemory, this.memory_used);
          if (currentSize <= units + 4) {
            this.exact++;
            // allocate whole block, need to cut out of free list
            this.linkAfter(prev + prevSize, successor);
          } else {
            this.fromLarge++;
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
    if (this.requiredSize > 0) { // still not enough memory
      this.gcSequence.push(1);
      this.growHeap(units);
    } else {
      this.gcSequence.push(0);
    }
    // now that either a sufficient block has been freed up or memory increased to make space, retry
    this.mallocsDone--;
    this.memory_used -= units * 4;
    return this.malloc(bytes);
  }

  // Since there are multiple lists now, must free/merge blocks before working out list to add to based on size
  free(blockPtr){
    this.numScans++;
    this.freesDone++;
    let sizeFreed = this.getSize(blockPtr);

    // TODO: Remove when not debugging
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
      }

      let successor = this.getNextTail(blockPtr); // could be 0

      if (listIndex == -1) {
        // just update fields, keep in current position in list

        // need to update successor of prev to point to new trailer position
        this.linkAfter(blockPtr + sizeFreed, successor);
        sizeFreed += prevSize;
        this.setSize(prev, sizeFreed);
      } else {
        // need to move to a new list
        sizeFreed += prevSize;
        this.setSize(prev, sizeFreed);
        this.removeFromList(listIndex, this.getPrev(prev), successor);
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
    this.gcsDone++;
  //  this.mallocSequence.push(this.mallocsDone);
 //   this.mallocsDone = 0;
 //   this.marked = 0;
    this.mark();
 //   console.log("live set:", this.marked);
    this.sweep();
 //   console.log("GC ran. freed:", this.freesDone, "total mallocs", this.mallocsDone);
//    this.freeSequence.push(this.freesDone);
 //   this.freesDone = 0;
  }

  stackLimitExceeded(){
    throw "Maximum stack size exceeded";
  }
}

exports.ManagedMemory = ManagedMemory;
