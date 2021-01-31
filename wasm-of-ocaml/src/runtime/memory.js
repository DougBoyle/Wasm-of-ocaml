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

const {STACK_LIMIT, Allocator} = require(process.env.OCAML_TO_WASM_RT + "/allocator");
const headerSize = 8;// just for 64 bit alignment, actually just 1 bit used for now [31 unused, 1 bit, 32 unused]

class ManagedMemory {
  constructor(memory) {
    this._memory = memory;
    this._uview = new Uint32Array(memory.buffer); // In i32 view, stack top is 2^14 = 16384
    // TODO: Initialise this when runtime instantiated, for calling functions moved into Wasm
    this.runtime = null;
    this.markedSet = []; // fringe left to explore

    this._refreshViews = this._refreshViews.bind(this);
    this.malloc = this.malloc.bind(this);
    this.references = this.references.bind(this);

    this.allocator = new Allocator(this);
  }

  _refreshViews() {
    this._view = new Int32Array(this._memory.buffer);
    this._uview = new Uint32Array(this._memory.buffer);
  }

  get uview() {
    return this._uview;
  }

  setRuntime(runtime) {
    this.runtime = runtime;
  }

  log(v){
    console.log("log sp:", v);
    return v;
  }

  malloc(bytes){
    let ptr = this.allocator.malloc(bytes + headerSize);
    // Enforce property that every block is unmarked unless current gc pass has marked it. But do indicate allocated
    this.uview[ptr>>2] = 2;
 //   console.log("malloc, ptr is:", ptr);
    return ptr + headerSize; // return pointer to data rather than header
  }

  // passed a pointer to this level's header.
  // i.e. ptr + 3 is the arity field
  *references(rawPtr) {
    // closures, data and floats have the same structure as far as garbage collection cares
    // identified by the pointer ending in a 1 rather than 0 (immediate integer)
    if (rawPtr) {
      let arity = this._view[rawPtr + 3];
  //    console.log("arity is:", arity);
      for (let i = 0; i < arity; i++){
        // TODO: Inline whole function so that we don't need 'yield'
        yield this._view[rawPtr + i + 4];
      }
    }
  }

  markReference(ptr){
    if (ptr & 1){
      let raw_ptr = (ptr>>2) - 2; // headersize/4 = 2
      if (!(this.uview[raw_ptr] & 1)){ // not yet marked
        this.uview[raw_ptr] = 1;
        this.markedSet.push(raw_ptr);
      }
    }
  }

  // go through stack and set marked flag on all memory objects accessible from the stack.
  // can use sp to avoid searching further than necessary up stack
  mark(){
    let stack_top = this.runtime.exports.sp >> 2 // shift to get in terms of i32s
    for (let i = 0; i < stack_top; i++){
      this.markReference(this.uview[i]);
    }

    // After identifying root set, do DFS until all reachable objects marked
    // TODO: May want to make it so DFS done alongside marking the root objects, or not?
    while (!this.markedSet.isEmpty()){
      let rawPtr = this.markedSet.pop();
      // mark all of its referenced objects
      for (let element of this.references(rawPtr)){
        this.markReference(element);
      }
    }
    // by end of function, markedSet is empty and all reachable objects have their marked bit set
  }

  // traverse all allocated objects of the heap, adding unmarked ones to the free list.
  // Uses fact that every block allocated has 0b10 in its header, and blocks are always
  // allocated in aligned chunks so can always read the header from the correct position
  sweep(){
    const heapLimit = this._memory.buffer.byteLength >> 2;
    let blockPtr = STACK_LIMIT; // starting point of scanning heap
    // TODO: Merge layers, using knowledge of lower levels header structure here, so no abstraction achieved
    while (blockPtr < heapLimit){
      let size = this.allocator.getSize(blockPtr);
      if (this.uview[blockPtr + 2] == 2){
        // allocated but not marked
        // TODO: Rewrite allocator.free to take a pointer of the correct form
        this.allocator.free((blockPtr + 2) << 2);
      }
    }
  }

  doGC(){
    this.mark(); // well isn't this simple, could probably just write them as a single function instead
    this.sweep();
  }
}

exports.ManagedMemory = ManagedMemory;
