//import { toHex, toBinary } from "../utils/utils";
//import treeify from "treeify";

//export const TRACE_MEMORY = false;

/* Notes:
 *
 * Grain's memory system uses a reference-counted garbage collector.
 * Because `ManagedMemory` is the point of communication between the Grain runtime
 * and the memory management system, we are able to neatly intercept the pointers
 * which are visible to the Grain runtime and tag them with our counts. It is
 * then incumbent upon the compiler (and any Grain plugins which interact with memory)
 * to make sure that reference counting functionality exists at appropriate places
 * in code. Here is the basic idea for how this looks in practice for an n-byte heap object:
 *
 * [ 0 bit <reserved> ][ 11-bit counter ][ 4-bit value tag ][ n-bit payload ]
 * ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^~~~~~~~~~~~~~~~
 * {start address}                                          {pointer used by grain runtime}
 *
 * Some notes about this scheme:
 * - The value tag is the same tag used to identify the pointer on the stack
 * - The value tag is admittedly inefficient (since it duplicates the stack value tag),
 *   but it is a "good enough for now" approach for allowing traversal
 * - If studying this design, one will notice this only allows a maximum of 2048 references to a given
 *   GC-managed object. In the future, we will ideally come up with a method of making this
 *   work with a greater number of values.
 *
 *
 * Variable Naming Conventions:
 *   rawPtr  : The pointer returned by the call to malloc()
 *   userPtr : The pointer returned (and referenced by) to the Grain runtime
 */
const {Allocator} = require("./allocator");

class ManagedMemory {
  constructor(memory) {
    this._memory = memory;
    this._view = new Int32Array(memory.buffer);
    this._uview = new Uint32Array(memory.buffer);
    this._u16view = new Uint16Array(memory.buffer);
    // TODO: This can be smaller? e.g. just 32 bit alignment?
    // only actually using the first 2 bytes for a counter (should possibly use 4 bytes)?
    this._headerSize = 8; // 32 bits in bytes (extra space needed for 64-bit alignment)
    // TODO: Initialise this when runtime instantiated
    this._runtime = null;

    // TODO: Work out which of these are needed
    this._refreshViews = this._refreshViews.bind(this);
    this.malloc = this.malloc.bind(this);
    this.decRef = this.decRef.bind(this);
    this.references = this.references.bind(this);
    this.decRefIgnoreZeros = this.decRefIgnoreZeros.bind(this);

    this.allocator = new Allocator(this);
  }

  _refreshViews() {
    this._view = new Int32Array(this._memory.buffer);
    this._uview = new Uint32Array(this._memory.buffer);
    this._u16view = new Uint16Array(this._memory.buffer);
  }


  get uview() {
    return this._uview;
  }

  setRuntime(runtime) {
    this._runtime = runtime;
  }

  malloc(bytes){
    // 2 bytes used for a counter, don't bother with the more complex header Grain uses
    let ptr = this.allocator.malloc(bytes + this._headerSize);
    // initialise reference count to 1
    // no type information stored, can be gotten from the tagged pointer passed to incr/decr

    // we don't actually use the lower 48-bits of the header, just there for alignment
    this._u16view[ptr>>1] = 1;
    return ptr + this._headerSize; // return pointer to data rather than header
  }

  // [TODO] These next three methods can probably be made more efficient
  _getRefCount(rawPtr) {
    return this._u16view[rawPtr>>1];
  }

  _setRefCount(rawPtr, count) {
    console.log("called setcount");
    this._u16view[rawPtr>>1] = count;
  }

  incRef(userPtr) {
    // addresses for values stored in memory end in a 1, integer literals end in 0
    if (userPtr & 1){
      let rawPtr = (userPtr & ~3) - this._headerSize;
      this._setRefCount(rawPtr, this._getRefCount(rawPtr) + 1)
    }
    return userPtr;
  }


  // when an object is freed, we need to decrement the references of each thing it contains
  *references(userPtr) {
    // TODO: Why recreate the view here?
  //  const view = new Int32Array(this._memory.buffer);
    // pointer into 32-bit cells
    const untaggedPtr = userPtr>>2;
    // closures, data and floats have the same structure as far as garbage collection cares
    // identified by the pointer ending in a 1 rather than 0 (immediate integer)
    if (userPtr & 1) {
      // TODO: Do we need to check for cycles ever?
      let arity = this._view[untaggedPtr + 1];
      console.log("arity is:", arity);
      for (let i = 0; i < arity; i++){
        // TODO: Inline whole function so that we don't need 'yield'
        yield this._view[untaggedPtr + i + 2];
      }
    }
  }

  // TODO: Remove checking for 0 once debugging complete
  _decRef(userPtr, ignoreZero){
    if ((userPtr & 1) == 0) {
      return;
    }
    let rawPtr = (userPtr & ~3) - this._headerSize;
    let count = this._getRefCount(rawPtr);
    if (count === 0){
      if (ignoreZero){
        return;
      } else {
        throw "decRef called on object with 0 ref count"
      }
    }
    count--;
    if (count === 0){
      // free object
      for (let element of this.references(userPtr)){
        console.log("recursively decrementing:", element);
        this._decRef(element, false);
      }
      this.allocator.free(rawPtr);
    } else {
      this._setRefCount(rawPtr, count);
    }
  }

  decRefIgnoreZeros(userPtr) {
    this._decRef(userPtr,  true);
    return userPtr;
  }

  decRef(userPtr){
    this._decRef(userPtr, false);
    return userPtr;
  }

  // TODO: A decRefIgnoreZeros without a return type would be useful?

}

exports.ManagedMemory = ManagedMemory;
