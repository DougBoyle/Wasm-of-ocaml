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
class ManagedMemory {
  constructor(memory) {
    this._memory = memory;
    this._view = new Int32Array(memory.buffer);
    this._uview = new Uint32Array(memory.buffer);
    this._u8view = new Uint8Array(memory.buffer);
    this._f32view = new Float32Array(memory.buffer);
    this._f64view = new Float64Array(memory.buffer);
    this._headerSize = 8; // 32 bits in bytes (extra space needed for alignment)
    this._runtime = null;
    // TODO: Work out which of these are needed
    this._refreshViews = this._refreshViews.bind(this);
    this._malloc = this._malloc.bind(this);
    this.populateHeader = this.populateHeader.bind(this);
  }



  _refreshViews() {
    this._view = new Int32Array(this._memory.buffer);
    this._uview = new Uint32Array(this._memory.buffer);
    this._u8view = new Uint8Array(this._memory.buffer);
    this._f32view = new Float32Array(this._memory.buffer);
    this._f64view = new Float64Array(this._memory.buffer);
  }

  get view() {
    return this._view;
  }

  get uview() {
    return this._uview;
  }

  get u8view() {
    return this._u8view;
  }

  get f32view() {
    return this._f32view;
  }

  get f64view() {
    return this._f64view;
  }

  setRuntime(runtime) {
    this._runtime = runtime;
  }

  _malloc(rawPtr, size) {
    if (rawPtr === -1) {
      throw "Out of memory";
    }
    this.populateHeader(rawPtr);
    let userPtr = rawPtr + this._headerSize;
    return userPtr; // offset by headerSize
  }

  populateHeader(rawPtr) {
    let tag = 0x0; // reserved
    var heap = this.u8view;
    for (let i = 0; i < this._headerSize; ++i) {
      heap[rawPtr + i] = 0;
    }
    this._setRefCount(rawPtr, 1);
    heap[rawPtr + 3] = tag & 0b1111; // <- 4-bit tag     --- Looks like a mistake here, taking & with 0?
  }

  // [TODO] These next three methods can probably be made more efficient
  _getRefCount(userPtr) {
    let rawPtr = (userPtr & ~7) - this._headerSize;
    let heap = this.u8view;
    let count = heap[rawPtr] & 0b0111; // reserved bit should always be zero, but let's be safe
    count = count << 8;
    count = count | heap[rawPtr + 1];
    count = count << 8;
    count = count | heap[rawPtr + 2];
    return count;
  }

  _getValueTag(userPtr) {
    let rawPtr = userPtr - this._headerSize;
    let heap = this.u8view;
    return heap[rawPtr + 3];
  }

  _toHex(n, minWidth) {
    return toHex(n, minWidth);
  }

  decRefIgnoreZeros(userPtr) {
    return this.decRef(userPtr, undefined, true);
  }


  _setRefCount(rawPtr, count) {
    const heap = this.u8view;
    
    heap[rawPtr] = (count & (0b01111111 << 16)) >> 16;
    heap[rawPtr + 1] = (count & (0b11111111 << 8)) >> 8;
    heap[rawPtr + 2] = count & 0b11111111;
  
  }

  incRef(userPtr, src) {
    let origInput = userPtr;
    let ptrTagType = getTagType(userPtr, true);
    if (
      userPtr === 0 ||
      ptrTagType === GRAIN_NUMBER_TAG_TYPE ||
      ptrTagType === GRAIN_CONST_TAG_TYPE
    ) {
      // no ref-counting for primitives
      // [TODO] The type-checker should make this not ever be called ideally, but it
      //        significantly complicates our codegen
      return origInput;
    }
    userPtr = userPtr & ~7;
    let rawPtr = userPtr - this._headerSize;
    let refCount = this._getRefCount(userPtr);
    ++refCount;
    this._setRefCount(rawPtr, refCount);
    return origInput;
  }

  *references(userPtr) {
    const view = new Int32Array(this._memory.buffer);
    const ptrTagType = getTagType(userPtr, true);
    const origInput = userPtr;
    userPtr = userPtr & ~7; // <- strip tag
    switch (ptrTagType) {
      case GRAIN_TUPLE_TAG_TYPE:
        let tupleIdx = userPtr / 4;
        let tupleLength = view[tupleIdx];
        if (tupleLength & 0x80000000) {
          // cyclic. return            ---- How does this ever happen, cyclic data should never get collected?
          return;
        } else {
          view[tupleIdx] |= 0x80000000;
        
          for (let i = 0; i < tupleLength; ++i) {
            yield view[tupleIdx + i + 1];
          }
          view[tupleIdx] = tupleLength;
        }
        break;
      case GRAIN_LAMBDA_TAG_TYPE:
        // 4 * (idx + 3)
        let lambdaIdx = userPtr / 4;
        let numFreeVars = view[lambdaIdx + 2];
      
        for (let i = 0; i < numFreeVars; ++i) {
          yield view[lambdaIdx + 3 + i];
        }
        break;
      case GRAIN_GENERIC_HEAP_TAG_TYPE: {
        let genericHeapValUserPtr = userPtr;
        switch (view[genericHeapValUserPtr / 4]) {
          case GRAIN_ADT_HEAP_TAG: {
            let x = genericHeapValUserPtr / 4;
            let arity = view[x + 4];
           
            for (let i = 0; i < arity; ++i) {
              yield view[x + 5 + i];
            }
            break;
          }
          case GRAIN_RECORD_HEAP_TAG: {
            let x = genericHeapValUserPtr / 4;
            let arity = view[x + 3];
           
            for (let i = 0; i < arity; ++i) {
              yield view[x + 4 + i];
            }
            break;
          }
          case GRAIN_ARRAY_HEAP_TAG: {
            let x = genericHeapValUserPtr / 4;
            let arity = view[x + 1];
           
            for (let i = 0; i < arity; ++i) {
              yield view[x + 2 + i];
            }
            break;
          }
          case GRAIN_CHAR_HEAP_TAG:
          case GRAIN_STRING_HEAP_TAG:
          case GRAIN_BOXED_NUM_HEAP_TAG:
            // No traversal necessary
            break;
          default: {
            console.warn(
              `<decRef: Unexpected heap tag: 0x${this._toHex(
                view[genericHeapValUserPtr / 4]
              )}> [userPtr=0x${this._toHex(userPtr)}]`
            );
          }
        }
        break;
      }
      default:
        console.warn(
          `<decRef: Unexpected value tag: 0x${this._toHex(
            ptrTagType
          )}> [userPtr=0x${this._toHex(userPtr)}]`
        );
    }
  }

  _isReference(userPtr) {
    let ptrTagType = getTagType(userPtr, true);
    return !(
      userPtr === 0 ||
      ptrTagType === GRAIN_NUMBER_TAG_TYPE ||
      ptrTagType === GRAIN_CONST_TAG_TYPE
    );
  }

  _incrementRefCount(userPtr) {
    if (!this._isReference(userPtr)) {
      // no ref-counting for primitives
      // [TODO] The type-checker should make this not ever be called ideally, but it
      //        significantly complicates our codegen
      let ptrTagType = getTagType(userPtr, true);
      return;
    }
    userPtr = userPtr & ~7;
    let rawPtr = userPtr - this._headerSize;
    let refCount = this._getRefCount(userPtr);
    this._setRefCount(rawPtr, refCount + 1);
  }

  _decrementRefCount(userPtr, ignoreZeros) {
    if (!this._isReference(userPtr)) {
      // no ref-counting for primitives
      // [TODO] The type-checker should make this not ever be called ideally, but it
      //        significantly complicates our codegen
      let ptrTagType = getTagType(userPtr, true);
      return;
    }
    userPtr = userPtr & ~7;
    let refCount = this._getRefCount(userPtr);
    if (refCount === 0) {
      if (ignoreZeros) {
        trace("ignoring zero refcount");
        return userPtr;
      }
      throw new Error(
        `decRef called when reference count was zero. `
      );
    }
    let rawPtr = userPtr - this._headerSize;
    this._setRefCount(rawPtr, refCount - 1);
  }

  decRef(userPtr, src, ignoreZeros) {
    // [TODO] This does not properly handle cycles yet!!
    let origInput = userPtr;
    let ptrTagType = getTagType(userPtr, true);
    if (
      userPtr === 0 ||
      ptrTagType === GRAIN_NUMBER_TAG_TYPE ||
      ptrTagType === GRAIN_CONST_TAG_TYPE
    ) {
      // no ref-counting for primitives
      // [TODO] The type-checker should make this not ever be called ideally, but it
      //        significantly complicates our codegen
      return origInput;
    }
    userPtr = userPtr & ~7;

    let heap = this.u8view;
    let rawPtr = userPtr - this._headerSize;
    let refCount = this._getRefCount(userPtr);
    // [TODO] This is a blazing-hot code path. Should we eschew error-checking?
    if (refCount === 0) {
      if (ignoreZeros) {
        trace("ignoring zero refcount");
        return userPtr;
      }
      throw new Error(
        `decRef called when reference count was zero.`
      );
    }
    --refCount;
    if (refCount === 0) {
      trace("Should traverse elements and decref() here!");
      for (let childUserPtr of this.references(origInput)) {
        this.decRef(childUserPtr, "FREE");
      }
      this._setRefCount(rawPtr, refCount);
      this.free(userPtr);
    } else {
      this._setRefCount(rawPtr, refCount);
    }
    return origInput;
  }

  malloc(userPtr) {
    return this._runtime.memoryManager.requiredExport("malloc")(userPtr);
  }
  free(userPtr) {
    this._runtime.memoryManager.requiredExport("free")(userPtr);
  }

  _free(userPtr) {
    trace(`free 0x${new Number(userPtr).toString(16)}`);
    trace("end_free");
  }

}



class ManagedType {
  constructor(name, initializer, finalizer, to_string, equals, tag) {
    this._name = name;
    this._initializer = initializer;
    this._finalizer = finalizer;
    this._to_string = to_string;
    this._equals = equals;
    this._tag = tag;
  }

  get name() {
    return this._name;
  }

  get tag() {
    return this._tag;
  }

  initialize(memory, address) {
    if (this._initializer) {
      this._initializer(memory, address);
    }
  }

  finalize(memory, address) {
    if (this._finalizer) {
      this._finalizer(memory, address);
    }
  }

  to_string(memory, address) {
    if (this._to_string) {
      return this._to_string(memory, address);
    }
    return `#<Instance: ${this.name}>`;
  }

  equals(memory, address1, address2) {
    if (this._equals) {
      return this._equals(memory, address1, address2);
    }
    return address1 === address2;
  }
}

exports.ManagedMemory = ManagedMemory;
