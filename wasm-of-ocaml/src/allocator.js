// Based on the allocator algorithm described in section 8.7 of K&R
// The C Programming Language 2nd Edition

// Wasm 'alloc' function receives the number of BYTES to allocate, not words (multiplied by 4 in compileWasm)

// Header size is 8 bytes, a 32-bit 'next' ptr and a 32-bit 'size' field
let headerSize = 8;
let logHeaderSize = 3; // rather than doing integer division as (x/headerSize)>>0, just shift >> 3

// allocator works in terms of header sizes, but malloc gets input/output in byte size/address
// NO! Needs to work in terms of 32-bit blocks (i.e. half headers)

// TODO: Make this less confusing
// store NEXT and SIZE both in terms of i32s (4 byte words)

let i32size = 4;
class Allocator {
    constructor(memoryManager) {
        // Can't store views as they only get refreshed in MemoryManger,
        // so have to access that way
        this.memoryManager = memoryManager;
        this.memory = memoryManager._memory;
        // TODO: How cautious do I need to be of signed vs unsigned int32s ?
        // TODO: When to actually call refresh?
        this.refresh = memoryManager._refreshViews;

        // TODO: Remove after, just for debugging
        this.memory_used = 0; // easier than trying to work out by inspecting memory manually

        // initialises first block the very first time
        this.memoryManager.uview[0] = 0; // only element of cyclic list points to itself
        this.memoryManager.uview[1] =  this.memoryManager.uview.byteLength >> 2;
        this.freep = 0; // point to first (and only) block

        this.growHeap = this.growHeap.bind(this);
        this.malloc = this.malloc.bind(this);
        this.free = this.free.bind(this);

    }

    // TODO: Gets the number of header units to grow memory by, not pages
    // also needs to update the free list so the last block has more space
    growHeap(units) {
        // convert to number of pages to allocate, rounded up
        let pages = (units*i32size + 65535)>>16;
        const ptr = this.memory.buffer.byteLength >> 2;
        // documented error value in
        // https://webassembly.github.io/spec/core/syntax/instructions.html#syntax-instr-memory
        if (this.memory.grow(pages) === -1){
            throw "Could not allocate memory"
        }
        this.refresh();
        this.setSize(ptr, pages << 14);
        // TODO: Disable when not debugging
        this.memory_used += pages << 16;
        this.free((ptr + 2)*i32size); // turns this into a block and merges it into adjacent cell if possible
        return this.freep;
    }

    // Remember uview is 32-bit cells, not bytes.
    // TODO: Since all cells are 8-byte aligned anyway, can do everything in terms of words?
    // i.e. Size field and ptr field are number of words, not bytes
    // TODO: Does compileWasm actively try to align anything? Probably pointless considering its done here
    getNext(ptr){
        return this.memoryManager.uview[ptr];
    }
    getSize(ptr){
        return this.memoryManager.uview[ptr + 1];
    }
    setNext(ptr, val){
        this.memoryManager.uview[ptr] = val;
    }
    setSize(ptr, val){
        this.memoryManager.uview[ptr + 1] = val;
    }

    // TODO: What happens if we try to allocate the only block in memory?
    // e.g. memory is 128 blocks and we ask for exactly that, then ask for the same again.
    // looks like the case when size is exact match assumes that next is not the current block.
    // K&R initialises the list to a static header with size 0. Can assume this will never be adjacent
    // to blocks provided by the OS, and has size 0, so free list will never be emptied.
    malloc(bytes){
  //      console.log("ALLC malloc");
        // round up to align block
        // *2 to put in terms of 32-bit words rather than 64-bit headers
        const units = (((bytes + headerSize - 1) >> 3) + 1)*2;
        this.memory_used += units * 4;
        // last block allocated exactly used up the last cell of the free list.
        // need to allocate more memory. Can't rely on 'free' since it expects freep to be defined
        if (this.freep == null){
            let pages = (units*i32size + 65535)>>16;
            const ptr = this.memory.buffer.byteLength >> 2;
            this.memory.grow(pages);
            // TODO: Throw an error if memory can't grow any more
            this.refresh();
            // only block so we know both of these
            this.setNext(ptr, ptr)
            this.setSize(ptr, pages << 14);
            this.freep = ptr;
        }
        // p is the block we are considering allocating in,
        // prev is its predecessor in the free list
        let prev = this.freep;
        for (let p = this.getNext(prev); ; prev = p, p = this.getNext(p)){
            let size = this.getSize(p);
            if (size >= units) {
                if (size === units) {
                    // exact fit, remove from list
                    if (this.getNext(p) === p){
                        this.freep = null; // list empty
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
       //         console.log("Allocating: ",i32size * (p+2), units * 4);
                return i32size * (p+2);

            }
            if (p === this.freep){
                // have searched the whole list, need to grow memory
                // returns freep after growHeap updates memory (freep may have changed)
                p = this.growHeap(units);
            }
        }
    }

    // takes a byte pointer
    free(ptr){
   //     console.log("ALLOC free");
        let blockPtr = (ptr>>2) - 2;
   //     console.log("Lower Freeing: ", ptr, this.getSize(blockPtr)*4);
        // TODO: Remove when not debugging
        this.memory_used -= this.getSize(blockPtr)*4;
        let p;
        // find the free block
        for (p = this.freep; blockPtr < p || blockPtr > this.getNext(p); p = this.getNext(p)){
            if (p >= this.getNext(p) && (blockPtr > p || blockPtr < this.getNext(p))){
                // block to free is at one end of list
                break;
            }
        }
        // join upper block
        // works even if memory has been extended since block was allocated, now pointed to by p
        if (blockPtr + this.getSize(blockPtr) === this.getNext(p)){
            this.setSize(blockPtr, this.getSize(blockPtr) + this.getSize(this.getNext(p)));
            this.setNext(blockPtr, this.getNext(this.getNext(p)));
        } else {
            this.setNext(blockPtr, this.getNext(p));
        }
        // join lower block
        if (p + this.getSize(p) === blockPtr){
            this.setSize(p, this.getSize(p) + this.getSize(blockPtr));
            this.setNext(p, this.getNext(blockPtr));
        } else {
            this.setNext(p, blockPtr);
        }
        // in case freep was the upper block, so freep would otherwise point to the middle of a block
        this.freep = p;
    }
}

exports.Allocator = Allocator;