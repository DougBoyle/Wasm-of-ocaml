let mem = new WebAssembly.Memory({initial : 2});
const manager = require(process.env.OCAML_TO_WASM_RT + "/memory.js");
let memmanager = new manager.ManagedMemory(mem);

// Not filling in values/stack so want to prevent GC from doing anything on page overflow
memmanager.doGC = () => {};


// from ptr, need to look at uview[(ptr >> 2) - 2] to get memory header
// (-4 to get trailer of previous block)
// not interested in actually populating stack/values in memory blocks,
// assume GC correct, just check blocks look correct too.

/*
Things to handle:
- Behaviour when last block allocated
- Behaviour when freeing if 0, 1 or 2 blocks other free blocks exist
- Size (at both ends), next and prev pointers all set correctly
- Behaviour after lone free, merge above, merge below, merge both
*/

// TODO: Free now takes a block pointer (i.e. points to header in terms of words, shift 2, minus 2)

const stack_limit = 16384;

console.log(memmanager.uview.slice(stack_limit, stack_limit + 8));

let size = memmanager.uview.byteLength;
console.log(size);
let block1 = memmanager.malloc(50);
let block2 = memmanager.malloc(80);
let block3 = memmanager.malloc(40);
console.log(block1, block2, block3);
// REMEMBER: BYTES VS WORDS AS POINTERS
console.log(memmanager.uview.slice((block3>>2) - 4));
console.log(memmanager.uview.slice(stack_limit, stack_limit + 8));
console.log(memmanager.freep);


memmanager.free((block2>>2) - 2);
console.log("\n\nblock 2 freed\n");
console.log(memmanager.freep);
console.log(memmanager.uview.slice(stack_limit, stack_limit + 8));
console.log(memmanager.uview.slice((block3>>2) - 4));

memmanager.free((block3 >> 2) - 2);
console.log("\n\nblock 3 freed\n");
console.log(memmanager.freep);
console.log("Printing starts at index:", (block3 >> 2) - 4);
console.log(memmanager.uview.slice(stack_limit, stack_limit + 8));
console.log(memmanager.uview.slice((block3>>2) - 4));


let block4 = memmanager.malloc(132000);
console.log(block4);
console.log(memmanager.uview.slice((block3>>2) - 4));
console.log(memmanager.uview.slice(stack_limit, stack_limit + 8));

