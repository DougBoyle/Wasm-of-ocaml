let mem = new WebAssembly.Memory({initial : 2});
const manager = require("./src/memory.js");
const allc = require("./src/allocator.js");
let memmanager = new manager.ManagedMemory(mem);
let allocator = new allc.Allocator(memmanager);

let size = memmanager.uview.byteLength;
console.log(size);
let block1 = allocator.malloc(50);
let block2 = allocator.malloc(80);
let block3 = allocator.malloc(40);
console.log(block1, block2, block3);
// REMEMBER: BYTES VS WORDS AS POINTERS
console.log(memmanager.uview.slice((block3>>2) - 2));
allocator.refresh();
console.log(memmanager.uview.slice((block3>>2) - 2));
allocator.free(block2);
console.log(memmanager.uview.slice((block3>>2) - 2));