let mem = new WebAssembly.Memory({initial : 2});
const manager = require("./src/memory.js");
let memmanager = new manager.ManagedMemory(mem);

/*
Testing low level allocator
let allocator = memmanager.allocator;

console.log(memmanager.uview.slice(0, 8));

let size = memmanager.uview.byteLength;
console.log(size);
let block1 = allocator.malloc(50);
let block2 = allocator.malloc(80);
let block3 = allocator.malloc(40);
console.log(block1, block2, block3);
// REMEMBER: BYTES VS WORDS AS POINTERS
console.log(memmanager.uview.slice((block3>>2) - 2));
console.log(memmanager.uview.slice(0, 8));

console.log(allocator.freep);
allocator.free(block2);
console.log(memmanager.uview.slice((block3>>2) - 2));
console.log(memmanager.uview.slice(0, 8));
allocator.free(block3);
console.log(memmanager.uview.slice((block3>>2) - 2));
console.log(memmanager.uview.slice(0, 8));

let block4 = allocator.malloc(132000);
console.log(block4);
console.log(memmanager.uview.slice((block3>>2) - 2));
console.log(memmanager.uview.slice(0, 8));
*/
let block1 = memmanager.malloc(12);
memmanager.uview[(block1>>2) + 1] = 1; // contains 1 element
memmanager.uview[(block1>>2) + 2] = memmanager.incRef(20); // integer 10
let ptr1 = block1 ^ 1

console.log("first block made");
console.log(memmanager.uview.slice((block1>>2) - 4));

let block2 = memmanager.malloc(16);
memmanager.uview[(block2>>2) + 1] = 2; // contains 2 elements
memmanager.uview[(block2>>2) + 2] = memmanager.incRef(ptr1);
memmanager.uview[(block2>>2) + 3] = memmanager.incRef(ptr1);
let ptr2 = block2 ^ 1

console.log("second block made");
console.log(memmanager.uview.slice((block1>>2) - 4));

// first block goes out of scope
memmanager.decRef(ptr1);

console.log("first block out of scope");
console.log(memmanager.uview.slice((block1>>2) - 4));

// element gets overwritten
// should technically do incref then decref, but value being put in is an int so it doesn't matter
memmanager.decRef(memmanager.uview[(block2>>2) + 2]);
memmanager.uview[(block2>>2) + 2] = memmanager.incRef(40);

console.log("field overwritten in 2nd block");
console.log(memmanager.uview.slice((block2>>2) - 4));


// block2 goes out of scope, both blocks deallocated
console.log("Second block goes out of scope:");
memmanager.decRef(ptr2);
console.log("second block deallocated");
console.log(memmanager.uview.slice((block2>>2) - 4));

console.log("end, should all be deallocated so free list is 1 big block");
console.log(memmanager.uview.slice(0, 8));
console.log("size in words:", memmanager.uview.byteLength>>2);
console.log(memmanager.uview.slice(32752));