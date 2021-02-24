const { performance } = require('perf_hooks');
const util = require('util');
const fs = require("fs");
const {ManagedMemory} = require(process.env.OCAML_TO_WASM_RT +  "/old_memory");
const readFile = util.promisify(fs.readFile);
const filename = process.argv[2];
const simple_name = filename.substring(filename.indexOf('/') + 1, filename.indexOf('.'));
const filesize = fs.statSync(filename).size;
// Handle all files (except memory for grain)
// TODO: Update to use memory manager JavaScript file
(async () => {
    var memory = new WebAssembly.Memory({initial: 2});
    var memoryManager = new ManagedMemory(memory);
    var rtimports = {
        jsRuntime: {
            malloc: memoryManager.malloc,
            stackOverflow: memoryManager.stackLimitExceeded,
            mem: memory,
            log: memoryManager.log
        }
    };

    var buffer = await readFile(process.env.OCAML_TO_WASM_RT + '/runtime.wasm');
    var module = await WebAssembly.compile(buffer);
    var runtime = await WebAssembly.instantiate(module, rtimports);
    memoryManager.setRuntime(runtime);

    var buffer = await readFile(filename);
    var module = await WebAssembly.compile(buffer);
    var instance = await WebAssembly.instantiate(module, {ocamlRuntime: runtime.exports});

   try {
       const t0 = performance.now();
       instance.exports["OCAML$MAIN"]();
       const millis = performance.now() - t0;
       // TODO: Include a way to select between the memory management methods
       // const remaining_memory = memoryManager.memory_used;
       const remaining_memory = memoryManager.maxMemory;
       //  const remaining_memory = runtime.exports["alloc"](0);
       console.log(simple_name, millis, remaining_memory, filesize);

       // TODO: Remove all this
       console.log(memoryManager.numScans);
       console.log("From large block:", memoryManager.fromLarge, "exact:", memoryManager.exact);
       console.log("pages allocated:", memory.buffer.byteLength >> 16);
       console.log("Total frees:", memoryManager.freesDone);
   } catch (e) {
       console.log("Crashed with error", e);
       console.log("mallocs total:", memoryManager.mallocsDone);
       console.log("max memory:", memoryManager.uview.byteLength);
       console.log(memoryManager.uview.length);
   }
})();