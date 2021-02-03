const { performance } = require('perf_hooks');
const util = require('util');
const fs = require("fs");
const {ManagedMemory} = require(process.env.OCAML_TO_WASM_RT +  "/memory");
const readFile = util.promisify(fs.readFile);
const filename = process.argv[2];
const simple_name = filename.substring(filename.indexOf('/') + 1, filename.indexOf('.'));
const filesize = fs.statSync(filename).size;
// Handle all files (except memory for grain)
// TODO: Update to use memory manager JavaScript file
(async () => {
    if (filename.endsWith(".js")){
        gc(); // measure baseline memory, need to run node with --expose-gc
        const mem0 = process.memoryUsage().heapUsed;
        const t0 = performance.now();
        require("./"+filename); // executes the program
        const millis = performance.now() - t0;
        const mem1 = process.memoryUsage().heapUsed;
        console.log(simple_name, millis, mem1-mem0, filesize);
    } else if (filename.endsWith(".gr.wasm")){
        // uses required part of grain/cli/bin/grain.js
        let run = require(process.env.GRAIN_STDLIB + "/../cli/bin/run.js");
        const runArgs = {includeDirs : [], stdlib : process.env.GRAIN_STDLIB};
        const t0 = performance.now();
        await run(filename, runArgs);
        const millis = performance.now() - t0;
        console.log(simple_name, "time", millis);
    } else if (filename.endsWith(".c.wasm")){
        var buffer = await readFile(filename);
        var module = await WebAssembly.compile(buffer);
        var instance = await WebAssembly.instantiate(module);
        const t0 = performance.now();
        instance.exports.main();
        const millis = performance.now() - t0;
        // sbrk(0) defined to return the current heap size. 0 if memory management wasn't needed by program
        var heap_top = "sbrk" in instance.exports ? instance.exports.sbrk() : 0;
        console.log(simple_name, millis, heap_top, filesize);
    } else {
        var memory = new WebAssembly.Memory({ initial: 2 });
        var memoryManager = new ManagedMemory(memory);
        var rtimports = {jsRuntime: {malloc : memoryManager.malloc,
                stackOverflow : memoryManager.stackLimitExceeded,
                mem : memory}};

        var buffer = await readFile(process.env.OCAML_TO_WASM_RT + '/runtime.wasm');
        var module = await WebAssembly.compile(buffer);
        var runtime = await WebAssembly.instantiate(module, rtimports);
        memoryManager.setRuntime(runtime);

        var buffer = await readFile(filename);
        var module = await WebAssembly.compile(buffer);
        var instance = await WebAssembly.instantiate(module, {ocamlRuntime: runtime.exports});
        const t0 = performance.now();
        instance.exports["OCAML$MAIN"]();
        const millis = performance.now() - t0;
        // TODO: Include a way to select between the memory management methods
        const remaining_memory = memoryManager.memory_used;
      //  const remaining_memory = runtime.exports["alloc"](0);
        console.log(simple_name, millis, remaining_memory, filesize);
    }
})();