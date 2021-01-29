// TODO: Decide about using the runtime wrapper or not. Adds extra work
//       Can improve efficiency by exporting globals directly rather than wrappers to them
// TODO: Update benchmarks to use memory manager
//const rt = require(process.env.OCAML_TO_WASM_RT +  "/ocaml.js");
const { performance } = require('perf_hooks');
const util = require('util');
const fs = require("fs");
const readFile = util.promisify(fs.readFile);

// each file tested in a new instance of node to try to avoid timing inaccuracies in JS GC running
// can assume node is running from benchmark directory and f is in out folder, already compiled
(async () => {
    var f = process.argv[2];
    const simple_name = f.substring(f.indexOf('/') + 1, f.indexOf('.'));

    var buffer = await readFile(process.env.OCAML_TO_WASM_RT + '/runtime.wasm');
    var module = await WebAssembly.compile(buffer);
    var runtime = await WebAssembly.instantiate(module);

    var imports = {ocamlRuntime: runtime.exports};
    var buffer = await readFile(f);
    var module = await WebAssembly.compile(buffer);
    var instance = await WebAssembly.instantiate(module, imports);

    const t0 = performance.now();
    instance.exports["OCAML$MAIN"]();
    const millis = performance.now() - t0;
    // alloc(0) just returns the next free address in memory, measures how much used by program
    const heap_top = runtime.exports["alloc"](0);
    console.log(simple_name, millis, heap_top, fs.statSync(f).size);
})();