// TODO: Decide about using the runtime wrapper or not. Adds extra work
//       Can improve efficiency by exporting globals directly rather than wrappers to them
//const rt = require("../src/ocaml.js");
const { performance } = require('perf_hooks');
const util = require('util');
const fs = require("fs");
const readFile = util.promisify(fs.readFile);

// each file tested in a new instance of node to try to avoid timing inaccuracies in JS GC running
// can assume node is running from benchmark directory and f is in out folder, already compiled
(async () => {
    var f = process.argv[2];
    const simple_name = f.substring(f.indexOf('/') + 1, f.indexOf('.'));

    var buffer = await readFile(f);
    var module = await WebAssembly.compile(buffer);
    var instance = await WebAssembly.instantiate(module);

    const t0 = performance.now();
    instance.exports.main();
    const millis = performance.now() - t0;
    var heap_top;
    if ("sbrk" in instance.exports){
        heap_top = instance.exports.sbrk();
    } else { // program doesn't require memory management
        heap_top = 0;
    }
    // filename  execution-time(ms) heap-size(bytes) file-size(bytes)
    console.log(simple_name, millis, heap_top, fs.statSync(f).size);
})();