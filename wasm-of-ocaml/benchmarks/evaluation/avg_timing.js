// Uses latest optimised version of memory.js to collect average + std dev time for 10 iters
// Uses same memory with debugging operations enabled to track peak memory usage
// output: filename  average_time  std_dev_time  heap_usage

const util = require('util');
const fs = require("fs");
const { performance } = require('perf_hooks');
// TODO: Switch this to select the implementation to use
const {ManagedMemory} = require(process.env.OCAML_TO_WASM_RT +  "/memory.js");
const traced_memory = require(process.env.OCAML_TO_WASM_RT +  "/memory_trace.js");
const readFile = util.promisify(fs.readFile);
const exec = util.promisify(require('child_process').exec);

// process.argv is a list of the command line arguments

//const check_mem = require("./checkmemory");

let iters = 15;

if (process.argv.length == 3){
(async () => {
  let times = [];

  const filename = process.argv[2];
  let simple_name = filename.replace(/^.*[\\\/]/, '').replace(".wasm","");
  if (simple_name.length < 16){
      simple_name = simple_name + "\t";
  }

  try {
      for (let i = 0; i < iters; i++) {
          const memory = new WebAssembly.Memory({initial: 2});
          var memoryManager = new ManagedMemory(memory);
          var rtimports = {
              jsRuntime: {
                  malloc: memoryManager.malloc,
                  stackOverflow: memoryManager.stackLimitExceeded,
                  mem: memory
              }
          };
          var buffer = await readFile(process.env.OCAML_TO_WASM_RT + '/runtime.wasm');
          var module = await WebAssembly.compile(buffer);
          var instance = await WebAssembly.instantiate(module, rtimports);
          memoryManager.setRuntime(instance);

          imports = {ocamlRuntime: instance.exports};
          var buffer = await readFile(filename);
          var module = await WebAssembly.compile(buffer);
          var instance = await WebAssembly.instantiate(module, imports);


          const t0 = performance.now();
          instance.exports["OCAML$MAIN"]();
          const millis = performance.now() - t0;

          // first couple of runs often about 2x slower,
          // so run several times and only record times once stable.
          // Unclear what causes this.
          if (i >= 5) {
              times.push(millis);
          }
      }

      const n = times.length;
      const mean = times.reduce((a, b) => a + b) / n;
      const std = Math.sqrt(times.map(x => Math.pow(x - mean, 2))
          .reduce((a, b) => a + b) / (n - 1)); // n - 1 for sample variance

      // Also measure memory usage using the version of memory that tracks this
      const memory = new WebAssembly.Memory({initial: 2});
      var memoryManager = new traced_memory.ManagedMemory(memory);
      var rtimports = {
          jsRuntime: {
              malloc: memoryManager.malloc,
              stackOverflow: memoryManager.stackLimitExceeded,
              mem: memory
          }
      };
      var buffer = await readFile(process.env.OCAML_TO_WASM_RT + '/runtime.wasm');
      var module = await WebAssembly.compile(buffer);
      var instance = await WebAssembly.instantiate(module, rtimports);
      memoryManager.setRuntime(instance);

      imports = {ocamlRuntime: instance.exports};
      var buffer = await readFile(filename);
      var module = await WebAssembly.compile(buffer);
      var instance = await WebAssembly.instantiate(module, imports);

      instance.exports["OCAML$MAIN"]();

      const mem_usage = memoryManager.maxMemory;
      const filesize = fs.statSync(filename).size;

      console.log(simple_name + "\t" + mean.toFixed(3) + "\t" + std.toFixed(3)
        + "\t" + mem_usage + "\t" + filesize);

  } catch (e) {
      console.log(simple_name + "\t" + "Error: " + e);
  }
})();
} else {
(async () => {console.log("Just expect file name");}
)();
}