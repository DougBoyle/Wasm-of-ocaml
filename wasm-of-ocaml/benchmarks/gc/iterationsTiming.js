const util = require('util');
const fs = require("fs");
const { performance } = require('perf_hooks');
// TODO: Switch this to select the implementation to use
const {ManagedMemory} = require(process.env.OCAML_TO_WASM_RT +  "/memory.js");
const readFile = util.promisify(fs.readFile);
const exec = util.promisify(require('child_process').exec);

// process.argv is a list of the command line arguments

//const check_mem = require("./checkmemory");

let iters = 25;

if (process.argv.length > 3){
(async () => {
  let times = [];

  for (let i = 0; i < iters; i++) {
      const memory = new WebAssembly.Memory({initial: 3});
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
      var buffer = await readFile(process.argv[2]);
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
    const std = 2*Math.sqrt(times.map(x => Math.pow(x - mean, 2))
        .reduce((a, b) => a + b) / (n*(n - 1))); // n - 1 for sample variance
    console.log(process.argv[3] + "\t" + mean.toFixed(3) + "\t" + std.toFixed(3));
})();
} else {
(async () => {console.log("File and number of iterations required");}
)();
}