// Uses latest optimised version of memory.js to collect average + std dev time for 10 iters
// Uses same memory with debugging operations enabled to track peak memory usage
// output: filename  average_time  std_dev_time  heap_usage

const util = require('util');
const fs = require("fs");
const { performance } = require('perf_hooks');
const readFile = util.promisify(fs.readFile);
const exec = util.promisify(require('child_process').exec);


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
      let mem_usage;
      for (let i = 0; i < iters; i++) {
          var buffer = await readFile(process.env.OCAML_TO_WASM_RT + '/no_gc_runtime.wasm');
          var module = await WebAssembly.compile(buffer);
          var runtime = await WebAssembly.instantiate(module);

          imports = {ocamlRuntime: runtime.exports};
          var buffer = await readFile(filename);
          var module = await WebAssembly.compile(buffer);
          var instance = await WebAssembly.instantiate(module, imports);


          const t0 = performance.now();
          instance.exports["OCAML$MAIN"]();
          const millis = performance.now() - t0;

          mem_usage = runtime.exports.malloc();

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