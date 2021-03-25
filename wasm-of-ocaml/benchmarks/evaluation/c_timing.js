const { performance } = require('perf_hooks');
const fs = require("fs");
const util = require('util');
const readFile = util.promisify(fs.readFile);

const filename = process.argv[2];


let simple_name = filename.replace(/^.*[\\\/]/, '').replace(".c.wasm","");
if (simple_name.length < 16){
    simple_name = simple_name + "\t";
}

const filesize = fs.statSync(filename).size

let times = [];

let iters = 20;

var heap;
(async () => {
    for (let i = 0; i < iters; i++) {
        var buffer = await readFile(filename);
        var module = await WebAssembly.compile(buffer);
        var instance = await WebAssembly.instantiate(module);
        const t0 = performance.now();
        instance.exports.main();
        const t1 = performance.now();
        times.push(t1 - t0);
        // sbrk(0) defined to return the current heap size.
        // 0 if memory management wasn't needed by program
        heap = "sbrk" in instance.exports ? instance.exports.sbrk() : 0;
    }
    const n = times.length;
    const t_mean = times.reduce((a, b) => a + b) / n;
    const t_std = 2 * Math.sqrt(times.map(x => Math.pow(x - t_mean, 2))
        .reduce((a, b) => a + b) / (n*(n - 1))); // n - 1 for sample variance

    console.log(simple_name + "\t" + t_mean.toFixed(3) + "\t" + t_std.toFixed(3)
        + "\t" + heap + "\t" + filesize);
})();