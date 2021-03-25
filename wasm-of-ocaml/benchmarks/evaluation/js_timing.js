const { performance } = require('perf_hooks');
const fs = require("fs");

const filename = process.argv[2];


let simple_name = filename.replace(/^.*[\\\/]/, '').replace(".js","");
if (simple_name.length < 16){
    simple_name = simple_name + "\t";
}

const filesize = fs.statSync(filename).size

let times = [];
let heaps = []; // collect average statistics of both, since memoryUsage not exact

let iters = 20;

for (let i = 0; i < iters; i++) {
    gc();
    const mem0 = process.memoryUsage().heapUsed;
    const t0 = performance.now();
    require("./" + filename);
    const t1 = performance.now();
    const mem1 = process.memoryUsage().heapUsed;
    if (iters >= 4){
        times.push(t1-t0);
        heaps.push(mem1-mem0);
    }
    // allows it to be 'reloaded', repeating evaluation
    delete require.cache[require.resolve("./" + filename)]
}

const n = times.length;
const t_mean = times.reduce((a, b) => a + b) / n;
const t_std = 2* Math.sqrt(times.map(x => Math.pow(x - t_mean, 2))
    .reduce((a, b) => a + b) / (n*(n - 1))); // n - 1 for sample variance

const h_mean = heaps.reduce((a, b) => a + b) / n;
const h_std = 2*Math.sqrt(heaps.map(x => Math.pow(x - h_mean, 2))
    .reduce((a, b) => a + b) / (n*(n - 1)));

console.log(simple_name + "\t" + t_mean.toFixed(3) + "\t" + t_std.toFixed(3)
    + "\t" + h_mean.toFixed(3) + "\t" + h_std.toFixed(3) + "\t" + filesize);