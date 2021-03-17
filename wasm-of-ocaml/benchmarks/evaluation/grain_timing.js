const { performance } = require('perf_hooks');
const fs = require("fs");
const util = require('util');
const readFile = util.promisify(fs.readFile);

var exec = require('child_process').exec;
function execute(command, callback){ // memory info gets logged to stderr
    exec(command, function(error, stdout, stderr){ callback(stderr); });
};


const filename = process.argv[2];


let simple_name = filename.replace(/^.*[\\\/]/, '').replace(".gr.wasm","");
if (simple_name.length < 16){
    simple_name = simple_name + "\t";
}

const filesize = fs.statSync(filename).size

let times = [];

// TODO: Is not having 'warm up' period fair? Should do the same for Js, Grain, Wasm
let iters = 10;

// just does timings
// want option to enable doing memory instead
// also care about disabling GC?

//var heap;
if (process.argv.length < 4){
    console.log("Must pass both filename and mode")
}
(async () => {
    if (process.argv[3] === "0") {
        let run = require(process.env.GRAIN_STDLIB + "/../cli/bin/run.js");
        const runArgs = {includeDirs: [], stdlib: process.env.GRAIN_STDLIB};
        for (let i = 0; i < iters; i++) {
            const t0 = performance.now();
            await run(filename, runArgs);
            const t1 = performance.now();

            times.push(t1 - t0);
        }

        const n = times.length;
        const t_mean = times.reduce((a, b) => a + b) / n;
        const t_std = Math.sqrt(times.map(x => Math.pow(x - t_mean, 2))
            .reduce((a, b) => a + b) / (n - 1)); // n - 1 for sample variance

        console.log(simple_name + "\t" + t_mean.toFixed(3) + "\t" + t_std.toFixed(3)
            + "\t" + filesize);
    } else {
        // Requires grain runtime compiled in dev mode so heap trace output produced
        execute("grain run " + filename, trace => {
            console.log(simple_name,  trace.split("\n")[1].split(": ")[1]);
        });
    }
})();