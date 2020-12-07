const { performance } = require('perf_hooks');

let filename = process.argv[2];
const simple_name = filename.substring(filename.indexOf('/') + 1, filename.indexOf('.'));

// Taken from Grain executable grain/cli/bin/grain.js
// By running directly, get much closer to the actual overhead of the program, not calling node.js and other setup
// Not done for memory/filesize - unaffected by any setup process overheads
let run = require(process.env.GRAIN_STDLIB + "/../cli/bin/run.js");
const runArgs = {includeDirs : [], stdlib : process.env.GRAIN_STDLIB};
(async () => {
    const t0 = performance.now();
    await run(filename, runArgs);
    const millis = performance.now() - t0;
    console.log(simple_name, "time", millis);
})();



