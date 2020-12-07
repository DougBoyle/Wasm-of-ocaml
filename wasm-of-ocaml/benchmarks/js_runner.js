const { performance } = require('perf_hooks');
const fs = require("fs");
const util = require('util');
const execSync = util.promisify(require('child_process').execSync);

let filename = process.argv[2];
const simple_name = filename.substring(filename.indexOf('/') + 1, filename.indexOf('.'));
const filesize = fs.statSync(filename).size

// See grain --help
// `grain run <f>` can be used to run a file with the Grain runtime, use that for timing
// as it ensures any loading/cleanup gets included in cost

gc();
//console.log(process.memoryUsage());
const mem0 = process.memoryUsage().heapUsed;
const t0 = performance.now();
// TODO: Is this fair in terms of overhead incurred? Needs to be cost when loaded
// Not really a full example of a test? Proper example would interact with the program

//execSync("node " + filename);
require("./"+filename); // Appears to have lower setup overhead than execSync, otherwise fairly similar results

// memory to be measured separately
//console.log(process.memoryUsage());
const millis = performance.now() - t0;
const mem1 = process.memoryUsage().heapUsed;
console.log(simple_name, millis, mem1-mem0, filesize);

/*
process.memoryUsage() =
{
  rss: 18268160,        Resident Set Size = total memory allocated to process execution
  heapTotal: 6045696,   Total size of allocated heap
  heapUsed: 3287968,    Actual memory used during execution
  external: 1125329,    V8 external memory - probably ignore
  arrayBuffers: 9398
}

From first inspection, heapUsed looks like the best choice for now.
Run with --expose-gc to be able to call gc(), which hopefully helps to ensure a more accurate baseline.
 */