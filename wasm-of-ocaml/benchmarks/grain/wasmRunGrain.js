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

const t0 = performance.now();
// TODO: Slightly unfair? This one includes more overhead costs like runtime setup
// Or so insignificant it doesn't actually matter?
// TODO: Done this way, no way to read out heap top
//       COULD MODIFY `GRAIN RUN` TO TELL ME THAT?
execSync("grain run " + filename);
// memory to be measured separately

const millis = performance.now() - t0;
console.log(simple_name, millis, filesize);

