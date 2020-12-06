const fs = require("fs");
const util = require('util');
const execSync = util.promisify(require('child_process').execSync);

let filename = process.argv[2];
const simple_name = filename.substring(filename.indexOf('/') + 1, filename.indexOf('.'));

// First need to change to `yarn runtime build:dev`
// Process will output summary statistics, need to store to a file.
// Looks like this:
//==== MEMORY TRACE INFO ===
//Max used span size: 1044776
//Objects allocated:  19066
//Objects freed:      3
//Objects leaked:     19063
//==== END MEMORY TRACE INFO ===
execSync("grain run " + filename);

