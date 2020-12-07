const fs = require("fs");

let filename = process.argv[2];
const simple_name = filename.substring(filename.indexOf('/') + 1, filename.indexOf('.'));

var exec = require('child_process').exec;
function execute(command, callback){ // memory info gets logged to stderr
    exec(command, function(error, stdout, stderr){ callback(stderr); });
};

// First need to change to `yarn runtime build:dev`
// Process will output summary statistics, need to store to a file.
// Looks like this:
//==== MEMORY TRACE INFO ===
//Max used span size: 1044776
//Objects allocated:  19066
//Objects freed:      3
//Objects leaked:     19063
//==== END MEMORY TRACE INFO ===
execute("grain run " + filename, trace => {
    // print mem to distinguish from the timing/file size cases
    // already doing separately so also print file size here
    console.log(simple_name, "mem:filesize", trace.split("\n")[1].split(": ")[1], fs.statSync(filename).size);
});

