const util = require('util');
const fs = require("fs");
const readFile = util.promisify(fs.readFile);

let fileList = __dirname + "/../samples/results.txt";

const rt = require(process.env.OCAML_TO_WASM_RT + "/ocaml.js");


(async () => {
    var files = (await readFile(fileList, "utf8")).split("\n").filter(line => !(line.startsWith("//") || line == ""));
    for (const f of files){
        var line = f.trim().split(" ");
        var output = line.slice(1);
        var filename = line[0];
        var basename = filename.split("/");
        basename = basename[basename.length - 1];
        try {

            var instance = await rt.instantiate(__dirname + "/../samples/out/" + basename + ".wasm");

            if (line[line.length - 1] == "!"){
                try {
                    instance.setup();
                } catch (err) {
                    if (err instanceof WebAssembly.RuntimeError && err.message == "unreachable"){
                        console.log('\x1b[92m%s\x1b[0m', filename + " passed");
                        continue;
                    } else {
                        console.log('\x1b[91m%s\x1b[0m', filename + " failed test: Exception '" + err.message + "' occured");
                        continue;
                    }
                }
                console.log('\x1b[91m%s\x1b[0m', filename + " failed test: Did not trap");
            } else {
                try {
                    instance.setup();
                } catch (err) {
                    if (err instanceof WebAssembly.RuntimeError){
                        console.log('\x1b[91m%s\x1b[0m', filename + " failed test: Trap occured");
                        continue;
                    } else{
                        console.log('\x1b[91m%s\x1b[0m', filename + " error: " + err);
                        continue;
                    }
                }
                var passed = true;
                for (const check of output){
                    var id = check.split("=")[0];
                    var val = check.split("=")[1];
                    var actual = instance[id]();
                    if (actual != val){
                        console.log('\x1b[91m%s\x1b[0m', filename + " failed test: Variable " + id + " was " + actual + ", expected " + val);
                        passed = false;
                        break;
                    }
                }
                if (passed){
                    console.log('\x1b[92m%s\x1b[0m', filename + " passed");
                }
            }
        } catch(err) {
            if (err instanceof TypeError){
                console.log("\x1b[91m%s\x1b[0m", "Check results entry for " + filename + ". Identifier not recognised as an export.")
            } else {
                console.log('\x1b[91m%s\x1b[0m', filename + " failed test: Exception occured");
                throw err;
            }
        }
    }
})();
