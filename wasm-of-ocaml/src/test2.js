const util = require('util');
const fs = require("fs");
const jsruntime = require("./runtime.js");
const readFile = util.promisify(fs.readFile);
const exec = util.promisify(require('child_process').exec);

// process.argv is a list of the command line arguments

//const memory = new WebAssembly.Memory({ initial: 1 });

if (process.argv.length > 2){
(async () => {
  var buffer = await readFile(__dirname + '/../samples/runtime.wasm');
  var module = await WebAssembly.compile(buffer);
  var instance = await WebAssembly.instantiate(module);

  var mem = instance.exports.mem;

  imports = {ocamlRuntime: instance.exports};
  try {
	  var buffer = await readFile(process.argv[2]);
	  var module = await WebAssembly.compile(buffer);
	  var instance = await WebAssembly.instantiate(module, imports);
	  instance.exports["OCAML$MAIN"]();

	  console.log(instance.exports);
	  var f = jsruntime.get_global(instance, "phi");
	  console.log(jsruntime.decode_int(jsruntime.call_closure(instance, mem, f, [jsruntime.encode_int(10)])));

  } catch (err) {
    console.log(err);
  }
})();
}