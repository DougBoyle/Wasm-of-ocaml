//const fetch = require("node-fetch");
var path = require("path");

let filename = "../unit_tests/add.wasm";

let basePath = path.dirname(filename);


const memory = new WebAssembly.Memory({ initial: 1 });

const util = require('util');
const fs = require("fs");
const readFile = util.promisify(fs.readFile);


(async () => {
  var buffer = await readFile('./runtime.wasm');
  var module = await WebAssembly.compile(buffer);
  var instance = await WebAssembly.instantiate(module);
  
  imports = {ocamlRuntime: instance.exports};
  
  console.log(imports);
  
  var buffer = await readFile(filename);
  var module = await WebAssembly.compile(buffer);
  var instance = await WebAssembly.instantiate(module, imports);
 // const response = await fetch('http://localhost:3000/wasm-files/fib.wasm');
 // const bytes = await response.arrayBuffer();
 // const { instance } = await WebAssembly.instantiate(bytes); //, {
  //  env: { log, memory }
//  });
    console.log("complete");
	console.log(instance.exports);
	
	
//  console.log(instance.exports.main(10));
})();