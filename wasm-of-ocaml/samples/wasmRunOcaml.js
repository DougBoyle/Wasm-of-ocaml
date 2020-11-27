//const fetch = require("node-fetch");
var path = require("path");

//let filename = "../unit_tests/add.wasm";
let filename = "../samples/arith.wasm";

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
  
  var buffer = await readFile(filename);
  var module = await WebAssembly.compile(buffer);
  var instance = await WebAssembly.instantiate(module, imports);
 // const response = await fetch('http://localhost:3000/wasm-files/fib.wasm');
 // const bytes = await response.arrayBuffer();
 // const { instance } = await WebAssembly.instantiate(bytes); //, {
  //  env: { log, memory }
//  });
	instance.exports["OCAML$MAIN"]();
	console.log(instance.exports.x());
	console.log(instance.exports.y());
//  console.log(instance.exports.main(10));
})();