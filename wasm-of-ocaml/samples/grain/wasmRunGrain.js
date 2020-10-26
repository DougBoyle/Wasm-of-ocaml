//const fetch = require("node-fetch");
var path = require("path");
var runtime = require("./runtime/dist/grain-runtime.js");

let filename = "./arith.wasm";

let basePath = path.dirname(filename);

let includeDirs = [basePath, "./stdlib"];

let locator = runtime.defaultFileLocator(includeDirs);

let GrainRunner = runtime.buildGrainRunner(locator, {limitMemory: -1});

GrainRunner.runFile(filename);


/*
const memory = new WebAssembly.Memory({ initial: 1 });

const util = require('util');
const fs = require("fs");

const readFile = util.promisify(fs.readFile);

var rt = require("D:/Programming/Project/Wasm-of-ocaml/wasm-of-ocaml/samples/grain/runtime/dist/grain-runtime.js");

var runner = rt.buildGrainRunner(rt.defaultFileLocator);

var allImports = runner.imports;

(async () => {
  var buffer = await readFile('D:/Programming/Project/Wasm-of-ocaml/wasm-of-ocaml/samples/grain/stdlib/stdlib-external/equal.wasm');
  var module = await WebAssembly.compile(buffer);
  var instance = await WebAssembly.instantiate(module, allImports);
 // const response = await fetch('http://localhost:3000/wasm-files/fib.wasm');
 // const bytes = await response.arrayBuffer();
 // const { instance } = await WebAssembly.instantiate(bytes); //, {
  //  env: { log, memory }
//  });
    console.log("complete");
//  console.log(instance.exports.main(10));
})();*/