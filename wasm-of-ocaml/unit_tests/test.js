//const fetch = require("node-fetch");
var path = require("path");
const util = require('util');
const fs = require("fs");
const readFile = util.promisify(fs.readFile);
const exec = util.promisify(require('child_process').exec);

let fileList = "./results.txt";

// process.argv is a list of the command line arguments

//const memory = new WebAssembly.Memory({ initial: 1 });

if (process.argv.length > 2){
(async () => {
  var buffer = await readFile('../samples/runtime.wasm');
  var module = await WebAssembly.compile(buffer);
  var instance = await WebAssembly.instantiate(module);
  
  imports = {ocamlRuntime: instance.exports};
  
  var buffer = await readFile(process.argv[2]);
  var module = await WebAssembly.compile(buffer);
  var instance = await WebAssembly.instantiate(module, imports);
 // const response = await fetch('http://localhost:3000/wasm-files/fib.wasm');
 // const bytes = await response.arrayBuffer();
 // const { instance } = await WebAssembly.instantiate(bytes); //, {
  //  env: { log, memory }
//  });
  try {
	instance.exports["OCAML$MAIN"]();
  } catch (err) {
	  if (err instanceof WebAssembly.RuntimeError){
		console.log("Trapped");  
	  } else{
		 console.log("ERROR: Some other error occured during execution");
		 throw(err);
	  }
  }
//	console.log(instance.exports.x());
//	console.log(instance.exports.y());
//  console.log(instance.exports.main(10));
})();
} else {
(async () => {
  var files = (await readFile(fileList, "utf8")).split("\n").filter(line => !(line.startsWith("//") || line == ""));
  for (const f of files){
	  var line = f.trim().split(" ");
	  var filename = line[0];
	  try {
		  var output = line.slice(1);
		  if (line[line.length - 1] == "!"){
			  await exec("../main.byte " + filename + ".ml");
			  var buffer = await readFile('../samples/runtime.wasm');
			  var module = await WebAssembly.compile(buffer);
			  var instance = await WebAssembly.instantiate(module);
			  
			  var imports = {ocamlRuntime: instance.exports};
			  var buffer = await readFile(filename + ".wasm");
			  var module = await WebAssembly.compile(buffer);
			  // TODO: Add extra try/catch wrapper to catch the trap we are expecting to see?
			  var instance = await WebAssembly.instantiate(module, imports);
			  try {
				instance.exports["OCAML$MAIN"]();
			  } catch (err) {
				  if (err instanceof WebAssembly.RuntimeError){
					console.log('\x1b[92m%s\x1b[0m', filename + " passed");
					continue;
				  } else{
					 console.log("ERROR: Some other error occured during execution");
					 throw(err);
				  }
			  }
			  console.log('\x1b[91m%s\x1b[0m', filename + " failed test: Did not trap");
		  } else {
			  await exec("../main.byte " + filename + ".ml");
			  var buffer = await readFile('../samples/runtime.wasm');
			  var module = await WebAssembly.compile(buffer);
			  var instance = await WebAssembly.instantiate(module);
			  
			  var imports = {ocamlRuntime: instance.exports};
			  var buffer = await readFile(filename + ".wasm");
			  var module = await WebAssembly.compile(buffer);
			  var instance = await WebAssembly.instantiate(module, imports);
			  instance.exports["OCAML$MAIN"]();
			  var passed = true;
			  for (const check of output){
				  var id = check.split("=")[0];
				  var val = check.split("=")[1];
				  var actual = instance.exports[id]();
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
		  console.log('\x1b[91m%s\x1b[0m', filename + " failed test: Exception occured");
	  }
  }
 /* 
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
//  console.log(instance.exports.main(10));*/
})();
}