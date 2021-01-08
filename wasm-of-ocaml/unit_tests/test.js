const util = require('util');
const fs = require("fs");
const readFile = util.promisify(fs.readFile);
const exec = util.promisify(require('child_process').exec);

let fileList = __dirname + "/results.txt";

// process.argv is a list of the command line arguments

//const memory = new WebAssembly.Memory({ initial: 1 });

if (process.argv.length > 2){
(async () => {
  var buffer = await readFile(__dirname + '/../samples/runtime.wasm');
  var module = await WebAssembly.compile(buffer);
  var instance = await WebAssembly.instantiate(module);
  
  imports = {ocamlRuntime: instance.exports};
  try {
	  var buffer = await readFile(process.argv[2]);
	  var module = await WebAssembly.compile(buffer);
	  var instance = await WebAssembly.instantiate(module, imports);
	  instance.exports["OCAML$MAIN"]();
	  console.log(instance.exports);
	 // console.log(imports.ocamlRuntime.alloc(0));
  } catch (err) {
    console.log(err);
    console.log(err.message);
  }
})();
} else {
(async () => {
  var files = (await readFile(fileList, "utf8")).split("\n").filter(line => !(line.startsWith("//") || line == ""));
  for (const f of files){
	  var line = f.trim().split(" ");
	  var filename = line[0];
	  var basename = filename.split("/");
	  basename = basename[basename.length - 1];
	  try {
		  await exec(__dirname + "/../main.byte -d " + __dirname + "/out " + __dirname + "/" + filename + ".ml");
		  var output = line.slice(1);
		  if (line[line.length - 1] == "!"){
			  var buffer = await readFile(__dirname + '/../samples/runtime.wasm');
			  var module = await WebAssembly.compile(buffer);
			  var instance = await WebAssembly.instantiate(module);
			  
			  var imports = {ocamlRuntime: instance.exports};
			  var buffer = await readFile(__dirname + "/out/" + basename + ".wasm");
			  var module = await WebAssembly.compile(buffer);
			  var instance = await WebAssembly.instantiate(module, imports);
			  try {
				instance.exports["OCAML$MAIN"]();
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
			  var buffer = await readFile(__dirname + '/../samples/runtime.wasm');
			  var module = await WebAssembly.compile(buffer);
			  var instance = await WebAssembly.instantiate(module);
			  
			  var imports = {ocamlRuntime: instance.exports};
			  var buffer = await readFile(__dirname + "/out/" + basename + ".wasm");
			  var module = await WebAssembly.compile(buffer);
			  var instance = await WebAssembly.instantiate(module, imports);
			  try {
				instance.exports["OCAML$MAIN"]();
			  } catch (err) {
				  if (err instanceof WebAssembly.RuntimeError){
					console.log('\x1b[91m%s\x1b[0m', filename + " failed test: Trap occured");
					continue;
				  } else{
					 console.log("ERROR: Some other error occured during execution");
					 throw(err);
				  }
			  }
			  var passed = true;
			  for (const check of output){
				  var id = check.split("=")[0];
				  var val = check.split("=")[1];
				  var actual = instance.exports[id].value/2;
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
}