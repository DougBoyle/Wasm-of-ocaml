const util = require('util');
const fs = require("fs");
const {ManagedMemory} = require(process.env.OCAML_TO_WASM_RT +  "/memory.js");
const readFile = util.promisify(fs.readFile);
const exec = util.promisify(require('child_process').exec);

let fileList = __dirname + "/results.txt";


if (process.argv.length > 2){
(async () => {
  const memory = new WebAssembly.Memory({ initial: 2 });
  var memoryManager = new ManagedMemory(memory);
  var rtimports = {jsRuntime: {malloc : memoryManager.malloc,
	stackOverflow : memoryManager.stackLimitExceeded,
	//log : memoryManager.log,
    mem : memory}};

  var buffer = await readFile(process.env.OCAML_TO_WASM_RT + '/runtime.wasm');
  var module = await WebAssembly.compile(buffer);
  var instance = await WebAssembly.instantiate(module, rtimports);
  memoryManager.setRuntime(instance);
  
  imports = {ocamlRuntime: instance.exports};
  try {
	  var buffer = await readFile(process.argv[2]);
	  var module = await WebAssembly.compile(buffer);
	  var instance = await WebAssembly.instantiate(module, imports);
	  instance.exports["OCAML$MAIN"]();
	  console.log(instance.exports);

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
		  const memory = new WebAssembly.Memory({ initial: 2 });
		  var memoryManager = new ManagedMemory(memory);
		  var rtimports = {jsRuntime: {malloc : memoryManager.malloc,
			stackOverflow : memoryManager.stackLimitExceeded,
		    mem : memory}};

		  var buffer = await readFile(process.env.OCAML_TO_WASM_RT + '/runtime.wasm');
		  var module = await WebAssembly.compile(buffer);
		  var instance = await WebAssembly.instantiate(module, rtimports);
		  memoryManager.setRuntime(instance);

		  var imports = {ocamlRuntime: instance.exports};
		  var buffer = await readFile(__dirname + "/out/" + basename + ".wasm");
		  var module = await WebAssembly.compile(buffer);
		  var instance = await WebAssembly.instantiate(module, imports);

		  if (line[line.length - 1] == "!"){
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
			  try {
				instance.exports["OCAML$MAIN"]();
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