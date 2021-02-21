const util = require('util');
const fs = require("fs");
const {ManagedMemory} = require(process.env.OCAML_TO_WASM_RT +  "/memory.js");
const readFile = util.promisify(fs.readFile);
const exec = util.promisify(require('child_process').exec);

let fileList = __dirname + "/results.txt";

// process.argv is a list of the command line arguments

//const check_mem = require("./checkmemory");

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
      memoryManager.mallocSequence.push(memoryManager.mallocsDone);
	 // console.log(instance.exports);
//	  console.log(instance.exports.y.value);
//	  console.log(instance.exports.z.value);
	 // console.log(imports.ocamlRuntime.alloc(0));

	  // TODO: Just for checking garbage collecting working
	  console.log("pages allocated:", memory.buffer.byteLength >> 16 );
	  console.log("mem allocated at end:", memoryManager.memory_used);
	  console.log("peak memory used was:", memoryManager.maxMemory);
	  console.log("GC called ", memoryManager.gcsDone, "times");
	  console.log("Frees found:", memoryManager.freeSequence);
	  console.log("Mallocs managed:", memoryManager.mallocSequence);
      console.log("Successful GC passes:", memoryManager.gcSequence);
//	  console.log(memoryManager.uview.slice(instance.exports.x.value >> 2));
//	  console.log(memoryManager.uview.byteLength);
//	  console.log(instance.exports.x.value);
      fs.writeFile(
          __dirname + '/mallocLog.txt',

          JSON.stringify(memoryManager.mallocSequence) + "\n" +
          JSON.stringify(memoryManager.freeSequence)  + "\n" +
          JSON.stringify(memoryManager.gcSequence),

          function (err) {
              if (err) {
                  console.error('Crap happens');
              }
          }
      );
  } catch (err) {
    console.log(err);
    console.log(err.message);
  }
})();
} else {
(async () => {console.log("One argument required");}
)();
}