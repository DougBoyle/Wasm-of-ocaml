const util = require('util');
const fs = require("fs");
const readFile = util.promisify(fs.readFile);
const {ManagedMemory} = require(process.env.OCAML_TO_WASM_RT +  "/memory.js");

// Doesn't provide methods to create values to pass to OCaml,
// and assumes values read out once returned, hence doesn't interact with GC

// Functions treated as JS functions, as are data blocks since values could be mutable so may need updating
// ints and floats are just stored in .value field
function wrap_ptr(i, instance){
	// 'View' (Int32Array) gets cleared whenever memory grows, so build a new view each time this is used
	var mem = new Int32Array(instance.exports["$mem"].buffer);
	// handle int/function/data separately. Still need to be able to pass to wasm so separate wasm/js values
	var result;
	switch (i & 3) {
		case 1:
			// Interpret floats
			const tag = mem[i/4>>0];
			if (tag == -1){
				// get just the float
				// +7 due to tag of 1, DataView to allow reading in Little Endian
				const f = new DataView(instance.exports["$mem"].buffer).getFloat64(i+7, true /* littleEndian */);
				result = {value: f};
				break;
			}
			const arity = mem[(i/4>>0) + 1];
			// If a mutable record, could be modified in future, so function must do the memory lookup when called
			result = () => {
				var ar = new Array(arity+1);
				ar[0] = wrap_ptr(mem[(i/4>>0)], instance);
				for (var j = 0; j < arity; j++){
					ar[j+1] = 	wrap_ptr(mem[(i/4>>0) + j + 2], instance); // don't copy arity in
				}
				return ar;
			}
		    break;
		case 3: // Externally visible functions only ever take 1 argument
			const func_idx = mem[i/4>>0]; // function pointer is at start of closure
			result = arg => wrap_ptr(instance.exports[func_idx](i ^ 3, arg._wasm), instance);
		    break;
		default:
		  // just an int, use as is for wasm, halve when returning
			result = {value: i/2>>0};
	}
	result._wasm = i;
	return result;
}

// Returns an object which initially just has a 'setup' function. Calling that runs OCAML$MAIN and returns its value,
// while also initialising each of the exported fields on the instance.
// TODO: Encode location of runtime wasm file better
// TODO: Use JavaScript runtime system
async function instantiate(file){
	// Uses garbage collected runtime system
	const memory = new WebAssembly.Memory({ initial: 2 });
	var memoryManager = new ManagedMemory(memory);
	var rtimports = {jsRuntime: {malloc : memoryManager.malloc,
			stackOverflow : memoryManager.stackLimitExceeded,
			mem : memory}};

	var buffer = await readFile(process.env.OCAML_TO_WASM_RT + '/runtime.wasm');
	var module = await WebAssembly.compile(buffer);
	var runtime_instance = await WebAssembly.instantiate(module, rtimports);
	memoryManager.setRuntime(runtime_instance);

	var imports = {ocamlRuntime: runtime_instance.exports};

	buffer = await readFile(file);
	module = await WebAssembly.compile(buffer);
	var instance = await WebAssembly.instantiate(module, imports);

	var result = {};

	result.setup = () => {
		var setup_val = wrap_ptr(instance.exports["OCAML$MAIN"](), instance);
		// each export that isn't a number just gets a global
		for (var exp in instance.exports) {
			// avoid exporting OCAML$MAIN or $mem
			if (!(/^(\d+|OCAML\$MAIN|\$.*)$/.test(exp))) {
				result[exp] = wrap_ptr(instance.exports[exp].value, instance);
			}
		}
		return setup_val;
	}
	return result;
}

function ocaml_int(n){
	var result = () => n;
	result._wasm = 2*n;
	return result
}



module.exports = {instantiate, ocaml_int};