const util = require('util');
const fs = require("fs");
const readFile = util.promisify(fs.readFile);

function wrap_ptr(i, instance){
	// handle int/function/data separately. Still need to be able to pass to wasm so separate wasm/js values
	var result;
	switch (i & 3) {
		case 1:
			const arity = instance._mem[(i/4>>0) + 1];
			// If a mutable record, could be modified in future, so function must do the memory lookup when called
			result = () => {
				var ar = new Array(arity+1);
				ar[0] = wrap_ptr(instance._mem[(i/4>>0)], instance);
				for (var j = 0; j < arity; j++){
					ar[j+1] = 	wrap_ptr(instance._mem[(i/4>>0) + j + 2], instance); // don't both copying arity in
				}
				return ar;
			}
		    break;
		case 3: // For now assume only every 1 argument. Tuples should just be a pointer
			const func_idx = instance._mem[i/4>>0]; // function pointer is at start of closure
			result = arg => wrap_ptr(instance.exports[func_idx](i ^ 3, arg._wasm), instance);
			//return instance.exports[func_idx].apply(null, [ptr ^ 3].concat(args));
		    break;
		default:
		  // just an int, use as is for wasm, halve when returning
			result = () => (i/2>>0);
	}
	result._wasm = i;
	return result;
}

// Returns an object which initially just has a 'setup' function. Calling that runs OCAML$MAIN and returns its value,
// while also initialising each of the exported fields on the instance.
// TODO: Encode location of runtime wasm file better
async function instantiate(file){
	var buffer = await readFile(__dirname + '/../samples/runtime.wasm');
	var module = await WebAssembly.compile(buffer);
	var runtime_instance = await WebAssembly.instantiate(module);
	var imports = {ocamlRuntime: runtime_instance.exports};

	buffer = await readFile(file);
	module = await WebAssembly.compile(buffer);
	var instance = await WebAssembly.instantiate(module, imports);
	// attach the memory from the runtime instance to this instance to avoid repeatedly making Int32Arrays
	instance._mem = new Int32Array(instance.exports["$mem"].buffer);

	var result = {};

	result.setup = () => {
		var setup_val = wrap_ptr(instance.exports["OCAML$MAIN"](), instance);
		// each export that isn't a number just gets a global
		for (var exp in instance.exports) {
			// avoid exporting OCAML$MAIN or $mem
			if (!(/^(\d+|OCAML\$MAIN|\$.*)$/.test(exp))) {
				result[exp] = wrap_ptr(instance.exports[exp](), instance);
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