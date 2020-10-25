const fetch = require("node-fetch");

const memory = new WebAssembly.Memory({ initial: 1 });

const util = require('util');
const fs = require("fs");

const readFile = util.promisify(fs.readFile);

(async () => {
	const buffer = await readFile('D:/Programming/Project/Wasm-of-ocaml/wasm-of-ocaml/C-examples/fib.wasm');
  const module = await WebAssembly.compile(buffer);
  const instance = await WebAssembly.instantiate(module);
 // const response = await fetch('http://localhost:3000/wasm-files/fib.wasm');
 // const bytes = await response.arrayBuffer();
 // const { instance } = await WebAssembly.instantiate(bytes); //, {
  //  env: { log, memory }
//  });

  console.log(instance.exports.main(10));
})();