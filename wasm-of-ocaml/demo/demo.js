const rt = require(process.env.OCAML_TO_WASM_RT + "/ocaml.js");

function fib(n) {
    let a = 0, b = 1;
    for (var i = 0; i < n; i++){
        b = a + b; a = b - a;
    }
    return a;
}

(async () => {
    let instance = await rt.instantiate("fib.wasm"); // load/instantiate WebAssembly module
    instance.setup(); // equivalent to 'Open Fact' or '#use "fact.ml"'

    console.log(`x = ${instance.x.value}`);

    // 0-44
    let n = 12;

    // call OCaml function with tagged representation of n
    let result = instance.fib(rt.ocaml_int(n));

    console.log(`fib(${n}) = ${result.value}`);

    // result computed by JS
    console.log(`fib(${n}) = ${fib(n)}`)
})();
