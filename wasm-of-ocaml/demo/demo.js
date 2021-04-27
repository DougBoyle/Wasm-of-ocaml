
const rt = require(process.env.OCAML_TO_WASM_RT + "/ocaml.js");

function js_fact(num) {
    var rval=1;
    for (var i = 2; i <= num; i++)
        rval = rval * i;
    return rval;
}

(async () => {
    let instance = await rt.instantiate("fact.wasm"); // load/instantiate WebAssembly module
    instance.setup(); // run the OCaml program

    let n = 12; // max 12 - only 31-bit integers in OCaml, so result must be less than 2^30

    // call the OCaml function, after converting n to its tagged representation in WebAssembly
    let wasm_result = instance.fact(rt.ocaml_int(n));

    console.log(wasm_result()); // function call untags the WebAssembly value
    console.log(js_fact(n))
})();
