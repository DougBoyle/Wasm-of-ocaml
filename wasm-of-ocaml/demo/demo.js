const rt = require(process.env.OCAML_TO_WASM_RT + "/ocaml.js");

function js_fact(num) {
    var rval=1;
    for (var i = 2; i <= num; i++)
        rval = rval * i;
    return rval;
}

(async () => {
    let instance = await rt.instantiate("fact.wasm"); // load/instantiate WebAssembly module
    instance.setup(); // equivalent to 'Open Fact' or '#use "fact.ml"'

    let n = 7; // max 12 - only 31-bit integers in OCaml

    // call OCaml function with tagged representation of n
    let result = instance.fact(rt.ocaml_int(n));

    console.log(result.value);
    console.log(js_fact(n))
})();
