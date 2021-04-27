
const rt = require(process.env.OCAML_TO_WASM_RT + "/ocaml.js");

function js_fact(num) {
    var rval=1;
    for (var i = 2; i <= num; i++)
        rval = rval * i;
    return rval;
}

(async () => {
    let instance = await rt.instantiate("lists.wasm"); // load/instantiate WebAssembly module
    instance.setup(); // run the OCaml program

    let sorted = instance.mergesort(instance.l);

    let wasm_result = instance.nth(sorted)(rt.ocaml_int(2)); // 3rd element of list


    console.log(wasm_result()); // function call untags the WebAssembly value
})();
