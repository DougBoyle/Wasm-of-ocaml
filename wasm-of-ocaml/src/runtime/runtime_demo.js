const rt = require(process.env.OCAML_TO_WASM_RT + "/ocaml.js");

(async () => {
    var instance = await rt.instantiate(__dirname + "/../../samples/out/arith.wasm");
    console.log(instance);
    var setup_result = instance.setup();
    console.log(instance);

   console.log(instance.x.value);
   console.log(instance.phi(rt.ocaml_int(10)).value);
})();