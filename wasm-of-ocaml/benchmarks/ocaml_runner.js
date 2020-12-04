const rt = require("../src/ocaml.js");

(async () => {
    var instance = await rt.instantiate("out/binary_trees.wasm");
  //  console.log(instance);
    var setup_result = instance.setup();
  //  console.log(instance);
    console.log("success");

  //  console.log(instance.x());
  //  console.log(instance.phi(rt.ocaml_int(10))());
})();