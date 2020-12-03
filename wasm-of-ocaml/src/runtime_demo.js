const rt = require("./ocaml.js");

if (process.argv.length > 2){
    (async () => {
        var instance = await rt.instantiate(process.argv[2]);
        console.log(instance);
        var setup_result = instance.setup();
        console.log(instance);

       console.log(instance.x());
       console.log(instance.phi(rt.ocaml_int(10))());
    })();
}