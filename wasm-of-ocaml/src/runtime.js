const OCAML_RUNTIME_FUNCS = 2; // not sure if needed or not

//var memory;

/*function import_mem(mem){
    // Wasm exposes a byte array mem.buffer, Int32Array treats this as an array of signed 32-bit values
    memory = new Int32Array(mem.buffer);
}*/

function encode_int(i){
    return 2*i;
}

function decode_int(i){
    return i/2;
}

function call_closure(instance, mem, ptr, args){
    const memory = new Int32Array(mem.buffer);
    if (ptr & 3 != 3){throw "Ptr is not tagged as being a closure";}
    // >> 0 is a hack to convert to integer
    const func_idx = memory[ptr/4>>0]; // function pointer is at start of closure
    console.log(instance.exports[func_idx]);
 //   return instance.exports["7"](203);
    return instance.exports[func_idx].apply(null, [ptr ^ 3].concat(args)); // works for args being an integer too
}

function get_global(instance, x){
    return instance.exports[x]();
}

module.exports = {encode_int, decode_int, call_closure, get_global};