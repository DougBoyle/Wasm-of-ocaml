const {ManagedMemory} = require(process.env.OCAML_TO_WASM_RT +  "/memory.js");

function checkPtr(memManager, blockPtr){
    let arity = memManager.uview[blockPtr + 1];
    if (arity < 0 || arity > 5){
        console.log("Bad arity");
    }
}

function checkStackValues(memManager, topWord){
    for (let i = 0; i < topWord; i++){
        let val = memManager.uview[i];
        if (val & 1){
            if (val < 0){
                console.log("Negative pointer found");
            }
        }
    }
}

function stackzeroaftertop(memManager, topWord, limit){
    let valid = true;
    for (let i = topWord; i < limit; i++){
        if (memManager.uview[i] != 0){
            valid = false;
            console.log("Stack not being reset correctly");
        }
    }
    if (valid) {
        console.log("stack correctly zeroed");
    }
}

function check(memManager){
    let blocks = [];
    let allocated = [];
    let free = [];
    const stack_limit = 16384;
    const stack_top = memManager.runtime.exports.sp.value;
    console.log("stack top is ", stack_top, "in words:", stack_top>>2);
    const mem_top = memManager.uview.byteLength >> 2;
    let ptr = stack_limit;
    while ( ptr < mem_top){
        let size = memManager.uview[ptr + 1];
        let next = memManager.uview[ptr];
        let tag = memManager.uview[ptr + 2];
   //     console.log("block", ptr, "size", size, "next", next, "tag", tag);
        if (tag == 2){
            allocated.push(ptr);
        } else if (tag == 0){
            free.push(ptr);
        } else {
            console.log("BAD TAG FOUND. ptr", ptr, "tag", tag);
        }
        blocks.push(ptr);
        ptr += size;
    }
    console.log("allocated blocks:", allocated.length);
    console.log("free blocks:", free.length);
    checkStackValues(memManager, stack_top>>2);
    stackzeroaftertop(memManager, stack_top>>2, stack_limit);
}

exports.check = check;