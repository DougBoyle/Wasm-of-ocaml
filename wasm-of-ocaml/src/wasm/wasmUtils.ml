(* Making use of the opam module "wasm" for a representation of Wasm programs and to output them to files *)
(* See: https://github.com/WebAssembly/spec/tree/master/interpreter *)
(* Also want my own low-level IR? Makes it slightly easier to work with usual flow that is less clear in Wasm:
   e.g. (while of block1 * block2) => block L1 {block1 (negated); brif L1; loop L2 {block2; pop; block1; brif L2}}
Example low IR: https://github.com/grain-lang/grain/blob/78dc08b2887226cf0b9f93357ca6fd689fcd1405/src/codegen/mashtree.ml
*)