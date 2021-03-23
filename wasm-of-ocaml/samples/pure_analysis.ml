let y = ref 0

let f _ = let x = !y in x

let a  = f(0)
let b = incr y
let c = f(0)
