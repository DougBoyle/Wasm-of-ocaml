let x = ref 10
let y = incr x; !x
let z = x := 20; decr x; !x
