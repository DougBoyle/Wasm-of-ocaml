type 'a ref = {mutable content : 'a}
let x = {content = 1}
let _ = while x.content < 10 do x.content <- x.content * 2 done
let y = x.content
