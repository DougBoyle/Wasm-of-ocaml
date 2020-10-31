let rec f x = if x = 0 then 1 else x * (g (x-1))

let a = f 5
