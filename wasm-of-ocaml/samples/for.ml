type 'a ref = {mutable content : 'a}
let x = {content = 0}
let _ = for i = 1 to 10 do x.content <- x.content + i done
let _ = for i = 10 downto 1 do x.content <- x.content - 1 done
let y = x.content
