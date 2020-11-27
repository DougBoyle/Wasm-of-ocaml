(module
  (memory (export "mem") 1)
  (global $heap_top (mut i32) (i32.const 0))
  (func $alloc (export "alloc") (param $n i32) (result i32)
    global.get $heap_top
	global.get $heap_top
	local.get $n
	i32.add
	global.set $heap_top
  )
)
