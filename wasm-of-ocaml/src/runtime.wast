(module
  (type $0 (func (result i32)))
  (memory (export "mem") 1) ;; initially just a 1 page memory i.e. 64KB. Can use up to 4GB in Wasm (32-bit)
  (global $heap_top (mut i32) (i32.const 0))
  (func $alloc (export "alloc") (param $n i32) (result i32)
    (local $difference i32)
    global.get $heap_top ;; returned value
	global.get $heap_top
	local.get $n
	i32.add
	global.set $heap_top ;; store updated heap_top

    global.get $heap_top
	memory.size
	i32.const 16
    i32.shl  ;; page size = 2^16 = 65536
    i32.gt_u
    if
      global.get $heap_top
      i32.const 16
      i32.shr_u
      memory.size
      i32.sub
      i32.const 1
      i32.add
      memory.grow

      i32.const -1
      i32.eq ;; check memory grow succeeded
      if
        unreachable
      else
      end
    else ;; memory not exceeded, do nothing
    end
  )
  (func $compare (export "compare") (param $v1 i32) (param $v2 i32) (result i32)
    (local $x i32) (local $arity i32) (local $i i32)
    block
      block
        block
          local.get $v1
          i32.const 3
          i32.and
          br_table 0 1 0 2
        end
        ;; int case
        local.get $v1
        local.get $v2
        i32.sub
        return
      end
      ;; data case. Compare tags, if those match, get arity and walk through recursively comparing values
      ;; remove type tags
      local.get $v1
      i32.const 1
      i32.xor
      local.set $v1
      local.get $v2
      i32.const 1
      i32.xor
      local.set $v2
      ;; compare variant tags (also orders arrays)
      local.get $v1
      i32.load
      local.get $v2
      i32.load
      i32.sub
      local.tee $x
      i32.eqz  ;; no need for this, just reverse if/else cases
      if (result i32)
        ;; recursively compare
        local.get $v1
        i32.load offset=4
        i32.const 4
        i32.mul
        local.set $arity
        i32.const 0
        local.set $i
        block
          loop
            local.get $i
            local.get $arity
            i32.ge_s
            br_if 1 ;; all elements compared
            local.get $v1
            local.get $i
            i32.add
            i32.load offset=8
            local.get $v2
            local.get $i
            i32.add
            i32.load offset=8
            call $compare
            local.tee $x
            if
              local.get $x
              return
            else
              ;; was zero, loop again
              local.get $i
              i32.const 4
              i32.add
              local.set $i
              br 1
            end
          end
        end
        i32.const 0 ;; recursively equal so overall considered equal
      else
        local.get $x
      end
      return
    end
    ;; function case
    unreachable
  )
)
