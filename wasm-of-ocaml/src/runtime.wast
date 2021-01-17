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
  (func $make_float (export "make_float") (param $f f64) (result i32)
    (local $result i32)
    i32.const 12
    call $alloc
    local.tee $result
    i32.const -1 ;; variant tag for floats, distinguishes from constructor blocks
    i32.store
    local.get $result
    local.get $f
    f64.store offset=4
    local.get $result
    i32.const 1 ;; data block tag
    i32.or
  )
  ;; given an already untagged closure pointer, copy its contents to a new closure and leave that untagged
  ;; just size of environment (bytes),
  ;; more efficient to directly copy known fields than to do extra address calculations
  ;; closure structure is [func_idx, num_elements (size), num_free_vars, elements...]
  ;; copy first 2 fields, then num_elements extra fields
  (func $copy_closure (export "copy_closure") (param $c i32) (result i32)
    (local $c2 i32) (local $size i32)
    local.get $c
    i32.load offset=4
    local.tee $size
    i32.const 12
    i32.add
    call $alloc ;; get block for copied closure

    local.tee $c2
    local.get $c ;; func_idx
    i32.load
    i32.store

    local.get $c2
    local.get $size ;; num_elements
    i32.store offset=4

    local.get $c2
    local.get $c ;; args left
    i32.load offset=8
    i32.store offset=8

    block
      loop ;; copy environment
        local.get $size
        i32.eqz
        br_if 1

        local.get $c2 ;; store address
        local.get $size
        i32.const 4
        i32.sub
        local.tee $size
        i32.add

        local.get $c ;; load address
        local.get $size
        i32.add

        i32.load offset=12
        i32.store offset=12
        br 0
      end
    end
    local.get $c2
  )
  (func $compare (export "compare") (param $v1 i32) (param $v2 i32) (result i32)
    (local $x i32) (local $arity i32) (local $i i32) (local $f1 f64) (local $f2 f64)
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
      ;; check for float variant tag (-1), in which case just do float equality
      i32.const -1
      i32.eq
      if ;; compare floats. Compares according to Wasm's semantics, need to check OCaml's behaviour
        local.get $v1
        f64.load offset=4
        local.tee $f1
        local.get $v2
        f64.load offset=4
        local.tee $f2
        f64.lt
        if
          i32.const -1
          return
        else
          local.get $f1
          local.get $f2
          f64.gt
          return
        end
      else ;; Not a float, should rewrite so that rest of compare goes in else block
      end
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
  ;; stdlib functions, written directly in Wasm rather than writing in OCaml and compiling to Wasm (inefficient)
  (func $abs (export "abs") (param $x i32) (result i32)
    local.get $x
    i32.const 0
    i32.ge_s
    if (result i32)
      local.get $x
    else
      i32.const 0
      local.get $x
      i32.sub
    end
  )
  (func $min (export "min") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.ge_s
    if (result i32)
      local.get $y
    else
      local.get $x
    end
  )
  (func $max (export "max") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.ge_s
    if (result i32)
      local.get $x
    else
      local.get $y
    end
  )
  (func $append (export "@") (param $l1 i32) (param $l2 i32) (result i32)
    (local $l1decoded i32) (local $result i32)
    local.get $l1
    i32.const 1
    i32.xor
    local.tee $l1decoded
    i32.load ;; tag - either 0 = [] or 2 (encoded) = x::xs
    if (result i32)
      ;; list non-empty
      i32.const 16
      call $alloc

      local.tee $result
      i32.const 2 ;; Dependent on runtime encoding of tags
      i32.store

      local.get $result
      i32.const 2
      i32.store offset=4 ;; arity=2

      local.get $result
      local.get $l1decoded
      i32.load offset=8 ;; head of l1
      i32.store offset=8

      local.get $result
      local.get $l1decoded
      i32.load offset=12 ;; tail of l1
      local.get $l2
      call $append ;; recursive result of appending tail to l2
      i32.store offset=12

      local.get $result
      i32.const 1
      i32.xor ;; tag result as data block
    else
      local.get $l2
    end
  )
)
