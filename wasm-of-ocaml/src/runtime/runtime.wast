(module
  (type $0 (func (result i32)))
  (type $1 (func (param i32) (result i32)))

  (import "jsRuntime" "malloc" (func $malloc (type 1)))
  (import "jsRuntime" "stackOverflow" (func $stackOverflow (type 0)))
  ;; for debugging sp
 ;; (import "jsRuntime" "log" (func $log (type 1)))
 ;; (export "log" (func $log))

  ;; memory now imported and re-exported so it gets linked to js functions
  (memory $mem (export "mem") (import "jsRuntime" "mem") 2) ;; now require minimum of 2 pages, 1st just for stack

  ;; for manipulating stack. 1st page is used purely for stack so limit is 2^16 = 65536 bytes
  (global $sp (export "sp") (mut i32) (i32.const 0))
  (func $create_fun (export "create_fun") (param $size i32)
    global.get $sp
    local.get $size
    i32.add
    global.set $sp

    ;; check for overflow
    global.get $sp
    i32.const 65536
    i32.gt_u
    if ;; overflow, trap
      call $stackOverflow ;; report overflow error
      unreachable
    else ;; no overflow, do nothing
    end
  )
  (func $exit_fun (export "exit_fun") (param $size i32) ;; TODO: FIND A BETTER SOLUTION
  ;; Because gc will assume every pointer in stack is live, an outdated stack pointer that hasn't yet been
  ;; overwritten causes problems as it could now point to the middle of a freshly allocated data block,
  ;; and attempting to tag that would modify actual values in memory
    (local $old i32) (local $new i32)
    global.get $sp
    local.tee $old
    local.get $size
    i32.sub
    local.tee $new
    global.set $sp

    ;; now wipe stack from old down to new
    loop
      local.get $old
      i32.const 4
      i32.sub
      local.tee $old
      i32.const 0
      i32.store

      local.get $old
      local.get $new
      i32.gt_u
      br_if 0
    end
  )
 ;; takes adjusted index, so first argument is 4, and swap slots aren't included  (x4 to get i32 index)
 (func $update_local (export "update_local") (param $value i32) (param $index i32) (result i32)
    global.get $sp
    local.get $index
    i32.sub
    local.get $value
    i32.store
    local.get $value ;; returns the value it was passed
  )

 ;; For now, just re-export rather than having to change things in compilewasm
  (export "malloc" (func $malloc))
  (func $make_float (export "make_float") (param $f f64) (result i32)
    (local $result i32)
    i32.const 16
    call $malloc
    local.tee $result
    i32.const -1 ;; variant tag for floats, distinguishes from constructor blocks
    i32.store
    local.get $result
    i32.const 0  ;; set 'arity' to 0 so we can treat it like any other data for GC
    i32.store offset=4
    local.get $result
    local.get $f
    f64.store offset=8
    local.get $result
    i32.const 1 ;; data block tag
    i32.or
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

  ;; doesn't map exactly to an OCaml program. Instead written to allow doing append as a loop.
  ;; since caller has a pointer to l1 and l2, only need to put 'result' on the shadow stack.
  ;; Equivalent C program:
;;  if (l1 = []) {return l2}    // no need for a new list
;;
;;  result = malloc(4)    // create a variable to hold the result
;;  result.0 = l1.0
;;  l1 = l1.1             // store the first element of l1 in a new cell, next ptr of cell undefined
;;  tail = result
;;  while (l1 != []){
;;    tail.1 = malloc(4)  // point next to a new cell
;;    tail = tail.1       // advance tail
;;    tail.0 = l1.0       // set value in that cell, next ptr of cell undefined
;;    l1 = l1.1           // advance l1
;;  }
;;  tail.1 = l2           // connect up the remaining undefined next ptr to point to l2
;;  return result

;;  In reality, need to initialise the next ptr to 0 in each case so it doesn't get followed during GC

  (func $append (export "@") (param $l1 i32) (param $l2 i32) (result i32)
    (local $result i32) (local $tail i32) (local $cell i32)

    local.get $l1

    i32.eqz
    if (result i32) ;; 0 = []
      local.get $l2

    else ;; block, untag pointer
      local.get $l1
      i32.const 1
      i32.xor
      local.set $l1  ;; untag l1


      i32.const 4
      call $create_fun  ;; allocate stack space to remember 'result'

      global.get $sp  ;; update_local for result list
      i32.const 4  ;; negative offsets not supported
      i32.sub

      i32.const 16
      call $malloc
      local.tee $result
      local.tee $tail ;; take the opportunity to also save in tail
      i32.const 1
      i32.xor

      i32.store

      ;; set the fields of result
      local.get $result
      i32.const 2 ;; tag - based on runtime encoding
      i32.store
      local.get $result
      i32.const 2 ;; arity
      i32.store offset=4
      local.get $result
      local.get $l1
      i32.load offset=8
      i32.store offset=8
      local.get $result ;; next ptr set to 0 for now
      i32.const 0
      i32.store offset=12

      ;; advance l1
      local.get $l1
      i32.load offset=12
      local.set $l1

      ;; while loop
      block
        loop
          ;; test condition
          local.get $l1
          i32.eqz

          br_if 1

          local.get $l1
          i32.const 1
          i32.xor
          local.set $l1


          local.get $tail
          ;; allocate new cell
          i32.const 16
          call $malloc
          local.tee $tail
          i32.const 1
          i32.xor
          i32.store offset=12

          local.get $tail
          i32.const 2 ;; tag - based on runtime encoding
          i32.store
          local.get $tail
          i32.const 2 ;; arity
          i32.store offset=4
          local.get $tail
          local.get $l1
          i32.load offset=8  ;; store next element of l1
          i32.store offset=8
          local.get $tail
          i32.const 0
          i32.store offset=12

          ;; advance l1
          local.get $l1
          i32.load offset=12
          local.set $l1

          br 0 ;; loop
        end
      end

      local.get $tail    ;; connect remaining pointer to l2
      local.get $l2
      i32.store offset=12

      local.get $result ;; put tag on final result
      i32.const 1
      i32.xor

      ;; free stack space
      i32.const 4
      call $exit_fun
    end
  )
)
