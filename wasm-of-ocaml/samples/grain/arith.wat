(module
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32 i32 i32) (result i32)))
  (type (;2;) (func (param i32) (result i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func (param i32)))
  (type (;5;) (func (param i64) (result i64)))
  (type (;6;) (func (param i32 i32 i32)))
  (import "grainRuntime" "relocBase" (global (;0;) i32))
  (import "grainRuntime" "moduleRuntimeId" (global (;1;) i32))
  (import "console" "log" (func (;0;) (type 2)))
  (import "grainRuntime" "checkMemory" (func (;1;) (type 4)))
  (import "grainRuntime" "malloc" (func (;2;) (type 2)))
  (import "grainRuntime" "incRef" (func (;3;) (type 2)))
  (import "grainRuntime" "incRef64" (func (;4;) (type 5)))
  (import "grainRuntime" "decRef" (func (;5;) (type 2)))
  (import "grainRuntime" "decRef64" (func (;6;) (type 5)))
  (import "console" "tracepoint" (func (;7;) (type 4)))
  (import "grainRuntime" "throwError" (func (;8;) (type 6)))
  (import "grainRuntime" "decRefIgnoreZeros" (func (;9;) (type 2)))
  (import "GRAIN$MODULE$pervasives" "GRAIN$EXPORT$GET$print" (func (;10;) (type 0)))
  (import "GRAIN$MODULE$pervasives" "GRAIN$EXPORT$GET$+" (func (;11;) (type 0)))
  (import "GRAIN$MODULE$pervasives" "GRAIN$EXPORT$GET$<" (func (;12;) (type 0)))
  (import "GRAIN$MODULE$pervasives" "GRAIN$EXPORT$GET$==" (func (;13;) (type 0)))
  (import "grainRuntime" "mem" (memory (;0;) 0))
  (import "grainRuntime" "tbl" (table (;0;) 27 funcref))
  (func (;14;) (type 0) (result i32)
    (local i32 i32 i64)
    global.get 16
    nop
    local.get 0
    nop
    drop
    local.set 0
    local.get 0
    call 3
    drop
    local.get 0
    nop
    call 9
    return)
  (func (;15;) (type 0) (result i32)
    (local i32 i32 i64)
    global.get 17
    nop
    local.get 0
    nop
    drop
    local.set 0
    local.get 0
    call 3
    drop
    local.get 0
    nop
    call 9
    return)
  (func (;16;) (type 0) (result i32)
    (local i32 i32 i64)
    global.get 18
    nop
    local.get 0
    nop
    drop
    local.set 0
    local.get 0
    call 3
    drop
    local.get 0
    nop
    call 9
    return)
  (func (;17;) (type 0) (result i32)
    (local i32 i32 i64)
    global.get 19
    nop
    local.get 0
    nop
    drop
    local.set 0
    local.get 0
    call 3
    drop
    local.get 0
    nop
    call 9
    return)
  (func (;18;) (type 2) (param i32) (result i32)
    (local i32 i32 i64 i32 i32 i32 i32)
    local.get 0
    i32.load offset=24
    i32.const 1
    i32.xor
    i32.load offset=4
    call 3
    local.get 4
    call 5
    drop
    local.set 4
    local.get 0
    i32.load offset=28
    i32.const 1
    i32.xor
    i32.load offset=4
    call 3
    local.get 5
    call 5
    drop
    local.set 5
    local.get 0
    i32.load offset=12
    i32.const 5
    i32.xor
    local.get 5
    i32.const 0
    local.get 0
    i32.load offset=12
    i32.const 5
    i32.xor
    i32.load offset=4
    call_indirect (type 1)
    call 3
    local.get 6
    call 5
    drop
    local.set 6
    local.get 6
    i32.const 31
    i32.shr_u
    if (result i32)  ;; label = @1
      local.get 4
    else
      local.get 4
      i32.const 1
      i32.shr_s
      i64.extend_i32_s
      local.get 5
      i32.eqz
      if  ;; label = @2
        i32.const 18
        i32.const 0
        i32.const 0
        call 8
        unreachable
      else
        nop
      end
      local.get 5
      i32.const 1
      i32.shr_s
      i64.extend_i32_s
      i64.rem_s
      i64.const 2
      i64.mul
      nop
      local.get 3
      nop
      drop
      local.tee 3
      local.get 4
      i32.const 31
      i32.shr_u
      local.get 5
      i32.const 31
      i32.shr_u
      i32.eq
      local.get 3
      i64.eqz
      i32.or
      if (result i64)  ;; label = @2
        i64.const 0
      else
        local.get 5
        i64.extend_i32_s
      end
      i64.add
      nop
      local.get 3
      nop
      drop
      local.set 3
      local.get 3
      local.get 3
      i64.const 2147483647
      i64.gt_s
      if  ;; label = @2
        i32.const 4
        i32.const 0
        i32.const 0
        call 8
        unreachable
      else
        nop
      end
      i64.const -2147483648
      i64.lt_s
      if  ;; label = @2
        i32.const 4
        i32.const 0
        i32.const 0
        call 8
        unreachable
      else
        nop
      end
      local.get 3
      i32.wrap_i64
      call 3
      local.get 7
      call 5
      drop
      local.set 7
      local.get 0
      i32.load offset=24
      i32.const 1
      i32.xor
      nop
      local.get 1
      nop
      drop
      local.set 1
      local.get 1
      local.get 5
      call 3
      local.get 1
      i32.load offset=4
      call 5
      drop
      i32.store offset=4
      local.get 5
      drop
      local.get 0
      i32.load offset=28
      i32.const 1
      i32.xor
      nop
      local.get 1
      nop
      drop
      local.set 1
      local.get 1
      local.get 7
      call 3
      local.get 1
      i32.load offset=4
      call 5
      drop
      i32.store offset=4
      local.get 7
      drop
      local.get 0
      i32.load offset=16
      i32.const 1
      i32.xor
      nop
      local.get 1
      nop
      drop
      local.set 1
      local.get 1
      i32.const -1
      call 3
      local.get 1
      i32.load offset=4
      call 5
      drop
      i32.store offset=4
      i32.const -1
      drop
      local.get 0
      i32.load offset=20
      i32.const 1
      i32.xor
      nop
      local.get 1
      nop
      drop
      local.set 1
      local.get 1
      local.get 0
      i32.load offset=32
      call 3
      local.get 1
      i32.load offset=4
      call 5
      drop
      i32.store offset=4
      local.get 0
      i32.load offset=32
      drop
      i32.const -1
    end
    nop
    local.get 1
    nop
    drop
    local.set 1
    local.get 1
    call 3
    drop
    local.get 4
    nop
    call 9
    drop
    local.get 5
    nop
    call 9
    drop
    local.get 6
    nop
    call 9
    drop
    local.get 7
    nop
    call 9
    drop
    local.get 1
    nop
    call 9
    return)
  (func (;19;) (type 1) (param i32 i32 i32) (result i32)
    (local i32 i32 i64 i32 i32 i32)
    local.get 0
    i32.load offset=20
    i32.const 1
    i32.xor
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    local.get 1
    call 3
    local.get 3
    i32.load offset=4
    call 5
    drop
    i32.store offset=4
    local.get 1
    drop
    local.get 0
    i32.load offset=24
    i32.const 1
    i32.xor
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    local.get 2
    call 3
    local.get 3
    i32.load offset=4
    call 5
    drop
    i32.store offset=4
    local.get 2
    drop
    local.get 0
    i32.load offset=12
    i32.const 1
    i32.xor
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    i32.const -1
    call 3
    local.get 3
    i32.load offset=4
    call 5
    drop
    i32.store offset=4
    i32.const -1
    drop
    local.get 0
    i32.load offset=16
    i32.const 1
    i32.xor
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    local.get 0
    i32.load offset=28
    call 3
    local.get 3
    i32.load offset=4
    call 5
    drop
    i32.store offset=4
    local.get 0
    i32.load offset=28
    drop
    i32.const 16
    call 2
    nop
    local.get 3
    nop
    drop
    local.tee 3
    i32.const 1
    i32.store
    local.get 3
    i32.const 0
    call 3
    i32.store offset=4
    local.get 3
    i32.const 1
    i32.or
    nop
    local.get 3
    nop
    drop
    local.tee 3
    nop
    local.get 6
    call 5
    drop
    local.set 6
    block (result i32)  ;; label = @1
      loop (result i32)  ;; label = @2
        i32.const 1879048191
        local.get 0
        i32.load offset=12
        i32.const 1
        i32.xor
        i32.load offset=4
        i32.const 31
        i32.shr_u
        i32.eqz
        br_if 1 (;@1;)
        call 9
        drop
        local.get 0
        i32.load offset=12
        i32.const 1
        i32.xor
        nop
        local.get 3
        nop
        drop
        local.set 3
        local.get 3
        i32.const 2147483647
        call 3
        local.get 3
        i32.load offset=4
        call 5
        drop
        i32.store offset=4
        i32.const 2147483647
        drop
        local.get 0
        i32.load offset=16
        i32.const 1
        i32.xor
        i32.load offset=4
        call 3
        local.get 7
        call 5
        drop
        local.set 7
        local.get 7
        i32.const 5
        i32.xor
        local.get 7
        i32.const 5
        i32.xor
        i32.load offset=4
        call_indirect (type 2)
        call 3
        local.get 8
        call 5
        drop
        local.set 8
        local.get 6
        i32.const 1
        i32.xor
        nop
        local.get 3
        nop
        drop
        local.set 3
        local.get 3
        local.get 8
        call 3
        local.get 3
        i32.load offset=4
        call 5
        drop
        i32.store offset=4
        local.get 8
        br 0 (;@2;)
      end
    end
    drop
    local.get 6
    i32.const 1
    i32.xor
    i32.load offset=4
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    call 3
    drop
    local.get 6
    nop
    call 9
    drop
    local.get 7
    nop
    call 9
    drop
    local.get 8
    nop
    call 9
    drop
    local.get 3
    nop
    call 9
    return)
  (func (;20;) (type 0) (result i32)
    (local i32 i32 i64)
    global.get 20
    nop
    local.get 0
    nop
    drop
    local.set 0
    local.get 0
    call 3
    drop
    local.get 0
    nop
    call 9
    return)
  (func (;21;) (type 3) (param i32 i32) (result i32)
    (local i32 i32 i64 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    i32.const 16
    call 2
    nop
    local.get 2
    nop
    drop
    local.tee 2
    i32.const 1
    i32.store
    local.get 2
    i32.const 0
    call 3
    i32.store offset=4
    local.get 2
    i32.const 1
    i32.or
    nop
    local.get 2
    nop
    drop
    local.tee 2
    nop
    local.get 5
    call 5
    drop
    local.set 5
    i32.const 16
    call 2
    nop
    local.get 2
    nop
    drop
    local.tee 2
    i32.const 1
    i32.store
    local.get 2
    i32.const 0
    call 3
    i32.store offset=4
    local.get 2
    i32.const 1
    i32.or
    nop
    local.get 2
    nop
    drop
    local.tee 2
    nop
    local.get 6
    call 5
    drop
    local.set 6
    i32.const 16
    call 2
    nop
    local.get 2
    nop
    drop
    local.tee 2
    i32.const 1
    i32.store
    local.get 2
    i32.const 0
    call 3
    i32.store offset=4
    local.get 2
    i32.const 1
    i32.or
    nop
    local.get 2
    nop
    drop
    local.tee 2
    nop
    local.get 7
    call 5
    drop
    local.set 7
    i32.const 16
    call 2
    nop
    local.get 2
    nop
    drop
    local.tee 2
    i32.const 1
    i32.store
    local.get 2
    i32.const 0
    call 3
    i32.store offset=4
    local.get 2
    i32.const 1
    i32.or
    nop
    local.get 2
    nop
    drop
    local.tee 2
    nop
    local.get 8
    call 5
    drop
    local.set 8
    i32.const 48
    call 2
    nop
    local.get 2
    nop
    drop
    local.tee 2
    i32.const 9
    local.get 2
    global.get 0
    i32.const 23
    i32.add
    local.get 2
    i32.const 1
    i32.store
    i32.store offset=4
    i32.store offset=8
    local.get 2
    i32.const 5
    i32.or
    nop
    local.get 2
    nop
    drop
    local.tee 2
    nop
    local.get 9
    call 5
    drop
    local.set 9
    i32.const 32
    call 2
    nop
    local.get 2
    nop
    drop
    local.tee 2
    i32.const 5
    local.get 2
    global.get 0
    i32.const 24
    i32.add
    local.get 2
    i32.const 3
    i32.store
    i32.store offset=4
    i32.store offset=8
    local.get 2
    i32.const 5
    i32.or
    nop
    local.get 2
    nop
    drop
    local.tee 2
    nop
    local.get 10
    call 5
    drop
    local.set 10
    local.get 9
    i32.const 5
    i32.xor
    nop
    local.get 2
    nop
    drop
    local.set 2
    local.get 2
    local.get 0
    i32.load offset=12
    call 3
    i32.store offset=12
    local.get 2
    local.get 1
    call 3
    i32.store offset=16
    local.get 2
    local.get 0
    i32.load offset=20
    call 3
    i32.store offset=20
    local.get 2
    local.get 0
    i32.load offset=24
    call 3
    i32.store offset=24
    local.get 2
    local.get 7
    call 3
    i32.store offset=28
    local.get 2
    local.get 8
    call 3
    i32.store offset=32
    local.get 2
    local.get 5
    call 3
    i32.store offset=36
    local.get 2
    local.get 6
    call 3
    i32.store offset=40
    local.get 2
    local.get 9
    call 3
    i32.store offset=44
    local.get 10
    i32.const 5
    i32.xor
    nop
    local.get 2
    nop
    drop
    local.set 2
    local.get 2
    local.get 7
    call 3
    i32.store offset=12
    local.get 2
    local.get 8
    call 3
    i32.store offset=16
    local.get 2
    local.get 5
    call 3
    i32.store offset=20
    local.get 2
    local.get 6
    call 3
    i32.store offset=24
    local.get 2
    local.get 9
    call 3
    i32.store offset=28
    local.get 0
    i32.load offset=16
    i32.const 5
    i32.xor
    local.get 1
    i32.const 2
    local.get 0
    i32.load offset=16
    i32.const 5
    i32.xor
    i32.load offset=4
    call_indirect (type 1)
    call 3
    local.get 11
    call 5
    drop
    local.set 11
    local.get 11
    i32.const 31
    i32.shr_u
    if (result i32)  ;; label = @1
      i32.const 2
    else
      local.get 10
      i32.const 5
      i32.xor
      i32.const 0
      i32.const 2
      local.get 10
      i32.const 5
      i32.xor
      i32.load offset=4
      call_indirect (type 1)
    end
    nop
    local.get 2
    nop
    drop
    local.set 2
    local.get 2
    call 3
    drop
    local.get 5
    nop
    call 9
    drop
    local.get 6
    nop
    call 9
    drop
    local.get 7
    nop
    call 9
    drop
    local.get 8
    nop
    call 9
    drop
    local.get 9
    nop
    call 9
    drop
    local.get 10
    nop
    call 9
    drop
    local.get 11
    nop
    call 9
    drop
    local.get 12
    nop
    call 9
    drop
    local.get 13
    nop
    call 9
    drop
    local.get 2
    nop
    call 9
    return)
  (func (;22;) (type 1) (param i32 i32 i32) (result i32)
    (local i32 i32 i64 i32 i32 i32)
    local.get 0
    i32.load offset=12
    i32.const 5
    i32.xor
    local.get 1
    local.get 2
    local.get 0
    i32.load offset=12
    i32.const 5
    i32.xor
    i32.load offset=4
    call_indirect (type 1)
    call 3
    local.get 6
    call 5
    drop
    local.set 6
    local.get 0
    i32.load offset=16
    i32.const 5
    i32.xor
    local.get 6
    i32.const 2
    local.get 0
    i32.load offset=16
    i32.const 5
    i32.xor
    i32.load offset=4
    call_indirect (type 1)
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    call 3
    drop
    local.get 6
    nop
    call 9
    drop
    local.get 7
    nop
    call 9
    drop
    local.get 8
    nop
    call 9
    drop
    local.get 3
    nop
    call 9
    return)
  (func (;23;) (type 2) (param i32) (result i32)
    (local i32 i32 i64 i32 i32 i32 i32 i32 i32 i32)
    local.get 0
    i32.load offset=36
    i32.const 1
    i32.xor
    i32.load offset=4
    call 3
    local.get 4
    call 5
    drop
    local.set 4
    local.get 0
    i32.load offset=40
    i32.const 1
    i32.xor
    i32.load offset=4
    call 3
    local.get 5
    call 5
    drop
    local.set 5
    local.get 0
    i32.load offset=20
    i32.const 5
    i32.xor
    local.get 5
    local.get 0
    i32.load offset=16
    local.get 0
    i32.load offset=20
    i32.const 5
    i32.xor
    i32.load offset=4
    call_indirect (type 1)
    call 3
    local.get 6
    call 5
    drop
    local.set 6
    local.get 6
    i32.const 31
    i32.shr_u
    if (result i32)  ;; label = @1
      local.get 0
      i32.load offset=12
      i32.const 5
      i32.xor
      local.get 0
      i32.load offset=16
      local.get 5
      local.get 0
      i32.load offset=12
      i32.const 5
      i32.xor
      i32.load offset=4
      call_indirect (type 1)
      call 3
      local.get 7
      call 5
      drop
      local.set 7
      local.get 7
      i32.const 31
      i32.shr_u
      if (result i32)  ;; label = @2
        local.get 0
        i32.load offset=24
        i32.const 5
        i32.xor
        local.get 4
        i32.const 2
        local.get 0
        i32.load offset=24
        i32.const 5
        i32.xor
        i32.load offset=4
        call_indirect (type 1)
      else
        local.get 4
      end
      call 3
      local.get 8
      call 5
      drop
      local.set 8
      local.get 0
      i32.load offset=24
      i32.const 5
      i32.xor
      local.get 5
      i32.const 2
      local.get 0
      i32.load offset=24
      i32.const 5
      i32.xor
      i32.load offset=4
      call_indirect (type 1)
      call 3
      local.get 9
      call 5
      drop
      local.set 9
      local.get 0
      i32.load offset=36
      i32.const 1
      i32.xor
      nop
      local.get 1
      nop
      drop
      local.set 1
      local.get 1
      local.get 8
      call 3
      local.get 1
      i32.load offset=4
      call 5
      drop
      i32.store offset=4
      local.get 8
      drop
      local.get 0
      i32.load offset=40
      i32.const 1
      i32.xor
      nop
      local.get 1
      nop
      drop
      local.set 1
      local.get 1
      local.get 9
      call 3
      local.get 1
      i32.load offset=4
      call 5
      drop
      i32.store offset=4
      local.get 9
      drop
      local.get 0
      i32.load offset=28
      i32.const 1
      i32.xor
      nop
      local.get 1
      nop
      drop
      local.set 1
      local.get 1
      i32.const -1
      call 3
      local.get 1
      i32.load offset=4
      call 5
      drop
      i32.store offset=4
      i32.const -1
      drop
      local.get 0
      i32.load offset=32
      i32.const 1
      i32.xor
      nop
      local.get 1
      nop
      drop
      local.set 1
      local.get 1
      local.get 0
      i32.load offset=44
      call 3
      local.get 1
      i32.load offset=4
      call 5
      drop
      i32.store offset=4
      local.get 0
      i32.load offset=44
      drop
      i32.const -1
    else
      local.get 4
    end
    nop
    local.get 1
    nop
    drop
    local.set 1
    local.get 1
    call 3
    drop
    local.get 4
    nop
    call 9
    drop
    local.get 5
    nop
    call 9
    drop
    local.get 6
    nop
    call 9
    drop
    local.get 7
    nop
    call 9
    drop
    local.get 8
    nop
    call 9
    drop
    local.get 9
    nop
    call 9
    drop
    local.get 10
    nop
    call 9
    drop
    local.get 1
    nop
    call 9
    return)
  (func (;24;) (type 1) (param i32 i32 i32) (result i32)
    (local i32 i32 i64 i32 i32 i32)
    local.get 0
    i32.load offset=20
    i32.const 1
    i32.xor
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    local.get 1
    call 3
    local.get 3
    i32.load offset=4
    call 5
    drop
    i32.store offset=4
    local.get 1
    drop
    local.get 0
    i32.load offset=24
    i32.const 1
    i32.xor
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    local.get 2
    call 3
    local.get 3
    i32.load offset=4
    call 5
    drop
    i32.store offset=4
    local.get 2
    drop
    local.get 0
    i32.load offset=12
    i32.const 1
    i32.xor
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    i32.const -1
    call 3
    local.get 3
    i32.load offset=4
    call 5
    drop
    i32.store offset=4
    i32.const -1
    drop
    local.get 0
    i32.load offset=16
    i32.const 1
    i32.xor
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    local.get 0
    i32.load offset=28
    call 3
    local.get 3
    i32.load offset=4
    call 5
    drop
    i32.store offset=4
    local.get 0
    i32.load offset=28
    drop
    i32.const 16
    call 2
    nop
    local.get 3
    nop
    drop
    local.tee 3
    i32.const 1
    i32.store
    local.get 3
    i32.const 0
    call 3
    i32.store offset=4
    local.get 3
    i32.const 1
    i32.or
    nop
    local.get 3
    nop
    drop
    local.tee 3
    nop
    local.get 6
    call 5
    drop
    local.set 6
    block (result i32)  ;; label = @1
      loop (result i32)  ;; label = @2
        i32.const 1879048191
        local.get 0
        i32.load offset=12
        i32.const 1
        i32.xor
        i32.load offset=4
        i32.const 31
        i32.shr_u
        i32.eqz
        br_if 1 (;@1;)
        call 9
        drop
        local.get 0
        i32.load offset=12
        i32.const 1
        i32.xor
        nop
        local.get 3
        nop
        drop
        local.set 3
        local.get 3
        i32.const 2147483647
        call 3
        local.get 3
        i32.load offset=4
        call 5
        drop
        i32.store offset=4
        i32.const 2147483647
        drop
        local.get 0
        i32.load offset=16
        i32.const 1
        i32.xor
        i32.load offset=4
        call 3
        local.get 7
        call 5
        drop
        local.set 7
        local.get 7
        i32.const 5
        i32.xor
        local.get 7
        i32.const 5
        i32.xor
        i32.load offset=4
        call_indirect (type 2)
        call 3
        local.get 8
        call 5
        drop
        local.set 8
        local.get 6
        i32.const 1
        i32.xor
        nop
        local.get 3
        nop
        drop
        local.set 3
        local.get 3
        local.get 8
        call 3
        local.get 3
        i32.load offset=4
        call 5
        drop
        i32.store offset=4
        local.get 8
        br 0 (;@2;)
      end
    end
    drop
    local.get 6
    i32.const 1
    i32.xor
    i32.load offset=4
    nop
    local.get 3
    nop
    drop
    local.set 3
    local.get 3
    call 3
    drop
    local.get 6
    nop
    call 9
    drop
    local.get 7
    nop
    call 9
    drop
    local.get 8
    nop
    call 9
    drop
    local.get 3
    nop
    call 9
    return)
  (func (;25;) (type 2) (param i32) (result i32)
    global.get 2
    local.get 0
    i32.add
    global.set 2
    global.get 2)
  (func (;26;) (type 0) (result i32)
    (local i32 i32 i64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    call 10
    nop
    global.get 16
    call 5
    drop
    global.set 16
    call 11
    nop
    global.get 17
    call 5
    drop
    global.set 17
    call 12
    nop
    global.get 18
    call 5
    drop
    global.set 18
    call 13
    nop
    global.get 19
    call 5
    drop
    global.set 19
    i32.const 16
    call 2
    nop
    local.get 0
    nop
    drop
    local.tee 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 0
    call 3
    i32.store offset=4
    local.get 0
    i32.const 1
    i32.or
    nop
    local.get 0
    nop
    drop
    local.tee 0
    nop
    local.get 3
    call 5
    drop
    local.set 3
    i32.const 16
    call 2
    nop
    local.get 0
    nop
    drop
    local.tee 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 0
    call 3
    i32.store offset=4
    local.get 0
    i32.const 1
    i32.or
    nop
    local.get 0
    nop
    drop
    local.tee 0
    nop
    local.get 4
    call 5
    drop
    local.set 4
    i32.const 16
    call 2
    nop
    local.get 0
    nop
    drop
    local.tee 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 0
    call 3
    i32.store offset=4
    local.get 0
    i32.const 1
    i32.or
    nop
    local.get 0
    nop
    drop
    local.tee 0
    nop
    local.get 5
    call 5
    drop
    local.set 5
    i32.const 16
    call 2
    nop
    local.get 0
    nop
    drop
    local.tee 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 0
    call 3
    i32.store offset=4
    local.get 0
    i32.const 1
    i32.or
    nop
    local.get 0
    nop
    drop
    local.tee 0
    nop
    local.get 6
    call 5
    drop
    local.set 6
    i32.const 40
    call 2
    nop
    local.get 0
    nop
    drop
    local.tee 0
    i32.const 6
    local.get 0
    global.get 0
    i32.const 18
    i32.add
    local.get 0
    i32.const 1
    i32.store
    i32.store offset=4
    i32.store offset=8
    local.get 0
    i32.const 5
    i32.or
    nop
    local.get 0
    nop
    drop
    local.tee 0
    nop
    local.get 7
    call 5
    drop
    local.set 7
    i32.const 32
    call 2
    nop
    local.get 0
    nop
    drop
    local.tee 0
    i32.const 5
    local.get 0
    global.get 0
    i32.const 19
    i32.add
    local.get 0
    i32.const 3
    i32.store
    i32.store offset=4
    i32.store offset=8
    local.get 0
    i32.const 5
    i32.or
    nop
    local.get 0
    nop
    drop
    local.tee 0
    nop
    local.get 8
    call 5
    drop
    local.set 8
    local.get 7
    i32.const 5
    i32.xor
    nop
    local.get 0
    nop
    drop
    local.set 0
    local.get 0
    global.get 19
    call 3
    i32.store offset=12
    local.get 0
    local.get 5
    call 3
    i32.store offset=16
    local.get 0
    local.get 6
    call 3
    i32.store offset=20
    local.get 0
    local.get 3
    call 3
    i32.store offset=24
    local.get 0
    local.get 4
    call 3
    i32.store offset=28
    local.get 0
    local.get 7
    call 3
    i32.store offset=32
    local.get 8
    i32.const 5
    i32.xor
    nop
    local.get 0
    nop
    drop
    local.set 0
    local.get 0
    local.get 5
    call 3
    i32.store offset=12
    local.get 0
    local.get 6
    call 3
    i32.store offset=16
    local.get 0
    local.get 3
    call 3
    i32.store offset=20
    local.get 0
    local.get 4
    call 3
    i32.store offset=24
    local.get 0
    local.get 7
    call 3
    i32.store offset=28
    i32.const 24
    call 2
    nop
    local.get 0
    nop
    drop
    local.tee 0
    i32.const 2
    local.get 0
    global.get 0
    i32.const 22
    i32.add
    local.get 0
    i32.const 3
    i32.store
    i32.store offset=4
    i32.store offset=8
    local.get 0
    i32.const 5
    i32.or
    nop
    local.get 0
    nop
    drop
    local.tee 0
    nop
    local.get 9
    call 5
    drop
    local.set 9
    local.get 9
    i32.const 5
    i32.xor
    nop
    local.get 0
    nop
    drop
    local.set 0
    local.get 0
    local.get 8
    call 3
    i32.store offset=12
    local.get 0
    global.get 19
    call 3
    i32.store offset=16
    i32.const 40
    call 2
    nop
    local.get 0
    nop
    drop
    local.tee 0
    i32.const 4
    local.get 0
    global.get 0
    i32.const 21
    i32.add
    local.get 0
    i32.const 2
    i32.store
    i32.store offset=4
    i32.store offset=8
    local.get 0
    i32.const 5
    i32.or
    nop
    local.get 0
    nop
    drop
    local.tee 0
    nop
    local.get 10
    call 5
    drop
    local.set 10
    local.get 10
    i32.const 5
    i32.xor
    nop
    local.get 0
    nop
    drop
    local.set 0
    local.get 0
    local.get 9
    call 3
    i32.store offset=12
    local.get 0
    global.get 19
    call 3
    i32.store offset=16
    local.get 0
    global.get 18
    call 3
    i32.store offset=20
    local.get 0
    global.get 17
    call 3
    i32.store offset=24
    local.get 10
    i32.const 5
    i32.xor
    i32.const 20
    local.get 10
    i32.const 5
    i32.xor
    i32.load offset=4
    call_indirect (type 3)
    call 3
    local.get 11
    call 5
    drop
    local.set 11
    global.get 16
    i32.const 5
    i32.xor
    local.get 11
    global.get 16
    i32.const 5
    i32.xor
    i32.load offset=4
    call_indirect (type 3)
    drop
    local.get 10
    call 3
    global.get 20
    call 5
    drop
    global.set 20
    i32.const 1879048191
    nop
    local.get 0
    nop
    drop
    local.set 0
    local.get 0
    call 3
    drop
    local.get 3
    nop
    call 9
    drop
    local.get 4
    nop
    call 9
    drop
    local.get 5
    nop
    call 9
    drop
    local.get 6
    nop
    call 9
    drop
    local.get 7
    nop
    call 9
    drop
    local.get 8
    nop
    call 9
    drop
    local.get 9
    nop
    call 9
    drop
    local.get 10
    nop
    call 9
    drop
    local.get 11
    nop
    call 9
    drop
    local.get 12
    nop
    call 9
    drop
    local.get 0
    nop
    call 9
    return)
  (func (;27;) (type 0) (result i32)
    global.get 0
    call 9
    drop
    global.get 1
    call 9
    drop
    global.get 2
    call 9
    drop
    global.get 3
    call 9
    drop
    global.get 4
    call 9
    drop
    global.get 5
    call 9
    drop
    global.get 6
    call 9
    drop
    global.get 7
    call 9
    drop
    global.get 8
    call 9
    drop
    global.get 9
    call 9
    drop
    global.get 10
    call 9
    drop
    global.get 11
    call 9
    drop
    global.get 12
    call 9
    drop
    global.get 13
    call 9
    drop
    global.get 14
    call 9
    drop
    global.get 15
    call 9
    drop
    global.get 16
    call 9
    drop
    global.get 17
    call 9
    drop
    global.get 18
    call 9
    drop
    global.get 19
    call 9
    drop
    global.get 20
    call 9
    drop
    global.get 21
    call 9
    drop
    global.get 22
    call 9
    drop
    global.get 23
    call 9
    drop
    global.get 24
    call 9
    drop
    i32.const 0
    return)
  (global (;2;) (mut i32) (i32.const 0))
  (global (;3;) (mut i32) (i32.const 0))
  (global (;4;) (mut i32) (i32.const 0))
  (global (;5;) (mut i32) (i32.const 0))
  (global (;6;) (mut i32) (i32.const 0))
  (global (;7;) (mut i32) (i32.const 0))
  (global (;8;) (mut i32) (i32.const 0))
  (global (;9;) (mut i32) (i32.const 0))
  (global (;10;) (mut i32) (i32.const 0))
  (global (;11;) (mut i32) (i32.const 0))
  (global (;12;) (mut i32) (i32.const 0))
  (global (;13;) (mut i32) (i32.const 0))
  (global (;14;) (mut i32) (i32.const 0))
  (global (;15;) (mut i32) (i32.const 0))
  (global (;16;) (mut i32) (i32.const 0))
  (global (;17;) (mut i32) (i32.const 0))
  (global (;18;) (mut i32) (i32.const 0))
  (global (;19;) (mut i32) (i32.const 0))
  (global (;20;) (mut i32) (i32.const 0))
  (global (;21;) (mut i32) (i32.const 0))
  (global (;22;) (mut i32) (i32.const 0))
  (global (;23;) (mut i32) (i32.const 0))
  (global (;24;) (mut i32) (i32.const 0))
  (global (;25;) (mut i32) (i32.const 0))
  (global (;26;) (mut i32) (i32.const 0))
  (global (;27;) (mut i32) (i32.const 0))
  (global (;28;) i32 (i32.const 27))
  (export "GRAIN$LAM_0" (func 14))
  (export "GRAIN$LAM_1" (func 15))
  (export "GRAIN$LAM_2" (func 16))
  (export "GRAIN$LAM_3" (func 17))
  (export "GRAIN$LAM_4" (func 18))
  (export "GRAIN$LAM_5" (func 19))
  (export "GRAIN$LAM_6" (func 20))
  (export "GRAIN$LAM_7" (func 21))
  (export "GRAIN$LAM_8" (func 22))
  (export "GRAIN$LAM_9" (func 23))
  (export "GRAIN$LAM_10" (func 24))
  (export "GRAIN$EXPORT$GET$+" (func 15))
  (export "GRAIN$EXPORT$GET$==" (func 17))
  (export "GRAIN$EXPORT$GET$print" (func 14))
  (export "GRAIN$EXPORT$GET$phi" (func 20))
  (export "GRAIN$EXPORT$GET$<" (func 16))
  (export "GRAIN$HEAP_ADJUST" (func 25))
  (export "_start" (func 26))
  (export "GRAIN$CLEANUP_GLOBALS" (func 27))
  (export "GRAIN$TABLE_SIZE" (global 28))
  (export "memory" (memory 0))
  (elem (;0;) (global.get 0) func 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
