(module $constparse 
  (type $t0 (func (param i64 i32) (result i64))) 
  (type $t1 (func (param i64) (result i64))) 
  (func $add (type $t0) (param $p0 i64) (param $p1 i32) (result i64) 
    (local $l0 i32) (local $l1 i32) (local $l2 i32) (local $l3 i64) (local $l4 i32) (local $l5 i32) (local $l6 i32) (local $l7 i32) (local $l8 i32) (local $l9 i32) (local $l10 i32) (local $l11 i32) (local $l12 i64) (local $l13 i64) (local $l14 i64) (local $l15 i64) (local $l16 i32) (local $l17 i32) (local $l18 i32) (local $l19 i32) (local $l20 i32) (local $l21 i32) (local $l22 i32) (local $l23 i32) (local $l24 i32) (local $l25 i32) (local $l26 i64) (local $l27 i64) (local $l28 i64) (local $l29 i64) (local $l30 i64) (local $l31 i64) (local $l32 i32) (local $l33 i32) (local $l34 i64) 
    global.get $g0 
    local.set $l0 
    i32.const 48 
    local.set $l1 
    local.get $l0 
    local.get $l1 
    i32.sub 
    local.set $l2 
    i64.const 0 
    local.set $l3 
    i32.const 20 
    local.set $l4 
    local.get $l2 
    local.get $l4 
    i32.add 
    local.set $l5 
    local.get $l5 
    local.set $l6 
    i32.const 36 
    local.set $l7 
    local.get $l2 
    local.get $l7 
    i32.add 
    local.set $l8 
    local.get $l8 
    local.set $l9 
    local.get $l2 
    local.get $p0 
    i64.store offset=40 
    i32.const 0 
    local.set $l10 
    local.get $l10 
    i32.load offset=1024 
    local.set $l11 
    local.get $l9 
    local.get $l11 
    i32.store 
    local.get $l2 
    local.get $l3 
    i64.store offset=24 
    local.get $l6 
    local.get $p1 
    i32.store 
    local.get $l2 
    local.get $l3 
    i64.store offset=8 
    block $B0 
      loop $L1 
        local.get $l2 
        i64.load offset=8 
        local.set $l12 
        local.get $l2 
        i64.load offset=40 
        local.set $l13 
        local.get $l12 
        local.set $l14 
        local.get $l13 
        local.set $l15 
        local.get $l14 
        local.get $l15 
        i64.lt_s 
        local.set $l16 
        i32.const 1 
        local.set $l17 
        local.get $l16 
        local.get $l17 
        i32.and 
        local.set $l18 
        local.get $l18 
        i32.eqz 
        br_if $B0 
        local.get $l2 
        i32.load offset=20 
        local.set $l19 
        i32.const 7 
        local.set $l20 
        local.get $l19 
        local.get $l20 
        i32.add 
        local.set $l21 
        i32.const -8 
        local.set $l22 
        local.get $l21 
        local.get $l22 
        i32.and 
        local.set $l23 
        i32.const 8 
        local.set $l24 
        local.get $l23 
        local.get $l24 
        i32.add 
        local.set $l25 
        local.get $l2 
        local.get $l25 
        i32.store offset=20 
        local.get $l23 
        i64.load 
        local.set $l26 
        local.get $l2 
        i64.load offset=24 
        local.set $l27 
        local.get $l27 
        local.get $l26 
        i64.add 
        local.set $l28 
        local.get $l2 
        local.get $l28 
        i64.store offset=24 
        local.get $l2 
        i64.load offset=8 
        local.set $l29 
        i64.const 1 
        local.set $l30 
        local.get $l29 
        local.get $l30 
        i64.add 
        local.set $l31 
        local.get $l2 
        local.get $l31 
        i64.store offset=8 
        br $L1 
      end 
    end 
    i32.const 20 
    local.set $l32 
    local.get $l2 
    local.get $l32 
    i32.add 
    local.set $l33 
    local.get $l33 
    drop 
    local.get $l2 
    i64.load offset=24 
    local.set $l34 
    local.get $l34 
    return ) 
  (func $test (export "test") (type $t1) (param $p0 i64) (result i64) 
    (local $l0 i32) (local $l1 i32) (local $l2 i32) (local $l3 i64) (local $l4 i32) (local $l5 i32) (local $l6 i32) (local $l7 i32) (local $l8 i32) (local $l9 i32) (local $l10 i32) (local $l11 i64) (local $l12 i32) (local $l13 i32) (local $l14 i64) (local $l15 i64) (local $l16 i64) (local $l17 i64) (local $l18 i32) (local $l19 i32) 
    global.get $g0 
    local.set $l0 
    i32.const 48 
    local.set $l1 
    local.get $l0 
    local.get $l1 
    i32.sub 
    local.set $l2 
    local.get $l2 
    global.set $g0 
    i64.const -9218868437227405313 
    local.set $l3 
    i32.const 28 
    local.set $l4 
    local.get $l2 
    local.get $l4 
    i32.add 
    local.set $l5 
    local.get $l5 
    local.set $l6 
    local.get $l2 
    local.get $p0 
    i64.store offset=40 
    i32.const 8 
    local.set $l7 
    local.get $l6 
    local.get $l7 
    i32.add 
    local.set $l8 
    i32.const 0 
    local.set $l9 
    local.get $l9 
    i32.load offset=1036 
    local.set $l10 
    local.get $l8 
    local.get $l10 
    i32.store 
    local.get $l9 
    i64.load offset=1028 align=4 
    local.set $l11 
    local.get $l6 
    local.get $l11 
    i64.store align=4 
    local.get $l2 
    local.get $l3 
    i64.store offset=16 
    local.get $l2 
    i32.load offset=32 
    local.set $l12 
    local.get $l12 
    local.set $l13 
    local.get $l13 
    i64.extend_i32_s 
    local.set $l14 
    local.get $l2 
    i64.load offset=40 
    local.set $l15 
    i64.const -9218868437227405313 
    local.set $l16 
    local.get $l2 
    local.get $l16 
    i64.store offset=8 
    local.get $l2 
    local.get $l15 
    i64.store 
    local.get $l14 
    local.get $l2 
    call $add 
    local.set $l17 
    i32.const 48 
    local.set $l18 
    local.get $l2 
    local.get $l18 
    i32.add 
    local.set $l19 
    local.get $l19 
    global.set $g0 
    local.get $l17 
    return ) 
  (table 1 1 funcref) 
  (memory (export "memory") 2) 
  (global $g0 (mut i32) (i32.const 66576)) 
  (data 0 (offset=i32.const 1024) "ABC\00\01\00\00\00\02\00\00\00\03\00\00\00"))
