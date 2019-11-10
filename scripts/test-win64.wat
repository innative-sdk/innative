(module
  (type $t0 (func (result i32)))
  (type $t1 (func (result i32)))
  (type $t2 (func (param i64)))
  (type $t3 (func (param i32) (result i32)))
  (type $t4 (func))
  (type $t5 (func (param i32) (result i64)))
  (type $t6 (func (param i64 i64 i32 i64 i64) (result i32)))
  (type $t7 (func (param i64) (result i64)))
  
  (import "!C" "_innative_internal_env_print" (func $impfunc (type $t2)))
  (import "!STD" "GetStdHandle" (func $getstdhandle (type $t5)))
  (import "!STD" "WriteConsoleA" (func $writeconsole (type $t6)))
  (import "" "_innative_to_c" (func $toc (type $t7)))
  (import "" "_innative_from_c" (func $fromc (type $t7)))
  
  (memory (;0;) 1 1)
  
  (func $t1 (type $t0) (result i32) i32.const 1)
  (func $t2 (type $t0) (result i32) i32.const 2)
  (func $t3 (type $t0) (result i32) i32.const 3)
  (func $u1 (type $t1) (result i32) i32.const 4)
  (func $u2 (type $t1) (result i32) i32.const 5)
  
  (func $callt (export "callt") (type $t3) (param $i i32) (result i32)
    get_local $i
    call_indirect (type $t0))
    
  (func $callu (export "callu") (type $t3) (param $i i32) (result i32)
    get_local $i
    call_indirect (type $t1))
    
  (func $caller (type $t4)
    i32.const 12
    i32.const 12
    i32.load8_u
    i32.store8
    i32.const -11
    call $getstdhandle
    i64.const 0
    call $toc
    i32.const 14
    i64.const 16
    call $toc
    i64.const 0
    call $writeconsole
    drop
    i32.const 4
    call $callu
    i64.extend_u/i32
    call $impfunc)
  (table $T0 7 7 anyfunc)
  (start 12)
  (elem (i32.const 0) $t1 $t2 $t3 $u1 $u2 $t1 $t3)
  (data (i32.const 0)
    "Hello, world!\n\00"
  )
)
