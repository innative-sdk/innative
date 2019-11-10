(module  
  (import "!STD" "GetStdHandle" (func $getstdhandle (param i32) (result cref)))
  (import "!STD" "WriteConsoleA" (func $writeconsole (param cref cref i32 cref i64) (result i32)))
  
  (memory (;0;) 1 1)
  
  (func $caller
    i32.const 12
    i32.const 12
    i32.load8_u
    i32.store8
    i32.const -11
    call $getstdhandle
    i64.const 0
    i32.const 14
    i64.const 16
    i64.const 0
    call $writeconsole
    drop)
  (start $caller)
  (data (i32.const 0)
    "Hello, world!\n\00"
  )
)
