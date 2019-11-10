(module  
  (import "!STD" "CreateWindowExA" (func $create (param i32 cref cref i32 i32 i32 i32 i32 i64 i64 i64 i64) (result i64)))
  (import "!STD" "DefWindowProcA" (func $winproc (param i64 i32 i64 i64) (result i64)))
  (import "!STD" "GetMessageA" (func $get (param cref i64 i32 i32) (result i32)))
  (import "!STD" "TranslateMessage" (func $translate (param cref) (result i32)))
  (import "!STD" "DispatchMessageA" (func $dispatch (param cref) (result i64)))
  (import "!STD" "GetModuleHandleA" (func $handle (param i64) (result i64)))
  (import "!STD" "ShowWindow" (func $show (param i64 i32) (result i32)))
  (import "!STD" "UpdateWindow" (func $update (param i64) (result i32)))
  (import "!STD" "RegisterClassA" (func $register (param cref) (result i32)))
  
  (import "" "_innative_to_c" (func $toc (param i64) (result i64)))
  (import "" "_innative_funcptr" (func $funcptr (param i32) (result i64)))
  
  (memory (;0;) 1 1)
  
  (func $caller (local $hwnd i64) (local $hinstance i64)
    (call $handle (i64.const 0))
    set_local $hinstance
    (i64.store (i32.const 1008) (call $funcptr (i32.const 1)))
    (i64.store (i32.const 1024) (get_local $hinstance))
    (i64.store (i32.const 1064) (call $toc (i64.const 64)))
    (call $register (i32.const 1000))
    drop
    i32.const 0
    i32.const 64
    i32.const 32
    i32.const 13565952
    i32.const 0x80000000
    i32.const 0x80000000
    i32.const 0x80000000
    i32.const 0x80000000
    i64.const 0
    i64.const 0
    get_local $hinstance
    i64.const 0
    call $create
    set_local $hwnd
    (call $show (get_local $hwnd) (i32.const 5))
    drop
    (call $update (get_local $hwnd))
    drop
    loop $process
    i32.const 256
    i64.const 0
    i32.const 0
    i32.const 0
    call $get
    i32.eqz
    br_if 0
    i32.const 256
    call $translate
    drop
    i32.const 256
    call $dispatch
    drop
    br $process
    end
    )
  (start $caller)
  
  (data (i32.const 64) "innative\00")
  (data (i32.const 32) "Test Window\00")
  (data (i32.const 1000) "\00")
)
