(module
 (import "" "_innative_to_c" (func $toc (param i64) (result i64)))
 (import "" "_innative_syscall" (func $syscall (param i64 i64 i64 i64 i64 i64 i64) (result i64)))
 (memory $0 1)
 (data (i32.const 0) "hello world!\n\00")
 (func $main (; 1 ;) (result i64)
    i64.const 1
    i64.const 1
    i64.const 0
    call $toc
    i64.const 13
    i64.const 0
    i64.const 0
    i64.const 0
    call $syscall
 )
 (func $wrap (drop (call $main)))
 (start $wrap)
)
