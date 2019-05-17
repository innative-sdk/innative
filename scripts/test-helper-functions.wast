(module
  (import "" "_innative_internal_write_out" (func $print (param $p cref) (param $i i64)))
  
  (memory (;0;) 1 1)
  
  (func (export "test-write")
    i32.const 0
    i64.const 14
    call $print
    )
  (data (i32.const 0)
    "Hello, world!\n\00"
  )
)

(invoke "test-write")

(module  
  (import "" "_innative_internal_env_print" (func $print (param $i i64)))
  
  (func (export "test-print")
    i64.const 14
    call $print
    )
)

(invoke "test-print")

(module
  (import "" "_innative_internal_env_memdump" (func $dump (param $p i64) (param $i i64)))
  (import "" "_innative_to_c" (func $toc (param i64) (result i64)))

  (memory (;0;) 1 1)
  
  (func (export "test-to-c")
    i64.const 0
    call $toc
    i64.const 14
    call $dump
    )
  (data (i32.const 0)
    "Hello, world!\n\00"
  )
)

(invoke "test-to-c")