(module
 (type $FUNCSIG$iiii (func (param i32 i32 i32) (result i32)))
 (import "" "write" (func $write (param i32 i32 i32) (result i32)))
 (table 0 anyfunc)
 (memory $0 1)
 (data (i32.const 16) "hello world!\00")
 (func $main (; 1 ;) (result i32)
  (local $0 i32)
  (i32.store offset=4
   (i32.const 0)
   (tee_local $0
    (i32.sub
     (i32.load offset=4
      (i32.const 0)
     )
     (i32.const 16)
    )
   )
  )
  (i32.store8
   (i32.add
    (get_local $0)
    (i32.const 12)
   )
   (i32.load8_u offset=28
    (i32.const 0)
   )
  )
  (i32.store
   (i32.add
    (get_local $0)
    (i32.const 8)
   )
   (i32.load offset=24 align=1
    (i32.const 0)
   )
  )
  (i64.store align=4
   (get_local $0)
   (i64.load offset=16 align=1
    (i32.const 0)
   )
  )
  (drop
   (call $write
    (i32.const 0)
    (get_local $0)
    (i32.const 13)
   )
  )
  (i32.store offset=4
   (i32.const 0)
   (i32.add
    (get_local $0)
    (i32.const 16)
   )
  )
  (i32.const 0)
 )
 (func $wrap (drop (call $main)))
 (start $wrap)
)
