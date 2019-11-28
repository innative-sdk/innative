(module
 (table 0 anyfunc)
 (memory $0 1)
 (export "memory" (memory $0))
 (export "fib" (func $_Z10vectorloopx))
 (func $_Z10vectorloopx (; 0 ;) (param $0 i64) (result i64)
  (local $1 i64)
  (local $2 i64)
  (set_local $2
   (i64.const 0)
  )
  (block $label$0
   (br_if $label$0
    (i64.lt_s
     (get_local $0)
     (i64.const 0)
    )
   )
   (set_local $0
    (i64.add
     (get_local $0)
     (i64.const 2)
    )
   )
   (set_local $2
    (i64.const 0)
   )
   (loop $label$1
    (set_local $2
     (i64.add
      (i64.add
       (get_local $0)
       (tee_local $1
        (i64.add
         (call $_Z10vectorloopx
          (i64.add
           (get_local $0)
           (i64.const -3)
          )
         )
         (get_local $2)
        )
       )
      )
      (i64.const -2)
     )
    )
    (br_if $label$1
     (i64.gt_s
      (tee_local $0
       (i64.add
        (get_local $0)
        (i64.const -2)
       )
      )
      (i64.const 1)
     )
    )
   )
   (set_local $2
    (i64.add
     (get_local $1)
     (get_local $0)
    )
   )
  )
  (get_local $2)
 )
)

