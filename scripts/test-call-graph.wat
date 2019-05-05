(module    
  (memory (;0;) 1 1)
  
  (func $left1 (export "left") (param $i i32) local.get $i call $left2)
  (func $left2 (param $i i32) local.get $i call $left3)
  (func $left3 (param $i i32) local.get $i call $left4)
  (func $left4 (param $i i32) local.get $i call $left5)
  (func $left5 (param $i i32) local.get $i (if $i (then i32.const 0 call $left1) (else i32.const 1 memory.grow drop)))
  
  (func $right1 (export "right") (param $i i32) local.get $i call $right2)
  (func $right2 (param $i i32) local.get $i call $right3)
  (func $right3 (param $i i32) local.get $i call $right4)
  (func $right4 (param $i i32) local.get $i call $right5)
  (func $right5 (param $i i32) local.get $i (if $i (then i32.const 0 call $right1)))
  
  (func $start 
    i32.const 1 
    call $left1
    )
  (start $start)
  (data (i32.const 0)
    "Hello, world!\n\00"
  )
)
