(module $embedded
  (import "env" "my_factorial" (func $my_factorial (param i32) (result i32)))
  (func $test (export "test") (param $p0 i32) (param $p1 i32) (result i32)
    (i32.add
      (call $my_factorial
        (local.get $p0))
      (call $my_factorial
        (local.get $p1))))
  (table $T0 1 1 funcref)
  (memory $memory (export "memory") 2)
  (global $g0 (mut i32) (i32.const 66560)))
