(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func))
  (import "_innative_internal" "print" (func (;0;) (type 0)))
  (func (;1;) (type 1)
    (local i32 i32 i32)
    i32.const 10
    local.set 0
    i32.const 104
    local.set 1
    i32.const 39
    local.set 2
    local.get 1
    call 0
    local.get 0
    call 0)
  (export "h" (func 1)))