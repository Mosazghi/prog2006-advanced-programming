(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    local.get $lhs
    local.get $rhs
    i32.add)
  (export "add" (func $add))
  ;; example for multiplication of two floats, 
  ;; without the use of named parameters
  (func (param f32 f32) (result f32)
    local.get 0
    local.get 1
    f32.mul)
  (export "mul" 1)
)





