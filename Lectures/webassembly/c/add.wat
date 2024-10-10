(module
 (type $0 (func))
 (type $1 (func (param i32 i32) (result i32)))
 (global $__stack_pointer (mut i32) (i32.const 66560))
 (global $global$1 i32 (i32.const 1024))
 (global $global$2 i32 (i32.const 1024))
 (global $global$3 i32 (i32.const 1024))
 (global $global$4 i32 (i32.const 66560))
 (global $global$5 i32 (i32.const 1024))
 (global $global$6 i32 (i32.const 66560))
 (global $global$7 i32 (i32.const 131072))
 (global $global$8 i32 (i32.const 0))
 (global $global$9 i32 (i32.const 1))
 (memory $0 2)
 (export "memory" (memory $0))
 (export "__wasm_call_ctors" (func $__wasm_call_ctors))
 (export "add" (func $add))
 (export "__dso_handle" (global $global$1))
 (export "__data_end" (global $global$2))
 (export "__stack_low" (global $global$3))
 (export "__stack_high" (global $global$4))
 (export "__global_base" (global $global$5))
 (export "__heap_base" (global $global$6))
 (export "__heap_end" (global $global$7))
 (export "__memory_base" (global $global$8))
 (export "__table_base" (global $global$9))
 (func $__wasm_call_ctors
 )
 (func $add (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local.set $2
   (global.get $__stack_pointer)
  )
  (local.set $3
   (i32.const 16)
  )
  (local.set $4
   (i32.sub
    (local.get $2)
    (local.get $3)
   )
  )
  (i32.store offset=12
   (local.get $4)
   (local.get $0)
  )
  (i32.store offset=8
   (local.get $4)
   (local.get $1)
  )
  (local.set $5
   (i32.load offset=12
    (local.get $4)
   )
  )
  (local.set $6
   (i32.load offset=8
    (local.get $4)
   )
  )
  (local.set $7
   (i32.add
    (local.get $5)
    (local.get $6)
   )
  )
  (return
   (local.get $7)
  )
 )
 ;; custom section "producers", size 108
 ;; features section: mutable-globals, sign-ext
)

