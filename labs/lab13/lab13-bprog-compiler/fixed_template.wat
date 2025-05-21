(module
  ;; Generated from BPROG language
  ;; Optimization level: 1
  ;; Default integer type: i32
  ;; Default float type: f32

  ;; Type section
  (type $i32_i32_=>_i32 (func (param i32 i32) (result i32)))
  (type $i32_=>_i32 (func (param i32) (result i32)))
  (type $i32_=>_void (func (param i32)))
  (type $void_=>_i32 (func (result i32)))
  (type $f32_f32_=>_f32 (func (param f32 f32) (result f32)))
  (type $void_=>_void (func))

  ;; Import section
  (import "env" "memory" (memory 1))
  (import "env" "print_i32" (func $print_i32 (param i32)))
  (import "env" "print_f32" (func $print_f32 (param f32)))
  (import "env" "print_string" (func $print_string (param i32)))
  (import "env" "read_line" (func $read_line (result i32)))

  ;; Global section
  (global $heap_ptr (mut i32) (i32.const 1024))
  (global $stack_ptr (mut i32) (i32.const 0))

  ;; Functions
  (func $main (result i32)
    ;; Local variables
    (local $temp i32)
    (local $temp1 i32)
    (local $temp2 i32)
    (local $i i32)
    (local $len i32)
    
    ;; Push integer onto stack
    i32.const 3
    ;; Push integer onto stack
    i32.const 5
    ;; + operation
    i32.add

    ;; Return value from stack
    return
  )

  ;; Runtime support functions
  (func $malloc (param $size i32) (result i32)
    (local $addr i32)
    ;; Get current heap pointer
    global.get $heap_ptr
    local.set $addr
    ;; Update heap pointer
    global.get $heap_ptr
    local.get $size
    i32.add
    global.set $heap_ptr
    ;; Return allocated address
    local.get $addr
  )

  (func $create_list (result i32)
    (local $temp i32)
    ;; Allocate memory for list header (4 bytes for length + 4 bytes for capacity)
    i32.const 8
    call $malloc
    ;; Initialize length to 0
    local.tee $temp
    i32.const 0
    i32.store
    ;; Return list address
    local.get $temp
  )

  (func $list_add (param $list i32) (param $item i32) (result i32)
    (local $len i32)
    (local $newAddr i32)
    (local $temp i32)
    ;; Get current list length
    local.get $list
    i32.load
    local.set $len
    ;; Update length
    local.get $list
    local.get $len
    i32.const 1
    i32.add
    i32.store
    ;; Calculate address for new item
    local.get $list
    i32.const 8
    i32.add
    local.get $len
    i32.const 4
    i32.mul
    i32.add
    local.set $newAddr
    ;; Store item at address
    local.get $newAddr
    local.get $item
    i32.store
    ;; Return list address
    local.get $list
  )

  (func $parse_integer (param $str i32) (result i32)
    ;; This is a placeholder for string to integer conversion
    ;; In a real implementation, this would parse the string
    i32.const 0
  )

  (func $parse_float (param $str i32) (result f32)
    ;; This is a placeholder for string to float conversion
    ;; In a real implementation, this would parse the string
    f32.const 0
  )

  (func $split_words (param $str i32) (result i32)
    ;; This is a placeholder for splitting a string into words
    ;; In a real implementation, this would return a list of strings
    call $create_list
  )

  ;; Export section
  (export "main" (func $main))
)