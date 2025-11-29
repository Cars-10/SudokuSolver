(module
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory 1)
  (export "memory" (memory 0))

  (data (i32.const 0) "WebAssembly Sudoku Solver Placeholder\n")

  (func $main (export "_start")
    (call $print_hello)
  )

  (func $print_hello
    (i32.store (i32.const 100) (i32.const 0))  ;; iov.iov_base
    (i32.store (i32.const 104) (i32.const 36)) ;; iov.iov_len (length of string)
    
    (call $fd_write
      (i32.const 1)   ;; file_descriptor - 1 for stdout
      (i32.const 100) ;; *iovs - pointer to the iov array
      (i32.const 1)   ;; iovs_len - number of iovs
      (i32.const 108) ;; nwritten - pointer to store number of bytes written
    )
    drop ;; drop the result
  )
)
