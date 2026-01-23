// Sudoku Solver in ARM64 Assembly (AArch64)
// Uses C library functions: fopen, fclose, fgetc, printf

.global _main
.global _solve
.global _is_valid
.global _print_board

.extern _fopen
.extern _fclose
.extern _fgetc
.extern _printf
.extern _exit

.data
    .align 3
    fmt_solved: .asciz "Solved in Iterations= %ld\n"
    fmt_nosol:  .asciz "No solution found.\n"
    fmt_err:    .asciz "Error opening file.\n"
    fmt_char:   .asciz "%c"
    fmt_newline: .asciz "\n"
    fmt_space:  .asciz " "
    mode_r:     .asciz "r"
    
    // Global variables
    .align 3
    iterations: .quad 0
    file_handle: .quad 0
    board: .fill 81, 1, 0

.text

// ----------------------------------------------------------------
// main(argc, argv)
// ----------------------------------------------------------------
_main:
    // Save fp, lr, and callee-saved x19, x20
    stp x29, x30, [sp, -32]!
    mov x29, sp
    stp x19, x20, [sp, 16]

    // Check args (argc >= 2)
    cmp x0, 2
    b.lt .exit_err

    // Open file: fopen(argv[1], "r")
    ldr x0, [x1, 8]        // argv[1]
    adrp x1, mode_r@PAGE
    add x1, x1, mode_r@PAGEOFF
    bl _fopen
    cbz x0, .open_err      // if NULL, print error and exit

    // Store file handle
    adrp x1, file_handle@PAGE
    add x1, x1, file_handle@PAGEOFF
    str x0, [x1]

    // Read file loop
    // x19 = index (0 to 80)
    mov x19, 0

.read_loop:
    cmp x19, 81
    b.ge .read_done

    // fgetc(file_handle)
    adrp x1, file_handle@PAGE
    add x1, x1, file_handle@PAGEOFF
    ldr x0, [x1]
    bl _fgetc
    
    // Check EOF (-1)
    cmp w0, -1
    b.eq .read_done

    // Check digits '0' (48) to '9' (57)
    cmp w0, 48
    b.lt .read_loop
    cmp w0, 57
    b.gt .read_loop

    // Convert to int and store in board
    sub w0, w0, 48
    adrp x1, board@PAGE
    add x1, x1, board@PAGEOFF
    strb w0, [x1, x19]
    add x19, x19, 1
    b .read_loop

.read_done:
    // fclose(file_handle)
    adrp x1, file_handle@PAGE
    add x1, x1, file_handle@PAGEOFF
    ldr x0, [x1]
    bl _fclose

    // Call solve()
    bl _solve
    cbz x0, .no_solution

    // Print result
    // printf("Solved in Iterations= %ld\n", iterations)
    adrp x0, fmt_solved@PAGE
    add x0, x0, fmt_solved@PAGEOFF
    adrp x1, iterations@PAGE
    add x1, x1, iterations@PAGEOFF
    ldr x1, [x1]
    bl _printf

    bl _print_board
    mov x0, 0
    bl _exit

.no_solution:
    adrp x0, fmt_nosol@PAGE
    add x0, x0, fmt_nosol@PAGEOFF
    bl _printf
    mov x0, 1
    bl _exit

.open_err:
    adrp x0, fmt_err@PAGE
    add x0, x0, fmt_err@PAGEOFF
    bl _printf
    mov x0, 1
    bl _exit

.exit_err:
    mov x0, 1
    bl _exit

// ----------------------------------------------------------------
// solve() -> 1 (true) or 0 (false)
// ----------------------------------------------------------------
_solve:
    stp x29, x30, [sp, -48]!
    mov x29, sp
    stp x19, x20, [sp, 16]
    stp x21, x22, [sp, 32]

    // Find first empty cell (0)
    mov x19, 0  // index

.find_empty:
    cmp x19, 81
    b.ge .solved

    adrp x1, board@PAGE
    add x1, x1, board@PAGEOFF
    ldrb w2, [x1, x19]
    cbz w2, .found_empty
    
    add x19, x19, 1
    b .find_empty

.found_empty:
    // Try numbers 1-9
    mov x20, 1

.try_loop:
    cmp x20, 9
    b.gt .backtrack

    // Increment iterations
    adrp x2, iterations@PAGE
    add x2, x2, iterations@PAGEOFF
    ldr x3, [x2]
    add x3, x3, 1
    str x3, [x2]

    // is_valid(index, num)
    mov x0, x19
    mov x1, x20
    bl _is_valid
    cbz x0, .next_num

    // Place number
    adrp x1, board@PAGE
    add x1, x1, board@PAGEOFF
    strb w20, [x1, x19]

    // Recurse solve()
    bl _solve
    cbnz x0, .return_true

    // Backtrack: reset cell to 0
    adrp x1, board@PAGE
    add x1, x1, board@PAGEOFF
    strb wzr, [x1, x19]

.next_num:
    add x20, x20, 1
    b .try_loop

.backtrack:
    mov x0, 0
    b .return

.solved:
    mov x0, 1
    b .return

.return_true:
    mov x0, 1

.return:
    ldp x19, x20, [sp, 16]
    ldp x21, x22, [sp, 32]
    ldp x29, x30, [sp], 48
    ret

// ----------------------------------------------------------------
// is_valid(index, num) -> 1 or 0
// x0: index, x1: num
// ----------------------------------------------------------------
_is_valid:
    stp x29, x30, [sp, -16]!
    mov x29, sp

    mov x9, x0  // index
    mov x10, x1 // num

    // Calculate row = index / 9, col = index % 9
    mov x2, 9
    udiv x3, x9, x2    // x3 = row
    msub x4, x3, x2, x9 // x4 = col (index - row*9)

    // Check row
    // for(i=0; i<9; i++) if(board[row*9 + i] == num) return 0
    mov x5, 0 // i
.check_row:
    cmp x5, 9
    b.ge .check_col_setup

    mul x6, x3, x2     // row * 9
    add x6, x6, x5     // + i
    
    adrp x7, board@PAGE
    add x7, x7, board@PAGEOFF
    ldrb w8, [x7, x6]
    cmp w8, w10
    b.eq .invalid
    
    add x5, x5, 1
    b .check_row

.check_col_setup:
    // Check col
    // for(i=0; i<9; i++) if(board[i*9 + col] == num) return 0
    mov x5, 0 // i
.check_col:
    cmp x5, 9
    b.ge .check_box_setup

    mul x6, x5, x2     // i * 9
    add x6, x6, x4     // + col
    
    adrp x7, board@PAGE
    add x7, x7, board@PAGEOFF
    ldrb w8, [x7, x6]
    cmp w8, w10
    b.eq .invalid
    
    add x5, x5, 1
    b .check_col

.check_box_setup:
    // startRow = (row / 3) * 3
    mov x11, 3
    udiv x12, x3, x11
    mul x12, x12, x11  // x12 = startRow

    // startCol = (col / 3) * 3
    udiv x13, x4, x11
    mul x13, x13, x11  // x13 = startCol

    // Loop 3x3
    mov x5, 0 // i
.box_i:
    cmp x5, 3
    b.ge .valid

    mov x6, 0 // j
.box_j:
    cmp x6, 3
    b.ge .next_box_i

    // index = (startRow + i) * 9 + (startCol + j)
    add x14, x12, x5   // startRow + i
    mul x14, x14, x2   // * 9
    add x15, x13, x6   // startCol + j
    add x14, x14, x15  // final index

    adrp x7, board@PAGE
    add x7, x7, board@PAGEOFF
    ldrb w8, [x7, x14]
    cmp w8, w10
    b.eq .invalid

    add x6, x6, 1
    b .box_j

.next_box_i:
    add x5, x5, 1
    b .box_i

.valid:
    mov x0, 1
    ldp x29, x30, [sp], 16
    ret

.invalid:
    mov x0, 0
    ldp x29, x30, [sp], 16
    ret

// ----------------------------------------------------------------
// print_board()
// ----------------------------------------------------------------
_print_board:
    stp x29, x30, [sp, -32]!
    mov x29, sp
    stp x19, x20, [sp, 16]

    mov x19, 0 // row
.p_row:
    cmp x19, 9
    b.ge .p_done

    mov x20, 0 // col
.p_col:
    cmp x20, 9
    b.ge .p_next_row

    // Get value
    mov x2, 9
    mul x3, x19, x2
    add x3, x3, x20 // index
    
    adrp x4, board@PAGE
    add x4, x4, board@PAGEOFF
    ldrb w1, [x4, x3]
    
    // Print char
    add w1, w1, 48 // + '0'
    adrp x0, fmt_char@PAGE
    add x0, x0, fmt_char@PAGEOFF
    bl _printf

    // Print space if col < 8
    cmp x20, 8
    b.ge .skip_space
    
    adrp x0, fmt_space@PAGE
    add x0, x0, fmt_space@PAGEOFF
    bl _printf

.skip_space:
    add x20, x20, 1
    b .p_col

.p_next_row:
    adrp x0, fmt_newline@PAGE
    add x0, x0, fmt_newline@PAGEOFF
    bl _printf

    add x19, x19, 1
    b .p_row

.p_done:
    ldp x19, x20, [sp, 16]
    ldp x29, x30, [sp], 32
    ret