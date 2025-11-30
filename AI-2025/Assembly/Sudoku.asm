; Sudoku Solver in NASM x86_64 Assembly for macOS
; Uses libc functions for I/O

global _main
extern _printf, _fopen, _fscanf, _fclose, _exit

default rel

section .data
    fmt_str db "%s", 0
    fmt_int db "%d", 0
    fmt_int_out db "%d ", 0
    fmt_newline db 10, 0
    fmt_puzzle_header db 10, "Puzzle:", 10, 0
    fmt_solved db 10, "Solved in Iterations=%d", 10, 10, 0
    mode_r db "r", 0
    
    ; 9x9 grid of 32-bit integers
    puzzle times 81 dd 0
    count dd 0

section .bss
    file_handle resq 1
    temp_int resd 1

section .text

_main:
    push rbp
    mov rbp, rsp
    sub rsp, 16  ; Align stack

    ; Check argc (rdi)
    cmp rdi, 2
    jl .exit_error

    ; Get filename from argv[1] (rsi points to argv array)
    mov rdi, [rsi + 8] ; argv[1]
    
    ; Open file
    lea rsi, [mode_r]
    call _fopen
    test rax, rax
    jz .exit_error
    
    ; Store file handle
    lea rbx, [file_handle]
    mov [rbx], rax

    ; Read puzzle
    mov r12, 0 ; loop counter (0 to 80)
.read_loop:
    cmp r12, 81
    jge .read_done

    lea rbx, [file_handle]
    mov rdi, [rbx]
    lea rsi, [fmt_int] ; "%d " works for input too
    
    ; Calculate address of puzzle[r12]
    lea rdx, [puzzle]
    lea rdx, [rdx + r12*4]
    
    xor rax, rax
    call _fscanf

    inc r12
    jmp .read_loop

.read_done:
    lea rbx, [file_handle]
    mov rdi, [rbx]
    call _fclose

    ; Print initial puzzle
    lea rdi, [fmt_str]
    lea rsi, [fmt_puzzle_header]
    xor rax, rax
    call _printf
    
    call print_puzzle

    ; Solve
    lea rbx, [count]
    mov dword [rbx], 0
    
    mov rdi, 0 ; index 0
    call solve

    ; Print result
    test rax, rax
    jz .no_solution

    call print_puzzle
    
    lea rdi, [fmt_solved]
    lea rbx, [count]
    mov esi, [rbx]
    xor rax, rax
    call _printf

.exit:
    xor rdi, rdi
    call _exit

.no_solution:
    ; Just exit if no solution (or print something if needed)
    jmp .exit

.exit_error:
    mov rdi, 1
    call _exit

; ---------------------------------------------------------
; print_puzzle
; Prints the 9x9 grid
print_puzzle:
    push rbp
    mov rbp, rsp
    push r12
    push r13
    sub rsp, 8 ; Align

    mov r12, 0 ; row
.pp_row:
    cmp r12, 9
    jge .pp_done

    mov r13, 0 ; col
.pp_col:
    cmp r13, 9
    jge .pp_row_done

    ; Calculate index: r12*9 + r13
    mov rax, r12
    imul rax, 9
    add rax, r13
    
    lea rdi, [fmt_int_out]
    
    lea rdx, [puzzle]
    mov esi, [rdx + rax*4]
    
    xor rax, rax
    call _printf

    inc r13
    jmp .pp_col

.pp_row_done:
    lea rdi, [fmt_str]
    lea rsi, [fmt_newline]
    xor rax, rax
    call _printf

    inc r12
    jmp .pp_row

.pp_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbp
    ret

; ---------------------------------------------------------
; is_possible(index, val)
; rdi: index (0-80)
; rsi: val (1-9)
; Returns 1 (true) or 0 (false) in rax
is_possible:
    push rbp
    mov rbp, rsp
    
    ; Calculate row = index / 9, col = index % 9
    mov rax, rdi
    xor rdx, rdx
    mov rcx, 9
    div rcx
    mov r8, rax ; row
    mov r9, rdx ; col
    mov r10, rsi ; val

    ; Check row
    mov rcx, 0
.check_row:
    cmp rcx, 9
    jge .row_ok
    
    ; puzzle[row*9 + rcx]
    mov rax, r8
    imul rax, 9
    add rax, rcx
    
    lea rdx, [puzzle]
    cmp [rdx + rax*4], r10d
    je .false
    
    inc rcx
    jmp .check_row

.row_ok:
    ; Check col
    mov rcx, 0
.check_col:
    cmp rcx, 9
    jge .col_ok
    
    ; puzzle[rcx*9 + col]
    mov rax, rcx
    imul rax, 9
    add rax, r9
    
    lea rdx, [puzzle]
    cmp [rdx + rax*4], r10d
    je .false
    
    inc rcx
    jmp .check_col

.col_ok:
    ; Check 3x3 box
    ; r0 = (row / 3) * 3
    mov rax, r8
    xor rdx, rdx
    mov rcx, 3
    div rcx
    imul rax, 3
    mov r11, rax ; r0

    ; c0 = (col / 3) * 3
    mov rax, r9
    xor rdx, rdx
    mov rcx, 3
    div rcx
    imul rax, 3
    mov rbx, rax ; c0 (save in rbx, need to preserve?)
    ; rbx is callee-saved, so push/pop if used. But I can use volatile regs.
    ; Let's use rcx for c0
    mov rcx, rax ; c0

    mov rdx, 0 ; i
.box_i:
    cmp rdx, 3
    jge .true

    mov rax, 0 ; j
.box_j:
    cmp rax, 3
    jge .box_next_i

    ; puzzle[(r0+i)*9 + (c0+j)]
    mov rsi, r11
    add rsi, rdx ; r0+i
    imul rsi, 9
    add rsi, rcx ; +c0
    add rsi, rax ; +j
    
    lea rdi, [puzzle] ; use rdi as temp
    cmp [rdi + rsi*4], r10d
    je .false

    inc rax
    jmp .box_j

.box_next_i:
    inc rdx
    jmp .box_i

.true:
    mov rax, 1
    pop rbp
    ret

.false:
    xor rax, rax
    pop rbp
    ret

; ---------------------------------------------------------
; solve(index)
; rdi: index (0-80)
; Returns 1 (found) or 0 (not found)
solve:
    push rbp
    mov rbp, rsp
    push r12
    push r13
    sub rsp, 16 ; Align

    mov r12, rdi ; index

    ; If index == 81, solved
    cmp r12, 81
    je .solve_true

    ; If puzzle[index] != 0, skip
    lea rdx, [puzzle]
    cmp dword [rdx + r12*4], 0
    jne .skip

    ; Try values 1-9
    mov r13, 1 ; val
.try_loop:
    cmp r13, 9
    jg .solve_false

    ; Increment count
    lea rdx, [count]
    inc dword [rdx]

    ; Check is_possible(index, val)
    mov rdi, r12
    mov rsi, r13
    call is_possible
    test rax, rax
    jz .next_val

    ; puzzle[index] = val
    lea rdx, [puzzle]
    mov [rdx + r12*4], r13d

    ; Recurse solve(index + 1)
    mov rdi, r12
    inc rdi
    call solve
    test rax, rax
    jnz .return_true

    ; Backtrack
    lea rdx, [puzzle]
    mov dword [rdx + r12*4], 0

.next_val:
    inc r13
    jmp .try_loop

.skip:
    ; solve(index + 1)
    mov rdi, r12
    inc rdi
    call solve
    jmp .return

.solve_true:
    mov rax, 1
    jmp .return

.return_true:
    mov rax, 1
    jmp .return

.solve_false:
    xor rax, rax

.return:
    add rsp, 16
    pop r13
    pop r12
    pop rbp
    ret
