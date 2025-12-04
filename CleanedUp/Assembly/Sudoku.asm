; Sudoku Solver in NASM (64-bit macOS)
; Uses C library functions: fopen, fclose, fgetc, printf

global _main
extern _fopen, _fclose, _fgetc, _printf, _exit

section .data
    fmt_solved db "Solved in Iterations= %d", 10, 0
    fmt_nosol db "No solution found.", 10, 0
    fmt_char db "%c", 0
    fmt_newline db 10, 0
    fmt_space db " ", 0
    mode_r db "r", 0
    iterations dq 0

section .bss
    board resb 81
    file_handle resq 1

section .text

_main:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    ; Check args
    cmp rdi, 2
    jl .exit_err
    
    ; Open file
    mov rdi, [rsi + 8] ; argv[1]
    lea rsi, [rel mode_r]
    call _fopen
    test rax, rax
    jz .exit_err
    mov [rel file_handle], rax

    ; Read file
    xor rbx, rbx ; index
.read_loop:
    cmp rbx, 81
    jge .read_done
    
    mov rdi, [rel file_handle]
    call _fgetc
    cmp eax, -1 ; EOF
    je .read_done
    
    cmp eax, '0'
    jl .read_loop
    cmp eax, '9'
    jg .read_loop
    
    sub eax, '0'
    lea rcx, [rel board]
    mov [rcx + rbx], al
    inc rbx
    jmp .read_loop

.read_done:
    mov rdi, [rel file_handle]
    call _fclose

    ; Solve
    call _solve
    test rax, rax
    jz .no_solution

    ; Print result
    lea rdi, [rel fmt_solved]
    mov rsi, [rel iterations]
    xor rax, rax
    call _printf
    
    call _print_board
    xor rdi, rdi
    call _exit

.no_solution:
    lea rdi, [rel fmt_nosol]
    xor rax, rax
    call _printf
    mov rdi, 1
    call _exit

.exit_err:
    mov rdi, 1
    call _exit

; ----------------------------------------------------------------
; solve() -> 1 (true) or 0 (false)
_solve:
    push rbp
    mov rbp, rsp
    push rbx ; index
    push r12 ; num
    
    ; Find empty
    xor rbx, rbx
.find_empty:
    cmp rbx, 81
    jge .solved
    lea rcx, [rel board]
    cmp byte [rcx + rbx], 0
    je .found_empty
    inc rbx
    jmp .find_empty

.found_empty:
    ; Try 1-9
    mov r12, 1
.try_loop:
    cmp r12, 9
    jg .backtrack
    
    mov rdi, rbx
    mov rsi, r12
    call _is_valid
    test rax, rax
    jz .next_num
    
    ; Place num
    lea rcx, [rel board]
    mov [rcx + rbx], r12b
    
    ; Increment iterations
    inc qword [rel iterations]
    
    call _solve
    test rax, rax
    jnz .return_true
    
    ; Reset
    lea rcx, [rel board]
    mov byte [rcx + rbx], 0

.next_num:
    inc r12
    jmp .try_loop

.backtrack:
    xor rax, rax
    jmp .return

.solved:
    mov rax, 1
    jmp .return

.return_true:
    mov rax, 1

.return:
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------
; is_valid(index, num) -> 1 or 0
_is_valid:
    push rbp
    mov rbp, rsp
    
    mov r8, rdi ; index
    mov r9, rsi ; num
    
    ; Calc row, col
    mov rax, r8
    mov rcx, 9
    xor rdx, rdx
    div rcx
    mov r10, rax ; row
    mov r11, rdx ; col
    
    ; Check row
    xor rcx, rcx
.check_row:
    cmp rcx, 9
    jge .check_col_start
    
    mov rax, r10
    imul rax, 9
    add rax, rcx ; index = row*9 + c
    lea rdx, [rel board]
    cmp byte [rdx + rax], r9b
    je .invalid
    inc rcx
    jmp .check_row

.check_col_start:
    xor rcx, rcx
.check_col:
    cmp rcx, 9
    jge .check_box_start
    
    mov rax, rcx
    imul rax, 9
    add rax, r11 ; index = r*9 + col
    lea rdx, [rel board]
    cmp byte [rdx + rax], r9b
    je .invalid
    inc rcx
    jmp .check_col

.check_box_start:
    ; startRow = (row / 3) * 3
    mov rax, r10
    mov rcx, 3
    xor rdx, rdx
    div rcx
    imul rax, 3
    mov r10, rax ; startRow
    
    ; startCol = (col / 3) * 3
    mov rax, r11
    mov rcx, 3
    xor rdx, rdx
    div rcx
    imul rax, 3
    mov r11, rax ; startCol
    
    xor rcx, rcx ; i
.box_i:
    cmp rcx, 3
    jge .valid
    
    xor rdx, rdx ; j
.box_j:
    cmp rdx, 3
    jge .next_box_i
    
    mov rax, r10
    add rax, rcx
    imul rax, 9
    add rax, r11
    add rax, rdx ; index
    lea rsi, [rel board]
    cmp byte [rsi + rax], r9b
    je .invalid
    
    inc rdx
    jmp .box_j

.next_box_i:
    inc rcx
    jmp .box_i

.valid:
    mov rax, 1
    leave
    ret

.invalid:
    xor rax, rax
    leave
    ret

; ----------------------------------------------------------------
_print_board:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    
    xor rbx, rbx ; row
.p_row:
    cmp rbx, 9
    jge .p_done
    
    xor r12, r12 ; col
.p_col:
    cmp r12, 9
    jge .p_next_row
    
    mov rax, rbx
    imul rax, 9
    add rax, r12
    lea rcx, [rel board]
    movzx rdi, byte [rcx + rax]
    add rdi, '0'
    lea rsi, [rel fmt_char]
    xor rax, rax
    ; call _printf ; Use char print
    
    ; Actually just print the char
    ; But printf needs format
    mov rsi, rdi
    lea rdi, [rel fmt_char]
    xor rax, rax
    call _printf
    
    cmp r12, 8
    jge .skip_space
    lea rdi, [rel fmt_space]
    xor rax, rax
    call _printf
.skip_space:
    
    inc r12
    jmp .p_col

.p_next_row:
    lea rdi, [rel fmt_newline]
    xor rax, rax
    call _printf
    inc rbx
    jmp .p_row

.p_done:
    pop r12
    pop rbx
    leave
    ret
