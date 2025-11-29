; Sudoku Solver in x86_64 NASM for macOS
; Links with C library for I/O

global _main
extern _printf, _fopen, _fscanf, _fclose, _exit

section .data
    fmt_str db "%s", 0
    fmt_int db "%d", 0
    fmt_space db "%d ", 0
    fmt_newline db 10, 0
    fmt_puzzle db "Puzzle:", 10, 0
    fmt_solved db 10, "Solved in Iterations=%d", 10, 10, 0
    fmt_nosol db "No solution found", 10, 0
    fmt_time db "Seconds to process 0.000", 10, 0 ; Placeholder for time
    mode_r db "r", 0
    
    puzzle times 81 dd 0
    count dd 0
    
section .bss
    file_handle resq 1
    val_temp resd 1

section .text

_main:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    
    ; argc in rdi, argv in rsi
    mov r12, rdi ; argc
    mov r13, rsi ; argv
    
    ; Loop through args
    mov r14, 1 ; index
    
.loop_args:
    cmp r14, r12
    jge .end_main
    
    mov rdi, [r13 + r14*8] ; argv[i]
    call _process_file
    
    inc r14
    jmp .loop_args
    
.end_main:
    xor eax, eax
    leave
    ret

_process_file:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    
    mov rbx, rdi ; filename
    
    ; Print filename
    mov rdi, fmt_str
    mov rsi, rbx
    xor eax, eax
    call _printf
    
    mov rdi, fmt_newline
    xor eax, eax
    call _printf
    
    ; Open file
    mov rdi, rbx
    mov rsi, mode_r
    call _fopen
    test rax, rax
    jz .ret
    mov [file_handle], rax
    
    ; Read file
    xor r12, r12 ; index 0..80
.read_loop:
    cmp r12, 81
    jge .read_done
    
    mov rdi, [file_handle]
    mov rsi, fmt_int
    mov rdx, val_temp
    xor eax, eax
    call _fscanf
    
    cmp eax, 1
    jne .read_skip ; Skip if failed (e.g. comments/whitespace handled by fscanf?)
                   ; Actually fscanf handles whitespace but not # comments easily.
                   ; We assume input is pre-cleaned or simple.
                   ; For robustness, we should rely on the wrapper script to clean input.
    
    mov eax, [val_temp]
    mov [puzzle + r12*4], eax
    inc r12
    jmp .read_loop
    
.read_skip:
    ; If fscanf failed, maybe EOF or bad input. 
    ; In a real solver we'd handle this better.
    ; For now, assume wrapper cleans it.
    jmp .read_loop

.read_done:
    mov rdi, [file_handle]
    call _fclose
    
    call _print_puzzle
    
    mov dword [count], 0
    call _solve
    test eax, eax
    jz .not_solved
    
    call _print_puzzle
    mov rdi, fmt_solved
    mov esi, [count]
    xor eax, eax
    call _printf
    jmp .ret
    
.not_solved:
    mov rdi, fmt_nosol
    xor eax, eax
    call _printf

.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret

_print_puzzle:
    push rbp
    mov rbp, rsp
    push r12
    push r13
    
    mov rdi, fmt_puzzle
    xor eax, eax
    call _printf
    
    xor r12, r12 ; row
.p_row:
    cmp r12, 9
    jge .p_done
    
    xor r13, r13 ; col
.p_col:
    cmp r13, 9
    jge .p_row_done
    
    ; index = r12*9 + r13
    mov rax, r12
    imul rax, 9
    add rax, r13
    
    mov rdi, fmt_space
    mov esi, [puzzle + rax*4]
    xor eax, eax
    call _printf
    
    inc r13
    jmp .p_col
    
.p_row_done:
    mov rdi, fmt_newline
    xor eax, eax
    call _printf
    inc r12
    jmp .p_row
    
.p_done:
    pop r13
    pop r12
    leave
    ret

_is_possible:
    ; rdi = r, rsi = c, rdx = val
    push rbp
    mov rbp, rsp
    
    mov r8, rdi ; r
    mov r9, rsi ; c
    mov r10, rdx ; val
    
    ; Check Row
    xor rcx, rcx
.check_row:
    cmp rcx, 9
    jge .check_col_start
    
    ; puzzle[r*9 + rcx]
    mov rax, r8
    imul rax, 9
    add rax, rcx
    cmp [puzzle + rax*4], r10d
    je .false
    
    inc rcx
    jmp .check_row

.check_col_start:
    xor rcx, rcx
.check_col:
    cmp rcx, 9
    jge .check_box_start
    
    ; puzzle[rcx*9 + c]
    mov rax, rcx
    imul rax, 9
    add rax, r9
    cmp [puzzle + rax*4], r10d
    je .false
    
    inc rcx
    jmp .check_col

.check_box_start:
    ; r0 = (r/3)*3
    mov rax, r8
    xor rdx, rdx
    mov rbx, 3
    div rbx
    imul rax, 3
    mov r11, rax ; r0
    
    ; c0 = (c/3)*3
    mov rax, r9
    xor rdx, rdx
    mov rbx, 3
    div rbx
    imul rax, 3
    mov rbx, rax ; c0 (reuse rbx)
    
    xor rcx, rcx ; i
.box_i:
    cmp rcx, 3
    jge .true
    
    xor rdx, rdx ; j
.box_j:
    cmp rdx, 3
    jge .box_i_next
    
    ; puzzle[(r0+i)*9 + (c0+j)]
    mov rax, r11
    add rax, rcx
    imul rax, 9
    add rax, rbx
    add rax, rdx
    
    cmp [puzzle + rax*4], r10d
    je .false
    
    inc rdx
    jmp .box_j
    
.box_i_next:
    inc rcx
    jmp .box_i

.true:
    mov eax, 1
    leave
    ret
.false:
    xor eax, eax
    leave
    ret

_solve:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    
    ; Find empty
    xor r12, r12 ; r
.find_r:
    cmp r12, 9
    jge .solved
    
    xor r13, r13 ; c
.find_c:
    cmp r13, 9
    jge .find_r_next
    
    ; if puzzle[r*9+c] == 0
    mov rax, r12
    imul rax, 9
    add rax, r13
    mov rbx, rax ; index
    
    cmp dword [puzzle + rbx*4], 0
    je .found_empty
    
    inc r13
    jmp .find_c
    
.find_r_next:
    inc r12
    jmp .find_r

.found_empty:
    ; r12=r, r13=c, rbx=index
    mov r14, 1 ; val
.try_loop:
    cmp r14, 9
    jg .backtrack
    
    inc dword [count]
    
    mov rdi, r12
    mov rsi, r13
    mov rdx, r14
    call _is_possible
    test eax, eax
    jz .try_next
    
    mov [puzzle + rbx*4], r14d
    call _solve
    test eax, eax
    jnz .return_true
    
    mov dword [puzzle + rbx*4], 0
    
.try_next:
    inc r14
    jmp .try_loop

.backtrack:
    xor eax, eax
    jmp .ret

.solved:
    mov eax, 1
    jmp .ret

.return_true:
    mov eax, 1

.ret:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
