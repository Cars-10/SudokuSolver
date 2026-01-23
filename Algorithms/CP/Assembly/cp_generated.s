	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 26, 0	sdk_version 26, 2
	.globl	_readMatrixFile                 ; -- Begin function readMatrixFile
	.p2align	2
_readMatrixFile:                        ; @readMatrixFile
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #192
	stp	x28, x27, [sp, #96]             ; 16-byte Folded Spill
	stp	x26, x25, [sp, #112]            ; 16-byte Folded Spill
	stp	x24, x23, [sp, #128]            ; 16-byte Folded Spill
	stp	x22, x21, [sp, #144]            ; 16-byte Folded Spill
	stp	x20, x19, [sp, #160]            ; 16-byte Folded Spill
	stp	x29, x30, [sp, #176]            ; 16-byte Folded Spill
	add	x29, sp, #176
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	.cfi_offset w25, -72
	.cfi_offset w26, -80
	.cfi_offset w27, -88
	.cfi_offset w28, -96
	mov	x20, x1
	mov	x21, x0
Lloh0:
	adrp	x1, l_.str@PAGE
Lloh1:
	add	x1, x1, l_.str@PAGEOFF
	bl	_fopen
	cbz	x0, LBB0_3
; %bb.1:
	mov	x19, x0
Lloh2:
	adrp	x1, l_.str.2@PAGE
Lloh3:
	add	x1, x1, l_.str.2@PAGEOFF
	mov	x0, x21
	mov	w2, #14                         ; =0xe
	bl	_strncmp
	cbz	w0, LBB0_4
; %bb.2:
	mov	x0, x21
	bl	_puts
	stp	xzr, xzr, [sp, #80]
	add	x0, sp, #88
	add	x1, sp, #80
	mov	x2, x19
	bl	_getline
	tbz	x0, #63, LBB0_5
	b	LBB0_13
LBB0_3:
Lloh4:
	adrp	x8, ___stderrp@GOTPAGE
Lloh5:
	ldr	x8, [x8, ___stderrp@GOTPAGEOFF]
Lloh6:
	ldr	x0, [x8]
	str	x21, [sp]
Lloh7:
	adrp	x1, l_.str.1@PAGE
Lloh8:
	add	x1, x1, l_.str.1@PAGEOFF
	bl	_fprintf
	mov	w20, #0                         ; =0x0
	b	LBB0_15
LBB0_4:
	add	x8, x21, #5
	str	x8, [sp]
Lloh9:
	adrp	x0, l_.str.3@PAGE
Lloh10:
	add	x0, x0, l_.str.3@PAGEOFF
	bl	_printf
	stp	xzr, xzr, [sp, #80]
	add	x0, sp, #88
	add	x1, sp, #80
	mov	x2, x19
	bl	_getline
	tbnz	x0, #63, LBB0_13
LBB0_5:
	mov	w23, #0                         ; =0x0
	mov	w24, #1                         ; =0x1
	mov	x25, #9216                      ; =0x2400
	movk	x25, #8, lsl #32
	mov	w26, #36                        ; =0x24
Lloh11:
	adrp	x21, l_.str.5@PAGE
Lloh12:
	add	x21, x21, l_.str.5@PAGEOFF
Lloh13:
	adrp	x22, l_.str.6@PAGE
Lloh14:
	add	x22, x22, l_.str.6@PAGEOFF
LBB0_6:                                 ; =>This Inner Loop Header: Depth=1
	ldr	x0, [sp, #88]
	ldrb	w8, [x0]
	cmp	w8, #35
	lsl	x8, x24, x8
	and	x8, x8, x25
	ccmp	x8, #0, #4, ls
	b.eq	LBB0_9
; %bb.7:                                ;   in Loop: Header=BB0_6 Depth=1
	add	x0, sp, #88
	add	x1, sp, #80
	mov	x2, x19
	bl	_getline
	tbnz	x0, #63, LBB0_11
LBB0_8:                                 ;   in Loop: Header=BB0_6 Depth=1
	cmp	w23, #9
	b.lt	LBB0_6
	b	LBB0_11
LBB0_9:                                 ;   in Loop: Header=BB0_6 Depth=1
	smaddl	x27, w23, w26, x20
	add	x8, x27, #4
	add	x9, x27, #8
	add	x10, x27, #12
	add	x11, x27, #16
	add	x12, x27, #20
	add	x13, x27, #24
	add	x14, x27, #28
	add	x15, x27, #32
	stp	x14, x15, [sp, #56]
	stp	x12, x13, [sp, #40]
	stp	x10, x11, [sp, #24]
	stp	x8, x9, [sp, #8]
	str	x27, [sp]
	mov	x1, x21
	bl	_sscanf
	cmp	w0, #9
	b.ne	LBB0_12
; %bb.10:                               ;   in Loop: Header=BB0_6 Depth=1
	ldr	w8, [x27]
	str	x8, [sp]
	mov	x0, x22
	bl	_printf
	smaddl	x27, w23, w26, x20
	ldr	w8, [x27, #4]
	str	x8, [sp]
	mov	x0, x22
	bl	_printf
	ldr	w8, [x27, #8]
	str	x8, [sp]
	mov	x0, x22
	bl	_printf
	ldr	w8, [x27, #12]
	str	x8, [sp]
	mov	x0, x22
	bl	_printf
	ldr	w8, [x27, #16]
	str	x8, [sp]
	mov	x0, x22
	bl	_printf
	ldr	w8, [x27, #20]
	str	x8, [sp]
	mov	x0, x22
	bl	_printf
	ldr	w8, [x27, #24]
	str	x8, [sp]
	mov	x0, x22
	bl	_printf
	ldr	w8, [x27, #28]
	str	x8, [sp]
	mov	x0, x22
	bl	_printf
	ldr	w8, [x27, #32]
	str	x8, [sp]
	mov	x0, x22
	bl	_printf
	mov	w0, #10                         ; =0xa
	bl	_putchar
	add	w23, w23, #1
	add	x0, sp, #88
	add	x1, sp, #80
	mov	x2, x19
	bl	_getline
	tbz	x0, #63, LBB0_8
LBB0_11:
	cmp	w23, #9
	cset	w20, eq
	b	LBB0_14
LBB0_12:
Lloh15:
	adrp	x8, ___stderrp@GOTPAGE
Lloh16:
	ldr	x8, [x8, ___stderrp@GOTPAGEOFF]
Lloh17:
	ldr	x3, [x8]
Lloh18:
	adrp	x0, l_.str.8@PAGE
Lloh19:
	add	x0, x0, l_.str.8@PAGEOFF
	mov	w1, #40                         ; =0x28
	mov	w2, #1                          ; =0x1
	bl	_fwrite
LBB0_13:
	mov	w20, #0                         ; =0x0
LBB0_14:
	mov	x0, x19
	bl	_fclose
	ldr	x0, [sp, #88]
	bl	_free
LBB0_15:
	mov	x0, x20
	ldp	x29, x30, [sp, #176]            ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #160]            ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #144]            ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #128]            ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #112]            ; 16-byte Folded Reload
	ldp	x28, x27, [sp, #96]             ; 16-byte Folded Reload
	add	sp, sp, #192
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.loh AdrpAdd	Lloh2, Lloh3
	.loh AdrpAdd	Lloh7, Lloh8
	.loh AdrpLdrGotLdr	Lloh4, Lloh5, Lloh6
	.loh AdrpAdd	Lloh9, Lloh10
	.loh AdrpAdd	Lloh13, Lloh14
	.loh AdrpAdd	Lloh11, Lloh12
	.loh AdrpAdd	Lloh18, Lloh19
	.loh AdrpLdrGotLdr	Lloh15, Lloh16, Lloh17
	.cfi_endproc
                                        ; -- End function
	.globl	_printPuzzle                    ; -- Begin function printPuzzle
	.p2align	2
_printPuzzle:                           ; @printPuzzle
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #64
	stp	x22, x21, [sp, #16]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #32]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #48]             ; 16-byte Folded Spill
	add	x29, sp, #48
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	mov	x19, x0
Lloh20:
	adrp	x0, l_str@PAGE
Lloh21:
	add	x0, x0, l_str@PAGEOFF
	bl	_puts
	add	x20, x19, #16
	mov	w21, #9                         ; =0x9
Lloh22:
	adrp	x19, l_.str.6@PAGE
Lloh23:
	add	x19, x19, l_.str.6@PAGEOFF
LBB1_1:                                 ; =>This Inner Loop Header: Depth=1
	ldur	w8, [x20, #-16]
	str	x8, [sp]
	mov	x0, x19
	bl	_printf
	ldur	w8, [x20, #-12]
	str	x8, [sp]
	mov	x0, x19
	bl	_printf
	ldur	w8, [x20, #-8]
	str	x8, [sp]
	mov	x0, x19
	bl	_printf
	ldur	w8, [x20, #-4]
	str	x8, [sp]
	mov	x0, x19
	bl	_printf
	ldr	w8, [x20]
	str	x8, [sp]
	mov	x0, x19
	bl	_printf
	ldr	w8, [x20, #4]
	str	x8, [sp]
	mov	x0, x19
	bl	_printf
	ldr	w8, [x20, #8]
	str	x8, [sp]
	mov	x0, x19
	bl	_printf
	ldr	w8, [x20, #12]
	str	x8, [sp]
	mov	x0, x19
	bl	_printf
	ldr	w8, [x20, #16]
	str	x8, [sp]
	mov	x0, x19
	bl	_printf
	mov	w0, #10                         ; =0xa
	bl	_putchar
	add	x20, x20, #36
	subs	x21, x21, #1
	b.ne	LBB1_1
; %bb.2:
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #64
	ret
	.loh AdrpAdd	Lloh22, Lloh23
	.loh AdrpAdd	Lloh20, Lloh21
	.cfi_endproc
                                        ; -- End function
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:
	stp	x28, x27, [sp, #-48]!           ; 16-byte Folded Spill
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	add	x29, sp, #32
	sub	sp, sp, #1504
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w27, -40
	.cfi_offset w28, -48
	mov	x19, x1
	mov	x20, x0
Lloh24:
	adrp	x8, ___stack_chk_guard@GOTPAGE
Lloh25:
	ldr	x8, [x8, ___stack_chk_guard@GOTPAGEOFF]
Lloh26:
	ldr	x8, [x8]
	stur	x8, [x29, #-40]
	add	x0, sp, #504
	mov	x1, #0                          ; =0x0
	bl	_gettimeofday
	cmp	w20, #2
	b.ne	LBB2_5
; %bb.1:
	ldr	x0, [x19, #8]
	add	x1, sp, #1172
	bl	_readMatrixFile
	cbz	w0, LBB2_6
; %bb.2:
	add	x0, sp, #1172
	bl	_printPuzzle
	add	x0, sp, #16
	add	x1, sp, #1172
	bl	_init_grid
	add	x0, sp, #16
	bl	_propagate
	cbz	w0, LBB2_7
; %bb.3:
	add	x0, sp, #16
	add	x1, sp, #848
	bl	_cp_search
	cbz	w0, LBB2_8
; %bb.4:
	add	x0, sp, #520
	add	x1, sp, #848
	mov	w2, #324                        ; =0x144
	bl	_memcpy
	add	x0, sp, #520
	bl	_printPuzzle
Lloh27:
	adrp	x8, _cp_iterations@GOTPAGE
Lloh28:
	ldr	x8, [x8, _cp_iterations@GOTPAGEOFF]
Lloh29:
	ldr	x8, [x8]
	str	x8, [sp]
Lloh30:
	adrp	x0, l_.str.14@PAGE
Lloh31:
	add	x0, x0, l_.str.14@PAGEOFF
	bl	_printf
	b	LBB2_10
LBB2_5:
Lloh32:
	adrp	x8, ___stderrp@GOTPAGE
Lloh33:
	ldr	x8, [x8, ___stderrp@GOTPAGEOFF]
Lloh34:
	ldr	x0, [x8]
	ldr	x8, [x19]
	str	x8, [sp]
Lloh35:
	adrp	x1, l_.str.10@PAGE
Lloh36:
	add	x1, x1, l_.str.10@PAGEOFF
	bl	_fprintf
	mov	w19, #1                         ; =0x1
	b	LBB2_11
LBB2_6:
Lloh37:
	adrp	x8, ___stderrp@GOTPAGE
Lloh38:
	ldr	x8, [x8, ___stderrp@GOTPAGEOFF]
Lloh39:
	ldr	x3, [x8]
Lloh40:
	adrp	x0, l_.str.11@PAGE
Lloh41:
	add	x0, x0, l_.str.11@PAGEOFF
	mov	w19, #1                         ; =0x1
	mov	w1, #27                         ; =0x1b
	mov	w2, #1                          ; =0x1
	bl	_fwrite
	b	LBB2_11
LBB2_7:
Lloh42:
	adrp	x0, l_str.16@PAGE
Lloh43:
	add	x0, x0, l_str.16@PAGEOFF
	b	LBB2_9
LBB2_8:
Lloh44:
	adrp	x0, l_str.17@PAGE
Lloh45:
	add	x0, x0, l_str.17@PAGEOFF
LBB2_9:
	bl	_puts
LBB2_10:
	add	x0, sp, #520
	mov	x1, #0                          ; =0x0
	bl	_gettimeofday
	ldr	x8, [sp, #520]
	ldr	x9, [sp, #504]
	sub	x8, x8, x9
	scvtf	d0, x8
	ldr	w8, [sp, #528]
	ldr	w9, [sp, #512]
	sub	w8, w8, w9
	scvtf	d1, w8
	mov	x8, #145685290680320            ; =0x848000000000
	movk	x8, #16686, lsl #48
	fmov	d2, x8
	fdiv	d1, d1, d2
	fadd	d0, d1, d0
	str	d0, [sp]
Lloh46:
	adrp	x0, l_.str.13@PAGE
Lloh47:
	add	x0, x0, l_.str.13@PAGEOFF
	bl	_printf
	mov	w19, #0                         ; =0x0
LBB2_11:
	ldur	x8, [x29, #-40]
Lloh48:
	adrp	x9, ___stack_chk_guard@GOTPAGE
Lloh49:
	ldr	x9, [x9, ___stack_chk_guard@GOTPAGEOFF]
Lloh50:
	ldr	x9, [x9]
	cmp	x9, x8
	b.ne	LBB2_13
; %bb.12:
	mov	x0, x19
	add	sp, sp, #1504
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x28, x27, [sp], #48             ; 16-byte Folded Reload
	ret
LBB2_13:
	bl	___stack_chk_fail
	.loh AdrpLdrGotLdr	Lloh24, Lloh25, Lloh26
	.loh AdrpAdd	Lloh30, Lloh31
	.loh AdrpLdrGotLdr	Lloh27, Lloh28, Lloh29
	.loh AdrpAdd	Lloh35, Lloh36
	.loh AdrpLdrGotLdr	Lloh32, Lloh33, Lloh34
	.loh AdrpAdd	Lloh40, Lloh41
	.loh AdrpLdrGotLdr	Lloh37, Lloh38, Lloh39
	.loh AdrpAdd	Lloh42, Lloh43
	.loh AdrpAdd	Lloh44, Lloh45
	.loh AdrpAdd	Lloh46, Lloh47
	.loh AdrpLdrGotLdr	Lloh48, Lloh49, Lloh50
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__cstring,cstring_literals
l_.str:                                 ; @.str
	.asciz	"r"

l_.str.1:                               ; @.str.1
	.asciz	"Error opening file '%s'\n"

l_.str.2:                               ; @.str.2
	.asciz	"/app/Matrices/"

l_.str.3:                               ; @.str.3
	.asciz	"../%s\n"

l_.str.5:                               ; @.str.5
	.asciz	"%i %i %i %i %i %i %i %i %i"

l_.str.6:                               ; @.str.6
	.asciz	"%i "

l_.str.8:                               ; @.str.8
	.asciz	"Error: line does not contain 9 integers\n"

l_.str.10:                              ; @.str.10
	.asciz	"Usage: %s <matrix_file>\n"

l_.str.11:                              ; @.str.11
	.asciz	"Failed to read matrix file\n"

l_.str.13:                              ; @.str.13
	.asciz	"Seconds to process %.3f\n"

l_.str.14:                              ; @.str.14
	.asciz	"\nSolved in Iterations=%lld\n\n"

l_str:                                  ; @str
	.asciz	"\nPuzzle:"

l_str.16:                               ; @str.16
	.asciz	"\nNo solution found (contradiction during initial propagation)"

l_str.17:                               ; @str.17
	.asciz	"\nNo solution found"

.subsections_via_symbols
