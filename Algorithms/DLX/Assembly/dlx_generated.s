	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 26, 0	sdk_version 26, 2
	.globl	_get_position_col               ; -- Begin function get_position_col
	.p2align	2
_get_position_col:                      ; @get_position_col
	.cfi_startproc
; %bb.0:
	add	w8, w0, w0, lsl #3
	add	w0, w8, w1
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_get_row_col                    ; -- Begin function get_row_col
	.p2align	2
_get_row_col:                           ; @get_row_col
	.cfi_startproc
; %bb.0:
	add	w8, w0, w0, lsl #3
	add	w8, w1, w8
	add	w0, w8, #80
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_get_col_col                    ; -- Begin function get_col_col
	.p2align	2
_get_col_col:                           ; @get_col_col
	.cfi_startproc
; %bb.0:
	add	w8, w0, w0, lsl #3
	add	w8, w1, w8
	add	w0, w8, #161
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_get_box_col                    ; -- Begin function get_box_col
	.p2align	2
_get_box_col:                           ; @get_box_col
	.cfi_startproc
; %bb.0:
	mov	w8, #21846                      ; =0x5556
	movk	w8, #21845, lsl #16
	smull	x9, w0, w8
	lsr	x10, x9, #63
	lsr	x9, x9, #32
	add	w9, w9, w10
	add	w9, w9, w9, lsl #1
	smull	x8, w1, w8
	lsr	x10, x8, #63
	lsr	x8, x8, #32
	add	w8, w8, w10
	add	w8, w9, w8
	add	w8, w8, w8, lsl #3
	add	w8, w2, w8
	add	w0, w8, #242
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_init_dlx_matrix                ; -- Begin function init_dlx_matrix
	.p2align	2
_init_dlx_matrix:                       ; @init_dlx_matrix
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #80
	stp	x24, x23, [sp, #16]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #32]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #48]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #64]             ; 16-byte Folded Spill
	add	x29, sp, #64
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	mov	w0, #1                          ; =0x1
	mov	w1, #72                         ; =0x48
	bl	_calloc
	mov	x20, #0                         ; =0x0
	adrp	x21, _root@PAGE
	str	x0, [x21, _root@PAGEOFF]
	mov	w8, #28530                      ; =0x6f72
	movk	w8, #29807, lsl #16
	str	w8, [x0, #52]
	strb	wzr, [x0, #56]
	stp	x0, x0, [x0, #16]
	stp	x0, x0, [x0]
	str	x0, [x0, #32]
	mov	w22, #-1                        ; =0xffffffff
	str	w22, [x0, #40]
Lloh0:
	adrp	x19, l_.str.1@PAGE
Lloh1:
	add	x19, x19, l_.str.1@PAGEOFF
Lloh2:
	adrp	x23, _columns@GOTPAGE
Lloh3:
	ldr	x23, [x23, _columns@GOTPAGEOFF]
LBB4_1:                                 ; =>This Inner Loop Header: Depth=1
	mov	w0, #1                          ; =0x1
	mov	w1, #72                         ; =0x48
	bl	_calloc
	str	x0, [x23, x20, lsl #3]
	str	x20, [sp]
	add	x0, x0, #52
	mov	w1, #16                         ; =0x10
	mov	x2, x19
	bl	_snprintf
	ldr	x8, [x23, x20, lsl #3]
	str	wzr, [x8, #48]
	stp	x8, x8, [x8]
	str	x8, [x8, #32]
	str	w22, [x8, #40]
	ldr	x9, [x21, _root@PAGEOFF]
	ldr	x10, [x9, #16]
	stp	x10, x9, [x8, #16]
	str	x8, [x10, #24]
	str	x8, [x9, #16]
	add	x20, x20, #1
	cmp	x20, #324
	b.ne	LBB4_1
; %bb.2:
Lloh4:
	adrp	x8, _max_nodes@PAGE
Lloh5:
	ldrsw	x8, [x8, _max_nodes@PAGEOFF]
	add	x8, x8, x8, lsl #1
	lsl	x19, x8, #4
	mov	x0, x19
	bl	_malloc
	adrp	x8, _nodes@PAGE
	str	x0, [x8, _nodes@PAGEOFF]
	mov	x1, x19
	bl	_bzero
	adrp	x8, _node_count@PAGE
	str	wzr, [x8, _node_count@PAGEOFF]
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #32]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #80
	ret
	.loh AdrpLdrGot	Lloh2, Lloh3
	.loh AdrpAdd	Lloh0, Lloh1
	.loh AdrpLdr	Lloh4, Lloh5
	.cfi_endproc
                                        ; -- End function
	.globl	_add_node                       ; -- Begin function add_node
	.p2align	2
_add_node:                              ; @add_node
	.cfi_startproc
; %bb.0:
	adrp	x9, _node_count@PAGE
	ldrsw	x8, [x9, _node_count@PAGEOFF]
Lloh6:
	adrp	x10, _max_nodes@PAGE
Lloh7:
	ldr	w10, [x10, _max_nodes@PAGEOFF]
	cmp	w8, w10
	b.ge	LBB5_2
; %bb.1:
Lloh8:
	adrp	x10, _nodes@PAGE
Lloh9:
	ldr	x10, [x10, _nodes@PAGEOFF]
	add	w11, w8, #1
	str	w11, [x9, _node_count@PAGEOFF]
	mov	w9, #48                         ; =0x30
	smaddl	x8, w8, w9, x10
	str	x0, [x8, #32]
	str	w1, [x8, #40]
	ldr	x9, [x0]
	stp	x9, x0, [x8]
	str	x8, [x9, #8]
	str	x8, [x0]
	ldr	w9, [x0, #48]
	add	w9, w9, #1
	str	w9, [x0, #48]
	mov	x0, x8
	ret
LBB5_2:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh10:
	adrp	x8, ___stderrp@GOTPAGE
Lloh11:
	ldr	x8, [x8, ___stderrp@GOTPAGEOFF]
Lloh12:
	ldr	x3, [x8]
Lloh13:
	adrp	x0, l_.str.2@PAGE
Lloh14:
	add	x0, x0, l_.str.2@PAGEOFF
	mov	w1, #35                         ; =0x23
	mov	w2, #1                          ; =0x1
	bl	_fwrite
	mov	w0, #1                          ; =0x1
	bl	_exit
	.loh AdrpLdr	Lloh6, Lloh7
	.loh AdrpLdr	Lloh8, Lloh9
	.loh AdrpAdd	Lloh13, Lloh14
	.loh AdrpLdrGotLdr	Lloh10, Lloh11, Lloh12
	.cfi_endproc
                                        ; -- End function
	.globl	_build_dlx_row                  ; -- Begin function build_dlx_row
	.p2align	2
_build_dlx_row:                         ; @build_dlx_row
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	mov	x29, sp
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
                                        ; kill: def $w3 killed $w3 def $x3
Lloh15:
	adrp	x8, _row_info@GOTPAGE
Lloh16:
	ldr	x8, [x8, _row_info@GOTPAGEOFF]
	mov	w9, #12                         ; =0xc
	smaddl	x8, w3, w9, x8
	stp	w0, w1, [x8]
	str	w2, [x8, #8]
	adrp	x9, _node_count@PAGE
	ldrsw	x8, [x9, _node_count@PAGEOFF]
	adrp	x13, _max_nodes@PAGE
	ldr	w10, [x13, _max_nodes@PAGEOFF]
	cmp	w8, w10
	b.ge	LBB6_5
; %bb.1:
	add	w12, w0, w0, lsl #3
	add	w11, w12, w1
Lloh17:
	adrp	x10, _columns@GOTPAGE
Lloh18:
	ldr	x10, [x10, _columns@GOTPAGEOFF]
	ldr	x14, [x10, w11, sxtw #3]
	adrp	x11, _nodes@PAGE
	ldr	x15, [x11, _nodes@PAGEOFF]
	add	w16, w8, #1
	str	w16, [x9, _node_count@PAGEOFF]
	mov	w16, #48                        ; =0x30
	smaddl	x8, w8, w16, x15
	str	x14, [x8, #32]
	str	w3, [x8, #40]
	ldr	x15, [x14]
	stp	x15, x14, [x8]
	str	x8, [x15, #8]
	str	x8, [x14]
	ldr	w15, [x14, #48]
	add	w15, w15, #1
	str	w15, [x14, #48]
	ldrsw	x14, [x9, _node_count@PAGEOFF]
	ldr	w15, [x13, _max_nodes@PAGEOFF]
	cmp	w14, w15
	b.ge	LBB6_5
; %bb.2:
	add	w12, w12, w2
	add	w12, w12, #80
	ldr	x15, [x10, w12, sxtw #3]
	ldr	x12, [x11, _nodes@PAGEOFF]
	add	w16, w14, #1
	str	w16, [x9, _node_count@PAGEOFF]
	mov	w16, #48                        ; =0x30
	smaddl	x12, w14, w16, x12
	str	x15, [x12, #32]
	str	w3, [x12, #40]
	ldr	x14, [x15]
	stp	x14, x15, [x12]
	str	x12, [x14, #8]
	str	x12, [x15]
	ldr	w14, [x15, #48]
	add	w14, w14, #1
	str	w14, [x15, #48]
	ldrsw	x14, [x9, _node_count@PAGEOFF]
	ldr	w15, [x13, _max_nodes@PAGEOFF]
	cmp	w14, w15
	b.ge	LBB6_5
; %bb.3:
	add	w15, w1, w1, lsl #3
	add	w15, w2, w15
	add	w15, w15, #161
	ldr	x15, [x10, w15, sxtw #3]
	ldr	x16, [x11, _nodes@PAGEOFF]
	add	w17, w14, #1
	str	w17, [x9, _node_count@PAGEOFF]
	mov	w17, #48                        ; =0x30
	smaddl	x14, w14, w17, x16
	str	x15, [x14, #32]
	str	w3, [x14, #40]
	ldr	x16, [x15]
	stp	x16, x15, [x14]
	str	x14, [x16, #8]
	str	x14, [x15]
	ldr	w16, [x15, #48]
	add	w16, w16, #1
	str	w16, [x15, #48]
	ldrsw	x15, [x9, _node_count@PAGEOFF]
	ldr	w13, [x13, _max_nodes@PAGEOFF]
	cmp	w15, w13
	b.ge	LBB6_5
; %bb.4:
	mov	w13, #21846                     ; =0x5556
	movk	w13, #21845, lsl #16
	smull	x16, w0, w13
	lsr	x17, x16, #63
	lsr	x16, x16, #32
	add	w16, w16, w17
	add	w16, w16, w16, lsl #1
	smull	x13, w1, w13
	lsr	x17, x13, #63
	lsr	x13, x13, #32
	add	w13, w13, w17
	add	w13, w16, w13
	add	w13, w13, w13, lsl #3
	add	w13, w2, w13
	add	w13, w13, #242
	ldr	x10, [x10, w13, sxtw #3]
	ldr	x11, [x11, _nodes@PAGEOFF]
	add	w13, w15, #1
	str	w13, [x9, _node_count@PAGEOFF]
	mov	w9, #48                         ; =0x30
	smaddl	x9, w15, w9, x11
	str	x10, [x9, #32]
	str	w3, [x9, #40]
	ldr	x11, [x10]
	stp	x11, x10, [x9]
	str	x9, [x11, #8]
	str	x9, [x10]
	ldr	w11, [x10, #48]
	add	w11, w11, #1
	str	w11, [x10, #48]
	stp	x9, x12, [x8, #16]
	stp	x8, x14, [x12, #16]
	stp	x12, x9, [x14, #16]
	stp	x14, x8, [x9, #16]
Lloh19:
	adrp	x9, _row_starts@GOTPAGE
Lloh20:
	ldr	x9, [x9, _row_starts@GOTPAGEOFF]
	str	x8, [x9, w3, sxtw #3]
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
LBB6_5:
Lloh21:
	adrp	x8, ___stderrp@GOTPAGE
Lloh22:
	ldr	x8, [x8, ___stderrp@GOTPAGEOFF]
Lloh23:
	ldr	x3, [x8]
Lloh24:
	adrp	x0, l_.str.2@PAGE
Lloh25:
	add	x0, x0, l_.str.2@PAGEOFF
	mov	w1, #35                         ; =0x23
	mov	w2, #1                          ; =0x1
	bl	_fwrite
	mov	w0, #1                          ; =0x1
	bl	_exit
	.loh AdrpLdrGot	Lloh15, Lloh16
	.loh AdrpLdrGot	Lloh17, Lloh18
	.loh AdrpLdrGot	Lloh19, Lloh20
	.loh AdrpAdd	Lloh24, Lloh25
	.loh AdrpLdrGotLdr	Lloh21, Lloh22, Lloh23
	.cfi_endproc
                                        ; -- End function
	.globl	_build_dlx_matrix_from_puzzle   ; -- Begin function build_dlx_matrix_from_puzzle
	.p2align	2
_build_dlx_matrix_from_puzzle:          ; @build_dlx_matrix_from_puzzle
	.cfi_startproc
; %bb.0:
	stp	x24, x23, [sp, #-64]!           ; 16-byte Folded Spill
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
	.cfi_offset w23, -56
	.cfi_offset w24, -64
	mov	x19, #0                         ; =0x0
	mov	w3, #0                          ; =0x0
Lloh26:
	adrp	x23, _puzzle@GOTPAGE
Lloh27:
	ldr	x23, [x23, _puzzle@GOTPAGEOFF]
	b	LBB7_2
LBB7_1:                                 ;   in Loop: Header=BB7_2 Depth=1
	add	x19, x19, #1
	add	x23, x23, #36
	cmp	x19, #9
	b.eq	LBB7_6
LBB7_2:                                 ; =>This Loop Header: Depth=1
                                        ;     Child Loop BB7_4 Depth 2
	mov	x20, #0                         ; =0x0
	b	LBB7_4
LBB7_3:                                 ;   in Loop: Header=BB7_4 Depth=2
	bl	_build_dlx_row
	mov	x3, x22
	add	x20, x20, #1
	cmp	x20, #9
	b.eq	LBB7_1
LBB7_4:                                 ;   Parent Loop BB7_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	w2, [x23, x20, lsl #2]
	add	w22, w3, #1
	mov	x0, x19
	mov	x1, x20
	cbnz	w2, LBB7_3
; %bb.5:                                ;   in Loop: Header=BB7_4 Depth=2
	mov	w2, #1                          ; =0x1
	mov	x21, x3
	bl	_build_dlx_row
	mov	x0, x19
	mov	x1, x20
	mov	w2, #2                          ; =0x2
	mov	x3, x22
	bl	_build_dlx_row
	add	w3, w21, #2
	mov	x0, x19
	mov	x1, x20
	mov	w2, #3                          ; =0x3
	bl	_build_dlx_row
	add	w3, w21, #3
	mov	x0, x19
	mov	x1, x20
	mov	w2, #4                          ; =0x4
	bl	_build_dlx_row
	add	w3, w21, #4
	mov	x0, x19
	mov	x1, x20
	mov	w2, #5                          ; =0x5
	bl	_build_dlx_row
	add	w3, w21, #5
	mov	x0, x19
	mov	x1, x20
	mov	w2, #6                          ; =0x6
	bl	_build_dlx_row
	add	w3, w21, #6
	mov	x0, x19
	mov	x1, x20
	mov	w2, #7                          ; =0x7
	bl	_build_dlx_row
	add	w3, w21, #7
	mov	x0, x19
	mov	x1, x20
	mov	w2, #8                          ; =0x8
	bl	_build_dlx_row
	add	w22, w21, #9
	add	w3, w21, #8
	mov	x0, x19
	mov	x1, x20
	mov	w2, #9                          ; =0x9
	b	LBB7_3
LBB7_6:
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp], #64             ; 16-byte Folded Reload
	ret
	.loh AdrpLdrGot	Lloh26, Lloh27
	.cfi_endproc
                                        ; -- End function
	.globl	_extract_solution               ; -- Begin function extract_solution
	.p2align	2
_extract_solution:                      ; @extract_solution
	.cfi_startproc
; %bb.0:
	stp	x22, x21, [sp, #-48]!           ; 16-byte Folded Spill
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	add	x29, sp, #32
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	mov	x21, x1
	mov	x20, x0
Lloh28:
	adrp	x19, _solution_grid@GOTPAGE
Lloh29:
	ldr	x19, [x19, _solution_grid@GOTPAGEOFF]
Lloh30:
	adrp	x1, _puzzle@GOTPAGE
Lloh31:
	ldr	x1, [x1, _puzzle@GOTPAGEOFF]
	mov	x0, x19
	mov	w2, #324                        ; =0x144
	bl	_memcpy
	cmp	w21, #1
	b.lt	LBB8_26
; %bb.1:
	mov	w11, w21
	and	x8, x11, #0x7
Lloh32:
	adrp	x9, _row_info@GOTPAGE
Lloh33:
	ldr	x9, [x9, _row_info@GOTPAGEOFF]
	cmp	w21, #8
	b.hs	LBB8_3
; %bb.2:
	mov	x10, #0                         ; =0x0
	cbnz	x8, LBB8_22
	b	LBB8_26
LBB8_3:
	mov	x10, #0                         ; =0x0
	and	x11, x11, #0x7ffffff8
	neg	x11, x11
	add	x12, x20, #16
	mov	w13, #12                        ; =0xc
	mov	w14, #36                        ; =0x24
	b	LBB8_5
LBB8_4:                                 ;   in Loop: Header=BB8_5 Depth=1
	sub	x10, x10, #8
	add	x12, x12, #32
	cmp	x11, x10
	b.eq	LBB8_21
LBB8_5:                                 ; =>This Inner Loop Header: Depth=1
	ldur	w15, [x12, #-16]
	cmp	w15, #728
	b.ls	LBB8_13
; %bb.6:                                ;   in Loop: Header=BB8_5 Depth=1
	ldur	w15, [x12, #-12]
	cmp	w15, #728
	b.ls	LBB8_14
LBB8_7:                                 ;   in Loop: Header=BB8_5 Depth=1
	ldur	w15, [x12, #-8]
	cmp	w15, #728
	b.ls	LBB8_15
LBB8_8:                                 ;   in Loop: Header=BB8_5 Depth=1
	ldur	w15, [x12, #-4]
	cmp	w15, #728
	b.ls	LBB8_16
LBB8_9:                                 ;   in Loop: Header=BB8_5 Depth=1
	ldr	w15, [x12]
	cmp	w15, #728
	b.ls	LBB8_17
LBB8_10:                                ;   in Loop: Header=BB8_5 Depth=1
	ldr	w15, [x12, #4]
	cmp	w15, #728
	b.ls	LBB8_18
LBB8_11:                                ;   in Loop: Header=BB8_5 Depth=1
	ldr	w15, [x12, #8]
	cmp	w15, #728
	b.ls	LBB8_19
LBB8_12:                                ;   in Loop: Header=BB8_5 Depth=1
	ldr	w15, [x12, #12]
	cmp	w15, #728
	b.hi	LBB8_4
	b	LBB8_20
LBB8_13:                                ;   in Loop: Header=BB8_5 Depth=1
	umaddl	x15, w15, w13, x9
	ldrsw	x17, [x15]
	ldp	w15, w16, [x15, #4]
                                        ; kill: def $w15 killed $w15 def $x15
	sxtw	x15, w15
	smaddl	x17, w17, w14, x19
	str	w16, [x17, x15, lsl #2]
	ldur	w15, [x12, #-12]
	cmp	w15, #728
	b.hi	LBB8_7
LBB8_14:                                ;   in Loop: Header=BB8_5 Depth=1
	umaddl	x15, w15, w13, x9
	ldrsw	x17, [x15]
	ldp	w15, w16, [x15, #4]
                                        ; kill: def $w15 killed $w15 def $x15
	sxtw	x15, w15
	smaddl	x17, w17, w14, x19
	str	w16, [x17, x15, lsl #2]
	ldur	w15, [x12, #-8]
	cmp	w15, #728
	b.hi	LBB8_8
LBB8_15:                                ;   in Loop: Header=BB8_5 Depth=1
	umaddl	x15, w15, w13, x9
	ldrsw	x17, [x15]
	ldp	w15, w16, [x15, #4]
                                        ; kill: def $w15 killed $w15 def $x15
	sxtw	x15, w15
	smaddl	x17, w17, w14, x19
	str	w16, [x17, x15, lsl #2]
	ldur	w15, [x12, #-4]
	cmp	w15, #728
	b.hi	LBB8_9
LBB8_16:                                ;   in Loop: Header=BB8_5 Depth=1
	umaddl	x15, w15, w13, x9
	ldrsw	x17, [x15]
	ldp	w15, w16, [x15, #4]
                                        ; kill: def $w15 killed $w15 def $x15
	sxtw	x15, w15
	smaddl	x17, w17, w14, x19
	str	w16, [x17, x15, lsl #2]
	ldr	w15, [x12]
	cmp	w15, #728
	b.hi	LBB8_10
LBB8_17:                                ;   in Loop: Header=BB8_5 Depth=1
	umaddl	x15, w15, w13, x9
	ldrsw	x17, [x15]
	ldp	w15, w16, [x15, #4]
                                        ; kill: def $w15 killed $w15 def $x15
	sxtw	x15, w15
	smaddl	x17, w17, w14, x19
	str	w16, [x17, x15, lsl #2]
	ldr	w15, [x12, #4]
	cmp	w15, #728
	b.hi	LBB8_11
LBB8_18:                                ;   in Loop: Header=BB8_5 Depth=1
	umaddl	x15, w15, w13, x9
	ldrsw	x17, [x15]
	ldp	w15, w16, [x15, #4]
                                        ; kill: def $w15 killed $w15 def $x15
	sxtw	x15, w15
	smaddl	x17, w17, w14, x19
	str	w16, [x17, x15, lsl #2]
	ldr	w15, [x12, #8]
	cmp	w15, #728
	b.hi	LBB8_12
LBB8_19:                                ;   in Loop: Header=BB8_5 Depth=1
	umaddl	x15, w15, w13, x9
	ldrsw	x17, [x15]
	ldp	w15, w16, [x15, #4]
                                        ; kill: def $w15 killed $w15 def $x15
	sxtw	x15, w15
	smaddl	x17, w17, w14, x19
	str	w16, [x17, x15, lsl #2]
	ldr	w15, [x12, #12]
	cmp	w15, #728
	b.hi	LBB8_4
LBB8_20:                                ;   in Loop: Header=BB8_5 Depth=1
	umaddl	x15, w15, w13, x9
	ldrsw	x17, [x15]
	ldp	w15, w16, [x15, #4]
                                        ; kill: def $w15 killed $w15 def $x15
	sxtw	x15, w15
	smaddl	x17, w17, w14, x19
	str	w16, [x17, x15, lsl #2]
	b	LBB8_4
LBB8_21:
	neg	x10, x10
	cbz	x8, LBB8_26
LBB8_22:
	add	x10, x20, x10, lsl #2
	mov	w11, #12                        ; =0xc
	mov	w12, #36                        ; =0x24
	b	LBB8_24
LBB8_23:                                ;   in Loop: Header=BB8_24 Depth=1
	subs	x8, x8, #1
	b.eq	LBB8_26
LBB8_24:                                ; =>This Inner Loop Header: Depth=1
	ldr	w13, [x10], #4
	cmp	w13, #728
	b.hi	LBB8_23
; %bb.25:                               ;   in Loop: Header=BB8_24 Depth=1
	umaddl	x13, w13, w11, x9
	ldrsw	x15, [x13]
	ldp	w13, w14, [x13, #4]
                                        ; kill: def $w13 killed $w13 def $x13
	sxtw	x13, w13
	smaddl	x15, w15, w12, x19
	str	w14, [x15, x13, lsl #2]
	b	LBB8_23
LBB8_26:
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	ret
	.loh AdrpLdrGot	Lloh30, Lloh31
	.loh AdrpLdrGot	Lloh28, Lloh29
	.loh AdrpLdrGot	Lloh32, Lloh33
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
Lloh34:
	adrp	x0, l_str@PAGE
Lloh35:
	add	x0, x0, l_str@PAGEOFF
	bl	_puts
	add	x20, x19, #16
	mov	w21, #9                         ; =0x9
Lloh36:
	adrp	x19, l_.str.4@PAGE
Lloh37:
	add	x19, x19, l_.str.4@PAGEOFF
LBB9_1:                                 ; =>This Inner Loop Header: Depth=1
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
	b.ne	LBB9_1
; %bb.2:
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #64
	ret
	.loh AdrpAdd	Lloh36, Lloh37
	.loh AdrpAdd	Lloh34, Lloh35
	.cfi_endproc
                                        ; -- End function
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
	mov	x20, x0
	stp	xzr, xzr, [sp, #80]
Lloh38:
	adrp	x1, l_.str.6@PAGE
Lloh39:
	add	x1, x1, l_.str.6@PAGEOFF
	bl	_fopen
	cbz	x0, LBB10_3
; %bb.1:
	mov	x19, x0
Lloh40:
	adrp	x1, l_.str.8@PAGE
Lloh41:
	add	x1, x1, l_.str.8@PAGEOFF
	mov	x0, x20
	mov	w2, #14                         ; =0xe
	bl	_strncmp
	cbz	w0, LBB10_4
; %bb.2:
	mov	x0, x20
	bl	_puts
	add	x0, sp, #88
	add	x1, sp, #80
	mov	x2, x19
	bl	_getline
	tbz	x0, #63, LBB10_5
	b	LBB10_14
LBB10_3:
Lloh42:
	adrp	x8, ___stderrp@GOTPAGE
Lloh43:
	ldr	x8, [x8, ___stderrp@GOTPAGEOFF]
Lloh44:
	ldr	x0, [x8]
	str	x20, [sp]
Lloh45:
	adrp	x1, l_.str.7@PAGE
Lloh46:
	add	x1, x1, l_.str.7@PAGEOFF
	bl	_fprintf
	mov	w0, #1                          ; =0x1
	b	LBB10_15
LBB10_4:
	add	x8, x20, #5
	str	x8, [sp]
Lloh47:
	adrp	x0, l_.str.9@PAGE
Lloh48:
	add	x0, x0, l_.str.9@PAGEOFF
	bl	_printf
	add	x0, sp, #88
	add	x1, sp, #80
	mov	x2, x19
	bl	_getline
	tbnz	x0, #63, LBB10_14
LBB10_5:
	mov	w23, #0                         ; =0x0
	mov	w24, #1                         ; =0x1
	mov	x25, #9216                      ; =0x2400
	movk	x25, #8, lsl #32
Lloh49:
	adrp	x27, _puzzle@GOTPAGE
Lloh50:
	ldr	x27, [x27, _puzzle@GOTPAGEOFF]
	mov	w28, #36                        ; =0x24
Lloh51:
	adrp	x20, l_.str.12@PAGE
Lloh52:
	add	x20, x20, l_.str.12@PAGEOFF
Lloh53:
	adrp	x21, l_.str.4@PAGE
Lloh54:
	add	x21, x21, l_.str.4@PAGEOFF
	b	LBB10_7
LBB10_6:                                ;   in Loop: Header=BB10_7 Depth=1
	add	x0, sp, #88
	add	x1, sp, #80
	mov	x2, x19
	bl	_getline
	tbnz	x0, #63, LBB10_14
LBB10_7:                                ; =>This Inner Loop Header: Depth=1
	ldr	x8, [sp, #88]
	ldrb	w9, [x8]
	cmp	w9, #35
	lsl	x9, x24, x9
	and	x9, x9, x25
	ccmp	x9, #0, #4, ls
	b.ne	LBB10_6
; %bb.8:                                ;   in Loop: Header=BB10_7 Depth=1
Lloh55:
	adrp	x9, _DEBUG@PAGE
Lloh56:
	ldr	w9, [x9, _DEBUG@PAGEOFF]
	cbz	w9, LBB10_10
; %bb.9:                                ;   in Loop: Header=BB10_7 Depth=1
	stp	x0, x8, [sp, #8]
	mov	x22, x0
	str	x23, [sp]
Lloh57:
	adrp	x0, l_.str.11@PAGE
Lloh58:
	add	x0, x0, l_.str.11@PAGEOFF
	bl	_printf
	ldr	x8, [sp, #88]
	b	LBB10_11
LBB10_10:                               ;   in Loop: Header=BB10_7 Depth=1
	mov	x22, x0
LBB10_11:                               ;   in Loop: Header=BB10_7 Depth=1
	smaddl	x26, w23, w28, x27
	add	x9, x26, #4
	add	x10, x26, #8
	add	x11, x26, #12
	add	x12, x26, #16
	add	x13, x26, #20
	add	x14, x26, #24
	add	x15, x26, #28
	add	x16, x26, #32
	stp	x15, x16, [sp, #56]
	stp	x13, x14, [sp, #40]
	stp	x11, x12, [sp, #24]
	stp	x9, x10, [sp, #8]
	str	x26, [sp]
	mov	x0, x8
	mov	x1, x20
	bl	_sscanf
	cmp	w0, #9
	b.ne	LBB10_16
; %bb.12:                               ;   in Loop: Header=BB10_7 Depth=1
	cmp	w23, #8
	b.gt	LBB10_6
; %bb.13:                               ;   in Loop: Header=BB10_7 Depth=1
	mov	x22, x23
	ldr	w8, [x26]
	str	x8, [sp]
	mov	x0, x21
	bl	_printf
	smaddl	x22, w22, w28, x27
	ldr	w8, [x22, #4]
	str	x8, [sp]
	mov	x0, x21
	bl	_printf
	ldr	w8, [x22, #8]
	str	x8, [sp]
	mov	x0, x21
	bl	_printf
	ldr	w8, [x22, #12]
	str	x8, [sp]
	mov	x0, x21
	bl	_printf
	ldr	w8, [x22, #16]
	str	x8, [sp]
	mov	x0, x21
	bl	_printf
	ldr	w8, [x22, #20]
	str	x8, [sp]
	mov	x0, x21
	bl	_printf
	ldr	w8, [x22, #24]
	str	x8, [sp]
	mov	x0, x21
	bl	_printf
	ldr	w8, [x22, #28]
	str	x8, [sp]
	mov	x0, x21
	bl	_printf
	ldr	w8, [x22, #32]
	str	x8, [sp]
	mov	x0, x21
	bl	_printf
	mov	w0, #10                         ; =0xa
	bl	_putchar
	add	w23, w23, #1
	b	LBB10_6
LBB10_14:
	mov	x0, x19
	bl	_fclose
	ldr	x0, [sp, #88]
	bl	_free
	mov	w0, #0                          ; =0x0
LBB10_15:
	ldp	x29, x30, [sp, #176]            ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #160]            ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #144]            ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #128]            ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #112]            ; 16-byte Folded Reload
	ldp	x28, x27, [sp, #96]             ; 16-byte Folded Reload
	add	sp, sp, #192
	ret
LBB10_16:
Lloh59:
	adrp	x0, l_str.19@PAGE
Lloh60:
	add	x0, x0, l_str.19@PAGEOFF
	bl	_puts
	ldr	x8, [sp, #88]
	stp	x22, x8, [sp, #8]
                                        ; kill: def $w23 killed $w23 killed $x23 def $x23
	str	x23, [sp]
Lloh61:
	adrp	x0, l_.str.11@PAGE
Lloh62:
	add	x0, x0, l_.str.11@PAGEOFF
	bl	_printf
	mov	w0, #1                          ; =0x1
	b	LBB10_15
	.loh AdrpAdd	Lloh38, Lloh39
	.loh AdrpAdd	Lloh40, Lloh41
	.loh AdrpAdd	Lloh45, Lloh46
	.loh AdrpLdrGotLdr	Lloh42, Lloh43, Lloh44
	.loh AdrpAdd	Lloh47, Lloh48
	.loh AdrpAdd	Lloh53, Lloh54
	.loh AdrpAdd	Lloh51, Lloh52
	.loh AdrpLdrGot	Lloh49, Lloh50
	.loh AdrpLdr	Lloh55, Lloh56
	.loh AdrpAdd	Lloh57, Lloh58
	.loh AdrpAdd	Lloh61, Lloh62
	.loh AdrpAdd	Lloh59, Lloh60
	.cfi_endproc
                                        ; -- End function
	.globl	_free_dlx_matrix                ; -- Begin function free_dlx_matrix
	.p2align	2
_free_dlx_matrix:                       ; @free_dlx_matrix
	.cfi_startproc
; %bb.0:
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
Lloh63:
	adrp	x8, _root@PAGE
Lloh64:
	ldr	x0, [x8, _root@PAGEOFF]
	cbz	x0, LBB11_2
; %bb.1:
	bl	_free
LBB11_2:
	mov	x19, #0                         ; =0x0
Lloh65:
	adrp	x20, _columns@GOTPAGE
Lloh66:
	ldr	x20, [x20, _columns@GOTPAGEOFF]
	b	LBB11_4
LBB11_3:                                ;   in Loop: Header=BB11_4 Depth=1
	add	x19, x19, #8
	cmp	x19, #2592
	b.eq	LBB11_6
LBB11_4:                                ; =>This Inner Loop Header: Depth=1
	ldr	x0, [x20, x19]
	cbz	x0, LBB11_3
; %bb.5:                                ;   in Loop: Header=BB11_4 Depth=1
	bl	_free
	b	LBB11_3
LBB11_6:
Lloh67:
	adrp	x8, _nodes@PAGE
Lloh68:
	ldr	x0, [x8, _nodes@PAGEOFF]
	cbz	x0, LBB11_8
; %bb.7:
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_free
LBB11_8:
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.loh AdrpLdr	Lloh63, Lloh64
	.loh AdrpLdrGot	Lloh65, Lloh66
	.loh AdrpLdr	Lloh67, Lloh68
	.cfi_endproc
                                        ; -- End function
	.globl	_cover_clues                    ; -- Begin function cover_clues
	.p2align	2
_cover_clues:                           ; @cover_clues
	.cfi_startproc
; %bb.0:
	stp	x26, x25, [sp, #-80]!           ; 16-byte Folded Spill
	stp	x24, x23, [sp, #16]             ; 16-byte Folded Spill
	stp	x22, x21, [sp, #32]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #48]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #64]             ; 16-byte Folded Spill
	add	x29, sp, #64
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
	mov	x19, #0                         ; =0x0
	mov	w20, #36                        ; =0x24
Lloh69:
	adrp	x21, _puzzle@GOTPAGE
Lloh70:
	ldr	x21, [x21, _puzzle@GOTPAGEOFF]
Lloh71:
	adrp	x22, _row_info@GOTPAGE
Lloh72:
	ldr	x22, [x22, _row_info@GOTPAGEOFF]
Lloh73:
	adrp	x23, _row_starts@GOTPAGE
Lloh74:
	ldr	x23, [x23, _row_starts@GOTPAGEOFF]
	b	LBB12_2
LBB12_1:                                ;   in Loop: Header=BB12_2 Depth=1
	add	x19, x19, #1
	cmp	x19, #9
	b.eq	LBB12_83
LBB12_2:                                ; =>This Loop Header: Depth=1
                                        ;     Child Loop BB12_5 Depth 2
                                        ;     Child Loop BB12_10 Depth 2
                                        ;     Child Loop BB12_14 Depth 2
                                        ;     Child Loop BB12_19 Depth 2
                                        ;     Child Loop BB12_23 Depth 2
                                        ;     Child Loop BB12_28 Depth 2
                                        ;     Child Loop BB12_32 Depth 2
                                        ;     Child Loop BB12_37 Depth 2
                                        ;     Child Loop BB12_41 Depth 2
                                        ;     Child Loop BB12_46 Depth 2
                                        ;     Child Loop BB12_50 Depth 2
                                        ;     Child Loop BB12_55 Depth 2
                                        ;     Child Loop BB12_59 Depth 2
                                        ;     Child Loop BB12_64 Depth 2
                                        ;     Child Loop BB12_68 Depth 2
                                        ;     Child Loop BB12_73 Depth 2
                                        ;     Child Loop BB12_77 Depth 2
                                        ;     Child Loop BB12_82 Depth 2
	mul	x8, x19, x20
	ldr	w8, [x21, x8]
	cbz	w8, LBB12_11
; %bb.3:                                ;   in Loop: Header=BB12_2 Depth=1
	mov	x9, #0                          ; =0x0
	add	x10, x22, #8
	b	LBB12_5
LBB12_4:                                ;   in Loop: Header=BB12_5 Depth=2
	add	x9, x9, #1
	add	x10, x10, #12
	cmp	x9, #729
	b.eq	LBB12_11
LBB12_5:                                ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x24, [x23, x9, lsl #3]
	cbz	x24, LBB12_4
; %bb.6:                                ;   in Loop: Header=BB12_5 Depth=2
	ldur	w11, [x10, #-8]
	cmp	x19, x11
	b.ne	LBB12_4
; %bb.7:                                ;   in Loop: Header=BB12_5 Depth=2
	ldur	w11, [x10, #-4]
	cbnz	w11, LBB12_4
; %bb.8:                                ;   in Loop: Header=BB12_5 Depth=2
	ldr	w11, [x10]
	cmp	w11, w8
	b.ne	LBB12_4
; %bb.9:                                ;   in Loop: Header=BB12_2 Depth=1
	mov	x25, x24
LBB12_10:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x0, [x25, #32]
	bl	_dlx_cover_column
	ldr	x25, [x25, #24]
	cmp	x25, x24
	b.ne	LBB12_10
LBB12_11:                               ;   in Loop: Header=BB12_2 Depth=1
	madd	x8, x19, x20, x21
	ldr	w8, [x8, #4]
	cbz	w8, LBB12_20
; %bb.12:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x9, #0                          ; =0x0
	add	x10, x22, #8
	b	LBB12_14
LBB12_13:                               ;   in Loop: Header=BB12_14 Depth=2
	add	x9, x9, #1
	add	x10, x10, #12
	cmp	x9, #729
	b.eq	LBB12_20
LBB12_14:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x24, [x23, x9, lsl #3]
	cbz	x24, LBB12_13
; %bb.15:                               ;   in Loop: Header=BB12_14 Depth=2
	ldur	w11, [x10, #-8]
	cmp	x19, x11
	b.ne	LBB12_13
; %bb.16:                               ;   in Loop: Header=BB12_14 Depth=2
	ldur	w11, [x10, #-4]
	cmp	w11, #1
	b.ne	LBB12_13
; %bb.17:                               ;   in Loop: Header=BB12_14 Depth=2
	ldr	w11, [x10]
	cmp	w11, w8
	b.ne	LBB12_13
; %bb.18:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x25, x24
LBB12_19:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x0, [x25, #32]
	bl	_dlx_cover_column
	ldr	x25, [x25, #24]
	cmp	x25, x24
	b.ne	LBB12_19
LBB12_20:                               ;   in Loop: Header=BB12_2 Depth=1
	madd	x8, x19, x20, x21
	ldr	w8, [x8, #8]
	cbz	w8, LBB12_29
; %bb.21:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x9, #0                          ; =0x0
	add	x10, x22, #8
	b	LBB12_23
LBB12_22:                               ;   in Loop: Header=BB12_23 Depth=2
	add	x9, x9, #1
	add	x10, x10, #12
	cmp	x9, #729
	b.eq	LBB12_29
LBB12_23:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x24, [x23, x9, lsl #3]
	cbz	x24, LBB12_22
; %bb.24:                               ;   in Loop: Header=BB12_23 Depth=2
	ldur	w11, [x10, #-8]
	cmp	x19, x11
	b.ne	LBB12_22
; %bb.25:                               ;   in Loop: Header=BB12_23 Depth=2
	ldur	w11, [x10, #-4]
	cmp	w11, #2
	b.ne	LBB12_22
; %bb.26:                               ;   in Loop: Header=BB12_23 Depth=2
	ldr	w11, [x10]
	cmp	w11, w8
	b.ne	LBB12_22
; %bb.27:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x25, x24
LBB12_28:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x0, [x25, #32]
	bl	_dlx_cover_column
	ldr	x25, [x25, #24]
	cmp	x25, x24
	b.ne	LBB12_28
LBB12_29:                               ;   in Loop: Header=BB12_2 Depth=1
	madd	x8, x19, x20, x21
	ldr	w8, [x8, #12]
	cbz	w8, LBB12_38
; %bb.30:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x9, #0                          ; =0x0
	add	x10, x22, #8
	b	LBB12_32
LBB12_31:                               ;   in Loop: Header=BB12_32 Depth=2
	add	x9, x9, #1
	add	x10, x10, #12
	cmp	x9, #729
	b.eq	LBB12_38
LBB12_32:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x24, [x23, x9, lsl #3]
	cbz	x24, LBB12_31
; %bb.33:                               ;   in Loop: Header=BB12_32 Depth=2
	ldur	w11, [x10, #-8]
	cmp	x19, x11
	b.ne	LBB12_31
; %bb.34:                               ;   in Loop: Header=BB12_32 Depth=2
	ldur	w11, [x10, #-4]
	cmp	w11, #3
	b.ne	LBB12_31
; %bb.35:                               ;   in Loop: Header=BB12_32 Depth=2
	ldr	w11, [x10]
	cmp	w11, w8
	b.ne	LBB12_31
; %bb.36:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x25, x24
LBB12_37:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x0, [x25, #32]
	bl	_dlx_cover_column
	ldr	x25, [x25, #24]
	cmp	x25, x24
	b.ne	LBB12_37
LBB12_38:                               ;   in Loop: Header=BB12_2 Depth=1
	madd	x8, x19, x20, x21
	ldr	w8, [x8, #16]
	cbz	w8, LBB12_47
; %bb.39:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x9, #0                          ; =0x0
	add	x10, x22, #8
	b	LBB12_41
LBB12_40:                               ;   in Loop: Header=BB12_41 Depth=2
	add	x9, x9, #1
	add	x10, x10, #12
	cmp	x9, #729
	b.eq	LBB12_47
LBB12_41:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x24, [x23, x9, lsl #3]
	cbz	x24, LBB12_40
; %bb.42:                               ;   in Loop: Header=BB12_41 Depth=2
	ldur	w11, [x10, #-8]
	cmp	x19, x11
	b.ne	LBB12_40
; %bb.43:                               ;   in Loop: Header=BB12_41 Depth=2
	ldur	w11, [x10, #-4]
	cmp	w11, #4
	b.ne	LBB12_40
; %bb.44:                               ;   in Loop: Header=BB12_41 Depth=2
	ldr	w11, [x10]
	cmp	w11, w8
	b.ne	LBB12_40
; %bb.45:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x25, x24
LBB12_46:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x0, [x25, #32]
	bl	_dlx_cover_column
	ldr	x25, [x25, #24]
	cmp	x25, x24
	b.ne	LBB12_46
LBB12_47:                               ;   in Loop: Header=BB12_2 Depth=1
	madd	x8, x19, x20, x21
	ldr	w8, [x8, #20]
	cbz	w8, LBB12_56
; %bb.48:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x9, #0                          ; =0x0
	add	x10, x22, #8
	b	LBB12_50
LBB12_49:                               ;   in Loop: Header=BB12_50 Depth=2
	add	x9, x9, #1
	add	x10, x10, #12
	cmp	x9, #729
	b.eq	LBB12_56
LBB12_50:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x24, [x23, x9, lsl #3]
	cbz	x24, LBB12_49
; %bb.51:                               ;   in Loop: Header=BB12_50 Depth=2
	ldur	w11, [x10, #-8]
	cmp	x19, x11
	b.ne	LBB12_49
; %bb.52:                               ;   in Loop: Header=BB12_50 Depth=2
	ldur	w11, [x10, #-4]
	cmp	w11, #5
	b.ne	LBB12_49
; %bb.53:                               ;   in Loop: Header=BB12_50 Depth=2
	ldr	w11, [x10]
	cmp	w11, w8
	b.ne	LBB12_49
; %bb.54:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x25, x24
LBB12_55:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x0, [x25, #32]
	bl	_dlx_cover_column
	ldr	x25, [x25, #24]
	cmp	x25, x24
	b.ne	LBB12_55
LBB12_56:                               ;   in Loop: Header=BB12_2 Depth=1
	madd	x8, x19, x20, x21
	ldr	w8, [x8, #24]
	cbz	w8, LBB12_65
; %bb.57:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x9, #0                          ; =0x0
	add	x10, x22, #8
	b	LBB12_59
LBB12_58:                               ;   in Loop: Header=BB12_59 Depth=2
	add	x9, x9, #1
	add	x10, x10, #12
	cmp	x9, #729
	b.eq	LBB12_65
LBB12_59:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x24, [x23, x9, lsl #3]
	cbz	x24, LBB12_58
; %bb.60:                               ;   in Loop: Header=BB12_59 Depth=2
	ldur	w11, [x10, #-8]
	cmp	x19, x11
	b.ne	LBB12_58
; %bb.61:                               ;   in Loop: Header=BB12_59 Depth=2
	ldur	w11, [x10, #-4]
	cmp	w11, #6
	b.ne	LBB12_58
; %bb.62:                               ;   in Loop: Header=BB12_59 Depth=2
	ldr	w11, [x10]
	cmp	w11, w8
	b.ne	LBB12_58
; %bb.63:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x25, x24
LBB12_64:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x0, [x25, #32]
	bl	_dlx_cover_column
	ldr	x25, [x25, #24]
	cmp	x25, x24
	b.ne	LBB12_64
LBB12_65:                               ;   in Loop: Header=BB12_2 Depth=1
	madd	x8, x19, x20, x21
	ldr	w8, [x8, #28]
	cbz	w8, LBB12_74
; %bb.66:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x9, #0                          ; =0x0
	add	x10, x22, #8
	b	LBB12_68
LBB12_67:                               ;   in Loop: Header=BB12_68 Depth=2
	add	x9, x9, #1
	add	x10, x10, #12
	cmp	x9, #729
	b.eq	LBB12_74
LBB12_68:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x24, [x23, x9, lsl #3]
	cbz	x24, LBB12_67
; %bb.69:                               ;   in Loop: Header=BB12_68 Depth=2
	ldur	w11, [x10, #-8]
	cmp	x19, x11
	b.ne	LBB12_67
; %bb.70:                               ;   in Loop: Header=BB12_68 Depth=2
	ldur	w11, [x10, #-4]
	cmp	w11, #7
	b.ne	LBB12_67
; %bb.71:                               ;   in Loop: Header=BB12_68 Depth=2
	ldr	w11, [x10]
	cmp	w11, w8
	b.ne	LBB12_67
; %bb.72:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x25, x24
LBB12_73:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x0, [x25, #32]
	bl	_dlx_cover_column
	ldr	x25, [x25, #24]
	cmp	x25, x24
	b.ne	LBB12_73
LBB12_74:                               ;   in Loop: Header=BB12_2 Depth=1
	madd	x8, x19, x20, x21
	ldr	w8, [x8, #32]
	cbz	w8, LBB12_1
; %bb.75:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x9, #0                          ; =0x0
	add	x10, x22, #8
	b	LBB12_77
LBB12_76:                               ;   in Loop: Header=BB12_77 Depth=2
	add	x9, x9, #1
	add	x10, x10, #12
	cmp	x9, #729
	b.eq	LBB12_1
LBB12_77:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x24, [x23, x9, lsl #3]
	cbz	x24, LBB12_76
; %bb.78:                               ;   in Loop: Header=BB12_77 Depth=2
	ldur	w11, [x10, #-8]
	cmp	x19, x11
	b.ne	LBB12_76
; %bb.79:                               ;   in Loop: Header=BB12_77 Depth=2
	ldur	w11, [x10, #-4]
	cmp	w11, #8
	b.ne	LBB12_76
; %bb.80:                               ;   in Loop: Header=BB12_77 Depth=2
	ldr	w11, [x10]
	cmp	w11, w8
	b.ne	LBB12_76
; %bb.81:                               ;   in Loop: Header=BB12_2 Depth=1
	mov	x25, x24
LBB12_82:                               ;   Parent Loop BB12_2 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x0, [x25, #32]
	bl	_dlx_cover_column
	ldr	x25, [x25, #24]
	cmp	x25, x24
	b.ne	LBB12_82
	b	LBB12_1
LBB12_83:
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #32]             ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #16]             ; 16-byte Folded Reload
	ldp	x26, x25, [sp], #80             ; 16-byte Folded Reload
	ret
	.loh AdrpLdrGot	Lloh73, Lloh74
	.loh AdrpLdrGot	Lloh71, Lloh72
	.loh AdrpLdrGot	Lloh69, Lloh70
	.cfi_endproc
                                        ; -- End function
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #464
	stp	x28, x27, [sp, #368]            ; 16-byte Folded Spill
	stp	x26, x25, [sp, #384]            ; 16-byte Folded Spill
	stp	x24, x23, [sp, #400]            ; 16-byte Folded Spill
	stp	x22, x21, [sp, #416]            ; 16-byte Folded Spill
	stp	x20, x19, [sp, #432]            ; 16-byte Folded Spill
	stp	x29, x30, [sp, #448]            ; 16-byte Folded Spill
	add	x29, sp, #448
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
	mov	x19, x1
	mov	x20, x0
Lloh75:
	adrp	x8, ___stack_chk_guard@GOTPAGE
Lloh76:
	ldr	x8, [x8, ___stack_chk_guard@GOTPAGEOFF]
Lloh77:
	ldr	x8, [x8]
	stur	x8, [x29, #-96]
	add	x0, sp, #8
	mov	x1, #0                          ; =0x0
	bl	_gettimeofday
	cmp	w20, #2
	b.lt	LBB13_21
; %bb.1:
	mov	w27, w20
	mov	w28, #1                         ; =0x1
	add	x22, sp, #24
Lloh78:
	adrp	x21, _columns@GOTPAGE
Lloh79:
	ldr	x21, [x21, _columns@GOTPAGEOFF]
Lloh80:
	adrp	x23, _solution_grid@GOTPAGE
Lloh81:
	ldr	x23, [x23, _solution_grid@GOTPAGEOFF]
	mov	w25, #12                        ; =0xc
Lloh82:
	adrp	x24, _row_info@GOTPAGE
Lloh83:
	ldr	x24, [x24, _row_info@GOTPAGEOFF]
	mov	w20, #36                        ; =0x24
	b	LBB13_4
LBB13_2:                                ;   in Loop: Header=BB13_4 Depth=1
Lloh84:
	adrp	x8, ___stderrp@GOTPAGE
Lloh85:
	ldr	x8, [x8, ___stderrp@GOTPAGEOFF]
Lloh86:
	ldr	x0, [x8]
	ldr	x8, [x19, x28, lsl #3]
	str	x8, [sp]
Lloh87:
	adrp	x1, l_.str.15@PAGE
Lloh88:
	add	x1, x1, l_.str.15@PAGEOFF
	bl	_fprintf
LBB13_3:                                ;   in Loop: Header=BB13_4 Depth=1
	add	x28, x28, #1
	cmp	x28, x27
	b.eq	LBB13_21
LBB13_4:                                ; =>This Loop Header: Depth=1
                                        ;     Child Loop BB13_10 Depth 2
                                        ;     Child Loop BB13_16 Depth 2
	ldr	x26, [x19, x28, lsl #3]
	mov	x0, x26
	mov	w1, #46                         ; =0x2e
	bl	_strrchr
	cbz	x0, LBB13_3
; %bb.5:                                ;   in Loop: Header=BB13_4 Depth=1
Lloh89:
	adrp	x1, l_.str.14@PAGE
Lloh90:
	add	x1, x1, l_.str.14@PAGEOFF
	bl	_strcmp
	cbnz	w0, LBB13_3
; %bb.6:                                ;   in Loop: Header=BB13_4 Depth=1
	mov	x0, x26
	bl	_readMatrixFile
	cbnz	w0, LBB13_2
; %bb.7:                                ;   in Loop: Header=BB13_4 Depth=1
Lloh91:
	adrp	x26, _puzzle@GOTPAGE
Lloh92:
	ldr	x26, [x26, _puzzle@GOTPAGEOFF]
	mov	x0, x26
	bl	_printPuzzle
	bl	_init_dlx_matrix
	bl	_build_dlx_matrix_from_puzzle
	bl	_cover_clues
Lloh93:
	adrp	x8, _dlx_iterations@GOTPAGE
Lloh94:
	ldr	x8, [x8, _dlx_iterations@GOTPAGEOFF]
Lloh95:
	str	wzr, [x8]
Lloh96:
	adrp	x8, _root@PAGE
Lloh97:
	ldr	x0, [x8, _root@PAGEOFF]
	add	x2, sp, #24
	mov	w1, #0                          ; =0x0
	bl	_dlx_search
	cbz	w0, LBB13_20
; %bb.8:                                ;   in Loop: Header=BB13_4 Depth=1
	mov	x0, x23
	mov	x1, x26
	mov	w2, #324                        ; =0x144
	bl	_memcpy
	mov	x8, #0                          ; =0x0
	b	LBB13_10
LBB13_9:                                ;   in Loop: Header=BB13_10 Depth=2
	add	x8, x8, #4
	cmp	x8, #324
	b.eq	LBB13_12
LBB13_10:                               ;   Parent Loop BB13_4 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	w9, [x22, x8]
	cmp	w9, #728
	b.hi	LBB13_9
; %bb.11:                               ;   in Loop: Header=BB13_10 Depth=2
	umaddl	x9, w9, w25, x24
	ldrsw	x11, [x9]
	ldp	w9, w10, [x9, #4]
                                        ; kill: def $w9 killed $w9 def $x9
	sxtw	x9, w9
	smaddl	x11, w11, w20, x23
	str	w10, [x11, x9, lsl #2]
	b	LBB13_9
LBB13_12:                               ;   in Loop: Header=BB13_4 Depth=1
	mov	x0, x23
	bl	_printPuzzle
Lloh98:
	adrp	x8, _dlx_iterations@GOTPAGE
Lloh99:
	ldr	x8, [x8, _dlx_iterations@GOTPAGEOFF]
Lloh100:
	ldr	w8, [x8]
	str	x8, [sp]
Lloh101:
	adrp	x0, l_.str.16@PAGE
Lloh102:
	add	x0, x0, l_.str.16@PAGEOFF
	bl	_printf
Lloh103:
	adrp	x8, _root@PAGE
Lloh104:
	ldr	x0, [x8, _root@PAGEOFF]
	cbz	x0, LBB13_14
LBB13_13:                               ;   in Loop: Header=BB13_4 Depth=1
	bl	_free
LBB13_14:                               ;   in Loop: Header=BB13_4 Depth=1
	mov	x26, #0                         ; =0x0
	b	LBB13_16
LBB13_15:                               ;   in Loop: Header=BB13_16 Depth=2
	add	x26, x26, #8
	cmp	x26, #2592
	b.eq	LBB13_18
LBB13_16:                               ;   Parent Loop BB13_4 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	ldr	x0, [x21, x26]
	cbz	x0, LBB13_15
; %bb.17:                               ;   in Loop: Header=BB13_16 Depth=2
	bl	_free
	b	LBB13_15
LBB13_18:                               ;   in Loop: Header=BB13_4 Depth=1
Lloh105:
	adrp	x8, _nodes@PAGE
Lloh106:
	ldr	x0, [x8, _nodes@PAGEOFF]
	cbz	x0, LBB13_3
; %bb.19:                               ;   in Loop: Header=BB13_4 Depth=1
	bl	_free
	b	LBB13_3
LBB13_20:                               ;   in Loop: Header=BB13_4 Depth=1
Lloh107:
	adrp	x0, l_str.20@PAGE
Lloh108:
	add	x0, x0, l_str.20@PAGEOFF
	bl	_puts
Lloh109:
	adrp	x8, _root@PAGE
Lloh110:
	ldr	x0, [x8, _root@PAGEOFF]
	cbnz	x0, LBB13_13
	b	LBB13_14
LBB13_21:
	add	x0, sp, #24
	mov	x1, #0                          ; =0x0
	bl	_gettimeofday
	ldr	x8, [sp, #24]
	ldr	x9, [sp, #8]
	sub	x8, x8, x9
	scvtf	d0, x8
	ldr	w8, [sp, #32]
	ldr	w9, [sp, #16]
	sub	w8, w8, w9
	scvtf	d1, w8
	mov	x8, #145685290680320            ; =0x848000000000
	movk	x8, #16686, lsl #48
	fmov	d2, x8
	fdiv	d1, d1, d2
	fadd	d0, d1, d0
	str	d0, [sp]
Lloh111:
	adrp	x0, l_.str.18@PAGE
Lloh112:
	add	x0, x0, l_.str.18@PAGEOFF
	bl	_printf
	ldur	x8, [x29, #-96]
Lloh113:
	adrp	x9, ___stack_chk_guard@GOTPAGE
Lloh114:
	ldr	x9, [x9, ___stack_chk_guard@GOTPAGEOFF]
Lloh115:
	ldr	x9, [x9]
	cmp	x9, x8
	b.ne	LBB13_23
; %bb.22:
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp, #448]            ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #432]            ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #416]            ; 16-byte Folded Reload
	ldp	x24, x23, [sp, #400]            ; 16-byte Folded Reload
	ldp	x26, x25, [sp, #384]            ; 16-byte Folded Reload
	ldp	x28, x27, [sp, #368]            ; 16-byte Folded Reload
	add	sp, sp, #464
	ret
LBB13_23:
	bl	___stack_chk_fail
	.loh AdrpLdrGotLdr	Lloh75, Lloh76, Lloh77
	.loh AdrpLdrGot	Lloh82, Lloh83
	.loh AdrpLdrGot	Lloh80, Lloh81
	.loh AdrpLdrGot	Lloh78, Lloh79
	.loh AdrpAdd	Lloh87, Lloh88
	.loh AdrpLdrGotLdr	Lloh84, Lloh85, Lloh86
	.loh AdrpAdd	Lloh89, Lloh90
	.loh AdrpLdr	Lloh96, Lloh97
	.loh AdrpLdrGotStr	Lloh93, Lloh94, Lloh95
	.loh AdrpLdrGot	Lloh91, Lloh92
	.loh AdrpLdr	Lloh103, Lloh104
	.loh AdrpAdd	Lloh101, Lloh102
	.loh AdrpLdrGotLdr	Lloh98, Lloh99, Lloh100
	.loh AdrpLdr	Lloh105, Lloh106
	.loh AdrpLdr	Lloh109, Lloh110
	.loh AdrpAdd	Lloh107, Lloh108
	.loh AdrpLdrGotLdr	Lloh113, Lloh114, Lloh115
	.loh AdrpAdd	Lloh111, Lloh112
	.cfi_endproc
                                        ; -- End function
	.globl	_DEBUG                          ; @DEBUG
.zerofill __DATA,__common,_DEBUG,4,2
	.globl	_root                           ; @root
.zerofill __DATA,__common,_root,8,3
	.globl	_nodes                          ; @nodes
.zerofill __DATA,__common,_nodes,8,3
	.globl	_node_count                     ; @node_count
.zerofill __DATA,__common,_node_count,4,2
	.section	__DATA,__data
	.globl	_max_nodes                      ; @max_nodes
	.p2align	2, 0x0
_max_nodes:
	.long	2916                            ; 0xb64

	.section	__TEXT,__cstring,cstring_literals
l_.str:                                 ; @.str
	.asciz	"root"

	.comm	_columns,2592,3                 ; @columns
l_.str.1:                               ; @.str.1
	.asciz	"C%d"

l_.str.2:                               ; @.str.2
	.asciz	"ERROR: Exceeded maximum node count\n"

	.comm	_row_info,8748,2                ; @row_info
	.comm	_row_starts,5832,3              ; @row_starts
	.comm	_puzzle,324,2                   ; @puzzle
	.comm	_solution_grid,324,2            ; @solution_grid
l_.str.4:                               ; @.str.4
	.asciz	"%i "

l_.str.6:                               ; @.str.6
	.asciz	"r"

l_.str.7:                               ; @.str.7
	.asciz	"Error opening file '%s'\n"

l_.str.8:                               ; @.str.8
	.asciz	"/app/Matrices/"

l_.str.9:                               ; @.str.9
	.asciz	"../%s\n"

l_.str.11:                              ; @.str.11
	.asciz	"line[%06d]: chars=%06zd, contents: %s"

l_.str.12:                              ; @.str.12
	.asciz	"%i %i %i %i %i %i %i %i %i"

l_.str.14:                              ; @.str.14
	.asciz	".matrix"

l_.str.15:                              ; @.str.15
	.asciz	"Error reading %s\n"

l_.str.16:                              ; @.str.16
	.asciz	"\nSolved in Iterations=%i\n\n"

l_.str.18:                              ; @.str.18
	.asciz	"Seconds to process %.3f\n"

l_str:                                  ; @str
	.asciz	"\nPuzzle:"

l_str.19:                               ; @str.19
	.asciz	"Error: line does not contain 9 integers"

l_str.20:                               ; @str.20
	.asciz	"\nNo solution found"

.subsections_via_symbols
