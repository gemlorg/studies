	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	w0, #10
	mov	w19, #10
	bl	_fac
	bl	_printInt
	mov	w0, #10
	bl	_rfac
	bl	_printInt
	mov	w0, #10
	bl	_mfac
	bl	_printInt
	mov	w0, #10
	bl	_ifac
	bl	_printInt
	mov	w0, #1
	mul	w8, w0, w19
	sub	w9, w19, #1                     ; =1
	cmp	w19, #0                         ; =0
	b.le	LBB0_2
LBB0_1:                                 ; %_B2
                                        ; =>This Inner Loop Header: Depth=1
	mov	w19, w9
	mov	w0, w8
	cmp	w19, #0                         ; =0
	b.gt	LBB0_1
LBB0_2:                                 ; %_B3
	bl	_printInt
Lloh0:
	adrp	x0, l__v24@PAGE
Lloh1:
	add	x0, x0, l__v24@PAGEOFF
	mov	w1, #60
	bl	_repStr
	bl	_printString
Lloh2:
	adrp	x0, l__v29@PAGE
Lloh3:
	add	x0, x0, l__v29@PAGEOFF
	bl	_printString
Lloh4:
	adrp	x0, l__v32@PAGE
Lloh5:
	add	x0, x0, l__v32@PAGEOFF
	bl	_printString
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	mov	w0, wzr
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh4, Lloh5
	.loh AdrpAdd	Lloh2, Lloh3
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_fac                            ; -- Begin function fac
	.p2align	2
_fac:                                   ; @fac
	.cfi_startproc
; %bb.0:                                ; %entry
	mov	w8, w0
	mov	w0, #1
	mul	w9, w0, w8
	sub	w10, w8, #1                     ; =1
	cmp	w8, #0                          ; =0
	b.le	LBB1_2
LBB1_1:                                 ; %_B5
                                        ; =>This Inner Loop Header: Depth=1
	mov	w8, w10
	mov	w0, w9
	cmp	w8, #0                          ; =0
	b.gt	LBB1_1
LBB1_2:                                 ; %_B6
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_rfac                           ; -- Begin function rfac
	.p2align	2
_rfac:                                  ; @rfac
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	w19, w0
	mov	w0, #1
	cbz	w19, LBB2_2
; %bb.1:                                ; %_B8
	sub	w0, w19, w0
	bl	_rfac
	mul	w0, w19, w0
LBB2_2:                                 ; %common.ret
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_mfac                           ; -- Begin function mfac
	.p2align	2
_mfac:                                  ; @mfac
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	w19, w0
	mov	w0, #1
	cbz	w19, LBB3_2
; %bb.1:                                ; %_B11
	sub	w0, w19, w0
	bl	_nfac
	mul	w0, w19, w0
LBB3_2:                                 ; %common.ret
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_ifac                           ; -- Begin function ifac
	.p2align	2
_ifac:                                  ; @ifac
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w1, w0
	mov	w0, #1
	bl	_ifac2f
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_repStr                         ; -- Begin function repStr
	.p2align	2
_repStr:                                ; @repStr
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x22, x21, [sp, #-48]!           ; 16-byte Folded Spill
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 48
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
Lloh6:
	adrp	x21, l__v81@PAGE
Lloh7:
	add	x21, x21, l__v81@PAGEOFF
	mov	w19, w1
	mov	x20, x0
	mov	w8, wzr
	mov	x0, x21
	cmp	w8, w19
	b.ge	LBB5_2
LBB5_1:                                 ; %_B21
                                        ; =>This Inner Loop Header: Depth=1
	mov	x0, x21
	mov	x1, x20
	bl	__concatStrings
	mov	w8, #1
	cmp	w8, w19
	b.lt	LBB5_1
LBB5_2:                                 ; %_B22
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh6, Lloh7
	.cfi_endproc
                                        ; -- End function
	.globl	_ifac2f                         ; -- Begin function ifac2f
	.p2align	2
_ifac2f:                                ; @ifac2f
	.cfi_startproc
; %bb.0:                                ; %entry
	cmp	w0, w1
	b.eq	LBB6_4
; %bb.1:                                ; %_B17
	b.le	LBB6_3
; %bb.2:                                ; %_B18
	mov	w0, #1
	ret
LBB6_3:                                 ; %_B19
	stp	x22, x21, [sp, #-48]!           ; 16-byte Folded Spill
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 48
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	add	w8, w0, w1
	cmp	w8, #0                          ; =0
	cinc	w8, w8, lt
	asr	w20, w8, #1
	mov	w19, w1
	mov	w1, w20
	bl	_ifac2f
	mov	w21, w0
	add	w0, w20, #1                     ; =1
	mov	w1, w19
	bl	_ifac2f
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	mul	w0, w21, w0
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
LBB6_4:                                 ; %common.ret
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_nfac                           ; -- Begin function nfac
	.p2align	2
_nfac:                                  ; @nfac
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	w19, w0
	mov	w0, #1
	cbz	w19, LBB7_2
; %bb.1:                                ; %_B13
	sub	w0, w19, w0
	bl	_mfac
	mul	w0, w0, w19
LBB7_2:                                 ; %common.ret
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_printInt                       ; -- Begin function printInt
	.p2align	2
_printInt:                              ; @printInt
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32                     ; =32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh8:
	adrp	x8, _dnl@PAGE
                                        ; kill: def $w0 killed $w0 def $x0
Lloh9:
	add	x8, x8, _dnl@PAGEOFF
	str	x0, [sp]
	mov	x0, x8
	bl	_printf
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32                     ; =32
	ret
	.loh AdrpAdd	Lloh8, Lloh9
	.cfi_endproc
                                        ; -- End function
	.globl	_printString                    ; -- Begin function printString
	.p2align	2
_printString:                           ; @printString
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	bl	_puts
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_error                          ; -- Begin function error
	.p2align	2
_error:                                 ; @error
	.cfi_startproc
; %bb.0:
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh10:
	adrp	x0, _runtimeError@PAGE
Lloh11:
	add	x0, x0, _runtimeError@PAGEOFF
	bl	_puts
	mov	w0, #1
	bl	_exit
	brk	#0x1
	.loh AdrpAdd	Lloh10, Lloh11
	.cfi_endproc
                                        ; -- End function
	.globl	_readInt                        ; -- Begin function readInt
	.p2align	2
_readInt:                               ; @readInt
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32                     ; =32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh12:
	adrp	x0, _dnl@PAGE
Lloh13:
	add	x0, x0, _dnl@PAGEOFF
	add	x8, sp, #12                     ; =12
	str	x8, [sp]
	bl	_scanf
	ldr	w0, [sp, #12]
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32                     ; =32
	ret
	.loh AdrpAdd	Lloh12, Lloh13
	.cfi_endproc
                                        ; -- End function
	.globl	_readString                     ; -- Begin function readString
	.p2align	2
_readString:                            ; @readString
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #48                     ; =48
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 48
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
Lloh14:
	adrp	x8, _stdin@PAGE
Lloh15:
	ldr	x2, [x8, _stdin@PAGEOFF]
	add	x0, sp, #8                      ; =8
	mov	x1, sp
	stp	xzr, xzr, [sp]
	bl	_getline
	ldr	x19, [sp, #8]
	cmn	x0, #1                          ; =1
	b.eq	LBB12_2
; %bb.1:                                ; %getlineSuccess
	add	x9, x0, x19
	mov	x0, x19
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	sturb	wzr, [x9, #-1]
	add	sp, sp, #48                     ; =48
	ret
LBB12_2:                                ; %getlineFailure
Lloh16:
	adrp	x0, _getlineError@PAGE
Lloh17:
	add	x0, x0, _getlineError@PAGEOFF
	bl	_puts
	mov	x0, x19
	bl	_free
	mov	w0, #1
	bl	_exit
	brk	#0x1
	.loh AdrpLdr	Lloh14, Lloh15
	.loh AdrpAdd	Lloh16, Lloh17
	.cfi_endproc
                                        ; -- End function
	.globl	__concatStrings                 ; -- Begin function _concatStrings
	.p2align	2
__concatStrings:                        ; @_concatStrings
	.cfi_startproc
; %bb.0:
	stp	x22, x21, [sp, #-48]!           ; 16-byte Folded Spill
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 48
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	mov	x19, x1
	mov	x20, x0
	bl	_strlen
	mov	x21, x0
	mov	x0, x19
	bl	_strlen
	add	x8, x21, x0
	add	x0, x8, #1                      ; =1
	bl	_malloc
	mov	x1, x20
	bl	_strcpy
	mov	x1, x19
	bl	_strcat
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__const
l__v14:                                 ; @_v14
	.space	1

l__v24:                                 ; @_v24
	.asciz	"="

l__v29:                                 ; @_v29
	.asciz	"hello */"

l__v32:                                 ; @_v32
	.asciz	"/* world"

l__v81:                                 ; @_v81
	.space	1

_dnl:                                   ; @dnl
	.asciz	"%d\n"

_runtimeError:                          ; @runtimeError
	.asciz	"runtime error"

	.p2align	4                               ; @getlineError
_getlineError:
	.asciz	"error: getline failed"

.subsections_via_symbols
