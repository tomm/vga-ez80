; https://github.com/sijnstra/agon-projects/blob/main/calc24/arith24.asm

;------------------------------------------------------------------------
;  arith24.asm 
;  24-bit ez80 arithmetic routines
;  Copyright (c) Shawn Sijnstra 2024
;  MIT license
;
;  This library was created as a tool to help make ez80
;  24-bit native assembly routines for simple mathematical problems
;  more widely available.
;  
;------------------------------------------------------------------------

;------------------------------------------------------------------------
; udiv24
; Unsigned 24-bit division
; Divides HLU by DEU. Gives result in DEU (and BC), remainder in HLU.
; 
; Uses AF BC DE HL
; Uses Restoring Division algorithm
;------------------------------------------------------------------------

udiv24:
	push	hl
	pop		bc	;move dividend to BCU
	ld		hl,0	;result
	and		a
	sbc		hl,de	;test for div by 0
	ret		z		;it's zero, carry flag is clear
	add		hl,de	;HL is 0 again
	ld		a,24	;number of loops through.
udiv1:
	push	bc	;complicated way of doing this because of lack of access to top bits
	ex		(sp),hl
	scf
	adc	hl,hl
	ex	(sp),hl
	pop	bc		;we now have bc = (bc * 2) + 1

	adc	hl,hl
	and	a		;is this the bug
	sbc	hl,de
	jr	nc,udiv2
	add	hl,de
;	dec	c
	dec	bc
udiv2:
	dec	a
	jr	nz,udiv1
	scf		;flag used for div0 error
	push	bc
	pop		de	;remainder
	ret

;------------------------------------------------------------------------
; umul24:	HL = HL*DE (unsigned)
; Preserves AF, BC, DE
; Uses a fast multiply routine.
;------------------------------------------------------------------------
umul24:
	push	DE 
	push	BC
	push	AF	
	push	HL
	pop		BC
    ld	 	a, 24 ; No. of bits to process 
    ld	 	hl, 0 ; Result
umul24_lp:
	add	hl,hl
	ex	de,hl
	add	hl,hl
	ex	de,hl
	jr	nc,umul24_nc
	add	hl,bc
umul24_nc: 
	dec	a
	jr	nz,umul24_lp
	pop	af
	pop	bc
	pop	de
	ret
