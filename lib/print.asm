	.assume adl=1

	macro print_asciz literal
		jr @after
	@lit:	asciz literal
	@after:	ld hl,@lit
		ld bc,0
		xor a
		rst.lil 0x18
	endmacro

	macro print_s str 
		push hl
		push bc
		ld hl,str
		ld bc,0
		xor a
		rst.lil $18
		pop bc
		pop hl
	endmacro

	macro print_c char
		ld a,char
		rst.lil $10
	endmacro

	macro print_crlf
		ld a, 13
		rst.lil $10
		ld a, 10
		rst.lil $10
	endmacro

print_hexbyte:
	push af
	srl a
	srl a
	srl a
	srl a
	; print high nibble
	call @print_hexnibble
	pop af
	and $f
	; print low nibble
@print_hexnibble:
	add a, 48
	cp 58
	jr c, @f
	add a, 7
@@:	rst.lil $10
	ret

print_hex24: ; print hex u24 in `hl`
	push ix
	push hl
	ld ix,0
	add ix,sp
	ld a,(ix+2)
	call print_hexbyte
	ld a,(ix+1)
	call print_hexbyte
	ld a,(ix+0)
	call print_hexbyte
	pop hl
	pop ix
	ret
