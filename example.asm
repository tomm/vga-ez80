		; interrupt-based GPIO video scanout, V2.0
		.assume adl=1
		.org $40000
		jp start
		.align $40

		.db "MOS"
		.db 0 ; version
		.db 1 ; ADL

		include "gpiovideo.asm"

macro print_asciz literal
	jr @after
@lit:	asciz literal
@after:	ld hl,@lit
	ld bc,0
	xor a
	rst.lil 0x18
endmacro

MODE:	.equ	1

; GPIO usage:
; gpio-c 8 bits colour data
; gpio-d pin 6: vsync
; gpio-d pin 7: hsync

start:
		push iy

		print_asciz "EZ80 GPIO VGA\r\n"

		; Start the GPIO video output
		call video_init
		ld a,MODE
		call video_set_mode

		; set pixel data
        	ld hl,fb_ptr
		ld bc,(hl)
        	ld hl,[160*480]
        	ld de,1
        	xor a
        @@:
        	ld (bc),a
        	inc a
        	inc bc
        	or a
        	sbc hl,de
        	jr nz,@b

		; some white pixels in the 4 corners
		ld hl,fb_ptr
		ld hl,(hl)
		ld a,255
		ld (hl),a

		ld hl,fb_ptr
		ld hl,(hl)
		ld de,155
		add hl,de
		ld (hl),a

		ld hl,fb_ptr
		ld hl,(hl)
		ld de,[156*119]
		add hl,de
		ld (hl),a

		ld hl,fb_ptr
		ld hl,(hl)
		ld de,[156*119] + 155
		add hl,de
		ld (hl),a

		pop iy
		ld hl,0
		ret
