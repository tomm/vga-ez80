		.assume adl=1
		.org $40000
		jp start
		.align $40

		.db "MOS"
		.db 0 ; version
		.db 1 ; ADL

USE_CUSTOM_KEYBOARD_BUFFER: .equ 1
		include "gpiovideo.asm"
		include "lib/print.asm"

MODE:	.equ	0
FB_BASE: .equ 0x50000

; GPIO usage:
; gpio-c 8 bits colour data
; gpio-d pin 6: vsync
; gpio-d pin 7: hsync

start:
		push ix
		push iy

		print_asciz "EZ80 GPIO VGA\r\n"

		; Start the GPIO video output
		call video_init

		; Position framebuffer and scanline offsets
		ld iy,video_setup
		ld hl,scanline_offsets
		ld (iy+4),hl
		ld hl,[FB_BASE+50] ; offset to right a bit to centre saturn image
		ld (iy+1),hl

		ld a,MODE
		call video_set_mode

		; set up a 256x120 virtual framebuffer
		ld ix,scanline_offsets
		ld de,256
		ld bc,240
		ld iy,2		; double-scan (and grille means 120 lines out of 480)
		call fill_scanline_offset_array

		; load image
		ld a,1		; api_mos_load
		ld hl,image_filename
		ld de,FB_BASE
		ld bc,[256*179]
		rst.lil 8

	@mainloop:
		call wibble_wobble

		; wait for next frame
		ld iy,video_setup
		xor a
		ld (iy+0),a
	@@:
		ld a,(iy+0)
		cp 4
		jr c,@b
		
		jp @mainloop

		call video_stop
		pop iy
		pop ix
		ld hl,0
		ret

angle:		dl 0

wibble_wobble:
		push iy
		ld b,80
		ld iy,scanline_offsets+[3*158]
		ld hl,scanline_offsets+[3*160]
		ld ix,(angle)
	@loop:
		ld de,(iy+0)
		inc ix
		push af
		push bc
			push hl
			push ix
			pop hl
			call sin
			pop hl
			add 127
			srl a
			ld c,a
			ld a,80
			sub a,b
			ld b,a
			mlt bc
			ld a,b
			

			add a,e
			ld e,a
			ld (hl),de
		pop bc
		pop af
		dec iy
		dec iy
		dec iy
		dec iy
		dec iy
		dec iy
		inc hl
		inc hl
		inc hl
		djnz @loop

		ld (angle),ix

		pop iy
		ret
			

		.include "sin.asm"

image_filename:	db "examples/saturn_256x179.raw",0
kb_event:	ds 4
scanline_offsets: ds [240*3]
