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

MODE:	.equ	8
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
		ld hl,FB_BASE
		ld (iy+1),hl

		ld a,MODE
		call video_set_mode

		; set up a 512x512 virtual framebuffer
		ld ix,scanline_offsets
		ld de,512
		ld bc,240
		ld iy,1
		call fill_scanline_offset_array

		; load image
		ld a,1		; api_mos_load
		ld hl,image_filename
		ld de,FB_BASE
		ld bc,[512*512]
		rst.lil 8

		; (hl) is frame_counter
		ld bc,0
		ld iy,video_setup
		ld hl,0	; angle
	@mainloop:
		; wait for next frame
		xor a
		ld (iy+0),a
	@@:
		ld a,(iy+0)
		or a
		jr z,@b

		inc hl
		push hl
			; 'hardware' scroll the framebuffer
			call sin
			ld hl,0
			add a,127
			ld h,a
			add hl,hl	; because 512 wide framebuffer
			push hl
			pop de

			pop hl
			push hl

			inc h		; move angle to next quadrant for x coord
			call sin
			add a,127
			ld e,a

			push de
			pop hl
			ld de,FB_BASE
			add hl,de
			
			ld (iy+1),hl
		pop hl
		jp @mainloop

		call video_stop
		pop iy
		pop ix
		ld hl,0
		ret

		.include "sin.asm"

image_filename:	db "examples/sc2000_512x512.raw",0
kb_event:	ds 4
scanline_offsets:
