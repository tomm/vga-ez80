		; interrupt-based GPIO video scanout, V2.0
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
        	ld hl,[156*160]
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

		ld hl,(fb_ptr)
		ld de,[156*119]
		add hl,de
		ld (hl),a

		ld hl,(fb_ptr)
		ld de,[156*119] + 155
		add hl,de
		ld (hl),a

		
		; Main loop. On any keypress, set a pixel to the
		; value of the vkey
		ld hl,(fb_ptr)
	@mainloop:	
		ld de,kb_event
		call get_next_key_event

		or a
		jr z,@mainloop		; no event

		ld a,(de)	; ascii
		cp 'q'
		jr z,@exit
		

		inc de
		inc de
		ld a,(de)	; fabgl vkey
	
		; set pixel with vkey
		ld (hl),a
		inc hl
		
		jr @mainloop
	@exit:
		call video_stop
		pop iy
		ld hl,0
		ret

kb_event:	ds 4
