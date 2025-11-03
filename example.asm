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

mode: db 0

; GPIO usage:
; gpio-c 8 bits colour data
; gpio-d pin 6: vsync
; gpio-d pin 7: hsync

set_mode:
		print_asciz "Setting mode 0x"
		ld a,(mode)
		call print_hexbyte
		print_crlf
		ld a,(mode)
		call video_set_mode
		ret

inc_mode:
		ld hl,mode
		ld a,(hl)
		inc a
		cp 10
		jr nc,@to_zero
		ld (hl),a
		ret
	@to_zero:
		xor a
		ld (hl),a
		ret

start:
		push iy

		print_asciz "EZ80 GPIO VGA\r\n"

		; Start the GPIO video output
		call video_init
		call set_mode

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

		; load image
		ld a,1		; api_mos_load
		ld de,[156*2]
		ld hl,(fb_ptr)
		add hl,de
		ld de,image_filename
		ex de,hl
		ld bc,20280
		rst.lil 8
		
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
		inc de
		ld a,(de)
		or a
		jr z,@mainloop		; key up
		
		dec de
		ld a,(de)	; fabgl vkey
	
		; set pixel with vkey
		ld (hl),a
		inc hl

		; increment mode
		call inc_mode
		call set_mode
		
		jr @mainloop
	@exit:
		call video_stop
		pop iy
		ld hl,0
		ret

kb_event:	ds 4
image_filename: db "pictures/andromeda.raw",0
