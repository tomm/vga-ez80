		.assume adl=1
		.org $40000
		jp start
		.align $40

		.db "MOS"
		.db 0 ; version
		.db 1 ; ADL

		include "lib/print.asm"

start:
		push iy

		; is the driver present (get version)
		xor a
		rst.lil 0x20

		or a
		jp z,@no_driver

		push af
		print_asciz "Found GPIO video driver version $"
		pop af
		call print_hexbyte
		print_asciz "\r\n"

		; set mode zero
		ld a,1
		ld l,0
		rst.lil 0x20
		
		; put stuff in video ram
		ld hl,0xb5000
		xor a
		ld b,255
	@loop:	ld (hl),a
		inc a
		inc hl
		djnz @loop

		; get modeinfo
		ld a,3
		rst.lil 0x20
		print_asciz "Screen height: $"
		; Width
		ld hl,(iy+9)
		call print_hex24
		print_crlf

		; get videosetup
		ld a,2
		rst.lil 0x20
		; scroll through MOS rom!
		ld hl,0
		ld (iy+1),hl		; set framebuffer fb_ptr
		; wait for frame_counter to increment before scrolling
	@@:	ld a,(iy+0)
		or a
		jr z,@b
		xor a
		ld (iy+0),a
		inc hl
		; exit after 255 frames
		xor a
		or l
		jr z,@endvid
		
		ld (iy+1),hl
		jr @b

	@endvid:
		; end video
		ld a,4
		rst.lil 0x20

	@exit:
		ld hl,0
		pop iy
		ret

	@no_driver:
		print_asciz "No GPIO video driver found\r\n"
		jr @exit
