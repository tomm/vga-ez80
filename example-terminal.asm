		; interrupt-based GPIO video scanout, V2.0
		.assume adl=1
		.org $40000
		jp start
		.align $40

		.db "MOS"
		.db 0 ; version
		.db 1 ; ADL
		
		; to move this binary out of the way of subsequent
		; 0x40000 binaries that might be run (keeping video alive perhaps)
		;.align $10000

macro print_asciz literal
	jr @after
@lit:	asciz literal
@after:	ld hl,@lit
	ld bc,0
	xor a
	rst.lil 0x18
endmacro

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

		; clear screen
        	ld hl,(fb_ptr)
		ld bc,156*175-1
		push hl
		pop de
		inc de
		xor a
		ld (hl),a
		ldir

		call term_init

		; hook into rst10. only works with agondev mos
		ld a,0x61
		ld e,0x10
		ld hl,rst10_handler
		rst.lil 8

		; enable fb console (needs agondev mos)
		ld a,0x63 ; mos_api_startfbconsole
		ld hl,(fb_ptr)
		ld ix,(_current_mode)
		ld de,(ix+6)	; screen.width
		ld bc,(ix+9)	; screen.height
		rst.lil 8

		; print a nice message
		ld hl,msg	
	@@:	ld a,(hl)
		or a
		jr z,@f
		push hl

		call term_putch
		pop hl
		inc hl
		jr @b
	@@:
		pop iy
		ld hl,0
		ret

		include "gpiovideo.asm"
		include "math.asm"

rst10_handler:
		push af
		push bc
		push de
		push hl
		push ix
		push iy
		call term_putch
		pop iy
		pop ix
		pop hl
		pop de
		pop bc
		pop af
		ret.lil

msg:
		.ascii "0123456789 Hello, world!\r\nThis is a test of the terminal capabilities of the awesome eZ80 GPIO ViDEO!\r\n"
		.ascii "RGB332 256 colours and near-retina resolutions from 156x120 to 310x480!!\r\n"
		.ascii "\r\nLet's scroll!\r\n"
		.ascii "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce quis elit et ex placerat finibus"
		.ascii "id quis velit. Phasellus vitae bibendum felis, et condimentum purus. Nunc condimentum "
		;.ascii "justo ac magna consectetur"
		.db 0

FONT_WIDTH: .equ 4
FONT_HEIGHT: .equ 6
term_width:	.ds 1
term_height:	.ds 1
term_fg:	.ds 1
term_bg:	.ds 1
curs_x:		.ds 1
curs_y:		.ds 1


term_init:	; size the terminal. needed after mode change
		push ix
		ld ix,(_current_mode)

		ld hl,(ix+6)	; screen.width
		ld de, FONT_WIDTH
		call udiv24
		ld a,e
		ld (term_width),a

		ld hl,(ix+9)	; screen.height
		ld de, FONT_HEIGHT
		call udiv24
		ld a,e
		ld (term_height),a

		xor a
		ld (curs_x),a
		ld (curs_y),a
		ld (term_bg),a
		ld a,255
		ld (term_fg),a

		pop ix
		ret

term_putch:	; character in `a`
		push ix
		push iy
		push af

		; rotate through colours
		ld hl,term_fg
		dec (hl)
		jr nz,@f
		dec (hl)
	@@:
	
		ld ix,(_current_mode)

		; find character y position
		ld hl,(ix+6)	; screen.width
		add hl,hl	; FONT_HEIGHT is 6
		push hl
		pop de
		add hl,hl
		add hl,de	; hl=screen.width*6

		; hl=screen.width*8*curs_y
		ld de,0
		ld a,(curs_y)
		ld e,a
		call umul24

		; seek x character pos in framebuffer
		ld a,(curs_x)
		ld b,a
		ld c,FONT_WIDTH
		mlt bc
		add hl,bc

		; add base framebuffer address
		ld de,(fb_ptr)
		add hl,de
		pop af

		; handle special characters
		cp 13
		jp z,@handle_cr
		cp 10
		jp z,@handle_lf

		push hl
			; seek to character in font
			ld b,a
			ld c,6
			mlt bc
			ld hl,font_4x6
			add hl,bc

		; framebuffer char position in iy
		pop iy

		ld b,FONT_HEIGHT
	@lineloop:
		ld c,(hl)
		inc hl

		rlc c
		ld a,(term_bg)
		jr nc,@f
		ld a,(term_fg)
	@@:	ld (iy+0),a
		rlc c
		ld a,(term_bg)
		jr nc,@f
		ld a,(term_fg)
	@@:	ld (iy+1),a
		rlc c
		ld a,(term_bg)
		jr nc,@f
		ld a,(term_fg)
	@@:	ld (iy+2),a
		rlc c
		ld a,(term_bg)
		jr nc,@f
		ld a,(term_fg)
	@@:	ld (iy+3),a

		ld de,(ix+6)
		add iy,de
		djnz @lineloop

		ld a,(term_width)
		ld e,a
		ld a,(curs_x)
		inc a
		ld (curs_x),a
		cp e
		jr nz,@end

	
		; go to next line
		xor a
		ld (curs_x),a

	@handle_lf:
		ld a,(term_height)
		ld e,a
		ld a,(curs_y)
		inc a
		cp e
		jr z,@handle_scroll
		ld (curs_y),a


	@end:
		pop iy
		pop ix
		ret

	@handle_cr:
		xor a
		ld (curs_x),a
		jr @end

	@handle_scroll:
		; bc = screen.width * (screen.height-6)
		ld hl,(ix+6)	; screen.width
		ld de,(ix+9)	; screen.height
		dec de
		dec de
		dec de
		dec de
		dec de
		dec de
		call umul24
		push hl
		pop bc

		ld hl,(ix+6)	; screen.width
		add hl,hl	; FONT_HEIGHT is 6
		push hl
		pop de
		add hl,hl
		add hl,de	; hl=screen.width*6
		ld de,(fb_ptr)
		add hl,de

		ldir
		
		ld de,0
		ld a,(term_height)
		dec a
		ld e,a
		call @clear_line

		jp @end

	@clear_line:	; terminal line in de
		; number of bytes to wipe in bc
		ld hl,(ix+6)	; screen.width
		push de
		add hl,hl	; FONT_HEIGHT is 6
		push hl
		pop de
		add hl,hl
		add hl,de	; hl=screen.width*6
		push hl
		pop bc
		
		; find character y position
		ld hl,(ix+6)	; screen.width
		add hl,hl	; FONT_HEIGHT is 6
		push hl
		pop de
		add hl,hl
		add hl,de	; hl=screen.width*6
		pop de
		call umul24	; hl=screen.width*8*line_no
		ld de,(fb_ptr)
		add hl,de
		xor a
		ld (hl),a
		push hl
		pop de
		inc de
		dec bc
		ldir
		ret

font_4x6:
		include "font.inc"
