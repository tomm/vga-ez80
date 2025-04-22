	; interrupt-based GPIO video scanout, V2.0
	.assume adl=1
	.org $40000
	jp start
	.align $40

	.db "MOS"
	.db 0 ; version
	.db 1 ; ADL

	include "print.asm"

PB_DR: .equ 0x9a
PB_DDR: .equ 0x9b
PB_ALT1: .equ 0x9c
PB_ALT2: .equ 0x9d

PC_DR: .equ 0x9e
PC_DDR: .equ 0x9f
PC_ALT1: .equ 0xa0
PC_ALT2: .equ 0xa1

PD_DR: .equ 0xa2
PD_DDR: .equ 0xa3
PD_ALT1: .equ 0xa4
PD_ALT2: .equ 0xa5

TMR1_CTL: .equ 0x83
TMR1_DR_L: .equ 0x84
TMR1_DR_H: .equ 0x85

PRT_INT_ENABLE: .equ 0b01000000
PRT_CLK_DIV_4: .equ 0b0000
PRT_CLK_DIV_16: .equ 0b0100
PRT_CLK_DIV_64: .equ 0b1000
PRT_CLK_DIV_256: .equ 0b1100
PRT_MODE_CONTINUOUS: .equ 0b10000
PRT_ENABLE: .equ 0b1
PRT_START: .equ 0b10

	macro REP_NOP num
		.fillbyte 0
		.ds num
	endmacro

	macro line_scanout_pixels
		call scanout_line_with_pixeldata
	endmacro

	macro line_scanout_pixels_x10
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
	endmacro

	macro line_scanout_x10_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
	endmacro

	macro line_scanout_x24_quadrupled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_doubled
	endmacro

	macro line_scanout_x30_tripled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_doubled
	endmacro

; GPIO usage:
; gpio-c 8 bits colour data
; gpio-d pin 6: vsync
; gpio-d pin 7: hsync

start:
		push iy

		print_asciz "EZ80 GPIO VGA\r\n"

		; Start the GPIO video output
		call video_init
		ld a,0
		call start_mode

		pop iy
		ld hl,0
		ret

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

		pop iy
		ld hl,0
		ret

video_init:
		; turn off vblank interupt, since it screws everything up
		in0 a,(PB_ALT2)
		res 1,a
		out0 (PB_ALT2),a
		in0 a,(PB_ALT1)
		res 1,a
		out0 (PB_ALT1),a
		in0 a,(PB_DDR)
		set 1,a
		out0 (PB_DDR),a

		; set port c for output
		xor a
		out0 (PC_DDR),a
		out0 (PC_ALT1),a
		out0 (PC_ALT2),a
		out0 (PC_DR),a

		; set port d pin 6&7 to output
		in0 a,(PD_DDR)
		and 0b00111111
		out0 (PD_DDR),a

		in0 a,(PD_ALT1)
		and 0b00111111
		out0 (PD_ALT1),a

		in0 a,(PD_ALT2)
		and 0b00111111
		out0 (PD_ALT2),a

		; clear timer1 first before messing with interrupt handlers
		xor a
		out0 (TMR1_CTL),a

		; find address of ram timer1 interrupt vector
		; - so we can manipulate it directly and quickly
		ld.sis hl,(0x10c)
		inc hl ; skip over call (0xc3)
		ld hl,(hl) ; load address of RAM jump target
		ld a,0xc3
		ld (hl),a ; write a call opcode
		inc hl ; skip over CALL opcode
		ld (_timer1_int_vector),hl

		ret

start_mode:	; mode in `a`
		ld l,a
		ld h,12
		mlt hl
		ld de,_modes
		add hl,de
		push hl

		; set timer1 interrupt handler to start of frame (vsync)
		ld bc,(hl)	; _modes[0] value: scanline handler
		ld hl,(_timer1_int_vector)
		ld (hl),bc
		xor a
		ld (_section_line_number),a

		pop hl
		inc hl
		inc hl
		inc hl
		; set up timer1
		ld hl,(hl)	; load _modes[1] value (scanline length in CPU clocks / 4)
		out0 (TMR1_DR_L),l
		out0 (TMR1_DR_H),h
		ld a, [PRT_ENABLE | PRT_CLK_DIV_4 | PRT_START | PRT_INT_ENABLE | PRT_MODE_CONTINUOUS]
		out0 (TMR1_CTL),a
	
		ret

_modes:
		; initial scanline handler, scanline length in CPU clocks divided by 4, width, height
		.dl vga_scanline_handler_vsync, 147, 153, 120
		.ds 12
		.ds 12
		.ds 12
		.dl scanline_handler_vsync_15khz, 294, 310, 240
		

; incremented at end of image data scanout
frame_counter:
		.db 0
fb_ptr:
		.dl	0xa0000

_timer1_int_vector:
		.ds 3
_section_line_number:
		.db 0

; Timings in 18.432Hz cpu cycles:
; 640x480x60hz vga spec is 585.73 cycles (31.778 us)
; Our PRT timer impl is 588 cycles (31.901 us)

; VGA spec:
; Total scanline: 585.73 cycles (31.778 us)
;     hsync pulse: 70.28 cycles ( 3.813 us)
;     front porch: 35.15 cycles ( 1.907 us)
;     pixel data: 468.58 cycles (25.422 us)
;     back porch   11.72 cycles ( 0.636 us)

; Our Implementation:
; Total scanline:    588 cycles (31.901 us)
;     hsync pulse:    71 cycles
;     front porch:    35 cycles
;     pixel data:    470 cycles
;     back porch      12 cycles

; Total lines: 525
;            2 lines vsync pulse
;           10 lines front porch
;          480 lines visible
;           33 lines back porch

macro END_INT
	pop hl
	pop bc
	pop af
	ei
	reti.lil
endmacro

	.include "15khz.asm"

macro SYNC_PULSE_31KHZ mask, endcount, next_handler
		push af
		push bc
		push hl
		in0 a,(TMR1_CTL) ; ACK

	; 71 cycles (-ve sync pulse)
		in0 a,(PD_DR)	; 4 cycles
		and 0b00111111  ; 2 cycles
		or mask		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 53
		or 0b10000000	; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles

	; Then GTFO (in some front-porch time)
		ld a,(_section_line_number)
		inc a
		cp endcount
		jr z,@end_section
		ld (_section_line_number),a
		pop hl
		pop bc
		pop af
		ei
		reti.lil

	@end_section:
		xor a
		ld (_section_line_number),a
		ld bc,next_handler
		ld hl,(_timer1_int_vector)
		ld (hl),bc
		pop hl
		pop bc
		pop af
		ei
		reti.lil
endmacro

vga_scanline_handler_vsync:
		SYNC_PULSE_31KHZ 0, 2, vga_scanline_handler_frontporch
vga_scanline_handler_frontporch:
		SYNC_PULSE_31KHZ 0b01000000, 9, vga_scanline_handler_pixeldata
vga_scanline_handler_pixeldata:
		push af
		push bc
		push hl
		in0 a,(TMR1_CTL) ; ACK

	; final scanline of front porch (line 10 of front porch)
	; 71 cycles (-ve sync pulse)
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111  ; 2 cycles
		or 0		; 2 cycles to match exact timing of SYNC_PULSE_31KHZ macro
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 53
		or 0b10000000	; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
	; 35 cycles of front porch
		REP_NOP 22
		ld hl,fb_ptr		; 4 cycles
		ld hl,(hl)		; 5 cycles
		push de			; 4 cycles
	; blank pixel area
		REP_NOP 470
		
	; 480 lines
		REP_NOP 4	; scanout expects 4 cycles overrun into back porch
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled
		line_scanout_x24_quadrupled

		; do hsync pulse of next blank line, then reti
		; to ensure the timer for the line after isn't missed

		; horizontal back porch
		REP_NOP 8 ; only 8, not 12. 4 cycle overrun from pixel scanout
		; hsync pulse
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111		; 2 cycles
		or 0
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 53
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles

		; mark end of frame
		ld hl,frame_counter
		inc (hl)

		; go to vertical back porch next
		xor a
		ld (_section_line_number),a
		ld bc,vga_scanline_handler_backporch
		ld hl,(_timer1_int_vector)
		ld (hl),bc

		pop de
		END_INT

vga_scanline_handler_backporch:
		SYNC_PULSE_31KHZ 0b01000000, 32, vga_scanline_handler_vsync


setup_gpio:

		ret


; Total 588 cycles per scanline (minus 7 consumed by 'call')
scanout_line_with_pixeldata:
	; 12 cycles (horizontal back porch)
	;               ; 7 cycles consumed by 'call'
	REP_NOP 1

	; 71 cyles hsync pulse (-ve)
	in0 a,(PD_DR)	; 4 cycles
	and 0b01111111		; 2 cycles
	out0 (PD_DR),a	; 4 cycles
	REP_NOP 55
	or 0b10000000		; 2 cycles (hsync off)
	out0 (PD_DR),a 	; 4 cycles

	; 35 cyles (horizontal front porch)
	xor a			; 1 cycle
	out0 (PC_DR),a	; 4 cycles
	REP_NOP 20
	ld de,PC_DR		; 4 cycles
	ld bc,155		; 4 cycles
	otirx			; 2 (+ 3*155 accounted for in next section)

	; 470 cycles (pixel data)
	;  - 465 cycles from otirx scanout
	out (PC_DR),a	; 3 cycles clear pixel data
	ret		; 6 cycles - 4 of these cycles overrun into horizontal back porch

; Total 588 cycles per scanline (minus 7 consumed by 'call')
scanout_line_doubled:
	; 12 cycles (horizontal back porch)
	;               ; 7 cycles consumed by 'call'
	;               ; 4 cycles consumed by 'ret' at end of pixel data scanout
	REP_NOP 1

	; 71 cyles hsync pulse (-ve)
	in0 a,(PD_DR)	; 4 cycles
	and 0b01111111		; 2 cycles
	out0 (PD_DR),a	; 4 cycles
	REP_NOP 55
	or 0b10000000		; 2 cycles (hsync off)
	out0 (PD_DR),a 	; 4 cycles

	; 35 cyles (horizontal front porch)
	xor a			; 1 cycle
	out0 (PC_DR),a	; 4 cycles
	REP_NOP 17
	ld de,PC_DR		; 4 cycles
	ld bc,155		; 4 cycles
	or a			; 1
	sbc hl,bc		; 2
	otirx			; 2 (+ 3*155 accounted for in next section)

	; 470 cycles (pixel data)
	;  - 465 cycles from otirx scanout
	out (PC_DR),a	; 3 cycles clear pixel data
	ret		; 6 cycles - 4 of these cycles overrun into horizontal back porch
