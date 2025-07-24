	.assume adl=1
	.org $40000
	jp start
	.align $40

	.db "MOS"
	.db 0 ; version
	.db 1 ; ADL

	include "print.asm"

	macro REP_NOP num
		.fillbyte 0
		.ds num
	endmacro
TMR1_CTL: .equ 0x83
PRT_CLK_DIV_4: .equ 0b0000
PRT_CLK_DIV_16: .equ 0b0100
PRT_CLK_DIV_64: .equ 0b1000
PRT_CLK_DIV_256: .equ 0b1100
PRT_MODE_CONTINUOUS: .equ 0b10000
PRT_ENABLE: .equ 0b1
PRT_START: .equ 0b10

PC_DR: .equ 0x9e
PC_DDR: .equ 0x9f
PC_ALT1: .equ 0xa0
PC_ALT2: .equ 0xa1

PD_DR: .equ 0xa2
PD_DDR: .equ 0xa3
PD_ALT1: .equ 0xa4
PD_ALT2: .equ 0xa5

; GPIO usage:
; gpio-c 8 bits colour data
; gpio-d pin 6: vsync
; gpio-d pin 7: hsync

start:
	push iy

	print_asciz "EZ80 GPIO VGA\r\n"

	di
	call setup_gpio
	call start_scanout

	pop iy
	ld hl,0
	ret

setup_gpio:
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

	ret

macro back_porch
	; 12 cycles (horizontal back porch)
	;               ; 8 cycles consumed by looping code (ex af,af' && dec a && jp nz,@loop && ex af,af')
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111		; 2 cycles
		nop
		nop
		out0 (PD_DR),a	; 4 cycles
endmacro

macro hsync_pulse
	; 71 cyles hsync pulse (-ve)
		REP_NOP 65
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
endmacro

; 586 cycles
macro scanline
	; 12 cycles (horizontal back porch)
		back_porch
	; 71 cyles hsync pulse (-ve)
		hsync_pulse
	; 35 cyles (horizontal front porch)
		REP_NOP 34
		xor a
	; 468 cycles (pixel data)
	;  - 465 cycles from otirx scanout
		out (PC_DR),a	; 3 cycles clear pixel data
		REP_NOP 465
endmacro

start_scanout:
		; 2 vsync+hsyncs
		back_porch
		hsync_pulse
		; assert vsync (at start of visible region)
		REP_NOP 28
		res 6,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		xor a
		REP_NOP 468

		back_porch
		hsync_pulse
		REP_NOP 34
		xor a
		REP_NOP 468

		; one more hsync that de-asserts the vsync at the visible region
		back_porch
		hsync_pulse
		; assert vsync
		REP_NOP 28
		set 6,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		xor a
		REP_NOP 468

		; 522 hsyncs
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 
		scanline 

	; FINAL SCANLINE. we copy here so we can fit the loop JP into the timings
	; 12 cycles (horizontal back porch)
		back_porch
	; 71 cyles hsync pulse (-ve)
		hsync_pulse
	; 35 cyles (horizontal front porch)
		REP_NOP 34
		xor a

	; 468 cycles (pixel data)
	;  - 465 cycles from otirx scanout
		out (PC_DR),a	; 3 cycles clear pixel data
		REP_NOP 460

		jp start_scanout
