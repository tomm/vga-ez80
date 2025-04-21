	; interrupt-based GPIO video scanout, V1.0

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


		call setup_gpio


		; set a timer1 interrupt handler
		ld e,0xc
		ld a,0x14 ; mos_setintvector
		ld hl,timer_int_handler
		rst.lil 8

		; set up timer1 (147*4 = 588 cycles)
		ld hl,147 ; 147 * 4 / 18.432 (MHz) = 31.901 microsecond horizontal scan (VGA spec is 31.778)
		out0 (TMR1_DR_L),l
		out0 (TMR1_DR_H),h
		ld a, [PRT_ENABLE | PRT_CLK_DIV_4 | PRT_START | PRT_INT_ENABLE | PRT_MODE_CONTINUOUS]
		out0 (TMR1_CTL),a

		; set pixel data
		ld hl,[160*480]
		ld de,1
		ld bc,pixeldata
		xor a
	@@:
		ld (bc),a
		inc a
		inc bc
		or a
		sbc hl,de
		jr nz,@b

		; white line at 479
		ld hl,pixeldata+[153*478]
		ld a,0xff
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl

		xor a
		ld (vsync_counter),a
	application_loop:
        	; wait for a vsync
        @@:	ld a,(vsync_counter)
        	cp 60
        	jr nz,@b
		xor a
		ld (vsync_counter),a

		ld a,'X'
		rst.lil 0x10

		; just mess with the pixel data
		ld hl,pixeldata+[160*100]
		ld b,255
	@@:
		inc (hl)
		inc hl
		djnz @b
		jr application_loop

		pop iy
		ld hl,0
		ret

vsync_counter:
		.db 0
_vga_line_number:
		.dl 	0 ; 0-524
_vga_sync_bitmask:
		.db 0	; 0 on vsync+hsync lines, 0b01000000 on hsync-only lines
fb_ptr:
		.dl	0

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

timer_int_handler:
		push af
		push bc
		push hl
		in0 a,(TMR1_CTL) ; ACK

		ld a,(_vga_sync_bitmask)
		ld b,a
		
	; 71 cyles (hsync pulse (-ve)) -> +0 spare
		;          (with -ve vsync pulse)
		in0 a,(PD_DR)	; 4 cycles
		and 0b00111111		; 2 cycles
		or b			; 1 cycles
		out0 (PD_DR),a	; 4 cycles
			; 28 cycles: prepare jump to scanline handler
		ld hl,0				; 4 cycles
		ld bc,(_vga_line_number)	; 7 cycles
		srl b				; 2 cycles
		rr c				; 2 cycles
		add hl,bc			; 1 cycle
		add hl,bc			; 1 cycle
		add hl,bc			; 1 cycle
		ld bc,scanline_handlers_lookup	; 4 cycles
		add hl,bc			; 1 cycle
		ld hl,(hl)			; 5 cycles

		REP_NOP 23

		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles

		jp (hl)				; 3 cycles
		; scanline handler will arrive at exact start of front porch
		; (so will have 35 cycles before pixel data should be scanned out)

scanline_handler_vsync:
		ld hl,(_vga_line_number)
		
		; is this line zero? if so, there is one more line of vsync
		ld a,l
		or a
		jr z,@f
		
		ld a,0b01000000 ; hsync only for next line (2)
		ld (_vga_sync_bitmask),a
		inc hl
		ld (_vga_line_number),hl
		END_INT
	@@:
		xor a ; hsync+vsync
		ld (_vga_sync_bitmask),a
		inc hl
		ld (_vga_line_number),hl
		END_INT

scanline_handler_frontporch:
		ld a,0b01000000 ; hsync only
		ld (_vga_sync_bitmask),a

		ld hl,(_vga_line_number)
		inc hl
		ld (_vga_line_number),hl

		END_INT

scanline_handler_pixeldata:
scanline_handler_pixeldata_120:
	; we have 35 cycles of front porch
		; 19 cycles incrementing _vga_line_number past the pixel scanout region
		; (plus 1 -- see end of function where 1 hsync is emitted for first line of vertical back porch)
		ld hl,(_vga_line_number)	; 7 cycles
		ld bc,481			; 4 cycles
		add hl,bc			; 1 cycle
		ld (_vga_line_number),hl	; 7 cycles
		; 16 more
		REP_NOP 8
		ld hl,pixeldata		; 4 cycles
		push de			; 4 cycles

	; miss a line (scan out no pixel data)
		REP_NOP 470
		
	; 479 lines
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

		; do hsync pulse of next blank line, then reti
		; to ensure the timer for the line after isn't missed

		; back porch
		REP_NOP 12
		; hsync pulse
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 55
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
		
		pop de

		END_INT

scanline_handler_pixeldata_160:
	; we have 35 cycles of front porch
		; 19 cycles incrementing _vga_line_number past the pixel scanout region
		; (plus 1 -- see end of function where 1 hsync is emitted for first line of vertical back porch)
		ld hl,(_vga_line_number)	; 7 cycles
		ld bc,481			; 4 cycles
		add hl,bc			; 1 cycle
		ld (_vga_line_number),hl	; 7 cycles
		; 16 more
		REP_NOP 8
		ld hl,pixeldata		; 4 cycles
		push de			; 4 cycles

	; miss a line (scan out no pixel data)
		REP_NOP 470
		
	; total 479 lines
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled
		line_scanout_x30_tripled

		; do hsync pulse of next blank line, then reti
		; to ensure the timer for the line after isn't missed

		; back porch
		REP_NOP 12
		; hsync pulse
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 55
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
		
		pop de

		END_INT
scanline_handler_pixeldata_240:
	; we have 35 cycles of front porch
		; 19 cycles incrementing _vga_line_number past the pixel scanout region
		; (plus 1 -- see end of function where 1 hsync is emitted for first line of vertical back porch)
		ld hl,(_vga_line_number)	; 7 cycles
		ld bc,481			; 4 cycles
		add hl,bc			; 1 cycle
		ld (_vga_line_number),hl	; 7 cycles
		; 16 more
		REP_NOP 8
		ld hl,pixeldata		; 4 cycles
		push de			; 4 cycles

	; miss a line (scan out no pixel data)
		REP_NOP 470
		
	; total 479 lines
	; 100 lines
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
	; 100 lines
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
	; 100 lines
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
	; 100 lines
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
	; 70 lines
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
		line_scanout_x10_doubled
	; 9 lines
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_with_pixeldata
		call scanout_line_doubled
		call scanout_line_with_pixeldata

		; do hsync pulse of next blank line, then reti
		; to ensure the timer for the line after isn't missed

		; back porch
		REP_NOP 12
		; hsync pulse
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 55
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
		
		pop de

		END_INT
scanline_handler_pixeldata_480:
	; we have 35 cycles of front porch
		; 19 cycles incrementing _vga_line_number past the pixel scanout region
		; (plus 1 -- see end of function where 1 hsync is emitted for first line of vertical back porch)
		ld hl,(_vga_line_number)	; 7 cycles
		ld bc,481			; 4 cycles
		add hl,bc			; 1 cycle
		ld (_vga_line_number),hl	; 7 cycles
		; 16 more
		REP_NOP 8
		ld hl,pixeldata		; 4 cycles
		push de			; 4 cycles

	; miss a line (scan out no pixel data)
		REP_NOP 470
		
	; total 479 lines
	; 100 lines
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
	; 100 lines
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
	; 100 lines
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
	; 100 lines
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
	; 70 lines
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
		line_scanout_pixels_x10
	; 9 lines
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata
		call scanout_line_with_pixeldata

		; do hsync pulse of next blank line, then reti
		; to ensure the timer for the line after isn't missed

		; back porch
		REP_NOP 12
		; hsync pulse
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 55
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
		
		pop de

		END_INT

scanline_handler_backporch:
		ld a,0b01000000 ; hsync only
		ld (_vga_sync_bitmask),a

		ld hl,(_vga_line_number)
		inc hl
		ld (_vga_line_number),hl

		END_INT

scanline_handler_scanreturn:
		xor a	; vsync
		ld (_vga_sync_bitmask),a

		ld hl,0
		ld (_vga_line_number),hl
		
		ld a,(vsync_counter)
		inc a
		ld (vsync_counter),a

		END_INT
		
scanline_handlers_lookup:
		; vga 640x480 525 lines divided by 2
		; 2 lines (1 scanline doubled) of vblank
		.dl scanline_handler_vsync
		; 10 lines (5 scanline doubled) of front porch
		.dl scanline_handler_frontporch
		.dl scanline_handler_frontporch
		.dl scanline_handler_frontporch
		.dl scanline_handler_frontporch
		.dl scanline_handler_frontporch
		; 480 (240 doubled) of pixel data
		.dl scanline_handler_pixeldata
		.ds [239 * 3]	; skipped over as pixeldata handler runs for 480 lines with interrupts disabled
		; 33 lines (16 scanline doubled) of back porch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		.dl scanline_handler_backporch
		; scan return for the final line
		.dl scanline_handler_scanreturn

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

	in0 a,(PD_DR)
flashloop:
	push af
	ld a,'X'
	rst.lil 0x10
	pop af

	set 6,a
	res 7,a
	out0 (PD_DR),a
	; wait
	ld hl,1000000
	ld de,-1
@@:
	or a
	adc hl,de
	jr nz,@b
	
	res 6,a
	set 7,a
	out0 (PD_DR),a
	; wait
	ld hl,1000000
	ld de,-1
@@:
	or a
	adc hl,de
	jr nz,@b
	jp flashloop

	ret

; Total 588 cycles per scanline (minus 7 consumed by 'call')
scanout_line_with_pixeldata:
	; 12 cycles (horizontal back porch)
	;               ; 7 cycles consumed by 'call'
	REP_NOP 5

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
	ld bc,153		; 4 cycles
	otirx			; 2 (+ 3*153 accounted for in next section)

	; 470 cycles (pixel data)
	;  - 459 cycles from otirx scanout
	out0 (PC_DR),a	; 4 cycles clear pixel data
	nop

	ret		; 6 cycles

; Total 588 cycles per scanline (minus 7 consumed by 'call')
scanout_line_doubled:
	; 12 cycles (horizontal back porch)
	;               ; 7 cycles consumed by 'call'
	REP_NOP 5

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
	ld bc,153		; 4 cycles
	or a			; 1
	sbc hl,bc		; 2
	otirx			; 2 (+ 3*153 accounted for in next section)

	; 470 cycles (pixel data)
	;  - 459 cycles from otirx scanout
	out0 (PC_DR),a	; 4 cycles clear pixel data
	nop

	ret		; 6 cycles

pixeldata:
	ds [160*480]
