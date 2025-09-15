; Timings in 18.432Hz cpu cycles:
; 640x480x60hz vga spec is 585.73 cycles (31.778 us)
; Our PRT timer impl is 584 cycles (31.684 us)

; VGA spec:
; Total scanline: 585.73 cycles (31.778 us)
;     hsync pulse: 70.28 cycles ( 3.813 us)
;     front porch: 35.15 cycles ( 1.907 us)
;     pixel data: 468.58 cycles (25.422 us)
;     back porch   11.72 cycles ( 0.636 us)

; Our Implementation:
; Total scanline:    584 cycles (31.901 us)
;     hsync pulse:    70 cycles
;     front porch:    35 cycles
;     pixel data:    467 cycles
;     back porch      12 cycles

; Total lines: 525
;            2 lines vsync pulse
;           10 lines front porch
;          480 lines visible
;           33 lines back porch

macro HSYNC_ONLY
	; 70 cycles (-ve sync pulse)
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 a,(PD_DR)	; 4 cycles
		res 7,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		; 70 cycles asserted
		REP_NOP 64
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
endmacro

macro DEJITTER_31KHZ_PRT
	; sample interrupt timing jitter from PRT register (16 cycles)
	; can read every 3 cycles, while PRT decrements every 4.
	; TMR1_DR_L into bc. XXX we assume bc container TMR1_CTL
	inc bc		; 1 cycle
	in a,(bc)	; 3
	in d,(bc)	; 3
	in e,(bc)	; 3
	in h,(bc)	; 3

	; sum it (3 cycles)
	add a,d
	add a,e
	add a,h

	; convert to number of cycles lagged (by the instruction executing when interrupt was due)
	ld b,a
	ld a,0x17	; 0x17 determined by experiment. will be invalidated if any code on interrupt entry are changed
			; (eg replacing pushes with exx)
	sub b
	; now cycles lagged is in `a`
	; negate it by a computed jump into nops
	; (8 cycles for self-modify & jr)
	and 0xf
	ld (@cmpjmp+1),a	; overwrite target of next instruction (jr)
@cmpjmp:
	jr $+2	; dummy jump target, as it will be overwritten
	nop
	nop
	nop
	nop

	nop
	nop
	nop
	nop

	nop
; isn't longest instruction 9 cycles (??), so don't need the rest of nops...
; but that isn't considering wait states...
; but this code isn't designed to work with wait states...
	nop
	nop
	nop

	nop
	nop
	nop
	nop
endmacro

; Total 588 cycles per scanline
macro scanline_pixel_scanout_31khz
	; 12 cycles (horizontal back porch)
	;               ; 8 cycles consumed by looping code (ex af,af' && dec a && jp nz,@loop && ex af,af')
		REP_NOP 4

	; 70 cyles hsync pulse (-ve)
		HSYNC_ONLY

	; 35 cyles (horizontal front porch)
		; these 2 are redundant really. can remove
		xor a			; 1 cycle
		out0 (PC_DR),a	; 4 cycles

		; update the framebuffer pointer
		ld hl,(fb_ptr)		; 7 cycles
		ld bc,(iy+0)		; 6 cycles
		add hl,bc		; 1 cycle
		lea iy,iy+3		; 3 cycles

		ld de,PC_DR		; 4 cycles
		ld bc,156		; 4 cycles
		otirx			; 2 + 3 (+ 3*155 accounted for in next section)

	; 470 cycles (pixel data)
	;  - 465 cycles from otirx scanout
		out (PC_DR),a	; 3 cycles clear pixel data
		nop
		nop
endmacro

macro HSYNC_VSYNC_PULSE_31KHZ endcount, next_handler
		push af
		push bc
		push de
		push hl
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 70 cycles (-ve sync pulse)
		HSYNC_ONLY

	; 35 cycles front porch -- assert vsync at end of this (start of visible section)
		REP_NOP 29
		res 6,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles

	; Then GTFO (in some front-porch time)
		ld a,(_section_line_number)
		inc a
		cp endcount
		jr z,@end_section
		ld (_section_line_number),a
		pop hl
		pop de
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
		pop de
		pop bc
		pop af
		ei
		reti.lil
endmacro

macro HSYNC_PULSE_31KHZ endcount, next_handler
		push af
		push bc
		push de
		push hl
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 70 cycles (-ve sync pulse)
		HSYNC_ONLY

	; 35 cycles front porch -- assert vsync at end of this (start of visible section)
		REP_NOP 29
		set 6,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles

	; Then GTFO (in some front-porch time)
		ld a,(_section_line_number)
		inc a
		cp endcount
		jr z,@end_section
		ld (_section_line_number),a
		pop hl
		pop de
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
		pop de
		pop bc
		pop af
		ei
		reti.lil
endmacro

macro HSYNC_PULSE_31KHZ_IMG endcount, next_handler
		push af
		push bc
		push de
		push hl
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 70 cycles (-ve sync pulse)
		HSYNC_ONLY

	; 35 cycles front porch -- assert vsync at end of this (start of visible section)
		REP_NOP 29
		set 6,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles

	; Then GTFO (in some front-porch time)
		ld bc,PC_DR
		ld a,0xff		; some color
		out (bc),a
		dec a
		out (bc),a
		dec a
		out (bc),a
		dec a
		out (bc),a
		dec a
		out (bc),a
		dec a
		out (bc),a
		dec a
		out (bc),a
		dec a
		out (bc),a
		dec a

		ld a,(_section_line_number)
		inc a
		cp endcount
		jr z,@end_section
		ld (_section_line_number),a
		pop hl
		pop de
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
		pop de
		pop bc
		pop af
		ei
		reti.lil
endmacro

vga_scanline_handler_vsync:
		HSYNC_VSYNC_PULSE_31KHZ 2, vga_scanline_handler_frontporch
vga_scanline_handler_frontporch:
		HSYNC_PULSE_31KHZ 10, vga_scanline_handler_img1
vga_scanline_handler_img1:
		HSYNC_PULSE_31KHZ_IMG 240, vga_scanline_handler_img2
vga_scanline_handler_img2:
		HSYNC_PULSE_31KHZ_IMG 240, vga_scanline_handler_backporch
vga_scanline_handler_backporch:
		HSYNC_PULSE_31KHZ 33, vga_scanline_handler_vsync
vga_scanline_handler_pixeldata:
		push af
		push bc
		push de
		push hl
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; final scanline of front porch (line 10 of front porch)
	; 70 cycles (-ve sync pulse)
		HSYNC_ONLY
	; 35 cycles of front porch
		ex af,af'		; 1 cycle
		push af			; 4 cycles
		push iy			; 5 cycles
		push de			; 4 cycles
		ld iy,(fb_scanline_offsets)	; 8 cycles
		; loop counter in a'
		ld a,240		; 2 cycles
		REP_NOP 11
	; blank pixel area
		REP_NOP 470
		
	; 480 lines
		REP_NOP 7	; scanline_pixel_scanout_31khz expects 7 cycles overrun into back porch
	@loop:
		ex af,af'		; 1 cycle
		scanline_pixel_scanout_31khz
		REP_NOP 8
		scanline_pixel_scanout_31khz
		ex af,af'		; 1 cycle
		dec a			; 1 cycle
		jp nz,@loop		; 5 cycles when taken

		; do hsync pulse of next blank line, then reti
		; to ensure the timer for the line after isn't missed

		; horizontal back porch
		REP_NOP 6 ; only 6, not 12. 6 cycle overrun from pixel scanout
		; hsync pulse
		HSYNC_ONLY

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
		pop iy
		pop af
		ex af,af'
		pop hl
		pop de
		pop bc
		pop af
		ei
		reti.lil
