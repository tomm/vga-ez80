; Timings in 18.432Hz cpu cycles:
; 640x480x60hz vga spec is 585.73 cycles (31.778 us)
; Our PRT timer impl is 588 cycles (0.3% slower - within VESA spec)

; VGA spec:
; Total scanline: 585.73 cycles (31.778 us)
;     pixel data: 468.58 cycles (25.422 us)
;     front porch: 11.72 cycles ( 0.636 us)
;     hsync pulse: 70.28 cycles ( 3.813 us)
;     back porch:  35.15 cycles ( 1.907 us)

; Our Implementation:
; Total scanline:    588 cycles (31.901 us)
;     pixel data:    470 cycles
;     front porch:    12 cycles
;     hsync pulse:    71 cycles
;     back porch      35 cycles

;     pixel data:    470 cycles
;     front porch:    12 cycles
;     hsync pulse:    71 cycles
;     back porch      35 cycles

; Total lines: 525
;          480 lines visible
;           10 lines front porch
;            2 lines vsync pulse
;           33 lines back porch

macro HSYNC_ONLY
	; 71 cycles (-ve sync pulse)
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 a,(PD_DR)	; 4 cycles
		res 7,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		; 71 cycles asserted
		REP_NOP 65
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
	ld a,0x25	; 0x25 determined by experiment. will be invalidated if any code on interrupt entry are changed
			; (or PRT counter reset value changed)
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

	; 71 cyles hsync pulse (-ve)
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
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 71 cycles (-ve sync pulse)
		HSYNC_ONLY

	; 35 cycles back porch -- assert vsync at end of this (start of visible section)
		REP_NOP 29
		res 6,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles

	; Then GTFO (in some visible image time)
		ld a,(_section_line_number)
		inc a
		cp endcount
		jr z,@end_section
		ld (_section_line_number),a
		exx
		ex af,af'
		ei
		reti.lil

	@end_section:
		xor a
		ld (_section_line_number),a
		ld bc,next_handler
		ld hl,(_timer1_int_vector)
		ld (hl),bc
		exx
		ex af,af'
		ei
		reti.lil
endmacro

macro HSYNC_PULSE_31KHZ endcount, next_handler
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 71 cycles (-ve sync pulse)
		HSYNC_ONLY

	; 35 cycles back porch -- de-assert vsync at end of this (start of visible section)
		REP_NOP 29
		set 6,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles

	; Then GTFO (in some visible area time)
		ld a,(_section_line_number)
		inc a
		cp endcount
		jr z,@end_section
		ld (_section_line_number),a
		exx
		ex af,af'
		ei
		reti.lil

	@end_section:
		xor a
		ld (_section_line_number),a
		ld bc,next_handler
		ld hl,(_timer1_int_vector)
		ld (hl),bc
		exx
		ex af,af'
		ei
		reti.lil
endmacro

macro HSYNC_PULSE_31KHZ_IMG endcount, next_handler
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 71 cycles (-ve sync pulse)
		HSYNC_ONLY

	; 35 cycles back porch
		REP_NOP 26
		ld bc,PC_DR	; 4
		ld a,0x1	; 2
		out (bc),a	; 3

	; Then GTFO (in some visible area time)
		inc a
		out (bc),a
		inc a
		out (bc),a
		inc a
		out (bc),a
		inc a
		out (bc),a
		inc a
		out (bc),a
		inc a
		out (bc),a
		xor a
		out (bc),a

		ld a,(_section_line_number)
		inc a
		cp endcount
		jr z,@end_section
		ld (_section_line_number),a
		exx
		ex af,af'
		ei
		reti.lil

	@end_section:
		xor a
		ld (_section_line_number),a
		ld bc,next_handler
		ld hl,(_timer1_int_vector)
		ld (hl),bc
		exx
		ex af,af'
		ei
		reti.lil
endmacro

;vga_scanline_handler_vsync:
;		HSYNC_VSYNC_PULSE_31KHZ 2, vga_scanline_handler_backporch
;vga_scanline_handler_backporch:
;		HSYNC_PULSE_31KHZ 33, vga_scanline_handler_img1
;vga_scanline_handler_img1:
;		HSYNC_PULSE_31KHZ_IMG 240, vga_scanline_handler_img2
;vga_scanline_handler_img2:
;		HSYNC_PULSE_31KHZ_IMG 240, vga_scanline_handler_frontporch
;vga_scanline_handler_frontporch:
;		HSYNC_PULSE_31KHZ 10, vga_scanline_handler_vsync

vga_scanline_handler_vsync:
		HSYNC_VSYNC_PULSE_31KHZ 2, vga_scanline_handler_backporch
vga_scanline_handler_backporch:
		HSYNC_PULSE_31KHZ 32, vga_scanline_handler_pixeldata
vga_scanline_handler_img1:
		HSYNC_PULSE_31KHZ_IMG 239, vga_scanline_handler_frontporch
vga_scanline_handler_frontporch:
		HSYNC_PULSE_31KHZ 9, vga_scanline_handler_vsync
vga_scanline_handler_pixeldata:
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; final scanline of vert. back porch (line 33 of back porch)
	; 71 cycles (-ve sync pulse)
		HSYNC_ONLY
	; 35 cycles of h.back porch
		push ix			; 5 cycles
		push iy			; 5 cycles
		; loop counter in ix
		ld ix,480		; 5 cycles
		REP_NOP 1
		ld iy,(fb_scanline_offsets)	; 8 cycles
		REP_NOP 11
	; blank pixel area
		REP_NOP 470

	; Setup first line of visible area
		; 12 cycles front porch (10 eaten by HSYNC_ONLY setup)
		REP_NOP 2
		; 71 cycles hsync
		HSYNC_ONLY
		; 35 cycles h.back porch (5 unused for start of loop)
		REP_NOP 4
		; update the framebuffer pointer (26 cycles)
		xor a			; 1 cycle
		ld hl,(fb_ptr)		; 7 cycles
		ld bc,(iy+0)		; 6 cycles
		add hl,bc		; 1 cycle
		lea iy,iy+3		; 3 cycles
		ld de,PC_DR		; 4 cycles
		ld bc,156		; 4 cycles
	; 480 lines
	@loop:
		; 5 cycles of h.back porch
		otirx			; 2 + 3 (+ 3*155 accounted for in next section)
		; 470 cycles: visible area (3*155=465 from otirx)
		out (PC_DR),a		; 3 cycles clear pixel data
		nop
		nop
		; 12 cycles front porch (10 eaten by HSYNC_ONLY setup)
		REP_NOP 2
		in0 a,(PD_DR)	; 4 cycles
		res 7,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		; 71 cycles hsync
		REP_NOP 40

		; update the framebuffer pointer (25 cycles)
		ld hl,(fb_ptr)		; 7 cycles
		ld bc,(iy+0)		; 6 cycles
		add hl,bc		; 1 cycle
		lea iy,iy+3		; 3 cycles
		ld de,PC_DR		; 4 cycles
		ld bc,156		; 4 cycles

		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
		; 35 cycles back porch (5 cycles unused to donate to start of loop)
		REP_NOP 8
		; long-winded way of using ix as a 16-bit loop counter...
		dec ix			; 2 cycles
		push bc			; 4 cycles
		lea bc,ix+0		; 3
		ld a,b			; 1
		or a,c			; 1
		pop bc			; 4
		ld a,0			; 2 cycles
		jp nz,@loop		; 5 cycles when taken (4 not taken)

		REP_NOP 6

		; now in 'visible area' portion of first line of v.front porch
		; do hsync pulse of next blank line, then reti
		; to ensure the timer for the line after isn't missed

		; 470 cycle 'visible area' minus 4 from not-taken `jp`
		; mark end of frame
		ld hl,frame_counter	; 4 cycles
		inc (hl)		; 2 cycles
		; mark: go to vertical front porch next
		xor a				; 1 cycle
		ld (_section_line_number),a	; 5 cycles
		ld bc,vga_scanline_handler_frontporch ; 4 cycles
		ld hl,(_timer1_int_vector)	; 7 cycles
		ld (hl),bc			; 5 cycles
		pop iy				; 5 cycles
		pop ix				; 5 cycles
		REP_NOP 428

		; 12 cycles front porch (10 eaten by HSYNC_ONLY setup)
		REP_NOP 2

		; 71 cycles hsync
		HSYNC_ONLY

		; ack timer interrupt since we have overrun
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		
		exx
		ex af,af'
		ei
		reti.lil
