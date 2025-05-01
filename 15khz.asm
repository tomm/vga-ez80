
	macro line_scanout_x10_15khz
		call scanout_pixels_15khz
		call scanout_pixels_15khz
		call scanout_pixels_15khz
		call scanout_pixels_15khz
		call scanout_pixels_15khz
		call scanout_pixels_15khz
		call scanout_pixels_15khz
		call scanout_pixels_15khz
		call scanout_pixels_15khz
		call scanout_pixels_15khz
	endmacro

macro SYNC_PULSE_15KHZ mask, endcount, next_handler
		push af
		push bc
		push hl
		in0 a,(TMR1_CTL) ; ACK

	; 142 cycles (-ve sync pulse)
		in0 a,(PD_DR)	; 4 cycles
		and 0b00111111  ; 2 cycles
		or mask		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 53
		REP_NOP 71
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

scanline_handler_vsync_15khz:
		SYNC_PULSE_15KHZ 0, 2, scanline_handler_frontporch_15khz
scanline_handler_frontporch_15khz:
		SYNC_PULSE_15KHZ 0b01000000, 9, scanline_handler_pixeldata_15khz
scanline_handler_pixeldata_15khz:
		push af
		push bc
		push hl
		in0 a,(TMR1_CTL) ; ACK

	; final scanline of front porch (line 10 of front porch)
	; 142 cycles (-ve sync pulse)
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111  ; 2 cycles
		or 0		; 2 cycles to match exact timing of SYNC_PULSE_15KHZ macro
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 53
		REP_NOP 71
		or 0b10000000	; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
	; 70 cycles of front porch
		push iy			; 5 cycles
		ld iy,(fb_scanline_offsets)	; 8 cycles
		REP_NOP 9
		REP_NOP 35
		ld hl,fb_ptr		; 4 cycles
		ld hl,(hl)		; 5 cycles
		push de			; 4 cycles
	; blank pixel area
		REP_NOP 940
		
	; 480 lines
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz
		line_scanout_x10_15khz

		; do hsync pulse of next blank line, then reti
		; to ensure the timer for the line after isn't missed

		; horizontal back porch
		REP_NOP 24
		; hsync pulse
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111		; 2 cycles
		or 0
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 53
		REP_NOP 71
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles

		; mark end of frame
		ld hl,frame_counter
		inc (hl)

		; go to vertical back porch next
		xor a
		ld (_section_line_number),a
		ld bc,scanline_handler_backporch
		ld hl,(_timer1_int_vector)
		ld (hl),bc

		pop de
		pop iy
		pop hl
		pop bc
		pop af
		ei
		reti.lil

scanline_handler_backporch:
		SYNC_PULSE_15KHZ 0b01000000, 32, scanline_handler_vsync_15khz

; Total 1176 cycles per scanline (minus 7 consumed by 'call')
scanout_pixels_15khz:
	; 24 cycles (horizontal back porch)
	;               ; 7 cycles consumed by 'call'
		REP_NOP 5
		REP_NOP 12

	; 142 cyles hsync pulse (-ve)
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 55
		REP_NOP 71
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles

	; 70 cyles (horizontal front porch)
		xor a			; 1 cycle
		out0 (PC_DR),a	; 4 cycles
		REP_NOP 3
		REP_NOP 35

		; update the framebuffer pointer
		ld hl,(fb_ptr)		; 7 cycles
		ld bc,(iy+0)		; 6 cycles
		add hl,bc		; 1 cycle
		lea iy,iy+3		; 3 cycles

		ld de,PC_DR		; 4 cycles
		ld bc,310		; 4 cycles
		otirx			; 2 (+ 3*310 accounted for in next section)

	; 940 cycles (pixel data)
	;  - 930 cycles from otirx scanout
		out (PC_DR),a	; 3 cycles clear pixel data
		nop
		ret		; 6 cycles
