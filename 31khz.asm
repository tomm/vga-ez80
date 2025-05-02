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

; Total 588 cycles per scanline
macro scanline_pixel_scanout_31khz
	; 12 cycles (horizontal back porch)
	;               ; 8 cycles consumed by looping code (ex af,af' && dec a && jp nz,@loop && ex af,af')
		REP_NOP 4

	; 71 cyles hsync pulse (-ve)
		in0 a,(PD_DR)	; 4 cycles
		and 0b01111111		; 2 cycles
		or 0		; 2 cycles to match exact timing of SYNC_PULSE_31KHZ macro
		out0 (PD_DR),a	; 4 cycles
		REP_NOP 53
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles

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
		pop iy
		pop af
		ex af,af'
		pop hl
		pop bc
		pop af
		ei
		reti.lil

vga_scanline_handler_backporch:
		SYNC_PULSE_31KHZ 0b01000000, 32, vga_scanline_handler_vsync
