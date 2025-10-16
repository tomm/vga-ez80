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

; Total lines: 449
;          350 lines visible
;           37 lines front porch
;            2 lines vsync pulse
;           60 lines back porch

macro PVE_HSYNC_ONLY
	; 71 cycles (-ve sync pulse)
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 a,(PD_DR)	; 4 cycles
		set 7,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		; 71 cycles asserted
		REP_NOP 65
		res 7,a			; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
endmacro

macro PVE_HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT endcount
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 a,(PD_DR)	; 4 cycles
		set 7,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		; 71 cycles asserted
		REP_NOP 52
		res 7,a			; 2 cycles (hsync off)
		ld e,a			; 1 cycles

		ld a,(_section_line_number)	; 5 cycles
		inc a				; 1
		cp endcount			; 1
		ld (_section_line_number),a	; 5 cycles

		out0 (PD_DR),e 	; 4 cycles
endmacro

macro PVE_HSYNC_NVE_VSYNC_PULSE_31KHZ endcount, next_handler
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 71 cycles (-ve sync pulse)
		PVE_HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT endcount

	; 35 cycles back porch -- assert vsync at end of this (start of visible section)
		REP_NOP 29
		res 6,e		; 2 cycles
		out0 (PD_DR),e	; 4 cycles

	; Then GTFO (in some visible image time)
		cp endcount
		jr z,@end_section
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

UART0_serial_RX:	IN0		A,(UART0_REG_LSR)	; Get the line status register
			AND 		UART_LSR_RDY		; Check for characters in buffer
			RET		Z			; Just ret (with carry clear) if no characters
			IN0		A,(UART0_REG_RBR)	; Read the character from the UART receive buffer
			SCF 					; Set the carry flag
			RET

macro UART0_RX_POLL
		; do uart0 poll
		push af
		push bc
		push de
		push hl
		ei
		call UART0_serial_RX
		jr nc, @skip
        	ld c,a
        	ld a,0x60
        	rst.lil 8
	@skip:
		pop hl
		pop de
		pop bc
		pop af
endmacro

macro PVE_HSYNC_PULSE_31KHZ_END_VSYNC endcount, next_handler
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 71 cycles (-ve sync pulse)
		PVE_HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT endcount

	; 35 cycles back porch -- de-assert vsync at end of this (start of visible section)
		REP_NOP 29
		set 6,e		; 2 cycles
		out0 (PD_DR),e	; 4 cycles

	; Then GTFO (in some visible area time)
		xor a
		ld (_section_line_number),a
		ld bc,next_handler
		ld hl,(_timer1_int_vector)
		ld (hl),bc
		exx
		ex af,af'

		UART0_RX_POLL

		reti.lil
endmacro

macro PVE_HSYNC_PULSE_31KHZ endcount, next_handler
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 71 cycles (-ve sync pulse)
		PVE_HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT endcount

	; Then GTFO
		jr z,@end_section
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

vga_640_350_scanline_handler_frontporch:
		PVE_HSYNC_PULSE_31KHZ 36, vga_640_350_scanline_handler_vsync
vga_640_350_scanline_handler_vsync:
		PVE_HSYNC_NVE_VSYNC_PULSE_31KHZ 2, vga_640_350_scanline_handler_backporch_firstline
vga_640_350_scanline_handler_backporch_firstline:
		PVE_HSYNC_PULSE_31KHZ_END_VSYNC 1, vga_640_350_scanline_handler_backporch
vga_640_350_scanline_handler_backporch:
		PVE_HSYNC_PULSE_31KHZ 58, vga_640_350_scanline_handler_pixeldata
vga_640_350_scanline_handler_pixeldata:
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; final scanline of vert. back porch (line 33 of back porch)
	; 71 cycles (-ve sync pulse)
		PVE_HSYNC_ONLY
	; 35 cycles of h.back porch
		push ix			; 5 cycles
		push iy			; 5 cycles
		; loop counter in ix
		ld ix,350		; 5 cycles
		REP_NOP 1
		ld iy,(fb_scanline_offsets)	; 8 cycles
		REP_NOP 11
	; blank pixel area
		REP_NOP 470

	; Setup first line of visible area
		; 12 cycles front porch (10 eaten by HSYNC_ONLY setup)
		REP_NOP 2
		; 71 cycles hsync
		PVE_HSYNC_ONLY
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
		set 7,a		; 2 cycles
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

		res 7,a			; 2 cycles (hsync off)
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
		ld bc,vga_640_350_scanline_handler_frontporch ; 4 cycles
		ld hl,(_timer1_int_vector)	; 7 cycles
		ld (hl),bc			; 5 cycles
		pop iy				; 5 cycles
		pop ix				; 5 cycles
		REP_NOP 428

		; 12 cycles front porch (10 eaten by HSYNC_ONLY setup)
		REP_NOP 2

		; 71 cycles hsync
		PVE_HSYNC_ONLY

		; ack timer interrupt since we have overrun
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		
		exx
		ex af,af'
		ei

		UART0_RX_POLL

		reti.lil
