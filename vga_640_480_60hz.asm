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

; Total lines: 525
;          480 lines visible
;           10 lines front porch
;            2 lines vsync pulse
;           33 lines back porch

macro HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT endcount
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 b,(PD_DR)	; 4 cycles
		res 7,b		; 2 cycles
		out0 (PD_DR),b	; 4 cycles
		; 71 cycles asserted
		UART0_RX_POLL_OR_AUDIO_46_CYC

		; 3 cycles compute de-assert hsync
		or 0b10000000		; 2 cycles (hsync off)
		ld b,a			; 1 cycles

		; 11 cycles increment _section_line_number
		ld hl,_section_line_number	; 4 cycles
		ld a,(hl)			; 2
		inc a				; 1
		cp endcount			; 2
		ld (hl),a			; 2

		REP_NOP 6
		ld l,b
		out0 (PD_DR),l 	; 4 cycles
endmacro

macro HSYNC_VSYNC_PULSE_31KHZ endcount, next_handler
		push af
		push bc
		push hl
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 71 cycles (-ve sync pulse)
		HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT endcount

	; 35 cycles back porch -- assert vsync at end of this (start of visible section)
		REP_NOP_10x 2
		REP_NOP 9
		res 6,l		; 2 cycles
		out0 (PD_DR),l	; 4 cycles

	; Then GTFO (in some visible image time)
		cp endcount
		jr z,@end_section
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

macro HSYNC_PULSE_31KHZ_END_VSYNC endcount, next_handler
		push af
		push bc
		push hl
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 71 cycles (-ve sync pulse)
		HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT endcount

	; 35 cycles back porch -- de-assert vsync at end of this (start of visible section)
		REP_NOP_10x 2
		REP_NOP 9
		set 6,l		; 2 cycles
		out0 (PD_DR),l	; 4 cycles

	; Then GTFO (in some visible area time)
		xor a
		ld (_section_line_number),a
		ld bc,next_handler
		ld hl,(_timer1_int_vector)
		ld (hl),bc

		push de
		ei

		; process contents of uart0_rx_buf
		ld hl,uart0_rx_buf
		ld de,(uart0_buf_pos)
		; zero the buffer length before we start pushing bytes to mos.
		; this allows the next scanline to begin pushing new data
		; without a race, PROVIDING that our pushing data here is
		; faster than new data accrues with each scanline (which will be so)
		ld (uart0_buf_pos),hl
	@loop:	or a
		sbc hl,de
		jr z,@end
		or a
		add hl,de
		ld c,(hl)
	.if USE_CUSTOM_KEYBOARD_BUFFER
		; push byte into our custom uart0 rx state machine
		push de
		push hl
		call inject_uart0_rx_byte
		pop hl
		pop de
	.else
		; push byte to MOS uart0 rx state machine (Rainbow MOS 2.5+)
		ld a,0x60
		push de
		push hl
		rst.lil 8
		pop hl
		pop de
	.endif
		inc hl
		jr @loop
	@end:
		pop de
		pop hl
		pop bc
		pop af

		; call pre_image_callback
		push hl
		ld hl,(pre_image_callback)
		ex (sp),hl
		ret		; pre_image_callback will reti.lil
endmacro

macro HSYNC_PULSE_31KHZ endcount, next_handler
		push af
		push bc
		push hl
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; 71 cycles (-ve sync pulse)
		HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT endcount

	; Then GTFO
		jr z,@end_section
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

; 480 scanline grille scanout (only draw every second scanline)
scan_vga_31khz_480p_60hz_grille:
vga_scanline_grille_handler_frontporch:
		HSYNC_PULSE_31KHZ 10, vga_scanline_grille_handler_vsync
vga_scanline_grille_handler_vsync:
		HSYNC_VSYNC_PULSE_31KHZ 2, vga_scanline_grille_handler_backporch_firstline
vga_scanline_grille_handler_backporch_firstline:
		HSYNC_PULSE_31KHZ_END_VSYNC 1, vga_scanline_grille_handler_backporch
vga_scanline_grille_handler_backporch:
		HSYNC_PULSE_31KHZ 32, vga_scanline_grille_handler_pixeldata
vga_scanline_grille_handler_pixeldata:
		push af
		push bc
		push hl
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; Setup visible line
		; 71 cycles hsync
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 b,(PD_DR)	; 4 cycles
		res 7,b		; 2 cycles
		out0 (PD_DR),b	; 4 cycles
		; 71 cycles asserted
		push de
		UART0_RX_POLL_OR_AUDIO_46_CYC
		or 0b10000000		; 2 cycles (hsync off)
		ld l,a
		ld a,(_section_line_number)	; 5 cycles
		ld bc,0				; 4 cycles
		ld c,a				; 1 cyc
		ld de,PC_DR		; 4 cycles
		out0 (PD_DR),l 	; 4 cycles
		
	; 35 cycles h.back porch (5 unused for start of loop)
		REP_NOP 3
		; hl = &fb_scanline_offsets[_section_line_number]
		ld hl,(fb_scanline_offsets)	; 7 cycles
		add hl,bc			; 1 cycles
		add hl,bc			; 1 cycles
		add hl,bc			; 1 cycles
		; update the framebuffer pointer (26 cycles)
		ld bc,(hl)		; 5 cycles
		ld hl,(fb_ptr)		; 7 cycles
		add hl,bc		; 1 cycle
		ld bc,156		; 4 cycles

		; 5 cycles of h.back porch
		xor a
		otirx			; 2 + 3 (+ 3*155 accounted for in next section)
		; 470 cycles: visible area (3*155=465 from otirx)
		out (PC_DR),a		; 3 cycles clear pixel data
		nop

		; now in non-scanned line of grille
		; horiz front porch
		REP_NOP 2
	; 71 cycles hsync
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 b,(PD_DR)	; 4 cycles
		res 7,b		; 2 cycles
		out0 (PD_DR),b	; 4 cycles
		; 71 cycles asserted
		UART0_RX_POLL_OR_AUDIO_46_CYC
		or 0b10000000		; 2 cycles (hsync off)
		ld e,a			; 1 cycles
		; ack timer interrupt since we have overrun
		in0 a,(TMR1_CTL)	; 4 cycles
		ld hl,_section_line_number	; 4 cycles
		ld a,(hl)			; 2
		inc a				; 1
		cp 240				; 2
		jr z,@end_section	; 2 not taken, 4 taken
		ld (hl),a			; 2
		REP_NOP 1
		; de-assert hsync
		out0 (PD_DR),e 	; 4 cycles
		pop de
		pop hl
		pop bc
		pop af
		ei
		reti.lil

	@end_section:
		xor a
		; de-assert hsync
		out0 (PD_DR),e 	; 4 cycles

		; mark end of frame
		ld (hl),a		; zero the _section_line_number
		ld hl,frame_counter
		inc (hl)
		ld bc,vga_scanline_grille_handler_frontporch
		ld hl,(_timer1_int_vector)
		ld (hl),bc
		pop de
		pop hl
		pop bc
		pop af
		ei
		reti.lil

; Full 480 line image scanout
scan_vga_31khz_480p_60hz:
vga_scanline_handler_frontporch:
		; only 9 because 1 line's hsync is handled by end of vga_scanline_handler_pixeldata (image section)
		HSYNC_PULSE_31KHZ 9, vga_scanline_handler_vsync
vga_scanline_handler_vsync:
		HSYNC_VSYNC_PULSE_31KHZ 2, vga_scanline_handler_backporch_firstline
vga_scanline_handler_backporch_firstline:
		HSYNC_PULSE_31KHZ_END_VSYNC 1, vga_scanline_handler_backporch
vga_scanline_handler_backporch:
		HSYNC_PULSE_31KHZ 32, vga_scanline_handler_pixeldata
vga_scanline_handler_pixeldata:
		push af
		push bc
		push hl
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_31KHZ_PRT

	; Setup first line of visible area
		; 71 cycles hsync
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 b,(PD_DR)	; 4 cycles
		res 7,b		; 2 cycles
		out0 (PD_DR),b	; 4 cycles
		; 71 cycles asserted
		push de
		UART0_RX_POLL_OR_AUDIO_46_CYC
		push ix			; 5 cycles
		push iy			; 5 cycles
		ld ix,480		; 5 cycles. loop counter
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles

		; 35 cycles h.back porch (5 unused for start of loop)
		; update the framebuffer pointer (26 cycles)
		ld iy,(fb_scanline_offsets)	; 8 cycles
		ld hl,(fb_ptr)		; 7 cycles
		ld bc,(iy+0)		; 6 cycles
		add hl,bc		; 1 cycle
		ld de,PC_DR		; 4 cycles
		ld bc,156		; 4 cycles
	; 480 lines
	@loop:
		xor a			; 1 cycle
		; 5 cycles of h.back porch
		otirx			; 2 + 3 (+ 3*155 accounted for in next section)
		; 470 cycles: visible area (3*155=465 from otirx)
		out (PC_DR),a		; 3 cycles clear pixel data
		nop
		; 12 cycles front porch (10 eaten by HSYNC_ONLY setup)
		REP_NOP 2
		in0 b,(PD_DR)	; 4 cycles
		res 7,b		; 2 cycles
		out0 (PD_DR),b	; 4 cycles
		; 71 cycles hsync
		UART0_RX_POLL_OR_AUDIO_46_CYC
		; update the framebuffer pointer (25 cycles)
		ld hl,(fb_ptr)		; 7 cycles
		lea iy,iy+3		; 3 cycles
		ld bc,(iy+0)		; 6 cycles
		add hl,bc		; 1 cycle
		REP_NOP 2

		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
		; 35 cycles back porch (5 cycles unused to donate to start of loop)
		REP_NOP 10
		; long-winded way of using ix as a 16-bit loop counter...
		dec ix			; 2 cycles
		ld b,ixh		; 2 cycles
		ld a,ixl		; 2 cycles
		or b			; 1 cycles
		ld bc,156		; 4 cycles
		ld de,PC_DR		; 4 cycles
		jp nz,@loop		; 5 cycles

		; now in first line of vertical front-porch. Have already provided hsync
		; so now we clean up and reti
		pop iy				; 5 cycles
		pop ix				; 5 cycles
		pop de

		; ack timer interrupt since we have overrun
		in0 a,(TMR1_CTL)

		; mark end of frame
		ld hl,frame_counter	; 4 cycles
		inc (hl)		; 2 cycles
		; mark: go to vertical front porch next
		xor a				; 1 cycle
		ld (_section_line_number),a	; 5 cycles
		ld bc,vga_scanline_handler_frontporch ; 4 cycles
		ld hl,(_timer1_int_vector)	; 7 cycles
		ld (hl),bc			; 5 cycles
		pop hl
		pop bc
		pop af
		ei
		reti.lil
