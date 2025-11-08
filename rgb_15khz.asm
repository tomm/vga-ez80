; Timings in 18.432Hz cpu cycles:
; 
; Generic 15khz modes.
;
; We follow exactly 2x slower horizontally than our 31khz vga_640_480_60hz.asm modes.
;
; Total scanline:   1176 cycles
;     pixel data:    940 cycles
;     front porch:    24 cycles
;     hsync pulse:   142 cycles
;     back porch      70 cycles
;
; There are two variants (both with same horizontal timings)
; One is 240p @ 60hz, the other is 480p @ 30hz (with scanline grille, so only
; 240 lines scanned out)
;
; 240p mode uses total lines: 262
;          240 lines visible
;            5 lines front porch
;            1 lines vsync pulse
;           16 lines back porch

macro HSYNC_ONLY_15KHZ
	; 71 cycles (-ve sync pulse)
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 a,(PD_DR)	; 4 cycles
		res 7,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		; 71 cycles asserted
		push af		; 4 cyc
		UART0_RX_POLL_32_CYC
		pop af		; 4 cyc
		REP_NOP 25
		REP_NOP 71
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
endmacro

macro DEJITTER_15KHZ_PRT
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

macro HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT_15KHZ endcount
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 a,(PD_DR)	; 4 cycles
		res 7,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		; 71 cycles asserted
		push af		; 4 cyc
		UART0_RX_POLL_32_CYC
		pop af		; 4 cyc
		REP_NOP 12
		REP_NOP 71
		or 0b10000000		; 2 cycles (hsync off)
		ld e,a			; 1 cycles

		ld a,(_section_line_number)	; 5 cycles
		inc a				; 1
		cp endcount			; 1
		ld (_section_line_number),a	; 5 cycles

		out0 (PD_DR),e 	; 4 cycles
endmacro

macro HSYNC_VSYNC_PULSE_15KHZ endcount, next_handler
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_15KHZ_PRT

	; 71 cycles (-ve sync pulse)
		HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT_15KHZ endcount

	; 35 cycles back porch -- assert vsync at end of this (start of visible section)
		REP_NOP 29
		REP_NOP 35
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

macro HSYNC_PULSE_15KHZ_END_VSYNC endcount, next_handler
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_15KHZ_PRT

	; 71 cycles (-ve sync pulse)
		HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT_15KHZ endcount

	; 35 cycles back porch -- de-assert vsync at end of this (start of visible section)
		REP_NOP 29
		REP_NOP 35
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
		push af
		push bc
		push de
		push hl
		ei

		; XXX not right place. should be 1 scanline before image
		ld hl,@callback_ret
		push hl
		ld hl,(pre_image_callback)
		jp (hl)
	@callback_ret:

		; process contents of uart0_rx_buf
		ld hl,uart0_rx_buf
		ld de,(uart0_buf_pos)
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
		; clear uart0 buf
		ld hl,uart0_rx_buf
		ld (uart0_buf_pos),hl

		pop hl
		pop de
		pop bc
		pop af

		reti.lil
endmacro

macro HSYNC_PULSE_15KHZ endcount, next_handler
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_15KHZ_PRT

	; 71 cycles (-ve sync pulse)
		HSYNC_PULSE_ONLY_WITH_SCANLINE_INCREMENT_15KHZ endcount

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

; 480 scanline grille scanout (only draw every second scanline)
scan_rgb_15khz_480p_30hz_grille:
_rgb_15khz_scanline_grille_handler_frontporch:
		HSYNC_PULSE_15KHZ 10, _rgb_15khz_scanline_grille_handler_vsync
_rgb_15khz_scanline_grille_handler_vsync:
		HSYNC_VSYNC_PULSE_15KHZ 2, _rgb_15khz_scanline_grille_handler_backporch_firstline
_rgb_15khz_scanline_grille_handler_backporch_firstline:
		HSYNC_PULSE_15KHZ_END_VSYNC 1, _rgb_15khz_scanline_grille_handler_backporch
_rgb_15khz_scanline_grille_handler_backporch:
		HSYNC_PULSE_15KHZ 32, _rgb_15khz_scanline_grille_handler_pixeldata
_rgb_15khz_scanline_grille_handler_pixeldata:
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_15KHZ_PRT

	; Setup visible line
		; 71 cycles hsync
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 a,(PD_DR)	; 4 cycles
		res 7,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		; 71 cycles asserted
		push af		; 4 cyc
		UART0_RX_POLL_32_CYC
		ld a,(_section_line_number)	; 5 cycles
		ld bc,0				; 4 cycles
		ld c,a				; 1 cyc
		; hl = &fb_scanline_offsets[_section_line_number]
		ld hl,(fb_scanline_offsets)	; 7 cycles
		add hl,bc			; 1 cycles
		add hl,bc			; 1 cycles
		add hl,bc			; 1 cycles
		REP_NOP 5
		REP_NOP 71
		pop af		; 4 cyc
		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles

		; 35 cycles h.back porch (5 unused for start of loop)
		REP_NOP 8
		REP_NOP 35
		; update the framebuffer pointer (26 cycles)
		xor a			; 1 cycle
		ld bc,(hl)		; 5 cycles
		ld hl,(fb_ptr)		; 7 cycles
		add hl,bc		; 1 cycle
		ld de,PC_DR		; 4 cycles
		ld bc,312		; 4 cycles

		; 5 cycles of h.back porch
		otirx			; 2 + 3 (+ 3*155 accounted for in next section)
		; 470 cycles: visible area (3*155=465 from otirx)
		out (PC_DR),a		; 3 cycles clear pixel data
		nop
		nop

		; now in non-scanned line of grille
		; horiz front porch
		REP_NOP 12
		REP_NOP 2
		; 71 cycles hsync
		HSYNC_ONLY_15KHZ
		
		; ack timer interrupt since we have overrun
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		
		; increment _section_line_number
		ld a,(_section_line_number)	; 5 cycles
		inc a				; 1
		cp 240			; 1
		ld (_section_line_number),a	; 5 cycles

	; Then GTFO
		jr z,@end_section
		exx
		ex af,af'
		ei
		reti.lil

	@end_section:
		; mark end of frame
		ld hl,frame_counter	; 4 cycles
		inc (hl)		; 2 cycles
		xor a
		ld (_section_line_number),a
		ld bc,_rgb_15khz_scanline_grille_handler_frontporch
		ld hl,(_timer1_int_vector)
		ld (hl),bc
		exx
		ex af,af'
		ei
		reti.lil

scan_rgb_15khz_240p_60hz:
_rgb_15khz_scanline_handler_frontporch:
		HSYNC_PULSE_15KHZ 4, _rgb_15khz_scanline_handler_vsync
_rgb_15khz_scanline_handler_vsync:
		HSYNC_VSYNC_PULSE_15KHZ 1, _rgb_15khz_scanline_handler_backporch_firstline
_rgb_15khz_scanline_handler_backporch_firstline:
		HSYNC_PULSE_15KHZ_END_VSYNC 1, _rgb_15khz_scanline_handler_backporch
_rgb_15khz_scanline_handler_backporch:
		HSYNC_PULSE_15KHZ 14, _rgb_15khz_scanline_handler_pixeldata
_rgb_15khz_scanline_handler_pixeldata:
	; Includes 1 scanline of vertical front porch at end of image scanout

		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		DEJITTER_15KHZ_PRT

	; Setup first line of visible area
		; 71 cycles hsync
		; 10 cycles pre-assertion (properly belonging to front porch)
		in0 a,(PD_DR)	; 4 cycles
		res 7,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		; 71 cycles asserted
		push af		; 4 cyc
		UART0_RX_POLL_32_CYC
		pop af		; 4 cyc

		push ix			; 5 cycles
		push iy			; 5 cycles
		; loop counter in ix
		ld ix,240		; 5 cycles
		ld iy,(fb_scanline_offsets)	; 8 cycles
		REP_NOP 2
		REP_NOP 71

		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles

		; 35 cycles h.back porch (5 unused for start of loop)
		REP_NOP 4
		REP_NOP 35
		; update the framebuffer pointer (26 cycles)
		xor a			; 1 cycle
		ld hl,(fb_ptr)		; 7 cycles
		ld bc,(iy+0)		; 6 cycles
		add hl,bc		; 1 cycle
		lea iy,iy+3		; 3 cycles
		ld de,PC_DR		; 4 cycles
		ld bc,312		; 4 cycles
	; 480 lines
	@loop:
		; 5 cycles of h.back porch
		otirx			; 2 + 3 (+ 3*155 accounted for in next section)
		; 470 cycles: visible area (3*155=465 from otirx)
		out (PC_DR),a		; 3 cycles clear pixel data
		nop
		nop
		; 12 cycles front porch (10 eaten by HSYNC_ONLY_15KHZ setup)
		REP_NOP 12
		REP_NOP 2
		in0 a,(PD_DR)	; 4 cycles
		res 7,a		; 2 cycles
		out0 (PD_DR),a	; 4 cycles
		; 71 cycles hsync
		push af		; 4 cyc
		UART0_RX_POLL_32_CYC
		pop af		; 4 cyc

		REP_NOP 71
		; update the framebuffer pointer (25 cycles)
		ld hl,(fb_ptr)		; 7 cycles
		ld bc,(iy+0)		; 6 cycles
		add hl,bc		; 1 cycle
		lea iy,iy+3		; 3 cycles
		ld de,PC_DR		; 4 cycles
		ld bc,312		; 4 cycles

		or 0b10000000		; 2 cycles (hsync off)
		out0 (PD_DR),a 	; 4 cycles
		; 35 cycles back porch (5 cycles unused to donate to start of loop)
		REP_NOP 35
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
		ld bc,_rgb_15khz_scanline_handler_frontporch ; 4 cycles
		ld hl,(_timer1_int_vector)	; 7 cycles
		ld (hl),bc			; 5 cycles
		pop iy				; 5 cycles
		pop ix				; 5 cycles
		REP_NOP 428
		REP_NOP	470

		; 12 cycles front porch (10 eaten by HSYNC_ONLY_15KHZ setup)
		REP_NOP 12
		REP_NOP 2

		; 71 cycles hsync
		HSYNC_ONLY_15KHZ

		; ack timer interrupt since we have overrun
		ld bc,TMR1_CTL
		in a,(bc)	; ACK
		
		exx
		ex af,af'
		ei
		reti.lil
