; Set to 1 unless running on Rainbow MOS 2.5+ (which provides a way to
; inject uart0 rx bytes into MOS, and provides a proper keyboard buffer)
;USE_CUSTOM_KEYBOARD_BUFFER: .equ 1

		.if USE_CUSTOM_KEYBOARD_BUFFER
			.include "custom_keyboard_buffer.asm"
		.endif

UART0_REG_RBR:		equ	0xc0	; receive buffer
UART0_REG_IER:		equ	0xc1	; interrupt enable register
UART0_REG_LSR:		equ	0xc5	; line status
UART_LSR_RDY:		equ	0x01	; data ready

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

modestruct_len: .equ 15
_modes:
		; struct Mode {
		; 	void (*initial_scanline_handler_fn)();
		;	uint24_t scanline_duration;  /* In CPU clocks divided by 4 */
		;	uint24_t width;
		;	uint24_t height;
		;	uint24_t scan_multiplier;    /* eg: 2 for doublescan */
		; }
		; Fast modes scanning every second line (~33% CPU free)
		.dl scan_vga_31khz_480p_60hz_grille, 147, 156, 120, 2
		.dl scan_vga_31khz_480p_60hz_grille, 147, 156, 240, 1

		.dl scan_rgb_15khz_480p_30hz_grille, 294, 312, 120, 2
		.dl scan_rgb_15khz_480p_30hz_grille, 294, 312, 240, 1
		; Slow modes with all scanlines drawn ~6% free CPU
		.dl scan_vga_31khz_480p_60hz, 147, 156, 120, 4
		.dl scan_vga_31khz_480p_60hz, 147, 156, 160, 3
		.dl scan_vga_31khz_480p_60hz, 147, 156, 240, 2
		.dl scan_vga_31khz_480p_60hz, 147, 156, 480, 1

		.dl scan_rgb_15khz_240p_60hz, 294, 312, 120, 2
		.dl scan_rgb_15khz_240p_60hz, 294, 312, 240, 1

; Public video_setup struct
video_setup:
	; incremented at end of image data scanout
	frame_counter:	.db 0
	fb_ptr:		.dl	0xb1000 ; [width*height] only 4k left for moslets...
	fb_scanline_offsets: .dl     0xba240 ; [480*3] enough space for 156*240 mode

_current_mode:	.dl 0		; ptr into _modes

_timer1_int_vector:
		.ds 3
_section_line_number:
		.db 0

uart0_buf_pos:	.ds 3		; ptr into uart0_rx_buf
uart0_rx_buf:	.ds 32

saved_uart0_reg_ier:	.ds 1

video_stop:
		; Turn off GPIO video scanout
		xor a
		out0 (TMR1_CTL),a
		; Re-enable uart0 interrupt(s)
		ld a,(saved_uart0_reg_ier)
		out0 (UART0_REG_IER),a
		ret

video_init:
		; turn off uart0 interrupt. we have to poll for uart0 rx
		in0 a,(UART0_REG_IER)
		ld (saved_uart0_reg_ier),a
		xor a
		out0 (UART0_REG_IER),a
		
		; turn off vblank interupt
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

		; clear uart0 buf
		ld hl,uart0_rx_buf
		ld (uart0_buf_pos),hl

		; grab any pending uart0 crap
		;ld hl,1000000
		;ld de,1
	;@loop:
		;IN0		A,(UART0_REG_RBR)	; 4 cyc. Read the character from the UART receive buffer
		;or a
		;sbc hl,de
		;jr nz,@loop

		ret

video_set_mode:	; mode in `a`
		push ix
		push iy
		ld l,a
		ld h,modestruct_len
		mlt hl
		ld de,_modes
		add hl,de
		push hl
		pop iy

		; save current mode pointer
		ld (_current_mode),hl

		; set timer1 interrupt handler to start of frame (vsync)
		ld bc,(hl)	; _modes[0] value: scanline handler
		ld hl,(_timer1_int_vector)
		ld (hl),bc
		xor a
		ld (_section_line_number),a

		; set up timer1
		ld hl,(iy+3)	; load _modes[1] value (scanline length in CPU clocks / 4)
		out0 (TMR1_DR_L),l
		out0 (TMR1_DR_H),h
		ld a, [PRT_ENABLE | PRT_CLK_DIV_4 | PRT_START | PRT_INT_ENABLE | PRT_MODE_CONTINUOUS]
		out0 (TMR1_CTL),a

		; set scanline framebuffer offset data (this implements scanline doubling/quadrupling if mode needs)
		ld ix,(fb_scanline_offsets)
		ld de,(iy+6) ; mode.width
		ld bc,(iy+9) ; mode.height
		ld iy,(iy+12) ; scan multiplier
		call fill_scanline_offset_array
	
		pop iy
		pop ix
		ret

; Params:
;   ix: fb_scanline_offsets array address
;   de: line stride (ie virtual framebuffer width in pixels)
;   bc: number of pixel rows
;   iy: scan multiplier (ie 2=doublescan)
;
; Array in ix will need to be [bc*iyl*3] bytes long.
;   eg for 156x240 mode with doublescan (480 scanned out lines)
;      there wil lbe de=156, bc=240, iyl=2
; Output:
;   array at [ix+0]..[ix+bc*iyl*3] will be filled with 24-bit scanline offsets
fill_scanline_offset_array:
		ld hl,0
	@loop:	; framebuffer scanline offset only increments every 4 frames, to implement quadscan
		ld a,iyl
	@@:
		ld (ix+0),hl
		lea ix,ix+3
		dec a
		jr nz,@b
		add hl,de
		
		dec bc
		ld a,b
		or c
		jr nz,@loop
		ret

	.include "uart0.asm"
	.include "vga_640_480_60hz.asm"
	.include "rgb_15khz.asm"
