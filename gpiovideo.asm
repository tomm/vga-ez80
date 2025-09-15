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
		.dl vga_scanline_handler_vsync, 147, 156, 120, 4
		.dl vga_scanline_handler_vsync, 147, 156, 160, 3
		.dl vga_scanline_handler_vsync, 147, 156, 240, 2
		.dl vga_scanline_handler_vsync, 147, 156, 480, 1
		.dl scanline_handler_vsync_15khz, 294, 310, 120, 4
		.dl scanline_handler_vsync_15khz, 294, 310, 160, 3
		.dl scanline_handler_vsync_15khz, 294, 310, 240, 2
		.dl scanline_handler_vsync_15khz, 294, 310, 480, 1

_current_mode:	.dl 0

; incremented at end of image data scanout
frame_counter:
		.db 0
; pointer to scanline offsets. there will be [screen_height * 3] bytes of these
fb_scanline_offsets:
		.dl     0xa0000
fb_ptr:
		.dl	0xa1000 ; could be lower. 240*3 is only 0x2d0

_timer1_int_vector:
		.ds 3
_section_line_number:
		.db 0

video_init:
		; turn off vblank interupt
		; XXX this is actually unnecessary - but will make sense for framebuffer MOS
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
		ld hl,0
		ld de,(iy+6) ; mode.width
		ld bc,(iy+9) ; mode.height
	@loop:	; framebuffer scanline offset only increments every 4 frames, to implement quadscan
		ld a,(iy+12)	; mode.scan_multiplier
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
	
		pop iy
		pop ix
		ret

	.include "15khz.asm"
	.include "31khz.asm"
