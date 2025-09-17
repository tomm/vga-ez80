		; interrupt-based GPIO video scanout, V2.0
		.assume adl=1
		.org $40000
		jp start
		.align $40

		.db "MOS"
		.db 0 ; version
		.db 1 ; ADL

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

TEST_COUNT: .equ 1000

	macro REP_NOP num
		.fillbyte 0
		.ds num
	endmacro

	include "lib/print.asm"

start:
		push iy

		call hardware_init
		call set_timer

		xor a
	@loop:
		inc a
		mlt bc
		ld hl,(test_position)
		ld de,TEST_COUNT
		or a
		sbc hl,de
		jr c,@loop

		; turn off timer
		xor a
		out0 (TMR1_CTL),a

		; print results
		ld hl,test_buffer
		ld de,TEST_COUNT
	@print_loop:
		ld a,(hl)
		call print_hexbyte
		ld a,' '
		rst.lil 0x10
		inc hl
		dec de	
		ld a,d
		or e
		jr nz,@print_loop
		
		pop iy
		ld hl,0
		ret

hardware_init:
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

_timer1_int_vector: ds 3

macro DEJITTER
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
	ld a,0x25	; 0x17 determined by experiment. will be invalidated if any code on interrupt entry are changed
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

timer_int_handler:
		ex af,af'
		exx
		ld bc,TMR1_CTL
		in a,(bc) ; ACK
		DEJITTER

		; save test result
		ld hl,test_buffer
		ld de,(test_position)
		add hl,de
		ld (hl),a
		inc de	
		ld (test_position),de

		exx
		ex af,af'
		ei
		reti.lil

set_timer:
		ld hl,(_timer1_int_vector)
		ld bc,timer_int_handler
		ld (hl),bc
		; set up timer1
		ld hl,147 	; 147x4 clock cycle period = 588 cycles
		out0 (TMR1_DR_L),l
		out0 (TMR1_DR_H),h
		ld a, [PRT_ENABLE | PRT_CLK_DIV_4 | PRT_START | PRT_INT_ENABLE | PRT_MODE_CONTINUOUS]
		out0 (TMR1_CTL),a

		ret

test_position: dl 0
	; note we lazily overrun this so don't put stuff after it
test_buffer: ds TEST_COUNT

