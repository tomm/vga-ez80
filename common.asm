	.assume adl=1

macro INCREMENT_MOS_SYSVAR_TIME
		ld hl,(mos_sysvar_time)
		ld de,(hl)
		inc de
		inc de
		ld (hl),de
endmacro

; fudge_factors determined experimentally (with dejitter_experiment.asm)
macro DEJITTER_PRT fudge_factor
	; sample interrupt timing jitter from PRT register (16 cycles)
	; can read every 3 cycles, while PRT decrements every 4.
	; TMR1_DR_L into bc. XXX we assume bc container TMR1_CTL
	inc bc		; 1 cycle
	in a,(bc)	; 3
	in h,(bc)	; 3
	in l,(bc)	; 3
	in b,(bc)	; 3

	; sum it (3 cycles)
	add a,h
	add a,l
	add a,b

	; convert to number of cycles lagged (by the instruction executing when interrupt was due)
	ld b,a
	ld a,fudge_factor
	sub b
	; now cycles lagged (+4) is in `a`
	; Why +4? we want 16 nops to ensure unexpected values don't
	; jump us somewhere weird (see `and 0xf`), but don't want to
	; execute any more nops than needed. expected lag cycles
	; should be 0-9, so +4 takes us further towards the end
	; of the nop region wasting fewer cycles. why not +5? +6?
	; yeah maybe could be

	; Negate lag by computed jump into nops
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

macro DEJITTER_31KHZ_PRT
	DEJITTER_PRT 0x1e
endmacro

macro DEJITTER_15KHZ_PRT
	DEJITTER_PRT 0x6a
endmacro
