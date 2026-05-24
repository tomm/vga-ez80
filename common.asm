	.assume adl=1

; fudge_factors determined experimentally (with dejitter_experiment.asm)
macro DEJITTER_PRT fudge_factor
	; sample interrupt timing jitter from PRT register (16 cycles)
	; can read every 3 cycles, while PRT decrements every 4.
	ld bc,TMR1_DR_L	; 4
	in a,(bc)	; 3
	in h,(bc)	; 3
	in l,(bc)	; 3
	in b,(bc)	; 3

	cpl
	sub a,h
	sub a,l
	sub a,b

	; convert to number of cycles lagged (by the instruction executing when interrupt was due)
	add fudge_factor
	; now cycles lagged (+4) is in `a`
	; Why +4? we want 16 nops to ensure unexpected values don't
	; jump us somewhere weird (see `and 0xf`), but don't want to
	; execute any more nops than needed. expected lag cycles
	; should be 0-9, so +4 takes us further towards the end
	; of the nop region wasting fewer cycles. why not +5? +6?
	; yeah maybe could be

	; ensure it's in range 0-15 (to be robust against very delayed timer interrupts -
	; likely caused by 'di' in user code). monitor won't tolerate this, but 
	; at least don't crash the driver.
	and 0xf

	; Negate lag by computed jump into nops
	; (8 cycles for self-modify & jr)
	ld (@cmpjmp+1),a	; overwrite target of next instruction (jr)
@cmpjmp:
	jr $+2	; dummy jump target, as it will be overwritten
	nop
	nop
	nop
	nop

	nop  ; <- where interrupting a 1 cycle instruction will jump to
	nop  ; <- 2 cycle instruction
	nop  ; <-  ... etc ...
	nop

	nop
	nop
	nop
	nop

	nop
	nop
	nop
	nop
endmacro

macro DEJITTER_31KHZ_PRT
	DEJITTER_PRT 0x25
endmacro

macro DEJITTER_15KHZ_PRT
	DEJITTER_PRT 0x71
endmacro
