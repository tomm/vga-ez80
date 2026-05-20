.align 0x100
; pointer arith requires the ordering of these fields
_padding:	ds 254
audio_bitpos:	ds 1
audio_readpos:	ds 1
audio_buf:	ds 256

; delete me
audio_osc_inc:	.ds 3	; sets audio note frequency.
audio_osc_pos:	.ds 3

; input: gpio-d value in `b`. pin-5 is audio, rest must be preserved
; output: updated gpio-d value in `a`
macro AUDIO_1BIT_SAMPLE_34_CYC
	; 22/12 for not-taken/taken path
	ld hl,audio_bitpos	; 4
	dec (hl)		; 4
	jr nz,@same_packet	; 2/4 (not taken/taken)

	; Move to next sample packet (of 8 sample bits)
	ld (hl),8		; 3
	inc hl			; 1. hl := audio_readpos
	ld a,(hl)		; 2
	inc a			; 1
	ld (hl),a		; 2
	jr @play_sample		; 3

	; still processing packet of 8 sample bits
@same_packet:
	REP_NOP 7
	inc hl			; 1. hl := audio_readpos
	ld a,(hl)		; 2
@play_sample:
	; 12
	inc hl			; 1. hl := audio_buf
	ld l,a			; 1
	xor a			; 1
	sla (hl)		; 5. 1-bit sample to be played in carry flag
	rra			; 1
	rra			; 1
	rra			; 1. 1-bit sample now in bit 5
	xor b			; 1. apply to gpio-d value
endmacro

macro UART0_RX_POLL_32_CYC
	; 32 cyc
		IN0		A,(UART0_REG_LSR)	; 4 cyc. Get the line status register
		AND 		UART_LSR_RDY		; 2 cyc. Check for characters in buffer
		JR		Z,@empty		; 2 cyc (4 if taken)
		IN0		A,(UART0_REG_RBR)	; 4 cyc. Read the character from the UART receive buffer
		ld hl,(uart0_buf_pos)			; 7 cyc
		ld (hl),a				; 2 cyc
		inc hl					; 1 cyc
		ld (uart0_buf_pos),hl			; 7 cyc
		jr @end					; 3 cyc
	@empty:
		; 10 cyc from above
		
		REP_NOP 22
	@end:
endmacro

; Expects GPIO-D value in `b`, and returns it in `a` (with bit 5 as audio)
macro UART0_RX_POLL_OR_AUDIO_39_CYC
	; 39 cyc: poll uart0 or update audio output
		IN0		A,(UART0_REG_LSR)	; 4 cyc. Get the line status register
		AND 		UART_LSR_RDY		; 2 cyc. Check for characters in buffer
		JR		Z,@empty		; 2 cyc (4 if taken)
		IN0		A,(UART0_REG_RBR)	; 4 cyc. Read the character from the UART receive buffer
		ld hl,(uart0_buf_pos)			; 7 cyc
		ld (hl),a				; 2 cyc
		inc hl					; 1 cyc
		ld (uart0_buf_pos),hl			; 7 cyc
		REP_NOP 6
		ld a,b		; 1 cyc. a is expected to contain preserved GPIO D value
		jr @end					; 3 cyc
	@empty:
		; 10 cyc from above
		; 29 cpu cycles to run gpio oscillator
		ld hl,(audio_osc_pos)		; 7
		ld de,(audio_osc_inc)		; 8
		xor a			; 1
		adc hl,de		; 2
		ld (audio_osc_pos),hl		; 7
		rra			; 1. move carry bit into bit 7 of a
		rra			; 1
		rra			; 1. now carry is in bit 5
		xor b			; 1
	@end:
endmacro

; Expects GPIO-D value in `b`, and returns it in `a` (with bit 5 as audio)
macro UART0_RX_POLL_OR_AUDIO_44_CYC
	; 45 cyc: poll uart0 or update audio output
		IN0		A,(UART0_REG_LSR)	; 4 cyc. Get the line status register
		AND 		UART_LSR_RDY		; 2 cyc. Check for characters in buffer
		JR		Z,@empty		; 2 cyc (4 if taken)
		IN0		A,(UART0_REG_RBR)	; 4 cyc. Read the character from the UART receive buffer
		ld hl,(uart0_buf_pos)			; 7 cyc
		ld (hl),a				; 2 cyc
		inc hl					; 1 cyc
		ld (uart0_buf_pos),hl			; 7 cyc
		REP_NOP 11
		ld a,b		; 1 cyc. a is expected to contain preserved GPIO D value
		jr @end					; 3 cyc
	@empty:
		; 10 cyc from above
		AUDIO_1BIT_SAMPLE_34_CYC
	@end:
endmacro

