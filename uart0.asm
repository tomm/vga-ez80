; pointer arith requires the ordering of these fields
; XXX note: placed at fixed location at end of 8k SRAM
audio_bitpos:	.equ 0xb7fefe
audio_readpos:	.equ 0xb7feff
audio_buf:	.equ 0xb7ff00

; input: gpio-d value in `b`. pin-5 is audio, rest must be preserved
; output: updated gpio-d value in `a`
macro AUDIO_1BIT_SAMPLE_36_CYC
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
	; 14
	inc hl			; 1. hl := audio_buf
	ld l,a			; 1
	xor a			; 1
	srl (hl)		; 5. 1-bit sample to be played in carry flag
	rra			; 1
	rra			; 1
	rra			; 1. 1-bit sample now in bit 5
	res 5,b			; 2
	or b			; 1. apply to gpio-d value
endmacro

; Poll uart0 or update audio output
; Expects GPIO-D value in `b`, and returns it in `a` (with bit 5 as audio)
macro UART0_RX_POLL_OR_AUDIO_46_CYC
		IN0		A,(UART0_REG_LSR)	; 4 cyc. Get the line status register
		AND 		UART_LSR_RDY		; 2 cyc. Check for characters in buffer
		JR		Z,@empty		; 2 cyc (4 if taken)
		IN0		A,(UART0_REG_RBR)	; 4 cyc. Read the character from the UART receive buffer
		ld hl,(uart0_buf_pos)			; 7 cyc
		ld (hl),a				; 2 cyc
		inc hl					; 1 cyc
		ld (uart0_buf_pos),hl			; 7 cyc
		REP_NOP 13
		ld a,b		; 1 cyc. a is expected to contain preserved GPIO D value
		jr @end					; 3 cyc
	@empty:
		; 10 cyc from above
		AUDIO_1BIT_SAMPLE_36_CYC
	@end:
endmacro

