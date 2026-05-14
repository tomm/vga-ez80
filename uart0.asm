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
