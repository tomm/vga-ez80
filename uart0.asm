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
