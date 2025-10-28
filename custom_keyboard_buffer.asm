		; This re-implements the MOS uart0 RX state machine, for versions
		; of MOS that don't provide an API to push bytes into the MOS state
		; machine, and don't provide a proper key event queue.
		; (Ie MOS other than Rainbow 2.5+)
		
		; This code is largely pilfered from Console8 MOS and Rainbow MOS 2.5

		.assume adl=1


VDPP_BUFFERLEN:		EQU		16	; VDP Protocol Buffer Length
_vdp_protocol_state:	DS	1		; UART state
_vdp_protocol_cmd:	DS	1		; Command
_vdp_protocol_len:	DS	1		; Size of packet data
_vdp_protocol_ptr:	DS	3		; Pointer into data
_vdp_protocol_data:	DS	VDPP_BUFFERLEN

inject_uart0_rx_byte:
			LD	A, C
			LD	HL, _vdp_protocol_data
			CALL	vdp_protocol
			RET

; Poll for next event in keyboard buffer.
;   DEU - Address of 4-byte buffer to write the event to
; Return:
;   A=0 IF no event
;   A=1 IF event
;   (DE+0) - Event ASCII value
;   (DE+1) - Event keymods (alt, ctrl, etc)
;   (DE+2) - Event FabGL vkey
;   (DE+3) - 0=Key Up, 1=Key Down
get_next_key_event:
			PUSH	BC
			PUSH	DE
			PUSH	HL
			CALL	kbuf_remove
			POP	HL
			POP	DE
			POP	BC
			LD	A,1
			RET	NZ
			XOR	A
			RET
;
; The UART protocol handler state machine
;
vdp_protocol:		LD	A, (_vdp_protocol_state)
			OR	A
			JR	Z, vdp_protocol_state0
			DEC	A
			JR	Z, vdp_protocol_state1
			DEC	A
			JR	Z, vdp_protocol_state2
			DEC	A
			JP	Z, vdp_protocol_state3
			XOR	A
			LD	(_vdp_protocol_state), A
			RET
;
; Wait for control byte (>=80h)
;
vdp_protocol_state0:	LD	A, C			; Wait for a header byte (bit 7 set)
			SUB	80h
			RET	C
			LD	(_vdp_protocol_cmd), A	; Store the cmd (discard the top bit)
			LD	(_vdp_protocol_ptr), HL	; Store the buffer pointer
			LD	A, 1			; Switch to next state
			LD	(_vdp_protocol_state), A
			RET

;
; Read the packet length in
;
vdp_protocol_state1:	LD	A, C			; Fetch the length byte
			CP	VDPP_BUFFERLEN + 1	; Check if it exceeds buffer length (16)
			JR	C, @f			;
			LD	A, 3			; If it does exceed buffer length, switch to state 3 (ignore packet)
			LD	(_vdp_protocol_state), A
			RET
;
@@:			LD	(_vdp_protocol_len), A	; Store the length
			OR	A			; If it is zero
			JR	Z, vdp_protocol_exec	; Then we can skip fetching bytes, otherwise
			LD	A, 2			; Switch to next state
			LD	(_vdp_protocol_state), A
			RET

; Read the packet body in
;
vdp_protocol_state2:	LD	HL, (_vdp_protocol_ptr)	; Get the buffer pointer
			LD	(HL), C			; Store the byte in it
			INC	HL			; Increment the buffer pointer
			LD	(_vdp_protocol_ptr), HL
			LD	A, (_vdp_protocol_len)	; Decrement the length
			DEC	A
			LD	(_vdp_protocol_len), A
			RET	NZ			; Stay in this state if there are still bytes to read
;
; When len is 0, we can action the packet
;

vdp_protocol_exec:	XOR	A			; Reset the state
			LD	(_vdp_protocol_state), A
			LD	A, (_vdp_protocol_cmd)	; Get the command byte...
			CP	vdp_protocol_vesize	; Check whether the command is in bounds
			RET	NC			; Out of bounds, so just ignore
			LD	DE, vdp_protocol_vector
			LD	HL, 0			; Index into the jump table
			LD	L, A			; ...in HLU
			ADD	HL, HL			; Multiply by four, as each entry is 4 bytes
			ADD	HL, HL			; And add the address of the vector table
			ADD	HL, DE
			JP	(HL)			; And jump to the entry in the jump table
;
; Jump table for UART commands
;
vdp_protocol_vector:	JP	vdp_protocol_GP
			JP	vdp_protocol_KEY
			JP	vdp_protocol_CURSOR
			JP	vpd_protocol_SCRCHAR
			JP	vdp_protocol_POINT
			JP	vdp_protocol_AUDIO
			JP	vdp_protocol_MODE
			JP	vdp_protocol_RTC
			JP	vdp_protocol_KEYSTATE
			JP	vdp_protocol_MOUSE
;
vdp_protocol_vesize:	EQU	[$-vdp_protocol_vector]/4

;
; Discard data (packet too long)
;
vdp_protocol_state3:	LD	A, (_vdp_protocol_len)
			DEC	A
			LD	(_vdp_protocol_len), A
			RET	NZ			; Stay in this state if there are still bytes to read
			XOR	A			; Reset the state
			LD	(_vdp_protocol_state), A
			RET

vdp_protocol_GP:	RET

; Keyboard Data
; Received after a keypress event in the VPD
;
vdp_protocol_KEY:	
			; Store event in keyboard event buffer
			LD	DE, _vdp_protocol_data
			call kbuf_append
			RET

vdp_protocol_CURSOR:	RET
vpd_protocol_SCRCHAR:	RET
vdp_protocol_POINT:	RET
vdp_protocol_AUDIO:	RET
vdp_protocol_MODE:	RET
vdp_protocol_RTC:	RET
vdp_protocol_KEYSTATE:	RET
vdp_protocol_MOUSE:	RET


_kbuf_wait_keydown:
		push ix
		ld ix,0
		add ix,sp
	@try:
		ld de,(ix+6)
		call kbuf_remove	
		jr z,@try

		; is it keydown?
		ld de,(ix+6)
		inc de
		inc de
		inc de
		ld a,(de)
		or a
		jr z,@try

		pop ix
		ret

_kbuf_poll_event:
		push ix
		ld ix,0
		add ix,sp
		ld de,(ix+6)
		call kbuf_remove	
		pop ix
		ld a,0
		jr nz,@success
		ret
	@success:
		inc a
		ret

kbuf_append:	; 4-byte value to append in (de). set `z` if no space
		; put (de)..(de+3) to kbbuf_data[kbbuf_end_idx*4]
		ld hl,0
		ld a,(kbbuf_end_idx)
		ld l,a
		add hl,hl
		add hl,hl
		ld bc,kbbuf_data
		add hl,bc

		ex de,hl
		ld bc,4
		ldir

		; c := (kbbuf_end_idx+1) & KBBUF_LEN
		ld a,(kbbuf_end_idx)
		inc a
		ld c,KBBUF_LEN
		and c
		ld c,a

		ld a,(kbbuf_start_idx)
		cp c

		; if kbbuf_start_idx==kbbuf_end_idx+1 then no space for appending
		ret z
		
		; otherwise write new kbbuf_end_idx
		ld a,c
		ld (kbbuf_end_idx),a
		ret

; Take 1 event from the keyboard buffer (store to (de) struct keyboard_event_t*)
kbuf_remove:	; remove 4-byte value into (de)..(de+3). `z` flag set if no bytes in fifo
		ld hl,0
		ld a,(kbbuf_start_idx)
		ld l,a
		ld a,(kbbuf_end_idx)
		cp l
		ret z

		add hl,hl
		add hl,hl
		ld bc,kbbuf_data
		add hl,bc
		ld bc,4
		ldir

		ld a,(kbbuf_start_idx)
		inc a
		and KBBUF_LEN
		ld (kbbuf_start_idx),a

		or a		; clear `z` flag
		ret

; Clear (flush) the keyboard buffer
_kbuf_clear:
		push hl
		ld hl,kbbuf_start_idx
		xor a
		; Clear buffer with interrupts disabled to make it atomic
		di
		ld (hl),a
		inc hl
		ld (hl),a
		ei
		pop hl
		ret

KBBUF_LEN: 	.equ 31		; must be POT-1, and <256
kbbuf_start_idx:	db 0
kbbuf_end_idx: 		db 0
kbbuf_data:		ds [KBBUF_LEN+1]*4
