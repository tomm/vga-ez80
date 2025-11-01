		; A terminate-and-stay-resident GPIO video driver,
		; providing an `rst.l $20` API to access framebuffer
		; video functions.
		.assume adl=1
		.org 0x40000
		jp start
		.align $40

		.db "MOS"
		.db 0 ; version
		.db 1 ; ADL
		include "lib/print.asm"

MOS_BOT: .equ 0xbc000
TSR_LOC: .equ [MOS_BOT-0x1000]		; must be 0x100 aligned

start:
		push iy
		; Copy driver to TSR_LOC
		ld de,TSR_LOC
		ld hl,tsr_begin
		ld bc,tsr_end-tsr_begin
		ldir

		ld a,0x61 ; mos_api_setresetvector (rainbow mos 2.5+)
		ld e,0x20 ; set 0x20 reset vector
		ld hl,rst20_api_handler
		rst.lil 8

		cp 23
		jr z,@cannot_set_resetvector

		print_asciz "Loaded GPIO video driver at $"
		ld hl,TSR_LOC
		call print_hex24
		print_c '-'
		ld hl,[TSR_LOC+[tsr_end-tsr_begin]]
		call print_hex24
		print_crlf

	@exit:
		ld hl,0
		pop iy
		ret
	@cannot_set_resetvector:
		print_asciz "mos_api_setresetvector not supported (rainbow mos 2.5+ needed)"
		print_crlf
		jr @exit

tsr_begin:
	.relocate TSR_LOC
jumptable:
		.dl api_getversion		; a=0
		.dl api_setmode			; a=1
		.dl api_getvideosetup		; a=2
		.dl api_getmodeinfo		; a=3
		.dl api_videostop		; a=4
		.dl api_fillscanlineoffsetarray ; a=5

rst20_api_handler:
		push hl
		; a *= 3
		ld l,a
		add a,a
		add a,l
		ld hl,jumptable
		ld l,a		; only works due to .align 0x100 (and <85 api calls)
		ld hl,(hl)
		jp (hl)

; Input: a=0
; Return version in `a` (will be non-zero)
api_getversion:
		pop hl
		ld a,1
		ret.lil

; Input: a=1, mode number in `l`.
api_setmode:
		call video_init
		pop hl
		ld a,l		; video mode number
		call video_set_mode
		ret.lil

; Input: a=2
; Return pointer to `video_setup` structure in IY (read/write)
; struct {
;   uint8_t frame_counter; /* incremented after each frame */
;   uint24_t fb_ptr; /* pointer to framebuffer data */
;   uint24_t fb_scanline_offsets; /* pointer to array of 480 uint24_t, each
;                                    an offset applied to fb_ptr to find the
;                                    pixel data of a given scanline */
api_getvideosetup:
		pop hl
		ld iy,video_setup
		ret.lil

; Input: a=3
; Return pointer to info of current mode in IY (read only)
; struct {
;    uint24_t scanline_isr;
;    uint24_t clockcycles_per_scanline_div_4;
;    uint24_t width;
;    uint24_t height;
;    uint24_t scan_multiplier;
; }
api_getmodeinfo:
		pop hl
		ld iy,(_current_mode)
		ret.lil

; Input: a=4
; End GPIO video scanout
api_videostop:
		pop hl
		call video_stop
		ret.lil

; Input: a=5
;   ix: fb_scanline_offsets array address
;   de: line stride (ie virtual framebuffer width in pixels)
;   bc: number of pixel rows
;   iy: scan multiplier (ie 2=doublescan)
; Output:
;   array at [ix+0]..[ix+bc*iyl*3] will be filled with 24-bit scanline offsets
api_fillscanlineoffsetarray:
		pop hl
		call fill_scanline_offset_array
		ret.lil

USE_CUSTOM_KEYBOARD_BUFFER: .equ 0	; Do not need custom logic. Use rainbow MOS 2.5+
		include "gpiovideo.asm"
	.endrelocate
tsr_end:
