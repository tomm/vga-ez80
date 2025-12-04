	.assume adl=1

macro INCREMENT_MOS_SYSVAR_TIME
		ld hl,(mos_sysvar_time)
		ld de,(hl)
		inc de
		inc de
		ld (hl),de
endmacro
