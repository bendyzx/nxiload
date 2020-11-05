					.module nxi
					.globl _main
					.area _CODE

					.lclequ  LAYER_2_PAGE, 					#9

					.lclequ  M_GETSETDRV,   				#0x89
					.lclequ  F_OPEN,        				#0x9a
					.lclequ  F_CLOSE,      					#0x9b
					.lclequ  F_READ,        				#0x9d
					.lclequ  FA_READ, 						#0x01
					.lclequ	 F_FSTAT,						#0xa1

					.lclequ	PERIPHERAL2,					#0x06
					.lclequ	TURBOMODE,						#0x07

					.lclequ  SPRITE_CONTROL_REGISTER,		#0x15			; Enables/disables Sprites and Lores Layer, and chooses priority of sprites and Layer 2.
					.lclequ  PALETTE_INDEX_REGISTER,		#0x40			; Chooses a ULANext palette number to configure.
					.lclequ  PALETTE_VALUE_REGISTER,		#0x41			; Used to upload 8-bit colors to the ULANext palette.
					.lclequ  PALETTE_CONTROL_REGISTER,		#0x43			; Enables or disables ULANext interpretation of attribute values and toggles active palette.
					.lclequ  PALETTE_VALUE_BIT9_REGISTER,	#0x44			; Holds the additional blue color bit for RGB333 color selection.
					.lclequ  MMU_REGISTER_4,				#0x54			; Set a Spectrum RAM page at position 0x8000 to 0x9FFF
					.lclequ  MMU_REGISTER_5,				#0x55			; Set a Spectrum RAM page at position 0xa000 to 0xBFFF
					.lclequ  MMU_REGISTER_6,				#0x56			; Set a Spectrum RAM page at position 0xC000 to 0xDFFF
					.lclequ  MMU_REGISTER_7,				#0x57			; Set a Spectrum RAM page at position 0xE000 to 0xFFFF

					.lclequ  GRAPHIC_PRIORITIES_SLU, 		#0b00000000		; sprites over l2 over ula
					.lclequ  GRAPHIC_PRIORITIES_SUL, 		#0b00001000
					.lclequ  GRAPHIC_SPRITES_VISIBLE,		#0b00000001

					.macro NEXTREG_A register								; Set Next hardware register using A
						.dw #0x92ED
						.db register
					.endm
					
					.macro NEXTREG_nn register, value						; Set Next hardware register using an immediate value
						.dw #0x91ED
						.db register
						.db value
					.endm

					.macro L_NEXTREG register
					    .db #0x3e
						.db register
						call _readnextreg
					.endm

					.macro SetSpriteControlRegister  
						NEXTREG_A SPRITE_CONTROL_REGISTER
					.endm

					.macro BREAK
						push bc
						.db #0xDD, #0x01
						nop
						nop
						pop bc
					.endm

					.macro PRESERVE
						;ld (_stackptr),sp										; value (stackptr) = add sp
						;ld sp,#_mystack										; sp = addr (_mystack)
						push af
						push bc
						push de
						push hl
						push ix
						push iy
					.endm

					.macro RESTORE
						pop iy
						pop ix
						pop hl
						pop de
						pop bc
						pop af
						;ld sp,(_stackptr)										; sp = value (_stackptr)
					.endm

					.macro OUTPUTLETTER, value
						push af
						push hl												; PUSH hl
						.db #0x3E
						.db value
						ld (_letters), a
						ld hl, #_letters
						call _conprint
						pop hl
						pop af												; POP hl
					.endm

_main::				PRESERVE
					
					ld hl, (_cmdline)
					ld a, h
					or l
					ld hl, #(_nofile)
					jp z, .exitmsg											; no filename argument provided

					L_NEXTREG PERIPHERAL2
					ld a,l
					ld (_nx6), a
					ld a, l
					and #0x7d
					NEXTREG_A PERIPHERAL2									; turbo off
					L_NEXTREG TURBOMODE
					ld a,l
					ld (_nx7), a
					ld a, #3
					NEXTREG_A TURBOMODE										; 28mhz
					
					call _setdrv
					ld hl, (_cmdline)										; hl = cmdline
					ld ix, #(_filename)										; ix = filename
					ld b, #0
					
.parse_filename:    ld a,(hl)
					or a					
					jp z, .got_filename										; finish if 0
					cp #0xd
					jp z, .got_filename										; finish if CR
					cp #0x3a
					jp z, .got_filename										; finish if :					
					cp #0x7f
					jp nc, .got_filename									; finish if >=127
					ld (ix), a
					inc ix
					inc hl
					inc b
					jp .parse_filename										; loop round to next character

.got_filename:		ld a, b
					inc ix
					ld (ix), #0												; null terminate filename
																			; Check File Extension
					ld a, #0x2e												; .
					sbc a, -5(ix)
					jp nz, .invalid
					ld a, #0x4e												; N
					sbc a, -4(ix)
					jp z, .nmatch					
					ld a, #0x6e												; n
					sbc a, -4(ix)
					jp z, .nmatch					
					jp .invalid					
.nmatch:			ld a, #0x58												; X
					sbc a, -3(ix)
					jp nz, .xmatch
					ld a, #0x78												; x
					sbc a, -3(ix)
					jp nz, .xmatch					
					jp .invalid
.xmatch:			ld a, #0x49												; I
					sbc a, -2(ix)
					jp z, .extok
					ld a, #0x69												; i
					sbc a, -2(ix)
					jp z, .extok
					jp .invalid

.extok:				ld hl, #(_filename)										; hl = _filename
					ld b,#1													; b = 1
					ld de,#0												; de = 0
    				call _fopen												; filename in hl, filehandle in _handle
					ld a,#2													; a = 2
					jp c,_fileError     									; if Fc JUMP to _fileError

					call _fstat
					jp c,_fileError											; if Fc JUMP to _fileError
					ld ix, #(_stat)
					ld h, 8(ix)												; hl = filesize
					ld l, 7(ix)

					ld de,#0xc200											; de = 49664
					sbc hl,de												; hl - de
					jp z,.validfile											; if Fz JUMP to .validfile
					ld h, 8(ix)
					ld l, 7(ix)												; hl = filesize
					ld de,#0xc000											; de = 49152
					or #0xff												; reset flags and preserve a
					sbc hl,de												; hl - de
					jp z,.valid256											; if Fz JUMP to .valid256
					jp .invalid

.valid256:			ld a, #1
					ld (_pal256), a
.validfile:			NEXTREG_nn PALETTE_CONTROL_REGISTER #0b10000			; NEXTREG $43(67) = 16															Layer 2 first palette
					NEXTREG_nn PALETTE_INDEX_REGISTER #0					; NEXTREG $40(64) = 0															Set colour index 0
					ld hl,#(_buffer)

.readpal:			ld bc, #0x200                                           ; bc = 512
					ld ix,#(_buffer)										; ix = addr(palette)
					call _fread
					jp c,_fileError											; if Fc JUMP to _fileError
                    ld b, #255                                              ; b = 255
                    ld hl,#(_buffer)

					ld a,(_pal256)
					or a
					jp z, .loadpalette										; if _pal256 = 0 JUMP .loop

.resetpalette:		ld a,#0xff												; a = buffer byte
					sub b
					NEXTREG_A PALETTE_VALUE_BIT9_REGISTER					; NEXTREG $44(68) = a - load palette value
                    ld a,#0													; a = buffer byte
					NEXTREG_A PALETTE_VALUE_BIT9_REGISTER					; NEXTREG $44(68) = a - load palette value
					djnz .resetpalette	                                    ; b-- JUMP to loop if b>0
					jp .startreadpixels

.loadpalette:		ld a,(hl)												; a = buffer byte
					NEXTREG_A PALETTE_VALUE_BIT9_REGISTER					; NEXTREG $44(68) = a - load palette value
					inc hl  												; #(_buffer)++
                    ld a,(hl)												; a = buffer byte
					NEXTREG_A PALETTE_VALUE_BIT9_REGISTER					; NEXTREG $44(68) = a - load palette value
					inc hl  												; #(_buffer)++
					djnz .loadpalette	                                    ; b-- JUMP to loop if b>0

.startreadpixels:	ld a,#192												;
					ld b,a
					ld a,#255
					ld (_currentline),a					
					
.readpixels:		ld hl,#(_buffer)
					push bc                                                 ; PUSH bc
                    ld bc, #0x0100                                          ; bc = 256
					ld ix,#(_buffer)										; ix = addr(palette)					
					call _fread
					jp c,_fileError											; if Fc JUMP to _fileError
                    
                    ld a,(_currentline)										; a = _currentline
					inc a													; a--
					and #0xe0												; a &= 224
					rlca													; 8 bit rotate left, bit0 = bit7, Fc = bit7  (a *= 2)
					rlca													; 8 bit rotate left, bit0 = bit7, Fc = bit7  (a *= 2)
					rlca													; 8 bit rotate left, bit0 = bit7, Fc = bit7  (a *= 2)
					add a,#LAYER_2_PAGE*#2									; a += 18
					NEXTREG_A MMU_REGISTER_6								; NEXTREG $56(86) = a

					ld a,(_currentline)										; a = _currentline
					inc a													; a--
					ld (_currentline), a
					and #0x1f												; a &= 31
					or #0xc0												; a |= 184

					ld d,a													; d = a
					ld e,#0													; e = 0

.copypixels:		ld bc, #0x0100                                          ; bc = 256
                    ld hl, #(_buffer)
					ldir													; copy hl[0:bc] - de[0:bc]

                    pop bc                                                  ; POP bc
                    djnz .readpixels                                        ; b-- JUMP to readpixels if b>0

.show:				ld a,(#0x5b5c)											; a = 23388
					and #7													; a &= 7
					add a,a													; a *= 2
					NEXTREG_A MMU_REGISTER_6								; NEXTREG $56(86) = a

					ld bc,#4667												; bc = 18023
					ld a,#2													; a = 2
					out (c),a												; OUT (c) = a

					ld a, #GRAPHIC_PRIORITIES_SUL + #GRAPHIC_SPRITES_VISIBLE; a = 9  -  OK
					SetSpriteControlRegister								; NEXTREG $43(67)  (set image priorities)

					; set transparency on ULA
					NEXTREG_nn PALETTE_CONTROL_REGISTER #0					; NEXTREG $43(67) = 0  -  OK
					NEXTREG_nn PALETTE_CONTROL_REGISTER #0					; NEXTREG $43(67) = 0  -  OK
					NEXTREG_nn PALETTE_INDEX_REGISTER #0x18					; NEXTREG $40(64) = 24  -  OK
					NEXTREG_nn PALETTE_VALUE_REGISTER #0xe3					; NEXTREG $41(65) = 227  -  OK
					ld hl,#0x4000											; hl = 16384
					ld de,#0x4001											; de = 16385
					ld bc,#0x17ff											; bc = 6143
					ld (hl),l												; value (hl) = 1
					ldir													; 6143 * value (de) = value(hl)

					ld hl,#0x5800											; hl = 22528
					ld de,#0x5801											; de = 22529
					ld bc,#0x2ff											; bc = 767
					ld (hl),#0x47											; value (hl) = 71
					ldir													; 767 * value (de) = value(hl)

					xor a													; a = 0
					out (#254),a											; OUT(254) = a (0)
					jp .exit

.invalid:			ld hl,#(_invalid)										; Set error message
.exitmsg:			call _print_rst16										; Print message
					ld a,#1
					ld (_error), a

.exit:				ld a,(_nx6)
					NEXTREG_A PERIPHERAL2									; restore turbo
					ld a, (_nx7)
					NEXTREG_A TURBOMODE										; restore speed
					RESTORE
					ld a,(_error)
					or a
					jp nz,.return
;--------------------
.pause:				ld b, #60
.p3:				push bc
					ld b, #0
.p2:				push bc
					call .keypressed
					ld b, #0					
.p1:				nop
					djnz .p1
					pop bc
					djnz .p2
					pop bc
					djnz .p3
					jp .return
;--------------------
.keypressed:		ld bc,#0xfefe
					in a,(c)
					and #0b00011111
					cp #31
					jp nz,.pressed
					ld bc,#0xfdfe
					in a,(c)
					and #0b00011111
					cp #31
					jp nz,.pressed
					ld bc,#0xfbfe
					in a,(c)
					and #0b00011111
					cp #31
					jp nz,.pressed
					ld bc,#0xf7fe
					in a,(c)
					and #0b00011111
					cp #31
					jp nz,.pressed
					ld bc,#0xeffe
					in a,(c)
					and #0b00011111
					cp #31
					jp nz,.pressed
					ld bc,#0xdffe
					in a,(c)
					and #0b00011111
					cp #31
					jp nz,.pressed
					ld bc,#0xbffe
					in a,(c)
					and #0b00011111
					cp #31
					jp nz,.pressed
					ld bc,#0x7ffe
					in a,(c)
					and #0b00011111
					cp #31
					jp nz,.pressed
					ret
;--------------------
.pressed:			pop bc
					pop bc
;--------------------
.return:			ret														; RETURN
;--------------------
_fileError:			out (#254),a											; OUT(254) = a (0)
					call _fclose

					ld a,(#0x5b5c)											; a = 23388
					and #7													; a &= 7
					add a,a													; a *= 2
	
					NEXTREG_A MMU_REGISTER_6								; NEXTREG $56(86) = a

					ld	bc,#4667											; bc = 18023
					xor a													; a = 0
					out (c),a												; OUT(c) = a

					ld a, #GRAPHIC_PRIORITIES_SLU + #GRAPHIC_SPRITES_VISIBLE; a = 9  -  OK
					SetSpriteControlRegister								; NEXTREG $43(67)  (set image priorities)
	
					; set transparency on ULA
					NEXTREG_nn PALETTE_CONTROL_REGISTER #0					; NEXTREG $43(67) = 0  -  OK
					NEXTREG_nn PALETTE_CONTROL_REGISTER #0					; NEXTREG $43(67) = 0  -  OK
					NEXTREG_nn PALETTE_INDEX_REGISTER #0x18					; NEXTREG $40(64) = 24  -  OK
					NEXTREG_nn PALETTE_VALUE_REGISTER #0xe3					; NEXTREG $41(65) = 227  -  OK

					ld a,(_nx6)
					NEXTREG_A PERIPHERAL2
					ld a,(_nx7)
					NEXTREG_A TURBOMODE

					RESTORE
					ret														; RETURN
;--------------------
_setdrv:			xor a													; a = 0
					rst #0x08												; call RST08
					.db M_GETSETDRV											; M_GETSETDRV

					ld (_drive),a											; value (_drive) = a
					ret														; RETURN
;--------------------
_fopen:				ld iy, (_osiy)
					ld b,#FA_READ											; b = 1
					.db #62													; ld a, N
;--------------------
_drive:				.db #0													; 0
					;push ix													; PUSH ix
					;pop hl													; POP hl
					rst #0x08												; RST08   a=drive, hl=filaname, b=access modes
					.db F_OPEN												; 154

					ld (_handle),a											; value (_handle) = a
					ret														; RETURN
;--------------------
_fread:				ld iy, (_osiy)
					push ix													; PUSH ix
					pop hl													; POP hl
					.db #62													; ld a, N

;--------------------
_handle:			.db #1													; 1
					rst #0x08												; RST08
					.db F_READ												; 157
					ret														; RETURN
;--------------------
_fstat:				ld hl, #(_stat)
					rst #0x08												; RST08
					.db F_FSTAT												; 161
					ret
;--------------------
_fclose:    		ld iy, (_osiy)
					ld a,#_handle											; a = addr(_handle)
					rst #0x08												; RST08
					.db F_CLOSE												; 155
					ret														; RETURN
;--------------------
_print_rst16:		ld a,(hl)
					inc hl
					or a
					ret z
					rst #16
					jr _print_rst16
;--------------------
_stackptr:			.dw	#0
;--------------------
_filename:			.db #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.db #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.db #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.db #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
;--------------------
_mystack:			.dw #0
					.dw #0
					.dw #0
					.dw #0
					.dw #0
					.dw #0
					.dw #0
					.dw #0
					.dw #0
;--------------------
_letters:			.db #83,#0
_currentline:		.db #0
_buffer:			.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
					.dw #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
_nx6:				.db #0
_nx7:				.db #0
_stat:				.db #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0
_nofile:			.ascii ".nxiload <filename> to load image to background"
					.db #10, #0
_invalid:			.ascii "Not a valid .nxi image"
					.db #10, #0
_pal256:			.db #0
_error:				.db #0
