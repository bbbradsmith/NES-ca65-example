;
; example.s
; Brad Smith (rainwarrior), 4/06/2014
; Modified for FDS 5/02/2017
; http://rainwarrior.ca
;
; This is intended as an introductory example to NES programming with ca65.
; It covers the basic use of background, sprites, and the controller.
; This does not demonstrate how to use sound.
;
; This is not intended as a ready-made game. It is only a very minimal
; playground to assist getting started in NES programming. The idea here is
; to get you past the most difficult parts of a minimal NES program setup
; so that you can experiment from an almost blank slate.
;
; To use your own graphics, replace the two 4k tile banks provided.
; They are named "background.chr" and "sprite.chr".
;
; The reset and nmi routines are provided as a simple working example of
; these things. Writing these from scratch is a more advanced topic, so they
; will not be fully explained here.
;
; Under "drawing utilities" are some very primitive routines for working
; with the NES graphics. See the "main" section for examples of how to use them.
;
; Finally at the bottom you will find the "main" section that provides
; a small example program. A cursor is shown. Pressing the d-pad will move
;   - pressing the d-pad will move the cursor around the screen
;   - pressing B will draw a tile to the screen
;   - pressing A will draw several tiles to the screen
;   - pressing SELECT will reset the background
;   - holding START will demonstrate scrolling
;
; Please note that this example code is intended to be simple, not necessarily
; efficient. I have tried to avoid optimization in favour of easier to understand code.
;
; You may notice some odd behaviour when using the A button around the edges of the screen.
; I will leave it as an exercise for the curious to understand what is going on.
;

;
; FDS headers and stuff
;

BYPASS   = 1    ; bypass the license screen
WIPE_RAM = 1    ; wipes unused portion of FDS RAM at startup

; setting the file count 1 higher than files on disk for the license "bypass" technique
FILE_COUNT = 5 + BYPASS

.segment "HEADER"
.byte 'F','D','S',$1A
.byte 1 ; side count

.segment "SIDE1A"
; block 1
.byte $01
.byte "*NINTENDO-HVC*"
.byte $00 ; manufacturer
.byte "EXA"
.byte $20 ; normal disk
.byte $00 ; game version
.byte $00 ; side
.byte $00 ; disk
.byte $00 ; disk type
.byte $00 ; unknown
.byte FILE_COUNT ; boot file count
.byte $FF,$FF,$FF,$FF,$FF
.byte $92 ; 2017
.byte $04 ; april
.byte $17 ; 17
.byte $49 ; country
.byte $61, $00, $00, $02, $00, $00, $00, $00, $00 ; unknown
.byte $92 ; 2017
.byte $04 ; april
.byte $17 ; 17
.byte $00, $80 ; unknown
.byte $00, $00 ; disk writer serial number
.byte $07 ; unknown
.byte $00 ; disk write count
.byte $00 ; actual disk side
.byte $00 ; unknown
.byte $00 ; price
; block 2
.byte $02
.byte FILE_COUNT

.segment "FILE0_HDR"
; block 3
.import __FILE0_DAT_RUN__
.import __FILE0_DAT_SIZE__
.byte $03
.byte 0,0
.byte "FILE0..."
.word __FILE0_DAT_RUN__
.word __FILE0_DAT_SIZE__
.byte 0 ; PRG
; block 4
.byte $04
;.segment "FILE0_DAT"
;.incbin "" ; this is code below

.segment "FILE1_HDR"
; block 3
.import __FILE1_DAT_RUN__
.import __FILE1_DAT_SIZE__
.byte $03
.byte 1,1
.byte "FILE1..."
.word __FILE1_DAT_RUN__
.word __FILE1_DAT_SIZE__
.byte 0 ; PRG
; block 4
.byte $04
;.segment "FILE1_DAT"
;.incbin "" ; this is code below

.segment "FILE2_HDR"
; block 3
.import __FILE2_DAT_SIZE__
.import __FILE2_DAT_RUN__
.byte $03
.byte 2,2
.byte "FILE2..."
.word __FILE2_DAT_RUN__
.word __FILE2_DAT_SIZE__
.byte 1 ; CHR
; block 4
.byte $04
.segment "FILE2_DAT"
.incbin "background.chr"

.segment "FILE3_HDR"
; block 3
.import __FILE3_DAT_SIZE__
.import __FILE3_DAT_RUN__
.byte $03
.byte 3,3
.byte "FILE3..."
.word __FILE3_DAT_RUN__
.word __FILE3_DAT_SIZE__
.byte 1 ; CHR
; block 4
.byte $04
.segment "FILE3_DAT"
.incbin "sprite.chr"

.if (BYPASS <> 0)
; This block is the last to load, and enables NMI by "loading" the NMI enable value
; directly into the PPU control register at $2000.
; While the disk loader continues searching for one more boot file,
; eventually an NMI fires, allowing us to take control of the CPU before the
; license screen is displayed.
.segment "FILE4_HDR"
; block 3
.import __FILE4_DAT_SIZE__
.import __FILE4_DAT_RUN__
.byte $03
.byte 4,4
.byte "FILE4..."
.word $2000
.word __FILE4_DAT_SIZE__
.byte 0 ; PRG (CPU:$2000)
; block 4
.byte $04
.segment "FILE4_DAT"
.byte $90 ; enable NMI byte sent to $2000

.else
; Alternative to the license screen bypass, just put the required copyright message at PPU:$2800.
.segment "FILE4_HDR"
; block 3
.import __FILE4_DAT_SIZE__
.import __FILE4_DAT_RUN__
.byte $03
.byte 4,4
.byte "KYODAKU-"
.word $2800
.word __FILE4_DAT_SIZE__
.byte 2 ; nametable (PPU:$2800)
; block 4
.byte $04
.segment "FILE4_DAT"
.incbin "check.bin"
.endif

;
; FDS vectors
;

.segment "FILE1_DAT"
.word nmi
.word nmi

.if (BYPASS <> 0)
	.word bypass
.else
	.word nmi
.endif

.word reset
.word irq

;
; reset routine
;

.segment "FILE0_DAT"

.if (BYPASS <> 0)
; this routine is entered by interrupting the last boot file load
; by forcing an NMI not expected by the BIOS, allowing the license
; screen to be skipped entirely.
;
; The last file writes $90 to $2000, enabling NMI during the file load.
; The "extra" file in the FILE_COUNT causes the disk to keep seeking
; past the last file, giving enough delay for an NMI to fire and interrupt
; the process.
bypass:
	; disable NMI
	lda #0
	sta $2000
	; replace NMI 3 "bypass" vector at $DFFA
	lda #<nmi
	sta $DFFA
	lda #>nmi
	sta $DFFB
	; tell the FDS reset routine that the BIOS initialized correctly
	lda #$35
	sta $0102
	lda #$AC
	sta $0103
	; reset the FDS to begin our program properly
	jmp ($FFFC)
.endif

reset:
	; set FDS to use vertical mirroring
	lda $FA
	and #%11110111
	sta $4025
	;
	sei       ; mask interrupts
	lda #0
	sta $2000 ; disable NMI
	sta $2001 ; disable rendering
	sta $4015 ; disable APU sound
	sta $4010 ; disable DMC IRQ
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; initialize stack
	; wait for first vblank
	bit $2002
	:
		bit $2002
		bpl :-
	; clear not-quite all RAM to 0
	lda #0
	tax
	:
		;sta $0000, X ; the FDS uses part of the stack and ZP,
		;sta $0100, X ; so only partially clearing them.
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	;ldx #$00
	:
		sta $00, X
		inx
		cpx #$F9 ; $F9-FF used by FDS BIOS
		bcc :-
	ldx #$04 ; $0100-$0103 used by FDS BIOS
	:
		sta $100, X
		inx
		bne :-
	; place all sprites offscreen at Y=255
	lda #255
	;ldx #0
	:
		sta oam, X
		inx
		bne :-
	; wipe unused portion of FDS RAM (between FILE0 and FILE1)
	.if (WIPE_RAM <> 0)
		WIPE_ADDR = __FILE0_DAT_RUN__ + __FILE0_DAT_SIZE__
		WIPE_SIZE = __FILE1_DAT_RUN__ - WIPE_ADDR
		lda #<WIPE_ADDR
		sta $00
		lda #>WIPE_ADDR
		sta $01
		lda #0
		tay
		ldx #>WIPE_SIZE
		beq :++
		: ; 256 byte blocks
			sta ($00), Y
			iny
			bne :-
			inc $01
			dex
			bne :-
		: ; leftover
		ldy #<WIPE_SIZE
		beq :++
		:
			dey
			sta ($00), Y
			bne :-
		:
		sta $00
		sta $01
	.endif
	; wait for second vblank
	:
		bit $2002
		bpl :-
	; NES is initialized, ready to begin!
	; enable the NMI for graphical updates, and jump to our main program
	lda #%10001000
	sta $2000
	jmp main

;
; nmi routine
;

.segment "ZEROPAGE"
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_count:      .res 1 ; is incremented every NMI
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
scroll_x:       .res 1 ; x scroll position
scroll_y:       .res 1 ; y scroll position
scroll_nmt:     .res 1 ; nametable select (0-3 = $2000,$2400,$2800,$2C00)
temp:           .res 1 ; temporary variable

.segment "BSS"
nmt_update: .res 256 ; nametable update entry buffer for PPU update
palette:    .res 32  ; palette buffer for PPU update

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "FILE0_DAT"
nmi:
	; save registers
	pha
	txa
	pha
	tya
	pha
	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp @nmi_end
	:
	lda #1
	sta nmi_lock
	; increment frame counter
	inc nmi_count
	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp @ppu_update_end
	:
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta $2001
		ldx #0
		stx nmi_ready
		jmp @ppu_update_end
	:
	; sprite OAM DMA
	ldx #0
	stx $2003
	lda #>oam
	sta $4014
	; palettes
	lda #%10001000
	sta $2000 ; set horizontal nametable increment
	lda $2002
	lda #$3F
	sta $2006
	stx $2006 ; set PPU address to $3F00
	ldx #0
	:
		lda palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; nametable update
	ldx #0
	cpx nmt_update_len
	bcs @scroll
	@nmt_update_loop:
		lda nmt_update, X
		sta $2006
		inx
		lda nmt_update, X
		sta $2006
		inx
		lda nmt_update, X
		sta $2007
		inx
		cpx nmt_update_len
		bcc @nmt_update_loop
	lda #0
	sta nmt_update_len
@scroll:
	lda scroll_nmt
	and #%00000011 ; keep only lowest 2 bits to prevent error
	ora #%10001000
	sta $2000
	lda scroll_x
	sta $2005
	lda scroll_y
	sta $2005
	; enable rendering
	lda #%00011110
	sta $2001
	; flag PPU update complete
	ldx #0
	stx nmi_ready
@ppu_update_end:
	; if this engine had music/sound, this would be a good place to play it
	; unlock re-entry flag
	lda #0
	sta nmi_lock
@nmi_end:
	; restore registers and return
	pla
	tay
	pla
	tax
	pla
	rti

;
; irq
;

.segment "FILE0_DAT"
irq:
	rti

;
; drawing utilities
;

.segment "FILE0_DAT"

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
	lda #1
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_skip: waits until next NMI, does not update PPU
ppu_skip:
	lda nmi_count
	:
		cmp nmi_count
		beq :-
	rts

; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via $2007)
ppu_off:
	lda #2
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a $2007 write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
ppu_address_tile:
	lda $2002 ; reset latch
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta $2006
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	txa
	ora temp
	sta $2006 ; low bits of Y + X
	rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
ppu_update_tile:
	pha ; temporarily store A on stack
	txa
	pha ; temporarily store X on stack
	ldx nmt_update_len
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta nmt_update, X
	inx
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	pla ; recover X value (but put in A)
	ora temp
	sta nmt_update, X
	inx
	pla ; recover A value (tile)
	sta nmt_update, X
	inx
	stx nmt_update_len
	rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
ppu_update_byte:
	pha ; temporarily store A on stack
	tya
	pha ; temporarily store Y on stack
	ldy nmt_update_len
	txa
	sta nmt_update, Y
	iny
	pla ; recover Y value (but put in Y)
	sta nmt_update, Y
	iny
	pla ; recover A value (byte)
	sta nmt_update, Y
	iny
	sty nmt_update_len
	rts

;
; gamepad
;

PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

.segment "ZEROPAGE"
gamepad: .res 1

.segment "FILE0_DAT"
; gamepad_poll: this reads the gamepad state into the variable labelled "gamepad"
;   This only reads the first gamepad, and also if DPCM samples are played they can
;   conflict with gamepad reading, which may give incorrect results.
gamepad_poll:
	; strobe the gamepad to latch current button state
	lda #1
	sta $4016
	lda #0
	sta $4016
	; read 8 bytes from the interface at $4016
	ldx #8
	:
		pha
		lda $4016
		; combine low two bits and store in carry bit
		and #%00000011
		cmp #%00000001
		pla
		; rotate carry into gamepad variable
		ror
		dex
		bne :-
	sta gamepad
	rts

;
; main
;

.segment "FILE0_DAT"
example_palette:
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$09,$19,$29 ; bg1 green
.byte $0F,$01,$11,$21 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$18,$28,$38 ; sp0 yellow
.byte $0F,$14,$24,$34 ; sp1 purple
.byte $0F,$1B,$2B,$3B ; sp2 teal
.byte $0F,$12,$22,$32 ; sp3 marine

.segment "ZEROPAGE"
cursor_x: .res 1
cursor_y: .res 1
temp_x:   .res 1
temp_y:   .res 1

.segment "FILE0_DAT"
main:
	; setup 
	ldx #0
	:
		lda example_palette, X
		sta palette, X
		inx
		cpx #32
		bcc :-
	jsr setup_background
	; center the cursor
	lda #128
	sta cursor_x
	lda #120
	sta cursor_y
	; show the screen
	jsr draw_cursor
	jsr ppu_update
	; main loop
@loop:
	; read gamepad
	jsr gamepad_poll
	; respond to gamepad state
	lda gamepad
	and #PAD_START
	beq :+
		jsr push_start
		jmp @draw ; start trumps everything, don't check other buttons
	:
	jsr release_start ; releasing start restores scroll
	lda gamepad
	and #PAD_U
	beq :+
		jsr push_u
	:
	lda gamepad
	and #PAD_D
	beq :+
		jsr push_d
	:
	lda gamepad
	and #PAD_L
	beq :+
		jsr push_l
	:
	lda gamepad
	and #PAD_R
	beq :+
		jsr push_r
	:
	lda gamepad
	and #PAD_SELECT
	beq :+
		jsr push_select
	:
	lda gamepad
	and #PAD_B
	beq :+
		jsr push_b
	:
	lda gamepad
	and #PAD_A
	beq :+
		jsr push_a
	:
@draw:
	; draw everything and finish the frame
	jsr draw_cursor
	jsr ppu_update
	; keep doing this forever!
	jmp @loop

push_u:
	dec cursor_y
	; Y wraps at 240
	lda cursor_y
	cmp #240
	bcc :+
		lda #239
		sta cursor_y
	:
	rts

push_d:
	inc cursor_y
	; Y wraps at 240
	lda cursor_y
	cmp #240
	bcc :+
		lda #0
		sta cursor_y
	:
	rts

push_l:
	dec cursor_x
	rts

push_r:
	inc cursor_x
	rts

push_select:
	; turn off rendering so we can manually update entire nametable
	jsr ppu_off
	jsr setup_background
	; wait for user to release select before continuing
	:
		jsr gamepad_poll
		lda gamepad
		and #PAD_SELECT
		bne :-
	rts

push_start:
	inc scroll_x
	inc scroll_y
	; Y wraps at 240
	lda scroll_y
	cmp #240
	bcc :+
		lda #0
		sta scroll_y
	:
	; when X rolls over, toggle the high bit of nametable select
	lda scroll_x
	bne :+
		lda scroll_nmt
		eor #$01
		sta scroll_nmt
	:
	rts

release_start:
	lda #0
	sta scroll_x
	sta scroll_y
	sta scroll_nmt
	rts

push_b:
	jsr snap_cursor
	lda cursor_x
	lsr
	lsr
	lsr
	tax ; X = cursor_x / 8
	lda cursor_y
	lsr
	lsr
	lsr
	tay ; Y = cursor_y / 8
	lda #4
	jsr ppu_update_tile ; puts tile 4 at X/Y
	rts

push_a:
	jsr snap_cursor
	lda cursor_x
	lsr
	lsr
	lsr
	sta temp_x ; cursor_x / 8
	lda cursor_y
	lsr
	lsr
	lsr
	sta temp_y ; cursor_y / 8
	; draw a ring of 8 tiles around the cursor
	dec temp_x ; x-1
	dec temp_y ; y-1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	inc temp_x ; x
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	inc temp_x ; x+1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	dec temp_x
	dec temp_x ; x-1
	inc temp_y ; y
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	inc temp_x
	inc temp_x ; x+1
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	dec temp_x
	dec temp_x ; x-1
	inc temp_y ; y+1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	inc temp_x ; x
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	inc temp_x ; x+1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	rts

; snap_cursor: snap cursor to nearest tile
snap_cursor:
	lda cursor_x
	clc
	adc #4
	and #$F8
	sta cursor_x
	lda cursor_y
	clc
	adc #4
	and #$F8
	sta cursor_y
	; Y wraps at 240
	cmp #240
	bcc :+
		lda #0
		sta cursor_y
	:
	rts

draw_cursor:
	; four sprites centred around the currently selected tile
	; y position (note, needs to be one line higher than sprite's appearance)
	lda cursor_y
	sec
	sbc #5 ; Y-5
	sta oam+(0*4)+0
	sta oam+(1*4)+0
	lda cursor_y
	clc
	adc #3 ; Y+3
	sta oam+(2*4)+0
	sta oam+(3*4)+0
	; tile
	lda #1
	sta oam+(0*4)+1
	sta oam+(1*4)+1
	sta oam+(2*4)+1
	sta oam+(3*4)+1
	; attributes
	lda #%00000000 ; no flip
	sta oam+(0*4)+2
	lda #%01000000 ; horizontal flip
	sta oam+(1*4)+2
	lda #%10000000 ; vertical flip
	sta oam+(2*4)+2
	lda #%11000000 ; both flip
	sta oam+(3*4)+2
	; x position
	lda cursor_x
	sec
	sbc #4 ; X-4
	sta oam+(0*4)+3
	sta oam+(2*4)+3
	lda cursor_x
	clc
	adc #4 ; X+4
	sta oam+(1*4)+3
	sta oam+(3*4)+3
	rts

setup_background:
	; first nametable, start by clearing to empty
	lda $2002 ; reset latch
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	; empty nametable
	lda #0
	ldy #30 ; 30 rows
	:
		ldx #32 ; 32 columns
		:
			sta $2007
			dex
			bne :-
		dey
		bne :--
	; set all attributes to 0
	ldx #64 ; 64 bytes
	:
		sta $2007
		dex
		bne :-
	; fill in an area in the middle with 1/2 checkerboard
	lda #1
	ldy #8 ; start at row 8
	:
		pha ; temporarily store A, it will be clobbered by ppu_address_tile routine
		ldx #8 ; start at column 8
		jsr ppu_address_tile
		pla ; recover A
		; write a line of checkerboard
		ldx #8
		:
			sta $2007
			eor #$3
			inx
			cpx #(32-8)
			bcc :-
		eor #$3
		iny
		cpy #(30-8)
		bcc :--
	; second nametable, fill with simple pattern
	lda #$24
	sta $2006
	lda #$00
	sta $2006
	lda #$00
	ldy #30
	:
		ldx #32
		:
			sta $2007
			clc
			adc #1
			and #3
			dex
			bne :-
		clc
		adc #1
		and #3
		dey
		bne :--
	; 4 stripes of attribute
	lda #0
	ldy #4
	:
		ldx #16
		:
			sta $2007
			dex
			bne :-
		clc
		adc #%01010101
		dey
		bne :--
	rts

;
; end of file
;
