	processor 6502
	include "vcs.h"
	include "macro.h"

	org  $f000
	
; We're going to mess with the playfield registers, PF0, PF1 and PF2.
; Between them, they represent 20 bits of bitmap information
; which are replicated over 40 wide pixels for each scanline.
; By changing the registers before each scanline, we can draw bitmaps.

Counter	equ $81

Start	CLEAN_START

NextFrame
; This macro efficiently gives us 3 lines of VSYNC
	VERTICAL_SYNC
	
; 37 lines of VBLANK
	ldx #37
LVBlank	sta WSYNC
	dex
	bne LVBlank
; Disable VBLANK
        stx VBLANK
; Set foreground color
	;lda #$0
        ;sta COLUPF
; Draw the 192 scanlines
	ldx #192

	lda #0		; changes every scanline
        sta COLUBK       
        
        lda #255
        sta COLUPF        
ScanLoop
	lda #160	;11110000 / 240		
	sta PF0	        

        
	lda #85		;01111111 / 255
        sta PF1

        
	lda #170	;11111111 / 255
        sta PF2
        
        sta WSYNC	; wait for next scanline
        dex         

	bne ScanLoop

; Reenable VBLANK for bottom (and top of next frame)
	lda #2
        sta VBLANK
; 30 lines of overscan
	ldx #30
LVOver	sta WSYNC
	dex
	bne LVOver
	
; Go back and do another frame
	inc Counter
	jmp NextFrame
	
	org $fffc
	.word Start
	.word Start
