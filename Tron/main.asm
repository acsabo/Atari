	processor 6502
	include "vcs.h"
	include "macro.h"
        
    seg.u Variables
	org $80

ptrPF	word; pointer to playfield data
Counter	equ $81

	seg Code
    org $f000

; We're going to mess with the playfield registers, PF0, PF1 and PF2.
; Between them, they represent 20 bits of bitmap information
; which are replicated over 40 wide pixels for each scanline.
; By changing the registers before each scanline, we can draw bitmaps.


Start	CLEAN_START
      
	lda #$60		
    sta COLUBK       
        
    lda #$52
    sta COLUPF        
      
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
    
     
      
    lda #<Screen_PF
    sta ptrPF
    lda #>Screen_PF
    sta ptrPF+1       
  
    
    sta WSYNC    
	;dex
	ldx #190 
ScanLoop
   
	ldy #0
	lda (ptrPF),y
	sta PF0	        

    iny
	lda (ptrPF),y
    sta PF1

    iny
	lda (ptrPF),y
    sta PF2
    
	sta WSYNC	; wait for next scanline 
    dex   
                 
            
	;ldy #0
	iny
	lda (ptrPF),y
	;eor (ptrPF),y
	sta PF0	        

    iny
	lda (ptrPF),y
	;eor (ptrPF),y
    sta PF1

    iny
	lda (ptrPF),y
	;eor (ptrPF),y
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

;----------------------------------
    align $100; make sure data doesn't cross page boundary
Screen_PF
	.byte #%10100000	; Scanline 184
	.byte #%01010101	; Scanline 176
	.byte #%10101010	; Scanline 168

	.byte #%01011111	; Scanline 184
	.byte #%10101010	; Scanline 176
	.byte #%01010101	; Scanline 168

	org $fffc
	.word Start
	.word Start
