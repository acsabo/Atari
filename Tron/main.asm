;===============================================================================
; Program Information
;===============================================================================

    ; Program:      Collect
    ; Program by:   Darrell Spice, Jr
    ; Last Update:  June 25, 2014
    ;
    ; Super simple game of "collect the boxes" used for presentation on
    ; developing Atari 2600 homebrew games.
    ;
    ; See readme.txt for compile instructions
    
    
;===============================================================================
; Change Log
;===============================================================================
 
    ; 2013.06.24 - generate a stable display
    ; 2013.06.25 - add timers 


;===============================================================================
; Initialize dasm
;===============================================================================

    ; Dasm supports a number of processors, this line tells dasm the code
    ; is for the 6502 CPU.  The Atari has a 6507, which is 6502 that's been
    ; put into a "reduced package".  This package limits the 6507 to an 8K
    ; address space and also removes support for external interrupts.
        PROCESSOR 6502
    
        include "vcs.h"
        include "macro.h"
        include "xmacro.h"
 
SpriteHeight	equ 8 
MaxRows		equ 18
 
;===============================================================================
; Define RAM Usage
;===============================================================================

    ; define a segment for variables
    ; .U means uninitialized, does not end up in ROM
        SEG.U VARS
    
    ; RAM starts at $80
        ORG $80             

    ; coming soon!
Player1X	.byte
Player1Y	.byte

Player2X	.byte
Player2Y	.byte      

Player1YPrev	.byte
Player1XPrev	.byte

Player2YPrev	.byte
Player2XPrev	.byte
TempP1		.byte
TempP2		.byte

PF0_left	ds 19
PF1_left	ds 19
PF2_left	ds 19
PF0_right	ds 19
PF1_right	ds 19
PF2_right	ds 19

;Player1Score	.byte
;Player2core	.byte            
	       
;===============================================================================
; Define Start of Cartridge
;===============================================================================

    ; define a segment for code
    SEG CODE    
    
    ; 2K ROM starts at $F800, 4K ROM starts at $F000
    ORG $F800


;===============================================================================
; Initialize Atari
;===============================================================================
    
InitSystem:
    	; CLEAN_START is a macro found in macro.h
    	; it sets all RAM, TIA registers and CPU registers to 0
        CLEAN_START   
        
        ;player position
        lda #0
        sta Player1X
	lda #0
        sta Player1Y
        
        ;player position
        lda #0
        sta Player2X
        lda #75
        sta Player2Y        
        
        ;init grid mem
        ldy #144
        lda #$00
initGrid:
	sta PF0_left,y	
        dey
        bne initGrid        
        
        
;===============================================================================
; Main Program Loop
;===============================================================================

Main:
        jsr VerticalSync    ; Jump to SubRoutine VerticalSync
	jsr VerticalBlank   ; Jump to SubRoutine VerticalBlank
        jsr Kernel          ; Jump to SubRoutine Kernel
        jsr OverScan        ; Jump to SubRoutine OverScan
        jmp Main            ; JuMP to Main
    

;===============================================================================
; Vertical Sync
; -------------
; here we generate the signal that tells the TV to move the beam to the top of
; the screen so we can start the next frame of video.
; The Sync Signal must be on for 3 scanlines.
;===============================================================================

VerticalSync:
        lda #2      ; LoaD Accumulator with 2 so D1=1
        ldx #49     ; LoaD X with 49
        sta WSYNC   ; Wait for SYNC (halts CPU until end of scanline)
        sta VSYNC   ; Accumulator D1=1, turns on Vertical Sync signal
        stx TIM64T  ; set timer to go off in 41 scanlines (49 * 64) / 76
        sta WSYNC   ; Wait for Sync - halts CPU until end of 1st scanline of VSYNC
        sta WSYNC   ; wait until end of 2nd scanline of VSYNC
        lda #0      ; LoaD Accumulator with 0 so D1=0
	sta WSYNC   ; wait until end of 3rd scanline of VSYNC
        sta VSYNC   ; Accumulator D1=0, turns off Vertical Sync signal
        rts         ; ReTurn from Subroutine
    
;===============================================================================
; Vertical Blank
; --------------
; game logic runs here.  Coming soon!
;===============================================================================

VerticalBlank:

	;grid color
        lda #$70
        sta COLUBK
        
	lda #$48
        sta COLUPF
                       
	;player color
        lda #$52
        sta COLUP0        
        lda #$A2
        sta COLUP1    

        ;jsr CheckCollisionP1
        ;jsr CheckCollisionP2

        rts             ; ReTurn from Subroutine
    
;===============================================================================
; Kernel
; ------
; here we update the registers in TIA (the video chip) in order to generate
; what the player sees.  For now we're just going to output 192 colored
; scanlines lines so we have something to see.
;===============================================================================

Kernel:            
        sta WSYNC       ; Wait for SYNC (halts CPU until end of scanline)
        lda INTIM       ; check the timer
        bne Kernel      ; Branch if its Not Equal to 0
    	; turn on the display
        sta VBLANK      ; Accumulator D1=0, turns off Vertical Blank signal (image output on)

        ; Set up timer (in case of bugs where we don't hit exactly)
        TIMER_SETUP 192
        SLEEP 10 ; to make timing analysis work out
       
        ;from the beging over and over
        ;sta WSYNC
        
	;reset grid
        ldy #MaxRows
        ldx #SpriteHeight
        
        lda Player1Y	; NÃO ESTÁ AGINTINGO O RANGE [0-18] = 19 ROWS
        lsr
        lsr
        sta TempP1

        lda Player2Y
        lsr
        lsr
        sta TempP2
        inc TempP2
              
        ;dey
RowsHeightLoop
         
	sta WSYNC        
	
PatternChanged

        lda PF0_left,y 
        sta PF0 ; PF0 
        
        lda PF1_left,y 
        sta PF1 ; PF1    
        
        lda PF2_left,y 
        sta PF2 ; PF2
               
	SLEEP 6
        
        lda PF0_right,y
        sta PF0 ; PF0 
        
        lda PF1_right,y
        sta PF1 ; PF1
        
        lda PF2_right,y
        sta PF2 ; PF2 

        
        dex
        bne RowsHeightLoop  ; Branch if Not Equal to 0    

	;-------------- AQUI OCORRE O SALTO PORQUE NÃO HOUVE TEMPO SUFICIENTE PARA DESENHAR I FIM DA LINHA
	lda #2
        cpy TempP2
        beq doDrawP2
        lda #0			; no, load the padding offset (0)        
doDrawP2:       
	tax	; backup para dar tempo !!! aproveitando o x já que está zerado
        
        lda #2
        cpy TempP1
        beq doDrawP1
        lda #0			; no, load the padding offset (0)
doDrawP1:       
        sta ENAM0; enable/disable missile        
	;--------------
	stx ENAM1; enable/disable missile     
        ldx #SpriteHeight
        
   	dey ;next grid line, if there is more
        bmi RowsEnd            
        jmp PatternChanged
        
RowsEnd        
        ;sta WSYNC
        lda #0
        sta PF0
        sta PF1
        sta PF2 ; clear playfield
        sta COLUBK
        sta COLUPF
        sta ENAM0
        sta ENAM1

        
        lda #0			; no, load the padding offset (0)
        sta ENAM0; enable/disable missile
	sta ENAM1; enable/disable missile    
        

        ; Wait for timer to finish
        TIMER_WAIT
        rts	; ReTurn from Subroutine

UpdatePositionP1 subroutine
; We're going to divide the horizontal position by 15.
; The easy way on the 6502 is to subtract in a loop.
; Note that this also conveniently adds 5 CPU cycles
; (15 TIA clocks) per iteration.
	sta WSYNC	; 36th line
	sta HMCLR	; reset the old horizontal position
DivideLoop1:
	sbc #15		; subtract 15
	bcs DivideLoop1	; branch until negative
; A now contains (the remainder - 15).
; We'll convert that into a fine adjustment, which has
; the range -8 to +7.
	eor #7
	asl		; HMOVE only uses the top 4 bits, so shift by 4
	asl
	asl
	asl
; The fine offset goes into HMP0
	sta HMM0
; Now let's fix the coarse position of the player, which as you
; remember is solely based on timing. If you rearrange any of the
; previous instructions, position 0 won't be exactly on the left side.
	sta RESM0
; Finally we'll do a WSYNC followed by HMOVE to apply the fine offset.
	sta WSYNC	; 37th line
	sta HMOVE	; apply offset
	rts


UpdatePositionP2 subroutine
; We're going to divide the horizontal position by 15.
; The easy way on the 6502 is to subtract in a loop.
; Note that this also conveniently adds 5 CPU cycles
; (15 TIA clocks) per iteration.
	sta WSYNC	; 36th line
	sta HMCLR	; reset the old horizontal position
DivideLoop2:
	sbc #15		; subtract 15
	bcs DivideLoop2	; branch until negative
; A now contains (the remainder - 15).
; We'll convert that into a fine adjustment, which has
; the range -8 to +7.
	eor #7
	asl		; HMOVE only uses the top 4 bits, so shift by 4
	asl
	asl
	asl
; The fine offset goes into HMP0
	sta HMM1
; Now let's fix the coarse position of the player, which as you
; remember is solely based on timing. If you rearrange any of the
; previous instructions, position 0 won't be exactly on the left side.
	sta RESM1
; Finally we'll do a WSYNC followed by HMOVE to apply the fine offset.
	sta WSYNC	; 37th line
	sta HMOVE	; apply offset
	rts


CheckCollisionP1 subroutine
        ;check collisions
; Did the player collide with the wall?
	bit CXM0FB
        bpl NoCollisionP1
; Yes, load previous position
        lda Player1YPrev
        sta Player1Y
        lda Player1XPrev
        sta Player1X
        jmp NoMoveJoyP1
NoCollisionP1:
; No collision, update previous position and move player
        lda Player1Y
        sta Player1YPrev
        lda Player1X
        sta Player1XPrev
        ;jsr MoveJoystick !!! no need for this?!
NoMoveJoyP1 
	rts

CheckCollisionP2 subroutine
        ;check collisions
; Did the player collide with the wall?
	bit CXM1FB
        bpl NoCollisionP2
; Yes, load previous position
        lda Player2YPrev
        sta Player2Y
        lda Player2XPrev
        sta Player2X
        jmp NoMoveJoyP2
NoCollisionP2:
; No collision, update previous position and move player
        lda Player2Y
        sta Player2YPrev
        lda Player2X
        sta Player2XPrev
        ;jsr MoveJoystick !!! no need for this?!
NoMoveJoyP2    
	rts
        
UpdateGrid subroutine
	tya
        lsr
        lsr
        tay
                
	txa	
	lsr
	lsr ; div 4

        cmp #20
        bpl SecondHalf
firstHalf:
		
pf0_l:	
        cmp #4
        bpl pf1_l
        tax
        lda PF0_left,y
        ora BitReprF0,x		
        sta PF0_left,y		
        rts 
pf1_l:
        cmp #12
        bpl pf2_l
        sec
        sbc #4
        tax
        lda PF1_left,y
        ora BitReprF1,x		
        sta PF1_left,y	
        rts
pf2_l:
        sec
        sbc #12
        tax
        lda PF2_left,y	
        ora BitReprF2,x		
        sta PF2_left,y	
        rts
		
SecondHalf: 
	sbc #20
		
pf0_r:	
        cmp #4
        bpl pf1_r
        tax
        lda PF0_right,y
        ora BitReprF0,x		
        sta PF0_right,y		
        rts 
pf1_r:	
        cmp #12
        bpl pf2_r
        sec
        sbc #4
        tax
        lda PF1_right,y	
        ora BitReprF1,x		
        sta PF1_right,y	
        rts
pf2_r: 
        sec
        sbc #12
        tax
        lda PF2_right,y	
        ora BitReprF2,x		
        sta PF2_right,y	
	rts	
        
        ; Read joystick movement and apply to object 0
MoveJoystick
        ; Move vertically
        ; (up and down are actually reversed since ypos starts at bottom)
        ldx Player1Y
        lda #%00100000	;Up?
        bit SWCHA
        bne SkipMoveUp
        cpx #1
        bcc SkipMoveUp
        dex
SkipMoveUp
        lda #%00010000	;Down?
        bit SWCHA 
        bne SkipMoveDown
        cpx #72
        bcs SkipMoveDown
        inx
SkipMoveDown
	stx Player1Y
	;Move horizontally
        ldx Player1X
	lda #%01000000	;Left?
	bit SWCHA
	bne SkipMoveLeft
        cpx #1
        bcc SkipMoveLeft
        dex
SkipMoveLeft
	lda #%10000000	;Right?
	bit SWCHA 
	bne SkipMoveRight
	cpx #159
	bcs SkipMoveRight
	inx
SkipMoveRight
	stx Player1X
	rts
        
;===============================================================================
; Overscan
; --------------
; game logic runs here.  Since we don't have any yet, just delay so that the
; entire video frame consists of 262 scanlines
;===============================================================================

OverScan:
	sta WSYNC   ; Wait for SYNC (halts CPU until end of scanline)
        lda #2      ; LoaD Accumulator with 2 so D1=1
        sta VBLANK  ; STore Accumulator to VBLANK, D1=1 turns image output off

        ; set the timer for 27 scanlines.  Each scanline lasts 76 cycles,
        ; but the timer counts down once every 64 cycles, so use this
        ; formula to figure out the value to set.  
        ;       (scanlines * 76) / 64    
        ; Also note that it might be slight off due to when on the scanline TIM64T
        ; is updated.  So use Stella to check how many scanlines the code is
        ; generating and adjust accordingly.
        lda #32     ; set timer for 27 scanlines, 32 = ((27 * 76) / 64)
        sta TIM64T  ; set timer to go off in 27 scanlines

        ; game logic will go here
   

        ;draw player 1
	ldy Player1Y	
        ldx Player1X
        jsr UpdateGrid                
        ;inc Player1X

        ;draw player 2
        ldy Player2Y	
        ldx Player2X
        jsr UpdateGrid                
        inc Player2X

        jsr MoveJoystick
        
        ;update positions
       	lda Player1X	; load the counter as horizontal position
        adc #6
        jsr UpdatePositionP1
        
        
        lda Player2X
        adc #6
        jsr UpdatePositionP2
OSwait:
        sta WSYNC   ; Wait for SYNC (halts CPU until end of scanline)
        lda INTIM   ; Check the timer
        bne OSwait  ; Branch if its Not Equal to 0
        rts         ; ReTurn from Subroutine
        
;===============================================================================
; free space check before DigitGfx
;===============================================================================
        
 if (* & $FF)
    echo "------", [(>.+1)*256 - .]d, "bytes free before DigitGfx"
    align 256
  endif    
    
  
;===============================================================================
; Digit Graphics
;===============================================================================
        align 256

BitReprF0
	.byte #%00010000,#%00100000,#%01000000,#%10000000

BitReprF1
	.byte #%10000000,#%01000000,#%00100000,#%00010000,#%00001000,#%00000100,#%00000010,#%00000001
		
BitReprF2
	.byte #%00000001,#%00000010,#%00000100,#%00001000,#%00010000,#%00100000,#%01000000,#%10000000

	          
;===============================================================================
; free space check before End of Cartridge
;===============================================================================
        
 if (* & $FF)
    echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"
    align 256
  endif        
        
;===============================================================================
; Define End of Cartridge
;===============================================================================
        ORG $FFFA        ; set address to 6507 Interrupt Vectors 
        .WORD InitSystem ; NMI
        .WORD InitSystem ; RESET
        .WORD InitSystem ; IRQ