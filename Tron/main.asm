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
NDigitRows	equ 5	; number of lines of bricks
NDR		equ NDigitRows	; abbreviation for number of brick rows
 
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

Scores		.byte
Controls	.byte

;PowerUP		.byte

PF0_left	ds 19
PF1_left	ds 19
PF2_left	ds 19
PF0_right	ds 19
PF1_right	ds 19
PF2_right	ds 19
         
	       
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
        lda #153
        sta Player1X
        lda #152
        sta Player1XPrev
        
	lda #36
        sta Player1Y
        sta Player1YPrev
        
        ;player position
        lda #5
        sta Player2X
        lda #4
        sta Player2XPrev
        
        lda #36
        sta Player2Y
        sta Player2YPrev
        
        ;init Score
        lda #$00
        sta Scores
        lda #%01001000; P1 goes left P2 goes right
        sta Controls
        ;sta ScoreP2
        
        ;init grid mem
        ldy #144
        lda #$00
initGrid:
	sta PF0_left,y	
        dey
        bne initGrid    
        
        ;test to be removed
        ;ldy #79
        ;lda #6
        ;sta PF0_left,y	
        
        ;ldy #22
        ;lda #$86
        ;sta PF0_left,y	
        
;===============================================================================
; Main Program Loop
;===============================================================================

Main:
;===============================================================================
; Vertical Sync
; -------------
; here we generate the signal that tells the TV to move the beam to the top of
; the screen so we can start the next frame of video.
; The Sync Signal must be on for 3 scanlines.
;===============================================================================

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

;===============================================================================
; Vertical Blank
; --------------
; game logic runs here.  Coming soon!
;===============================================================================        
        ;player color
        lda #$52
        sta COLUP0        
        lda #$A2
        sta COLUP1    
        
        ;line delimiter before grid
        ldx #5
        lda GradientColorBK,x		        
        sta COLUBK		


;===============================================================================
; Kernel
; ------
; here we update the registers in TIA (the video chip) in order to generate
; what the player sees.  For now we're just going to output 192 colored
; scanlines lines so we have something to see.
;===============================================================================        
Kernel        
        sta WSYNC       ; Wait for SYNC (halts CPU until end of scanline)
        lda INTIM       ; check the timer
        bne Kernel      ; Branch if its Not Equal to 0
    	; turn on the display
        sta VBLANK      ; Accumulator D1=0, turns off Vertical Blank signal (image output on)

        ; Set up timer (in case of bugs where we don't hit exactly)
        TIMER_SETUP 192
        SLEEP 2;10 ; to make timing analysis work out
        
	;grid color
        lda #$70
        sta COLUBK
        
	lda #$48
        sta COLUPF        
        
        lda Player1Y
        lsr        
        lsr
        sta TempP1
        

        lda Player2Y
        lsr
        lsr
        sta TempP2        
        
        ldy #MaxRows	; start        
        ;===========
        SLEEP 18	; TRICK TO WAIT FOR THE RIGHT TIME
PatternChanged      

	;-------------- AQUI OCORRE O SALTO PORQUE NÃO HOUVE TEMPO SUFICIENTE PARA DESENHAR I FIM DA LINHA
        lda #$F0
        cpy TempP2
        beq doDrawP2
        lda #0			; no, load the padding offset (0)        
doDrawP2:       
	tax	; backup para dar tempo !!! aproveitando o x já que está zerado
        
        lda #$F0
        cpy TempP1
        beq doDrawP1
        lda #0			; no, load the padding offset (0)
doDrawP1:       
        
	;--------------
	;enable/disable player
        stx GRP1
        ldx #SpriteHeight
        sta GRP0 
	jmp SkipLine        ; TO AVOID BLANK LINE

RowsHeightLoop
	sta WSYNC        
SkipLine
        lda PF0_left,y 
        sta PF0 
        
        lda PF1_left,y 
        sta PF1     
        
        lda PF2_left,y 
        sta PF2 
               
	SLEEP 6
        
        lda PF0_right,y
        sta PF0
        
        lda PF1_right,y
        sta PF1
        
        lda PF2_right,y
        sta PF2
        
        dex
        bne RowsHeightLoop  	; Branch if Not Equal to 0    

   	dey  
        bpl PatternChanged	; NEXT LINE OF THE GRID
        
RowsEnd        
        sta WSYNC	; NEED TO WAIT FOR THE CURRENT LINE TO COMPLETE
        lda #0
        sta GRP0
        sta GRP1            
        sta PF0
        sta PF1
        sta PF2 	; clear playfield        
        sta WSYNC
        
        ; line delimimter after grid
        ldx #5
        lda GradientColorBK,x		        
        sta COLUBK	

        lda #0
        sta PF0
        sta PF1
        sta PF2 	; clear playfield
        
    
        sta WSYNC	; add extra line to keep simetry with the top	
        sta COLUBK
        sta COLUPF
        
;===============================================================================
; Scoreboard
;===============================================================================
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC

        ldx #5
        lda GradientColorBK,x		        
        sta COLUBK		

        lda #%00000010; score mode 
        sta CTRLPF

        ldx #4 ; digit height
nxtDigitLine:        
		lda #0
        sta PF1
        
        jsr UpdateScoreLine
        ldy #3
nxtScanLine:
        sta WSYNC
        lda GradientColorBK,x		        
        sta COLUBK

        lda TempP1
        sta PF1
        
	SLEEP #20

        lda TempP2
        sta PF1 
        
        SLEEP #20
        
        dey        
        bne nxtScanLine
        
        dex
        bpl nxtDigitLine
        
	lda #0
        sta PF1
        
        ;end of digits panel
        lda #%00000000; clear score mode 
        sta CTRLPF; -> CTRLPF
        
        lda #0
        sta PF0
        sta PF1
        
        sta GRP0
        sta GRP1        
        sta WSYNC	; add extra line to keep simetry with the top	
        sta WSYNC
        sta COLUBK

        ; Wait for timer to finish
        TIMER_WAIT

;===============================================================================
; Overscan
; --------------
; game logic runs here.  Since we don't have any yet, just delay so that the
; entire video frame consists of 262 scanlines
;===============================================================================
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

        ;update positions
       	lda Player1X
        jsr UpdatePositionP1            
        jsr CheckCollisionP1
        jsr MoveJoystick1

	ldy Player1YPrev;Player1Y
        ldx Player1XPrev;Player1X
        jsr UpdateGrid       


        sta WSYNC
        sta HMCLR	; reset the old horizontal position
        sta WSYNC
	;-----------------
        
        ;jsr CheckCollisionP2	
	;ldy Player2YPrev
        ;ldx Player2XPrev
        ;jsr UpdateGrid                
        ;jsr MoveJoystick2
        ;lda Player2X        
        ;jsr UpdatePositionP2
        
;===============================================================================
; CHECKING SWITCHES
;===============================================================================

ProcessSwitches:
        lda SWCHB       ; load in the state of the switches
        lsr             ; D0 is now in C
        bcs OSwait    	; if D0 was on, the RESET switch was not held        
	jmp InitSystem
        
        
;===============================================================================
; Restaring game loop
;===============================================================================

        
OSwait:
        sta WSYNC   ; Wait for SYNC (halts CPU until end of scanline)
        lda INTIM   ; Check the timer
        bne OSwait  ; Branch if its Not Equal to 0

	
        jmp Main            ; JuMP to Main

; Fetches bitmap data for two digits of a
; BCD-encoded number, storing it in TempP1 and TempP2
; FontBuf+x to FontBuf+4+x.
UpdateScoreLine subroutine
	;---------- PLAYER 1 SCORE
        lda Scores
        and #$0F	; mask out the least significant digit
	
        sta TempP1
        asl
        asl        
        adc TempP1	; multiply by 5
        
        stx TempP1	; jump to current line of the digit
        adc TempP1
        
        tay
        lda DigitsBitmap,y
        ;and $0F;temporary
        sta TempP1

	;---------- PLAYER 2 SCORE
        lda Scores
        and #$0F	; mask out the least significant digit
	
        sta TempP2
        asl
        asl        
        adc TempP2	; multiply by 5
        
        stx TempP2	; jump to current line of the digit
        adc TempP2
        
        tay
        lda DigitsBitmap,y
        ;and $0F;temporary        
        sta TempP2
        
	rts

;===============================================================================
; UpdatePositionP1 subroutine
; --------------
;
;===============================================================================
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
        sta HMP0
; Now let's fix the coarse position of the player, which as you
; remember is solely based on timing. If you rearrange any of the
; previous instructions, position 0 won't be exactly on the left side.
        sta RESP0
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
        sta HMP1
; Now let's fix the coarse position of the player, which as you
; remember is solely based on timing. If you rearrange any of the
; previous instructions, position 0 won't be exactly on the left side.
        sta RESP1
; Finally we'll do a WSYNC followed by HMOVE to apply the fine offset.
	sta WSYNC	; 37th line
	sta HMOVE	; apply offset
	rts

;===============================================================================
; CheckCollision
; --------------
;
;===============================================================================

CheckCollisionP1 subroutine
	
        ;check collisions
; Did the player collide with the wall?
        ;lda #%10000000
        bit CXP0FB
        bpl PossibleCollisionP1        
        sta CXCLR	
        
        ;if collision in Y
        lda Player1Y
        sec
        sbc Player1YPrev
        cmp #4
        bcs CollisionP1         
        
        ;or if collision in X
        lda Player1X
        sec
        sbc Player1XPrev
        cmp #6
        bpl CollisionP1  
        ;or X < PrevX
        lda Player1XPrev
        sec
        sbc Player1X
        cmp #4
        bpl CollisionP1 
        ;inc ScoreP1
                
        rts
        
CollisionP1:	

        ;Update scores
        inc Scores
        
PossibleCollisionP1:
	
	;if moving to the right
        lda Player1X
        sec
        sbc Player1XPrev
        cmp #7
        bcc NoCollisionAtAll  
        
	;if moving to the left	
        lda Player1XPrev
        sec
        sbc Player1X
        cmp #4
        bpl CollisionLeftP1
        
CollisionRightP1:
	lda Player1X
        lsr
        lsr
        asl
        asl
	sta Player1XPrev ; rounded X collision    
	jmp NoCollisionAtAll
        
CollisionLeftP1:

	lda Player1X
        lsr
        lsr
        asl
        asl
        adc #4;needed when moving to the left        
	sta Player1XPrev ; rounded X collision   
        
NoCollisionAtAll:      

	lda Player1Y
        lsr
        lsr
        asl
        asl        
        sta Player1YPrev
        rts


CheckCollisionP2 subroutine
	
        ;check collisions
; Did the player collide with the wall?
        ;lda #%10000000
        bit CXP1FB
        bpl PossibleCollisionP2        
        sta CXCLR	
        
        ;if collision in Y
        lda Player2Y
        sec
        sbc Player2YPrev
        cmp #4
        bcs CollisionP2
        
        ;or if collision in X
        lda Player2X
        sec
        sbc Player2XPrev
        cmp #6
        bpl CollisionP2
        ;or X < PrevX
        lda Player2XPrev
        sec
        sbc Player2X
        cmp #4
        bpl CollisionP2
        ;inc ScoreP1
                
        rts
        
CollisionP2:	

        ;Update scores
        inc Scores
        
PossibleCollisionP2:
	
	;if moving to the right
        lda Player2X
        sec
        sbc Player2XPrev
        cmp #7
        bcc NoCollisionAtAll2  
        
	;if moving to the left	
        lda Player2XPrev
        sec
        sbc Player2X
        cmp #4
        bpl CollisionLeftP2
        
CollisionRightP2:
	lda Player2X
        lsr
        lsr
        asl
        asl
	sta Player2XPrev ; rounded X collision    
	jmp NoCollisionAtAll2
        
CollisionLeftP2:

	lda Player2X
        lsr
        lsr
        asl
        asl
        adc #4;needed when moving to the left        
	sta Player2XPrev ; rounded X collision   
        
NoCollisionAtAll2:      

	lda Player2Y
        lsr
        lsr
        asl
        asl        
        sta Player2YPrev
        rts
        
;===============================================================================
; UpdateGrid
; --------------
;
;===============================================================================
UpdateGrid subroutine
	tya
        ;adc 1
        lsr
        lsr
        tay
                
	txa
        ;adc 1	; 
	lsr
	lsr ; div 4
        ;

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
	sec
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
        
;===============================================================================
; MoveJoystick
; --------------
; Read joystick movement and apply to object 0
;===============================================================================        
; Read joystick movement and apply to object 0
MoveJoystick1
; Move vertically
; (up and down are actually reversed since ypos starts at bottom)
	ldx Player1Y
	lda #%00100000	;Up?
	bit SWCHA
	bne SkipMoveUp
        cpx #1
        bcc SkipMoveUp
        dex
        
        stx Player1Y
        ;round X position
        lda Player1X
        lsr
        lsr
        asl
        asl        
        sta Player1XPrev
        adc #1
        sta Player1X        
        ;---        
        rts
SkipMoveUp
	lda #%00010000	;Down?
	bit SWCHA 
	bne SkipMoveDown
        cpx #72
        bcs SkipMoveDown
        inx        
        
        stx Player1Y
        ;round X position
        lda Player1X
        lsr
        lsr
        asl
        asl
        sta Player1XPrev
        adc #1
        sta Player1X
        ;---        
        rts        
SkipMoveDown
	stx Player1Y
	; Move horizontally
        ldx Player1X
	lda #%01000000	;Left?
	bit SWCHA
	bne SkipMoveLeft
        cpx #2
        bcc SkipMoveLeft
        dex
        
        stx Player1X
        ;round Y position
        lda Player1Y
        lsr
        lsr
        asl
        asl
        ;adc #1
        sta Player1Y
        ;---        
        rts         
SkipMoveLeft
	lda #%10000000	;Right?
	bit SWCHA 
	bne SkipMoveRight
        cpx #157
        bcs SkipMoveRight
        inx        
        
        stx Player1X
        ;round Y position
        lda Player1Y
        lsr
        lsr
        asl
        asl
        ;adc #1
        sta Player1Y
        ;---        
        ;rts          
SkipMoveRight	
	;stx Player1X   
	rts
        
; Read joystick movement and apply to object 0
MoveJoystick2
; Move vertically
; (up and down are actually reversed since ypos starts at bottom)
	ldx Player2Y
	lda #%00000010	;Up?
	bit SWCHA
	bne SkipMoveUp2
        cpx #1
        bcc SkipMoveUp2
        dex
        
        stx Player2Y
        ;round X position
        lda Player2X
        lsr
        lsr
        asl
        asl        
        sta Player2XPrev
        adc #1
        sta Player2X        
        ;---        
        rts
SkipMoveUp2
	lda #%00000001	;Down?
	bit SWCHA 
	bne SkipMoveDown2
        cpx #72
        bcs SkipMoveDown2
        inx        
        
        stx Player2Y
        ;round X position
        lda Player2X
        lsr
        lsr
        asl
        asl
        sta Player2XPrev
        adc #1
        sta Player2X
        ;---        
        rts        
SkipMoveDown2
	stx Player2Y
	; Move horizontally
        ldx Player2X
	lda #%00000100	;Left?
	bit SWCHA
	bne SkipMoveLeft2
        cpx #2
        bcc SkipMoveLeft2
        dex
        
        stx Player2X
        ;round Y position
        lda Player2Y
        lsr
        lsr
        asl
        asl
        ;adc #1
        sta Player2Y
        ;---        
        rts         
SkipMoveLeft2
	lda #%00001000	;Right?
	bit SWCHA 
	bne SkipMoveRight2
        cpx #157
        bcs SkipMoveRight2
        inx        
        
        stx Player2X
        ;round Y position
        lda Player2Y
        lsr
        lsr
        asl
        asl
        ;adc #1
        sta Player2Y
        ;---        
        ;rts          
SkipMoveRight2
	;stx Player1X   
	rts
     
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

; Bitmap pattern for digits
DigitsBitmap 
        .byte $EE,$AA,$AA,$AA,$EE;0
        .byte $22,$22,$22,$22,$22;1
        .byte $EE,$88,$EE,$22,$EE;2
        .byte $EE,$22,$66,$22,$EE;3
        .byte $22,$22,$EE,$AA,$AA;4
        .byte $EE,$22,$EE,$88,$EE;5
        .byte $EE,$AA,$EE,$88,$EE;6
        .byte $22,$22,$22,$22,$EE;7
        .byte $EE,$AA,$EE,$AA,$EE;8
        .byte $EE,$22,$EE,$AA,$EE;9
	    
GradientColorBK
        .byte #$60
        .byte #$62
        .byte #$64
        .byte #$66
        .byte #$68
        .byte #$6A
        .byte #$6C	      
		    
				
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