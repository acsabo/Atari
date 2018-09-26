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
PARP0 		equ 0
PARP1		equ 1
 
;===============================================================================
; Define RAM Usage
;===============================================================================

    ; define a segment for variables
    ; .U means uninitialized, does not end up in ROM
        SEG.U VARS
    
    ; RAM starts at $80
        ORG $80             

    ; coming soon!
Player0X	.byte
Player1X	.byte

Player0Y	.byte
Player1Y	.byte      

Player0XPrev	.byte
Player1XPrev	.byte

Player0YPrev	.byte
Player1YPrev	.byte

TempP0		.byte
TempP1		.byte

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
        lda #4
        sta Player0X
        lda #4
        sta Player0XPrev
        
	lda #36
        sta Player0Y
        sta Player0YPrev
        
        ;player position
        lda #152
        sta Player1X
        lda #152
        sta Player1XPrev
        
        lda #36
        sta Player1Y
        sta Player1YPrev
        
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
        lda #$33
        sta COLUBK
        
	lda #$60
        sta COLUPF        
        
        lda Player0Y
        lsr        
        lsr
        sta TempP0
        

        lda Player1Y
        lsr
        lsr
        sta TempP1        
        
        ldy #MaxRows	; start        
        ;===========
        SLEEP 18	; TRICK TO WAIT FOR THE RIGHT TIME
PatternChanged      

	;-------------- AQUI OCORRE O SALTO PORQUE NÃO HOUVE TEMPO SUFICIENTE PARA DESENHAR I FIM DA LINHA
        lda #$F0
        cpy TempP1
        beq doDrawP2
        lda #0			; no, load the padding offset (0)        
doDrawP2:       
	tax	; backup para dar tempo !!! aproveitando o x já que está zerado
        
        lda #$F0
        cpy TempP0
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

        lda TempP0
        sta PF1
        
	SLEEP #20

        lda TempP1
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

        ;-------------------
        sta HMCLR	; reset the old horizontal position
        
        ldx PARP0
        jsr SetHorizPos
        
        ldx PARP1
        jsr SetHorizPos     
        
        sta WSYNC
        sta HMOVE	; gotta apply HMOVE        
        ;-------------------

	ldx PARP0
        jsr CheckCollision
        
        ldx PARP1
        jsr CheckCollision

        
        ldy Player0YPrev
        ldx Player0XPrev
        jsr UpdateGrid       

	
        ldy Player1YPrev
        ldx Player1XPrev
        jsr UpdateGrid       
        
   
   	ldy PARP0
        jsr MoveJoystick
        
        ldy PARP1        
        jsr MoveJoystick
        
        
        
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
        
SetHorizPos
	lda Player0X,x
	sta WSYNC	; start a new line
        bit 0		; waste 3 cycles
	sec		; set carry flag
DivideLoop
	sbc #15		; subtract 15
	bcs DivideLoop	; branch until negative
	eor #7		; calculate fine offset
	asl
	asl
	asl
	asl
	sta RESP0,x	; fix coarse position
	sta HMP0,x	; set fine offset
	rts		; return to caller        

; Fetches bitmap data for two digits of a
; BCD-encoded number, storing it in TempP1 and TempP2
; FontBuf+x to FontBuf+4+x.
UpdateScoreLine subroutine
	;---------- PLAYER 1 SCORE
        lda Scores
        and #$0F	; mask out the least significant digit
	
        sta TempP0
        asl
        asl        
        adc TempP0	; multiply by 5
        
        stx TempP0	; jump to current line of the digit
        adc TempP0
        
        tay
        lda DigitsBitmap,y
        ;and $0F;temporary
        sta TempP0

	;---------- PLAYER 2 SCORE
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
        
	rts

;===============================================================================
; CheckCollision
; --------------
;
;===============================================================================
CheckCollision subroutine	
;check collisions
; Did the player collide with the wall?
        ;lda #%10000000
        bit CXP0FB;x
        bpl PossibleCollision        
        sta CXCLR	
        
        ;if collision in Y
        lda Player0Y,x
        sec
        sbc Player0YPrev,x
        cmp #4
        bcs CollisionPlayer
        
        ;or if collision in X
        lda Player0X,x
        sec
        sbc Player0XPrev,x
        cmp #5
        bpl CollisionPlayer
        
        ;or X < PrevX
        lda Player0XPrev,x
        sec
        sbc Player0X,x
        cmp #5
        bpl CollisionPlayer 
                
        rts
        
CollisionPlayer:	

        ;Update scores
        inc Scores        
        
PossibleCollision:
	
	;if moving to the right
        lda Player0X,x
        sec
        sbc Player0XPrev,x
        cmp #6
        bcc NoCollisionAtAll  
                
	;if moving to the left	
        lda Player0XPrev,x
        sec
        sbc Player0X,x
        cmp #1
        bpl CollisionLeft
        
CollisionRight:
	     
	lda Player0X,x
        lsr
        lsr
        asl
        asl        
	sta Player0XPrev,x ; rounded X collision    
	jmp NoCollisionAtAll
        
CollisionLeft:
	      
	lda Player0X,x
        adc #4 ;needed when moving to the left        
        lsr
        lsr
        asl
        asl
	sta Player0XPrev,x ; rounded X collision   
        
NoCollisionAtAll:      

	lda Player0Y,x
        lsr
        lsr
        asl
        asl        
        sta Player0YPrev,x
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
MoveJoystick
; Move vertically
; (up and down are actually reversed since ypos starts at bottom)
	ldx Player0Y,y
	lda CRTP0DOWN,y;Down?
	bit SWCHA
	bne SkipMoveUp
        cpx #1
        bcc SkipMoveUp
        dex
        
        stx Player0Y,y
        ;round X position
        lda Player0X,y
        lsr
        lsr
        asl
        asl    
        sta Player0X,y 
        sta Player0XPrev,y
        ;---        
        rts
SkipMoveUp
	lda CRTP0UP,y;UP?
	bit SWCHA 
	bne SkipMoveDown
        cpx #72
        bcs SkipMoveDown
        inx        
        
        stx Player0Y,y
        ;round X position
        lda Player0X,y        
        lsr
        lsr
        asl
        asl
        sta Player0X,y 
        sta Player0XPrev,y
        ;---        
        rts        
SkipMoveDown
	stx Player0Y,y
	; Move horizontally
        ldx Player0X,y
	lda CRTP0LEFT,y;Left?
	bit SWCHA
	bne SkipMoveLeft
        cpx #1
        bcc SkipMoveLeft
        dex
        
        stx Player0X,y
        ;round Y position
        ;---        
        rts         
SkipMoveLeft
	lda CRTP0RIGHT,y;Right?
	bit SWCHA 
	bne SkipMoveRight
        cpx #156
        bcs SkipMoveRight
        inx        
        
        stx Player0X,y
        ;round Y position
SkipMoveRight	
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
        
CRTP0UP		.byte #%00010000
CRTP1UP		.byte #%00000001

CRTP0DOWN	.byte #%00100000
CRTP1DOWN	.byte #%00000010

CRTP0LEFT	.byte #%01000000
CRTP1LEFT	.byte #%00000100

CRTP0RIGHT	.byte #%10000000	
CRTP1RIGHT	.byte #%00001000	

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