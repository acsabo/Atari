;===============================================================================
; Program Information
;===============================================================================

    ; Program:      TRONIC
    ; Program by:   Adriano C. Sabo
    ; Last Update:  Semptemper 27, 2018
    
;===============================================================================
; Change Log
;===============================================================================
 
    ; 2018.10.27 - generate a stable display

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
PARP0 		equ 0
PARP1		equ 1
INITIAL_STATE   equ 0
P0WINS_STATE	equ 254
P1WINS_STATE	equ 253
NOWINS_STATE	equ 252
RESET_STATE	equ 255
COUNTDOWN_STATE equ 128
COUNTDOWN_VALUE	equ 60

INIT_Player0X	equ 4
INIT_Player1X	equ 152
INIT_PlayerY	equ 36

TXT_GETREAD     equ 0
TXT_PLAYER0     equ 72
TXT_PLAYER1     equ 144

DGT_1		equ 0
ROW_SHIFT1	equ 3
ROW_SHIFT2	equ 6

;===============================================================================
; Define RAM Usage
;===============================================================================

    ; define a segment for variables
    ; .U means uninitialized, does not end up in ROM
        SEG.U VARS
    
    ; RAM starts at $80
        ORG $80             

;Defines the grid/matrix to track players movement
PF0_left	ds 19
PF1_left	ds 19
PF2_left	ds 19
PF0_right	ds 19
PF1_right	ds 19
PF2_right	ds 19

;Stores players positions (it will reset each frame)
TempP0		.byte
TempP1		.byte

;Players Y coordinates (do not change this order)
Player0X	.byte
Player1X	.byte

;Players Y coordinates (do not change this order)
Player0Y	.byte
Player1Y	.byte      

;Stores the points for both players; it also is used to control the game State
Scores		.byte

;Keep the last changed direction of the controllers for both players
Controls	.byte

;Used to control speed and other flags
Flags		.byte

;Used to control the countdown and text blink
VarP0		.byte
VarP1		.byte
	       
;===============================================================================
; Define Start of Cartridge
;===============================================================================

    ; define a segment for code
    SEG CODE    
    
    ; 2K ROM starts at $F800, 4K ROM starts at $F000
    ORG $F800
     
ResetGame subroutine	
        ;init Score
        lda #$00
        sta Scores
        rts
        
ResetPositions subroutine
        ;init grid memory
        ldy #0
        lda #$00
        ldx #114
        
        ;resets each grid cells
initGrid:
        sta PF0_left,y	
        iny
        dex
        bne initGrid    

        ;player vertical position on the grid
        lda #INIT_Player0X
        sta Player0X
        
	;player vertical position on the grid
        lda #INIT_Player1X
        sta Player1X
        
        ;set players horizontal position on the grid
        lda #INIT_PlayerY
        sta Player0Y
        sta Player1Y

        rts
        
InitSystem
        ; CLEAN_START is a macro found in macro.h
        ; it sets all RAM, TIA registers and CPU registers to 0
        CLEAN_START
        
        lda INITIAL_STATE
        sta Controls
        
	jsr ResetPositions

        
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

;MORE CODE CAN COME IN HERE!!!

        ;player color
        lda #$84
        sta COLUP0        
        lda #$14
        sta COLUP1    

        ;line delimiter before grid
        lda #$68
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
        lda GradientColorGrid,x
        sta COLUBK

        ;lda #$40
        ;sta COLUPF        

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
        SLEEP #18	; TRICK TO WAIT FOR THE RIGHT TIME
PatternChanged:
        ;-------------- AQUI OCORRE O SALTO PORQUE N�O HOUVE TEMPO SUFICIENTE PARA DESENHAR I FIM DA LINHA
        lda #$F0
        cpy TempP1
        beq doDrawP2
        lda #0			; no, load the padding offset (0)        
doDrawP2:       
        tax	; backup para dar tempo !!! aproveitando o x j� que est� zerado

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

        lda GradientColorGrid,x	; guideline on the grid
        sta WSYNC        
        sta COLUBK
SkipLine
        lda PF0_left,y 
        sta PF0 

        lda PF1_left,y 
        sta PF1     

        lda PF2_left,y 
        sta PF2 

        SLEEP #4

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
        lda #$68
        sta COLUBK	

        lda #0
        sta PF0
        sta PF1
        sta PF2 	; clear playfield


        sta WSYNC	; add extra line to keep simetry with the top	
        sta COLUBK
        sta COLUPF
        
        lda #0
        sta COLUBK
        
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC
       
         
;===============================================================================
; Scoreboard
;===============================================================================
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
        lda TempP0        
        sta WSYNC
        sta PF1
        
        lda GradientColorBK,x		        
        sta COLUBK

        SLEEP #26

        lda TempP1
        sta PF1

        dey        
        bne nxtScanLine

        SLEEP #8

        dex
        bpl nxtDigitLine
        sta WSYNC       

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
        

        ldx #5
        lda GradientColorBK,x		        
        sta COLUBK
        sta WSYNC
        sta WSYNC
                
        lda #0
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
        sta HMCLR		; reset the old horizontal position

        ldx PARP0
        jsr SetHorizPos

        ldx PARP1
        jsr SetHorizPos     

        sta WSYNC
        sta HMOVE		; gotta apply HMOVE        
        ;-------------------
        
;===============================================================================
; CHECKING SWITCHES
;===============================================================================

ProcessSwitches:
        lda SWCHB       	; load in the state of the switches
        lsr             	; D0 is now in C
        bcs skipSwitches    	; if D0 was on, the RESET switch was not held       
   
	jsr ResetGame		; prepare game state
        jsr ResetPositions	; reset the grid
       
        lda #COUNTDOWN_STATE	; change state to countdown
        sta Controls
        
        lda #20			; set initial countdown to 3
        sta VarP0; start from 3        
        lda #COUNTDOWN_VALUE
        sta VarP1; restart timer
        jmp OSwait  
        
ResetTurn:        
        jsr ResetPositions
        
        ;set init directions
        lda CRTP0RIGHT
        ora CRTP1LEFT
        sta Controls
        
        jmp OSwait        
skipSwitches:


;===============================================================================
; CHECKING SWITCHES
;===============================================================================

	;if in start mode 
        lda Controls        
        cmp #RESET_STATE
        beq ResetTurn
        
        lda Controls
        cmp #P0WINS_STATE
        bne SkipP0Wins

        ldy #TXT_PLAYER0
        jsr DrawText
        jmp OSwait        
SkipP0Wins:

        lda Controls
        cmp #P1WINS_STATE
        bne SkipP1Wins

        ldy #TXT_PLAYER1
        jsr DrawText
        jmp OSwait        
SkipP1Wins:

        lda Controls        
        cmp #INITIAL_STATE
        bne SkipDrawGetReady
        
        ldy #TXT_GETREAD
        jsr DrawText
        
        jmp OSwait
        
SkipDrawGetReady:   

        lda Controls        
        cmp #COUNTDOWN_STATE
        bne SkipCountdown
        
        jsr DrawCountdown        
        jmp OSwait
        
SkipCountdown:

        lda #$40
        sta COLUPF     

        ldy Player0Y
        ldx Player0X
        jsr UpdateGrid       


        ldy Player1Y
        ldx Player1X
        jsr UpdateGrid       

     	;------------------------
        ldy PARP0
        jsr UpdateJoystickStatus

        ldy PARP1        
        jsr UpdateJoystickStatus
        ;------------------------
        
        
        ;MOVE SLOWER, by skeeping some frames 
        ldy Flags        
        cpy #0
        beq SkipMoveAround
        
        dey
        sty Flags
        
        ldy PARP0
        jsr MovePlayerAround

        ldy PARP1        
        jsr MovePlayerAround   
        
	jmp ContinueMoveAround        
SkipMoveAround:

        ldy #1
        sty Flags
ContinueMoveAround:        
        
        ;------------------------        
        ldy PARP0
        ;jsr CheckCollision

        ldy PARP1
        ;jsr CheckCollision
        
        ;Clear collision detection for this frame
        sta CXCLR

        ;Will use IA to control the other player
        ;jsr UpdateIAPlayer

;===============================================================================
; Restaring game loop
;===============================================================================

        
OSwait:
        sta WSYNC   ; Wait for SYNC (halts CPU until end of scanline)
        lda INTIM   ; Check the timer
        bne OSwait  ; Branch if its Not Equal to 0

        jmp Main            ; JuMP to Main


;===============================================================================
; DrawText
; --------------
;
;===============================================================================
DrawText subroutine
	ldx #12
doLoop:
        ; fill left side
        lda TextPanel,y 
        sta PF0_left,x+ROW_SHIFT1
        iny

        lda TextPanel,y
        sta PF1_left,x+ROW_SHIFT1
        iny

        lda TextPanel,y
        sta PF2_left,x+ROW_SHIFT1
        iny

        ; fill right side
        lda TextPanel,y
        sta PF0_right,x+ROW_SHIFT1
        iny

        lda TextPanel,y
        sta PF1_right,x+ROW_SHIFT1
        iny

        lda TextPanel,y
        sta PF2_right,x+ROW_SHIFT1
        iny

        dex        
        bne doLoop
        
        ;reusing variable to cause text blink effect
        inc VarP0
        lda VarP0
        sta COLUPF   
        
	rts      
        

;===============================================================================
; Draw Contdown digits
; --------------
; Value will be put in PF2 PF0 in assynchronous grid
;===============================================================================             
DrawCountdown subroutine
	ldx #5
        
        ldy VarP1
        dey
        beq DecCountdown 
        dec VarP1
        
        ldy VarP0

doLoop_:        
        ; fill left side
        lda #00
        sta PF0_left,x+ROW_SHIFT2
        sta PF1_left,x+ROW_SHIFT2
        lda Countdown,y
        sta PF2_left,x+ROW_SHIFT2
        iny

        ; fill right side
        lda Countdown,y
        sta PF0_right,x+ROW_SHIFT2
        iny

	lda #00
        sta PF1_right,x+ROW_SHIFT2
        sta PF2_right,x+ROW_SHIFT2
        
        dex        
        bne doLoop_

        lda #$65
        sta COLUPF   
        rts
        
DecCountdown:
	ldy VarP0
        dey
        cpy #0
        bmi SkipCountStatus        

	lda VarP0
        ;sec
        sbc #10
        sta VarP0	; jump to the next digt
        
        lda #COUNTDOWN_VALUE
        sta VarP1	; restart timer
	rts
SkipCountStatus:        
        
        lda #RESET_STATE
        sta Controls
	rts        

;===============================================================================
; UpdateJoystick
; --------------
; Read joystick movement and apply to object 0
;===============================================================================        
UpdateJoystickStatus subroutine     
        lda Controls
        and CRTP0HALF,y
        sta TempP0; store current direction
        
	; Move vertically
        ; (up and down are actually reversed since ypos starts at bottom)
        lda CRTP0DOWN,y;Down?
        bit SWCHA
        bne skpDown
 
       	;check the oposite direction
        lda CRTP0UP,y;UP?
        bit TempP0                
        bne skpDown	;avoid going in the oposite direction
        ;----
        
        lda CRTP0DOWN,y
        sta TempP0
        lda Controls
        and CRTP0MERGE,y        
        ora TempP0
        sta Controls
        rts
skpDown:
        lda CRTP0UP,y;UP?
        bit SWCHA 
        bne skpUp

       	;check the oposite direction
        lda CRTP0DOWN,y;UP?
        bit TempP0                
        bne skpUp	;avoid going in the oposite direction
        ;----

        lda CRTP0UP,y
        sta TempP0
        lda Controls
        and CRTP0MERGE,y        
        ora TempP0
        sta Controls
        rts
skpUp:
        lda CRTP0LEFT,y;Left?
        bit SWCHA
        bne skpLeft        
        
       	;check the oposite direction
        lda CRTP0RIGHT,y;UP?
        bit TempP0                
        bne skpLeft	;avoid going in the oposite direction
        ;----
        
        lda CRTP0LEFT,y
        sta TempP0
        lda Controls
        and CRTP0MERGE,y        
        ora TempP0
        sta Controls
        rts
skpLeft:
        lda CRTP0RIGHT,y;Right?
        bit SWCHA 
        bne skpRight        
        
       	;check the oposite direction
        lda CRTP0LEFT,y;UP?
        bit TempP0                
        bne skpRight	;avoid going in the oposite direction
        ;----
        
        lda CRTP0RIGHT,y
        sta TempP0
        lda Controls
        and CRTP0MERGE,y        
        ora TempP0
        sta Controls
        rts

skpRight:	

        rts
        
;===============================================================================
; MovePlayerAround
; --------------
MovePlayerAround subroutine
	lda Controls        
        and CRTP0HALF,y
        sta TempP0
	
        ; Move vertically
        ; (up and down are actually reversed since ypos starts at bottom)
        ldx Player0Y,y
        
        lda CRTP0DOWN,y;Down?
        bit TempP0
        
        beq SkipMoveUp
        cpx #1
        bcc SkipMoveUp
        dex
        stx Player0Y,y        
        
        ;round X
	lda Player0X,y
        lsr
        lsr
        asl
        asl
        sta Player0X,y 
        ;sta Player0XPrev,y
        rts
SkipMoveUp
        lda CRTP0UP,y;UP?
        bit TempP0 
        
        beq SkipMoveDown
        cpx #72
        bcs SkipMoveDown
        inx
        stx Player0Y,y
        
        ;round X
	lda Player0X,y
        lsr
        lsr
        asl
        asl
        sta Player0X,y 
        ;sta Player0XPrev,y       
        rts     
SkipMoveDown
        ; Move horizontally
        ldx Player0X,y
        lda CRTP0LEFT,y;Left?
        
        bit TempP0
        beq SkipMoveLeft
        cpx #1
        bcc SkipMoveLeft
        dex        
        stx Player0X,y
        ;round Y
	lda Player0Y,y
        ;sta Player0YPrev,y        
        rts       
SkipMoveLeft
        lda CRTP0RIGHT,y;Right?
        bit TempP0 
        beq SkipMoveRight
        cpx #156
        bcs SkipMoveRight
        inx        
        stx Player0X,y      
        ;round Y
	lda Player0Y,y
        ;sta Player0YPrev,y        
SkipMoveRight	
        rts        
        
;===============================================================================
; SetHorizPos
; --------------
;
;===============================================================================
SetHorizPos subroutine
        lda Player0X,x
        sta WSYNC	; start a new line
        bit 0		; waste 3 cycles
        sec		; set carry flag
DivideLoop:
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

;===============================================================================
; I.A Player 2
; --------------
;
;===============================================================================
UpdateIAPlayer	subroutine
	lda Player0Y
        and $0F
        lsr
        lsr
        lsr
  
        tay        
	lda RandomDirP1,y
        ora Controls
        sta Controls

        rts
        

;===============================================================================
; UpdateScoreLine
; --------------
;
;===============================================================================
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

        stx TempP0	; add relative index being rendered
        adc TempP0

        tay
        lda DigitsBitmap,y       
        and #$0F

        sta TempP0

        ;---------- PLAYER 2 SCORE
        lda Scores
        and #$F0
        lsr
        lsr        
        lsr
        lsr        

        sta TempP1
        asl
        asl        
        adc TempP1	; multiply by 5        

        stx TempP1
        adc TempP1

        tay
        lda DigitsBitmap,y
        and #$0F
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
        cpx PARP0
        beq SkipCollP0        
        bit CXP1FB
        bpl CollP0
        jmp SkipCollP1
        
SkipCollP0:        
        bit CXP0FB
        bpl CollP1        
SkipCollP1:         
        rts

CollP0:     
        ; updating Score
	lda Scores
        sta TempP0
        inc TempP0
        lda TempP0
        sta Scores
        
        cmp #9
        bcc SkipP0Inc
        
        ;flag to reset the turn
        lda #P0WINS_STATE
        sta Controls
        rts        
SkipP0Inc: 
        ;flag to reset the turn
        lda #RESET_STATE
        sta Controls                
	rts
CollP1:        
	;rts; REMOVE THIS
	; updating Score
        lda Scores
        ;and #$F0
        lsr
        lsr
        lsr
        lsr
        adc #1
        asl
        asl
        asl
        asl
        
        sta TempP0
        lda Scores
        and #$0F
        ora TempP0
        sta Scores
        
        lda TempP0
        cmp #144; 9 << 4
        bcc SkipP1Inc
        
        ;flag to reset the turn
        lda #P1WINS_STATE
        sta Controls
        rts        
SkipP1Inc: 
        ;flag to reset the turn
        lda #RESET_STATE
        sta Controls                
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

CRTP0HALF	.byte $F0
CRTP1HALF	.byte $0F

CRTP0MERGE	.byte $0F
CRTP1MERGE	.byte $F0

BitReprF0	.byte #%00010000,#%00100000,#%01000000,#%10000000

BitReprF1	.byte #%10000000,#%01000000,#%00100000,#%00010000,#%00001000,#%00000100,#%00000010,#%00000001
		
BitReprF2	.byte #%00000001,#%00000010,#%00000100,#%00001000,#%00010000,#%00100000,#%01000000,#%10000000

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
	.byte #$64        
	.byte #$66
        .byte #$68
        .byte #$66
        .byte #$64        
	.byte #$62	; edge
        
GradientColorGrid     
        .byte #0
        .byte #0
        .byte #0
        .byte #0
        .byte #80
        .byte #0
        .byte #0
        .byte #0
        

TextPanel
;TextTextGetReady
	;GET
        .byte #$00,#$00,#$00,#$00,#$80,#$00
        .byte #$00,#$00,#$DE,#$B0,#$E0,#$00
        .byte #$00,#$00,#$42,#$20,#$80,#$00
        .byte #$00,#$00,#$DA,#$30,#$80,#$00
        .byte #$00,#$00,#$52,#$00,#$80,#$00
        .byte #$00,#$00,#$DE,#$30,#$80,#$00
        ;READY
        .byte #$00,#$00,#$00,#$00,#$02,#$00
        .byte #$00,#$03,#$DD,#$D0,#$AA,#$00
        .byte #$00,#$02,#$45,#$50,#$AA,#$00
        .byte #$00,#$03,#$CC,#$50,#$BA,#$00
        .byte #$00,#$02,#$45,#$50,#$90,#$00
        .byte #$00,#$02,#$5D,#$D0,#$12,#$00        
;TextPlayer0Wins
        .byte #$00,#$00,#$00,#$00,#$80,#$00
        .byte #$00,#$00,#$DE,#$B0,#$E0,#$00
        .byte #$00,#$00,#$42,#$20,#$80,#$00
        .byte #$00,#$00,#$DA,#$30,#$80,#$00
        .byte #$00,#$00,#$52,#$00,#$80,#$00
        .byte #$00,#$00,#$DE,#$30,#$80,#$00
        
        .byte #$00,#$00,#$00,#$00,#$02,#$00
        .byte #$00,#$03,#$DD,#$D0,#$AA,#$00
        .byte #$00,#$02,#$45,#$50,#$AA,#$00
        .byte #$00,#$03,#$CC,#$50,#$BA,#$00
        .byte #$00,#$02,#$45,#$50,#$90,#$00
        .byte #$00,#$02,#$5D,#$D0,#$12,#$00        
;TextPlayer1Wins
        .byte #$00,#$00,#$00,#$00,#$80,#$00
        .byte #$00,#$00,#$DE,#$B0,#$E0,#$00
        .byte #$00,#$00,#$42,#$20,#$80,#$00
        .byte #$00,#$00,#$DA,#$30,#$80,#$00
        .byte #$00,#$00,#$52,#$00,#$80,#$00
        .byte #$00,#$00,#$DE,#$30,#$80,#$00
        
        .byte #$00,#$00,#$00,#$00,#$02,#$00
        .byte #$00,#$03,#$DD,#$D0,#$AA,#$00
        .byte #$00,#$02,#$45,#$50,#$AA,#$00
        .byte #$00,#$03,#$CC,#$50,#$BA,#$00
        .byte #$00,#$02,#$45,#$50,#$90,#$00
        .byte #$00,#$02,#$5D,#$D0,#$12,#$00           

Countdown  ;PF2  PF0 
	;Number 1
	.byte #$80,#$00
	.byte #$80,#$00
	.byte #$80,#$00
	.byte #$80,#$00
	.byte #$80,#$00
	;Number 2
	.byte #$C0,#$10
	.byte #$00,#$10
	.byte #$80,#$00
	.byte #$40,#$00
	.byte #$C0,#$10
	;Number 3
	.byte #$C0,#$10
	.byte #$00,#$10
	.byte #$80,#$00
	.byte #$00,#$10
	.byte #$C0,#$10


RandomDirP1   
	.byte #%00000100;LEFT
	.byte #%00000001;UP
	.byte #%00001000;RIGHT
	.byte #%00000010;DOWN
	.byte #%00000100;LEFT
	.byte #%00000001;UP
	.byte #%00001000;RIGHT
	.byte #%00000010;DOWN
	.byte #%00000100;LEFT
	.byte #%00000001;UP
	.byte #%00001000;RIGHT
	.byte #%00000010;DOWN
	.byte #%00000100;LEFT
	.byte #%00000001;UP
	.byte #%00001000;RIGHT
	.byte #%00000010;DOWN
	.byte #%00000100;LEFT        

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