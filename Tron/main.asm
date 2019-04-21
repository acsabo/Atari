;===============================================================================
; Program Information
;===============================================================================
	; Program: KTRON
	; By:	 Adriano C. Sabo
	; Contact:	 acsabo@hotmail.com
	; Last Update: February 25, 2019
;===============================================================================
; Change Log
;===============================================================================
	; 2018.10.27 - generate a stable display
	; 2019.02.25 - refactoring players movement on the grid
;===============================================================================
; Initialize dasm
;===============================================================================
	; Dasm supports a number of processors, this line tells dasm the code
	; is for the 6502 CPU. The Atari has a 6507, which is 6502 that's been
	; put into a "reduced package". This package limits the 6507 to an 8K
	; address space and also removes support for external interrupts.
	PROCESSOR 6502
	include "vcs.h"
	include "macro.h"
	include "xmacro.h"
SPEED				equ 12
SpriteHeight		equ 8
MaxRows				equ 19
PARP0				equ 0
PARP1				equ 1
INITIAL_STATE 		equ 0
P0WINS_STATE		equ 1
P1WINS_STATE		equ 2
RESET_STATE			equ 3
START_STATE			equ 4
PAUSE_STATE			equ 5
TIE_STATE			equ 6
COUNTDOWN_STATE 	equ 7
NORMAL_STATE		equ 8
PS_P0_TURN			equ %10000000
PS_P1_TURN			equ %00001000
PS_P0_POWR			equ %01000000
PS_P1_POWR			equ %00000100
NO_WINNER			equ $99 ; tie
COUNTDOWN_VALUE		equ 60
INIT_Player0X		equ 8
INIT_Player1X		equ 148
INIT_PlayerY		equ 40
COLOR_Player0		equ 65;25
COLOR_Player1		equ 130
COLOR_Playfield 	equ $44
TXT_GETREAD			equ 0
TXT_PLAYER0			equ 60
TXT_PLAYER1			equ 120
ROW_SHIFT1			equ 3
ROW_SHIFT2			equ 7

;===============================================================================
; Define RAM Usage
;===============================================================================
	; define a segment for variables
	; .U means uninitialized, does not end up in ROM
	SEG.U VARS
	; RAM starts at $80
	ORG $80
;Defines the grid/matrix to track players movement
PF0_left		ds 20
PF1_left		ds 20
PF2_left		ds 20
;PF0_right		ds 18
PF1_right		ds 20
PF2_right		ds 20
;Stores players positions (it will reset each frame)
TempP0			.byte
TempP1			.byte
;Players X coordinates (do not change this order)
Player0X		.byte
Player1X		.byte
;Players Y coordinates (do not change this order)
Player0Y		.byte
Player1Y		.byte
;Stores the points for both players; it also is used to control the game State
Scores			.byte
;Keep the last changed direction of the controllers for both players
Controls		.byte
;Used to control speed and other flags
SpeedCounter		.byte
;Game status Control
GameState		.byte
;Used to control the countdown and text blink
VarP0			.byte
VarP1			.byte
;extras
PlayerState		.byte
PowerUps		.byte
SndP0			.byte
SndP1			.byte
SndP2			.byte
;There's no bytes left :) the last one if for the stack pointer
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
		sta PlayerState
		;store powerups for each player
		lda #%00010111
		sta PowerUps
		rts
                
ResetPositions subroutine
		;init grid memory
		ldy #0
		lda #$00
		ldx #100	;20 lines x 5 bytes 
                
		;Clear collision detection for this frame
		sta CXCLR
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
		ldy #4
		sty SpeedCounter
		;reset status
		lda #00
 		sta PlayerState
		;player color
		lda #COLOR_Player0
		sta COLUP0
		lda #COLOR_Player1
		sta COLUP1
		rts
InitSystem
		; CLEAN_START is a macro found in macro.h
		; it sets all RAM, TIA registers and CPU registers to 0
		CLEAN_START
		lda #INITIAL_STATE
		sta GameState
		sta Controls
		jsr ResetPositions
		ldy #sfxCOLLECT ; Game Over sound effect
		jsr SFX_TRIGGER
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
		lda #2 ; LoaD Accumulator with 2 so D1=1
		ldx #49 ; LoaD X with 49
		sta WSYNC ; Wait for SYNC (halts CPU until end of scanline)
		sta VSYNC ; Accumulator D1=1, turns on Vertical Sync signal
		stx TIM64T ; set timer to go off in 41 scanlines (49 * 64) / 76
		sta WSYNC ; Wait for Sync - halts CPU until end of 1st scanline of VSYNC
		sta WSYNC ; wait until end of 2nd scanline of VSYNC
		lda #0 ; LoaD Accumulator with 0 so D1=0
		sta WSYNC ; wait until end of 3rd scanline of VSYNC
		sta VSYNC ; Accumulator D1=0, turns off Vertical Sync signal
;===============================================================================
; Vertical Blank
; --------------
; game logic runs here. Coming soon!
;===============================================================================
		;---BIG TRICK TO FLICK PLAYERS TO GET ENOUGHT TIME TO DRAW THE GRID
		ldx #PARP1;#0
		lda SndP2
		cmp #PARP0;#1
		beq switchPlayer
		stx SndP2;receive 1
		ldx #PARP0;#1
switchPlayer:
		stx SndP2
                ;---
                
		;MORE CODE CAN COME IN HERE!!!
		jsr SFX_UPDATE
                
		;line delimiter before grid
		lda #$68
		sta COLUBK
;===============================================================================
; Kernel
; ------
; here we update the registers in TIA (the video chip) in order to generate
; what the player sees. For now we're just going to output 192 colored
; scanlines lines so we have something to see.
;===============================================================================
Kernel
		sta WSYNC ; Wait for SYNC (halts CPU until end of scanline)
		lda INTIM ; check the timer
		bne Kernel ; Branch if its Not Equal to 0
		; turn on the display
		sta VBLANK ; Accumulator D1=0, turns off Vertical Blank signal (image output on)
		; Set up timer (in case of bugs where we don't hit exactly)
		TIMER_SETUP 192
                
		ldy GameState
                cpy #INITIAL_STATE
                beq InitialMode
GameMode:
		sta WSYNC
		jsr DrawGrid
		sta WSYNC
                jmp BeginScoreboard
InitialMode:    
		;Display a Splash Screen
		lda #$00
                sta COLUBK
                sta COLUPF
                sta WSYNC
                
                ldy #TXT_GETREAD
		jsr DrawText

		;lda #$00
                ;sta COLUBK
                ;sta COLUPF

		jsr DrawSplash
                jmp BeginOverscan                
BeginScoreboard:

;===============================================================================
; Scoreboard
;===============================================================================
		ldx #5
		lda ScoreboardColorBK,x
		sta COLUBK
		lda #%00000011; score mode
		sta CTRLPF
		;blink feedback
		lda PlayerState
		and #PS_P0_TURN
		cmp #PS_P0_TURN
		bne SkipP0Blink
		;---
		lda VarP0;random color
		sta COLUP0
SkipP0Blink:
		lda PlayerState
		and #PS_P1_TURN
		cmp #PS_P1_TURN
		bne SkipP1Blink
		lda VarP0;random color
		sta COLUP1
SkipP1Blink:
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
		lda ScoreboardColorBK,x
		sta COLUBK
		;may print power counters
		cpx #0
		bne SkeepPWShowP0
		lda PowerUps
		and #$07
		sta PF2
		SLEEP #8
		lda PowerUps
		lsr
		lsr
		lsr
		lsr
		sta PF2
		jmp SkipDelpayP0
SkeepPWShowP0:
		SLEEP #15
SkipDelpayP0:
		;SLEEP #5
		lda TempP1
		sta PF1
		SLEEP #2
		dey
		bne nxtScanLine
		SLEEP #10
		sty PF2
		dex
		bpl nxtDigitLine
                
		;end of digits panel
		lda #0
		sta PF0
		sta PF1
		sta PF2

BeginOverscan:
		;Wait for timer to finish
		TIMER_WAIT
		lda #%00000000; clear score mode
		sta CTRLPF; -> CTRLPF
;===============================================================================
; Overscan
; --------------
; game logic runs here. Since we don't have any yet, just delay so that the
; entire video frame consists of 262 scanlines
;===============================================================================
		sta WSYNC ; Wait for SYNC (halts CPU until end of scanline)
		lda #2 ; LoaD Accumulator with 2 so D1=1
		sta VBLANK ; STore Accumulator to VBLANK, D1=1 turns image output off
		; set the timer for 27 scanlines. Each scanline lasts 76 cycles,
		; but the timer counts down once every 64 cycles, so use this
		; formula to figure out the value to set.
		; (scanlines * 76) / 64
		; Also note that it might be slight off due to when on the scanline TIM64T
		; is updated. So use Stella to check how many scanlines the code is
		; generating and adjust accordingly.
		lda #32 ; set timer for 27 scanlines, 32 = ((27 * 76) / 64)
		sta TIM64T ; set timer to go off in 27 scanlines
;===============================================================================
; CHECKING SWITCHES
;===============================================================================
ProcessSwitches:
		;may change the game mode
		;lda #$02
		;and SWCHB
		;bne SkipSelect
		;change the game mode
		;lda GameState
		;and #$F0
		;;cmp #$00
		;bne SwithToDots
		;lda #$FF
		;jmp SwithToTracks
;SwithToDots:
		;lda #$00
;SwithToTracks:
		;ora #TIE_STATE
		;sta GameState
		;jmp StartGame
;SkipSelect:
		;if not pressing RESET
		lda SWCHB	; load in the state of the switches
		lsr		; D0 is now in C
		bcs SkipSwitches	; if D0 was on, the RESET switch was not held
StartGame:
		jsr ResetGame		; prepare game state
		jsr ResetPositions	; reset the grid
		;lda GameState
		;and #$F0
		lda #COUNTDOWN_STATE	; change state to countdown
		sta GameState
		lda #20			; set initial countdown to 3
		sta VarP0; start from 3
		lda #COUNTDOWN_VALUE
		sta VarP1; restart timer
		jmp OSwait
ResetTurn:
		lda #20			; set initial countdown to 3
		sta VarP0; start from 3
		lda #COUNTDOWN_VALUE
		sta VarP1; restart timer
		jsr ResetPositions
		;set init directions
		lda CRTP0RIGHT
		ora CRTP1LEFT
		sta Controls
		; lda GameState
		;and #$F0
		lda #NORMAL_STATE
		sta GameState
		ldy #sfxCOLLECT ; select sound effect
		jsr SFX_TRIGGER ; and trigger it
		jmp OSwait
SkipSwitches:
		;Set the grid color
		lda #COLOR_Playfield;#$40
		sta COLUPF
                
		sta HMCLR		; reset the old horizontal position
		ldx #PARP0
		jsr SetHorizPos
		ldx #PARP1
		jsr SetHorizPos
                
		sta WSYNC
		sta HMOVE
;===============================================================================
; CHECKING GAME STATUS
;===============================================================================
		;Checking if Game is over
		lda Scores
		cmp #NO_WINNER
		bne SkipTie
		;set the Game state to TIE
		lda #TIE_STATE
		sta GameState
SkipTie:
		;---- CONTINUE WHEN BUTTON IS PRESSED
		bit INPT4
		bmi SkipButtonP0
		;P0 is pressing the button
		lda #PS_P0_POWR
		ora PlayerState
		sta PlayerState
		;flag a button press
		lda #1
		sta TempP0
SkipButtonP0:
		bit INPT5
		bmi SkipButtonP1
		;P1 is pressing the button
		lda #PS_P1_POWR
		ora PlayerState
		sta PlayerState
		;flag a button press
		lda #1
		sta TempP0
SkipButtonP1:
		;load the Game Status
		lda GameState
		;and #$0F	; to ignore the Game State
		;load Game State
		cmp #PAUSE_STATE
		bne SkipButtons
		;any button pressed?
		ldy TempP0
		cpy #1
		bne SkipButtons
		;will force a restart
		lda #RESET_STATE
                
		;RESET PLAYER SWITHCHING TIME
                ldx #PARP0
                stx SndP2                
SkipButtons:
		;if in start mode
		cmp #RESET_STATE
		beq ResetTurn
		;restart the game
		cmp #START_STATE
		bne SkipStartGame
		jmp StartGame
SkipStartGame:
		;Checking for Tie
		cmp #TIE_STATE
		bne SkipNoWinner
		;ldy #$FF
		;jsr DrawText
		;ldy #sfxCOLLIDE ; select sound effect
		;jsr SFX_TRIGGER ; and trigger it
		jmp OSwait
SkipNoWinner:
		;Checking if Game is over
		cmp #P0WINS_STATE
		bne SkipP0Wins
		ldy #TXT_PLAYER0; Player 1 WINS
		jsr DrawText
		jmp OSwait
SkipP0Wins:
		;Checking if Game is over
		cmp #P1WINS_STATE
		bne SkipP1Wins
		ldy #TXT_PLAYER1; Player 2 WINS
		jsr DrawText
		jmp OSwait
SkipP1Wins:
		cmp #INITIAL_STATE
		bne SkipDrawGetReady
		ldy #TXT_GETREAD
		jsr DrawText
		jmp OSwait
SkipDrawGetReady:
		cmp #COUNTDOWN_STATE
		bne SkipCountdown
                
		jsr DrawCountdown
		jmp OSwait
SkipCountdown:
		;Do nothing if Paused
		cmp #PAUSE_STATE
		;beq OSwait
		bne SkipPauseBlink
		;reusing variable to cause text blink effect
		inc VarP0
		lda VarP0
		sta COLUPF
		jmp OSwait
SkipPauseBlink:

                ;check collisions                                                                                                                                                                                     
		ldx #PARP0;SndP2                                                                                                                                                                                           
		jsr CheckCollision                                                                                                                                                                                    
                                                                                                                                                                                                                      
		;ldx PARP1                                                                                                                                                                                             
		;jsr CheckCollision 
                
		;-------------------
		;MOVE SLOWER, by skeeping some frames
		ldy SpeedCounter
		dey
		sty SpeedCounter
		bne SkipUpdates


		;ldx PARP0
		;jsr CheckCollision
		;ldx PARP1
		;jsr CheckCollision
                
		lda GameState
		;and #$0F
		cmp #PAUSE_STATE
		beq SkipUpdates
		;update joystick directions P0
		ldy #PARP0
		jsr UpdateJoystickStatus
		;update the grid
		ldy Player0Y
		ldx Player0X
		jsr UpdateGrid
		;update joystick directions P1
		ldy #PARP1
		jsr UpdateJoystickStatus
		;if changed direction
		ldy Player1Y
		ldx Player1X
		jsr UpdateGrid
		;update movement
		ldy #PARP0
		jsr MovePlayerAround
		ldy #PARP1
		jsr MovePlayerAround
		ldy #SPEED		;reset move time
		sty SpeedCounter
SkipUpdates:
                
;===============================================================================
; Restaring game loop
;===============================================================================
OSwait:
		;Clear collision detection for this frame
		sta CXCLR
		;------------------------
		;sta WSYNC ; Wait for SYNC (halts CPU until end of scanline)
		lda INTIM ; Check the timer
		bne OSwait ; Branch if its Not Equal to 0
		jmp Main ; JuMP to Main

;===============================================================================
; Draw the presentation Screen
;===============================================================================
DrawSplash subroutine
                
		ldy #MaxRows	; start
SlashLoop:
		ldx #2
                sty COLUPF
                lda ScoreboardColorBK,y
repeatLine:
		sta WSYNC
                ;+MaxRows	; guideline on the grid
		sta COLUBK
                
		lda PF0_left,y
		sta PF0
		lda PF1_left,y
		sta PF1
		lda PF2_left,y
		sta PF2
		
		lda PF0_left,y
		asl
		asl
		asl
		asl
		sta PF0
		lda PF1_right,y
		sta PF1
		lda PF2_right,y
		sta PF2
                
                dex 
                bne repeatLine
                
                
		dey
		bne SlashLoop 	; Branch if Not Equal to 0                


                
		rts

                
;===============================================================================
; Draw the grid
;===============================================================================
DrawGrid subroutine
		;grid color
		lda GradientColorGrid,x
		sta COLUBK
		lda Player0Y
		lsr
		lsr
		sta TempP0
		lda Player1Y
		lsr
		lsr
		sta TempP1
		ldy #MaxRows	; start
		SLEEP #12	; TRICK TO WAIT FOR THE RIGHT TIME
		ldx #$00
		stx GRP0	; fix top right flicking
		stx GRP1
PatternChanged:
		
		;---P0 show/hide
		lda SndP2
		cmp #1
		beq skipP0
		cpy TempP0
		bne hideP0
		ldx #$F0
hideP0:
		stx GRP0
		jmp skipP1
skipP0:
		;---P1 show/hide
		;ldx #$00
		cpy TempP1
		bne hideP1
		ldx #$F0
hideP1:
		stx GRP1
skipP1:
		;enable/disable player
		ldx #SpriteHeight
                
		jmp SkipLine ; TO AVOID BLANK LINE
RowsHeightLoop:
		lda GradientColorGrid,x	; guideline on the grid
                
		sta WSYNC
		sta COLUBK
SkipLine:
		lda PF0_left,y
		sta PF0
		lda PF1_left,y
		sta PF1
		lda PF2_left,y
		sta PF2
		;-------------
		lda PF0_left,y
		asl
		asl
		asl
		asl
		sta PF0
		lda PF1_right,y
		sta PF1
		lda PF2_right,y
		sta PF2
		dex
		bne RowsHeightLoop 	; Branch if Not Equal to 0
                
		dey
		bpl PatternChanged	; NEXT LINE OF THE GRID
RowsEnd:
		sta WSYNC	; NEED TO WAIT FOR THE CURRENT LINE TO COMPLETE
		lda #0
		sta GRP0
		sta GRP1
		sta PF0
		sta PF1
		sta PF2 	; clear playfield
		
		; line delimimter after grid
		lda #$68
		sta COLUBK
                sta WSYNC
		lda #0
		sta PF0
		sta PF1
		sta PF2 	; clear playfield
		sta WSYNC	; add extra line to keep simetry with the top
		sta COLUBK
		sta COLUPF
		lda #0
		sta COLUBK
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
		;reach the second half of the mirrored byte
		lsr
		lsr
		lsr
		lsr
		;and #$0F
		sta TempP1
		rts
;===============================================================================
; DrawText
; --------------
;
;===============================================================================
DrawText subroutine
		cpy #$FF ; will not print anything
		beq SkipText
		ldx #12
doLoop:
		; fill left side (store PF0 2x)
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
SkipText:
		;Continue when a button is pressed
		bit INPT5
		bmi SkiP1Pressed
		jmp ButtonHit
SkiP1Pressed:
		bit INPT4
		bmi SkipRestart
ButtonHit:
		and #$F0
		and GameState
		ora #START_STATE
		sta GameState
		lda #0
		sta Scores
SkipRestart:
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
		
		lda Countdown,y
		sta PF2_left,x+ROW_SHIFT2
		iny
                
		; fill right side
		lda Countdown,y
		sta PF0_left,x+ROW_SHIFT2
                
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
         
		ldy #sfxPING ; trigger a sound effect
		jsr SFX_TRIGGER   
                
		rts
SkipCountStatus:
		;lda GameState
		;and #$F0
		lda #RESET_STATE
		sta GameState
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
; CheckCollision
; --------------
;
;===============================================================================
CheckCollision subroutine                                                                                                                                                                                             
		;check collisions                                                                                                                                                                                     
		; Did the player collide with the wall?                                                                                                                                                               
		cpx #PARP0                                                                                                                                                                                             
		bne TryCollP1                                                                                                                                                                                         
                                                                                                                                                                                                                      
		;players collide against each other?                                                                                                                                                                  
		bit CXPPMM                                                                                                                                                                                            
		bmi CollP0                                                                                                                                                                                            
                                                                                                                                                                                                                      
		bit CXP0FB                                                                                                                                                                                            
		bpl SkipCollP0                                                                                                                                                                                        
		;collision P0 HERE                                                                                                                                                                                    
		jmp CollP1                                                                                                                                                                                            
SkipCollP0:                                                                                                                                                                                                           
		rts                                                                                                                                                                                                   
                                                                                                                                                                                                                      
TryCollP1:                                                                                                                                                                                                            
		;players collide against each other?                                                                                                                                                                  
		bit CXPPMM                                                                                                                                                                                            
		bmi CollP1                                                                                                                                                                                            
                                                                                                                                                                                                                      
		bit CXP1FB                                                                                                                                                                                            
		bpl SkipCollP1                                                                                                                                                                                        
		;collision P0 HERE                                                                                                                                                                                    
		jmp CollP0                                                                                                                                                                                            
SkipCollP1:                                                                                                                                                                                                           
		rts                                                                                                                                                                                                   
                                                                                                                                                                                                                      
CollP0: 	                                                                                                                                                                                                      
		; updating Score                                                                                                                                                                                      
		lda Scores                                                                                                                                                                                            
		and #$0F                                                                                                                                                                                              
		tay                                                                                                                                                                                                   
		iny                                                                                                                                                                                                   
		sty TempP0                                                                                                                                                                                            
		lda Scores                                                                                                                                                                                            
		and #$F0                                                                                                                                                                                              
		ora TempP0                                                                                                                                                                                            
		sta Scores                                                                                                                                                                                            
                                                                                                                                                                                                                      
		cpy #9                                                                                                                                                                                                
		bne SkipP0Inc                                                                                                                                                                                         
                                           
                                           
		lda GameState                                                                                                                                                                                              
		and #$F0                                               
		;flag to reset the turn                                                                                                                                                                               
		ora #P0WINS_STATE                                                                                                                                                                                     
		sta GameState                                                                                                                                                                                         
		rts                                                                                                                                                                                                   
SkipP0Inc: 	                                                                                                                                                                                                      
		lda GameState                                                                                                                                                                                         
		and #$0F                                                                                                                                                                                              
		cmp #P1WINS_STATE                                                                                                                                                                             
		beq DoNothing	                                                                                                                                                                                      
                                                                                                                                                                                                                      
		lda GameState                                                                                                                                                                                              
		and #$F0                                      
		;flag to reset the turn                                                                                                                                                                               
		ora #PAUSE_STATE                                                                                                                                                                                 
		sta GameState                                                                                                                                                                                         
		rts                                                                                                                                                                                                   
CollP1:                                                                                                                                                                                                               
		; updating Score                                                                                                                                                                                      
		lda Scores                                                                                                                                                                                            
		lsr                                                                                                                                                                                                   
		lsr                                                                                                                                                                                                   
		lsr                                                                                                                                                                                                   
		lsr                                                                                                                                                                                                   
                                                                                                                                                                                                                      
		tay; increment                                                                                                                                                                                        
		iny                                                                                                                                                                                                   
		tya                                                                                                                                                                                                   
                                                                                                                                                                                                                      
		asl                                                                                                                                                                                                   
		asl                                                                                                                                                                                                   
		asl                                                                                                                                                                                                   
		asl                                                                                                                                                                                                   
                                                                                                                                                                                                                      
		sta TempP0                                                                                                                                                                                            
		lda Scores                                                                                                                                                                                            
		and #$0F                                                                                                                                                                                              
		ora TempP0                                                                                                                                                                                            
		sta Scores                                                                                                                                                                                            
                                                                                                                                                                                                                      
		;ldy TempP0                                                                                                                                                                                           
		cpy #9                                                                                                                                                                                                
		bne SkipP1Inc                                                                                                                                                                                         
                                                                                                                                                                                                                      
		;flag to reset the turn                                                                                                                                                                               
		lda #P1WINS_STATE                                                                                                                                                                                     
		sta GameState                                                                                                                                                                                         
		rts                                                                                                                                                                                                   
SkipP1Inc:                                                                                                                                                                                                            
		lda GameState                                                                                                                                                                                         
		and #$0F                                                                                                                                                                                              
		cmp #P0WINS_STATE;#RESET_STATE                                                                                                                                                                        
		beq DoNothing                                                                                                                                                                                         
                                  
                                  
		lda GameState                                                                                                                                                                                              
		and #$F0                                      
		;flag to reset the turn                                                                                                                                                                               
		ora #PAUSE_STATE                                                                                                                                             
		sta GameState                                                                                                                                                                                         
DoNothing:                                                                                                                                                                                                            
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
; UpdateGrid
; --------------
;
;===============================================================================
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
		ora BitReprF0l,x
		ora PF0_left,y
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
		lda PF0_left,y		
		ora BitReprF0r,x
		ora PF0_left,y
		sta PF0_left,y
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
		dex
		dex
		dex
		stx Player0Y,y
		rts
SkipMoveUp
		lda CRTP0UP,y;UP?
		bit TempP0
		beq SkipMoveDown
		cpx #76
		bcs SkipMoveDown
		inx
		inx
		inx
		inx
		stx Player0Y,y
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
		dex
		dex
		dex
		stx Player0X,y
		lda Player0Y,y
		rts
SkipMoveLeft
		lda CRTP0RIGHT,y;Right?
		bit TempP0
		beq SkipMoveRight
		cpx #156
		bcs SkipMoveRight
		inx
		inx
		inx
		inx
		stx Player0X,y
		lda Player0Y,y
SkipMoveRight
		rts
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
BitReprF0l	.byte #%00010000,#%00100000,#%01000000,#%10000000
BitReprF0r	.byte #%00000001,#%00000010,#%00000100,#%00001000
BitReprF1	.byte #%10000000,#%01000000,#%00100000,#%00010000,#%00001000,#%00000100,#%00000010,#%00000001
BitReprF2	.byte #%00000001,#%00000010,#%00000100,#%00001000,#%00010000,#%00100000,#%01000000,#%10000000
; Bitmap pattern for digits
DigitsBitmap
		.byte $EE,$AA,$AA,$AA,$EE;0
		.byte $22,$22,$22,$22,$22;1
		.byte $EE,$28,$EE,$82,$EE;2
		.byte $EE,$82,$C6,$82,$EE;3
		.byte $82,$82,$EE,$AA,$AA;4
		.byte $EE,$82,$EE,$28,$EE;5
		.byte $EE,$AA,$EE,$28,$EE;6
		.byte $82,$82,$82,$82,$EE;7
		.byte $EE,$AA,$EE,$AA,$EE;8
		.byte $EE,$82,$EE,$AA,$EE;9

ScoreboardColorBK
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
		.byte #$00,#$00,#$00,#$80,#$00
		.byte #$0B,#$00,#$DE,#$E0,#$00
		.byte #$02,#$00,#$42,#$80,#$00
		.byte #$03,#$00,#$DA,#$80,#$00
		.byte #$00,#$00,#$52,#$80,#$00
		.byte #$03,#$00,#$DE,#$80,#$00
		;READY
		.byte #$00,#$00,#$00,#$02,#$00
		.byte #$0D,#$03,#$DD,#$AA,#$00
		.byte #$05,#$02,#$45,#$AA,#$00
		.byte #$05,#$03,#$CC,#$BA,#$00
		.byte #$05,#$02,#$45,#$90,#$00
		.byte #$0D,#$02,#$5D,#$12,#$00
		;TextPlayer0Wins
		;Player 1
		.byte #$0D,#$32,#$4C,#$B1,#$03
		.byte #$05,#$2A,#$54,#$28,#$01
		.byte #$05,#$2A,#$54,#$28,#$01
		.byte #$0D,#$3A,#$DC,#$30,#$01
		.byte #$04,#$22,#$94,#$28,#$01
		.byte #$0C,#$23,#$95,#$A9,#$03
		;Wins
		.byte #$00,#$00,#$00,#$00,#$00
		.byte #$09,#$00,#$51,#$70,#$00
		.byte #$09,#$00,#$51,#$40,#$00
		.byte #$0B,#$00,#$55,#$30,#$00
		.byte #$0D,#$00,#$5B,#$10,#$00
		.byte #$09,#$00,#$51,#$70,#$00
		;TextPlayer1Wins
		;Player 2
		.byte #$0D,#$32,#$4C,#$B1,#$03
		.byte #$05,#$2A,#$54,#$28,#$02
		.byte #$05,#$2A,#$54,#$28,#$02
		.byte #$0D,#$3A,#$DC,#$30,#$01
		.byte #$04,#$22,#$94,#$29,#$00
		.byte #$0C,#$23,#$95,#$A9,#$03
		;Wins
		.byte #$00,#$00,#$00,#$00,#$00
		.byte #$09,#$00,#$51,#$70,#$00
		.byte #$09,#$00,#$51,#$40,#$00
		.byte #$0B,#$00,#$55,#$30,#$00
		.byte #$0D,#$00,#$5B,#$10,#$00
		.byte #$09,#$00,#$51,#$70,#$00

Countdown ;PF2 PF0
		;Number 1
		.byte #$80,#$00
		.byte #$80,#$00
		.byte #$80,#$00
		.byte #$80,#$00
		.byte #$80,#$00
		;Number 2
		.byte #$C0,#$01
		.byte #$00,#$01
		.byte #$80,#$00
		.byte #$40,#$00
		.byte #$C0,#$01
		;Number 3
		.byte #$C0,#$01
		.byte #$00,#$01
		.byte #$80,#$00
		.byte #$00,#$01
		.byte #$C0,#$01

;===============================================================================
; free space check before End of Cartridge
;===============================================================================
;	if (* & $FF)
;	echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"
;		align 256;256
;	endif
; Like player graphics, sound data is stored in reverse order.
; two tables are used, SFX_F and SFX_CV. Values in the tables are used in
; pairs, one from SFX_F and one from SFX_CV. As such, both tables must be the
; same size. Also, the size of each table is limited to just 256 bytes. DASM
; will output a compile-time warning if it spots a size problem.
;
; Each pair of values are used for a single frame (ie: 1/60th of a secon). A
; 0 value in the SFX_CV table means "end of sound effect", though for clarity
; it is recommended to also use a matching 0 in SFX_F.
;
; table SFX_F holds the Frequency for the sound effects.
; each .byte line contains the Frequency data for a single sound effect.
; Frequency values range from 0-31
SFX_F:
 .byte 0, 31 ; collide
 .byte 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3 ; collect
 .byte 0, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8 ; ping
 .byte 0, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31 ; game over
; calculate size of SFX_F table and validate size
SFX_Fcount = *-SFX_F
 if SFX_Fcount > 256
 echo "SFX Warning: table SFX_F is too large"
 endif
; table SFX_CV holds the sound effect Channel (tone) and Volume values.
; Both values range from 0-15, so they are combined together.
; The $ denotes a HEX value where the digits are 0123456789abcdef (a=10, f=15).
; the first digit is the Channel value.
; the second digit is the Volume value.
; each .byte line contains the Channel and Volume data for a single sound effect
; the first value of every .byte line should be 0, which denotes end-of-sfx
; the = line below each .byte line calculates the value used when calling
; sfxtrigger.
; Channel values are:
; 0 = No sound (silent).
; 1 = Buzzy tones.
; 2 = Carries distortion 1 downward into a rumble.
; 3 = Flangy wavering tones, like a UFO.
; 4 = Pure tone.
; 5 = Same as 4.
; 6 = Between pure tone and buzzy tone (Adventure death uses this).
; 7 = Reedy tones, much brighter, down to Enduro car rumble.
; 8 = White noise/explosions/lightning, jet/spacecraft engine.
; 9 = Same as 7.
; a = Same as 6.
; b = Same as 0.
; c = Pure tone, goes much lower in pitch than 4 & 5.
; d = Same as c.
; e = Electronic tones, mostly lows, extends to rumble.
; f = Electronic tones, mostly lows, extends to rumble.
SFX_CV:
 .byte 0,$8f ; collide
sfxCOLLIDE = *-SFX_CV-1
 .byte 0,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f ; collect
sfxCOLLECT = *-SFX_CV-1
 .byte 0,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f ; ping
sfxPING = *-SFX_CV-1
 .byte 0,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf ; game over
sfxGAMEOVER = *-SFX_CV-1
 ; calculate size of SFX_CV table and validate size
SFX_CVcount = *-SFX_CV
 if SFX_CVcount > 256
 echo "SFX Warning: table SFX_CV is too large"
 endif
 if SFX_CVcount != SFX_Fcount
 echo "SFX Warning: table SFX_F is not the same size as table SFX_CV"
 endif
SFX_OFF subroutine
 ldx #0 ; silence sound output
 stx SndP0
 stx SndP1
 stx AUDV0
 stx AUDV1
 stx AUDC0
 stx AUDC1
 rts
SFX_TRIGGER subroutine
 ldx SndP0 ; test left channel
 lda SFX_CV,x ; CV value will be 0 if channel is idle
 bne .leftnotfree ; if not 0 then skip ahead
 sty SndP0 ; channel is idle, use it
 rts ; all done
.leftnotfree:
 ldx SndP1 ; test right channel
 lda SFX_CV,x ; CV value will be 0 if channel is idle
 bne .rightnotfree ; if not 0 then skip ahead
 sty SndP1 ; channel is idle, use it
 rts ; all done
.rightnotfree:
 cpy SndP0 ; test sfx priority with left channel
 bcc .leftnotlower ; skip ahead if new sfx has lower priority than active sfx
 sty SndP0 ; new sfx has higher priority so use left channel
 rts ; all done
.leftnotlower:
 cpy SndP1 ; test sfx with right channel
 bcc .rightnotlower ; skip ahead if new sfx has lower priority than active sfx
 sty SndP1 ; new sfx has higher priority so use right channel
.rightnotlower:
 rts
SFX_UPDATE subroutine
 ldx SndP0 ; get the pointer for the left channel
 lda SFX_F,x ; get the Frequency value
 sta AUDF0 ; update the Frequency register
 lda SFX_CV,x ; get the combined Control and Volume value
 sta AUDV0 ; update the Volume register
 lsr ; prep the Control value,
 lsr ; it's stored in the upper nybble
 lsr ; but must be in the lower nybble
 lsr ; when Control is updated
 sta AUDC0 ; update the Control register
 beq .skipleftdec ; skip ahead if Control = 0
 dec SndP0 ; update pointer for left channel
.skipleftdec:
 ldx SndP1 ; get the pointer for the right channel
 lda SFX_F,x ; get the Frequency value
 sta AUDF1 ; update the Frequency register
 lda SFX_CV,x ; get the combined Control and Volume value
 sta AUDV1 ; update the Volume register
 lsr ; prep the Control value,
 lsr ; it's stored in the upper nybble
 lsr ; but must be in the lower nybble
 lsr ; when Control is updated
 sta AUDC1 ; update the Control register
 beq .skiprightdec ; skip ahead if Control = 0
 dec SndP1 ; update pointer for right channel
.skiprightdec:
 rts ; all done
;===============================================================================
; Define End of Cartridge
;===============================================================================
		ORG $FFFA ; set address to 6507 Interrupt Vectors
		.WORD InitSystem ; NMI
		.WORD InitSystem ; RESET
		.WORD InitSystem ; IRQ