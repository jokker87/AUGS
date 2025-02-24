; SWiSS QUALiTy GROUP
; AUGS
; (C) 2025 JokerX
;
; THX TO DEPECHE/SPREADPOINT FOR OPEN SOURCING YOUR DEMO CODES

	INCDIR	       	"include"
	INCLUDE			"custom.i"

AbsExecBase	equ	4
OldOpenLibrary	equ	-408
CloseLibrary	equ	-414
_LVOOpenFont	EQU	-72
Write		equ	-48
Output		equ	-60
AvailMem	equ	-216
AllocMem	equ	-198
FreeMem		equ	-210
TypeOfMem	equ	-534
WaitTOF		equ	-270
Forbid		equ	-132
Permit		equ	-138
LoadView	equ	-222

custom		equ	$dff000

starfieldwidth	equ	40
starfieldheight	equ	256
starfieldpsize	equ	starfieldwidth*starfieldheight
numboingsteps	equ	14
boingheight	equ	98
boingpsize	equ	starfieldwidth*boingheight

imageWidth equ 40
imageHeight equ 256
imagePSize equ imageWidth*imageHeight
imagePCount equ 4

; profiling
timing		equ	0

; DMACON
SET			equ	1<<15		; 0=clear, 1=set bits that are set to 1 below
BLTPRI		equ	1<<10		; Blitter DMA priority (over CPU) "blitter nasty"
DMAEN		equ	1<<9		; Enable all DMA below
BPLEN		equ	1<<8		; Bit plane DMA
COPEN		equ	1<<7		; Copper DMA
BLTEN		equ	1<<6		; Blitter DMA
SPREN		equ	1<<5		; Sprite DMA

COLOR00 equ $0180
COLOR01 equ $0182
COLOR02 equ $0184
COLOR03 equ $0186
COLOR04 equ $0188
COLOR05 equ $018a
COLOR06 equ $018c
COLOR07 equ $018e
COLOR08 equ $0190
COLOR09 equ $0192
COLOR10 equ $0194
COLOR11 equ $0196
COLOR12 equ $0198
COLOR13 equ $019a
COLOR14 equ $019c
COLOR15 equ $019e
COLOR16 equ $01A0
COLOR17 equ $01A2
COLOR18 equ $01A4
COLOR19 equ $01A6
COLOR20 equ $01A8
COLOR21 equ $01AA
COLOR22 equ $01AC
COLOR23 equ $01AE
COLOR24 equ $01B0
COLOR25 equ $01B2
COLOR26 equ $01B4
COLOR27 equ $01B6
COLOR28 equ $01B8
COLOR29 equ $01BA
COLOR30 equ $01BC
COLOR31 equ $01BE

charMirrorHeightBefore = 212
charMirrorHeightAfter = 44
SpriteWidthPixels = 16


*------	MACROS ----------------------------------------------------------------*

	macro STARTACTOR
	add.l	#1<<\1,v_actors(a5)	
	endm

	macro STOPACTOR
	sub.l	#1<<\1,v_actors(a5)	
	endm


*------	ALLOCATE MEMORY AND SAVE STATE ----------------------------------------*

base	movem.l	a0-a6/d0-d7,-(a7)	
	bsr	alloc			
	bne	.exit			; out of memory error?

	move.l	AbsExecBase.w,a6	
	lea	.gfx(pc),a1		
	jsr	OldOpenLibrary(a6)	; open gfx library
	move.l	d0,a6			;
	beq	.exit			; could not open gfx library
	move 	34(a6),-(a7)		; view
	move.l	d0,-(a7)		; gfx base
	move.l 	38(a6),-(a7)		; copper list 1
	move.l 	50(a6),-(a7)		; copper list 2

	lea         fontreq(pc),a0        ; Zeiger auf TextFont-Struktur
	jsr         _LVOOpenFont(a6)
	move.l      d0,topazPointer

	sub.l	a1,a1			
	jsr	LoadView(a6)		
	jsr	WaitTOF(a6)		
	jsr	WaitTOF(a6)		
	move.l	AbsExecBase.w,a6	
	jsr	Forbid(a6)		
	
	lea	custom,a6		
	bsr	waitblitter		

	move.w	$02(a6),-(a7)		; store DMA control
	move.w	$1c(a6),-(a7)		; store interrupt enable bits
	move.l	$6c.w,-(a7)		; store irq3
	move.w	#$7fff,d0		
	move.w	d0,$9a(a6)		
	move.w	d0,$9c(a6)		; delete all interrupt requests
	move.w	d0,$96(a6)		; disable all DMAs

	clr.w	-(a7)			; store LED state
	btst	#1,$bfe001		
	beq	.ledstate		
	not.w	(a7)			
.ledstate
	bset	#1,$bfe001		; LED dark

*------	INIT ------------------------------------------------------------------*

	lea	vars(pc),a5		

	move.l	v_starfield(a5),v_starfieldp(a5) 

	lea	playcmds(pc),a0		
	move.l	a0,v_cmdspointer(a5)	
	STARTACTOR actor_player		

	move.l	b_boing(pc),v_boingp(a5) 
	
	move.w	#2,v_coladdsub(a5)	; start with fading in
	move.w	#-2,v_coladdsub2(a5)	; start with fading in
	move.w	#fcolorpend-fcolorp,v_colstop(a5) 
	move.w	#24,v_colstop2(a5) 
	move.w	#24,v_colindex2(a5)

	move.w #1,v_boing_direction(a5)
	move.w #25,v_boing_sin_table_index(a5)
	move.w	#0,v_boingRotate(a5)
	move.w	#1,v_boingRotateDirection(a5)
	move.w	#0,v_boing_bounce(a5)
	move.w #$ff+$10,v_augs_logo_pos(a5)

	move.w #(40/4)-1,d7
	move.l  b_dummyrow(pc),a0
.loop
	move.l #$00000000,(a0)+
	dbf d7,.loop

	STARTACTOR actor_starfield		; start with starfield

	move.l b_dummyrow(pc),a0
	
	bsr.w	LSP_MusicInit		

	lea	irq3(pc),a0		
	move.l	a0,$6c.w		

	move.l	b_clist2(pc),a0		; init clist
	move.l	a0,$80(a6)		

	lea	LSP_State+m_dmaconPatch(pc),a1 
	add.w	#lspdmacon2-clist2+3,a0	
	move.l	a0,(a1)			

	bsr	waitraster		; avoid flickering (?)
	move.w	#SET+DMAEN+BPLEN+BLTEN+COPEN+SPREN,$96(a6) 

	move.w	#$c030,$9a(a6)		; enable coper and vertb interrupts

*------	IDLE LOOP -------------------------------------------------------------*

	bsr	precalc			
	
*------	RESTORE STATE AND EXIT ------------------------------------------------*

	bsr	waitblitter		
	
	tst.w	(a7)+			; restore state
	bne	.leddark		
	bclr	#1,$bfe001		; LED bright
.leddark
	move.w	#$7fff,d0		
	move.w	d0,$9a(a6)		
	move.w	d0,$9c(a6)		
	move.w	d0,$96(a6)		
	
	moveq	#0,d0			; volume to zero
	move.w	d0,$a8(a6)		
	move.w	d0,$b8(a6)		
	move.w	d0,$c8(a6)		
	move.w	d0,$d8(a6)		
	
	move.l	(a7)+,$6c.w		
	move.w	(a7)+,d0		
	or.w	#$c000,d0		
	move.w	d0,$9a(a6)		
	move.w	(a7)+,d0		
	or.w	#$8000,d0		
	move.w	d0,$96(a6)		

	move.l	(a7)+,$84(a6)		; copper list 2
	move.l	(a7)+,$80(a6)		; copper list 1
	move.l	(a7)+,a6		; gfx base
	move.l	(a7)+,a1		; view
	jsr	LoadView(a6)		
	jsr	WaitTOF(a6)		
	jsr	WaitTOF(a6)		
	move.l	a6,a1			; parameter for CloseLibrary
	move.l	AbsExecBase.w,a6	
	jsr	CloseLibrary(a6)	; close gfx library
	jsr	Permit(a6)		

	bsr	dealloc			
.exit	movem.l	(a7)+,a0-a6/d0-d7	
	moveq	#0,d0			
	rts				

.gfx	dc.b	"graphics.library",0
	even

fontreq:
		dc.l  fontname
		dc.w    8                      ; Größe (Topaz 8)
		dc.b    0                      ; style
		dc.b    0                      ; Flags
 
fontname:
		 dc.b    "topaz.font",0         ; Name des Fonts
		 even

*------	VARS ------------------------------------------------------------------*

; # = do not change order

	rsreset
v_doquit	rs.b	1	; signal quit
v_wait		rs.b	1

v_actors	rs.l	1

v_cmdspointer	rs.l	1

v_starfieldp	rs.l	1	; pointer to starfield data
v_boingp	rs.l	1
v_colindex	rs.w	1
v_colindex2	rs.w	1
v_colstop	rs.w	1
v_colstop2	rs.w	1
v_coladdsub	rs.w	1
v_coladdsub2	rs.w	1
v_starfield	rs.l	1

v_boing_direction rs.w 1
v_boing_sin_table_index rs.w 1

v_boingRotate rs.w 1
v_boingRotateDirection rs.w 1
v_boing_bounce rs.w 1

v_augslogo_sin_table_index rs.w 1

v_augs_color_index rs.w 1
v_augs_color rs.w 1
v_augs_logo_pos rs.w 1

sizeofvars	rs.w	0



vars	ds.b	sizeofvars
	even


*------	WAIT RASTER -----------------------------------------------------------*

waitraster
.wait	move.l	$04(a6),d0			; h0ffman's variant
	and.l	#$0001ff00,d0			
	cmp.l	#$30<<8,d0			
	bne	.wait				
	rts					

*------	PRECALCULATION --------------------------------------------------------*

precalc
.idle	
	bsr	handleprepareempty
	bsr	handlepreparecowee
	
	tst.b	v_doquit(a5)			
	beq	.idle				
	rts					


*------	COPER -----------------------------------------------------------------*

coper	moveq	#$0010,d0		; delete coper request bit
	move.w	d0,$9c(a6)			; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)			
	move.w	d0,$9c(a6)			

	if timing
	move.w	#$0f00,$180(a6)			
	endif

	bsr	LSP_MusicPlayTick		

	movem.l	(a7)+,a0-a6/d0-d7		

	if timing
	move.w	#$0000,$180(a6)			
	endif

	rte					


*------	IRQ3 ------------------------------------------------------------------*

irq3
	movem.l	a0-a6/d0-d7,-(a7)		
	lea	vars(pc),a5			
	lea	custom,a6			
	move.l #$000049e0,d1
	move.w	$1e(a6),d0			; read interrupt request bits
	btst	#4,d0				
	bne	coper				

	moveq	#$0030,d0			; delete vertb and coper request bit
	move.w	d0,$9c(a6)			; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)			
	move.w	d0,$9c(a6)			
		
	move	#$2200,sr			; allow other (coper) level 3 interrupts

	move.l	v_actors(a5),d7			; process actors
	btst	#actor_player,d7		
	beq	.noplay				
	bsr	play				
.noplay	move.l	v_actors(a5),d7			; process actors
	btst	#actor_starfield,d7			
	beq	.actImage				
	bsr	starfieldRoutine			
	bra	.done				

.actImage move.l	v_actors(a5),d7			; process actors
	btst	#actor_cowee,d7		
	beq	.done
	bsr coweeRoutine				

.done

	btst	#6,$bfe001			; left mouse button pressed?
	seq	v_doquit(a5)			; thanks MnemoTroN ;-)

	if timing
	move.w	#$0020,$180(a6)			; dark green color indicates free capacity
	endif

	movem.l	(a7)+,a0-a6/d0-d7		
	rte					



sintable:
	dc.b 2*$f,2*$10,2*$11,2*$12,2*$13,2*$14,2*$15,2*$16
	dc.b 2*$17,2*$17,2*$18,2*$19,2*$1a,2*$1a,2*$1b,2*$1b
	dc.b 2*$1c,2*$1c,2*$1d,2*$1d,2*$1d,2*$1e,2*$1e,2*$1e
	dc.b 2*$1e,2*$1e,2*$1e,2*$1e,2*$1d,2*$1d,2*$1d,2*$1c
	dc.b 2*$1c,2*$1b,2*$1b,2*$1a,2*$1a,2*$19,2*$18,2*$17
	dc.b 2*$17,2*$16,2*$15,2*$14,2*$13,2*$12,2*$11,2*$10
	dc.b 2*$f,2*$e,2*$d,2*$c,2*$b,2*$a,2*$9,2*$8
	dc.b 2*$8,2*$7,2*$6,2*$5,2*$4,2*$4,2*$3,2*$3
	dc.b 2*$2,2*$2,2*$1,2*$1,2*$1,2*$0,2*$0,2*$0
	dc.b 2*$0,2*$0,2*$0,2*$0,2*$1,2*$1,2*$1,2*$2
	dc.b 2*$2,2*$3,2*$3,2*$4,2*$4,2*$5,2*$6,2*$7
	dc.b 2*$7,2*$8,2*$9,2*$a,2*$b,2*$c,2*$d,2*$e
sintableEnd:
sintableSize = sintableEnd-sintable
	even

*------	STARFIELD    ------------------------------------------------------*

starfieldRoutine

	move.l	b_clist2(pc),a1			
	move.l  b_dummyrow(pc),d1
	move.w  d1,dummyBitplane-clist2+2+4(a1)
	move.w  d1,dummyBitplane2-clist2+2+4(a1)
	swap	d1
	move.w  d1,dummyBitplane-clist2+2(a1)
	move.w  d1,dummyBitplane2-clist2+2(a1)

	sub.w   d1,d1
	move.b	boing_pos_begin-clist2(a1),d1


	move.w  v_boing_sin_table_index(a5),d2
	lea	sintable(pc),a2
	add.w	d2,a2
	move.b  (a2),d1

	add		#$38,d1

	move.w	v_boing_bounce(a5),d3
	beq		.noBounce
	addq	#2,d2
.noBounce
	cmp		#sintableSize,d2
	blt		.noSinReset
	moveq	#0,d2
	.noSinReset:
	move.w	d2,v_boing_sin_table_index(a5)
	

	move.b	d1,boing_pos_begin-clist2(a1)
	add.b   #boingheight,d1
	move.b  d1,boing_pos_end-clist2(a1)

	move.l  a1,a3
	add.l	#augs_logo_pointer-clist2,a3
	move.l	b_augslogo(pc),d1		
	move.w	d1,6(a3)	; 1st plane (bitplane 0)
	swap	d1
	move.w	d1,2(a3)
	swap	d1
	move v_augs_color(a5),d1
	move.w	d1,10(a3)

	move.l	v_boingp(a5),d0			; boingball
	move.l	b_boing(pc),a0			
	move.w  v_boingRotate(a5),d1

	beq .isNotRotating2
	move.w  v_boingRotateDirection(a5),d1
	beq .right
	add.l	#boingpsize,d0			; next frame
	bra .isNotRotating2
.right
	sub.l	#boingpsize,d0
.isNotRotating2

	cmp.l d0,a0
	ble .nolowreset
	move.l	b_boing(pc),d0		; reset boing animation
	add.l	#(numboingsteps-1)*boingpsize,d0
	bra .nohighreset
.nolowreset
	add.l	#numboingsteps*boingpsize,a0	
	cmp.l d0,a0
	bgt .nohighreset
	move.l	b_boing(pc),d0		; reset boing animation
.nohighreset



	move.w	d0,bplstarfield-clist2+2+4(a1)	; 1st plane (bitplane 0)
	swap	d0				
	move.w	d0,bplstarfield-clist2+2(a1)	
	swap	d0				
	move.l	d0,d1				
	add.l	#numboingsteps*boingpsize,d1	; 2nd plane (bitplane 1)
	move.w	d1,bplstarfield-clist2+2+8+4(a1)	
	swap	d1				
	move.w	d1,bplstarfield-clist2+8+2(a1)	
	
	move.l	d0,v_boingp(a5)			

	bsr fading				
	bsr rotate
	bsr direction
	bsr bounce
	bsr starfieldplayer
	bsr augsColorChange

	rts					


*------	COLOR FADING ----------------------------------------------------------*

fcolorp
       dc.w	$0000,$0111,$0122,$0222,$0233,$0234,$0344,$0355
	dc.w	$0456,$0466,$0467,$0578,$0588,$0589,$069a,$06aa
	dc.w	$07ab,$07bb,$07bc,$08cd,$08dd,$09de,$09ef,$0fa9
fcolorpend

fcolort
       dc.w	$0000,$0111,$0111,$0122,$0222,$0233,$0234,$0344
	dc.w	$0345,$0455,$0456,$0466,$0567,$0577,$0578,$0688
	dc.w	$0689,$0699,$079a,$079a,$07ab,$08ab,$08bc,$0ca7

fcolorsa
	dc.w	$0000,$0011,$0011,$0111,$0112,$0122,$0122,$0123
	dc.w	$0123,$0133,$0133,$0134,$0144,$0144,$0145,$0145
	dc.w	$0155,$0156,$0156,$0156,$0166,$0167,$0167,$0767

fcolorb1
	dc.w	$0000,$0111,$0111,$0222,$0222,$0333,$0444,$0444
	dc.w	$0555,$0555,$0666,$0666,$0777,$0777,$0888,$0888
	dc.w	$0999,$0aaa,$0aaa,$0bbb,$0bbb,$0ccc,$0ddd,$0fff

fcolorb2
	dc.w	$0000,$0111,$0111,$0222,$0222,$0333,$0333,$0444
	dc.w	$0444,$0555,$0555,$0666,$0666,$0777,$0777,$0888
	dc.w	$0888,$0999,$0999,$0aaa,$0aaa,$0aaa,$0aaa,$0aaa

fcolorb3
	dc.w	$0000,$0000,$0100,$0100,$0200,$0300,$0300,$0400
	dc.w	$0400,$0500,$0600,$0600,$0700,$0700,$0800,$0900
	dc.w	$0900,$0a00,$0a00,$0b00,$0c00,$0d00,$0e00,$0f00


fading
       move.l	v_actors(a5),d7			; process actors
	btst	#actor_starfield_color,d7		
	beq	.no				
	move.l	b_clist2(pc),a1			
	move.w	v_colindex(a5),d1		

	lea	fcolorp(pc),a0			
	move.w	(a0,d1.w),d0			
	move.w	d0,cp1-clist2+2(a1)		
	move.w	d0,cp2-clist2+2(a1)		
	move.w	d0,cp3-clist2+2(a1)		
	move.w	d0,cp4-clist2+2(a1)		
	move.w	d0,cp5-clist2+2(a1)		
	move.w	d0,cp6-clist2+2(a1)		
	move.w	d0,cp7-clist2+2(a1)		
	move.w	d0,cp8-clist2+2(a1)		
	move.w	d0,csd1-clist2+2(a1)		

	lea	fcolort(pc),a0			
	move.w	(a0,d1.w),d0			
	move.w	d0,csc1-clist2+2(a1)		

	lea	fcolorsa(pc),a0			
	move.w	(a0,d1.w),d0			
	move.w	d0,csa1-clist2+2(a1)		
	move.w	d0,csa2-clist2+2(a1)		

	lea	fcolorb1(pc),a0			
	move.w	(a0,d1.w),d0			
	move.w	d0,cb1a-clist2+2(a1)		
	move.w	d0,cb1b-clist2+2(a1)		

	lea	fcolorb2(pc),a0			
	move.w	(a0,d1.w),d0			
	move.w	d0,cb2a-clist2+2(a1)		
	move.w	d0,cb2b-clist2+2(a1)		
	move.w	d0,csb1-clist2+2(a1)		
	move.w	d0,csb2-clist2+2(a1)		

	lea	fcolorb3(pc),a0			
	move.w	(a0,d1.w),d0			
	move.w	d0,cb3a-clist2+2(a1)		
	move.w	d0,cb3b-clist2+2(a1)		

	move.w	v_coladdsub(a5),d0		
	add.w	d0,v_colindex(a5)		
	move.w	v_colstop(a5),d0		
	cmp.w	v_colindex(a5),d0		
	bne	.notfinished			
	STOPACTOR actor_starfield_color		
	subq.w	#2,v_colindex(a5)		
	move.w	#-2,v_colstop(a5)		; prepare fade out
	neg.w	v_coladdsub(a5)			
.notfinished	
.no	rts					

rotate	move.l	v_actors(a5),d7			; process actors
	btst	#actor_rotate,d7		
	beq	.no
	move.w v_boingRotate(a5),d0			
	beq .turnon
	move.w #0,v_boingRotate(a5)
	bra .finished
.turnon
	move.w #1,v_boingRotate(a5)
.finished	
	STOPACTOR actor_rotate		
.no rts


direction	move.l	v_actors(a5),d7			; process actors
	btst	#actor_rotate_direction,d7		
	beq	.no
	move.w v_boingRotateDirection(a5),d0			
	beq .turnon
	move.w #0,v_boingRotateDirection(a5)
	bra .finished
.turnon
	move.w #1,v_boingRotateDirection(a5)
.finished	
	STOPACTOR actor_rotate_direction		

.no	rts

augsColorChange:
	move.l	v_actors(a5),d7			; process actors
	btst	#actor_augs_color,d7		
	beq	.no
	clr.l d1
	move.w	v_augs_color_index(a5),d1		
	cmp #2*7,d1
	blt .noreset
	move.w #0,v_augs_color_index(a5)
	moveq #0,d1
.noreset



	lea	augs_color_change(pc),a0			
	move.w	(a0,d1.w),d0			
	move.w d0,v_augs_color(a5)

	addq #2,v_augs_color_index(a5)
	STOPACTOR actor_augs_color
.no rts


augs_color_change:
	dc.w	$0f00,$0f80,$0ff0,$08f0,$00f0,$008f,$000f

bounce	move.l	v_actors(a5),d7			; process actors
	btst	#actor_bounce,d7		
	beq	.no
	move.w v_boing_bounce(a5),d0			
	beq .turnon
	move.w #0,v_boing_bounce(a5)
	bra .finished
.turnon
	move.w #1,v_boing_bounce(a5)
.finished	
	STOPACTOR actor_bounce
.no rts


*------	Cowee screen -------------------------------------------------------------*

coweeRoutine:

	bsr.w printChar
	bsr.w scroll
	bsr.w printBubbleText
	bsr.w bubbleBounce
	bsr.w scrollBounce
	bsr.w matrixScreenLine
	rts			

*------	PRINT BIG CHAR ON SPRITE -------------------------------------------------------------*

printChar:
	lea .frameCounter(pc),a0
	lea scrolltextPointer(pc),a1

	move (a0),d0
	bne .nochar
	move.w #25,(a0)

	move.l (a1),a3
	
	lea scrolltextEnd(pc),a2

	cmp.l a2,a3
	bne .noreset
	lea scrolltext(pc),a4
	move.l a4,(a1)
	move.l (a1),a3

.noreset
	
	lea    asciiLookupTable(pc),a2	
	move.w #275,d3
	
	
	
	clr.l d0
	clr.l d1
	clr.l d2
	move.b (a3)+,d0
	sub.b #$20,d0
	asl #1,d0
	add.l d0,a2
	move.b (a2)+,d1
	move.b (a2),d2


	bsr BigFontBlitterCopy

	
	add.l #1,(a1)
.nochar
	subq #1,(a0)
	rts
 
 
.frameCounter dc.w 0
 

*------	SCROLL BIG CHARS -------------------------------------------------------------*
 
bytesPerSpriteLine=2
scrollHeight=(12*25)
scroll:
		bsr waitblitter
		moveq	#0,d0
 
		move.w	#0,bltamod(a6)			;modulo A (src)
		move.w	#0,bltdmod(a6)	;modulo D (dst)
		move.l	#$09f00000,bltcon0(a6)		;D=A
		subq.l	#1,d0
		move.l	d0,bltafwm(a6)			;mask = $ffffffff
 
		
		move.l	b_sprite1(pc),a0
		addq   #4+4,a0
		move.l	a0,bltapt(a6)		;src A
		move.l b_sprite1(pc),a1
		addq #4,a1
		move.l a1,bltdpt(a6)
 
		move.w	#scrollHeight*bytesPerSpriteLine*64+1,bltsize(a6)	;start
 
 
 
		bsr waitblitter
		moveq	#0,d0
 
		move.w	#0,bltamod(a6)			;modulo A (src)
		move.w	#0,bltdmod(a6)	;modulo D (dst)
		move.l	#$09f00000,bltcon0(a6)		;D=A
		subq.l	#1,d0
		move.l	d0,bltafwm(a6)			;mask = $ffffffff
 
		
		move.l	b_sprite2(pc),a0
		addq   #4+4,a0
		move.l	a0,bltapt(a6)		;src A
		move.l b_sprite2(pc),a1
		addq #4,a1
		move.l a1,bltdpt(a6)
 
		move.w	#scrollHeight*bytesPerSpriteLine*64+1,bltsize(a6)	;start
 
 
 
		bsr waitblitter
		moveq	#0,d0
 
		move.w	#0,bltamod(a6)			;modulo A (src)
		move.w	#0,bltdmod(a6)	;modulo D (dst)
		move.l	#$09f00000,bltcon0(a6)		;D=A
		subq.l	#1,d0
		move.l	d0,bltafwm(a6)			;mask = $ffffffff
 
		
		move.l	b_sprite3(pc),a0
		addq   #4+4,a0
		move.l	a0,bltapt(a6)		;src A
		move.l b_sprite3(pc),a1
		addq #4,a1
		move.l a1,bltdpt(a6)
 
		move.w	#scrollHeight*bytesPerSpriteLine*64+1,bltsize(a6)	;start
 
 
 
		bsr waitblitter
		moveq	#0,d0
 
		move.w	#0,bltamod(a6)			;modulo A (src)
		move.w	#0,bltdmod(a6)	;modulo D (dst)
		move.l	#$09f00000,bltcon0(a6)		;D=A
		subq.l	#1,d0
		move.l	d0,bltafwm(a6)			;mask = $ffffffff
 
		
		move.l	b_sprite4(pc),a0
		addq   #4+4,a0
		move.l	a0,bltapt(a6)		;src A
		move.l b_sprite4(pc),a1
		addq #4,a1
		move.l a1,bltdpt(a6)
 
		move.w	#scrollHeight*bytesPerSpriteLine*64+1,bltsize(a6)	;start
 
 
 
 
		bsr waitblitter
		move.l b_sprite1(pc),a0
		rts

waitText      dc.w        0
topazPointer  DC.L        0

*------	TEXT SCROLLER BOUNCE -------------------------------------------------------------*

scrollBounce
	move.l	v_actors(a5),d7			; process actors
	btst	#actor_scroll_bounce,d7	
	beq	.no

	clr.l d0
	clr.w d2
	move.w (scrollBounceIndex),d0

	lea scrollerBounce(pc),a0
	add.l d0,a0
	move.b #spriteScrollRightStart,d1
	move.b (a0),d2
	move.w d2,d1

	bsr.w spritePositionSetter


	addq #1,(scrollBounceIndex)
	cmp #scrollBounceValueCount,(scrollBounceIndex)
	blt .noreset
	move.w #0,(scrollBounceIndex)
	STOPACTOR actor_scroll_bounce
.noreset
.no	rts

*------	SPEECHBUBBLE BOUNCE -------------------------------------------------------------*

bubbleBounce
	move.l	v_actors(a5),d7			; process actors
	btst	#actor_bubble_bounce,d7	
	beq	.no

	clr.l d0
	move.w (speechBounceIndex),d0

	lea speechBounceX1(pc),a0
	move.l b_sprite5(pc),a1
	move.l b_sprite7(pc),a2
	add.l d0,a0
	move.b #speechSpriteOrigYpos,d1
	move.b (a0),d2
	add.b d2,d1
	move.b d1,0(a1)
	move.b d1,0(a2)
	add.b #spriteSpeechbubbleHeight,d1
	move.b d1,2(a1)
	move.b d1,2(a2)


	lea speechBounceX2(pc),a0
	move.l b_sprite6(pc),a1
	move.l b_sprite8(pc),a2
	add.l d0,a0
	move.b #speechSpriteOrigYpos,d1
	move.b (a0),d2
	add.b d2,d1
	move.b d1,0(a1)
	move.b d1,0(a2)
	add.b #spriteSpeechbubbleHeight,d1
	move.b d1,2(a1)
	move.b d1,2(a2)

	addq #1,(speechBounceIndex)
	cmp #speechBounceXValueCount,(speechBounceIndex)
	blt .noreset
	move.w #0,(speechBounceIndex)
	STOPACTOR actor_bubble_bounce
.noreset
.no	rts

*------	PRINT TEXT TO SPEECHBUBBLE -------------------------------------------------------------*

printBubbleText
		move.w	(waitText),d0		
		cmp.w #0,d0
		bgt .noreset
 
		move.w #4,d0
 
		lea bubbletextPointer(pc),a1
		move.l (a1),a2

		lea speechbubbleTextEnd-1(pc),a3
		cmp.l a2,a3
		bne .noreset2
		lea speechbubbleText(pc),a2

.noreset2
              clr d1
		move.b (a2)+,d1
		cmp.b #$24,d1                             ; is it a '$' sign?
		bne .normalChar
		add.w #96,d0
		move.b (a2)+,d1
 
.normalChar
		move.l a2,(a1)
		moveq #3,d5
 
		move.w (bubbletextLastX),d3
		
		cmp #speechbubbleLineCharWidth,d3
		blt .noXreset
 
		moveq #0,d3
		addq #1,(bubbletextLastY)
.noXreset
		
		move.w (bubbletextLastY),d7
		cmp #7,d7
		blt .noYreset
 
		bsr resetBitmap
		moveq #0,d7
		move.w #0,(bubbletextLastY)
 
.noYreset
 
		asl.w #3,d7
		addq #3,d7
		moveq #0,d6
 
		; d5 = shift right
		; d7 = pos Y
		; d3 = pos x (grid)
		; d1 = ascii char
		bsr printAsciiToBitmap
 
 
		subq #1,d5
		addq #1,d7
		bsr printAsciiToBitmapBlack
 
		bsr copyBitmapToAllSprites
 
		addq #1,d3
		move.w d3,(bubbletextLastX)
 
		
 
.noreset
		sub.w #1,d0
		move.w d0,(waitText)
 
		rts

*------	RESET SPEECHBUBBLE BITMAP (WITH EMPTY ONE) -------------------------------------------------------------*

resetBitmap:
	movem.l d0-d1,-(sp)
	bsr waitblitter
	moveq	#0,d0

	move.w	#0,bltamod(a6)			;modulo A (src)
	move.w	#0,bltdmod(a6)	;modulo D (dst)
	move.l	#$09f00000,bltcon0(a6)		;D=A
	subq.l	#1,d0
	move.l	d0,bltafwm(a6)			;mask = $ffffffff

	
	

	move.l	b_speechbubble(pc),a0
	move.l	a0,bltapt(a6)		;src A

	move.l b_speechbubbleCopy(pc),a1
	
	move.l a1,bltdpt(a6)

	move.w	#spriteSpeechbubbleHeight*2*64+4,bltsize(a6)	;start 


	bsr copyBitmapToAllSprites
	movem.l (sp)+,d0-d1
	rts

*------	COPY SPEECHBUBBLE BITMAP TO SPRITES -------------------------------------------------------------*

copyBitmapToAllSprites:
	movem.l d3,-(sp)
	move.l	b_sprite5(pc),a1
	moveq #0,d1
	moveq #(16-2),d2
	moveq #0,d3
	bsr blitter_speechbubble

	move.l	b_sprite5(pc),a1
	moveq #8,d1
	moveq #(16-2),d2
	moveq #1,d3
	bsr blitter_speechbubble

	move.l	b_sprite6(pc),a1
	moveq #2,d1
	moveq #(16-2),d2
	moveq #0,d3
	bsr blitter_speechbubble

	move.l	b_sprite6(pc),a1
	moveq #10,d1
	moveq #(16-2),d2
	moveq #1,d3
	bsr blitter_speechbubble

	move.l	b_sprite7(pc),a1
	moveq #4,d1
	moveq #(16-2),d2
	moveq #0,d3
	bsr blitter_speechbubble

	move.l	b_sprite7(pc),a1
	moveq #12,d1
	moveq #(16-2),d2
	moveq #1,d3
	bsr blitter_speechbubble

	move.l	b_sprite8(pc),a1
	moveq #6,d1
	moveq #(16-2),d2
	moveq #0,d3
	bsr blitter_speechbubble

	move.l	b_sprite8(pc),a1
	moveq #14,d1
	moveq #(16-2),d2
	moveq #1,d3
	bsr blitter_speechbubble

	movem.l (sp)+,d3
	rts


*------	GET CHARLOCATION FROM TEXTFONT POINTER -------------------------------------------------------------*

; a0 - textfont pointer
; d1 - char index cleaned
getCharloc:
	clr.l d2
	move.l 40(a0),d0
	beq .noCharloc
	move.l d0,a1
	asl #2,d1
	add.l d1,a1
	move.w (a1),d2           ; pixel position in bitmap
.noCharloc
	rts

*------	COPY TOPAZ CHAR TO BITMAP -------------------------------------------------------------*

printAsciiToBitmap:
	movem.l d0/d1/d2/d3/d5/d7,-(sp)
	move.l topazPointer,a0
	move.b 32(a0),d0
	sub.b d0,d1

	bsr getCharloc

	; d2 = pixels into bitmap
	clr.l d0

	move.l 34(a0),a1
	move.w 38(a0),d0
	asr #3,d2
	add.l d2,a1

	moveq #8-1,d6
	clr d4

	
	move.l	b_speechbubbleCopy(pc),a2
	move.l a2,a3
	add.l d3,a2

	tst.b d7
	beq .no_yShift
	mulu #(64/8)*2,d7
	add d7,a2

.no_yShift

	
.loop4
	
	move.b (a1),d4

	tst.b d5
	beq .no_xShift

	clr.w d7
	asl #8,d4
	lsr.w d5,d4
	tst.b d4
	beq .noAdjacendByteInvolved
	move.b d4,d7
	or.b 1(a2),d7
	move.b d7,1(a2)

	move.b 1+8(a2),d7
	eor.b d4,d7
	move.b d7,1+8(a2)
	
	
.noAdjacendByteInvolved
	asr #8,d4
.no_xShift
	move.b d4,d7
	move.b (a2),d3
	or.b d3,d4
	move.b d4,(a2)

	move.b d7,d4
	move.b 8(a2),d3
	eor.b d3,d4
	move.b d4,8(a2)

	add.l d0,a1
	add.l #(64/8)*2,a2
	dbf d6,.loop4

	movem.l (sp)+,d0/d1/d2/d3/d5/d7

	rts

*------	COPY TOPAZ CHAR TO BITMAP SHADOW -------------------------------------------------------------*

printAsciiToBitmapBlack:
	movem.l d0/d1/d2/d3/d5/d7,-(sp)
	move.l topazPointer,a0
	move.b 32(a0),d0
	sub.b d0,d1

	bsr getCharloc

	; d2 = pixels into bitmap
	clr.l d0

	move.l 34(a0),a1
	move.w 38(a0),d0
	asr #3,d2
	add.l d2,a1

	moveq #8-1,d6
	clr d4

	
	move.l	b_speechbubbleCopy(pc),a2
	move.l a2,a3
	add.l d3,a2

	tst.b d7
	beq .no_yShift
	mulu #(64/8)*2,d7
	add d7,a2

.no_yShift

	
.loop4
	
	move.b (a1),d4

	tst.b d5
	beq .no_xShift

	clr.w d7
	asl #8,d4
	lsr.w d5,d4
	tst.b d4
	beq .noAdjacendByteInvolved
	move.b d4,d7
	or.b 1(a2),d7
	move.b d7,1(a2)

.noAdjacendByteInvolved
	asr #8,d4

.no_xShift
	move.b (a2),d3
	or.b d3,d4
	move.b d4,(a2)

	add.l d0,a1
	add.l #(64/8)*2,a2
	dbf d6,.loop4

	movem.l (sp)+,d0/d1/d2/d3/d5/d7

	rts


*------	BLIT SPEECHBUBBLE TO SPRITES -------------------------------------------------------------*

; a1=dest sprite
; d1=offset
; d2=modulo
; d3=odd plane
blitter_speechbubble:
	MOVEM.L  d0-d3/a0-a1,-(sp)

	bsr waitblitter
	moveq	#0,d0

	move.w	d2,bltamod(a6)			;modulo A (src)
	move.w	#2,bltdmod(a6)	              ;modulo D (dst)
	move.l	#$09f00000,bltcon0(a6)		;D=A
	subq.l	#1,d0
	move.l	d0,bltafwm(a6)			;mask = $ffffffff

	move.l	b_speechbubbleCopy(pc),a0
	add.l d1,a0
	move.l	a0,bltapt(a6)		;src A
	addq #4,a1
	tst.b d3
	beq .isEvenPlane
	add.l #2,a1
.isEvenPlane:
	
	move.l a1,bltdpt(a6)

	move.w	#spriteSpeechbubbleHeight*1*64+1,bltsize(a6)	;start 
	MOVEM.L  (sp)+,d0-d3/a0-a1
	rts

*------	MATRIX SCREEN "RANDOM" ANIMATION -------------------------------------------------------------*

matrixScreenLine:


	move.l	v_actors(a5),d7			; process actors
	btst	#actor_matrix,d7		
	beq	no
	
	move.w	(waitMatrix),d0			
	cmp.w #0,d0
	bgt done
matrixScreenLineWithoutWait:
	move.w #10,(waitMatrix)

	move.l b_matrixBitmap(pc),a0
	
	move.l #matrixBitmapSize,d0
	
	bsr.w clearmem
	bsr.w waitblitter

	moveq #8-1,d2

.restartNewLine
	moveq #24-1,d6
	bsr PRNG
	lea matrixTempLine1(pc),a1
	lea matrixTempLine2(pc),a2
	clr d4
	clr d5
	clr d7
.loop
	move.b d0,d1
	beq .nextBit
	and.b #1,d1
	beq .1pixelHeight

	; 2 pixel height
	move.b d7,d1
	asl.b #1,d1
	bset d1,d4
	bset d1,d5

	
	bra .nextBit


.1pixelHeight
	move.b d7,d1
	asl.b #1,d1
	bset d1,d5


.nextBit
	lsr.l #1,d0
	addq #1,d7
	cmp #4,d7
	blt .noreset
	move.b d4,(a1)+
	move.b d5,(a2)+
	clr d4
	clr d5
	clr d7
.noreset

	dbf d6,.loop

	lea matrixTempLine1(pc),a1
	lea matrixTempLine2(pc),a2
	add.l #2*16,a0


	

	moveq #6-1,d6
.loop2
	move.b (a1)+,(a0)
	move.b (a2)+,16(a0)
	addq #1,a0

	dbf d6,.loop2

	add.l #8-6+8+16,a0

	

	dbf d2,.restartNewLine

	move.l b_matrixBitmap(pc),a0

	bsr.w copyMatrixBitmapToSprite
done
	subq #1,(waitMatrix)

.notfinished
no	rts		


matrixTempLine1 dcb.b 6
matrixTempLine2 dcb.b 6

*------	COPY MATRIX BITMAP TO SPRITES -------------------------------------------------------------*

copyMatrixBitmapToSprite:
	movem.l d3,-(sp)
	move.l	b_sprite5(pc),a1
	add.l #sprite5matrixOffset,a1

	moveq #0,d1
	moveq #(16-2),d2
	moveq #0,d3
	bsr blitter_matrix

	move.l	b_sprite6(pc),a1
	add.l #sprite6matrixOffset,a1
	moveq #2,d1
	moveq #(16-2),d2
	moveq #0,d3
	bsr blitter_matrix

	
	move.l	b_sprite7(pc),a1
	add.l #sprite7matrixOffset,a1
	moveq #4,d1
	moveq #(16-2),d2
	moveq #0,d3
	bsr blitter_matrix

	movem.l (sp)+,d3
	rts


*------	BLIT MATRIX TO SPRITES -------------------------------------------------------------*

; a1=dest sprite
; d1=offset
; d2=modulo
; d3=odd plane
blitter_matrix:
	MOVEM.L  d0-d3/a0-a1,-(sp)

	bsr waitblitter
	moveq	#0,d0

	move.w	d2,bltamod(a6)			;modulo A (src)
	move.w	#2,bltdmod(a6)	;modulo D (dst)
	move.l	#$09f00000,bltcon0(a6)		;D=A
	subq.l	#1,d0
	move.l	d0,bltafwm(a6)			;mask = $ffffffff

	
	

	move.l	b_matrixBitmap(pc),a0
	add.l d1,a0
	move.l	a0,bltapt(a6)		;src A
	addq #4,a1
	tst.b d3
	beq .isEvenPlane
	add.l #2,a1
.isEvenPlane:
	
	move.l a1,bltdpt(a6)

	move.w	#spriteMatrixScreenHeight*1*64+1,bltsize(a6)	;start 
	MOVEM.L  (sp)+,d0-d3/a0-a1
	rts
	

*------	STARFIELD PLAYER ------------------------------------------------------------*

starfieldplayer
	move.l	v_starfieldp(a5),a0	
	move.l	a0,a2			
	move.l	b_clist2(pc),a1		; sprites clist 2
	add.w	#spritepointersclist2+2-clist2,a1 
	
	
	moveq	#8-1,d2			; 8 sprites
.set	move.l	a2,a3			
	add.w	(a0)+,a3		
	move.l	a3,d1			
	cmp.l #0,(a3)
	bne .notZero
	cmp.w #1,d1
.notZero
	move.w	d1,4(a1)		
	swap	d1			
	move.w	d1,(a1)			
	addq.w	#8,a1			
	dbf	d2,.set			

	add.w	(a0)+,a2		
	cmp.w	#$DEAD,(a2)		; end of data? nice magic word. #TLB1994
	bne	.noreset		
	move.l	v_starfield(a5),a2		
.noreset
	move.l	a2,v_starfieldp(a5)	
	rts				


*------	PLAYER ----------------------------------------------------------------*

play	tst.b	v_wait(a5)		
	beq	.donotwait		
	subq.b	#1,v_wait(a5)		
	rts				

.donotwait
	move.l	v_cmdspointer(a5),a0	
.loop	move.b	(a0)+,d0		; cmd_wait (0)?
	bne	.1			
	move.b	(a0)+,v_wait(a5)	; duration
	move.l	a0,v_cmdspointer(a5)	

	rts				

.1	subq.b	#1,d0			; cmd_start (1)?
	bne	.2			
	moveq	#0,d1			
	move.b	(a0)+,d1		; actor
	move.l	v_actors(a5),d2		
	bset	d1,d2			
	move.l	d2,v_actors(a5)		
	bra	.loop			

.2	subq.b	#1,d0			; cmd_stop (2)?
	bne	.3			
	moveq	#0,d1			
	move.b	(a0)+,d1		; actor
	move.l	v_actors(a5),d2		
	bclr	d1,d2			
	move.l	d2,v_actors(a5)		
	bra	.loop			

.3	subq.b	#1,d0			; cmd_rewind (3)?
	bne	.4			
	lea	playrewind(pc),a0	
	bra	.loop			

.4	bra	.loop

; commands
cmd_wait	equ 	0
cmd_start	equ	1
cmd_stop	equ	2
cmd_rewind	equ	3

; actor bits
actor_player		       equ 8
actor_starfield		equ 9
actor_starfield_color	equ 10
actor_rotate                equ 11
actor_rotate_direction      equ 12
actor_bounce	              equ 13
actor_augs_color            equ 14
actor_cowee                 equ 15
actor_prepare_cowee         equ 16
actor_matrix                equ 17
actor_bubble_bounce         equ 18
actor_scroll_bounce         equ 19
actor_prepare_empty         equ 20

wait_rotate_temp equ (96/2)-1
wait_rotate_temp_half equ (96/4)-1
wait_rotate_temp_double equ (96)-1

playcmds
	
	dc.b	cmd_wait,97
	dc.b	cmd_wait,97
	
	dc.b	cmd_start,actor_starfield_color
	dc.b	cmd_wait,50   
	
	dc.b	cmd_start,actor_augs_color
	dc.b	cmd_wait,46
	dc.b	cmd_wait,100
	dc.b	cmd_wait,180+1
	dc.b	cmd_wait,50+1

	dc.b	cmd_start,actor_bounce
	dc.b	cmd_start,actor_rotate

	rept 3
	dc.b	cmd_start,actor_rotate_direction
	dc.b	cmd_start,actor_augs_color
	dc.b	cmd_wait,wait_rotate_temp
	dc.b	cmd_start,actor_rotate_direction
	dc.b	cmd_start,actor_augs_color
	dc.b	cmd_wait,wait_rotate_temp
	dc.b	cmd_start,actor_rotate_direction
	dc.b	cmd_start,actor_augs_color
	dc.b	cmd_wait,wait_rotate_temp
	dc.b	cmd_start,actor_rotate_direction
	dc.b	cmd_start,actor_augs_color
	endr

	dc.b	cmd_start,actor_rotate_direction
	dc.b	cmd_start,actor_augs_color
	dc.b	cmd_wait,wait_rotate_temp
	dc.b	cmd_start,actor_rotate_direction
	dc.b	cmd_start,actor_augs_color
	dc.b	cmd_wait,wait_rotate_temp

	dc.b	cmd_start,actor_bounce


	dc.b	cmd_wait,wait_rotate_temp_double
	dc.b	cmd_wait,wait_rotate_temp_double
	dc.b	cmd_start,actor_rotate_direction

	rept 2
	dc.b	cmd_wait,wait_rotate_temp_double
	dc.b	cmd_wait,wait_rotate_temp_double
	dc.b	cmd_start,actor_rotate_direction
	endr

	dc.b	cmd_wait,wait_rotate_temp_double
	dc.b	cmd_start,actor_rotate_direction
	dc.b	cmd_wait,wait_rotate_temp
	dc.b	cmd_start,actor_rotate_direction
	dc.b	cmd_wait,wait_rotate_temp
	dc.b	cmd_start,actor_rotate_direction
	dc.b	cmd_wait,wait_rotate_temp

	rept 16
	dc.b	cmd_start,actor_augs_color
	dc.b	cmd_wait,wait_rotate_temp_half
	endr

	dc.b	cmd_wait,50
	
	dc.b	cmd_wait,100

	dc.b	cmd_start,actor_starfield_color  ; here could be colorfade for cowee

	dc.b	cmd_stop,actor_starfield
	dc.b	cmd_start,actor_prepare_empty
	dc.b	cmd_wait,wait_rotate_temp_double
	dc.b	cmd_wait,wait_rotate_temp_double
	dc.b	cmd_start,actor_prepare_cowee
	dc.b	cmd_wait,50
	dc.b	cmd_start,actor_cowee
	dc.b	cmd_wait,200	
	dc.b	cmd_wait,200
	dc.b	cmd_wait,200
	dc.b	cmd_wait,200
	dc.b	cmd_wait,200
	dc.b	cmd_wait,200
	dc.b	cmd_wait,200
	dc.b	cmd_wait,200
	dc.b	cmd_wait,180

	dc.b	cmd_start,actor_matrix
	dc.b	cmd_wait,200
	dc.b	cmd_wait,200
	dc.b	cmd_wait,200

	rept 8
	dc.b	cmd_start,actor_bubble_bounce
	dc.b	cmd_start,actor_scroll_bounce
	dc.b	cmd_wait,wait_rotate_temp_double
	endr


	dc.b	cmd_rewind

playrewind
	dc.b	cmd_wait,1
	dc.b	cmd_rewind
	even



clistEmpty
	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0102,$0000
	dc.w	$0104,$0000
	dc.w	$0108,$0000
	dc.w	$010a,$0000

spritepointersclistEmpty
	dc.w	$013c,0,$013e,0,$0138,0,$013a,0,$0134,0,$0136,0
	dc.w	$0130,0,$0132,0,$012c,0,$012e,0,$0128,0,$012a,0
	dc.w	$0124,0,$0126,0,$0120,0,$0122,0

	; sprite (stars) colors
	dc.w	$01a0,$0000
	dc.w	$01a2,$0000
	dc.w	$01a4,$0000
	dc.w	$01a6,$0000
	dc.w	$01a8,$0000
	dc.w	$01aa,$0000
	dc.w	$01ac,$0000
	dc.w	$01ae,$0000
	dc.w	$01b0,$0000
	dc.w	$01b2,$0000
	dc.w	$01b4,$0000
	dc.w	$01b6,$0000
	dc.w	$01b8,$0000
	dc.w	$01ba,$0000
	dc.w	$01bc,$0000
	dc.w	$01be,$0000

	dc.w	$0108,-starfieldwidth
	dc.w	$010a,-starfieldwidth
	dc.w	$0100,$1200

	dc.w	$00e0,0 ; bitplane 0
	dc.w	$00e2,0
	dc.w	$00e4,0 ; bitplane 1
	dc.w	$00e6,0
	dc.w	$00e8,0 ; bitplane 2
	dc.w	$00ea,0
	dc.w	$00ec,0 ; bitplane 3
	dc.w	$00ee,0

	dc.w	$0180,$0000
	dc.w	$0182,colorp
	dc.w	$0184,$0000
	dc.w	$0186,colorp

	dc.w	$0188,colorb3
	dc.w	$018a,colorp
	dc.w	$018c,$0000
	dc.w	$018e,colorp

	dc.w	$0190,colorb1
	dc.w	$0192,colorp
	dc.w	$0194,$0000 
	dc.w	$0196,colorp

	dc.w	$0198,colorb2
	dc.w	$019a,colorp
	dc.w	$019c,$0000 
	dc.w	$019e,colorp

	dc.b	lspline,$07,$ff,$fe
	dc.w	$009c,$8010 ; trigger coper interrupt
	dc.b	lspline+11,$07,$ff,$fe
lspdmacon4
	dc.b	$00,$96,$80,0
	dc.b	$FF,$FF,$FF,$FE
clistEmptyEnd
clistEmptySize=clistEmptyEnd-clistEmpty

lspline	equ $2d

*------	COPPER INSTRUCTION LIST 2 ---------------------------------------------*

; boing colors
colorb1	equ	$0000 
colorb2	equ	$0000
colorb3	equ	$0000

colort	equ	$0000 
colorp	equ	$0000

clist2
	dc.w	$1007,$fffe ; chance for player to alter clist in time
	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0102,$0000
	dc.w	$0104,$0000

	dc.w	$0108,$0000
	dc.w	$010a,$0000

spritepointersclist2
	dc.w	$013c,0,$013e,0,$0138,0,$013a,0,$0134,0,$0136,0
	dc.w	$0130,0,$0132,0,$012c,0,$012e,0,$0128,0,$012a,0
	dc.w	$0124,0,$0126,0,$0120,0,$0122,0

	; sprite (stars) colors
	dc.w	$01a0,$0000
csa1	dc.w	$01a2,$0000
csb1	dc.w	$01a4,$0000
csc1	dc.w	$01a6,$0000
	dc.w	$01a8,$0000
csa2	dc.w	$01aa,$0000
csb2	dc.w	$01ac,$0000
	dc.w	$01ae,$0000
	dc.w	$01b0,$0000
csd1	dc.w	$01b2,$0000
	dc.w	$01b4,$0000
	dc.w	$01b6,$0000
	dc.w	$01b8,$0000
	dc.w	$01ba,$0000
	dc.w	$01bc,$0000
	dc.w	$01be,$0000

	dc.w	$0108,-starfieldwidth
	dc.w	$010a,-starfieldwidth
	dc.w	$0100,$1200
dummyBitplane:
	dc.w	$00e0,0 ; bitplane 0
	dc.w	$00e2,0

	dc.w	$0180,$0000
cp1	dc.w	$0182,colorp
	dc.w	$0184,$0000
cp2	dc.w	$0186,colorp

cb3a	dc.w	$0188,colorb3
cp3	dc.w	$018a,colorp
	dc.w	$018c,$0000
cp4	dc.w	$018e,colorp

cb1a	dc.w	$0190,colorb1
cp5	dc.w	$0192,colorp
	dc.w	$0194,$0000
cp6	dc.w	$0196,colorp

cb2a	dc.w	$0198,colorb2
cp7	dc.w	$019a,colorp
	dc.w	$019c,$0000
cp8	dc.w	$019e,colorp

	dc.b	lspline,$07,$ff,$fe
	dc.w	$009c,$8010 ; trigger coper interrupt
	dc.b	lspline+11,$07,$ff,$fe
lspdmacon2
	dc.b	$00,$96,$80,0

boing_pos_begin
	dc.w	$3f07,$fffe
bplstarfield
	dc.w	$00e0,0,$00e2,0 ; bitplane 0
	dc.w	$00e4,0,$00e6,0 ; bitplane 1
	dc.w	$0108,0
	dc.w	$010a,0
	dc.w	$0100,$2200

cb3b	dc.w	$0182,colorb3
cb1b	dc.w	$0184,colorb1
cb2b	dc.w	$0186,colorb2


boing_pos_end	
	dc.b	$3f+boingheight,$07,$ff,$fe
	
	dc.w	$0108,-starfieldwidth
	dc.w	$0104,$0044
dummyBitplane2:
	dc.w	$00e0,0,$00e2,0
	dc.w	$0100,$1200

augs_logo_pos:
	dc.b	$d8,$07,$ff,$fe
	
	dc.w	$0108,0
	dc.w	$010a,0
	dc.w	$0104,$0044
augs_logo_pointer:
	dc.w	$00e0,$0000
	dc.w	$00e2,$0000
	dc.W	$0182,$0000
	dc.w	$0100,$1200

	dc.w	$ffff,$fffe
clist2end

screenwidth = 40
              
clistCowee:
	dc.w	$1007,$fffe ; chance for player to alter clist in time
	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0
	dc.w	$0102,$0000
	dc.w	$0104,$003f
	dc.w   $01fc,$0000
	dc.w   $0108,0
	dc.w   $010a,0
	dc.w   $00E0
p_bitplane1: dc.w   $0000
	dc.w   $00E2
	dc.w   $0000
	dc.w   $00E4
p_bitplane2: dc.w   $0000
	dc.w   $00E6
	dc.w   $0000
	dc.w   $00E8
p_bitplane3: dc.w   $0000
	dc.w   $00Ea
	dc.w   $0000
	dc.w   $00Ec
p_bitplane4: dc.w   $0000
	dc.w   $00Ee
	dc.w   $0000
	dc.w   $00f0
p_bitplane5: dc.w   $0000
	dc.w   $00f2
	dc.w   $0000

	dc.b $01,$40,spriteYStart,spriteXStart; SPR0POS
	dc.w $0142,$2c02           ; SPR0CTL
	dc.b $01,$48,spriteYStart,spriteXStart; SPR1POS
	dc.w $014A,$2c82           ; SPR1CTL
	dc.b $01,$50,spriteYStart,spriteXStart; SPR2POS
	dc.w $0152,$2c02           ; SPR2CTL
	dc.b $01,$58,spriteYStart,spriteXStart; SPR3POS
	dc.w $015A,$2c82           ; SPR3CTL

	dc.b $01,$60,spriteYStart,spriteXStart; SPR0POS
	dc.w $0162,$2c02           ; SPR0CTL
	dc.b $01,$68,spriteYStart,spriteXStart; SPR1POS
	dc.w $016A,$2c82           ; SPR1CTL
	dc.b $01,$70,spriteYStart,spriteXStart; SPR2POS
	dc.w $0172,$2c02           ; SPR2CTL
	dc.b $01,$78,spriteYStart,spriteXStart; SPR3POS
	dc.w $017A,$2c82           ; SPR3CTL




	dc.w   $0120
p_sprite1: dc.w      $0000
	dc.w   $0122,$0000

	dc.w   $0124
p_sprite2: dc.w      $0000
	dc.w   $0126,$0000

	dc.w   $0128
p_sprite3: dc.w      $0000
	dc.w   $012a,$0000

	dc.w   $012c
p_sprite4: dc.w      $0000
	dc.w   $012e,$0000
	
	dc.w   $0130
p_sprite5: dc.w      $0000
	dc.w   $0132,$0000

	dc.w   $0134
p_sprite6: dc.w      $0000
	dc.w   $0136,$0000

	dc.w   $0138
p_sprite7: dc.w      $0000
	dc.w   $013a,$0000

	dc.w   $013c
p_sprite8: dc.w      $0000
	dc.w   $013e,$0000

	dc.w   $0096,$87e0
	dc.w   COLOR00,$0000
	dc.w   COLOR01,$0433
	dc.w   COLOR02,$0363
	dc.w   COLOR03,$0426 
	dc.w   COLOR04,$0467
	dc.w   COLOR05,$0556
	dc.w   COLOR06,$0a24
	dc.w   COLOR07,$0e12
	dc.w   COLOR08,$0926
	dc.w   COLOR09,$0e57
	dc.w   COLOR10,$05a6
	dc.w   COLOR11,$0359
	dc.w   COLOR12,$03bc
	dc.w   COLOR13,$0bdc
	dc.w   COLOR14,$0348
	dc.w   COLOR15,$036a
	dc.w   COLOR16,$059a
	dc.w   COLOR17,$0BEF
	dc.w   COLOR18,$09cf
	dc.w   COLOR19,$07ad
	dc.w   COLOR20,$058b
	dc.w   COLOR21,$0369
	dc.w   COLOR22,$0147
	dc.w   COLOR23,$0F00
	dc.w   COLOR24,$0711
	dc.w   COLOR25,$0000
	dc.w   COLOR26,$0fff
	dc.w   COLOR27,$0aaa
	dc.w   COLOR28,$02cb
	dc.w   COLOR29,$0000
	dc.w   COLOR30,$0fff
	dc.w   COLOR31,$0aaa

	dc.b	$12,$07,$ff,$fe
	dc.w	$009c,$8010 ; trigger coper interrupt
	dc.b	$12+11,$07,$ff,$fe
lspdmacon3
	dc.b	$00,$96,$80,0

	dc.w   $0100,$4200
	dc.w   COLOR00,$0349
	dc.w   $2b07,$FFFE
	dc.w   COLOR00,$056c
	dc.w   $2c01,$FFFE
	dc.w   COLOR00,$0000

spritePosAreaBefore:
	dcb.w 9*2*charMirrorHeightBefore+10

	dc.w   $ffdf,$FFFE

spritePosAreaAfter:
	dcb.w 9*2*charMirrorHeightAfter
	dc.w   $2c07,$FFFE
	dc.w   COLOR00,$056c
	dc.w   $2d07,$FFFE
	dc.w   COLOR00,$0349
	dc.w   $FFFF,$FFFE
clistCoweeEnd:
clistCoweeSize equ clistCoweeEnd-clistCowee


*------	MEMORY MANAGEMENT -----------------------------------------------------*

BESTMEMORY	equ	0
MEMF_CHIP	equ	1<<1
MEMF_CLEAR	equ	1<<16

clist2size	equ	clist2end-clist2
lspbanksize	equ	lspbankend-lspbank
boingsize	equ	boingend-boing
imageSize	equ imageEnd-image
augslogoSize equ augslogoend-augslogo
starfieldsize		equ	starfieldend-starfield
matrixBitmapSize equ 8*spriteMatrixScreenHeight*2

memtable
b_clist_cowee dc.l	0,MEMF_CHIP,clistCoweeSize
b_clist_empty dc.l	0,MEMF_CHIP,clistEmptySize
b_clist2	dc.l	0,MEMF_CHIP,clist2size
b_lspbank	dc.l	0,MEMF_CHIP,lspbanksize
b_boing		dc.l	0,MEMF_CHIP,boingsize
b_image		dc.l	0,MEMF_CHIP,imageSize
b_augslogo  dc.l	0,MEMF_CHIP,augslogoSize
b_spritenull:dc.l   0,MEMF_CHIP,spritenullsize
b_sprite1:   dc.l   0,MEMF_CHIP,sprite1size
b_sprite2:   dc.l   0,MEMF_CHIP,sprite2size
b_sprite3:   dc.l   0,MEMF_CHIP,sprite3size
b_sprite4:   dc.l   0,MEMF_CHIP,sprite4size
b_sprite5:   dc.l   0,MEMF_CHIP,sprite5size
b_sprite6:   dc.l   0,MEMF_CHIP,sprite6size
b_sprite7:   dc.l   0,MEMF_CHIP,sprite7size
b_sprite8:   dc.l   0,MEMF_CHIP,sprite8size
b_speechbubble: dc.l   0,MEMF_CHIP,speechbubblesize
b_speechbubbleCopy: dc.l   0,MEMF_CHIP,speechbubbleCopysize
b_font:       dc.l   0,MEMF_CHIP,fontsize
b_starfield   dc.l   0,MEMF_CHIP,starfieldsize
b_matrixBitmap dc.l  0,MEMF_CHIP,matrixBitmapSize

memtable2
b_dummyrow dc.l 0,MEMF_CHIP+MEMF_CLEAR,40
memtableend

entrysize 	equ	12 ; one entry in the memtable is 12 bytes large (3 longwords)
entries		equ	(memtableend-memtable)/entrysize
entrieschip	equ	(memtable2-memtable)/entrysize

*------	ALLOCATE MEMORY -------------------------------------------------------------*

alloc	lea	clist2(pc),a1			
	move.l	AbsExecBase.w,a6		
	jsr	TypeOfMem(a6)			
	btst	#1,d0				; chipmem?
	beq	.notchipmem			

	lea	clistEmpty(pc),a0			
	lea	b_clist_empty(pc),a1			
	move.l	a0,(a1)				

	lea	clistCowee(pc),a0			
	lea	b_clist_cowee(pc),a1			
	move.l	a0,(a1)				

	lea	clist2(pc),a0			
	lea	b_clist2(pc),a1			
	move.l	a0,(a1)				

	lea	base(pc),a0			
	add.l	#lspbank-base,a0		
	lea	b_lspbank(pc),a1		
	move.l	a0,(a1)				

	lea	base(pc),a0			
	add.l	#boing-base,a0			
	lea	b_boing(pc),a1			
	move.l	a0,(a1)				

	lea	base(pc),a0			
	add.l	#image-base,a0			
	lea	b_image(pc),a1			
	move.l	a0,(a1)				

	lea	base(pc),a0			
	add.l	#augslogo-base,a0			
	lea	b_augslogo(pc),a1			
	move.l	a0,(a1)				

	lea spritenull(pc),a0
       lea b_spritenull(pc),a1
       move.l a0,(a1)

       lea sprite1(pc),a0
       lea b_sprite1(pc),a1
       move.l a0,(a1)

       lea sprite2(pc),a0
       lea b_sprite2(pc),a1
       move.l a0,(a1)

       lea sprite3(pc),a0
       lea b_sprite3(pc),a1
       move.l a0,(a1)

       lea sprite4(pc),a0
       lea b_sprite4(pc),a1
       move.l a0,(a1)

       lea sprite5(pc),a0
       lea b_sprite5(pc),a1
       move.l a0,(a1)

       lea sprite6(pc),a0
       lea b_sprite6(pc),a1
       move.l a0,(a1)

       lea sprite7(pc),a0
       lea b_sprite7(pc),a1
       move.l a0,(a1)

       lea sprite8(pc),a0
       lea b_sprite8(pc),a1
       move.l a0,(a1)

       lea speechbubble(pc),a0
       lea b_speechbubble(pc),a1
       move.l a0,(a1)

       lea speechbubbleCopy(pc),a0
       lea b_speechbubbleCopy(pc),a1
       move.l a0,(a1)

       lea font(pc),a0
       lea b_font(pc),a1
       move.l a0,(a1)
	
.notchipmem
	lea	memtable(pc),a5			
	moveq	#entries-1,d7			
.loop	tst.l	(a5)				; not to be allocated?
	bne	.noalloc			
	move.l	8(a5),d0			; bytesize
	move.l	4(a5),d1			; requirements
	move.l	AbsExecBase.w,a6		
	jsr	AllocMem(a6)			
	move.l	d0,(a5)				
	beq	.printerrorandfreemem		; out of memory
.noalloc	
	add.w	#entrysize,a5			; next entry
	dbf	d7,.loop			
	bsr	initmemory			
	moveq	#0,d0				; ok, all entries allocated
	rts					

.printerrorandfreemem
	bsr	printoutofmemory		
dealloc	move.l	AbsExecBase.w,a6		
	jsr	TypeOfMem(a6)			
	lea	memtable(pc),a5			
	moveq	#entries-1,d7			
	btst	#1,d0				; chipmem?
	beq	.loop				; we are not in chipmem so free all entries
	lea	memtable2(pc),a5		
	moveq	#entries-entrieschip-1,d7	
.loop	tst.l	(a5)				; end of memtable?
	beq	.done				
	move.l	(a5),a1				; address of memory block
	move.l	8(a5),d0			; bytesize
	move.l	AbsExecBase.w,a6		
	jsr	FreeMem(a6)			
	add.w	#entrysize,a5			
	dbf	d7,.loop			
.done	moveq	#-1,d0				; alloc error
	rts					

*------	INIT MEMORY -------------------------------------------------------------*

initmemory
	lea	vars(pc),a5			
	
	lea	base(pc),a0			; copy "starfield"
	add.l	#starfield-base,a0			
	move.l	b_starfield(pc),a1			
	move.l	a1,a2				
	move.l	a2,v_starfield(a5)			
	add.l	#starfieldsize,a2			
	
	move.l	#starfieldsize,d7			
.copyc	move.b	(a0)+,(a1)+			
	subq.l	#1,d7				
	bne	.copyc				

	lea	base(pc),a0			; copy lspbank to chip memory
	add.l	#lspbank-base,a0		
	move.l	b_lspbank(pc),a1		
	move.l	#lspbanksize,d0			
.copylspbank
	move.b	(a0)+,(a1)+			
	subq.l	#1,d0				
	bne	.copylspbank			

	lea	clist2(pc),a0			; copy clist 2 to chip memory
	move.l	b_clist2(pc),a1			
	move.w	#clist2size-1,d7		
.copyc2	move.b	(a0)+,(a1)+			
	dbf	d7,.copyc2			

	lea	clistCowee(pc),a0			; copy clist 2 to chip memory
	move.l	b_clist_cowee(pc),a1			
	move.w	#clistCoweeSize-1,d7		
.copyCowee	move.b	(a0)+,(a1)+			
	dbf	d7,.copyCowee

	lea	clistEmpty(pc),a0			; copy clist 2 to chip memory
	move.l	b_clist_empty(pc),a1			
	move.w	#clistEmptySize-1,d7		
.copyEmpty	move.b	(a0)+,(a1)+			
	dbf	d7,.copyEmpty

	lea	base(pc),a0			; copy boing
	add.l	#boing-base,a0			
	move.l	b_boing(pc),a1			
	move.l	#boingsize,d7			
.copyb	move.b	(a0)+,(a1)+			
	subq.l	#1,d7				
	bne	.copyb				

	lea	base(pc),a0			; copy image
	add.l	#image-base,a0			
	move.l	b_image(pc),a1			
	move.l	#imageSize,d7			
.copyi	move.b	(a0)+,(a1)+			
	subq.l	#1,d7				
	bne	.copyi				

	lea	base(pc),a0			; copy augslogo
	add.l	#augslogo-base,a0			
	move.l	b_augslogo(pc),a1			
	move.l	#augslogoSize,d7			
.copyaugslogo	move.b	(a0)+,(a1)+			
	subq.l	#1,d7				
	bne	.copyaugslogo


	lea sprite1(pc),a0
	move.l b_sprite1(pc),a1
	move.w #sprite1size-1,d7
.copysprite1
	move.b (a0)+,(a1)+
	dbf d7,.copysprite1

	lea sprite2(pc),a0
	move.l b_sprite2(pc),a1
	move.w #sprite2size-1,d7
.copysprite2
	move.b (a0)+,(a1)+
	dbf d7,.copysprite2

	lea sprite3(pc),a0
	move.l b_sprite3(pc),a1
	move.w #sprite3size-1,d7
.copysprite3
	move.b (a0)+,(a1)+
	dbf d7,.copysprite3

	lea sprite4(pc),a0
	move.l b_sprite4(pc),a1
	move.w #sprite4size-1,d7
.copysprite4
	move.b (a0)+,(a1)+
	dbf d7,.copysprite4

	lea sprite5(pc),a0
	move.l b_sprite5(pc),a1
	move.w #sprite5size-1,d7
.copysprite5
	move.b (a0)+,(a1)+
	dbf d7,.copysprite5

	lea sprite6(pc),a0
	move.l b_sprite6(pc),a1
	move.w #sprite6size-1,d7
.copysprite6
	move.b (a0)+,(a1)+
	dbf d7,.copysprite6

	lea sprite7(pc),a0
	move.l b_sprite7(pc),a1
	move.w #sprite7size-1,d7
.copysprite7
	move.b (a0)+,(a1)+
	dbf d7,.copysprite7

	lea sprite8(pc),a0
	move.l b_sprite8(pc),a1
	move.w #sprite8size-1,d7
.copysprite8
	move.b (a0)+,(a1)+
	dbf d7,.copysprite8

	lea speechbubble(pc),a0
	move.l b_speechbubble(pc),a1
	move.l b_speechbubbleCopy(pc),a2
	move.w #speechbubblesize-1,d7
.copyspeechbubble
	move.b (a0),(a1)+
	move.b (a0)+,(a2)+
	dbf d7,.copyspeechbubble

	
	lea font(pc),a0
	move.l b_font(pc),a1
	move.w #fontsize-1,d7
.copyfont
	move.b (a0)+,(a1)+
	dbf d7,.copyfont

	rts					

*------	CLEAR MATRIX BITMAP -------------------------------------------------------------*

clearmem:
	bsr     waitblitter        ; Make sure previous blit is done
	move.l  a0,bltdpt(a6)   ; Set up the D pointer to the region to clear
	clr.w   bltdmod(a6)     ; Clear the D modulo (don't skip no bytes)
	asr.l   #1,d0           ; Get number of words from number of bytes
	clr.w   bltcon1(a6)     ; No special modes
	move.w  #$0100,bltcon0(a6)       ; only enable destination

	move.w  #spriteMatrixScreenHeight*2*64+4,bltsize(a6)

	rts                     ; finished.  Blit still in progress.

*------	PRNG -------------------------------------------------------------*

Seed:
        move.l  #$9876fedc,d0
        move.l  #$abcd1234,d1
        move.w  #1337,d2
.loop   swap    d0
        add.l   d1,d0
        add.l   d0,d1
        dbf     d2,.loop
        movem.l d0-d1,prngstate
        rts

PRNG:   
        movem.l prngstate(pc),d0-d1
        swap    d0
        add.l   d1,d0
        add.l   d0,d1
        movem.l d0-d1,prngstate
        rts

prngstate  ds.l    2

*------	EMPTY SCREEN PREPARE -------------------------------------------------------------*

handleprepareempty
	move.l	v_actors(a5),d7			
	btst	#actor_prepare_empty,d7	
	beq	.no				

	move.l	b_clist_empty(pc),a0		; init clist
	move.l	a0,$80(a6)		
	; strobe copper jump
	move.w      #$ffff,$88(a6)

	lea	LSP_State+m_dmaconPatch2(pc),a1 
	add.w	#lspdmacon4-clistEmpty+3,a0	
	move.l	a0,(a1)		


	STOPACTOR actor_prepare_empty	
.no	rts			

*------	COWEE SCREEN PREPARE -------------------------------------------------------------*

handlepreparecowee
	move.l	v_actors(a5),d7			
	btst	#actor_prepare_cowee,d7	
	beq	.no				

	move.w #imagePCount-1,d7
	move.l b_image,d0
	move.l b_clist_cowee(pc),a2
	add #p_bitplane1-clistCowee,a2

.loop4:
	move.w  d0,4(a2)
	swap d0
	move.w d0,(a2)
	swap d0

	add.l #imagePSize,d0
	addq  #8,a2
	dbf d7,.loop4


	move.l b_clist_cowee(pc),a2
	add #p_sprite1-clistCowee,a2

	move.l b_sprite1(pc),d0
	move.w  d0,4(a2)
	swap d0
	move.w d0,(a2)
	
	add.l #8,a2

	move.l b_sprite2(pc),d0
	move.w  d0,4(a2)
	swap d0
	move.w d0,(a2)

	add.l #8,a2

	move.l b_sprite3(pc),d0
	move.w  d0,4(a2)
	swap d0
	move.w d0,(a2)

	add.l #8,a2

	move.l b_sprite4(pc),d0
	move.w  d0,4(a2)
	swap d0
	move.w d0,(a2)

	add.l #8,a2

	move.l b_sprite5(pc),d0
	move.w  d0,4(a2)
	swap d0
	move.w d0,(a2)

	add.l #8,a2

	move.l b_sprite6(pc),d0
	move.w  d0,4(a2)
	swap d0
	move.w d0,(a2)

	add.l #8,a2

	move.l b_sprite7(pc),d0
	move.w  d0,4(a2)
	swap d0
	move.w d0,(a2)

	add.l #8,a2

	move.l b_sprite8(pc),d0
	move.w  d0,4(a2)
	swap d0
	move.w d0,(a2)

	bsr Seed

	lea asciiLookupTable(pc),a0
	move.l a0,a1
	lea	scrolltext(pc),a2			

	moveq #11-1,d6
	moveq #0,d3
.loop2
	move.l a1,a0
	clr.l d0
	clr.l d1
	clr.l d2
	move.b (a2)+,d0
	sub.b #$20,d0
	asl #1,d0
	add.l d0,a0
	move.b (a0)+,d1
	move.b (a0),d2


	bsr BigFontBlitterCopy

	add.w #fontcharheight,d3



	dbf d6,.loop2

	lea scrolltextPointer(pc),a0
	move.l a2,(a0)

	lea speechbubbleText(pc),a2
	lea bubbletextPointer(pc),a0
	move.l a2,(a0)


	moveq #0,d1
	bsr.w spritePositionSetter

	bsr.w matrixScreenLineWithoutWait



	move.l	b_clist_cowee(pc),a0		; init clist
	move.l	a0,$80(a6)		
	move.w      #$ffff,$88(a6)         ; strobe copper jump

	lea	LSP_State+m_dmaconPatch2(pc),a1 
	add.w	#lspdmacon3-clistCowee+3,a0	
	move.l	a0,(a1)		


	STOPACTOR actor_prepare_cowee	
.no	rts					


*------ TEXTSCROLLER SPRITE HORIZONTAL DUPLICATION TRICK -------------------------------------------------------------*

fontcharheight = 25 ;pixel
fontwidth = 40 ; bytes (320px)
fontheight = 200
fontpsize = fontwidth*fontheight
fontcharrowsize = fontwidth*fontcharheight
spriteScrollRightStart = $c8

; d1=offset
spritePositionSetter:
	move.w #charMirrorHeightBefore-1,d6
	move.l b_clist_cowee(pc),a2
	add #spritePosAreaBefore-clistCowee,a2
	move #spriteScrollRightStart,d2
	move #spriteScrollRightStart,d3
	sub.w d1,d2
	add.w d1,d3
	addq #8,d3
	move a2,a3
	move #screenStart,d0
.loop:
	move.b d0,(a2)+
	move.b #$31,(a2)+
	move.w #$FFFE,(a2)+
	
	move.w #$0140,(a2)+
	move.b d0,(a2)+
	move.b #$40,(a2)+

	move.w #$0148,(a2)+
	move.b d0,(a2)+
	move.b #$40,(a2)+

	move.w #$0150,(a2)+
	move.b d0,(a2)+
	move.b #$48,(a2)+

	move.w #$0158,(a2)+
	move.b d0,(a2)+
	move.b #$48,(a2)+

	cmp #spriteMatrixScreenPosY-6,d0			; sprite color change after speechbubble for matrix screen
	bne .noMatrixScreenColorChange

	move.b d0,(a2)+
	move.b #$01,(a2)+
	move.w #$FFFE,(a2)+

	
	move.w #$01B6,(a2)+
	move.w #$0000,(a2)+
	move.w #$01BE,(a2)+
	move.w #$0000,(a2)+
	move.w #$01BA,(a2)+
	move.w #$00F0,(a2)+
	move.w #$01B2,(a2)+
	move.w #$00F0,(a2)+

.noMatrixScreenColorChange:

	move.w #$0140,(a2)+
	move.b d0,(a2)+
	move.b d2,(a2)+

	move.w #$0148,(a2)+
	move.b d0,(a2)+
	move.b d2,(a2)+

	move.w #$0150,(a2)+
	move.b d0,(a2)+
	move.b d3,(a2)+

	move.w #$0158,(a2)+
	move.b d0,(a2)+
	move.b d3,(a2)+

	

	addq #1,d0

	dbf d6,.loop




	move.w #charMirrorHeightAfter-1,d6
	move.l b_clist_cowee(pc),a2
	add #spritePosAreaAfter-clistCowee,a2
	move a2,a3
	move #$00,d0
.loop3:
	move.b d0,(a2)+
	move.b #$31,(a2)+
	move.w #$FFFE,(a2)+
	
	move.w #$0140,(a2)+
	move.b d0,(a2)+
	move.b #$40,(a2)+

	move.w #$0148,(a2)+
	move.b d0,(a2)+
	move.b #$40,(a2)+

	move.w #$0150,(a2)+
	move.b d0,(a2)+
	move.b #$48,(a2)+

	move.w #$0158,(a2)+
	move.b d0,(a2)+
	move.b #$48,(a2)+

	move.w #$0140,(a2)+
	move.b d0,(a2)+
	move.b d2,(a2)+

	move.w #$0148,(a2)+
	move.b d0,(a2)+
	move.b d2,(a2)+

	move.w #$0150,(a2)+
	move.b d0,(a2)+
	move.b d3,(a2)+

	move.w #$0158,(a2)+
	move.b d0,(a2)+
	move.b d3,(a2)+

	

	addq #1,d0

	dbf d6,.loop3
	rts

*------	BLIT BIG FONT TO SPRITES -------------------------------------------------------------*

; d1 = fontx
; d2 = fonty
; d3 = posy

BigFontBlitterCopy:
	MOVEM.L  d0-d3/a0-a1,-(sp)
	asl    #2,d1
	muls   #fontcharrowsize,d2
	asl    #2,d3

	bsr waitblitter
	moveq	#0,d0

	move.w	#40-2,bltamod(a6)			;modulo A (src)
	move.w	#2,bltdmod(a6)	;modulo D (dst)
	move.l	#$09f00000,bltcon0(a6)		;D=A
	subq.l	#1,d0
	move.l	d0,bltafwm(a6)			;mask = $ffffffff

	
	

	move.l	b_font(pc),a0
	add.l d1,a0
	add.l d2,a0
	move.l	a0,bltapt(a6)		;src A
	move.l b_sprite1(pc),a1
	addq #4,a1
	add.l d3,a1
	move.l a1,bltdpt(a6)

	move.w	#fontcharheight*1*64+1,bltsize(a6)	;start 

	
	bsr waitblitter
	moveq	#0,d0

	move.w	#40-2,bltamod(a6)			;modulo A (src)
	move.w	#2,bltdmod(a6)	;modulo D (dst)
	move.l	#$09f00000,bltcon0(a6)		;D=A
	subq.l	#1,d0
	move.l	d0,bltafwm(a6)			;mask = $ffffffff

	
	move.l	b_font(pc),a0
	add.l d1,a0
	add.l d2,a0
	add.l #fontpsize,a0
	move.l	a0,bltapt(a6)		;src A
	move.l b_sprite1(pc),a1
	addq #4+2,a1
	add.l d3,a1
	move.l a1,bltdpt(a6)

	move.w	#fontcharheight*1*64+1,bltsize(a6)	;start 




	bsr waitblitter
	moveq	#0,d0

	move.w	#40-2,bltamod(a6)			;modulo A (src)
	move.w	#2,bltdmod(a6)	;modulo D (dst)
	move.l	#$09f00000,bltcon0(a6)		;D=A
	subq.l	#1,d0
	move.l	d0,bltafwm(a6)			;mask = $ffffffff

	
	move.l	b_font(pc),a0
	add.l d1,a0
	add.l d2,a0
	add.l #(fontpsize*2),a0
	move.l	a0,bltapt(a6)		;src A
	move.l b_sprite2(pc),a1
	addq #4,a1
	add.l d3,a1
	move.l a1,bltdpt(a6)

	move.w	#fontcharheight*1*64+1,bltsize(a6)	;start 




	bsr waitblitter
	moveq	#0,d0

	move.w	#40-2,bltamod(a6)			;modulo A (src)
	move.w	#2,bltdmod(a6)	;modulo D (dst)
	move.l	#$09f00000,bltcon0(a6)		;D=A
	subq.l	#1,d0
	move.l	d0,bltafwm(a6)			;mask = $ffffffff

	
	

	move.l	b_font(pc),a0
	add.l d1,a0
	add.l d2,a0
	add.l #2,a0
	move.l	a0,bltapt(a6)		;src A
	move.l b_sprite3(pc),a1
	addq #4,a1
	add.l d3,a1
	move.l a1,bltdpt(a6)

	move.w	#fontcharheight*1*64+1,bltsize(a6)	;start 

	
	bsr waitblitter
	moveq	#0,d0

	move.w	#40-2,bltamod(a6)			;modulo A (src)
	move.w	#2,bltdmod(a6)	;modulo D (dst)
	move.l	#$09f00000,bltcon0(a6)		;D=A
	subq.l	#1,d0
	move.l	d0,bltafwm(a6)			;mask = $ffffffff

	
	move.l	b_font(pc),a0
	add.l d1,a0
	add.l d2,a0
	add.l #fontpsize+2,a0
	move.l	a0,bltapt(a6)		;src A
	move.l b_sprite3(pc),a1
	addq #4+2,a1
	add.l d3,a1
	move.l a1,bltdpt(a6)

	move.w	#fontcharheight*1*64+1,bltsize(a6)	;start 




	bsr waitblitter
	moveq	#0,d0

	move.w	#40-2,bltamod(a6)			;modulo A (src)
	move.w	#2,bltdmod(a6)	;modulo D (dst)
	move.l	#$09f00000,bltcon0(a6)		;D=A
	subq.l	#1,d0
	move.l	d0,bltafwm(a6)			;mask = $ffffffff

	
	move.l	b_font(pc),a0
	add.l d1,a0
	add.l d2,a0
	add.l #(fontpsize*2)+2,a0
	move.l	a0,bltapt(a6)		;src A
	move.l b_sprite4(pc),a1
	addq #4,a1
	add.l d3,a1
	move.l a1,bltdpt(a6)

	move.w	#fontcharheight*1*64+1,bltsize(a6)	;start 


	MOVEM.L  (sp)+,d0-d3/a0-a1
	rts



*------	WAIT BLITTER ----------------------------------------------------------*

waitblitter:
	BTST #6,$2(A6)
	.WAIT: BTST #6,$2(A6)
	BNE.S .WAIT
	rts	

scrolltext
		dc.b	"              SQG PRESENTS $ AUGS $ "
		dc.b	"MOUNTAINBYTES 2025 $ "
		dc.b	"WE ARE PROUD TO ANNOUNCE THE BIRTH OF OUR GROUP $ SQG $ SWISS QUALITY GROUP $ "
		dc.b	"CODE   JOKERX $ GFX   NK13 $ SFX   MARCEL   "
		
scrolltextEnd
		even
scrolltextPointer dc.l 0

asciiLookupTable
		dc.b 3,5 ;[space]
		dc.b 6,2 ;!
		dc.b 9,4 ;"
		dc.b 3,5 ;#
		dc.b 7,4 ;$
		dc.b 8,4 ;%
		dc.b 3,5 ;&
		dc.b 6,4 ;'
		dc.b 1,4 ;(
		dc.b 2,4 ;)
		dc.b 3,5 ;*
		dc.b 3,5 ;+
		dc.b 0,5 ;,
		dc.b 1,5 ;-
		dc.b 5,4 ;.
		dc.b 3,5 ;/
		dc.b 0,3 ;0
		dc.b 1,3 ;1
		dc.b 2,3 ;2
		dc.b 3,3 ;3
		dc.b 4,3 ;4
		dc.b 5,3 ;5
		dc.b 6,3 ;6
		dc.b 7,3 ;7
		dc.b 8,3 ;8
		dc.b 9,3 ;9
		dc.b 8,2 ;:
		dc.b 9,2 ;;
		dc.b 3,5 ;<
		dc.b 3,5 ;=
		dc.b 3,5 ;>
		dc.b 7,2 ;?
		dc.b 3,5 ;@
		dc.b 0,0 ;A
		dc.b 1,0 ;B
		dc.b 2,0 ;C
		dc.b 3,0 ;D
		dc.b 4,0 ;E
		dc.b 5,0 ;F
		dc.b 6,0 ;G
		dc.b 7,0 ;H
		dc.b 8,0 ;I
		dc.b 9,0 ;J
		dc.b 0,1 ;K
		dc.b 1,1 ;L
		dc.b 2,1 ;M
		dc.b 3,1 ;N
		dc.b 4,1 ;O
		dc.b 5,1 ;P
		dc.b 6,1 ;Q
		dc.b 7,1 ;R
		dc.b 8,1 ;S
		dc.b 9,1 ;T
		dc.b 0,2 ;U
		dc.b 1,2 ;V
		dc.b 2,2 ;W
		dc.b 3,2 ;X
		dc.b 4,2 ;Y
		dc.b 5,2 ;Z
		dc.b 3,5 ;[
		dc.b 3,5 ;\
		dc.b 3,5 ;]
		dc.b 3,5 ;^
		dc.b 3,5 ;_
		dc.b 3,5 ;`
		dc.b 3,5 ;a
		dc.b 3,5 ;b
		dc.b 3,5 ;c
		dc.b 3,5 ;d
		dc.b 3,5 ;e
		dc.b 3,5 ;f
		dc.b 3,5 ;g
		dc.b 3,5 ;h
		dc.b 3,5 ;i
		dc.b 3,5 ;j
		dc.b 3,5 ;k
		dc.b 3,5 ;l
		dc.b 3,5 ;m
		dc.b 3,5 ;n
		dc.b 3,5 ;o
		dc.b 3,5 ;p
		dc.b 3,5 ;q
		dc.b 3,5 ;r
		dc.b 3,5 ;s
		dc.b 3,5 ;t
		dc.b 3,5 ;u
		dc.b 3,5 ;v
		dc.b 3,5 ;w
		dc.b 3,5 ;x
		dc.b 3,5 ;y
		dc.b 3,5 ;z
		dc.b 3,5 ;{
		dc.b 3,5 ;|
		dc.b 3,5 ;}
		dc.b 3,5 ;~
		even

*------	PRINT OUT OF MEMORY ---------------------------------------------------*

printoutofmemory
	lea	.dos(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	OldOpenLibrary(a6)		;
	move.l	d0,a6				;
	beq	.error				;
	jsr	Output(a6)			;
	move.l	d0,d1				;
	beq	.error				;
	moveq	#.textend-.text,d3		; length
	lea	.text(pc),a1			;
	move.l	a1,d2				;
	jsr	Write(a6)			;
	tst.l	d0				;
	beq	.error				;
	move.l	a6,a1				;
	move.l	AbsExecBase.w,a6		;
	jsr	CloseLibrary(a6)		;
.error	moveq	#0,d0				;
	rts					;

.dos	dc.b	"dos.library",0
.text	dc.b	"Error: Could not allocate enough memory",10
.textend
	even


;*****************************************************************
;
;	Light Speed Player v1.13 (modified)
;	Fastest Amiga MOD player ever :)
;	Written By Arnaud Carré (aka Leonard / OXYGENE)
;	https://github.com/arnaud-carre/LSPlayer
;	twitter: @leonard_coder
;
;*****************************************************************

;------------------------------------------------------------------
;
;	LSP_MusicInit
;
;		In:	a0: LSP music data(any memory)
;			a1: LSP sound bank(chip memory)
;		Out:a0: music BPM pointer (16bits)
;			d0: music len in tick count
;
;------------------------------------------------------------------
LSP_MusicInit
	lea	base(pc),a0			; a0: music data (any mem) + 10
	add.l	#lspmusic-base,a0		;
	move.l	b_lspbank(pc),d1		; a1: sound bank data (chip mem)

	lea	LSP_State(pc),a3
	move.l	a0,a4				; relocation flag ad
	addq.w	#2,a0				; skip relocation flag
	move.w	(a0)+,m_currentBpm(a3)		; default BPM
	move.w	(a0)+,m_escCodeRewind(a3)
	move.w	(a0)+,m_escCodeSetBpm(a3)
	move.w	(a0)+,m_escCodeGetPos(a3)
	move.l	(a0)+,-(a7)			; music len in frame ticks
	move.w	(a0)+,d0			; instrument count
	lea	-12(a0),a2			; LSP data has -12 offset on instrument tab (win 2 cycles in insane player)
	move.l	a2,m_lspInstruments(a3)		; instrument tab addr (minus 4)
	subq.w	#1,d0
	move.l	a0,a1				; keep relocated flag
.relocLoop
	tst.b	(a4)				; relocation guard
	bne	.relocated
	add.l	d1,(a0)
	add.l	d1,6(a0)
.relocated
	lea	12(a0),a0
	dbf	d0,.relocLoop
	move.w	(a0)+,d0			; codes table size
	move.l	a0,m_codeTableAddr(a3)		; code table
	add.w	d0,d0
	add.w	d0,a0

	; read sequence timing infos (if any)
	move.w	(a0)+,m_seqCount(a3)
	beq	.noSeq
	move.l	a0,m_seqTable(a3)
	clr.w	m_currentSeq(a3)
	move.w	m_seqCount(a3),d0
	moveq	#0,d1
	move.w	d0,d1
	lsl.w	#3,d1				; 8 bytes per entry
	add.w	#12,d1				; add 3 last 32bits (word stream size, byte stream loop, word stream loop)
	add.l	a0,d1				; word stream data address
	subq.w	#1,d0
.seqRel	tst.b	(a4)
	bne	.skipRel
	add.l	d1,(a0)
	add.l	d1,4(a0)
.skipRel
	addq.w	#8,a0
	dbf	d0,.seqRel

.noSeq	movem.l	(a0)+,d0-d2			; word stream size, byte stream loop point, word stream loop point
	st	(a4)				; mark this music score as "relocated"
	move.l	a0,m_wordStream(a3)
	lea	(a0,d0.l),a1			; byte stream
	move.l	a1,m_byteStream(a3)
	add.l	d2,a0
	add.l	d1,a1
	move.l	a0,m_wordStreamLoop(a3)
	move.l	a1,m_byteStreamLoop(a3)
	lea	m_currentBpm(a3),a0
	move.l	(a7)+,d0			; music len in frame ticks
	rts


;------------------------------------------------------------------
;
;	LSP_MusicPlayTick
;
;		In:	a6: must be $dff000
;			Scratched regs: d0/d1/d2/a0/a1/a2/a3/a4/a5
;		Out:None
;
;------------------------------------------------------------------
LSP_MusicPlayTick
	lea	LSP_State(pc),a1
	move.l	(a1),a0				; byte stream
	move.l	m_codeTableAddr(a1),a2		; code table
.process
	moveq	#0,d0
.cloop	move.b	(a0)+,d0
	beq	.cextended
	add.w	d0,d0
	move.w	(a2,d0.w),d0			; code
	beq	.noInst
.cmdExec
	add.b	d0,d0
	bcc	.noVd
	move.b	(a0)+,$d9(a6)
.noVd	add.b	d0,d0
	bcc	.noVc
	move.b	(a0)+,$c9(a6)
.noVc	add.b	d0,d0
	bcc	.noVb
	move.b	(a0)+,$b9(a6)
.noVb	add.b	d0,d0
	bcc	.noVa
	move.b	(a0)+,$a9(a6)
.noVa	move.l	a0,(a1)+			; store byte stream ptr
	move.l	(a1),a0				; word stream
	tst.b	d0
	beq	.noPa
	add.b	d0,d0
	bcc	.noPd
	move.w	(a0)+,$d6(a6)
.noPd	add.b	d0,d0
	bcc	.noPc
	move.w	(a0)+,$c6(a6)
.noPc	add.b	d0,d0
	bcc	.noPb
	move.w	(a0)+,$b6(a6)
.noPb	add.b	d0,d0
	bcc	.noPa
	move.w	(a0)+,$a6(a6)
.noPa	tst.w	d0
	beq	.noInst

	moveq	#0,d1
	move.l	m_lspInstruments-4(a1),a2	; instrument table
	lea	.resetv+12(pc),a4
	lea	$d0(a6),a5
	moveq	#4-1,d2
.vloop	add.w	d0,d0
	bcs	.setIns
	add.w	d0,d0
	bcc	.skip
	move.l	(a4),a3
	move.l	(a3)+,(a5)
	move.w	(a3)+,4(a5)
	bra	.skip
.setIns	add.w	(a0)+,a2
	add.w	d0,d0
	bcc	.noReset
	bset	d2,d1
	move.w	d1,$96(a6)
.noReset
	move.l	(a2)+,(a5)
	move.w	(a2)+,4(a5)
	move.l	a2,(a4)
.skip	subq.w	#4,a4
	sub.w	#$10,a5
	dbf	d2,.vloop

	move.l	m_dmaconPatch-4(a1),a3		; dmacon patch
	move.b	d1,(a3)				; dmacon			

	move.l	m_dmaconPatch2-4(a1),a3		; dmacon patch
	move.b	d1,(a3)				; dmacon			

	move.l	m_dmaconPatch3-4(a1),a3		; dmacon patch
	move.b	d1,(a3)				; dmacon			

	move.l	m_dmaconPatch4-4(a1),a3		; dmacon patch
	move.b	d1,(a3)				; dmacon			


.noInst	move.l	a0,(a1)				; store word stream (or byte stream if coming from early out)
	rts

.cextended
	add.w	#$100,d0
	move.b	(a0)+,d0
	beq	.cextended
	add.w	d0,d0
	move.w	(a2,d0.w),d0			; code
	cmp.w	m_escCodeRewind(a1),d0
	beq	.r_rewind
	cmp.w	m_escCodeSetBpm(a1),d0
	beq	.r_chgbpm
	cmp.w	m_escCodeGetPos(a1),d0
	bne	.cmdExec
.r_setPos
	move.b	(a0)+,(m_currentSeq+1)(a1)
	bra	.process

.r_rewind	
	move.l	m_byteStreamLoop(a1),a0
	move.l	m_wordStreamLoop(a1),m_wordStream(a1)
	bra	.process

.r_chgbpm
	move.b	(a0)+,(m_currentBpm+1)(a1)	; BPM
	bra	.process

.resetv	dc.l	0,0,0,0

	rsreset	
m_byteStream		rs.l	1	;  0 byte stream
m_wordStream		rs.l	1	;  4 word stream
m_dmaconPatch		rs.l	1	;  8 m_lfmDmaConPatch
m_codeTableAddr		rs.l	1	; 12 code table addr
m_escCodeRewind		rs.w	1	; 16 rewind special escape code
m_escCodeSetBpm		rs.w	1	; 18 set BPM escape code
m_lspInstruments	rs.l	1	; 20 LSP instruments table addr
m_relocDone		rs.w	1	; 24 reloc done flag
m_currentBpm		rs.w	1	; 26 current BPM
m_byteStreamLoop	rs.l	1	; 28 byte stream loop point
m_wordStreamLoop	rs.l	1	; 32 word stream loop point
m_seqCount		rs.w	1
m_seqTable		rs.l	1
m_currentSeq		rs.w	1
m_escCodeGetPos		rs.w	1

m_dmaconPatch2		rs.l	1	; added
m_dmaconPatch3		rs.l	1	; added
m_dmaconPatch4		rs.l	1	; added

sizeof_LSPVars		rs.w	0

LSP_State	ds.b	sizeof_LSPVars
	even

spritenull:
		dc.w   $0,$0
		dc.w   $0,$0
spritenullend
spritenullsize=spritenullend-spritenull

spriteXStart=$48
spriteYStart=$2c
spriteSpeechbubbleHeight=69

spriteMatrixScreenHeight=39
spriteMatrixScreenWidth=55

spriteMatrixScreenPosX=$5f
spriteMatrixScreenPosY=$9e

screenStart=$2c

sprite1:
		dc.b spriteYStart,spriteXStart,screenStart,$02
		dcb.w 2*256
		dcb.w 2*50
sprite1end
sprite1size=sprite1end-sprite1

sprite2:
		dc.b spriteYStart,spriteXStart,screenStart,$82
		dcb.w 2*256
		dcb.w 2*50
sprite2end
sprite2size=sprite2end-sprite2

sprite3:
		dc.b spriteYStart,spriteXStart+8,screenStart,$02
		dcb.w 2*256
		dcb.w 2*50

sprite3end
sprite3size=sprite3end-sprite3

sprite4:
		dc.b spriteYStart,spriteXStart+8,screenStart,$82
		dcb.w 2*256
		dcb.w 2*50
sprite4end
sprite4size=sprite4end-sprite4

sprite5:
		dc.b speechSpriteOrigYpos,speechSprite5OrigXpos,speechSpriteOrigYpos+spriteSpeechbubbleHeight,$00
		dcb.w 2*spriteSpeechbubbleHeight
sprite5matrix:
		dc.b spriteMatrixScreenPosY,spriteMatrixScreenPosX,spriteMatrixScreenPosY+spriteMatrixScreenHeight,$00
		dcb.w 2*spriteMatrixScreenHeight
		dc.w $0000,$0000
sprite5end
sprite5size=sprite5end-sprite5
sprite5matrixOffset=sprite5matrix-sprite5

sprite6:
		dc.b speechSpriteOrigYpos,speechSprite6OrigXpos,speechSpriteOrigYpos+spriteSpeechbubbleHeight,$00
		dcb.w 2*spriteSpeechbubbleHeight
sprite6matrix:
		dc.b spriteMatrixScreenPosY,spriteMatrixScreenPosX+8,spriteMatrixScreenPosY+spriteMatrixScreenHeight,$00
		dcb.w 2*spriteMatrixScreenHeight
		dc.w $0000,$0000
sprite6end
sprite6size=sprite6end-sprite6
sprite6matrixOffset=sprite6matrix-sprite6

sprite7:
		dc.b speechSpriteOrigYpos,speechSprite7OrigXpos,speechSpriteOrigYpos+spriteSpeechbubbleHeight,$00
		dcb.w 2*spriteSpeechbubbleHeight
sprite7matrix:
		dc.b spriteMatrixScreenPosY,spriteMatrixScreenPosX+2*8,spriteMatrixScreenPosY+spriteMatrixScreenHeight,$00
		dcb.w 2*spriteMatrixScreenHeight
		dc.w $0000,$0000
sprite7end
sprite7size=sprite7end-sprite7
sprite7matrixOffset=sprite7matrix-sprite7

sprite8:
		dc.b speechSpriteOrigYpos,speechSprite8OrigXpos,speechSpriteOrigYpos+spriteSpeechbubbleHeight,$00
		dcb.w 2*spriteSpeechbubbleHeight
sprite8matrix:
		dc.b 0,0,0,$00
sprite8end
sprite8size=sprite8end-sprite8
sprite8matrixOffset=sprite8matrix-sprite8

speechbubbleLineCharWidth=7
              EVEN

speechbubbleText
		dc.b "It's me"
		dc.b "       "
		dc.b " Cowee "
		dc.b "       "
		dc.b "Welcome"
		dc.b "  to   "
		dc.b "       "

		dc.b "       "
		dc.b "Mountai"
		dc.b "nBytes "
		dc.b "       "
		dc.b " 2025  "
		dc.b "       "
		dc.b "      $ "

		dc.b "       "       
		dc.b "  SQG  "
		dc.b "       "
		dc.b " is    "
		dc.b " proud "
		dc.b "  to   "
		dc.b "presen$t"

		dc.b "       "       
		dc.b "       "
		dc.b "       "
		dc.b " AUGS  "
		dc.b "       "
		dc.b "       "
		dc.b "      $ "

		dc.b "       "       
		dc.b "  now  "
		dc.b "       "
		dc.b "       "
		dc.b " shout "
		dc.b "       "
		dc.b "       "

		dc.b "       "
		dc.b "AAAAAAA"       
		dc.b "MMMMMMM"
		dc.b "IIIIIII"
		dc.b "GGGGGGG"
		dc.b "AAAAAAA"
		dc.b "      $ "

		dc.b "       "       
		dc.b "LAUTER!"
		dc.b "       "
		dc.b " !!!   "
		dc.b "       "
		dc.b "   !!! "
		dc.b "       "

		dc.b "       "       
		dc.b "  see  "
		dc.b "       "
		dc.b "  it's "
		dc.b "       "
		dc.b " alive "
		dc.b "       "



		dc.b "I hope "
		dc.b "you all"
		dc.b " enjoy "
		dc.b "the    "
		dc.b "   fine"
		dc.b " music "
		dc.b " from  "

		dc.b "       "
		dc.b "       "
		dc.b "Marcel "
		dc.b "       "
		dc.b "       "
		dc.b "       "
		dc.b "       "

		dc.b "       "
		dc.b "Wooahh "
		dc.b " that  "
		dc.b "   Bass"
		dc.b "       "
		dc.b "       "
		dc.b " $ $ $ $ $ $ "

		dc.b " visit "       
		dc.b "       "
		dc.b " AUGS  "
		dc.b "       "
		dc.b "   @   "
		dc.b "       "
		dc.b "augs.c$h"

		dc.b "JokerX "
		dc.b "on the "
		dc.b "keys:  "
		dc.b "       "
		dc.b "Hi all,"
		dc.b "first  "
		dc.b " time $ "

		dc.b "doing  "
		dc.b "a compo"
		dc.b "prod.  "
		dc.b "   Had "
		dc.b " a lot "
		dc.b " of fun"
		dc.b "       "

		dc.b "getting"
		dc.b "to know"
		dc.b "  the  "
		dc.b "chipset"
		dc.b "       "
		dc.b "Greetz "
		dc.b " go out"

		dc.b " to:   "
		dc.b "       "
		dc.b "Darkage"
		dc.b "       "
		dc.b "DESiRE "
		dc.b "       "
		dc.b "FLT    "

		dc.b "H0ffman"
		dc.b "       "
		dc.b "Melon  "
		dc.b "       "
		dc.b "Nah-Kol"
		dc.b "or     "
		dc.b "       "

		dc.b "Scoopex"
		dc.b "       "
		dc.b "Spreadp"
		dc.b "oint   "
		dc.b "       "
		dc.b "       "
		dc.b "       "

		dc.b "Now get"
		dc.b "a drink"
		dc.b " and   "
		dc.b "  have "
		dc.b " some  "
		dc.b "       "
		dc.b "FUN$!$!$!$!"
		even
speechbubbleTextEnd
speechbubbleTextSize=speechbubbleTextEnd-speechbubbleText


bubbletextPointer dc.l 0
bubbletextLastX dc.w 0
bubbletextLastY dc.w 0

scrollBounceValueCount=24
speechBounceXValueCount=12
speechSprite5OrigXpos = $60
speechSprite6OrigXpos = $68
speechSprite7OrigXpos = $70
speechSprite8OrigXpos = $78
speechSpriteOrigYpos = $3c
speechBounce1:
		dc.b 1,3,5,6,8,9,10,11,12,13,14,15,16,16,17,17,18,18,18,18,18,18,18,18,17,17,16,16,15,15,14,14,13,13,12,12,11,11,10,10,9,9,8,8,7,7,6,6,6,5,5,4,4,3,3,3,2,2,2,1,1,1,0,0
		even
speechBounce2:
		dc.b 0,1,1,2,2,3,3,3,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,5,5,5,5,5,5,5,4,4,4,4,4,3,3,3,3,3,2,2,2,2,2,2,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0
		even
speechBounce3:
		dc.b 0,-1,-1,-2,-2,-3,-3,-3,-4,-4,-4,-5,-5,-5,-5,-5,-5,-6,-6,-6,-6,-6,-6,-6,-6,-6,-6,-5,-5,-5,-5,-5,-5,-5,-5,-5,-3,-3,-3,-3,-2,-2,-2,-2,-2,-2,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0
		even
speechBounce4:
		dc.b -1,-3,-5,-6,-8,-9,-10,-11,-12,-13,-14,-15,-16,-16,-17,-17,-18,-18,-18,-18,-18,-18,-18,-18,-18,-17,-17,-17,-17,-16,-16,-15,-15,-14,-14,-13,-13,-12,-11,-10,-10,-9,-9,-8,-8,-7,-7,-6,-6,-6,-5,-5,-4,-4,-3,-3,-3,-2,-2,-2,-1,-1,-1,0
		even


scrollerBounce:
		dc.b 0,1,2,2,3,4,4,4,5,5,5,5,5,4,4,4,4,4,2,2,1,1,1,0
		even


speechBounceX1:
		dc.b -1,-2,-1,0,-1,-2,-1,0,-1,-2,-1,0,-1,-2,-1,0,-1,-2,-1,0,-1,-2,-1,0
speechBounceX2:
		dc.b 1,2,1,0,1,2,1,0,1,2,1,0,1,2,1,0,1,2,1,0,1,2,1,0


speechBounceIndex dc.w 0
scrollBounceIndex dc.w 0
waitMatrix dc.w 10

speechbubble incbin "speech_bubble.raw"
speechbubbleend
		even
speechbubblesize=speechbubbleend-speechbubble

speechbubbleCopy incbin "speech_bubble.raw"
speechbubbleCopyend
		even
speechbubbleCopysize=speechbubbleCopyend-speechbubbleCopy

font incbin "font.raw"
fontend
		even
fontsize=fontend-font

lspbank	incbin	"TrackLooped.lsbank"
lspbankend
	even	; very important

lspmusic
	incbin	"TrackLooped.lsmusic",10 ; skip header (10 bytes)
	even

starfield	incbin	"starfield.raw"
starfieldend
	even

image
	incbin "cowee_4_bitplanes.raw"
imageEnd
	even

boing	incbin	"boingball"
boingend
	even

augslogo
	incbin "augs_1_bitplane.raw"
augslogoend
	even