; THE MEGA-MIGHTY SWISS CRACKING ASSOCIATION AND SPREADPOINT
; TTCC - THE TERRA CRESTA CRACKTRO (AMIGA, PAL, >= OCS, >= 68000, >= 512 KB)
; (C) 2023 DEPECHE

; Build with vasm
; vasmm68k_mot -kick1hunks -Fhunkexe -o ttcc -nosym ttcc.s

AbsExecBase		equ	4
OldOpenLibrary	equ -408
CloseLibrary	equ -414
Write			equ -48
Output			equ -60
AvailMem		equ	-216
AllocMem		equ -198
FreeMem			equ -210
TypeOfMem		equ -534
WaitTOF			equ -270
Forbid			equ	-132
Permit			equ	-138
LoadView		equ	-222

custom			equ	$dff000

inviswidth		equ	24 ; *8 = 192 px
pwidth			equ 40+inviswidth ; 320+192=512 to get rid of multiplication
pheight			equ 256 ; px
psize			equ pwidth*pheight
center			equ psize/2+((pwidth-inviswidth)/2)
invistop		equ	inviswidth*8 ; = 192 px
invisbottom		equ	inviswidth*8 ; = 192 px
p1and2size		equ	pwidth*(invistop+pheight+invisbottom+pheight+invisbottom)

availablemem	equ	0
numbers			equ 0
testing			equ	0
timing			equ 0

color1			equ	$09ef ; logos
color2			equ $0478 ; sausboden
color3			equ $08bc ; scrolltext

maxlaserpairs	equ 4

; DMACON
; see http://coppershade.org/articles/Code/Reference/DMACON/
SET				equ	1<<15			; 0=clear, 1=set bits that are set to 1 below
BLTPRI			equ	1<<10			; Blitter DMA priority (over CPU micro) "blitter nasty"
DMAEN			equ	1<<9			; Enable all DMA below
BPLEN			equ	1<<8			; Bit plane DMA
COPEN			equ	1<<7			; Copper DMA
BLTEN			equ	1<<6			; Blitter DMA

*------	ALLOCATE MEMORY AND SAVE STATE -----------------------------------*

base
	movem.l	a0-a6/d0-d7,-(a7)		;
	bsr		alloc					;
	bne		.exit					; out of memory error?

	if availablemem
	move.l	AbsExecBase.w,a6		;
	move.l	#MEMF_CHIP,d1			;
	jsr		AvailMem(a6)			;
	move.l	d0,$210.w				; Free (available) memory
	endif

	move.l	AbsExecBase.w,a6		;
	lea		.gfx(pc),a1				;
	jsr		OldOpenLibrary(a6)		; open gfx library
	tst.l	d0						;
	beq		.exit					; couldn't open gfx library!
	move.l	d0,a6					;
	move.l 	34(a6),-(a7)			; view
	move.l	d0,-(a7)				; gfx base
	move.l 	38(a6),-(a7)			; copper list 1
	move.l 	50(a6),-(a7)			; copper list 2
	sub.l	a1,a1					;
	jsr		LoadView(a6)			;
	jsr		WaitTOF(a6)				;
	jsr		WaitTOF(a6)				;
	move.l	AbsExecBase.w,a6		;	
	jsr		Forbid(a6)				;
	
	lea		custom,a6				;
	bsr		waitblitter				;

	move.w	$02(a6),-(a7)			; store DMA control
	move.w	$1c(a6),-(a7)			; store interrupt enable bits
	move.l	$6c.w,-(a7)				; store irq3
	move.w	#$7fff,d0				;
	move.w	d0,$9a(a6)				;
	move.w	d0,$9c(a6)				; delete all interrupt requests
	move.w	d0,$96(a6)				; disable all DMAs

	clr.w	-(a7)					; store LED state
	btst	#1,$bfe001				;
	beq		.ledstate				;
	not.w	(a7)					;
.ledstate
	bset	#1,$bfe001				; LED dark

*------	INIT -------------------------------------------------------------*

    lea     vars(pc),a5             ;

	add.w	#1<<actordbuf2,v_actors(a5) ; bset #actordbuf2

;	move.l	b_clisttails(pc),$210.w

	move.w	#512,viperay(a5)		;
	move.w	#14,viperdax(a5)		;
	move.w	#6,viperday(a5)			;

	lea		aseq(pc),a0				;
	move.l	a0,v_aseqpointer(a5)	;

	lea		playcmds(pc),a0			;
	move.l	a0,v_cmdspointer(a5)	;

	move.w	#SET+DMAEN+BLTPRI+BLTEN,$96(a6) ;

	lea		sausboden(pc),a2		;
	move.l	b_staticplane1(pc),a0	;
	add.l	#center,a0				;
	bsr		drawlines				;

	bsr		drawearth				;
	bsr		clearbelowearth			;
	
	bsr		initfont				;

;	move.l	v_staticplane2(a5),a0	;
;	add.l	#center,a0				;
;	bsr		drawtext				;

	bsr		lspinit					;

	lea		irq3(pc),a0				;
	move.l	a0,$6c.w				;

	bsr		waitraster				; avoid flickering (?)
    move.l  clistbase(pc),$80(a6)   ;
	move.l	b_clisttails(pc),$84(a6)	;
    
;	move.w	#SET+DMAEN+BPLEN+BLTPRI+BLTEN+COPEN,$96(a6) ;
	move.w	#SET+DMAEN+BPLEN+BLTEN+COPEN,$96(a6) ; No BLTPRI -> viper3dto2d

	move.w  #$c030,$9a(a6)          ; enable coper and vertb interrupts

*------	IDLE LOOP --------------------------------------------------------*

	bsr		precalc					; pre calculate requested logo
	
*------	RESTORE STATE AND EXIT -------------------------------------------*

	bsr		waitblitter				;
	
	tst.w	(a7)+					; restore state
	bne		.leddark				;
	bclr	#1,$bfe001				; LED bright
.leddark
	move.w	#$7fff,d0				;
	move.w	d0,$9a(a6)				;
	move.w	d0,$9c(a6)				;
	move.w	d0,$96(a6)				;
	
	moveq	#0,d0					; volume to zero
	move.w	d0,$a8(a6)				;
	move.w	d0,$b8(a6)				;
	move.w	d0,$c8(a6)				;
	move.w	d0,$d8(a6)				;
	
	move.l	(a7)+,$6c.w				; 
	move.w	(a7)+,d0				;
	or.w	#$c000,d0				;
	move.w	d0,$9a(a6)				;
	move.w	(a7)+,d0				;
	or.w	#$8000,d0				;
	move.w	d0,$96(a6)				;

	move.l	(a7)+,$84(a6)			; copper list 2
	move.l	(a7)+,$80(a6)			; copper list 1
	move.l	(a7)+,a6				; gfx base
	move.l	(a7)+,a1				; view
	jsr		LoadView(a6)			;
	jsr		WaitTOF(a6)				;
	jsr		WaitTOF(a6)				;
	move.l	a6,a1					; parameter for CloseLibrary
	move.l	AbsExecBase.w,a6		;
	jsr		CloseLibrary(a6)		; close gfx library
	jsr		Permit(a6)				;

	bsr		dealloc					;
.exit
	movem.l	(a7)+,a0-a6/d0-d7		;
	moveq	#0,d0					;
	rts								;

.gfx
	dc.b	"graphics.library",0
	even


*------	VARS -------------------------------------------------------------*

    rsreset
mtrigger		rs.w	1
mdataend		rs.l	1
mframe			rs.w	1
; order of dbplanes, dbplanes2, dbplanes3 must not be changed (movem)
dbplanes		rs.l	2	; double buffer plane1 and plane2
dbplanes2		rs.l	2	; double buffer plane3 and plane4
dbplanes3		rs.l	2	; double buffer scrollplane 1 and scrollplane 2
doquit			rs.w	1	; signal to quit
frame			rs.w	1	; frame counter
datapointer		rs.l	1
matrix			rs.w	3*3	; 3D rotation matrix
vipertx			rs.l	1
viperty			rs.l	1
viperax			rs.w	1
viperay			rs.w	1
viperdax		rs.w	1
viperday		rs.w	1

temptrigger		rs.w	1
tempabc			rs.w	3

extralines		rs.w	1	; addtional lines for lasers to draw
numparticles	rs.w	1

v_staticplane2	rs.l	1
v_earthpos		rs.l	1

v_cmdspointer	rs.l	1
v_textpindex	rs.w	1

v_actors		rs.w	1
v_sausboden		rs.w	1
v_precalcid		rs.w	1

v_chardelay		rs.w	1
v_textpos		rs.w	1

v_wait			rs.b	1
v_diwstop		rs.b	1

v_aseqpointer	rs.l	1

v_charpos		rs.w	1

	if numbers&testing
v_number2		rs.l	1
	endif

sizeofvars		rs.w    0

vars    ds.b    sizeofvars,0
	even


*------ INIT TERRA CRESTA LOGO 1 ----------------------------------------------*

triggeradd	equ	2

inittclogo1
	move.w	#numparticlestc-1,numparticles(a5)

	lea		logoterracresta(pc),a0			;
	move.l	b_particles(pc),a1				;
		
	clr.w	temptrigger(a5)					;
	move.w	temptrigger(a5),mtrigger(a5)	;
	
	moveq	#pT1-1,d7						; T
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;
		
	moveq	#pE1-1,d7						; E
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#pR1o-1,d7						; R
	bsr		initlogolettertc				;
	moveq	#pR1i-1,d7						;
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#pR2o-1,d7						; R
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#pR2i-1,d7						;
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#pA1o-1,d7						; A
	bsr		initlogolettertc				;
	moveq	#pA1i-1,d7						;
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#pC-1,d7						; C
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#pR3o-1,d7						; R
	bsr		initlogolettertc				;
	moveq	#pR3i-1,d7						;
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#pE2-1,d7						; E
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#pS-1,d7						; S
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#pT2-1,d7						; T
	bsr		initlogolettertc				;
	add.w	#triggeradd,temptrigger(a5)		;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#pA2o-1,d7						; A
	bsr		initlogolettertc				;
	moveq	#pA2i-1,d7						;
	bsr		initlogolettertc				;
	rts										;


*------ INIT TERRA CRESTA LOGO 2 ----------------------------------------------*

dtrigger	equ	6

inittclogo2
	move.w	#numparticlestc-1,numparticles(a5)

	lea		logoterracresta(pc),a0			;
	move.l	b_particles(pc),a1				;
	
	moveq	#pT1-1,d7						; T
	bsr		initlogolettertc				;
	addq.w	#dtrigger,mtrigger(a5)			;
		
	moveq	#pE1-1,d7						; E
	bsr		initlogolettertc				;
	addq.w	#dtrigger,mtrigger(a5)			;

	moveq	#pR1o-1,d7						; R
	bsr		initlogolettertc				;
	moveq	#pR1i-1,d7						;
	bsr		initlogolettertc				;
	addq.w	#dtrigger,mtrigger(a5)			;

	moveq	#pR2o-1,d7						; R
	bsr		initlogolettertc				;
	addq.w	#dtrigger,mtrigger(a5)			;

	moveq	#pR2i-1,d7						;
	bsr		initlogolettertc				;
	add.w	#dtrigger,mtrigger(a5)			;

	moveq	#pA1o-1,d7						; A
	bsr		initlogolettertc				;
	moveq	#pA1i-1,d7						;
	bsr		initlogolettertc				;
	addq.w	#dtrigger,mtrigger(a5)			;

	moveq	#pC-1,d7						; C
	bsr		initlogolettertc				;
	addq.w	#dtrigger,mtrigger(a5)			;

	moveq	#pR3o-1,d7						; R
	bsr		initlogolettertc				;
	moveq	#pR3i-1,d7						;
	bsr		initlogolettertc				;
	addq.w	#dtrigger,mtrigger(a5)			;

	moveq	#pE2-1,d7						; E
	bsr		initlogolettertc				;
	addq.w	#dtrigger,mtrigger(a5)			;

	moveq	#pS-1,d7						; S
	bsr		initlogolettertc				;
	addq.w	#dtrigger,mtrigger(a5)			;

	moveq	#pT2-1,d7						; T
	bsr		initlogolettertc				;
	addq.w	#dtrigger,mtrigger(a5)			;

	moveq	#pA2o-1,d7						; A
	bsr		initlogolettertc				;
	moveq	#pA2i-1,d7						;
	bsr		initlogolettertc				;
	rts										;


*------ INIT TERRA CRESTA LOGO 3 ----------------------------------------------*

; TERRA explodes first, CRESTA later
inittclogo3
	move.w	#numparticlestc-1,numparticles(a5)

	lea		logoterracresta(pc),a0			;
	move.l	b_particles(pc),a1				;
	
	clr.w	mtrigger(a5)					;
	
	moveq	#pT1-1,d7						; T
	bsr		initlogolettertc2				;
		
	moveq	#pE1-1,d7						; E
	bsr		initlogolettertc2				;

	moveq	#pR1o-1,d7						; R
	bsr		initlogolettertc2				;
	moveq	#pR1i-1,d7						;
	bsr		initlogolettertc2				;

	moveq	#pR2o-1,d7						; R
	bsr		initlogolettertc2				;

	moveq	#pR2i-1,d7						;
	bsr		initlogolettertc2				;

	moveq	#pA1o-1,d7						; A
	bsr		initlogolettertc2				;
	moveq	#pA1i-1,d7						;
	bsr		initlogolettertc2				;

	move.w	#50,mtrigger(a5)				; CRESTA explodes 1 sec later

	moveq	#pC-1,d7						; C
	bsr		initlogolettertc2				;

	moveq	#pR3o-1,d7						; R
	bsr		initlogolettertc2				;
	moveq	#pR3i-1,d7						;
	bsr		initlogolettertc2				;

	moveq	#pE2-1,d7						; E
	bsr		initlogolettertc2				;

	moveq	#pS-1,d7						; S
	bsr		initlogolettertc2				;

	moveq	#pT2-1,d7						; T
	bsr		initlogolettertc2				;

	moveq	#pA2o-1,d7						; A
	bsr		initlogolettertc2				;
	moveq	#pA2i-1,d7						;
	bsr		initlogolettertc2				;
	rts										;


*------ INIT LOGO LETTER TC ---------------------------------------------------*

initlogolettertc
.loop
	movem.w	(a0),d0-d3			; d0=p1x d1=p1y d2=p2x d3=p2y
	
	move.w	d0,d4				; p1x
	add.w	d2,d4				; p2x
	asr.w	#1,d4				; d4 = focusX = (p1x+p2x)/2
	sub.w	d4,d0				; p1x-focusX
	move.w	d0,p1x(a1)			;
	sub.w	d4,d2				; p2x-focusX
	move.w	d2,p2x(a1)			;
	move.w	d4,tx(a1)			;
	asr.w	#4,d4 				; 3
	move.w	d4,dtx(a1)			;

	move.w	d1,d4				; p1y
	add.w	d3,d4				; p2y
	asr.w	#1,d4				; d4 = focusY = (p1y+p2y)/2
	sub.w	d4,d1				; p1y-focusY
	move.w	d1,p1y(a1)			;
	sub.w	d4,d3				; p2y-focusY
	move.w	d3,p2y(a1)			;
	move.w	d4,ty(a1)			;
	asr.w	#4,d4 				; 3
	move.w	d4,dty(a1)			;
	
	clr.w	p1z(a1)				;
	clr.w	p2z(a1)				;
	
	move.w	#-50,tz(a1)			;
	move.w	#-1,dtz(a1)			;
	
	clr.w	a(a1)				;
	clr.w	b(a1)				;
	clr.w	c(a1)				;

	add.w	d1,d4				; "random" number
	and.w	#%1110,d4			; 2 - 14
	move.w	.variationda(pc,d4.w),da(a1)	

	add.w	d7,d4				; "random" number
	and.w	#%1110,d4			; 2 - 14
	move.w	.variationdb(pc,d4.w),db(a1)	

	add.w	d4,d3				; "random" number
	and.w	#%1110,d3			; 2 - 14
	move.w	.variationdc(pc,d3.w),dc(a1)	
	
	move.w	mtrigger(a5),trigger(a1)
	move.w	#150,ttl(a1)		;
	addq.l	#2*2,a0				; skip p1
	add.w	#particlesize,a1	;
	dbf		d7,.loop			;

	addq.l	#2*2,a0				; next letter
	rts							;

.variationda
.variationdb
.variationdc
	dc.w	0,-14,16,18,-20,22,10,-12


*------ INIT LOGO LETTER TC 2 -------------------------------------------------*

initlogolettertc2
.loop
	movem.w	(a0),d0-d3			; d0=p1x d1=p1y d2=p2x d3=p2y
	
	move.w	d0,d4				; p1x
	add.w	d2,d4				; p2x
	asr.w	#1,d4				; d4 = focusX = (p1x+p2x)/2
	sub.w	d4,d0				; p1x-focusX
	move.w	d0,p1x(a1)			;
	sub.w	d4,d2				; p2x-focusX
	move.w	d2,p2x(a1)			;
	move.w	d4,tx(a1)			;
	asr.w	#4,d4 				;
	move.w	d4,dtx(a1)			;

	move.w	d1,d4				; p1y
	add.w	d3,d4				; p2y
	asr.w	#1,d4				; d4 = focusY = (p1y+p2y)/2
	sub.w	d4,d1				; p1y-focusY
	move.w	d1,p1y(a1)			;
	sub.w	d4,d3				; p2y-focusY
	move.w	d3,p2y(a1)			;
	move.w	d4,ty(a1)			;
	asr.w	#4,d4 				;
	move.w	d4,dty(a1)			;
	
	clr.w	p1z(a1)				;
	clr.w	p2z(a1)				;

	move.w	#-50,tz(a1)			;
	clr.w	dtz(a1)				;

	clr.w	a(a1)				;
	clr.w	b(a1)				;
	clr.w	c(a1)				;
	move.w	#2,da(a1)			;
	move.w	#4,db(a1)			;
	
	add.w	d7,d4				; "random" number
	and.w	#%1110,d4			; 2 - 14
	move.w	.variationdc(pc,d4.w),dc(a1)	
	
	move.w	mtrigger(a5),trigger(a1)
	move.w	#120,ttl(a1)		;
	
;	addq.w	#1,mtrigger(a5)		;
	
	addq.l	#2*2,a0				; skip p1
	add.w	#particlesize,a1	;
	dbf		d7,.loop			;

	addq.l	#2*2,a0				; next letter
	rts							;

.variationdc
	dc.w	0,-2,2,4,-4,2,4,-4


*------ INIT SPREADPOINT LOGO 1 -----------------------------------------------*

initspreadpointlogo1
	move.w	#numparticlessp-1,numparticles(a5)

	lea		logospreadpoint(pc),a0			;
	move.l	b_particles(pc),a1				;
		
	clr.w	mtrigger(a5)					;
	
	moveq	#spS-1,d7						; S
	bsr		initlogolettersp				;
		
	moveq	#spP1-1,d7						; P
	bsr		initlogolettersp				;

	moveq	#spR-1,d7						; R
	bsr		initlogolettersp				;

	moveq	#spE-1,d7						; E
	bsr		initlogolettersp				;

	moveq	#spAo-1,d7						; A
	bsr		initlogolettersp				;
	moveq	#spAi-1,d7						;
	bsr		initlogolettersp				;

	moveq	#spD-1,d7						; D
	bsr		initlogolettersp				;

	moveq	#spP2-1,d7						; P2
	bsr		initlogolettersp				;
	
	moveq	#spOo-1,d7						; O
	bsr		initlogolettersp				;
	moveq	#spOi-1,d7						;
	bsr		initlogolettersp				;

	moveq	#spI-1,d7						; I
	bsr		initlogolettersp				;

	moveq	#spN-1,d7						; N
	bsr		initlogolettersp				;

	moveq	#spT-1,d7						; T
	bsr		initlogolettersp				;
	rts										;


*------ INIT SPREADPOINT LOGO 2 -----------------------------------------------*

triggeraddsp2	equ	8

initspreadpointlogo2
	move.w	#numparticlessp-1,numparticles(a5)

	move.l	b_particles(pc),a1				;
		
	clr.w	temptrigger(a5)					;
	clr.w	mtrigger(a5)					;
	
	moveq	#spS-1,d7						; S
	lea		lspS(pc),a0						;
	bsr		initlogolettersp2				;
	add.w	#triggeraddsp2,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;
	
	moveq	#spP2-1,d7						; P2
	lea		lspP2(pc),a0					;
	bsr		initlogolettersp2				;
	add.w	#triggeraddsp2,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#spP1-1,d7						; P
	lea		lspP1(pc),a0					;
	bsr		initlogolettersp2				;
	add.w	#triggeraddsp2,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#spOo-1,d7						; O
	lea		lspOo(pc),a0					;
	bsr		initlogolettersp2				;
	moveq	#spOi-1,d7						;
	lea		lspOi(pc),a0					;
	bsr		initlogolettersp2				;
	add.w	#triggeraddsp2,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#spR-1,d7						; R
	lea		lspR(pc),a0						;
	bsr		initlogolettersp2				;
	add.w	#triggeraddsp2,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#spI-1,d7						; I
	lea		lspI(pc),a0						;
	bsr		initlogolettersp2				;
	add.w	#triggeraddsp2,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#spE-1,d7						; E
	lea		lspE(pc),a0						;
	bsr		initlogolettersp2				;
	add.w	#triggeraddsp2,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#spN-1,d7						; N
	lea		lspN(pc),a0						;
	bsr		initlogolettersp2				;
	add.w	#triggeraddsp2,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#spAo-1,d7						; A
	lea		lspAo(pc),a0					;
	bsr		initlogolettersp2				;
	moveq	#spAi-1,d7						;
	bsr		initlogolettersp2				;
	add.w	#triggeraddsp2,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#spT-1,d7						; T
	lea		lspT(pc),a0						;
	bsr		initlogolettersp2				;
	add.w	#triggeraddsp2,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#spD-1,d7						; D
	lea		lspD(pc),a0						;
	bsr		initlogolettersp2				;
	rts										;


*------ INIT ATARI LOGO EFFECT ------------------------------------------------*

initatarilogo
	move.w	#numparticlessp-1,numparticles(a5)

	lea		logoatari(pc),a0				; /|\
	move.l	b_particles(pc),a1				;
	clr.w	mtrigger(a5)					;
	
	moveq	#pAl-1,d7						; /
	bsr		initlogoletteratari				;
		
	moveq	#pAm-1,d7						; |
	bsr		initlogoletteratari				;

	moveq	#pAr-1,d7						; \
	bsr		initlogoletteratari				;
	rts										;


*------ INIT LOGO LETTER ATARI ------------------------------------------------*

initlogoletteratari
.loop
	movem.w	(a0),d0-d3			; d0=p1x d1=p1y d2=p2x d3=p2y
	
	move.w	d0,d4				; p1x
	add.w	d2,d4				; p2x
	asr.w	#1,d4				; d4 = focusX = (p1x+p2x)/2
	sub.w	d4,d0				; p1x-focusX
	move.w	d0,p1x(a1)			;
	sub.w	d4,d2				; p2x-focusX
	move.w	d2,p2x(a1)			;
	move.w	d4,tx(a1)			;
	asr.w	#3,d4 				;
	move.w	d4,dtx(a1)			;

	move.w	d1,d4				; p1y
	add.w	d3,d4				; p2y
	asr.w	#1,d4				; d4 = focusY = (p1y+p2y)/2
	sub.w	d4,d1				; p1y-focusY
	move.w	d1,p1y(a1)			;
	sub.w	d4,d3				; p2y-focusY
	move.w	d3,p2y(a1)			;
	move.w	d4,ty(a1)			;
	asr.w	#3,d4 				;
	move.w	d4,dty(a1)			;
	
	clr.w	p1z(a1)				;
	clr.w	p2z(a1)				;

	move.w	#-50,tz(a1)			;
	clr.w	dtz(a1)				;

	clr.w	a(a1)				;
	clr.w	b(a1)				;
	clr.w	c(a1)				;
	
	add.w	d2,d3				; "random" number
	and.w	#%1110,d3			; 2 - 14
	move.w	.variationdb(pc,d3.w),da(a1)

	add.w	d5,d0				; "random" number
	and.w	#%1110,d0			; 2 - 14
	move.w	.variationdb(pc,d0.w),db(a1)

	add.w	d7,d4				; "random" number
	and.w	#%1110,d4			; 2 - 14
	move.w	.variationdc(pc,d4.w),dc(a1)
	
	move.w	mtrigger(a5),trigger(a1)
	move.w	#120,ttl(a1)		;
	
;	addq.w	#1,mtrigger(a5)		;
	
	addq.l	#2*2,a0				; skip p1
	add.w	#particlesize,a1	;
	dbf		d7,.loop			;

	addq.l	#2*2,a0				; go to next letter
	rts							;

.variationdb
	dc.w	0,-14,16,-18,20,18,-12,10
.variationdc
	dc.w	0,-14,14,-16,16,20,-20,18


*------ INIT LOGO LETTER SPREADPOINT ------------------------------------------*

initlogolettersp
.loop
	movem.w	(a0),d0-d3			; d0=p1x d1=p1y d2=p2x d3=p2y
	
	move.w	d0,d4				; p1x
	add.w	d2,d4				; p2x
	asr.w	#1,d4				; d4 = focusX = (p1x+p2x)/2
	sub.w	d4,d0				; p1x-focusX
	move.w	d0,p1x(a1)			;
	sub.w	d4,d2				; p2x-focusX
	move.w	d2,p2x(a1)			;
	move.w	d4,tx(a1)			;
	asr.w	#4,d4 				;
	move.w	d4,dtx(a1)			;

	move.w	d1,d4				; p1y
	add.w	d3,d4				; p2y
	asr.w	#1,d4				; d4 = focusY = (p1y+p2y)/2
	sub.w	d4,d1				; p1y-focusY
	move.w	d1,p1y(a1)			;
	sub.w	d4,d3				; p2y-focusY
	move.w	d3,p2y(a1)			;
	move.w	d4,ty(a1)			;
	asr.w	#4,d4 				;
	move.w	d4,dty(a1)			;
	
	clr.w	p1z(a1)				;
	clr.w	p2z(a1)				;
	clr.w	tz(a1)				;
	move.w	#1,dtz(a1)			;
	
	clr.w	a(a1)				;
	clr.w	b(a1)				;
	clr.w	c(a1)				;
	
	clr.w	da(a1)				;
	clr.w	db(a1)				;

	add.w	d7,d4				; "random" number
	and.w	#%1110,d4			; 2 - 14
	move.w	.variationdc(pc,d4.w),dc(a1)
	
	move.w	mtrigger(a5),trigger(a1)
	move.w	#120,ttl(a1)		;
	
	addq.l	#2*2,a0				; skip p1
	add.w	#particlesize,a1	;
	dbf		d7,.loop			;

	addq.l	#2*2,a0				; go to next letter
	rts							;

.variationdc
	dc.w	0,-24,28,-34,32,26,-32,30


*------ INIT LOGO LETTER SPREADPOINT 2 ----------------------------------------*

initlogolettersp2
.loop
	movem.w	(a0),d0-d3			; d0=p1x d1=p1y d2=p2x d3=p2y
	
	move.w	d0,d4				; p1x
	add.w	d2,d4				; p2x
	asr.w	#1,d4				; d4 = focusX = (p1x+p2x)/2
	sub.w	d4,d0				; p1x-focusX
	move.w	d0,p1x(a1)			;
	sub.w	d4,d2				; p2x-focusX
	move.w	d2,p2x(a1)			;
	move.w	d4,tx(a1)			;
	asr.w	#4,d4 				;
	move.w	d4,dtx(a1)			;

	move.w	d1,d4				; p1y
	add.w	d3,d4				; p2y
	asr.w	#1,d4				; d4 = focusY = (p1y+p2y)/2
	sub.w	d4,d1				; p1y-focusY
	move.w	d1,p1y(a1)			;
	sub.w	d4,d3				; p2y-focusY
	move.w	d3,p2y(a1)			;
	move.w	d4,ty(a1)			;
	asr.w	#4,d4 				;
	move.w	d4,dty(a1)			;
	
	clr.w	p1z(a1)				;
	clr.w	p2z(a1)				;
	clr.w	tz(a1)				;
	move.w	#1,dtz(a1)			;
	
	clr.w	a(a1)				;
	clr.w	b(a1)				;
	clr.w	c(a1)				;
	
	clr.w	da(a1)				;
	clr.w	db(a1)				;

	add.w	d7,d4				; "random" number
	and.w	#%1110,d4			; 2 - 14
	move.w	.variationdc(pc,d4.w),dc(a1)
	
	move.w	mtrigger(a5),trigger(a1)
	move.w	#120,ttl(a1)		;
	
	addq.l	#2*2,a0				; skip p1
	add.w	#particlesize,a1	;
	dbf		d7,.loop			;

	addq.l	#2*2,a0				; go to next letter
	rts							;

.variationdc
	dc.w	0,-14,14,-16,16,20,-20,18


*------ INIT SCA LOGO EFFECT --------------------------------------------------*

triggeraddsca	equ	12

initscalogo
	move.w	#numparticlessca-1,numparticles(a5)

	lea		logosca(pc),a0					;
	move.l	b_particles(pc),a1				;
		
	clr.w	temptrigger(a5)					;
	move.w	temptrigger(a5),mtrigger(a5)	;
	
	moveq	#scaS-1,d7						; S
	bsr		initlogolettersca				;	
	add.w	#triggeraddsca,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#scaC-1,d7						; C
	bsr		initlogolettersca				;	
	add.w	#triggeraddsca,temptrigger(a5)	;
	move.w	temptrigger(a5),mtrigger(a5)	;

	moveq	#scaAi-1,d7						; A
	bsr		initlogolettersca				;	
	moveq	#scaAo-1,d7						;
	bsr		initlogolettersca				;	
	rts										;


*------ INIT LOGO LETTER SCA --------------------------------------------------*

initlogolettersca
.loop
	movem.w	(a0),d0-d3			; d0=p1x d1=p1y d2=p2x d3=p2y
	
	moveq	#0,d4				;

	move.w	d0,d4				; p1x
	add.w	d2,d4				; p2x
	asr.w	#1,d4				; d4 = focusX = (p1x+p2x)/2
	sub.w	d4,d0				; p1x-focusX
	move.w	d0,p1x(a1)			;
	sub.w	d4,d2				; p2x-focusX
	move.w	d2,p2x(a1)			;
	move.w	d4,tx(a1)			;
	asr.w	#4,d4 				;
	move.w	d4,dtx(a1)			;

	move.w	d1,d4				; p1y
	add.w	d3,d4				; p2y
	asr.w	#1,d4				; d4 = focusY = (p1y+p2y)/2
	sub.w	d4,d1				; p1y-focusY
	move.w	d1,p1y(a1)			;
	sub.w	d4,d3				; p2y-focusY
	move.w	d3,p2y(a1)			;
	move.w	d4,ty(a1)			;
	asr.w	#4,d4 				;
	move.w	d4,dty(a1)			;
	
	clr.w	p1z(a1)				;
	clr.w	p2z(a1)				;
	clr.w	tz(a1)				;
	move.w	#1,dtz(a1)			;
	
	clr.w	a(a1)				;
	clr.w	b(a1)				;
	clr.w	c(a1)				;
	
	clr.w	da(a1)

	add.w	d5,d0				; "random" number
	and.w	#%1110,d0			; 2 - 14
	move.w	.variationdb(pc,d0.w),db(a1)

	add.w	d7,d4				; "random" number
	and.w	#%1110,d4			; 2 - 14
	move.w	.variationdc(pc,d4.w),dc(a1)	
	
	move.w	mtrigger(a5),trigger(a1)
	move.w	#120,ttl(a1)		;

	addq.l	#2*2,a0				; skip p1
	add.w	#particlesize,a1	;
	dbf		d7,.loop			;

	addq.l	#2*2,a0				; go to next letter
	rts							;

.variationdb
	dc.w	0,2,4,6,2,2,4,4
.variationdc
	dc.w	0,-24,28,-34,32,26,-32,30


*------ INIT SCA LOGO EFFECT 2 ------------------------------------------------*

initscalogo2
	move.w	#numparticlessca-1,numparticles(a5)
	lea		logoscareversed(pc),a0			;
	move.l	b_particles(pc),a1				;
	clr.w	mtrigger(a5)					;

	moveq	#scaAo-1,d7						; A
	bsr		initlogolettersca2				;	
	moveq	#scaAi-1,d7						;
	bsr		initlogolettersca2				;	

	moveq	#scaC-1,d7						; C
	bsr		initlogolettersca2				;	
	
	moveq	#scaS-1,d7						; S
	bsr		initlogolettersca2				;	
	rts										;


*------ INIT LOGO LETTER SCA 2 ------------------------------------------------*

initlogolettersca2
.loop
	movem.w	(a0),d0-d3			; d0=p1x d1=p1y d2=p2x d3=p2y
	
	moveq	#-1,d4				;

	move.w	d0,d4				; p1x
	add.w	d2,d4				; p2x
	asr.w	#1,d4				; d4 = focusX = (p1x+p2x)/2
	sub.w	d4,d0				; p1x-focusX
	move.w	d0,p1x(a1)			;
	sub.w	d4,d2				; p2x-focusX
	move.w	d2,p2x(a1)			;
	move.w	d4,tx(a1)			;
	asr.w	#4,d4 				;
	move.w	d4,dtx(a1)			;

	move.w	d1,d4				; p1y
	add.w	d3,d4				; p2y
	asr.w	#1,d4				; d4 = focusY = (p1y+p2y)/2
	sub.w	d4,d1				; p1y-focusY
	move.w	d1,p1y(a1)			;
	sub.w	d4,d3				; p2y-focusY
	move.w	d3,p2y(a1)			;
	move.w	d4,ty(a1)			;
	asr.w	#4,d4 				;
	move.w	d4,dty(a1)			;
	
	clr.w	p1z(a1)				;
	clr.w	p2z(a1)				;
	clr.w	tz(a1)				;
	move.w	#1,dtz(a1)			;
	
	clr.w	a(a1)				;
	clr.w	b(a1)				;
	clr.w	c(a1)				;
	
	add.w	d2,d3				; "random" number
	and.w	#%1110,d3			; 2 - 14
	move.w	.variationdb(pc,d3.w),da(a1)

	add.w	d5,d0				; "random" number
	and.w	#%1110,d0			; 2 - 14
	move.w	.variationdb(pc,d0.w),db(a1)

	add.w	d7,d4				; "random" number
	and.w	#%1110,d4			; 2 - 14
	move.w	.variationdc(pc,d4.w),dc(a1)	
	
	move.w	mtrigger(a5),trigger(a1)
	addq.w	#1,mtrigger(a5)		;
	move.w	#120,ttl(a1)		;
	
	subq.l	#2*2,a0				; skip p1 (reverse)
	add.w	#particlesize,a1	;
	dbf		d7,.loop			;

	subq.l	#2*2,a0				; go to next letter (reverse)
	rts							;

.variationdb
	dc.w	0,2,4,6,-2,2,-4,4
.variationdc
	dc.w	0,-24,28,-34,32,26,-32,30


	rsreset
p1x		rs.w	1
p1y		rs.w	1
p1z		rs.w	1
p2x		rs.w	1
p2y		rs.w	1
p2z		rs.w	1

tx		rs.w	1
ty		rs.w	1
tz		rs.w	1
dtx		rs.w	1
dty		rs.w	1
dtz		rs.w	1

a		rs.w	1
b		rs.w	1
c		rs.w	1
da		rs.w	1
db		rs.w	1
dc		rs.w	1

ttl		rs.w	1
trigger	rs.w	1

particlesize	rs.w	0


*------ PARTICLE SYSTEM -------------------------------------------------------*

particlesystem
	clr.w	mframe(a5)			;
    clr.w	mtrigger(a5)		;

	move.l	b_lines2d(pc),a4	;
.mainloop
	move.l	a4,a3				;
	clr.w	(a4)+				; num lines

	move.l	b_particles(pc),a0	;
	move.w	numparticles(a5),d7	; (-1 for dbf is already done)
.loop
	tst.w	ttl(a0)				; dead particle?
	beq		.nextparticle		;

	lea		a(a0),a2			; angles
	movem.l	a0/a3/d7,-(a7)		;
	bsr		mtx					;
	movem.l	(a7)+,a0/a3/d7		;
	
	movem.w	p1x(a0),d1-d3		; p1
	bsr		applymtxandcoh		;
	tst.w	d6					; Cohen-Sutherland
	beq		.p1inside			;
	clr.w	ttl(a0)				; die
	bra		.nextparticle		;

.p1inside
	move.w	d4,(a4)+			; x1
	moveq	#-1,d0				; = 255
	sub.w	d5,d0				;
	move.w	d0,(a4)+			; y1

	movem.w	p2x(a0),d1-d3		; p2
	bsr		applymtxandcoh		;
	tst.w	d6					; Cohen-Sutherland
	beq		.p2inside			;
	clr.w	ttl(a0)				; die
	subq.l	#4,a4				; get rid of x1 and y1 (was 3)
	bra		.nextparticle		;
	
.p2inside
	move.w	d4,(a4)+			; x2
	moveq	#-1,d0				; = 255
	sub.w	d5,d0				;
	move.w	d0,(a4)+			; y2
	
	addq.w	#1,(a3)				; one line more in list

	move.w	mframe(a5),d0		; if frameNumber >= data[d+18] 
	cmp.w	trigger(a0),d0		;
	blo		.notyetalive		;

	move.w	dtx(a0),d0			; update particle
	add.w	d0,tx(a0)			;
	move.w	dty(a0),d0			;
	add.w	d0,ty(a0)			;
	move.w	dtz(a0),d0			;
	add.w	d0,tz(a0)			;
	move.w	da(a0),d0			;
	add.w	d0,a(a0)			;
	move.w	db(a0),d0			;
	add.w	d0,b(a0)			;
	move.w	dc(a0),d0			;
	add.w	d0,c(a0)			;
	subq.w	#1,ttl(a0)			;

.notyetalive
.nextparticle
	add.w	#particlesize,a0	; next particle
	dbf		d7,.loop			;

	if testing
	move.l	clistbase(pc),a1	;
	add.w	#precalccolor-clist,a1	;
	move.w	mframe(a5),(a1)		;
	endif

	addq.w	#1,mframe(a5)		; next frame

	tst.w	doquit(a5)			;
	bne		.quit				;
	tst.w	(a3)				; any particles alive in this frame?
	bne		.mainloop			;

	move.l	a4,mdataend(a5)		;
	clr.l	(a4)+				; empty (no lines) last frame
	
	if numbers&testing
	move.l	b_lines2d(pc),a1	;
	sub.l	a1,a4				;
	move.l	a4,v_number2(a5)	;
	endif
.quit
	rts							;

; a0 = particle
; d1 = x
; d2 = y
; d3 = z

; must not trash a0,a3,a4,a5,d7
applymtxandcoh
	lea		matrix(a5),a1		;
	
	moveq	#0,d4				;
	move.w	d1,d4				;
	muls	(a1)+,d4			; 16 * 16 -> 32

	move.w	d2,d5				;
	muls	(a1)+,d5			;
	add.l	d5,d4				;

	move.w	d3,d5				;
	muls	(a1)+,d5			; 
	add.l	d5,d4				;

	move.w	d1,d5				;
	muls	(a1)+,d5			;
	
	move.w	d2,d6				;
	muls	(a1)+,d6			;
	add.l	d6,d5				;
			
	move.w	d3,d6				;
	
	muls	(a1)+,d6			;
	add.l	d6,d5				;
	muls	(a1)+,d1			;
	muls	(a1)+,d2			;
	muls	(a1)+,d3			;
	add.l	d1,d3				;
	add.l	d2,d3				;
	
	asr.l	#8,d4				;
	asr.l	#8,d5				;
	asr.l	#8,d3				;

	move.w	tx(a0),d6			;
	ext.l	d6					;
	add.l	d6,d4				;

	move.w	ty(a0),d6			;
	ext.l	d6					;
	add.l	d6,d5				;

	move.w	tz(a0),d6			;
	ext.l	d6					;
	add.l	d6,d3				;
	
	asr.l	#8,d3				; .w
	addq.l	#3,d3				;

	divs	d3,d4				; x
	divs	d3,d5				; y
;	rts							; fall through


*------ COHEN SUTHERLAND ALGORITHM --------------------------------------------*

xmin 	equ	-255	; -255 ; -159  320+192+192=704 / 2 = 352
xmax 	equ	255  	; 255 ; 159
ymin	equ	-255	; -127
ymax	equ	255		; 127

cinside	equ	0
cleft	equ	1
cright	equ	2
cbottom	equ	4
ctop	equ	8

cohensutherland
	moveq	#cinside,d6			;
		
	cmp.w	#xmin,d4			; if x < xmin { code |= left }
	bge		.checkright			; xmin >= x?
	moveq	#cleft,d6			;
    bra		.checkbottom		;
        
.checkright
	cmp.w	#xmax,d4			; else if x > xmax { code |= right }
	ble		.checkbottom		; xmax <= x?
	addq.l	#cright,d6			;

.checkbottom
	cmp.w	#ymin,d5			; if y < ymin { code |= bottom }
	bge		.checktop			; ymin >= y?
	addq.l	#cbottom,d6			;
	rts							; done
.checktop
	cmp.w	#ymax,d5			; else if y > ymax { code |= top }
	ble		.done				; ymax <= y?
	addq.l	#ctop,d6			;
.done
	rts							;


*------ PRINT NUMBER ----------------------------------------------------------*

; d0.l: number, d1.w: pos
    if numbers
printnumber
	move.l	dbplanes(a5),a0			;
    add.w   d1,a0           		;

    moveq   #8-1,d7                 ;
.loop
    move.w  d0,d1                   ; number
    and.w   #$000f,d1               ; mask digit out
    asl.w   #3,d1                   ; offset to font data
    lea     .digits(pc,d1.w),a1		;
    move.b  (a1)+,(a0)              ; print digit
    move.b  (a1)+,pwidth(a0)        ;
    move.b  (a1)+,2*pwidth(a0)      ;
    move.b  (a1)+,3*pwidth(a0)      ;
    move.b  (a1)+,4*pwidth(a0)      ;
    asr.l   #4,d0                   ; next digit
    subq.w  #1,a0                   ; next x position
    dbf     d7,.loop                ;
    rts                             ;

.digits
    dc.b    %11111000               ; 0
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %00100000               ; 1
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    ds.b    3,0
    
    dc.b    %11111000               ; 2
    dc.b    %00001000
    dc.b    %11111000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 3
    dc.b    %00001000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %10001000               ; 4
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %00001000
    ds.b    3,0

    dc.b    %11111000               ; 5
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 6
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 7
    dc.b    %00001000
    dc.b    %00010000
    dc.b    %00100000
    dc.b    %00100000
    ds.b    3,0

    dc.b    %11111000               ; 8
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 9
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; A
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %10001000
    dc.b    %10001000
    ds.b    3,0

    dc.b    %11110000               ; B
    dc.b    %10001000
    dc.b    %11110000
    dc.b    %10001000
    dc.b    %11110000
    ds.b    3,0

    dc.b    %11111000               ; C
    dc.b    %10000000
    dc.b    %10000000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11110000               ; D
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %11110000
    ds.b    3,0

    dc.b    %11111000               ; E
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; F
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %10000000
    dc.b    %10000000
    ds.b    3,0
    
    even
    endif


*------	LAME TEXT SCROLLER (LTS) -----------------------------------------*

stheight	equ	6
invis		equ 20

scrolltext
	movem.l	dbplanes3(a5),a0/a1		; a0 draw to, a1 is shown
	move.l	a0,a2					;
	add.w	#stheight*pwidth-invis-2,a0	;
	add.w	#stheight*pwidth-invis-2,a1	;
	moveq	#0,d0					;
	bsr		waitblitter				;
	move.l	a1,$50(a6)				; source A
	move.l	a0,$54(a6)				; destination D
	move.l	#invis<<16+invis,$64(a6); modulo A/D
	move.l	#$ff00ffff,$44(a6)		; first/last word mask (clears crap)

	move.l	#$29f00002,$40(a6)		; was 3
	move.w	#stheight<<6+(pwidth-invis)>>1,$58(a6);

	subq.w	#1,v_chardelay(a5)		;
	tst.w	v_chardelay(a5)			;
	bpl		.noprint				;

	add.w	#41,a2					; print outside of visible area
	moveq	#0,d1					;
	lea		.text(pc),a0			;
	add.w	v_textpos(a5),a0		;
	addq.w	#1,v_textpos(a5)		;
	move.b	(a0),d1					;
	bpl		.notend					;
	clr.w	v_textpos(a5)			;
	moveq	#' ',d1					; put a space at the end
.notend
	asl.w   #3,d1                   ; offset to font data
	lea		ltsfont-(' '*8)(pc),a0	;
    add.w	d1,a0					;
	bsr		waitblitter				;
    move.b  (a0)+,0*pwidth(a2)		; print char
    move.b  (a0)+,1*pwidth(a2)		;
    move.b  (a0)+,2*pwidth(a2)      ;
    move.b  (a0)+,3*pwidth(a2)      ;
    move.b  (a0)+,4*pwidth(a2)      ;
    move.b  (a0)+,5*pwidth(a2)      ;
   	move.w	#4,v_chardelay(a5)		; next char after this delay (frames) was 3
.noprint
	rts

.text
;	dc.b	" !"
;	dc.b	34,"#$%&'()*+,-./0123456789"
	
	dc.b	"THE MEGA-MIGHTY SWISS CRACKING ASSOCIATION AND SPREADPOINT PRESENT "
	dc.b	"TERRA CRESTA"
	dc.b	"        * * *        "
	dc.b	"RELEASED AT POSADAS AMIGA PARTY 2023"
	dc.b	"        * * *        "
	dc.b	"ONCE UPON A TIME IN THE BUSTLING, NEON CITY OF AKIHABARA, TOKYO, THE SO-CALLED "
	dc.b	34,"ELECTRIC TOWN", 34, ", LIVED A YOUNG MAN NAMED KENJI WHO WAS CAPTIVATED BY THE WORLD "
	dc.b	"OF VINTAGE TECHNOLOGY, RETRO VIDEO GAMES, AND ARCANE COMPUTER SYSTEMS. "
	dc.b	"HE SPENT HIS DAYS ROAMING THE LABYRINTHINE ALLEYWAYS AND NARROW STREETS, "
	dc.b	"DIGGING THROUGH THE TREASURE TROVES OF ANTIQUE ELECTRONICS SHOPS FOR "
	dc.b	"THE HIDDEN GEMS OF YESTERYEAR."
	dc.b	"      "
	dc.b	"ONE SWELTERING SUMMER DAY, KENJI ENTERED A DUSTY, UNASSUMING SHOP SPECIALIZING "
	dc.b	"IN AMIGA COMPUTER PARTS AND MEMORABILIA. GUIDED BY HIS INTUITION, HE FOUND "
	dc.b	"HIMSELF DRAWN TO A DIMLY LIT CORNER AT THE BACK, WHERE HE DISCOVERED A "
	dc.b	"LARGE, TATTERED CARDBOARD BOX BURIED UNDER LAYERS OF FORGOTTEN COMPUTER PERIPHERALS."
	dc.b	"      "
	dc.b	"TO KENJI'S EXCITEMENT, THIS CARDBOARD RELIC WAS FILLED TO THE BRIM WITH OLD "
	dc.b	"AMIGA DISKETTES, EACH SUFFUSED WITH THE POTENTIAL TO HARBOR THE FORGOTTEN CODE "
	dc.b	"OF ANCIENT, PIXEL-BASED WORLDS. DRIVEN BY A MIX OF CURIOSITY AND A COLLECTOR'S "
	dc.b	"PASSION, KENJI PURCHASED THE ENTIRE CACHE OF DISKS WITHOUT HESITATION, "
	dc.b	"AND EAGERLY TOOK THEM BACK TO HIS OWN COLLECTION-FILLED APARTMENT."
	dc.b	"      "
	dc.b	"FOR YEARS, KENJI DIVED DEEP INTO THE REALMS OF EVERY DISKETTE HE FOUND "
	dc.b	"IN THAT ENIGMATIC BOX. HE CATALOGUED, EXPLORED, AND ENDEAVORED TO PRESERVE "
	dc.b	"EACH UNIQUE PIECE OF RETRO SOFTWARE AND GAMING HISTORY. BIT BY BIT, "
	dc.b	"HE PIECED TOGETHER THE VIVID, PIXELATED STORIES CONTAINED WITHIN THE DISKS, "
	dc.b	"BUT THERE WAS ALWAYS ONE PARTICULARLY STRANGE DISKETTE THAT REMAINED "
	dc.b	"UNREADABLE BY HIS AMIGA 1200. ITS SUBTLE DIFFERENCES FROM THE OTHERS "
	dc.b	"PIQUED HIS CURIOSITY AND KEPT IT AT THE BACK OF HIS MIND AS HE CONTINUED "
	dc.b	"TO UNEARTH THE CONTENTS OF THE OTHER DISKS."
	dc.b	"      "
	dc.b	"DESPERATE TO UNLOCK THE SECRETS HIDDEN WITHIN THE UNREADABLE DISKETTE, "
	dc.b	"KENJI SOUGHT OUT THE ADVICE OF AMIGA ENTHUSIASTS AND COLLECTORS ACROSS "
	dc.b	"THE INTERNET. THEY SHARED WITH HIM CRYPTIC TALES OF LONG-LOST PROTOTYPES, "
	dc.b	"RARE DEMOS, AND EVEN UNRELEASED GAMES THAT NEVER SAW THE LIGHT OF DAY."
	dc.b	"      "
	dc.b	"FUELED BY THIS NEWFOUND KNOWLEDGE, HE REDOUBLED HIS EFFORTS. HE TRIED "
	dc.b	"VARIOUS METHODS TO ACCESS THE MYSTERIOUS DISK - USING DIFFERENT DRIVES, "
	dc.b	"SOFTWARE, AND EVEN ATTEMPTING DATA RECOVERY. HIS PERSISTENCE PAID OFF "
	dc.b	"WHEN HE FINALLY MANAGED TO GET A GLIMMER OF SOMETHING ON HIS SCREEN, "
	dc.b	"A BOOTING SCREEN WITH AN ICONIC LOGO THAT TOOK HIS BREATH AWAY: "
	dc.b	"A BOLD, NEVER-SEEN-BEFORE AMIGA VERSION OF THE BELOVED CLASSIC GAME, "
	dc.b	"TERRA CRESTA."
	dc.b	"      "
	dc.b	"NOW PREPARE YOUR AMIGA 1200 TO START THE FIGHT AGAINST "
	dc.b	" [ \ ] ^ _ "
	dc.b	" SOON. IT WON'T BE LONG NOW."
	dc.b	"                                                "
	dc.b	"                                                "
	dc.b	-1


	rem

	dc.b	"CRACKED, TRAINED, FILED, AND CRUNCHED IN JULY 1987"
	dc.b	"      "
	dc.b	"      "
	dc.b	"      "
	dc.b	"THE FOLLOWING PRODUCTIONS BY SCA AND SPREADPOINT FROM 1987 ARE ALSO WORTH SEEING:      "
	dc.b	"SCA'S MIAMI VICE THEME"
	dc.b	"        ***        "
	dc.b	"MULTISCANNER"
	dc.b	"        ***        "
	dc.b	"KARATE KID II"
	dc.b	"        ***        "
	dc.b	"GOLDRUNNER"
	dc.b	"                  "

	erem

	
	

	even

ltsfont
    dc.b    %00000000               ; space
    dc.b    %00000000
    dc.b    %00000000
    dc.b    %00000000
    dc.b    %00000000
    ds.b    3,0

    dc.b    %00100000               ; !
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00000000
    dc.b    %00100000
    ds.b    3,0

    dc.b    %01010000               ; "
    dc.b    %01010000
    dc.b    %00000000
    dc.b    %00000000
    dc.b    %00000000
    ds.b    3,0

	ds.b	8,0						; #
	ds.b	8,0						; $
	ds.b	8,0						; %
	ds.b	8,0						; &

    dc.b    %00100000               ; '
    dc.b    %00100000
    dc.b    %00000000
    dc.b    %00000000
    dc.b    %00000000
    ds.b    3,0

    dc.b    %00111000               ; (
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00111000
    ds.b    3,0

    dc.b    %11100000               ; )
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %11100000
    ds.b    3,0

    dc.b    %10101000               ; *
    dc.b    %01110000
    dc.b    %11111000
    dc.b    %01110000
    dc.b    %10101000
    ds.b    3,0

    dc.b    %00100000               ; +
    dc.b    %00100000
    dc.b    %11111000
    dc.b    %00100000
    dc.b    %00100000
    ds.b    3,0

    dc.b    %00000000               ; ,
    dc.b    %00000000
    dc.b    %00000000
    dc.b    %00000000
    dc.b    %01000000
    dc.b    %10000000
    ds.b    2,0

    dc.b    %00000000               ; -
    dc.b    %00000000
    dc.b    %11111000
    dc.b    %00000000
    dc.b    %00000000
    ds.b    3,0

    dc.b    %00000000               ; .
    dc.b    %00000000
    dc.b    %00000000
    dc.b    %00000000
    dc.b    %00100000
    ds.b    3,0

    dc.b    %00001000               ; /
    dc.b    %00010000
    dc.b    %00100000
    dc.b    %01000000
    dc.b    %10000000
    ds.b    3,0

    dc.b    %11111000               ; 0
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %00100000               ; 1
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    ds.b    3,0
    
    dc.b    %11111000               ; 2
    dc.b    %00001000
    dc.b    %11111000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 3
    dc.b    %00001000
    dc.b    %01111000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %10001000               ; 4
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %00001000
    ds.b    3,0

    dc.b    %11111000               ; 5
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 6
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 7
    dc.b    %00001000
    dc.b    %00001000
    dc.b    %00001000
    dc.b    %00001000
    ds.b    3,0

    dc.b    %11111000               ; 8
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; 9
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %00000000               ; :
    dc.b    %00000000
    dc.b    %00100000
    dc.b    %00000000
    dc.b    %00100000
    ds.b    3,0

	ds.b	8,0						; ;
	ds.b	8,0						; <
	ds.b	8,0						; =
	ds.b	8,0						; >

    dc.b    %11111000               ; ?
    dc.b    %00001000
    dc.b    %00111000
    dc.b    %00000000
    dc.b    %00100000
    ds.b    3,0

    dc.b    %11111000               ; @
    dc.b    %10001000
    dc.b    %10111000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %01110000               ; A
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %10001000
    dc.b    %10001000
    ds.b    3,0

    dc.b    %11110000               ; B
    dc.b    %10001000
    dc.b    %11110000
    dc.b    %10001000
    dc.b    %11110000
    ds.b    3,0

    dc.b    %11111000               ; C
    dc.b    %10000000
    dc.b    %10000000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11110000               ; D
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %11110000
    ds.b    3,0

    dc.b    %11111000               ; E
    dc.b    %10000000
    dc.b    %11110000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; F
    dc.b    %10000000
    dc.b    %11110000
    dc.b    %10000000
    dc.b    %10000000
    ds.b    3,0

    dc.b    %11111000               ; G
    dc.b    %10000000
    dc.b    %10111000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %10001000               ; H
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %10001000
    dc.b    %10001000
    ds.b    3,0

    dc.b    %11111000               ; I
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %00001000               ; J
    dc.b    %00001000
    dc.b    %00001000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %10001000               ; K
    dc.b    %10010000
    dc.b    %11100000
    dc.b    %10010000
    dc.b    %10001000
    ds.b    3,0

    dc.b    %10000000               ; L
    dc.b    %10000000
    dc.b    %10000000
    dc.b    %10000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %10001000               ; M
    dc.b    %11011000
    dc.b    %10101000
    dc.b    %10001000
    dc.b    %10001000
    ds.b    3,0

    dc.b    %10001000               ; N
    dc.b    %11001000
    dc.b    %10101000
    dc.b    %10011000
    dc.b    %10001000
    ds.b    3,0

    dc.b    %11111000               ; O
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; P
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %10000000
    dc.b    %10000000
    ds.b    3,0

    dc.b    %11111000               ; Q
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %10101000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; R
    dc.b    %10001000
    dc.b    %11111000
    dc.b    %10010000
    dc.b    %10001000
    ds.b    3,0

    dc.b    %11111000               ; S
    dc.b    %10000000
    dc.b    %11111000
    dc.b    %00001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111000               ; T
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    ds.b    3,0

    dc.b    %10001000               ; U
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %10001000               ; V
    dc.b    %10001000
    dc.b    %10001000
    dc.b    %01010000
    dc.b    %00100000
    ds.b    3,0

    dc.b    %10001000               ; W
    dc.b    %10001000
    dc.b    %10101000
    dc.b    %11011000
    dc.b    %10001000
    ds.b    3,0

    dc.b    %10001000               ; X
    dc.b    %01010000
    dc.b    %00100000
    dc.b    %01010000
    dc.b    %10001000
    ds.b    3,0

    dc.b    %10001000               ; Y
    dc.b    %01010000
    dc.b    %00100000
    dc.b    %00100000
    dc.b    %00100000
    ds.b    3,0

    dc.b    %11111000               ; Z
    dc.b    %00010000
    dc.b    %00100000
    dc.b    %01000000
    dc.b    %11111000
    ds.b    3,0

    dc.b    %11111111               ; [ J1
    dc.b    %00000010
    dc.b    %00100100
    dc.b    %00011000
    dc.b    %00001000
    dc.b    %00000100
    ds.b    2,0

    dc.b    %10000000               ; \ J2
    dc.b    %01000010
    dc.b    %00100010
    dc.b    %00001100
    dc.b    %00110000
    dc.b    %11000000
    ds.b    2,0

    dc.b    %10010100               ; ] J3
    dc.b    %10001010
    dc.b    %11100000
    dc.b    %10011000
    dc.b    %10000110
    dc.b    %10000000
    ds.b    2,0


    dc.b    %01111100               ; ^ J4
    dc.b    %00000000
    dc.b    %11111110
    dc.b    %00000100
    dc.b    %00001000
    dc.b    %00110000
    ds.b    2,0

    dc.b    %00000000               ; _ J5
    dc.b    %00000000
    dc.b    %00000000
    dc.b    %11111110
    dc.b    %00000000
    dc.b    %00000000
    ds.b    2,0


*------	DRAW EARTH -------------------------------------------------------*

drawearth
	bsr		waitblitter				;

	move.w	#pwidth,$60(a6)			;
	move.w	#pwidth,$66(a6)			;
	move.l	#$ffff8000,$72(a6)		; texture data/index
	move.w	#$8000,$44(a6)			; first word mask

	move.l	v_staticplane2(a5),a0	;	
	add.l	#center,a0				;

	moveq	#0,d6					; angle
	move.w	#256-1,d7				; num steps
.loop
	move.l	b_sintab(pc),a2			; sin
	lea		512(a2),a3				; cos

	move.w	(a3,d6.w),d0			; x1
	move.w	(a2,d6.w),d1			; y1
	addq.w	#8,d6					; 256 (steps) * 8 = 2048 (full circle)
	and.w	#$07ff,d6				;
	move.w	(a3,d6.w),d2			; x2
	move.w	(a2,d6.w),d3			; y2
	
	asr.w	#2,d0					;
	asr.w	#2,d1					;
	asr.w	#2,d2					;
	asr.w	#2,d3					;

	add.w	#30,d1					; move down
	add.w	#30,d3					;

	movem.l	d6-d7,-(a7)				;
	moveq	#0,d5					; don't loop
	lea		$52(a6),a6				;
	bsr		drawline				; a0 bitplane, d0-d3 coords
	movem.l	(a7)+,d6-d7				;
	dbf		d7,.loop				;
	rts								;


*------	FILL BELOW EARTH -------------------------------------------------*

filly	equ	176
fillbelowearth
	move.l	v_staticplane2(a5),a0	;
	add.w	#filly*pwidth,a0		;
	move.w	#pheight-filly-1,d7		;
	moveq	#-1,d0					;
.loop
	rept 10
	move.l	d0,(a0)+				; fill 10*4 bytes
	endr
	add.w	#inviswidth,a0			; no need to fill invisible space
	dbf		d7,.loop				;
	rts								;


*------	CLEAR BELOW EARTH ------------------------------------------------*

clearbelowearth
	move.l	v_staticplane2(a5),a0	;
	add.w	#filly*pwidth,a0		;
	move.w	#pheight-filly-1,d7		;
	moveq	#0,d0					;
.loop
	rept 10
	move.l	d0,(a0)+				; fill pwidth (10*4 bytes)
	endr
	add.w	#inviswidth,a0			; no need to fill invisible space
	dbf		d7,.loop				;
	rts								;


*------	DRAW VIPER -------------------------------------------------------*

; a0 bitplane
drawviper
	bsr		waitblitter				;
	move.w	#pwidth,$60(a6)			;
	move.w	#pwidth,$66(a6)			;
	move.l	#$ffff8000,$72(a6)		; texture data/index
	move.w	#$8000,$44(a6)			; first word mask

	lea		viperlines(pc),a2		;
	move.l	b_viper2d(pc),a1		;
	moveq	#numviperlines-numviperlineslasers-1,d7		;
	add.w	extralines(a5),d7		;
.loop
	movem.w	(a2),d5/d6				; i1, i2
	movem.w	(a1,d5.w),d0/d1			;
	movem.w	(a1,d6.w),d2/d3			;

	move.l	d7,-(a7)				;
	moveq	#0,d5					; don't loop
	lea		$52(a6),a6				;
	bsr		drawline				; a0 bitplane, d0-d3 coords
	move.l	(a7)+,d7				;
	
	addq.l	#4,a2					;
	dbf		d7,.loop				;
	rts								;


	rem
*------	TEXTER -----------------------------------------------------------*

; a0 bitplane
drawtext
	bsr		waitblitter				;
	move.w	#pwidth,$60(a6)			;
	move.w	#pwidth,$66(a6)			;
	move.l	#$ffff8000,$72(a6)		; texture data/index
	move.w	#$8000,$44(a6)			; first word mask

	lea		text(pc),a1				;
	moveq	#textend-text-1,d7		;
	move.w	#-160+44+3,d5			; x pos
	moveq	#-100,d4				;
.loop
	moveq	#0,d0					;
	move.b	(a1)+,d0				;
	lea		fontoffsets-'A'(pc),a2	; indices
	move.b	(a2,d0),d0				;
	lea		font(pc),a2				;	
	add.l	d0,a2					;

.lineloop
	moveq	#0,d6					;
	move.b	(a2)+,d6				;
	bmi		.nextletter				;

.line
	move.l	b_font2d(pc),a3			;
	add.w	#22*4*numfont3dvertices,a3
	lea		font3d2(pc),a3
	

	moveq	#0,d3					;
	move.b	(a2)+,d3				; start index
	movem.w	(a3,d3.w),d0/d1			; x1 y1
	add.w	d5,d0					; add x pos (delta)
	add.w	d4,d1					; add y pos (delta)
	
	move.b	(a2),d3					; end index -> becomes new start index (no +)
	movem.w	(a3,d3.w),d2/d3			; x2 y2
	add.w	d5,d2					; x pos
	add.w	d4,d3					;

	movem.l	a0-a2/d4-d7,-(a7)		;
	moveq	#0,d5					; don't loop	
	lea		$52(a6),a6				;
	bsr		drawline				; a0 bitplane, d0-d3 coords
	movem.l	(a7)+,a0-a2/d4-d7		;
	dbf		d6,.line				;

	addq.l	#1,a2					; skip end index
	bra		.lineloop				;

.nextletter
	add.w	#28,d5					; unitx=8  32
	cmp.w	#140,d5					;
	blt		.noy					;
	move.w	#-150,d5				; x pos
		
	add.w	#5*unity,d4				;
.noy
	dbf		d7,.loop				;
	rts								;



nitx	equ	2  ; 4
nity	equ	1  ; 3

font3d2	; z = 0
	dc.w	-nitx,	-2*nity
	dc.w	0,		-2*nity
	dc.w	nitx,	-2*nity

	dc.w	-nitx,	-nity  ;-2*1 ; 2
	dc.w	0,		-nity  ;-2*1 ; 2
	dc.w	nitx,	-nity  ;-2*1 ; 2

	dc.w	-nitx,	0
	dc.w	0,		0
	dc.w	nitx,	0

	dc.w	-nitx,	nity  ;2*1 ; 2
	dc.w	0,		nity  ;2*1 ; 2
	dc.w	nitx,	nity  ;2*1 ; 2

	dc.w	-nitx,	2*nity
	dc.w	0,		2*nity
	dc.w	nitx,	2*nity

	dc.w	nitx/2,	0		; used for letters B,E,F
	
	dc.w	-nitx-1,	2*nity	; 16
	dc.w	0-1,		2*nity	; 17
	dc.w	nitx-1,	2*nity	; 18

	erem


*------	DRAW TEXT PARTICLES ----------------------------------------------*

; a0 bitplane
drawtextparticles
	bsr		waitblitter				;
	move.w	#pwidth,$60(a6)			;
	move.w	#pwidth,$66(a6)			;
	move.l	#$ffff8000,$72(a6)		; texture data/index
	move.w	#$8000,$44(a6)			; first word mask

	move.l	a0,a1					;

	move.w	v_textpindex(a5),d0		;
	lea		textparticles(pc),a3	;
	lea		base(pc),a2				;
	add.l	(a3,d0.w),a2			;
	move.w	4(a3,d0.w),d7			;

.loop
	tst.w	c_trigger(a2)			;
	beq		.active					;
	subq.w	#1,c_trigger(a2)		;
	bra		.waiting				;

.active
	moveq	#0,d0					; important
	move.w	c_char(a2),d0			;
	move.w	c_xpos(a2),d5			;
	move.w	c_ypos(a2),d4			;

	movem.l	a1-a2/d7,-(a7)			;
	move.l	a1,a0					; active (bright) plane

	move.w	c_ttl(a2),d6			;
	cmp.w	#-3,d6					;
	beq		.dead					;

	cmp.w	#-2,d6					;
	beq		.drawtootherplayfield	;
	cmp.w	#-1,d6					;
	bne		.stillanimating			;
.drawtootherplayfield
	move.l	dbplanes2(a5),a0		; active (bright) plane 2
	add.l	#center,a0				;
	moveq	#0,d6					; draw final position again (in uncleared playfield)
.stillanimating
	add.w	d6,d6					; make offset
	move.l	b_font2d(pc),a3			;
	lea		foffsets(pc),a4			;
	add.w	(a4,d6.w),a3			;	
	subq.w	#1,c_ttl(a2)			;
	bsr		drawchar				; trashes many registers
.dead
	movem.l	(a7)+,a1-a2/d7			;
.waiting
	add.w	#c_size,a2				; next char/text particle
	dbf		d7,.loop				;
	rts								;

drawchar
	lea		fontoffsets-'A'(pc),a2	; indices
	move.b	(a2,d0.w),d0			;
	lea		font(pc),a2				;	
	add.l	d0,a2					;

.lineloop
	moveq	#0,d6					;
	move.b	(a2)+,d6				;
	bmi		.done					;
.line
	moveq	#0,d3					;
	move.b	(a2)+,d3				; start index
	movem.w	(a3,d3.w),d0/d1			; x1 y1
	add.w	d5,d0					; add x pos (delta)
	add.w	d4,d1					; add y pos (delta)
	
	move.b	(a2),d3					; end index -> becomes new start index (no +)
	movem.w	(a3,d3.w),d2/d3			; x2 y2
	add.w	d5,d2					; add x pos (delta)
	add.w	d4,d3					; add y pos (delta)

	movem.l	a0-a3/d4-d6,-(a7)		;
	moveq	#0,d5					; don't loop in drawline
	lea		$52(a6),a6				;
	bsr		drawline				; a0 bitplane, d0-d3 coords
	movem.l	(a7)+,a0-a3/d4-d6		;
	dbf		d6,.line				;

	addq.l	#1,a2					; skip end index
	bra		.lineloop				;
.done
	rts								;

initfont
	lea		tempabc(a5),a0			; init angles
	clr.w	(a0)+					;
	move.w	#22*40,(a0)+			;
	clr.w	(a0)					;

	move.l	b_font2d(pc),a2			;
	moveq	#49,d3					; start z (big char)
;	moveq	#26,d3					; start z (big char)

	moveq	#23-1,d6				; 23 rotations
.loop
	movem.l	a0-a5/d0-d7,-(a7)		;
	lea		tempabc(a5),a2			; angles
;	move.w	2(a2),(a2)				; TESTING ONLY
	bsr		mtx						;
	sub.w	#40,tempabc+2(a5)		; alter angle
	movem.l	(a7)+,a0-a5/d0-d7		;

	lea		font3d(pc),a3			; coords
	moveq	#numfont3dvertices-1,d7	;
	movem.l	a0/d6,-(a7)				;
	bsr		applymtxfont			;
	movem.l	(a7)+,a0/d6				;
	
	add.w	#9,d3					; alter z so that 9*23 (=207) + 49 = 256
;	add.w	#10,d3					; alter z so that 10*23 (=230) + 26 = 256

	dbf		d6,.loop				;
	rts								;

foffsets
	dc.w	22*4*numfont3dvertices
	dc.w	21*4*numfont3dvertices
	dc.w	20*4*numfont3dvertices
	dc.w	19*4*numfont3dvertices
	dc.w	18*4*numfont3dvertices
	dc.w	17*4*numfont3dvertices
	dc.w	16*4*numfont3dvertices
	dc.w	15*4*numfont3dvertices
	dc.w	14*4*numfont3dvertices
	dc.w	13*4*numfont3dvertices
	dc.w	12*4*numfont3dvertices
	dc.w	11*4*numfont3dvertices
	dc.w	10*4*numfont3dvertices
	dc.w	9*4*numfont3dvertices
	dc.w	8*4*numfont3dvertices
	dc.w	7*4*numfont3dvertices
	dc.w	6*4*numfont3dvertices
	dc.w	5*4*numfont3dvertices
	dc.w	4*4*numfont3dvertices
	dc.w	3*4*numfont3dvertices
	dc.w	2*4*numfont3dvertices
	dc.w	1*4*numfont3dvertices
	dc.w	0*4*numfont3dvertices

	rsreset
c_char		rs.w	1
c_xpos		rs.w	1
c_ypos		rs.w	1
c_trigger	rs.w	1
c_ttl		rs.w	1
c_size		rs.w	0


;numtextparticles	equ	(textp1end-textp1)/c_size
cs		equ	28
ctr		equ	5

; weird centering start x offsets
cx3		equ	-160+8+(320-(2*cs+16))/2 ; 3 chars
cx5		equ	-160+8+(320-(4*cs+16))/2 ; 5 chars
cx6		equ	-160+8+(320-(5*cs+16))/2 ; 6 chars
cx7		equ	-160+8+(320-(6*cs+16))/2 ; 7 chars
cx8		equ	-160+8+(320-(7*cs+16))/2 ; 8 chars
cx9		equ	-160+8+(320-(8*cs+16))/2 ; 9 chars
cx11	equ	-160+8+(320-(10*cs+16))/2 ; 11 chars

; max 4 lines of text on screen
cy1even 	equ	-80
cy2even 	equ	cy1even+50
cy3even 	equ	cy2even+50
cy4even 	equ	cy3even+50

; max 3 lines of text on screen
cy1odd		equ	-80+(50/2)
cy2odd	 	equ	cy1odd+50
cy3odd	 	equ	cy2odd+50

textp1 ; 4 lines (even)
	dc.w	'S',cx9+0*cs,cy1even,0*ctr,22
	dc.w	'O',cx9+1*cs,cy1even,1*ctr,22
	dc.w	'M',cx9+2*cs,cy1even,2*ctr,22
	dc.w	'E',cx9+3*cs,cy1even,3*ctr,22
	dc.w	'T',cx9+4*cs,cy1even,4*ctr,22
	dc.w	'H',cx9+5*cs,cy1even,5*ctr,22
	dc.w	'I',cx9+6*cs,cy1even,6*ctr,22
	dc.w	'N',cx9+7*cs,cy1even,7*ctr,22
	dc.w	'G',cx9+8*cs,cy1even,8*ctr,22

	dc.w	'W',cx9+0*cs,cy2even,9*ctr,22
	dc.w	'O',cx9+1*cs,cy2even,10*ctr,22
	dc.w	'N',cx9+2*cs,cy2even,11*ctr,22
	dc.w	'D',cx9+3*cs,cy2even,12*ctr,22
	dc.w	'E',cx9+4*cs,cy2even,13*ctr,22
	dc.w	'R',cx9+5*cs,cy2even,14*ctr,22
	dc.w	'F',cx9+6*cs,cy2even,15*ctr,22
	dc.w	'U',cx9+7*cs,cy2even,16*ctr,22
	dc.w	'L',cx9+8*cs,cy2even,17*ctr,22

	dc.w	'H',cx3+0*cs,cy3even,18*ctr,22
	dc.w	'A',cx3+1*cs,cy3even,19*ctr,22
	dc.w	'S',cx3+2*cs,cy3even,20*ctr,22

	dc.w	'H',cx9+0*cs,cy4even,21*ctr,22
	dc.w	'A',cx9+1*cs,cy4even,22*ctr,22
	dc.w	'P',cx9+2*cs,cy4even,23*ctr,22
	dc.w	'P',cx9+3*cs,cy4even,24*ctr,22
	dc.w	'E',cx9+4*cs,cy4even,25*ctr,22
	dc.w	'N',cx9+5*cs,cy4even,26*ctr,22
	dc.w	'E',cx9+6*cs,cy4even,27*ctr,22
	dc.w	'D',cx9+7*cs,cy4even,28*ctr,22
	dc.w	'[',cx9+8*cs,cy4even,29*ctr,22
textp1end

textp2 ; 1 line (odd)
	dc.w	'[',cx6+0*cs-8,cy2odd,0*ctr,22 ; manual correction (-8)
	dc.w	'A',cx6+1*cs-8,cy2odd,1*ctr,22
	dc.w	'G',cx6+2*cs-8,cy2odd,2*ctr,22
	dc.w	'A',cx6+3*cs-8,cy2odd,3*ctr,22
	dc.w	'I',cx6+4*cs-8,cy2odd,4*ctr,22
	dc.w	'N',cx6+5*cs-8,cy2odd,5*ctr,22
textp2end

textp3 ; 1 line (odd)
	dc.w	'S',cx11+0*cs,cy2odd,0*ctr,22
	dc.w	'P',cx11+1*cs,cy2odd,1*ctr,22
	dc.w	'R',cx11+2*cs,cy2odd,2*ctr,22
	dc.w	'E',cx11+3*cs,cy2odd,3*ctr,22
	dc.w	'A',cx11+4*cs,cy2odd,4*ctr,22
	dc.w	'D',cx11+5*cs,cy2odd,5*ctr,22
	dc.w	'P',cx11+6*cs,cy2odd,6*ctr,22
	dc.w	'O',cx11+7*cs,cy2odd,7*ctr,22
	dc.w	'I',cx11+8*cs,cy2odd,8*ctr,22
	dc.w	'N',cx11+9*cs,cy2odd,9*ctr,22
	dc.w	'T',cx11+10*cs,cy2odd,10*ctr,22
textp3end

textp4 ; 2 lines (even)
	dc.w	'A',cx7+0*cs,cy2even,0*ctr,22
	dc.w	'N',cx7+1*cs,cy2even,1*ctr,22
	dc.w	'D',cx7+2*cs,cy2even,2*ctr,22
	; space
	dc.w	'T',cx7+4*cs,cy2even,4*ctr,22
	dc.w	'H',cx7+5*cs,cy2even,5*ctr,22
	dc.w	'E',cx7+6*cs,cy2even,6*ctr,22

	dc.w	'M',cx11+0*cs,cy3even,6*ctr,22
	dc.w	'E',cx11+1*cs,cy3even,7*ctr,22
	dc.w	'G',cx11+2*cs,cy3even,8*ctr,22
	dc.w	'A',cx11+3*cs,cy3even,9*ctr,22
	dc.w	'\',cx11+4*cs,cy3even,10*ctr,22
	dc.w	'M',cx11+5*cs,cy3even,11*ctr,22
	dc.w	'I',cx11+6*cs,cy3even,12*ctr,22
	dc.w	'G',cx11+7*cs,cy3even,13*ctr,22
	dc.w	'H',cx11+8*cs,cy3even,14*ctr,22
	dc.w	'T',cx11+9*cs,cy3even,15*ctr,22
	dc.w	'Y',cx11+10*cs,cy3even,16*ctr,22
textp4end

textp5 ; 3 lines (odd)
	dc.w	'S',cx5+0*cs,cy1odd,0*ctr,22
	dc.w	'W',cx5+1*cs,cy1odd,1*ctr,22
	dc.w	'I',cx5+2*cs,cy1odd,2*ctr,22
	dc.w	'S',cx5+3*cs,cy1odd,3*ctr,22
	dc.w	'S',cx5+4*cs,cy1odd,4*ctr,22

	dc.w	'C',cx8+0*cs,cy2odd,5*ctr,22
	dc.w	'R',cx8+1*cs,cy2odd,6*ctr,22
	dc.w	'A',cx8+2*cs,cy2odd,7*ctr,22
	dc.w	'C',cx8+3*cs,cy2odd,8*ctr,22
	dc.w	'K',cx8+4*cs,cy2odd,9*ctr,22
	dc.w	'I',cx8+5*cs,cy2odd,10*ctr,22
	dc.w	'N',cx8+6*cs,cy2odd,11*ctr,22
	dc.w	'G',cx8+7*cs,cy2odd,12*ctr,22

	dc.w	'A',cx11+0*cs,cy3odd,13*ctr,22
	dc.w	'S',cx11+1*cs,cy3odd,14*ctr,22
	dc.w	'S',cx11+2*cs,cy3odd,15*ctr,22
	dc.w	'O',cx11+3*cs,cy3odd,16*ctr,22
	dc.w	'C',cx11+4*cs,cy3odd,17*ctr,22
	dc.w	'I',cx11+5*cs,cy3odd,18*ctr,22
	dc.w	'A',cx11+6*cs,cy3odd,19*ctr,22
	dc.w	'T',cx11+7*cs,cy3odd,20*ctr,22
	dc.w	'I',cx11+8*cs,cy3odd,21*ctr,22
	dc.w	'O',cx11+9*cs,cy3odd,22*ctr,22
	dc.w	'N',cx11+10*cs,cy3odd,23*ctr,22
textp5end

textp6 ; 1 line (odd)
	dc.w	'P',cx7+0*cs,cy2odd,0*ctr,22
	dc.w	'R',cx7+1*cs,cy2odd,1*ctr,22
	dc.w	'E',cx7+2*cs,cy2odd,2*ctr,22
	dc.w	'S',cx7+3*cs,cy2odd,3*ctr,22
	dc.w	'E',cx7+4*cs,cy2odd,4*ctr,22
	dc.w	'N',cx7+5*cs,cy2odd,5*ctr,22
	dc.w	'T',cx7+6*cs,cy2odd,6*ctr,22
textp6end

textparticles
	dc.l	textp1-base
	dc.w	(textp1end-textp1)/c_size-1
	dc.l	textp2-base
	dc.w	(textp2end-textp2)/c_size-1
	dc.l	textp3-base
	dc.w	(textp3end-textp3)/c_size-1
	dc.l	textp4-base
	dc.w	(textp4end-textp4)/c_size-1
	dc.l	textp5-base
	dc.w	(textp5end-textp5)/c_size-1
	dc.l	textp6-base
	dc.w	(textp6end-textp6)/c_size-1


*------	APPLY MTX FONT ---------------------------------------------------*

; a3 = 3d data
; a2 = destination
; d7 num vertices
applymtxfont
	lea		matrix(a5),a0		;
.loop
	move.l	a0,a1				;

	moveq	#0,d0				; z
	move.b	(a3)+,d1			; x
	ext.w	d1					;
	move.b	(a3)+,d2			; y
	ext.w	d2					;

	move.w	d1,d4				;
	muls	(a1)+,d4			;
	move.w	d2,d5				;
	muls	(a1)+,d5			;
	add.l	d5,d4				;
	move.w	d0,d5				;
	muls	(a1)+,d5			;
	add.l	d5,d4				;
	move.w	d1,d5				;
	muls	(a1)+,d5			;
	move.w	d2,d6				;
	muls	(a1)+,d6			;
	add.l	d6,d5				;
	move.w	d0,d6				;
	muls	(a1)+,d6			;
	add.l	d6,d5				;
	muls	(a1)+,d1			;
	muls	(a1)+,d2			;
	muls	(a1)+,d0			;
	add.l	d1,d0				;
	add.l	d2,d0				;
	asr.l	#8,d0				;
;	move.w	d0,d6				; (no need for z value)

	add.w	d3,d0				;
;	add.w	#48,d0				; distance  (256 1:1) 16 too big  48 fits
	divs	d0,d4				;
	divs	d0,d5				;
	move.w	d4,(a2)+			; x
	move.w	d5,(a2)+			; y

	dbf		d7,.loop			;
	rts							;

	rem
text
	dc.b	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
;	dc.b	"SPREADPOINT"
textend
	erem
	
	even

; x   x   x        0     1     2
; 
; x   x   x        3     4     5
; 
; x   x x x        6     7  15 8
; 
; x   x   x        9    10    11
; 
;xx  xx  xx    16 12 17 13 17 14

unitx	equ	4*2
unity	equ	3*2

numfont3dvertices	equ (font3dend-font3d)/2
font3d	; z = 0
	dc.b	-unitx,	-2*unity
	dc.b	0,		-2*unity
	dc.b	unitx,	-2*unity

;	dc.b	-unitx,	-unity
;	dc.b	0,		-unity
;	dc.b	unitx,	-unity
	dc.b	-unitx,	-2*2
	dc.b	0,		-2*2
	dc.b	unitx,	-2*2

	dc.b	-unitx,	0
	dc.b	0,		0
	dc.b	unitx,	0

;	dc.b	-unitx,	unity
;	dc.b	0,		unity
;	dc.b	unitx,	unity
	dc.b	-unitx,	2*2
	dc.b	0,		2*2
	dc.b	unitx,	2*2

	dc.b	-unitx,	2*unity
	dc.b	0,		2*unity
	dc.b	unitx,	2*unity

	dc.b	unitx/2,	0		; used for letters B,E,F
	
	dc.b	-unitx-1,	2*unity	; 16
	dc.b	0-1,		2*unity	; 17
	dc.b	unitx-1,	2*unity	; 18
font3dend

font ; indices
fa	dc.b	5-2, 12*4,3*4,1*4,5*4,14*4, 2-2,6*4,8*4, -1	; A
fb	dc.b	9-2, 6*4,15*4,5*4,1*4,0*4,12*4,13*4,11*4,15*4, -1	; B
fc	dc.b	4-2, 14*4,12*4,0*4,2*4, -1 ; C
fd	dc.b	7-2, 0*4,1*4,5*4,11*4,13*4,12*4,0*4, -1 ; D
fe	dc.b	4-2, 2*4,0*4,12*4,14*4, 2-2, 6*4,15*4, -1 ; E
ff	dc.b	3-2, 2*4,0*4,12*4, 2-2, 6*4,15*4, -1 ; F
fg	dc.b	6-2, 2*4,0*4,12*4,14*4,8*4,7*4, -1 ; G
fh	dc.b	2-2, 0*4,12*4, 2-2,2*4,14*4, 2-2, 6*4,8*4, -1 ; H
fi	dc.b	2-2, 0*4,2*4, 2-2, 12*4,14*4, 2-2, 1*4,13*4, -1 ; I
fj	dc.b	4-2, 2*4,14*4,13*4,9*4, -1 ; J
fk	dc.b	2-2, 0*4,12*4, 3-2, 2*4,6*4,14*4, -1 ; K
fl	dc.b	3-2, 0*4,12*4,14*4, -1 ; L
fm	dc.b	5-2, 12*4,0*4,7*4,2*4,14*4, -1 ; M
fn	dc.b	4-2, 12*4,0*4,14*4,2*4, -1 ; N
fo	dc.b	6-2, 12*4,0*4,2*4,14*4,12*4,0*4, -1 ; O
fp	dc.b	5-2, 12*4,0*4,2*4,8*4,6*4, -1 ; P
fq	dc.b	6-2, 10*4,14*4,12*4,0*4,2*4,14*4, -1 ; Q
fr	dc.b	6-2, 12*4,0*4,2*4,8*4,6*4,14*4, -1 ; R
fs	dc.b	6-2, 2*4,0*4,6*4,8*4,14*4,12*4, -1 ; S
ft	dc.b	2-2, 0*4,2*4, 2-2, 1*4,13*4, -1 ; T
fu	dc.b	4-2, 0*4,12*4,14*4,2*4, -1 ; U
fv	dc.b	5-2, 0*4,6*4,13*4,8*4,2*4, -1 ; V
fw	dc.b	5-2, 0*4,12*4,7*4,14*4,2*4, -1 ; W
fx	dc.b	2-2, 0*4,14*4, 2-2, 2*4,12*4, -1; X
fy	dc.b	3-2, 0*4,7*4,2*4, 2-2, 7*4,13*4, -1; Y
fz	dc.b	4-2, 0*4,2*4,12*4,14*4, -1 ; Z
f0	dc.b	2-2, 12*4,16*4,  2-2,13*4,17*4,  2-2,14*4,18*4, -1 ; [ = ...
f1	dc.b	2-2, 6*4,8*4, -1 ; \ = -

fontoffsets
	dc.b	fa-font, fb-font, fc-font, fd-font, fe-font, ff-font
	dc.b	fg-font, fh-font, fi-font, fj-font, fk-font, fl-font
	dc.b	fm-font, fn-font, fo-font, fp-font, fq-font, fr-font
	dc.b	fs-font, ft-font, fu-font, fv-font, fw-font, fx-font
	dc.b	fy-font, fz-font, f0-font, f1-font

	even


*------	WAIT BLITTER -----------------------------------------------------*

; http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0123.html
waitblitter
	btst.b  #14-8,$02(a6)			; DMAB_BLTDONE = 14
.wait
    btst.b  #14-8,$02(a6)			;
    bne		.wait					;
	rts								;


*------	WAIT RASTER ------------------------------------------------------*

waitraster
	cmp.b	#1,$06(a6)				;
	bne		waitraster				;
	btst	#0,$05(a6)				;
	bne		waitraster				;
	rts								;


*------	SLOW LINER DRAWER ------------------------------------------------*

; a0 bitplane, a2 data
; note: a1 is free/available
drawlines
	move.w	(a2)+,d5				; num lines
	subq.w	#1,d5					; adjust for dbf

	lea		$52(a6),a6				;
.waitblitter
	btst	#14-8,$02-$52(a6)		;
	bne		.waitblitter			;
	move.w	#pwidth,$60-$52(a6)		;
	move.w	#pwidth,$66-$52(a6)		;
	move.l	#$ffff8000,$72-$52(a6)	; texture data/index
	move.w	#$8000,$44-$52(a6)		; first word mask

loop
	movem.w	(a2)+,d0-d3				;

; entry for 1 line only (d5 must be 0)
drawline							; a0 bitplane, d0 x1, d1 y1, d2 x2, d3 y2
	moveq	#4,d7					; moveq clears d7's upper word
	move.l	d7,a4					; octant code

	move.w	d0,d7					; x1
	and.w	#$000f,d7				;
	ror.w	#4,d7					; startbit of line
	or.w	#$0bca,d7				;
	swap	d7						;
	sub.w	d0,d2					; x2-x1
	bpl		.rightwards				;
	addq.l	#1,a4					;
	neg.w	d2						;
.rightwards
	sub.w	d1,d3					; y2-y1
	bpl		.upwards				;
	addq.l	#2,a4					; bset #1
	neg.w	d3						; d3=y
.upwards
	move.w	d3,d6					;
	sub.w	d2,d6					; d6=y-x
	bmi		.nsteep					; steepness <1
	exg		d2,d3					; swap x and y
	subq.l	#4,a4					; bclr #2
	neg.w	d6						;
.nsteep
	lsl.w	#6,d2					; 64 = 1<<6
	add.w	#64+2,d2				; +2 required (width)
	move.w	d6,d4					; d2=y-x
	add.w	d3,d4					; d2=2y-x
	bpl		.nosign					;
	addq.l	#8,a4					; sign of 2y-x (in oct code)
.nosign
	lsl.w	#2,d3					; d3=4y
	lsl.w	#2,d6					; d6=4y-4x
	swap	d3						;
	move.w	d6,d3					;
	clr.w	d7						;
	move.b	.octs(pc,a4.l),d7		; read octant
	asl.w	#6,d1					; = muls #pwidth,d1

	lea		(a0,d1.w),a4			;
	asr.w	#3,d0					;
	add.w	d0,a4					;

	move.l	a6,a3					;
.waitblitter2
	btst	#14-8,$02-$52(a6)		;
	bne		.waitblitter2			;
	move.l	d3,$62-$52(a6)			; 4y, 4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)			;		BLTCON 0,1
	move.l	a4,$48-$52(a6)			;
	move.w	d4,(a3)+				; 2y-x		BLTAPTL
	move.l	a4,(a3)+				; set starting address
	move.w	d2,(a3)					; start
	dbf		d5,loop					;

	lea		-$52(a6),a6				; a6 becomes $dff000 again
	rts								;

.octs
	dc.b	0*4+1,2*4+1,1*4+1,3*4+1
	dc.b	4*4+1,5*4+1,6*4+1,7*4+1
	dc.b	0*4+65,2*4+65,1*4+65,3*4+65
	dc.b	4*4+65,5*4+65,6*4+65,7*4+65


*------	PRECALCULATION ---------------------------------------------------*

precalc
.loop
	move.w	v_precalcid(a5),d0		;
	beq		.idle					;
	subq.w	#1,d0					;
	asl.w	#2,d0					; longword offset
	lea		base(pc),a0				;
	add.l	.logos(pc,d0.w),a0		;
	jsr		(a0)					;
	move.l	b_lines2d(pc),a0		; player data
	move.l	a0,datapointer(a5)		;
	bsr		particlesystem			;
	clr.w	v_precalcid(a5)			; signal/mark done
.idle
	tst.w	doquit(a5)				;
	beq		.loop					;
	rts								;

.logos
	dc.l	inittclogo1-base			; 1: Terra Cresta (1)
	dc.l	initscalogo-base			; 2: SCA (1)
	dc.l	initspreadpointlogo1-base	; 3: Spreadpoint (1)
	dc.l	inittclogo2-base			; 4: Terra Cresta (2)
	dc.l	inittclogo3-base			; 5: Terra Cresta (3)
	dc.l	initatarilogo-base			; 6: Atari (1)
	dc.l	initspreadpointlogo2-base	; 7: Spreadpoint (2)
	dc.l	initscalogo2-base			; 8: SCA (2)


*------	COPER ------------------------------------------------------------*

coper
	moveq	#$0010,d0				; delete coper request bit
	move.w	d0,$9c(a6)				;
	move.w	d0,$9c(a6)				; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)				;

	if timing
	move.w	#$0f00,$180(a6)			;
	endif
	bsr		lspplay					;
		
	movem.l	(a7)+,a0-a6/d0-d7		;
	rte								;


*------	IRQ3 -------------------------------------------------------------*

irq3
	movem.l	a0-a6/d0-d7,-(a7)		;
	lea		custom,a6				;
	move.w	$1e(a6),d0				; read interrupt request bits
	btst	#4,d0					;
	bne		coper					;

    lea     vars(pc),a5             ;
    
	moveq	#$0030,d0				; delete vertb and coper request bit
	move.w	d0,$9c(a6)				; 
	move.w	d0,$9c(a6)				; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)				;
		
	move	#$2200,sr				; allow other (coper) level 3 interrupts

	bsr		play					;
	
	move.w	v_actors(a5),d7			; process actors
	btst	#actorscroller,d7		; actor scrolltext?
	beq		.act0					;
	bsr		scrolltext				;

.act0
	movem.l	dbplanes(a5),d0-d5		; double buffering
;	sub.l	#pwidth*invistop,d0		; TESTING only: show clipping area
	move.l	d0,$e0(a6)				;
;	add.l	#pwidth*invistop,d0		; TESTING only: show clipping area
	exg		d0,d1					; bright planes 1
	move.l	d2,$e8(a6)				;
	btst	#actordbuf2,d7			;
	beq		.nodbuf2				;	
	exg		d2,d3					; bright planes 2
.nodbuf2
	move.l	d4,$f0(a6)				;
	exg		d4,d5					; scrolltext planes
	movem.l	d0-d5,dbplanes(a5)		;

	move.w	v_actors(a5),d7			; actor greetings?
	lea		chardata(pc),a0			;
	move.w	#chardataend-chardata,d5
	btst	#actorgreet,d7			;
	beq		.actnogreet				;
	bsr		charwriter				; Writes to unbuffered plane - first thing to do in frame
	bra		.actchardone			;
.actnogreet
	btst	#actorcredits,d7		; actor credits?
	beq		.actnocredits			;
	lea		chardata2(pc),a0		;
	move.w	#chardata2end-chardata2,d5
	bsr		charwriter				; Writes to unbuffered plane - first thing to do in frame
.actnocredits
.actchardone

	btst	#actorcls2,d7			; Actor CLS second bright plane aka CLS2?
	beq		.act1					;
	move.l	dbplanes2(a5),a0		;
	move.w	#(pheight-45)<<6+(pwidth-inviswidth)>>1,d1 ; no need to clear screen completely
	bsr		cls						;
	move.l	dbplanes2+4(a5),a0		; This is very stupid. Who cares. Plenty of time left.
	bsr		cls						;

.act1
	btst	#actorearthrise,d7		; actor earthrise?
	beq		.act2					;
	move.l	v_earthpos(a5),d1		;
	bne		.earthrise				; final position?
	sub.w	#1<<actorearthrise,v_actors(a5)	; stop actor
	bra		.act2					;
.earthrise
	sub.l	#pwidth,d1				;
	move.l	d1,v_earthpos(a5)		;
	move.l	v_staticplane2(a5),d0	;
	sub.l	d1,d0					;
	move.l	clistbase(pc),a1		;
	add.w	#bitplane4-clist+6,a1	;
	move.w	d0,(a1)					;
	swap	d0						;
	move.w	d0,-4(a1)				;

.act2
	btst	#actorsausboden,d7		; actor sausboden?
	beq		.act4					;
	move.l	clistbase(pc),a1		;
	move.w	v_sausboden(a5),d0		;
	bne		.sb1					;
	moveq	#-36,d1					; = $(ffffff)dc (byte)
	move.b	d1,v_diwstop(a5)		;
	addq.w	#diwstop-clist+2,a1		;
	move.b	d1,(a1)					; takes effect in next frame (clist is not double buffered)
	bra		.sbend					;
.sb1	
	cmp.w	#1,d0					;
	bne		.sb2					;
	move.w	#color2,d1				;
	add.w	#playfield2colors-clist+2,a1	;
	move.w	d1,(a1)					; $0192
	move.w	d1,4(a1)				; $0194
	move.w	d1,8(a1)				; $0196
	addq.b	#1,v_diwstop(a5)		; (remove this? --> no)
	bsr		fillbelowearth			; trashes d7
	bra		.sbend					;
.sb2
	addq.w	#diwstop-clist+2,a1		;
	move.b	v_diwstop(a5),(a1)		;
	cmp.b	#$2c,v_diwstop(a5)		; done?
	bne		.sb22					;
	sub.w	#1<<actorsausboden,v_actors(a5) ; stop actor
	bra		.sbend					;
.sb22
	addq.b	#1,v_diwstop(a5)		;
.sbend
	addq.w	#1,v_sausboden(a5)		;

.act4
	move.l	dbplanes(a5),a0			; CLS bright plane 1
	move.w	#pheight<<6+(pwidth-inviswidth)>>1,d1
	bsr		cls						; trashed d0, a0
	
	move.w	v_actors(a5),d0			; actor viper?
	btst	#actorviper,d0			;
	beq		.act3					;
	bsr		viper3dto2d				;
	move.l	dbplanes(a5),a0			; plane
	add.l	#center,a0				;
	bsr		drawviper				;

.act3
	move.w	v_actors(a5),d7			; actor text?
	btst	#actortext,d7			;
	beq		.act5					;
	move.l	dbplanes(a5),a0			; plane
	add.l	#center,a0				;
	bsr		drawtextparticles		;

.act5
	move.w	v_actors(a5),d7			; actor logo?
	btst	#actorlogo,d7			;
	beq		.act6					;
	
	tst.w	v_precalcid(a5)			;
	bne		.notready				;
	
	move.l	dbplanes(a5),a0			; plane
	add.l	#center,a0				;
	move.l	datapointer(a5),a2		;
	bsr		drawlines				;
	move.l	a2,datapointer(a5)		;
	addq.l	#4,a2					; Why why? -> crash oterhwise never mind
	cmp.l	mdataend(a5),a2			;
	bls		.noreset				;
	sub.w	#1<<actorlogo,v_actors(a5)	; stop actor
.noreset
.notready

.act6
	move.w	v_actors(a5),d7			; actor still logo?
	btst	#actorlogostill,d7		;
	beq		.actend					;
	tst.w	mframe(a5)				;
	beq		.firstframenotready		;
	move.l	dbplanes2(a5),a0		;
	add.l	#center,a0				;
	move.l	datapointer(a5),a2		;
	bsr		drawlines				;
.firstframenotready

.actend
	if numbers&timing
	move.w	#$0440,$180(a6)			; dark yellow color indicates numbers consumption
	endif
	
	if numbers
	moveq	#0,d0					; number
	move.w	frame(a5),d0			;
;	move.w	v_precalcid(a5),d0		;
;	move.l	$200.w,d0				;
;	sub.l	b_lines2d(pc),d0		;
;	move.l	#numparticlestc,d0		; value
	moveq	#8-1,d1					; pos
	bsr		printnumber				;

	moveq	#0,d0					; number
	
	if numbers&testing	
	move.l	v_number2(a5),d0		;
	endif

	move.l	viperty(a5),d0			;
;	asr.l	#2,d0					;
;	asr.l	#8,d0					;
;	move.w	viperay(a5),d0			;
;	move.l	#22*4*numfont3dvertices,d0
;	move.l	#numparticlessp,d0		; value
	move.w	#10*pwidth+8-1,d1		; pos
	bsr		printnumber				;
	endif

	addq.w	#1,frame(a5)			; advance frame number

	btst	#6,$bfe001				; left mouse button pressed?
	bne		.noquit					;
	st		doquit(a5)				;
.noquit

	if timing
	move.w	#$0020,$180(a6)			; dark green color indicates free capacity
	endif

	movem.l	(a7)+,a0-a6/d0-d7		;
	rte								;
	
		
*------	PLAYER -----------------------------------------------------------*

play
    tst.b   v_wait(a5)              ;
    beq     .donotwait              ;
    subq.b  #1,v_wait(a5)           ;
    rts                             ;

.donotwait
    move.l  v_cmdspointer(a5),a0	;
.loop
    move.b  (a0)+,d0                ; cmd_eof (0)?
    beq     .eof                    ;
    
	subq.b  #1,d0                   ; cmd_wait (1)?
    bne     .2                      ;
    move.b  (a0)+,v_wait(a5)		;
    bra     .loop                   ;

.2  subq.b  #1,d0                   ; cmd_playtext (2)?
    bne     .3                      ;
    moveq	#0,d1					;
    move.b	(a0)+,d1				;
    move.w	d1,v_textpindex(a5)		;
    bra		.loop					;
  
.3	subq.b	#1,d0					; cmd_start (3)?
	bne		.4						;
	moveq	#0,d1					;
	move.b	(a0)+,d1				; actor
	move.w	v_actors(a5),d2			;
	bset	d1,d2					;
	move.w	d2,v_actors(a5)			;
	bra		.loop					; 
	
.4	subq.b	#1,d0					; cmd_stop (4)?
	bne		.5						;
	moveq	#0,d1					;
	move.b	(a0)+,d1				; actor
	move.w	v_actors(a5),d2			;
	bclr	d1,d2					;
	move.w	d2,v_actors(a5)			;
	bra		.loop					; 

.5	subq.b	#1,d0					; cmd_precalc (5)?
	bne		.6						;
	moveq	#0,d1					;
	move.b	(a0)+,d1				; id (logo)
	move.w	d1,v_precalcid(a5)		;
	bra		.loop					;

.6	subq.b	#1,d0					; cmd_viperdax (6)?
	bne		.7						;
	move.b	(a0)+,d1				;
	ext.w	d1						;
	move.w	d1,viperdax(a5)			;
	bra		.loop					;

.7	subq.b	#1,d0					; cmd_viperday (7)?
	bne		.8						;
	move.b	(a0)+,d1				;
	ext.w	d1						;
	move.w	d1,viperday(a5)			;
	bra		.loop					;

.8	subq.b	#1,d0					; cmd_waitpos (8)?
	bne		.9						;
	moveq	#0,d1					;
	move.b	(a0)+,d1				; y range
	move.l	viperty(a5),d2			;
	asr.l	#2,d2					;
	asr.l	#8,d2					;
	cmp.w	d1,d2					; (50)
	bgt		.wait					; viper is flying too low, wait a bit
	neg.w	d1						;
	cmp.w	d1,d2					; (-50)
	blt		.wait					; viper is flying too high, wait a bit
	bra		.eof					; this cmd must NOT be followed by cmd_eof
.wait
	move.b	#15,v_wait(a5)			; wait 15 frames
	subq.l	#2,a0					; repeat this cmd (and parameter)
	bra		.eof					;

.9
;	subq.b	#1,d0					; IT IS cmd_rewind (9)
;	bne		.eof					;
	lea		playrewind(pc),a0		;
.eof
    move.l  a0,v_cmdspointer(a5)    ;
    rts                             ;

; commands
cmd_eof			equ	0
cmd_wait		equ 1
cmd_playtext	equ 2
cmd_start		equ 3
cmd_stop		equ 4
cmd_precalc		equ 5
cmd_viperdax	equ	6
cmd_viperday	equ 7
cmd_waitpos		equ 8
cmd_rewind		equ 9

; actor bits
actorcls2		equ 0
actorviper		equ 1
actorearthrise	equ 2
actorsausboden	equ 3
actortext		equ 4
actorlogo		equ	5
actorlogostill	equ	6
actordbuf2		equ 7
actorscroller	equ	8
actorlasers		equ	9
actoraseq1		equ	10
actoraseq2		equ	11
actorgreet		equ 12
actorcredits	equ	13
actorlowviper	equ 14

laserduration	equ	100
playcmds
	; precalc TC logo
	dc.b	cmd_precalc,1
	dc.b	cmd_playtext,0*6, cmd_start,actortext, cmd_eof ; SOMETHING WONDERFUL...

	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,1, cmd_eof, cmd_stop,actorcls2, cmd_eof
	
	dc.b	cmd_wait,20, cmd_eof
	
	dc.b	cmd_playtext,1*6, cmd_eof ; AGAIN...
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,1, cmd_eof, cmd_stop,actorcls2, cmd_eof

	dc.b	cmd_wait,20, cmd_eof

	dc.b	cmd_playtext,2*6, cmd_eof ; SPREADPOINT

	dc.b	cmd_wait,10,cmd_eof

	dc.b	cmd_start,actorsausboden

	dc.b	cmd_wait,130,cmd_eof
	dc.b    cmd_start,actorearthrise,cmd_eof

	dc.b	cmd_wait,20, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,1, cmd_eof, cmd_stop,actorcls2, cmd_eof

	dc.b	cmd_playtext,3*6, cmd_eof ; AND THE MEGA-MIGHTY
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,1, cmd_eof, cmd_stop,actorcls2, cmd_eof

	dc.b	cmd_wait,20, cmd_eof

	dc.b	cmd_playtext,4*6, cmd_eof ; SWISS CRACKING ASSOCIATION
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,1, cmd_eof, cmd_stop,actorcls2, cmd_eof

	dc.b	cmd_wait,20, cmd_eof

	dc.b	cmd_playtext,5*6, cmd_eof ; PRESENT
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,1, cmd_eof, cmd_stop,actorcls2, cmd_eof

	dc.b	cmd_wait,120, cmd_eof

	; show TC logo
	dc.b	cmd_start,actorlogostill, cmd_eof
	dc.b	cmd_stop,actorlogostill, cmd_stop,actordbuf2, cmd_wait,100+120-70-48, cmd_eof

	dc.b	cmd_start,actoraseq1

	dc.b	cmd_start,actorviper, cmd_wait,70, cmd_eof
	
	dc.b	cmd_waitpos,220
	
	dc.b	cmd_start,actorlasers, cmd_wait,laserduration, cmd_eof
	dc.b	cmd_start,actordbuf2 
	dc.b	cmd_start,actorlogo, cmd_eof
	dc.b	cmd_stop,actordbuf2

playrewind
	dc.b	cmd_stop,actorlasers

	; hide TC logo
	dc.b	cmd_wait,250, cmd_eof
	dc.b	cmd_viperdax,14, cmd_viperday,6, cmd_eof
	dc.b	cmd_stop,actorlogo, cmd_eof ; hide after 5 secs

	; precalc SCA logo
	dc.b	cmd_precalc,2, cmd_eof

	dc.b	cmd_wait,15, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,2, cmd_eof, cmd_start,actordbuf2, cmd_wait,2,  cmd_stop,actorcls2, cmd_eof
	dc.b	cmd_start,actorscroller
	dc.b	cmd_wait,15, cmd_eof   ; let pass some time to precalc first frame

	; show SCA logo
	dc.b	cmd_start,actorlogostill, cmd_eof
	dc.b	cmd_stop,actorlogostill, cmd_stop,actordbuf2, cmd_wait,170, cmd_eof, cmd_wait,180-48, cmd_eof

	dc.b	cmd_viperdax,12, cmd_eof

	dc.b	cmd_waitpos,50

	dc.b	cmd_start,actorlasers, cmd_wait,laserduration, cmd_eof
	dc.b	cmd_start,actordbuf2 
	dc.b	cmd_start,actorlogo, cmd_eof
	dc.b	cmd_stop,actordbuf2

	dc.b	cmd_stop,actorlasers

	dc.b	cmd_viperdax,10, cmd_viperday,8, cmd_eof

	; hide SCA logo
	dc.b	cmd_wait,250, cmd_eof
	dc.b	cmd_stop,actorlogo, cmd_eof ; hide after 5 secs

	; precalc Spreadpoint logo
	dc.b	cmd_precalc,3, cmd_eof

	dc.b	cmd_wait,15, cmd_eof

	dc.b	cmd_start,actorcls2, cmd_wait,2, cmd_eof, cmd_start,actordbuf2, cmd_wait,2,  cmd_stop,actorcls2, cmd_eof
	
	dc.b	cmd_wait,15, cmd_eof   ; let pass some time to precalc first frame

	; show Spreadpoint logo
	dc.b	cmd_start,actorlogostill, cmd_eof
	dc.b	cmd_stop,actorlogostill, cmd_stop,actordbuf2, cmd_wait,120, cmd_eof, cmd_wait,240-48, cmd_eof

	dc.b	cmd_wait,250, cmd_eof
	dc.b	cmd_wait,50, cmd_eof
	
	dc.b	cmd_waitpos,200

	dc.b	cmd_start,actorlasers, cmd_wait,laserduration, cmd_eof
	dc.b	cmd_start,actordbuf2 
	dc.b	cmd_start,actorlogo, cmd_eof
	dc.b	cmd_stop,actordbuf2

	dc.b	cmd_viperdax,14, cmd_viperday,6, cmd_eof

	; hide Spreadpoint logo
	dc.b	cmd_stop,actorlasers

	dc.b	cmd_wait,250, cmd_eof ; hide after 5 secs
	dc.b	cmd_stop,actorlogo, cmd_eof

	; precalc TC logo
	dc.b	cmd_precalc,4, cmd_eof
	dc.b	cmd_wait,15, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,2, cmd_eof, cmd_start,actordbuf2, cmd_wait,2,  cmd_stop,actorcls2, cmd_eof
	dc.b	cmd_wait,15, cmd_eof
	dc.b	cmd_viperdax,12, cmd_viperday,8, cmd_eof

	dc.b	cmd_start,actorlowviper, cmd_eof ; prepare viper for greetings

	dc.b	cmd_wait,150, cmd_eof

	; greetings
	dc.b	cmd_start,actorgreet, cmd_stop,actordbuf2, cmd_eof ; Show (eor, will stop itself)	
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_wait,100, cmd_eof

	dc.b	cmd_start,actorgreet, cmd_stop,actordbuf2, cmd_eof ; Hide (eor, will stop itself)
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_wait,100, cmd_eof
	dc.b	cmd_viperday,8, cmd_eof
	dc.b	cmd_wait,50, cmd_eof
	; greetings end

	; show TC logo
	dc.b	cmd_start,actorlogostill, cmd_eof
	dc.b	cmd_stop,actorlogostill, cmd_stop,actordbuf2, cmd_wait,200, cmd_eof
	dc.b	cmd_wait,200, cmd_eof, cmd_wait,200-48, cmd_eof
	dc.b	cmd_waitpos,220
	
	dc.b	cmd_viperday,10
	
	dc.b	cmd_start,actorlasers, cmd_wait,laserduration, cmd_eof
	dc.b	cmd_start,actordbuf2 
	dc.b	cmd_start,actorlogo, cmd_eof
	dc.b	cmd_stop,actordbuf2

	dc.b	cmd_stop,actorlasers

	dc.b	cmd_viperdax,10, cmd_viperday,12, cmd_eof

	; hide TC logo
	dc.b	cmd_wait,250, cmd_eof
	dc.b	cmd_stop,actorlogo, cmd_eof ; hide after 5 secs

	dc.b	cmd_viperdax,8, cmd_viperday,14, cmd_eof

	; precalc SCA logo (2)
	dc.b	cmd_precalc,8, cmd_eof

	dc.b	cmd_wait,15, cmd_eof

	dc.b	cmd_start,actorcls2, cmd_wait,2, cmd_eof, cmd_start,actordbuf2, cmd_wait,2,  cmd_stop,actorcls2, cmd_eof
	
	dc.b	cmd_wait,15, cmd_eof   ; let pass some time to precalc first frame

	; show SCA logo (2)
	dc.b	cmd_start,actorlogostill, cmd_eof
	dc.b	cmd_stop,actorlogostill, cmd_stop,actordbuf2, cmd_wait,250, cmd_eof

	dc.b	cmd_viperdax,10, cmd_viperday,12, cmd_eof
	
	dc.b	cmd_wait,180-48, cmd_eof

	dc.b	cmd_waitpos,50

	dc.b	cmd_start,actorlasers, cmd_wait,laserduration, cmd_eof
	dc.b	cmd_start,actordbuf2 
	dc.b	cmd_start,actorlogo, cmd_eof
	dc.b	cmd_stop,actordbuf2

	dc.b	cmd_stop,actorlasers

	dc.b	cmd_viperdax,12, cmd_viperday,10, cmd_eof

	; hide SCA logo (2)
	dc.b	cmd_wait,250, cmd_eof
	dc.b	cmd_stop,actorlogo, cmd_eof ; hide after 5 secs

	dc.b	cmd_viperdax,14, cmd_viperday,8, cmd_eof

	; precalc Spreadpoint logo
	dc.b	cmd_precalc,7, cmd_eof
	dc.b	cmd_wait,15, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,2, cmd_eof, cmd_start,actordbuf2, cmd_wait,2,  cmd_stop,actorcls2, cmd_eof
	
	dc.b	cmd_wait,15, cmd_eof   ; let pass some time to precalc first frame

	; show Spreadpoint logo
	dc.b	cmd_start,actorlogostill, cmd_eof
	dc.b	cmd_stop,actorlogostill, cmd_stop,actordbuf2, cmd_wait,220, cmd_eof, cmd_wait,180, cmd_eof, cmd_wait,240-48, cmd_eof

	dc.b	cmd_waitpos,200
	dc.b	cmd_start,actorlasers, cmd_wait,laserduration, cmd_eof
	dc.b	cmd_start,actordbuf2 
	dc.b	cmd_start,actorlogo, cmd_eof
	dc.b	cmd_stop,actordbuf2

	dc.b	cmd_viperdax,16, cmd_viperday,6, cmd_eof

	; hide Spreadpoint logo
	dc.b	cmd_stop,actorlasers
	dc.b	cmd_wait,250, cmd_eof ; hide after 5 secs
	dc.b	cmd_stop,actorlogo, cmd_eof

	dc.b	cmd_viperdax,14, cmd_viperday,4, cmd_eof

	; precalc Atari logo
	dc.b	cmd_precalc,6, cmd_eof
	dc.b	cmd_wait,15, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,2, cmd_eof, cmd_start,actordbuf2, cmd_wait,2,  cmd_stop,actorcls2, cmd_eof
	
	; credits
	dc.b	cmd_start,actorcredits, cmd_stop,actordbuf2, cmd_eof ; Show (will stop itself)	
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_wait,200, cmd_eof
	dc.b	cmd_wait,150, cmd_eof
	dc.b	cmd_start,actorcls2, cmd_wait,2, cmd_eof, cmd_start,actordbuf2, cmd_wait,2,  cmd_stop,actorcls2, cmd_eof
	; credits end
	
	dc.b	cmd_wait,15, cmd_eof   ; let pass some time to precalc first frame

	; show Atari logo
	dc.b	cmd_start,actorlogostill, cmd_eof
	dc.b	cmd_stop,actorlogostill, cmd_stop,actordbuf2, cmd_wait,240-48, cmd_eof

	dc.b	cmd_waitpos,200
	dc.b	cmd_start,actorlasers, cmd_wait,laserduration, cmd_eof
	dc.b	cmd_start,actordbuf2 
	dc.b	cmd_start,actorlogo, cmd_eof
	dc.b	cmd_stop,actordbuf2

	dc.b	cmd_viperdax,12, cmd_viperday,8, cmd_eof

	; hide Atari logo
	dc.b	cmd_stop,actorlasers
	dc.b	cmd_wait,250, cmd_eof ; hide after 5 secs
	dc.b	cmd_stop,actorlogo, cmd_eof

	; precalc TC logo
	dc.b	cmd_precalc,5, cmd_eof

	dc.b	cmd_wait,15, cmd_eof

	dc.b	cmd_start,actorcls2, cmd_wait,2, cmd_eof, cmd_start,actordbuf2, cmd_wait,2,  cmd_stop,actorcls2, cmd_eof
	dc.b	cmd_wait,15, cmd_eof

	dc.b	cmd_wait,200, cmd_eof

	; show TC logo
	dc.b	cmd_start,actorlogostill, cmd_eof
	dc.b	cmd_stop,actorlogostill, cmd_stop,actordbuf2, cmd_wait,200, cmd_eof
	dc.b	cmd_wait,200, cmd_eof, cmd_wait,200-48, cmd_eof
	
	dc.b	cmd_waitpos,220

	dc.b	cmd_start,actorlasers, cmd_wait,laserduration, cmd_eof

	dc.b	cmd_start,actordbuf2 
	dc.b	cmd_start,actorlogo, cmd_eof
	dc.b	cmd_stop,actordbuf2

	dc.b	cmd_wait,50, cmd_eof ; extra time (lasers)

	dc.b	cmd_viperdax,14, cmd_viperday,6, cmd_eof
	dc.b	cmd_rewind ; no need for cmd_eof after this

	even


*------	CLEAR SCREEN -----------------------------------------------------*

; a0 bitplane
; d1 bltsize
cls	moveq	#0,d0					;
	bsr		waitblitter				;
	move.l	a0,$54(a6) 				; destination d
	move.w	#$0100,$40(a6)			; bltcon0
	move.w	d0,$42(a6)				; bltcon1
	moveq	#inviswidth,d0			;
	move.l	d0,$64(a6)				; modulo
	move.w	d1,$58(a6)				; bltsize and start
	rts								;


*------	COPPER INSTRUCTION LIST ------------------------------------------*

clist
	dc.w	$1007,$fffe	; chance for player to alter copper list in time

diwstop
	dc.w	$0090,$2cc1 ; DIWSTOP  set to $dc...$2c later
	dc.w	$008e,$2c81 ; DIWSTRT

	dc.w	$0092,$0038 ; DDFSTRT
	dc.w	$0094,$00d0 ; DDFSTOP
	
bitplane2 ; static plane 1
	dc.w	$00e4,0,$00e6,0
bitplane4 ; static plane 2
	dc.w	$00ec,0,$00ee,0

	dc.w	$0100,$4600	; 4 bitplanes, dual playfield mode
	dc.w	$0102,$0000	;

; http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0159.html
	dc.w	$0104,$0000 ; sprites have no priority

	dc.w	$0108,inviswidth
	dc.w	$010a,inviswidth

; sprites off
	dc.w	$0144,$0000,$0146,$0000 ; data 0
	dc.w	$014c,$0000,$014e,$0000 ; data 1
	dc.w	$0154,$0000,$0156,$0000 ; data 2
	dc.w	$0164,$0000,$0166,$0000 ; data 4
	dc.w	$016c,$0000,$016e,$0000 ; data 5
	dc.w	$0174,$0000,$0176,$0000	; data 6

; unused sprites (3 and 7)
	dc.w	$0158,$2040,$015a,$2100 ; ctrl 3
	dc.w	$0178,$2040,$017a,$2100 ; ctrl 7
	dc.w	$015c,$0000,$015e,$0000 ; data 3
	dc.w	$017c,$0000,$017e,$0000 ; data 7

	dc.w	$0180,$0000		; playfield 1 logo
	dc.w	$0182,color1	;
	dc.w	$0184,color1	;
	dc.w	$0186,color1	;

	dc.w	$0188,color3	; 5th plane (scrolltext planes)
	dc.w	$018a,color1	;
	dc.w	$018c,color1	;
	dc.w	$018e,color1	;

	dc.w	$0190,$0000		; playfield 2 earth, sausboden
playfield2colors
	dc.w	$0192,$0000		; later set to color2
	dc.w	$0194,$0000		; dito
	dc.w	$0196,$0000		; dito

spritecolors
	dc.w	$01a0,$0000,$01a2,0,$01a4,$0222,$01a6,$0444 ; sprite 0 and 1 color
	dc.w	$01a8,$0000,$01aa,0,$01ac,$0222,$01ae,$0444 ; sprite 2 and 3 color
	dc.w	$01b0,$0000,$01b2,0,$01b4,$0222,$01b6,$0444 ; sprite 4 and 5 color
	dc.w	$01b8,$0000,$01ba,0,$01bc,$0222,$01be,$0444 ; sprite 6 and 7 color

	if testing
	dc.w	$1507,$fffe
	dc.w	$0180
precalccolor
	dc.w	0
	dc.w	$2c07,$fffe
	dc.w	$0180,$0000
	endif

lspline	equ $40
	dc.b	lspline,$07,$ff,$fe
	dc.w	$009c,$8010	; trigger coper interrupt
	dc.b	lspline+11,$07,$ff,$fe
lspdmacon
	dc.w	$0096,$8000

	; sprites off
;	dc.w	$0144,0,$0146,0 ; 0
;	dc.w	$014c,0,$014e,0 ; 1
;	dc.w	$0154,0,$0156,0 ; 2
;	dc.w	$0164,0,$0166,0 ; 4
;	dc.w	$016c,0,$016e,0 ; 5
;	dc.w	$0174,0,$0176,0	; 6

	dc.w	$dc07,$fffe ; horizon
bitplane4h ; static plane 2
	dc.w	$00ec,0,$00ee,0
	dc.w	$0194,color2,$dcdf,$fffe,$0194,$0000
	dc.w	$008a,$0000 ; jump to clist tail(s)
clistend


*------	MTX --------------------------------------------------------------*

; a2 = angles
mtx
	move.l	b_sintab(pc),a0		; sin
	lea		512(a0),a1			; cos

	move.w	#$07ff,d5			;
	move.w	(a2)+,d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d0		; sin a
	move.w	(a1,d6.w),d3		; cos a

	move.w	(a2)+,d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d1		; sin b
	move.w	(a1,d6.w),d4		; cos b

	move.w	(a2)+,d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d2		; sin c
	move.w	(a1,d6.w),d5		; cos c

	lea		matrix(a5),a0		;

	move.w	d0,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d6,a1				;
	move.w	d3,d7				;
	muls	d2,d7				;
	asr.l	#8,d7				;
	move.w	d7,a2				;
	muls	d5,d6				;
	asr.l	#8,d6				;
	sub.w	d7,d6				;
	move.w	d6,6(a0)			;
	move.w	d3,d7				;
	muls	d5,d7				;
	asr.l	#8,d7				;
	move.w	d7,a3				;
	move.w	a1,d6				;
	muls	d2,d6				;
	asr.l	#8,d6				;
	add.w	d7,d6				;
	move.w	d6,8(a0)			;
	move.w	a3,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d0,d7				;
	muls	d2,d7				;
	asr.l	#8,d7				;
	add.w	d7,d6				;
	move.w	d6,12(a0)			;
	move.w	a2,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d0,d7				;
	muls	d5,d7				;
	asr.l	#8,d7				;
	sub.w	d7,d6				;
	move.w	d6,14(a0)			;
	muls	d4,d5				;
	asr.l	#8,d5				;
	move.w	d5,(a0)				;
	muls	d4,d2				;
	asr.l	#8,d2				;
	move.w	d2,2(a0)			;
	muls	d4,d0				;
	asr.l	#8,d0				;
	move.w	d0,10(a0)			;
	muls	d4,d3				;
	asr.l	#8,d3				;
	move.w	d3,16(a0)			;
	neg.w	d1					;
	move.w	d1,4(a0)			;
	rts


*------	VIPER 3D to 2D --------------------------------------------------*

viper3dto2d
	move.l	b_sintab(pc),a3			;

	movem.w	viperax(a5),d0/d1		;
	and.w	#$07ff,d0				;
	move.w	(a3,d0.w),d0			;
	ext.l	d0						;
	asl.l	#2,d0					;
	asl.l	#8,d0					;
	move.l	d0,vipertx(a5)			;

	move.w	viperdax(a5),d0			;
	add.w	d0,viperax(a5)			;
	and.w	#$07ff,d1				;
	move.w	(a3,d1.w),d1			;
	
	btst	#actoraseq1,d7			; actor move in viper from top?
	beq		.noaseq1				;
	move.l	v_aseqpointer(a5),a0	;
	move.w	(a0)+,d1				;
	move.l	a0,v_aseqpointer(a5)	;
	lea		aseqend(pc),a1			;
	cmp.l	a0,a1					;
	bne		.noaseq1				;
	sub.w	#1<<actoraseq1,v_actors(a5) ; 
	add.w	#1<<actoraseq2,v_actors(a5) ; 
.noaseq1
	ext.l	d1						;
	asl.l	#2,d1					;
	asl.l	#8,d1					;
	move.l	d1,viperty(a5)			;

	btst	#actoraseq2,d7			; actor move viper y sine?
	beq		.noaseq2				;
	move.w	viperday(a5),d0			;
	add.w	d0,viperay(a5)			;
.noaseq2
	btst	#actorlowviper,d7		;
	beq		.notlow					;
	cmp.l	#$40000,d1				; viper at bottom?
	bne		.notlow					;
	sub.w	#1<<actorlowviper,v_actors(a5) ; 
	clr.w	viperday(a5)			; stop y axis movement
.notlow

	btst	#actorlasers,d7			; actor lasers?
	beq		.nopulse				;
	move.w	frame(a5),d0			;
	and.w	#%1111,d0				; maybe 1111 looks better
	bne		.nopulse				;
	bsr		addlaser				;
.nopulse

; viper vertices
	lea		viper3d(pc),a3			;
	move.l	b_viper2d(pc),a2		;
	moveq	#numviper3d-numviper3dlasers-1,d7	;
.projectionloop
	movem.l	(a3)+,d0-d2				;
	add.l	vipertx(a5),d1			; x += tx
	add.l	viperty(a5),d2			; y += ty
	divs	d0,d1					;
	divs	d0,d2					;
	move.w	d1,(a2)+				; x
	move.w	d2,(a2)+				; y
	dbf		d7,.projectionloop		;

; lasers vertices
	moveq	#-1,d3					; inactive marker value
	moveq	#0,d4					; extra lines to draw
	moveq	#maxlaserpairs*4-1,d7	; 4 vertices per laser pair
.projectionloop2
	movem.l	(a3)+,d0-d2				; z, x, y
	cmp.l	d0,d3					;
	beq		.inactive				;
	divs	d0,d1					;
	divs	d0,d2					;
	move.w	d1,(a2)+				; x
	move.w	d2,(a2)+				; y
	addq.w	#1,d4					; draw extra laser line
.inactive
	dbf		d7,.projectionloop2		;
	asr.w	#1,d4					; vertices/2 = lines to draw
	move.w	d4,extralines(a5)		;

	lea		viper3dlasers(pc),a1	; move and stop lasers
	move.l	#16*vs,d1				;
	moveq	#maxlaserpairs-1,d7		;
.loop
	cmp.l	(a1),d3					;
	beq		.laserisinactive		;
	cmp.l	#300*vs+vz,(a1)			; stop laser beyond this z pos
	blt		.laserisactive			;
	move.l	d3,0*lasersize(a1)		; mark laser pair as inactive
	move.l	d3,1*lasersize(a1)		;
	move.l	d3,2*lasersize(a1)		;
	move.l	d3,3*lasersize(a1)		;
	bra		.laserisinactive		;

.laserisactive
	add.l	d1,0*lasersize(a1)		; move laser pair
	add.l	d1,1*lasersize(a1)		;
	add.l	d1,2*lasersize(a1)		;
	add.l	d1,3*lasersize(a1)		;

.laserisinactive
	add.w	#4*lasersize,a1			;
	dbf		d7,.loop				;
	rts								;


*------	ADD LASER --------------------------------------------------------*

addlaser
	moveq	#-1,d3					; inactive marker value
	lea		viper3dlasers(pc),a1	;
	moveq	#maxlaserpairs-1,d7		;
.loop
	cmp.l	laserz(a1),d3			; find an inactive laser pair
	bne		.active					;

	move.l	#10*vs+vz,laserz(a1)	; laser 1 left
	move.l	#-20*vt,d0				;
	add.l	vipertx(a5),d0			;
	move.l	d0,laserx(a1)			;
	move.l	viperty(a5),lasery(a1)	;
	add.w	#lasersize,a1			;
	move.l	#50*vs+vz,laserz(a1)	; laser 1 right
	move.l	d0,laserx(a1)			;
	move.l	viperty(a5),lasery(a1)	;
	add.w	#lasersize,a1			;
	
	move.l	#10*vs+vz,laserz(a1)	; laser 2 left
	move.l	#20*vt,d0				;
	add.l	vipertx(a5),d0			;
	move.l	d0,laserx(a1)			;
	move.l	viperty(a5),lasery(a1)	;
	add.w	#lasersize,a1			;
	move.l	#50*vs+vz,laserz(a1)	; laser 2 right
	move.l	d0,laserx(a1)			;
	move.l	viperty(a5),lasery(a1)	;
	rts								; done: added

.active
	add.w	#4*lasersize,a1			; to the next 4 vertices
	dbf		d7,.loop				;
	rts								;


*------	MEMORY MANAGEMENT ------------------------------------------------*

BESTMEMORY			equ	0
MEMF_CHIP			equ 1<<1
MEMF_CLEAR   		equ 1<<16

clistsize			equ	clistend-clist
lspbanksize			equ lspbankend-lspbank

clisttailssize		equ	2688 ; checked

memtable
clistbase			dc.l	0,MEMF_CHIP,clistsize
lspbankbase			dc.l	0,MEMF_CHIP,lspbanksize

memtable2
b_plane1and2		dc.l	0,MEMF_CHIP+MEMF_CLEAR,p1and2size+psize ; + static plane 2
b_plane3			dc.l	0,MEMF_CHIP+MEMF_CLEAR,psize
b_plane4			dc.l	0,MEMF_CHIP+MEMF_CLEAR,psize
b_staticplane1		dc.l	0,MEMF_CHIP+MEMF_CLEAR,psize
b_scrollplane1		dc.l	0,MEMF_CHIP+MEMF_CLEAR,stheight*pwidth
b_scrollplane2		dc.l	0,MEMF_CHIP+MEMF_CLEAR,stheight*pwidth
b_sintab			dc.l	0,BESTMEMORY+MEMF_CLEAR,2560
b_clisttails		dc.l	0,MEMF_CHIP+MEMF_CLEAR,clisttailssize
b_lines2d			dc.l	0,BESTMEMORY+MEMF_CLEAR,150000 ; size is checked
b_particles			dc.l	0,BESTMEMORY+MEMF_CLEAR,maxparticles*particlesize
b_viper2d			dc.l	0,BESTMEMORY+MEMF_CLEAR,4*numviper3d  ; 2*2 words per 2d vertex
b_font2d			dc.l	0,BESTMEMORY,4*numfont3dvertices*23
memtableend

entrysize 	equ	12 ; one entry in the memtable is 12 bytes large (3 longwords)
entries		equ	(memtableend-memtable)/entrysize
entrieschip	equ	(memtable2-memtable)/entrysize

alloc
	lea		clist(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr		TypeOfMem(a6)			;
	btst	#1,d0					; chipmem?
	beq		.notchipmem				;

	lea		clist(pc),a0			; mark data that is in chipmen already
	lea		clistbase(pc),a1		;
	move.l	a0,(a1)					;
		
	lea		base(pc),a0				;
	add.l	#lspbank-base,a0		;
	lea		lspbankbase(pc),a1		;
	move.l	a0,(a1)					;
.notchipmem
	lea		memtable(pc),a5			;
	moveq	#entries-1,d7			;
.loop	
	tst.l	(a5)					; not to be allocated?
	bne		.noalloc				;
	move.l	8(a5),d0				; bytesize
	move.l	4(a5),d1				; requirements
	move.l	AbsExecBase.w,a6		;
	jsr		AllocMem(a6)			; allocmem
	tst.l	d0						; out of memory?
	beq		.printerrorandfreemem	;
	move.l	d0,(a5)					;
.noalloc	
	add.w	#entrysize,a5			; next entry
	dbf		d7,.loop				;
	bsr		initmemory				;
	moveq	#0,d0					; ok, all entries allocated
	rts								;

.printerrorandfreemem
	bsr		printoutofmemory		;
dealloc
	move.l	AbsExecBase.w,a6		;
	jsr		TypeOfMem(a6)			;
	lea		memtable(pc),a5			;
	moveq	#entries-1,d7			;
	btst	#1,d0					; chipmem?
	beq		.loop					; we are not in chipmem so free all entries
	lea		memtable2(pc),a5		;
	moveq	#entries-entrieschip-1,d7;
.loop
	tst.l	(a5)					; end of memtable?
	beq		.done					;
	move.l	(a5),a1					; address of memory block
	move.l	8(a5),d0				; bytesize
	move.l	AbsExecBase.w,a6		;
	jsr		FreeMem(a6)				;
	add.l	#entrysize,a5			;
	dbf		d7,.loop				;
.done
	moveq	#-1,d0					; alloc error
	rts								;

initmemory
    lea     vars(pc),a5             ;

; copy copper list to chip memory
	lea		clist(pc),a0			;
	move.l	clistbase(pc),a1		;
	move.w	#clistsize-1,d0			;
.copyclist
	move.b	(a0)+,(a1)+				;
	dbf		d0,.copyclist			;

	lea		dbplanes(a5),a1			; plane 1 and 2 are sharing some invis space
	move.l	b_plane1and2(pc),d0		; init plane double buffering
	add.l	#pwidth*invistop,d0		;
	move.l	d0,(a1)+				;
	add.l	#pwidth*(pheight+invisbottom),d0		;
	move.l	d0,(a1)					;

	move.l	b_plane1and2(pc),d0		;
	add.l	#p1and2size,d0			; here begins the static plane 2
	move.l	d0,v_staticplane2(a5)	;

	lea		dbplanes2(a5),a1		; init (non gross) plane double buffering
	move.l	b_plane3(pc),d0			; 
	move.l	d0,(a1)+				;
	move.l	b_plane4(pc),d0			;
	move.l	d0,(a1)					;

	move.l	b_staticplane1(pc),d0	;
	move.l	clistbase(pc),a1		;
	add.w	#bitplane2-clist+6,a1	;
	move.w	d0,(a1)					;
	swap	d0						;
	move.w	d0,-4(a1)				;

	move.l	v_staticplane2(a5),d0	; earth and sausboden (filled background)
	move.l	d0,d1					;
	move.l	#82*pwidth,d2			; earth is hidden, rise start pos
	move.l	d2,v_earthpos(a5)		; used by actor
	sub.l	d2,d0					;	
	move.l	clistbase(pc),a1		;
	move.l	a1,a2					;
	add.w	#bitplane4-clist+6,a1	;
	move.w	d0,(a1)					;
	swap	d0						;
	move.w	d0,-4(a1)				;
	add.l	#filly*pwidth,d1		;
	add.w	#bitplane4h-clist+6,a2	; sausboden starts at horizon
	move.w	d1,(a2)					;
	swap	d1						;
	move.w	d1,-4(a2)				;

	lea		dbplanes3(a5),a1		; init scroll plane double buffering
	move.l	b_scrollplane1(pc),d0	; 
	move.l	d0,(a1)+				;
	move.l	b_scrollplane2(pc),d0	;
	move.l	d0,(a1)					;

	bsr		generatesintab			;
	bsr		generateclisttails		;

	lea		base(pc),a0				; copy lspbank
	add.l	#lspbank-base,a0		;
	move.l	lspbankbase(pc),a1		;
	move.l	#lspbanksize,d0			;
copylspbank
	move.b	(a0)+,(a1)+				;
	subq.l	#1,d0					;
	bne		copylspbank				;
	rts								;


*------	GENERATE COPPER LIST TAILS ---------------------------------------*

; example
; dc.w ($dc+5)<<8+$41,$fffe,$0180,color2,($dc+5)<<8+$df,$fffe,$0180,$0000
; ...
; dc.w $ffdf,$fffe
; dc.w $0086,$0000,$0084,$0000  next clist list tail (note order)
; dc.w	$ffff,$fffe

b5on	equ	56
b5off	equ b5on+stheight

generateclisttails
	lea		sausbodenplan(pc),a0	;
	move.l	b_clisttails(pc),a1		;
	
.nextclisttail
	moveq	#0,d1					; ntsc/pal border flag
	moveq	#0,d3					; 5 bitplanes on flag
	moveq	#0,d4					; 5 bitplanes off flag
.loop
	moveq	#0,d0					;
	move.b	(a0)+,d0				;
	bmi		.done					; -1?
	beq		.endoftail				; 0?

	add.w	#$00dc,d0				; base/start line (horizon)

	tst.w	d1						; handle ntsc/pal border
	bne		.borderwritten			;
	cmp.w	#$00ff,d0				; special case: don't emit border wait for line $ff
	beq		.borderdone				;
	blo		.aboveborder			;
	move.l	#$ffdffffe,(a1)+		; border wait

.borderdone
	moveq	#-1,d1					;
.aboveborder
.borderwritten

	tst.w	d3						; already written?
	bne		.b5onwritten			;
	cmp.w	#b5on+$dc,d0			;
	blo		.b5onnotreached			; below? do not switch yet
	moveq	#-1,d3					; set written flag
	move.l	#((b5on+$dc)&$ff)<<24+$07fffe,(a1)+	;
	move.l	#$01005600,(a1)+		; 5 bitplanes from here
.b5onnotreached
	cmp.w	#b5on+$dc,d0			;
	beq		.b5onwaitwritten		;
.b5onwritten

	tst.w	d4						;
	bne		.b5offwaitwritten		;
	cmp.w	#b5off+$dc,d0			;
	blo		.b5offnotreached		;
	moveq	#-1,d4					;
	move.l	#((b5off+$dc)&$ff)<<24+$07fffe,(a1)+	;
	move.l	#$01004600,(a1)+		; 4 bitplanes from here again

.b5offnotreached	
	cmp.w	#b5off+$dc,d0			;
	beq		.b5offhit				;
.b5offwaitwritten
	move.b	d0,(a1)+				; start of line (left size)
	move.b	#$07,(a1)+				;
	move.w	#$fffe,(a1)+			;
.b5offhit
.b5onwaitwritten

	move.l	#$0194<<16+color2,(a1)+	;
	
	move.b	d0,(a1)+				; end of line (right side)
	move.b	#$df,(a1)+				;
	move.w	#$fffe,(a1)+			;
	move.l	#$01940000,(a1)+		;
	
	cmp.w	#$00ff,d0				;
	bne		.notborder				;
	moveq	#-1,d1					;
.notborder	
	bra		.loop					;

.endoftail
	tst.w	d3						; 
	bne		.end					;
	move.l	#((b5on+$dc)&$ff)<<24+$07fffe,(a1)+	;
	move.l	#$01005600,(a1)+		; 5 bitplanes from here
.end
	tst.w	d4						;
	bne		.end2					;
	move.l	#((b5off+$dc)&$ff)<<24+$07fffe,(a1)+	;
	move.l	#$01004600,(a1)+		; 4 bitplanes from here again
.end2
	move.l	a1,d2					;
	add.l	#12,d2					; address of next clist tail
	move.w	#$0086,(a1)+			;
	move.w	d2,(a1)+				; low
	move.w	#$0084,(a1)+			;
	swap	d2						;
	move.w	d2,(a1)+				; high
	moveq	#-2,d2					; $ffff,$fffe
	move.l	d2,(a1)+				; end of clist
	bra		.nextclisttail			;

.done
	move.l	b_clisttails(pc),d2		; last clist tail points to first
	move.w	d2,-10(a1) 				; low
	swap	d2						;
	move.w	d2,-6(a1)				; high

;	move.l	b_clisttails(pc),d0		; TESTING only
;	move.l	d0,$220.w
;	sub.l	d0,a1					;
;	move.l	a1,$210.w				; size of
	rts								;
	
; cmds: 0 next, -1 end
; baseline/horizon = $dc (0)
; 35 ($23): on ntsc/pal edge $dc + 35 = $ff
	
sausbodenplan
	dc.b	5,11,19,29,43,63,0		; 0
	dc.b	5,11,19,30,44,64,0		; 10
	dc.b	5,12,20,30,45,65,0		; 20
	dc.b	6,12,20,31,45,67,0		; 30
	dc.b	1,6,12,21,32,46,68,0	; 40
	dc.b	1,6,13,21,32,47,69,0	; 50
	dc.b	1,7,13,22,33,48,71,0	; 60
	dc.b	1,7,14,22,33,49,72,0	; 70
	dc.b	2,7,14,23,34,50,73,0	; 80
	dc.b	2,7,14,23,35,51,75,0	; 90
	dc.b	2,8,15,24,35,52,76,0	; 100
	dc.b	2,8,15,24,36,53,78,0	; 110
	dc.b	3,8,16,25,37,54,80,0	; 120
	dc.b	3,9,16,25,38,55,0		; 130 (the tricky 55 one)
	dc.b	3,9,16,26,38,56,0		; 140
	dc.b	3,9,17,26,39,57,0		; 150
	dc.b	4,10,17,27,40,58,0		; 160
	dc.b	4,10,18,27,41,59,0		; 170
	dc.b	4,10,18,28,41,60,0		; 180
	dc.b	5,11,18,28,42,62,0		; 190
	dc.b	-1
	
	even


*------	CHAR WRITER ------------------------------------------------------*

; a0 chardata, d5 length

charwriter
	add.w	v_charpos(a5),a0		;
	addq.w	#4,v_charpos(a5)		; next char (element = 4 bytes)

	moveq	#0,d1					;
	move.b	(a0)+,d1				; char
    asl.w   #3,d1                   ; offset to font data
	lea		ltsfont-(' '*8)(pc),a1	;
    add.w	d1,a1					;

	moveq	#0,d1					;
	move.b	(a0)+,d1				; y
	asl.w	#6,d1					; = muls #pwidth,d1
	move.l	dbplanes2(a5),a2		;
	add.w	d1,a2					;

	moveq	#0,d1					;
	move.b	(a0)+,d1				; x
	add.w	d1,a2					;
	
	moveq	#0,d1					;
	move.b	(a0)+,d1				; shift value
	addq.b	#2,d1					; 2px padding on the left
	
	moveq	#5-1,d6					; draw 5 lines of char (leave 6th line out)
.print
	moveq	#0,d0					;
	move.b	(a1)+,d0				;
	lsl.w	#8,d0					;
	lsr.w	d1,d0					;
	eor.b	d0,1(a2)				;
	lsr.w	#8,d0					;
	eor.b	d0,(a2)					;
	add.w	#pwidth,a2				; next line of char	
	dbf		d6,.print				;
	
	cmp.w	v_charpos(a5),d5		;
	bne		.ongoing				;
	and.w	#$ffff-(1<<actorgreet)-(1<<actorcredits),v_actors(a5) ; stop greetings and credits
	clr.w	v_charpos(a5)			; reset for next time
.ongoing
	rts								;

chardata
	incbin	"greetings"
chardataend

chardata2
	incbin	"credits"
chardata2end

	even


*------	PRINT OUT OF MEMORY ----------------------------------------------*

printoutofmemory
	lea		.dos(pc),a1				;
	move.l	AbsExecBase.w,a6		;
	jsr		OldOpenLibrary(a6)		;
	move.l	d0,a6					;
	beq		.error					;
	jsr		Output(a6)				;
	move.l	d0,d1					;
	beq		.error					;
	moveq	#.textend-.text,d3		; length
	lea		.text(pc),a1			;
	move.l	a1,d2					;
	jsr		Write(a6)				;
	tst.l	d0						;
	beq		.error					;
	move.l	a6,a1					;
	move.l	AbsExecBase.w,a6		;
	jsr		CloseLibrary(a6)		;
.error
	moveq	#0,d0					;
	rts								;

.dos
	dc.b	"dos.library",0

.text
	dc.b	"Error: Could not allocate enough memory",10
.textend
	even


*------	GENERATE SINE TABLE ----------------------------------------------*

; sine table, 1024 angle steps, factor 256

generatesintab
    lea     .sinb(pc),a0			;
    move.l	b_sintab(pc),a1			;
    move.w  #246-1,d7				;
.gensin
    moveq   #0,d0					;
    move.b  (a0)+,d0				;
    move.w  d0,(a1)+				;
    dbf     d7,.gensin				;
    
    move.l  a1,a0                   ; used for cos
    
    moveq   #10+11-1,d7             ; 10 values for sin, 11 values for cos
.fill256
    move.w  #$0100,(a1)+			;
    dbf     d7,.fill256				;

    move.w  #245-1,d7               ; cos
.gencos
    move.w  -(a0),(a1)+				;
    dbf     d7,.gencos				;

    move.w  #512-1,d7				;
    move.l	b_sintab(pc),a0			;
.genneg
    move.w  (a0)+,d0				;
    neg.w   d0						;
    move.w  d0,(a1)+				;
    dbf     d7,.genneg				;

    move.l	b_sintab(pc),a0			;
    move.w  #256-1,d7				;
.gensin2
    move.w  (a0)+,(a1)+				;
    dbf     d7,.gensin2				;
    rts								;
    
.sinb
    dc.b	$00,$02,$03,$05,$06,$08,$09,$0b
	dc.b	$0d,$0e,$10,$11,$13,$14,$16,$18
	dc.b	$19,$1b,$1c,$1e,$1f,$21,$22,$24
	dc.b	$26,$27,$29,$2a,$2c,$2d,$2f,$30
	dc.b	$32,$33,$35,$37,$38,$3a,$3b,$3d
	dc.b	$3e,$40,$41,$43,$44,$46,$47,$49
	dc.b	$4a,$4c,$4d,$4f,$50,$52,$53,$55
	dc.b	$56,$58,$59,$5b,$5c,$5e,$5f,$61
	dc.b	$62,$63,$65,$66,$68,$69,$6b,$6c
	dc.b	$6d,$6f,$70,$72,$73,$75,$76,$77
	dc.b	$79,$7a,$7b,$7d,$7e,$80,$81,$82
	dc.b	$84,$85,$86,$88,$89,$8a,$8c,$8d
	dc.b	$8e,$90,$91,$92,$93,$95,$96,$97
	dc.b	$99,$9a,$9b,$9c,$9e,$9f,$a0,$a1
	dc.b	$a2,$a4,$a5,$a6,$a7,$a8,$aa,$ab
	dc.b	$ac,$ad,$ae,$af,$b1,$b2,$b3,$b4
	dc.b	$b5,$b6,$b7,$b8,$b9,$ba,$bc,$bd
	dc.b	$be,$bf,$c0,$c1,$c2,$c3,$c4,$c5
	dc.b	$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd
	dc.b	$ce,$cf,$cf,$d0,$d1,$d2,$d3,$d4
	dc.b	$d5,$d6,$d7,$d7,$d8,$d9,$da,$db
	dc.b	$dc,$dc,$dd,$de,$df,$e0,$e0,$e1
	dc.b	$e2,$e3,$e3,$e4,$e5,$e5,$e6,$e7
	dc.b	$e7,$e8,$e9,$e9,$ea,$eb,$eb,$ec
	dc.b	$ed,$ed,$ee,$ee,$ef,$ef,$f0,$f1
	dc.b	$f1,$f2,$f2,$f3,$f3,$f4,$f4,$f5
	dc.b	$f5,$f5,$f6,$f6,$f7,$f7,$f8,$f8
	dc.b	$f8,$f9,$f9,$f9,$fa,$fa,$fa,$fb
	dc.b	$fb,$fb,$fc,$fc,$fc,$fd,$fd,$fd
	dc.b	$fd,$fd,$fe,$fe,$fe,$fe,$fe,$ff
	dc.b	$ff,$ff,$ff,$ff,$ff,$ff             ; 246 values 


;*****************************************************************
;
;	Light Speed Player v1.05 (modified)
;	Fastest Amiga MOD player ever :)
;	Written By Arnaud Carré (aka Leonard / OXYGENE)
;	https://github.com/arnaud-carre/LSPlayer
;	twitter: @leonard_coder
;
;	--------How to use--------- 
;
;	bsr LSP_MusicDriver+0 : Init LSP player code
;		In:	a0: LSP music data(any memory)
;			a1: LSP sound bank(chip memory)
;			a2: DMACON 8bits byte address (should be odd address!)
;		Out:a0: music BPM pointer (16bits)
;			d0: music len in tick count
;
;	bsr LSP_MusicDriver+4 : LSP player tick (call once per frame)
;		In:	a6: must be $dff000
;			Scratched regs: d0/d1/d2/a0/a1/a2/a3/a4/a5
;		Out:None
;
;*****************************************************************

lspplay		lea		LSPVars(pc),a1
			move.l	(a1),a0					; byte stream
.process	moveq	#0,d0
.cloop		move.b	(a0)+,d0
			bne		.swCode
			addi.w	#$0100,d0
			bra		.cloop
.swCode		add.w	d0,d0
			move.l	m_codeTableAddr(a1),a2	; code table
			move.w	(a2,d0.w),d0			; code
			beq		.noInst
			cmp.w	m_escCodeRewind(a1),d0
			beq		.r_rewind
			cmp.w	m_escCodeSetBpm(a1),d0
			beq		.r_chgbpm

			add.b	d0,d0
			bcc		.noVd
			move.b	(a0)+,$d9(a6)
.noVd		add.b	d0,d0
			bcc		.noVc
			move.b	(a0)+,$c9(a6)
.noVc		add.b	d0,d0
			bcc		.noVb
			move.b	(a0)+,$b9(a6)
.noVb		add.b	d0,d0
			bcc		.noVa
			move.b	(a0)+,$a9(a6)
.noVa		
			move.l	a0,(a1)+	; store byte stream ptr
			move.l	(a1),a0		; word stream

			tst.b	d0
			beq		.noPa

			add.b	d0,d0
			bcc		.noPd
			move.w	(a0)+,$d6(a6)
.noPd		add.b	d0,d0
			bcc		.noPc
			move.w	(a0)+,$c6(a6)
.noPc		add.b	d0,d0
			bcc		.noPb
			move.w	(a0)+,$b6(a6)
.noPb		add.b	d0,d0
			bcc		.noPa
			move.w	(a0)+,$a6(a6)
.noPa		tst.w	d0
			beq		.noInst

			moveq	#0,d1
			move.l	m_lspInstruments-4(a1),a2	; instrument table
			lea		resetv+12(pc),a4

			lea		$d0(a6),a5
			moveq	#4-1,d2
.vloop		add.w	d0,d0
			bcs		.setIns
			add.w	d0,d0
			bcc		.skip
			move.l	(a4),a3
			move.l	(a3)+,(a5)
			move.w	(a3)+,4(a5)
			bra		.skip
.setIns		add.w	(a0)+,a2
			add.w	d0,d0
			bcc		.noReset
			bset	d2,d1
			move.w	d1,$96(a6)
.noReset	move.l	(a2)+,(a5)
			move.w	(a2)+,4(a5)
			move.l	a2,(a4)
.skip		subq.w	#4,a4
			sub.w	#$10,a5
			dbf		d2,.vloop

			move.l	m_dmaconPatch-4(a1),a3		; dmacon patch
			move.b	d1,(a3)						; dmacon			

.noInst		move.l	a0,(a1)			; store word stream (or byte stream if coming from early out)
			rts

.r_rewind	move.l	m_byteStreamLoop(a1),a0
			move.l	m_wordStreamLoop(a1),m_wordStream(a1)
			bra		.process

.r_chgbpm	move.b	(a0)+,m_currentBpm+1(a1)	; BPM
			bra		.process

lspinit		lea		base(pc),a0				; a0: music data (any mem) + 10
			add.l	#lspmusic-base,a0		;
			move.l	lspbankbase(pc),a1		; a1: sound bank data (chip mem)
			move.l	clistbase(pc),a2		; a2: 16bit DMACON word address
			lea		lspdmacon+3-clist(a2),a2

			lea		LSPVars(pc),a3
			move.w	(a0)+,m_currentBpm(a3)	; default BPM
			move.w	(a0)+,m_escCodeRewind(a3)
			move.w	(a0)+,m_escCodeSetBpm(a3)
			move.l	(a0)+,-(a7)				; who cares? replace with addq.w #4,a0?
			move.l	a2,m_dmaconPatch(a3)
;PATCHED	move.w	#$8000,-1(a2)			; Be sure DMACon word is $8000 (note: a2 should be ODD address)
			move.w	(a0)+,d0				; instrument count
			lea		-12(a0),a2				; LSP data has -12 offset on instrument tab ( to win 2 cycles in fast player :) )
			move.l	a2,m_lspInstruments(a3)	; instrument tab addr ( minus 4 )
			subq.w	#1,d0
			move.l	a1,d1
.relocLoop	bset.b	#0,3(a0)				; bit0 is relocation done flag
			bne		.relocated
			add.l	d1,(a0)
			add.l	d1,6(a0)
.relocated	lea		12(a0),a0
			dbf		d0,.relocLoop
			move.w	(a0)+,d0				; codes count (+2)
			move.l	a0,m_codeTableAddr(a3)	; code table
			add.w	d0,d0
			add.w	d0,a0
			movem.l	(a0)+,d0-d2				; word stream size, byte stream loop point, word stream loop point
			move.l	a0,m_wordStream(a3)
			lea		(a0,d0.l),a1			; byte stream
			move.l	a1,m_byteStream(a3)
			add.l	d2,a0
			add.l	d1,a1
			move.l	a0,m_wordStreamLoop(a3)
			move.l	a1,m_byteStreamLoop(a3)
			lea		m_currentBpm(a3),a0
			move.l	(a7)+,d0				; music len in frame ticks? who cares? REMOVE?
			rts

	rsreset
m_byteStream		rs.l	1	;  0 byte stream
m_wordStream		rs.l	1	;  4 word stream
m_dmaconPatch		rs.l	1	;  8 m_lfmDmaConPatch
m_codeTableAddr		rs.l	1	; 12 code table addr
m_escCodeRewind		rs.w	1	; 16 rewind special escape code
m_escCodeSetBpm		rs.w	1	; 18 set BPM escape code
m_lspInstruments	rs.l	1	; 20 LSP instruments table addr
m_relocDone			rs.w	1	; 24 reloc done flag
m_currentBpm		rs.w	1	; 26 current BPM
m_byteStreamLoop	rs.l	1	; 28 byte stream loop point
m_wordStreamLoop	rs.l	1	; 32 word stream loop point
sizeof_LSPVars		rs.w	0

LSPVars		ds.b	sizeof_LSPVars
	even			
resetv		dc.l	0,0,0,0


*------	GFX STUFF --------------------------------------------------------*

sausboden
	dc.w	13

	dc.w	-144,48
	dc.w	-160,53
	
	dc.w	-120,48
	dc.w	-160,63
	
	dc.w	-96,48
	dc.w	-160,79
	
	dc.w	-72,48
	dc.w	-160,106
	
	dc.w	-48,48
	dc.w	-127,127
	
	dc.w	-24,48
	dc.w	-63,127
	
	dc.w	0,48
	dc.w	0,127
	
	dc.w	24,48
	dc.w	63,127
	
	dc.w	48,48
	dc.w	127,127
	
	dc.w	72,48
	dc.w	159,106
	
	dc.w	96,48
	dc.w	159,79
	dc.w	120,48
	dc.w	159,63
	dc.w	144,48
	dc.w	159,53


numparticlestc		equ	pT1+pE1+pR1o+pR1i+pR2o+pR2i+pA1o+pA1i+pC+pR3o+pR3i+pE2+pS+pT2+pA2o+pA2i
numparticlessp		equ	spS+spP1+spR+spE+spAo+spAi+spD+spP2+spOo+spOi+spI+spN+spT
numparticlessca		equ	scaS+scaC+scaAo+scaAi
numparticlesatari	equ pAl+pAm+pAr
maxparticles		equ numparticlestc

logoterracresta
pT1		equ	10
	dc.w	-135,84, -76,110, -85,81, -98,76, -114,21, -157,3, -169,21, -130,37
	dc.w	-121,66, -143,59, -135,84
pE1		equ	12
	dc.w	-71,113, -29,131, -33,106, -53,98, -55,90, -35,96, -37,76, -61,68
	dc.w	-63,59, -38,67, -41,46, -95,27, -71,113
pR1o	equ	11
	dc.w	-41,159, 14,159, 28,146, 29,121, 19,110, 37,48, 11,56, -1,104
	dc.w	-4,57, -30,49, -22,138, -41,159
pR1i	equ	4
	dc.w	0,141, 8,136, 8,128, 0,124, 0,141
pR2o	equ	11
	dc.w	34,130, 78,108, 89,92, 90,78, 81,70, 92,31, 72,37, 64,68
	dc.w	59,70, 62,40, 41,47, 34,130
pR2i	equ	5
	dc.w	57,97, 66,94, 69,89, 64,85, 57,88, 57,97
pA1o	equ	11
	dc.w	87,105, 131,86, 145,35, 174,18, 165,1, 126,20, 125,35, 114,38
	dc.w	114,25, 95,31, 97,81, 87,105
pA1i	equ	3
	dc.w	116,67, 120,56, 114,57, 116,67
pC	equ	17
	dc.w	-209,39, -192,47, -152,-11, -148,-21, -165,-52, -196,-52, -173,-25, -191,0
	dc.w	-216,-78, -126,-78, -36,-78, 0,-101, 0,-133, -36,-97, -144,-97, -252,-97, -230,-29, -209,39
pR3o	equ	11
	dc.w	-133,9, -93,19, -71,1, -75,-28, -92,-41, -81,-73, -107,-73, -114,-46
	dc.w	-127,-48, -133,-73, -158,-73, -133,9
pR3i	equ	5
	dc.w	-114,-9, -97,-5, -92,-12, -95,-19, -119,-25, -114,-9
pE2		equ	12
	dc.w	-58,34, -2,51, -2,25, -35,17, -38,-2, -2,1, -2,-23, -42,-29
	dc.w	-45,-46, -2,-47, -2,-74, -76,-72, -58,34
pS		equ	15
	dc.w	19,46, 60,33, 62,4, 29,15, 30,0, 62,-16, 73,-42, 43,-73
	dc.w	3,-73, 3,-46, 41,-46, 43,-32, 16,-17, 4,4, 4,31, 19,46
pT2		equ	8
	dc.w	66,31, 135,9, 139,-19, 116,-12, 123,-72, 93,-72, 88,-7, 69,-2
	dc.w	66,31
pA2o	equ	15
	dc.w	196,46, 210,39, 232,-29, 254,-97, 146,-97, 38,-97, 0,-133, 0,-101
	dc.w	38,-78, 129,-78, 220,-78, 209,-48, 174,-41, 158,-72, 129,-72, 196,46
pA2i	equ	3
	dc.w	196,-3, 203,-23, 187,-19, 196,-3
		
logospreadpoint
spS		equ	16
lspS
	dc.w	-351,153, -247,153, -247,126, -336,126, -336,99, -259,99, -247,87, -247,30
	dc.w	-259,18, -363,18, -363,45, -273,45, -273,72, -351,72, -363,84, -363,141, -351,153
spP1	equ	13
lspP1
	dc.w	-240,153, -138,153, -124,141, -124,87, -138,72, -213,72, -213,18, -240,18
	dc.w	-240,87, -228,99, -150,99, -150,126, -240,126, -240,153
spR		equ	13
lspR
	dc.w	-117,153, -15,153, -3,141, -3,87, -15,72, -36,72, -3,39, -24,18
	dc.w	-78,72, -78,99, -28,99, -28,126, -117,126, -117,153
spE		equ	14
lspE
	dc.w	16,153, 121,153, 121,126, 33,126, 33,99, 121,99, 121,72, 33,72
	dc.w	33,45, 121,45, 121,18, 16,18, 4,30, 4,141, 16,153
spAo	equ	10
lspAo
	dc.w	141,153, 232,153, 244,141, 244,18, 217,18, 217,72, 154,72, 154,18
	dc.w	129,18, 129,141, 141,153
spAi	equ	4
lspAi
	dc.w	154,126, 217,126, 217,99, 154,99, 154,126
spD		equ	12
lspD
	dc.w	252,153, 357,153, 369,141, 369,30, 357,18, 252,18, 252,99, 279,99
	dc.w	279,45, 345,45, 345,126, 252,126, 252,153
spP2	equ	13
lspP2
	dc.w	-262,3, -160,3, -147,-9, -147,-63, -160,-78, -235,-78, -235,-132, -262,-132
	dc.w	-262,-63, -250,-51, -172,-51, -172,-24, -262,-24, -262,3
spOo	equ	8
lspOo
	dc.w	-126,3, -33,3, -21,-9, -21,-120, -33,-132, -126,-132, -138,-120, -138,-9
	dc.w	-126,3
spOi	equ	4
lspOi
	dc.w	-111,-24, -48,-24, -48,-105, -111,-105, -111,-24
spI		equ	4
lspI
	dc.w	-10,3, 16,3, 16,-132, -10,-132, -10,3
spN		equ	10
lspN
	dc.w	40,3, 135,3, 147,-9, 147,-132, 118,-132, 118,-24, 55,-24, 55,-132
	dc.w	28,-132, 28,-9, 40,3
spT		equ	7
lspT
	dc.w	154,3, 256,3, 268,-9, 268,-132, 241,-132, 241,-24, 154,-24, 154,3

logosca
scaS	equ	32
	dc.w	-292,112, -247,112, -202,112, -157,112, -120,112, -120,67, -165,67, -210,67
	dc.w	-267,67, -267,22, -222,22, -177,22, -140-1,22, -120,2, -120,-42, -120,-92
	dc.w	-140-1,-112, -185,-112, -230,-112, -275,-112, -312,-112, -312,-67, -267,-67, -222,-67
	dc.w	-162,-67, -162,-22, -207,-22, -252,-22, -292,-22, -312,-2, -312,42, -312,92
	dc.w	-292,112
scaC	equ	25
	dc.w	-75+1,112, -30,112, 15,112, 60,112, 100,112, 100,67, 55,67, 10,67
	dc.w	-47,67, -47,22, -47,-22, -47,-67, -2,-67, 42,-67, 100,-67, 100,-112
	dc.w	55,-112, 10,-112, -35,-112, -75+1,-112, -95,-92, -95,-47, -95,-2, -95,42
	dc.w	-95,92, -75+1,112
scaAi	equ	6
	dc.w	165,67, 217,67, 270,67, 270,22, 217,22, 165,22, 165,67
scaAo	equ	23
	dc.w	142,112, 187,112, 232,112, 277,112, 295,112, 315,92, 315,47, 315,2
	dc.w	315,-42, 315,-112, 270,-112, 270,-67, 270,-22, 225,-22, 165,-22, 165,-67
	dc.w	165,-112, 122,-112, 122,-67, 122,-22, 122,22, 122,67
logoscareversed
	dc.w	122,92, 142,112

logoatari
pAl		equ	36
	dc.w	-49,110, -28,110, -28,90, -28,72, -29,53, -29,35, -30,16, -32,-1
	dc.w	-34,-20, -38,-38, -45,-55, -53,-72, -63,-87, -76,-101, -89,-113, -105,-124
	dc.w	-121,-133, -137,-141, -155,-147, -172,-148, -172,-130, -172,-111, -157,-105, -140,-98
	dc.w	-123,-89, -109,-77, -96,-64, -84,-50, -74,-34, -66,-17, -61,0, -57,18
	dc.w	-54,36, -52,55, -50,73, -49,91, -49,110
pAm		equ	32
	dc.w	-19,110, -1,110, 19,110, 19,91, 19,73, 19,54, 19,36, 19,17
	dc.w	19,0, 19,-19, 19,-37, 19,-56, 19,-74, 19,-93, 19,-111, 19,-130
	dc.w	19,-148, -1,-148, -19,-148, -19,-130, -19,-111, -19,-93, -19,-74, -19,-56
	dc.w	-19,-37, -19,-19, -19,0, -19,17, -19,36, -19,54, -19,73, -19,91
	dc.w	-19,110
pAr		equ	36
	dc.w	49,110, 28,110, 28,90, 28,72, 29,53, 29,35, 30,16, 32,-1
	dc.w	34,-20, 38,-38, 45,-55, 53,-72, 63,-87, 76,-101, 89,-113, 105,-124
	dc.w	121,-133, 137,-141, 155,-147, 172,-148, 172,-130, 172,-111, 157,-105, 140,-98
	dc.w	123,-89, 109,-77, 96,-64, 84,-50, 74,-34, 66,-17, 61,0, 57,18
	dc.w	54,36, 52,55, 50,73, 49,91, 49,110


*------	VIPER ------------------------------------------------------------*

numviper3d			equ	(viper3dend-viper3d)/12
numviper3dlasers 	equ (viper3dend-viper3dlasers)/12

vs	equ	8
vt	equ	8*256
vz	equ	200+2560

viper3d
	dc.l	60*vs+vz,0*vt,0*vt
	dc.l	-40*vs+vz,40*vt,0*vt
	dc.l	-40*vs+vz,-40*vt,0*vt
	dc.l	40*vs+vz,0*vt,-8*vt
	dc.l	-40*vs+vz,30*vt,-10*vt
	dc.l	-40*vs+vz,-30*vt,-10*vt
	dc.l	40*vs+vz,0*vt,8*vt
	dc.l	-40*vs+vz,30*vt,10*vt
	dc.l	-40*vs+vz,-30*vt,10*vt
	dc.l	10*vs+vz,0*vt,-9*vt
 	dc.l	-35*vs+vz,-15*vt,-10*vt
	dc.l	-35*vs+vz,15*vt,-10*vt

	dc.l	-41*vs+vz,10*vt,0*vt
	dc.l	-41*vs+vz,15*vt,5*vt
	dc.l	-41*vs+vz,25*vt,5*vt
	dc.l	-41*vs+vz,30*vt,0*vt
	dc.l	-41*vs+vz,25*vt,-5*vt
	dc.l	-41*vs+vz,15*vt,-5*vt

	dc.l	-41*vs+vz,-10*vt,0*vt
	dc.l	-41*vs+vz,-15*vt,-5*vt
	dc.l	-41*vs+vz,-25*vt,-5*vt
	dc.l	-41*vs+vz,-30*vt,0*vt
	dc.l	-41*vs+vz,-25*vt,5*vt
	dc.l	-41*vs+vz,-15*vt,5*vt
viper3dlasers
	; (index 24 starts here) 1 laser pair = 2 lines, 4 indices
	rept maxlaserpairs
	dc.l	-1,0,0	; index 24 laser 1 left (-1 inactive)
	dc.l	-1,0,0
	dc.l	-1,0,0	; laser 1 right
	dc.l	-1,0,0
	endr
viper3dend

	rsreset
laserz		rs.l	1
laserx		rs.l	1
lasery		rs.l	1
lasersize	rs.l	0

numviperlines 		equ (viperlinesend-viperlines)/4
numviperlineslasers equ (viperlinesend-viperlineslasers)/4

vi	equ	4

viperlines
	dc.w	0*vi,3*vi, 3*vi,4*vi, 4*vi,1*vi, 1*vi,0*vi
	dc.w	6*vi,0*vi, 0*vi,1*vi, 1*vi,7*vi, 7*vi,6*vi
	dc.w	5*vi,3*vi, 3*vi,0*vi, 0*vi,2*vi, 2*vi,5*vi
	dc.w	8*vi,2*vi, 2*vi,0*vi, 0*vi,6*vi, 6*vi,8*vi
	dc.w	1*vi,4*vi, 4*vi,5*vi, 5*vi,2*vi, 2*vi,8*vi, 8*vi,7*vi, 7*vi,1*vi
	dc.w	3*vi,5*vi, 5*vi,4*vi, 4*vi,3*vi
	dc.w	6*vi,7*vi, 7*vi,8*vi, 8*vi,6*vi
	dc.w	9*vi,10*vi, 10*vi,11*vi, 11*vi,9*vi
	dc.w	12*vi,13*vi, 13*vi,14*vi, 14*vi,15*vi, 15*vi,16*vi, 16*vi,17*vi, 17*vi,12*vi
	dc.w	18*vi,19*vi, 19*vi,20*vi, 20*vi,21*vi, 21*vi,22*vi, 22*vi,23*vi, 23*vi,18*vi
viperlineslasers
index set 24
; generates indices for 1 laser pair: dc.w 24*vi,25*vi,26*vi,27*vi
	rept	maxlaserpairs*4
	dc.w	index*vi
index set index+1 
	endr
viperlinesend


*------	ANIMATION SEQUENCE -----------------------------------------------*

; viper appears
aseq
	dc.w	-410, -392, -375, -358, -341, -324, -308, -291
	dc.w	-275, -260, -244, -229, -214, -199, -185, -170
	dc.w	-156, -142, -129, -115, -102, -89, -77, -64
	dc.w	-52, -40, -28, -17, -6, 5, 16, 27
	dc.w	37, 47, 57, 67, 76, 85, 94, 103
	dc.w	111, 119, 127, 135, 142, 149, 156, 163
	dc.w	170, 176, 182, 188, 193, 199, 204, 209
	dc.w	213, 218, 222, 226, 229, 233, 236, 239
	dc.w	242, 244, 246, 248, 250, 252, 253, 254
	dc.w	255, 256
aseqend


*------	MUSIC ------------------------------------------------------------*

lspbank
	incbin	"vectrax_by_lord_sp.lsbank"
lspbankend

	even
lspmusic
	incbin	"vectrax_by_lord_sp.lsmusic",10	; skip header (10 bytes)
