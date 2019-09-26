;	Black Moon
;	(C) 1994-96 CYDoNiA
	
		incdir	hd1:cdn/blackmoon/
		include	includes/custom2.i
		include	music/maccros.i


fade_workbench	equ	0
screen_res	equ	0		; 0 = lowres, 1 = hires.
screen_width    equ     44
screen_height   equ     276
screen_depth    equ     4
screen_bitplane equ     screen_width*screen_height
screen_size     equ     screen_depth*screen_bitplane
screen_interleaved	equ	1	; 0 = yes, 1 = no.

screen_bitplaneoffset	equ	screen_bitplane
screen_nextlineoffset	equ	screen_width

numtextlines	equ	4	;20	; lines of text to be displayed in digiwriter
xmin            equ     0
xmax            equ     (screen_width*8)-1
ymin            equ     0
ymax            equ     screen_height-1


; compile equates
yes		equ	0
no		equ	1

music_routine	equ	yes
dt_routine	equ	yes
title_routine	equ	yes
final		equ	no

; routine equates
	IFEQ	dt_routine
; dot tunnel
letdelay	equ	150
num_dots	equ	32	;30
num_circles	equ	256
num_circleson	equ	32
circle_radius	equ	64
circle_dist	equ	256
circle_zstart	equ	-192
	ENDC

*******************************************************************************

startup:	bsr	take_system
		tst.l	d0
		beq.s	.error
		bsr	initiations
		bsr	main
		move.l	oldirq,$6c
	IFEQ	music_routine
		stop_music
	ENDC
		bsr	free_system
		moveq	#0,d0
.error		rts

		include	includes/takefree_system.i

******************************************************************************
************************* demo controls
	IFEQ	title_routine
run_title
		bsr	initial_wait
		bsr	init_title
		bsr	title
		bsr	setpic2
		bsr	title
		bsr	setpic3
		bsr	title
		rts

initial_wait
.wait_vbl	tst.w	vblank
		beq.s	.wait_vbl
		clr.w	vblank
		subq.w	#1,fdelay
		bne.s	.wait_vbl
		move.w	#100,fdelay
		rts
	ENDC
	IFEQ	dt_routine
run_dt
.wait_vbl	tst.w	vblank
		beq.s	.wait_vbl
		clr.w	vblank
;		move.l  vposr(a5),d0		; synchronise to v-blank.
;		and.l   #$1ff00,d0
;		cmp.l	#$13700,d0
;		bne.s	run_dt

		tst.w	finished_dt
		beq.s	.out

		move.w	dt_state,d0
		lsl.w	#2,d0
		move.l	dt_rout(pc,d0.w),a0
		jsr	(a0)

		btst	#6,$bfe001		; lmb ?
		bne.s	run_dt
.out		rts

dt_rout		dc.l	init_screen
		dc.l	run_dot_tunnel

dt_state	dc.w	0

finished_dt	dc.w	numtextlines		; =lines of text+1

init_screen
		move.w	#$22<<8+$71,diwstrt(a5)
		move.w	#($136-$100)<<8+($1d1-$100),diwstop(a5)
		move.w	#0+color,bplcon0(a5)
		move.w	#($71-17)/2,ddfstrt(a5)
		move.w	#($71-17)/2+8*(($1d1-$71)/16-1),ddfstop(a5)
		move.w	#44,bpl1mod(a5)
		move.w	#44,bpl2mod(a5)

		movem.l	screen_col,d0-d7
		movem.l	d0-d7,color00(a5)
		addq.w	#1,dt_state
		rts

vblank		dc.w	0
screen_col	dc.w	$000,$ddd,$242,$242,$464,$464,$686,$686
		dc.w	$8a8,$8a8,$aca,$aca,$eee,$eee,$06e,$06e



	ENDC


; a5 = custom
; a6 = data_segment

main:
; setup and run the welcome screen
		bsr	run_title	; change order to suit demo
; setup and run Dot Tunnel
		move.l  #dt_copper,cop1lch(a5)
		move.w  #set+dmaen+blten+bplen+copen,dmacon(a5)
		lea	screen1,a0
		move.l	a0,show_screen(a6)
		lea	screen2,a0
		move.l	a0,work_screen(a6)
		lea	screen3,a0
		move.l	a0,idle_screen(a6)
		bsr	run_dt

; setup and run Plasma routine
		bsr	plasmascroll              ; this is the actual

		rts


*******************************************************************************

initiations:	lea	custom,a5
		lea	data_segment,a6

		
		move.l  #copper,cop1lch(a5)
		move.w  #set+dmaen+blten+bplen+copen,dmacon(a5)

		lea	cust_registers,a0
		move.w	#((cust_registers_end-cust_registers)/4)-1,d0
.loop           move.w	(a0)+,d1
		move.w	(a0)+,00(a5,d1.w)
		dbra	d0,.loop

	IFEQ	music_routine
		move.l	$6c,oldirq
		move.w	#$7fff,intena(a5)	; install blitter q interrupt.
		move.l	#music_irq,$6c
		move.w	#set+inten+vertb,intena(a5)
	ENDC
	
************************ program initiations.
	IFEQ	music_routine
		init_music
	ENDC
; start dot-tunnel stuff
		bsr	calculate_circles
		lea	x_sinus(pc),a0
		lea	y_sinus(pc),a1
		lea	centre_list,a2

		move.w	x_sinus_pos(a6),d0
		move.w	y_sinus_pos(a6),d1

		move.w	#num_circles-1,d7
.daloop
		move.w	(a0,d0.w),d2
		add.w	#12*2,d0
		and.w	#1024*2-2,d0
		move.w	(a1,d1.w),d3
		add.w	#8*2,d1
		and.w	#1024*2-2,d1

		addq	#2,a2
		move.w	d2,(a2)+
		move.w	d3,(a2)+
		dbra	d7,.daloop

		move.w	d0,x_sinus_pos(a6)
		move.w	d1,y_sinus_pos(a6)
		bsr	make_text

		lea	digi_cop,a0
		lea	nscreen1,a1
		move.l	a1,d0
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		addq.w	#8,a0
		lea	nscreen2,a1
		move.l	a1,d0
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		addq.w	#8,a0
		lea	nscreen3,a1
		move.l	a1,d0
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)

; for plasma routine
		bsr.w	set_lookups
		bsr	make_picture

		rts

oldirq		dc.l	0

	
	IFEQ	dt_routine
init_dt
		jsr	calculate_circles
		lea	x_sinus,a0
		lea	y_sinus,a1
		lea	centre_list,a2
		move.w	x_sinus_pos(a6),d0
		move.w	y_sinus_pos(a6),d1
		move.w	#num_circles-1,d7
.daloop
		move.w	(a0,d0.w),d2
		add.w	#12*2,d0
		and.w	#1024*2-2,d0
		move.w	(a1,d1.w),d3
		add.w	#8*2,d1
		and.w	#1024*2-2,d1
		addq	#2,a2
		move.w	d2,(a2)+
		move.w	d3,(a2)+
		dbra	d7,.daloop
		move.w	d0,x_sinus_pos(a6)
		move.w	d1,y_sinus_pos(a6)
		bsr	make_text
		rts
make_text
		lea	text2,a0
.loop		moveq	#0,d0
		move.b	(a0),d0
		beq	.end
		cmp.b	#" ",d0
		beq	.space
		sub.w	#$40,d0
		bra.s	.skip
.space		sub.w	#$20,d0
.skip		move.b	d0,(a0)+
		bra.s	.loop
.end		lea	text2,a0
		lea	compare1,a1
		rept	30
		move.b	(a0)+,(a1)+
		endr
		rts
	ENDC	; dt_routine

	IFEQ	title_routine
init_title
		move.w	#ystrt<<8+xstrt,diwstrt(a5)
		move.w	#(ystop-$100)<<8+(xstop-$100),diwstop(a5)
		move.w	#hires+screen_depth<<12+color+lace,bplcon0(a5)
		move.w	#$3c,ddfstrt(a5)
		move.w	#$d4,ddfstop(a5)
		move.w	#0,bpl1mod(a5)
		move.w	#0,bpl2mod(a5)
		movem.l	blank,d0-d7
		movem.l	d0-d7,color00(a5)	; black
		move.l  #t_copper1,cop1lch(a5)
		lea	screen1+32,a0
		lea	t_copper1,a1
		rept	4
		move.l	a0,d0
		move.w	d0,6(a1)
		swap	d0
		move.w	d0,2(a1)
		addq.w	#8,a1
		addi.l	#80*240,a0
		endr
		lea	screen1+32,a0
		lea	t_copper2,a1
		rept	4
		move.l	a0,d0
		move.w	d0,6(a1)
		swap	d0
		move.w	d0,2(a1)
		addq.w	#8,a1
		addi.l	#80*240,a0
		endr
		lea	screen1+64,a0
		lea	t_copper3,a1
		rept	5
		move.l	a0,d0
		move.w	d0,6(a1)
		swap	d0
		move.w	d0,2(a1)
		addq.w	#8,a1
		addi.l	#40*256,a0
		endr
		lea	pic1,a0
		lea	screen1,a1
		jsr	ice_decrunch
		lea	screen1,a0
		lea	dest_pal,a1
		movem.l	(a0),d0-d7
		movem.l	d0-d7,(a1)
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#16,d0
		move.w	#3,d1
		move.l	a5,-(a7)
		jsr	trigfadeto
		move.l	(a7)+,a5
		rts

setpic2		
		lea	pic2,a0
		lea	screen1,a1
		jsr	ice_decrunch
		move.w	#$2c<<8+xstrt,diwstrt(a5)
		move.w	#($11c-$100)<<8+(xstop-$100),diwstop(a5)
		lea	screen1,a0
		lea	dest_pal,a1
		movem.l	(a0),d0-d7
		movem.l	d0-d7,(a1)
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#16,d0
		move.w	#3,d1
		move.l	a5,-(a7)
		jsr	trigfadeto
		move.l	(a7)+,a5
		move.w	#0,state
		move.w	#200,fdelay
		move.l	#t_copper2,cop1lch(a5)
		move.w	#1,logopic
		rts

setpic3		
		lea	pic3,a0
		lea	screen1,a1
		jsr	ice_decrunch
		move.w	#$38,ddfstrt(a5)	; lores 320x256
		move.w	#$d0,ddfstop(a5)
		move.w	#$2c<<8+xstrt,diwstrt(a5)
		move.w	#($12c-$100)<<8+(xstop-$100),diwstop(a5)
		move.w	#5<<12+color,bplcon0(a5)
		lea	screen1,a0
		lea	dest_pal,a1
		movem.l	(a0)+,d0-d7
		movem.l	d0-d7,(a1)
		add.w	#32,a1
		movem.l	(a0),d0-d7
		movem.l	d0-d7,(a1)
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#32,d0
		move.w	#3,d1
		move.l	a5,-(a7)
		jsr	trigfadeto
		move.l	(a7)+,a5
		move.w	#0,state
		move.w	#500,fdelay
		move.l	#t_copper3,cop1lch(a5)
		move.w	#32,numcols
		move.w	#1,the_title
		rts
the_title	dc.w	1
	ENDC	;title_routine

*******************************************************************************
*******************************************************************************
************************ code.
	IFEQ	title_routine
title:

.wait_vbl	tst.w	vblank
		beq.s	.wait_vbl
		clr.w	vblank

		move.w	state,d0
		cmp.w	#0,d0
		bne.s	.not0
		bsr	fadeup
		bra.s	.loop
.not0		cmp.w	#1,state
		bne.s	.not1
		bsr.s	setfadedown
		bra.s	.loop
.not1		cmp.w	#2,state
		bne.s	.not2
		bsr	wait
		bra.s	.loop
.not2		cmp.w	#3,state
		bne.s	.not3
		bsr	fadedown
		bra.s	.loop
.not3		bra.s	.exit		; finished fade down
.loop		
	IFEQ	final
		bra.w	title
	ELSE
		btst	#6,$bfe001		; lmb ?
		bne.w	title
	ENDC
.exit		rts

fadeup		bsr	fadeit
		tst.b	d0
		bne.s	.exit
		addq.w	#1,state
.exit		rts

setfadedown	lea	dest_pal,a0
		rept	16
		move.l	#0,(a0)+
		endr
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	numcols,d0
		move.w	#5,d1
		move.l	a5,-(a7)
		jsr	trigfadeto
		move.l	(a7)+,a5
		addq.w	#1,state
		rts

wait		cmp.w	#1,logopic
		bne.s	.nodecrunchlogo
		move.w	#0,logopic
.nodecrunchlogo
		tst.w	the_title
		beq.s	.skip_startdt
		movem.l	a0-a6/d0-d7,-(a7)
		bsr	start_dt
		bsr	start_dt
		bsr	start_dt
		bsr	start_dt
		bsr	start_dt
		bsr	start_dt
		movem.l	(a7)+,a0-a6/d0-d7
		tst.w	onyet
		bne.s	.exit
.skip_startdt
		subq.w	#1,fdelay
		cmp.w	#0,fdelay
		bgt.s	.exit
		addq.w	#1,state
.exit		rts

logopic		dc.w	0

fadedown	bsr	fadeit
		tst.b	d0
		bne.s	.exit
		addq.w	#1,state
.exit		rts

fadeit		move.l	a5,-(a7)
		jsr	fade_to
		move.b	fadetoflag(a5),d0
		move.l	(a7)+,a5
		move.w	d0,-(a7)
		lea	source_pal,a0
		movem.l	(a0)+,d0-d7
		movem.l	d0-d7,color00(a5)
		movem.l	(a0)+,d0-d7
		movem.l	d0-d7,color16(a5)
		move.w	(a7)+,d0
		rts
	ENDC
	

	IFEQ	dt_routine
start_dt
		move.l	a5,-(sp)
		bsr	dot_tunnel
		move.l	(sp)+,a5
;		subq.w	#1,onyet
		bne.s	.start_exit
		move.w	#0,the_title
.start_exit	rts

run_dot_tunnel
		move.l  work_screen(a6),a0	; shift screens.
		move.l  idle_screen(a6),work_screen(a6)
		move.l  show_screen(a6),idle_screen(a6)
		move.l  a0,show_screen(a6)
		move.l	show_screen(a6),a0
		lea	138*88+22(a0),a0
		move.l	a0,bpl1pth(a5)	; first screen

blitfree	btst	#6,dmaconr(a5)		; blitter clear the idlescreen.
		bne.s	blitfree
		move.l	#USED<<16,bltcon0(a5)
		move.l	idle_screen(a6),a0	; FUDGED!
		lea	138*88+22(a0),a0
		move.l	a0,bltdpth(a5)
		move.w	#44,bltdmod(a5)
	move.w	#64*screen_height*1+screen_width/2,bltsize(a5)
		move.l	a5,-(sp)
		bsr	dot_tunnel
		move.l	(sp)+,a5
		bsr	digiwriter
		rts		

finished	dc.w	0

digiwriter
		move.w	digi_state,d0
		lsl.w	#2,d0
		move.l	dg_routs(pc,d0.w),a0
		jsr	(a0)
back		rts

dg_routs	dc.l	letters_up
		dc.l	wait_letters
		dc.l	letters_down
		dc.l	res_digi

res_digi	
		move.w	#0,digi_state
		subq.w	#1,finished_dt
;		move.w	#1,finished_dt
		rts

wait_letters
		bsr	draw
		move.w	letter_delay,d0
		subq.w	#1,d0
		tst.w	d0
		beq.s	reset_wait
		move.w	d0,letter_delay
		rts

reset_wait	move.w	#letdelay,d0
		move.w	d0,letter_delay
		addq.w	#1,digi_state
		rts

letters_up
		moveq	#0,d2
		moveq	#0,d3
		lea	onscreen1,a0
		lea	compare1,a1
		move.w	#30-1,d7
		moveq	#0,d0
.loop		
		move.b	(a1),d2
		move.b	(a0),d3
		cmp.b	d2,d3
		beq	.skip
		addq.b	#1,(a0)
		moveq	#1,d0
.skip		
		addq.l	#1,a0
		addq.l	#1,a1
		dbf	d7,.loop
		bsr	draw

		tst.w	d0
		beq.s	.newstate
		rts
.newstate	addq.w	#1,digi_state
		rts

letters_down
		lea	onscreen1,a0
		move.w	#30-1,d7
		moveq	#0,d0
.loop		
		move.b	(a0),d2
		cmp.b	#0,d2
		beq	.skip
		subq.b	#1,(a0)
		moveq	#1,d0
.skip		
		addq.w	#1,a0
		dbf	d7,.loop
		bsr	draw

		tst.w	d0
		beq.s	.newstate
		rts
.newstate	addq.w	#1,digi_state
		movea.l	textp,a0
		lea	compare1,a1
		rept	30
		move.b	(a0)+,(a1)+
		endr
		move.l	a0,textp
		rts

draw	
		movem.l	d0-d1/a0-a1,-(a7)
		lea	nscreen1+28,a0		; work screen
		lea	nscreen2+28,a1		; work screen
		lea	nscreen3+28,a2		; work screen

		lea	onscreen1,a4		; onscreen
		moveq	#1,d7
fontdrawloop
		rept	15			; onscreen width
		moveq	#0,d0
		move.b	(a4)+,d0		; first letter
		lea	digifont+16,a3
		add.w	d0,a3			; offset into font
x		set	0
y		set	0
		rept	14
		move.b	x(a3),y(a0)		; plane 1
		move.b	x+560(a3),y(a1)		; plane 2
		move.b	x+560+560(a3),y(a2)	; plane 3
x		set	x+40
y		set	y+44*2
		endr
		addq.w	#1,a0
		addq.w	#1,a1
		addq.w	#1,a2
		endr

		lea	nscreen1+28+(30*44),a0		; work screen
		lea	nscreen2+28+(30*44),a1		; work screen
		lea	nscreen3+28+(30*44),a2		; work screen

		dbf	d7,fontdrawloop
		movem.l	(a7)+,d0-d1/a0-a1
		rts


dot_tunnel:	lea	x_sinus(pc),a0		; get new x-dir sinus value.
		move.w	x_sinus_pos(a6),d0
		move.w	(a0,d0.w),a3
		add.w	#4*2,d0
		and.w	#1024*2-2,d0
		move.w	d0,x_sinus_pos(a6)

		lea	y_sinus(pc),a0		; get new y-dir sinus value.
		move.w	y_sinus_pos(a6),d0
		move.w	(a0,d0.w),a4
		add.w	#2*2,d0
		and.w	#1024*2-2,d0
		move.w	d0,y_sinus_pos(a6)

		lea	centre_list(pc),a0
		lea	circle_data(pc),a1
		sub.l	#num_dots*4,a1
		move.l	work_screen(a6),a2
		move.w	z_add(a6),d5		; increment z-movement.
		move.w	#num_circles-1,d6
		subq.w	#2,d5
		and.w	d6,d5
		move.w	d5,z_add(a6)

		moveq	#0,d2

		move.w	#num_circleson,d7
		bra	.lo

.loop		move.w	(a0)+,d0		; get z for circle.
		add.w	d5,d0			; add the movement.
		and.w	d6,d0
		bne.s	.cont
		move.w	a3,(a0)		; new sinus to x,y if circle has moved
		move.w	a4,2(a0)	; thru its entire z-path.
.cont
		move.w	(a0)+,d3
		move.w	(a0)+,d4
		mulu	#num_dots*4,d0	; find the perspectified circle
		move.l	a1,a5		; corresponding to the z coordinate.
		add.l	d0,a5

		tst.w	onyet
		beq.s	.yess
		subq.w	#1,onyet
		bra.w	.lo
.yess
		rept	num_dots
		move.w	(a5)+,d0	; get a point on the circle.
		move.w	(a5)+,d1
		add.w	d3,d0		; add sinii.
		add.w	d4,d1

		move.w	d0,d2		; write out pixel.
		lsr.w	#3,d2
		add.w	d1,d2
		not.b	d0
		bset	d0,(a2,d2.l)
		endr

.lo		dbra	d7,.loop	; loop for all circles.
		rts

************************
onyet		dc.w	num_circleson*num_circleson*4

calculate_circles:
		move.w	#num_circles-1,d6
		move.w	#circle_zstart+circle_dist,d2
		lea	circle_data(pc),a0
		lea	sinetable32767,a1
		lea	256*2(a1),a2
.build_circles:	
		move.w	#0,d5
		move.w	#num_dots-1,d7
.calc_circle:	move.w	#circle_radius,d0
		move.w	#0,d1
		move.w	d0,d3
		move.w	d1,d4

		muls	(a1,d5.w),d1
		muls	(a2,d5.w),d0
		add.l	d0,d1
		muls	(a1,d5.w),d3
		muls	(a2,d5.w),d4
		sub.l	d3,d4
		add.w	#(1024/num_dots)*2,d5
		and.w	#1024*2-2,d5

		divs	d2,d1
		divs	d2,d4
		asr.w	#15-8,d1
		asr.w	#15-8,d4

		add.w	#352,d1			; hardcoded!
		add.w	#276,d4			; hardcoded!
		mulu	#88,d4

		move.w	d1,(a0)+
		move.w	d4,(a0)+
		dbra	d7,.calc_circle

		add.w	#2,d2
		dbra	d6,.build_circles
		rts
	ENDC	; dt_routine

******************************************************************************
************************* utility code

random_byte:	move.b	$dff007,d0
		move.b	$bfd800,d1
		eor.b	d1,d0		
		rts
;----------------------------------------------------------
; Fade routs.
;----------------------------------------------------------
TrigFadeTo	lea 	fadeto_vars(pc),a5
		move.l 	A0,fadeto_curptr(a5)
		move.l 	A1,fadeto_palptr(a5)
		move.w 	d0,fadeto_nocols(a5)
		move.w 	#16,fadeto_cnt(a5)
		move.b 	d1,fadeto_tim(a5)	; speed delay in calls to rout
		move.b 	d1,fadeto_spd(a5)
		st.b 	fadetoflag(a5)
		rts

	rsreset
fadeto_curptr	rs.l 1	; current palette(from -> dest)
fadeto_palptr	rs.l 1	; destination palette
fadeto_nocols	rs.w 1	; no of colours in palette
fadeto_cnt	rs.w 1	; count until fade is done.
fadeto_tim	rs.b 1	; vbl count
fadeto_spd	rs.b 1	; vbl fade delay speed
fadetoflag:	rs.b 1	; fade flag(true if fading)
fadeto_varsize	rs.b 1	; structure size

Fade_to		
		lea 	fadeto_vars(pc),a5
		tst.b 	fadetoflag(a5)		; fading ?
		beq 	.nofadeto	
		subq.b 	#1,fadeto_tim(a5) 	; vbl count-1
		bgt 	.nofadeto
		move.b 	fadeto_spd(a5),fadeto_tim(a5)   ; reset speed (vbl count)
		subq 	#1,fadeto_cnt(a5)	; faded -> totally?
		bne.s 	.okstillfade
		sf.b 	fadetoflag(a5)		; yes signal fade done.
		move 	#8,fadeto_cnt(a5)  	; and reset for next fade. 
.okstillfade	
		move.l 	fadeto_curptr(a5),a0 	; pal to fade FROM
		move.l 	fadeto_palptr(a5),a1	; ptr to pal to fade TO.
		move.l 	#$f00,d4		; R mask
		move.l 	#$0f0,d5		; G mask
		move.l 	#$00f,d6		; B mask
		move.w 	fadeto_nocols(a5),d7
		subq 	#1,d7
.col_lp		
		move.w 	(a0),d0			; curr value
		move.w 	(a1)+,d1
		move.w 	d0,d2
		move.w 	d1,d3
		and.w 	d4,d2
		and.w 	d4,d3
		cmp.w 	d3,d2
		beq.s 	.R_done
		blt.s 	.R_add
		sub.w 	#$100,d0
		bra.s 	.R_done
.R_add		
		add.w 	#$100,d0
.R_done		
		move.w 	d0,d2
		move.w 	d1,d3
		and.w 	d5,d2
		and.w 	d5,d3
		cmp.w 	d3,d2
		beq.s 	.G_done
		blt.s 	.G_add
		sub.w 	#$010,d0
		bra.s 	.G_done
.G_add	
		add.w 	#$010,d0
.G_done 	
		move.w 	d0,d2
		move.w 	d1,d3
		and.w 	d6,d2
		and.w 	d6,d3
		cmp.w 	d3,d2
		beq.s 	.B_done
		blt.s 	.B_add
		subq.w 	#$001,d0
		bra.s 	.B_done
.B_add	
		addq.w 	#$001,d0
.B_done	
		move.w 	d0,(a0)+
		dbf 	d7,.col_lp
.nofadeto	
		rts

fadeto_vars:	ds.b fadeto_varsize
		even

source_pal	dc.l	0,0,0,0,0,0,0,0
		dc.l	0,0,0,0,0,0,0,0		; does last 16 cols...
dest_pal	dc.l	0,0,0,0,0,0,0,0
		dc.l	0,0,0,0,0,0,0,0
orig_pal	dc.l	0,0,0,0,0,0,0,0
blank		dc.l	0,0,0,0,0,0,0,0
blank2		dc.w	$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff
		dc.w	$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff

	IFEQ	title_routine
ice_decrunch	movem.l	d0-a6,-(sp)
		bsr.s	getinfo
		cmpi.l	#'ICE!',d0
		bne	not_packed
		bsr.s	getinfo	
		lea.l	-8(a0,d0.l),a5
		bsr.s	getinfo
		move.l	d0,(sp)
		move.l	a1,a4
		move.l	a1,a6
		adda.l	d0,a6
		move.l	a6,a3
		move.b	-(a5),d7
		bsr	normal_bytes
		move.l	a3,a6
		bsr	get_1_bit
		bcc.s	not_packed
		move.w	#$0f9f,d7
		bsr	get_1_bit
		bcc.s	ice_00
		moveq	#15,d0	
		bsr	get_d0_bits
		move.w	d1,d7
ice_00:		moveq	#3,d6
ice_01:		move.w	-(a3),d4
		moveq	#3,d5
ice_02:		add.w	d4,d4
		addx.w	d0,d0
		add.w	d4,d4
		addx.w	d1,d1
		add.w	d4,d4
		addx.w	d2,d2
		add.w	d4,d4
		addx.w	d3,d3
		dbra	d5,ice_02
		dbra	d6,ice_01
		movem.w	d0-d3,(a3)
		dbra	d7,ice_00
not_packed:	movem.l	(sp)+,d0-a6
		rts
getinfo		moveq	#3,d1
getbytes	lsl.l	#8,d0
		move.b	(a0)+,d0
		dbf	d1,getbytes
		rts
normal_bytes	bsr.s	get_1_bit
		bcc.s	test_if_end
		moveq.l	#0,d1
		bsr.s	get_1_bit
		bcc.s	copy_direkt
		lea.l	direkt_tab+20(pc),a1
		moveq.l	#4,d3
nextgb		move.l	-(a1),d0
		bsr.s	get_d0_bits
		swap.w	d0
		cmp.w	d0,d1
		dbne	d3,nextgb
no_more		add.l	20(a1),d1
copy_direkt	move.b	-(a5),-(a6)
		dbf	d1,copy_direkt
test_if_end	cmpa.l	a4,a6
		bgt.s	strings
		rts	
get_1_bit	add.b	d7,d7
		bne.s	bitfound
		move.b	-(a5),d7
		addx.b	d7,d7
bitfound:	rts	
get_d0_bits	moveq.l	#0,d1
hole_bit_loop	add.b	d7,d7
		bne.s	on_d0
		move.b	-(a5),d7
		addx.b	d7,d7
on_d0:		addx.w	d1,d1
		dbf	d0,hole_bit_loop
		rts	
strings		lea.l	length_tab(pc),a1
		moveq.l	#3,d2
get_length_bit	bsr.s	get_1_bit
		dbcc	d2,get_length_bit
no_length_bit	moveq.l	#0,d4
		moveq.l	#0,d1
		move.b	1(a1,d2.w),d0
		ext.w	d0
		bmi.s	no_ber
get_ber	bsr.s	get_d0_bits
no_ber		move.b	6(a1,d2.w),d4
		add.w	d1,d4
		beq.s	get_offset_2
		lea.l	more_offset(pc),a1
		moveq.l	#1,d2
getoffs:	bsr.s	get_1_bit
		dbcc	d2,getoffs
		moveq.l	#0,d1
		move.b	1(a1,d2.w),d0
		ext.w	d0
		bsr.s	get_d0_bits
		add.w	d2,d2
		add.w	6(a1,d2.w),d1
		bpl.s	depack_bytes
		sub.w	d4,d1
		bra.s	depack_bytes
get_offset_2	moveq.l	#0,d1
		moveq.l	#5,d0
		moveq.l	#-1,d2
		bsr.s	get_1_bit
		bcc.s	less_40
		moveq.l	#8,d0
		moveq.l	#$3f,d2
less_40:	bsr.s	get_d0_bits
		add.w	d2,d1
depack_bytes:	lea.l	2(a6,d4.w),a1
		adda.w	d1,a1
		move.b	-(a1),-(a6)
dep_b:		move.b	-(a1),-(a6)
		dbf	d4,dep_b
		bra	normal_bytes
direkt_tab	dc.l	$7fff000e,$00ff0007,$00070002,$00030001,$00030001
		dc.l	270-1,	15-1,	 8-1,	 5-1,	 2-1
length_tab	dc.b	9,1,0,-1,-1
		dc.b	8,4,2,1,0
more_offset	dc.b	11,   4,   7,  0
		dc.w	$11f,  -1, $1f

	ENDC

set_lookups
		lea	plasmabin,a0		; source
		lea	lookup1,a1
		rept	184*5			; lines high*planes
		move.l	a0,(a1)+
		add.l	#40,a0			; next line
		endr
		rts

plasmascroll:		
.wait_vbl	tst.w	vblank
		beq.s	.wait_vbl
		clr.w	vblank

		cmp.w	#200,palette_counter
		bge.s	.exitcount
		not.b	toggle
		tst.b	toggle
		beq.s	.skipincrement
		addq.w	#1,palette_counter
		bra.s	.skipincrement
.exitcount	subq.w	#1,exit_plas
		bne.s	.skipincrement
		move.w	#-1,exit_plasma
.skipincrement	
        	move.l	show_copper,a0	; swap screens.
		move.l	work_copper,show_copper
		move.l	a0,work_copper
		move.l	show_copper,cop1lch(a5)
		bsr.w	plasma          ; do it!!!
		btst	#6,$bfe001		; lmb ?
		beq.s	.exit
		tst.w	exit_plasma
		beq.s	plasmascroll
.exit		rts

exit_plas	dc.w	200
exit_plasma	dc.w	0
toggle		dc.b	0
		even
palette_counter	dc.w	0
plasma
		lea	$dff180,a0
		move.w	angle+2,d5
		move.w	angle,d1
		movem.l	a5-a6,-(a7)
		jsr	plasmawave
		movem.l	(a7)+,a5-a6
		add.w	#9,angle
    		add.w	#17,angle+2
		rts

plasmawave
		move.w	#120-1,d7	;270-1,d7	; lines
		lea	sine,a0
		lea	cosine,a1
		lea	lookup1,a2
		lea	lookup2,a3
		lea	lookup3,a4
		lea	lookup4,a5
		lea	work_copper,a6
		move.l	0(a6),a6		
		add.l	#28,a6		; skip screen stuff
		move.w	#$31,d2		; first line...
		lsl.w	#8,d2		; shift to high byte
		move.b	#$07,d2		; wait state

; start copper list
		move.w	#bplcon0,(a6)+
		move.w	#0,(a6)+	; turn off bitplanes
		move.w	d2,(a6)+
		move.w	#$fffe,(a6)+	; wait for end of first line
		move.w	#bplcon0,(a6)+
		move.w	#5<<12+color,(a6)+	; turn on bitplanes
		add.w	#$100,d2	; first image line...
		move.w	#$fffe,d3
.bigloop			
		move.w	d5,d4
		and.w	#1023,d4
		lsl.w	#1,d4
		move.w	0(a0,d4.w),d0
		add.w	#280,d0		; 260
		lsr.w	#2,d0
		sub.w	#11,d5		;9
		add.w	#2,d1		;4
		move.w	d1,d4
		and.w	#1023,d4
		lsl.w	#1,d4
		add.w	0(a1,d4.w),d0
		add.w	#260,d0
		lsr.w	#3,d0
		add.w	#5,d0           ; this bit is the actual sine-wave
                                        ; calculation bit... It should be
                                        ; pre-done into a lookup table
                                        ; I know, but I was a bit slack..

.okhigh
		lsl.w	#2,d0	; offset into jump tables
; create copper list
		swap	d2
		move.w	d3,d2
		move.l	d2,(a6)+
		swap	d2

		move.w	#bplcon0,(a6)+
		move.w	#5<<12+color,(a6)+	; turn on bitplanes

		move.w	#bpl1pth,(a6)+
		move.w	0(a2,d0.w),(a6)+
		move.w	#bpl1ptl,(a6)+
		move.w	2(a2,d0.w),(a6)+	; first plane

		move.w	#bpl2pth,(a6)+
		move.w	0(a3,d0.w),(a6)+
		move.w	#bpl2ptl,(a6)+
		move.w	2(a3,d0.w),(a6)+	; second plane

		move.w	#bpl3pth,(a6)+
		move.w	0(a4,d0.w),(a6)+
		move.w	#bpl3ptl,(a6)+
		move.w	2(a4,d0.w),(a6)+	; third plane

		move.w	#bpl4pth,(a6)+
		move.w	0(a5,d0.w),(a6)+
		move.w	#bpl4ptl,(a6)+
		move.w	2(a5,d0.w),(a6)+	; fourth plane

		add.w	#$100,d2

		move.w	d2,(a6)+
		move.w	#$fffe,(a6)+	; wait for end of line

		move.w	#bpl1pth,(a6)+
		move.w	0(a2,d0.w),(a6)+
		move.w	#bpl1ptl,(a6)+
		move.w	2(a2,d0.w),(a6)+	; first plane

		move.w	#bpl2pth,(a6)+
		move.w	0(a3,d0.w),(a6)+
		move.w	#bpl2ptl,(a6)+
		move.w	2(a3,d0.w),(a6)+	; second plane

		move.w	#bpl3pth,(a6)+
		move.w	0(a4,d0.w),(a6)+
		move.w	#bpl3ptl,(a6)+
		move.w	2(a4,d0.w),(a6)+	; third plane

		move.w	#bpl4pth,(a6)+
		move.w	0(a5,d0.w),(a6)+
		move.w	#bpl4ptl,(a6)+
		move.w	2(a5,d0.w),(a6)+	; fourth plane

		move.l	a5,-(a7)
		lea	lookup5,a5
		move.w	#bpl5pth,(a6)+
		move.w	0(a5,d0.w),(a6)+
		move.w	#bpl5ptl,(a6)+
		move.w	2(a5,d0.w),(a6)+        ; fifth plane - it's 32 cols

; move colours in here...
		move.l	palette_p,a5
		move.w	#color02,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color03,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color04,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color05,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color06,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color07,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color08,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color09,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color10,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color11,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color12,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color13,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color14,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color15,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color16,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color17,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color18,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color19,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color20,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color21,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color22,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color23,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color24,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color25,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color26,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color27,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color28,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color29,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color30,(a6)+
		move.w	(a5)+,(a6)+
		move.w	#color31,(a6)+
		move.w	(a5)+,(a6)+
		lea	4(a5),a5	; skip last 2 colours
		move.l	a5,palette_p
;OK, done one entire horizontal line of colour now...
; now double it :-)

		move.l	(a7)+,a5
		add.w	#$100,d2
		cmp.w	#$07,d2			; high byte clear?
		bne.s	.notyet
		move.l	#$ff09fffe,(a6)+
		move.l	#$ffddfffe,(a6)+	; end of copper list 1
.notyet		
		dbf	d7,.bigloop             ; this loop should really
                                                ; be rolled out for more
		move.w	#bplcon0,(a6)+
		move.w	#0<<12+color,(a6)+	; turn on bitplanes
                                                ; speed, but I'm slack :-)

		move.l	#$fffffffe,(a6)+

		lea	a_palette,a5
		move.w	palette_counter,d0
		lsl.w	#6,d0
		add.l	d0,a5			; next line in picture
		move.l	a5,palette_p		; reset picture

		rts

make_picture
		lea	build_palette,a0	; palette list
		lea	a_palette,a1		; colour to here
		lea	base_picture,a2		; chunky bitmap
		move.w	#(320*32)-1,d0
loopit
		move.b	(a2)+,d1
		lsl.w	#1,d1
		move.w	0(a0,d1.w),(a1)+	; this colour!
		dbf	d0,loopit
		rts
build_palette
	DC.W	$0000,$0000,$0011,$0113,$0124,$0125,$0225,$0236
	DC.W	$0337,$0347,$0348,$0448,$0458,$0559,$0569,$056A
	DC.W	$066A,$067A,$067B,$077B,$078B,$088B,$088C,$089C
	DC.W	$099C,$09AD,$0AAD,$0BBE,$0BCE,$0CDE,$0EEF,$0FFF


	IFEQ	music_routine
use	=	$202952d
playback_speed	= 3

		include	music/types.i
		include	music/player6.i
		incdir 	hd1:cdn/blackmoon/music/
		include	player.s
		incdir	hd1:cdn/blackmoon/

music_irq
		move.w	#-1,vblank
		movem.l	d0-d7/a0-a6,-(a7)
		lea	custom,a5
		btst	#5,Intreqr+1(a5)
		beq.b	.out
		play_music
		move.w	#$0020,Intreq(a5)
.out		movem.l	(a7)+,d0-d7/a0-a6
		rte


;		move.w	#-1,vblank
;		movem.l	d0-d7/a0-a6,-(a7)
;		lea	custom,a5
;		btst	#5,Intreqr+1(a5)
;		beq.b	.out
;		play_music
;		move.w	#$0020,Intreq(a5)
;.out		movem.l	(a7)+,d0-d7/a0-a6
;		rte
	ENDC	; music_routine

*******************************************************************************
************************ fast data.

data_segment
base

bitplaneLUT	equ	*-base
x		set	0
	IFNE	screen_depth
		rept	screen_depth
		dc.l	x
x		set	x+screen_bitplaneoffset
		endr
	ENDC

y_coord_LUT	equ	*-base
x		set	0
		rept	screen_height
		dc.w	x
x		set	x+screen_nextlineoffset
		endr

show_screen	equ	*-base
	dc.l	screen1
work_screen	equ	*-base
	dc.l	screen2
	IFEQ	dt_routine
idle_screen	equ	*-base
	dc.l	screen3
z_add		equ	*-base
	dc.w	0

x_sinus_pos	equ	*-base
	dc.w	0

y_sinus_pos	equ	*-base
	dc.w	0
	ENDC
	
screen_addr     equ     0
show_struct	dc.l    screen1
work_struct	dc.l    screen2

************************ program data.

	IFEQ	title_routine
fdelay		dc.w	50
numcols		dc.w	16
	ENDC


	IFEQ	dt_routine
centre_list		; coordinates for the centre of each circle.
x		set	0
		rept	num_circles+1
		dc.w	x,0,0
x		set	x+(num_circles/num_circleson)
		endr

x_sinus		include	includes/dt_xsinus.asm
y_sinus		include	includes/dt_ysinus.asm
circle_data	dcb.w	num_circles*num_dots*2,0
stack		dc.l	0	; stack save when a7 needed.

letter_delay	dc.w	letdelay


digi_state	dc.w	0

textp		dc.l	text
resettext


text2		; 15 wide
		dc.b	"CYDONIA        "
		dc.b	"            ARE"
text		
		dc.b	"ACCOLYTE       "
		dc.b	"           CODE"
		dc.b	"BONZA          "
		dc.b	"          SYSOP"
		dc.b	"COOLCAT        "
		dc.b	"            GFX"
		dc.b	"CRO            "
		dc.b	"           CODE"
		dc.b	"EMINENCE       "
		dc.b	"          MUSIC"
		dc.b	"GUSTO          "
		dc.b	"          MUSIC"
		dc.b	"INFINITY       "
		dc.b	"            GFX"
		dc.b	"MUSE           "
		dc.b	"          MUSIC"
		dc.b	"PROWLER        "
		dc.b	"          SYSOP"
		dc.b	"SERKUL         "
		dc.b	"          MUSIC"
		dc.b	"SNYKERS        "
		dc.b	"            GFX"
		dc.b	"SOURI          "
		dc.b	"            GFX"
		dc.b	"STORM          "
		dc.b	"           CODE"
		dc.b	"TACHYON        "
		dc.b	"           CODE"
		dc.b	"               "
		dc.b	"               "
		dc.b	" WATCH OUT FOR "
		dc.b	"  OUR DISKMAG  "
		dc.b	"DEFY  MORE THAN"
		dc.b	"    A VISION   "
		dc.b	"               "
		dc.b	"               "
		dc.b	"NEXT ROUTINE   "
		dc.b	"         PLEASE"
		dc.b	"               "
		dc.b	"               "
		dc.b	"               "
		dc.b	"               "
		dc.b	"               "
		dc.b	"               "
		dc.b	0		
		even
onscreen1	dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
onscreen2	dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
compare1	dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
compare2	dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		dc.l	0,0,0
		even

		
digifont	incbin	fonts/digifont.raw
	ENDC

state		dc.w	0
	
sinetable32767	include	includes/sinetable32767.asm

Angle		dcb.w	0,10

lookup1		rept	184     ; the bitmap is 184 lines high
		dc.l	0
		endr
lookup2		rept	184
		dc.l	0
		endr
lookup3		rept	184
		dc.l	0
		endr
lookup4		rept	184
		dc.l	0
		endr
lookup5		rept	184     
		dc.l	0
		endr
show_copper	dc.l	plasmacopper1
work_copper	dc.l	plasmacopper2

		Include includes/sincos1k.asm

*******************************************************************************
************************ custom registers that don't require refreshing.

xstrt		equ	$071		; normal = $081
ystrt		equ	$022		; normal = $02c
xstop		equ	$1d1		; normal = $1c1
ystop		equ	$136		; normal = $12c

cust_registers:	
		dc.w	diwstrt,ystrt<<8+xstrt
		dc.w	diwstop,(ystop-$100)<<8+(xstop-$100)

		dc.w	bplcon0,screen_depth<<12+color
		dc.w	ddfstrt,(xstrt-17)/2
		dc.w	ddfstop,(xstrt-17)/2+8*((xstop-xstrt)/16-1)
		dc.w	bpl1mod,0
		dc.w	bpl2mod,0

		dc.w	color00,$0000,color01,$0000,color02,$0000,color03,$0000
		dc.w	color04,$0000,color05,$0000,color06,$0000,color07,$0000
		dc.w	color08,$0000,color09,$0000,color10,$0000,color11,$0000
		dc.w	color12,$0000,color13,$0000,color14,$0000,color15,$0000
		dc.w	color16,$0000,color17,$0000,color18,$0000,color19,$0000
		dc.w	color20,$0000,color21,$0000,color22,$0000,color23,$0000
		dc.w	color24,$0000,color25,$0000,color26,$0000,color27,$0000
		dc.w	color28,$0000,color29,$0000,color30,$0000,color31,$0000
cust_registers_end:

palette_p	dc.l	a_palette

a_palette
		rept	320	; pic is 320 lines high
		dc.l	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		endr
base_picture
		incdir	hd1:cdn/blackmoon/
		incbin	gfx/lightning3.raw
		incdir	hd1:cdn/blackmoon/

pic1		incbin	gfx/cdn7.ice
		even
pic2		incbin	gfx/pres2c.ice
		even
pic3		incbin	gfx/BlackMoon_Title.ice
		even

*******************************************************************************
************************ chip data.

		section	chipdata,data_c

plasmabin	incbin	gfx/wibblescroll.raw

copper:		dc.w	-1,-2


dt_copper	
		dc.w	color00,$0000,color01,$0ddd,color02,$0242,color03,$0242
		dc.w	color04,$0464,color05,$0464,color06,$0686,color07,$0686
		dc.w	color08,$08a8,color09,$08a8,color10,$0aca,color11,$0aca
		dc.w	color12,$0eee,color13,$0eee,color14,$006e,color15,$006e

		dc.w	bplcon0,1<<12+color
		dc.w 	$ff09,$fffe,$ffdd,$fffe	
;		dc.w	$ff09,$fffe,$fffe,$ffdd
		dc.w	$0409,$fffe,bplcon0,4<<12+color

digi_cop	dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0,bpl4pth,0,bpl4ptl,0
		dc.w	-1,-2


	IFEQ	title_routine
t_copper1		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	-1,-2

t_copper2		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	-1,-2

t_copper3		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	bpl5pth,0,bpl5ptl,0
		dc.w	-1,-2

		cnop	0,4
plasmacopper1
		dc.w	diwstrt,$22<<8+$81
		dc.w	diwstop,($136-$100)<<8+($1c1-$100)
		dc.w	bplcon0,5<<12+color
		dc.w	ddfstrt,$38
		dc.w	ddfstop,$b8
		dc.w	bpl1mod,0
		dc.w	bpl2mod,0
		dc.w	fmode,$3
pc1
	rept	240	;screen_height+20	; 50 lines high for test
                                ; this number is the same as used
                                ; for the big loop in the copper-create
                                ; section. It's the number of lines
                                ; displayed.
; this is the bitplane pointer section
		dc.l	0,0
		dc.l	0,0
		dc.l	0,0
		dc.l	0,0
		dc.l	0,0
		dc.l	0,0
; this is colour section
		dc.l	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		dc.l	0,0,0,0,0,0,0,0,0,0,0,0,0,0	; 30 cols
	endr
		dc.w	-1,-2

		cnop	0,4
plasmacopper2
		dc.w	diwstrt,$22<<8+$81
		dc.w	diwstop,($136-$100)<<8+($1c1-$100)
		dc.w	bplcon0,5<<12+color
		dc.w	ddfstrt,$38
		dc.w	ddfstop,$b8
		dc.w	bpl1mod,0
		dc.w	bpl2mod,0
		dc.w	fmode,$3
pc2
	rept	240	;screen_height+20	; 50 lines high for test
		dc.l	0,0
		dc.l	0,0
		dc.l	0,0
		dc.l	0,0
		dc.l	0,0
		dc.l	0,0
		dc.l	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		dc.l	0,0,0,0,0,0,0,0,0,0,0,0,0,0	; 30 cols
	endr
		dc.w	-1,-2

	ENDC

a_copper2	
a_c2		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	color00,$070,color01,$fff,color02,$fff,color03,$fff
		dc.w	color04,$fff,color05,$fff,color06,$fff,color07,$fff
		dc.w	color08,$fff,color09,$fff,color10,$fff,color11,$fff
		dc.w	color12,$fff,color13,$fff,color14,$fff,color15,$fff
		dc.w	-1,-2

	IFEQ	music_routine
mod		incbin	music/p60.blackmoon
	ENDC


************************

		section bss,bss_c

screen1		ds.b	1*(44*2)*(276*2)	; FUDGED!
screen2		ds.b	1*(44*2)*(276*2)
screen3		ds.b	1*(44*2)*(276*2)
		ds.b	(44*2)*(276*2)
	IFEQ	dt_routine
nscreen1	ds.b	(44*2)*(276*2)
nscreen2	ds.b	(44*2)*(276*2)
nscreen3	ds.b	(44*2)*(276*2)
	ENDC


*******************************************************************************

		end

