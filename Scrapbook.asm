; ScrapBook Code	
		opt	o+,w-

yes		equ	0
no		equ	1
scroll_speed	equ	2

music_on	set	yes		; include music files?

		;incdir	hd1:
		;incdir	work:cdn/scrapbook/
	include	music/maccros.i

		;incdir	work:cdn/scrapbook/
	include	includes/custom2.i
		;incdir	work:cdn/scrapbook/

fade_workbench	equ	yes
screen_res	equ	1		; 0 = lowres, 1 = hires.
screen_width    equ     80
screen_height   equ     512
screen_depth    equ     4
screen_bitplane equ     screen_width*screen_height
screen_size     equ     screen_depth*screen_bitplane

screen_bitplaneoffset	equ	screen_bitplane
screen_nextlneoffset	equ	screen_width

xmin            equ     0
xmax            equ     (screen_width*8)-1
ymin            equ     0
ymax            equ     screen_height-1

xstrt		equ	$081
ystrt		equ	$01c
xstop		equ	$1c1
ystop		equ	$11c	; 256 lines high - using LACE!

f_w		equ	80
f_h		equ	8
f_d		equ	1

*******************************************************************************
first		jmp	startup
		dc.b	' - ScrapBook - ©1995 SeCT - '
		even 
startup:	bsr	take_system
		tst.l	d0
		beq.s	.error
		bsr	initiations

		bsr	intro_bit
; for testing
;		bsr	set_menu
;		bsr	fade_it
;		move.w	#0,this_one

		bsr	main

	
		stop_music	
		
		bsr	free_system
		moveq	#0,d0
.error		rts

		include	includes/takefree_system.i


*******************************************************************************

initiations:	lea	custom,a5
		lea	data_segment,a6
		movem.l	blank,d0-d7
		movem.l	d0-d7,color00(a5)
		movem.l	d0-d7,color16(a5)

.wait		move.l  vposr(a5),d0		; wait for vertical blank.
		and.l   #$1ff00,d0
		bne.s	.wait

		move.l	#copper,cop1lch(a5)	; for intro
		move.w  #set+dmaen+blten+bplen+copen,dmacon(a5)

		move.w	#$7fff,intena(a5)
		move.l	#music_int,$6c
		move.w	#set+inten+vertb,intena(a5)
; set copper

		lea	mod1,a0		; this is the first mod!
		init_music
	

;set_screen
		move.l  #copper,cop1lch(a5)
		move.w	#ystrt<<8+xstrt,diwstrt(a5)
		move.w	#(ystop-$100)<<8+(xstop-$100),diwstop(a5)
;		move.w	#screen_depth<<12+color+hires+LACE,bplcon0(a5)
		move.w	#0,bplcon1(a5)
		move.w	#0,bplcon2(a5)
		move.w	#$3c,ddfstrt(a5)
		move.w	#$d4,ddfstop(a5)

		move.w	#80,bpl1mod(a5)
		move.w	#80,bpl2mod(a5)

		moveq	#0,d0
		move.w	joy0dat(a5),d0
		and.w	#$ff00,d0
		lsr.w	#8,d0		; vert move only
		move.w	d0,mouse_y	; first one
		rts

set_logo
		lea	sect_logo,a0
		move.l	#80*512,d6
		bsr	set_screen16
		move.w	#hires+4<<12+color,bplcon0(a5)
		move.w	#80,bpl1mod(a5)
		move.w	#80,bpl2mod(a5)
		bsr	set_lace
		rts

set_title	lea	sb_title,a0
		move.l	#80*512,d6
		bsr	set_screen16
		move.w	#4<<12+color+hires,bplcon0(a5)
		bsr	set_lace
		rts

set_menu2
.vbl1		tst.w	vblank
		beq.s	.vbl1			; wait for VBL interrupt
		clr.w	vblank
.vbl2		tst.w	vblank
		beq.s	.vbl2			; wait for VBL interrupt
		clr.w	vblank

		bra.s	set_menu3
set_menu
		lea	sb_menu,a0
		move.l	#80*512,d6

		lea	menu_pal,a1
		movem.l	d0-d7/a0-a6,-(a7)
		jsr	ice_decrunch
		movem.l	(a7)+,d0-d7/a0-a6

set_menu3
		movem.l	blank,d0-d7
		movem.l	d0-d7,color00(a5)
		movem.l	d0-d7,source_pal
		movem.l	menu_pal,d0-d7
		movem.l	d0-d7,dest_pal


; set fade details
		move.l	a5,-(a7)
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#32,d0	; colours
		move.w	#3,d1	; speed
		bsr	trigfadeto
		move.l	(a7)+,a5

		move.w	#ystrt<<8+xstrt,diwstrt(a5)
		move.w	#(ystop-$100)<<8+(xstop-$100),diwstop(a5)
		move.w	#4<<12+color+hires+LACE,bplcon0(a5)
		move.w	#0,bplcon1(a5)
		move.w	#0,bplcon2(a5)
		move.w	#$3c,ddfstrt(a5)
		move.w	#$d0,ddfstop(a5)

		move.w	#80,bpl1mod(a5)
		move.w	#80,bpl2mod(a5)



;		move.w	#4<<12+color+hires,bplcon0(a5)
;		move.w	#80,bpl1mod(a5)
;		move.w	#80,bpl2mod(a5)
		
		lea	menupl1,a0
		lea	menupl2,a1
		lea	menu_scr,a3
		move.l	a3,d0
		rept	4
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		swap	d0		; field 1
		add.l	#80,d0
		move.w	d0,6(a1)
		swap	d0
		move.w	d0,2(a1)
		swap	d0
		sub.l	#80,d0		; field 2
		add.l	#80*512,d0
		add.l	#8,a0
		add.l	#8,a1
		endr
		lea	menucopper1,a3
		move.l	a3,d0
		lea	menucopper2,a3
		move.l	a3,d1
		lea	menucop1,a0
		lea	menucop2,a1
		move.w	d0,6(a1)	; set copper1 to copper2
		swap	d0
		move.w	d0,2(a1)
		swap	d0
		move.w	d1,6(a0)
		swap	d1
		move.w	d1,2(a0)
		swap	d1

		lea	menucopper1,a3
		move.l	a3,d0
		lea	menucopper2,a3
		move.l	a3,d1
		lea	lacecop1,a0
		lea	lacecop2,a1
		move.w	d0,6(a1)	; set copper1 to copper2
		swap	d0
		move.w	d0,2(a1)
		swap	d0
		move.w	d1,6(a0)
		swap	d1
		move.w	d1,2(a0)
		swap	d1

		lea	menu_scr,a0
		add.l	#504*80,a0		; this far down, base
		lea	sc1,a1
		lea	sc2,a2
		move.l	a0,d0
		move.w	d0,6(a1)
		move.w	d0,6(a2)
		swap	d0
		move.w	d0,2(a1)
		move.w	d0,2(a2)
		swap	d0

		move.l	#menucopper1,cop1lch(a5)
		addq.w	#1,this_one

		rts

set_lace
		lea	lacepl1,a0
		lea	lacepl2,a1
		lea	screen,a3
		move.l	a3,d0
		lea	80(a3),a3
		move.l	a3,d1
		rept	4
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		swap	d0		; field 1
;		add.l	#80,d0

		move.w	d1,6(a1)
		swap	d1
		move.w	d1,2(a1)
		swap	d1

;		sub.l	#80,d0		; field 2
		add.l	#80*512,d0
		add.l	#80*512,d1
		add.l	#8,a0
		add.l	#8,a1

		endr

		lea	lacecopper1,a3
		move.l	a3,d0
		lea	lacecopper2,a3
		move.l	a3,d1
		lea	lacecop1,a0
		lea	lacecop2,a1
		move.w	d0,6(a1)	; set copper1 to copper2
		swap	d0
		move.w	d0,2(a1)
		swap	d0
		move.w	d1,6(a0)
		swap	d1
		move.w	d1,2(a0)
		swap	d1
		move.l	#lacecopper1,cop1lch(a5)
		rts



set_screen16
; call with d6=bitplanesize
; a0=source_pic
		lea	piccy16,a1
		movem.l	d0-d7/a0-a6,-(a7)
		jsr	ice_decrunch
		movem.l	(a7)+,d0-d7/a0-a6

		lea	screen,a1
		lea	c4,a0
		move.l	a1,d0
		rept	4		; 4 bitplanes
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		swap	d0
		add.l	d6,d0	; next bitplane
		add.l	#8,a0
		endr
		movem.l	blank,d0-d7
		movem.l	d0-d7,color00(a5)
		movem.l	d0-d7,source_pal
		movem.l	piccy16,d0-d7
		movem.l	d0-d7,dest_pal
		lea	menucop1,a0
		lea	menucop2,a1
		lea	copper4,a2
		move.l	a2,d0
		move.w	d0,6(a0)
		move.w	d0,6(a1)
		swap	d0
		move.w	d0,2(a0)
		move.w	d0,2(a1)
		swap	d0
;		move.l	#copper4,cop1lch(a5)

; set fade details
		move.l	a5,-(a7)
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#16,d0	; colours
		move.w	#3,d1	; speed
		bsr	trigfadeto
		move.l	(a7)+,a5
		rts

set_screen32
; call with d6=bitplanesize
; a0=source_pic
		lea	piccy32,a1
		jsr	ice_decrunch

		lea	screen,a1
		lea	c5,a0
		move.l	a1,d0
		rept	5		; 5 bitplanes
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		swap	d0
		add.l	d6,d0	; next bitplane
		add.l	#8,a0
		endr
		movem.l	blank,d0-d7
		movem.l	d0-d7,color00(a5)
		movem.l	d0-d7,color16(a5)
		movem.l	d0-d7,source_pal
		movem.l	piccy32,d0-d7
		movem.l	d0-d7,dest_pal
		movem.l	piccy32+32,d0-d7
		movem.l	d0-d7,dest_pal+32

		lea	menucop1,a0
		lea	menucop2,a1
		lea	copper5,a2
		move.l	a2,d0
		move.w	d0,6(a0)
		move.w	d0,6(a1)
		swap	d0
		move.w	d0,2(a0)
		move.w	d0,2(a1)
		swap	d0

;		move.l	#copper5,cop1lch(a5)
		move.l	a5,-(a7)
; set fade details
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#32,d0	; colours
		move.w	#3,d1	; speed
		bsr	trigfadeto
		move.l	(a7)+,a5
		rts

set_screen64ehb
; call with d6=bitplanesize
; a0=source_pic
		lea	piccy32,a1
		jsr	ice_decrunch

		lea	screen,a1
		lea	ehbc5,a0
		move.l	a1,d0
		rept	6		; 6 bitplanes
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		swap	d0
		add.l	d6,d0	; next bitplane
		add.l	#8,a0
		endr
		movem.l	blank,d0-d7
		movem.l	d0-d7,color00(a5)
		movem.l	d0-d7,color16(a5)
		movem.l	d0-d7,source_pal
		movem.l	piccy32,d0-d7
		movem.l	d0-d7,dest_pal
		movem.l	piccy32+32,d0-d7
		movem.l	d0-d7,dest_pal+32

		lea	menucop1,a0
		lea	menucop2,a1
		lea	ehbcopper5,a2
		move.l	a2,d0
		move.w	d0,6(a0)
		move.w	d0,6(a1)
		swap	d0
		move.w	d0,2(a0)
		move.w	d0,2(a1)
		swap	d0

		move.l	#ehbcopper5,cop1lch(a5)
		move.l	a5,-(a7)
; set fade details
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#32,d0	; colours
		move.w	#3,d1	; speed
		bsr	trigfadeto
		move.l	(a7)+,a5
		rts

fade_it_down
		lea	dest_pal,a0
		movem.l	(a0),d0-d7
		movem.l	d0-d7,source_pal
		movem.l	32(a0),d0-d7
		movem.l	d0-d7,source_pal+32
		movem.l	blank,d0-d7
		movem.l	d0-d7,dest_pal
		movem.l	d0-d7,dest_pal+32
		move.l	a5,-(a7)
; set fade details
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#32,d0	; colours
		move.w	#3,d1	; speed
		bsr	trigfadeto
		move.l	(a7)+,a5

fade_it
		tst.w	vblank
		beq.s	fade_it		; relies on VertB
		clr.w	vblank
		move.l	a5,-(a7)
		bsr	fade_to
		move.b	fadetoflag(a5),d0
		move.l	(a7)+,a5
		move.l	d0,-(a7)
		movem.l	source_pal,d0-d7
		movem.l	d0-d7,color00(a5)
		movem.l	source_pal+32,d0-d7
		movem.l	d0-d7,color16(a5)
		move.l	(a7)+,d0
		tst.b	d0
		bne.s	fade_it
		add.w	#1,this_one
		rts

fade_it_down2
		lea	dest_pal,a0
		movem.l	(a0),d0-d7
		movem.l	d0-d7,source_pal
		movem.l	32(a0),d0-d7
		movem.l	d0-d7,source_pal+32
		movem.l	blank,d0-d7
		movem.l	d0-d7,dest_pal
		movem.l	d0-d7,dest_pal+32
		move.l	a5,-(a7)
; set fade details
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#32,d0	; colours
		move.w	#3,d1	; speed
		bsr	trigfadeto
		move.l	(a7)+,a5
fade_it2
		tst.w	vblank
		beq.w	fade_it2		; relies on VertB
		clr.w	vblank
		bsr	decrease_cols
		move.l	a5,-(a7)
		bsr	fade_to
		move.b	fadetoflag(a5),d0
		move.l	(a7)+,a5
		move.l	d0,-(a7)
		movem.l	source_pal,d0-d7
		movem.l	d0-d7,color00(a5)
		movem.l	source_pal+32,d0-d7
		movem.l	d0-d7,color16(a5)
		move.l	(a7)+,d0
		tst.b	d0
		bne.w	fade_it2
		add.w	#1,this_one
		rts

pause_it
		tst.w	vblank
		beq.s	pause_it		; relies on VertB
		clr.w	vblank

		btst	#2,$dff017
		bne.s	.noright
		move.w	#1,delay_it

.noright	subq.w	#1,delay_it
		bne.s	pause_it
		move.w	#300,delay_it
		addq.w	#1,this_one
		rts

delay_it	dc.w	300
		
intro_bit
		bsr	set_logo
		bsr	fade_it
		bsr	pause_it
		bsr	fade_it_down
		bsr	set_title
		bsr	fade_it
		bsr	pause_it
		bsr	fade_it_down
		bsr	set_menu
		bsr	fade_it
		move.w	#0,this_one
		rts
		
*******************************************************************************
* Main
*
* The main control routines for the mag
****************************************************************************
main:
		tst.w	vblank
		beq.s	main			; wait for VBL interrupt
		clr.w	vblank

		move.w	this_one,d0
		lsl.w	#2,d0
		lea	routines,a0
		move.l	0(a0,d0.w),a0
		jsr	(a0)

;		btst	#10,$dff016
;		beq.s	.bye

		btst	#6,$bfe001		; lmb ?
		bne.w	main

		move.w	menu_item,d0
		bmi.s	main		; -1, no menu item
		
		cmp.w	#22,d0
		beq.s	.bye		; pressed exit...
		
		cmp.w	#12,d0
		bge.s	music_maybe
; got pic...
		addq.w	#1,this_one
		bra.s	main

.bye		rts

music_maybe
		cmp.w	#17,d0
		bge.s	.scrolltext
		sub.w	#12,d0		; get music number
	
		stop_music
	
		lea	mod_l,a0
		lsl.w	#2,d0
		move.l	0(a0,d0.w),a0
	
		init_music
	
		move.w	#-1,menu_item
		bra.w	main

.scrolltext	
		sub.w	#17,d0		; this scroll

		cmp.w	last_menu,d0
		beq.s	.no_change

		move.w	d0,last_menu

		lea	texts,a0
		lsl.w	#2,d0
		move.l	0(a0,d0.w),a0	; this scroll!
		move.l	a0,textp
		move.l	a0,lasttext
.no_change
		move.w	#-1,menu_item
		bra.w	main
				


exit_flag	dc.w	0
		even
vblank		dc.w	0

routines	dc.l	wait_menu
		dc.l	fade_it_down2
		dc.l	set_picture
		dc.l	fade_it
		dc.l	pause_it
		dc.l	fade_it_down
		dc.l	set_menu2
		dc.l	fade_it
		dc.l	resetit

		dc.l	0



this_one	dc.w	0		; show menu is first!

resetit		move.w	#0,this_one
		rts
set_picture

		move.w	menu_item,d0
		lsl.w	#1,d0
		lea	size_look,a0
		move.w	0(a0,d0.w),d0
		tst.w	d0
		beq.s	.hires
		sub.w	#1,d0
		beq.s	.loresehb
		sub.w	#1,d0
		beq.w	.lores32
		addq.w	#1,this_one
		rts
.hires
		move.w	menu_item,d0
		lsl.w	#2,d0
		lea	pic_look,a0
		move.l	0(a0,d0.w),a0
		move.w	#80*256,d6
		bsr	set_screen16

.vbl1		tst.w	vblank
		beq.s	.vbl1			; wait for VBL interrupt
		clr.w	vblank
.vbl2		tst.w	vblank
		beq.s	.vbl2			; wait for VBL interrupt
		clr.w	vblank

		addq.w	#1,this_one
		move.w	#hires+4<<12+color,bplcon0(a5)
		move.w	#0,bpl1mod(a5)
		move.w	#0,bpl2mod(a5)
		rts
.loresehb	
		move.w	menu_item,d0
		lsl.w	#2,d0
		lea	pic_look,a0
		move.l	0(a0,d0.w),a0
		move.w	#40*256,d6
		bsr	set_screen64ehb
.vbl3		tst.w	vblank
		beq.s	.vbl3			; wait for VBL interrupt
		clr.w	vblank
.vbl4		tst.w	vblank
		beq.s	.vbl4			; wait for VBL interrupt
		clr.w	vblank

;		move.w	#6<<12+color+homod,bplcon0(a5)
		move.w	#0,bpl1mod(a5)
		move.w	#0,bpl2mod(a5)
		addq.w	#1,this_one
		rts
.lores32	
		move.w	menu_item,d0
		lsl.w	#2,d0
		lea	pic_look,a0
		move.l	0(a0,d0.w),a0
		move.w	#40*256,d6
		bsr	set_screen32
.vbl5		tst.w	vblank
		beq.s	.vbl5			; wait for VBL interrupt
		clr.w	vblank
.vbl6		tst.w	vblank
		beq.s	.vbl6			; wait for VBL interrupt
		clr.w	vblank
		move.w	#5<<12+color,bplcon0(a5)
		move.w	#0,bpl1mod(a5)
		move.w	#0,bpl2mod(a5)

		addq.w	#1,this_one
		rts


mouse_y		dc.w	0
mouse_yo	dc.w	0

update_mouse
		moveq	#0,d0
		move.w	joy0dat(a5),d0
		and.w	#$ff00,d0
		lsr.w	#8,d0		; vert move only
		move.w	mouse_yo,d2
		move.w	d0,mouse_yo
		sub.w	d2,d0
		move.w	mouse_y,d2
		ext.w	d0
		add.w	d0,d2		; new Y
		
		cmp.w	#40/2,d2
		bge.s	.ok1
		move.w	#40/2,d2
.ok1		
		cmp.w	#465/2,d2
		ble.s	.ok2
		move.w	#465/2,d2
.ok2		move.w	d2,mouse_y
		rts

; actual routines here
wait_menu
		bsr	update_mouse
		bsr	update_menu_cop
		bsr	put_col
		bsr	text_writer
		rts

update_menu_cop

		bsr	decrease_cols
		move.w	#-1,menu_item
;		btst	#6,$bfe001
;		bne.s	.ok_nomouse
;		rts
;.ok_nomouse
		move.w	mouse_y,d0
; coarse
		cmp.w	#456/2,d0
		bge.w	exit_bit

		cmp.w	#373/2,d0
		bge.w	scroll_bits

		cmp.w	#273/2,d0
		bge.w	music_bits

		cmp.w	#156/2,d0
		bge.s	rob_bits

		cmp.w	#40/2,d0
		bge.s	zex_bits
		move.w	#-1,menu_item
		rts
zex_bits
		cmp.w	#55/2,d0
		bge.s	.2
		move.w	#0,menu_item	; first picture
		rts
.2		cmp.w	#70/2,d0
		bge.s	.3
		move.w	#1,menu_item
		rts
.3		cmp.w	#86/2,d0
		bge.s	.4
		move.w	#2,menu_item
		rts
.4		cmp.w	#100/2,d0
		bge.s	.5
		move.w	#3,menu_item
		rts
.5		cmp.w	#115/2,d0
		bge.s	.6
		move.w	#4,menu_item
		rts
.6		cmp.w	#126/2,d0
		bge.s	.zex_e
		move.w	#5,menu_item
.zex_e		rts

rob_bits
		cmp.w	#156/2,d0
		blt.s	.rob_e		; outside range!
		cmp.w	#171/2,d0
		bge.s	.2
		move.w	#6,menu_item	; first picture
		rts
.2		cmp.w	#186/2,d0
		bge.s	.3
		move.w	#7,menu_item
		rts
.3		cmp.w	#201/2,d0
		bge.s	.4
		move.w	#8,menu_item
		rts
.4		cmp.w	#215/2,d0
		bge.s	.5
		move.w	#9,menu_item
		rts
.5		cmp.w	#231/2,d0
		bge.s	.6
		move.w	#10,menu_item
		rts
.6		cmp.w	#242/2,d0
		bge.s	.rob_e
		move.w	#11,menu_item
.rob_e		rts

music_bits
		cmp.w	#273/2,d0
		blt.s	.music_e
		cmp.w	#287/2,d0
		bge.s	.2
		move.w	#12,menu_item	; first picture
		rts
.2		cmp.w	#302/2,d0
		bge.s	.3
		move.w	#13,menu_item
		rts
.3		cmp.w	#317/2,d0
		bge.s	.4
		move.w	#14,menu_item
		rts
.4		cmp.w	#332/2,d0
		bge.s	.5
		move.w	#15,menu_item
		rts
.5		cmp.w	#344/2,d0
		bge.s	.music_e
		move.w	#16,menu_item
.music_e	rts

scroll_bits
		cmp.w	#373/2,d0
		blt.s	.scroll_e
		cmp.w	#388/2,d0
		bge.s	.2
		move.w	#17,menu_item	; first picture
		rts
.2		cmp.w	#404/2,d0
		bge.s	.3
		move.w	#18,menu_item
		rts
.3		cmp.w	#418/2,d0
		bge.s	.4
		move.w	#19,menu_item
		rts
.4		cmp.w	#433/2,d0
		bge.s	.5
		move.w	#20,menu_item
		rts
.5		cmp.w	#444/2,d0
		bge.s	.scroll_e
		move.w	#21,menu_item
.scroll_e	rts

exit_bit
		cmp.w	#456/2,d0
		blt.s	.exit_e
		cmp.w	#467/2,d0
		bge.s	.exit_e
		move.w	#22,menu_item
.exit_e		rts

		
put_col
		lea	menu_colours1,a0
		lea	menu_colours2,a1
		move.w	menu_item,d0
		bmi.s	.skipit
		bra.s	.l2
.loop		add.w	#8,a0
		add.w	#8,a1
.l2		dbf	d0,.loop
		move.w	#$949,2(a0)
		move.w	#$949,2(a1)
.skipit		rts

decrease_cols
		lea	menu_colours1,a0
		lea	menu_colours2,a1
		move.w	#23-1,d6
.loop
		tst.w	2(a0)
		beq.s	.no_change
		move.w	2(a0),d0
		move.w	d0,d1
		move.w	d1,d2
		and.w	#$f00,d0
		and.w	#$f0,d1
		and.w	#$f,d2
		tst.w	d0
		beq.s	.skip_red
		sub.w	#$100,d0
.skip_red
		tst.w	d1
		beq.s	.skip_green
		sub.w	#$10,d1
.skip_green
		tst.w	d2
		beq.s	.skip_blue
		sub.w	#$1,d2
.skip_blue
		or.w	d2,d0
		or.w	d1,d0
		move.w	d0,2(a0)
		move.w	d0,2(a1)
.no_change	add.w	#8,a0
		add.w	#8,a1
		dbf	d6,.loop
		rts				

menu_item	dc.w	0
last_menu	dc.w	0

text_writer
		rept	scroll_speed
		bsr	scroll_text
		endr
		bsr	write_text
		rts

scroll_text
		lea	scroll_data,a0
		move.w	#8-1,d7
.loop
		moveq	#0,d3
		addx.w	d3,d3

		roxl	40(a0)
		roxl	38(a0)
		roxl	36(a0)
		roxl	34(a0)
		roxl	32(a0)
		roxl	30(a0)
		roxl	28(a0)
		roxl	26(a0)
		roxl	24(a0)	
		roxl	22(a0)
		roxl	20(a0)
		roxl	18(a0)
		roxl	16(a0)
		roxl	14(a0)
		roxl	12(a0)
		roxl	10(a0)
		roxl	8(a0)
		roxl	6(a0)
		roxl	4(a0)
		roxl	2(a0)
		roxl	(a0)
		add.l	#42,a0
		dbf	d7,.loop
		
		subq.w	#1,counter

		cmp.w	#0,counter
		bgt.s	.exit
		bsr.s	newletter
		move.w	#8,counter
.exit		rts
newletter
		movea.l	textp,a1
		tst.b	(a1)
		bne.s	.noreset
		move.l	lasttext,a1
		add.l	#14,a1		; skip '<TEXT CHANGE>'
		move.l	a1,textp
		bra.s	newletter

.noreset
		move.l	textp,a1
		moveq	#0,d0
		move.b	(a1)+,d0
		move.l	a1,textp

		sub.w	#" ",d0
		bpl.s	.skip_ret
		moveq	#0,d0

.skip_ret
		lsl.w	#1,d0
		lea	scroll_data+40,a0
		lea	font,a1
		lea	font_lookup,a2

		move.w	0(a2,d0.w),d0
		add.w	d0,a1		; this letter

x		set	0
y		set	0
		rept	8
		move.b	x(a1),y(a0)
		move.b	#0,y+1(a0)
x		set	x+80
y		set	y+42
		endr

		move.w	#7,counter
		rts

counter		dc.w	8

write_text


		lea	menu_scr,a0
		add.l	#504*80,a0		; this far down, base

		moveq	#0,d6
		lea	scroll_data,a1		; source

		rept	8

		move.l	(a1),0(a0)
		move.l	4(a1),4(a0)
		move.l	8(a1),8(a0)
		move.l	12(a1),12(a0)
		move.l	16(a1),16(a0)
		move.l	20(a1),20(a0)
		move.l	24(a1),24(a0)
		move.l	28(a1),28(a0)
		move.l	32(a1),32(a0)
		move.l	36(a1),36(a0)

;		move.l	(a1),40(a0)
;		move.l	4(a1),44(a0)
;		move.l	8(a1),48(a0)
;		move.l	12(a1),52(a0)
;		move.l	16(a1),56(a0)
;		move.l	20(a1),60(a0)
;		move.l	24(a1),64(a0)
;		move.l	28(a1),68(a0)
;		move.l	32(a1),72(a0)
;		move.l	36(a1),76(a0)

		add.l	#42,a1
		add.l	#40,a0
		endr

		rts

scroll_data
		rept	8
		dc.w	0,0,0,0,0,0,0,0,0,0
		dc.w	0,0,0,0,0,0,0,0,0,0
		dc.w	0
		endr



*******************************************************************************
* Music_Int
*
* The vertical blank music driver
*************************************************************************
music_int
		move.w	#-1,vblank
		movem.l	d0-d7/a0-a6,-(a7)
		lea	custom,a5
		btst	#5,Intreqr+1(a5)
		beq.b	.out
	
		play_music
	
		bsr	update_mouse
.skipit		move.w	#$0020,Intreq(a5)
.out		movem.l	(a7)+,d0-d7/a0-a6
		rte

intro		dc.b	-1,0

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
blank		dc.l	0,0,0,0,0,0,0,0
		dc.l	0,0,0,0,0,0,0,0
blank2		dc.w	$001,$001,$001,$001,$001,$001,$001,$001
		dc.w	$001,$001,$001,$001,$001,$001,$001,$001
blank3		dc.w	$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff
		dc.w	$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff

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



; music routines go here!
	
use		=	$955c&$b55a&$9c5e&$951e&$9806
playback_speed	=	3
		include	music/types.i
		include	music/player6.i
		incdir 	work:cdn/scrapbook/music/
		include	player.s
		incdir	work:cdn/scrapbook/
	
		
*******************************************************************************
************************ fast data.

data_segment
base

bitplaneLUT
x		set	0
	IFNE	screen_depth
		rept	screen_depth
		dc.l	x
x		set	x+screen_bitplaneoffset
		endr
	ENDC

y_coord_LUT
x	set	0

	rept	screen_height
	dc.w	x
x	set	x + screen_nextlineoffset
	endr


page_offsets	dc.l	0,0,0,0,0,0,0,0,0



	even
text_lookup
	; 80 lookups
y		set	0

x		set	0
		rept	79
		dc.w	x+y
x		set	x+1
		endr
y		set	y+f_h*f_w
x		set	0

x		set	0
		rept	79
		dc.w	x+y
x		set	x+1
		endr
x		set	0

x		set	0
		rept	79
		dc.w	x+y
x		set	x+1
		endr


min_y		dc.w	232-5

mod_l		dc.l	mod1,mod2,mod3,mod4,mod5,mod1


;pix
sect_logo	incbin	gfx/sctlogo.raw
		even
sb_title	incbin	gfx/sbtitle.raw
		even
sb_menu		incbin	gfx/sbmenu.raw
		even

pic_look	
		dc.l	zex_1,zex_2,zex_3,zex_4,zex_5,zex_6
		dc.l	rob_1,rob_2,rob_3,rob_4,rob_5,rob_6
size_look	
		dc.w	2,2,0,2,2,0
		dc.w	0,0,0,1,2,0

;	0=640x256x16
;	1=320x256 EHB
;	2=320x256x32



rob_1		incbin	gfx/rob_renr
		even
rob_2		incbin	gfx/rob_grl.raw
		even
rob_3		incbin	gfx/rob_grr.raw
		even
rob_4		incbin	gfx/rob_alde.raw
		even
rob_5		incbin	gfx/rob_clow.raw
		even
rob_6		incbin	gfx/rob_abc.raw
		even
zex_1		incbin	gfx/zex_kni.raw
		even
zex_2		incbin	gfx/zex_ange.raw
		even
zex_3		incbin	gfx/zex_fone
		even
zex_4		incbin	gfx/zex_elf.raw
		even
zex_5		incbin	gfx/zex_rave.raw
		even
zex_6		incbin	gfx/babysit.raw

*******************************************************************************

; colour text stuff

curr_col	dc.w	1

; scroll stuff

texts		dc.l	about,greets,kimba,raptor,rob

textp		dc.l	about+14
lasttext	dc.l	about
		dc.w	0

about		
		dc.b	'  --ABOUT--   '
;		dc.b	' TEXT CHANGE          '
;		dc.b	'Welcome to SCRAPBOOK - The first and last '
;		dc.b	'slideshow ever from SeCT....    '
		incbin	sb.about
		dc.b	'         ',0
		even
greets		
		dc.b	' --GREETS--  '
;		dc.b	' TEXT CHANGE '
		incbin	sb.greets
		dc.b	'         ',0
		even
kimba		
		dc.b	' --KIMBA--   '
;		dc.b	' TEXT CHANGE '
		incbin	sb.kimba
		dc.b	'         ',0
		even
raptor		
		dc.b	' --RAPTOR--  '
;		dc.b	' TEXT CHANGE '
		incbin	sb.raptor
		dc.b	'         ',0
		even
rob		
		dc.b	' ---ROB---   '
;		dc.b	' TEXT CHANGE '
		dc.b	'This text should have described how Rob '
		dc.b	'created his art, but due to one MAJOR '
		dc.b	'mistake by me (Kimba) I managed to delete '
		dc.b	'his text file. I apologize to Rob, '
		dc.b	'(if he is reading this) and I hope that '
		dc.b	'he will forgive my stupid mistake... '
		dc.b	'Rob, you were a very good artist........ '
		dc.b	'                         '
		dc.b	0
		even
		
font_lookup
	; 80 lookups
y		set	0
x		set	0
		rept	79
		dc.w	x+y
x		set	x+1
		endr
y		set	y+f_h*f_w
x		set	0
		rept	79
		dc.w	x+y
x		set	x+1
		endr
y		set	y+f_h*f_w
x		set	0
		rept	79
		dc.w	x+y
x		set	x+1
		endr
y		set	y+f_h*f_w
x		set	0

font		incbin	fonts/souri.font

*******************************************************************************
************************ chip data.

		section	chipdata,data_c



copper:		dc.w	-1,-2

copper4		
;		dc.w	$2207,$fffe
c4		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	bplcon0,hires+4<<12+color
		dc.w	bpl1mod,0

		dc.w	diwstrt,$28<<8+xstrt
		dc.w	diwstop,($128-$100)<<8+(xstop-$100)

		dc.w	ddfstrt,$38
		dc.w	ddfstop,$d0

		dc.w	-1,-2			; 4 bitplane copper

copper5
;		dc.w	$2207,$fffe
c5		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	bpl5pth,0,bpl5ptl,0
		dc.w	bplcon0,5<<12+color
		dc.w	bpl1mod,0
		dc.w	diwstrt,$28<<8+xstrt
		dc.w	diwstop,($128-$100)<<8+(xstop-$100)
		dc.w	ddfstrt,$38
		dc.w	ddfstop,$d0
		dc.w	-1,-2			; 5 bitplane copper

ehbcopper5
;		dc.w	$2207,$fffe
ehbc5		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	bpl5pth,0,bpl5ptl,0
		dc.w	bpl6pth,0,bpl6ptl,0
		dc.w	bplcon0,6<<12+color
		dc.w	bpl1mod,0

		dc.w	diwstrt,$28<<8+xstrt
		dc.w	diwstop,($128-$100)<<8+(xstop-$100)

		dc.w	ddfstrt,$38
		dc.w	ddfstop,$d0

		dc.w	-1,-2			; 4 bitplane copper


lacecopper1	
		dc.w	$2a01,$fffe
		dc.w	diwstrt,$581
		dc.w	bplcon0,$204
		dc.w	bplcon2,$24
		dc.w	diwstrt,ystrt<<8+xstrt
		dc.w	diwstop,(ystop-$100)<<8+(xstop-$100)
		dc.w	ddfstrt,$3c
		dc.w	ddfstop,$d0
		dc.w	bplcon0,0
		dc.w	bpl1mod,$50
		dc.w	bpl2mod,$50
lacepl1		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	$2c01,$fffe
		dc.w	bplcon0,$c204
		dc.w	$ffdf,$fffe
		dc.w	$2c01,$fffe
		dc.w	bplcon0,$204
lacecop1	dc.w	cop1lch,0,cop1lcl,0
		dc.w	-1,-2
		


lacecopper2
		dc.w	$2a01,$fffe
		dc.w	diwstrt,$581
		dc.w	bplcon0,$204
		dc.w	bplcon2,$24
		dc.w	diwstrt,ystrt<<8+xstrt
		dc.w	diwstop,(ystop-$100)<<8+(xstop-$100)
		dc.w	ddfstrt,$3c
		dc.w	ddfstop,$d0
		dc.w	bplcon0,0
		dc.w	bpl1mod,$50
		dc.w	bpl2mod,$50
lacepl2		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	$2c01,$fffe
		dc.w	bplcon0,$c204
		dc.w	$ffdf,$fffe
		dc.w	$2c01,$fffe
		dc.w	bplcon0,$204
lacecop2	dc.w	cop1lch,0,cop1lcl,0
		dc.w	-1,-2



menucopper1	

;		dc.w	$2a01,$fffe

;		dc.w	bplcon0,$204
		dc.w	bplcon2,$24

		dc.w	bplcon0,$0
		dc.w	bpl1mod,$50
		dc.w	bpl2mod,$50
menupl1		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
;		dc.w	$2c01,$fffe
		dc.w	bplcon0,$c204

menu_colours1
		dc.w	color15,0
		dc.w	$3707,$fffe
		dc.w	color15,0
		dc.w	$3f07,$fffe
		dc.w	color15,0
		dc.w	$4607,$fffe
		dc.w	color15,0	; and so on!
		dc.w	$4d07,$fffe
		dc.w	color15,0
		dc.w	$5407,$fffe
		dc.w	color15,0	; one

		dc.w	$6907,$fffe
		dc.w	color15,0
		dc.w	$7107,$fffe
		dc.w	color15,0
		dc.w	$7907,$fffe
		dc.w	color15,0
		dc.w	$8107,$fffe
		dc.w	color15,0
		dc.w	$8807,$fffe
		dc.w	color15,0
		dc.w	$8f07,$fffe
		dc.w	color15,0	; two

		dc.w	$a307,$fffe
		dc.w	color15,0
		dc.w	$ab07,$fffe
		dc.w	color15,0
		dc.w	$b307,$fffe
		dc.w	color15,0
		dc.w	$bb07,$fffe
		dc.w	color15,0
		dc.w	$c107,$fffe
		dc.w	color15,0	; three

		dc.w	$d507,$fffe
		dc.w	color15,0
		dc.w	$dd07,$fffe
		dc.w	color15,0
		dc.w	$e507,$fffe
		dc.w	color15,0
		dc.w	$ed07,$fffe
		dc.w	color15,0
		dc.w	$f507,$fffe
		dc.w	color15,0	; four

		dc.w	$fe07,$fffe
		dc.w	color15,0

		dc.w 	$ff09,$fffe,$ffdd,$fffe	

		dc.w	$0f09,$fffe
		dc.w	color15,0
		dc.w	bplcon0,1<<12+color
		dc.w	bpl1mod,0
sc1		dc.w	bpl1pth,0,bpl1ptl,0
menucop1	dc.w	cop1lch,0,cop1lcl,0

		dc.w	-1,-2			; 4 bitplane copper

menucopper2
;		dc.w	$2a01,$fffe
;		dc.w	bplcon0,$204
		dc.w	bplcon2,$24
		dc.w	bplcon0,0
		dc.w	bpl1mod,$50
		dc.w	bpl2mod,$50

menupl2		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0

;		dc.w	$2c01,$fffe
		dc.w	bplcon0,$c204

menu_colours2
		dc.w	color15,0
		dc.w	$3707,$fffe
		dc.w	color15,0
		dc.w	$3f07,$fffe
		dc.w	color15,0
		dc.w	$4607,$fffe
		dc.w	color15,0	; and so on!
		dc.w	$4d07,$fffe
		dc.w	color15,0
		dc.w	$5407,$fffe
		dc.w	color15,0	; one

		dc.w	$6907,$fffe
		dc.w	color15,0
		dc.w	$7107,$fffe
		dc.w	color15,0
		dc.w	$7907,$fffe
		dc.w	color15,0
		dc.w	$8107,$fffe
		dc.w	color15,0
		dc.w	$8707,$fffe
		dc.w	color15,0
		dc.w	$8f07,$fffe
		dc.w	color15,0	; two

		dc.w	$a307,$fffe
		dc.w	color15,0
		dc.w	$ab07,$fffe
		dc.w	color15,0
		dc.w	$b307,$fffe
		dc.w	color15,0
		dc.w	$bb07,$fffe
		dc.w	color15,0
		dc.w	$c107,$fffe
		dc.w	color15,0	; three

		dc.w	$d507,$fffe
		dc.w	color15,0
		dc.w	$dd07,$fffe
		dc.w	color15,0
		dc.w	$e507,$fffe
		dc.w	color15,0
		dc.w	$ed07,$fffe
		dc.w	color15,0
		dc.w	$f507,$fffe
		dc.w	color15,0	; four

		dc.w	$fe07,$fffe
		dc.w	color15,0

		dc.w 	$ff09,$fffe,$ffdd,$fffe	

		dc.w	$0f09,$fffe
		dc.w	color15,0
		dc.w	bplcon0,1<<12+color
		dc.w	bpl1mod,0
sc2		dc.w	bpl1pth,0,bpl1ptl,0

menucop2	dc.w	cop1lch,0,cop1lcl,0


		dc.w	-1,-2			; 4 bitplane copper




	
		incdir	work:cdn/scrapbook/music/
mod1		incbin	p60.soothethebeast.9806
		even
mod2		incbin	p60.flibble.955c
		even
mod3		incbin	p60.gurgle.b55a
		even
mod4		incbin	p60.kindachip.9c5e
		even
mod5		incbin	p60.latest.951e
		even
	
		even


************************
		section bss,bss_c
piccy32		ds.b	32		; for 32 colour pictures
piccy16		ds.b	32		; for 16 colour pictures
screen		ds.b	screen_bitplane*4

menu_pal	ds.b	32
menu_scr	ds.b	screen_bitplane*4	

*******************************************************************************

		end

