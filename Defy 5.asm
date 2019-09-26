;------------------------------------------------------------------------------
;
; ***    ****  **** *   *           **** 
; *  *  *     *      * *            *           Source Code
; *   *  **    **     *              **         
; *   * *     *       *                *        All code by cro / CYDoNiA
; * ***  **** *       *             ***         Startup by Extremist / CDN
;
;------------------------------------------------------------------------------

; Please note: This is a good example of how NOT to code a disk-mag
;
; The data format used by this mag for storing articles is proprietry. The
; data cruncher is not available for use under AmigaOS
;
; Now includes multi-colour text!
;
; Now uses StoneCruncher instead of Ice Cruncher
;
; Devpac form

		opt	o+,w-

yes		equ	0
no		equ	1

music_on	set	yes		; include music files?

	
	ifeq	music_on
		incdir	defy5:
		include	music/maccros.i
	endc
		incdir	defy5:
		include	includes/custom2.i
		incdir	defy5:

fade_workbench	equ	yes
screen_res	equ	1		; 0 = lowres, 1 = hires.
screen_width    equ     80
screen_height   equ     194
screen_depth    equ     3
screen_bitplane equ     screen_width*screen_height
screen_size     equ     screen_depth*screen_bitplane

screen_bitplaneoffset	equ	screen_bitplane
screen_nextlineoffset	equ	screen_width

xmin            equ     0
xmax            equ     (screen_width*8)-1
ymin            equ     0
ymax            equ     screen_height-1

xstrt		equ	$081
ystrt		equ	$01c
xstop		equ	$1c1
ystop		equ	$126+12

f_w		equ	80
f_h		equ	8
f_d		equ	1

*******************************************************************************
first		jmp	startup
		dc.b	' - DEFY Issue #5 - ©1996 CYDoNiA - '
		dc.b	' - DEFY is now one year old!!! '
		even 
startup:	bsr.s	take_system
		tst.l	d0
		beq.s	.error
		bsr	initiations

		bsr	intro_bit
		bsr	set_main
		lea	modname1,a0
		bsr	write_music
		bsr	fade_panels
		bsr	main
		bsr	start_down2

.ghh		bsr	t_fade
		tst.b	d0
		bne.s	.ghh

		bsr	clear_sprites
		bsr	fade_panels2
	ifeq		music_on
		stop_music	
	endc
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
		move.w  #set+dmaen+blten+bplen+copen+spren,dmacon(a5)

		move.w	#$7fff,intena(a5)
		move.l	#music_int,$6c
		move.w	#set+inten+vertb,intena(a5)
; set copper
		bsr	clear_sprites
		lea	textbpl,a0	
		lea	text_screen,a1
		move.l	a1,d0
		rept	3
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		swap	d0
		addq.w	#8,a0
		add.l	#screen_bitplane,d0
		endr
		lea	topbpl,a0
		lea	top+32,a1
		move.l	a1,d0
		rept	4
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		swap	d0
		add.l	#80*30,d0	; new top is 30 lines high
		addq.w	#8,a0
		endr
		lea	botbpl,a0
		lea	bottom+32,a1
		move.l	a1,d0
		rept	4
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		swap	d0
		add.l	#40*80,d0
		addq.w	#8,a0
		endr
		move.l	highlight1,d0
		move.w	d0,highbpl+6
		swap	d0
		move.w	d0,highbpl+2	; set highlight plane

		lea	font1,a0
		moveq	#8-1,d0
.lkj
		move.b	#0,(a0)
		lea	80(a0),a0
		dbf	d0,.lkj
		rts

clear_sprites
		lea	screen_copper,a0
		lea	a_copper,a2
		lea	a_copper2,a3
		lea	nullspr,a1
		move.l	a1,d0
		rept	8
		move.w	d0,6(a0)
		move.w	d0,6(a2)
		move.w	d0,6(a3)
		swap	d0
		move.w	d0,2(a0)
		move.w	d0,2(a2)
		move.w	d0,2(a3)
		addq.w	#8,a0
		addq.w	#8,a2
		addq.w	#8,a3
                swap    d0
		endr		
		rts


****************************************************************************
* Set_First
*
* Decrunches selected article to 'text-32'
****************************************************************************
set_first
		lea	decr,a0
		bsr	write_header	; wait for new font from Souri :-)
		move.w	article_num,d0
		lsl.w	#2,d0
		lea	articles,a0
		move.l	0(a0,d0.w),a0	; this pointer for pack
		lea	text-32,a1
		move.l	a5,-(a7)
		bsr	ice_decrunch
		move.l	(a7)+,a5
		lea	text+26+20,a0	; data store, skip author
		move.w	(a0),max_pagenum
		move.w	(a0)+,d0
		lea	page_offsets,a1
.loop		move.w	(a0)+,(a1)+
		dbf	d0,.loop	; get page offsets
		move.l	a0,start_p
		move.l	a0,textp	; reset_pointers!

		move.w	curr_pagenum,d0
		lea	page_offsets,a0
		move.l	start_p,a1
		lsl.w	#1,d0
		add.w	0(a0,d0.w),a1
		move.l	a1,textp

		bsr	write_pagenum
		move.w	#1,curr_col
		move.w	#0,curr_font
		rts

decr		dc.b	'DECRUNCHING TEXT - WAIT   '
		dc.b	' -^-[ CYDoNiA ]-^-  '
		even
****************************************************************************
* Write_Header
*
* Writes article Name to top panel
* Writes Author Name to top panel
****************************************************************************
write_header
		move.l	a0,-(a7)
		bsr	clear_header
		move.l	(a7)+,a0
		
		move.w	#26-1,d6	; this many chars		
		lea	top+32+(320/8)+(3*80),a1	; this screen, was 5
titleloop		
		lea	font2,a4
		moveq	#0,d0
		move.b	(a0)+,d0
		sub.w	#" ",d0
		lsl.w	#1,d0		; offset
		lea	text_lookup,a2
		move.w	0(a2,d0.w),d0	; this offset
		add.w	d0,a4
		
tf1		set	0
		rept	7		; height of font
		move.b	tf1(a4),d0
		or.b	d0,tf1(a1)
		or.b	d0,tf1+(30*80)(a1)
		or.b	d0,tf1+(60*80)(a1)
		or.b	d0,tf1+(90*80)(a1)
tf1		set	tf1+80
		endr
		addq.l	#1,a1
		dbf	d6,titleloop

		move.w	#20-1,d6	; this many chars		
		lea	top+32+(320/8)+(17*80),a1	; this screen
authorloop		
		lea	font2,a4
		moveq	#0,d0
		move.b	(a0)+,d0
		sub.w	#" ",d0
		lsl.w	#1,d0		; offset
		lea	text_lookup,a2
		move.w	0(a2,d0.w),d0	; this offset
		add.w	d0,a4
		
tf1		set	0
		rept	7		; height of font
		move.b	tf1(a4),d0
		or.b	d0,tf1(a1)
		or.b	d0,tf1+(30*80)(a1)
		or.b	d0,tf1+(60*80)(a1)
		or.b	d0,tf1+(90*80)(a1)
tf1		set	tf1+80
		endr
		addq.l	#1,a1
		dbf	d6,authorloop

		rts

clear_header
		lea	top+32+(320/8),a1	; this screen
		lea	top+32+(320/8)+(17*80),a3
		lea	clips+(320/8),a2
		lea	clips+(320/8)+(17*80),a4
		move.w	#10-1,d0
.clear_loop
		move.l	(a2),(a1)
		move.l	4(a2),4(a1)
		move.l	8(a2),8(a1)
		move.l	12(a2),12(a1)
		move.l	16(a2),16(a1)
		move.l	20(a2),20(a1)
		move.l	24(a2),24(a1)
		move.l	28(a2),28(a1)
		move.l	(a4),(a3)
		move.l	4(a4),4(a3)
		move.l	8(a4),8(a3)
		move.l	12(a4),12(a3)
		move.l	16(a4),16(a3)

		move.l	4000(a2),2400(a1)
		move.l	4004(a2),2404(a1)
		move.l	4008(a2),2408(a1)
		move.l	4012(a2),2412(a1)
		move.l	4016(a2),2416(a1)
		move.l	4020(a2),2420(a1)
		move.l	4024(a2),2424(a1)
		move.l	4028(a2),2428(a1)
		move.l	4000(a4),2400(a3)
		move.l	4004(a4),2404(a3)
		move.l	4008(a4),2408(a3)
		move.l	4012(a4),2412(a3)
		move.l	4016(a4),2416(a3)

		move.l	8000(a2),4800(a1)
		move.l	8004(a2),4804(a1)
		move.l	8008(a2),4808(a1)
		move.l	8012(a2),4812(a1)
		move.l	8016(a2),4816(a1)
		move.l	8020(a2),4820(a1)
		move.l	8024(a2),4824(a1)
		move.l	8028(a2),4828(a1)
		move.l	8000(a4),4800(a3)
		move.l	8004(a4),4804(a3)
		move.l	8008(a4),4808(a3)
		move.l	8012(a4),4812(a3)
		move.l	8016(a4),4816(a3)

		move.l	12000(a2),7200(a1)
		move.l	12004(a2),7204(a1)
		move.l	12008(a2),7208(a1)
		move.l	12012(a2),7212(a1)
		move.l	12016(a2),7216(a1)
		move.l	12020(a2),7220(a1)
		move.l	12024(a2),7224(a1)
		move.l	12028(a2),7228(a1)
		move.l	12000(a4),7200(a3)
		move.l	12004(a4),7204(a3)
		move.l	12008(a4),7208(a3)
		move.l	12012(a4),7212(a3)
		move.l	12016(a4),7216(a3)

		lea	80(a1),a1
		lea	80(a2),a2
		lea	80(a3),a3
		lea	80(a4),a4
		dbf	d0,.clear_loop
		rts
		
****************************************************************************
* Write_Pagenum
*
* Writes curr_pagenum and Max_pagenum to top panel
****************************************************************************
write_pagenum
;		rts
		movem.l	d0/a1/a2,-(a7)
		bsr	clear_page
		movem.l	(a7)+,d0/a1/a2
		move.w	#3-1,d6

		lea	page_text,a0
		tst.w	max_pagenum
		beq.s	.nopagenum	; 1 page only, don't write
		move.w	curr_pagenum,d0
		addq.w	#1,d0
		lsl.w	#1,d0
		lea	pagenumbers,a1
		add.w	d0,a1
		move.b	(a1)+,(a0)+
		move.b	(a1)+,(a0)+

;		addq.w	#1,d0
;		move.b	#"0",(a0)
;		add.b	d0,(a0)+	; first char
;		move.b	#"/",(a0)+
;		move.w	max_pagenum,d0
;		addq.w	#1,d0
;		move.b	#"0",(a0)
;		add.b	d0,(a0)		; all set...

		bra.s	.doit
.nopagenum	rept	3		; was 5
		move.b	#" ",(a0)+
		endr
.doit		lea	page_text,a0
		lea	top+32+68+(17*80),a1
titleloop2
		lea	font2,a4
		moveq	#0,d0
		move.b	(a0)+,d0
		sub.w	#" ",d0
		add.w	d0,a4
		
tf1		set	0
		rept	f_h-1
		move.b	tf1(a4),d0
		or.b	d0,tf1(a1)
		or.b	d0,tf1+(30*80)(a1)
		or.b	d0,tf1+(60*80)(a1)
		or.b	d0,tf1+(90*80)(a1)
tf1		set	tf1+80
		endr
		addq.l	#1,a1
		dbf	d6,titleloop2
		rts

; change this to number only!!!
		even
page_text	dc.b	'   '
		even
pagenumbers	dc.b	'  '
		dc.b	' 1'
		dc.b	' 2'
		dc.b	' 3'
		dc.b	' 4'
		dc.b	' 5'
		dc.b	' 6'
		dc.b	' 7'
		dc.b	' 8'
		dc.b	' 9'
		dc.b	'10'
		dc.b	'11'
		dc.b	'12'
		dc.b	'13'
		dc.b	'14'
		dc.b	'15'
		dc.b	'16'
		dc.b	'17'
		dc.b	'18'
		dc.b	'19'
		dc.b	'20'
		dc.b	'21'
		dc.b	'22'
		dc.b	'23'
		dc.b	'24'
		dc.b	'25'
		dc.b	'26'
		dc.b	'27'
		even
clear_page
		lea	top+32+68+(17*80),a1
		lea	clips+68+(17*80),a2
		move.w	#7-1,d0
.clear_loop
		move.l	(a2),(a1)
		move.l	4(a2),4(a1)
		move.l	4000(a2),2400(a1)
		move.l	4004(a2),2404(a1)
		move.l	8000(a2),4800(a1)
		move.l	8004(a2),4804(a1)
		move.l	12000(a2),7200(a1)
		move.l	12004(a2),7204(a1)
		lea	80(a2),a2
		lea	80(a1),a1
		dbf	d0,.clear_loop
		rts


******************************************************************************
* Set_Main
*
* Sets up main screen
****************************************************************************
set_main
		move.l  #screen_copper,cop1lch(a5)
		move.w	#ystrt<<8+xstrt,diwstrt(a5)
		move.w	#(ystop-$100)<<8+(xstop-$100),diwstop(a5)
		move.w	#screen_depth<<12+color,bplcon0(a5)
		move.w	#$3c,ddfstrt(a5)
		move.w	#$d4,ddfstop(a5)
		move.w	#0,bpl1mod(a5)
		move.w	#0,bpl2mod(a5)
		lea	screen_copper,a0
		lea	pointer,a1
		move.l	a1,d0
		move.w	d0,6(a0)
		swap	d0
		move.w	d0,2(a0)
		move.w	#0,curr_pagenum
		bsr	set_first
		rts

****************************************************************************
* Fade_Panels
*
* Fades top and bottom panels smoothly
****************************************************************************
fade_panels
		move.l	a5,-(a7)
		movem.l	top,d0-d7
		movem.l	d0-d7,dest_pal
		movem.l	blank,d0-d7
		movem.l	d0-d7,source_pal
		move.w	#16,d0
		move.w	#4,d1
		lea	source_pal,a0
		lea	dest_pal,a1
		bsr	trigfadeto
		move.l	(a7)+,a5
panel_fade
		tst.w	vblank
		beq.s	panel_fade
		clr.w	vblank
		move.l	a5,-(a7)
		bsr	fade_to
		move.b	fadetoflag(a5),d0
		move.l	(a7)+,a5
		lea	source_pal,a0
		lea	topcol+2,a1
		lea	botcol+2,a2
		rept	16
		move.w	(a0),(a1)+
		move.w	(a0)+,(a2)+
		addq.w	#2,a1
		addq.w	#2,a2
		endr
		tst.b	d0
		bne	panel_fade
		rts

start_down2
		move.l	a5,-(a7)
		movem.l	fontpal,d0-d7
		movem.l	d0-d7,source_pal
		movem.l	blank,d0-d7
		movem.l	d0-d7,dest_pal
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#16,d0
		move.w	#8,d1
		bsr	trigfadeto
		move.l	(a7)+,a5
		addq.w	#1,this_one
		rts
		
fade_panels2
		move.l	a5,-(a7)
		movem.l	top,d0-d7
		movem.l	d0-d7,source_pal
		movem.l	blank,d0-d7
		movem.l	d0-d7,dest_pal
		move.w	#16,d0
		move.w	#4,d1
		lea	source_pal,a0
		lea	dest_pal,a1
		bsr	trigfadeto
		move.l	(a7)+,a5
		jmp	panel_fade

****************************************************************************
* intro_Bit
*
* Shows title pages
****************************************************************************
intro_bit
 		tst.w	vblank
		beq.w	intro_bit
		clr.w	vblank

		move.w	intro_state,d0
		lsl.w	#2,d0
		move.l	intro_routines(pc,d0.w),a0
		jsr	(a0)
		tst.w	d7		; 0 = exit!!
		beq.s	intro_bit
		rts

intro_routines
		dc.l	title_up
		dc.l	set_title_down
		dc.l	title_up
		dc.l	set_defy_title
		dc.l	title_up
		dc.l	delay_a_bit
		dc.l	set_defy_down
		dc.l	title_up
		dc.l	quit_it

intro_state	dc.w	0

intro_delay	dc.w	200
quit_it	
		move.w	#-1,d7
		move.w	#-1,intro
;		bsr	clear_screen
		rts

title_up
		move.l	a5,-(a7)
		bsr	fade_to
		move.b	fadetoflag(a5),d0
		move.l	(a7)+,a5
		move.b	d0,-(a7)
		movem.l	source_pal,d0-d7
		movem.l	d0-d7,color00(a5)
		movem.l	source_pal+32,d0-d7
		movem.l	d0-d7,color16(a5)
		move.b	(a7)+,d0
		tst.b	d0
		bne.s	.exit
		addq.w	#1,intro_state
.exit		moveq	#0,d7
		rts		

delay_a_bit
		subq.w	#1,intro_delay
		bne.s	.exit
		addq.w	#1,intro_state
		move.w	#500,intro_delay
.exit		moveq	#0,d7
		rts

set_title_down
		movem.l	dest_pal,d0-d7
		movem.l	d0-d7,source_pal
		movem.l	dest_pal+32,d0-d7
		movem.l	d0-d7,source_pal+32
		movem.l	blank3,d0-d7
		movem.l	d0-d7,dest_pal
		movem.l	d0-d7,dest_pal+32
		move.l	a5,-(a7)
		move.w	#32,d0
		move.w	#4,d1
		lea	source_pal,a0
		lea	dest_pal,a1
		bsr	trigfadeto
		move.l	(a7)+,a5
		addq.w	#1,intro_state
		moveq	#0,d7
		rts

set_defy_title

;		dc.w	diwstrt,$22<<8+$81
;		dc.w	diwstop,($136-$100)<<8+($1c1-$100)
;		dc.w	bplcon0,1<<12+color
;		dc.w	ddfstrt,($81-17)/2
;		dc.w	ddfstop,($81-17)/2+8*(($1c1-$81)/16-1)
;		dc.w	bpl1mod,0
;		dc.w	bpl2mod,0


		move.l	#copper,cop1lch(a5)
		move.w	#$2c<<8+xstrt,diwstrt(a5)
		move.w	#($12c-$100)<<8+(xstop-$100),diwstop(a5)
		move.w	#5<<12+color,bplcon0(a5)
		move.w	#($81-17)/2,ddfstrt(a5)
		move.w	#($81-17)/2+8*(($1c1-$81)/16-1),ddfstop(a5)
		move.w	#0,bpl1mod(a5)
		move.w	#0,bpl2mod(a5)
		lea	defylogo,a0
		lea	text_screen-64,a1
		move.l	a5,-(a7)
		bsr	ice_decrunch
		move.l	(a7)+,a5

	ifeq	music_on
		lea	mod1,a0
		init_music
	endc

		lea	text_screen,a0
		lea	a_c2,a1
		move.l	a0,d0
		rept	5
		move.w	d0,6(a1)
		swap	d0
		move.w	d0,2(a1)
		swap	d0
		add.l	#40*256,d0
		addq.w	#8,a1
		endr		; set title copper
		move.l	#a_copper2,cop1lch(a5)
		move.l	a5,-(a7)
		movem.l	text_screen-64,d0-d7
		movem.l	d0-d7,dest_pal
		movem.l	blank3,d0-d7
		movem.l	d0-d7,source_pal
		movem.l	d0-d7,source_pal+32
		movem.l	text_screen-32,d0-d7
		movem.l	d0-d7,dest_pal+32
		move.w	#32,d0
		move.w	#4,d1
		lea	source_pal,a0
		lea	dest_pal,a1
		bsr	trigfadeto
		move.l	(a7)+,a5
		addq.w	#1,intro_state
		moveq	#0,d7
		rts

set_defy_down
		movem.l	dest_pal,d0-d7
		movem.l	d0-d7,source_pal
		movem.l	dest_pal+32,d0-d7
		movem.l	d0-d7,source_pal+32
		movem.l	blank,d0-d7
		movem.l	d0-d7,dest_pal
		movem.l	d0-d7,dest_pal+32
		move.l	a5,-(a7)
		move.w	#32,d0
		move.w	#4,d1
		lea	source_pal,a0
		lea	dest_pal,a1
		bsr	trigfadeto
		move.l	(a7)+,a5
		addq.w	#1,intro_state
		moveq	#0,d7
		rts

clear_screen
		lea	text_screen,a0
		lea	custom,a5
.blt		btst	#6,dmaconr(a5)
		bne.s	.blt
.bltwait	btst	#6,dmaconr(a5)
		bne.s	.bltwait
		move.l	a0,bltdpth(a5)	; dest 1
		move.l	#USED<<16,bltcon0(a5)
		move.w	#0,bltcon1(a5)
		move.w	#0,bltamod(a5)
		move.w	#0,bltdmod(a5)
		move.w	#(256-1)*64+80/2,bltsize(a5)

.bltwait2	btst	#6,dmaconr(a5)
		bne.s	.bltwait2
		lea	255*80(a0),a0
		move.l	a0,bltdpth(a5)	; dest 1
		move.l	#USED<<16,bltcon0(a5)
		move.w	#0,bltcon1(a5)
		move.w	#0,bltamod(a5)
		move.w	#0,bltdmod(a5)
		move.w	#(256-1)*64+80/2,bltsize(a5)

.bltwait3	btst	#6,dmaconr(a5)
		bne.s	.bltwait3
		lea	255*80(a0),a0
		move.l	a0,bltdpth(a5)	; dest 1
		move.w	#0,bltcon1(a5)
		move.w	#0,bltamod(a5)
		move.l	#USED<<16,bltcon0(a5)
		move.w	#0,bltdmod(a5)
		move.w	#(256-1)*64+80/2,bltsize(a5)

.bltwait4	btst	#6,dmaconr(a5)
		bne.s	.bltwait4
		lea	255*80(a0),a0
		move.l	a0,bltdpth(a5)	; dest 1
		move.l	#USED<<16,bltcon0(a5)
		move.w	#0,bltcon1(a5)
		move.w	#0,bltamod(a5)
		move.w	#0,bltdmod(a5)
		move.w	#(256-1)*64+80/2,bltsize(a5)

.bltwait5	btst	#6,dmaconr(a5)
		bne.s	.bltwait5		

		rts



write_music
		move.l	a0,-(a7)
		bsr	clear_music
		move.l	(a7)+,a0
		
		move.w	#20-1,d6	; this many chars		
		lea	bottom+32+(394/8)+(7*80),a1	; this screen, was 8
mus_loop		
		lea	font2,a4
		moveq	#0,d0
		move.b	(a0)+,d0
		sub.w	#" ",d0
		lsl.w	#1,d0		; offset
		lea	text_lookup,a2
		move.w	0(a2,d0.w),d0	; this offset
		add.w	d0,a4
		
tf1		set	0
		rept	7		; height of font
		move.b	tf1(a4),d0
		or.b	d0,tf1(a1)
		or.b	d0,tf1+(40*80)(a1)
		or.b	d0,tf1+(80*80)(a1)
		or.b	d0,tf1+(120*80)(a1)
tf1		set	tf1+80
		endr
		addq.l	#1,a1
		dbf	d6,mus_loop
		rts


no_music_text	dc.b	'Mod not playing     '
		even
modname1	dc.b	'Cro my Friend       '
		even
modname2	dc.b	'Never Liked Dean !! '
		even

clear_music
		lea	bottom+32+(384/8)+(6*80),a1	; this screen
		lea	clips+(384/8)+(36*80),a2

		move.w	#8-1,d0
.clear_loop
		move.l	(a2),(a1)
		move.l	4(a2),4(a1)
		move.l	8(a2),8(a1)
		move.l	12(a2),12(a1)
		move.l	16(a2),16(a1)
		move.l	20(a2),20(a1)
;		move.l	24(a2),24(a1)
;		move.l	28(a2),28(a1)

		move.l	4000(a2),3200(a1)
		move.l	4004(a2),3204(a1)
		move.l	4008(a2),3208(a1)
		move.l	4012(a2),3212(a1)
		move.l	4016(a2),3216(a1)
		move.l	4020(a2),3220(a1)
;		move.l	4024(a2),3224(a1)
;		move.l	4028(a2),3228(a1)

		move.l	8000(a2),6400(a1)
		move.l	8004(a2),6404(a1)
		move.l	8008(a2),6408(a1)
		move.l	8012(a2),6412(a1)
		move.l	8016(a2),6416(a1)
		move.l	8020(a2),6420(a1)
;		move.l	8024(a2),6424(a1)
;		move.l	8028(a2),6428(a1)

		move.l	12000(a2),9600(a1)
		move.l	12004(a2),9604(a1)
		move.l	12008(a2),9608(a1)
		move.l	12012(a2),9612(a1)
		move.l	12016(a2),9616(a1)
		move.l	12020(a2),9620(a1)
;		move.l	12024(a2),9624(a1)
;		move.l	12028(a2),9628(a1)

		lea	80(a1),a1
		lea	80(a2),a2
		dbf	d0,.clear_loop
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
		bsr	do_highlight

		tst.w	this_one
		bne.s	.doing_stuff	; so you can't do anything while
					; changing pages
		bsr	get_key
		bsr	check_key
		tst.w	d7
		bpl.s	.keyit		; key pressed ???...
.doing_stuff
		btst	#6,$bfe001		; lmb ?
		bne.w	main

; LMB pressed... got a new article??
		tst.w	d6
		bmi.s	.notnewart
.waitlmb	btst	#6,$bfe001
		beq.s	.waitlmb
		move.w	d6,article_num
		move.w	curr_pagenum,last_menu
		move.w	#0,curr_pagenum
		bsr	set_first
		bsr	clear_last
		move.w	#1,this_one
		move.w	#232-5,min_y
		jmp	main
.notnewart
		bsr	checkit
.keyit		tst.w	d7
		bmi.w	main			; not a button
		cmp.w	#0,d7
		beq	PageLeft
		cmp.w	#1,d7
		beq	PageUp
		cmp.w	#2,d7
		beq	PageDown
		cmp.w	#3,d7
		beq	PageRight
		cmp.w	#4,d7
		beq	PageIndex
		cmp.w	#6,d7			; music
		beq.s	musicstuff
		cmp.w	#5,d7			; exit?
		bne.w	main
.quiit		rts

music_c		dc.w	1
music_p		dc.l	no_music_text,modname1,modname2
music_m		dc.l	0,mod1,mod2

musicstuff
	ifeq	music_on
.loopit		btst	#6,$bfe001
		beq.s	.loopit

		stop_music
		
		addq.w	#1,music_c
		cmp.w	#3,music_c
		bne.s	.restart
		move.w	#0,music_c
.stopit		move.b	#0,musicflag
		lea	no_music_text,a0
		bsr	write_music
		bra.s	.noright
.restart
		move.w	music_c,d0
		lsl.w	#2,d0
		lea	music_m,a0
		move.l	0(a0,d0.w),a0
		init_music
		move.b	#-1,musicflag
		move.w	music_c,d0
		lsl.w	#2,d0
		lea	music_p,a0
		move.l	0(a0,d0.w),a0
		bsr	write_music
.noright	
	endc
		jmp	main


last_menu	dc.w	0
musicflag	dc.b	-1		; 0 = no music
		even
vblank		dc.w	0

routines	dc.l	wait
		dc.l	start_down
		dc.l	t_fade
		dc.l	some_text
		dc.l	set_new_buttons
		dc.l	start_up
		dc.l	t_fade
		dc.l	resetit

		dc.l	0

this_one	dc.w	3

check_key
		moveq	#0,d0
		moveq	#-1,d7
		move.b	key,d0
		move.b	#0,key
		cmp.w	#$4f,d0
		bne.s	.notleft
		move.w	#0,d7		; left arrow
		rts
.notleft	cmp.w	#$4e,d0
		bne.s	.notright
		move.w	#3,d7		; right arrow
		rts
.notright	cmp.w	#$4c,d0
		bne.s	.notup
		move.w	#1,d7		; up arrow
		rts
.notup		cmp.w	#$4d,d0
		bne.s	.notdown
		move.w	#2,d7		; down arrow
		rts
.notdown	cmp.w	#$45,d0
		bne.s	.notesc
		move.w	#5,d7		; escape key
		rts
.notesc		cmp.w	#$40,d0
		bne.s	.notspc
		move.w	#4,d7		; space bar
.notspc		rts


resetit		move.w	#0,this_one
wait		rts			; nothing, do it again

set_new_buttons
; do header text
		lea	text,a0
		bsr	write_header

; clear arrows...
		lea	clips,a0
		lea	bottom+2+32,a1
		move.w	#40-1,d7
.loop
		move.l	(a0),(a1)
		move.l	4(a0),4(a1)
		move.w	8(a0),8(a1)
		move.b	10(a0),10(a1)

		move.l	4000(a0),3200(a1)
		move.l	4004(a0),3204(a1)
		move.w	4008(a0),3208(a1)
		move.b	4010(a0),3210(a1)

		move.l	8000(a0),6400(a1)
		move.l	8004(a0),6404(a1)
		move.w	8008(a0),6408(a1)
		move.b	8010(a0),6410(a1)

		move.l	12000(a0),9600(a1)
		move.l	12004(a0),9604(a1)
		move.w	12008(a0),9608(a1)
		move.b	12010(a0),9610(a1)
		lea	80(a1),a1
		lea	80(a0),a0
		dbf	d7,.loop

; set left button	
		cmp.w	#0,curr_pagenum
		bne.s	.right_button
		lea	clips+(88/8),a0
		lea	bottom+32+(16/8)+(80*13),a1
		move.w	#14-1,d7
.left_loop
		move.b	(a0),(a1)
		move.b	1(a0),1(a1)
		move.b	2(a0),2(a1)
		move.b	3(a0),3(a1)
		move.b	4000(a0),3200(a1)
		move.b	4001(a0),3201(a1)
		move.b	4002(a0),3202(a1)
		move.b	4003(a0),3203(a1)
		move.b	8000(a0),6400(a1)
		move.b	8001(a0),6401(a1)
		move.b	8002(a0),6402(a1)
		move.b	8003(a0),6403(a1)
		move.b	12000(a0),9600(a1)
		move.b	12001(a0),9601(a1)
		move.b	12002(a0),9602(a1)
		move.b	12003(a0),9603(a1)
		lea	80(a0),a0
		lea	80(a1),a1
		dbf	d7,.left_loop

.right_button
		move.w	max_pagenum,d0
		cmp.w	curr_pagenum,d0
		bne.s	.up_button

		lea	clips+(88/8)+(80*14),a0
		lea	bottom+32+(72/8)+(80*13),a1
		move.w	#14-1,d7
.right_loop
		move.b	(a0),(a1)
		move.b	1(a0),1(a1)
		move.b	2(a0),2(a1)
		move.b	3(a0),3(a1)
		move.b	4000(a0),3200(a1)
		move.b	4001(a0),3201(a1)
		move.b	4002(a0),3202(a1)
		move.b	4003(a0),3203(a1)
		move.b	8000(a0),6400(a1)
		move.b	8001(a0),6401(a1)
		move.b	8002(a0),6402(a1)
		move.b	8003(a0),6403(a1)
		move.b	12000(a0),9600(a1)
		move.b	12001(a0),9601(a1)
		move.b	12002(a0),9602(a1)
		move.b	12003(a0),9603(a1)
		lea	80(a0),a0
		lea	80(a1),a1
		dbf	d7,.right_loop


.up_button
		cmp.w	#0,article_num
		bne.s	.down_button

		lea	clips+(120/8)+(80*0),a0
		lea	bottom+32+(40/8)+(80*0),a1
		move.w	#14-1,d7
.up_loop
		move.b	(a0),(a1)
		move.b	1(a0),1(a1)
		move.b	2(a0),2(a1)
		move.b	3(a0),3(a1)
		move.b	4000(a0),3200(a1)
		move.b	4001(a0),3201(a1)
		move.b	4002(a0),3202(a1)
		move.b	4003(a0),3203(a1)
		move.b	8000(a0),6400(a1)
		move.b	8001(a0),6401(a1)
		move.b	8002(a0),6402(a1)
		move.b	8003(a0),6403(a1)
		move.b	12000(a0),9600(a1)
		move.b	12001(a0),9601(a1)
		move.b	12002(a0),9602(a1)
		move.b	12003(a0),9603(a1)
		lea	80(a0),a0
		lea	80(a1),a1
		dbf	d7,.up_loop


.down_button
		cmp.w	#max_art,article_num
		bne.s	.all_done

		lea	clips+(120/8)+(80*14),a0
		lea	bottom+32+(40/8)+(80*26),a1
		move.w	#14-1,d7
.down_loop
		move.b	(a0),(a1)
		move.b	1(a0),1(a1)
		move.b	2(a0),2(a1)
		move.b	3(a0),3(a1)
		move.b	4000(a0),3200(a1)
		move.b	4001(a0),3201(a1)
		move.b	4002(a0),3202(a1)
		move.b	4003(a0),3203(a1)
		move.b	8000(a0),6400(a1)
		move.b	8001(a0),6401(a1)
		move.b	8002(a0),6402(a1)
		move.b	8003(a0),6403(a1)
		move.b	12000(a0),9600(a1)
		move.b	12001(a0),9601(a1)
		move.b	12002(a0),9602(a1)
		move.b	12003(a0),9603(a1)
		lea	80(a0),a0
		lea	80(a1),a1
		dbf	d7,.down_loop

.all_done	addq.w	#1,this_one
		rts

start_down
		move.l	a5,-(a7)
		movem.l	fontpal,d0-d7
		movem.l	d0-d7,source_pal
		movem.l	blank2,d0-d7
		movem.l	d0-d7,dest_pal
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#16,d0
		move.w	#1,d1
		bsr	trigfadeto
		move.l	(a7)+,a5
		addq.w	#1,this_one

t_fade		move.l	a5,-(a7)
		bsr	fade_to
		move.b	fadetoflag(a5),d0
		move.l	(a7)+,a5
		lea	source_pal,a0
		lea	textcol+2,a1
		move.w	#16-1,d7
.loop		move.w	(a0)+,(a1)+
		addq.w	#2,a1
		dbf	d7,.loop
		tst.b	d0
		bne.s	.exit
		addq.w	#1,this_one
.exit		rts		


checkit
		moveq	#-1,d7		; checkit
		move.w	mouse_x,d0
		move.w	mouse_y,d1
		cmp.w	#10,d0
		bge.s	.ok_button
		rts
.ok_button	cmp.w	#23,d0
		bge.s	.notleft
		cmp.w	#232+13-5,d1
		bge.s	.maybeleft
.go		rts
.maybeleft	cmp.w	#232+26-5,d1
		bgt.s	.go
		move.w	#0,d7		; left=0
		rts
.notleft	cmp.w	#36,d0
		bge.s	.rightother
		cmp.w	#232+13-5,d1
		bgt.s	.down
		move.w	#1,d7		; up=1
		rts
.down		cmp.w	#232+25-5,d1
		bge.s	.downbutton
		rts
.downbutton	move.w	#2,d7		; down=2
		rts
.rightother	cmp.w	#48,d0
		bgt.s	.other
		cmp.w	#232+13-5,d1
		bge.s	.mayberight
		rts
.mayberight	cmp.w	#232+26-5,d1
		ble.s	.rightbutton
		rts
.rightbutton	move.w	#3,d7		; right=3
		rts
.other		cmp.w	#94,d0
		bge.s	.quit
		move.w	#4,d7		; menu=4
		rts
.quit		cmp.w	#131,d0
		bgt.s	.quitbutton
		move.w	#5,d7
		rts
.quitbutton	cmp.w	#167,d0
		bgt.s	.musicbutton
		move.w	#6,d7
.musicbutton	move.w	#7,d0
		rts


start_up
		move.l	a5,-(a7)
		movem.l	fontpal,d0-d7
		movem.l	d0-d7,dest_pal
		movem.l	blank2,d0-d7
		movem.l	d0-d7,source_pal
		lea	source_pal,a0
		lea	dest_pal,a1
		move.w	#16,d0
		move.w	#1,d1
		bsr	trigfadeto
		move.l	(a7)+,a5
		addq.w	#1,this_one
		jmp	t_fade


do_highlight
		moveq	#-1,d6			; art num in d6
		cmp.w	#0,this_one
		bne.s	.quiit
		cmp.w	#0,article_num
		beq.s	.high
.quiit		rts				; not if not menu
.high		cmp.w	#232,mouse_y
		bge.s	.quiit

		bsr	clear_last

		move.l	highlight1,a0
		move.w	mouse_y,d1
		move.w	d1,old_mouse
		moveq	#0,d3
		cmp.w	#160,mouse_x
		blt.s	.leftside
		add.w	#40,a0
		add.w	#24,d3
.leftside	cmp.w	#0,curr_pagenum
		beq.s	.firstpage
		add.w	#48,d3		; page 2 of menu...
		cmp.w	#1,curr_pagenum
		beq.s	.firstpage	; already at second page
		add.w	#48,d3		; third page :-)
.firstpage
		sub.w	#32,d1		; correct for top panel
		lsr.w	#3,d1		; steps of eight lines ...
		add.w	d1,d3
		lsl.w	#2,d3
		lea	art_menu,a4
		move.w	0(a4,d3.w),d4
		tst.w	d4
		bne.s	.doit		; no article, no draw!
		rts
; highlight stuff
.doit		lsl	#3,d1
		mulu	#80,d1
		add.w	d1,a0
x1		set	0
		rept	8
		move.l	#-1,x1(a0)
		move.l	#-1,x1+4(a0)
		move.l	#-1,x1+8(a0)
		move.l	#-1,x1+12(a0)
		move.l	#-1,x1+16(a0)
		move.l	#-1,x1+20(a0)
		move.l	#-1,x1+24(a0)
		move.l	#-1,x1+28(a0)
		move.l	#-1,x1+32(a0)
		move.l	#-1,x1+36(a0)
x1		set	x1+80
		endr
		move.w	2(a4,d3.w),d6	; return article in d3
		rts

old_mouse	dc.w	0
clear_last
		move.l	highlight1,a0
		move.w	old_mouse,d1
		sub.w	#32,d1		; correction for top panel
		lsr	#3,d1
		lsl	#3,d1
		mulu	#80,d1
		add.w	d1,a0
x2 		set 	0
		rept	8
		move.l	#0,x2(a0)
		move.l	#0,x2+4(a0)
		move.l	#0,x2+8(a0)
		move.l	#0,x2+12(a0)
		move.l	#0,x2+16(a0)
		move.l	#0,x2+20(a0)
		move.l	#0,x2+24(a0)
		move.l	#0,x2+28(a0)
		move.l	#0,x2+32(a0)
		move.l	#0,x2+36(a0)
		move.l	#0,x2+40(a0)
		move.l	#0,x2+44(a0)
		move.l	#0,x2+48(a0)
		move.l	#0,x2+52(a0)
		move.l	#0,x2+56(a0)
		move.l	#0,x2+60(a0)
		move.l	#0,x2+64(a0)
		move.l	#0,x2+68(a0)
		move.l	#0,x2+72(a0)
		move.l	#0,x2+76(a0)
x2		set	x2+80
		endr
		rts

update_mouse
		bsr.s	get_new_xy
		lea	pointer,a0
		move.w	mouse_x,d0
		move.w	mouse_y,d1
		move.w	#16,d5
		bsr	makesprite
		rts

get_new_xy	moveq	#0,d0
		move.w	joy0dat(a5),d0
		move.w	d0,d1		; for x movement
		and.w	#$ff00,d0
		and.w	#$ff,d1
		lsr.w	#8,d0		; vert move only
		move.w	mouse_yo,d2
		move.w	d0,mouse_yo
		sub.w	d2,d0
		move.w	mouse_y,d2
		ext.w	d0
		add.w	d0,d2		; new Y
		
		move.w	d2,mouse_y
		
		move.w	mouse_xo,d2
		move.w	d1,mouse_xo
		sub.w	d2,d1
		move.w	mouse_x,d2
		ext.w	d1
		add.w	d1,d2
		move.w	d2,mouse_x	; new X

		move.w	mouse_y,d2

		cmp.w	min_y,d2
		bge.s	.ok1
		move.w	min_y,mouse_y
.ok1		tst.w	mouse_x
		bge.s	.ok2
		move.w	#0,mouse_x
.ok2		cmp.w	#270,mouse_y
		ble.s	.ok3
		move.w	#270,mouse_y
.ok3		cmp.w	#319-16,mouse_x
		ble.s	.ok4
		move.w	#319-16,mouse_x
.ok4		rts

; page stuff!!!
PageIndex		
.waitlmb	btst	#6,$bfe001		; lmb ?
		beq.s	.waitlmb
		cmp.w	#0,article_num
		beq.s	.exitit
		move.w	#0,article_num
		move.w	last_menu,curr_pagenum
		bsr	set_first
		move.w	#1,this_one
		move.w	#45+24,min_y		; new minimum
.exitit		movem.l	d0-d7,-(a7)
		movem.l	basepal,d0-d7
		movem.l	d0-d7,fontpal
		movem.l	(a7)+,d0-d7
		jmp	main

PageRight	
.waitlmb	btst	#6,$bfe001		; lmb ?
		beq.s	.waitlmb
; draw eyes
;		lea	clips+(176/8),a0
;		lea	bottom+32+(48/8)+(80*14),a1
;		bsr	draw_eye

		move.w	curr_pagenum,d0
		move.w	max_pagenum,d1
		cmp.w	d0,d1
		beq.s	.exit
		addq.w	#1,d0
		move.w	d0,curr_pagenum
		lea	page_offsets,a0
		move.l	start_p,a1
		lsl.w	#1,d0
		add.w	0(a0,d0.w),a1
		move.l	a1,textp
		move.w	#1,this_one		
		moveq	#0,d3
		move.w	#45+24,min_y
		tst.w	article_num
		beq.s	.exit
		move.w	#232-5,min_y
.exit		jmp	main

PageLeft	
.waitlmb	btst	#6,$bfe001		; lmb ?
		beq.s	.waitlmb
		cmp.w	#0,curr_pagenum
		beq.s	.exit

; draw eyes
;		lea	clips+(200/8),a0
;		lea	bottom+32+(48/8)+(80*14),a1
;		bsr	draw_eye

		subq.w	#1,curr_pagenum
		move.w	curr_pagenum,d0
		lea	page_offsets,a0
		move.l	start_p,a1
		lsl.w	#1,d0
		add.w	0(a0,d0.w),a1
		move.l	a1,textp
		move.w	#1,this_one
		tst.w	article_num
		beq.s	.exit
		move.w	#232-5,min_y
.exit		jmp	main

PageDown	
.waitlmb	btst	#6,$bfe001		; lmb ?
		beq.s	.waitlmb

; draw eyes
;		lea	clips+(224/8),a0
;		lea	bottom+32+(48/8)+(80*14),a1
;		bsr	draw_eye

		bsr	clear_last
		cmp.w	#max_art,article_num
		beq.s	.exit
		addq.w	#1,article_num
		move.w	#0,curr_pagenum
		bsr	set_first
		move.w	#1,this_one
		move.w	#232-5,min_y
.exit		jmp	main
PageUp		
.waitlmb	btst	#6,$bfe001		; lmb ?
		beq.s	.waitlmb
		cmp.w	#0,article_num
		beq.s	.exit

; draw eyes
;		lea	clips+(248/8),a0
;		lea	bottom+32+(48/8)+(80*14),a1
;		bsr.s	draw_eye

		subq.w	#1,article_num
		move.w	#0,curr_pagenum
		bsr	set_first
		move.w	#1,this_one
		cmp.w	#0,article_num		; index?
		bne.s	.exit
		move.w	#45+24,min_y		; index, so new min_y
		move.w	last_menu,curr_pagenum	; and last menu page viewed
		bra.s	.exit2
.exit		move.w	#232-5,min_y
.exit2		jmp	main

;draw_eye
;		move.w	#10-1,d0
;.dloop
;		move.b	(a0),(a1)
;		move.b	1(a0),1(a1)
;		move.b	2(a0),2(a1)
;		move.b	4000(a0),3200(a1)
;		move.b	4001(a0),3201(a1)
;		move.b	4002(a0),3202(a1)
;		move.b	8000(a0),6400(a1)
;		move.b	8001(a0),6401(a1)
;		move.b	8002(a0),6402(a1)
;		move.b	12000(a0),9600(a1)
;		move.b	12001(a0),9601(a1)
;		move.b	12002(a0),9602(a1)
;		lea	80(a0),a0
;		lea	80(a1),a1
;		dbf	d0,.dloop
;		rts


get_key
		tst.w	this_one
		bne.s	.noke		; not wait state
		move.b	$bfed01,d0
		btst	#3,d0
		beq.s	.noke
		move.b	$bfec01,d0
		bset	#6,$bfee01
		moveq	#2,d2
.lop2		move.b	$dff006,d1
.lop1		move.b	#$ff,$bfec01
		cmp.b	$dff006,d1
		beq.s	.lop1
		dbf	d2,.lop2

		bclr	#6,$bfee01
		tst.b	d0
		beq.s	.noke
		ror.b	d0
		not.b	d0
		move.b	d0,key
.noke
		rts

key	dc.b	0,0

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
		tst.b	musicflag
		beq.b	.skipmusic
	;ifeq	music_on
		play_music
	;endif
	;ifne	music_on
	;	nop
	;endif
.skipmusic
		tst.b	intro
		beq.b	.skipit
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
		bgt.w 	.nofadeto
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
		moveq 	#$00f,d6		; B mask
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

ice_decrunch	
; swap source pointers from Ice_decrunch to stc_decrunch
		move.l	a1,a2
		move.l	a0,a1

;-----------------------------------------------------------------------------
;S404 highly optimized data_decruncher_lib 1.0_turbo for use in stc.library
;23.11.93 by Marcus 'Cozine' Ottosson
;
; This is a highly optimized library decruncher.  The code does not fit into
; the instruction cache (286 bytes).  Generally, it's NOT possible to use all
; the 256 bytes available in the instruction cache.  The caches in the 68020
; and the 68030 consist of 16 rows, each of which contains 16 bytes.
; AllocMem() and AllocPooled(), the memory allocation routines used for
; loading executables, guarantee 8-byte alignment.
; On the 68000 my large testfile decrunches 3.7% faster in this version than
; the standard version.  Some files may decrunch much slower on the 68020 and
; the 68030.  I don't know the maximum loss, probably about 10-20%.  The loss
; will not be very high when we are multitasking, since the interrupts cause a
; partial or complete cache flush anyway.  I want some some test reports!!!
;
;-----------------------------------------------------------------------------
;S404 data_decruncher v0.2
;(c) 1993 by Jouni 'Mr.Spiv' Korhonen (SWSW)
;-----------------------------------------------------------------------------
;call with registers: a2 = destination address
;                     a1 = crunched data
;-----------------------------------------------------------------------------

;Uses d0-d7/a0-a6

stc_decrunch
decrunch:
l0:		addq	#8,a1
edDCopyableStart:
		move.l	a2,a5
		add.l	(a1)+,a2
		add.l	(a1),a1
edDCopyableStart2:
		moveq	#0,d4
		moveq	#16,d5
		movem	(a1),d2/d6/d7
		not	d4
		lea	loff6(pc),a3
		lea	llen5a(pc),a4
		moveq	#1,d0
		moveq	#-1,d3
		bra.s	ltest1

		cnop	0,8			; Use if main loop>=244 bytes

;*** Here's the start of the instruction cache

lins:		subq	#8,d7
		bpl.s	lins2
lins1:		move	d7,d1
		addq	#8,d7
		lsl.l	d7,d6
		move	-(a1),d6
		neg	d1
		lsl.l	d1,d6
		addq	#8,d7
		swap	d6
		move.b	d6,-(a2)
		swap	d6
		cmp.l	a2,a5
		dbhs	d7,lmain
		bra.s	lexma

lins2:		rol	#8,d6
		move.b	d6,-(a2)
ltest1:		cmp.l	a2,a5
		dbhs	d7,lmain
lexma		bhs.s	lexit

lmain1:		move	-(a1),d6
		moveq	#15,d7
lmain:		add	d6,d6
		bcc.s	lins

		dbf	d7,llen1
		move	-(a1),d6
		moveq	#15,d7
llen1:		add	d6,d6
		bcs.s	llen6
		moveq	#2,d1
		moveq	#4-2,d3
		dbf	d7,llen2
		move	-(a1),d6
		moveq	#15,d7
llen2:		add	d6,d6
		bcs.s	llen5
		dbf	d7,llen3
		move	-(a1),d6
		moveq	#15,d7
llen3:		add	d6,d6
		bcc.s	llen4
		moveq	#4,d1
		moveq	#8-2,d3
		lea	llen3a(pc),a6
		bra.s	lbits
llen3a:		add	d1,d3
		cmp	#15,d1
		blo.s	loff1

		moveq	#5,d1
		moveq	#14-1,d3
		lea	llen3b(pc),a6
		bra.s	lbits

llen4:		moveq	#23-2,d3
lloop:		moveq	#8,d1
llen5:		move.l	a4,a6
		bra.s	lbits
llen5a:		add	d1,d3
		not.b	d1
		dbeq	d7,loff2
		bne.s	loff2a
		bra.s	lloop

loff6:		add	d1,a0
		move.b	(a0),-(a2)
lcopy:		move.b	-(a0),-(a2)
		dbf	d3,lcopy
ltest:		cmp.l	a2,a5
		dbhs	d7,lmain
		blo.s	lmain1
lexit:		rts

llen6:		dbf	d7,llen7
		move	-(a1),d6
		moveq	#15,d7
llen7:		add	d6,d6
		addx	d0,d3
loff1:		dbf	d7,loff2
loff2a:		move	-(a1),d6
		moveq	#15,d7
loff2:		add	d6,d6
		bcs.s	loff3

		dbf	d7,loff4
		move	-(a1),d6
		moveq	#15,d7
loff4:		moveq	#9,d1
		lea	32(a2),a0
		add	d6,d6
		bcc.s	loff5
		moveq	#5,d1
		move.l	a2,a0
		bra.s	loff5
loff3:		lea	544(a2),a0
		move	d2,d1
loff5:		move.l	a3,a6

lbits:		and.l	d4,d6
		sub	d1,d7
		bpl.s	lbits2
		add	d7,d1
		lsl.l	d1,d6
		move	d7,d1
		move	-(a1),d6
		neg	d1
		add	d5,d7
lbits2:		lsl.l	d1,d6
		move.l	d6,d1
		swap	d1
		jmp	(a6)

; This part is not executed very often.  Some files may decrunch much slower
; on the 68020/68030.

llen3b:		add	d1,d3
l2ins:		subq	#8,d7
		bmi.s	l2ins1
		rol	#8,d6
		move.b	d6,-(a2)
		dbf	d3,l2ins
		bra.s	ltest

l2ins1:		move	d7,d1
		addq	#8,d7
		lsl.l	d7,d6
		move	-(a1),d6
		neg	d1
		lsl.l	d1,d6
		addq	#8,d7
		swap	d6
		move.b	d6,-(a2)
		swap	d6
		dbf	d3,l2ins
		bra	ltest

*******************************************************************************
* MakeSprite
* 
* Makes correct CTL/POS words for each sprite
*******************************************************************************
makesprite	add.w	#xstrt,d0	; correction for screen
		add.w	#ystrt+6,d1	; correction for screen
		moveq	#0,d3
		moveq	#0,d4
		move.w	d0,d2
		roxr.w	#1,d4
		roxr.w	#1,d2
		bcc.b	.nohihsat
		roxr.w	#1,d4
		bset	#0,d3
.nohihsat	move.w	d1,-(a7)
		ror.w	#8,d1
		roxr.b	#1,d1
		bcc.b	.nohivstart
		bset	#2,d3
		roxr.w	#1,d4
.nohivstart	or.w	d1,d2
		move.w	(a7)+,d1
		add.w	d5,d1	;height
		ror.w	#8,d1
		roxr.b	#1,d1
		bcc.b	.nohivstop
		roxr.w	#1,d4
		bset	#1,d3
.nohivstop	or.w	d1,d3
		move.w	d2,(a0)+
		move.w	d3,(a0)+
		rts

******************************************************************************
* Some_Text
*
* The text writer for the mag
******************************************************************************
some_text
		move.l	a0,-(a7)
		bsr	clear_screen
		move.l	(a7)+,a0
		move.w	#0,xx
		move.w	#0,yy
		bsr.s	text_writer
		addq.w	#1,this_one
		bsr	write_pagenum
		rts

text_writer
		move.l	textp,a0	; next characters
		tst.b	(a0)		; is zero?
		bne.s	.not_zero	; nope
		add.w	#screen_width*f_h,yy	; next line
		move.w	#0,xx		; start of it
		move.b	(a0)+,d0
.not_zero
		cmp.b	#-1,(a0)
		bne.s	.not_reset
		rts
.not_reset
		cmp.b	#1,(a0)
		bne.s	.not_colour
		moveq	#0,d0
		move.b	(a0)+,d0
		move.b	(a0)+,d0
		move.w	d0,curr_col	; set new colour
		move.l	a0,textp
		bra.w	text_writer
.not_colour		
		cmp.b	#2,(a0)
		bne.s	.notpicture
		moveq	#0,d0
		move.b	(a0)+,d0
		move.b	(a0)+,d0
		move.l	a0,textp

;		bra.w	text_writer	; remove this for full

; ignore for testing
		subq.w	#1,d0
		lsl.w	#2,d0
		lea	pix,a0
		move.l	0(a0,d0.w),a0	; this pic
		lea	pic_pal2,a1
		move.l	a5,-(a7)
		bsr	ice_decrunch	; decrunch pic to text screen
		move.l	(a7)+,a5
		lea	pic_pal2,a0
		lea	fontpal,a1
		movem.l	(a0),d0-d7
		movem.l	d0-d7,(a1)	; set correct pic palette
		lea	basepal,a0
		movem.l	(a0),d0-d1
		movem.l	d0-d1,(a1)	; set correct font palette

		bra.w	text_writer	; do text now...
.notpicture
		cmp.b	#3,(a0)
		bne.s	.not_font
		moveq	#0,d0
		move.b	(a0)+,d0	; skip signifier	
		move.b	(a0)+,d0	; get font number
		move.w	d0,curr_font
		move.l	a0,textp
		bra.w	text_writer
.not_font
		moveq	#0,d0
		move.b	(a0)+,d0	; next letter
		move.l	a0,textp	; save it
		sub.w	#" ",d0		; into table
		lsl.w	#1,d0		; offset
		lea	text_lookup,a0
		move.w	0(a0,d0.w),d0	; this offset
		lea	text_screen,a0	; this screen	was +80
		add.w	xx,a0		; this far in
		add.w	yy,a0		; this far down
		lea	fonts,a1	; font
		move.w	curr_font,d1
;		bne.s	.not_micro
;		add.w	#80,d0
;.not_micro
		lsl.w	#2,d1
		add.w	d1,a1
		move.l	(a1),a1		; this font
		add.w	d0,a1		; font letter

; draw the text in colour
		move.l	a0,-(a7)
		move.w	curr_col,d0
		lsl.w	#2,d0
		move.l	colours(pc,d0.w),a3
		jsr	(a3)
		move.l	(a7)+,a0

		addq.w	#1,xx
		bra.w	text_writer


colours		dc.l	colour0
		dc.l	colour1
		dc.l	colour2
		dc.l	colour3

colour0		
		rts

colour1
		bsr.s	textit
		rts

colour2
		lea	screen_bitplane(a0),a0
		bsr.s	textit
		rts

colour3		rept	2
		bsr.s	textit
		lea	screen_bitplane(a0),a0
		endr
		rts
		
textit
		move.b	d0,-(a7)
y1		set	0
		rept	f_h
		move.b	y1(a1),d0
		or.b	d0,y1(a0)
y1		set	y1+80
		endr
		move.b	(a7)+,d0
		rts

	ifeq	music_on
use		=	$9c38&$9d05
playback_speed	=	3
		include	music/types.i
		include	music/player6.i
		incdir 	work:cdn/defy5/music/
		include	player.s
		incdir	work:cdn/defy5/
	endc

		
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
x		set	0
		rept	screen_height
		dc.w	x
x		set	x+screen_nextlineoffset
		endr


page_offsets	dc.l	0,0,0,0,0,0,0,0,0


mouse_x		dc.w	0
mouse_y		dc.w	221
mouse_xo	dc.w	0
mouse_yo	dc.w	221

textp	dc.l	text

	dc.w	0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dc.w	0,0,0,0,0,0,0,0,0,0,0,0,0,0

text
defylogo	incbin	gfx/defy_5_title.raw.stc	;gfx/defy4-title.raw.stc
	dcb.b	5000
			
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
y		set	y+f_h*f_w
x		set	0

x		set	0
		rept	79
		dc.w	x+y
x		set	x+1
		endr
y		set	y+f_h*f_w
x		set	0


min_y		dc.w	232-5

*******************************************************************************

; colour text stuff

curr_col	dc.w	1

; stuff for storing pages!!!!
line_counter	dc.w	0
xx		dc.w	0
yy		dc.w	0
max_pagenum	dc.w	0
curr_pagenum	dc.w	0
article_num	dc.w	1
start_p		dc.l	0
articles
		dc.l	aa,aa1,aa2,aa3,aa4
		dc.l	aa5,aa6,aa7,a8,a9
		dc.l	a10,a11,a12,a13,a14,a15,a16,a17,a18
		dc.l	a19,a20,a21,a22,a23,a24,a25,a26,a27
		dc.l	a28
		dc.l	a29,a30,a31,a32,a32a,a33,a34
		dc.l	a35,a36,a37,a38,a39,a39a,a40
		dc.l	a41,a42,a43,a44,a45,a46
		dc.l	0,0

		incdir	work:cdn/defy5/art2/

; put path names, 0 terminated, instead of incbins...
		
aa		incbin	menu.stc
		even
aa1		incbin	editorial.stc
		even
aa2		incbin	disclaimer.stc
		even	; start FUN here
aa3		incbin	contributors.stc
		even
aa4		incbin	accolytes101tips.stc
		even
aa5		incbin	amigan.stc
		even
aa6		incbin	amivpc.stc
		even
aa7		incbin	ashestoashes.stc
		even
a8		incbin	designbacklash.stc
		even
a9		incbin	international.stc
		even
a10		incbin	kimbaspeaksout.stc
		even
a11		incbin	myold_a500.stc
		even
a12		incbin	cougar_pic.stc
		even
a13		incbin	defy5-ozscene.stc
		even
a14		incbin	pixelbypixel.stc
		even
a15		incbin	wind4defy.stc
		even
a16		incbin	zipsanewleaseonlife.stc
		even	
a17		incbin	spanishreport2.stc
		even
a18		incbin	theparty-5.results.stc
		even
a19		incbin	dskreportdefy.stc
		even
a20		incbin	da-party.stc	
		even
a21		incbin	defy4.stc
		even
a22		incbin	basecom-interview.stc
		even
a23		incbin	dhryston_interview.stc
		even
a24		incbin	jagspeed.stc
		even
a25		incbin	defy5-gothcolumn.stc
		even
a26		incbin	ftp-faq.stc
		even
a27		incbin	ftp-faq2.stc
		even
a28		incbin	rave-faq.stc
		even
a29		incbin	rave-faq2.stc
		even
a30		incbin	rave-faq3.stc
		even
a31		incbin	game_intros.stc
		even
a32		incbin	homemade.stc
		even
a32a		incbin	placname.stc
		even
a33		incbin	stars_cocks1.stc
		even
a34		incbin	telemall.stc
		even	
a35		incbin	whynostradamusiscrap.stc
		even	
a36		incbin	cd-review.stc
		even	
a37		incbin	mortal.stc
		even	
a38		incbin	summersault.stc
		even	
a39		incbin	oasis.stc
		even	
a39a		incbin	our_amy.stc
		even
a40		incbin	ads.stc
		even	
a41		incbin	bbsads.stc
		even	
a42		incbin	scenerse-mail.stc
		even	
a43		incbin	scenerse-mail2.stc
		even	
a44		incbin	news.stc
		even	
a45		incbin	theend.stc
		even	
a46		incbin	charts.stc
		even
; put charts in here as well!

max_art		equ	48

		incdir	work:cdn/defy5/
art_menu
; page 1left side
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	1,1
		dc.w	1,2
		dc.w	1,3
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	1,4
		dc.w	1,5
		dc.w	1,6
		dc.w	1,7
		dc.w	1,8
		dc.w	1,9
		dc.w	1,10
		dc.w	1,11
		dc.w	1,12

; right side
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	1,13
		dc.w	1,14
		dc.w	1,15
		dc.w	1,16
		dc.w	1,17
		dc.w	1,18
		dc.w	1,19
		dc.w	1,20
		dc.w	1,21
		dc.w	1,22
		dc.w	1,23
		dc.w	1,24
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	1,25
		dc.w	1,26
		dc.w	1,28
		dc.w	1,31

; page 2left side
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	1,32
		dc.w	1,33
		dc.w	1,34
		dc.w	1,35
		dc.w	1,36
		dc.w	1,37
		dc.w	1,38
		dc.w	1,39
		dc.w	1,40
		dc.w	1,41
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	1,42
		dc.w	1,43
		dc.w	1,44
		dc.w	1,46
		dc.w	0,0
		dc.w	0,0


; right side
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	1,47
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	1,48
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0

; page 3 left side
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0

; right side
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0

; end of menu lookup

		dc.l	0,0,0,0,0,0,0,0
		even
		

curr_font	dc.w	0
fonts		dc.l	font1,font2,font1,font2

pix		dc.l	pic1,pic2,pic3,pic4,pic5,pic6,pic7,pic8,pic9
		dc.l	pic10,pic11,pic12,pic13,pic14,pic15,pic16
		dc.l	0


; put path names, 0 terminated, instead of incbins

		incdir	work:cdn/defy5/clips/
pic1		incbin	3atomicfate.clip2.raw.stc
		even
pic2		incbin	6rage.clip2.raw.stc
		even
pic3		incbin	d-face.clip2.raw.stc
		even
pic4		incbin	sunset2.clip2.raw.stc
		even
pic5		incbin	teethman.clip2.raw.stc
		even
pic6		incbin	defyadvertceltic.raw.stc
		even
pic7		incbin	pd-clipart.raw.stc
		even
pic8		incbin	phobia_bbsad.raw.stc
		even
pic9		incbin	defy2000.clip.raw.stc
		even
pic10		incbin	defygirl.clip.raw.stc
		even
pic11		incbin	giveme5.clip.raw.stc
		even
pic12		incbin	sexydefy.clip.raw.stc
		even
pic13		incbin	stargirl2.clip.raw.stc
		even
pic14		incbin	mainmenu.clip.raw.stc
		even
pic15		incbin	menu1.raw.stc
		even
pic16		incbin	menu2.raw.stc
		even

fontpal		dc.w	$001,$aac,$484,$66a,$ccc,$bbb,$aaa,$999
hc2		dc.w	$888,$777,$666,$555,$444,$333,$222,$111
basepal		dc.w	$001,$aac,$484,$66a,$ccc,$bbb,$aaa,$999
		dc.w	$030,$777,$666,$555,$444,$333,$222,$111


	
		incdir	work:cdn/defy5/
clips		incbin	gfx/clips3.raw
		even

highlight1	dc.l	pl1

font1		incbin	gfx/mknight2.font
		even
font2		incbin	fonts/souri.font
		even

*******************************************************************************
************************ chip data.

		section	chipdata,data_c
		even
top		incbin	gfx/top2.panel
		even
bottom		incbin	gfx/bottom3.panel
		even


nullspr		rept	40
		dc.l	0
		endr

pointer		dc.l	0
; new pointer :-)

		dc.w	%1110100000000000,%1111000000000000
		dc.w	%1111111010000000,%1111111100000000
		dc.w	%1111111110100000,%1111111111000000		
		dc.w	%0111111101000000,%1111111110000000		
		dc.w	%1111111010000000,%0111111100000000		
		dc.w	%0111111100000000,%0111111000000000		
		dc.w	%0111111000000000,%0111111100000000
		dc.w	%0011010100000000,%0111101100000000		
		dc.w	%0110100101000000,%0011000010000000		
		dc.w	%0001000011000000,%0010000000000000		
		dc.w	%0010000000000000,%0000000000000000		
		dc.w	%0000000000000000,%0000000000000000
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0
		dc.w	0,0


copper:		dc.w	-1,-2

a_copper	
		dc.w	spr0pth,0,spr0ptl,0
		dc.w	spr1pth,0,spr1ptl,0
		dc.w	spr2pth,0,spr2ptl,0
		dc.w	spr3pth,0,spr3ptl,0
		dc.w	spr4pth,0,spr4ptl,0
		dc.w	spr5pth,0,spr5ptl,0
		dc.w	spr6pth,0,spr6ptl,0
		dc.w	spr7pth,0,spr7ptl,0
a_c		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	-1,-2
a_copper2	
		dc.w	spr0pth,0,spr0ptl,0
		dc.w	spr1pth,0,spr1ptl,0
		dc.w	spr2pth,0,spr2ptl,0
		dc.w	spr3pth,0,spr3ptl,0
		dc.w	spr4pth,0,spr4ptl,0
		dc.w	spr5pth,0,spr5ptl,0
		dc.w	spr6pth,0,spr6ptl,0
		dc.w	spr7pth,0,spr7ptl,0
a_c2		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	bpl5pth,0,bpl5ptl,0
		dc.w	-1,-2

screen_copper	
		dc.w	spr0pth,0,spr0ptl,0
		dc.w	spr1pth,0,spr1ptl,0
		dc.w	spr2pth,0,spr2ptl,0
		dc.w	spr3pth,0,spr3ptl,0
		dc.w	spr4pth,0,spr4ptl,0
		dc.w	spr5pth,0,spr5ptl,0
		dc.w	spr6pth,0,spr6ptl,0
		dc.w	spr7pth,0,spr7ptl,0
		dc.w	bplcon0,color
		dc.w	color00,0,color01,0,color02,0,color03,0,color04,0
		dc.w	color05,0,color06,0,color07,0
		dc.w	$2509,$fffe
topbpl		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	bplcon0,hires+4<<12+color
topcol		dc.w	color00,0,color01,0,color02,0,color03,0,color04,0
		dc.w	color05,0,color06,0,color07,0
		dc.w	color08,0,color09,0,color10,0,color11,0
		dc.w	color12,0,color13,0,color14,0,color15,0
		dc.w	$4109,$fffe
		dc.w	color00,0,color01,0,color02,0,color03,0,color04,0
		dc.w	color05,0,color06,0,color07,0
		dc.w	color08,0,color09,0,color10,0,color11,0
		dc.w	color12,0,color13,0,color14,0,color15,0
		dc.w	$4209,$fffe
textbpl		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
highbpl		dc.w	bpl4pth,0,bpl4ptl,0
textcol		dc.w	color00,1,color01,1,color02,1,color03,1,color04,1
		dc.w	color05,1,color06,1,color07,1
highcol		dc.w	color08,$001,color09,$001,color10,$001
		dc.w	color11,$001,color12,$001,color13,$001
		dc.w	color14,$001,color15,$001
		dc.w 	$ff09,$fffe,$ffdd,$fffe	
		dc.w	$0309,$fffe
		dc.w	color00,0,color01,0,color02,0,color03,0,color04,0
		dc.w	color05,0,color06,0,color07,0
		dc.w	color08,0,color09,0,color10,0,color11,0
		dc.w	color12,0,color13,0,color14,0,color15,0
		dc.w	$0409,$fffe
botcol		dc.w	color00,0,color01,0,color02,0,color03,0,color04,0
		dc.w	color05,0,color06,0,color07,0
		dc.w	color08,0,color09,$0,color10,$0
		dc.w	color11,$0,color12,$0,color13,$0
		dc.w	color14,$0,color15,$0
botbpl		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	bpl4pth,0,bpl4ptl,0
		dc.w	color16,$000,color17,$444,color18,$888
		dc.w	color19,$ccc,color20,$444,color21,$444
		dc.w	color22,$444,color23,$444,color24,$444
		dc.w	color25,$444,color26,$444,color27,$444
		dc.w	color28,$444,color29,$444,color30,$444
		dc.w	color31,$444
		dc.w	$2c09,$fffe
		dc.w	bplcon0,0+color
		dc.w	-1,-2

	ifeq	music_on

		incdir	work:cdn/defy5/music/
mod1		incbin	p60.nk-cro-myfriend
		even
mod2		incbin	p60.neverlikeddean
	endc
		even


************************
		section bss,bss_c
		even
pic_pal		ds.b	32
pic_pal2	ds.b	32

text_screen
		ds.b	screen_bitplane*3	; 3 planes for text!
pl1		ds.b	screen_bitplane		; for highlights
		ds.b	20000			; to fill in rest on logopic

*******************************************************************************

		end

