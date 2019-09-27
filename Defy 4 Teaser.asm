	
		incdir	hd1:cdn/defy4teaser/
		include	includes/custom2.i

; demo equates
lines_high	equ	274
chars_wide	equ	40
font_high	equ	12
f_w		equ	80
f_h		equ	8
f_d		equ	1
music_on	equ	0

fade_workbench	equ	0
screen_res	equ	0		; 0 = lowres, 1 = hires.
screen_width    equ     40
screen_height   equ     274
screen_depth    equ     4
screen_bitplane equ     screen_width*screen_height
screen_size     equ     screen_depth*screen_bitplane
screen_interleaved	equ	1	; 0 = yes, 1 = no.

	IFEQ	screen_interleaved
screen_bitplaneoffset	equ	screen_width
screen_nextlineoffset	equ	screen_width*screen_depth
	ELSE
screen_bitplaneoffset	equ	screen_bitplane
screen_nextlineoffset	equ	screen_width
	ENDC		

xmin            equ     0
xmax            equ     (screen_width*8)-1
ymin            equ     0
ymax            equ     screen_height-1

	ifeq	music_on
;		incdir	hd1:cdn/project95intro/
		include	music/maccros.i
	endif

xstrt		equ	$081		; normal = $081
ystrt		equ	$01c		; normal = $02c
xstop		equ	$1c1		; normal = $1c1
ystop		equ	$126+12		; normal = $12c

*******************************************************************************

startup:	bsr.b	take_system
		tst.l	d0
		beq.s	.error
		bsr.w	initiations
		bsr.w	intro_bit
		bsr.w	main
	ifeq	music_on
		stop_music
	endif

		bsr.w	free_system
		moveq	#0,d0
.error		rts

		include	includes/takefree_system.i

*******************************************************************************

initiations:	lea	custom,a5
		lea	data_segment,a6

.wait		move.l  vposr(a5),d0		; wait for vertical blank.
		and.l   #$1ff00,d0
		bne.s	.wait
		
		move.l  #copper,cop1lch(a5)
		move.w  #set+dmaen+blten+bplen+copen,dmacon(a5)


		move.w	#$7fff,intena(a5)	; install blitter q interrupt.
		move.l	#music_int,$6c
		move.w	#set+inten+vertb,intena(a5)


		movem.l	blank,d0-d7
		movem.l	d0-d7,color00(a5)
		movem.l	d0-d7,color16(a5)

		lea	custom,a5
		move.w	#$022<<8+$81,diwstrt(a5)
		move.w	#($136-$100)<<8+($1c1-$100),diwstop(a5)
		move.w	#4<<12+color,bplcon0(a5)
		move.l	#screen1,bpl4pth(a5)	; set for clear...
		move.w	#($81-17)/2,ddfstrt(a5)
		move.w	#($81-17)/2+8*(($1c1-$81)/16-1),ddfstop(a5)
		move.w	#0,bpl1mod(a5)
		move.w	#0,bpl2mod(a5)


	ifeq	music_on
		lea	mod,a0
		init_music
	endif

set_defy_title
		move.l	#copper,cop1lch(a5)
		move.w	#$28<<8+xstrt,diwstrt(a5)
		move.w	#($128-$100)<<8+(xstop-$100),diwstop(a5)
		move.w	#3<<12+color,bplcon0(a5)
		move.w	#(xstrt-17)/2,ddfstrt(a5)
		move.w	#(xstrt-17)/2+8*((xstop-xstrt)/16-1),ddfstop(a5)
		move.w	#0,bpl1mod(a5)
		move.w	#0,bpl2mod(a5)

		lea	defylogo+16,a0
		lea	a_c2,a1
		move.l	a0,d0
		rept	3
		move.w	d0,6(a1)
		swap	d0
		move.w	d0,2(a1)
		swap	d0
		add.l	#40*256,d0
		addq.w	#8,a1
		endr		; set title copper
		move.l	#a_copper2,cop1lch(a5)
		move.l	a5,-(a7)
		movem.l	defylogo,d0-d7
		movem.l	d0-d7,dest_pal
		movem.l	blank3,d0-d7
		movem.l	d0-d7,source_pal
		move.w	#8,d0
		move.w	#4,d1
		lea	source_pal,a0
		lea	dest_pal,a1
		bsr.w	trigfadeto
		move.l	(a7)+,a5
		addq.w	#1,intro_state
		moveq	#0,d7
		rts

intro_bit
 		tst.w	vblank
		beq.b	intro_bit
		clr.w	vblank

		move.w	intro_state,d0
		lsl.w	#2,d0
		move.l	intro_routines(pc,d0.w),a0
		jsr	(a0)
		tst.w	d7		; 0 = exit!!
		beq.s	intro_bit
		rts

intro_routines
;		dc.l	title_up
;		dc.l	set_title_down
;		dc.l	title_up
		dc.l	set_defy_title
		dc.l	title_up
		dc.l	delay_a_bit
		dc.l	set_defy_down
		dc.l	title_up
		dc.l	quit_it

intro_state	dc.w	0

intro_delay	dc.w	100
quit_it	
		move.w	#-1,d7
		move.w	#4<<12+color,bplcon0(a5)
		rts

title_up
		move.l	a5,-(a7)
		bsr.w	fade_to
		move.b	fadetoflag(a5),d0
		move.l	(a7)+,a5
		move.b	d0,-(a7)
		movem.l	source_pal,d0-d7
		movem.l	d0-d7,color00(a5)
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
		movem.l	blank3,d0-d7
		movem.l	d0-d7,dest_pal
		move.l	a5,-(a7)
		move.w	#16,d0
		move.w	#4,d1
		lea	source_pal,a0
		lea	dest_pal,a1
		bsr.w	trigfadeto
		move.l	(a7)+,a5
		addq.w	#1,intro_state
		moveq	#0,d7
		rts

set_defy_down
		movem.l	dest_pal,d0-d7
		movem.l	d0-d7,source_pal
		movem.l	blank4,d0-d7
		movem.l	d0-d7,dest_pal
		move.l	a5,-(a7)
		move.w	#16,d0
		move.w	#4,d1
		lea	source_pal,a0
		lea	dest_pal,a1
		bsr.w	trigfadeto
		move.l	(a7)+,a5
		addq.w	#1,intro_state
		moveq	#0,d7
		rts

*******************************************************************************

; a5 = custom
; a6 = data_segment

main:		
		tst.w	vblank
		beq.b	main			; wait for VBL interrupt
		clr.w	vblank


		move.l	show_screen,a0	; swap screens.

		tst.w	scroll_pause
		bne.s	.noswap

		move.l	work_screen,show_screen
		move.l	a0,work_screen
.noswap
		move.l	a0,bpl4pth(a5)

************************ program calls.

;	move.w	#$f00,color00(a5)
		bsr.b	vtext_writer
;	move.w	#$000,color00(a5)
		btst	#6,$bfe001		; lmb ?
		bne.b	main
		rts

vblank		dc.w	0

music_int
		move.w	#-1,vblank
		movem.l	d0-d7/a0-a6,-(a7)
		lea	custom,a5
		btst	#5,Intreqr+1(a5)
		beq.b	.out
	ifeq	music_on
		play_music
	endif
.skipit		move.w	#$0020,Intreq(a5)
.out		movem.l	(a7)+,d0-d7/a0-a6
		rte

************************ code.

vtext_writer
		tst.w	scroll_pause
		beq.s	.nopause2
		subq.w	#1,scroll_pause
		rts
.nopause2
		move.w	vtextc,d0
		subq.w	#1,d0
		bne.s	.nonew
		bsr.w	new_line

.nonew
		move.w	d0,vtextc
		movea.l	show_screen,a0		; source
		move.l	work_screen,a1		; dest


		btst	#10,$dff016
		bne.s	.scroll

.ns2		cmp.w	#font_high,vtextc
		beq.s	.noscroll		; for top line
		sub.l	#40,vtextap1
		sub.l	#40,vtextap2
		add.w	#1,vtextc
		bra.s	.noscroll
.scroll
		add.l	#screen_width,a0
		add.l	#screen_width,a2
.noscroll

.bltwait2	btst	#6,dmaconr(a5)
		bne.s	.bltwait2
		move.l	a0,bltapth(a5)	; source 2
		move.l	a1,bltdpth(a5)	; dest 2
		move.w	#USEA+USED+A,bltcon0(a5)
		move.w	#0,bltcon1(a5)
		move.w	#0,bltamod(a5)
		move.w	#0,bltdmod(a5)
		move.w	#(lines_high-1)*64+screen_width/2,bltsize(a5)

		lea	screen_width*(lines_high-1)(a1),a1
		lea	screen_width*(lines_high-1)(a3),a3

		move.l	vtextap1,a0
		move.l	vtextap2,a2
		rept	40/4
		move.l	(a0)+,(a1)+
		move.l	(a2)+,(a3)+
		endr
		move.l	a0,vtextap1
		move.l	a2,vtextap2
vtextexit
		rts

scroll_pause	dc.w	0

new_line
		lea	vtextlookup,a2
		lea	vfont+80,a3		; mknight is stuffed :(
		lea	vtextarea1,a0
		movea.l	vtextp,a1
		moveq	#0,d0
		tst.b	(a1)
		bpl.s	.noreset
		cmp.b	#-1,(a1)
		bne.s	.noreset
		lea	vtext,a1
.noreset
		cmp.b	#-2,(a1)
		bne.w	.no_pause
		move.b	(a1)+,d0
		move.b	(a1)+,d0
		move.w	d0,scroll_pause
		move.l	a1,vtextp
		lea	screen1+(272*40),a0
		lea	screen2+(272*40),a1
		rept	4
		rept	10
		move.l	#0,(a0)+
		move.l	#0,(a1)+
		endr
		endr
		bra.w	new_line
.no_pause
		move.w	#chars_wide-1,d7	; width

drloop
		moveq	#0,d0
		move.b	(a1)+,d0
		sub.w	#" ",d0
		lsl.w	#1,d0
		move.w	0(a2,d0.w),d0
		move.l	a3,a4
		add.w	d0,a4		; this letter

y1		set	0
x1		set	0
		rept	8		; font height
		move.b	y1(a4),x1(a0)
y1		set	y1+80
x1		set	x1+40
		endr

		addq.w	#1,a0
		dbf	d7,drloop
		lea	vtextarea1,a0
		move.l	a0,vtextap1
		lea	vtextarea2,a0
		move.l	a0,vtextap2
		moveq	#font_high+1,d0
		move.w	#font_high+1,vtextc
		move.l	a1,vtextp
		rts

	ifeq	music_on
use		=	$40991c
playback_speed	=	3
		include	music/types.i
		include	music/player6.i
		incdir 	work:cdn/defy4teaser/music/
		include	player.s
		incdir	work:cdn/defy4teaser/
	endif

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
		beq.w 	.nofadeto	
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
blank2		dc.w	$324,$324,$324,$324,$324,$324,$324,$324
		dc.w	$324,$324,$324,$324,$324,$324,$324,$324
blank3		dc.w	$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff
		dc.w	$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff
blank4		dc.w	$012,$012,$013,$013,$124,$345,$557,$777
		dc.w	$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff
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
		even
		
show_screen	dc.l	screen1
work_screen	dc.l	screen2

************************ program data.

vfont		incbin	fonts/mknight.font
vtextp		dc.l	vtext
vtextc		dc.w	1
vtext		;dc.w	' .......................................... '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'          COMING SOON! FROM             '
		dc.b	'          -^-[ CYDoNiA ]-^-             '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'               DEFY 4!                  '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	-2,55
		dc.b	'                                        '
		dc.b	'   Defy 4 is on it''s way, so you had    '
		dc.b	'    better hurry up and write those     '
		dc.b	'articles, draw those clips and compose  '
		dc.b	'              those tunes!              '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	' The deadline for Issue 4 is the end of '
		dc.b	'             NOVEMBER! 1995             '
		dc.b	'             ==============             '
		dc.b	'                                        '
		dc.b	'    For those of you who want to be     '
		dc.b	'  artistic, please supply all clips as  '
		dc.b	'   640x256, 16 colour IFF files, but    '
		dc.b	'remember, the actual clip itself cannot '
		dc.b	'  be bigger than 320x192, and you also  '
		dc.b	'  can''t use the first three colours in  '
		dc.b	' the palette. Other than that, you may  '
		dc.b	'   change them to whatever you wish!    '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	' Ads should be in one of the following  '
		dc.b	'                 forms:                 '
		dc.b	'     38x24 ASCII        78x24 ASCII     '
		dc.b	'IFF clip (Same restrictions as before!) '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	-2,255
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	' -------------------------------------- '
		dc.b	'                                        '
		dc.b	' Teaser Credits:                        '
		dc.b	'             Code : cro / CYDoNiA       '
		dc.b	'            Music : Laxical / Scoopex   '
		dc.b	'         Graphics : Souri / CYDoNiA     '
		dc.b	'                                        '
		dc.b	' -------------------------------------- '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	' CYDoNiA says hi to :-                  '
		dc.b	'                                        '
		dc.b	'                 Access                 '
		dc.b	'                Anathema                '
		dc.b	'                  Axis                  '
		dc.b	'               BlackJack                '
		dc.b	'             Digital Access             '
		dc.b	'                  Dusk                  '
		dc.b	'            Flying Cows, Inc.           '
		dc.b	'                Freezers                '
		dc.b	'                Frontier                '
		dc.b	'                 F.U.N.                 '
		dc.b	'                  Gods                  '
		dc.b	'                 K!inky                 '
		dc.b	'                 Logic                  '
		dc.b	'              Metrophysics              '
		dc.b	'                  Myth                  '
		dc.b	'                Passion                 '
		dc.b	'                 Pearl                  '
		dc.b	'             Pygmy Projects             '
		dc.b	'                Scoopex                 '
		dc.b	'                Solitude                '
		dc.b	'                  TNC                   '
		dc.b	'                                        '
		dc.b	' -------------------------------------- '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'          Call a CYDoNiA BBS!           '
		dc.b	'          ===================           '
		dc.b	'                                        '
		dc.b	'          Twisted Dreams BBS            '
		dc.b	'                                        '
		dc.b	'              Lucid Dreams              '
		dc.b	'                                        '
		dc.b	'               Syndicate                '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'  You can leave contributions on ANY    '
		dc.b	'             CYDoNiA board...           '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	-2,255
		dc.b	' -------------------------------------- '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'            Write to CYDONIA            '
		dc.b	'                                        '
		dc.b	'       (SYDNEY)                         '
		dc.b	'                                        '
		dc.b	'       (ADELAIDE)                       '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'  Contributions can be mailed to either '
		dc.b	'           of these addresses.          '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	-2,255
		dc.b	' -------------------------------------- '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	' You can also contribute by E-Mail to:  '
		dc.b	'                                        '
		dc.b	' CRO:                                   '
		dc.b	'                                        '
		dc.b	' STORM:                                 '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	-2,255
		dc.b	' -------------------------------------- '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'        CYDONIA MEMBERS ARE             '
		dc.b	'                                        '
		dc.b	'             (ADELAIDE)                 '
		dc.b	'             ----------                 '
		dc.b	'   Bonza                       -Sysop   '
		dc.b	'   Cro                 -Code/Swapping   '
		dc.b	'   Eminence                    -Music   '
		dc.b	'   Serkul                      -Music   '
		dc.b	'                                        '
		dc.b	'              (SYDNEY)                  '
		dc.b	'              --------                  '
		dc.b	'   Accolyte                     -Code   '
		dc.b	'   Coolcat                       -GFX   '
		dc.b	'   Infinity                      -GFX   '
		dc.b	'   Krion                  -Sysop/Code   '
		dc.b	'   Prowler            -Sysop/Swapping   '
		dc.b	'   Snykers                       -GFX   '
		dc.b	'   Souri                         -GFX   '
		dc.b	'   Storm                        -Code   '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'           -^-[ CYDoNiA ]-^-            '
		dc.b	'     PACKING CLASS AND KICKING ASS!     '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'              TEXT REPEAT               '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	'                                        '
		dc.b	-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
		dc.b	-1,-1,-1,-1,-1,-1,-1,-1
		dc.b	-1
		even


;	> !"#$%&'()*+,-./0123<
;	>456789:;<=>?@ABCDEFG<
;	>HIJKLMNOPQRSTUVWXYZ[<

vtextap1	dc.l	vtextarea1
vtextap2	dc.l	vtextarea2

vtextlookup
y		set	0
		rept	3
x		set	0
		rept	79
		dc.w	x+y
x		set	x+1
		endr
y		set	y+f_h*f_w
x		set	0
		endr
		even

*******************************************************************************

*******************************************************************************
************************ chip data.

		section	chipdata,data_c

copper:		dc.w	-1,-2
a_copper2	
a_c2		dc.w	bpl1pth,0,bpl1ptl,0
		dc.w	bpl2pth,0,bpl2ptl,0
		dc.w	bpl3pth,0,bpl3ptl,0
		dc.w	-1,-2

defylogo	incbin	gfx/defy2.pic
		even
mod		incbin	music/p60.chipit
		even
************************

		section bss,bss_c

vtextarea1
		rept	font_high+1	; height
		ds.b	40	; width
		endr
		even
vtextarea2	rept	font_high+1	; plane2
		ds.b	40
		endr
		even

		even
screen1		ds.b	276*40
		even
		ds.b	10*40
		even
screen2		ds.b	276*40
		even
		ds.b	10*40
		even
*******************************************************************************

		end

