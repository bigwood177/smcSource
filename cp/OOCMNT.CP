;oocmnt.cp
;
; open order report comment maintenance.

.include 'wnd:windows.def'


record	oocmnt
	ooc_ord	,d6
	ooc_not	,a25

record	win
	wnd_1	,d4
	wnd_2	,d4
	w_id	,d4
	WN_NAME	,A6,	'opnord'
	W2_NAME	,A6,	'notes '
	WN_TNMBR	,D2


RECORD	WRKLIN
	W_DATE	,D8
	W_ORDER	,D6
	W_SREP	,A3
	W_SC_NAME	,A4
	W_PLINE	,A132


record	arras
	opn_rec	,20d6	;opnord record numbers
	opn_not	,20a25	;opnord notes
	opn_ord	,20d6	;opnord order #'s for opnord.ism
	i	,d6	;index
;

record	tmplt
	tn	,a3		;prompt number
		,a1		
	tline	,a101
	tx	,a25		;notes


record	funkey
	.include 'def:funkey.def'

record	vars
	print	,d1
	f_desc	,a47,	'<Page Up> <Page Down> # to change, <Home>=Print'
;                        12345678901234567890123456789012345678901234567890
;	f_desc	,a33,	'<Page Up> <Page Down> # to change'

	row	,d2
	col	,d3,	106
	a1	,a1
	entry	,a30
	inxctl	,d1
	cngctl	,d1
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	whatno	,d2
	f_key	,d3
	recno	,d6
	maxrec	,d6

	v	,d1

;
proc
	open (15,i,'tt:')

	call init_window

;;;	xcall terid (v)
;;;	xcall outpt (1,1,2,'Open Order Comments',1)
	xcall w_disp (w_id, wd_pos, 1,1, wd_clr, wdc_wnd, 'Open Orders')
;
	call opens
	call count_records


	recno = 0
	call get_page
displa,
	w_id = wnd_1	

anycn,
	print = 0
	whatno = 0
	cngctl = 6
	xcall wanfk (w_id, 24, cngctl, whatno, f_key, f_desc)
	using f_key select
	(PAGE_DN),	call get_page
	(PAGE_UP),	begin
			recno = recno - 40
			if (recno .le. 0) recno = 0
			call get_page
			end		
	(EXIT_KEY),	goto endoff
	(END_KEY),	goto endoff
	(HOME_KEY),	begin
			print = 1
			goto endoff
			end
	endusing

	goto (proces, cngbr), cngctl + 1
cngbr,

	if (whatno.lt.1 .or. whatno.gt.20) goto anycn
	i = whatno
	row = i + 2

	xcall winpl (w_id, row,col,25,0,'AE',entry,inxctl)
	goto (displa,displa),inxctl
	opn_not(i) = entry
	goto anycn
proces,
	call write_page
	goto displa
endoff,
	call write_page
	call close
	if (print)
	then	xcall pgchn ('cp:opnord',1)
	else	xcall pgchn ('cp:oemnu',1)


get_page,
	call write_page

	for i from 1 thru 20
		begin
		opn_rec(i) = 
		opn_not(i) =
		opn_ord(i) = 
		end

	for i from 1 thru 20
		begin
		incr recno
		if (recno .gt. maxrec) recno = 1	;start over
		read (1, wrklin, recno)
		xcall isio (2, oocmnt, w_order, read, lokctl)
		if (lokctl .ne. 0) clear ooc_not
		tn = i,	'ZX.'

		tline(1,95) = w_pline
		tline(97,100) = w_pline(101,104)

		tx = ooc_not
		opn_rec(i) = recno
		opn_not(i) = ooc_not
		opn_ord(i) = w_order
		xcall w_disp (wnd_1, wd_pos, i+2, 1, wd_clr, wdc_lin, tmplt)
		end	

	return
;--------------------------------------------------

write_page,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	for i from 1 thru 20
		begin
		using opn_ord(i) select
		(.le.0),	nop

		(),	begin
			lokctl = 1
			xcall isio (2, oocmnt, opn_ord(i), read, lokctl)
			if (lokctl .eq. 0)
			  begin
			  ooc_not = opn_not(i)
			  xcall isio (2, oocmnt, opn_ord(i), write, lokctl)
			  end
			end
		endusing
		end
	return
;--------------------------------------------------


count_records,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	maxrec = 0
loop,
	reads (1, wrklin, eof)
	incr maxrec
	clear oocmnt
	ooc_ord = w_order
	xcall isio (2, oocmnt, ooc_ord, store, lokctl)
	goto loop
eof,
	return
;--------------------------------------------------

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	open (1,i,'tst:opnord.wrk')
;;;	open (2,su,'tst:opnord.tsm')	;comments

	open (1,i,'smc:opnord.wrk')	;last open order report
	open (2,su,'smc:opnord.smm')	;comments
	return
;--------------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	close 1
	close 2
	return
;--------------------------------------------------
;
;=================================================================
INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL U_START("LIB:SMC_WND",,,,,,,100)

	xcall u_resize (25,132)

	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,24,132)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'Open Orders',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL W_UPDT
	W_ID = WND_1

	RETURN
;-------------------------------------------------------

