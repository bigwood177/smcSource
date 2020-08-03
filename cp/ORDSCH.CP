;ordsch.cp
;
;	maintain order schedule file
;
;	10-29-18 display opromd :MM/DD
;
	.include 'wnd:windows.def'

record	o_buffer		;for locking ordsch
		,a250

record	ordsch
	.include 'def:rd087a.def'

record	ordhdr
	.include 'def:rd044a.def'

record	jobhdr
	.include 'def:rd071a.def'


record	POPSCH
	.include 'def:popSCH.def'

record	coptbl
	.include 'def:rd182a.def'


;
RECORD TITLE
	TIT_DEPT	,A30

RECORD HDR1
	,A*,	'SEQ OPR   ORDER#                           PRINTED     '
	,A*,	'DUE-DATE       LBS'


RECORD	PRINT
	num9	,d6
	PLINE	,A80
	HDR	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	LPSW	,D1
	SPLFIL	,A14
	LINCNT	,D3
	LPARG	,D1
	LPONSW	,D1
	PGCNT	,D3
	PRTCTL	,D3
	PRTTYP	,A1
	RPTNUM	,D3
	
	
record	
	mdhs	,d8
record,x
	mn	,d2
	dd	,d2
	hh	,d2
	mm	,d2

record
	due	,d4
record,x
	dmm	,d2
	ddd	,d2	


record	channel
	chn044	,d2
	chn071	,d2
	chn087	,d2
	chn182	,d2

record	wvars
	wnd_1	,d4
	w_id	,d4
	wn_name	,a10

record	vars
	cmpcod	,a3
	opnok	,d1
	tmp_ara	,a80
	tmp_rfa	,a6
	tmp_cpl	,d1
	first	,d6
	last	,d6
	n	,d1
	xdept	,a2		;current dept
	pounds	,d12
	xdate	,d4
	time_stamp	,d8
	a6	,a6
	d2	,d2
	d4	,d4
	stat	,d1
	dec3	,d3
	i	,d6
	j	,d6
	cnt	,d6
	lbs	,d8
	entry	,a30
	inxctl	,d1
	cngctl	,d1
	whatno	,d2

	read	,d1,0
	write	,d1,1
	store	,d1,2
	lokctl	,d1
	switch	,d1
	v	,d1
;
proc
	xcall terid (v)
	xcall who (cmpcod)

	call init_window
	xcall w_disp (wnd_1, wd_pos, 1, 1, 'ORDER SCHEDULE MAINTENANCE  <HOME>=PRINT')

	using cmpcod select
	('TST'),	open (66, u, 'tst:ordsc.tst')		;locking
	('SMC'),	open (66, u, 'smc:ordsc.smc')		;locking
	('ROC'),	open (66, u, 'roc:ordsc.roc')		;locking
	(),		NOP		;not yet
	endusing

;;;	open (66, u, 'smc:ordsc.smc')		;locking
	reads (66, o_buffer, o_eof) [err=o_eof]

displa,
	xcall w_disp (wnd_1, wd_pos, 4,4, 'DEPT: ')
	xcall winpt (wnd_1, 4, 10, 02, 00, 'AE', entry, inxctl)
	goto (displa, endoff), inxctl

	xdept = entry(1,2)
	xcall wancn (wnd_1, 23, cngctl, whatno)
	cngctl = 2
	goto (displa), cngctl

	call close 

	xcall ordsc(cmpcod)	;rebuild ordsch.ism
;;;	xcall ordsc 		;rebuild ordsch.ism

	call opens
	if (.not. opnok) goto endoff

loop,
	call get_dept	;get ordsch records for a dept
			;sets numara and num9

	if (numara .le. 0) goto displa		;no records

	XCALL TTSTS (STAT)
	IF (STAT)	XCALL W_DISP(WND_1,WD_ACCEPT,DEC3)

	dline = '  chg seq   order# customer              time stamp     due   pounds'
	pop_title(12,13) = xdept
	xcall schp (popsch)		;resets numara=numara+num9

	using p_action select
	(0),	goto displa		; <end>=done w/ this dept
	(1),	nop			; selected
	(5),	begin			; print
		call print_dept
		goto displa
		end
	(),	goto displa
	endusing


	if (new_seq .le. 0) goto displa	;nothing changed
	using old_seq select
	(.lt. new_seq),		begin
				n = 1	;move records up
				dline = parry(old_seq)
				tmp_rfa = parrfa(old_seq)
				tmp_cpl = parcpl(old_seq)	;5-29-19
				end
	(.gt. new_seq),		begin
				n = -1	;move records down
				dline = parry(old_seq)
				tmp_rfa = parrfa(old_seq)
				tmp_cpl = parcpl(old_seq)	;5-29-19
				end
	(),			goto displa	;nothing changed

	endusing

	first = old_seq
	last = new_seq -n

	for i from first thru last by n 
		begin
		parry(i) = parry(i+n)
		parrfa(i) = parrfa(i+n)
		parcpl(i) = parcpl(i+n)		;5-29-19
		end

	parry(i) = dline
	parrfa(i) = tmp_rfa
	parcpl(i) = tmp_cpl			;5-29-19

	
; rebuild schedule file

; clear ordsch.ism
;

;5-31-19 insure exclusive use of file...
	switch = 3
	xcall files (chn087, 'SU', 87, switch)
	if (switch .eq. 9)
		begin
		xcall olmsg (wnd_1,24, 'Schedule not updated!',1)
		goto endoff
		end

	for i from 1 thru numara		;delete the existing records
		begin
		read (chn087, ordsch, rfa:parrfa(i) )	;read the old recod
		delete (chn087)				;delete the old record
		end

	for i from 1 thru numara		;write back changed records
		begin

		dline = parry(i)

		sch_dpt = pardpt(i)
		sch_dp1 = xdept(1,1)
		sch_ord = dline(7,12)
		sch_seq = i
		sch_lbs = dline(55,61)
		d4 = dline(35,36)
		d2 = dline(38,39)
		d4 = d4*100 + d2
		sch_dat = d4

		d4 = dline(41,42)
		d2 = dline(44,45)
		d4 = d4*100 + d2
		sch_tim = d4

		sch_opr = dline(5,5)
		sch_typ = dline(65,65)
		sch_cpl = parcpl(i)		;5-29-19
		store (chn087, ordsch, sch_key)
		end

	xcall files (chn087, 'SU', 87, 4)	;close
	switch = 5
	xcall files (chn087, 'SU', 87, switch)	;re-open

	goto loop

endoff,
	call close
o_eof,
	xcall u_finish
	close 66
	stop

;==========================================================
;   internal routines:

add_new_orders,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; add any new orders to end of sched
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	return
;-------------------------------------------------------
print_dept,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; print dept schedule
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	clear tbl_key
	tblcod = 'DD'
	dd_dept = xdept
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .ne. 0) clear coptbl
	tit_dept = dd_desc

	lincnt = 66
	pgcnt = 0

	clear ordsch
	sch_dp1 = xdept
	find (chn087, ordsch, sch_key, krf=1) [err=ploop]
ploop,

	xcall ios (chn087, ordsch, read, lokctl)
	if (lokctl .ne. 0) goto p_eof
	if (sch_dp1 .ne. xdept(1,1)) goto p_eof	;7-22-19
;;;	if (sch_dpt .ne. xdept) goto p_eof	;done	removed 7-22-19
	if (sch_cpl .eq. 9) goto ploop		;skip if completed

	xcall isio (chn044, ordhdr, sch_ord, read, lokctl)
	if (lokctl .ne. 0) clear ordhdr

	pline (1,3) = sch_seq,	'ZZX'
	pline (6,6) = sch_opr
	pline (11,16) = sch_ord,	'ZZZZZX'
	pline (18,37) = ocusnm
	pline (42,46) = sch_dat, 'ZX/XX'
	pline (48,52) = sch_tim, 'ZX:XX'
;;;	xdate(1,2) = opromd(5,6)
;;;	xdate(3,4) = opromd(3,4)
	xdate = opromd(5,8)			;10-29-18 mm/dd
	pline (58,62) = xdate,		'ZX/XX'
	pline (66,73) = sch_lbs,	'ZZZ,ZZX'
	call print
	goto ploop
p_eof,
	IF (LPONSW.EQ.1) XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	return
print,
	if (lponsw.eq.0) call prnton
	prtctl = 80
	xcall lpout (lincnt,pgcnt,pline,title,hdr1,hdr,hdr,
&			lg,lg,lg,0,080,prtctl,0,lpsw,rptnum,prttyp)
	return
prnton,
	splfil (5,6) = 'el'
	lpsw = 1	; print, spool or display
	lpsw = 8	; don't ask - local printer
	xcall lpon (lpsw,splfil)
	if (lpsw.eq.0) call close
	if (lpsw.eq.0) xcall pgchn ('cp:cpmenu',1)

	lponsw = 1
	return
;----------------------------------------------------------------

;SEQ OPR   ORDER#                           PRINTED     DUE-DATE       LBS
;ZZX  A    ZZZZZX AAAAAAAAAAAAAAAAAAAA    ZX/XX HH:MM     ZX/XX   ZZZZ,ZZX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890çç 
;         1         2         3         4         5         6         7
	return
;-------------------------------------------------------

get_dept,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; get ordsch records for this dept
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	clear new_seq, old_seq, num9

	for i from 1 thru maxara
		begin
		clear parry(i)
		clear pflags(i)
		clear parrfa(i)
		clear parseq(i)
		clear pardpt(i)		;9-6-17
		clear parcpl(i)		;5-29-19
		end

	clear i

	clear ordsch
	sch_dp1 = xdept(1,1)		;6-24-19
;;;	sch_dp1 = xdept
	find (chn087, ordsch, sch_key, krf=1) [err=gd_loop]
gd_loop,
	xcall ios (chn087, ordsch, read, lokctl)
	if (lokctl .ne. 0) goto gd_eof
	if (sch_dp1 .ne. xdept(1,1)) goto gd_eof		;6-26-19
;;;	if (sch_dpt .ne. xdept) goto gd_loop		;might be HV, FV, KV
	
	using sch_typ select
	('O'),	xcall isio (chn044, ordhdr, sch_ord, read, lokctl)
	('J'),	begin
		xcall isio (chn071, jobhdr, sch_ord, read, lokctl)
		opromd = jhshdt
		ocusnm = 'SMC Job Ticket'
		end
	endusing

	if (sch_cpl .eq. 9) incr num9	;5-31-19

;;;	xcall isio (chn044, ordhdr, sch_ord, read, lokctl)
	if (lokctl .ne. 0) clear ordhdr, jobhdr

	clear dline
	dline ( 1,3) = sch_seq,		'ZZX'
	dline ( 5,5) = sch_opr
	dline ( 7,12) = sch_ord,	'ZZZZZX'
	dline (14,33) = ocusnm
	dline (35,39) = sch_dat,	'ZX/XX'
	dline (41,45) = sch_tim,	'ZX:XX'
;;;	xdate (1,2) = opromd(5,6)
;;;	xdate (3,4) = opromd(3,4)
	xdate = opromd(5,8)		;10-29-18 mm/dd
	dline (49,53) = xdate,	'ZX/XX'
	dline (55,61) = sch_lbs,	'ZZZZZZX'
	dline (65,65) = sch_typ

	incr i
	parry(i) = dline
	parseq(i) = sch_seq
	pardpt(i) = sch_dpt
	parcpl(i) = sch_cpl		;complete flag 5-29-19
	xcall getrfa (chn087, parrfa(i) )

	if (i .eq. 2) parclr(i) = 'R'
	if (i .ge. maxara) goto gd_eof	;2-3-15
	goto gd_loop

gd_eof,
	numara = i
	numara = numara - num9		;9's will be skipped
	return
;-------------------------------------------------------

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear opnok


	switch = 5
	xcall files (4,'SI',044, switch)
	if (switch .eq. 9) return
	chn044 = 4

	switch = 5
	xcall files (71, 'SI', 071, switch)
	if (switch .eq. 9) return
	chn071 = 71

	switch = 5
	xcall files (8, 'SU', 87, switch)
	if (switch .eq. 9) return
	chn087 = 8

	switch = 5
	xcall files (17, 'SI', 182, switch)
	if (switch .eq. 9) return
	chn182 = 17

	opnok = 1

	return
;--------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	if (chn044) close chn044
	if (chn071) close chn071
	if (chn087) close chn087
	if (chn182) close chn182
	return
;---------------------------------------------

init_window,

	xcall u_start(,,,,,,,99)

	wn_name = 'sched'
;;;	wn_name = 'plabl'

	xcall w_proc(wp_find,wnd_1,wn_name)
	if (.not. wnd_1)
		begin
		xcall w_proc(wp_create,wnd_1,wn_name,0,0)
		end
	xcall w_brdr(wnd_1,wb_title,'department schedule',
&			wb_tpos,wbt_top,wbt_center)
	xcall w_proc(wp_place,wnd_1,1,1)	
	xcall w_disp(wnd_1,wd_clear)

	w_id = wnd_1

;Pallets...
	xcall w_proc(wp_palet, 4, 1, 0)		;1=blue
	xcall w_proc(wp_palet, 5, 2, 0)		;2=green
	xcall w_proc(wp_palet, 6, 3, 0)		;3=cyan
	xcall w_proc(wp_palet, 7, 4, 7)		;4=red
	xcall w_proc(wp_palet, 8, 7, 0)		;7=white
	xcall w_proc(wp_palet, 9, 0, 7)		;4=black on white
	xcall w_proc(wp_palet,10, 6, 0)		;6=yellow
	xcall w_proc(wp_palet,15, 0, 15)	;default


	paletR = 7		;01-17-11

;pop40:
	maxara = 2000		;5-29-19 increased from 700 to 2000
	plen = 68
	plen = 69
	numrow = 20
	wx = 3
	wy = 1
	pop_wid(1,5) = "depts"
	pop_title = "dept sched"


	RETURN

;---------------------------------------

	end



