;SCVIEW.CP
;ordsch.cp
;
;	maintain order schedule file
;
	.include 'wnd:windows.def'

record	ordsch
	.include 'def:rd087a.def'

record	ordhdr
	.include 'def:rd044a.def'

record	POPSCH
	.include 'def:pop80.def'
;;;	.include 'def:popSCH.def'

record	sch_tmp
	.include 'def:rd087t.def'

;
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

record	print
	pline	,a80

record	channel
	chn044	,d2
	chn087	,d2

record	wvars
	wnd_1	,d4
	w_id	,d4
	wn_name	,a10

record	vars
	opnok	,d1
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
	xcall terid (V)
	call init_window
	xcall w_disp (wnd_1, wd_pos, 1, 1, 'ORDER SCHEDULE MAINTENANCE')

	call opens
	if (.not. opnok) goto endoff

displa,
	xcall w_disp (wnd_1, wd_pos, 4,4, 'DEPT: ')
	xcall winpt (wnd_1, 4, 10, 02, 00, 'AE', entry, inxctl)
	clear lokctl
	goto (displa, endoff), inxctl

	xdept = entry(1,2)
	xcall wancn (wnd_1, 23, cngctl, whatno)
	cngctl = 2
	goto (displa), cngctl


loop,
	call get_dept	;get ordsch records for a dept

	if (numara .le. 0) goto displa		;no records

	XCALL TTSTS (STAT)
	IF (STAT)	XCALL W_DISP(WND_1,WD_ACCEPT,DEC3)

	dline = '  chg seq   order# customer          time stamp     due       pounds'
	pop_title(12,13) = xdept
;;;	xcall schp (popsch)
	xcall pop80 (popsch)

	using p_action select
	(5),	call print_dept
	endusing

	goto displa

endoff,
	call close
	xcall u_finish
	xcall flags (7000000)
	stop

print_dept,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; print dept schedule
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	open (14,o,'p.dat')
	pline = 'Shedule for Department ' + xdept
	call print

	pline = 'SEQ OPR   ORDER#                           PRINTED     DUE-DATE       LBS'
	call print
	clear ordsch
	sch_dp1 = xdept
	find (chn087, ordsch, sch_key, krf=1) [err=ploop]
ploop,

	xcall ios (chn087, ordsch, read, lokctl)
	if (lokctl .ne. 0) goto p_eof
	if (sch_dp1 .ne. xdept) goto p_eof	;done

	xcall isio (chn044, ordhdr, sch_ord, read, lokctl)
	if (lokctl .ne. 0) clear ordhdr

	pline (1,3) = sch_seq,	'ZZX'
	pline (6,6) = sch_opr
	pline (11,16) = sch_ord,	'ZZZZZX'
	pline (18,37) = ocusnm
	pline (42,46) = sch_dat, 'ZX/XX'
	pline (48,52) = sch_tim, 'ZX:XX'
	pline (58,62) = xdate,		'ZX/XX'
	pline (66,73) = sch_lbs,	'ZZZ,ZZX'
	call print
	goto ploop
p_eof,
	close 14
	return
print,
	writes (14, pline)
	clear pline
	return

;SEQ OPR   ORDER#                           PRINTED     DUE-DATE       LBS
;ZZX  A    ZZZZZX AAAAAAAAAAAAAAAAAAAA    ZX/XX HH:MM     ZX/XX   ZZZZ,ZZX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890çç 
;         1         2         3         4         5         6         7
	return
;-------------------------------------------------------

get_dept,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; get ordsch records for this dept
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	for i from 1 thru maxara
		begin
		clear parry(i)
		clear pflags(i)
		clear parrfa(i)
	;;;	clear parseq(i)
	;;;	clear o_ara(i)
	;;;	clear o_rfa(i)
		end

	clear i

	clear ordsch
	sch_dp1 = xdept
	find (chn087, ordsch, sch_key, krf=1) [err=gd_loop]
gd_loop,
	xcall ios (chn087, ordsch, read, lokctl)
	if (lokctl .ne. 0) goto gd_eof
	if (sch_dp1 .ne. xdept) goto gd_eof

	xcall isio (chn044, ordhdr, sch_ord, read, lokctl)
	if (lokctl .ne. 0) clear ordhdr

	clear dline
	dline ( 1,3) = sch_seq,		'ZZX'
	dline ( 5,5) = sch_opr
	dline ( 7,12) = sch_ord,	'ZZZZZX'
	dline (14,33) = ocusnm
	dline (35,39) = sch_dat,	'ZX/XX'
	dline (41,45) = sch_tim,	'ZX:XX'
	xdate (1,2) = opromd(5,6)
	xdate (3,4) = opromd(3,4)
	dline (49,53) = xdate,	'ZX/XX'
	dline (55,61) = sch_lbs,	'ZZZZZZX'

	incr i
	parry(i) = dline
;;;	parseq(i) = sch_seq
	xcall getrfa (chn087, parrfa(i) )

	goto gd_loop

gd_eof,
	numara = i
	return
;-------------------------------------------------------

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear opnok

	switch = 5
	xcall files (4,'SI',044, switch)
	if (switch .eq. 9) return
	chn044 = 4


	switch = 5
	xcall files (8, 'SI', 87, switch)
	if (switch .eq. 9) return
	chn087 = 8

	opnok = 1
	return
;--------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (chn044) close chn044
	if (chn087) close chn087
	return
;---------------------------------------------

init_window,

	xcall u_start(,,,,,,,99)

	wn_name = 'plabl'

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

;pop40:
	maxara = 80
	plen = 68
	numrow = 20
	wx = 3
	wy = 1
	pop_wid(1,5) = "depts"
	pop_title = "dept sched"


	RETURN

;---------------------------------------

	end




