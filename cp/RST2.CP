;rst2.cp
;
;		get qty committed from lines
;
record	invqty
	.include 'def:invqty.def'
;
record	itmmas
	.include 'def:rd041a.def'
record	itmkey
	.include 'def:rd041k.def'

record	ordlin
	.include 'def:rd045a.def'

record	ordhdr
	.include 'def:rd044a.def'

record	coptbl
	.include 'def:rd182a.def'

record	dis
	ii	,d6

record	channel
	chn041	,d2
	chn044	,d2
	chn045	,d2
	chn182	,d2
	chnwrk	,d2
	chnerr	,d2
;
record	vars
	opnok	,d1
	wrkfil	,a14
	vanrfa	,a6
	vanflg	,d1
	itmrfa	,a6
	ivflag	,d1
	switch	,d1
	v	,d1
;
proc
	xcall terid (v)
	xcall outpt (1,1,2,'Reset Qty Committed',1)

	call opens
	if (.not. opnok) goto endoff

	find (chn045, ordlin, ^first) [err=loop]
loop,
	incr ii
	if (ii/1000*1000 .eq. ii) display (15,$scr_pos(1,70),dis)
	reads (chn045, ordlin, eof)
	if (ltype .ne. 'L') goto loop
	if (lordno .ne. oordno) read (chn044, ordhdr, lordno) [err=loop]
	if (oloc .ne. 'O') goto loop	;orders only
	if (lqtyor .eq. 0) goto loop	;nothing to commit


;;;	xcall gtitm (litmno, lf1, lf2, lf3, itmrfa, ivflag)
	xcall gtitm (litmno, lf1, lf2, lf3, itmrfa, ivflag,vanflg,vanrfa)

	using ivflag select
	(0),	call error
	(1),	call found
	endusing

	goto loop
;=====================================================
found,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	read (chn041, itmmas, rfa:itmrfa)
	clear invqty
	in_item = itemno
	in_f1 = if1
	in_f2 = if2
	in_f3 = if3	
	read (chnwrk, invqty, in_key) [err=newiq]
	in_ord = in_ord + lqtyor
	write (chnwrk, invqty, in_key)

	return

newiq,
	clear invqty
	in_item = itemno
	in_f1 = if1
	in_f2 = if2
	in_f3 = if3	
	in_ord = lqtyor
	in_com = qtycom

	store (chnwrk, invqty, in_key)	
	return
;------------------------------------------------------

error,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	writes (chnerr, ordlin)

	return
;------------------------------------------------------

eof,
endoff,
	call close
	stop

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	clear opnok

	switch = 5
	xcall files (1, 'SI', 41, switch)
	if (switch .eq. 9) return
	chn041 = 1

	switch = 5
	xcall files (4, 'SI', 44, switch)
	if (switch .eq. 9) return
	chn044 = 4

	switch= 5
	xcall files (5, 'SI', 45, switch)
	if (switch .eq. 9) return
	chn045 = 5

	switch= 5
	xcall files (17, 'SI', 182, switch)
	if (switch .eq. 9) return
	chn182 = 17

	wrkfil = 'spl:rstcom.ism'
	XCALL ISAMC (wrkfil, 39, 1, 'START=1, LENGTH=27, NODUPS, ASCEND')
	OPEN (6, SU, wrkfil)
	chnwrk = 6

	open (10,o,'rsterr.dat')
	chnerr = 10

	opnok = 1
	return
;-----------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (chn041) close chn041
	if (chn044) close chn044
	if (chn045) close chn045
	if (chn182) close chn182
	if (chnwrk) close chnwrk
	close chnerr
	return
;-----------------------------------------------
