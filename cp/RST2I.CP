;rst2i.cp
;
record	invqty
	.include 'def:invqty.def'

record	itmmas
	.include 'def:rd041a.def'
record	itmkey
	.include 'def:rd041k.def'
;
record	out
	o_item	,a15
	o_f1	,a3
	o_f2	,a3
	o_f3	,a5
		,a1
	o_com	,d6

record	hdr
		,a*,	'ITEM             F1  F2    F3 DESCRIPTION'
		,a*,	'                    INV-COM  ORD-COM'

record	pbuf
	pline	,a132

record	dis
	ii	,d6

record	channel
	chn041	,d2
	chnwrk	,d2
;
record	vars
	opnok	,d1
	switch	,d1
	v	,d1
;
proc
	xcall terid (v)
	xcall outpt (1,1,2,'Qty Committed Discrepency Report',1)

	call opens
	if (.not. opnok) goto eof
loop,
	incr ii
	if (ii/500*500 .eq. ii) xcall outpt (1,70,1,dis,1)
	reads (chn041, itmmas, eof)
	if (stock .ne. 'S') goto loop	;only stocked items
	
	clear invqty
	in_item = itemno
	in_f1 = if1
	in_f2 = if2
	in_f3 = if3
	read (chnwrk, invqty, in_key) [err=noinv]
	if (qtycom .ne. in_ord) call discr

	goto loop

noinv,	
	if (qtycom .eq. 0) goto loop	;who cares?
	
	clear out
	o_item = itemno
	o_f1 = if1
	o_f2 = if2
	o_f3 = if3
	o_com = qtycom
	write (10, out)

	goto loop

eof,
	call close
	stop

;================================================
;------------------------------------------------


discr,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; discrepency between itmmas & invqty
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	pline (1,15) = itemno
	pline (17,19) = if1
	pline (21,23) = if2
	pline (25,29) = if3
	pline (31,60) = descr
	pline (62,68) = qtycom,	'ZZZ,ZZX'
	pline (71,77) = in_ord,	'ZZZ,ZZX'
	call print
	return
;------------------------------------------------

print,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	writes (14, pline)
	clear pline
	return
;------------------------------------------------


;ITEM             F1  F2    F3 DESCRIPTION                    INV-COM  ORD-COM
;AAAAAAAAAAAAAAA AAA AAA AAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA ZZZ,ZZX  ZZZ,ZZX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

	switch = 5
	xcall files (4, 'SI', 041, switch)
	if (switch .eq. 9) return
	chn041 = 4

	open (5,si,'spl:rstcom.ism')
	chnwrk = 5

	open (10, o, 'spl:noinvq.dat')
	open (14, o, 'spl:rstcom.spl')
	writes (14, hdr)

	opnok = 1
	return
;--------------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (chn041) close chn041
	if (chnwrk) close chnwrk
	close 10
	close 14

	return
;--------------------------------------------------

