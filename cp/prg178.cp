;prg178.cp
;
;	purge old cctran info.

record	cctran
	.include 'def:rd138a.def'

record	channel
	chn138	,d2

record	dis
	ii	,d6

record	vars
	opnok	,d1
	i	,d6
	pdate	,d4
	today	,d8
	xdate	,d4
	entry	,a30
	inxctl	,d1
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	delete	,d1,3
	switch	,d1
	v	,d1

proc
	xcall terid (v)
	xcall outpt (1,1,2,'PURGE EXPIRED CUSTOMER CREDIT CARDS',1)
	call opens
	if (.not. opnok) goto endoff

	xcall rdat8(today)
	pdate(1,2) = today(7,8)
	pdate(3,4) = today(5,6)

displa,
	xcall outpt (1,1,2,'PURGE EXPIRED CUSTOMER CREDIT CARDS',1)
	xcall outpt (5,5,0, 'PURGE EXPIRED CUSTOMER CREDIT CARDS?',1)
	xcall input (5,45,01,01,'YN',entry,inxctl,1)
	goto (proces, endoff),inxctl
proces,

	ii = 0
loop,
	incr ii
	if (ii/500*500 .eq. ii) xcall outpt (1,70,1,dis,1)
	xcall ios (chn138, cctran, read, lokctl)
	if (lokctl .ne. 0) goto eof

	if (ct_order.lt.536530)goto loop
	if (ct_order.gt.800000) goto loop

	delete (chn138)
	goto loop
eof,
endoff,
	call close
	stop

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok
	clear chn138
	
	switch = 5
	xcall files (1, 'SU', 138, switch)
	if (switch .eq. 9) return
	chn138 = 1
	opnok = 1

	return
;--------------------------------------------------


close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (chn138) close chn138
	return
;--------------------------------------------------

