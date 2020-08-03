;prgcc.cp
;
;	purge expired credit card records from CCINFO.ISM.

record	ccinfo
	.include 'def:rd137a.def'

record	channel
	chn137	,d2

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
	pdate(1,2) = today(3,4)	;year
	pdate(3,4) = today(5,6)	;month

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
	xcall ios (chn137, ccinfo, read, lokctl)
	if (lokctl .ne. 0) goto eof

	xdate(1,2) = ci_exdat(3,4)
	xdate(3,4) = ci_exdat(1,2)

	if (xdate .lt. pdate) delete (chn137)
	goto loop
eof,
endoff,
	call close
	stop

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok
	clear chn137
	
	switch = 5
	xcall files (1, 'SU', 137, switch)
	if (switch .eq. 9) return
	chn137 = 1
	opnok = 1

	return
;--------------------------------------------------


close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (chn137) close chn137
	return
;--------------------------------------------------

