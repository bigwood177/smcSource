.function mail_cnt
	cust	,d
	chan	,d	;emailc.ism


record	emailc
	.include 'def:rd084a.def'

record	vars
	dec	,d18
	hi_val	,d6
.proc
	dec = 0
	hi_val = 0

	clear emailc
	e_cust = cust
	find (chan, emailc, e_key) [err=loop]
loop,
	reads (chan, emailc, eof) [err=eof]
	if (e_cust .ne. cust) goto eof
	incr dec
	if (e_nbr .gt. hi_val) hi_val = e_nbr
	goto loop
eof,
	if (hi_val .gt. dec) dec = hi_val
	unlock chan			;10-11-16 ssq

	freturn dec
.end
