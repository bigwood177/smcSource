;d.dbl
;
record	dptdet
	.include 'def:rd198a.def'

record	out
	o_ord	,d6
		,a1
	o_dpt	,a2
		,a1
	o_dat	,a10
		,a1
	o_tim	,a5
		,a1
	o_op	,a5

;
record	vars
	order	,d6
	entry	,a30
	splfil	,a14,	'spl:dddddd.dat'
	xdate	,d8
;
proc
	open (15,i,'tt:')
	display (15, $scr_clr(screen), 'd.dbl')
	open (1,si,'smc:dptdet.smm')
	open (2,o,splfil)
;
	display (15, $scr_pos(4,4), 'order: ')
	reads (15, entry)
	order = entry(1,6)
	
	find (1, dptdet, order) [err=loop]
loop,
	reads (1, dptdet, eof)
	if (de_ord .ne. order) goto eof

	o_ord = de_ord
	o_dpt = de_dpt
	xdate(1,4) = de_date(5,8)
	xdate(5,8) = de_date(1,4)
	o_dat = xdate,	'XX/XX/XXXX'
	o_tim = de_time, 'ZX:ZX'

	using de_stat select
	(0),	o_op = 'open'
	(1),	o_op = 'burn'
	(2),	o_op = 'form'
	(3),	o_op = 'close'
	(),	o_op = '****'
	endusing

	writes (2, out)
	goto loop

eof,
	close 1
	close 2

	xcall spedit (splfil)
	stop
	end
