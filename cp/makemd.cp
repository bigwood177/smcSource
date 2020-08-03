;makemd.cp
;
record	empdep	;from timclock...
	ed_emp	,a6	;employee code
	ed_dpt	,a1	;1st letter of dept

record	coptbl
	.include 'def:rd182a.def'
	
record channels
	chn085	,d2
	chn182	,d2
record	in
		,a1024
record
	wrkdat	,d8
record,x
	yy	,d4
	mm	,d2
	dd	,d2

record
	mon	,12d2,	31,28,31,30,31,30,31,31,30,31,30,31
record	key
	emp	,a6
	dept	,a1
	job	,d5

record	vars
	opnok	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	lokctl	,d1
	wrkfil	,a*,	'wrk:\time_clock_plus\timeclock.ddf'
	switch	,d1
	i	,d6
	numfld	,d6
	row	,d18
	com	,a1
	t1	,d6
	t2	,d6
	t3 	,d6
	t4	,d6
	s5	,d9
	fpos	,d6	;ending pos of last field
	field	,a15	;return field
	tdate	,d8	;yyyymmdd
	v	,d1
	
proc
	
	call opens
	
	open (1,i,wrkfil)
	open (10,o,'makemd.dat')
	open (2,su,'empdep.ism')
	reads (1, in, eof)	;header
loop,
	reads (1, in, eof)
	row = %trim(in)			;row is = number of characters per line
	if (row .le. 5) goto loop

	numfld = 4			;just need employee code (tc_exc) and job (tc_job)

	fpos = 0
	for i from 1 thru numfld
		begin
		xcall nxtfl (in, row, ',', fpos, field)
		using i select
		(1),	nop
		(2),	emp = field
		(3),	nop
		(4),	job = field
			endusing
		end

	if (job(1,2) .ne. 18) goto loop	;smc only
	
	clear dept
	clear tbl_key
	tblcod = 'D1'
	d1_job = job
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .ne. 0)
		begin
		writes (10, key)
		goto loop
		end

	dept = d1_dept(1,1)
	
	ed_emp = emp	;employee code
	ed_dpt = dept	;1st letter of dept
	store (2, empdep, emp) [err=exist]
	goto loop

exist,
	read (2, empdep, emp)
	delete (2)
	store (2, empdep, emp) 
	
	goto loop
	
	
eof,
		call close	
		stop
		
opens,
	clear opnok
	open (18, si, 'smc:coptbl.smm')
	chn182 = 18
	
	
	opnok = 1
	return
close,
	close chn182
	close 1
	close 2
	
