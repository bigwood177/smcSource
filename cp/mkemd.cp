subroutine mkemd
	ch182	,d		;channel for coptbl
;
record	empdep	;from timclock...
	.include 'def:rd086a.def'


record	coptbl
	.include 'def:rd182a.def'
	
record channels
	chn086	,d2
	chn182	,d2
	chnwrk	,d4

record	in
		,a1024
record	key
	emp	,a6
	dept	,a1
	job	,d5

record	vars
	opnok	,d1
	redfil	,a14
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
	if (.not. opnok) xreturn

	
	reads (chnwrk, in, eof)	;header
loop,
	reads (chnwrk, in, eof)
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
	store (chn086, empdep, emp) [err=exist]
	goto loop

exist,
	read (chn086, empdep, emp)
	delete (chn086)
	store (chn086, empdep, emp) 
	
	goto loop
	
	
eof,
	call close	
	xreturn
		
opens,
	clear opnok
	if (.not. %passed(ch182) )
	then	begin
		switch = 5
		xcall files (82, 'SI', 182, switch)
		if (switch .eq. 9) return
		chn182 = 82
		end
	else	chn182 = ch182

	open (chnwrk=0,i,wrkfil)

	xcall ffile (86, redfil, switch)
	redfil(14,14) = 'm'
	for chn086 from 1 thru 100
		begin
		if (.not. %chopen(chn086))
			begin
			open (chn086, su, redfil)
			exitloop
			end
		end

	open (10,o,'makemd.dat')
	
	
	opnok = 1
	return
;-------------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	close chnwrk
	close chn086
	close 10
	
	return
;-------------------------------------------------
	
end