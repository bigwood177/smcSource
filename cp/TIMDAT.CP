;timdat.dbl
;tcxfr.dbl
;
;	author: Brandon Korus
;	date: 1/29/14
;
record	in
		,a1024
record	coptbl
	.include 'def:rd182a.def'

record 	smcdata
	.include 'def:rd085a.def'

record channels
	chn085	,d2
	chn182	,d2

record
	wrkdat	,d8
record,x
	yy	,d4
	mm	,d2
	dd	,d2

record
	mon	,12d2,	31,28,31,30,31,30,31,31,30,31,30,31

record	vars
	opnok	,d1
	jobno	,d5
;;;	wrkfil	,a*,	'wrk:\time_clock_plus\timeclock.ddf'
	wrkfil	,a*,	'wrk:\time_clock_plus\timeclock.txt'	;8-26-19
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


proc
;;;	open (1, i, 'tst:timeclock')	;opens timeclock.ddf

	call opens
	if (.not.opnok) goto eof

	xcall tdate (tdate)
	call yesterday

	numfld = 8			;number of fields in buffer
	reads (1, in, eof)
loop,
	reads (1, in, eof)
	row = %trim(in)			;row is = number of characters per line
	if (row .le. 5) goto loop

	fpos = 0
	for i from 1 thru numfld
		begin
		xcall nxtfl (in, row, ',', fpos, field)
		using i select
		(1),	tc_nbr = field
		(2),	tc_exc = field
		(3),	tc_ess = field
		(4),	jobno = field
		(5),	tc_rate = field
		(6),	tc_ehrs = field
		(7),	tc_eovt = field
		(8),	tc_edblo = field
		endusing
		end

	if (jobno(1,2) .ne. 18) goto loop	;smc only

	clear coptbl
	tblcod = 'D1'
	d1_job = jobno
	read (chn182, coptbl, tbl_key) [err=no_dept]
	tc_dpt = d1_dept
	goto dept_ok
no_dept,
	tc_dpt = 'XX'
dept_ok,
	tc_jdate = tdate
	dk_dat = tc_jdate
;;;	dk_job = tc_job
	dk_dpt = tc_dpt
	dk_nbr = tc_nbr
	store (chn085, smcdata, date_key)
	goto loop
eof,
	close 1
	call close
	xcall flags(7000000)
	stop ;;>'timrpt'

;=======================================================================
yesterday,	;;;;;;;;;;;;;;;;;;;;;;;

	wrkdat = tdate
	dd = dd - 1
	if (dd .eq. 0)
	  begin
	  mm = mm - 1
	  if (mm.eq.0)
		begin
		mm = 12
		yy = yy -1
		end
	  dd = mon(mm)
	  end
	tdate = wrkdat

	return
;--------------------------------------


opens,
	opnok = 0
	switch = 5
	
	xcall files(2,'SU',85,switch)
	if(switch.eq.9) return
	chn085 = 2

	switch = 5
	xcall files (17,'SI',182,switch)
	if (switch .eq. 9) return
	chn182 = 17

	onerror no_file
	open (1, i, wrkfil)		;opens last export file

	opnok = 1
no_file,
	offerror
	return

close,
	if(chn085) close chn085
	if(chn182) close chn182
	xcall delet (wrkfil)
	return

end
