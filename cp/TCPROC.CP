;tcproc.cp
;
;	process timeclock data.

record	timeck
	.include 'def:rd085a.def'

record	buf
		,a1024

record	vars
	i	,d6
	today	,d8
	numfld	,d6
	tl	,d6
	nl	,d6
	com	,a1
	fpos	,d6
	field	,a20
	
proc
	xcall rdat8(today)

	open (1,i,'tst:timeclock.ddf')
	open (2,su,'tst:timeck.tsm')
;
	numfld = 11

	reads (1,buf, eof)		;header
loop,
	reads (1,buf, eof)
	tl = %trim (buf)
	if (tl .le. 12) goto loop

	clear timeck
	onerror bad_field

	fpos = 0
	for i from 1 thru numfld
		begin
		xcall nxtfl (buf, tl, ',', fpos, field)
		nl = %trim(field)
		if (nl.gt.1)
		  begin
		    using i select
		    	(1),	tc_nbr = field(1,nl)
		    	(2),	tc_exc = field
		    	(3),	tc_ssn = field
			(4),	tc_job = field(1,nl)
			(5),	tc_rte = field(1,nl)
			(6),	tc_reg = field(1,nl)
			(7),	tc_ot1 = field(1,nl)
			(8),	tc_ot2 = field(1,nl)
			(9),	tc_tk1 = field(1,nl)
			(10),	tc_tk2 = field(1,nl)
			(11),	tc_tk3 = field(1,nl)
		  endusing
		  offerror
		  end

		end

	tc_dat = today
	store (2, timeck, tc_key)
bad_field,
	offerror
	goto loop
eof,
	close 1
	close 2
	stop
	end

