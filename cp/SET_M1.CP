;set_m1.cp
;
;	set mat'l mult in "M1" memo table
;
record	coptbl
	.include 'def:rd182a.def'
;
record	channels
	chn182	,d2

record	vars
	opnok	,d1
	i	,d6
	sst	,d4
	alu	,d4
	pgr	,d4

	switch	,d1
	v	,d1
;
proc
	xcall terid (v)
	xcall outpt (1,1,2,'SET NOTE 1 MATERIAL MULTIPLIER',1)
	call opens
	if (.not. opnok) goto endoff


	clear tbl_key
	tblcod = 'CF'
	read (chn182, coptbl, tbl_key) [err = no_cf]
	sst = sstmul
	alu = alumul
	pgr = pgrmul

	clear tbl_key
	tblcod = 'M1'
	find (chn182, coptbl, tbl_key) [err = loop]
loop,
	reads (chn182, coptbl, eof)
	if (tblcod .ne. 'M1') goto eof

	using m1_matl select
	(2),	m1_mult = alu
	(4),	m1_mult = sst
	(3),	m1_mult = pgr
	(),	m1_mult = 100
	endusing

	write (chn182, coptbl, tbl_key) [err=eof]
	goto loop


opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

	switch = 5
	xcall files (1, 'SU', 182, switch)
	if (switch .eq. 9) return

	chn182 = 1
	opnok = 1
	return
;--------------------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	if (chn182) close chn182

	return
;--------------------------------------------------------
no_cf,
	xcall mesag ('No General Config Parameters',1)
	goto endoff
no_m1,
	xcall mesag ('No Memo 1 Table',1)
eof,
endoff,
	call close
	stop
	end

