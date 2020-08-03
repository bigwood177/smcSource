subroutine tprord
	cmpcod	,a



; 2-4-13: now a subroutine.
;TPRORD.CP	- process time-stamped orders (ordtsf.ism)
;
;		add a new record to smc:orfile.dat
;		each smc:orfile.dat record contains the name of a temp file
;		of the latest batch of orders to email.
;

record	ordhdr
	.include 'def:rd044a.def'

record	orfile	;smc:orfile.dat' - add a new record to this file every hour
	or_file	,a17
	or_proc	,d1	;1 = processed


record	del_ism		;for deleting empty files
		,a17
		,a1	,'.'
	is_ext	,a3

record	m_file,x	;record layout for records added to 'smc:orfile.dat'
	m_cmp	,a3	; Company Code
	m_col	,a1	;,	':'
	m_prd	,a3
	m_dt,	a10	; YYMMDDHHMM



record	c2mail	; format  of records written to M_FILE
	.include 'def:c2mail.def'

record	oddtsf	;time stamp records created by PDORD
	.include 'def:rd079a.def'

record	channel
	chn044	,d2
	chn079	,d2	;smc:oddtsf.ism
	ch_ofl	,d2	;smc:orfile.dat'
	ch_mfl	,d2	;smc:MYYYYMMDDHHMM.DDF

record	
	dt	,a20	;yyyymmddhhmmss
record,x
	now	,d12

record	err_file
	er_cmp	,a3
		,a1, ':'
		,a10,	'tprord.err'

record	err_msg
	em_cmp	,a3
		,a1
	errmsg	,a40

record	vars
	reccnt	,d6
;;;	cmpcod	,a3
	wrkfil	,a14,	'smc:podwrk.ism'
	key_spec	,a*,	'START=1, LENGTH=15, NODUPS, ASCEND'
	upd_ok	,d1
	opnok	,d1
	i	,d6
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	delete	,d1,3
	switch	,d1
	v	,d1

proc
	onerror catch_all


	dt = %datetime
;;;	xcall who (cmpcod)

	clear reccnt

	call opens

	find (chn079, oddtsf, ^first) [err=loop]
loop,
	reads (chn079, oddtsf, eof)
	if (now .lt. tsfdt+100) goto loop

	call get_order
	call write_c2mail

	if (.not. upd_ok) goto loop

	delete (chn079)	;record has been processed, delete it.
	goto loop
eof,
	close ch_mfl

	if (reccnt.gt.0)
	then	begin
		or_file = m_file
		or_proc = 0
		writes (ch_ofl, orfile)
		end
	else	begin
		is_ext = 'ism'
		xcall delet (del_ism)
	
		is_ext = 'is1'
		xcall delet (del_ism)

		end

	call close

	xreturn


;;;	stop
;;;	xcall flags (7000000)
;;;	stop	'cp:mailor'

;======================================================
get_order,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear upd_ok
	read (chn044, ordhdr, tsford) [err=no_hdr]
	
	upd_ok = 1		;at least this far
	return
no_hdr,
	clear ordhdr
	return
;-----------------------------------------------------

write_c2mail,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (.not. upd_ok) return
	clear c2mail
	
	c2_cust = ocusno
	c2_nbr = oc_nbr
	c2_order = oordno
	c2_amount = osale
	c2_date = oorddt
	c2_flag = 0

	store (ch_mfl, c2mail, c2_key) [err=dup]
	incr reccnt
dup,						;only store the firt occurance

	return
;-----------------------------------------------------

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

	m_prd = 'T' + cmpcod(1,1) + 'X'
	m_dt = now(3,12)
;;;	m_dt = now
	m_cmp = cmpcod
	m_col = ':'

	xcall isamc (m_file, 34, 1, key_spec)
	open (1, su, m_file)
	ch_mfl = 1

	using cmpcod select
	('TST'),	begin
			open (2, a, 'tst:orfile.dat')
			open (4, su, 'tst:oddtsf.tsm')
			open (5, si, 'tst:ordhdr.tsm')
			end

	('SMC'),	begin
			open (2, a, 'smc:orfile.dat')
			open (4, su, 'smc:oddtsf.smm')
			open (5, si, 'smc:ordhdr.smm')
			end

	('ROC'),	begin
			open (2, a, 'roc:orfile.dat')
			open (4, su, 'roc:oddtsf.rom')
			open (5, si, 'roc:ordhdr.rom')
			end

	('CAT'),	begin
			open (2, a, 'cat:orfile.dat')
			open (4, su, 'cat:oddtsf.cam')
			open (5, si, 'cat:ordhdr.cam')
			end

	endusing

	ch_ofl = 2

	chn079 = 4

	chn044 = 5

	opnok = 1
	return
;----------------------------------------------------


close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	close ch_mfl
	close ch_ofl
	close chn079
	close chn044

	return
;----------------------------------------------------

catch_all,
	offerror
	em_cmp = cmpcod
	errmsg = %ernum, 'Error XXX: '
	xcall ertxt (%ernum, errmsg(12,40))

	er_cmp = cmpcod
	open (97, A, err_file)
	writes (97, err_msg)
	close 97

	xreturn
end


