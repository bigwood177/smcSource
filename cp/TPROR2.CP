;TPROR2.CP - for once a day customers (cuscd = E2)
; made a subroutine to handle multiple companies...

subroutine tpror2
	cmpcod	,a	;company code to process...



;TPRORD.CP	- process time-stamped orders (ordtsf.ism)
;
;		add a new record to smc:orfile.dat
;		each smc:orfile.dat record contains the name of a temp file
;		of the latest batch of orders to email.
;

record	ordhdr
	.include 'def:rd044a.def'

; use same file as TPRORD...
record	orfile	;smc:orfile.dat' - add a new record to this file every hour
	or_file	,a17
	or_proc	,d1	;1 = processed


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
record	m_file	;record layout for records added to 'smc:orfile.dat'
	m_cmp	,a3	; Company Code
		,a1,	':'
	m_prd	,a3
	m_dt,	a10	; YYMMDDHHMM

;;;record	m_file	;record layout for records added to 'smc:orfile.dat'
;;;	m_cmp	,a3	; Company Code
;;;		,a2,	':M'
;;;	m_dt,	a12	; YYYYMMDDHHMM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

record	vars
	reccnt	,d6
;;;	cmpcod	,a3
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
	dt = %datetime
;;;	xcall who (cmpcod)
;;;	cmpcod = 'tst'


	reccnt =

	call opens

	find (chn079, oddtsf, ^first) [err=loop]
loop,
	reads (chn079, oddtsf, eof)
;;;	if (now .lt. tsfdt+100) goto loop

	call get_order
	call write_c2mail

	if (.not. upd_ok) goto loop

	delete (chn079)	;record has been processed, delete it.
	goto loop
eof,
	if (reccnt.gt.0)
		begin
		or_file = m_file
		or_proc = 0
		writes (ch_ofl, orfile)
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

	m_prd = cmpcod(1,1) + '2X'
	m_dt = now(3,12)

;;;	m_dt = now
	m_cmp = cmpcod
	xcall isamc (m_file, 34, 1, key_spec)
	open (1, su, m_file)
	ch_mfl = 1

	using cmpcod select
	('TST'),	begin
			open (2, a, 'tst:orfile.dat')
			open (4, su, 'tst:oddts2.tsm')
			open (5, si, 'tst:ordhdr.tsm')
			end

	('SMC'),	begin
			open (2, a, 'smc:orfile.dat')
			open (4, su, 'smc:oddts2.smm')
			open (5, si, 'smc:ordhdr.smm')
			end

	('ROC'),	begin
			open (2, a, 'roc:orfile.dat')
			open (4, su, 'roc:oddts2.rom')
			open (5, si, 'roc:ordhdr.rom')
			end

	('CAT'),	begin
			open (2, a, 'cat:orfile.dat')
			open (4, su, 'cat:oddts2.cam')
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
	close ch_mfl
	close ch_ofl
	close chn079
	close chn044

	return
;----------------------------------------------------

end



