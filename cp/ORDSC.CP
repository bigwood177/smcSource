subroutine ordsc
	cmpcod	,a		;company code added 3-21-18
;
; refreshed ordsch
;
; 10-22-18 ssq: problem w/ ordsch records getting deleted erroneously because:
;	a) order changed to quote, then back to order
;	b) dept status changed to complete and then back to open
; new field ordsch.sch_cpl added

; 12-5-18 ssq: add fab tickes to sched.
; 5-31-19 ssq: open & protect ordsch to avoid chknew colliding with ordsch
; 7-22-19 ssq: no more "V" depts...
; 6-17-20: ordlin.lpwnw = 1 => ordlin.lpwgt is XXX,XXX instead of X,XXX.XX

record	o_buffer		;for smc:ordsc.smc - lock file
		,a250

record	n_err			;problem w/ sch_typ not getting set
	e_ord	,d6
		,a1
	e_note	,a30

record	jobhdr
	.include 'def:rd071a.def'

record	joblin
	.include 'def:rd072a.def'

record	neword
	.include 'def:rd088a.def'

record	ordsch
	.include 'def:rd087a.def'
;
record	ordhdr
	.include 'def:rd044a.def'
;
record	ordlin
	.include 'def:rd045a.def'

record	ducacc
	.include 'def:rd175a.def'

record	dpt_key
	dk_ord	,d6
	dk_dpt	,a2


;
record	dptsts
	.include 'def:rd193a.def'

record	itmmas
	.include 'def:rd041a.def'

;
RECORD	KEYSP
	W_KEY1	,A*,	'START=1,LENGTH=8'
	W_KEY2	,A*,	'START=1:9,LENGTH=2:4'

record	channel
	chn041	,d2
	chn044	,d2
	chn045	,d2
	chn182	,d2
	chn087	,d2
	chn088	,d2
	chn175	,d2
	chn193	,d2
	chn071	,d2
	chn072	,d2
	chntmp	,d2

record	vars
	o_sts	,d1	;1=skip, 2=mark
	opnok	,d1
	x	,d10
	is_vulcan	,d1
	lbs	,d10
	wd_dept	,a2
	dept_lbs,d10		;zzzzzz.xx
	duct_lbs,d10
	savdpt	,a2
	nxt_seq	,d4		;set at newdpt
	dpt_seq	,d4

	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	delete	,d1,3

	switch	,d1
	v	,d1

proc
;;;	open (22,a,'smc:n_err.err')	;temp debug order type

	call opens
	if (.not. opnok) goto endoff
;

	clear nxt_seq

; write existing shedule records to work file
; only skip records w/ no order header (presumibly invoiced)

	find (chn087, ordsch, ^first, krf=1) [err=loop]	;by dept/seq
loop,
	lokctl = 1
	xcall ios (chn087, ordsch, read, lokctl)
	if (lokctl .ne. 0) goto eof


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 12-5-18 add jobs to ordsch

	call check_order			;check orders/jobs
	using o_sts select
	(1),	goto loop			;skip
	(2), 	goto mark			;mark
	endusing

;;;	xcall isio (chn044, ordhdr, sch_ord, read, lokctl)
;;;	sch_cpl = 0				;clear ignore flag
;;;	if (lokctl .ne. 0) goto loop		;order gone
;;;	if (oloc .ne. 'O') goto mark		;make sure it's still an order
;;;	if (oshdte .gt. 0) goto mark		;shipped, therefor not open 8-7-17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	call check_dpt_status			;12-5-18 add jobs to ordsch

	if (s_stat .eq. 3) goto mark

	using sch_typ select			;12-5-18 add jobs to ordsch
	('O'),	begin
		call get_lines			; total all lines for current dept
		call get_duct
		end

	('J'),	call get_job_lines
	endusing


	call round_lbs_up			; always round pounds up	9-11-17
	
	sch_lbs = dept_lbs#2 + duct_lbs		; total lbs this dept, current order 8-7-17: round here instead of at get_lbs

	sch_dp1(2,2) = 				; 8-2-17 only using first char of dept

store,
	using sch_typ select
	('O','J'),	nop
	(),		begin
		;;;	clear n_err
		;;;	e_ord = sch_ord
		;;;	e_note = 'Building Temp File'
		;;;	writes (22, n_err)
			sch_typ = 'O'		;1-9-19 can't figure out why this is not set!!
			end
	endusing

	lokctl = 1				; no abort option
	xcall isio (chntmp, ordsch, sch_key, store, lokctl)
	if (lokctl .ne. 0) call debug

	goto loop
mark,
	sch_cpl = 9		;complete, other programs to ignore this record
	goto store

debug,
	x = 1
	return
eof,

; now we need to read thru the NEWORD file to pick up any orders
; added since the last refresch...
; we may duplicate orders already in ORDSCH, but we'll take care of that later... 


	savdpt = '-1'

	nxt_seq = 8000			;assume won't add more that 2000 records

	find (chn088, neword, ^first) [err=loop2]
loop2,
	xcall ios (chn088, neword, read, lokctl)
	if (lokctl .ne. 0) goto eof2

	using new_type select
	('O'),	begin
		xcall isio (chn044, ordhdr, new_ord, read, lokctl)
		if (lokctl .ne. 0) goto mark_done	;make sure order is still there
		if (oloc .ne. 'O') goto mark_done	;not a quote
		end
	('J'),	begin
		xcall isio (chn071, jobhdr, new_ord, read, lokctl)
		if (lokctl .ne. 0) goto mark_done	;make sure jobs hasn't been deleted
		if (jhsts .ne. 'P') goto mark_done	;must have been printed but not yet received
		end
	endusing


	clear ordsch
	sch_cpl = 0			;1-2-19
	sch_ord = new_ord
	sch_dpt = new_dpt
	if (sch_dpt(2,2) .eq. 'V') sch_dpt(2,2) = 	;7-22-19
	sch_dp1 = new_dpt(1,1)		;8-2-17 first char of dept
	sch_typ = new_type		;12-27-18

	call check_dpt_status			;is dept still open?
	if (s_stat .eq. 3) goto mark_done

	sch_dat = new_dat
	sch_tim = new_tim

	using sch_typ select			;12-5-18 add jobs to ordsch
	('O'),	begin
		call get_lines			; total all lines for current dept
		call get_duct
		end

	('J'),	call get_job_lines		;12-5-18 get job lines
	endusing

;;;	call get_lines				; total all lines for current dept
;;;	call get_duct

	call round_lbs_up			; always round pounds up	9-11-17
	sch_lbs = dept_lbs#2 + duct_lbs		; total lbs this dept, current order

	incr nxt_seq			; next seq for current dept
	sch_seq = nxt_seq		; force new records to end of file

	using sch_typ select
	('O','J'),	nop
	(),		begin
		;;;	clear n_err
		;;;	e_ord = sch_ord
		;;;	e_note = 'From Neword'
		;;;	writes (22, n_err)
			sch_typ = 'O'	;1-9-19 can't figure out why this is not set!
			end
	endusing
	lokctl = 1
	xcall isio (chntmp, ordsch, sch_key, store, lokctl)
;debug
;	new_stat = lokctl
;;;	writes (22, neword)

mark_done,
	new_stat = 9				;processed
	xcall isio (chn088, neword, new_key, write, lokctl)
	
	goto loop2

eof2,
	find (chn087, ordsch, ^first) [err=loop3]
loop3,
	do forever			; clear the schedule file...
		begin
		lokctl = 1
		xcall ios (chn087, ordsch, read, lokctl)
		if (lokctl .ne. 0) exitloop
		delete (chn087)
		end

	close chntmp
	open (6, si, 'spl:tmpsch.ism')		;same record as ordsch.ism

; now, reload ORDSCH...

	find (chntmp, ordsch, ^first) [err=loop4]
loop4,
	savdpt = '-1'
	do forever
		begin
		xcall ios (chntmp, ordsch, read, lokctl)
		if (lokctl .ne. 0) exitloop
		if (sch_dp1 .ne. savdpt)
			begin
			savdpt = sch_dp1
			nxt_seq = 0
			end

		incr nxt_seq
		sch_seq = nxt_seq

	using sch_typ select
	('O','J'),	nop
	(),		begin
		;;;	clear n_err
		;;;	e_ord = sch_ord
		;;;	e_note = 'reload ORDSCH'
		;;;	writes (22, n_err)
			sch_typ = 'O'		;1-9-19 can't figure out why this is not set!
			end
	endusing

		lokctl = 1
		xcall isio (chn087, ordsch, sch_key, store, lokctl)
		end
eof4,

; remove processed records from neword
	find (chn088, neword, ^first) [err=loop5]
loop5,
	do forever
		begin	
		xcall ios (chn088, neword, read, lokctl)
		if (lokctl .ne. 0) exitloop
		if (new_stat.eq.9) xcall isio (chn088, neword, new_key, delete, lokctl)
		end



endoff,
	call close
;	stop

	xreturn

;=====================================================================================

check_order,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; check both orders and jobs
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	sch_cpl = 0				;clear ignore flag
	o_sts = 0

	IF (SCH_TYP .EQ. 'J') GOTO C_JOB

	LOKCTL = 1
	XCALL ISIO (CHN044, ORDHDR, SCH_ORD, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO C_SKIP		;order gone
	IF (OLOC .NE. 'O') GOTO C_MARK		;make sure it's still an order
	IF (OSHDTE .GT. 0) GOTO C_MARK		;shipped - therefor not open	
	RETURN
C_JOB,
	LOKCTL = 1
	XCALL ISIO (CHN071, JOBHDR, SCH_ORD, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO C_SKIP		;job gone
	IF (JHSTS .EQ. 'R') GOTO C_SKIP		;recieved
	RETURN


C_SKIP,
	o_sts = 1	;don't save this ORDSCH record
	RETURN

C_MARK,
	o_sts = 2	;mark this ORDSCH records as closed
	RETURN
;-----------------------------------------------------------	


;-----------------------------------------------------------------
round_lbs_up,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 9-11-17: round up to nearest pound, eg: .46 = 1.00

	x = dept_lbs#2 *100
	dept_lbs = dept_lbs  - x
	if (dept_lbs .gt. 0)
	then	dept_lbs = x + 100		;add 1.00 lbs
	else	dept_lbs = x

	return
;-----------------------------------------------------------------

check_dpt_status,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; is dept complete (s_stat-=3) ?
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	s_ordn = sch_ord
	s_dept = sch_dpt

	using s_dept select
	('H','F','K'),	begin
			if (s_dept(2,2) .ne. 'V') s_dept(2,2) = 
			end
	endusing

	xcall isio (chn193, dptsts, s_key, read, lokctl)
	if (lokctl)
		begin
		if(s_ordn.ne.sch_ord .or. s_dept(1,1).ne.sch_dpt)
			begin
			 clear dptsts		;assume not started yet
			 sch_opr = 'O'		;open
			end
		end
; until all "V' gone from system 7-24-19
;;;	if (lokctl)
;;;		begin
;;;		 clear dptsts		;assume not started yet
;;;		 sch_opr = 'O'		;open
;;;		end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	using s_stat select
	(0),	sch_opr = 'O'
	(1),	sch_opr = 'B'
	(2),	sch_opr = 'F'
	(3),	sch_opr = 'C'
	endusing

	return
;-----------------------------------------------------------------------


get_lines,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; read all lines in for current order in current dept
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear dept_lbs, lbs

	dk_ord = sch_ord
	dk_dpt = sch_dp1

	find (chn045, ordlin, dpt_key, krf=3) [err=lin_lop]	;read by dept/order
lin_lop,
	xcall ios (chn045, ordlin, read, lokctl)
	if (lokctl .ne. 0) goto eof_lin
	if (ltype .eq. 'M') goto lin_lop		;skip memos
	if (lordno .ne. sch_ord) goto eof_lin

	if (ldept(1,1) .ne. sch_dp1) goto eof_lin
	

	lbs = (lqtyor * lpwgt) 	;>8-7-17 >>#2	;6-17-17
	if (lpwnw) lbs = lbs * 100		;6-22-20 ZZZ,ZZX instead of Z,ZZX.XX

	if (lbs .gt.0)	dept_lbs = dept_lbs + lbs	;8-9-17: skip discounts & credits

	goto lin_lop


eof_lin,
	return
;-----------------------------------------------------------------------------
	
get_job_lines,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; read all lines for current job
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	clear dept_lbs, duct_lbs, lbs
	if (jhdept(1,1) .ne. sch_dp1) return		;nothing for current dept

	find (chn072, joblin, jhnum) [err=jl_loop]
jl_loop,
	xcall ios (chn072, joblin, read, lokctl)
	if (lokctl .ne. 0) goto eof_jl
	if (jlnum .ne. jhnum) goto eof_jl
	
	lbs = jlqty * jlwgt
	dept_lbs = dept_lbs + lbs
	goto jl_loop

eof_jl,
	return
;----------------------------------------------------


get_duct,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear duct_lbs

	using sch_dpt select
	('O','P'),	nop
	(),		return
	endusing

	find (chn175, ducacc, sch_ord) [err=gd_loop]
gd_loop,
	xcall ios (chn175, ducacc, read, lokctl)
	if (lokctl .ne. 0) goto gd_eof
	if (ductor .ne. sch_ord) goto gd_eof

	xcall ddept (gauge,size3,dutype,wd_dept,sty)
	if (wd_dept .ne. sch_dpt) goto gd_loop

	duct_lbs = duct_lbs + pounds

	goto gd_loop

gd_eof,

	return
;----------------------------------------------------


opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear opnok

	clear chn087		;just in case...

	switch = 5
	xcall filec (cmpcod, 41,'SI',041, switch)
	if (switch .eq. 9) return
	chn041 = 41

	switch = 5
	xcall filec (cmpcod, 44,'SI',044, switch)
	if (switch .eq. 9) return
	chn044 = 44

	switch = 5
	xcall filec (cmpcod, 45, 'SI', 45, switch)
	if (switch .eq. 9) return
	chn045 = 45

	switch = 5		
	xcall filec (cmpcod, 87, 'SU', 87, switch)
	if (switch .eq. 9) return
	chn087 = 87

	switch = 5
	xcall filec (cmpcod, 88, 'SU', 88, switch)	;88 = ordnew
	if (switch .eq. 9) return
	chn088 = 88

	switch = 5
	xcall filec (cmpcod, 75, 'SI',175, switch)	;175 = ducass
	if (switch .eq. 9) return
	chn175 = 75

	switch = 5
	xcall filec (cmpcod, 93, 'SI', 193, switch)
	if (switch .eq. 9) return
	chn193 = 93

	XCALL ISAMC('spl:tmpsch.ism', 40, 1, W_KEY2)		;1 key: dept/seq
	open (6, su, 'spl:tmpsch.ism')		;same record as ordsch.ism
	chntmp = 6

	switch = 5
	xcall filec (cmpcod, 71, 'SI', 71, SWITCH)		;71- jobhdr
	if (switch .eq. 9) return
	chn071 = 71

	switch = 5
	xcall filec (cmpcod, 72, 'SI', 72, SWITCH)		;72 - joblin
	if (switch .eq. 9) return
	chn072 = 72

	opnok = 1
	return
;--------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	close 22

	if (chn041) close chn041
	if (chn044) close chn044
	if (chn045) close chn045
	if (chn087) close chn087
	if (chn193) close chn193
	if (chntmp) close chntmp
	if (chn088) close chn088
	if (chn175) close chn175
	if (chn071) close chn071
	if (chn072) close chn072

	return
;--------------------------------------------

	end



