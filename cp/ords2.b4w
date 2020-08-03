no used anywhere...
subroutine ords2
;
; 7-26-17: revised version of ordsc.cp

; refreshed ordsch
;
record	neword
	.include 'def:rd088a.def'

;record	neword		;record added by prwod (dept copy)
;	new_dpt	,a2	;dept
;	new_dat	,d4	;mmddhh:mm
;	new_tim	,d4
;	new_ord	,d6	;order
;	new_stat,d1	;9=processed
;record,x
;	new_key	,a16
;

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

record	blhead
	.include 'def:rd178a.def'

record	dpt_key
	dk_ord	,d6
	dk_dpt	,a2


;
record	dptsts
	.include 'def:rd193a.def'

record	itmmas
	.include 'def:rd041a.def'

;
record	channel
	chn041	,d2
	chn044	,d2
	chn045	,d2
	chn087	,d2
	chn088	,d2
	chn175	,d2
	chn178	,d3
	chn193	,d2
	chntmp	,d2

record	vars
	opnok	,d1
	order_closed	,d1
	lbs	,d10
	wd_dept	,a2
	dept_lbs,d10
	duct_lbs,d10
	savdpt	,a2
	nxt_seq	,d4		;set at new_dept
	dpt_seq	,d4

	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	delete	,d1,3

	switch	,d1
	v	,d1

proc
	call opens
	if (.not. opnok) goto endoff
;

;=====================================================================

; remove processed orders from ordsch.smm.


	find (chn087, ordsch, ^first, krf=1) [err=loop]	;by dept/seq
loop,
	xcall ios (chn087, ordsch, read, lokctl)
	if (lokctl .ne. 0) goto eof


	xcall isio (chn044, ordhdr, sch_ord, read, lokctl)
	using lokctl select
	(2,3),		goto del_ord			;order gone
	(.ne. 0),	goto loop			;skip order
	endusing

	if (oloc .ne. 'O') goto del_ord		;make sure it's still an order

	call check_dptsts_closed
	goto (del_ord), order_closed

	call check_this_dept
	if (s_stat .eq. 3) goto del_ord		;this dept is completed
	goto loop

del_ord,
	delete (chn087)
	goto loop
eof,
;=====================================================================

;=====================================================================
; read thru the ORDHDR and either add or update ORDSCH records.


	find (chn044, ordhdr, ^first) [err=loop2]
loop2,
	call get_hdr
	if (lokctl .ne. 0) goto eof2

; process lines..
	clear dept_lbs, lbs
	savdpt = '-1'


	clear dpt_key
	dk_ord = oordno

	find (chn045, ordlin, dpt_key, krf=3) [err=lin_loop]	;read by dept/order
lin_loop,
	xcall ios (chn045, ordlin, read, lokctl)
	if (lokctl .ne. 0) goto eof_lin
	if (ltype .eq. 'M') goto lin_loop		;skip memos
	if (lordno .ne. oordno) goto eof_lin

	if (ldept .ne. savdpt) call new_dept
	
	dept_lbs = dept_lbs + (lqtyor * lpwgt)		;round (#2) at new_dept

	goto lin_loop
eof_lin,
	call new_dept
	call get_duct
	dept_lbs = duct_lbs
	savdpt = 'P'
	call new_dept		;add duct
	goto loop2					;next ordhdr

eof2,
endoff,
	call close
	xreturn
;=====================================================================
GET_HDR,
	XCALL IOS (CHN044, ORDHDR, READ, LOKCTL)
	IF (LOKCTL.NE.0) RETURN
	IF (OLOC.NE.'O') GOTO GET_HDR
;;;	IF (OORDDT .GT. SDATE) GOTO GET_HDR	;PAST THRU DATE	
	IF (OSHDTE .GT. 0) GOTO GET_HDR		;SHIP DATE 5-13-14

	LOKCTL = 1
	XCALL ISIO (CHN178, BLHEAD, OORDNO, READ, LOKCTL)
	if (lokctl .eq. 0)
		begin
		IF (BHSHPD .GT. 0) GOTO GET_HDR	;ALREADY SHIPPED
		end
	lokctl = 0				;return value

	SAVDPT = '-1'

	RETURN
;-------------------------------------------

new_dept,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (savdpt .eq. '-1') goto outdpt
	if (dept_lbs .eq. 0) goto outdpt

	call check_this_dept

	if (s_stat .eq. 3) goto outdpt		;dept completed


	clear sch_key
	sch_dpt = savdpt
	sch_ord = oordno

	xcall isio (chn087, ordsch, sch_key, read, lokctl)
	if (lokctl .ne. 0) goto add_new

	sch_lbs = sch_lbs + dept_lbs#2
	xcall isio (chn087, ordsch, sch_key, write, lokctl)
	goto outdpt

add_new,
	call find_next_seq		;get next seq for this dept

	sch_dpt = savdpt
	sch_ord = oordno

	sch_seq = nxt_seq
	sch_lbs = lbs#2
	sch_dat = oorddt(5,8)	;mm/dd
	sch_tim =
	using s_stat select
	(0),	sch_opr = 'O'
	(1),	sch_opr = 'B'
	(2),	sch_opr = 'F'
	(3),	sch_opr = 'C'
	endusing

	xcall isio (chn087, ordsch, sch_key, store, lokctl)
	
outdpt,
	clear lbs
	savdpt = ldept
	return
;----------------------------------------------------------------------

check_this_dept,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; is current dept still open?
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;is this dept still open?
	s_ordn = oordno
	s_dept = savdpt
	xcall isio (chn193, dptsts, s_key, read, lokctl)
	if (lokctl) clear dptsts		;assume not started yet

	return
;-----------------------------------------------------------------------

;=====================================================================================



;=====================================================================================
check_dptsts_closed,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; check DPTSTS header 
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	order_closed = 1				;assume closed
	s_ordn = sch_ord
	s_dept = '**'
	xcall isio (chn193, dptsts, s_key, read, lokctl)
	if (lokctl) return				;no record?
	if (s_date .eq. 0) order_closed = 0		;still open

	return
;---------------------------------------------------------------------


find_next_seq,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; find next open seq #
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	find (chn087, ordsch, savdpt, krf=1) [err=fns_loop]
fns_loop,
	xcall ios (chn087, ordsch, read, lokctl)
	if (lokctl .ne. 0) goto fns_eof
	if (sch_dpt .ne. savdpt) goto fns_eof
	nxt_seq = sch_seq			;keep track of last seq used
	goto fns_loop
fns_eof,
	incr nxt_seq

	return
;-----------------------------------------------------------------------

get_duct,
	clear duct_lbs

	using sch_dpt select
	('O','P'),	nop
	(),		return
	endusing

	find (chn175, ducacc, oordno) [err=gd_loop]
gd_loop,
	xcall ios (chn175, ducacc, read, lokctl)
	if (lokctl .ne. 0) goto gd_eof
	if (ductor .ne. oordno) goto gd_eof

	xcall ddept (gauge,size3,dutype,wd_dept,sty)
	if (wd_dept .ne. sch_dpt) goto gd_loop

	duct_lbs = duct_lbs + pounds

	goto gd_loop

gd_eof,

	return
;----------------------------------------------------




opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear opnok

	switch = 5
	xcall files (41,'SI',041, switch)
	if (switch .eq. 9) return
	chn041 = 41

	switch = 5
	xcall files (44,'SI',044, switch)
	if (switch .eq. 9) return
	chn044 = 44

	switch = 5
	xcall files (45, 'SI', 45, switch)
	if (switch .eq. 9) return
	chn045 = 45

	switch = 5
	open (87,su,'tst:ordsch.tsm')
;;;	xcall files (87, 'SU', 87, switch)
	if (switch .eq. 9) return
	chn087 = 87

	switch = 5
;;;	xcall files (88, 'SU', 88, switch)	;88 = ordnew
;;;	if (switch .eq. 9) return
;;;	chn088 = 88
;;;	open (88,su,'tst:neword.tsm')

	switch = 5
	xcall files (75, 'SI',175, switch)	;175 = ducass
	if (switch .eq. 9) return
	chn175 = 75

	switch = 5
	xcall files (93, 'SI', 193, switch)
	if (switch .eq. 9) return
	chn193 = 93

	switch = 5
	xcall files (178, 'SI', 178, switch)
	if (switch .eq. 9) return
	chn178 = 178

	open (6, o, 'spl:tmpsch')		;same record as ordsch.ism
	chntmp = 6


	opnok = 1
	return
;--------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (chn041) close chn041
	if (chn044) close chn044
	if (chn045) close chn045
	if (chn087) close chn087
	if (chn193) close chn193
	if (chntmp) close chntmp
	if (chn088) close chn088
	if (chn175) close chn175
	if (chn178) close chn178

	return
;--------------------------------------------

	end



