.function	schdt
	order	,d


; Determin promise date by checking schedule

record	ordlin
	.include 'def:rd045a.def'
;
record	itmmas
	.include 'def:rd041a.def'

record	coptbl
	.include 'def:rd182a.def'

record	ordsch
	.include 'def:rd087a.def'

record arrays
	a_dept	,20a2
	a_lbs	,20d8	;this order
	a_cap	,20d8	;capacity in lbs
	a_used	,20d8	;used - excluding this order

record	channels
	chn041	,d3
	chn045	,d3
	chn087	,d3
	chn182	,d3

record	vars
	opnok	,d1
	not_schd	,d1	;1 = this dept not scheduled
	nodays	,d6		;greatest number of days out
	temp	,d6
	dept_lbs	,d8	;lbs this order this dept
	dept_used	,d8	;lbs already scheduled
	dept_cap	,d8	;capacity in lbs
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	pounds	,d8
	savdpt	,a2
	numdpt	,d6
	switch	,d1

proc

	call opens
	if (.not. opnok) goto endoff



; read thru ordlin by dept,
; get total lbs for each dept,
; compute days out for each dept, and
; set nodays to the most days out.

	clear nodays, temp

	savdpt = '-1'

	find (chn045, ordlin, order, krf=3) [err=loop]
loop,
	xcall ios (chn045, ordlin, read, lokctl)
	if (lokctl .ne. 0) goto eof
	if (lordno .ne. order) goto eof
	if (ltype .eq. 'M') goto loop
	if (ldept .ne. savdpt) call newdpt
	if (not_schd) goto loop			;this dept not scheduled
	call get_lbs
	dept_lbs = dept_lbs + pounds
	goto loop

newdpt,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (savdpt .eq. '-1') goto outdpt


	call get_used				;how many lbs already sched?

	clear temp
	if (dept_cap .gt. 0) temp = (dept_used + dept_lbs)/dept_cap + 1
	if (temp .gt. nodays) nodays = temp
	
outdpt,
	not_schd = 0		;assume dept is sched

	clear dept_lbs		;total lbs this order, this dept
	clear dept_used		;total lbs already scheduled

	savdpt = ldept

	clear tbl_key
	tblcod = 'DD'
	tblkey = savdpt		;new dept
	xcall isio (chn182,coptbl,tbl_key,read,lokctl)
	if (lokctl .ne. 0) clear coptbl
	if (dd_sch .ne. 1) 
		begin
		not_schd = 1		;nope
		return
		end
	
	dept_cap = dd_cap * dd_cnv	;cap in lbs
	
	

	return
;------------------------------------------------------

;======================================================

get_lbs,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear pounds

	xcall isio (chn041, itmmas, litmno, read, lokctl)
	if (lokctl .ne. 0) clear itmmas

	pounds = (lqtyor * ip_wgt)#2

	return
;----------------------------------------------------

get_used,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear dept_used

	clear ordsch
	sch_dpt = savdpt
	find (chn087, ordsch, sch_key) [err=gu_loop]
gu_loop,
	xcall ios (chn087, ordsch, read, lokctl)
	if (lokctl .ne. 0) goto gu_eof
	if (sch_dpt .ne. savdpt) goto gu_eof
	if (sch_ord .eq. order) goto gu_loop	;in case already schd

	dept_used = dept_used + sch_lbs
	goto gu_loop

gu_eof,
	return
no_sch,
	return
;----------------------------------------------------
eof,
	call newdpt
endoff,
	call close
	freturn nodays

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	opnok = 0
	clear channels
	
	switch = 5
	xcall files (45, 'SI', 45, switch)
	if (switch .eq. 9) return
	chn045 = 45

	switch = 5
	xcall files (87, 'SI', 87, switch)
	if (switch .eq. 9) return
	chn087 = 87

	chn041 = 1		;already opened in input mode
	chn182 = 17		;already opened in input mode

	opnok = 1
	return
;----------------------------------------------------	

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (chn045) close chn045
	if (chn087) close chn087

	return
;----------------------------------------------------	

.end

