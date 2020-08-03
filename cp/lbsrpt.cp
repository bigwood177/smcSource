;lbsrpt.cp

SUBROUTINE LBSRPT
	ME	,D		;1 = running from autome
	PROW	,D				;0 = from coprpt
	MEDATE	,D
	SPLFIL	,A

record	coptbl
	.include 'def:Rd182a.def'

record	datlbs
	.include 'def:rd063a.def'

record	timdat
	.include 'def:rd085a.def'

;

record	totals
	m_lbs	,d10
	m_hrs	,d10	;ZZ,ZZZ,ZZZ.XX
	y_lbs	,d10
	y_hrs	,d10
	lm_lbs	,d10
	lm_hrs	,d10	;ZZ,ZZZ,ZZZ.XX
	ly_lbs	,d10
	ly_hrs	,d10

RECORD TITLE
		,A30,	'POUNDS AND HOURS BY DEPARTMENT'
		,A1
	TDS	,A7

RECORD HDR1
		,A3
		,A25
		,A15,   '-------------- '
		,A11,	'M-T-D THRU '
	MNTH	,A3
		,A1
	YEAR	,A4
		,A15,	'--------------'    
		,A3
		,A28,	'--------------- Y-T-D THRU '
	L_MNTH	,A3
		,A1
	L_YEAR	,A4 
		,A13,	' ------------'
RECORD HDR2
		,a*,	'                            --------CURRENT--------  -------LAST YEAR-------    '
		,a*,	'--------CURRENT--------   -------LAST YEAR-------'

RECORD	HDR3
		,a*,	'   DEPT                            LBS  HRS     AVG         LBS   HRS     AVG'
		,a*,	'          LBS  HRS     AVG         LBS   HRS     AVG'


RECORD	COMPRES
	C_CHAR	,A1		;<ESC>
		,A4,	"&k2S"	;pitch mode = 2 = 16.5-16.7 (compressed)

RECORD	COMP14
	ESC_11	,A1		;<ESC>
		,a4,	"&l1O"	;landscape mode
	ESC_12	,A1		;<ESC>
		,A4,	"&l8D"	;vertical spacing, 8 lines/inch
	ESC_13	,A1		;<ESC>
	C14V	,A8,	"(s13.00H"	;pitch 14 CPI

RECORD	PRINT
	LINCNT	,D2,	60
;;;	SPLFIL	,A14
	LPONSW	,D1
	LPARG	,D1
	RPTNUM	,D3
	PRTTYP	,A1
	PGCNT	,D6,	000000
	PLINE	,A132
	PRTCTL	,D3
	LPSW	,D2
	PRNTSW	,D1

record	out
	o_sdat	,a10
		,a1
	o_edat	,a10
		,a1
	o_lbs	,a8
		,a1
	o_hrs	,a10
		,a1
	o_lh	,a8

record
	xdate	,d8
record,x
	yyyy	,d4
	mm	,d2
	dd	,d2

record
	month	,12a3,	'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'

record	param
	endat	,d8
	savdpt	,a2
	savdsc	,a30
	xstdat	,d8
	xendat	,d8
	lystr	,d8
	lyend	,d8

record	channels
	chn063	,d2
	chn085	,d2
	chn182	,d2

record	vars
	opnok	,d1
	detsum	,a1
	temp	,d18
	avg	,d10
	w_emp	,d6		;number of employees this dept
	m_emp	,d6		;number of employees this dept
	q_emp	,d6
	emp_ara	,100d3		;array of employees this dept
	i	,d6
	days	,12d2,	31,28,31,30,31,30,31,31,30,31,30,31
;;;	splfil	,a14,	'spl:timlbs.spl'
	entry	,a30
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	inxctl	,d1
	cngctl	,d1
	whatno	,d2
	switch	,d1
	v	,d1

proc
	xcall terid (v)

;;;	xcall outpt (1,1,2, 'POUNDS AND HRS BY DEPTARTMENT',1)
	call opens
	if (.not. opnok) goto endoff


	if (.not. me) goto displa			;from coprpt

	INCR PROW
	XCALL OUTPT (PROW,1,1,'POUNDS AND HRS BY DEPARTMENT',1)

	LPSW = 1
	open (14,o,splfil)

	xcall ascii (27, esc_11)
	esc_12 = esc_11
	esc_13 = esc_11
	C_CHAR = ESC_11

	display (14, compres)

	lponsw = 1
	LINCNT = 61
	PGCNT =

	endat = MEDATE
	detsum = 'D'
	TDS = 'DETAIL'

	call proc_data
	;email


	LINCNT = 61
	PGCNT =

	detsum = 'S'
	TDS = 'SUMMARY'
	call proc_data
	close 14

	goto endoff
;============================================================


displa,
	xcall outpt (1,1,2, 'POUNDS AND HRS BY DEPTARTMENT',1)

	xcall outpt (4,4,1, '1. END DATE',1)
	XCALL OUTPT (6,4,1, '2. DETAIL/SUMMARY (D/S)',1)

endat,
	xcall input (4,19,08,00,'DE',entry,inxctl,1)
	goto (displa,endoff),inxctl
	endat = entry(1,8)
	goto (anycng),cngctl
detsum,
	xcall input (6,32,01,01,'AE',entry,inxctl,1)
	goto (displa,endoff),inxctl
	detsum = entry(1,1)
	using detsum select
	('D'),	TDS = 'DETAIL'
	('S'),	TDS = 'SUMMARY'
	(),		goto detsum
	endusing

anycng,
	xcall anycn (cngctl, whatno)
	goto (proces, cngbr), cngctl+1
cngbr,
	goto (endat,detsum), whatno
	goto anycng
proces,

	call proc_data

	goto endoff

proc_data,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; a routine...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	savdpt = '-1'
	
	xdate = endat
	mnth = month(mm)
	year = yyyy, 'ZZZZ'
	l_mnth = mnth
	l_year = (yyyy-1), 'ZZZZ'

	for i from 1 thru 100 clear emp_ara(i)


	xendat = endat				;thru date current year
	xdate = endat
	dd = 1					;first day of the month
	xstdat = xdate		

	clear coptbl
	tblcod = 'DD'			;dept table
	find (chn182, coptbl, tbl_key) [err=loop]

; read thru dept table, accumlate current month, ytd, same month last year, last y-t-d...
loop,
	xcall ios (chn182, coptbl, read, lokctl)
	if (lokctl .ne. 0) goto eof
	if (tbl_key .ne. 'DD') goto eof

	using dd_dept select
	('O'),	DD_DESC = 'DUCTLINE O AND P'
;;;	('P'),	GOTO LOOP			;COMBINE O & P
	endusing

	if (dd_dept .ne. savdpt) call newdpt

	call current_lbs
	call current_hrs
	call ly_lbs
	call ly_hrs

;;;	call print_dept

	goto loop


;==================================================================

newdpt,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (savdpt .eq. '-1') goto outdpt

	if (detsum.eq.'S' .and. savdpt(1,1).eq.dd_dept(1,1)) return
	if (dd_dept.eq.'P' .and. savdpt.eq.'O') return
	call print_dept

outdpt,
	savdpt = dd_dept
	savdsc = dd_desc

	clear m_lbs, y_lbs
	clear m_hrs, y_hrs
	clear lm_lbs, ly_lbs
	clear lm_hrs, ly_hrs

	return
;------------------------------------------------------------------



current_lbs,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (dd_dept(1,1) .eq. 'G')	;debug
		begin
		mm = 1
		end
	xdate = xstdat
	mm = 1			;start at beg of year

	clear ldd_key
	ldd_dpt = dd_dept
;;;	if (detsum .eq. 'S') ldd_dpt = dd_dept(1,1)		;summary

	ldd_dat = xdate		;beg of current year
	find (chn063, datlbs, ldd_key, krf=1) [err=cl_loop]
cl_loop,
	reads (chn063, datlbs, cl_eof)
	if (cl_date .gt. xendat) goto cl_eof

	if (cl_dept.eq.'P' .and. dd_dept.eq.'O') goto combineOP		;combine O & P

	if (cl_dept .eq. 'E2')
		begin
		mm = 1
		end

	if (cl_dept .ne. dd_dept) goto cl_eof	

;;;	using detsum select
;;;	('D'), if (cl_dept .ne. dd_dept) goto cl_eof
;;;	('S'), if (cl_dept(1,1) .ne. dd_dept(1,1) ) goto cl_eof
;;;	endusing

combineOP,
	if (cl_date.ge.xstdat .and. cl_date.le.xendat) m_lbs = m_lbs + cl_lbs
	y_lbs = y_lbs + cl_lbs

	goto cl_loop
cl_eof,

	return
;------------------------------------------------------------------

current_hrs,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	clear m_hrs, y_hrs
	xdate = xstdat
	mm = 1			;first day of month

	clear job_key
	jk_dpt = dd_dept
;;;	if (detsum .eq. 'S') jk_dpt = dd_dept(1,1)		;summary
	jk_dat = xdate
	find (chn085, timdat, job_key, krf=1) [err=ch_loop]
ch_loop,
	reads (chn085, timdat, ch_eof)
	if (tc_jdate .gt. xendat) goto ch_eof
	if (tc_dpt .ne. dd_dept) goto ch_eof

	if (tc_jdate.ge.xstdat .and. tc_jdate.le.xendat) m_hrs = m_hrs + (tc_ehrs+tc_eovt+tc_edblo)
	y_hrs = y_hrs + (tc_ehrs+tc_eovt+tc_edblo)

	goto ch_loop

ch_eof,
	return
;------------------------------------------------------------------

ly_lbs,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	clear lm_lbs, ly_lbs

	xdate = xstdat
	yyyy = yyyy - 1		;last year
	dd = 31			;last day of month
	lyend = xdate

	dd = 1
	lystr = xdate		;first of month

	mm = 1			;start at beg of year
	dd = 1
	
	clear ldd_key
	ldd_dpt = dd_dept
;;;	if (detsum .eq. 'S') ldd_dpt = dd_dept(1,1)		;summary
	ldd_dat = xdate		;beg of prior year

	find (chn063, datlbs, ldd_key, krf=1) [err=lyl_loop]
lyl_loop,
	reads (chn063, datlbs, lyl_eof)
	if (cl_date .gt. lyend) goto lyl_eof

	if (cl_dept.eq.'P' .and. dd_dept.eq.'O') goto combineOP2		;combine O & P

	if (cl_dept .ne. dd_dept) goto cl_eof		
;;;	using detsum select
;;;	('D'), if (cl_dept .ne. dd_dept) goto cl_eof
;;;	('S'), if (cl_dept(1,1) .ne. dd_dept(1,1) ) goto cl_eof
;;;	endusing

combineOP2,

	if (cl_date.ge.lystr .and. cl_date.le.lyend) lm_lbs = lm_lbs + cl_lbs
	ly_lbs = ly_lbs + cl_lbs

	goto lyl_loop
lyl_eof,

	return
;------------------------------------------------------------------

ly_hrs,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;	clear lm_hrs, ly_hrs

; dates set in ly_lbs above.

;;;
	mm = 1			;start at beg of year
	dd = 1

	clear job_key
	jk_dpt = dd_dept
;;;	if (detsum .eq. 'S') jk_dpt = dd_dept(1,1)		;summary
	jk_dat = xdate
	find (chn085, timdat, job_key, krf=1) [err=ly_loop]
ly_loop,
	reads (chn085, timdat, ly_eof)
	if (tc_jdate .gt. lyend) goto ly_eof
	if (tc_dpt .ne. dd_dept) goto ly_eof

	if (tc_jdate.ge.lystr .and. tc_jdate.le.lyend) lm_hrs = lm_hrs + (tc_ehrs+tc_eovt+tc_edblo)
	ly_hrs = ly_hrs + (tc_ehrs+tc_eovt+tc_edblo)

	goto ly_loop

ly_eof,
	return
;------------------------------------------------------------------

print_dept,

	temp = m_hrs+y_hrs+lm_hrs+ly_hrs+m_lbs+y_lbs+lm_lbs+ly_lbs
	if (temp .le. 0) return

; round to whole hours...
	m_hrs = m_hrs#2
	y_hrs = y_hrs#2
	lm_hrs = lm_hrs#2
	ly_hrs = ly_hrs#2

	pline (1,2) = savdpt
	pline (4,28) = savdsc

	pline (30,38) = m_lbs,	'Z,ZZZ,ZZX'
	pline (40,43) = m_hrs,	'ZZZX'
	if (m_hrs .gt. 0)
	then	avg = m_lbs/m_hrs
	else	avg = 0
	pline (45,51) = avg,	'ZZZ,ZZZ'

	pline (54,63) = lm_lbs,	'ZZ,ZZZ,ZZX'
	pline (65,69) = lm_hrs,	'ZZZZX'
	if (lm_hrs .gt. 0)
	then	avg = lm_lbs/lm_hrs
	else	avg = 0
	pline (71,77) = avg,	'ZZZ,ZZZ'

	pline (81,90) = y_lbs,	'ZZ,ZZZ,ZZX'
	pline (92,95) = y_hrs,	'ZZZZX'
	if (y_hrs .gt. 0)
	then	avg = y_lbs/y_hrs
	else	avg = 0
	pline (97,103) = avg,	'ZZZ,ZZZ'

	pline (106,115) = ly_lbs,	'ZZ,ZZZ,ZZX'
	pline (117,121) = ly_hrs,	'ZZZZX'
	if (ly_hrs .gt. 0)
	then	avg = ly_lbs/ly_hrs
	else	avg = 0
	pline (123,129) = avg,	'ZZZ,ZZZ'
	call print

;                            --------CURRENT--------  -------LAST YEAR-------    --------CURRENT--------   -------LAST YEAR-------'
;   DEPT                            LBS  HRS     AVG         LBS   HRS     AVG          LBS  HRS     AVG         LBS   HRS     AVG'

;AA AAAAAAAAAAAAAAAAAAAAAAAAA Z,ZZZ,ZZX ZZZX ZZZ,ZZX  ZZ,ZZZ,ZZX ZZZZX ZZZ,ZZX    Z,ZZZ,ZZX ZZZX ZZZ,ZZX  ZZ,ZZZ,ZZX ZZZZX ZZZ,ZZX  
;1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6          7        8         9         0         1         2


;   --------CURRENT--------  -------LAST YEAR-------    --------CURRENT--------   -------LAST YEAR-------'
;DP       LBS  HRS     AVG         LBS   HRS     AVG           LBS  HRS     AVG         LBS   HRS     AVG'

;AA Z,ZZZ,ZZX ZZZX ZZZ,ZZX  ZZ,ZZZ,ZZX ZZZZX ZZZ,ZZX    Z,ZZZ,ZZX ZZZX ZZZ,ZZX  ZZ,ZZZ,ZZX ZZZZX ZZZ,ZZX  
;12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6          7        8         9


	savdpt = dd_dept
	savdsc = dd_desc

	return
;----------------------------------------------------
PRINT,
	IF (LPONSW.EQ.0) CALL PRNTON
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,HDR2,HDR3,
&			'NO LEGEND',' ',' ',1,132,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
PRNTON,
	SPLFIL (5,6) = 'EL'
	LPSW = 1	; PRINT, SPOOL OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPONSW = 1
	RETURN
;************************************************************************

eof,
	IF (ME) GOTO ME_EOF

	IF (LPONSW.EQ.1) XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	return
ME_EOF,
;;;	close 14
	RETURN
;;;	goto endoff

;-----------------------------------------------------------
opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	opnok =

	switch = 5
	xcall files (1,'SI', 63, switch)		;63 - datlbs.ism
	if (switch .eq. 9) return
	chn063 = 1

	switch = 5
	xcall files (2,'SI', 85, switch)		;85 - timdat.ism
	if (switch .eq. 9) return
	chn085 = 2

	switch = 5
	xcall files (17,'SI',182, switch)		;182 - coptbl
	chn182 = 17

	opnok = 1
	return
;-----------------------------------------------------------
close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (chn063) close chn063
	if (chn085) close chn085
	if (chn182) close chn182


	return
;-----------------------------------------------------------

endoff,
	call close
	xreturn

;;;	xcall pgchn ('cp:coprpt',1)
;;;	stop
	end
;===============================================


;   ------------- M-T-D THRU JUL 2019 --------------    --------------- Y-T-D THRU JUL 2019 ------------
;   ------------------------ M-T-D -----------------    ------------------------ Y-T-D -----------------
;   --------CURRENT--------  -------LAST YEAR-------    --------CURRENT--------  -------LAST YEAR-------
;DP       LBS  HRS     AVG         LBS   HRS     AVG          LBS  HRS     AVG         LBS   HRS     AVG
;AA Z,ZZZ,ZZX ZZZX ZZZ,ZZX  ZZ,ZZZ,ZZX ZZZZX ZZZ,ZZX    Z,ZZZ,ZZX ZZZX ZZZ,ZZX  ZZ,ZZZ,ZZX ZZZZX ZZZ,ZZX  
;12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6          7        8         9

