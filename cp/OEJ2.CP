subroutine oej2		;returns gasket used
	fmemos	,a
	totjg	,d	;jr flange
	tottg	,d	;tg flange
;
;	create bc, flange & gasket line items...
;	modifications:
;	25-sep-03 ssq:	allow call from BILLS.CP add lqtysh code...
;	08-oct-03 ssq:	bills not using runtot, taxtot anymore
;

GLOBAL ORDL	
RECORD ORDLIN  		
		.INCLUDE 'DEF:RD045A.def'
RECORD,X
		.INCLUDE 'DEF:RD045D.def'
RECORD,X
		.INCLUDE 'DEF:RD045M.def'
ENDGLOBAL
;------------------------------------------


RECORD	F_MEMOS
	.INCLUDE 'DEF:CPMEMO.DEF'

RECORD
	SEGS	,A12			;first 2 dig are gauge
RECORD,X
	GA	,D2
	SARA	,5D2

record
	tkey	,a5
record,x
	typ9	,d1
	typ	,4d1

record	vars
	otyp	,d1
	dia 	,d2
	all_zeros	,d1
	decmal		,d10
	new_ext		,d10
	orig_ext	,d10
	no_gasket	,d1
	t_num	,d12
	tg_len	,d6		;gasket lenght in feet
	jg_len	,d6		;jr gasket length in feet
	bc_cnt	,d6
	ef_cnt	,d6
	seg_max	,d2,	04	;1-30-14 see "typ" array
;;;	seg_max	,d2,	05
	j	,d6
	i	,d6
;
proc
	f_memos = fmemos		;memo keys
	clear i, j
	clear tg_len, jg_len, totjg, tottg
	clear no_gasket
	if(lordno.gt.503000 .and. lordno.le.607650) goto xdone	;ssq 9/24/03
	using litmno select
	('EB'),	goto xdone
	('EF'),	goto xdone
	('JG'),	goto xdone	;jr gasket
	('TG'),	goto xdone	;regular gasket
	('JEB'),	goto xdone	;barrel clamps
	('JEF'),	goto xdone	;e-z flange, all types
	('JJG'),	goto xdone	;jr gasket
	('JTG'),	goto xdone	;regular gasket
	endusing

	call make_work
xdone,
	xreturn

;================================================

make_work,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; create line items and add to work file
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (ltype .eq. 'M') return
	if (lmsq3 .le. 0) return		;must have <f3> memo
	using lcfgim select			;skip these items...
	('SPCC1' thru 'SPCC9'),	return
	('SSSP'),		return
	('ASP'),		return
	('NOGASKET'),		return
	('???'),		return	;ssq 1-30-14
	endusing

	tkey = f3_key(lmsq3),	'ZZZZX' [LEFT]
	if (typ9.ne.9 .and. tkey.ne.'15 ') return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1-30-14 for prdcat changes:

	using lprdcd select
	('G1','G3','G4','G5','G9'),	xcall catg(lcfgim,segs)
	(),	using lcfgim select
		('SL'),	xcall catg(lcfgim,segs)		;parse for segs
		('HD'),	xcall catg(lcfgim,segs)		;parse for segs
		('HE'),	xcall catg(lcfgim,segs)		;parse for segs
		('HO'),	xcall catg(lcfgim,segs)		;parse for segs
		('H'),	xcall catg(lcfgim,segs)		;parse for segs

		(),	xcall CFG2(lcfgim,segs)
		endusing
	endusing


;;;	if (lprdcd .eq. 'G' .or. lcfgim.eq.'SL')
;;;	then	xcall catg(lcfgim,segs)		;catg
;;;	else	xcall cfg2(lcfgim,segs)		;parse for segs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	all_zeros = 1				;assume all zeros
	for i from 1 thru seg_max		;dim of seq is dia of item #
		begin
		otyp = typ(i)
		dia = sara(i)
		if (tkey.eq.'15 ') using dia select
					(1 thru 5),	otyp=3	;sp
					(6 thru 24),	otyp=2	;jr
					(.gt. 24),	otyp=1	;ef
					(),		otyp=0	;anything else
				   endusing
		if (otyp .gt. 0) call add_work
		end		

	if (tg_len .gt. 0) call tg		;reg gasket
	if (jg_len .gt. 0) call jg		;jr gasket
	return
;------------------------------------------------------

add_work,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	bc_cnt = lqtyor
	ef_cnt = lqtyor
	if(i.eq.1 .and. otyp.eq.2) return	;jr is 1st
	if(i.eq.1 .and. otyp.eq.1) clear bc_cnt	;no bc for ef if 1st

	using otyp select
	(1,4),	if(bc_cnt.gt.0)tg_len = tg_len + (dia*35*lqtyor)	;this is off by 10
	(2),	if(bc_cnt.gt.0)jg_len = jg_len + (dia*35*lqtyor)	;this is off by 10
	(3),	nop		;sp don't count
	endusing

	return
;----------------------------------------------
jg,
	if (no_gasket) return		;ssq 9-16-03

;;;	jg_len = (jg_len*10/12)#2	;convert to feet
;;;	t_num = jg_len#2 *100
;;;	if (jg_len .gt. t_num)
;;;	then	jg_len = t_num + 50
;;;	else	jg_len = t_num

	totjg = jg_len
	return
;--------------------------------
tg,
	if (no_gasket) return		;ssq 9-16-03
;;;	tg_len = (tg_len*10/12)#2	;convert to feet
;;;	t_num = tg_len#2 *100
;;;	if (tg_len .gt. t_num)
;;;	then	tg_len = t_num + 50
;;;	else	tg_len = t_num

	tottg = tg_len
	return
;--------------------------------

	END


