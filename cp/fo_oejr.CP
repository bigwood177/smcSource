;flat oval
subroutine fo_oejr
	order	,d
	chn041	,d
	chn045	,d
	runtot	,d
	taxtot	,d
	oloc	,a
	has_ssefi	,d
	cmx	,d	;xx.x non-contractor mark up
;
;	create bc, flange & gasket line items...
;	modifications:
;
;
			
record	f_data			;flat oveal
	f_in	,a15		;input item
	f_itm	,a15		;resultant smc item #
	f_mlt	,d3			;price multiplier
	f_descr	,a30		;resultant description
	f_err	,d1
	fo_item	,a20
;
record	cfgmat
	.include 'def:cfgmat.def'

record	coptbl
	.include 'def:rd182a.def'

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT


record	ordlin
	.include 'def:rd045a.def'
record,x
	.include 'def:rd045m.def'
record,x
	.include 'def:rd045d.def'
record	savlin
	.include 'def:rd045e.def'


record	itmmas
	.include 'def:rd041a.def'
record,x
	.include 'def:rd041b.def'

record	itmkey
	.include 'def:rd041k.def'

RECORD	F_MEMOS
	.INCLUDE 'DEF:CPMEMO.DEF'

RECORD	WRKFIL
		,A4,	'SPL:'
		,A1,	'W'
	WFORD	,A6
		,A4,	'.ISM'

RECORD	SPLRPT
		,A4,	'SPL:'
		,A1,	'W'
	SPORD	,A6
		,A4,	'.ERR'
record	out
	otyp	,d1
	dia 	,d2
	bc_qty	,d6	;count for barrel clamps
	ef_qty	,d6	;count for ez-flange
record,x
	okey	,d3

RECORD	SEGS
	ASEGS	,2D4	
RECORD,X
	XXYY	,D4
	BBCC	,D4

record
	tkey	,a5
record,x
	typ	,5d1


record	strng
	str	,a15

record	channels
	chncfg	,d2		;ordlin work file
	chnspl	,d2		;error report file
	chn182	,d2,17		;chan 17 already opened to coptbl...

record	err_line
	el_order	,d6
			,a1
	el_litmno	,a12
			,a1
	el_msg		,a60

record
	,a60,	'Invalid item for this type                                  '
	,a60,	'required item not in inventory - could not create line item ' 
	,a60,	'Invalid size for EF                                         '
	,a60,	'Invalid size for JR                                         '
	,a60,	'123456789012345678901234567890123456789012345678901234567890'
	,a60,	'123456789012345678901234567890123456789012345678901234567890'
	,a60,	'123456789012345678901234567890123456789012345678901234567890'
	,a60,	'123456789012345678901234567890123456789012345678901234567890'
	,a60,	'123456789012345678901234567890123456789012345678901234567890'
	,a60,	'123456789012345678901234567890123456789012345678901234567890'
;                123456789012345678901234567890123456789012345678901234567890
record,x
	err_msg		,10a60

record	nine_values
	rw	,d1,	8	;raw
	sp	,d1,	4	;spiral end
	jr	,d1,	3	;ez jr
	ez	,d1,	3	;ez flange

	xx	,d1,	0	;anything else

record	vars
	sav_xxyy	,d4
	sav_bbcc	,d4

	xdia	,d2	;for dual wall
	dl_wall	,d2	;if dual wall add 2 or 4 inches to barrel clamps
	rd_efi		,d1	;if adding extra ez-flange for reducer	4-9-18
	F6A	,5A4,	'GALV','ALUM','SST ','PGRP','S.S.'
	A4	,A4
	tl	,d6
	mi	,d6
	mat_sel	,d1
	p_mult	,d3
	is_jr	,d1	;1=is jr size (6-24)
	itmrfa	,a6
	vanrfa	,a6
	ivflag	,d1
	vanflg	,d1
	key	,a15
	clctl	,d1
	fil041	,a14
	partno	,a15
	fl	,d4
	maxflen	,d4
	LPON		,D1		;SSQ 4-6-04
	all_zeros	,d1
	err_num		,d3
	err_on		,d1
	routine		,a32
	decmal		,d10
	new_ext		,d10
	orig_ext	,d10
	no_gasket	,d1
	t_num	,d12
	tg_len	,d6		;gasket lenght in feet
	jg_len	,d6		;jr gasket length in feet
	stg_len	,d6		;shipped
	sjg_len	,d6		;shipped
	st_seq	,d3		;start linseq at 800
	bc_cnt	,d6
	ef_cnt	,d6
	sbc_cnt	,d6		;shipped
	sef_cnt	,d6		;shipped
	seg_max	,d2,	05
	entry	,a6
	blanks	,a30
	orginv	,d5
	bsmid	,d5
	bsend	,d5
	srcctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	lokctl	,d1
	i	,d6
	j	,d6
;
proc

	clear has_ssefi, dl_wall

	call opens
	clear err_on
	clear lpon

	XCALL ASCII (27, E_CHAR)

	clear i, j
	clear tg_len, jg_len, stg_len, sjg_len
	clear no_gasket

	call load_memos
	call delete_lines
	call make_work
	call add_lines
	
xdone,
	close chncfg
	xcall delet(wrkfil)

	if (lpon)
		begin
		display (chnspl, eject)
		close chnspl
		if (err_on) lpque(splrpt,DELETE)
		end
	xreturn

;================================================

delete_lines,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; delete any flange, bc, and gasket tagged w/ notes...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear orig_ext, new_ext

	find (chn045, ordlin, order) [err=d_loop]
d_loop,
	xcall ios (chn045,ordlin,read,lokctl)
	if (lokctl .ne. 0) goto d_eof
	if (lordno .ne. order) goto d_eof
	if (ltype .eq. 'M') goto d_loop
;;;	if (ljrmak .ne. 9) goto d_loop		;oejr
	if (ljrmak .ne. 8) goto d_loop		;fo_oejr

	using litmno select
	('JFOEB'),	call del45	;barrel clamps
	('JFOEF'),	call del45	;e-z flange, all types
	('JJG'),	call del45	;gasket
	endusing
	orig_ext = orig_ext + (lqtysh*lprice)#1

	goto d_loop
d_eof,
	return
;----------------------------------------------------------------
make_work,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; create line items and add to work file
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	find (chn045, ordlin, order) [err=loop]
loop,
	xcall ios(chn045,ordlin,read,lokctl)
	if (lokctl .ne. 0) goto eof
	if (lordno .ne. order) goto eof
	if (ltype .eq. 'M') goto loop
	if (lmsq3 .le. 0) goto loop		;must have <f3> memo

	call find_damper	;remove any *'s from lcfgim, return partno

	using partno select			;skip these items...
	('FO'),		NOP		;ONLY FO'S
	(),		goto loop
	endusing

	tkey = f3_key(lmsq3),	'ZZZZX' [LEFT]
	if (tkey .ne. '3') goto loop	;only ez flange
	
	p_mult = 100		;1.00
	mat_sel = lmat

; 4-30-14: case for items like ECS06 where mat'l is built into the item and
; lmat does not get set to a material value (ie lmat=1 => galv).

	if (lmat.eq.1 .and. lf1.gt.0)	;material note running for galv.
		begin
		clear tbl_key
		tblcod = 'M1'
		m1_key = lf1
		xcall isio (chn182, coptbl, tbl_key, read, lokctl)
		if (lokctl.eq.0) mat_sel = m1_matl
		end


	if (mat_sel .gt. galv) call chk_f6_matl

	savlin = ordlin
	call load_line	
	goto loop		

eof,

	ordlin = savlin
	clear lmsq1, lmsq2, lmsq3, lmsq4	;loose, out side of notes
	if (tg_len .gt. 0) call tg		;reg gasket
	if (jg_len .gt. 0) call jg		;jr gasket
	return
;---------------------------------------------------------------------


Load_line,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
		;;; sbt: now a routine
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear rd_efi				;4-9-18
	xcall fo_cfg (chn182, litmno, f_itm, f_mlt, f_descr, f_err, segs)

; 7-13-29 "READ_ITMMAS" calls fo_cfg and re-sets segs

	SAV_XXYY = XXYY			;7-13-20
	SAV_BBCC = BBCC			;7-13-20
	using LITMNO select
	('FOSG'),	CALL SPIRAL
	('FOR','FOER'),	CALL REDUCER
	('FOEC'),	CALL END_CAPS
	('FOSW'),	CALL SADDLES
	('FOE'),	CALL ELBOWS	;AND ANGLES
	('FOC'),	CALL CONNECTORS
	(),		RETURN
	endusing
	return			;sbt
;------------------------------------------------------

NEW_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; MAKE A NEW ORDLIN RECORD
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ordlin = savlin			;fresh copy each time...

	call read_itmmas
	if (lokctl) return

	clear lcpflg			
	store (chncfg, ordlin, ordkey)

	RETURN
;-------------------------------------------------------

gasket,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (dia .gt. 24)	;jr?
	then	tg_len = tg_len + (dia*35)*lqtyor
	else	jg_len = jg_len + (dia*35)*lqtyor

	return
;-------------------------------------------------------

SPIRAL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	DIA = F_ITM(3,4)

	PARTNO = 'FOEB' + ^A(XXYY)
	call new_line

	PARTNO = 'FOEFI' + ^A(SAV_XXYY)
	call new_line

	PARTNO = 'FOEFI' + ^A(SAV_XXYY)
	call new_line

	call gasket

	;JG (SG202210) – gasket calc based on SG202210
	RETURN
;-------------------------------------------------------
REDUCER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FOR27142206 - REDUCER

	USING F_ITM SELECT
	('RW'),		DIA = F_ITM(3,4)
	('ERW'),	DIA = F_ITM(4,5)
	ENDUSING

	IF (XXYY .LE. BBCC)
	THEN	PARTNO = 'FOEB' + ^A(XXYY)
	ELSE	PARTNO = 'FOEB' + ^A(BBCC)
	call new_line

	PARTNO = 'FOEFI' + ^A(SAV_XXYY)
	call new_line
	PARTNO = 'FOEFI' + ^A(SAV_BBCC)
	call new_line

	call gasket
	;JG (RW202220) – figured on the 20” size

	RETURN
;-------------------------------------------------------
END_CAPS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
        PARTNO = 'FOEFI' + ^A(XXYY)
	call new_line

	RETURN
;-------------------------------------------------------
SADDLES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	DIA = F_ITM(6,7)

	PARTNO = 'FOEB' + ^A(XXYY)
	call new_line
	PARTNO = 'FOEFI' + ^A(SAV_XXYY)
	call new_line

	;JG (SW9201699)
	call gasket

	RETURN
;-------------------------------------------------------
ELBOWS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	DIA = F_ITM(3,4)

	PARTNO = 'FOEB' + ^A(XXYY)
	call new_line
	PARTNO = 'FOEFI' + ^A(SAV_XXYY)
	call new_line
	PARTNO = 'FOEFI' + ^A(SAV_XXYY)
	call new_line

	call gasket
	;JG (EG169020)

	RETURN
;-------------------------------------------------------
CONNECTORS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;EZ FLANGE NOT ALLOWED
	RETURN
;-------------------------------------------------------

	
	
find_damper,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; check for *'s in lcfgim, return partno
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	partno = lcfgim
	maxflen = %trim(partno)
	for i from 1 thru 3
		begin
		xcall instr(1, partno, "*", fl)
		if (.not. fl) exitloop
		partno(fl,maxflen) = partno(fl+1, maxflen)
		end
	return
;------------------------------------------------------------

chk_f6_matl,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR TBL_KEY
	TBLCOD = 'CF'
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_CF_MUL]

	USING mat_sel SELECT
	(GALV),	p_mult = 100		;1.00
	(VIN),  p_mult = PVSMUL		;7-12-17
	(ALUM),	p_mult = ALUMUL
	(PGRP),	p_mult = PGRMUL
	(SST),	p_mult = SSTMUL
	ENDUSING

not_cf_mul,
	return
;------------------------------------------------------------


add_work,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear all_zeros			;something...
	ordlin = savlin			;fresh copy each time...
	clear lcpflg			;ssq 10-9-03
	bc_cnt = lqtyor
	ef_cnt = lqtyor
	sbc_cnt = lqtysh
	sef_cnt = lqtysh

	if(i.eq.1 .and. is_jr) return		;jr is 1st item
	if(i.eq.1 .and. otyp.eq.ez) 
		begin
		clear bc_cnt	;no bc for ef if 1st
		clear sbc_cnt
		end
	return
;----------------------------------------------


jg,	;25' rolls
	if (no_gasket) return		;ssq 9-16-03
;;;	str = 'JG'
;;;	call read_itmmas
	f_itm = 'JJG'			;no conversion for gasket
	partno = 'JG'
	f_mlt = 100
	call ri_gasket
	if (lokctl) return

	ldescr = descr			;from itmmas
	jg_len = (jg_len*10/12)#2	;convert to feet
	t_num = jg_len/25*25
	if (jg_len .gt. t_num) jg_len = jg_len + 25

	lqtyor = (jg_len*100/25)#2	;25' rolls  ssq 3-30-06

	sjg_len = (sjg_len*10/12)#2	;convert to feet
	t_num = sjg_len/25*25
	if (sjg_len .gt. t_num) sjg_len = jg_len + 25

	lqtysh = (sjg_len*100/25)#2	;ssq 3-30-06

	clear lcpflg			;ssq 04-13-04
	store (chncfg, ordlin, ordkey)
	return
;--------------------------------
tg,
	if (no_gasket) return		;ssq 9-16-03
;;;	str = 'JTG'	
;;;	call read_itmmas
	f_itm = 'TG'			;no conversion for gasket
	partno = 'TG'
	f_mlt = 100
	call ri_gasket
	if (lokctl) return

	ldescr = descr			;from itmmas
	tg_len = (tg_len*10/12)#2	;convert to feet
	t_num = tg_len#2 *100
	if (tg_len .gt. t_num)
	then	tg_len = t_num + 50
	else	tg_len = t_num

	lqtyor = (tg_len*100/50)#2	;50' rolls  ssq 3-30-06

	stg_len = (stg_len*10/12)#2	;convert to feet
	t_num = stg_len#2 *100
	if (stg_len .gt. t_num)
	then	stg_len = t_num + 50
	else	stg_len = t_num

	lqtysh = (stg_len*100/50)#2	;50' rolls ssq 3-30-06

	clear lcpflg			;ssq 04-13-04
	store (chncfg, ordlin, ordkey)
	return
;--------------------------------


add_lines,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; read thru work file and add lines to ordlin
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	st_seq = 700
	clear ordkey
	find (chncfg, ordlin, ^first) [err=al_loop]
al_loop,
	xcall ios(chncfg,ordlin,read,lokctl)
	if (lokctl .ne. 0) goto al_eof
	if (lqtyor .eq. 0) goto al_loop		;skip zero qty
	
	using litmno select
	('JEB','JEF'),	clear lf2, lf3		;need mat'l
	(),		clear lf1, lf2, lf3
	endusing

	call commit

	new_ext = new_ext + (lqtysh*lprice)#1

al_next_seq,
;;;	incr st_seq
	linseq = st_seq
;;;	ljrmak = 9				;record created in oejr
	ljrmak = 8				;record created in fo_oejr
;;;	new_ext = new_ext + (lqtysh*lprice)#1	;moved above 6-1-20

;;;	clear lf1, lf2, lf3, lf4		;isam - never keys

	xcall isio (chn045,ordlin,ordkey,store,lokctl)
	if (lokctl .ne. 0) goto al_next_seq
	goto al_loop
al_eof,
	decmal = new_ext - orig_ext
	runtot = runtot + decmal
	taxtot = taxtot + decmal
	return
;----------------------------------------------------------------


read_itmmas,	;;;;;;;;;;;;;;;;;;
	
	CLEAR ITMKEY		;don't need itmkey for these items as they
				;are not affected by f* notes


	xcall fo_cfg (chn182, partno, f_itm, f_mlt, f_descr, f_err, segs)
	f_itm = 'J' + f_itm

ri_gasket,
	read (chn041, itmmas, f_itm) [err=baditm]
;;;	read (chn041, itmmas, strng) [err=baditm]
	lokctl = 0


	clear ldampr
	litmno = 'J' + partno			;flat oval actual item
	lcfgim = 'J' + partno	
	ldescr = f_descr		;flat oval descr

	if (mat_sel .gt. galv) call chk_m_desc

	lstokt = stock

	lprice = (price*p_mult)#2
	using itemno select		;flat oval f_itm for price and other info
	('JEFI'),	begin
			if(mat_sel.eq.sst)lprice = price
			using dl_wall select				;4-26-17
			(2),	if (dia.ge.24) lprice = (lprice*25)#1
			(4),	if (dia.ge.22) lprice = (lprice*25)#1
			endusing
			end
	('JTG','JJG'),	lprice = price
	endusing

	lprice = (lprice*cmx)#3*10	;ssq 2-29-16
	lprice = (lprice*f_mlt)#2	;flat oval mult

;;;	lprice = price
	luofm = suofm
	lprdcd = prdcat
	ldept = usrdef
	lsrtsq = priccd
	if (txflag .eq. 'Y') ltxflg = 1

; 1-21-19: get the weight...
	using itemno select
	('JEB'),	KEY = itemno(2,15)	
	('JEFI'),	KEY = itemno(2,3) + itemno(5,15)	;11-1-17: this first
	('JEF'),	KEY = itemno(2,15)			;11-1-17: then this
	('JJG'),	KEY = itemno(2,15)	
	('JTG'),	KEY = itemno(2,15)	
	(),		return
	endusing


	read (chn041, itmmas, key) [err=baditm]
	lpwgt = ip_wgt
	return
;---------------------------------
baditm,
	lokctl = 9		;not found
	err_num = 2
	call in_error
	return
;---------------------------------

chk_m_desc,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; does ldescr need matl?
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	mi = 1
cf6l,
	a4 = f6a(mi)
	tl = %instr(1, ldescr, a4)
	if (tl .le. 1)
		begin
		incr mi
		if (mi .le. 5) goto cf6l
		end

	if (tl .gt. 1)
	  begin
	  using mat_sel select
	  (.le.galv),	nop
	  (),	begin
		tl = %instr(1,ldescr,'GALV')
		if (tl.le.1) 
			begin
			tl = %trim(ldescr)
			tl = tl + 2
			end
		if (tl.gt.30)tl=30		
		ldescr(tl,30) = c_short(mat_sel)
		end
	  endusing
	  end

	return
;--------------------------------------------------------------

LOAD_MEMOS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Read thru ordlin file, and
		;;; load memo arrays
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR F1_NUM, F2_NUM, F3_NUM
	FOR J FROM 1 THRU F_MAX
		BEGIN
		CLEAR F1_MEMOS(J)
		CLEAR F2_MEMOS(J)
		CLEAR F3_MEMOS(J)

		CLEAR F1_MEMOL(J)
		CLEAR F2_MEMOL(J)
		CLEAR F3_MEMOL(J)

		CLEAR F1_KEY(J)
		CLEAR F2_KEY(J)
		CLEAR F3_KEY(J)
		END

	FIND (CHN045, ORDLIN, ORDER) [ERR=MLOOP]
MLOOP,
	READS (CHN045, ORDLIN, EOF_M)
	IF (LINSEQ .NE. 0) GOTO EOF_M
	IF (LMSQ1 .GT. 0) 
		BEGIN
		f1_num = lmsq1			;1-3-19
		F1_MEMOS(LMSQ1) = M_SHORTD
		F1_MEMOL(LMSQ1) = M_LDESCR
		F1_KEY(LMSQ1) = M_KEY
		END
	IF (LMSQ2 .GT. 0) 
		BEGIN
		f2_num = lmsq2			;1-3-19
		F2_MEMOS(LMSQ2) = M_SHORTD
		F2_MEMOL(LMSQ2) = M_LDESCR
		F2_KEY(LMSQ2) = M_KEY
		END
	IF (LMSQ3 .GT. 0) 
		BEGIN
		f3_num = lmsq3			;1-3-19
		F3_MEMOS(LMSQ3) = M_SHORTD
		F3_MEMOL(LMSQ3) = M_LDESCR
		F3_KEY(LMSQ3) = M_KEY
		END
	GOTO MLOOP
EOF_M,
	RETURN
;-------------------------------------------------

in_error,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (lpon .eq. 0) call spool_on

	clear err_line
	el_order = order
	el_litmno = str
	el_msg = err_msg(err_num)
	writes (chnspl, err_line)
	err_on = 1
	return
;--------------------------------------------
del45,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	using litmno select
	('JEB','JEF'),	clear lf2, lf3		;need mat'l
	(),		clear lf1, lf2, lf3
	endusing

	lqtyor = -lqtyor
	call commit			;un-commit
	
	delete(chn045)			;delete the ordlin record
	return
;---------------------------------------------


commit,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (oloc .ne. 'O') return	;orders only

	using litmno select
	('JEB'),	KEY = LITMNO(2,15)	
	('JEFI'),	KEY = LITMNO(2,3) + LITMNO(5,15)	;11-1-17: this first
	('JEF'),	KEY = LITMNO(2,15)			;11-1-17: then this
	('JJG'),	KEY = LITMNO(2,15)	
	('JTG'),	KEY = LITMNO(2,15)	
	(),		KEY = LITMNO
	endusing

;---------------------------------------------------------
	xcall gtitm (key, lf1, lf2, lf3, itmrfa, ivflag, vanflg, vanrfa)
	if (ivflag .eq. 0) return	;nothing to allocate,
					;need error message...
	close chn041
	open (chn041, su, fil041)

	read (chn041, itmmas, rfa:itmrfa) [err=badcmt]
;---------------------------------------------------------

	IF (stock.EQ.'S') QTYCOM = QTYCOM + LQTYOR
	if (qtycom .lt. 0) qtycom = 0
	write (chn041, itmmas, key)

badcmt,
	close chn041
	open (chn041, si, fil041)
	return
;--------------------------------------------

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Create work file...
	CHNCFG = 8
	WFORD = ORDER,	'XXXXXX'
	XCALL ISAMC (WRKFIL, 180, 1, 'START=1, LENGTH=9, DUPS, ASCEND')
	OPEN (CHNCFG, SU, WRKFIL)

	XCALL FFILE (41, FIL041, CLCTL)
	fil041(14,14) = 'M'

	RETURN
;-------------------------------------------------

SPOOL_ON,
	CHNSPL = 20
	SPORD = ORDER,	'XXXXXX'
	OPEN(CHNSPL, O, SPLRPT)
	LPON = 1

	RETURN
;-------------------------------------------------

	END


