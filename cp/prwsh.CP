SUBROUTINE	PRWSH
	ORDNO	,D
	LOCAL	,D	;1=print to local printer
	ODEPT	,A	;just print this dept if not blank




RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PCL code for IDX C8800 printer

record	reset
	re	,a1	;esc
		,a1,	'E'

record	l1
	e1	,a1	;esc
		,a*,	'*v6W'
	lx	,8a1
;;;		,a*,	' 020108080808'
record	idx_pal
	e2	,a1	;esc
		,a*,	'*v'
	ipcR	,d3	;color 1 
		,a*,	'a'
	ipcG	,d3	;color 2
		,a*,	'b'
	ipcB	,d3	;color 3
		,a*,	'c'
	ipidx	,d2	
		,a*,	'I'


record	idx_sel
	e5	,a1	;esc
		,a*,	'*v'
	isidx	,d2
		,a*,	'S'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT
RECORD	BOLD
	E_CHA2	,A1
		,A*,	"(s3B"

RECORD	MEDIUM
	E_CHA3	,A1
		,A*,	'(s0B'


		
RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	CRHOLD
	.INCLUDE 'DEF:RD195A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CHANNEL
	CHN044	,D2
	CHN182	,D2
	CHN195	,D2



RECORD	ORDFMT
	OF_ORD	,A6
		,A1,	'.'
	OF_NUM	,D1
		,A1
	OF_LOC	,A1

RECORD,X
		,A5
	LDIG	,D1

RECORD	PVARS

	MAXLIN	,D2,	57
	LINCNT	,D2
	PAG	,D2
	NPAG	,D2
	paklin	,a80
	pakdpt	,a2
	PLINE	,A85
	SAVLIN	,A85


	BLANKS	,A30
	BLANK90	,A90
	TMPDSC	,A56
	REM	,D6
	DASHES	,A30,	'------------------------------'
	UNDER	,A58
	DECMAL	,D18
	NUMASK	,A8,	'ZZZZZZX-'


RECORD	VARS
	SG_BOLD		,D1	;1 = BOLD SG/SX IF NOT 10' 4-17-18
	c_desc		,a30	;desc for cross (sbt)
	onedpt	,a2	;just print this dept
	d6	,d6
	a30		,a30
	is_qmark	,d1	;1 = dept has ??? item
	prt_dept,d1	;1 = print this dept
	fnl	,d1	;if this fnote, print to local printer.
	fcode	,d5	;f-note
	fkey	,d1	;1,2,3
	fnote_local	,d1	;1=print to local printer...
	ln	,d6
	fl	,d6
	partno	,a15
	printq	,a20
	A1	,A1
	CDPT	,50A2
	MAXC	,D2,50
	hex0	,a1
	hex1	,a1
	hex2	,a1
	hex8	,a1
	e_esc	,a1
	a6	,a6
	got_duc	,d1
	numcop	,d2	;number of copies to print
	prow	,d3
	rb_row	,d3	;row for rbox
	CMPCOD	,A3
	PL_INFO	,D1
	IS_VULCAN	,D1
	HAS_H	,D1
	HAS_HV	,D1
	HAS_K	,D1
	HAS_KV	,D1
	HAS_F	,D1
	HAS_FV	,D1
	W2	,A2
;;;	WX_KEY	,A59
	WX_KEY	,A60			;3-27-17
	I	,D6
	lines_printed	,d1
	skip_i	,d1	;process dept "I" first, send to UPS printer
	bold_on	,d1
	sav_bold	,d1
	XDEPT	,A2
	DT	,A20
	D	,D8
	T	,D6
	OPNOK	,D1
	J	,D5
	LL	,D2
	TL	,D2
	STAT	,D3
	READ	,D1,0
	WRITE	,D1,1
	store	,d1,2
	LOKCTL	,D1
	SWITCH	,D1

PROC
	XCALL WHO (CMPCOD)
	USING CMPCOD SELECT
	('CAT'),	XRETURN
	ENDUSING

	SWITCH = 5
	XCALL FILES (40, 'SI', 195, SWITCH)
	read (40, CRHOLD, ORDNO) [ERR=NOT_ON_HOLD]
	goto is_on_hold
NOT_ON_HOLD,
	clear crhold
is_on_hold,
	CLOSE 40

	USING CH_FLAG SELECT
	('3'),	XRETURN		;Nothing prints for 555 crdlmt...	
	('4'),	XRETURN		;5555 cod ...	
	ENDUSING

	IF (%PASSED(ODEPT) )	;7-29-13 for printing a single dept...
	THEN ONEDPT = ODEPT
	ELSE ONEDPT = 


	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF



;1-28-15:need to do this at the end of both "O" & "P"
	if (need_o)
		begin
		duc_only = 
		savdpt = 'Y'
		wdept = 'Y'
		read(chnduc,ducwrk,savdpt)[err=no_need_o]
		clear got_duc
		close 14	;just in case
		call gd_newdpt
		got_duc = 1	;2-10-15
		call getduc
		end
no_need_o,
	if (need_p)
		begin
		duc_only = 
		savdpt = 'Z'
		wdept = 'Z'
		read(chnduc,ducwrk,savdpt)[err=no_need_p]
		clear got_duc
		close 14	;just in case
		call gd_newdpt
		got_duc = 1		;2-10-15
		call getduc
		end
no_need_p,
done_getduc,

	bold_on = 0

	CLOSE CHNDUC
	XCALL DELET (DUCFIL)

ENDOFF,
;;;	CLOSE CHN045
	CLOSE CHNWRK
	close 14	;2-2-15 - in case still open from duct printing issue.

; create DPTSTS records...
	XCALL MKDPS (OORDNO, OCUSNO, CHN045, CHN175, CHN182)
	CLOSE CHN045

	RETURN

;================================================
NEW_BX,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SAV_BX = W_BX
;;;	IF (SAVLOC .EQ. BLANKS) RETURN		; SKIP IF LOC IS BLANK
	IF (WSEQ1 .NE. SAVSQ1) CALL NEWSQ1
	IF (COMSEQ .NE. SAVCOM) CALL NEWCOM

	RETURN
;------------------------------------------
NEWLOC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SAVLOC = WLOC
	IF (SAVLOC .EQ. BLANKS) RETURN		; SKIP IF LOC IS BLANK
	IF (WSEQ1 .NE. SAVSQ1) CALL NEWSQ1
	IF (COMSEQ .NE. SAVCOM) CALL NEWCOM

	RETURN
;------------------------------------------

NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVDPT = WDEPT
	SAVSQ1 = 0
	SAVCOM = 0
	SAVTAG = 0
	SAVLOC =
	SAV_BX =
	call mak_neword

	RETURN
;------------------------------------------

mak_neword,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	using ocusno select
	(2),	return		;don't sched test orders	5-22-19
	endusing

	xdept = savdpt
	using savdpt select	;5-31-19
	('Y'),	xdept = 'O'
	('Z'),	xdept = 'P'
	endusing


	clear neword

	clear tbl_key
	tblcod = 'DD'
	tblkey = xdept
	xcall isio (chn182,coptbl,tbl_key,read,lokctl)
	if (lokctl .ne. 0)
		begin
		clear coptbl
		end

	if (dd_sch)
		begin
	;;;	new_dpt = savdpt
		new_dpt = xdept

		xcall rdate(d6)
		new_dat = d6(1,4)	;mm/dd
		xcall time(d6)
		new_tim = d6(1,4)	;hh:mm

		new_ord = ordno
		new_stat = 0
		new_type = 'O'		;order 12-5-18

		xcall isio (chn088, neword, new_key, store, lokctl)
		end
	RETURN
;------------------------------------------

NEWSQ1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVSQ1 = WSEQ1
	clear savcom			;7-30-09

	IF (WSEQ1 .LE. 0) RETURN
	CLEAR LINE2
	WSEQ12 = SAVSQ1
	CLEAR WSEQ22, WSEQ32
	WITEM2 = '   F1'		;changed back ssq 2/16/01
	WTYPE2 = 'M'
	WDESC2(1,90) = F1_MEMOL(WSEQ12)
	WDEPT2 = SAVDPT
	WTAG2 = SAVTAG
	WLOC2 = SAVLOC
	W_BX2 = SAV_BX
	STORE (CHNOUT, LINE2, W_KEY)

	RETURN
;------------------------------------------
NEWCOM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; COMBINATION OF SEQ2 & SEQ3
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVCOM = COMSEQ
	CLEAR TM2, TM3
	IF (WSEQ2 .NE. 0) TM2 = F2_MEMOL(WSEQ2)
	IF (WSEQ3 .NE. 0) TM3 = F3_MEMOL(WSEQ3)


	FOR J FROM 1 THRU 3
		BEGIN
		CLEAR LINE

		W_BX = SAV_BX
		WLOC = SAVLOC
		WSEQ1 = SAVSQ1
		COMSEQ = SAVCOM
		wtag = savtag
		WITEM = 
		WTYPE = 'M'
		WDEPT = SAVDPT
		WDESC(1,30) = TM2A(J)
		WDESC(34,63) = TM3A(J)
		IF (WDESC .EQ. BLANK90) NEXTLOOP
		IF (TM2A(J).NE.BLANKS) WITEM(4,5) = ;;;>'F2'
		IF (TM3A(J).NE.BLANKS) 
			BEGIN
			WITEM(4,5) = ;;;>'F3'
			WDESC(32,32) = '*'
			END
		IF (TM2A(J).NE.BLANKS .AND. TM3A(J).NE.BLANKS) 
			BEGIN
			WITEM(4,8) = ;;;>'F2&F3'
			END
		WITEM (10,10) = J,'X'
		STORE (CHNOUT, LINE, W_KEY)
		END
		
		

	RETURN
;------------------------------------------

LOAD_MEMOS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Read thru ordlin file, and
		;;; load memo arrays
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR F1_NUM, F2_NUM, F3_NUM, F4_NUM
	FOR J FROM 1 THRU F_MAX
		BEGIN
		CLEAR F1_KEY(J)
		CLEAR F2_KEY(J)
		CLEAR F3_KEY(J)

		CLEAR F1_MEMOS(J)
		CLEAR F2_MEMOS(J)
		CLEAR F3_MEMOS(J)
		CLEAR F4_MEMOS(J)

		CLEAR F1_MEMOL(J)
		CLEAR F2_MEMOL(J)
		CLEAR F3_MEMOL(J)
		CLEAR F4_MEMOL(J)
		END

	FIND (CHN045, ORDLIN, ORDNO) [ERR=MLOOP]
MLOOP,
	READS (CHN045, ORDLIN, EOF_M)
	IF (LINSEQ .NE. 0) GOTO EOF_M

	clear fnl

;1-3-19 ssq: l1_num, l2_num, l3_num were never set..

	IF (LMSQ1 .GT. 0) 
		BEGIN
		f1_num = lmsq1			;1-3-19
		F1_MEMOS(LMSQ1) = M_SHORTD
		F1_MEMOL(LMSQ1) = M_LDESCR
		F1_KEY(LMSQ1) = M_KEY
		fkey = 1
		fcode = m_key
		call get_fnl
		f1_cdpt(lmsq1) = fnl	;use for print local flag
		END
	IF (LMSQ2 .GT. 0) 
		BEGIN
		f2_num = lmsq2			;1-3-19
		F2_MEMOS(LMSQ2) = M_SHORTD
		F2_MEMOL(LMSQ2) = M_LDESCR
		F2_KEY(LMSQ2) = M_KEY
		fkey = 2
		fcode = m_key
		call get_fnl
		f2_cdpt(lmsq2) = fnl	;use for print local flag
		END
	IF (LMSQ3 .GT. 0) 
		BEGIN
		f3_num = lmsq3			;1-3-19
		F3_MEMOS(LMSQ3) = M_SHORTD
		F3_MEMOL(LMSQ3) = M_LDESCR
		F3_KEY(LMSQ3) = M_KEY
		fkey = 3
		fcode = m_key
		call get_fnl
		f3_cdpt(lmsq3) = fnl	;use for print local flag
		END
	IF (LMSQ4 .GT. 0) 
		BEGIN
		F4_MEMOS(LMSQ4) = M_SHORTD
		F4_MEMOL(LMSQ4) = M_LDESCR
		END
	GOTO MLOOP
EOF_M,
	RETURN
;-------------------------------------------------

LOAD_WORK,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Move non-memo line items
		;;; into work file
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FOR I FROM 1 THRU MAXC
		BEGIN
		CLEAR CDPT(I)
		END

	SET SR_QTY(1), SR_QTY(2) = 0
	SET HAS_H, HAS_HV, HAS_K, HAS_KV, HAS_F, HAS_FV = 0

	FIND (CHN045, ORDLIN, ORDNO) [ERR=LOOP]
LOOP,
	READS (CHN045, ORDLIN, EOF)
	IF (LORDNO .NE. ORDNO) GOTO EOF
	IF (LTYPE .EQ. 'M') GOTO LOOP
	IF (LROCPO .EQ. 9) GOTO LOOP		;SSQ 4/27/04
	IF (LPRDCD .EQ. 'Z ') GOTO LOOP		;SSQ 3/22/05 skip priceduct.com
;;;	IF (LQTYOR .EQ. 0) GOTO LOOP		;SSQ 3/24/00
	IF (ONEDPT.NE.'  ' .AND. LDEPT.NE.ONEDPT) GOTO LOOP ;SSQ 7-29-13
	
	CLEAR TBL_KEY
	TBLCOD = 'EX'
	TBLKEY = EX_ITEM = LITMNO
	FIND (CHN182,COPTBL,TBL_KEY) [ERR=LCONT]	;continue if NOT in table
	GOTO LOOP
LCONT,
	call check_sb_tees		;10-11-17
	if (.not. sbt) goto no_sbt

;;;	c_desc = ldescr

	for sbt_idx from 1 thru 9
		begin
		fv_data = funcv(sbt_item, code(sbt_idx), sbt_f3)
		if (f_item .ne. blanks)
			begin
		;;;	if (code(sbt_idx).ne.'R')call insert_f2
			if (code(sbt_idx).eq.'SG' .or. code(sbt_idx).eq.'SW')call insert_f2
		;;;	call insert_f3
			if (code(sbt_idx).ne. 'SG')
			then	call insert_f3
			else	if (sbt_f3.eq.3) call insert_f3

			xcall g_item (f_item, itmmas, ordlin, f_memos, sbt_mat, sbt_f1idx, f2_idx, f3_idx, f_ga, sbt_qty,chn041,chn182)
			if (code(sbt_idx).eq.'C1 ') lqtyor = lqtyor*2
		;;;	if (code(sbt_idx).eq.'R') clear lf2, lmsq2
			if (code(sbt_idx).ne.'SG' .and. code(sbt_idx).ne.'SW')clear lf2, lmsq2
			if (code(sbt_idx).eq.'SG' .and. sbt_f3.ne.3) clear lf3, lmsq3
			if (code(sbt_idx).eq.'MN')
			then	c_desc = ldescr		;modified main, f2(2) desc only
			else	call load_line
			end
		end
	goto loop

no_sbt,
	call load_line			;10-11-17
	goto loop


EOF,
	RETURN	;end of load_work
;----------------------------------------------

load_line,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; 10-11-17: now a routine 
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear line			;3-27-17
	w_bx = 0			;3-29-17

	WTYPE = 'L'

	WDEPT = LDEPT(1,1)

;--------------------------------------------------------------
; 4-20-09: problem w/ items dept=J in roc but "ZZ" in smc...
	if (ldept .eq. 'ZZ') wdept = 'J'
;--------------------------------------------------------------
; 3-1-09: dept is set to vulcan in scrnx now
; 3-17-09: not getting set correctly, or prob w/ older orders,
;		do this check here until resolved...

	CALL CHECK_VULCAN		;7-29-08: go to vulcan?
	IF (IS_VULCAN)	WDEPT(2,2) = 'V'
;--------------------------------------------------------------

; note the order of testing below: 'FV' catches FV only, the subsequent 'F'
; will catch any other LDEPT starting w/ F
	USING WDEPT SELECT		;7-29-08
	('FV'),	HAS_FV = 1
	('F'),	HAS_F = 1
	('HV'), HAS_HV = 1
	('H'),	HAS_H = 1
	('KV'),	HAS_KV = 1
	('K'),	HAS_K = 1
	ENDUSING


	WDDD2 = LDEPT
	IF(WDEPT(1,1).EQ.'I') WDDD2 = LPRDCD(1,1)	;SSQ 5-10-00

; ssq 6-4-01 force duct work to end
	USING WDEPT(1,1) SELECT
	('O'),	WDEPT(1,1) = 'Y'
	('P'),	WDEPT(1,1) = 'Z'
	ENDUSING

	A2 = LMSQ1, 'XX'
	WSEQ1 = A2
	A2 = LMSQ2, 'XX'
	WSEQ2 = A2
	A2 = LMSQ3, 'XX'
	WSEQ3 = A2

	A2 = LMSQ4, 'XX'
	WTAG = A2

	WPG = LPIPEG		;1-21-09
	WSQNO = LSRTSQ
	WITEM = LITMNO
	using witem select
	('JEB','JEF','JJG','JTG'), WITEM = LITMNO(2,15)	;SKIP THE "J"
	endusing

	WDESC = LDESCR
	IF (LDAMPR) WITEM(10,15) = 'Damper'
	WQTY = LQTYOR
	WUM = LUOFM

	if (local .eq. 1)
	then	call no_cng	;print everything
	else	IF (OPRTF .GT. 1) 
		THEN	CALL CHK_CNG	;something changed...
		ELSE	CALL NO_CNG	;everything printes 1st time thru..

	WCNGD = LCNGD			;7-16-09

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	call check_stock			;4-25-16 - need box/bin loc from itmmas
;;;	need direct read to itmmas in case cfg'd item
;;;
	clear itmkey
	k_item = litmno

;;;> 4-26-17 (problem when R260403 gets changed to R240603
;;;	if (lcfgim .ne. blanks) k_item = lcfgim

	k_f1 = lf1,	'XXX'
	k_f2 = lf2,	'XXX'
	k_f3 = lf3,	'XXXXX'
	read (chn041, itmmas, itmkey, keynum:1) [err=item_nf]
	goto itm_ok
item_nf,
	clear itmmas
itm_ok,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	wloc = ibnloc				;loc for loose qty
	if (ldept .ne. 'I') ibxqty =		;only convert to boxes for dept "I"
	if (ibxqty .gt. 0)
	then	begin
		qbox = lqtyor/ibxqty		;# of boxes based on qty/box
		if(qbox.gt.0)
		then	begin			;create a line for the box qty
			wqty = qbox		;number of boxes
			wum = 'BX'
			W_bx = 9		;3-27-17 sort these to bottom of w/o
			wloc = ibxloc		;box loc
			store (chnwrk, line, w_key)	;store the box record
			rem = lqtyor - (qbox*ibxqty)	;remaining loose items
			if (rem.gt.0)
				begin
				wloc = ibnloc		;bin loc		
				wqty = rem		;remaining loose qty
				wum = luofm
				w_bx = 0		;3-27-17 these go w/ regular lines
				store(chnwrk, line, w_key)	;store the loose items record
				end
			end

		else	store (chnwrk, line, w_key)		
		end
	else	store (chnwrk, line, w_key)

	return	;;end of load_line
;--------------------------------------------------------------------------------------


CHK_CNG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (LCNGD .NE. 1) RETURN	;this item did not change
NO_CNG,
	FOR I FROM 1 THRU MAXC
		BEGIN
		IF (WDEPT .EQ. CDPT(I) ) RETURN	;this dept already marked
		IF (CDPT(I) .EQ. A1)
			BEGIN
			CDPT(I) = WDEPT		;mark this dept
			RETURN
			END
		END
	RETURN
;--------------------------------------------------
WCHK_CNG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (DCNGD .NE. 1) RETURN	;this item did not change
WNO_CNG,
	FOR I FROM 1 THRU MAXC
		BEGIN
		IF (WD_DEPT .EQ. CDPT(I) ) RETURN	;this dept already marked
		IF (CDPT(I) .EQ. A1)
			BEGIN
			CDPT(I) = WD_DEPT		;mark this dept
			RETURN
			END
		END
	RETURN
;--------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; done in scrnx as of 3-11-09 
CHECK_VULCAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;7-29-08: go to vulcan?
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 7-29-13 use routine cvcan in all routines for consistency
	xcall cvcan (ordlin, ocusno, chn182, is_vulcan)	;7-29-13
	return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CONSOLIDATE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Consolidate line items for the
		;;; same part #
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR MULTLINE, SAVQTY
	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_CON]

	SAVKEY = '***'
	XSAVKEY = '***'
CONLOOP,
	READS(CHNWRK, LINE, EOF_CON)
;;;	W2 = WTAG,	'XX'		;3-27-17 this was moved up to part of key a long time ago
;;;	WX_KEY = W_KEY + W2
	WX_KEY = W_KEY			;3-27-17

;;;	IF (W_KEY .NE. SAVKEY)
	IF (W_KEY.NE.SAVKEY .or. SAVUM.NE.WUM)
	THEN	CALL NEWKEY
	ELSE	BEGIN
		INCR MULTLINE
		SAVQTY = SAVQTY + WQTY
		DELETE (CHNWRK)
		END
	GOTO CONLOOP

NEWKEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (XSAVKEY .EQ. '***') GOTO OUTKEY
	IF (MULTLINE .EQ. 0) GOTO OUTKEY

	READ (CHNWRK, LINE, SAVKEY)
	WQTY = SAVQTY

;;;	W2 = WTAG,	'XX'		;need to reset key
;;;	WX_KEY = W_KEY + W2
	WX_KEY = W_KEY			;3-27-17 SSQ
	WRITE (CHNWRK, LINE, SAVKEY)
OUTKEY,
	SAVKEY = W_KEY
	XSAVKEY = WX_KEY
	SAVQTY = WQTY
	SAVUM = WUM
	CLEAR MULTLINE
	RETURN
;-----------------------------------------------

EOF_CON,
	CALL NEWKEY
	RETURN
;-----------------------------------------------





HP_PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;

	INCR LINCNT
	IF (LINCNT .GE. maxlin) 
		BEGIN
		if (lincnt.ne.67) 
			begin
			writes (14, paklin)
			end
		SAVLIN = PLINE
		sav_bold = bold_on
		bold_on = 0
		CALL PRTHDR
		PLINE = SAVLIN
		bold_on = sav_bold
		END

	xcall hp (14, hpPOS, lincnt, 1, 0, pline)
	CLEAR PLINE
	RETURN
;----------------------------------------

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INCR LINCNT
	IF (LINCNT .GE. maxlin) 
		BEGIN
		if (lincnt.ne.67) 
			begin
			writes (14, paklin)
			end
		SAVLIN = PLINE
		sav_bold = bold_on
		bold_on = 0
		CALL PRTHDR
		PLINE = SAVLIN
		bold_on = sav_bold
		END

	if (bold_on) display (14,bold)
	if (wum .eq. 'BX')
		begin
		isidx = 4	;red
		display(14,idx_sel)
		end

	WRITES (14, PLINE)
	if (bold_on) display (14,medium)
	if (wum .eq. 'BX')
		begin
		isidx = 1	;black
		display(14,idx_sel)
		end

	CLEAR PLINE
	RETURN
PRINT2,
	INCR LINCNT
	WRITES (14, PLINE)
	CLEAR PLINE
	RETURN

PRINT_MEMO,
	RETURN

;-----------------------------------------------------

PRTHDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR LINCNT
	CLEAR PLINE

	OF_ORD = OORDNO,	'ZZZZZX'
	OF_NUM = OPRTF
	xcall hp (14,hpPOS, 1, 1, 0, ' ')

	XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)		;ssq 5-2-16


;top of order...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XPOL1 = OPONO
	XPOL2 =
	IF (OMETRO .NE. BLANKS)
		BEGIN
		XPOL1 = OMETRO
		XPOL2 = OPONO
		END

	PLINE(28,39) = XPOL1
	PLINE(41,50) = OJOBNO

	PLINE(70,79) = S_DATE
	CALL PRINT2

	PLINE(28,39) = XPOL2
	CALL PRINT2

	FORMS(14,1)

	LINCNT = 17		;ssq 11-5-14

	USING WDEPT SELECT
;;;	('O','P'),	WRITES (14, H_LINED)
	('Y','Z'),	begin
			WRITES (14, H_LINED)
			incr lincnt
			end
	(),		WRITES (14, H_LINE2)
	ENDUSING

;;;	LINCNT = 17

	RETURN

;DATE ENTERED    SLS-REP     PO NUMBER   JOB NUMBER  SHIP VIA         SHIP DATE
;  XX/XX/XXXX    AAAAAAAAAA  AAAAAAAAAA  AAAAAAAAAA  AAAAAAAAAAAAAAA  XX/XX/XXXX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
;-----------------------------------------------
ga_color,	;;;;;;;;;;;;;;;;;;;;;;;;
	using wgauge select
	(18),	isidx = 5	;5, Orange
	(20),	isidx =	3	;3, Brown
	(22),	isidx = 8	;8, Green
	(24),	isidx = 4	;0, Red
	(26),	isidx = 0	;0, Blue
	(),	isidx = 1	;1, black
	endusing

	xcall rbox2 (16, lincnt+1, isidx, 6, 4)	;

;TDC = green, S&D = red
	using wsty select
	(2),	isidx = 8	;8, Green
	(1),	isidx = 4	;0, Red
	endusing

	xcall rbox2 (56, lincnt+1, isidx, 6, 3)	;
	
;PIT = brown
	using wseam select
	(2),	begin
		isidx =	5	;5, Orange
		xcall rbox2 (61, lincnt+1, isidx, 6, 3)	;
		end
	endusing

	

	return
;---------------------------------------



open_splfil,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	OPEN (14, O, SPLFIL)

	set re, e1,e2,e5 = e_char
	
	display (14, reset)


; set the color palettes...
	xcall ascii (0, hex0)
	xcall ascii (1, hex1)
	xcall ascii (8, hex8)
	xcall ascii (2, hex2)	;8-07-09

	set lx(1) = hex2	;color space = sRGB
	set lx(2) = hex1

	set lx(3) = hex8
	set lx(4), lx(5), lx(6) = hex0
	
	writes (14,l1)

	clear tbl_key
	tblcod = 'CL'
	find (chn182, coptbl, tbl_key) [err=color_loop]
color_loop,
	xcall ios (chn182, coptbl, read, lokctl)
	if (lokctl .ne. 0) goto eof_color
	if (tblcod .ne. 'CL') goto eof_color	

	ipidx = cl_num
	ipcR = cl_red
	ipcG = cl_grn
	ipcB = cl_blu
	display (14,idx_pal)
	
	goto color_loop
eof_color,


;--------------------------------------

	XCALL HP (14,hpDOTS, 0)

	B_DATA(1,2) = 'S~'
	B_DATA(3,8) = OORDNO, 'XXXXXX'
	b_data(9,9) = '~'
	b_data(10,11) = savdpt

	XCALL B128(B_DATA, B_STRNG, 70)
;;;	XCALL OF128

	return
;---------------------------------------------------


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK
	
	XCALL ASCII (27, E_CHAR)

	E_CHA2 = E_CHAR
	E_CHA3 = E_CHAR



	XCALL ISIO (CHN044, ORDHDR, ORDNO, READ, LOKCTL)

	WFORD = ORDNO,'XXXXXX'
	XCALL DATE8(OORDDT, D_OUT, D_OUTR, E_DATE, D_SW)
	XCALL DATE8(OPROMD, D_OUT, D_OUTR, S_DATE, D_SW)


	SPORD = ORDNO,'XXXXXX'

	call open_splfil

	OPNOK = 1

	RETURN
;-------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN044
	CLOSE CHN045
	CLOSE CHN175
	CLOSE CHN088

	RETURN



	END
