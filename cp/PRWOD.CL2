;prwod.cl2
;colored notes version
SUBROUTINE	PRWOD
	ORDNO	,D
	LOCAL	,D	;1=print to local printer
;-------------------------------------------------------------------------
;Mods:
; 7-29-08 ssq: dept's "FV", "HV", "KV" need to print on separate sheets.
; 7-16-09 ssq: if oprtf > 0 only print if a line in that dept has changed.
;-------------------------------------------------------------------------


EXTERNAL FUNCTION
	TRN3	,D

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


;9-07-06 ssq: removed bolding logic at "print,"
;

GLOBAL DUCK	
	.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL

RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.DEF'

	.include 'def:hpsub.def'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PCL code for IDX C8800 printer

record	code
	esc	,a1	;esc
	cd	,a20

record	reset
	re	,a1	;esc
		,a1,	'E'

record	l1
	e1	,a1	;esc
		,a*,	'*v6W'
	lx	,8a1
;;;		,a*,	' 010108080808'
record	idx_pal
	e2	,a1	;esc
		,a*,	'*v'
	ipc1	,d3	;color 1 
		,a*,	'a'
	ipc2	,d3	;color 2
		,a*,	'b'
	ipc3	,d3	;color 3
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

RECORD	WRKFIL
		,A4,	'SPL:'
		,A1,	'W'
	WFORD	,A6
		,A4,	'.ISM'
RECORD	DUCFIL
		,A4,	'SPL:'
		,A1,	'D'
	WRORD	,A6
		,A4,	'.ISM'
RECORD	OUTFIL
		,A4,	'SPL:'
		,A1,	'X'
	OFORD	,A6
		,A4,	'.ISM'

RECORD	SPLFIL
		,A4,	'SPL:'
		,A1,	'S'
	SPORD	,A6
		,A4,	'.SPL'
	
RECORD	F_MEMOS
	.INCLUDE 'def:CPMEMO.DEF'
		
RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.def'
RECORD,X
	.INCLUDE 'DEF:RD045M.def'
RECORD,X
	.INCLUDE 'DEF:RD045D.def'


RECORD	CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'
RECORD	DUMCUS
	.INCLUDE 'DEF:RD001B.DEF'
RECORD	CUSIDX
	.INCLUDE 'DEF:RD002A.DEF'

RECORD	SALMAN
	.INCLUDE 'DEF:RD054A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD PARAM
	.INCLUDE 'DEF:PARAM2.DEF'


RECORD
	MAXDUC	,D3,100
	QTY	,D5
	ITM	,A15
	DES	,A30
	PRC	,D7
	COMPA	,A15
	COMPB	,A15

RECORD ARRAYS
	SDUCRC	,100A32	;SUMMARIZED DUCT CONFIGURATIONS   (SEE DUCREC BELOW)
	LINSQF	,8D6	;SUMMARIZE LINER SQUARE FEET

RECORD DUCREC
	DUCCFG	,D7	;CONFIGURATION CODE
	DUCSQF	,D7	;SQ FEET OF MATERIAL
	DUCPND	,D7	;POUNDS OF MATERIAL
	DUCGPR	,D7	;GAUGE PRICE

RECORD ACCREC
	.INCLUDE 'DEF:ACCREC.DEF'


RECORD	LINE			
	WDEPT	,A2
	WSEQ1	,D2		;reversed
	WSEQ2	,D2
	WSEQ3	,D2	
	WDDD2	,A2		;DEPARTMENT
	WSQNO	,A2
	WITEM	,A15
	WDESC	,A90
	WTYPE	,A1		;L=LINE, M=MEMO
	WQTY	,D5
	WUM	,A2
	WTAG	,D2		;LMSQ4
	WPG	,A3		;PIPE GA
	WCNGD	,D1		;1 = CHANGED
RECORD	MEMO,X
	MDEPT	,A2
	MSEQ	,D6
	MDDD2	,A2
	MSQNO	,A2
	MITEM	,A15
	MMEMO	,3A30
	fnl	,d1		;1 = print local
		,D5
		,A2
		,A2		;LMSQ4
		,A3		;PIPEG
		,A1		;WCNGD
RECORD	,X
		,A2
		,D6
		,A2
		,A2
		,A15
	M_LONG	,A63
		,A27
		,A1
		,D5
		,A2
		,A2		;LMSQ4
		,A3		;PIPEG
		,A1		;WCNGD
RECORD,X
	W_KEY	,A57

RECORD,X
		,A2
		,D2		;not reversed
	COMSEQ	,A4
		,A2
;---------------------------
RECORD	LINE2			
	WDEPT2	,A2
	WSEQ12	,D2		
	WSEQ22	,D2
	WSEQ32	,D2	;REVERSE
	WDDD22	,A2
	WSQNO2	,A2
	WITEM2	,A15
	WDESC2	,A30
	WADD2	,A60	;FILLER
	WTYPE2	,A1		;L=LINE, M=MEMO
	WQTY2	,D5
	WUM2	,A2
	WTAG2	,D2		;LMSQ4
	WPG2	,A3		;PIPEG
	WCNG2	,D1		;1= LINE CHANGED
RECORD,X
	W_KEY2	,A57

RECORD,X
		,A2
		,D2		;not reversed
	COMSEQ2	,A4
		,A2
;---------------------------

RECORD	H_LINE
;;;	,A*,	'DATE ENTERED    SLS-REP     PO NUMBER   JOB NUMBER'
	,A*,	'DATE ENTERED  SLS-REP       PO NUMBER   JOB NUMBER'
	,A*,	'  SHIP VIA         SHIP DATE'

RECORD	H_LINE2
	,A*,	'  QTY  ITEM NUMBER     DESCRIPTION                    UM '

RECORD	H_LINED
	,A*,	'======================== DUCTWORK ========================'

RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN044	,D2
	CHN045	,D2
	CHN054	,D2
	CHN175	,D2
	CHN182	,D2
	CHNWRK	,D2
	CHNOUT	,D2
	CHNDUC	,D2


RECORD	PAGEOF
	P1	,A2
		,A4,	' OF '
	P2	,A2


RECORD	ORDFMT
	OF_ORD	,A6
		,A1,	'.'
	OF_NUM	,D1


RECORD	PVARS
	dl_3	,d6
	num_p	,d6
	num_s	,d6
	num_dl	,d6
	rem_d	,d6
	LNAM	,A25
	SNAM	,A12
	SINT	,A3

	SAV_CL_COUNT	,D6
	A2	,A2
	MEMO_ON	,D1
	MAXLIN	,D2,	58
	LINCNT	,D2
	PAG	,D2
	NPAG	,D2
	PLINE	,A80
	SAVLIN	,A80
	BLANKS	,A30
	BLANK90	,A90
	TMPDSC	,A56
	REM	,D6
	DASHES	,A30,	'------------------------------'
	UNDER	,A58
	DECMAL	,D18
	NUMASK	,A8,	'ZZZZZZX-'

RECORD
	TM2	,A90
	TM3	,A90
RECORD,X
	TM2A	,3A30
	TM3A	,3A30

;---------------------------------
; for f4 tags... 1-03-08
RECORD
	D_LONG	,A90
RECORD,X
	DL_ARA	,3A30
;---------------------------------

RECORD	DATIME
	DATE	,A10
		,A2
	TIME	,A8

RECORD	NJOINT
	JT1	,A3
		,A1,	'/'
	JT2	,A3

RECORD	KEYSP
	KEY_SPEC	,A*,	'START=1:126, LENGTH=57:2, DUPS, ASCEND'


RECORD	VARS
	fnote_local	,d1	;1=print to local printer...
	ln	,d6
	printq	,a12
	A1	,A1
	CDPT	,50A2
	MAXC	,D2,50
	hex0	,a1
	hex1	,a1
	hex8	,a1
	e_esc	,a1
	a6	,a6
	got_duc	,d1
	numcop	,d2	;number of copies to print
	prow	,d3
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
	WX_KEY	,A59
	I	,D6
	lines_printed	,d1
	skip_i	,d1	;process dept "I" first, send to UPS printer
	bold_on	,d1
	sav_bold	,d1
	J_CNT	,D2	;number of "J" copies printed	
	J_RFA	,A6	;rfa of first "J" record
	JT_ERR	,D1
	JT	,D4
	XPOL1	,A12
	XPOL2	,A12
	XDEPT	,A2
	DT	,A20
	D	,D8
	T	,D6
	SZ1	,D3
	SZ2	,D3
	CNT_SD	,D8
	CNT_TDC	,D8
	DUCT_DPT	,D1
	SAVJOINT	,D5
	SAVFEET		,D6
	SAVFLIN		,D6
	SAVLBS		,D6
	OPNOK	,D1
	FIRST_PAGE	,D1
	WD_RFA	,A6
	CL_COUNT	,D6
	SAV_MO	,D1
	SV_NAME	,A15
	E_DATE	,A10		;DATE ENTERED
	S_DATE	,A10		;SHIP DATE
	CONFIG	,D7
	CSZ	,A30
	KEY	,A6
	BSMID	,D6
	BSEND	,D6
	SRCCTL	,D1
	ACCUOM	,A2
	J	,D5
	LL	,D2
	TL	,D2
	SEQNO	,D2
	SAVSEQ	,D2
	SAVSQ1	,D2
	SAVSQ2	,D2
	SAVSQ3	,D2
	SAVDPT	,A2
	SAVCOM	,A4
	SAVKEY	,A57
	XSAVKEY	,A59
	SAVQTY	,D5
	MULTLINE	,D5
	SKIP_LINE	,D1		;SKIP THIS LINE IF BLOCK MEMO
	STAT	,D3
	READ	,D1,0
	WRITE	,D1,1
	LOKCTL	,D1
	SWITCH	,D1

PROC
	XCALL WHO (CMPCOD)
	USING CMPCOD SELECT
	('CAT'),	XRETURN
	ENDUSING



	CLEAR BLANKS, BLANK90, bold_on
	MEMO_ON = 1

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	CALL LOAD_MEMOS
	CALL LOAD_WORK
	CALL CONSOLIDATE

	CALL LOAD_DUCT
	CALL CONS_DUCT

;;; Add memos to output file...
	CLEAR SAVCOM, SAVSQ1, SAVDPT

	FIND (CHNWRK, LINE, ^FIRST)[ERR=EOF_BM]

BM_LOOP,
	READS (CHNWRK, LINE, EOF_BM)
	USING WDEPT(1,1) SELECT
	('F','H','K'),	IF (WDEPT .NE. SAVDPT) CALL NEWDPT
	(),		IF (WDEPT(1,1) .NE. SAVDPT(1,1)) CALL NEWDPT
	ENDUSING

	IF (WSEQ1 .NE. SAVSQ1) CALL NEWSQ1
	IF (COMSEQ .NE. SAVCOM) CALL NEWCOM

	GOTO BM_LOOP
EOF_BM,
	FIND (CHNWRK, LINE, ^FIRST)[ERR=EOF_FIN]
FIN_LOOP,
	READS (CHNWRK, LINE, EOF_FIN)
	STORE (CHNOUT, LINE, W_KEY)
	GOTO FIN_LOOP
EOF_FIN,
	CLOSE CHNWRK


	CALL WRTMEM		;INTERNAL ROUTINE TO WRITE WORKSHEETS
	CALL GETDUC		;PROCESS DUCTWORK

	bold_on = 0

	CLOSE CHNDUC
	XCALL DELET (DUCFIL)

ENDOFF,
	CLOSE CHN045
	CLOSE CHNWRK

	RETURN

;================================================

NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVDPT = WDEPT
	SAVSQ1 = 0
	SAVCOM = 0
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

		WSEQ1 = SAVSQ1
		COMSEQ = SAVCOM
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
	IF (LMSQ1 .GT. 0) 
		BEGIN
		F1_MEMOS(LMSQ1) = M_SHORTD
		F1_MEMOL(LMSQ1) = M_LDESCR
		if (lf1 .eq. 71) fnl = 1
		END
	IF (LMSQ2 .GT. 0) 
		BEGIN
		F2_MEMOS(LMSQ2) = M_SHORTD
		F2_MEMOL(LMSQ2) = M_LDESCR
		using lf2 select
		(12,31),	fnl = 1
		(),		fnl = 0
		endusing
		END
	IF (LMSQ3 .GT. 0) 
		BEGIN
		F3_MEMOS(LMSQ3) = M_SHORTD
		F3_MEMOL(LMSQ3) = M_LDESCR
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

	SET HAS_H, HAS_HV, HAS_K, HAS_KV, HAS_F, HAS_FV = 0

	FIND (CHN045, ORDLIN, ORDNO) [ERR=LOOP]
LOOP,
	READS (CHN045, ORDLIN, EOF)
	IF (LORDNO .NE. ORDNO) GOTO EOF
	IF (LTYPE .EQ. 'M') GOTO LOOP
	IF (LROCPO .EQ. 9) GOTO LOOP		;SSQ 4/27/04
	IF (LPRDCD .EQ. 'Z ') GOTO LOOP		;SSQ 3/22/05 skip priceduct.com
;;;	IF (LQTYOR .EQ. 0) GOTO LOOP		;SSQ 3/24/00
	
	CLEAR TBL_KEY
	TBLCOD = 'EX'
	TBLKEY = EX_ITEM = LITMNO
	FIND (CHN182,COPTBL,TBL_KEY) [ERR=LCONT]	;continue if NOT in table
	GOTO LOOP
LCONT,
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
	STORE (CHNWRK, LINE, W_KEY)
	GOTO LOOP

EOF,
	RETURN
;----------------------------------------------
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; done in scrnx as of 3-11-09 
CHECK_VULCAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;7-29-08: go to vulcan?
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR IS_VULCAN
	IF (LSTOKT .EQ. 'S') RETURN
	IF (OCUSNO .EQ. 5) RETURN

	USING LDEPT SELECT
	('F','H','K'),	NOP
	(),		RETURN
	ENDUSING

; if notes are "send to vulcan = n" skip item...
	clear tblkey
	tblcod = 'M1'
	mm_key = lf1
	read (chn182, coptbl, tbl_key) [err=nk1]
	if (mm_vulc .eq. 2) return
nk1,
	clear tblkey
	tblcod = 'M2'
	mm_key = lf2
	read (chn182, coptbl, tbl_key) [err=nk2]
	if (mm_vulc .eq. 2) return
nk2,
	clear tblkey
	tblcod = 'M3'
	mm_key = lf3
	read (chn182, coptbl, tbl_key) [err=nk3]
	if (mm_vulc .eq. 2) return
nk3,

	USING LITMNO SELECT
	('BN'),				NOP;	CALL BN		;BULLNOSE TEE
	('C4020','C4420','C4620'),	NOP
	('C4820','C4220'),		NOP
	('C9','C4','CV9','CV4'),	IS_VULCAN = 1	;CALL SHOE_TAP		;CROSS
	('CT','CC','CTV','CCV'),	IS_VULCAN = 1	;CALL SHOE_TAP		;TEE	;CONICAL TEE/CROSS
	('T4','T9','TV4','TV9'),	IS_VULCAN = 1	;CALL SHOE_TAP		;TEE
	('PD'),				NOP;	CALL ELBOW	
	('EC'),				IS_VULCAN = 1	;NOP;	CALL END_CAP
	('EG'),				IS_VULCAN = 1	;CALL ELBOW	
	('EV'),				IS_VULCAN = 1	;CALL ELBOW
	('AV'),				IS_VULCAN = 1	;CALL ELBOW
	('GA','GE'),			IS_VULCAN = 1	;CALL ROUND_ELBOW_ANGLE
	('ER16' THRU 'ER26'),		IS_VULCAN = 1	;CALL ECC_REDUCER
	('ERW16' THRU 'ERW26'),		IS_VULCAN = 1	;CALL ECC_REDUCER
	('R16' THRU 'R26'),		IS_VULCAN = 1	;CALL ROUND_CONCENTRIC_REC
	('RW16' THRU 'RW26'),		IS_VULCAN = 1	;CALL ROUND_CONCENTRIC_REC
	('RV16' THRU 'RV26'),		NOP;	CALL REDU
	('RT','RTV'),			NOP;	CALL REG_TAKE_OFF
	('STC','STT'),			IS_VULCAN = 1	;CALL SHOE_TAP		;SHOE TAP CROSS
	('SC0' THRU 'SC8'),		NOP;	STORM COLLARS
	('SWF'),			NOP;	ST STL WIRE FEED 3-24-09
	('S'),				IS_VULCAN = 1	;CALL SADDLE
	('P4','P9'),			NOP;	CALL PP		;PAIR/PANTS
	('PV4'),			NOP;	CALL PP		
	ENDUSING

	RETURN				
;----------------------------------------------
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
	W2 = WTAG,	'XX'
	WX_KEY = W_KEY + W2
;;;	IF (W_KEY .NE. SAVKEY)
	IF (WX_KEY .NE. XSAVKEY)
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

	READ (CHNWRK, LINE, XSAVKEY)
	WQTY = SAVQTY

	W2 = WTAG,	'XX'		;need to reset key
	WX_KEY = W_KEY + W2

	WRITE (CHNWRK, LINE, XSAVKEY)
OUTKEY,
	SAVKEY = W_KEY
	XSAVKEY = WX_KEY
	SAVQTY = WQTY
	CLEAR MULTLINE
	RETURN
;-----------------------------------------------

EOF_CON,
	CALL NEWKEY
	RETURN
;-----------------------------------------------


LOAD_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; LOAD DUCTWORK INTO WORKFILE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Note: only the ductwork data is moved into the work file,
; it is assumed that all the acc info has already been moved
; to the ordlin file...

	FIND(CHN175,DUCACC,ORDNO) [ERR=LD_LOOP]
LD_LOOP,
	LOKCTL = 1
	XCALL IOS (CHN175,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.ORDNO) GOTO LD_EOF

	USING STY SELECT
	(1,4,5),	WD_DEPT='P'		;TDC
	(2,3,6),	WD_DEPT='O'		;S&D
	ENDUSING

; ssq 6-4-01 force duct work to end
	USING WD_DEPT(1,1) SELECT
	('O'),	WD_DEPT(1,1) = 'Y'
	('P'),	WD_DEPT(1,1) = 'Z'
	ENDUSING

	WDUTYPE = DUTYPE	; Type of duct 
	WSTY = 10-STY		; Style		descending
	WLINER = LINER		; Liner 
	WSEAM = SEAM		; Seam  
	WSEAL = SEAL		; Seal
	WGAUGE = GAUGE		; Gauge ( 26,24,22,20,18 )
	WSIZE3 = 100-SIZE3	; LENGTH	descending
	WSIZE1 = 1000000-SIZE1	; SLIP		descending
	WTHICK = THICK		; Dec. Thickness .024-24g .032-22g .040-20g .050-18g
	WCAT = CAT		; Catagory 
	WJOINT = JOINT		; Number of joints ( quantity)
	WSIZE2 = 1000000-SIZE2	; DRIVE		descending	

	WLOCAT = LOCAT
	WSQFEET = SQFEET
	WPOUNDS = POUNDS
	WSQFLIN = SQFLIN
	WGPRICE = GPRICE
	WLINPRC = LINPRC
	WDPUNCH = DPUNCH
	if (wdpunch .eq. '0') clear wdpunch	;ssq 6-28-05
	WDSEQ = DSEQ	
	STORE(CHNDUC,DUCWRK,WD_KEY)

	GOTO LD_LOOP
LD_EOF,
	RETURN
;-----------------------------------------------
CONS_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; CONSOLIDATE DUCTWORK
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR MULTLINE, SAVJOINT, SAVFEET, SAVFLIN, SAVLBS
	FIND(CHNDUC,DUCWRK,^FIRST) [ERR=EOF_CD]
	SAV_WD_KEY = '***'

CD_LOOP,
	READS(CHNDUC,DUCWRK,EOF_CD)
	IF(WD_KEY .NE. SAV_WD_KEY)
	THEN	CALL WD_NEWKEY
	ELSE	BEGIN
		INCR MULTLINE
		SAVJOINT = SAVJOINT + WJOINT
		SAVFEET = SAVFEET + WSQFEET
		SAVFLIN = SAVFLIN + WSQFLIN
		SAVLBS = SAVLBS + WPOUNDS
		DELETE(CHNDUC)
		END
	GOTO CD_LOOP		
EOF_CD,
	CALL WD_NEWKEY
	RETURN

WD_NEWKEY,
	IF(SAV_WD_KEY .EQ. '***') GOTO OUT_WDKEY
	IF(MULTLINE .EQ. 0) GOTO OUT_WDKEY
	READ(CHNDUC,DUCWRK,SAV_WD_KEY)
	WJOINT = SAVJOINT
	WSQFEET = SAVFEET
	WPOUNDS = SAVLBS
	WSQFLIN = SAVFLIN
	WRITE(CHNDUC,DUCWRK,SAV_WD_KEY)

OUT_WDKEY,
	SAV_WD_KEY = WD_KEY
	SAVJOINT = WJOINT
	SAVFEET = WSQFEET
	SAVFLIN = WSQFLIN
	SAVLBS = WPOUNDS

	CLEAR MULTLINE
	RETURN
;-----------------------------------------------

WRTMEM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; ROUTINE TO FORMAT AND PRINT WO'S
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear lines_printed
	clear fnote_local

	CLOSE CHNOUT
	OPEN (CHNOUT, SI, OUTFIL)
	XCALL FILL ('_', UNDER)
	SAVDPT = -1
	FIRST_PAGE = 1
	MEMO_ON = 0		;ADDED 2-16-01 to keep header from being
				;bolded if 1st line is a memo.

	
W_LOOP,
	READS (CHNOUT, LINE, W_EOF)

w_re_j,
	USING WDEPT SELECT	;7-29-08
	('FV','HV','KV'),	IF (WDEPT .NE. SAVDPT) CALL W_NEWDPT
	(),			IF (WDEPT(1,1) .NE. SAVDPT(1,1)) CALL W_NEWDPT
	ENDUSING

	lines_printed = 1

	IF (WTYPE .EQ. 'M')
	THEN	BEGIN
		IF (MEMO_ON .EQ. 0)
			BEGIN
			CLEAR PLINE
			CALL PRINT
			MEMO_ON = 1
			END
		if (fnl .eq. 1) fnote_local = 1		;f-notes requiring local printer
		bold_on = 1
		IF (MITEM.EQ.'   F1') 
		THEN	
			BEGIN
			XCALL HP (14, hpFONT, hpBOLD)	

			call red_box
;;;			isidx = 2	;2, red
			display (14, idx_sel)
			FOR J FROM 1 THRU 3
				BEGIN
				TMPDSC = MMEMO(J)
				CALL MAIN_MEMO
				PLINE (8,54) = TMPDSC
				IF (MMEMO(J) .NE. BLANKS) CALL PRINT
				END
			isidx = 0	;color 0, black
			display (14, idx_sel)
			END
		ELSE	BEGIN
			XCALL HP (14, hpFONT, hpBOLD)	
			prow = lincnt + 3
		;;;	isidx = 1	;1, blue
			isidx = 4	;4, blue
			display (14, idx_sel)
			xcall hp (14,hpPOS, prow, 7, 0, m_long(1,30))
			isidx = 0	;0, black
			display (14, idx_sel)
			xcall hp (14,hpPOS, prow, 38, 0, m_long(31,63))
			clear pline
			CALL PRINT
			isidx = 0	;color 0, black
			display (14, idx_sel)
			XCALL HP (14, hpFONT, hpMEDIUM)	
			GOTO W_LOOP
			END

		END
	ELSE	BEGIN
		USING WUM SELECT				;SSQ 3-30-06
		('RL','BG','BX'),	bold_on = 1
		(),			bold_on = 0
		ENDUSING
	;;;	IF (WDEPT .EQ. 'B')	bold_on = 1	;SSQ 3-30-06
	;;;	IF (WDEPT .EQ. 'A')	bold_on = 1	;SSQ 4-04-06
	;;;	IF (WDEPT .EQ. 'D')	bold_on = 1	;SSQ 4-04-06
	;;;	IF (WDDD2 .EQ. 'D')	bold_on = 1	;SSQ 4-11-06

		IF (WCNGD .EQ. 1)	bold_on = 1	;7-16-09

		MEMO_ON = 0
		CLEAR PLINE
		PLINE(1,6) = WQTY,	'ZZZZX-'
		PLINE(8,22) = WITEM
		PLINE(24,53) = WDESC
		PLINE(55,56) = WUM
		PLINE(58,60) = WPG
;ZZ,ZZX- AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA  AABBBBBBAAAAAAAAAAAAAAA
;ZZZZX- AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA AA AAABBBBBAAAAAAAAAAAAAAA
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
		IF (WTAG) 
			BEGIN
			d_long = f4_memol(wtag)
			pline(61,65) =  BOLD
			PLINE (67,80) = dl_ara(1)
			if(dl_ara(2).ne.blanks)
				begin
				call print	;current line
				pline(51,80) = dl_ara(2)
				call print
				if (dl_ara(3).ne.blanks)
					begin
					pline(51,80) = dl_ara(3)
					call print
					end
				end

			END

		CALL PRINT
		bold_on = 0
		if (wtag .or. wcngd) DISPLAY(14,MEDIUM)			;ssq 3-30-06
		END

	GOTO W_LOOP

;   QTY  ITEM NUMBER     DESCRIPTION                     UM
;ZZZZZZ  AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA  AA
;1234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
;====================================================
MAIN_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;
	TL = %TRIM(TMPDSC)
	REM = 46 - TL
	REM = REM/2
	IF (REM.GT.0)
		BEGIN
		CLEAR TMPDSC
		TMPDSC(1,REM) = DASHES
		TMPDSC(REM+1,REM+TL) = MMEMO(J)
		TMPDSC(REM+1+TL,46) = DASHES
		END
	RETURN
;-----------------------------------------
W_EOF,
	if (savdpt.eq.'J'.and. j_cnt.le.1) 
		begin
		call w_newdpt
		goto w_re_j
		end

	using savdpt select
	('Y','Z'),	begin
			call w_newdpt	;print these dept's w/ duct
			end

	(),		begin
			clear got_duc
			call w_newdpt
			close 14
			end
	endusing

	CLOSE CHNOUT
	CLOSE CHNWRK
	XCALL DELET (WRKFIL)
	XCALL DELET (OUTFIL)
	RETURN
;===============================================

W_NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; print work order for each dept
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	for i from 1 thru maxc
		begin
		if (savdpt .eq. cdpt(i) ) goto d_ok	;this dept changed
		if (cdpt(i) .eq. a1)
			begin
			close 14		;this dept did not change
			call opn2	;clears lines_counted
			exitloop	;this should fall thu to skip_d...
			end
		end

d_ok,

; if the prev dept was 'J', then print another copy...
	if (savdpt.eq.'J' .and. j_cnt.le.1)
	then	begin
		read (chnout, line, rfa:j_rfa)
		j_cnt = 2			;pass 2
		end
	else	begin
		xcall getrfa(chnout, j_rfa)	;save the rfa
		j_cnt = 1
		end

	if (lines_printed .eq. 0) 
		begin
		call clp	;nothing to print
		goto skip_d_print
		end

	if(savdpt(1,1) .eq. 'Y')	;ductwork
		begin
		got_duc = 1	;print these dept's w/ duct
		call clp	;nothing to print
		goto skip_d_p2
		end

; create an print a spool file for each dept...
	writes (14, eject)
	close 14

	numcop = 1

	ln = %trim (printq)

	if (ln .lt. 3)		goto print_local	;blank or ivalid que
	if (local .eq. 1)	goto print_local	;local param passed
	if (fnote_local)	goto print_local	;f-note requires local

	upcase printq
	
	onerror print_local
	lpque (splfil, copies:numcop, lpnum:printq(1,ln), delete)		
	offerror

skip_d_print,

	LINCNT = 66
	CLEAR PAG
	CLEAR NPAG

	SAVDPT = WDEPT
skip_d_p2,
	IF(SAVDPT(1,1) .EQ. 'Y')	;DUCTWORK
	THEN	BEGIN
		DUCTOR = ORDNO
		LOKCTL = 1
		XCALL ISIO (CHN175,DUCACC,DUCTOR,READ,LOKCTL)
		IF (LOKCTL.EQ.0.AND.DUCTOR.EQ.ORDNO)
			BEGIN
			call count_lines
			num_p = cl_count	;prior lines
			CALL COUNT_DUCT
			END
		END
	ELSE	CALL COUNT_LINES

	RETURN				;from w_newdept
;------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; these routines call from w_newdept...
print_local,
	offerror

	IF (OSCAC .EQ. '10')		;use will-call printer
	THEN	LPQUE (SPLFIL, copies:numcop, LPNUM:"new_blue", delete)
	ELSE	LPQUE (SPLFIL, copies:numcop, delete)

OPN2,
	call open_splfil
clp,
	clear lines_printed
	return
;------------------------------------


COUNT_LINES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL GETRFA(CHNOUT, WD_RFA)
;;;	CLEAR CL_COUNT
	CL_COUNT = 1		;SSQ 6-4-01 already read 1st record...
	SAV_MO = MEMO_ON
CL_LOOP,
	READS (CHNOUT, LINE, CL_EOF)
	IF (WDEPT(1,1) .NE. SAVDPT(1,1)) GOTO CL_EOF
	IF (WTYPE .EQ. 'M')
	THEN	BEGIN
		IF (SAV_MO .EQ. 0)
			BEGIN
			INCR CL_COUNT
			SAV_MO = 1
			END
		IF (MITEM.EQ.'   F1') 
		THEN	FOR J FROM 1 THRU 3
			BEGIN
			IF (MMEMO(J) .NE. BLANKS) INCR CL_COUNT
			END
		ELSE	BEGIN
			INCR CL_COUNT
			GOTO CL_LOOP
			END

		END
	ELSE	BEGIN
		SAV_MO = 0
		INCR CL_COUNT
		END

	GOTO CL_LOOP
CL_EOF,
	NPAG = CL_COUNT/44 + 1
	XCALL POSRFA(CHNOUT, WD_RFA)
;;;	READS (CHNOUT, LINE, W_EOF) 	;GET BACK TO LAST LINE READ
	READS (CHNOUT, LINE, no_rfa) [err=no_rfa] 	;GET BACK TO LAST LINE READ
no_rfa,
	RETURN
;----------------------------------------------------

GETDUC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PROCESS DUCTWORK
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (got_duc .ne. 1)	call open_splfil

	READ(CHNDUC,DUCWRK,^FIRST) [ERR=GETDUC_EOF]

	got_duc = 1		;at least 1 line printed...

	GOTO CONDUC
NXTDUC,
	READS(CHNDUC,DUCWRK,GETDUC_EOF) [ERR=GETDUC_EOF]
CONDUC,
	IF (SAVDPT(1,1) .NE. WD_DEPT(1,1)) 
		BEGIN
		CALL PRTSDU
		CLEAR PAG
		SAVDPT=WD_DEPT
		WDEPT=WD_DEPT
		clear num_p
		CALL COUNT_DUCT
		CALL PRTHDR
		END

	WSIZE3 = 100-WSIZE3	; LENGTH 	restore original value
	WSIZE1 = 1000000-WSIZE1	; SLIP		restore original value
	WSIZE2 = 1000000-WSIZE2	; DRIVE		restore original value	
	WSTY = 10-WSTY		; Style		restore original value
	SZ1 = %TRN3(WSIZE1)
	SZ2 = %TRN3(WSIZE2)

	CALL PRTDUC

SUMDUC,
	CONFIG(1,3) = WGAUGE
	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3) CONFIG(1,3) = WTHICK
	CONFIG(4,4) = WDUTYPE
	CONFIG(5,5) = WCAT
	CONFIG(6,6) = WSTY
	CONFIG(7,7) = WSEAM

		; Check to see if the configuration has all ready 
		; been on this order
	I =
	DO BEGIN
	  INCR I
	  DUCREC = SDUCRC(I)
	END
	UNTIL (DUCCFG.EQ.CONFIG.OR.DUCCFG.EQ.0.OR.I.GE.MAXDUC)
	IF (I.GE.MAXDUC) GOTO BADCON

	DUCCFG = CONFIG
	DUCSQF = DUCSQF + WSQFEET
	DUCPND = DUCPND + WPOUNDS
	DUCGPR = WGPRICE
	SDUCRC(I) = DUCREC

	IF (WLINER.GE.1.AND.WLINER.LE.8.AND.WLINER.NE.4)
&		LINSQF(WLINER) = LINSQF(WLINER) + WSQFLIN

	GOTO NXTDUC

GETDUC_EOF,
	CALL PRTSDU
	
;-
	WRITES (14, EJECT)
	CLOSE 14
	if (.not. got_duc) return	;nothing to print

	IF (OSCAC .EQ. '10')		;use will-call printer
	THEN	LPQUE (SPLFIL, LPNUM:"new_blue", delete)
	ELSE	LPQUE (SPLFIL, copies:1, delete)

;-
	RETURN
;------------------------------------------

COUNT_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; COUNT DUCT PRINT LINES
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR CL_COUNT, num_dl, num_s, rem, rem_d
	CLEAR ARRAYS
	FIND(CHNDUC,DUCWRK,SAVDPT)[ERR=CD_NEXT]

CD_NEXT,
	READS(CHNDUC,DUCWRK,CD_EOF)
	if(wd_dept .ne. savdpt) goto cd_eof
	CL_COUNT = CL_COUNT + 3
	incr num_dl
	CONFIG(1,3) = WGAUGE
	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3) CONFIG(1,3) = WTHICK
	CONFIG(4,4) = WDUTYPE
	CONFIG(5,5) = WCAT
	CONFIG(6,6) = WSTY
	CONFIG(7,7) = WSEAM

		; Check to see if the configuration has all ready 
		; been on this order
	I =
	DO BEGIN
	  INCR I
	  DUCREC = SDUCRC(I)
	END
	UNTIL (DUCCFG.EQ.CONFIG.OR.DUCCFG.EQ.0.OR.I.GE.MAXDUC)
	IF (I.GE.MAXDUC) GOTO CD_EOF

	DUCCFG = CONFIG
	DUCSQF = DUCSQF + WSQFEET
	DUCPND = DUCPND + WPOUNDS
	DUCGPR = WGPRICE
	SDUCRC(I) = DUCREC

	IF (WLINER.GE.1.AND.WLINER.LE.8.AND.WLINER.NE.4)
&		LINSQF(WLINER) = LINSQF(WLINER) + WSQFLIN

	GOTO CD_NEXT

CD_EOF,	;GET BACK TO FIRST DUCT RECORD...

	CALL CD_SDU

	FIND(CHNDUC,DUCWRK,SAVDPT)[ERR=CDE_NEXT]
CDE_NEXT,
	READS(CHNDUC,DUCWRK,NO_CDE)
NO_CDE,
	CLEAR ARRAYS

	npag = 0		
	rem= 44 - num_p		;subtract prior lines
	dl_3 = rem/3		;number of duct work "records" left on page
	if (dl_3 .ge. 13)
	then	rem_d = num_dl - 13
	else	rem_d = num_dl - dl_3

check,
	if(rem_d .ge. 13)
		begin
		incr npag
		rem_d = rem_d - 13
		goto check
		end

	incr npag
	rem = rem_d * 3		;convert remaining duct "records"
				;back to print lines.
	
	rem = rem + num_s	;add summary lines
check2,
	if(rem .le. 44) goto c_don
	rem = rem - 44
	incr npag
	goto check2
c_don,	
	RETURN
;--------------------------------------------

CD_SDU,
		;Print summarized duct from this order
	INCR CL_COUNT

	FOR I FROM 1 THRU MAXDUC
	  BEGIN
	  DUCREC = SDUCRC(I)
	  IF (DUCCFG.EQ.0) GOTO CD_SD2
	  INCR CL_COUNT
	incr num_s
	  SDUCRC(I) =
	END
CD_SD2,
	FOR I FROM 1 THRU 8
	  BEGIN
	  IF (LINSQF(I).NE.0.AND.I.NE.4)
		BEGIN
		INCR CL_COUNT
	incr num_s
		LINSQF(I) =
		END
	  END

	RETURN	
;----------------------------------------------------


BADCON,
	FORMS (14,2)
	PLINE (32,60) = 'DUCT INFORMATION IS '	
	CALL PRINT
	GOTO ERRMSG
BADACC,
	FORMS (14,2)
	PLINE (32,60) = 'ACCESSORY INFORMATION IS'	
	CALL PRINT
ERRMSG,
	PLINE (32,60) = 'PRESENTLY OVERFLOWING THE'
	CALL PRINT
	PLINE (32,60) = 'ARRAY BOUNDARIES.  PLEASE CALL'
	CALL PRINT
	PLINE (32,60) = 'SOFTWARE SUPPORT TO CORRECT THE'
	CALL PRINT
	PLINE (32,60) = 'PROBLEM'
	CALL PRINT
	RETURN
;---------------------------------------------------

PRTDUC,		;Print duct record

	IF ((LINCNT+3).GT.MAXLIN) CALL PRTHDR

	CLEAR JT_ERR
	USING WCAT SELECT
	(1),	BEGIN			;LSHAP
		PLINE(1,6) = WJOINT*2,	'ZZZZZ-' 
		IF (WJOINT*2 .GT. 99999) JT_ERR = 1
		END
	(2),	BEGIN			;WRAPAROUND
		PLINE(1,6) = WJOINT,	'ZZZZZ-'
		IF (WJOINT .GT. 99999) JT_ERR = 1
		END
	(3),	BEGIN			;4 PC
		JT1 = (WJOINT*2),	'ZZZ' [RIGHT]
		JT2 = (WJOINT*2),	'ZZZ' [LEFT]
		PLINE(1,7) = NJOINT
		IF (WJOINT*2 .GT. 999) JT_ERR = 1
		END
	(4),	BEGIN			;OTHER
		PLINE(1,6) = WJOINT*2,	'ZZZZZ-' 
		IF (WJOINT*2 .GT. 99999) JT_ERR = 1
		END
	(5),	BEGIN			;OVERSZ
		JT1 = (WJOINT*2),	'ZZZ' [RIGHT]
		JT2 = (WJOINT*2),	'ZZZ' [LEFT]
		PLINE(1,7) = NJOINT
		IF (WJOINT*2 .GT. 999) JT_ERR = 1
		END

	ENDUSING
	PLINE(9,11) = 'PCS'
	IF (JT_ERR .EQ. 1) PLINE(1,12) = 'Qty Overflow'

	IF (WDUTYPE.NE.2.AND.WDUTYPE.NE.3)
	BEGIN
	  PLINE (13,14) = WGAUGE,'XX'
	  PLINE (15,16) = 'GA'
	END
	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3)
	BEGIN
	  PLINE (13,16) = WTHICK,'.XXX'
	END
	IF(SZ1.EQ.WSIZE1/1000 .AND. SZ2.EQ.WSIZE2/1000)
	THEN	BEGIN
		PLINE(20,21)=WSIZE3,'ZX'
		PLINE(23,23)='X'
		PLINE(24,26)=SZ1,'ZZX'
		PLINE(27,27)='X'
		PLINE(28,30)=SZ2,'ZZX'
		PLINE(35,38)=DTYPE(WDUTYPE)
		PLINE(40,45)=DCAT(WCAT)
		PLINE(47,49)=DSTY(WSTY)
		PLINE(51,53)=DSEAM(WSEAM)
		IF (WSEAL.EQ.1) PLINE (55,58) = 'SEAL'
		END
	ELSE	BEGIN
		PLINE(20,21)=WSIZE3,'ZX'
		PLINE(23,23)='X'
		PLINE(24,30)=WSIZE1,'ZZX.XXX'
		PLINE(31,31)='X'
		PLINE(32,38)=WSIZE2,'ZZX.XXX'
		PLINE (42,45) = DTYPE(WDUTYPE)
		PLINE (48,53) = DCAT(WCAT)
		PLINE (55,57) = DSTY(WSTY)
		PLINE (60,62) = DSEAM(WSEAM)
		IF (WSEAL.EQ.1) PLINE (64,67) = 'SEAL'
		END
	CALL PRINT

	PLINE (  1,7  ) = WPOUNDS,NUMASK
	PLINE (  8,8  ) = '#'
	PLINE ( 12,17 ) = WSQFLIN,NUMASK
	PLINE ( 19,23 ) = ;;;>'SQ FT'
	PLINE (24,38) = '     NO LINER '
	IF (WLINER.GE.1.AND.WLINER.LE.8.AND.WLINER.NE.4)
	BEGIN
	  PLINE (25,31) = DLINER( WLINER )
	  PLINE (32,36) = 'LINER'
	END
	PLINE (38,52) = WLOCAT
	PLINE (54,80) = WDPUNCH
	CALL PRINT
	CALL PRINT		;3/22/00 SSQ
	RETURN


PRTSDU,
		;Print summarized duct from this order
	INCR LINCNT
	FOR I FROM 1 THRU MAXDUC
	  BEGIN
	  DUCREC = SDUCRC(I)
	  IF (DUCCFG.EQ.0) GOTO PRTSD2
	  IF ((LINCNT+1).GT.MAXLIN) CALL PRTHDR
	  PLINE (1,7) = DUCPND,NUMASK
	  PLINE (8,8) = '#'
	  PLINE (18,19) = DUCCFG(1,3),'XX'
	  PLINE (21,22) = 'GA'
	  IF (DUCCFG(4,4).EQ.2.OR.DUCCFG(4,4).EQ.3)
	  BEGIN
	    PLINE (18,22) = DUCCFG(1,3),'.XXX'
	  END
	  PLINE (33,36) = DTYPE(DUCCFG(4,4))
	  PLINE (38,43) = DCAT(DUCCFG(5,5))
	  PLINE (45,47) = DSTY(DUCCFG(6,6))
	  PLINE (49,51) = DSEAM(DUCCFG(7,7))
	  CALL PRINT
	  SDUCRC(I) =
	END
PRTSD2,
	FOR I FROM 1 THRU 8
	  BEGIN
	  IF (LINSQF(I).NE.0.AND.I.NE.4)
	  BEGIN
	    IF ((LINCNT+1).GT.MAXLIN) CALL PRTHDR
	    PLINE (1,7) = LINSQF(I),NUMASK
	    PLINE (8,10) = 'SQF'
	    PLINE (33,39) = DLINER(I)
	    PLINE (40,45) = ' LINER'
	    CALL PRINT
	    LINSQF(I) =
	  END
	END
	RETURN	

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INCR LINCNT
	IF (LINCNT .GE. 58) 
		BEGIN
		SAVLIN = PLINE
		sav_bold = bold_on
		bold_on = 0
		CALL PRTHDR
		PLINE = SAVLIN
		bold_on = sav_bold
		END

	if (bold_on) display (14,bold)
	WRITES (14, PLINE)
	if (bold_on) display (14,medium)

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
	IF (.NOT. FIRST_PAGE) WRITES (14, EJECT)
	CLEAR FIRST_PAGE

	CLEAR LINCNT
	CLEAR PLINE

	DISPLAY(14,MEDIUM)	;reset to medium

	DT = %DATETIME
	D = DT(1,8)
	T = DT(9,14)
	DATE = D,	'XXXX-XX-XX'
	TIME = T,	'XX:XX:XX'
	PLINE = DATIME

	PLINE(32,67)=OCOMNT(1)
	CALL PRINT
	PLINE(32,67)=OCOMNT(2)
	CALL PRINT

	if (oprtf.gt.1 .and. local.ne.1)
		begin
		display (14, bold)
		pline (28,80) = 'N E W   C O P Y'
		end

	CALL PRINT

	display (14, medium)

;----------
; ssq 6-4-01 force duct work to end
	USING WDEPT(1,1) SELECT
	('Y'),	XDEPT= 'O'
	('Z'),	XDEPT= 'P'
	(),	XDEPT = WDEPT
	ENDUSING
;----------

	PLINE(28,29) = XDEPT
	PLINE(31,31) = '-'
	CLEAR TBL_KEY
	TBLCOD = 'DD'
	TBLKEY = XDEPT
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		DD_Desc = "* NOT ON FILE *"
		END

	printq = dd_pque		;save the print que name

	if (xdept.eq. 'J' .and. j_cnt.eq.2) dd_desc = 'Packing List'

	PLINE(33,62) = DD_DESC
	
	OF_ORD = OORDNO,	'ZZZZZX'
	OF_NUM = OPRTF
	PLINE(62,69) = ORDFMT
	CALL PRINT2

	INCR PAG

	PL_INFO = 1
	USING XDEPT SELECT
	('FV'),	IF (HAS_F .EQ. 1) PLINE(28,60) = 'CHECK F COPY'
	('F'),	IF (HAS_FV .EQ. 1) PLINE(28,60) = 'CHECK FV COPY'
	('HV'),	IF (HAS_H .EQ. 1) PLINE(28,60) = 'CHECK H COPY'
	('H'),	IF (HAS_HV .EQ. 1) PLINE(28,60) = 'CHECK HV COPY'
	('KV'),	IF (HAS_K .EQ. 1) PLINE(28,60) = 'CHECK K COPY'
	('K'),	IF (HAS_KV .EQ. 1) PLINE(28,60) = 'CHECK KV COPY'
	ENDUSING

	P1 = PAG,	'ZX'
	P2 = NPAG,	'ZZ' [LEFT]
	PLINE(62,69) = PAGEOF
	CALL PRINT2

	FORMS (14,1)

	PLINE(10,15) = OCUSNO,	'ZZZZZZ' [LEFT]
	PLINE(25,30) = CRDLMT,	'ZZZZZZ' [LEFT]
	CALL PRINT2

	PLINE(1,8) = 'BILL TO:'
	PLINE(43,50) = 'SHIP TO:'
	PLINE(10,39) = NAME
	PLINE(52,80) = OSHPNM
	CALL PRINT2

	PLINE(10,39) = ADD1
	PLINE(52,80) = OSHAD1
	CALL PRINT2

	PLINE(10,39) = ADD2
	PLINE(52,80) = OSHAD2
	CALL PRINT2

	PLINE(10,39) = CSZ
	PLINE(52,80) = OSHAD3
	CALL PRINT2

	FORMS (14,2)
	WRITES (14, H_LINE)
	PLINE(3,12) = E_DATE
	PLINE(15,26) = SNAM

	XPOL1 = OPONO
	XPOL2 =
	IF (OMETRO .NE. BLANKS)
		BEGIN
		XPOL1 = OMETRO
		XPOL2 = OPONO
		END

	PLINE(28,39) = XPOL1
	PLINE(41,50) = OJOBNO
	PLINE(53,65) = SV_NAME

	USING OCLPPD SELECT
	('C'),	PLINE(66,68)='COL'
	('P'),	PLINE(66,68)='PPD'
	('D'),	PLINE(66,68)='COD'
	ENDUSING

	PLINE(70,79) = S_DATE
	CALL PRINT2

	PLINE(28,39) = XPOL2
	CALL PRINT2

	FORMS(14,1)

	USING WDEPT SELECT
;;;	('O','P'),	WRITES (14, H_LINED)
	('Y','Z'),	WRITES (14, H_LINED)
	(),		WRITES (14, H_LINE2)
	ENDUSING

	LINCNT = 17

	RETURN

;DATE ENTERED    SLS-REP     PO NUMBER   JOB NUMBER  SHIP VIA         SHIP DATE
;  XX/XX/XXXX    AAAAAAAAAA  AAAAAAAAAA  AAAAAAAAAA  AAAAAAAAAAAAAAA  XX/XX/XXXX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
;-----------------------------------------------

GET_SCAC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; COP TABLE SCAC LOOK-UP
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'SC'
	TBLKEY = OSCAC
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		SC_NAME = "* NOT ON FILE *"
		END
	SV_NAME = SC_NAME
	RETURN
;-----------------------------------------------------

GET_SALESMAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL SREP(OSLMAN,LNAM,SNAM,SINT)
	RETURN
;-----------------------------------------------------

open_splfil,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	OPEN (14, O, SPLFIL)

	set re, e1,e2,e5 = e_char
	
	display (14, reset)


; set the color palettes...
	xcall ascii (0, hex0)
	xcall ascii (1, hex1)
	xcall ascii (8, hex8)
	set lx(1), lx(2) = hex1
	set lx(3) = hex8
	set lx(4), lx(5), lx(6) = hex0
	
	writes (14,l1)

	ipc1 = 255
	ipc2 = 255
	ipc3 = 255
	ipidx = 0	;black
	display (14,idx_pal)

	ipc1 = 0
	ipc2 = 0
	ipc3 = 255
	ipidx = 1	;green
	display (14,idx_pal)

	ipc1 = 0	;B
	ipc2 = 255	;R
	ipc3 = 0	;G
	ipidx = 2	;red
	display (14,idx_pal)

	ipc1 = 0
	ipc2 = 255
	ipc3 = 255
	ipidx = 3	;orange
	display (14,idx_pal)

	ipc1 = 255	;B
	ipc2 = 0	;R
	ipc3 = 0	;G
	ipidx = 4	;blue
	display (14,idx_pal)

	ipc1 = 255
	ipc2 = 0
	ipc3 = 255
	ipidx = 5	;green
	display (14,idx_pal)

	ipc1 = 0
	ipc2 = 48
	ipc3 = 194
	ipidx = 6	;dark blue
	display (14,idx_pal)

	ipc1 = 194
	ipc2 = 0
	ipc3 = 145	
	ipidx = 7	;purple
	display (14,idx_pal)

	ipc1 = 240
	ipc2 = 0
	ipc3 = 180
	ipidx = 8	;bright pink
	display (14,idx_pal)

	ipc1 = 214
	ipc2 = 164
	ipc3 = 0
	ipidx = 9	;bad yellow
	display (14,idx_pal)

;--------------------------------------

	XCALL HP (14,hpDOTS, 0)

	return
;---------------------------------------------------

red_box,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	isidx = 2	;2, red

	xcall hp (14,hpPOS, prow, 7, 0,' ')	;cursur pos...

	cd = '*c1470A'	;width, 49 chars at 10 char/inch
	call put

	cd = '*c75B'	;height
	call put

	cd = '*c70G'	;70%
	call put

	cd = '*c2P'
	call put
	
	isidx = 0	;back to black
	return
;---------------------------------------------

put,
	ln = %trim (code)
	display (14,code(1,ln) )
	return
;-------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	XCALL ASCII (27, E_CHAR)

	E_CHA2 = E_CHAR
	E_CHA3 = E_CHAR

	SWITCH = 5
	XCALL CHNOPN (24,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (24,'SI',044,SWITCH)
		END
	CHN044 = 24

	XCALL CHNOPN (25,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (25,'SI',045,SWITCH)
		END
	CHN045 = 25

	XCALL CHNOPN (26,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (26,'SI',175,SWITCH)
		END
	CHN175 = 26

	XCALL CHNOPN (36,STAT)
	IF (STAT .EQ. 0) 
		BEGIN
		XCALL FILES (36,'I',001,SWITCH)
		END
	CHN001 = 36

	XCALL CHNOPN (37,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (37,'I',002,SWITCH)
		END
	CHN002 = 37


	XCALL CHNOPN(38,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (38,'I',054,SWITCH)
		END
	CHN054 = 38

	CHN182 = 17	;COP TABLES

	XCALL ISIO (CHN044, ORDHDR, ORDNO, READ, LOKCTL)
	WFORD = ORDNO,'XXXXXX'
	XCALL DATE8(OORDDT, D_OUT, D_OUTR, E_DATE, D_SW)
	XCALL DATE8(OPROMD, D_OUT, D_OUTR, S_DATE, D_SW)

	CALL GET_SCAC
	CALL GET_SALESMAN

	XCALL IO (CHN001, DUMCUS, 1, READ, LOKCTL)

	KEY = OCUSNO,'XXXXXX'
	XCALL SERCH (CHN002,CUSIDX,KEY,1,6,ORG001,BSMID,SRCCTL,4,7,11,0,0,0,0)
	LOKCTL = 1
	CASE SRCCTL OF 
	BEGINCASE
	  0:	XCALL IO (CHN001,CUSMAS,IRC001,READ,LOKCTL)
	  1:	BEGIN
		CUSMAS =
		NAME = '* CUSTOMER NOT ON FILE *'
		END
	ENDCASE

	CSZ(1,15) = CITY
	CSZ(17,18) = STATE
	CSZ(20,29) = ZIP

;Create work file...
	XCALL ISAMC (WRKFIL, 131, 1, key_spec)
;;;	XCALL ISAMC (WRKFIL, 130, 1, key_spec)
	OPEN (33, SU, WRKFIL)
	CHNWRK = 33

	OFORD = ORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (OUTFIL, 131, 1, key_spec)
;;;	XCALL ISAMC (OUTFIL, 130, 1, key_spec)
	OPEN (34, SU, OUTFIL)
	CHNOUT = 34

	WRORD = ORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (39, SU, DUCFIL)
	CHNDUC = 39


	SPORD = ORDNO,'XXXXXX'

	call open_splfil
;;;	OPEN (14, O, SPLFIL)
;;;	XCALL HP (14,hpDOTS, 0)

	OPNOK = 1

	RETURN
;-------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN044
	CLOSE CHN045
	CLOSE CHN175
	RETURN
	END

