;PDEST.CP
;	PRINT ESTIMATE, DEPT/WO FORMAT
;
;	13-NOV-03: read company info from file instead of hard-coding
; 5-30-18 ssq: make cusmas isam

SUBROUTINE	PDEST
	ORDNO	,D

;DDEPT 5-29-14

	.INCLUDE 'DEF:HPSUB.DEF'

EXTERNAL FUNCTION
	TRN3	,D

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR

record	pterms
	terms0,	a*,	'All prices are subject to change without notice, all bids are valid for 30 days.'
	terms1,	a*,	'All shipments are F.O.B. Minneapolis, MN. or F.O.B. Rockford, IL.'
	terms2,	a*,	'No returns unless authorized by SMC.'
	terms2c,a*,	'No returns unless authorized by SMP.'
	terms3,	a*,	'All Accounts have payment terms of 1% 10/net 30'
	terms3c,a*,	'All Accounts have payment terms of net 30'
	terms4,	a*,	'All Accounts Past Due Subject to Finance Charges of 1.5% Per Month'

RECORD ARTERM
		.INCLUDE 'DEF:RD170A.DEF'
RECORD ARTCTL	,X
		.INCLUDE 'DEF:RD170B.DEF'

RECORD	COINFO
	.INCLUDE 'DEF:COINFO.DEF'

RECORD	FAXFIL
	.INCLUDE 'DEF:FAXFIL.DEF'

RECORD	CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'
RECORD	DUMCUS
	.INCLUDE 'DEF:RD001B.DEF'
;;;RECORD	CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'

RECORD	SALMAN
	.INCLUDE 'DEF:RD054A.DEF'

RECORD	SHIPTO
	.INCLUDE 'DEF:RD171A.DEF'	;5-18-09

RECORD	TMPCUS
	.INCLUDE 'DEF:RD139A.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.def'
RECORD,X
	.INCLUDE 'DEF:RD045M.def'
RECORD,X
	.INCLUDE 'DEF:RD045D.def'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD PARAM
	.INCLUDE 'DEF:PARAM2.DEF'


GLOBAL DUCK	
	.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL


RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT

RECORD	RESET
	R_CHAR	,A1
		,A1,	'E'
RECORD	HP_V
	H_CHAR	,A1		;<ESC>
		,A4,	"&l8C"	;PAGE EJECT

RECORD	WRKFIL
		,A4,	'SPL:'
		,A1,	'W'
	WFORD	,A6
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
	
RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.new'

RECORD	DUCFIL
		,A4,	'SPL:'
		,A1,	'D'
	WRORD	,A6
		,A4,	'.ISM'

RECORD	F_MEMOS
	.INCLUDE 'DEF:CPMEMO.DEF'
		

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
	LINSQF	,9D6	;SUMMARIZE LINER SQUARE FEET

RECORD DUCREC
	DUCCFG	,D7	;CONFIGURATION CODE
	DUCSQF	,D7	;SQ FEET OF MATERIAL
	DUCPND	,D7	;POUNDS OF MATERIAL
	DUCGPR	,D7	;GAUGE PRICE

RECORD ACCREC
	.INCLUDE 'DEF:ACCREC.DEF'


RECORD	SAVE_LINE
;;;		,A132		;SAME LENGHT A LINE
		,A133		;SAME LENGHT A LINE
RECORD	LINE			
	WSS	,D1
	WSEQ1	,D2		;reversed
	WSEQ2	,D2
	WSEQ3	,D2	
	WDEPT	,A2
	WSQNO	,A2
	WITEM	,A15
;;;	WPRICE	,D8
	WDESC	,A90
	WPRICE	,D8
	WTYPE	,A1		;L=LINE, M=MEMO
	WQTY	,D5
	WUM	,A2
	WCPFLG	,D1
RECORD	MEMO,X
		,D1
	MSEQ	,D6
	MDEPT	,A2
	MSQNO	,A2
	MITEM	,A15
	MMEMO	,3A30
		,D8
		,A1
		,D5
		,A2
		,D1
RECORD	,X
		,D1
		,D6
		,A2
		,A2
		,A15
	M_LONG	,A63
		,A27
		,D8
		,A1
		,D5
		,A2
		,D1
RECORD,X
	W_KEYA	,A56	;FIRST PART OF KEY
		,A60
	W_KEYB	,A8		;PRICE
RECORD,X
		,D1
		,D2
	COMSEQ	,D4
		,A2		;reversed
;---------------------------
RECORD	LINE2			
	WSS2	,D1
	WSEQ12	,D2		
	WSEQ22	,D2
	WSEQ32	,D2	;REVERSE
	WDEPT2	,A2
	WSQNO2	,A2
	WITEM2	,A15
	WDESC2	,A30
	WADD2	,A60	;FILLER
	WPRICE2	,D8
	WTYPE2	,A1		;L=LINE, M=MEMO
	WQTY2	,D5
	WUM2	,A2
	WCPFLGW	,D1
RECORD,X
	W_KEY2A	,A56		;FIRST PART OF KEY
		,A60
	W_KEY2B	,A8		;PRICE

RECORD,X
		,D1
		,D2
	COMSEQ2	,D4
		,A2		;not reversed
;---------------------------

RECORD	H_LINE
	,A*,	'DATE ENTERED    SLS-REP     PO NUMBER   JOB NUMBER'
	,A*,	'  SHIP VIA         SHIP DATE'

RECORD	H_LINE2
	,A*,	'   QTY  ITEM NUMBER     DESCRIPTION                     UM'

RECORD	PC_LINE
		,A*,	'PRESSURE CLASS'
		,A1
	PC_PC	,A5
		,A1
		,A2,	'IN'

RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN044	,D2
	CHN045	,D2
	CHN054	,D2
	CHN139	,D2
	CHN171	,D2
	CHN175	,D2
	CHN182	,D2
	CHNWRK	,D2
	CHNOUT	,D2
	CHNFAX	,D2
	CHNDUC	,D2
	CHN170	,D2

RECORD	PPRN
	PLINE	,A95
RECORD,X
		,A12
	RLINE	,A83

RECORD	FOB
	FOB_N	,A5	;MPLS.
		,A3,	' - '

RECORD	PVARS
	A2		,A2
	NEG_ZERO	,D1,	1
	ZERO		,D1,	0
	X_FRT		,D1
	A_FRT		,A6
	LROW		,D2
	LCOL		,D3
	TOTPRC		,D8
	NO_DOLLARS	,D1
	PAGE		,D2
	LFEED		,D2
	ENTRY		,A30
	INXCTL		,D1
	TODAY		,D6
	XDATE		,D6
	MEMO_ON	,D1
	MAXLIN	,D2,	60
	LINCNT	,D2
	PAG	,D2
	NPAG	,D2
;;;	PLINE	,A95
	SAVLIN	,A95
	BLANKS	,A30
	BLANK90	,A90
	TMPDSC	,A62
	REM	,D2
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

RECORD	FOR_ISAMC
	KEYSPEC,	A*,	'START=1:117, LEN=56:8, DUPS, ASCEND'

RECORD	VARS
	CMPCOD	,A3			;11-6-19
	TOTPND	,D10
	MASK1	,A6,'ZZZZX-'
	orgart	,d6
	a5	,a5
	SAVSS	,D1
	dt	,a20	;date/time stamp
	dt_date	,d8
	dt_time	,d4

	I	,D6
	AREAC		,D3	;AREA CODE
	VALID_AC	,D1
	LNAM		,A25
	SNAM		,A12
	SINT		,A3
	SZ1		,D3
	SZ2		,D3
	SAVJOINT	,D5
	SAVFEET		,D6
	SAVFLIN		,D6
	SAVLBS		,D6
	OPNOK	,D1
	FIRST_PAGE	,D1
	M_RFA	,A6
	M_ON	,D1
	A6A	,A6
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
	SAVCOM	,D4
	SAVKEY	,A63
	W_KEY	,A63
	W_KEY2	,A63
	SAVQTY	,D5
	MULTLINE	,D5
	SKIP_LINE	,D1		;SKIP THIS LINE IF BLOCK MEMO
	STAT	,D3
	READ	,D1,0
	WRITE	,D1,1
	LOKCTL	,D1
	SWITCH	,D1

PROC
	XCALL ASCII (27, E_CHAR)
	H_CHAR = E_CHAR
	R_CHAR = E_CHAR

	XCALL WHO(CMPCOD)		;11-6-19

	CLEAR BLANKS, BLANK90

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
	call read_liner			;2-22-17

	XCALL RDATE(TODAY)
	CALL TIME_STAMP

	XCALL OUTPT (24,1,1,'SUPPRESS PRICE INFO ?',1)
	XCALL INPUT (24,23,01,00,'YN',ENTRY,INXCTL,1)
	IF (INXCTL.EQ.1) 
	THEN	NO_DOLLARS = 1
	ELSE	CLEAR NO_DOLLARS

	MEMO_ON = 1
	CLEAR M_RFA, M_ON
	CALL LOAD_MEMOS
	CALL LOAD_WORK
	CALL CONSOLIDATE

	CALL LOAD_DUCT
	CALL CONS_DUCT

;---------------------------------
	XCALL HP (14,hpDOTS,$false)

	TOTPRC =
	PAGE = 1
	MAXLIN = 34

	CALL PRTHDR
;---------------------------------

;;; Add memos to output file...
	CLEAR SAVCOM, SAVSQ1, SAVDPT
	clear savss

	FIND (CHNWRK, LINE, ^FIRST)[ERR=EOF_BM]

BM_LOOP,
	READS (CHNWRK, LINE, EOF_BM)
	IF (WDEPT .NE. SAVDPT) CALL NEWDPT
	IF (WSEQ1 .NE. SAVSQ1) CALL NEWSQ1
	IF (COMSEQ .NE. SAVCOM) CALL NEWCOM

	GOTO BM_LOOP
EOF_BM,
	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_FIN]
FIN_LOOP,
	READS (CHNWRK, LINE, EOF_FIN)
	W_KEY = W_KEYA+W_KEYB
	STORE (CHNOUT, LINE, W_KEY)
	GOTO FIN_LOOP
EOF_FIN,
	CLOSE CHNWRK
	CALL WRTMEM		;INTERNAL ROUTINE TO WRITE WORKSHEETS
	CALL GETDUC		;PROCESS DUCTWORK
	CALL ENDORD

	display(14,eject)	;12-30-02
	display(14,reset)	;01-02-03
	CLOSE 14
	LPQUE (SPLFIL, copies:1, delete)

ENDOFF,
	CLOSE CHN045
	CLOSE CHNWRK
	CLOSE CHNFAX
	CLOSE CHNDUC
	CLOSE CHN139
	close chn171
	close chn170

	XCALL DELET(DUCFIL)
	RETURN

;================================================

NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVDPT = WDEPT
	RETURN
;------------------------------------------

NEWSQ1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVSQ1 = WSEQ1
	SAVSS = WSS

	IF (WSEQ1 .LE. 0) RETURN
	CLEAR LINE2
	WSS2 = SAVSS

	WSEQ12 = SAVSQ1
	CLEAR WSEQ22, WSEQ32
	WITEM2 = '   F1'
	WTYPE2 = 'M'
	WDESC2(1,90) = F1_MEMOL(WSEQ12)
	WDEPT2 = SAVDPT
	W_KEY = W_KEYA+W_KEYB
	STORE (CHNOUT, LINE2, W_KEY)

	clear savcom		;4-24-07 ssq
	RETURN
;------------------------------------------
NEWCOM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; COMBINATION OF SEQ2 & SEQ3
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVCOM = COMSEQ
	SAVSS = WSS

	CLEAR TM2, TM3
	IF (WSEQ2 .NE. 0) TM2 = F2_MEMOL(WSEQ2)
	IF (WSEQ3 .NE. 0) TM3 = F3_MEMOL(WSEQ3)


	FOR J FROM 1 THRU 3
		BEGIN
		CLEAR LINE

		WSS = SAVSS
		WSEQ1 = SAVSQ1
		COMSEQ = SAVCOM
		WITEM = 
		WTYPE = 'M'
		WDEPT = SAVDPT
		WDESC(1,30) = TM2A(J)
		WDESC(34,63) = TM3A(J)
		IF (WDESC .EQ. BLANK90) NEXTLOOP
		IF (TM2A(J).NE.BLANKS) WITEM(4,5) = 'F2'
		IF (TM3A(J).NE.BLANKS) 
			BEGIN
			WITEM(4,5) = 'F3'
			WDESC(32,32) = '*'
			END
		IF (TM2A(J).NE.BLANKS .AND. TM3A(J).NE.BLANKS) 
			BEGIN
		;;;	WITEM(4,8) = 'F2&F3'	;screws up print seq.
			WITEM(4,8) = 'F2'
			END
		WITEM (10,10) = J,'X'
		W_KEY = W_KEYA+W_KEYB
		STORE (CHNOUT, LINE, W_KEY)
		END
		
	RETURN
;------------------------------------------
;------------------------------------------


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
		END

	FIND (CHN045, ORDLIN, ORDNO) [ERR=MLOOP]
MLOOP,
	READS (CHN045, ORDLIN, EOF_M)
	IF (LINSEQ .NE. 0) GOTO EOF_M
	IF (LMSQ1 .GT. 0) 
		BEGIN
		F1_MEMOS(LMSQ1) = M_SHORTD
		F1_MEMOL(LMSQ1) = M_LDESCR
		END
	IF (LMSQ2 .GT. 0) 
		BEGIN
		F2_MEMOS(LMSQ2) = M_SHORTD
		F2_MEMOL(LMSQ2) = M_LDESCR
		END
	IF (LMSQ3 .GT. 0) 
		BEGIN
		F3_MEMOS(LMSQ3) = M_SHORTD
		F3_MEMOL(LMSQ3) = M_LDESCR
		END
	GOTO MLOOP
EOF_M,
	RETURN
;-------------------------------------------------

LOAD_WORK,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Move non-memo line items
		;;; into work file
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FIND (CHN045, ORDLIN, ORDNO) [ERR=LOOP]
LOOP,
	READS (CHN045, ORDLIN, EOF)
	IF (LORDNO .NE. ORDNO) GOTO EOF
	IF (LTYPE .EQ. 'M') GOTO LOOP
	IF (LQTYOR .EQ. 0) GOTO LOOP	;SSQ 3/24/00

	WTYPE = 'L'
	WDEPT = LPRDCD(1,1)
	A2 = LMSQ1,	'XX'
	WSEQ1 = A2

	A2 = LMSQ2,	'XX'
	WSEQ2 = A2

	A2 = LMSQ3,	'XX'
	WSEQ3 = A2

	WSQNO = LSRTSQ

	WSS = 0
	A5 = LF3,	'XXXXX'
	IF (%INSTR(1, A5, '1') ) WSS = 1	;CONTAINS SS
	FOR I FROM 1 THRU 5 IF (A5(I,I) .GT. '1') WSS = 0

	WITEM = LITMNO
	using witem select
	('JEB','JEF','JJG','JTG'), WITEM = LITMNO(2,15)	;SKIP THE "J"
	endusing

	WDESC = LDESCR
	IF (LDAMPR) WITEM(10,15) = 'Damper'
	WQTY = LQTYOR
	WUM = LUOFM
	WPRICE = LPRICE

	WCPFLG = LCPFLG
	W_KEY = W_KEYA + W_KEYB
	STORE (CHNWRK, LINE, W_KEY)
	GOTO LOOP

EOF,
	RETURN
;----------------------------------------------

CONSOLIDATE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Consolidate line items for the
		;;; same part #
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_CON]
	SAVKEY = '***'
CONLOOP,
	READS(CHNWRK, LINE, EOF_CON)
	W_KEY = W_KEYA+W_KEYB
	IF (W_KEY .NE. SAVKEY)
;;;5-26-00	IF (W_KEY.NE.SAVKEY .OR. WCPFLG)
	THEN	CALL NEWKEY
	ELSE	BEGIN
		INCR MULTLINE
		SAVQTY = SAVQTY + WQTY
		DELETE (CHNWRK)
		END
	GOTO CONLOOP

NEWKEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (SAVKEY .EQ. '***') GOTO OUTKEY
	IF (MULTLINE .EQ. 0) GOTO OUTKEY

	READ (CHNWRK, LINE, SAVKEY)
	W_KEY = W_KEYA+W_KEYB		;ssq 6-19-03
	WQTY = SAVQTY
	WRITE (CHNWRK, LINE, SAVKEY)
OUTKEY,
	SAVKEY = W_KEY
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

	XCALL DDEPT (GAUGE,SIZE3,DUTYPE,WD_DEPT,STY)
;;;	USING STY SELECT
;;;	(1,4,5),	WD_DEPT='P'		;TDC
;;;	(2,3,6),	WD_DEPT='O'		;S&D
;;;	ENDUSING

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
	WDSEQ = DSEQ	
	WDPC = DPC		;1-27-09

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
	CLOSE CHNOUT
	OPEN (CHNOUT, SI, OUTFIL)
	XCALL FILL ('_', UNDER)
	SAVDPT = -1
	savss = 0
	FIRST_PAGE = 1

W_LOOP,
	READS (CHNOUT, LINE, W_EOF)
	IF (WTYPE.EQ.'L' .AND. WQTY.EQ.0) GOTO W_LOOP		;SKIP OF QTY=0
	IF (WDEPT .NE. SAVDPT) CALL W_NEWDPT
	IF (WSS .NE. SAVSS)
		BEGIN
		XCALL HP(14,hpFONT,hpBOLD)
		clear pline
		call print
	;;;	PLINE(25,70) = '   Safety Seal is a self sealing'
	;;;	PLINE(25,70) = '     EZ Seal is a self sealing'
		PLINE(25,70) = '  Complete Seal is a self sealing'
		call print
		pline(25,70) = '    spiral pipe connection that'
		call print
		pline(25,70) = '    meets SMACNA Leakage Class 3'
		call print
		savss = wss
		XCALL HP(14,hpFONT,hpMEDIUM)
		savss = wss
		END


	IF (WTYPE .EQ. 'M')
	THEN	BEGIN
		XCALL HP(14,hpFONT,hpBOLD)
		IF(M_ON.EQ.0) 
			BEGIN
			XCALL GETRFA(CHNOUT, M_RFA)	;SAVE THE RFA OF 1ST MEMO
			M_ON = 1
			END
		IF (MITEM.EQ.'   F1') 
		THEN	FOR J FROM 1 THRU 3
			BEGIN
			TMPDSC = MMEMO(J)
			CALL MAIN_MEMO
			PLINE (8,70) = TMPDSC
			IF (MMEMO(J) .NE. BLANKS) CALL MEMO_PRINT
			END
		ELSE	BEGIN
			PLINE(8,70) = M_LONG
			CALL MEMO_PRINT
			GOTO W_LOOP
			END

		END
	ELSE	BEGIN
		;bold all lines assoc w/ memos
	;;;	IF (MSEQ.EQ.0 .AND. MFSEQ.EQ.0)XCALL HP(14,hpFONT,hpMEDIUM)
	;;;	IF (MSEQ.EQ.0)XCALL HP(14,hpFONT,hpMEDIUM)
		XCALL HP(14,hpFONT,hpMEDIUM)
		CLEAR M_ON
		CALL PRTLIN
		END

	GOTO W_LOOP
;====================================================

PRTLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Print out line item detail
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF ((LINCNT+2).GT.MAXLIN) 
		BEGIN
		CALL CONTIN
		IF (MSEQ.NE.0) CALL H_MEMO	;PRINT ANY ACTIVE MEMOS
		END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR PLINE			;SSQ 9-23-02
	IF (WCPFLG) RLINE(67,68) = '**'	;SSQ 9-23-02
;;;	IF (WCPFLG) 
;;;	THEN	BEGIN
;;;		PLINE (67,68) = '**'	
;;;		END
;;;	ELSE	BEGIN
;;;		CLEAR PLINE
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	PLINE (9,23) = WITEM
	IF ( WITEM.EQ.'???' ) 	PLINE (19,20) = WITEM(4,5)

	PLINE (25,54) = WDESC
;;;	IF (WITEM.NE.'M')	;any item starting w/ "M" gets ignored!!!
	IF (WITEM.NE.'M   ')
	BEGIN
	  PLINE ( 1,7 ) = WQTY,NUMASK
	  RLINE (55,63) = WPRICE,'ZZZZZZ.XXX-'
	  RLINE (65,66) = WUM
	  DECMAL = ( WQTY * WPRICE ) # 1 
	  RLINE (68,79) = DECMAL,'ZZZZ,ZZZ.XX-'
	  TOTPRC = TOTPRC + DECMAL		;INVENTORY ITEMS
	END
	IF (NO_DOLLARS)
		BEGIN
		RLINE(55,63) = 
		RLINE(68,79) =
		END

	CALL PRINT
	RETURN

;====================================================
H_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; NEW PAGE; PRINT ANY ACTIVE MEMOS...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (M_RFA .EQ. A6A) RETURN
	SAVE_LINE = LINE

	OPEN (39, SI, OUTFIL)
	XCALL POSRFA(39, M_RFA)		;GET TO LAST MEMO
HM_LOOP,
	READS (39, LINE, HM_EOF)
	IF (WTYPE .EQ. 'L') GOTO HM_EOF
	XCALL HP(14,hpFONT,hpBOLD)
	IF (MITEM.EQ.'   F1') 
	THEN	FOR J FROM 1 THRU 3
		BEGIN
		TMPDSC = MMEMO(J)
		CALL MAIN_MEMO
		PLINE (8,70) = TMPDSC
		IF (MMEMO(J) .NE. BLANKS) CALL MEMO_PRINT
		END
	ELSE	BEGIN
		PLINE(8,70) = M_LONG
		CALL MEMO_PRINT
		GOTO HM_LOOP
		END

	GOTO HM_LOOP

HM_EOF,
	CLOSE 39
	XCALL HP(14,hpFONT,hpMEDIUM)
	LINE = SAVE_LINE
	RETURN
;------------------------------------------------


MAIN_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;
	TL = %TRIM(TMPDSC)
	REM = 50 - TL
	REM = REM/2
	IF (REM.GT.0)
		BEGIN
		CLEAR TMPDSC
		TMPDSC(1,REM) = DASHES
		TMPDSC(REM+1,REM+TL) = MMEMO(J)
		TMPDSC(REM+1+TL,50) = DASHES
		END
	RETURN
;-----------------------------------------
W_EOF,
	CLOSE CHNOUT
	CLOSE CHNWRK
	XCALL DELET (WRKFIL)
	XCALL DELET (OUTFIL)
	RETURN
;===============================================

W_NEWDPT,
	SAVDPT = WDEPT
	RETURN
;------------------------------------

GETDUC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PROCESS DUCTWORK
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	CLEAR PLINE
;;;	CALL PRINT
;;;	WDEPT=SAVDPT
	WDEPT = 'O'
	READ(CHNDUC,DUCWRK,^FIRST) [ERR=GETDUC_EOF]
	GOTO CONDUC
NXTDUC,
	READS(CHNDUC,DUCWRK,GETDUC_EOF) [ERR=GETDUC_EOF]
CONDUC,
	WSIZE3 = 100-WSIZE3
	WSIZE2 = 1000000-WSIZE2
	WSIZE1 = 1000000-WSIZE1
	WSTY=10-WSTY
	SZ1 = %TRN3(WSIZE1)
	SZ2 = %TRN3(WSIZE2)
	CALL PRTDUC
SUMDUC,
	CONFIG(1,3) = WGAUGE
;;;	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3) CONFIG(1,3) = WTHICK
	IF (WDUTYPE.EQ.2) CONFIG(1,3) = WTHICK			;should have been changed for ss to gauge mod
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

	IF (WLINER.GE.1.AND.WLINER.LE.9.AND.WLINER.NE.4)
&		LINSQF(WLINER) = LINSQF(WLINER) + WSQFLIN


	GOTO NXTDUC

GETDUC_EOF,
	CALL PRTSDU
	RETURN
;------------------------------------------

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

;;;	IF ((LINCNT+3).GT.MAXLIN) CALL CONTIN
	IF ((LINCNT+4).GT.MAXLIN) CALL CONTIN	;1-27-09
	INCR LINCNT		;SSQ 12-4-96

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	IF (WDPC .NE. 999)
;;;	THEN	BEGIN
;;;		PC_PC = WDPC,	'ZX.X-' 
;;;		PLINE (25,50) = PC_LINE
;;;		END
;;;	ELSE	PLINE (25,50) = 'DESIGN BUILD'
;;;	CALL PRINT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	PLINE (1,7) = WJOINT,NUMASK
	PLINE (8,9) = 'JT'
	IF (WDUTYPE.NE.2.AND.WDUTYPE.NE.3)
	BEGIN
	  PLINE (13,14) = WGAUGE,'XX'
	  PLINE (15,16) = 'GA'
	END
;;;	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3)
	IF (WDUTYPE.EQ.2)			;2-18-19
	BEGIN
	  PLINE (13,16) = WTHICK,'.XXX'
	END
	PLINE (18,20) = SZ1,'ZZX'
	PLINE (21,21) = 'X'
	PLINE (22,24) = SZ2,'ZZX'
	PLINE (25,25) = 'X'
	PLINE (26,27) = WSIZE3,'ZX'
	IF(WSIZE1/1000.NE.SZ1 .OR. WSIZE2/1000.NE.SZ2) PLINE(28,28)='*'
	PLINE (31,34) = DTYPE(WDUTYPE)
	PLINE (36,41) = DCAT(WCAT)
	PLINE (43,45) = DSTY(WSTY)
	PLINE (48,50) = DSEAM(WSEAM)
	PLINE (52,66) = WLOCAT
;;;	IF (WSEAL.EQ.1) PLINE (52,55) = 'SEAL'
	RLINE (56,63) = WGPRICE,'ZZZ.XXX-'
	DECMAL = ( WPOUNDS * WGPRICE ) # 1
	RLINE (65,66) = 'LB'
	RLINE (68,79) = DECMAL,'ZZZZ,ZZZ.XX-'
	TOTPRC = TOTPRC + DECMAL		;DUCT

	IF (NO_DOLLARS)
		BEGIN
		RLINE(56,63) = 
		RLINE(68,79) =
		END
	CALL PRINT

	PLINE (  1,7  ) = WPOUNDS,NUMASK
	PLINE (  8,8  ) = '#'
	PLINE ( 12,17 ) = WSQFLIN,NUMASK
	PLINE ( 19,23 ) = 'SQ FT'
	PLINE (30,44) = '     NO LINER '
	IF (WLINER.GE.1.AND.WLINER.LE.9.AND.WLINER.NE.4)
	BEGIN
	  PLINE (31,37) = DLINER( WLINER )
	  PLINE (38,42) = 'LINER'
	END
;;;	PLINE (43,54) = WLOCAT
	IF (WSEAL.EQ.1) PLINE (44,47) = 'SEAL'
	if (wdpunch .eq. '0') clear wdpunch	;ssq 12-13-05
	PLINE (49,63) = WDPUNCH
	RLINE (56,63) = WLINPRC,'ZZZ.XXX-'
	RLINE (65,66) = 'SF'
	DECMAL = ( WLINPRC * WSQFLIN ) # 1
	RLINE (68,79) = DECMAL,'ZZZZ,ZZZ.XX-'
	TOTPRC = TOTPRC + DECMAL		;LINER

	IF (NO_DOLLARS)
		BEGIN
		RLINE(56,63) = 
		RLINE(68,79) =
		END
	CALL PRINT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	IF (ACC.EQ.1)
;;;&	DECMAL = ( ( SLIPS  * SLPPRC ) # 1 ) +
;;;&		 ( ( DRIVES * DRVPRC ) # 1 ) +
;;;&		 ( ( TCORN  * CORPRC ) # 1 ) +
;;;&		 ( ( TNBQTY * TNBPRC ) # 1 ) +
;;;&		 ( ( TGAQTY * TGAPRC ) # 1 ) +
;;;&		 ( ( TCLQTY * TCLPRC ) # 1 ) +
;;;&		 ( ( TBAQTY * TBAPRC ) # 1 ) +
;;;&		 ( ( TBNQTY * TBNPRC ) # 1 )
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;PRTSAC,
;;;		; Sort then Print summarized accessories from this order
;;;;
;;;;	BUBBLE SORT ACCREC ARRAY
;;;;
;;;	FOR I FROM 1 THRU MAXACC
;;;	  BEGIN
;;;	  ACCREC = SACCRC(I)
;;;	  IF (ACCQTY.EQ.0) GOTO PRTSA2
;;;	  COMPA = ACCITM
;;;	  ACCREC = SACCRC(I+1)
;;;	  IF (ACCQTY.EQ.0) GOTO PRTSA2
;;;	  COMPB = ACCITM
;;;	  IF (COMPA.GT.COMPB)
;;;	  BEGIN
;;;	    SACCRC(I+1) = SACCRC(I)
;;;	    SACCRC(I) = ACCREC
;;;	    I = I - 2
;;;	    IF (I.LT.0) I = 0
;;;	  END
;;;	END
;;;	GOTO BADACC
;;;PRTSA2,
;;;	FOR I FROM 1 THRU MAXACC
;;;	  BEGIN
;;;	  ACCREC = SACCRC(I)
;;;	  IF (ACCITM.EQ.BLANKS) RETURN
;;;	  IF ((LINCNT+1).GT.MAXLIN) CALL CONTIN
;;;	  PLINE (1,7) = ACCQTY,NUMASK
;;;	  PLINE (9,23) = ACCITM
;;;	  PLINE (25,54) = ACCDES
;;;	  ACCUOM = 'EA'
;;;	  IF (ACCITM(4,6).EQ.'GS ') ACCUOM = 'FT'
;;;	  IF (ACCITM(4,6).EQ.'BAR') 
;;;	  BEGIN
;;;	    ACCUOM = 'IN'
;;;	    DECMAL = ACCITM(10,12)
;;;	    ACCQTY = ACCQTY * DECMAL
;;;	  END
;;;	  PLINE (65,66) = ACCUOM
;;;	  PLINE (55,63) = ACCPRC,'ZZZZZ.XXX-'
;;;	  DECMAL = (ACCQTY * ACCPRC) #1
;;;	  PLINE (68,79) = DECMAL,'ZZZZ,ZZZ.XX-'
;;;	  TOTPRC = TOTPRC + DECMAL		;ACCESSORIES
;;;
;;;	IF (NO_DOLLARS)
;;;		BEGIN
;;;		PLINE(55,63) = 
;;;		PLINE(68,79) =
;;;		END
;;;
;;;	  CALL PRINT
;;;	  SACCRC(I) =
;;;	END
;;;	IF (I.GE.MAXACC) GOTO BADACC
;;;	RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRTSDU,
		;Print summarized duct from this order

	TOTPND =		;3-5-19

	IF ((LINCNT+1).GT.MAXLIN) CALL CONTIN
	INCR LINCNT
	FOR I FROM 1 THRU MAXDUC
	  BEGIN
	  DUCREC = SDUCRC(I)
	  IF (DUCCFG.EQ.0) GOTO PRTSD2
	TOTPND = TOTPND + DUCPND			;3-5-19
	  IF ((LINCNT+1).GT.MAXLIN) CALL CONTIN
	  PLINE (1,7) = DUCPND,NUMASK
	  PLINE (8,8) = '#'
	  PLINE (18,19) = DUCCFG(1,3),'XX'
	  PLINE (21,22) = 'GA'
;;;	  IF (DUCCFG(4,4).EQ.2.OR.DUCCFG(4,4).EQ.3)
	  IF (DUCCFG(4,4).EQ.2)				;2-19-18
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
	FOR I FROM 1 THRU 9
	  BEGIN
	  IF (LINSQF(I).NE.0.AND.I.NE.4)
	  BEGIN
	    IF ((LINCNT+1).GT.MAXLIN) CALL CONTIN
	    PLINE (1,7) = LINSQF(I),NUMASK
	    PLINE (8,10) = 'SQF'
	    PLINE (33,39) = DLINER(I)
	    PLINE (40,45) = ' LINER'
	    CALL PRINT
	    LINSQF(I) =
	  END
	END

	IF (TOTPND .GT. 0)
		BEGIN
		PLINE (1,7) = TOTPND,MASK1
		PLINE (8,17) = 'TOTAL LBS.'
		CALL PRINT
		END

	RETURN	

MEMO_PRINT,
; don't print memo on page by itself...
	IF (LINCNT+5 .GT. MAXLIN) 
		BEGIN
		XCALL HP(14,hpFONT,hpMEDIUM)
		SAVLIN = PLINE
		CLEAR PLINE
		CALL CONTIN	
		PLINE = SAVLIN
		XCALL HP(14,hpFONT,hpBOLD)
		END

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	WRITES (14,PLINE)
	XCALL HP (14,hpPOS,Lrow+LINCNT,1,0,PLINE)
	PLINE =
	INCR LINCNT
	RETURN
;-----------------------------------------------------

ENDORD,

	A_FRT = OFRGHT
	USING A_FRT SELECT
	('    -0'),	X_FRT = NEG_ZERO
	('     0'),	X_FRT = ZERO
	(),		X_FRT = 9
	ENDUSING

	LFEED = MAXLIN - LINCNT -1
	IF (LFEED) LINCNT = MAXLIN

	Lrow = Lrow + LINCNT
;-
;added 5-16-06 for new message...
	XCALL HP(14,hpFONT,hpBOLD)
	Lrow = Lrow - 1
	
;9-18-19: fine print...
	XCALL HP (14,hpFONT,hp167CPI+hpBOLD)
	xcall HP (14, hpPOS,Lrow, 1, 0, terms0)
	xcall HP (14, hpPOS,Lrow+1, 1, 0, terms1)
	if (cmpcod .eq. 'CAT')
	then	xcall HP (14, hpPOS,Lrow+2, 1, 0, terms2c)
	else	xcall HP (14, hpPOS,Lrow+2, 1, 0, terms2)

	if (cmpcod .eq. 'CAT')
	then	xcall HP (14, hpPOS,Lrow+3, 1, 0, terms3c)
	else	xcall HP (14, hpPOS,Lrow+3, 1, 0, terms3)

	xcall HP (14, hpPOS,Lrow+4, 1, 0, terms4)
	XCALL HP (14,hpFONT,hp12CPI)

;--------------------------------------------	
	
;;;	pline(1,55) = 'Prices shown will be honored for 30 days '
;;;	XCALL HP (14,hpPOS,Lrow,1,0,PLINE)
	Lrow = Lrow + 1

	XCALL HP(14,hpFONT,hpBOLD)
	PLINE (63,77) = '   NET AMOUNT:'
	PLINE (80,91) = TOTPRC,'ZZZZ,ZZZ.ZZ-'
	xcall HP (14, hpPOS, Lrow, 63, 0, pline(63,91) )

;;;	pline(1,55) = 'from date of Quotation.  Call Factory for current '

;;;	XCALL HP (14,hpPOS,Lrow,1,0,PLINE)
	INCR Lrow
	
	CLEAR PLINE
	USING X_FRT SELECT
	(NEG_ZERO),	BEGIN
			PLINE(63,77) = 'ESTIMATED FRT: '
			PLINE(88,90) = 	'N/C'
			END
	(9),		BEGIN
			PLINE(63,77) = 'ESTIMATED FRT: '
			PLINE(80,91) = OFRGHT,	'ZZ,ZZZ.XX-'
			END
	ENDUSING

;;;	pline(1,55) = 'pricing after 30 days.'
	XCALL HP (14,hpPOS,Lrow,63,0,PLINE(63,91) )
	INCR Lrow
	
	CLEAR PLINE
	IF (X_FRT .NE. ZERO)
		BEGIN
	;;;	PLINE (49,65) = 'ESTIMATED TOTAL: '
	;;;	PLINE (68,79) = (TOTPRC+OFRGHT),'ZZZZ,ZZZ.ZZ-'
		PLINE (61,77) = 'ESTIMATED TOTAL: '
		PLINE (80,91) = (TOTPRC+OFRGHT),'ZZZZ,ZZZ.ZZ-'
		END
;;;	XCALL HP (14,hpPOS,Lrow,22,0,PLINE(10,79))
	XCALL HP (14,hpPOS,Lrow,49,0,PLINE(49,91))

;;;	IF (CUSCD.NE.'99' .AND. VALID_AC)
	IF (CUSCD.NE.'99' .AND. VALID_AC .AND. CMPCOD.NE.'CAT')		;1-29-20
		BEGIN
		PLINE = 
& 'Looking for Quick and Accurate Bidding?  Please Visit WWW.PRICEDUCT.COM'
		INCR LROW
		XCALL HP (14,hpPOS,Lrow,12,0,PLINE)
		END

	XCALL HP(14,hpFONT,hpMEDIUM)
	INCR Lrow

ENDUP,

;;; write FAXFIL record...
	CLEAR FAXFIL
	FAX_ORDER = OORDNO
	XDATE(1,2) = TODAY(5,6)
	XDATE(3,6) = TODAY(1,4)
	FAX_DATE = XDATE		;01-04-16
;;;	FAX_DATE = TODAY

	FAX_SLSMAN = OSLMAN
	FAX_STATUS = 0		;OPEN
	FAX_OTYPE = 'E'		;12-17-01
	STORE (CHNFAX,FAXFIL,FAX_ORDER) [ERR=BAD_FAX]
BAD_FAX,
	
	RETURN
;--------------------------------------------------------------
;--------------------------------------------------------------

CONTIN,
	LFEED = MAXLIN - LINCNT
	IF (LFEED) LINCNT = MAXLIN

	CALL PRINT
	PLINE (64,78) = '** CONTINUED **'
	CALL PRINT
	XCALL HP (14,hpFLUSH)

	INCR PAGE

PRTHDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	Lrow = 1
	XCALL HP (14,hpFONT,hp8CPI+hpBOLD)
	XCALL HP (14,hpPOS,Lrow,22,0,"Q U O T A T I O N")
	XCALL HP (14,hpFONT,hp12CPI)
	INCR Lrow

;;;	XCALL HP (14,hpPOS,Lrow,1,0,"SHEET METAL CONNECTORS, INC.")
	XCALL HP (14,hpPOS,Lrow,1,0,C_NAME)
	XCALL HP (14,hpPOS,Lrow,68,0,"Quotation No.")
	XCALL HP (14,hpFONT,hp10CPI+hpBOLD)
	XCALL HP (14,hpPOS,Lrow,70,0,OORDNO)
	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	INCR Lrow

	XCALL HP (14,hpFONT,hpMEDIUM)

	XCALL HP (14,hpPOS,Lrow,67,0,"DATE")

	XCALL DATE8(OORDDT, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE (72,81) = D_FMT
	XCALL HP (14,hpPOS,Lrow,73,0,PLINE(72,81))
	XCALL HP (14,hpPOS,Lrow,84,0,"Page:")
	PLINE (92,93) = PAGE,'XX'
	XCALL HP (14,hpPOS,Lrow,90,0,PLINE(92,93))
	CLEAR PLINE
	IF (PAGE .GT. 1)
	THEN	BEGIN
	;;;	MAXLIN = 51
		MAXLIN = 50
		INCR Lrow
		GOTO DONE_HDR
		END
	ELSE	XCALL HP (14,hpPOS,Lrow,1,0,C_ADD1)
;;;	ELSE	XCALL HP (14,hpPOS,Lrow,1,0,"5850 MAIN STREET N. E.")

	INCR Lrow

	XCALL HP (14,hpPOS,Lrow,1,0,C_ADD2)
;;;	XCALL HP (14,hpPOS,Lrow,1,0,"MINNEAPOLIS,   MN 55432")
	INCR Lrow

	XCALL HP(14,hpFONT,hpBOLD)

	PLINE = %string(C_LD,'ZZZ-ZZX-XXXX') + '  ' + %string(C_LOC,'ZZZ-ZZX-XXXX')
	XCALL HP(14,hpPOS,Lrow,1,0,PLINE(1,%TRIM(PLINE)))
;;;	XCALL HP(14,hpPOS,Lrow,1,0,"800-328-1966  763-572-0000")
	clear pline
	PLINE(1,5) = DT_DATE(5,8),	'ZX/XX'
	PLINE(6,6) = '/'
	PLINE(7,10) = DT_DATE(1,4),	'XXXX'
	PLINE(12,16) = DT_TIME,	'XX:XX'
	XCALL HP(14,hpPOS,Lrow,70,0,PLINE(1,16))

	INCR Lrow

	XCALL HP(14,hpFONT,hpMEDIUM)

	PLINE = 'WEB SITE: ' + C_WEB
	XCALL HP(14,hpPOS,Lrow,1,0,PLINE(1,%TRIM(PLINE)))
;;;	XCALL HP(14,hpPOS,Lrow,1,0,'WEB SITE: WWW.SMCDUCT.COM')
	INCR Lrow

	XCALL HP(14,hpFONT,hpBOLD)

	PLINE = 'FAX:' + %STRING(C_FAX,'ZZZ-ZZX-XXXX')
	XCALL HP(14,hpPOS,Lrow,1,0,PLINE(1,%TRIM(PLINE)))
;;;	XCALL HP(14,hpPOS,Lrow,1,0,"FAX:763-572-1100")
	XCALL HP(14,hpFONT,hpMEDIUM)
	INCR Lrow

	CLEAR PLINE

;removed per Dianna 7-2-99 ...
;back in per steve 1-9-2012
	PLINE (23,28) = CRDLMT,'ZZZZZZ'
	XCALL HP(14,hpPOS,Lrow,23,0,PLINE(23,28))
;removed per Dianna 7-2-99 ...

	XCALL HP(14,hpPOS,Lrow,55,0,"SHIP TO:")
	INCR Lrow

	XCALL HP(14,hpPOS,Lrow,17,0,OCUSNM,hpPOS,Lrow,55,0,OSHPNM)
	INCR Lrow
	
	XCALL HP(14,hpPOS,Lrow,17,0,ADD1,hpPOS,Lrow,55,0,OSHAD1)
	INCR Lrow

	XCALL HP(14,hpPOS,Lrow,17,0,ADD2,hpPOS,Lrow,55,0,OSHAD2)
	INCR Lrow

	CLEAR PLINE
	PLINE (8,22) = CITY
	PLINE (25,26) = STATE
	PLINE (29,33) = ZIP
	PLINE (46,75) = OSHAD3
	XCALL HP(14,hpPOS,Lrow,10,0,PLINE)

;per Steve M...
	INCR Lrow
	clear pline
	PLINE (8,12) = 'ATTN:'
	PLINE (14,23) = OPONO
	XCALL HP (14,hpFONT,hpBOLD)
	XCALL HP (14,hpPOS,Lrow,10,0,PLINE)
	XCALL HP (14,hpFONT,hpMEDIUM)


;per Leon 1/25/05
	incr Lrow
	clear pline
	pline(8,20) = phone, 'ZZZ XXX-XXXX'

	IF (SHPHON .GT. 0)	PLINE(46,58) = SHPHON,	'ZZZ XXX-XXXX'
	if (t_phone .gt. 0)	pline(46,58) = t_phone, 'ZZZ XXX-XXXX'

	XCALL HP (14,hpFONT,hpBOLD)
	XCALL HP (14,hpPOS,Lrow,10,0,PLINE)
	XCALL HP (14,hpFONT,hpMEDIUM)

	Lrow = Lrow + 2

	PLINE = 
&	'Cust #     Sales-Rep         Job #           Freight FOB                Fab Time     Terms'

	XCALL HP (14,hpPOS,Lrow,1,0,PLINE)
	INCR Lrow

	CLEAR PLINE
	PLINE (1,6) = OCUSNO,NUMASK
	XCALL HP (14,hpPOS,Lrow,1,0,PLINE(1,6))
	CLEAR PLINE

	XCALL HP (14,hpFONT,hpBOLD)
	CALL GET_SALESMAN
;;;	XCALL HP (14,hpPOS,Lrow,12,0,SLSNM)
	XCALL HP (14,hpPOS,Lrow,12,0,SNAM)
	XCALL HP (14,hpFONT,hpMEDIUM)


	PLINE (33,42) = OJOBNO
;;;	IF (OCLPPD.EQ.'C') PLINE (49,56) = 'MPLS. - '
	IF (OCLPPD.EQ.'C') PLINE (49,56) = FOB		;SSQ 1-29-04
	IF (OCLPPD.EQ.'P') PLINE (49,56) = 'DEST. - '
	CALL GET_SCAC
	PLINE (57,71) = SC_NAME

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	PLINE (89,95) = 'NET 30'
	SRCCTL = 2
	BSEND = ORGART
	XCALL SERCH (CHN170,ARTERM,OTERMS,1,1,BSEND,BSMID,SRCCTL,4,2,7,0,0,0,0)
	IF (SRCCTL.EQ.1) ARTRDS = '*NF*'
	PLINE (89,95) = ARTRDS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL HP (14,hpPOS,Lrow,24,0,PLINE(27,95))

	XCALL HP(14,hpFONT,hpBOLD)
	PLINE (77,79) = OPROMD,NUMASK
	PLINE (80,83) = 'DAYS'
	XCALL HP (14,hpPOS,Lrow,74,0,PLINE(77,83))
	XCALL HP(14,hpFONT,hpMEDIUM)

	Lrow = Lrow + 3

	XCALL HP (14,hpFONT,hp10CPI+hpBOLD)
	PLINE (3,53) = 'We are pleased to quote on your inquiry as follows:'
	XCALL HP (14,hpPOS,Lrow,1,0,PLINE(3,53))

DONE_HDR,
	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
;;;	XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)
	Lrow = Lrow + 2

	PLINE = 
&	"   Qty  Item No.        Description                                  Price  UM  Ext. Price"
;;;&	"   Qty  Item No.        Description                      Price  UM  Ext. Price"
	XCALL HP(14,hpPOS,Lrow,1,0,PLINE)
	CLEAR PLINE
	Lrow = Lrow + 2

	LINCNT =
	RETURN

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
;;;	XCALL IO (CHN054,SALMAN,OSLMAN,READ,LOKCTL)
;;;	IF (SLSNM.EQ.']]]]]]' .OR. SLSNM.EQ.']]]DEL' .OR. LOKCTL.NE.0) SLSNM =
	RETURN
;-----------------------------------------------------

TIME_STAMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; TIME & DATE STAMP			;SSQ 10-26-06
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	DT = %DATETIME
	DT_DATE = DT(1,8)
	DT_TIME = DT(9,12)

	RETURN
;----------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

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
		XCALL FILES (36,'SI',001,SWITCH)
		END
	CHN001 = 36

;;;	XCALL CHNOPN (37,STAT)
;;;	IF (STAT .EQ. 0)
;;;		BEGIN
;;;		XCALL FILES (37,'I',002,SWITCH)
;;;		END
;;;	CHN002 = 37


	XCALL CHNOPN(38,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (38,'I',054,SWITCH)
		END
	CHN054 = 38

	OPEN (40,SU,'SPL:FAXFIL')
	CHNFAX = 40
	CHN182 = 17	;COP TABLES


	XCALL OECO(CHN182,COINFO)	;GET COMPANY INFO 11/13/03 SSQ
	USING CODE SELECT
	('ROC'),	FOB_N = 'ROCK.'
	(),		FOB_N = 'MPLS.'
	ENDUSING

	XCALL ISIO (CHN044, ORDHDR, ORDNO, READ, LOKCTL)
	WFORD = ORDNO,'XXXXXX'
	XCALL DATE8(OORDDT, D_OUT, D_OUTR, E_DATE, D_SW)
	XCALL DATE8(OPROMD, D_OUT, D_OUTR, S_DATE, D_SW)

	CALL GET_SCAC
	CALL GET_SALESMAN

	XCALL ISIO (CHN001, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CUSMAS =
		NAME = '*CUSTOMER NOT ON FILE *'
		END
		
;;;	XCALL IO (CHN001, DUMCUS, 1, READ, LOKCTL)

;;;	KEY = OCUSNO,'XXXXXX'
;;;	XCALL SERCH (CHN002,CUSIDX,KEY,1,6,ORG001,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	LOKCTL = 1
;;;	CASE SRCCTL OF 
;;;	BEGINCASE
;;;	  0:	XCALL IO (CHN001,CUSMAS,IRC001,READ,LOKCTL)
;;;	  1:	BEGIN
;;;		CUSMAS =
;;;		NAME = '* CUSTOMER NOT ON FILE *'
;;;		END
;;;	ENDCASE

	CSZ(1,15) = CITY
	CSZ(17,18) = STATE
	CSZ(20,29) = ZIP
	AREAC = PHONE(1,3)	;SSQ 4-28-05

	USING AREAC SELECT	;SSQ 4-28-05
	(612,651,952,763),	VALID_AC = 1
	(),			VALID_AC = 0
	ENDUSING

;Create work file...
;;;	XCALL ISAMC (WRKFIL, 132, 1, 'START=1, LENGTH=63, DUPS, ASCEND')
	XCALL ISAMC (WRKFIL, 133, 1, KEYSPEC)
	OPEN (33, SU, WRKFIL)
	CHNWRK = 33

	OFORD = ORDNO,'XXXXXX'
;Create output file...
;;;	XCALL ISAMC (OUTFIL, 132, 1, 'START=1, LENGTH=63, DUPS, ASCEND')
	XCALL ISAMC (OUTFIL, 133, 1, KEYSPEC)
	OPEN (34, SU, OUTFIL)
	CHNOUT = 34

	WRORD = ORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (DUCFIL, 114, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
;;;	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (41, SU, DUCFIL)
	CHNDUC = 41

	SWITCH = 5
	XCALL FILES (42, 'SI', 139, SWITCH)
	CHN139 = 42

	xcall isio (chn139, tmpcus, oordno, read, lokctl)
	if (lokctl .ne. 0) clear tmpcus
	
	SWITCH = 5
	XCALL FILES (43,'SI',171,SWITCH)
	CHN171 = 43

	SHCSNO = OCUSNO
	SHTONO = OSHPTO
	XCALL ISIO (CHN171, SHIPTO, SHTKEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR SHIPTO


	SPORD = ORDNO,'XXXXXX'
	OPEN (14, O, SPLFIL)
	WRITES(14,HP_V)		;vertical spacing for HP laserJet 2100 series


	switch = 5
	xcall files (8,'I', 170, switch)
	chn170 = 8

	xcall io (chn170, arterm, 1, read, lokctl)
	orgart = org170

	OPNOK = 1

	RETURN
;-------------------------------------------


;------------------------------------------------

READ_LINER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; GET LINER DESCRIPTIONS FROM LINER PRICE TABLE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR COPTBL
	TBLCOD = 'LP'
	FOR I FROM 1 THRU 9
		BEGIN
		LP_KEY = I
		XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
		IF (TBLCOD .NE. 'LP') EXITLOOP
		DLINER(I) = lp_liner
		END

	RETURN
;--------------------------------------------------
