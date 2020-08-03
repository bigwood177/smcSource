;BFORD.CP - FAX
;DDEPT 5-29-14
SUBROUTINE	BFORD
	ORDNO	,D
	B_ERR	,D
	O_CHAN	,A		;OPEN CHANNELS
	C_CODE	,A		;COMPANY CODE 2-17-04 SSQ
;
;
;	MOD: SSQ 11-13-96 Print '*' above u of m if user has 
;			changed price.
;
;	9-19-97 SCAC MOD
;	NT-SERVER VERSION.
;	2-16-00 SSQ:	CONSOLIDATE LINE ITEMS.
;	5-23-18 ssq: make cusmas isam


EXTERNAL FUNCTION
	TRN3	,D

	.include 'DEF:RTF.DEF'

RECORD	COINFO
	.INCLUDE 'DEF:COINFO.DEF'

RECORD	P_EJECT
	PJ_ESC	,A1
		,A1,	'&'
		,A1,	'l'
		,A1,	'0'
		,A1,	'H'
		
	
RECORD	M_BOX
	MB_ESC	,A1
		,A1,	'&'
		,A1,	'l'
	MBOX	,A2
RECORD	M_BOX2
		,A1,	'g'
	TRA	,A1
		,A1,	'h'
		,A1,	'8'
		,A1,	'C'	;vertical spacing
RECORD	TOP_MARGIN
	TM_ESC	,A1
		,A4,	'&l0E'

RECORD	LEFT_MARGIN
	LM_ESC	,A1
		,A4,	'&a0L'
;-

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


RECORD	SALMAN
	.INCLUDE 'DEF:RD054A.DEF'

RECORD	COPCTL
	.INCLUDE 'DEF:RD060A.DEF'

RECORD CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'

RECORD CUSCTL	,X
	.INCLUDE 'DEF:RD001B.DEF'

;;;RECORD CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'

RECORD ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD ORDLIN
	.INCLUDE 'DEF:RD045A.def'
RECORD,X
	.INCLUDE 'DEF:RD045M.def'
RECORD,X
	.INCLUDE 'DEF:RD045D.def'

RECORD	SAVLIN
	.INCLUDE 'DEF:RD045Z.def'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD PARAM
	.INCLUDE 'DEF:PARAM2.DEF'

;;;GLOBAL DUCK	
	.INCLUDE 'DEF:RD175D.DEF'
;;;ENDGLOBAL

RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.DEF'


record	values
	r_bold	,a*,	'\b '
	r_plain	,a*,	'\plain\f3\fs20\cf0 '
	r_font	,a*,	'\fs'

record	t_bold
	besc	,a1
		,a*,	'(s3B'
record	t_plain
	pesc	,a1
		,a*,	'(s0B'

record	t_font
	fesc	,a1
		,a*,	'(s'

RECORD	SPLFIL
		,A*,	'SPL:'
		,A1,	'S'
	SORD	,D6
		,A4,	'.SPL'

RECORD	FAXFIL
		,A*,	'SPL:'
		,A1,	'S'
	SFAX	,D6
		,A4,	'.FAX'


record	buf
		,a5000
;-
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
RECORD	DUCFIL
		,A4,	'SPL:'
		,A1,	'D'
	WRORD	,A6
		,A4,	'.ISM'

	
RECORD	F_MEMOS
	.INCLUDE 'DEF:CPMEMO.DEF'
		
RECORD	FFLLIN			;ORDLIN
	L_DEV	,A3
		,A1,	':'
		,A6,	'ORDLIN'
		,A1,	'.'
	L_EXT	,A3
RECORD	FFLDUC
	D_DEV	,A3
		,A1,	':'
		,A6,	'DUCACC'
		,A1,	'.'
	D_EXT	,A3
RECORD	FFLSLS			;SALMAN
	S_DEV	,A3
		,A1,	':'
		,A6,	'SALMAN'
		,A1,	'.'
	S_EXT	,A3

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

RECORD	LINE			
	WSEQ1	,D2		;not reversed
	WSEQ2	,D2
	WSEQ3	,D2	
	WDEPT	,A2
	WSQNO	,A2
	WITEM	,A15
	WPRICE	,D8
	WDESC	,A90
	WTYPE	,A1		;L=LINE, M=MEMO
	WQTY	,D5
	WUM	,A2
	WCPFLG	,D1
RECORD	MEMO,X
	MSEQ	,D6
	MDEPT	,A2
	MSQNO	,A2
	MITEM	,A15
		,D8
	MMEMO	,3A30
		,A1
		,D5
		,A2
		,D1
RECORD	,X
		,D2
		,D6
		,A2
		,A15
		,D8
	M_LONG	,A63
		,A27
		,A1
		,D5
		,A2
		,D1
RECORD,X
	W_KEY	,A63

RECORD,X
		,D2
	COMSEQ	,A4
		,A2		
;---------------------------
RECORD	LINE2			
	WSEQ12	,D2		
	WSEQ22	,D2
	WSEQ32	,D2	
	WDEPT2	,A2
	WSQNO2	,A2
	WITEM2	,A15
	WPRICE2	,D8
	WDESC2	,A30
	WADD2	,A60	;FILLER
	WTYPE2	,A1		;L=LINE, M=MEMO
	WQTY2	,D5
	WUM2	,A2
	WCPFLGW	,D1
RECORD,X
	W_KEY2	,A63

RECORD,X
		,D2
	COMSEQ2	,A4
		,A2		;not reversed
;---------------------------

RECORD	PTEST
		,A80
RECORD,X
	P_TST	,15A5

RECORD
	INIT	,20A3,	'JJM','PDS','LAM','DEG','JTH',
&			'DLH','JIM','SJM','CJM','BIL',
&			'MAW','PAT','BJZ','JPV','MJM',
&			'JWS','GJK','   ','   ','   '
RECORD	FF
	NEG_ZERO	,D1,1
	ZERO		,D1,0
	X_FRT		,D1
	A_FRT		,A7


RECORD	OPNCHN
	XHN001	,D2
	XHN002	,D2
	XHN044	,D2
	XHN171	,D2
	XHN147	,D2
	FAXCHN	,D2
	XRRCHN	,D2
	XHN182	,D2
	XHNRET	,D2
	XHN139	,D2

RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN182	,D2
	CHN044	,D2
	CHN045	,D2
	CHN054	,D2
	CHN175	,D2
	CHNWRK	,D2
	CHNOUT	,D2
	CHNDUC	,D2
	CHN139	,D2

RECORD
	TM2	,A90
	TM3	,A90
RECORD,X
	TM2A	,3A30
	TM3A	,3A30


RECORD	ORDFMT
	OF_ORD	,A6
		,A1,	'.'
	OF_NUM	,D1

RECORD	VARS
	I	,D6
	FRT_LN	,D1
	BL	,D4
	STS	,D1
	OPNOK	,D1
	cd_rfa	,a6
	POLX1	,A12
	POLX2	,A12
	SLM	,D3		;length of salesmans name
	SREC	,D5
	S_NAME	,A12
	LL	,D2
	PRINTER_NAME	,A4
	LP_ON		,D1	;first pass just to count pages
	NPAG		,D4
	HPR		,D4	;ROW FOR CALL HP

	P_OLINE	,D1	;1=PRINTING ORDER LINE
	SPLFL2	,A14
	t_cnt	,d4
	VL	,D3,	082
	VL2	,D3
	SZ1	,D3
	SZ2	,D3

	SKPLIN	,D2
	NO_DUCT	,D1
	A2	,A2
	SAVKEY	,A63
	CURKEY	,A63
	SAVQTY	,D6
	STAT	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	DECMAL	,D18
	PLINE	,A95
	S_PLINE	,A95
	LONG_LINE	,A110
	LINCNT	,D3
	TOTPRC		,D8
	SAV_TOTPRC	,D8
	BLANKS		,A30
	SAVJOINT	,D5
	SAVFEET		,D6
	SAVFLIN		,D6
	SAVLBS		,D6
	CONFIG	,D7
	PAGE	,D2
	KEY	,A6
	BSMID	,D5
	SRCCTL	,D1
	SWITCH	,D1
	ORGCUS	,D5
	ORGSHV	,D5
	V	,D1
	ACCUOM	,A2
	J	,D5
	TL	,D3
	SEQNO	,D2
	SAVSEQ	,D2
	SAVSQ1	,D2
	SAVSQ2	,D2
	SAVSQ3	,D2
	SAVDPT	,A2
	SAVCOM	,A4
	MULTLINE	,D5
	SKIP_LINE	,D1		;SKIP THIS LINE IF BLOCK MEMO
	MEMO_ON		,D1
	BLANK90		,A90
	TMPDSC		,A30
	REM		,D4
	DASHES		,A30,	'------------------------------'
	LFEED	,D2
	DTMASK	,A8,	'XX/XX/XX'
	NUMASK	,A8,	'ZZZZZZX-'
	MAXLIN	,D2

PROC
	MAXLIN = 57

; these channels opened in the mainling...
	OPNCHN = O_CHAN

	CHN001 = XHN001
	CHN002 = XHN002
	CHN044 = XHN044
	CHN182 = XHN182

;;;	XCALL OECO(CHN182,COINFO)	;SSQ 11-21-03
	XCALL OECO(CHN182,COINFO,C_CODE)	;SSQ 02-17-04

;;;	B_ERR = 2	;ASSUME ERROR CLEAR IF EVERYTHING WORKS.

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	call read_liner		;2-27-17

	READ (CHN044, ORDHDR, ORDNO) [ERR=ENDOFF]

	SORD = OORDNO
	SFAX = OORDNO

	TOTPRC =
	MEMO_ON = 1
	CALL LOAD_MEMOS
	CALL LOAD_WORK
	CALL CONSOLIDATE

	CALL LOAD_DUCT
	CALL CONS_DUCT


;.....................................
;;; Add memos to output file...
	CLEAR SAVCOM, SAVSQ1, SAVDPT

	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_BM]

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
	STORE (CHNOUT, LINE, W_KEY)
	GOTO FIN_LOOP
EOF_FIN,
	CLOSE CHNWRK
; beginning of print stuff, make two passes thru the logic,
; one to count pages, two to do the actual printing...

;;;	lp_on = 0	;don't print
	PAGE = 0

START_PRINT,
	CALL OPEN_SPLFIL
	
	TOTPRC =
	NPAG = PAGE	;TOTAL # OF PAGES FROM 1ST PASS
	PAGE = 0

	CALL PRTHDR

	CALL WRTMEM		;INTERNAL ROUTINE TO WRITE WORKSHEETS
	CALL GETDUC		;PROCESS DUCTWORK
	CALL ENDORD

;;;	CLOSE 14
;;;	INCR LP_ON
;;;	IF(LP_ON .LE. 1) GOTO START_PRINT

	CLOSE CHNOUT
	CLOSE CHNWRK
	XCALL DELET (WRKFIL)
	XCALL DELET (OUTFIL)

	CLOSE CHNDUC
	XCALL DELET (DUCFIL)

;;;	B_ERR = 1		;SUCCESS
	RETURN
;------------------------------------------

ENDOFF,
	CLOSE CHN045
	CLOSE CHNWRK
	RETURN

;================================================

NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVDPT = WDEPT
	RETURN
;------------------------------------------

NEWSQ1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVSQ1 = WSEQ1
	IF (WSEQ1 .LE. 0) RETURN
	CLEAR LINE2
	WSEQ12 = SAVSQ1
	CLEAR WSEQ22, WSEQ32
	WITEM2 = '   F1'
	WTYPE2 = 'M'
	WDESC2 = F1_MEMOL(WSEQ12)
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
		IF (TM2A(J).NE.BLANKS) WITEM(4,5) = 'F2'
		IF (TM3A(J).NE.BLANKS) 
			BEGIN
			WITEM(4,5) = 'F3'
			WDESC(32,32) = '*'
			END
		IF (TM2A(J).NE.BLANKS .AND. TM3A(J).NE.BLANKS) 
			BEGIN
			WITEM(4,5) = 'F2'
			END
		WITEM (10,10) = J,'X'
		STORE (CHNOUT, LINE, W_KEY)
		END
		
	RETURN
;------------------------------------------

GETDUC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	READ(CHNDUC,DUCWRK,^FIRST)[ERR=NO_DUCT]

	GOTO CONDUC
NXTDUC,
	READS(CHNDUC,DUCWRK,GETDUC_EOF) [ERR=GETDUC_EOF]
CONDUC,
	WSTY = 10 - WSTY
	WSIZE3 = 100 - WSIZE3
	WSIZE1 = 1000000 - WSIZE1
	WSIZE2 = 1000000 - WSIZE2
	SZ1 = %TRN3(WSIZE1)
	SZ2 = %TRN3(WSIZE2)
	
	CALL PRTDUC
	GOTO NXTDUC

GETDUC_EOF,
	CALL PRTSDU
NO_DUCT,
	RETURN
;--------------------------------------------

BADCON,
	LFEED = 8
	XCALL LINFD (LFEED)
	LINCNT =
	PLINE (32,60) = 'DUCT INFORMATION IS '	
	CALL PRINT
	GOTO ERRMSG
ERRMSG,
	PLINE (32,60) = 'PRESENTLY OVERFLOWING THE'
	CALL PRINT
	PLINE (32,60) = 'ARRAY BOUNDARIES.  PLEASE CALL'
	CALL PRINT
	PLINE (32,60) = 'SOFTWARE SUPPORT TO CORRECT THE'
	CALL PRINT
	PLINE (32,60) = 'PROBLEM'
	CALL PRINT
	LFEED = 41 - LINCNT
	IF (LFEED) XCALL LINFD (LFEED)
	GOTO ENDUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

INVORD,
	LFEED = 8
	XCALL LINFD (LFEED)
	LINCNT =
	PLINE (32,60) = ORDNO,'ORDER NUMBER: XXXXXX IS NOT'	
	CALL PRINT
	PLINE (32,60) = 'FOUND ON THE ORDER HEADER FILE'
	CALL PRINT
	PLINE (32,60) = 'PLEASE NOTE THIS AND CALL'
	CALL PRINT
	PLINE (32,60) = 'SOFTWARE SUPPORT TO CORRECT THE'
	CALL PRINT
	PLINE (32,60) = 'PROBLEM'
	LFEED = 41 - LINCNT
	IF (LFEED) XCALL LINFD (LFEED)
	GOTO ENDUP

;------------------------------------------

ENDORD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	LL = 63 - LINCNT
	LL = 62 - LINCNT
	IF (LL .GT. 0) 
		BEGIN	
		A2 = LL,'ZX'
		XCALL RTF(faxchn, LN_FEED, A2)
		LINCNT = LINCNT + LL
		END

	SAV_TOTPRC = TOTPRC

	A_FRT = OFRGHT
	USING A_FRT SELECT
	('    -0'),	X_FRT = NEG_ZERO
	('     0'),	X_FRT = ZERO
	(),		X_FRT = 9
	ENDUSING


	PLINE (70,82) = ' NET AMOUNT:'
	PLINE (83,93) = TOTPRC,'ZZZ,ZZZ.ZZ-'

	IF (B_ERR .EQ. 3) CLEAR PLINE(70,95)

	CALL PRINT

	LFEED = MAXLIN - LINCNT
	A2 = LFEED,'ZX'
	IF(LFEED .GT. 0) XCALL RTF(FAXCHN,LN_FEED,A2)

;;;	LFEED = 2	;SSQ 2-1-05
;;;	XCALL RTF(FAXCHN, LN_FEED, LFEED)

;---------------------------------------------
	CLEAR FRT_LN
	USING X_FRT SELECT
	(NEG_ZERO),	BEGIN
			PLINE(67,82) =  ' ESTIMATED FRT:'
			PLINE(91,93) = 'N/C'
			FRT_LN = 1
			END
	(9),		BEGIN
			PLINE(67,81) =  ' ESTIMATED FRT:'
			PLINE(82,93) = OFRGHT,	'ZZ,ZZZ.XX-'
			FRT_LN = 1
			END
	ENDUSING
	IF (B_ERR .EQ. 3) CLEAR PLINE(67,95)
	CALL PRINT

	IF (X_FRT .NE. ZERO)
		BEGIN
		PLINE(66,82) = 'ESTIMATED TOTAL:'
		PLINE(8,93) = (TOTPRC+OFRGHT),'ZZZ,ZZZ.ZZ-'
		FRT_LN = 1
		END
	
	IF (B_ERR .EQ. 3) CLEAR PLINE(66,95)
	CALL PRINT

;;;	LFEED = 3
;;;	IF (FRT_LN .EQ. 1) LFEED = 2	;DON'T KNOW WHY THIS NEEDS TO BE
;;;	XCALL RTF(FAXCHN,LN_FEED,LFEED)
	RETURN
;---------------------------------------------

ENDUP,
;;;	CLOSE 14

	CALL CLOSE
	RETURN	
;----------------------------------------------
;----------------------------------------------

CONTIN,
	LFEED = MAXLIN - LINCNT
;;;	IF (LFEED) XCALL LINFD (LFEED)
	A2 = LFEED,'ZX'
	IF(LFEED .GT. 0) XCALL RTF(FAXCHN,LN_FEED,A2)
	CLEAR PLINE
	CALL PRINT
	PLINE (70,84) = '** CONTINUED **'
	CALL PRINT
;;;	LFEED = 10
;;;	LFEED = 11
	LFEED = 8
	A2 = LFEED,'ZX'
	XCALL RTF(FAXCHN,LN_FEED,A2)

PRTHDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Print this info on every page...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this is an attempt to avoid extra page at end of fax.  Line-feed
; 2 less lines at the end of each page...

	LFEED = 3	;ssq 2-1-5
;;;	LFEED = 1
;;;	LFEED = 5
	A2 = LFEED,'ZX'
	XCALL RTF(FAXCHN,LN_FEED,A2)

;;;	writes (faxchn, '\page')
	INCR PAGE

	CLEAR P_OLINE
	CLEAR LINCNT
	
	IF(PAGE.EQ.1)
	THEN	CALL PAGE_1
	ELSE	CALL PAGE_2

	CALL SUB_HDR

	RETURN
;-----------------------------------------


PAGE_1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; FORMAT FOR FIRST PAGE
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CALL DIS_BOLD
	CALL DIS_8
;;;	DISPLAY(faxchn,'SHEET METAL')

	CALL DIS_12
	CALL DIS_BOLD
	CLEAR PLINE
	pline (46,60) = 'Confirming Only'
	DISPLAY(faxchn,PLINE(1,67))
	DISPLAY(faxchn, '  ORDER NO.')
;;;	PLINE (78,95) = '  ORDER NO.'
	CLEAR PLINE
	CALL PRINT
	A2 = '32'
	XCALL RTF(faxchn,FS,A2)
	XCALL RTF(faxchn,BOLD)
	DISPLAY (FAXCHN, C_NAME)
	CALL DIS_10
	OF_ORD = OORDNO,	'ZZZZZX'
	CLEAR PLINE
	DISPLAY(faxchn, PLINE(1,15))
	DISPLAY(faxchn, OF_ORD)
	CALL PRINT

	XCALL RTF(faxchn,FS,'18')
;;;	XCALL RTF(faxchn,PLAIN)
	PLINE = C_ADD1
	CALL PRINT

	PLINE = C_ADD2
;;;	PLINE(76,95) = ' Ship Date'
	PLINE(76,95) = '  Fab Date'
	CALL PRINT

	CALL DIS_BOLD
	PLINE(2,13) = C_LOC,	'XXX-XXX-XXXX'
	PLINE(16,27) = C_LD,	'XXX-XXX-XXXX'
;;;	PLINE = " 763-572-0000  800-328-1966"
;                123456789012345678901234567890
;                         1         2
	display (faxchn,pline(1,%trim(pline)))
	CALL DIS_10
	
	XCALL DATE8(OPROMD, D_OUT, D_OUTR, D_FMT, D_SW)
	clear pline
	display(faxchn,pline(1,40))	;40 spaces
	display(faxchn,d_fmt)
	clear pline
	call print

	XCALL RTF(faxchn,BOLD)
	XCALL RTF(faxchn,FS,'18')
	PLINE = 'FAX:'
	PLINE(5,16) = C_FAX,	'XXX-XXX-XXXX'
	CALL PRINT

;;;	XCALL RTF(faxchn,PLAIN)

	RETURN
;---------------------------------------------------------

PAGE_2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; FORMAT FOR ALL PAGES EXCEPT FIRST...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL RTF(faxchn,FS,'18')
	XCALL RTF(faxchn,BOLD)
	pline (46,60) = 'Confirming Only'
	PLINE (78,95) = '  ORDER NO.'
	CALL PRINT

	XCALL RTF(faxchn,FS,'20')
	CLEAR PLINE
	DISPLAY(faxchn, PLINE(1,70))
	OF_ORD = OORDNO,	'ZZZZZX'
	DISPLAY(faxchn, OF_ORD)
	CALL PRINT

	RETURN
;---------------------------------------------------------


SUB_HDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CALL DIS_MEDIUM
	CALL DIS_12
	CALL PRINT			;SKIP A LINE

	PLINE(7,13)=OCUSNO
;;; 3-31-03 ssq>>>	PLINE(20,25)=CRDLMT,	'ZZZZZZ'
	CALL PRINT
	
	XCALL RTF(faxchn,BOLD)
	PLINE = 'SOLD'
	PLINE(51,95) = 'SHIP'
	PLINE(7,37) = OCUSNM
	PLINE(58,95) = OSHPNM
	CALL PRINT

	PLINE = 'TO'
	PLINE(52,53) = 'TO'
	PLINE(7,37) = ADD1
	PLINE(58,95) = OSHAD1
	CALL PRINT


;;;	XCALL RTF(faxchn,PLAIN)
	XCALL RTF(FAXCHN,FS,'18')

	PLINE(7,37) = ADD2
	PLINE(58,95) = OSHAD2
	CALL PRINT

	PLINE(7,21) = CITY
	PLINE(23,24) = STATE
	PLINE(26,35) = ZIP
	PLINE(58,95) = OSHAD3
	CALL PRINT
	CALL PRINT	;SKIP A LINE


;first header...
	XCALL RTF(faxchn,BOLD)
	PLINE = 'Sales-Rep     Cust P.O.    Cust Job      Entered   Shipped   Shipped Via
&       Ppd Col Cod'
;;;&           Ppd Col Cod'
	CALL PRINT


	SREC = OSLMAN
	IF(SREC.LT.1 .OR. SREC.GT.99) SREC = 1
	XCALL IO (CHN054, SALMAN, SREC, READ, LOKCTL)
	SLM = %TRIM(SLSNM)
	IF(SLM .LE. 12) 
	THEN	S_NAME = SLSNM
	ELSE	S_NAME = SLSAD1

	CALL DIS_BOLD
	DISPLAY (faxchn, ' ')
	DISPLAY (faxchn, S_NAME)

	CALL DIS_MEDIUM
	CALL DIS_12
	POLX1 = OPONO
	POLX2 =
	IF (OMETRO .NE. BLANKS)
		BEGIN
		POLX1 = OMETRO
		POLX2 = OPONO
		END

	PLINE (1,12) = POLX1
	PLINE (14,23) = OJOBNO

	XCALL DATE8(OORDDT, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE (28,35) = D_OUT,DTMASK		; PROMISE DATE
	CALL GET_SCAC
	PLINE (48,63) = SC_NAME		;ssq 10-17-01

	CASE OCLPPD OF 
	  BEGINCASE
	    'P':	PLINE (68,68) = 'X'
	    'C':	PLINE (71,71) = 'X'
	    'D':	PLINE (76,76) = 'X'
	  ENDCASE
;;;PLINE='12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345'

	DISPLAY(faxchn,BLANKS(1,3))
	DISPLAY(faxchn,PLINE)
	CLEAR PLINE
	CALL PRINT

	PLINE ( 15,26 ) = POLX2
	CALL PRINT

;;;	CALL DIS_14
;;;	CALL DIS_BOLD
	XCALL RTF(faxchn,BOLD)
	XCALL RTF(faxchn,FS,'16')
	LONG_LINE=" ORDER   SHIP       B.O.      ITEM NO.                 DESCRIPTION              UNIT PRICE UNIT    TOTAL"
	DISPLAY(faxchn,LONG_LINE)

;;;	XCALL RTF(faxchn,PLAIN)
	XCALL RTF(faxchn,FS,'18')
	CLEAR PLINE
	CALL PRINT		

	RETURN
;------------------------------------------------------------

PRTLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Print out line item detail
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (WTYPE .EQ. 'M') RETURN

	IF (LINCNT.GE.MAXLIN) CALL CONTIN

	CALL PRINT

	PLINE ( 28,40 ) = WITEM

; print * if memo attached...
	IF (MSEQ.GT.0) PLINE(40,40) = '*'	

	IF ( WITEM.EQ.'???' ) 
		BEGIN
		PLINE (28,40) = '*'
		PLINE (29,30) = WITEM(4,5)
		END

	PLINE ( 1,7 ) = WQTY,NUMASK
	PLINE ( 41,70 ) = WDESC

	PLINE ( 70,79 ) = WPRICE,'ZZZZZ.XXX-'
	PLINE ( 80,81 ) = WUM
	DECMAL = ( WQTY * WPRICE ) # 1 
	PLINE ( 82,92 ) = DECMAL,'ZZZZZZZ.XX-'
	TOTPRC = TOTPRC + DECMAL		;INVENTORY ITEMS

	IF (B_ERR .EQ. 2)
		BEGIN
		PLINE(71,95) =
		END
	P_OLINE=1
	CALL PRINT				;10
	RETURN
;----------------------------------------------------

PRTDUC,		;Print duct record
	IF ((LINCNT+3).GT.MAXLIN) CALL CONTIN
	CLEAR PLINE
	CALL PRINT
;;;	XCALL LINFD (1)		;SSQ 11-27-96
;;;	INCR LINCNT		;SSQ 12-4-96

	PLINE (1,7) = WJOINT,NUMASK
	PLINE (8,9) = 'JT'
	IF (WDUTYPE.NE.2.AND.WDUTYPE.NE.3)
	BEGIN
	  PLINE (13,14) = WGAUGE,'XX'
	  PLINE (15,16) = 'GA'
	END
	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3)
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
	PLINE (47,49) = DSEAM(WSEAM)
	PLINE ( 52,66 ) = WLOCAT
	PLINE (74,80) = WGPRICE,'ZZ.XXX-'
	DECMAL = ( WPOUNDS * WGPRICE ) # 1
	PLINE (81,82) = 'LB'
	PLINE (83,92) = DECMAL,'ZZZZZZ.XX-'
	TOTPRC = TOTPRC + DECMAL		;DUCT
	P_OLINE=1
	IF (B_ERR .EQ. 2)
		BEGIN
		PLINE(75,95) =
		END
	CALL PRINT

	PLINE (  1,7  ) = WPOUNDS,NUMASK
	PLINE (  8,8  ) = '#'
	PLINE ( 12,17 ) = WSQFLIN,NUMASK
	PLINE ( 19,23 ) = 'SQ FT'

	PLINE ( 31,38 ) = 'NO LINER'
	IF (WLINER.GE.1.AND.WLINER.LE.9.AND.WLINER.NE.4)
	BEGIN
	  PLINE ( 31,38 ) = DLINER( WLINER )
	END
	IF (WSEAL.EQ.1) PLINE (40,43) = 'SEAL'

	PLINE ( 46,60 ) = WDPUNCH
	PLINE ( 73,80 ) = WLINPRC,'ZZZ.XXX-'
	PLINE ( 82,83 ) = 'SF'
	DECMAL = ( WLINPRC * WSQFLIN ) # 1
	PLINE ( 84,92 ) = DECMAL,'ZZZZZZZ.XX-'
	TOTPRC = TOTPRC + DECMAL		;LINER
	P_OLINE=1

	IF (B_ERR .EQ. 2)
		BEGIN
		PLINE(74,95) =
		END
	CALL PRINT

	RETURN
;--------------------------------------------------

PRTSDU,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;Print summarized duct from this order
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF ((LINCNT+1).GT.MAXLIN) CALL CONTIN
;;;	XCALL LINFD (1)
;;;	INCR LINCNT
	CLEAR PLINE
	CALL PRINT
	FOR I FROM 1 THRU MAXDUC
	  BEGIN
	  DUCREC = SDUCRC(I)
	  IF (DUCCFG.EQ.0) GOTO PRTSD2
	  IF ((LINCNT+1).GT.MAXLIN) CALL CONTIN
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
;;;	  SDUCRC(I) =
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
	;;;		LINSQF(I) =
			END
		END
	RETURN	
;------------------------------------------------

FNDCUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (OCUSNO.NE.CUSNO)
	BEGIN
	  XCALL ISIO (CHN001, CUSMAS, OCUSNO, READ ,LOKCTL)
	  IF (LOKCTL .NE. 0)
		BEGIN
		CUSMAS =
		NAME = '* CUSTOMER NOT ON FILE *'
		END
;;;	  KEY = OCUSNO,'XXXXXX'
;;	  XCALL SERCH (CHN002,CUSIDX,KEY,1,6,ORGCUS,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	  LOKCTL = 1
;;;	  CASE SRCCTL OF 
;;;	  BEGINCASE
;;;	  0:	XCALL IO (CHN001,CUSMAS,IRC001,READ,LOKCTL)
;;;	  1:	BEGIN
;;;		  CUSMAS =
;;;		  NAME = '* CUSTOMER NOT ON FILE *'
;;;		END
;;;	  ENDCASE

	END
	RETURN
;--------------------------------------------------

MEMO_PRINT,
; don't print memo on page by itself...
	IF (LINCNT .GT. MAXLIN-1)
		BEGIN
		S_PLINE = PLINE
		CALL CONTIN
		PLINE = S_PLINE
		END

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TL = %TRIM(PLINE)
	IF (TL .LE. 0) TL = 1
	WRITES (faxchn,PLINE(1,TL))
	WRITES (faxchn, '\par')

	PLINE =
	INCR LINCNT
	RETURN
;--------------------------------------------------

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
	RETURN
;-----------------------------------------------------
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
	IF (LQTYOR .EQ. 0) GOTO LOOP		;SSQ 3/24/00

	WTYPE = 'L'
	WDEPT = LPRDCD(1,1)
	WSQNO = LSRTSQ

	A2 = LMSQ1, 'XX'
	WSEQ1 = A2
	A2 = LMSQ2, 'XX'
	WSEQ2 = A2
	A2 = LMSQ3, 'XX'
	WSEQ3 = A2

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
;;;2-21-01	TOTPRC = TOTPRC + (WQTY*WPRICE)#1
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
	STORE(CHNDUC,DUCWRK,WD_KEY)

	GOTO LD_LOOP
LD_EOF,
	RETURN
;-----------------------------------------------
CONS_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; CONSOLIDATE DUCTWORK
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR ARRAYS, DUCREC, ACCREC
	FOR I FROM 1 THRU MAXACC	SACCRC (I) =
	FOR I FROM 1 THRU MAXDUC	SDUCRC(I)=
	FOR I FROM 1 THRU 9		LINSQF(I)=

	CLEAR MULTLINE, SAVJOINT, SAVFEET, SAVFLIN, SAVLBS
	FIND(CHNDUC,DUCWRK,^FIRST) [ERR=EOF_CD]
	SAV_WD_KEY = '***'

CD_LOOP,
	READS(CHNDUC,DUCWRK,EOF_CD)
;;;2-21-01	TOTPRC = TOTPRC + (WPOUNDS*WGPRICE)#1 + (WSQFLIN*WLINPRC)#1
	CALL SUMDUC
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
;;;	READS(CHNDUC,DUCWRK,EOF_CD)	;8-15-01 ssq: this gets us back
	READS(CHNDUC,DUCWRK,OUT_WDKEY)	;8-15-01 ssq: this gets us back
					;to the record just read at cd_loop


OUT_WDKEY,
	SAV_WD_KEY = WD_KEY
	SAVJOINT = WJOINT
	SAVFEET = WSQFEET
	SAVFLIN = WSQFLIN
	SAVLBS = WPOUNDS

	CLEAR MULTLINE
	RETURN
;-----------------------------------------------

SUMDUC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CONFIG(1,3) = WGAUGE
	IF (DUTYPE.EQ.2.OR.DUTYPE.EQ.3) CONFIG(1,3) = WTHICK
	CONFIG(4,4) = WDUTYPE
	CONFIG(5,5) = WCAT
	CONFIG(6,6) = 10-WSTY
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

	RETURN
;------------------------------------------------------------

WRTMEM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; ROUTINE TO FORMAT AND PRINT WO'S
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLOSE CHNOUT
	OPEN (CHNOUT, SI, OUTFIL)
	SAVDPT = -1

W_LOOP,
	READS (CHNOUT, LINE, W_EOF)
	IF (WTYPE.EQ.'L' .AND. WQTY.EQ.0) GOTO W_LOOP		;SKIP OF QTY=0
	IF (WDEPT .NE. SAVDPT) CALL W_NEWDPT
	IF (WTYPE .EQ. 'M')
	THEN	BEGIN
		IF (MEMO_ON .EQ. 0)
			BEGIN
			CLEAR PLINE
			CALL MEMO_PRINT
			MEMO_ON = 1
			END
		IF (MITEM.EQ.'   F1') 
		THEN	FOR J FROM 1 THRU 3
			BEGIN
			TMPDSC = MMEMO(J)
			CALL MAIN_MEMO
			PLINE (8,38) = TMPDSC
			IF (MMEMO(J) .NE. BLANKS) CALL MEMO_PRINT
			END
		ELSE	BEGIN
			PLINE(8,70) = M_LONG
			CALL MEMO_PRINT
			GOTO W_LOOP
			END

		END
	ELSE	BEGIN
		P_OLINE=1
		CALL PRTLIN
		END

	GOTO W_LOOP
;====================================================

MAIN_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;
	TL = %TRIM(TMPDSC)
	REM = 30 - TL
	REM = REM/2
	IF (REM.GT.0)
		BEGIN
		CLEAR TMPDSC
		TMPDSC(1,REM) = DASHES
		TMPDSC(REM+1,30) = MMEMO(J)
		TMPDSC(31-REM,30) = DASHES
		END
	RETURN
;-----------------------------------------

W_NEWDPT,
	SAVDPT = WDEPT
	RETURN
;------------------------------------
W_EOF,
	RETURN
;===============================================

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ASCII(27, MB_ESC)
	XCALL ASCII(27,PJ_ESC)
	SWITCH = 5

;---------------------------------------------------
; ORDHDR already opened on ch 4 in calling routines...
;;;	CHN044 = 4

;;;	CHN044 = 24
;;;	XCALL CHNOPN (24,STAT)
;;;	IF (STAT .EQ. 0)
;;;		BEGIN
;;;		XCALL FILES (24,'SI',044,SWITCH)
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	L_EXT = C_CODE
	D_EXT = C_CODE
	S_EXT = C_CODE

	L_DEV = C_CODE
	D_DEV = C_CODE
	S_DEV = C_CODE
	
	L_EXT(3,3) = 'M'
	D_EXT(3,3) = 'M'

	CHN045 = 25
	XCALL CHNOPN (25,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		OPEN(25,SI,FFLLIN)
	;;;	XCALL FILES (25,'SI',045,SWITCH)
		END

	CHN175 = 26
	XCALL CHNOPN (26,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		OPEN(26,SI,FFLDUC)
	;;;	XCALL FILES (26,'SI',175,SWITCH)
		END

	CHN054 = 27
	XCALL CHNOPN(27,STAT)
	IF(STAT .EQ. 0)
		BEGIN
		OPEN(27,I,FFLSLS)
	;;;	XCALL FILES (27,'I',054,SWITCH)
		END


;Create work file...
	WFORD = ORDNO,'XXXXXX'
	XCALL ISAMC (WRKFIL, 132, 1, 'START=1, LENGTH=63, DUPS, ASCEND')
	OPEN (33, SU, WRKFIL)
	CHNWRK = 33

	OFORD = ORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (OUTFIL, 132, 1, 'START=1, LENGTH=63, DUPS, ASCEND')
	OPEN (34, SU, OUTFIL)
	CHNOUT = 34

	WRORD = ORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (39, SU, DUCFIL)
	CHNDUC = 39

	LOKCTL = 1
	XCALL ISIO (CHN044,ORDHDR,ORDNO,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.OORDNO.NE.ORDNO) GOTO INVORD


;;;	LOKCTL = 1
;;;	XCALL IO (CHN001,CUSCTL,1,READ,LOKCTL)
;;;	ORGCUS = ORG001

	CALL FNDCUS

;;;	LOKCTL = 1
;;;	XCALL IO(3,COPCTL,1,READ,LOKCTL)

	OPNOK = 1
	RETURN
;----------------------------------------------------------

OPEN_SPLFIL,	;;;;;;;;;;;;;;;;;;;;;;;
;;;	OPEN (14,O,FAXFIL)
;;;	OPEN (14,O,SPLFIL)
	RETURN
;----------------------------------------------------------


CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN044
	CLOSE CHN045
	CLOSE CHN175
	RETURN
;----------------------------------------------------------
;;; RTF ROUTINES
;
DIS_8,
	DISPLAY(faxchn, '\fs32')
	RETURN
;---------------------------------------------------------
DIS_10,
	DISPLAY(faxchn, '\fs20')
	RETURN
;---------------------------------------------------------
DIS_12,
	DISPLAY(faxchn, '\fs18')
	RETURN
;---------------------------------------------------------
DIS_14,
	DISPLAY(faxchn, '\fs16')
	RETURN
;---------------------------------------------------------
DIS_BOLD,
	DISPLAY(faxchn, '\b')
	RETURN
;---------------------------------------------------------
DIS_MEDIUM,
; removed 7-23-09 per Cathy & Jessie...
;;;	DISPLAY(faxchn, '\plain\f3\cf0')
	RETURN
;---------------------------------------------------------
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

END

