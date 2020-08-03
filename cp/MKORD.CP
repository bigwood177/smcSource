Subroutine mkord
	ordno	,d
	c_mail	,a
	no_dollars	,d	;2-17-20
;
;
;	MOD: SSQ 11-13-96 Print '*' above u of m if user has 
;			changed price.
;
;	9-19-97 SCAC MOD
;	NT-SERVER VERSION.
;	2-16-00 SSQ:	CONSOLIDATE LINE ITEMS.
;	27-jun-03 ssq:	printer HP 2300 for SMP
;	22-dec-04 ssq: 	add barcodes
; 	5-29-18 ssq: make cusmas isam
;
;DDEPT 5-29-14
EXTERNAL FUNCTION
	TRN3	,D
	R1	,D

EXTERNAL FUNCTION
	GU_NAME	,A

	.INCLUDE 'DEF:HPSUB.DEF'

RECORD	B3OF9
	E_ESC	,A1
		,A*,	'(80X'

RECORD	COINFO
	.INCLUDE 'DEF:COINFO.DEF'

RECORD	C1_LINE
	C1_TYPE	,A1
		,A1
		,A15,	'XXXX-XXXX-XXXX-'
	C1_NUM	,D4

RECORD	C2_LINE
		,A*,	'EXP '
	C2_DAT	,A5	;XX/XX
		,A3
		,A*,	'AUTH '
	C2_AUTH	,A6
	
RECORD	CCTRAN
	.INCLUDE 'DEF:RD138A.DEF'

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
	.INCLUDE 'DEF:RD045A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD045M.DEF'
RECORD,X
	.INCLUDE 'DEF:RD045D.DEF'

RECORD	SAVLIN
	.INCLUDE 'DEF:RD045Z.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD PARAM
	.INCLUDE 'DEF:PARAM2.DEF'

;;;GLOBAL DUCK	
	.INCLUDE 'DEF:RD175D.def'
;;;ENDGLOBAL

RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.NEW'	;1-29-09


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;RECORD	SPLFIL
;;;		,A4,	'SPL:'
;;;		,A1,	'O'
;;;	SPORD	,A6
;;;		,A4,	'.SPL'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
RECORD	F_MEMOS
	.INCLUDE 'def:CPMEMO.DEF'
		


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
	WDESC	,A90
	WPRICE	,D8
	WTYPE	,A1		;L=LINE, M=MEMO
	WQTY	,D5
	WUM	,A2
	WCPFLG	,D1
RECORD	MEMO,X
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
		,D2
		,D6
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
;;;	W_KEY	,A63
	W_KEYA	,A55
		,A60
	W_KEYB	,A8
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
	WDESC2	,A30
	WADD2	,A60	;FILLER
	WPRICE2	,D8
	WTYPE2	,A1		;L=LINE, M=MEMO
	WQTY2	,D5
	WUM2	,A2
	WCPFLGW	,D1
RECORD,X
;;;	W_KEY2	,A63
	W_KEY2A	,A55
		,A60
	W_KEY2B	,A8

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
	A_FRT		,A6


RECORD	CHANNEL
	CHN182	,D2
	CHN044	,D2
	CHN045	,D2
	CHN054	,D2
	CHN138	,D2
	CHN175	,D2
	CHNWRK	,D2
	CHNOUT	,D2
	CHNDUC	,D2

RECORD
	TM2	,A90
	TM3	,A90
RECORD,X
	TM2A	,3A30
	TM3A	,3A30

RECORD	LP_BOLD
	B_ESC	,A1
		,A1,	'%'
	L_BOLD	,A1

RECORD	LP_MEDIUM
	M_ESC	,A1
		,A1,	'%'
	L_MED	,A1


RECORD	ORDFMT
	OF_ORD	,A6
		,A1,	'.'
	OF_NUM	,D1

RECORD	FOR_ISAMC
	KEYSPEC,	A*,	'START=1:116, LEN=55:8, DUPS, ASCEND'

RECORD	PC_LINE
		,A*,	'PRESSURE CLASS'
		,A1
	PC_PC	,A5
		,A1
		,A2,	'IN'

record	lines
	ln1	,a*,	'Here is a copy of your current order with '
;;;	ln2	,a*,	' Sheet Metal Connectors.'
	ln3	,a*,	'Thank you for the order, we appreciate your business!'
	ln4	,a*,	'If you should have any questions or concerns'
	ln5	,a*,	' please do not hesitate to call me at '
;;;800 328-1966'

RECORD	VARS
	X_NUMBR	,D16	;decrypted CC #
	atc	,a2
	cmpcod	,a3
	cname	,a35
;;;	cphone	,a12
	cphone	,a27
	lf	,a1
	body	,a1000
	send	,d1
	LEN	,D6
	LENS	,D6
	LENP	,D6
	TRANS	,A100
	PDFFIL	,A100
	splfil	,a100

	I	,D6
	B_DATA	,A100
	B_STRNG	,A100
	YELLOW	,D1	;Tray number for yellow paper
	BLUE	,D1	;Tray number for blue paper
	U_NAME	,A25
	TRA_B	,D1
	TRA_Y	,D1
	TRA_W	,D1
	cd_rfa	,a6
	POLX1	,A12
	POLX2	,A12
	SLM	,D3		;length of salesmans name
	SREC	,D5
	S_NAME	,A12
	LL	,D2
	S_LL	,D2
;;;	PRINTER_NAME	,A4
	PRINTER_NAME	,A8
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
	W_KEY	,A63
	W_KEY2	,A63
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
	S_LINCNT	,D3
	S_PAGE		,D3
	LINCNT	,D3
	TOTPRC		,D8
	SAV_TOTPRC	,D8
	BLANKS	,A30
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
	xcall who(cmpcod)

	cmhpLPI = 12		;this is critical for vline spacing !!!

	U_NAME = %GU_NAME
	UPCASE U_NAME
	XCALL ASCII(27,E_ESC)
	set tm_esc, lm_esc = e_esc

	CALL OPENS

	call read_liner

	call get_cmpny

	USING U_NAME SELECT		;6/27/03 SSQ
	('WCALL'),	CALL WCALL
;;;	('SMP'),	CALL SMP
	('CATHYM'),	CALL SMP
	(),		CALL SMC
	ENDUSING

	IF (OSCAC .EQ. '10') CALL WCALL

	TRA_B = BLUE
	TRA_Y = YELLOW

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	PRINTER_NAME='BLUE'
;;;	TRA_B = 1	;TRAY 2 IS BLUE
;;;	TRA_Y = 5	;TRAY 3 IS YELLOW
;;;	IF (OSCAC.EQ.'10' .OR. U_NAME.EQ.'WCALL') 
;;;		BEGIN
;;;		PRINTER_NAME='new_blue'
;;;		TRA_B = 4	;TRAY 1 IS BLUE
;;;		TRA_Y = 1	;TRAY 2 IS YELLOW
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

	W_KEY = W_KEYA + W_KEYB
	STORE (CHNOUT, LINE, W_KEY)
	GOTO FIN_LOOP
EOF_FIN,
	CLOSE CHNWRK
; beginning of print stuff, make two passes thru the logic,
; one to count pages, two to do the actual printing...

	lp_on = 0	;don't print
	PAGE = 0
	S_LINCNT = 0
	S_PAGE = 0
	S_LL = 0
START_PRINT,
	CALL OPEN_SPLFIL
	
	TOTPRC =
	NPAG = PAGE	;TOTAL # OF PAGES FROM 1ST PASS
	PAGE = 0

	CALL PRTHDR

	CALL WRTMEM		;INTERNAL ROUTINE TO WRITE WORKSHEETS
	CALL GETDUC		;PROCESS DUCTWORK
	CALL ENDORD

	CLOSE 14
;;;	CLOSE 46
	INCR LP_ON
	IF(LP_ON .LE. 1) GOTO START_PRINT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	xcall getlog ('spl', trans, len)
	pdffil = trans
	trans (len+1, len+11) = '\Smkord.spl'
	pdffil(len+1, len+7) = ordno,	'\XXXXXX'
	len = %trim (pdffil)
	pdffil (len+1, len+4) = '.pdf'
	lens = %trim (trans)
	lenp = %trim (pdffil)
	xcall mkpdf (trans(1,lens), pdffil(1,lenp) )


	clear pline
	pline (1,7) = 'Order'
	pline (9,14) = ordno,	'ZZZZZX'
	pline (16,18) = 'for'
	pline (20,44) = ocusnm
	send = 2	;don't send, display...

	xcall ascii (10,lf)	
	body = ln1+cname(1,%trim(cname))+'.'+lf+ln3+lf+lf	;;;+slsnm+lf+lf 
;;;+ln4+ln5+cphone

	xcall mail (c_mail, pline(1,44), body, pdffil,atc,atc,atc,atc,atc,atc,send)	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHNOUT
	CLOSE CHNWRK
	XCALL DELET (WRKFIL)
	XCALL DELET (OUTFIL)

	CLOSE CHNDUC
	XCALL DELET (DUCFIL)
ENDOFF,
;;;	CLOSE CHN045
	CLOSE CHNWRK
	CLOSE CHN138
	RETURN

;================================================

NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVDPT = WDEPT
	RETURN
;------------------------------------------

NEWSQ1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear savcom		;ssq 4-26-07

	SAVSQ1 = WSEQ1
	IF (WSEQ1 .LE. 0) RETURN
	CLEAR LINE2
	WSEQ12 = SAVSQ1
	CLEAR WSEQ22, WSEQ32
	WITEM2 = '   F1'
	WTYPE2 = 'M'
	WDESC2(1,90) = F1_MEMOL(WSEQ12)
	WDEPT2 = SAVDPT

	W_KEY = W_KEYA + W_KEYB
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

		W_KEY = W_KEYA + W_KEYB
		STORE (CHNOUT, LINE, W_KEY)
		END
		
	RETURN
;------------------------------------------

GETDUC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	S_LINCNT = LINCNT	;save for pass 2
	S_PAGE = PAGE		;save for pass 2
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
	SAV_TOTPRC = TOTPRC
	S_LL=LINCNT
	LL = HPR+7+LINCNT+1		;1 PAST LAST LINE PRINTED
	IF(LL .LT. 60)
		BEGIN
		XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)
	
		XCALL HP (14, hpPOS,LL,0, hpHLINE,164)
	
		XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
		END

	A_FRT = OFRGHT
	USING A_FRT SELECT
	('    -0'),	X_FRT = NEG_ZERO
	('     0'),	X_FRT = ZERO
	(),		X_FRT = 9
	ENDUSING


	PLINE (72,84) = ' NET AMOUNT:'
	PLINE (85,95) = TOTPRC,'ZZZ,ZZZ.ZZ-'
	XCALL HP (14, hpPOS,60,1,0, PLINE)
	PLINE(72,95)=
	CLEAR PLINE

;---------------------------------------------
	USING X_FRT SELECT
	(NEG_ZERO),	BEGIN
			PLINE(69,84) =  ' ESTIMATED FRT:'
			PLINE(92,94) = 'N/C'
			END
	(9),		BEGIN
			PLINE(69,83) =  ' ESTIMATED FRT:'
			PLINE(85,95) = OFRGHT,	'ZZ,ZZZ.XX-'
			END
	ENDUSING
	XCALL HP (14, hpPOS,61,1,0, PLINE)
	PLINE(69,95)=
	CLEAR PLINE

	IF (X_FRT .NE. ZERO)
		BEGIN
		PLINE(68,84) = 'ESTIMATED TOTAL:'
		PLINE(85,95) = (TOTPRC+OFRGHT),'ZZZ,ZZZ.ZZ-'
		END
	
	XCALL HP (14, hpPOS,62,1,0, PLINE)
	PLINE(68,95)=
	CLEAR PLINE

	RETURN
;---------------------------------------------

ENDUP,
	CLOSE 14

	LPQUE (SPLFIL, copies:1)
	CALL CLOSE
	RETURN	
;----------------------------------------------
;----------------------------------------------

CONTIN,
	LFEED = MAXLIN - LINCNT
;;;	IF (LFEED) XCALL LINFD (LFEED)
	IF(LFEED) FOR I FROM 1 THRU LFEED CALL PRINT
	CALL PRINT
	PLINE (70,84) = '** CONTINUED **'
	CALL PRINT
	WRITES(14,P_EJECT)

PRTHDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Print this info on every page...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	INCR PAGE

	CLEAR P_OLINE
	DISPLAY(14,TOP_MARGIN)
	DISPLAY(14,LEFT_MARGIN)

	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	
	XCALL HP (14, hpPOS,63,1,0, ORDCOM(1))
	XCALL HP (14, hpPOS,64,1,0, ORDCOM(2))

;top of order...
	IF(PAGE.EQ.1)
		BEGIN
		PLINE(40,80) = B_STRNG
		XCALL HP (14,hpPOS,8,20,0,PLINE(20,80))
	;;;	XCALL OF128

		XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)	
		XCALL HP (14, hpPOS,1,26,0, OCOMNT(1))
		XCALL HP (14, hpPOS,2,26,0, OCOMNT(2))
		END

	XCALL HP (14,hpFONT,hp12CPI+hpBOLD)
	XCALL HP (14, hpPOS,2,78,0,'  ORDER NO. ')

	IF(PAGE.EQ.1)		;8-15-01 SSQ
		BEGIN
		XCALL HP (14, hpPOS,5,73,0,'   Estimated Ship Date ')
	;;;	XCALL HP (14, hpPOS,5,78,0,'   Date Due ')
		END
	XCALL HP (14,hpFONT,hp10CPI)

	OF_ORD = OORDNO,	'ZZZZZX'
	IF(OPRTF.LE.0)OPRTF=0
	OF_NUM = OPRTF	;;;+1
	PLINE(1,8) = ORDFMT
	XCALL HP (14, hpPOS,3,67,0,PLINE(1,8))	;ORDER #

	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	
	PLINE = 'Page'
	PLINE(6,8) = PAGE,	'ZZZ' [LEFT]
	TL = %TRIM(PLINE)
	TL = TL + 2
	PLINE(TL,TL+3) = 'OF '
	TL = TL + 3
	PLINE(TL,TL+3) = NPAG,	'ZZZ' [LEFT]
	TL = TL + 3
	XCALL HP (14, hpPOS,4,50,0,PLINE(1,TL))	
	CLEAR PLINE
	
	XCALL HP (14,hpFONT,hp10CPI+hpBOLD)
	XCALL DATE8(OPROMD, D_OUT, D_OUTR, D_FMT, D_SW)
	IF(PAGE.EQ.1)		;8-15-01 SSQ
		BEGIN
		XCALL HP (14, hpPOS,6,66,0, D_FMT)
		END
	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	

	IF(PAGE.EQ.1)
	THEN	CALL HDR_1
	ELSE	CALL HDR_2

	CALL SUB_HDR

	LINCNT =
	RETURN
;---------------------------------------------------------

HDR_1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; FIRST PAGE
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bottom of the order:
	XCALL HP (14,hpFONT,hp14CPI+hpBOLD)
	XCALL HP (14, hpPOS,63,40,0, 
&	"Customer Signature ___________________________________")

	XCALL HP (14,hpFONT,hp12CPI+hpBOLD)
	CLEAR PLINE
	PLINE (72,84) = ' NET AMOUNT:'
	PLINE (85,95) = SAV_TOTPRC,'ZZZ,ZZZ.ZZ-'
	IF(NPAG.GT.1)
		BEGIN
		XCALL HP (14, hpPOS,64,1,0, PLINE)
		PLINE(72,95)=
		END
	CLEAR PLINE

;company name & address...
	XCALL HP (14,hpFONT,hp8CPI+hpBOLD)


	XCALL HP (14, hpPOS,3,1,0, C_NAME)

	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	

	XCALL HP (14, hpPOS,4,1,0, C_ADD1)

	XCALL HP (14, hpPOS,5,1,0, C_ADD2)
	XCALL HP (14,hpFONT,hpBOLD)

	PLINE = %STRING(C_LOC,'ZZZ-ZZX-XXXX') + '  ' + %STRING(C_LD,'ZZZ-ZZX-XXXX')
	XCALL HP (14, hpPOS,6,1,0, PLINE(1,%TRIM(PLINE)))

	PLINE = 'FAX:' + %STRING(C_FAX,'ZZZ-ZZX-XXXX')
	XCALL HP (14, hpPOS,7,1,0, PLINE(1,%TRIM(PLINE)))
	CLEAR PLINE
	IF (OCCRD.EQ.'C')	;SSQ 5-4-05
		BEGIN
		XCALL HP (14, hpPOS,4,30,0, "CREDIT CARD ACCT")
	;;;	C1_TYPE = CT_TYPE
	;;;	C1_NUM = CT_NUMBR(13,16)
		C1_NUM = X_NUMBR(13,16)
		XCALL HP (14, hpPOS,5,30,0, C1_LINE)
		C2_DAT = CT_EXDAT	,'XX/XX'
		C2_AUTH = CT_AUTH,		'ZZZZZX'
		XCALL HP (14, hpPOS,6,30,0, C2_LINE)
		END
	if (ordtyp .eq. 'I')
		begin
		xcall HP (14, hpPOS,4,30,0,'Internet Order')
		end

	HPR = 10		;FOR SUB_HDR
	MAXLIN = 39		;41 PRINT LINES ON FIRST PAGE

	RETURN
;-----------------------------------------

HDR_2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; 2ND THUR LAST PAGE
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	HPR = 5		;FOR SUB_HDR
	MAXLIN = 46
	RETURN
;-----------------------------------------

SUB_HDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	
	CLEAR PLINE
	PLINE(7,13)=OCUSNO
	PLINE(20,25)=CRDLMT,	'ZZZZZZ'
	XCALL HP (14,hpPOS,HPR-1,1,0,PLINE(1,25))
	
	XCALL HP (14,hpFONT,hp12CPI+hpBOLD)
	XCALL HP (14,hpPOS,HPR,1,0,'SOLD')
	XCALL HP (14,hpPOS,HPR,51,0,'SHIP')

	XCALL HP (14,hpPOS,HPR+1,2,0,'TO')
	XCALL HP (14,hpPOS,HPR+1,52,0,'TO')
	XCALL HP (14,hpPOS,HPR,8,0,OCUSNM)

	XCALL HP (14,hpPOS,HPR,58,0,OSHPNM)

	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	XCALL HP (14,hpPOS,HPR+1,8,0,ADD1)
	XCALL HP (14,hpPOS,HPR+2,8,0,ADD2)
	CLEAR PLINE
	PLINE(1,15) = CITY
	PLINE(17,18) = STATE
	PLINE(20,29) = ZIP
	XCALL HP (14,hpPOS,HPR+3,8,0,PLINE(1,30))
	XCALL HP (14,hpPOS,HPR+1,58,0,OSHAD1)
	XCALL HP (14,hpPOS,HPR+2,58,0,OSHAD2)
	XCALL HP (14,hpPOS,HPR+3,58,0,OSHAD3)

	XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)
	XCALL HP (14, hpPOS,HPR+3,0, hpHLINE,164)	;
	XCALL HP (14, hpPOS,HPR+6,0, hpHLINE,164)	;
	XCALL HP (14, hpPOS,HPR+7,0, hpHLINE,164)	;

	XCALL HP (14,hpFONT,hp12CPI+hpBOLD)
	XCALL HP (14, hpPOS,HPR+3,13, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,26, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,38, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,49, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,60, hpVLINE,6)	

	XCALL HP (14, hpPOS,HPR+3,83, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,87, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,91, hpVLINE,6)	

;first header...
;;;	PLINE = 'Date Entered  Cust P.O.    Cust Job     Date Due   Shipped   Shipped Via
	PLINE = 'Sales-Rep     Cust P.O.    Cust Job      Entered   Shipped   Shipped Via
&           Ppd Col Cod'

	POLX1 = OPONO
	POLX2 =
	IF (OMETRO .NE. BLANKS)
		BEGIN
		POLX1 = OMETRO
		POLX2 = OPONO
		END

	XCALL HP (14,hpPOS,HPR+4,0,0,PLINE(1,%TRIM(PLINE)) )
	
	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	CLEAR PLINE

	SREC = OSLMAN
	IF(SREC.LT.1 .OR. SREC.GT.99) SREC = 1
	XCALL IO (CHN054, SALMAN, SREC, READ, LOKCTL)
	SLM = %TRIM(SLSNM)
	IF(SLM .LE. 12) 
	THEN	S_NAME = SLSNM
	ELSE	S_NAME = SLSAD1

	XCALL HP (14,hpFONT,hpBOLD)
	XCALL HP (14,hpPOS,HPR+5,0,0,S_NAME)
	XCALL HP (14,hpFONT,hpMEDIUM)

	PLINE ( 15,26 ) = POLX1
	PLINE ( 28,37 ) = OJOBNO

	XCALL DATE8(OORDDT, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE ( 41,48 ) = D_OUT,DTMASK		; PROMISE DATE
	CALL GET_SCAC
;;;	PLINE (62,72) = SC_NAME
	PLINE (62,77) = SC_NAME		;ssq 10-17-01

	CASE OCLPPD OF 
	  BEGINCASE
	    'P':	PLINE ( 85,85 ) = 'X'
	    'C':	PLINE ( 89,89 ) = 'X'
	    'D':	PLINE ( 93,93 ) = 'X'
	  ENDCASE
;;;PLINE='12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345'

	XCALL HP (14,hpPOS,HPR+5,0,0,PLINE)
	CLEAR PLINE

	PLINE ( 15,26 ) = POLX2
	XCALL HP (14,hpPOS,HPR+6,0,0,PLINE)
	CLEAR PLINE
	
	IF(PAGE.EQ.1)
	THEN	VL = 82
	ELSE	VL = 94

	USING PAGE SELECT
	(.EQ. S_PAGE),	REM = MAXLIN - S_LINCNT	;VL to ductwork
	(.GT. S_PAGE),	REM = MAXLIN 		;no VL
	(.LT. S_PAGE),	REM = 0			;VL for entire page
	ENDUSING

	VL2 = (MAXLIN-REM)*2 + 4

	XCALL HP (14, hpPOS,HPR+6,7, hpVLINE,VL2)
	XCALL HP (14, hpPOS,HPR+6,17, hpVLINE,VL2)	
	XCALL HP (14, hpPOS,HPR+6,27, hpVLINE,VL2)	
	XCALL HP (14, hpPOS,HPR+6,40, hpVLINE,VL2)	

	VL2 = VL
	IF(PAGE.EQ.NPAG)	VL2 = S_LL*2 + 4

	XCALL HP (14, hpPOS,HPR+6,72, hpVLINE,VL2)
	XCALL HP (14, hpPOS,HPR+6,81, hpVLINE,VL2)	
	XCALL HP (14, hpPOS,HPR+6,85, hpVLINE,VL)	

	XCALL HP (14,hpFONT,hp14CPI+hpBOLD)
	LONG_LINE=" ORDER   SHIP        B.O.        ITEM NO.                  DESCRIPTION                UNIT PRICE UNIT    TOTAL"
	XCALL HP (14,hpPOS,HPR+7,0,0,LONG_LINE)

	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	CLEAR PLINE
	CALL PRINT		

	RETURN
;------------------------------------------------------------

PRTLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Print out line item detail
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (WTYPE .EQ. 'M') RETURN

	IF (LINCNT.GE.MAXLIN) CALL CONTIN

	IF (WCPFLG) 
	THEN	BEGIN
		PLINE (82,83) = '**'	
		END
	ELSE	BEGIN
		CLEAR PLINE
		END
	CALL PRINT

	PLINE ( 28,40 ) = WITEM

; print * if memo attached...
;;;	IF (MSEQ.GT.0 .OR. MFSEQ.GT.0) PLINE(30,30) = '*'	
	IF (MSEQ.GT.0) PLINE(40,40) = '*'	

	IF ( WITEM.EQ.'???' ) 
		BEGIN
		PLINE (28,40) = '*'
		PLINE (29,30) = WITEM(4,5)
		END

	PLINE ( 1,7 ) = WQTY,NUMASK
	PLINE ( 41,70 ) = WDESC

	PLINE ( 71,81 ) = WPRICE,'ZZZZZZ.XXX-'
	PLINE ( 82,83 ) = WUM
	DECMAL = ( WQTY * WPRICE ) # 1 
	PLINE ( 85,95 ) = DECMAL,'ZZZZZZZ.XX-'
	TOTPRC = TOTPRC + DECMAL		;INVENTORY ITEMS

	P_OLINE=1
	IF (NO_DOLLARS)				;2-17-20
		BEGIN
		PLINE (71,81) = 
		PLINE (85,95) = 
		END

	CALL PRINT				;10
	RETURN
;----------------------------------------------------

PRTDUC,		;Print duct record
	IF ((LINCNT+3).GT.MAXLIN) CALL CONTIN
	CLEAR PLINE
	CALL PRINT

; 3-17-09, not yet...
;;;	IF (WDPC .NE. 999)
;;;	THEN	BEGIN
;;;		PC_PC = WDPC,	'ZX.X-' 
;;;		PLINE (25,50) = PC_LINE
;;;		END
;;;	ELSE	PLINE (25,50) = 'DESIGN BUILD'
;;;	CALL PRINT
; 3-17-09, not yet...

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
	PLINE (75,81) = WGPRICE,'ZZ.XXX-'
	DECMAL = ( WPOUNDS * WGPRICE ) # 1
	PLINE (82,83) = 'LB'
	PLINE (86,95) = DECMAL,'ZZZZZZ.XX-'
	TOTPRC = TOTPRC + DECMAL		;DUCT
	P_OLINE=1
	IF (NO_DOLLARS)				;2-17-20
		BEGIN
		PLINE (71,81) = 
		PLINE (85,95) = 
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

	if (wdpunch .eq. '0') clear wdpunch	;ssq 12-13-05
	PLINE ( 46,60 ) = WDPUNCH
	PLINE ( 74,81 ) = WLINPRC,'ZZZ.XXX-'
	PLINE ( 82,83 ) = 'SF'
	DECMAL = ( WLINPRC * WSQFLIN ) # 1
	PLINE ( 85,95 ) = DECMAL,'ZZZZZZZ.XX-'
	TOTPRC = TOTPRC + DECMAL		;LINER
	P_OLINE=1
	IF (NO_DOLLARS)				;2-17-20
		BEGIN
		PLINE (71,81) = 
		PLINE (85,95) = 
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
	
	XCALL ISIO (36, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL.NE.0)
		BEGIN
		CUSMAS =
		NAME = '* CUSTOMER NOT ON FILE *'
		END
		
	;;;  KEY = OCUSNO,'XXXXXX'
	;;;  XCALL SERCH (37,CUSIDX,KEY,1,6,ORGCUS,BSMID,SRCCTL,4,7,11,0,0,0,0)
	;;;  LOKCTL = 1
	;;;  CASE SRCCTL OF 
	;;;  BEGINCASE
	;;;  0:	XCALL IO (36,CUSMAS,IRC001,READ,LOKCTL)
	;;;  1:	BEGIN
	;;;	  CUSMAS =
	;;;	  NAME = '* CUSTOMER NOT ON FILE *'
	;;;	END
	;;;  ENDCASE
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
	WRITES (14,PLINE)
	IF(P_OLINE)
		BEGIN
		CLEAR PLINE(71,81)
		CLEAR PLINE(85,95)
		CLEAR P_OLINE
		END
	PLINE =
	INCR LINCNT
	RETURN
;--------------------------------------------------

GET_CMPNY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; COP TABLE SCAC LOOK-UP
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'CO'
	C1_CODE = CMPCOD
	C1_REC = 1
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		C1_NAME = 
		END

	CNAME = C1_NAME

	C1_REC = 2
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		C2_LD = 
		END
	CPHONE(1,12) = C2_LOC,	'ZZZ XXX-XXXX'
	CPHONE(13,15) = ' / '
	CPHONE(16,27) = C2_LD,	'ZZZ XXX-XXXX'
	RETURN
;-----------------------------------------------------

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
	W_KEY = W_KEYA + W_KEYB
	IF (W_KEY .NE. SAVKEY)
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
	W_KEY = W_KEYA + W_KEYB		;6-23-03
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
	if (wdpunch .eq. '0') clear wdpunch
	WDSEQ = DSEQ	
	WDPC = DPC	;1-29-09
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
;;;	IF (DUTYPE.EQ.2.OR.DUTYPE.EQ.3) CONFIG(1,3) = WTHICK	;ssq 3-21-06
	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3) CONFIG(1,3) = WTHICK	;ssq 3-21-06
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
;;;	CLOSE CHNOUT
;;;	CLOSE CHNWRK
;;;	XCALL DELET (WRKFIL)
;;;	XCALL DELET (OUTFIL)
	RETURN
;===============================================

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ASCII(27, B_ESC)
	XCALL ASCII(27,PJ_ESC)
	L_BOLD = 'G'		;START ITALIC
	M_ESC = B_ESC
	L_MED = 'H'		;END ITALIC
	SWITCH = 5

;---------------------------------------------------
; ORDHDR already opened on ch 4 in calling routines...

	CHN044 = 24
	XCALL CHNOPN (24,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (24,'SI',044,SWITCH)
		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; already open in inquir...

	CHN045 = 25
;;;	XCALL CHNOPN (25,STAT)
;;;	IF (STAT .EQ. 0)
;;;		BEGIN
;;;		XCALL FILES (25,'SI',045,SWITCH)
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CHN175 = 26
	XCALL CHNOPN (26,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (26,'SI',175,SWITCH)
		END

	CHN054 = 27
	XCALL CHNOPN(27,STAT)
	IF(STAT .EQ. 0)
		BEGIN
		XCALL FILES (27,'I',054,SWITCH)
		END

	XCALL CHNOPN (36,STAT)
	IF (STAT .EQ. 0) XCALL FILES (36,'SI',001,SWITCH)

;;;	XCALL CHNOPN (37,STAT)
;;;	IF (STAT .EQ. 0)XCALL FILES (37,'I',002,SWITCH)

	chn182 = 17	;ssq 10-17-18
;;;	chn182 = 3	;from inquir
;;;	XCALL FILES (18,'SI',182,SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN182 = 18

	XCALL OECO(CHN182,COINFO)	;SSQ 11-13-03

;Create work file...
	WFORD = ORDNO,'XXXXXX'
;;;	XCALL ISAMC (WRKFIL, 132, 1, 'START=1, LENGTH=63, DUPS, ASCEND')
	XCALL ISAMC (WRKFIL, 132, 1, KEYSPEC)
	OPEN (33, SU, WRKFIL)
	CHNWRK = 33

	OFORD = ORDNO,'XXXXXX'
;Create output file...
;;;	XCALL ISAMC (OUTFIL, 132, 1, 'START=1, LENGTH=63, DUPS, ASCEND')
	XCALL ISAMC (OUTFIL, 132, 1, KEYSPEC)
	OPEN (34, SU, OUTFIL)
	CHNOUT = 34

	WRORD = ORDNO,'XXXXXX'
;Create output file...
;;;	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	XCALL ISAMC (DUCFIL, 114, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (39, SU, DUCFIL)
	CHNDUC = 39

	LOKCTL = 1
	XCALL ISIO (CHN044,ORDHDR,ORDNO,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.OORDNO.NE.ORDNO) GOTO INVORD


;;;	LOKCTL = 1
;;;	XCALL IO (36,CUSCTL,1,READ,LOKCTL)
;;;	ORGCUS = ORG001

	CALL FNDCUS

	switch = 5
	xcall files (51, 'I', 060, switch)
	
	LOKCTL = 1
	XCALL IO(51,COPCTL,1,READ,LOKCTL)
	close 51

	SWITCH = 5
	XCALL FILES (51,'SI',138,SWITCH)
	CHN138 = 51

	XCALL ISIO (CHN138,CCTRAN,OORDNO,READ,LOKCTL)
	IF (LOKCTL .NE. 0) CCTRAN =

	X_NUMBR = %R1(CT_NUMBR,'D')	;DECRYPT
	RETURN
;----------------------------------------------------------

OPEN_SPLFIL,	;;;;;;;;;;;;;;;;;;;;;;;
;;;	SPORD = ORDNO,'XXXXXX'

	xcall getlog ('spl', trans, len)
	pdffil = trans
	trans (len+1, len+11) = '\Smkord.spl'

	splfil = trans
	OPEN (14, O, SPLFIL)

;;;	SPORD = ORDNO,'XXXXXX'
;;;	OPEN (14, O, SPLFIL)
;;;	SPLFL2=SPLFIL
;;;	SPLFL2(14,14)='A'
;;;	OPEN (46, O, SPLFL2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL HP (14,hpDOTS, 0)
	XCALL HP (14,hpTHICK,3)
	RETURN
;--------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	CLOSE CHN044
;;;	CLOSE CHN045
;;;	CLOSE CHN175
	CLOSE CHN138

	RETURN
;----------------------------------------------------------

SMC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;4=TRAY1, 1=TRAY2, 5=TRAY3

	PRINTER_NAME = 'SMC_BLUE'
;DEBUG 3-12-18 remove this when new lexmark printers installed...
	blue = 2

;;;	PRINTER_NAME = 'BLUE'
;;;	BLUE = 1	;TRAY 2 IS BLUE
;;;	YELLOW = 5	;TRAY 3 IS YELLOW


	RETURN
;----------------------------------------------------------

SMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;4=TRAY1, 1=TRAY2, 5=TRAY3

	PRINTER_NAME= 'HP 2300'

;for now use same settings as smc... ssq 7/10/03
	BLUE = 1	;TRAY 2 IS BLUE
	YELLOW = 5	;TRAY 3 IS YELLOW
;;;	BLUE = 4	;TRAY 1 IS BLUE
;;;	YELLOW = 1	;TRAY 2 IS YELLOW

	RETURN
;----------------------------------------------------------

WCALL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;4=TRAY1, 1=TRAY2, 5=TRAY3

	using cmpcod select
	('SMC','TST'),	begin
		PRINTER_NAME= 'oki_blue'
		BLUE = 1	;TRAY 1 IS BLUE
		YELLOW = 5	;TRAY 2 IS YELLOW
		end
	(),	begin
		PRINTER_NAME= 'new_blue'
		BLUE = 4	;TRAY 1 IS BLUE
		YELLOW = 1	;TRAY 2 IS YELLOW
		end	
	endusing

;;;	PRINTER_NAME= 'new_blue'
;;;	BLUE = 4	;TRAY 1 IS BLUE
;;;	YELLOW = 1	;TRAY 2 IS YELLOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	RETURN
;----------------------------------------------------------
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


