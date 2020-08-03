;  BILEDT / COP isam
;DDEPT 5-29-14
;
;	12-SEP-1997 SSQ: ADD SCAC CODE
;
;		PGM TO PRINT BILLING EDIT LIST
;
;
;	5-29-18 ssq: make cusmas isam
;	10-8-18 ssq: sbt pricing
;	11-11-19 ssq: remove ocomdu (change to omicus)


RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR

EXTERNAL FUNCTION
	TRN3	,D
	
EXTERNAL FUNCTION
	FUNCV	,A		;FUNCTION RETURNS VIRTUAL ITEM NUMBERS FOR SPIRAL BODY TEES

record	fv_data
	f_item	,a15	;virtual item to return
	f_f3	,5d1	;
	f_cfg	,d1	;same as cfg_item in scrnx
	f_ga	,d2	;

RECORD
	SEGS	,A12			;first 2 dig are gauge
RECORD,X
	GA	,D2
	SARA	,5D2
RECORD,X
		,D2	;GA
	MA	,D2
	MB	,D2
	BC	,D2
	BD	,D2
	
record	itmmas
	.include 'def:rd041a.def'	;required for sbt

RECORD	TMPCUS
	.INCLUDE 'DEF:RD139A.DEF'

RECORD	ORDNOT
	.INCLUDE 'DEF:RD075A.DEF'

RECORD CUSMAS   	
	.INCLUDE 'DEF:RD001A.DEF'
RECORD DUMCUS  ,X	
	.INCLUDE 'DEF:RD001B.DEF'

RECORD ORDHDR   	
	.INCLUDE 'DEF:RD044A.DEF'
RECORD ORDLIN   	
	.INCLUDE 'DEF:RD045A.def'
RECORD ,X		
	.INCLUDE 'DEF:RD045M.def'
RECORD ,X		
	.INCLUDE 'DEF:RD045D.def'
RECORD	SAVLIN
	.INCLUDE 'DEF:RD045Z.def'

RECORD DUMINV		
	.INCLUDE 'DEF:RD041B.def'
RECORD COPCTL
	.INCLUDE 'DEF:RD060A.DEF'
RECORD ARTERM
	.INCLUDE 'DEF:RD170A.DEF'
RECORD ARTCTL,X
	.INCLUDE 'DEF:RD170B.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD PARAM
	.INCLUDE 'DEF:PARAM2.DEF'

GLOBAL DUCK, INIT
	.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL

RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.DEF'

RECORD	DUCFIL
		,A4,	'SPL:'
		,A1,	'D'
	WFORD	,A6
		,A4,	'.ISM'
RECORD	WRKFIL
		,A4,	'SPL:'
		,A1,	'W'
	WRORD	,A6
		,A4,	'.ISM'
RECORD	OUTFIL
		,A4,	'SPL:'
		,A1,	'X'
	OFORD	,A6
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


RECORD	FOR_ISAMC
	KEYSPEC,	A*,	'START=1:116, LEN=55:8, DUPS, ASCEND'

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
	WQTYSH	,D5
	WQTYBO	,D5
	WFLAG	,D1
	WSTATS	,D1
	WTXFLG	,D1
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
		,D5
		,D5
		,D1
		,D1
		,D1
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
		,D5
		,D5
		,D1
		,D1
		,D1
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
	WQTYSH2	,D5
	WQTYBO2	,D5
	WFLAG2	,D1
	WSTATS2	,D1
	WTXFLG2	,D1
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
;--------------------------------------

RECORD
	TM2	,A90
	TM3	,A90
RECORD,X
	TM2A	,3A30
	TM3A	,3A30

RECORD TITLE
		,A17,'BILLING EDIT LIST'
RECORD LEGEND
		,A7,'LEGEND:'
		,A2
		,A18,'B/O = BACK-ORDERED'
		,A6
		,A18,'O/S = OUT-OF-STOCK'
RECORD DASHES
		,A50,'--------------------------------------------------'
		,A50,'--------------------------------------------------'
		,A32,'--------------------------------'
RECORD SUBHD1
		,A50,' ITEM NUMBER      ITEM DESCRIPTION                '
		,A50,'     ORD   SHIP   B/O  UNIT-PRICE   UOM    EXT-PRI'
		,A32,'CE    DISC   NET-PRICE          '
RECORD SUBHD2
		,A50,'                                                  '
		,A50,'     QTY    QTY   QTY                             '
RECORD PBUF
	PLINE	,A132
RECORD
	STRTNA	,A6
	ENDNA	,A6
RECORD	,X
	STRTNO	,D6
	ENDNO	,D6

RECORD	WRK_DATE
	MM	,D2
	DD	,D2
	YY	,D2

RECORD
	MDAYS,	12D2,	31,28,31,30,31,30,31,31,30,31,30,31

RECORD	CHANNEL
	CHNWRK	,D2
	CHNOUT	,D2
	CHN001	,D2
	CHN002	,D2
	CHN041	,D2
	CHN044	,D2
	CHN045	,D2
	CHN075	,D2
	CHN060	,D2
	CHN139	,D2
	CHN170	,D2
	CHN175	,D2
	CHN182	,D2
	CHNDUC	,D2

RECORD	VARSX
	TODAY	,D6
	D_TRY	,D1
	D_DAYS	,D2
	D_DATE	,D6
	D_30_DATE	,D6
	D_AMT	,D6
	D_LIN1	,A50
	D_LIN2	,A50
	D_LIN3	,A50
	DI	,D2

RECORD	FFILE
	REDFIL	,A14
	FILNUM	,D2
	CLCTL	,D1

RECORD	VARS
	sav_lflag	,d1
	sav_ltxflg	,d1
	sbt		,d1	;1=item is spiral body tee
	sbt_price	,d6
	sbt_qty		,d6
	sbt_mat		,d1
	sbt_item	,a15
	sbt_idx		,d6
	sbt_code	,6a3,	'R  ','MN ','C1 ','C2 ','BR1','BR2'
	sbt_lmsq1	,d6
	fl	,d6
	ln	,d6
	mm_code	,a5
	partno	,a15
	xmcod	,a5
	XPOL1	,A12
	XPOL2	,A12
	SAVJOINT	,D5
	SAVFEET		,D6
	SAVFLIN		,D6
	SAVLBS		,D6
	IS_NOTE	,D1
	SZ1	,D3
	SZ2	,D3
	OPNOK	,D1
	NO_DUCT	,D1
	MEMO_ON	,D1
	A2	,A2
	W_KEY	,A63
	W_KEY2	,A63
	SAVKEY	,A63
	CURKEY	,A63
	SAVQTY	,D5
	SAVSHP	,D5
	STAT	,D1
	S_PLINE	,A132
	I	,D6	;SSQ 10-24-06
	J	,D5
	ACCUOM	,A2
	LL	,D2
	TL	,D2
	SEQNO	,D2
	SAVSEQ	,D2
	SAVSQ1	,D2
	SAVSQ2	,D2
	SAVSQ3	,D2
	SAVDPT	,A2
	SAVCOM	,A4
;;;	MULTLINE	,D1	;ssq 4-25-06 BIG problem when this overflows!!
	MULTLINE	,D6
	SKIP_LINE	,D1
	BLANK90		,A90
	TMPDSC	,A30
	REM	,D4
	SAV001	,D5
	ADCNT2	,D4
	ASTRIX	,A17,'*****************'
	DECMAL	,D10
	DSPDAT	,D6
	WRKDAT	,D6
	TOTORD	,D10	;;;
	TOTSAL	,D10
	TAXTOT	,D10
	TOTWGT	,D10
	COMAMT	,D7
	CONFIG	,D7
	LINCNT	,D2,60
	PGCNT	,D3
	MASK	,A13,'Z,ZZZ,ZZZ.XX-'
	MASK1	,A4,'ZZZX'
	MASK2	,A8,'XXXX-XXX'
	PAGENO	,D2
	PRTCTL	,D2
	SOLDTO	,4A29
	SHIPTO	,4A29
	SVDSFL	,A1
	KEY   	,A6
	BSEND	,D5
	BSND2	,D5
	BSMID	,D5
	SRCCTL	,D1
	ENTRY	,A14
	SPLFIL	,A14
	STXCTL	,D1
	BLANKS	,A25
	LPSW	,D2
	LPARG	,D1
	LPONSW	,D1
	RPTNUM	,D3
	PRTTYP	,A1
	SWITCH	,D1
	RECCTR	,D5, 00001
	TOTREC	,D5
	TAX	,D6
	TTDISC	,D2
	V	,D1
	READ	,D1	,0
	WRITE	,D1	,1
	LOKCTL	,D1
	JUST	,D1
	CTR	,D2
	TRMS	,D4
	TMAX	,D2,	30
	STATUS	,D1		;;;
	ORLFIL	,A14		;;;
;;;	DUCFIL	,A14		;;;
	SHVFIL	,A14		;;;
	ARTFIL	,A14		;;;
	ORGSHV	,D5		;;;
	ORGART	,D5		;;;
	DUCTOT	,D8		;;;
	LINTOT	,D8		;;;
	ACCTOT	,D8		;;;
PROC
	XCALL TERID (V)
	XCALL RDATE (TODAY)
	CALL OPENS
	IF (.NOT. OPNOK) 
		BEGIN
		CALL CLOSE
		GOTO ENDOFF
		END
	call read_liner			;2-27-17
BEGIN,
	XCALL OUTPT (3,1,2,'\',1)
	XCALL OUTPT (1,1,0,'BILLING',V)
	XCALL OUTPT (2,1,0,'BILLING EDIT LIST',1)
	TOTORD =
	STXCTL = 1
	XCALL STENO (STRTNA,ENDNA,'ORDER #',6,STXCTL,V)
	GO TO (END2), STXCTL
	STRTNO = STRTNA
	IF (ENDNA.EQ.'[[[') ENDNO = 999999
	IF (ENDNA.NE.'[[[') ENDNO = ENDNA
	READ(4,ORDHDR,^FIRST) [ERR=ENDHDR]
RDHDR,
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GO TO ENDHDR
	IF (ORDHDR.EQ.']]]]]]]]]]') GOTO ENDHDR
	IF (OORDNO.LT.STRTNO) GO TO RDHDR
	IF (OORDNO.GT.ENDNO) GO TO RDHDR
FRSHDR,
;;;	ODSCAL =
	IF (OFLAG.EQ.0) GO TO RDHDR

	MEMO_ON = 1
	CALL LOAD_MEMOS
	CALL LOAD_WORK
	CALL CONSOLIDATE

	CALL LOAD_DUCT
	CALL CONS_DUCT

	CALL PRINT_HEADER

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
	CALL WRTMEM		;INTERNAL ROUTINE TO WRITE WORKSHEETS
	CALL GETDUC		;PROCESS DUCTWORK
	CALL ENDORD
	GO TO RDHDR

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
	;;;		WITEM(4,8) = 'F2&F3'
			END
		WITEM (10,10) = J,'X'

		W_KEY = W_KEYA + W_KEYB
		STORE (CHNOUT, LINE, W_KEY)
		END
		

	RETURN
;------------------------------------------

GETDUC,
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
;;;	CALL PRTSDU
NO_DUCT,
	CLOSE CHNDUC
	XCALL DELET (DUCFIL)
	RETURN
;--------------------------------------------

PRTDUC,
	PLINE (2,3) = WGAUGE,'XX'
	PLINE (4,5) = 'GA'
	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3) PLINE (2,5) = WTHICK,'.XXX'
	PLINE (7,9) = SZ1,'ZZX'
	PLINE (10,12) = ' X '
	PLINE (13,14) = SZ2,'ZX'
	PLINE (15,17) = ' X '
	PLINE (18,19) = WSIZE3,'ZX'
	IF(WSIZE1/1000.NE.SZ1 .OR. WSIZE2/1000.NE.SZ2) PLINE(20,20)='*'
	PLINE (22,25) = DTYPE(WDUTYPE)
	PLINE (27,32) = DCAT(WCAT)
	PLINE (34,36) = DSTY(WSTY)
	PLINE (38,40) = DSEAM(WSEAM)
	IF (WSEAL.EQ.1) PLINE (42,45) = 'SEAL'
	PLINE (47,52) = WJOINT,'ZZZZX-'
	PLINE (53,54) = 'JT'
	PLINE (62,68) = WPOUNDS,'ZZZZZX-'
	PLINE (69,70) = 'LB'
	
	PLINE (73,83) = WGPRICE,'ZZZ,ZZZ.XXX'
	PLINE (88,89) = 'LB'
	DECMAL = (WGPRICE * WPOUNDS)#1
	PLINE (94,104) = DECMAL,'ZZZ,ZZZ.XX-'
	PLINE (114,124) = DECMAL,'ZZZ,ZZZ.XX-'
	DUCTOT = DUCTOT + DECMAL
	call print

LINER,
	PLINE (22,28) = DLINER(WLINER) 
	PLINE (30,34) = 'LINER'
	PLINE (40,50) = WLOCAT
	PLINE (62,68) = WSQFLIN,'ZZZZZX-'
	PLINE (69,70) = 'SQF'
	PLINE (73,83) = WLINPRC,'ZZZ,ZZZ.XXX'
	PLINE (88,89) = 'SF'
	DECMAL = (WLINPRC * WSQFLIN)#1
	PLINE (94,104) = DECMAL,'ZZZ,ZZZ.XX-'
	PLINE (114,124) = DECMAL,'ZZZ,ZZZ.XX-'
	LINTOT = LINTOT + DECMAL
	call print

	RETURN
;-------------------------------------------

PRTLIN,
	IF (WTYPE .EQ. 'M') RETURN
	IF (WQTY.EQ.0.AND.WQTYSH.EQ.0) RETURN
	IF (WSTATS.EQ.2.AND.WQTY.EQ.0) RETURN
	IF (WFLAG.EQ.0) RETURN
	CLEAR PLINE
	PLINE (2,16) = WITEM
	IF (JUST) XCALL LEFTJ (PLINE(2,16),15)

	PLINE (22,52) = WDESC
	IF (WITEM.EQ.'M               ') 
	BEGIN
	  CALL PRINT
	  RETURN
	END
NODATE,
	PLINE (54,59) = WQTY,'ZZZZX-'
	PLINE (61,66) = WQTYSH,'ZZZZX-'
	IF (WSTATS.NE.2.AND.WQTYBO.GT.0) PLINE (68,73) = WQTYBO,'ZZZZ-'
	PLINE (75,83) = WPRICE, 'ZZZZZ.XXX'
	PLINE (88,89) = WUM
	DECMAL = (WQTYSH*WPRICE)#1
	PLINE (94,104) = DECMAL, MASK
	TOTSAL = TOTSAL + DECMAL
	PLINE (114,124) = DECMAL, MASK
	CALL PRINT
;;;	TOTWGT = TOTWGT +(WQTYSH*LITMWT)
	IF (WTXFLG) TAXTOT = TAXTOT + DECMAL

	RETURN
;********************************************************
MEMO_PRINT,
PRINT,
	IF (LPONSW.EQ.0) CALL PRNTON
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,'NO HDR',
&		' ',' ',LEGEND,DASHES,'NO LEGEND',0,0,132,0,LPSW,RPTNUM,PRTTYP)
	RETURN

PRNTON,
	SPLFIL (5,6) = 'EB'
	LPSW = 1		; PRINT, SPOOL OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GO TO ENDOFF
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	LPONSW = 1
	RETURN
;********************************************************
JUSTPT,
	IF (PLINE(PRTCTL-1,PRTCTL).NE.'  ') RETURN
	PRTCTL = PRTCTL - 2
	IF (PRTCTL.EQ.1) RETURN
	GOTO JUSTPT

ENDORD,
; print bottom notes...
	XCALL LINFD (1)
	INCR LINCNT
	OC_ORD = OORDNO
	OC_SEQ = 900
	FIND(CHN075,ORDNOT,COMKEY) [ERR=BXTNOT]
BXTNOT,
	XCALL IOS (CHN075, ORDNOT, READ, LOKCTL)
	IF(LOKCTL .NE. 0) GOTO EOF_BNT
	IF(OC_ORD .NE. OORDNO) GOTO EOF_BNT
	PLINE(22,52) = OC_COM
	CALL PRINT
	GOTO BXTNOT
EOF_BNT,

ORDTOT,
;;;	CALL PRTSAC

	CALL DISPLA_DISC
	XCALL LINFD (1)
	INCR LINCNT

	IF (LINCNT.GT.55) LINCNT = 60
	PLINE (101,112) = 'SALE AMOUNT:'
	PLINE (114,124) = TOTSAL+DUCTOT+LINTOT+ACCTOT, MASK
	IF (DSTFLG.EQ.'Y') PLINE (126,131) = '*ACCT*'
	CALL PRINT
	PLINE (15,65) = D_LIN1

	IF (TAXFLG.NE.'   ') TAXTOT = TAXTOT + DUCTOT + LINTOT + ACCTOT
	PLINE (70,84) = 'TAXABLE AMOUNT:'
	PLINE (86,95) = TAXTOT, MASK
	PLINE (100,112) = 'MISC CHARGES:'
	PLINE (116,124) = OMISC,MASK
	IF (DSTFLG.EQ.'Y'.AND.OMISC.NE.0) PLINE (125,132) = OMSACT, MASK2
	CALL PRINT

	PLINE (15,65) = D_LIN2
	PLINE (74,84) = 'TOTAL COST:'
	PLINE (86,95) = OCOST, MASK
	PLINE (105,112) = 'FREIGHT:'
	PLINE (115,124) = OFRGHT, MASK
	IF (DSTFLG.EQ.'Y'.AND.OFRGHT.NE.0) PLINE (125,132) = OFRACT, MASK2
	CALL PRINT
	PLINE (72,84) = 'TOTAL WEIGHT:'
	PLINE (86,95) = TOTWGT, MASK
	PLINE (99,102) = OTAXFL
	IF (OTAXFL.EQ.'   ') PLINE (99,102) = '***'
	PLINE (103,112) = 'SALES TAX:'
	TAX = OTAX(1) + OTAX(2) + OTAX(3)
	PLINE (115,124) = OTAX(1),MASK
	IF (DSTFLG.EQ.'Y'.AND.OTAX(1).NE.0) PLINE (125,132) = OTXACT(1), MASK2

	PLINE (15,65) = D_LIN3
	CALL PRINT

;;;	PLINE (88,95) = ODSCAL, MASK
;;;NOTRMS,
;;;	PLINE (70,84) = 'TERMS DISCOUNT:'

	IF (OTAX(2).NE.0) PLINE (115,124) = OTAX(2), MASK
	IF (OTAX(2).NE.0) PLINE (125,132) = OTXACT(2), MASK2
	CALL PRINT
	IF (OTAX(3).NE.0) PLINE (115,124) = OTAX(3), MASK
	IF (OTAX(3).NE.0) PLINE (125,132) = OTXACT(3), MASK2
	CALL PRINT
	IF (OTAX(2).NE.0 .OR. OTAX(3).NE.0) XCALL LINFD(1)
	IF (OTAX(2).NE.0 .OR. OTAX(3).NE.0) INCR LINCNT
	PLINE (21,33) = 'COMMISSN AMT:'
	PLINE (46,58) = 'COMMISSN PCT:'

	comamt = 			;11-11-19
;;;	IF (OCOMDU.GT.0) GO TO SKPCOM
;;;	COMAMT = (OCOMDU*OSALE)#4
	COMAMT = (-1)*COMAMT
	PLINE (35,43) = COMAMT, MASK
SKPCOM,
;;;	IF (OCOMDU.GT.0) PLINE (35,43) = OCOMDU, MASK
;;;	IF (OCOMDU.LE.0) PLINE (60,64) = OCOMDU*(-1),'ZZ.XX'
	PLINE (107,112) = 'TOTAL:'
	DECMAL = OSALE + OMISC + OFRGHT + TAX
	PLINE (114,124) = DECMAL, MASK
	CALL PRINT
	TOTORD = TOTORD + TOTSAL + DUCTOT + LINTOT + ACCTOT
	XCALL LINFD (1)
	LINCNT = 60
	TOTWGT =
	TOTSAL =
	TAXTOT =
	RETURN
;------------------------------------

ENDHDR,
	PLINE =					;;;10/5/92
	CALL PRINT				;;;10/5/92
	PLINE (93,112) = 'TOTAL SALES DOLLARS:'	;;;
	PLINE (114,124) = TOTORD, MASK		;;;
	CALL PRINT				;;;
	LINCNT = 62				;;;
	IF (ENDNA.NE.'[[[ ') GO TO PRNTOF
END2,
	IF (STXCTL.NE.1 .AND. LPONSW.EQ.0) XCALL MESAG
&	('NO ORDERS SELECTED FOR BILLING',2)
	IF (LPONSW.EQ.1) XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	LPONSW =
ENDOFF,
	XCALL WATE (3,V)
	CALL CLOSE
	XCALL PGCHN ('CP:BILLS',1)


PRNTOF,
	LINCNT = 60
	IF (LPONSW.EQ.0) XCALL MESAG
&	('NO RECORDS FIT THESE PARAMETERS',2)
	IF (LPONSW.EQ.1) XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	IF (LPONSW) LPONSW =
	PGCNT =
	GOTO BEGIN

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
;Create work file...
	WFORD = OORDNO,'XXXXXX'

;;;	XCALL ISAMC (WRKFIL, 145, 1, 'START=1, LENGTH=63, DUPS, ASCEND')
	XCALL ISAMC (WRKFIL, 145, 1, KEYSPEC)
	OPEN (33, SU, WRKFIL)
	CHNWRK = 33

	OFORD = OORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (OUTFIL, 145, 1, 'START=1, LENGTH=63, DUPS, ASCEND')
	XCALL ISAMC (OUTFIL, 145, 1, KEYSPEC)
	OPEN (34, SU, OUTFIL)
	CHNOUT = 34

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

	FIND (CHN045, ORDLIN, OORDNO) [ERR=MLOOP]
MLOOP,
	READS (CHN045, ORDLIN, EOF_M)
	IF (LINSEQ .NE. 0) GOTO EOF_M
	IF (LMSQ1 .GT. 0) 
		BEGIN
		F1_NUM = LMSQ1
		F1_MEMOS(LMSQ1) = M_SHORTD
		F1_MEMOL(LMSQ1) = M_LDESCR
		END
	IF (LMSQ2 .GT. 0) 
		BEGIN
		F2_NUM = LMSQ2
		F2_MEMOS(LMSQ2) = M_SHORTD
		F2_MEMOL(LMSQ2) = M_LDESCR
		END
	IF (LMSQ3 .GT. 0) 
		BEGIN
		F3_NUM = LMSQ3
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
	FIND (CHN045, ORDLIN, OORDNO) [ERR=LOOP]
LOOP,
	READS (CHN045, ORDLIN, EOF)
	IF (LORDNO .NE. OORDNO) GOTO EOF
	IF (LTYPE .EQ. 'M') GOTO LOOP
	IF (LQTYOR .EQ. 0) GOTO LOOP		;SSQ 3/24/00
	sav_lflag = lflag					;ssq 10-15-18
	sav_ltxflg = ltxflg					;ssq 10-15-18
;-
; sbt ---------------------------------------------------------------------------
	call check_sb_tees		;10-15-17
	if (.not. sbt) goto no_sbt
	f2_idx = 0
	for sbt_idx from 1 thru 6
		begin
		fv_data = funcv(sbt_item, sbt_code(sbt_idx), lf3)
		if (f_item .ne. blanks)
			begin
			call insert_f3
			xcall g_item (f_item, itmmas, ordlin, f_memos, sbt_mat, sbt_lmsq1, f2_idx, f3_idx, f_ga, sbt_qty, chn041, chn182)
			if (sbt_code(sbt_idx).eq.'C1 ') lqtyor = lqtyor*2
			if (sav_lflag.gt.0) lqtysh = lqtyor
			call load_line
			end
		end
	goto loop

no_sbt,
	call load_line			;10-11-17
	goto loop

EOF,
	RETURN
;---------------------------------------------------------
;-
load_line,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	WTYPE = 'L'
;;;	WDEPT = LDEPT
	WDEPT = LPRDCD(1,1)		;SSQ 6-21-00
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
	WQTYSH = LQTYSH
	LQTYBO=LQTYOR-LQTYSH	;SSQ 6-21-00
	WQTYBO = LQTYBO
	WUM = LUOFM
	WPRICE = LPRICE
	WCPFLG = LCPFLG
	WFLAG = sav_lflag
;;;	WFLAG = LFLAG
	WSTATS = LSTATS
	WTXFLG = sav_ltxflg
;;;	WTXFLG = LTXFLG	

	W_KEY = W_KEYA + W_KEYB
	IF(LITMNO.EQ.'M') CLEAR W_KEY(1,6)
	IF(LITMNO.EQ.'ME') W_KEY(1,1) = 'p'	;neg zero
	STORE (CHNWRK, LINE, W_KEY)

	return
;------------------------------------------------
;;;	GOTO LOOP



CONSOLIDATE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Consolidate line items for the
		;;; same part #
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_CON]
	SAVKEY = '***'
CONLOOP,
	READS(CHNWRK, LINE, EOF_CON)
;;;	IF (W_KEY .NE. SAVKEY)

	W_KEY = W_KEYA + W_KEYB
	IF (W_KEY.NE.SAVKEY .OR. WCPFLG)
	THEN	CALL NEWKEY
	ELSE	BEGIN
		INCR MULTLINE
		SAVQTY = SAVQTY + WQTY
		SAVSHP = SAVSHP + WQTYSH
		DELETE (CHNWRK)
		END
	GOTO CONLOOP

NEWKEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (SAVKEY .EQ. '***') GOTO OUTKEY
	IF (MULTLINE .EQ. 0) GOTO OUTKEY

	READ (CHNWRK, LINE, SAVKEY)
	W_KEY = W_KEYA + W_KEYB		;ssq 6-23-03
	WQTY = SAVQTY
	WQTYSH = SAVSHP
	WRITE (CHNWRK, LINE, SAVKEY)
OUTKEY,
	SAVKEY = W_KEY
	SAVQTY = WQTY
	SAVSHP = WQTYSH
	CLEAR MULTLINE
	RETURN
;-----------------------------------------------

EOF_CON,
	CALL NEWKEY
	RETURN
;-----------------------------------------------
;-----------------------------------------------
LOAD_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; LOAD DUCTWORK INTO WORKFILE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Create work file...
	WRORD = OORDNO,'XXXXXX'
	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (39, SU, DUCFIL)
	CHNDUC = 39
;Note: only the ductwork data is moved into the work file,
; it is assumed that all the acc info has already been moved
; to the ordlin file...

	FIND(CHN175,DUCACC,OORDNO) [ERR=LD_LOOP]
LD_LOOP,
	LOKCTL = 1
	XCALL IOS (CHN175,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.OORDNO) GOTO LD_EOF

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
;;;	CALL SUMDUC
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

SUMDUC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	CONFIG(1,3) = WGAUGE
;;;	IF (DUTYPE.EQ.2.OR.DUTYPE.EQ.3) CONFIG(1,3) = WTHICK
;;;	CONFIG(4,4) = WDUTYPE
;;;	CONFIG(5,5) = WCAT
;;;	CONFIG(6,6) = 10-WSTY
;;;	CONFIG(7,7) = WSEAM

		; Check to see if the configuration has all ready 
		; been on this order
;;;	I =
;;;	DO BEGIN
;;;	  INCR I
;;;	  DUCREC = SDUCRC(I)
;;;	END
;;;	UNTIL (DUCCFG.EQ.CONFIG.OR.DUCCFG.EQ.0.OR.I.GE.MAXDUC)
;;;	IF (I.GE.MAXDUC) GOTO BADCON

;;;	DUCCFG = CONFIG
;;;	DUCSQF = DUCSQF + WSQFEET
;;;	DUCPND = DUCPND + WPOUNDS
;;;	DUCGPR = WGPRICE
;;;	SDUCRC(I) = DUCREC

;;;	IF (WLINER.GE.1.AND.WLINER.LE.9.AND.WLINER.NE.4)
;;;&		LINSQF(WLINER) = LINSQF(WLINER) + WSQFLIN

	RETURN
;------------------------------------------------------------

WRTMEM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; ROUTINE TO FORMAT AND PRINT WO'S
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; print top notes...
	CLEAR IS_NOTE
	OC_ORD = OORDNO
	OC_SEQ = 0
	FIND(CHN075,ORDNOT,COMKEY) [ERR=NXTNOT]
NXTNOT,
	XCALL IOS (CHN075, ORDNOT, READ, LOKCTL)
	IF(LOKCTL .NE. 0) GOTO EOF_NNT
	IF(OC_ORD .NE. OORDNO) GOTO EOF_NNT
	IF(OC_SEQ .GE. 900) GOTO EOF_NNT
	PLINE(22,52) = OC_COM
	CALL PRINT
	IS_NOTE = 1
	GOTO NXTNOT
EOF_NNT,
	IF(IS_NOTE)
		BEGIN
		XCALL LINFD (1)
		INCR LINCNT
		END

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
	CLOSE CHNOUT
	CLOSE CHNWRK
	XCALL DELET (WRKFIL)
	XCALL DELET (OUTFIL)
	RETURN
;===============================================

DISPLA_DISC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR D_LIN1, D_LIN2, D_LIN3

	D_DAYS = 30
	CALL MAKE_D_DATE
	D_30_DATE = D_DATE

	USING DSCODE SELECT
	(0),	BEGIN
		D_LIN1 = ocomnt(1)
		D_LIN2 = ocomnt(2)
		END
	(1),	BEGIN
		D_DAYS = 15
		CALL MAKE_D_DATE
		IF (DSAMT .EQ. 0)
	;;;	THEN	D_AMT = (2*OSALE)#2
		THEN	D_AMT = (1*OSALE)#2
		ELSE	D_AMT = DSAMT

		D_LIN1 = "A discount of $"
		D_LIN1(16,23) = D_AMT, 'ZZZX.XX' [LEFT]
		DI = %TRIM(D_LIN1)
		D_LIN1(DI+1, 50) = " may be taken only if"
		
		D_LIN2 = "payment is received and in our office by"
		D_LIN2(42,49) = D_DATE, 'ZX/XX/XX' [LEFT]
		END
	(2),	BEGIN
		D_LIN1 = "Payment must be received in our office by"
		D_LIN1(43,50) = D_30_DATE, 'ZX/XX/XX' [LEFT]

		D_LIN2 = "for discounted delivered prices to be valid."
		END
	(3),	BEGIN
		D_LIN1 = "Payment must be received in our office by"
		D_LIN1(43,50) = D_30_DATE, 'ZX/XX/XX' [LEFT]

		D_LIN2 = "for discount to apply."
		END
	(4),	BEGIN
		D_LIN1 = "A discount of $"
		D_LIN1(16,23) = DSAMT, 'ZZZX.XX' [LEFT]
		DI = %TRIM(D_LIN1)
		D_LIN1(DI+1, 50) = " and the freight charges"

		D_LIN2 = "may be deducted only if this invoice"
		
		D_LIN3 = "is paid and in our office by"
		D_LIN3(30,37) = D_30_DATE, 'ZX/XX/XX' [LEFT]
		END
	(5),	BEGIN
		D_DAYS = 30
		CALL MAKE_D_DATE
		IF (DSAMT .EQ. 0)
	;;;	THEN	D_AMT = (2*OSALE)#2
		THEN	D_AMT = (1*OSALE)#2
		ELSE	D_AMT = DSAMT

		D_LIN1 = "A discount of $"
		D_LIN1(16,23) = D_AMT, 'ZZZX.XX' [LEFT]
		DI = %TRIM(D_LIN1)
		D_LIN1(DI+1, 50) = " may be taken only if"
		
		D_LIN2 = "payment is received and in our office by"
		D_LIN2(42,49) = D_DATE, 'ZX/XX/XX' [LEFT]
		END
		
	ENDUSING

	RETURN
;-------------------------------------------------------
MAKE_D_DATE,
	D_TRY=0
	WRK_DATE = TODAY
	DD = DD + D_DAYS
D_FEB,
	IF (DD .GT. MDAYS(MM))
	   BEGIN
	   DD = DD - MDAYS(MM)
	   MM = MM + 1
	   IF (MM .GT. 12)
	      BEGIN
	      YY = YY + 1
	      MM = 1
	      END
	   END
	INCR D_TRY
	IF(D_TRY.LE.1)GOTO D_FEB
	D_DATE = WRK_DATE
	RETURN
;-------------------------------------------------------

PRINT_HEADER,	;;;;;;;;;;;;;;;;;;;;;;;;;
	PAGENO = 1
	SHIPTO (1) = OSHPNM
	SHIPTO (2) = OSHAD1
	SHIPTO (3) = OSHAD2
	SHIPTO (4) = OSHAD3
	
	XCALL ISIO (chn001, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CUSMAS =
	
;;;	KEY = OCUSNO,'XXXXXX'
;;;	BSEND = SAV001
;;;	XCALL SERCH (2,CUSIDX,KEY,1,6,BSEND,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	IF (SRCCTL.EQ.1) 
;;;	THEN	CUSMAS =
;;;	ELSE	BEGIN
;;;		LOKCTL = 1
;;;		XCALL IO (1,CUSMAS,IRC001,READ,LOKCTL)
;;;		END

	USING OCUSNO SELECT
	(3,10,58455,999000),	BEGIN
			XCALL ISIO (CHN139, TMPCUS, OORDNO, READ, LOKCTL)
			IF (LOKCTL .EQ. 0)
				BEGIN
				NAME = T_NAME
				ADD1 = T_ADD1
				ADD2 = T_ADD2
				CITY = T_CITY
				STATE = T_STATE
				ZIP = T_ZIP
				END
			END
	ENDUSING

;;;	SOLDTO (1) = OCUSNM	;ssq 10-13-05
	SOLDTO (1) = NAME
	SOLDTO (2) = ADD1
	SOLDTO (3) = ADD2
	SOLDTO (88,102) = CITY
	SOLDTO (104,105) = STATE
	SOLDTO (107,116) = ZIP
	IF (OCUSNO.EQ.999999) 
		BEGIN
		OCUSNM = SHIPTO(1)
		SOLDTO(1) = SHIPTO(1)
		SOLDTO(2) = SHIPTO(2)
		SOLDTO(3) = SHIPTO(3)
		SOLDTO(4) = SHIPTO(4)
		SHIPTO(1,116) =
		END

	TOTSAL =
	DUCTOT =
	LINTOT =
	ACCTOT =
	CALL PRTHDR
	PBUF = SUBHD1
	CALL PRINT
	PBUF = SUBHD2
	CALL PRINT
	XCALL LINFD (1)
	INCR LINCNT
	RETURN
;----------------------------------

PRTHDR,
	PLINE (68,73) = OSHPTO, 'ZZZZZZ' [LEFT]
	CALL PRINT

	PLINE (1,8) = 'ORDER #:'
	PLINE (10,15) = OORDNO

	PLINE (20,27) = 'SOLD TO:'
	PLINE (59,66) = 'SHIP TO:'
	CALL BUFSLD
	CALL BUFSHP
	PLINE (29,57) = SOLDTO (1)
	PLINE (68,96) = SHIPTO (1)
	CALL PRINT
	PLINE (29,57) = SOLDTO (2)
	PLINE (68,96) = SHIPTO (2)
	CALL PRINT
	PLINE (29,57) = SOLDTO (3)
	PLINE (68,96) = SHIPTO (3)
	PLINE (1,11) = 'ORDER DATE:'

	XCALL DATE8(OORDDT, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE (13,22) = D_FMT

	PLINE (105,115) = 'SHIP DATE: '

	XCALL DATE8(OSHDAT, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE (116,125) = D_FMT
	CALL PRINT
	PLINE (29,57) = SOLDTO (4)
	PLINE (68,96) = SHIPTO (4)
	CALL PRINT
	XCALL LINFD (1)
	INCR LINCNT
	PLINE (1,8) = 'CUST NO:'
	PLINE (10,15) = OCUSNO
	PLINE (23,31) = 'ORDR/EST:'
	PLINE (33,34) = OLOC
	PLINE (44,52) = 'SHIP-VIA:'

	CALL GET_SCAC
	PLINE (54,68) = SC_NAME

	PLINE (74,82) = 'SALESMAN:'
	PLINE (84,85) = OSLMAN,'XX'
	PLINE (88,98) = 'ORDER DISC:'
	PLINE (100,101) = ODISC
	PLINE (102,102) = '%'
	IF (DSTFLG.NE.'Y') GO TO PRTHD1
	PLINE (106,116) = 'A/R ACCT-#:'
	PLINE (118,125) = OARACT, MASK2
PRTHD1,
	CALL PRINT
	XPOL1 = OPONO
	XPOL2 =
	IF (OMETRO .NE. BLANKS)
		BEGIN
		XPOL1 = OMETRO
		XPOL2 = OPONO
		END
	PLINE (1,6) = 'PO NO:'
;;;	PLINE (10,19) = OPONO
	PLINE (10,21) = XPOL1
	PLINE (23,29) = 'JOB NO:'
	PLINE (33,42) = OJOBNO
	PLINE (44,49) = 'TERMS:'

;;;	OPEN (10,I,ARTFIL)
	SRCCTL = 2
	BSEND = ORGART
	XCALL SERCH (10,ARTERM,OTERMS,1,1,BSEND,BSMID,SRCCTL,4,2,7,0,0,0,0)
;;;	CLOSE 10
	GO TO (TRMNFD), SRCCTL
	PLINE (54,68) = ARTRDS
	GO TO TERMOK
TRMNFD,
	ARTRDP = 
	PLINE (54,68) = 'TERM NOT FOUND '
TERMOK,
	PLINE (69,70) =
	PLINE (74,82) = 'PMT-TYPE:'
	IF (OCLPPD.EQ.'C') PLINE (84,91) = 'COLLECT'
	IF (OCLPPD.EQ.'P') PLINE (84,91) = 'PREPAID'
	CALL PRINT

	PLINE (10,21) = XPOL2
	CALL PRINT

	XCALL LINFD (1)
	LINCNT = LINCNT + 1
	RETURN
BUFSLD,
	IF (SOLDTO(2).NE.BLANKS) GOTO BUFSL2
	SOLDTO (2) = SOLDTO (3)
	SOLDTO (3) =
BUFSL2,
	IF (SOLDTO (3).NE.BLANKS) RETURN
	SOLDTO (3) = SOLDTO (4)
	SOLDTO (4) =
	RETURN
BUFSHP,
	IF (SHIPTO(2).NE.BLANKS) GOTO BUFSH2
	SHIPTO (2) = SHIPTO (3)
	SHIPTO (3) =
BUFSH2,
	IF (SHIPTO (3).NE.BLANKS) RETURN
	SHIPTO (3) = SHIPTO (4)
	SHIPTO (4) =
	RETURN
;----------------------------------------------
;==============================================


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1,'SI',41,SWITCH)		;FILE # 41 -- ITMMAS FILE
	IF (SWITCH .EQ. 9) RETURN
	chn041 = 1
	
;;;	LOKCTL = 1
;;;	XCALL IO (2,DUMINV,1,READ,LOKCTL)
	read (2, duminv, ^first) [err=no_hdr]
no_hdr,
	JUST = JSTIFY
	CLOSE 2


	SWITCH = 5
	XCALL FILES (5,'SI',45,SWITCH)		;FILE # 45 -- ORDLIN FILE
	IF (SWITCH.EQ.9) RETURN
	CHN045 = 5

	XCALL FFILE (44, REDFIL, CLCTL)		;OPEN ON SAME DEVICE AS ORDHDR
	IF (CLCTL) RETURN
	REDFIL(5,14) = 'BILEDT.WRK'
	OPEN (4, I, REDFIL)
						;TEMP ORDHDR FILE
	SWITCH = 1
	XCALL FILES (2,'SI',1,SWITCH)		;FILE # 01 -- CUSMAS FILE
	IF (SWITCH.EQ.9)RETURN
	CHN001 = 2

	SWITCH  = 5
	XCALL FILES (3,'I',60,SWITCH)
	IF (SWITCH.EQ.9) RETURN
	CHN060 = 3

	XCALL FILES (17,'SI',182,SWITCH)		;COPTBL
	IF (SWITCH.EQ.9) RETURN
	CHN182 = 17

	SWITCH = 1
	XCALL FILES (10,'I',170,SWITCH)		;FILE # 170 -- ARTERM
	IF (SWITCH.EQ.9) RETURN
	CHN170 = 10

	SWITCH = 5
	XCALL FILES (12,'SI',175,SWITCH)	;FILE #175 -- DUCACC
	IF (SWITCH.EQ.9) RETURN
	CHN175 = 12

	XCALL FILES (75,'SI',075,SWITCH)	;FILE #075 -- ORDCOM
	IF(SWITCH.EQ.9) RETURN
	CHN075 = 75

	XCALL FILES (49, 'SI', 139, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN139 = 49

;;;	LOKCTL = 1
;;;	XCALL IO (1,DUMCUS,1,READ,LOKCTL)
;;;	SAV001 = ORG001

	LOKCTL = 1
	XCALL IOS (3,COPCTL,READ,LOKCTL)

	LOKCTL = 1
	XCALL IO (10,ARTCTL,1,READ,LOKCTL)
	ORGART = ORG170

	OPNOK = 1
	RETURN
;------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SWITCH = 4
;;;	IF(CHN002) XCALL FILES (2,'I',02,SWITCH) ;FILE # 02 -- CUSIDX FILE

	IF(CHN045) CLOSE CHN045
	CLOSE 4

	IF(CHN001) XCALL FILES (2,'SI',1,SWITCH)		;FILE # 01 -- CUSMAS FILE

	IF(CHN060) CLOSE CHN060
	IF(CHN182) CLOSE CHN182
	IF(CHN170) XCALL FILES (10,'I',170,SWITCH)	;FILE # 170 -- ARTERM
	IF(CHN175) CLOSE CHN175
	IF(CHN075) CLOSE CHN075
	if(chn041) close chn041
	if(chn182) close chn182
	RETURN
;------------------------------------------
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
INSERT_F3,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF(F3_NUM .GE. F_MAX) goto not_key	;too many

; check to see if key is already in array...

	clear mm_code
	for j from 1 thru 5 if (f_f3(j).ne.0) mm_code(j,j) = f_f3(j), 'X'

	for j from 1 thru f3_num
		begin
		xmcod = f3_key(j),	'ZZZZZ' [left]
	   	if (xmcod .eq. mm_code)
		  begin
		  f3_idx = j		;index to existing key
		  return		;key already in array
		  end
		end

	CLEAR TBL_KEY
	TBLCOD = 'M3'
	MM_KEY = MM_CODE
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_KEY]

	INCR F3_NUM
	F3_IDX = F3_NUM
	F3_MEMOL(F3_IDX) = MM_LONG
	F3_MEMOS(F3_IDX) =  MM_SHORT		
	F3_KEY(F3_IDX) = MM_KEY
	RETURN

not_key,
	;something
	return
;---------------------------------------------------------

check_sb_tees,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; is this item a spiral body tee?
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	sbt = 0
	using litmno select
	('BN'),				NOP		;BULLNOSE TEE
	('C9','C4','CV9','CV4'),	NOP	;CROSS
	('CT9','CC9','CTV','CCV'),	NOP	;CONICAL TEE/CROSS
	('T4','T9','TV4','TV9'),	NOP	;TEE
	('STC','STT'),			NOP
	(),				RETURN	; anything else
	endusing

	if (lf2 .eq. 31) return		;custom f2 notes 11-26-18
	
	partno = lcfgim
	if (ldampr) call damper

	xcall cfg2(partno,segs)
	if (ma .gt. 60) return		;1-3-19

	if (ma.gt.36 .and. (bc.ge.20 .or. bd.ge.20) )goto sb_tee
	return
sb_tee,
	sbt_item = lcfgim
	sbt_qty = lqtyor
	sbt_mat = lmat
	sbt_lmsq1 = lmsq1

	sbt = 1
	return
;----------------------------------------------------
damper,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ln = %trim (partno)
	for j from 1 thru 3
		begin
		xcall instr(1, partno, '*', fl)
		if (.not. fl)exitloop
		partno(fl, ln) = partno(fl+1, ln)
		end
	return
;----------------------------------------------------

END
