;  INVOIC / COP isam
;
;		INVOICES PRINT 
;

; 11-1-05 SSQ: use today's date for computing discount date...
; 11-08-11 SSQ: don't use OSALE instead of TOTSAL.  OSALE is computed
;	in BILLS and seems to be always correct.  TOTSAL is computed here
;	and for some reason is occasionally wrong - including times when
;	the last line of the invoice is duplicated.  I'm hoping this might
;	at least partially solve the problem.

;	5-29-18 ssq: make cusmas isam
;	10-08-18 ssq: sbt pricing
;	12-16-19 Ssq: cash_customer
;	02-13-20 ssq: email invoices for CC orders (including cash customers)


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

.DEFINE MID_LINES,	51
.DEFINE END_LINES,	44

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.INCLUDE 'DEF:HPSUB.DEF'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

record	email	;to save invoice email info
	.include 'def:cemail.def'

RECORD	M_FILE
	M_CMP	,A3		;COMPANY CODE
		,A2,	':M'
	M_DT	,A12		;YYYYMMDDHHMM

RECORD	EMFILE
	EF_FILE	,A17
	EF_PROC	,D1		;1 = PROCESSED

RECORD	TMPCUS
	.INCLUDE 'DEF:RD139A.DEF'

RECORD	COINFO
	.INCLUDE 'DEF:COINFO.DEF'

RECORD	ORDNOT
	.INCLUDE 'DEF:RD075A.DEF'

RECORD CUSMAS
		.INCLUDE 'DEF:RD001A.DEF'
RECORD DUMCUS  ,X	
		.INCLUDE 'DEF:RD001B.DEF'
;;;RECORD CUSIDX   	
;;;		.INCLUDE 'DEF:RD002A.DEF'

RECORD	SALMAN
		.INCLUDE 'DEF:RD054A.DEF'

RECORD ORDHDR   	
		.INCLUDE 'DEF:RD044A.DEF'
RECORD ORDLIN
		.INCLUDE 'DEF:RD045A.def'
RECORD		,X
		.INCLUDE 'DEF:RD045M.def'
RECORD		,X		
		.INCLUDE 'DEF:RD045D.def'
		
RECORD	SAVLIN			;FOR SBT
		.INCLUDE 'DEF:RD045S.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD ARTERM
		.INCLUDE 'DEF:RD170A.DEF'
RECORD ARTCTL	,X
		.INCLUDE 'DEF:RD170B.DEF'

RECORD PBUF
	PLINE	,A150

RECORD PARAM
		.INCLUDE 'DEF:PARAM2.DEF'

GLOBAL DUCK,INIT
		.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;page eject

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

	
RECORD	F_MEMOS
	.INCLUDE 'DEF:CPMEMO.DEF'

RECORD
	MAXDUC	,D3,150
	QTY	,D5
	ITM	,A15
	DES	,A30
	PRC	,D7
	COMPA	,A15
	COMPB	,A15

RECORD	ARRAYS
	SDUCRC	,150A28	;SUMMARIZED DUCT CONFIGURATIONS   (SEE DUCREC BELOW)
	LINSQF	,9D6	;SUMMARIZE LINER SQUARE FEET

RECORD DUCREC
	DUCCFG	,D7	;CONFIGURATION CODE
	DUCSQF	,D7	;SQ FEET OF MATERIAL
	DUCPND	,D7	;POUNDS OF MATERIAL
	DUCGPR	,D7	;GAUGE PRICE

RECORD ACCREC
	.INCLUDE 'DEF:ACCREC.DEF'

;---------------------------
RECORD	FOR_ISAMC
	KEYSPEC,	A*,	'START=1:116, LEN=55:8, DUPS, ASCEND'

RECORD	LINE			
	WSEQ1	,D2		;reversed
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
		,A2
		,D1
RECORD	,X
		,D6
		,A2
		,A2
		,A15
	M_LONG	,A63
		,A27
		,D8
		,A1
		,D5
		,D5
		,A2
		,D1
RECORD,X
;;;	W_KEY	,A63
	W_KEYA	,A55		;FIRST PART OF KEY
		,A60
	W_KEYB	,A8		;PRICE
RECORD,X
		,D2
	COMSEQ	,D4
		,A2		;reversed
;---------------------------
RECORD	LINE2			
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
	WQTYSH2	,D5
	WUM2	,A2
	WCPFLGW	,D1
RECORD,X
;;;	W_KEY2	,A63
	W_KEY2A	,A55		;FIRST PART OF KEY
		,A60
	W_KEY2B	,A8		;PRICE

RECORD,X
		,D2
	COMSEQ2	,D4
		,A2		;not reversed
;---------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RECORD	TOP_MARGIN
	TM_ESC	,A1
		,A4,	'&l0E'

RECORD	LEFT_MARGIN
	LM_ESC	,A1
		,A4,	'&a0L'

RECORD	PAGE_OF
		,A*,	' PAGE '
	A_PAGOF	,A3
		,A4,	' OF '
	A_TPAGE	,A3

RECORD	WRK_DATE
	CC	,D2
	YY	,D2
	MM	,D2
	DD	,D2

RECORD
	MDAYS,	12D2,	31,28,31,30,31,30,31,31,30,31,30,31

RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN041	,D2
	CHN044	,D2
	CHN045	,D2
	CHN054	,D2
	CHN068	,D2
	CHN075	,D2
	CHN139	,D2
	CHN170	,D2
	CHN175	,D2
	CHN182	,D2
	CHNWRK	,D2
	CHNOUT	,D2
	CHMAIL	,D2

RECORD	VARSX
	D_DAYS	,D2
	D_TRY	,D1
	D_DATE	,D6
	D_30_DATE	,D6
	D_AMT	,D6
	D_LIN1	,A50
	D_LIN2	,A50
	D_LIN3	,A50
	DI	,D2

RECORD	
	TM2	,A90
	TM3	,A90
RECORD,X
	TM2A	,3A30
	TM3A	,3A30

record	dup_sale
	ds_ordno	,d6
	ds_loc		,a1
	ds_osale	,a12
			,a1
	ds_totsal	,a12

RECORD	NEWVARS
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
	a2	,a2
	sr_qty	,d5	;not by dept
	sr_dlr	,d8	;not by dept
	dup_tries	,d6
	chn_dup	,d3
	filen	,a14
	CMPCOD	,A3
	P1_CNT	,D6	;# OF LINES ON PAGE 1 IF MULTI-PAGE
	P2_CNT	,D6	;# OF LINES ON REMAINING PAGES
	PX_CNT	,D6	;# OF LINES ON PAGE 1 IF ONLY 1 PAGE
	REM	,D6
	D_REM	,D6
	X_REM	,D6	; WORKING VAR
	T_LINES	,D6

	L_CNT	,D6		;# LINES
	D_CNT	,D6		;# DUCT RECORDS
	S_CNT	,D6		;SUMMARY LINES TO PRINT
	LNAM	,A25
	SNAM	,A12
	SINT	,A3
	HDR_LINES	,D2
	IS_NOTE	,D1
	LP_ON	,D1
	SZ1	,D3
	SZ2	,D3
	J	,D5
	MULTLINE	,D4
	MEMO_ON	,D1
	BLANKS	,A30
	BLANK90	,A90
;;;	SAVLIN	,A150
	TMPDSC	,A62
	DASHES	,A30,	'------------------------------'
	LL	,D2
	TL	,D2
	SEQNO	,D2
	SAV_MSEQ	,D6
	SAVSEQ	,D2
	SAVSQ1	,D2
	SAVSQ2	,D2
	SAVSQ3	,D2
	SAVDPT	,A2
	SAVCOM	,D4
	W_KEY	,A63
	W_KEY2	,A63
	SAVKEY	,A63
	SAVQTY	,D5
	SAVSHP	,D5

record
	remit1	,a*,	'     * Please Remit All Payments To:'
	remit2	,a*,	'5850 Main Street NE, Minneapolis, MN 55432'

RECORD	VARS
	I	,D6	;SSQ 5-18-06 ("I" was part of rd175d.def record)
	IS_PDC	,D1	;IS PriceDuct.Com
	XPOL1	,A12
	XPOL2	,A12
	OPNOK	,D1
	PAGOF	,D3
	TPAGE	,D3
	DL_AMT	,D6
	DL_DATE	,D6
	DL1	,A62
	DL2	,A62
	IS_CMEMO	,D1
	SPLFIL	,A14

RECORD
	STRTNA	,A6
	ENDNA	,A6
RECORD	,X
	STRTNO	,D6
	ENDNO	,D6

RECORD	VARS2
	VL	,D3
	LROW	,D3
;;;	TOTAL_LINES	,D4
	FIRST_MAX	,D2
	PRINTED_LINES	,D4
	SUM_ACC		,D4
	SUM_DUC		,D4
	NUM_LNR		,D4
	NUM_MID	,D3
	SAVORD	,D6
	CONFIG	,D7	
	LFEED	,D2	
	ACCUOM	,A2	
	ORGSHV	,D5	
	ORGART	,D5	
	MAXLIN	,D2	
	N	,D2
	ADCNT2	,D4
	DECMAL	,D10
	LINCNT	,D2,	00
	X	,D2
	PGCNT	,D3,	000
	SWITCH	,D1
	ERRRET	,D1
	MASK	,A12,'ZZZZ,ZZZ.XX-'
	MASK1	,A6,'ZZZZX-'
	MASK2	,A6,'ZXXXXX'
	MASK3	,A8,'XX/XX/XX'
	MASK4	,A9,'ZZZZZ.XXX'
	SAV001	,D5
	KEY   	,D6
	BSEND	,D5
	BSND2	,D5
	BSMID	,D5
	SRCCTL	,D1
	ENTRY	,A8	;ENTRY MUST BE DEFINED AS A MINIMUM OF 8 CHARACTERS
	INXCTL  ,D1
	CNGCTL	,D1
	WHATNO	,D1
	RPTNUM	,D3
	PRTTYP	,A1
	TAX	,D6
	CTR	,D2
	TRMS	,D4
	EXEPRC	,D8
	SAVINV	,D6
	INVDAT	,D8	;DATE8
	INVDAA	,A10
	TDATE	,D6
	TODAY	,D8
	TOTSAL	,D8
	STXCTL	,D1
;;;	BLANKS	,A15
	V	,D1
	READ	,D1	,0
	WRITE	,D1	,1
	LOKCTL	,D1
	LPSW	,D1
	INIT	,20A3,	'JJM','PDS','LAM','DEG','JTH',
&			'DLH','JIM','SJM','CJM','BIL',
&			'MAW','PAT','BJZ','JPV','MJM',
&			'JWS','GJK','   ','   ','   '
	RESTRT	,D1,	0
	REDFIL	,A14
	CLCTL	,D1
PROC
	cmhpLPI = 12	;ssq 12/16/02 vline prob

	XCALL TERID (V)
	XCALL RDATE (TDATE)
	XCALL DATE8(TDATE, D_OUT, TODAY, INVDAA, D_SW)

	XCALL WHO(CMPCOD)

	XCALL OUTPT (1,1,2,'PRINT INVOICES',V)
	XCALL WATE (4,V)
	CALL OPENS
	IF (.NOT. OPNOK) 
		BEGIN
		CALL CLOSE
		GOTO ENDOFF
		END

	call read_liner
DATE,
	XCALL OUTPT (6,20,2,'PLEASE ENTER INVOICE DATE:',V)
	XCALL INPUT (06,48,08,00,'D ',ENTRY,INXCTL,V)
	GO TO (DATE), INXCTL
	INVDAT = ENTRY (1,8)
	IF (ENTRY.EQ.BLANKS) INVDAT = TODAY
	XCALL DATE8(INVDAT, D_OUT, D_OUTR, INVDAA, D_SW)
	XCALL OUTPT (6,48,0,INVDAA,V)

	CNGCTL = 2
	XCALL ANYCN (CNGCTL,WHATNO)
	GO TO (DATE), CNGCTL
BEGIN,
	STRTNA =
	ENDNA =
	XCALL OUTPT (08,20,0,'PRINT WHOLE BATCH <Y> ',1)
	XCALL INPUT (08,42,01,00,'AE',ENTRY,INXCTL,1)
	GOTO (BEGIN,END2), INXCTL
	IF (ENTRY.EQ.BLANKS.OR.ENTRY.EQ.'Y') 
	BEGIN
	  XCALL OUTPT (08,42,0,'Y',1)
	  STRTNO = 0
	  ENDNO = 999999
	  RESTRT = 0
	  GOTO OPENLP
	END
	XCALL OUTPT (10,20,0,'REPRINT ONE ORDER: ',1)
	XCALL INPUT (10,40,06,00,'#E',ENTRY,INXCTL,1)
	GOTO (BEGIN,END2), INXCTL
	IF (ENTRY.EQ.BLANKS) 
	THEN XCALL OUTPT (10,40,0,'NO    ',1)
	ELSE BEGIN
	  STRTNO = ENTRY
	  ENDNO = ENTRY
	  RESTRT = 1		;PRINT ONE ORDER ONLY
	  GOTO OPENLP
	END
	XCALL OUTPT (12,20,0,'REPRINT TO END OF BATCH STARTING WITH ORDER: ',1)
	XCALL INPUT (12,65,06,00,'#E',ENTRY,INXCTL,1)
	GOTO (BEGIN,END2), INXCTL
	IF (ENTRY.EQ.BLANKS) 
	THEN XCALL OUTPT (12,65,0,'NO    ',1)
	ELSE BEGIN
	  STRTNO = ENTRY
	  ENDNO = 999999
	  RESTRT = 2		;PRINT REST OF BATCH STARTING WITH XXXXXX
	  GOTO OPENLP
	END
	GOTO BEGIN

OPENLP,
	LPSW = 5			;SPECIAL FORMS PRINTOUT

;---------------------------
	XCALL HPON (LPSW,SPLFIL)
	XCALL HP (14,hpDOTS, 0)
	XCALL HP (14,hpTHICK,3)
	XCALL ASCII (27,TM_ESC)
	LM_ESC = TM_ESC
	E_CHAR = TM_ESC
;---------------------------

	IF (LPSW.EQ.0) XCALL PGCHN ('CP:UNPRBL',1)
	LOKCTL = 1
	XCALL IO (7,ORDHDR,1,READ,LOKCTL)
	IF (RESTRT.EQ.0) GOTO RDHDR
	IF (RESTRT.EQ.1) 
	BEGIN
	  OORDNO = STRTNO
	  GOTO RDHD2
	END
FIND,
	LOKCTL = 1
	XCALL IOS (7,ORDHDR,READ,LOKCTL)
	IF (ORDHDR.EQ.']]]]]]]]]]')
	BEGIN
	  XCALL MESAG ('CANNOT FIND STARTING ORDER NUMBER',1)
	  GOTO BEGIN
	END
	IF (OORDNO.EQ.STRTNO) GOTO RDHD2
	GOTO FIND
RDHDR,
	LOKCTL = 1
	XCALL IOS (7,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.NE.0) GOTO ENDHDR
	IF (ORDHDR.EQ.']]]]]]]]]]') GOTO ENDHDR
	IF (RESTRT.EQ.1) GOTO ENDHDR
RDHD2,
	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GO TO ENDHDR
	IF (OFLAG.NE.1.AND.OFLAG.NE.2) GO TO RDHDR
	
; 2-31-20 we are now mailing cc invoices per jess
;;;	IF (OCCRD .NE. ' ') GOTO SKIP_EMAIL	;ALWAY PRINT CC ORDERS
;---------------------------------------------------------------------
; 01-12-11: read cusmas to see if we should print or email...

	XCALL ISIO (chn001, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CUSMAS =

	if (omicus .ne. 0)
		begin
		call write_email
		goto rdhdr		;don't print invoice
		end
	
;;;	KEY = OCUSNO
;;;	BSEND = SAV001
;;;	XCALL SERCH (2,CUSIDX,KEY,1,6,BSEND,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	IF (SRCCTL.EQ.1 .OR. IRC001.LE.0) 
;;;	THEN	CUSMAS =
;;;	ELSE	XCALL IO (1,CUSMAS,IRC001,READ,LOKCTL)

	USING CEMAIL SELECT
	(1),	BEGIN
		CALL WRITE_EMAIL
		GOTO RDHDR		;don't print invoice
		END
	(2),	BEGIN
		CALL WRITE_EMAIL	;write email and print invoice
		LOKCTL = 1
		XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)	;6-22-11
		END
	ENDUSING

SKIP_EMAIL,
;---------------------------------------------------------------------
	CLEAR IS_PDC		;SSQ 3-1-05
	MEMO_ON = 1
	CALL LOAD_MEMOS
	CALL LOAD_WORK
	CALL CONSOLIDATE
;---------------------------------------------------------------------
;.....................................
;;; Add memos to output file...
	CLEAR SAVCOM, SAVSQ1, SAVDPT

	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_BM]

BM_LOOP,
	XCALL IOS (CHNWRK, LINE, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF_BM
;;;	READS (CHNWRK, LINE, EOF_BM)
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
;;;	CLOSE CHNWRK

; beginning of print stuff, make two passes thru the logic,
; one to count pages, two to do the actual printing...

;;;	lp_on = 0	;don't print
	lp_on = 1	;new method, not making 2 passes..
	PAGOF = 0
	CLEAR PRINTED_LINES
START_PRINT,
	CALL COUNT_LINES

	CLEAR PRINTED_LINES
	PAGOF = 0

	CALL PRTHDR

	CALL WRTMEM		;INTERNAL ROUTINE TO WRITE WORKSHEETS
	CALL GETDUC		;PROCESS DUCTWORK
	CALL ORDTOT		;ORDER TOTALS
	INCR LP_ON
	IF(LP_ON .LE. 1) GOTO START_PRINT

	CLOSE CHNOUT
	CLOSE CHNWRK
	XCALL DELET (WRKFIL)
	XCALL DELET (OUTFIL)

	GOTO RDHDR		;NEXT ORDER

;---------------------------------------------------------------------
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

; 8-17-11: compare totals here to see if there
; is going to be a problem...

; first, get a total for the ductwork...

	dup_tries = 0		;re-load once if osale .ne. totsal
re_load,
	clear totsal

	find (6, ducacc, oordno) [err=add_duc]
add_duc,
	lokctl = 1
	xcall ios (6, ducacc, read, lokctl)
	if(lokctl.ne.0 .or. ductor.ne.oordno) goto ad_eof
	DECMAL = ( POUNDS * GPRICE ) # 1
	TOTSAL = TOTSAL + DECMAL		;DUCT
	DECMAL = ( LINPRC * SQFLIN ) # 1
	TOTSAL = TOTSAL + DECMAL		;LINER
	goto add_duc
ad_eof,

;-------------------------------------------------------

;---------------------------------------------------------------
;Create work file...
	WFORD = OORDNO,'XXXXXX'

	XCALL ISAMC (WRKFIL, 137, 1, KEYSPEC)
	OPEN (33, SU, WRKFIL)
	CHNWRK = 33

;Create output file...
	OFORD = OORDNO,'XXXXXX'
	XCALL ISAMC (OUTFIL, 137, 1, KEYSPEC)
	OPEN (34, SU, OUTFIL)
	CHNOUT = 34
;---------------------------------------------------------------
;;;	clear sr_dlr, sr_qty

	FIND (CHN045, ORDLIN, OORDNO) [ERR=LOOP]
LOOP,
	XCALL IOS (CHN045, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (LORDNO .NE. OORDNO) GOTO EOF
	IF (LTYPE .EQ. 'M') GOTO LOOP
	IF (LDESCR.EQ.']]]CANCEL') GOTO LOOP
	IF (LFLAG.EQ.0) GOTO LOOP
	IF (LQTYOR .EQ. 0) GOTO LOOP		;SSQ 3/24/00
	sav_lflag = lflag					;ssq 10-15-18
	sav_ltxflg = ltxflg					;ssq 10-15-18
	
	;moved here 10-15-18
; mark as if line item was printed on invoic...
	LFLAG = 2
	LOKCTL = 1
	XCALL ISIO (CHN045,ORDLIN,ORDKEY,WRITE,LOKCTL)
;;-
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
			xcall g_item (f_item, itmmas, ordlin, f_memos, sbt_mat, sbt_lmsq1, f2_idx, f3_idx, f_ga, sbt_qty,chn041,chn182)
			sbt_price = sbt_price + lprice
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
load_line,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;now a routine for sbt
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			
	WTYPE = 'L'
;;;	WDEPT = LDEPT
	WDEPT = LPRDCD(1,1)		;SSQ 6-21-00
	WSEQ1 = LMSQ1
	WSEQ2 = LMSQ2
	WSEQ3 = LMSQ3
	WSQNO = LSRTSQ
	WITEM = LITMNO
	using witem select
	('JEB','JEF','JJG','JTG'), WITEM = LITMNO(2,15)	;SKIP THE "J"
	endusing
	
	WDESC = LDESCR
	IF (LDAMPR) WITEM(10,15) = 'Damper'
	WQTY = LQTYOR
	WQTYSH = LQTYSH
	WUM = LUOFM
	WPRICE = LPRICE
	WCPFLG = LCPFLG

;
	W_KEY = W_KEYA + W_KEYB
	STORE (CHNWRK, LINE, W_KEY)


	totsal = totsal + (wqtysh*wprice)#1

s_line,
;;;; mark as if line item was printed on invoic...
;;;	LFLAG = 2
;;;	LOKCTL = 1
;;;	XCALL ISIO (CHN045,ORDLIN,ORDKEY,WRITE,LOKCTL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3-24-05 ssq: decided not to do this...
;;;	IF (LITMNO(1,4) .EQ. 'PDCS') IS_PDC = 1	;SSQ 3-1-05
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	return
;;;	GOTO LOOP
;------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 9-26-11: need to re-test all this
cc_dup,
;;;	if (totsal .ne. osale)
;;;		begin
;;;		ds_loc = 'W'			;for load work
;;;		call dup_sale
;;;		incr dup_tries
;;;		if(dup_tries .le. 1) 
;;;			begin
;;;			CLOSE CHNOUT
;;;			CLOSE CHNWRK
;;;			XCALL DELET (WRKFIL)
;;;			XCALL DELET (OUTFIL)
;;;			goto re_load	;start over
;;;			end
;;;		end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	PLINE(35,64) = OC_COM
	CALL PRINT
	IS_NOTE = 1
	GOTO NXTNOT
EOF_NNT,
	IF(IS_NOTE)
		BEGIN
		CLEAR PLINE
		CALL PRINT
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
		IF(LP_ON)XCALL HP (14,hpFONT,hpBOLD)
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
		IF(LP_ON)XCALL HP (14, hpFONT,hpMEDIUM)
		CALL PRTLIN
		END

	GOTO W_LOOP
;====================================================

PRTLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR PLINE
	IF (LINCNT.GE.MAXLIN) CALL NXTPAG
	PLINE (18,32) = WITEM

	IF (WITEM.EQ.'???') PLINE (18,32) = '***'
	IF (WITEM.EQ.'M              ') PLINE (18,32) =
	PLINE (35,64) = WDESC
	IF ( WITEM.NE.'M              ' )
	BEGIN
	  PLINE (1,7) = WQTY, 'ZZ,ZZX-'
	  PLINE (9,17) = WQTYSH, 'ZZZ,ZZX-'
	  PLINE (69,77) = WPRICE,	'Z,ZZZ.XXX'
	  PLINE (80,81) = WUM
	  EXEPRC = (WQTYSH*WPRICE)#1
	  PLINE (85,94) = EXEPRC, MASK
	  TOTSAL = TOTSAL + EXEPRC
	END
	CALL PRINT
	RETURN
;-----------------------------------------------------------


MAIN_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;
	TL = %TRIM(TMPDSC)
	REM = 59 - TL
	REM = REM/2
	IF (REM.GT.0)
		BEGIN
		CLEAR TMPDSC
		TMPDSC(1,REM) = DASHES
		TMPDSC(REM+1,REM+TL) = MMEMO(J)
		TMPDSC(REM+1+TL,59) = DASHES
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

PRTHDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	ODSCAL =

	XCALL ISIO (chn001, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CUSMAS =
	
	
;;;	KEY = OCUSNO
;;;	BSEND = SAV001
;;;	XCALL SERCH (2,CUSIDX,KEY,1,6,BSEND,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	IF (SRCCTL.EQ.1 .OR. IRC001.LE.0) 
;;;	THEN	CUSMAS =
;;;	ELSE	XCALL IO (1,CUSMAS,IRC001,READ,LOKCTL)

;---------------------------
	USING OCUSNO SELECT
	(3,10,58455,999000),	BEGIN
			READ (CHN139, TMPCUS, OORDNO) [ERR=NO_TMPCUS]
			NAME = T_NAME
			ADD1 = T_ADD1
			ADD2 = T_ADD2
			CITY = T_CITY
			STATE = T_STATE
			ZIP = T_ZIP
			END
	ENDUSING

NO_TMPCUS,
;---------------------------


	WRITES (14, TOP_MARGIN)
	WRITES (14, LEFT_MARGIN)

	LINCNT =
	TOTSAL =

;try the logo...
;;;	IF(LP_ON)XCALL HP (14,hpGRAPHIC,'SMC.HP', 100)

	IF (IS_PDC)		;SSQ 3-1-05
	THEN	XCALL OECO(CHN182,COINFO,'PDC')
	ELSE	XCALL OECO(CHN182,COINFO)

;company name & address...
	IF(LP_ON)XCALL HP (14,hpFONT,hp8CPI+hpBOLD)
	IF (OSALE+OMISC+OTAX(1)+OTAX(2)+OTAX(3)+OFRGHT.LT.0)
	THEN	BEGIN
		IS_CMEMO = 1
		IF(LP_ON)XCALL HP (14,hpPOS,2,24,0, "C R E D I T   M E M O")
		END
	ELSE	BEGIN
		IS_CMEMO = 0
		IF(LP_ON)XCALL HP (14,hpPOS,2,24,0, "I N V O I C E")
		END

	IF(LP_ON)XCALL HP (14, hpPOS,4,1,0, C_NAME)
;;;	IF(LP_ON)XCALL HP (14, hpPOS,2,1,0, "SHEET METAL")
;;;	IF(LP_ON)XCALL HP (14, hpPOS,3,1,0, "CONNECTORS, INC.")

	IF(LP_ON)XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	

	IF(LP_ON)XCALL HP (14, hpPOS,5,1,0, C_ADD1)
;;;	IF(LP_ON)XCALL HP (14, hpPOS,4,1,0, "5850 MAIN STREET N. E.")

	IF(LP_ON)XCALL HP (14, hpPOS,6,1,0, C_ADD2)
;;;	IF(LP_ON)XCALL HP (14, hpPOS,5,1,0, "MINNEAPOLIS,   MN 55432")
	IF(LP_ON)XCALL HP (14,hpFONT,hpBOLD)

	PLINE = %STRING(C_LOC,'XXX-XXX-XXXX') + '  ' + %STRING(C_LD,'XXX-XXX-XXXX')
	IF(LP_ON)XCALL HP (14, hpPOS,7,1,0, PLINE(1,%TRIM(PLINE)))
;;;	IF(LP_ON)XCALL HP (14, hpPOS,6,1,0, "763-572-0000  800-328-1966")
	IF(LP_ON)XCALL HP (14, hpFONT,hpMEDIUM)
	IF(LP_ON)XCALL HP (14,hpFONT,hpBOLD)

	PLINE = 'FAX:' + %STRING(C_FAX,'XXX-XXX-XXXX')
	IF(LP_ON)XCALL HP (14, hpPOS,8,1,0, PLINE(1,%TRIM(PLINE)))
;;;	IF(LP_ON)XCALL HP (14, hpPOS,7,1,0, "FAX:763-572-1100")

	IF(LP_ON)XCALL HP (14,hpFONT,hp12CPI+hpBOLD)

	IF(LP_ON)XCALL HP (14, hpPOS,2,78,0,' INVOICE NO   ')
	IF(LP_ON)XCALL HP (14, hpPOS,5,78,0,' INVOICE DATE ')

	INCR PAGOF

	A_PAGOF = PAGOF,'ZZX'
	A_TPAGE = TPAGE,'ZZX' [LEFT]
	IF(LP_ON)XCALL HP (14,hpPOS,8,78,0,PAGE_OF)

	ENTRY(1,6) = OCUSNO,	'ZZZZZX' [LEFT]
	IF(LP_ON)XCALL HP (14,hpPOS,12,8,0,'Acct #')
	IF(LP_ON)XCALL HP (14,hpPOS,12,15,0,ENTRY(1,6))

	IF(LP_ON)XCALL HP (14,hpPOS,13,1,0,'SOLD')
	IF(LP_ON)XCALL HP (14,hpPOS,13,51,0,'SHIP')
	IF(LP_ON)XCALL HP (14,hpPOS,14,2,0,'TO')
	IF(LP_ON)XCALL HP (14,hpPOS,14,52,0,'TO')
		
	IF(LP_ON)XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)
	IF(LP_ON)XCALL HP (14, hpPOS,6,66,0,INVDAA)	;invoice date

	ENTRY(1,6) = OORDNO,	'ZZZZZX'
	IF(LP_ON)XCALL HP (14, hpPOS,3,67,0,ENTRY(1,6))	;invoic #

	IF(LP_ON)XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)

;;;	IF(LP_ON)XCALL HP (14,hpPOS,13,8,0,OCUSNM)
	IF(LP_ON)XCALL HP (14,hpPOS,13,8,0,NAME)
	IF(LP_ON)XCALL HP (14,hpPOS,14,8,0,ADD1)
	IF(LP_ON)XCALL HP (14,hpPOS,15,8,0,ADD2)
	CLEAR PLINE
	PLINE(1,15) = CITY
	PLINE(17,18) = STATE
	PLINE(20,29) = ZIP
	IF(LP_ON)XCALL HP (14,hpPOS,16,8,0,PLINE(1,30))

	IF(LP_ON)XCALL HP (14,hpPOS,13,58,0,OSHPNM)
	IF(LP_ON)XCALL HP (14,hpPOS,14,58,0,OSHAD1)
	IF(LP_ON)XCALL HP (14,hpPOS,15,58,0,OSHAD2)
	IF(LP_ON)XCALL HP (14,hpPOS,16,58,0,OSHAD3)

	IF(LP_ON)XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)
	IF(LP_ON)XCALL HP (14, hpPOS,17,0, hpHLINE,164)	;
	IF(LP_ON)XCALL HP (14, hpPOS,20,0, hpHLINE,164)	;

	

	IF(LP_ON)XCALL HP (14,hpFONT,hp12CPI+hpBOLD)
;first header...
;removed per jess 1-1-12...
;;;	PLINE = '                                          * F.O.B. Destination *'
;;;	IF(LP_ON)XCALL HP (14,hpPOS,18,0,0,PLINE(1,%TRIM(PLINE)) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	PLINE = 'Salesperson     P.O. Number     Job Number        Ship Via
&         Date Shipped    Terms'
	IF(LP_ON)XCALL HP (14,hpPOS,19,0,0,PLINE(1,%TRIM(PLINE)) )

	IF(LP_ON)XCALL HP (14, hpPOS,20,13, hpVLINE,4)	
	IF(LP_ON)XCALL HP (14, hpPOS,20,31, hpVLINE,4)	
	IF(LP_ON)XCALL HP (14, hpPOS,20,43, hpVLINE,4)	
	IF(LP_ON)XCALL HP (14, hpPOS,20,67, hpVLINE,4)	
	IF(LP_ON)XCALL HP (14, hpPOS,20,79, hpVLINE,4)	

	
	IF(LP_ON)XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	CLEAR PLINE

	XCALL SREP(OSLMAN,LNAM,SNAM,SINT)
	PLINE(1,12) = SNAM

	XPOL1 = OPONO
	XPOL2 =
	IF (OMETRO.NE.BLANKS)
		BEGIN
		XPOL1 = OMETRO
		XPOL2 = OPONO
		END
;;;	PLINE (17,26) = OPONO
	PLINE (17,28) = XPOL1
	PLINE (33,42) = OJOBNO

	CALL GET_SCAC
	PLINE (46,60) = SC_NAME

;;; for orders in system prior to SCAC switch...
;;;	IF (OSCAC.EQ.BLANKS .AND. OSHVIA.EQ.'1') OSCAC = 'WC'
	IF (OSCAC.NE.'WC')
	BEGIN
	  IF (OCLPPD.EQ.'C') PLINE (62,66) = '(COL)'
	  IF (OCLPPD.EQ.'D') PLINE (62,66) = '(COD)'
	  IF (OCLPPD.EQ.'P') PLINE (62,66) = '(PPD)'
	END

	XCALL DATE8(OSHDAT, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE (69,78) = D_FMT

	SRCCTL = 2
	BSEND = ORGART
	XCALL SERCH (8,ARTERM,OTERMS,1,1,BSEND,BSMID,SRCCTL,4,2,7,0,0,0,0)
	IF (SRCCTL.EQ.1) ARTRDS = '*NF*'

	PLINE (81,95) = ARTRDS

	IF(LP_ON)XCALL HP (14,hpPOS,21,0,0,PLINE)

	CLEAR PLINE
	PLINE (17,28) = XPOL2
	IF(LP_ON)XCALL HP (14,hpPOS,22,0,0,PLINE)

	CLEAR PLINE

	VL = 56
	LROW = 22

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bottom of 1st page...
	IF (TPAGE .GT. 1)
		BEGIN
		VL = 70
		IF(LP_ON)XCALL HP (14,hpFONT,hp10CPI+hpBOLD)
		IF(LP_ON)XCALL HP (14, hpPOS,59,1, hpHLINE,168)	;
		IF(LP_ON)XCALL HP (14, hpPOS,60,63, hpHLINE,24)	;
		PLINE(1,12) = "Continued"
		IF(LP_ON)XCALL HP (14,hpPOS,61,64,0,PLINE(1,12))
		IF(LP_ON)XCALL HP (14, hpPOS,62,63, hpHLINE,24)	;
		IF(LP_ON)XCALL HP (14, hpPOS,60,63, hpVLINE,4)	;
		IF(LP_ON)XCALL HP (14, hpPOS,60,75, hpVLINE,4)	;
		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CALL BODY_HEADER		;1ST PAGE
	LINCNT = 25			;starting line for 1st page
	CLEAR PRINTED_LINES		;CLEAR FOR EACH NEW INVOICE
;;;	MAXLIN = 34 + 25 
	MAXLIN = 25 + FIRST_MAX		;computed at "count_line"
	RETURN
;-------------------------------------------------------------

FNDITM,
	LORDNO = OORDNO
	LINSEQ =
	LOKCTL = 1
	XCALL ISIO (5,ORDLIN,LORDNO,READ,LOKCTL)
	IF (LOKCTL.EQ.3) GO TO GETDUC
	IF (LORDNO.NE.OORDNO) GO TO GETDUC
ITEMS,
	IF (LORDNO.EQ.OORDNO) CALL PRTLIN
	IF (LORDNO.GT.OORDNO) GO TO GETDUC
	LOKCTL = 1
	XCALL IOS (5,ORDLIN,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GO TO GETDUC
	GO TO ITEMS

GETDUC,
	CALL CLEAR_SUM_ARRAYS

	FIND (6, DUCACC, OORDNO) [ERR=NXTDUC]
NXTDUC,
	LOKCTL = 1
	XCALL IOS (6,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0 .OR. DUCTOR.NE.OORDNO) RETURN
	SZ1=%TRN3(SIZE1)
	SZ2=%TRN3(SIZE2)

	CALL PRTDUC
	CALL SUMDUC
	GOTO NXTDUC
;----------------------------------------------------

SUMDUC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CONFIG(1,3) = GAUGE
	IF (DUTYPE.EQ.2.OR.DUTYPE.EQ.3) CONFIG(1,3) = THICK
	CONFIG(4,4) = DUTYPE
	CONFIG(5,5) = CAT
	CONFIG(6,6) = STY
	CONFIG(7,7) = SEAM

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
	DUCSQF = DUCSQF + SQFEET
	DUCPND = DUCPND + POUNDS
	DUCGPR = GPRICE
	SDUCRC(I) = DUCREC
	SUM_DUC = I		;# OF ARRAY ELEMENTS
	SUM_DUC = SUM_DUC + 1	;4-30-99 FOR LINFEED

	IF (LINER.GE.1.AND.LINER.LE.9.AND.LINER.NE.4)
&		LINSQF(LINER) = LINSQF(LINER) + SQFLIN

		;Summarize accessories

	IF (ACC.EQ.2) RETURN

	IF (SLIPS)
	BEGIN
	  QTY = SLIPS
	  ITM = SLPITM
	  PRC = SLPPRC
	  DES = SLPDES
	  CALL SUMACC
	END
	IF (DRIVES)
	BEGIN
	  QTY = DRIVES
	  ITM = DRVITM
	  PRC = DRVPRC
	  DES = DRVDES
	  CALL SUMACC
	END
	IF (TCORN)
	BEGIN
	  QTY = TCORN
	  ITM = CORITM
	  PRC = CORPRC
	  DES = CORDES
	  CALL SUMACC
	END
	IF (TNBQTY)
	BEGIN
	  QTY = TNBQTY
	  ITM = TNBITM
	  PRC = TNBPRC
	  DES = TNBDES
	  CALL SUMACC
	END
	IF (TGAQTY)
	BEGIN
	  QTY = TGAQTY
	  ITM = TGAITM
	  PRC = TGAPRC
	  DES = TGADES
	  CALL SUMACC
	END
	IF (TCLQTY)
	BEGIN
	  QTY = TCLQTY
	  ITM = TCLITM
	  PRC = TCLPRC
	  DES = TCLDES
	  CALL SUMACC
	END
	IF (TBAQTY)
	BEGIN
	  QTY = (JOINT * 4)
	  ITM = TBAITM
	  ITM (10,13) = SIZE1,'ZZX"'
	  PRC = TBAPRC
	  DES = TBADES
	  CALL SUMACC
	  QTY = (JOINT * 4)
	  ITM = TBAITM
	  ITM (10,13) = SIZE2,'ZZX"'
	  PRC = TBAPRC
	  DES = TBADES
	  CALL SUMACC
	END
	IF (TBNQTY)
	BEGIN
	  QTY = TBNQTY
	  ITM = TBNITM
	  PRC = TBNPRC
	  DES = TBNDES
	  CALL SUMACC
	END

	RETURN
;----------------------------------

SUMACC,
		; Check to see if the accessories have all ready 
		; been on this order
	I =
	DO BEGIN
	  INCR I
	  ACCREC = SACCRC(I)
	END
	UNTIL (ACCITM.EQ.ITM.OR.ACCITM.EQ.BLANKS.OR.I.GE.MAXACC)
	IF (I.GE.MAXACC) GOTO BADACC

	ACCQTY = ACCQTY + QTY
	ACCITM = ITM
	ACCPRC = PRC
	ACCDES = DES
	SACCRC(I) = ACCREC
	SUM_ACC = I		;# OF ARRAY ELEMENTS
	RETURN
;----------------------------------------------

BADCON,
	XCALL LINFD (1)
	INCR LINCNT
	PLINE (32,60) = 'DUCT INFORMATION IS '	
	CALL PRINT
	GOTO ERRMSG
BADACC,
	XCALL LINFD (1)
	INCR LINCNT
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


PRTDUC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Print duct record
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	IF ((LINCNT+2).GT.MAXLIN) CALL NXTPAG
	IF ((LINCNT+3).GT.MAXLIN) CALL NXTPAG
	INCR LINCNT
	INCR PRINTED_LINES

	PLINE (1,6) = JOINT,MASK1
	PLINE (8,9) = 'JT'
	IF (DUTYPE.NE.2.AND.DUTYPE.NE.3)
	BEGIN
	  PLINE (35,36) = GAUGE,'XX'
	  PLINE (37,38) = 'GA'
	END
	IF (DUTYPE.EQ.2.OR.DUTYPE.EQ.3)
	BEGIN
	  PLINE (35,38) = THICK,'.XXX'
	END
	PLINE (18,20) = SZ1,'ZZX'
	PLINE (21,21) = 'X'
	PLINE (22,24) = SZ2,'ZZX'
	PLINE (25,25) = 'X'
	PLINE (26,27) = SIZE3,'ZX'
	IF(SIZE1/1000.NE.SZ1 .OR. SIZE2/1000.NE.SZ2) PLINE(28,28)='*'
	PLINE (40,43) = DTYPE(DUTYPE)
	PLINE (45,50) = DCAT(CAT)
	PLINE (52,54) = DSTY(STY)
	PLINE (56,58) = DSEAM(SEAM)
	IF (SEAL.EQ.1) PLINE (60,63) = 'SEAL'
	PLINE (69,77) = GPRICE,MASK4
	DECMAL = ( POUNDS * GPRICE ) # 1
	PLINE (80,81) = 'LB'
	PLINE (85,94) = DECMAL,MASK
	TOTSAL = TOTSAL + DECMAL		;DUCT
	CALL PRINT

	PLINE (  1,6  ) = POUNDS,MASK1
	PLINE (  8,8  ) = '#'
	PLINE ( 40,46 ) = DLINER( LINER )
	IF (SQFLIN.NE.0) 
	BEGIN
	  PLINE ( 16,21 ) = SQFLIN,MASK1
	  PLINE ( 24,28 ) = 'SQ FT'
	  PLINE ( 47,51 ) = 'LINR '
 	END
	PLINE ( 52,64 ) = LOCAT
	PLINE ( 69,77 ) = LINPRC,MASK4
	PLINE ( 80,81 ) = 'SF'
	DECMAL = ( LINPRC * SQFLIN ) # 1
	PLINE (85,94) = DECMAL,MASK
	TOTSAL = TOTSAL + DECMAL		;LINER
	CALL PRINT

	FLG175 = 2
	LOKCTL = 1
	XCALL ISIO (6,DUCACC,KEY175,WRITE,LOKCTL)

	RETURN
;-------------------------------------------------------

;--------------------------------------------------------------

PRTSDU,
		;Print summarized duct from this order
	IF ((LINCNT).GT.MAXLIN) CALL NXTPAG
	INCR LINCNT
	FOR I FROM 1 THRU MAXDUC
	  BEGIN
	  DUCREC = SDUCRC(I)
	  IF (DUCCFG.EQ.0) GOTO PRTSD2
;;;	  IF ((LINCNT).GT.MAXLIN) CALL NXTPAG
	  IF ((LINCNT).GE.MAXLIN) CALL NXTPAG
	  PLINE (1,6) = DUCPND,MASK1
	  PLINE (7,7) = '#'
	  PLINE (18,19) = DUCCFG(1,3),'XX'
	  PLINE (21,22) = 'GA'
	  IF (DUCCFG(4,4).EQ.2.OR.DUCCFG(4,4).EQ.3)
	  BEGIN
	    PLINE (18,22) = DUCCFG(1,3),'.XXX'
	  END
	  PLINE (35,38) = DTYPE(DUCCFG(4,4))
	  PLINE (40,45) = DCAT(DUCCFG(5,5))
	  PLINE (47,49) = DSTY(DUCCFG(6,6))
	  PLINE (51,53) = DSEAM(DUCCFG(7,7))
	  CALL PRINT
	  SDUCRC(I) =
	END
PRTSD2,
	FOR I FROM 1 THRU 9		;ssq 1-10-01
	  BEGIN
	  IF (LINSQF(I).NE.0)
	  BEGIN
	    IF ((LINCNT).GE.MAXLIN) CALL NXTPAG
	    PLINE (1,6) = LINSQF(I),MASK1
	    PLINE (18,22) = 'SQ FT'
	    PLINE (40,46) = DLINER(I)
	    PLINE (47,51) = ' LINER'
	    CALL PRINT
	    LINSQF(I) =
	  END
	END
	RETURN	
;-----------------------------------------------
MEMO_PRINT,

PRINT,
	IF(LP_ON)XCALL HP(14, hpPOS, LINCNT, 0, 0, PLINE(1,%TRIM(PLINE)) )
	CLEAR PLINE
	INCR PRINTED_LINES	
	INCR LINCNT

	RETURN
;--------------------------------------------------

ORDTOT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; ORDER TOTALS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CALL PRTSDU

; print bottom notes...
	CLEAR PLINE
;;;	CALL PRINT

	OC_ORD = OORDNO
	OC_SEQ = 900
	FIND(CHN075,ORDNOT,COMKEY) [ERR=BXTNOT]
BXTNOT,
	XCALL IOS (CHN075, ORDNOT, READ, LOKCTL)
	IF(LOKCTL .NE. 0) GOTO EOF_BNT
	IF(OC_ORD .NE. OORDNO) GOTO EOF_BNT
	PLINE(35,64) = OC_COM
	CALL PRINT
	GOTO BXTNOT
EOF_BNT,
	IF(PAGOF.LT.TPAGE) CALL NXTPAG

	IF(LP_ON)XCALL HP (14,hpFONT,hp10CPI+hpBOLD)
	IF(LP_ON)XCALL HP (14, hpPOS,52,1, hpHLINE,168)	;
	IF(LP_ON)XCALL HP (14, hpPOS,60,1, hpHLINE,168)	;

	if (lp_on) xcall hp (14, hpPOS,61,17,0,remit1)
	if (lp_on) xcall hp (14, hpPOS,62,17,0,remit2)

	IF(LP_ON)XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	IF(LP_ON)XCALL HP (14, hpPOS,54,52, hpHLINE,88)	;
	IF(LP_ON)XCALL HP (14, hpPOS,58,52, hpHLINE,88)	;

	IF(LP_ON)XCALL HP (14,hpFONT,hpBOLD)

	CALL DISPLA_DISC

	IF(LP_ON)XCALL HP (14, hpPOS,54,3,0,D_LIN1(1,%TRIM(D_LIN1)))
	IF(LP_ON)XCALL HP (14, hpPOS,55,3,0,D_LIN2(1,%TRIM(D_LIN2)))
	IF(LP_ON)XCALL HP (14, hpPOS,56,3,0,D_LIN3(1,%TRIM(D_LIN3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF(LP_ON)XCALL HP 
&		(14, hpPOS,58,3,0,'Past due balances are subject to a')
	IF(LP_ON)XCALL HP 
&		(14, hpPOS,59,3,0,'finance charge of 1.50% per month.')

	IF (CMPCOD .EQ. 'CAT') GOTO IS_CAT
;;;	IF(LP_ON)XCALL HP 
;;;&		(14, hpPOS,57,1,0,'**************************************')
;;;	IF(LP_ON)XCALL HP 
;;;&		(14, hpPOS,58,1,0,'* EFFECTIVE IMMEDIATELY our discount *')
;;;	IF(LP_ON)XCALL HP 
;;;&		(14, hpPOS,59,1,0,'* terms have changed to 1% 10 net 30 *')
;;;	IF(LP_ON)XCALL HP 
;;;&		(14, hpPOS,60,1,0,'**************************************')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IS_CAT,
	IF(LP_ON)XCALL HP (14,hpFONT,hpMEDIUM)

	IF(LP_ON)XCALL HP (14, hpPOS,53,58,0,'SALE AMOUNT')	;
	IF(LP_ON)XCALL HP (14, hpPOS,55,53,0, OTAXFL)
	IF(LP_ON)XCALL HP (14, hpPOS,55,58,0,'STATE SALES TAX')	;
	IF(LP_ON)XCALL HP (14, hpPOS,56,58,0,'CITY SALES TAX')	;
	IF(LP_ON)XCALL HP (14, hpPOS,57,58,0,'SHIPPING CHARGES')	;
	IF(LP_ON)XCALL HP (14,hpFONT,hpBOLD)
	IF(LP_ON)XCALL HP (14, hpPOS,59,53,0,'TOTAL DUE BY ')	;
	PLINE(1,8) = D_30_DATE,	'ZX/XX/XX'
	IF(LP_ON)XCALL HP (14, hpPOS,59,66,0,PLINE(1,8))	;
	CLEAR PLINE
	
	IF(LP_ON)XCALL HP (14, hpPOS,52,52, hpVLINE,16)	;
	IF(LP_ON)XCALL HP (14, hpPOS,52,75, hpVLINE,16)	;

;test lines...

	IF(LP_ON)XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)

	PLINE(1,12) = OSALE,MASK	;11-08-11 SSQ
;;;	PLINE(1,12) = TOTSAL,MASK
	IF(LP_ON)XCALL HP (14,hpPOS,53,64,0,PLINE(1,12))

 	PLINE(1,12) = OTAX(1),MASK
	IF(LP_ON)XCALL HP (14,hpPOS,55,64,0,PLINE(1,12))

	PLINE(1,12) = (OTAX(2)+OTAX(3)) ,MASK
	IF(LP_ON)XCALL HP (14,hpPOS,56,64,0,PLINE(1,12))

	PLINE(1,12) = OFRGHT ,MASK
	IF(LP_ON)XCALL HP (14,hpPOS,57,64,0,PLINE(1,12))

	TAX = OTAX(1) + OTAX(2) + OTAX(3)

	DECMAL = OSALE + OMISC + TAX + OFRGHT
;;;	DECMAL = TOTSAL + OMISC + TAX + OFRGHT
	PLINE(1,12) = DECMAL ,MASK
	IF(LP_ON)XCALL HP (14,hpFONT,hpBOLD)
	IF(LP_ON)XCALL HP (14,hpPOS,59,64,0,PLINE(1,12))

	if(lp_on.eq.0)return

email_zero,
	OINVNO = oordno
	OINVDT = INVDAT
	OFLAG = 2		;OFLAG = 2 / INVOICE PRINTED
	if (osale .ne. totsal) 
		begin
		ds_loc = 'T'			;for ordtot
		call dup_sale
		end
;;;	OSALE = TOTSAL
	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,WRITE,LOKCTL)


	CALL PAGE_FEED
	RETURN
;---------------------------------------------------------
WRITE_EMAIL,

;;;	if (osale .eq. 0) return	;2-18-2011
;;;	if (osale .eq. 0) 
	if ((osale+omisc+otax+ofrght) .eq. 0) 	;6-15-15
		begin			;5-4-11
		call email_zero		; need to up date oflag
		return	
		end

	CLEAR EMAIL
	CE_CUST = OCUSNO
	CE_ORDER = OORDNO
	CE_AMOUNT = OSALE
	CE_DATE = invdat
	CE_FLAG = 0

	IF (OMICUS .NE. 0)		;12-16-19
	THEN	BEGIN
		CE_CC = 1		;CASH CUSTOMER
		CE_CUST = OMICUS
		END
	ELSE	CE_CC = 0		;NOT CASH CUSTOMER

	STORE (CHMAIL, EMAIL, CE_KEY) [err=dup_em]
dup_em,
	RETURN
;---------------------------------------------------------

dup_sale,
	chn_dup = %syn_freechn(1,99)		;get a free channel
	if (.not. chn_dup)
	then	nop
	else	begin
		open (chn_dup, a, 'smc:dup_sal.err')
		ds_ordno = oordno
		ds_osale = osale,	'ZZZ,ZZX.XX'
		ds_totsal = totsal,	'ZZZ,ZZX.XX'
		writes (chn_dup, dup_sale)
		close chn_dup
		end

	return
;---------------------------------------------------------


DISPLA_DISC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR D_LIN1, D_LIN2, D_LIN3

	D_DAYS = 30
	CALL MAKE_D_DATE
	D_30_DATE = D_DATE

	USING DSCODE SELECT
	(0),	BEGIN
		D_LIN1 = OCOMNT(1)
		D_LIN2 = OCOMNT(2)
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
;;;	WRK_DATE = INVDAT
	WRK_DATE = TODAY	;SSQ 11-1-05
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
	XCALL DATE8(WRK_DATE, D_DATE, D_OUTR, D_FMT, D_SW)
	RETURN
;-------------------------------------------------------

PAGE_FEED,
	if(lp_on)DISPLAY (14, EJECT)
	RETURN
;---------------------------------------------------------
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
			WITEM(4,8) = 'F2&F3'
			END
		WITEM (10,10) = J,'X'

		W_KEY = W_KEYA + W_KEYB
		STORE (CHNOUT, LINE, W_KEY)
		END
		

	RETURN
;------------------------------------------

ENDHDR,
	PGCNT = -9		;SSQ 6-10-98 no final form feed...

;restored 11-15-10
	IF(LP_ON)
		begin
		XCALL HPOFF (LPSW,SPLFIL,PGCNT)
		XCALL MESAG ('INVOICES HAVE BEEN PRINTED',1)
		end


	IF(LP_ON)
	;;;	BEGIN
	;;;	using cmpcod select
	;;;	('SMC','ROC','CAT'),	XCALL HPOFF (LPSW,SPLFIL,PGCNT)
	;;;	(),		XCALL HPOF3 (LPSW,SPLFIL,PGCNT)
	;;;	endusing
	;;;	XCALL MESAG ('INVOICES HAVE BEEN PRINTED',1)
	;;;	END

	GOTO BEGIN
END2,
	XCALL OUTPT (6,20,2,'ARE ALL INVOICES OK <N> ? ',V)
	XCALL INPUT (06,46,01,01,'YN',ENTRY,INXCTL,V)
	IF (ENTRY.EQ.' ') INXCTL = 2
	GO TO (END3,END4), INXCTL	;1=YES   2=NO
	GOTO END2
END3,
	XCALL WATE(4,V)
	CALL RBLIDX	;;;ADDED 6/23/93 TO REBUILD INDEX AFTER INVOICES PRINT
			;	AND PICKUP ANY ADDED INVOICES
	CALL CLOSE 

; mailin needs name of email file...
	filen(1,3) = cmpcod
	filen(4,4) = ':'
	filen(5,14) = 'emfile.dat'
	OPEN (CHMAIL,A,filen)
	WRITES (CHMAIL, EMFILE)
	CLOSE CHMAIL

	xcall pgchn ('cp:mailin',1)
;;;	XCALL PGCHN ('CP:PSTRIN',1)	;SSQ 2-22-05
;;;	XCALL PGCHN ('CP:POSTAR',1)

;;;
;	ADDED 6/23/93 TO REBUILD TEMP INDEX AFTER ALL INVOICES ARE OK
;;;
RBLIDX,
	XCALL OUTPT (2,1,1,'REBUILD ALPHA BILLING INDEX',1)
	CLOSE 7

;;;	open (7,o,'smc:invoic.wrk')	;ssq 10-14-98
	OPEN (7, O, REDFIL)		;SSQ 6-25-99

	ORDHDR =
	LOKCTL = 1
	XCALL IOS (7,ORDHDR,WRITE,LOKCTL)
	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)
RBLP2,
	IF (OFLAG.GE.2)
	BEGIN
	  LOKCTL = 1
	  XCALL IOS (7,ORDHDR,WRITE,LOKCTL)
	END
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.EQ.0) GOTO RBLP2
	ORDHDR = ']]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]'
	LOKCTL = 1
	XCALL IOS (7,ORDHDR,WRITE,LOKCTL)
	CLOSE 7
	XCALL OUTPT (2,1,0,'SORT    ALPHA BILLING INDEX',1)
;;;	redfil = "smc:invoic.wrk"
	SORT (INPUT=REDFIL,RECORD=ORDHDR,KEY=(OCUSNO,OORDNO))
	XCALL OUTPT (0,0,0,' ... COMPLETED',1)
	RETURN
;
END4,
	XCALL OUTPT (7,20,0,'END OFF FOR FURTHER CHANGES ?',V)
	XCALL INPUT (07,51,01,01,'YN',ENTRY,INXCTL,V)
	GO TO (END5,BEGIN), INXCTL
END5,
	XCALL WATE(3,V)
	XCALL OUTPT (2,1,1,'UNSET ORDER HEADER INVOICE FLAG',1)
	OORDNO = 0
	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)
	IF (LOKCTL.EQ.3) GO TO END6
	GO TO UNSET1
UNSET,
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GO TO END6
UNSET1,
	IF (OFLAG.NE.2) GO TO UNSET
	OFLAG = 1
	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,WRITE,LOKCTL)
	GO TO UNSET
END6,
	CLOSE 4
	XCALL OUTPT (2,1,1,'UNSET ORDER LINE INVOICE FLAG',1)
	ORDKEY =
	LOKCTL = 1
	XCALL ISIO (5,ORDLIN,ORDKEY,READ,LOKCTL)
	IF (LOKCTL.EQ.3) GO TO END7
	GO TO UNSTL1
UNSETL,
	LOKCTL = 1
	XCALL IOS (5,ORDLIN,READ,LOKCTL)
	IF (LOKCTL.NE.0) GO TO END7
UNSTL1,
	IF (LFLAG.NE.2) GO TO UNSETL
	LFLAG = 1
	LOKCTL = 1
	XCALL ISIO (5,ORDLIN,ORDKEY,WRITE,LOKCTL)
	GO TO UNSETL
END7,
	XCALL OUTPT (2,1,1,'UNSET DUCT ACCESSORY INVOICE FLAG',1)
UNSETD,
	LOKCTL = 1
	XCALL IOS (6,DUCACC,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GO TO END8
UNSTD1,
	IF (FLG175.NE.2) GO TO UNSETD
	FLG175 = 1
	LOKCTL = 1
	XCALL ISIO (6,DUCACC,KEY175,WRITE,LOKCTL)
	GO TO UNSETD
END8,
	CLOSE 6
	XCALL OUTPT (2,1,1,'\',1)
	XCALL MESAG('INVOICES FROM THIS RUN NOT VALID',2)
	XCALL MOUNT ('REGULAR PAPER','PRINTER')
	XCALL FILES (10,'I',41,4)		; FILE # 41 -- ITMMAS
	XCALL FILES (8,'I',68,4)		; FILE # 68 -- TMPIDX
	XCALL PGCHN ('CP:UNPRBL',1)


ENDOFF,
	XCALL MESAG ('FILES IN USE, RUN CLRFIL AND TRY AGAIN',1)
	CALL CLOSE
	XCALL PGCHN ('CP:UNPRBL',1)

;======================================================

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

CLEAR_SUM_ARRAYS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; CLEAR SACCRC ARRAY & SDUCRC ARRAY
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ACCREC =
	ACCQTY =
	FOR I FROM 1 THRU MAXACC SACCRC(I) = ACCREC
	DUCREC =
	DUCCFG =
	FOR I FROM 1 THRU MAXDUC	SDUCRC(I) = DUCREC

	RETURN
;-----------------------------------------------------
NXTPAG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INCR PAGOF
	CALL PAGE_FEED

	IF(PAGOF.LT.TPAGE)		;NOT LAST PAGE
	THEN	BEGIN
		VL = 102
		IF(LP_ON)XCALL HP (14,hpFONT,hp10CPI+hpBOLD)
		IF(LP_ON)XCALL HP (14, hpPOS,59,1, hpHLINE,168)	;
		IF(LP_ON)XCALL HP (14, hpPOS,60,63, hpHLINE,24)	;
		PLINE(1,12) = "Continued"
		IF(LP_ON)XCALL HP (14,hpPOS,61,64,0,PLINE(1,12))
		IF(LP_ON)XCALL HP (14, hpPOS,62,63, hpHLINE,24)	;
		IF(LP_ON)XCALL HP (14, hpPOS,60,63, hpVLINE,4)	;
		IF(LP_ON)XCALL HP (14, hpPOS,60,75, hpVLINE,4)	;
		END
	ELSE	BEGIN
		VL = 88
		END

	LINCNT = 09		
	MAXLIN = 44 + 9 

	LROW = 6
	CALL INV_HEADER
	CALL BODY_HEADER
	RETURN
;-------------------------------------------------------

INV_HEADER,
	IF(LP_ON)XCALL HP (14,hpFONT,hp8CPI+hpBOLD)
	IF (IS_CMEMO)
	THEN	IF(LP_ON)XCALL HP (14,hpPOS,2,24,0, "C R E D I T   M E M O")
	ELSE	IF(LP_ON)XCALL HP (14,hpPOS,2,24,0, "I N V O I C E")

	IF(LP_ON)XCALL HP (14, hpPOS,2,1,0, "SHEET METAL")
	IF(LP_ON)XCALL HP (14, hpPOS,3,1,0, "CONNECTORS, INC.")

	IF(LP_ON)XCALL HP (14,hpFONT,hp10CPI+hpBOLD)
	ENTRY(1,6) = OORDNO,'ZZZZZX'
	IF(LP_ON)XCALL HP (14, hpPOS,1,65,0,ENTRY(1,6))
	A_PAGOF = PAGOF,'ZZX'
	A_TPAGE = TPAGE,'ZZX' [LEFT]
	IF(LP_ON)XCALL HP (14,hpPOS,2,64,0,PAGE_OF)
	RETURN
;------------------------------------------------------------------------

BODY_HEADER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF(LP_ON)XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)
	IF(LP_ON)XCALL HP (14, hpPOS,LROW,0, hpHLINE,168)	;

	IF(LP_ON)XCALL HP (14,hpFONT,hp12CPI+hpBOLD)
	PLINE = '  QTY     QTY'
	INCR LROW
	IF(LP_ON)XCALL HP (14,hpPOS,LROW,0,0,PLINE(1,%TRIM(PLINE)) )

	PLINE = 'ORDERED SHIPPED    ITEM NUMBER             DESCRIPTION
&             UNIT PRICE  UM    EXT PRICE'
	INCR LROW
	IF(LP_ON)XCALL HP (14,hpPOS,LROW,0,0,PLINE(1,%TRIM(PLINE)) )
	CLEAR PLINE

	IF(LP_ON)XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)
	IF(LP_ON)XCALL HP (14, hpPOS,LROW,0, hpHLINE,168)	;

	IF(LP_ON)XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	IF(LP_ON)XCALL HP (14, hpPOS,LROW,7, hpVLINE,VL)	;
	IF(LP_ON)XCALL HP (14, hpPOS,LROW,17, hpVLINE,VL)	;
	IF(LP_ON)XCALL HP (14, hpPOS,LROW,33, hpVLINE,VL)	;
	IF(LP_ON)XCALL HP (14, hpPOS,LROW,66, hpVLINE,VL)	;
	IF(LP_ON)XCALL HP (14, hpPOS,LROW,79, hpVLINE,VL)	;
	IF(LP_ON)XCALL HP (14, hpPOS,LROW,82, hpVLINE,VL)	;
	RETURN
;-----------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
COUNT_LINES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; DETERMIN # OF PAGES ON INVOICE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR L_CNT, D_CNT, S_CNT

	SAVORD = OORDNO		;CURRENT ORDER #
	CLEAR SUM_ACC, SUM_DUC, NUM_LNR
	CALL CLEAR_SUM_ARRAYS

;;; 3-9-04: ssq use "outfil" which contains memo records...
;;;	FIND (CHNWRK, LINE, ^FIRST) [ERR=CL_EOF]
	FIND (CHNOUT, LINE, ^FIRST) [ERR=CL_EOF]
CL_LOOP,
;;;	READS (CHNWRK, LINE, CL_EOF)
	READS (CHNOUT, LINE, CL_EOF)
	INCR L_CNT
;;;	IF(SAV_MSEQ .NE. MSEQ) INCR L_CNT	;SSQ 4-14-00
;;;	SAV_MSEQ = MSEQ
	GOTO CL_LOOP
CL_EOF,
;-
	CLEAR IS_NOTE
	OC_ORD = OORDNO
	OC_SEQ = 0
	FIND(CHN075,ORDNOT,COMKEY) [ERR=NXTNOT2]
NXTNOT2,
	XCALL IOS (CHN075, ORDNOT, READ, LOKCTL)
	IF(LOKCTL .NE. 0) GOTO EOF_NNT2
	IF(OC_ORD .NE. OORDNO) GOTO EOF_NNT2
	incr l_cnt
	IS_NOTE = 1
	GOTO NXTNOT2
EOF_NNT2,
	IF(IS_NOTE) incr l_cnt

;-

	LOKCTL = 1
	XCALL ISIO (6, DUCACC, OORDNO, READ, LOKCTL)
	WHILE	(DUCTOR.EQ.OORDNO .AND. LOKCTL.EQ.0)
		BEGIN
		INCR D_CNT
		CALL SUMDUC		;summarize duct and acc
		LOKCTL = 1
		XCALL IOS (6, DUCACC, READ, LOKCTL)
		END

	FOR I FROM 1 THRU 9  
		BEGIN
		IF (LINSQF(I) .NE. 0) INCR NUM_LNR
		CLEAR LINSQF(I)
		END
	S_CNT = SUM_DUC + NUM_LNR
	CALL PG_CNT
	RETURN
;----------------------------------------------------

PG_CNT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;compute number of pages:
	P1_CNT = 34	;# of lines on 1st page of multi-page invoice.
	PX_CNT = 28	;# of lines on 1st page of 1 page invoice.
	P2_CNT = 44
	TPAGE = 1				;ALWAYS AT LEAST 1 PAGE

	T_LINES = L_CNT + (3*D_CNT) + S_CNT + 1
;;;	IF (T_LINES .LE. P1_CNT) RETURN		;1 PAGE
	FIRST_MAX = PX_CNT
	IF (T_LINES .LE. PX_CNT) RETURN
	IF(T_LINES.GT.PX_CNT .AND. T_LINES.LE.P1_CNT)
		BEGIN
		FIRST_MAX = T_LINES - 1		;FORCE 1 LINE ON 2ND PAGE.
		IF(D_CNT.EQ.0 .AND. S_CNT.EQ.0) FIRST_MAX = T_LINES -2
		TPAGE = 2
		RETURN
		END

	FIRST_MAX = P1_CNT

	REM = L_CNT - P1_CNT
NL_LOOP,				;COUNT PAGES OF JUST L/I'S
	IF (REM .LE. 0) GOTO CALC_DUCT
	INCR TPAGE
	REM = REM - P2_CNT
	GOTO NL_LOOP

CALC_DUCT,		;DUCTWORK MIXED W/ L/I'S
	REM = -REM
	D_REM = REM/3	;# OF DUCTWORK RECORDS WHICH CAN BE PRINTED ON
			;REMAINDER OF PAGE.

	X_REM = D_REM*3 - REM	;UNUSABLE (EXTRA) LINES
	D_REM = D_CNT -D_REM
ND_LOOP,
	IF(D_REM .LE. 0) GOTO CALC_SUM
	INCR TPAGE
	D_REM = D_REM - 14	;14*3 = 42 (closest to 44/3)
	X_REM = 2
	GOTO ND_LOOP

CALC_SUM,
	REM = -D_REM	;# OF 3 LINE SEGMENTS LEFT
	REM = REM * 3	;# OF LINES REMAINING
	REM = REM + X_REM - 1	;LEAVE BLANK LINE BEFORE SUMMARY LINES

	REM = S_CNT - REM
NS_LOOP,
	IF(REM .LE. 0) RETURN	;WILL FIT ON LAST PAGE
	INCR TPAGE
	REM = REM - P2_CNT
	GOTO NS_LOOP
	RETURN
;----------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (2,'SI',001,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN001 = 2
	
	xcall files (1, 'SI', 041, switch)
	chn041 = 1

;;;	XCALL FILES (2,'I', 002, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN002 = 2

	XCALL FILES (4,'SU',44,SWITCH)		;FILE # 44 -- ORDHDR FILE
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 4

	XCALL FILES (5,'SU',45,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 5

	XCALL FILES (75,'SI',075,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN075 = 75

	XCALL FILES (6,'SU',175,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN175 = 6

;;;	XCALL FILES (7,'I',68,SWITCH)		;FILE # 68 -- TMPIDX FILE
;;;	IF (SWITCH .EQ. 9) RETURN
	XCALL FFILE (44, REDFIL, CLCTL)
	REDFIL(5,14) = 'INVOIC.WRK'
	OPEN (7, I, REDFIL)
	CHN068 = 7

	XCALL FILES (8,'I',170, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN170 = 8

	XCALL FILES (17,'SI',182,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 17

	XCALL OECO(CHN182,COINFO)

	XCALL FILES (12, 'I', 54, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN054 = 12

	XCALL FILES (13, 'SI', 139, SWITCH)	;TMPCUS
	IF (SWITCH .EQ. 9) RETURN
	CHN139 = 13

;;;	LOKCTL = 1
;;;	XCALL IO (1,CUSMAS,1,READ,LOKCTL)
;;;	SAV001 = ORG001

	LOKCTL = 1
	XCALL IO (8,ARTCTL,1,READ,LOKCTL)
	ORGART = ORG170

	M_CMP = CMPCOD
	M_DT = %DATETIME

	xcall isamc (m_file,32,1,'start=1, length=13, nodups, ascend')
	open (41, su, m_file)
	chmail = 41

	EF_FILE = M_FILE
	EF_PROC = 0

	OPNOK = 1
	RETURN
;-----------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN001) CLOSE CHN001
;;;	IF (CHN002) CLOSE CHN002
	IF (CHN041) CLOSE CHN041
	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045
	IF (CHN068) CLOSE CHN068
	IF (CHN075) CLOSE CHN075
	IF (CHN175) CLOSE CHN175
	IF (CHN054) CLOSE CHN054
	IF (CHN170) CLOSE CHN170
	IF (CHN182) CLOSE CHN182
	IF (CHN139) CLOSE CHN139
	IF (CHMAIL) CLOSE CHMAIL

	RETURN
;-----------------------------------------------

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

