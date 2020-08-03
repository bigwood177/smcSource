;rocrcv.cp
;	receive smc invoiced orders into rockfor inventory
;	1-14-10 use open orders instead of invoiced orders...

;
RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD045D.DEF'

RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.DEF'
RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	RECTRX
	.INCLUDE 'DEF:RD191A.DEF'
RECORD	DUMREC
	.INCLUDE 'DEF:RD191B.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	TMPLT
	T_NUM	,A3
		,A2
	T_ITEM	,A15
		,A2
	T_DESC	,A30
		,A2
	T_QTY	,A6
		,A2
		,A10
		,A*,	'<TAB>'

RECORD	CHANNEL
	CHN041	,D2
	CHN044	,D2
	CHN045	,D2
	CHN182	,D2
	CHN191	,D2
;
RECORD	VARS
	TODAY	,D8
	OPNOK	,D1
	NEXT	,D6
	JOB	,D6
	I	,D6
	drow	,d2
	ROW	,D2
	NUM	,D2
	NUMITM	,D2
	LRFA	,A6
	RFA	,40A6
	QTYS	,40D6
	A6	,A6
	A10	,A10
	XDATE	,D8
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	READ	,D1,0
	WRITE	,D1,1
	LOKCTL	,D1
	SWITCH	,D1
	V	,D1
;
PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'ROCKFORD ORDERS FROM SMC RECEIPTS',1)
	XCALL RDAT8 (TODAY)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	CALL CLRARA

DISPLA,
	XCALL OUTPT (1,1,2,'ROCKFORD ORDERS FROM SMC RECEIPTS',1)
	CLEAR CNGCTL
	XCALL OUTPT (2,01,0, '1. SMC ORD#',1)
;;;	XCALL OUTPT (2,01,0, '1. ROC ORD#',1)
JHNUM,
	XCALL INPUT (02, 13, 06, 00, '#E' ,ENTRY, INXCTL,1)
	GOTO (DISPLA, ENDOFF), INXCTL
	JOB = ENTRY(1,6)
	IF (JOB .LE. 0) GOTO JHNUM

;;;	clear ordhdr
;;;	ojobno = job
;;;	READ (CHN044, ORDHDR, OJOBNO, KEYNUM:4) [ERR=JHNUM]

	LOKCTL = 1
	XCALL ISIO (CHN044, ORDHDR, JOB, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO JHNUM

	A6 = OORDNO,	'ZZZZZX' [LEFT]
	XCALL OUTPT (2, 23, 0, A6,1)

	XCALL OUTPT (24,1,1, 'RIGHT ORDER?        <Y>',1)
	XCALL INPUT (24,16,01,00,'YY',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL-1

	ROW = 4
	clear num
	FIND (CHN045, ORDLIN, OORDNO) [ERR=LINLOP]
LINLOP,
	CLEAR CNGCTL
	READS (CHN045, ORDLIN, ENDLIN, GETRFA:LRFA) [ERR=ENDLIN]
	IF (LORDNO .NE. OORDNO) GOTO ENDLIN
	IF (LQTYOR .LE. 0) GOTO LINLOP
;;;	IF (LQTYSH .LE. 0) GOTO LINLOP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	INCR ROW
;;;	NUM = ROW-4
	incr num
	using num select
	(.lt. 20),	row = num + 4
	(.eq. 20),	begin
			xcall outpt (5,1,2,' ',1)
			row = 5
			end
	(.gt. 20),	row = (num-19) + 4
	endusing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	RFA(NUM) = LRFA

	T_NUM = NUM,	'ZX.'
	T_ITEM = LITMNO
	T_DESC = LDESCR
	T_QTY = LQTYOR,	'ZZZZZX' 
;;;	T_QTY = LQTYSH,	'ZZZZZX' 
	XCALL OUTPT (ROW, 1, 0, TMPLT, 1)
ASK,
	XCALL INPUT (ROW, 63, 06, 00, '#X', ENTRY, INXCTL, 1)
	GOTO (DISPLA, DISPLA),INXCTL
	IF (INXCTL .EQ. 3)
		BEGIN
		ENTRY(1,6) = LQTYOR,	'ZZZZZX' [LEFT]
	;;;	ENTRY(1,6) = LQTYSH,	'ZZZZZX' [LEFT]
		XCALL OUTPT (ROW,63,1,ENTRY(1,6),1)
		END

	QTYS(NUM) = ENTRY(1,6)
	GOTO (ANYCNG), CNGCTL

	CNGCTL = 2
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (ASK),CNGCTL

	IF (ROW .LT. 40) GOTO LINLOP
ENDLIN,
	IF (NUM .LT. 1) GOTO DISPLA
	NUMITM = NUM

ANYCNG,
	XCALL ANYCN (CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR),CNGCTL+1
CNGBR,
	IF (WHATNO .GT. NUMITM)	GOTO ANYCNG
	NUM = WHATNO
	ROW = WHATNO + 4
	GOTO ASK
PROCES,
	RH_KEY = '!!!!!!'
	READ (CHN191, DUMREC, RH_KEY)  
	NEXT = RH_NEXT

	FOR I FROM 1 THRU NUMITM
		BEGIN
		READ (CHN045, ORDLIN, RFA:RFA(I)) [ERR=BAD_READ]
		CLEAR RECTRX

		INCR NEXT
		R_SEQ = NEXT
		R_ITEM = LITMNO
		R_QTY = QTYS(I)
		R_ORD = LQTYOR	;QTY ORDERED
		R_DATE = TODAY
		R_PONO = LORDNO
		R_F1 = LF1
		R_F2 = LF2
		R_F3 = LF3
		
		STORE (CHN191, RECTRX, R_SEQ)

		END

	RH_KEY = '!!!!!!'
	READ (CHN191, DUMREC, RH_KEY)  
	RH_NEXT = NEXT
	WRITE (CHN191, DUMREC, RH_KEY)

	CALL CLRARA
	GOTO DISPLA

BAD_READ,
ENDOFF,
	CALL CLOSE
	xcall pgchn ('cp:immenu',1)
	STOP
	
CLRARA,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR NUM, NUMITM
	FOR I FROM 1 THRU 40
		BEGIN
		CLEAR RFA(I)
		CLEAR QTYS(I)
		END

	RETURN
;------------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK
	
;;;	SWITCH = 5
;;;	XCALL FILES (1, 'SU', 044, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN

	OPEN (1,SI,'SMC:ORDHDR.SMM')
;;;	OPEN (1,SI,'tst:SLHHDR.tsM')
	CHN044 = 1

;;;	SWITCH = 5
;;;	XCALL FILES (2, 'SU', 045, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN

	OPEN (2,SI,'SMC:ORDLIN.SMM')
;;;	OPEN (2,SI,'tst:SLHLIN.tsM')
	CHN045 = 2

;;;	SWITCH = 5
;;;	XCALL FILES (17, 'SI', 182, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
	OPEN (17,SI,'SMC:COPTBL.SMM')
	CHN182 = 17

;;;	SWITCH = 5
;;;	XCALL FILES (4, 'SI', 041, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN

	OPEN (4, SI, 'ROC:ITMMAS.ROM')
;;;	OPEN (4, SI, 'TST:ITMMAS.TSM')
	CHN041 = 4

;;;	SWITCH = 5
;;;	XCALL FILES (9, 'SU', 191, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN

	OPEN (9, SU, 'ROC:RECTRX.ROM')
;;;	OPEN (9, SU, 'TST:RECTRX.TSM')

	CHN191 = 9
	
	OPNOK = 1

	RETURN
;-------------------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN041) CLOSE CHN041
	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045
	IF (CHN182) CLOSE CHN182
	IF (CHN191) CLOSE CHN191

	RETURN
;-------------------------------------------------------------

END


