;SYNCIM.CP
;	Update Rockford Inventory prices from SMC.
;
RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.DEF'
RECORD	ITMCTL
	.INCLUDE 'DEF:RD041B.DEF'
RECORD	ITMIDX
	.INCLUDE 'DEF:RD042A.DEF'

RECORD	SYNC
	S_ITEM	,A15
	S_PRICE	,D9
		,A1
	S_REC	,D6
		,A1
	S_FLAG	,D1	;0=REG, 1=]]]DEL, 2=PROCESSED, 3=UPDATED ROC

RECORD	ERROR
	ER_ITEM	,A15
		,A1
	ER_MSG	,A30

RECORD	DIS
	II	,D5

RECORD	CHANNEL
	ROC041	,D2
	ROC042	,D2
	SMC041	,D2
	SMC042	,D2
	CHNSYN	,D2
	CHNERR	,D2
;
RECORD	VARS
	OPNOK	,D1
	RECNO	,D6
;
PROC
	OPEN(15,I,'TT:')
;;;	DISPLAY(15,$SCR_CLR(SCREEN),'SYNCIM')
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	CALL CLEAR_SYNC
	CALL READ_SMC
	CALL UPDATE_ROC

ENDOFF,
	CALL CLOSE
	XCALL FLAGS(7000000)
	STOP

CLEAR_SYNC,	;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; CLEAR SYNCIM.ISM FILE
		;;;;;;;;;;;;;;;;;;;;;;;;;

	FIND (CHNSYN, SYNC, ^FIRST) [ERR=CS_EOF]
	DO FOREVER
		BEGIN
		READS (CHNSYN, SYNC, CS_EOF)
		DELETE (CHNSYN)
		END
CS_EOF,
	RETURN
;----------------------------------------
READ_SMC,	;;;;;;;;;;;;;;;;;;;;;;;;;
	RECNO = 1
	READ (SMC041, ITMMAS, 1)		;FIRST RECORD
	DO FOREVER
		BEGIN
		READS (SMC041, ITMMAS, RS_EOF)
		IF(ITMMAS .EQ. ']]]]]]') GOTO RS_EOF
		INCR RECNO
		S_ITEM = ITEMNO
		S_PRICE = PRICE
		S_REC = RECNO
		IF(DESCR.EQ.']]]DEL')
		THEN	S_FLAG = 1
		ELSE	BEGIN
			S_FLAG = 0
			STORE (CHNSYN, SYNC, S_ITEM)
			END
		END
RS_EOF,
	RETURN
;----------------------------------------

UPDATE_ROC,	;;;;;;;;;;;;;;;;;;;;;;;;;
	RECNO = 1
UR_LOOP,
	INCR RECNO
	READ (ROC041, ITMMAS, RECNO)
	IF (ITMMAS .EQ. ']]]]]]') GOTO UR_EOF
	READ (CHNSYN, SYNC, ITEMNO) [ERR=NOF]
	IF(S_FLAG .EQ. 0)
		BEGIN
		S_FLAG = 2
		IF (PRICE .NE. S_PRICE)
			BEGIN
			PRICE = S_PRICE
			WRITE (ROC041, ITMMAS, RECNO)
			S_FLAG = 9
			END
		WRITE (CHNSYN, SYNC, ITEMNO)
		END
	GOTO UR_LOOP
NOF,
	ER_ITEM = ITEMNO
	ER_MSG = 'Not found in SYNC file'
	WRITES(CHNERR,ERROR)
	GOTO UR_LOOP

UR_EOF,
	FIND (CHNSYN, SYNC, ^FIRST) [ERR=URE_EOF]
	DO FOREVER
		BEGIN
		READS (CHNSYN, SYNC, URE_EOF)
		USING S_FLAG SELECT
		(0),	CALL ADD_ITEM
		ENDUSING

		IF (S_FLAG .LT. 2)
			BEGIN
			ER_ITEM = S_ITEM
		;;;	ER_MSG = 'Did not update Rockford'
			IF (S_FLAG.EQ.1) ER_MSG(25,30) = 'DELETE'
			WRITES (CHNERR, ERROR)
			END
		END
URE_EOF,
	RETURN
;----------------------------------------

ADD_ITEM,	;;;;;;;;;;;;;;;;;;;;;;;;;
	READ (SMC041, ITMMAS, S_REC) [ERR=E1]		;NOT IN ROCKFORD ITMMAS
	READ (ROC041, ITMCTL, 1) [ERR=E2]

	INCR REC041
	CLEAR USEMTD, USEYTD, QTYMTD, QTYYTD, SLSMTD, SLSYTD,
&		CSTMTD, CSTYTD, QTYONH, QTYONO, QTYCOM

	WRITE (ROC041,ITMMAS, REC041) [ERR=E3]
	WRITE (ROC041, ITMCTL, 1) [ERR=E4]

	IITMNO = ITEMNO
	IRC041 = REC041
	IPRCAT = PRDCAT
	WRITE (ROC042, ITMIDX, REC041) [ERR=E5]

	S_FLAG = 3			;WROTE NEW ROCKFORD RECORD
	RETURN
;----------------------------------------
E1,
	ER_MSG = 'Could not read SMC itmmas'
	RETURN
E2,
	ER_MSG = 'Could not read ROC header'
	RETURN
E3,
	ER_MSG = 'Could not write ROC itmmas'
	RETURN
E4,
	ER_MSG = 'Could not write ROC header'
	RETURN
E5,
	ER_MSG = 'Could not write ROC itmidx'
	RETURN
;----------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLOSE SMC041
	CLOSE SMC042
	CLOSE ROC041
	CLOSE ROC042
	CLOSE CHNSYN
	CLOSE CHNERR

	RETURN
;----------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	OPEN(1,I,'SMC:ITMMAS.SMC')
	SMC041 = 1

	OPEN(2,I,'SMC:ITMIDX.SMC')
	SMC042 = 2

	OPEN(3,U,'ROC:ITMMAS.ROC')
;;;	OPEN(3,U,'TST:ITMMAS.TST')
	ROC041 = 3

	OPEN(4,U,'ROC:ITMIDX.ROC')
;;;	OPEN(4,U,'TST:ITMIDX.TST')
	ROC042 = 4

	OPEN(5,SU,'SMC:SYNCIM.ISM')
	CHNSYN = 5

	OPEN(6,O,'SMC:SYNCIM.ERR')
	CHNERR = 6

	OPNOK = 1
	RETURN
;----------------------------------------
