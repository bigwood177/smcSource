;NOTE: 7-24-07: alwasy open ordlin, otherwise reading ordlin in this routine
; will leave you at the end of the order in calling routine.

SUBROUTINE	CPM
	ORDN	,D
	MEMDAT	,A
	CHAN45	,D	;optional - if ordlin already open
;
; read through order file and fill memo array
;

RECORD	CPMEMO
	.INCLUDE 'DEF:CPMEMO.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD045D.DEF'
RECORD,X
	.INCLUDE 'DEF:RD045M.DEF'

RECORD	VARS
	mcode	,a2	;tblcod for memos
	CHN044	,D3
	CHN045	,D3
	CHN182	,D3
	I	,D6
	SWITCH	,D1
	LOKCTL	,D1
	READ	,D1,0
;
PROC

	CALL OPENS

	CLEAR F1_NUM, F2_NUM, F3_NUM
	FOR I FROM 1 THRU F_MAX
		BEGIN
		CLEAR F1_MEMOS(I)
		CLEAR F2_MEMOS(I)
		CLEAR F3_MEMOS(I)

		CLEAR F1_MEMOL(I)
		CLEAR F2_MEMOL(I)
		CLEAR F3_MEMOL(I)
		END

	CLEAR ORDKEY
	LORDNO = ORDN
	
	FIND (CHN045, ORDLIN, ORDKEY) [ERR=MLOOP, EOF=EOF_M]
MLOOP,
;;;	LOKCTL = 1
;;;	XCALL IOS (CHN045, ORDLIN, READ, LOKCTL)
;;;	IF (LOKCTL .NE. 0) GOTO EOF_M

	READS (CHN045, ORDLIN, EOF_M) [ERR=EOF_M]
	IF (LORDNO .NE. ORDN) GOTO EOF_M
	IF (LTYPE .NE. 'M') GOTO MLOOP		;SSQ 12-16-99

	CALL GET_MEMO_TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; removed 6-03-10 ssq
;;;	IF (MM_IK) GOTO MLOOP		;this memo not used as part of item key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (LMSQ1 .GT. 0) 
		BEGIN
		F1_MEMOS(LMSQ1) = M_SHORTD
		F1_MEMOL(LMSQ1) = M_LDESCR
		F1_KEY(LMSQ1) = M_KEY
		INCR F1_NUM
		END
	IF (LMSQ2 .GT. 0) 
		BEGIN
		F2_MEMOS(LMSQ2) = M_SHORTD
		F2_MEMOL(LMSQ2) = M_LDESCR
		F2_KEY(LMSQ2) = M_KEY
		INCR F2_NUM
		END
	IF (LMSQ3 .GT. 0) 
		BEGIN
		F3_MEMOS(LMSQ3) = M_SHORTD
		F3_MEMOL(LMSQ3) = M_LDESCR
		F3_KEY(LMSQ3) = M_KEY
		INCR F3_NUM
		END

	GOTO MLOOP

GET_MEMO_TABLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	if (m_lmsq1 .gt. 0) tblcod = 'M1'
	if (m_lmsq2 .gt. 0) tblcod = 'M2'
	if (m_lmsq3 .gt. 0) tblcod = 'M3'

;;;	TBLCOD = 'MM'
	MM_KEY = M_KEY
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_KEY]
	RETURN
NOT_KEY,
	CLEAR MM_IK
	RETURN
;------------------------------------------------------
	
EOF_M,
	MEMDAT = CPMEMO
;;;	IF (.NOT. %PASSED(CHAN45)) CLOSE CHN045
	CLOSE CHN045
	CLOSE CHN182

	XRETURN

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	IF (%PASSED(CHAN45))
;;;	THEN	CHN045 = CHAN45
;;;	ELSE	BEGIN

	FOR CHN045 FROM 50 THRU 100
		BEGIN
		IF (.NOT.%CHOPEN(CHN045)) EXITLOOP
		END
	SWITCH = 5
	XCALL FILES (CHN045, 'SI', 045, SWITCH)
;;;	open (chn045, si, 'smc:slhlin.smm')

;;;		END	

	FOR CHN182 FROM 50 THRU 100
		BEGIN
		IF (.NOT.%CHOPEN(CHN182)) EXITLOOP
		END
	SWITCH = 5
;;;	XCALL FILES (CHN182, 'SI', 182, SWITCH)
	open (chn182, si, 'smc:coptbl.smm')	
	RETURN
;--------------------------------------------------------------
