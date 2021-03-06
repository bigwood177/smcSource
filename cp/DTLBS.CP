SUBROUTINE DTLBS		;UPDATE DATLBS.ISM
	FLAG	,A		;D=DUCACC, L=ORDLIN
	DATA	,A		;ORDLIN/DUCACC
	OINVDT	,D		;INVOICE DATE
	CHN063	,D		;DATLBS
	CHN193	,D		;DPTSTS

; 10-9-19 SSQ: use product category instead of dept...
;6-17-20: ordlin.lpwnw = 1 => ordlin.lpwgt is XXX,XXX instead of X,XXX.XX



RECORD	DATLBS
	.INCLUDE 'DEF:RD063A.DEF'

RECORD	DPTSTS
	.INCLUDE 'DEF:RD193A.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:rD045A.DEF'

RECORD	DUCACC
	.INCLUDE 'DEF:RD175A.DEF'


RECORD	VARS
	XLBS	,D8
	XDATE	,D8
	XDEPT	,A2	;10-9-19 is actually product cat
	XORD	,D6
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2

PROC
	USING FLAG SELECT
	('D'),	CALL DUCACC
	('L'),	CALL ORDLIN
	ENDUSING

	XRETURN
;======================================================
ORDLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PROCESS ORDLIN
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ORDLIN = DATA
;
;6-17-20 weight may be whole number...
	IF (LPWNW .EQ. 1)
	THEN	XLBS = (LQTYSH*LPWGT)		;ALREADY WHOLE LBS
	ELSE	XLBS = (LQTYSH*LPWGT)#2
;;;	XLBS = (LQTYSH*LPWGT)#2
	IF (XLBS .LE. 0) RETURN	;NO POUNDS

;;;	XDEPT = LDEPT
	XDEPT = LPRDCD		;10-9-19

	XORD = LORDNO
	CALL LOAD_REC
	RETURN
;--------------------------------------------------------

DUCACC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PROCES DUCACC
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear ordlin			;9-11-19
	DUCACC = DATA
	XLBS = POUNDS
	IF (XLBS .LE. 0) RETURN	;NO LBS
	XCALL DDEPT (GAUGE,SIZE3,DUTYPE,XDEPT,STY)
	XORD = DUCTOR
	CALL LOAD_REC
	RETURN
;--------------------------------------------------------

LOAD_REC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; CREATE DATLBS DATA FROM ORDLIN/DUCACC
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 10-9-19: using lprdcd so no sense in checking dptsts...
	
	XDATE = OINVDT		;10-9-19

;;;	CLEAR DPTSTS
;;;	S_ORDN = XORD
;;;	S_DEPT = XDEPT(1,1)
;;;	XCALL ISIO (CHN193, DPTSTS, S_KEY, READ, LOKCTL)
;;;	IF (LOKCTL .EQ. 0)
;;;	THEN	XDATE = S_DATE		;DATE CLOSED
;;;	ELSE	XDATE = OINVDT		;INVOICE DATE

	CLEAR DATLBS
	CL_DATE = XDATE
	CL_DEPT = XDEPT			;10-7-19
;;;	CL_DEPT = XDEPT(1,1)
	CL_ORD = XORD
	XCALL ISIO (CHN063, DATLBS, LDA_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0)
	THEN	CALL UPDATE
	ELSE	CALL ADDNEW

	RETURN
;===================================================

UPDATE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CL_LBS = CL_LBS + XLBS
	XCALL ISIO (CHN063, DATLBS, LDA_KEY, WRITE, LOKCTL)
	RETURN
;------------------------------------------------

ADDNEW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR DATLBS
	CL_DATE = XDATE
	CL_DEPT = XDEPT			;10-07-19
	CL_ORD = XORD
	CL_LBS = XLBS
	XCALL ISIO (CHN063, DATLBS, LDA_KEY, STORE, LOKCTL)
	RETURN
;--------------------------------------------------


END