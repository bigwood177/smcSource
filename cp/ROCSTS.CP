;DPTSTS.CP
;
;	DEPARTMENT STATUS UPDATE
;
EXTERNAL FUNCTION
	GU_NAME	,A

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.DEF'

RECORD	SALMAN
	.INCLUDE 'DEF:RD054A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	DPTSTS
	.INCLUDE 'DEF:RD193A.DEF'

RECORD	DUCACC
	.INCLUDE 'DEF:RD175A.DEF'

RECORD	HLINE
	,A*,'DATE ENTERED  SALESMAN   EXT  PO NUMBER  JOB NUMBER  SHIP VIA'

RECORD	HLINE2
	,A*,'  DEPARTMENT           STATUS   COMMENT'

RECORD	DPT_DATA
	DPTARA	,20A2
	NUMDPT	,D2

RECORD	CHANNEL
	CHN044	,D2
	CHN045	,D2
	CHN054	,D2
	CHN175	,D2
	CHN182	,D2
	CHN193	,D2

RECORD	VARS
	pline	,a80
	GOTP	,D1
	GOTO	,D1
	UNAME	,A25
	I	,D5
	BLANKS	,A30
	XDATE	,D8
	D_LINE	,A60
	DCHAR	,D3
	TCHAR	,D3
	OPNOK	,D1
	XORD	,D6
	ROW	,D2
	COL	,D2
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	WDEPT	,A2
	LOKCTL	,D1

	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	SWITCH	,D1
	V	,D1

.PROC
	XCALL TERID (V)

	UNAME = %GU_NAME
	UPCASE UNAME
	
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'ORDER COMPLETION STATUS',1)
	XCALL OUTPT (3,4,0,'ORDER #',1)
	XCALL INPTT (3, 14, 06, 00 , '#E', ENTRY, INXCTL, 1)
	IF (INXCTL .EQ. 9) GOTO ENDOFF
	GOTO (DISPLA, ENDOFF), INXCTL
	XORD = ENTRY(1,6)

	CALL GETORD
	IF (LOKCTL) GOTO DISPLA

	CALL DSPDPT

ANYCN,
	XCALL OUTPT (24,1,1,'DEPT TO CHANGE:         (Z = Print)',1)
	XCALL INPTT (24, 19, 01, 00, 'AE' ,ENTRY, INXCTL, 1)
	IF (INXCTL .EQ. 9) GOTO ENDOFF
	GOTO (DISPLA, PROCES), INXCTL
	WDEPT = ENTRY(1,2)
	IF (WDEPT .EQ. '  ') GOTO DISPLA
	if (wdept .eq. 'Z') call print_sheet
		
	FOR I FROM 1 THRU NUMDPT	IF (WDEPT .EQ. DPTARA(I))
						BEGIN
						CALL CNGDPT
						EXITLOOP
						END
	GOTO ANYCN

print_sheet,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	open (14,o,'spl:dptsts.spl')
	pline = 'Order Completion Status'
	call print
	forms (14,2)
	pline (3,80) = 'Order:'
	pline (14,19) = oordno,	'ZZZZZX'
	pline (23,80) = ocusnm
	call print
	forms(14,2)

	writes (14,hline)
	pline (3,12) = xdate,	'ZX/XX/XXXX'
	pline (15,80) = slsnm(1,10)
	pline (26,28) = slstno,	'XXX'
	pline (31,80) = ojobno
	pline (42,80) = opono
	pline (54,80) = sc_name
	call print
	forms (14,2)

	writes (14,hline2)
	call prtdpt


	close 14
	lpque ('spl:dptsts.spl', delete)

	return
print,
	writes (14,pline)
	clear pline
	return
;---------------------------------------------------------

CNGDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	S_ORDN = OORDNO
	S_DEPT = WDEPT(1,1)
	XCALL ISIO (CHN193, DPTSTS, S_KEY, READ, LOKCTL)
	IF (LOKCTL) RETURN

	ROW = I + 8
	IF (S_STAT .EQ. 1)
		BEGIN
		S_STAT = 0
		XCALL OUTPT (ROW, 24, 0, 'OPEN ', 1)
	;;;	CLEAR S_COMM
		XCALL ISIO (CHN193, DPTSTS, S_KEY, WRITE, LOKCTL)
		RETURN
		END

	S_STAT = 1
	XCALL OUTPT (ROW, 24, 0, 'COMPL', 1)
	IF (S_COMM.EQ. BLANKS) 
		BEGIN
		CALL GET_TABLE
		S_COMM = DD_LOC
		XCALL OUTPT (ROW,33,1,S_COMM,1)
		END
	COL = 53
	CALL ACCEPT
	IF (TCHAR .EQ. 9) 		;<TAB>
		BEGIN
		XCALL INPTT (ROW, 33, 20, 00, 'A ', ENTRY, INXCTL, 1)
		IF (INXCTL .EQ. 9) GOTO ENDOFF
		IF (INXCTL .EQ. 0) 	S_COMM = ENTRY(1,20)
		END
	XCALL ISIO (CHN193, DPTSTS, S_KEY, WRITE, LOKCTL)

	RETURN
;---------------------------------------------------------


PROCES,
	GOTO DISPLA

ENDOFF,
	CALL CLOSE
	IF (UNAME(1,5) .EQ. 'PLANT')
		BEGIN
		XCALL PGCHN('CP:SHPMNU',1)
		END
	XCALL PGCHN ('CP:CPLMNU',1)
	
DSPDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL OUTPT (8,1,1,HLINE2,1)
	CLEAR GOTP, GOTO
	READ (CHN175, DUCACC, OORDNO) [ERR=FNDDUC]
FNDDUC,
	WHILE (DUCTOR .EQ. OORDNO)
		BEGIN
		USING STY SELECT
		(1,4,5),	GOTP=1
		(2,3,6),	GOTO=1
		ENDUSING
		READS(CHN175, DUCACC, EOF_FDD)
		END
EOF_FDD,	
	CLEAR DPTSTS
	S_ORDN = OORDNO
	IF(GOTP)
		BEGIN
		S_DEPT(1,1) = 'P'
		XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)
		END

	IF(GOTO)
		BEGIN
		S_DEPT(1,1) = 'O'
		XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)
		END

FNDLIN,
	FIND (CHN045, ORDLIN, OORDNO) [ERR=NXTLIN]
NXTLIN,
	XCALL IOS (CHN045, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF_LIN
	IF (LORDNO .NE. OORDNO) GOTO EOF_LIN
	IF (LTYPE .EQ. 'M') GOTO NXTLIN		;SKIP MEMOS
	if (lprdcd .eq. 'Z') goto nxtlin	;8-2-07 internet discount

; create a DPTSTS record for every dept in order.  If there are
; already records in DPTSTS file, they will not be over-written.
	CLEAR DPTSTS
	S_ORDN = LORDNO
	S_DEPT(1,1) = LDEPT(1,1)
	XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)
;;;	STORE (CHN193, DPTSTS, S_KEY)
	GOTO NXTLIN

EOF_LIN,
prtdpt,
	CLEAR DPT_DATA
	ROW = 8
	NUMDPT = 0
	FIND (CHN193, DPTSTS, OORDNO) [ERR=NXTDPT]
NXTDPT,
	XCALL IOS (CHN193, DPTSTS, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF_DPT
	IF (S_ORDN .NE. OORDNO) GOTO EOF_DPT
	IF (S_DEPT .EQ. '**') GOTO NXTDPT		;HEADER 6-21-17

	CALL GET_TABLE

	D_LINE(3,4) = DD_DEPT
	D_LINE(6,20) = DD_DESC
	USING S_STAT SELECT
	(0),	D_LINE(24,28) = 'OPEN '
	(1),	D_LINE(24,28) = 'COMPL'
	ENDUSING

	D_LINE(33,52) = S_COMM
	INCR ROW

	pline = d_line
	if (%chopen(14)) call print

	XCALL OUTPT (ROW, 1, 0, D_LINE, 1)
	IF (S_STAT) XCALL XYB(ROW,24,'COMPL')

	INCR NUMDPT
	DPTARA(NUMDPT) = S_DEPT(1,1)
	GOTO NXTDPT

EOF_DPT,
	RETURN
;---------------------------------------------------------

GET_TABLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; READ DEPT FROM COPTBL
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'DD'
;;;	DD_DEPT = S_DEPT	
	DD_DEPT = S_DEPT(1,1)
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL) DD_DESC = '* NOT ON FILE *'
	RETURN
;--------------------------------------------------------


GETORD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ISIO (CHN044, ORDHDR, XORD, READ, LOKCTL)
	IF (LOKCTL)
		BEGIN
		XCALL MESAG ('ORDER NOT ON FILE',1)
		RETURN
		END
	XCALL IO (CHN054, SALMAN, OSLMAN, READ, LOKCTL)
	IF (LOKCTL) CLEAR SALMAN

	XCALL OUTPT (3,23,0,OCUSNM,1)
	XCALL OUTPT (5,1,0,HLINE,1)

	XDATE(1,4) = OORDDT(5,8)
	XDATE(5,8) = OORDDT(1,4)
	ENTRY(1,10) = XDATE,	'XX/XX/XXXX'
	XCALL OUTPT (6,3,0,ENTRY(1,10),1)

;;;	XCALL OUTPT (6,15,0,SLSNM(1,10),1)
	XCALL XYB(6,15,SLSNM(1,10))
	ENTRY(1,3) = SLSTNO,'XXX'
	XCALL XYB(6,26,ENTRY(1,3))
;;;	XCALL OUTPT (6,26,0,ENTRY(1,3),1)

	XCALL OUTPT (6,31,0,OPONO,1)
	XCALL OUTPT (6,42,0,OJOBNO,1)
	
	TBLCOD = 'SC'
	SC_SCAC = OSCAC
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL) SC_NAME = '* NOT ON FILE *'
	XCALL OUTPT (6,54,0,SC_NAME,1)


	RETURN
;---------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (4, 'SI', 044, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 4

	XCALL FILES (5, 'SI', 045, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 5

	XCALL FILES (6, 'I', 054, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN054 = 6

	XCALL FILES (7, 'SI', 175, SWITCH)
	IF(SWITCH .EQ. 9) RETURN
	CHN175 = 7

	XCALL FILES (8, 'SI',  182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 8 

	XCALL FILES (9, 'SU', 193, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN193 = 9

	OPNOK = 1
	RETURN
;---------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045
	IF (CHN054) CLOSE CHN054
	IF (CHN193) CLOSE CHN193
	IF (CHN175) CLOSE CHN175

	RETURN
;---------------------------------------------
ACCEPT,
	XCALL FLAGS (00010000,1)
	XCALL OUTPT (ROW,COL,0,'\',1)
;;;	ACCEPT (15,TCHAR)
	ACCEPT (15,TCHAR,WAIT:60) [ERR=AC_RET]
	IF (TCHAR.EQ.10) GOTO ACCEPT
AC_RET,
	IF (TCHAR.EQ.13) ACCEPT (15,DCHAR)
	XCALL FLAGS (00010000,0)
	RETURN
.END

.SUBROUTINE XYB
	X	,D
	Y	,D
	TEXT	,A

.PROC
	DISPLAY (15, $SCR_ATT(REVERSE), $SCR_POS(X,Y), TEXT)
;;;	DISPLAY (15, $SCR_ATT(BOLD), $SCR_POS(X,Y), TEXT)
	DISPLAY (15, $SCR_ATT(CLEAR))
	RETURN
.END
