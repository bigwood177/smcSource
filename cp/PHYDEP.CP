;PHYDEP.CP
;
;	WIP REPORT.  WIP is defined as all orders packed but not shipped.
;		dptsts.s_stat=1 => this item is packed.  Also create a 
;		file of these lines so pstinv does not relieve inventory
;		when they are invoiced (since they were not counted during
;		the last physical inventory).

;		note that with the new status codes, dptsts.s_stat=3 means
;		closed

;5-29-14: Sort by lprdcd instead of ldept.

RECORD	DPTSTS
	.INCLUDE 'DEF:RD193A.DEF'
;
RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD045D.DEF'
;
RECORD	HDR
		,A*,	'DP      TOTAL     VALUE'
		,A3
	H_PAGE	,A4

RECORD	PRINT
	TITLE	,A*,	'WIP ORDERS'
	HD	,A6,	'NO HDR'
	LINCNT	,D2,	60
	SPLFIL	,A14
	LPONSW	,D1
	LPARG	,D1
	RPTNUM	,D3
	PRTTYP	,A1
	PGCNT	,D6,	000000
	PLINE	,A80
	PRTCTL	,D3
	LPSW	,D2

RECORD	FILEN
	F_NAME	,A14

RECORD	DIS
	II	,D6

RECORD	CHANNEL
	CHN044	,D2
	CHN045	,D2
	CHN182	,D2
	CHN193	,D2
	CHNWRK	,D2
;
RECORD	KEYS
	W_KEY1	,A40,	'START=1,LENGTH=6,NAME=LORDNO,DUPS,ASCEND'
	W_KEY2	,A42,	'START=074,LENGTH=2,NAME=LPRDCD,DUPS,ASCEND'
;;;	W_KEY2	,A42,	'START=130,LENGTH=2,NAME=LORDNO,DUPS,ASCEND'

RECORD	VARS
	cmpcod	,a3
	entry	,a30
	inxctl	,d1
	SAVDPT	,A2
	SAVORD	,D6
	DEC	,D18
	VAL	,D18
	OPNOK	,D1
	DPTTOT	,D18
	DPTVAL	,D18
	RPTTOT	,D18
	RPTVAL	,D18
	XDATE	,D8
	SWITCH	,D1
	V	,D1
;
PROC
;
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'WIP - COMPLETED ORDERS',1)

	xcall outpt (4,4,1,'You will be creating a new wip file by',1)
	xcall outpt (5,4,1,'running this report.  Are you sure?',1)
	xcall input (6,4,01,01,'YN',entry,inxctl,1)
	goto (endoff),inxctl-1

	xcall who (cmpcod)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	LPONSW = 0
	LINCNT = 60

	CLEAR RPTTOT, RPTVAL, DPTTOT, DPTVAL
	CLEAR II
LOOP,
	INCR II
	IF (II/250*250 .EQ. II) XCALL OUTPT (1,70,0,DIS,1)

	READS (CHN045, ORDLIN, EOF)

	IF (LTYPE .NE. 'L') GOTO LOOP		;5-29-14
	DEC = (LQTYOR*LPRICE)#1			;5-29-14
	IF (DEC .EQ. 0) GOTO LOOP		;5-29-14

	S_ORDN = LORDNO
	S_DEPT = LDEPT
	READ (CHN193, DPTSTS, S_KEY) [ERR=LOOP]

	using cmpcod select
	('SMC'),	if (s_stat .ne. 3) goto loop	;3= closed
	(),		IF (S_STAT .EQ. 0) GOTO LOOP
	endusing

	STORE (CHNWRK, ORDLIN, LORDNO)

	GOTO LOOP


EOF,
	FIND (CHNWRK, ORDLIN, ^FIRST, KRF=1) [ERR=P_EOF]	;BY DEPT
	CLEAR PLINE
	SAVDPT = '-1'
PLOOP,
	READS (CHNWRK, ORDLIN, P_EOF)
	IF (LPRDCD .NE. SAVDPT) CALL NEWDPT
;;;	IF (LDEPT .NE. SAVDPT) CALL NEWDPT

	DEC = (LQTYOR*LPRICE)#1

	CALL GET_MULT

	VAL = (DEC * PH_PCT)#2


	DPTTOT = DPTTOT + DEC
	DPTVAL = DPTVAL + VAL
	GOTO PLOOP

NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (SAVDPT .EQ. '-1') GOTO OUTDPT
;DP     TOTAL     VALUE
;
;AA ZZZ,ZZX.XX ZZZ,ZZX.XX   
;123456789012345678901234
;         1         2    

	PLINE (1,2) = SAVDPT
	PLINE (4,13) = DPTTOT,	'ZZZ,ZZX.XX'
	PLINE (15,24) = DPTVAL,	'ZZZ,ZZX.XX'
	CALL PRINT

	RPTTOT = RPTTOT + DPTTOT
	RPTVAL = RPTVAL + DPTVAL
OUTDPT,
	SAVDPT = LPRDCD
;;;	SAVDPT = LDEPT
	CLEAR DPTTOT, DPTVAL
	RETURN
;-------------------------------------------------
P_EOF,
	call newdpt

	CALL PRINT

	PLINE (1,13) = 'REPORT TOTAL:'
	PLINE (14,25) = RPTTOT,	'Z,ZZZ,ZZX.XX'
	PLINE (27,38) = RPTVAL,	'Z,ZZZ,ZZX.XX'
	CALL PRINT

;;;	CLOSE 14
	IF (LPONSW.EQ.1) XCALL LPOFF (LPSW,SPLFIL,PGCNT)

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:PHYMNU',1)
	stop

;==============================================================



;

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (LPONSW.EQ.0) CALL PRNTON
	PRTCTL = 70
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR,HD,HD,
&			'NO LEGEND',' ',' ',0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
PRNTON,
	SPLFIL (5,6) = 'EL'
	LPSW = 1	; PRINT, SPOOL OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) CALL CLOSE
	IF (LPSW.EQ.0) XCALL PGCHN ('CP:PHYMNU',1)
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	LPONSW = 1
	RETURN
;------------------------------------------------
;
GET_MULT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBLKEY
	TBLCOD = 'PH'
	PH_CAT = LPRDCD
	READ (CHN182, COPTBL, TBL_KEY) [ERR=NO_MULT]
	RETURN

NO_MULT,
	PH_PCT = 0
	RETURN
;-------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (4,'SI',44,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 4

	SWITCH = 5
	XCALL FILES (5,'SI',45,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 5
;
	SWITCH = 5
	XCALL FILES (18,'SI',182,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 18

	SWITCH = 5
	XCALL FILES (19,'SI',193,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN193 = 19


	XCALL FFILE (45, F_NAME, SWITCH)
	F_NAME(5,10) = 'PHYDEP'
	F_NAME(14,14) = 'M'

;;;	XCALL ISAMC(F_NAME, 154, 2, W_KEY1, W_KEY2)
	XCALL ISAMC(F_NAME, 180, 2, W_KEY1, W_KEY2)
	CHNWRK = 10
;;;	OPEN (CHNWRK, SU, 'SPL:PHYDEP')
	OPEN (CHNWRK, SU, F_NAME)

	OPNOK = 1

	RETURN
;-------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\

	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045
	IF (CHN182) CLOSE CHN182
	IF (CHN193) CLOSE CHN193

	RETURN
;-------------------------------------------

;DP      TOTAL     VALUE
;
;AA ZZZ,ZZX.XX ZZZ,ZZX.XX   
;123456789012345678901234
;         1         2    
