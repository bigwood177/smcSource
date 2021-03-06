;opnor4.cp
;	this is actually closed orders
;	opnor2.cp was split into opnor3 and this program
;
;	4-23-14 use pounds instead of dollars.

;DDEPT 5-29-14

.DEFINE	NO_REC	0
.DEFINE	D_OPN,	1
.DEFINE	CMPLT,	2


RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'
;
RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.DEF'

RECORD	DUCACC
	.INCLUDE 'DEF:RD175A.DEF'

RECORD	DPTSTS
	.INCLUDE 'DEF:RD193A.DEF'

RECORD	JOBHDR
	.INCLUDE 'DEF:RD071A.DEF'

RECORD	JOBLIN
	.INCLUDE 'DEF:RD072A.DEF'

RECORD	TEMP
	T_DEPT		,A2
	T_OPNORD	,D12	;OPEN
	T_CPLORD	,D12	;COMPLETED
	T_OPNJOB	,D12	;OPEN JOBS
	T_CPLJOB	,D12	;COMPLETED JOBS

RECORD	HDR2
	,A*,'              -----COMPLETED-----       '
RECORD	HDR3
	,A*,'DEPT          ORDERS   FAB-TICKETS           TOTAL'


RECORD	HDR1
		,A*,	'FROM: '
	H2_DAT1	,A10
		,A*,	' THRU: '
	H2_DAT2	,A10

;            -------OPEN--------       -----COMPLETED-----
;DEPT        ORDERS  FAB-TICKETS       ORDERS  FAB-TICKETS
;AA     ZZZZ,ZZX.XX- ZZZZ,ZZX.XX- ZZZZ,ZZX.XX- ZZZZ,ZZX.XX- ZZZZ,ZZX.XX-
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
RECORD	PRINT
	TITLE	,A*,	'DEPARTMENT SUMMARY - COMPLETED'
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

RECORD	DIS
	II	,D6

RECORD	CHANNEL
	CHN041	,D2
	CHN044	,D2
	CHN045	,D2
	CHN071	,D2
	CHN072	,D2
	CHN175	,D2
	CHN193	,D2
	CHN182	,D2
	CHNTMP	,D2

RECORD	VARS
	limbo	,d12
	opncmp	,d12

	ORD_JOB	,A1
	IS_VULCAN	,D1
	LINAMT	,D10
	DPT_STS	,D1	;0=NO DPTSTS RECORD 1=OPEN 2=COMPLETE
	DUCT_OC	,A1	;O=OPEN ORDERS, C=COMPLETED ORDERS
	TOT_OPNORD	,D12
	TOT_CPLORD	,D12
	TOT_OPNJOB	,D12
	TOT_CPLJOB	,D12


	tmpfil	,a14,	'SPL:OPNOR2.ISM'
	REDFIL	,A14
	CLCTL	,D1
	YEAR	,D4
	YR	,D2
	DECMAL	,D18
	TOTPRC	,D18
	D_DEPT	,A2	;DUCT DEPT
	DEPT_O	,D9
	DEPT_P	,D9
	O_AMT	,D9
	C_AMT	,D9
	O_DEPTO_AMT	,D12
	O_DEPTP_AMT	,D12
	C_DEPTO_AMT	,D12
	C_DEPTP_AMT	,D12

	SAVDPT	,A2
	SDATE	,D8	;START DATE
	EDATE	,D8	;END DATE
	XDATE	,D8
	TODAY	,D8	;TODAY'S DATE
	DATE1	,D8	;FIRST DAY OF THE YEAR
	SYEAR	,D4
	EYEAR	,D4
;
	ENTRY	,A30
	OPNOK	,D1
	WHATNO	,D2
	CNGCTL	,D1
	INXCTL	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2

	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'DEPARTMENT SUMMARY REPORT -  COMPLETED ORDERS',1)
;
	XCALL RDAT8(TODAY)
DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'DEPARTMENT SUMMARY REPORT -  COMPLETED ORDERS',1)
	XCALL OUTPT (4,4,0,'1. START DATE',1)
	XCALL OUTPT (6,4,0,'2. END   DATE',1)
SDATE,
	XCALL INPUT (4, 19, 08, 00, 'DE', ENTRY, INXCTL, 1)
	GOTO (DISPLA,ENDOFF),INXCTL
	SDATE = ENTRY(1,8)
	IF (SDATE .EQ. 0)
		BEGIN
		SDATE = TODAY
		XDATE(1,4) = SDATE(5,8)
		XDATE(5,8) = SDATE(1,4)
		ENTRY(1,10) = XDATE, 'ZX/XX/XXXX'
		XCALL OUTPT (4,19,1,ENTRY(1,10),1)
		END

	GOTO (ANYCNG),CNGCTL
EDATE,
	XCALL INPUT (6, 19, 08, 00, 'DE', ENTRY, INXCTL, 1)
	GOTO (DISPLA),INXCTL
	EDATE = ENTRY(1,8)
	IF (EDATE .EQ. 0)
		BEGIN
		EDATE = SDATE
		XDATE(1,4) = EDATE(5,8)
		XDATE(5,8) = EDATE(1,4)
		ENTRY(1,10) = XDATE, 'ZX/XX/XXXX'
		XCALL OUTPT (6,19,1,ENTRY(1,10),1)
		END

ANYCNG,
	XCALL ANYCN (CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR),CNGCTL+1
CNGBR,
	GOTO (SDATE, EDATE),WHATNO
	GOTO ANYCNG

PROCES,
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	CLEAR PGCNT
	LINCNT = 60

	CLEAR TOT_CPLORD, TOT_CPLJOB


	XDATE(1,4) = SDATE(5,8)
	XDATE(5,8) = SDATE(1,4)
	H2_DAT1 = XDATE,	'ZX/XX/XXXX'

	XDATE(1,4) = EDATE(5,8)
	XDATE(5,8) = EDATE(1,4)
	H2_DAT2 = XDATE,	'ZX/XX/XXXX'

	SYEAR = SDATE(1,4)
	EYEAR = EDATE(1,4)

	CALL GET_JOBS
	CALL GET_OPEN_ORD

	FOR YEAR FROM SYEAR THRU EYEAR CALL GET_COMPLETE

;PRINT REPORT...

	CALL CLOSE

	XCALL LPON (LPSW, SPLFIL)
	FIND (CHNTMP, TEMP, ^FIRST) [ERR=P_LOOP]
P_LOOP,
	READS (CHNTMP, TEMP, P_EOF)
	PLINE (1,4) = 	T_DEPT
	PLINE ( 8,21) = T_CPLORD, 'ZZZZ,ZZX.XX-'
	PLINE (23,34) = T_CPLJOB, 'ZZZZ,ZZX.XX-'
	PLINE (37,51) = (T_CPLORD+T_CPLJOB),  'ZZZ,ZZZ,ZZX.XX-'
	CALL PRINT

	TOT_CPLORD = TOT_CPLORD + T_CPLORD
	TOT_CPLJOB = TOT_CPLJOB + T_CPLJOB
	GOTO P_LOOP

P_EOF,
	CALL PRINT
	PLINE (1,5) = 'TOTAL'
	PLINE ( 7,21) = TOT_CPLORD,  'ZZZ,ZZZ,ZZX.XX-'
	PLINE (23,36) = TOT_CPLJOB,  'ZZ,ZZZ,ZZX.XX-'
	PLINE (37,51) = (TOT_CPLORD+TOT_CPLJOB),  'ZZZ,ZZZ,ZZX.XX-'

	CALL PRINT
	
	XCALL LPOFF (LPSW, SPLFIL, PGCNT)

	CLOSE CHNTMP
	XCALL DELET (TMPFIL)

	GOTO DISPLA

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	PRTCTL = 70
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,HDR2,HDR3,
&			'NO LEGEND',' ',' ',0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)

	RETURN
;-------------------------------------------

GET_OPEN_ORD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR ORDLIN, C_AMT, II
	SAVDPT = '-1'
	DUCT_OC = 'C'		;OPEN ORDERS

	ORD_JOB = 'O'
	DISPLAY (15,$SCR_POS(1,62), "ORDERS:")	

	FIND (CHN044, ORDHDR, ^FIRST) [ERR=FIND_NEXT]

FIND_NEXT,
	CALL GET_HDR
	IF (LOKCTL .NE. 0) RETURN

	FIND (CHN045, ORDLIN, OORDNO) [ERR=GO_LOOP]
GO_LOOP,
	INCR II
	IF (II/1000*1000.EQ.II) DISPLAY (15,$SCR_POS(1,70),DIS)
	XCALL IOS (CHN045, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO GO_EOF
	IF (LTYPE .EQ. 'M') GOTO GO_LOOP	;SKIP MEMOS

	LINAMT = LQTYOR * LITMWT		;SSQ 4-23-14 
	LINAMT = (LQTYOR*LPRICE)#1
	IF (LINAMT .LT. 0) GOTO GO_LOOP		;SKIP DISCOUNTS, CR'S

	IF (LORDNO .NE. OORDNO) 
		BEGIN
		CALL GETDPT
		CALL GET_DUCT
		GOTO FIND_NEXT
		END


	IF (LDEPT .NE. SAVDPT) 	CALL GETDPT

	CALL CHK_DPT_STS

	USING DPT_STS SELECT
	(CMPLT),	begin
			C_AMT = C_AMT + LINAMT	;COMPLETE
			end
	(9),		limbo = limbo + linamt
	ENDUSING


	GOTO GO_LOOP
GO_EOF,
	CALL GETDPT
	CALL GET_DUCT
	RETURN
;-------------------------------------------------

GET_JOBS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; OPEN & CLOSED JOBS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR C_AMT, II
	SAVDPT = '-1'

	ORD_JOB = 'J'

	DISPLAY (15,$SCR_POS(2,64), "JOBS:")	
	FIND (CHN071, JOBHDR, ^FIRST) [ERR=GJ_LOOP]
GJ_LOOP,
	INCR II
	IF (II/500*500 .EQ. II) DISPLAY (15,$SCR_POS(2,70), DIS)
	XCALL IOS (CHN071, JOBHDR, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO GJ_EOF
	IF (JHRCDT.LT.SDATE .OR. JHRCDT.GT.EDATE) GOTO GJ_LOOP

	LDEPT = JHDEPT		;for getdpt
	FIND (CHN072, JOBLIN, JHNUM) [ERR=JL_LOOP]
JL_LOOP,
	XCALL IOS (CHN072, JOBLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO JL_EOF
	IF (JLNUM .NE. JHNUM) GOTO GJ_LOOP	;NEXT HEADER

	IF (JHDEPT .NE. SAVDPT) CALL GETDPT

	XCALL ISIO (CHN041, ITMMAS, JLITM, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR ITMMAS

	LINAMT = JLQTY * IP_WGT			;SSQ 4-23-14 
	LINAMT = (JLQTY * PRICE)#1
	if (linamt .lt. 0) linamt = 0
	USING JLSTS SELECT
	('R'),	C_AMT = C_AMT + (JLRCV*PRICE)#1		;COMPLETED
	ENDUSING

	GOTO JL_LOOP
JL_EOF,
GJ_EOF,
	CALL GETDPT

	RETURN
;-------------------------------------------


;-------------------------------------------
GET_HDR,
	XCALL IOS (CHN044, ORDHDR, READ, LOKCTL)
	IF (LOKCTL.NE.0) RETURN
	IF (OLOC.NE.'O') GOTO GET_HDR
	IF (OSHDTE .LT. SDATE) GOTO GET_HDR	;ALREADY SHIPPED	

	SAVDPT = '-1'

	RETURN
;-------------------------------------------
CHK_DPT_STS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; IS THIS OPERATION OPEN OR COMPLETE?
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR DPT_STS
;;;	dpt_sts = 9	;default to limbo (01/06/14)
	dpt_sts = 1	;default to OPEN 

	xcall cvcan (ordlin, ocusno, chn182, is_vulcan)
	if (is_vulcan) ldept(2,2) = 'V'

	S_ORDN = LORDNO
	S_DEPT = LDEPT

	XCALL ISIO (CHN193, DPTSTS, S_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) RETURN	;NO RECORD

	IF (S_STAT.EQ.3) 
	THEN	DPT_STS = 2	;OPERATION IS COMPLETE
	ELSE	DPT_STS = 1	;OPERATION IS OPEN

	RETURN
;-------------------------------------------

GET_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR DEPT_O, DEPT_P
	CLEAR O_DEPTO_AMT, C_DEPTO_AMT, O_DEPTP_AMT, C_DEPTP_AMT

	FIND (CHN175, DUCACC, OORDNO) [ERR=GD_LOOP]
GD_LOOP,
	XCALL IOS (CHN175, DUCACC, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO GD_EOF
	IF (DUCTOR .NE. OORDNO) GOTO GD_EOF


	TOTPRC = POUNDS				;SSQ 4-23-14
	TOTPRC = (POUNDS * GPRICE ) # 1

	DECMAL =		;SSQ 4-23-14 still need to figure this out

	DECMAL = (LINPRC * SQFLIN ) # 1
	TOTPRC = TOTPRC + DECMAL		;LINER

	

	XCALL DDEPT (GAUGE,SIZE3,DUTYPE,D_DEPT,STY)
;;;	USING STY SELECT
;;;	(1,4,5),	D_DEPT = 'P'		;TDC
;;;	(2,3,6),	D_DEPT = 'O' 		;S&D
;;;	ENDUSING

	S_ORDN = OORDNO
	S_DEPT = D_DEPT
	XCALL ISIO (CHN193, DPTSTS, S_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR DPTSTS
	IF (S_STAT.EQ.3)
	THEN 	DPT_STS = 2		;COMPLETE
	ELSE	DPT_STS = 1		;OPEN

	USING DPT_STS SELECT
	(CMPLT),	USING D_DEPT SELECT
			('O'),	C_DEPTO_AMT = C_DEPTO_AMT + TOTPRC
			('P'),	C_DEPTP_AMT = C_DEPTP_AMT + TOTPRC
			ENDUSING

	ENDUSING

	GOTO GD_LOOP

GD_EOF,

	SAVDPT = 'O'
	C_AMT = C_DEPTO_AMT
	CALL GETDPT

	SAVDPT = 'P'
	C_AMT = C_DEPTP_AMT
	CALL GETDPT


	RETURN
;-------------------------------------------

GET_COMPLETE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Close the order files...
	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045
	IF (CHN175) CLOSE CHN175

	USING YEAR SELECT
	(.EQ.TODAY(1,4) ), BEGIN
		 	   XCALL FILES (84, 'SI', 184, SWITCH)
			   CHN044 = 84
		 	   XCALL FILES (85, 'SI', 185, SWITCH)
			   CHN045 = 85
		 	   XCALL FILES (86, 'SI', 186, SWITCH)
			   CHN175 = 86
			   END

	(.GT. TODAY(1,4)), 	RETURN		;IN THE FUTURE
	(.GT. 2006),	   BEGIN		;ARBITRARY, DON'T PROC PREV
			   YR=YEAR(3,4)		;SLHHXX, SLHLXX
			   XCALL FFILE (184, REDFIL, CLCTL)
			   REDFIL(9,10) = YR, 'XX'
			   REDFIL(14,14) = 'M'
			   OPEN (84, SI, REDFIL)
			   CHN044 = 84

			   XCALL FFILE (185, REDFIL, CLCTL)
			   REDFIL(9,10) = YR, 'XX'
			   REDFIL(14,14) = 'M'
			   OPEN (85, SI, REDFIL)
			   CHN045 = 85

			   XCALL FFILE (186, REDFIL, CLCTL)
			   REDFIL(9,10) = YR, 'XX'
			   REDFIL(14,14) = 'M'
			   OPEN (86, SI, REDFIL)
			   CHN175 = 86
			   END

	ENDUSING

	CLEAR ORDLIN, C_AMT, O_AMT, II
	SAVDPT = '-1'
	DUCT_OC = 'C'		;COMPLETE ORDERS
	FIND (CHN044, ORDHDR, ^FIRST) [ERR=GET_NEXT]

GET_NEXT,
	CALL GET_HDR
	IF (LOKCTL .NE. 0) RETURN

	FIND (CHN045, ORDLIN, OORDNO) [ERR=GC_LOOP]
GC_LOOP,
	INCR II
	IF (II/1000*1000.EQ.II) DISPLAY (15,$SCR_POS(1,70),DIS)
	XCALL IOS (CHN045, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO GC_EOF
	IF (LTYPE .EQ. 'M') GOTO GC_LOOP	;SKIP MEMOS

	CALL CHK_DPT_STS

	IF (LORDNO .NE. OORDNO) 
		BEGIN
		CALL GETDPT
		CALL GET_DUCT
		GOTO GET_NEXT
		END


	IF (LDEPT .NE. SAVDPT) CALL GETDPT

	LINAMT = (LQTYOR*LPRICE)#1
	if (linamt .lt. 0) goto gc_loop		;skip credits

	USING DPT_STS SELECT
	(CMPLT),	BEGIN
			IF (S_DATE.LT.SDATE .OR. S_DATE.GT.EDATE) LINAMT=0
			IF (LINAMT.GT.0) C_AMT = C_AMT + LINAMT	;COMPLETE
			END
	(D_OPN),	IF (OINVDT.GE.SDATE .AND. OINVDT.LE.EDATE) C_AMT = C_AMT + LINAMT
	ENDUSING

	GOTO GC_LOOP
GC_EOF,
	CALL GETDPT
	CALL GET_DUCT
	RETURN
;-------------------------------------------

GETDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (SAVDPT .EQ. '-1') GOTO OUTDPT
	IF (O_AMT.EQ.0 .AND. C_AMT.EQ.0) GOTO OUTDPT

	XCALL ISIO (CHNTMP, TEMP, SAVDPT, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO NEWDPT

	USING ORD_JOB SELECT
	('J'),	BEGIN
		T_CPLJOB = T_CPLJOB + C_AMT
		END
	('O'),	BEGIN
		T_CPLORD = T_CPLORD + C_AMT
		END
	ENDUSING

	XCALL ISIO (CHNTMP, TEMP, LDEPT, WRITE, LOKCTL)

	SAVDPT = LDEPT
	CLEAR C_AMT
	RETURN
;-------------------------------------------
NEWDPT,
	CLEAR TEMP
	T_DEPT = SAVDPT


	USING ORD_JOB SELECT
	('J'),	BEGIN
		T_CPLJOB = C_AMT
		END
	('O'),	BEGIN
		T_CPLORD = C_AMT
		END
	ENDUSING
	
	XCALL ISIO (CHNTMP, TEMP, LDEPT, STORE, LOKCTL)
	CLEAR C_AMT

OUTDPT,
	SAVDPT = LDEPT
	CLEAR C_AMT
	RETURN
;-------------------------------------------


ENDOFF,
	CALL CLOSE
	IF (CHN182) CLOSE CHN182
	XCALL PGCHN ('CP:OPNMNU',1)
	STOP


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK

	CLEAR CHN041, CHN044, CHN045, CHN175, CHN071, CHN072, CHN182, CHN193

	SWITCH = 5
	XCALL FILES (41, 'SI', 41, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 41

	SWITCH = 5
	XCALL FILES (4, 'SI', 44, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 4

	SWITCH = 5
	XCALL FILES (5,'SI',45, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 5

	SWITCH = 5
	XCALL FILES (75, 'SI', 175, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN175 = 75

	SWITCH = 5
	XCALL FILES (71, 'SI', 071, SWITCH)	;JOBHDR
	IF (SWITCH .EQ. 9) RETURN
	CHN071 = 71

	SWITCH = 5
	XCALL FILES (72, 'SI', 072, SWITCH)	;JOBLIN
	IF (SWITCH .EQ. 9) RETURN
	CHN072 = 72

	SWITCH = 5
	XCALL FILES (18, 'SI', 182, SWITCH)
	CHN182 = 18

	SWITCH = 5
	XCALL FILES (19, 'SI', 193, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN193 = 19

	ISAMC (TMPFIL, 50, 1, 'START=1, LENGTH=2, NODUPS, ASCEND')
	OPEN (10, SU, TMPFIL)
	CHNTMP = 10

	OPNOK = 1
	RETURN
;-------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN041) CLOSE CHN041
	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045
	IF (CHN175) CLOSE CHN175
	IF (CHN071) CLOSE CHN071
	IF (CHN072) CLOSE CHN072
	IF (CHN182) CLOSE CHN182
	IF (CHN193) CLOSE CHN193

	RETURN
;-------------------------------------------



