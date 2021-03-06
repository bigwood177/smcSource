;OPNOR3.CP
; 3-18-14: created from the "open" order part of opnor2.cp
; 4-23-14: use new itmmas.ip_wgt (lbs) instead of dollars

; 1. If there is no DPTSTS record the ordlin/dept is complete.
;DDEPT 5-29-14


.DEFINE	NO_REC	0
.DEFINE	D_OPN,	1
.DEFINE	CMPLT,	2

record	out
	o_ord	,d6
		,a1
	o_lbs	,d10

RECORD	BLHEAD
	.INCLUDE 'DEF:RD178A.DEF'


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
	T_OPNORD	,D10	;OPEN
	T_OPNJOB	,D10	;OPEN JOBS

RECORD	HDR2
	,A*,'            -------OPEN--------       '
RECORD	HDR3
	,A*,'DEPT        ORDERS  FAB-TICKETS        TOTAL'
RECORD	HDR1
		,A*,	'THRU: '
	H2_DAT1	,A10

;            -------OPEN--------       -----COMPLETED-----
;DEPT        ORDERS  FAB-TICKETS       ORDERS  FAB-TICKETS
;AA     ZZZZ,ZZX.XX- ZZZZ,ZZX.XX- ZZZZ,ZZX.XX- ZZZZ,ZZX.XX- ZZZZ,ZZX.XX-
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
RECORD	PRINT
	TITLE	,A*,	'DEPARTMENT SUMMARY - OPEN' 
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
	CHN178	,D2
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
	TOT_OPNORD	,D10
	TOT_OPNJOB	,D10


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
	O_DEPTO_AMT	,D9
	O_DEPTP_AMT	,D9
	C_DEPTO_AMT	,D9
	C_DEPTP_AMT	,D9

	SAVDPT	,A2
	SDATE	,D8	;START DATE
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
	XCALL OUTPT (1,1,2,'DEPARTMENT SUMMARY',1)
;
	XCALL RDAT8(TODAY)
DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'DEPARTMENT SUMMARY REPORT',1)
	XCALL OUTPT (4,4,0,'1. THRU  DATE',1)
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

ANYCNG,
	CNGCTL = 2
	XCALL ANYCN (CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR),CNGCTL+1
CNGBR,
	GOTO (SDATE),WHATNO
	GOTO ANYCNG

PROCES,
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	CLEAR PGCNT
	LINCNT = 60

	CLEAR TOT_OPNORD, TOT_OPNJOB


	XDATE(1,4) = SDATE(5,8)
	XDATE(5,8) = SDATE(1,4)
	H2_DAT1 = XDATE,	'ZX/XX/XXXX'


	SYEAR = SDATE(1,4)

	CALL GET_JOBS
	CALL GET_OPEN_ORD			;OPEN ORDERS


	CALL CLOSE

	XCALL LPON (LPSW, SPLFIL)
	FIND (CHNTMP, TEMP, ^FIRST) [ERR=P_LOOP]
P_LOOP,
	READS (CHNTMP, TEMP, P_EOF)
	PLINE (1,4) = 	T_DEPT
	PLINE ( 8,19) = T_OPNORD, 'ZZZZ,ZZX.XX-'
	PLINE (21,32) = T_OPNJOB, 'ZZZZ,ZZX.XX-'
	PLINE (34,45) = (T_OPNORD+T_OPNJOB),  'ZZZZ,ZZX.XX-'
	CALL PRINT

	TOT_OPNORD = TOT_OPNORD + T_OPNORD
	TOT_OPNJOB = TOT_OPNJOB + T_OPNJOB
	GOTO P_LOOP

P_EOF,
	CALL PRINT
	PLINE (1,5) = 'TOTAL'
	PLINE ( 7,19) = TOT_OPNORD,  'Z,ZZZ,ZZX.XX-'
	PLINE (20,32) = TOT_OPNJOB,  'Z,ZZZ,ZZX.XX-'
	PLINE (33,45) = (TOT_OPNORD+TOT_OPNJOB),  'Z,ZZZ,ZZX.XX-'

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

GET_JOBS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; OPEN & CLOSED JOBS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR C_AMT, O_AMT, II
	SAVDPT = '-1'

	ORD_JOB = 'J'

	DISPLAY (15,$SCR_POS(2,64), "JOBS:")	
	FIND (CHN071, JOBHDR, ^FIRST) [ERR=GJ_LOOP]
GJ_LOOP,
	INCR II
	IF (II/500*500 .EQ. II) DISPLAY (15,$SCR_POS(2,70), DIS)
	XCALL IOS (CHN071, JOBHDR, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO GJ_EOF
	IF (JHRLDT .LE. 0) GOTO GJ_LOOP	;NOT RELEASED 
;as of 3-20-14 jhrcdt not set anywhere
;;;	IF (JHRCDT.GT.0 .AND. JHRCDT.LT.SDATE) GOTO GJ_LOOP	;CLOSED BEFOR START DATE
	IF (JHSTS .EQ. 'R') GOTO GJ_LOOP			;11-17-14

	LDEPT = JHDEPT		;for getdpt
	FIND (CHN072, JOBLIN, JHNUM) [ERR=JL_LOOP]
JL_LOOP,
	XCALL IOS (CHN072, JOBLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO JL_EOF
	IF (JLNUM .NE. JHNUM) GOTO GJ_LOOP	;NEXT HEADER
	IF (JLRCDT.GT.0 .AND. JLRCDT.LT.SDATE) GOTO JL_LOOP	;RCV'D PRIOR TO DATE = CLOSED
;;;	IF (JLRCDT.GT.0 .AND. JLRCDT.LT.SDATE) GOTO GJ_LOOP	;RCV'D PRIOR TO DATE = CLOSED
	IF (JHDEPT .NE. SAVDPT) CALL GETDPT
;;;	if (jhdept .eq. 'D') 
;;;		begin
;;;		writes (22, jobhdr)
;;;		end

	XCALL ISIO (CHN041, ITMMAS, JLITM, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR ITMMAS

	LINAMT = JLQTY * IP_WGT		;SSQ 4-23-14
	LINAMT = (JLQTY * PRICE)#1
	O_AMT = O_AMT + LINAMT


	GOTO JL_LOOP
JL_EOF,
GJ_EOF,
	CALL GETDPT

	RETURN
;-------------------------------------------


GET_OPEN_ORD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR ORDLIN, C_AMT, O_AMT, II
	SAVDPT = '-1'
	DUCT_OC = 'O'		;OPEN ORDERS

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

;;;	if (ldept .eq. 'C') writes (22, ordlin)
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
			opncmp = opncmp + linamt
			end
	(9),		limbo = limbo + linamt
;;;	(),		O_AMT = O_AMT + LINAMT	;OPEN
	(),		begin
			O_AMT = O_AMT + LINAMT	;OPEN
			if (ldept.eq.'C') 
				begin
				o_ord = lordno
				o_lbs = linamt
			;;;	writes(22,out)
				end
			end
	ENDUSING


	GOTO GO_LOOP
GO_EOF,
	CALL GETDPT
	CALL GET_DUCT
	RETURN

;-------------------------------------------
GET_HDR,
	XCALL IOS (CHN044, ORDHDR, READ, LOKCTL)
	IF (LOKCTL.NE.0) RETURN
	IF (OLOC.NE.'O') GOTO GET_HDR
	IF (OORDDT .GT. SDATE) GOTO GET_HDR	;PAST THRU DATE	
	IF (OSHDTE .GT. 0) GOTO GET_HDR		;SHIP DATE 5-13-14

	LOKCTL = 1
	XCALL ISIO (CHN178, BLHEAD, OORDNO, READ, LOKCTL)
	if (lokctl .eq. 0)
		begin
		IF (BHSHPD .GT. 0) GOTO GET_HDR	;ALREADY SHIPPED
		end
	lokctl = 0				;return value

	SAVDPT = '-1'

	RETURN
;-------------------------------------------
CHK_DPT_STS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; IS THIS OPERATION OPEN OR COMPLETE?
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR DPT_STS
	dpt_sts = 1	;default to open (03/18/14)
;;;	dpt_sts = 9	;default to limbo (01/06/14)
;;;	dpt_sts = 2	;default to complete (01/02/14)

	S_DEPT(1,1) = LDEPT(1,1)	;9-28-15
	xcall cvcan (ordlin, ocusno, chn182, is_vulcan)
	if (is_vulcan) s_dept(2,2) = 'V'
;;;	if (is_vulcan) ldept(2,2) = 'V'

	S_ORDN = LORDNO
;;; 9-28-15 >	S_DEPT = LDEPT

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


	TOTPRC = POUNDS			;SSQ 4-23-14
	TOTPRC = (POUNDS * GPRICE ) # 1

	DECMAL =			;SSQ 4-23-14 still need to figure this out
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

	(),		USING D_DEPT SELECT
			('O'),	O_DEPTO_AMT = O_DEPTO_AMT + TOTPRC
			('P'),	O_DEPTP_AMT = O_DEPTP_AMT + TOTPRC
			ENDUSING
	ENDUSING

	GOTO GD_LOOP

GD_EOF,

	SAVDPT = 'O'
	O_AMT = O_DEPTO_AMT
	CALL GETDPT

	SAVDPT = 'P'
	O_AMT = O_DEPTP_AMT
	CALL GETDPT


	RETURN
;-------------------------------------------


GETDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (SAVDPT .EQ. '-1') GOTO OUTDPT
	IF (O_AMT.EQ.0) GOTO OUTDPT

	XCALL ISIO (CHNTMP, TEMP, SAVDPT, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO NEWDPT

	USING ORD_JOB SELECT
	('J'),	BEGIN
		T_OPNJOB = T_OPNJOB + O_AMT
		END
	('O'),	BEGIN
		T_OPNORD = T_OPNORD + O_AMT
		END
	ENDUSING

	XCALL ISIO (CHNTMP, TEMP, SAVDPT, WRITE, LOKCTL)
;;;	XCALL ISIO (CHNTMP, TEMP, LDEPT, WRITE, LOKCTL)

	SAVDPT = LDEPT
	CLEAR O_AMT, C_AMT
	RETURN
;-------------------------------------------
NEWDPT,
	CLEAR TEMP
	T_DEPT = SAVDPT


	USING ORD_JOB SELECT
	('J'),	BEGIN
		T_OPNJOB = O_AMT
		END
	('O'),	BEGIN
		T_OPNORD = O_AMT
		END
	ENDUSING
	
	XCALL ISIO (CHNTMP, TEMP, SAVDPT, STORE, LOKCTL)
;;;	XCALL ISIO (CHNTMP, TEMP, LDEPT, STORE, LOKCTL)
	CLEAR O_AMT, C_AMT

OUTDPT,
	SAVDPT = LDEPT
	CLEAR O_AMT, C_AMT
	RETURN
;-------------------------------------------


ENDOFF,
	CALL CLOSE
	IF (CHN182) CLOSE CHN182
	XCALL PGCHN ('CP:OPNMNU',1)
	STOP


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	open (22,o,'opnor3.dat')

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

	SWITCH = 5
	XCALL FILES (20, 'SI', 178, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN178 = 20

	ISAMC (TMPFIL, 22, 1, 'START=1, LENGTH=2, NODUPS, ASCEND')
	OPEN (10, SU, TMPFIL)
	CHNTMP = 10

	OPNOK = 1
	RETURN
;-------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	close 22

	IF (CHN041) CLOSE CHN041
	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045
	IF (CHN175) CLOSE CHN175
	IF (CHN071) CLOSE CHN071
	IF (CHN072) CLOSE CHN072
	IF (CHN178) CLOSE CHN178
	IF (CHN182) CLOSE CHN182
	IF (CHN193) CLOSE CHN193

	RETURN
;-------------------------------------------



