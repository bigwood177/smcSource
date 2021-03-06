;TRTMNT.CP
;
;	ROUTINES FOR QUOTE EXPRESS DATA CONVERSION
;
;
RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

	.include 'def:fabmat.def'

RECORD	CHANNEL
	CHN182	,D2


RECORD	PRINT
	TITLE	,A*,	'QUOTE EXPRRESS ROUTINES TABLE'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PLINE	,A80
	PRNTON	,D1
	LINCNT	,D2,60
	PGCNT	,D6
	LPSW	,D2
	SPLFIL	,A14
	RPTNUM	,D3
	PRTTYP	,A1
	PRTCTL	,D3,080
	LPARG	,D1
	PRNTSW	,D1
	PRTCTR	,D1


RECORD	HD0
	,A*,	'FOR MATERIAL: '
	HD0_MAT	,A7

RECORD	HD1
	,A*,	'PC CODE  RTN  DESC                           '
	,A*,	'MATL  DMPR  CONICL  SCOOP  S-OFF'

;;;RECORD	CFGMAT
;;;	.INCLUDE 'DEF:CFGMAT.DEF'

RECORD	TMPLT
	TN	,D1
		,A1
	TD	,A15

RECORD
	I_POS	,9A3
RECORD,X
	I_SEQ	,A12

RECORD	VARS
	M_MAX	,D2,	09
	matno	,d1
	YESNO	,2A1,	'N','Y'
	ALPHA	,A12
	OPNOK	,D1
	BLANKS	,A15
	PC	,D2
	CODE	,D2
	RTN	,A5
	DAMP	,D1
	CONE	,D1
	SCOP	,D1
	MATL	,D1
	SAVPC	,D1
	SAVMAT	,D1	
	ST_PC	,D2
	EN_PC	,D2
	PGM	,D1
	ROW	,D2
	ENTRY	,A30
	INXCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	LOKCTL	,D1
	WHATNO	,D2
	SELECT	,D1
	CNGCTL	,D1
	I	,D3
	J	,D6
	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'QUOTE EXPRESS ROUTINES TABLE',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO CLOSE

MENU,
	XCALL OUTPT (1,1,2,'QUOTE EXPRESS ROUTINES TABLE',1)
	XCALL OUTPT (3,9,0,'PLEASE SELECT APPLICATION',1)
	XCALL OUTPT (5,15,0,'1. TABLE MAINTENANCE',1)
	XCALL OUTPT (6,15,0,'2. PRINT TABLE',1)
MINPUT,
	XCALL INPUT (3,36,1,1,'#E',ENTRY,INXCTL,1)
	GOTO (MINPUT,ENDOFF), INXCTL
	PGM = ENTRY(1,1)
	GOTO (DISPLA,PRINT_TABLE),PGM
	GOTO MINPUT

DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'QUOTE EXPRESS ROUTINES TABLE',1)
	XCALL OUTPT ( 4,4,0,'1. MATERIAL',1)
	XCALL OUTPT ( 6,4,0,'2. PRESSURE FILE',1)
	XCALL OUTPT ( 8,4,0,'3. CODE',1)
	XCALL OUTPT (10,4,0,'4. ROUTINE',1)
	XCALL OUTPT (10,31,0,'5. DESC',1)

	XCALL OUTPT (12,4,0,'6. DAMPER (Y/N)',1)
	XCALL OUTPT (14,4,0,'7. CONICAL (Y/N',1)
	XCALL OUTPT (16,4,0,'8. SCOOP (Y/N)',1)
	XCALL OUTPT (18,4,0,'9. STANDOFF (Y/N)',1)
	
	XCALL OUTPT (20,3,0,'10. ANGLE',1)

	XCALL OUTPT (12,40,0,'11.',1)
	XCALL OUTPT (14,40,0,'12.',1)
	XCALL OUTPT (16,40,0,'13.',1)
	XCALL OUTPT (18,40,0,'14.',1)
	XCALL OUTPT (20,40,0,'15.',1)
	XCALL OUTPT (22,40,0,'16.',1)

MATL,
	XCALL OUTPT (4,24,1,' ',1)
	FOR I FROM 1 THRU M_MAX
		BEGIN
		TN = I
		TD = fmt_mat(I)
		XCALL OUTPT (4+I-1, 30, 0, TMPLT, 1)
		END

	XCALL INPUT(4,24,01,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	MATL = ENTRY(1,1)
	IF (MATL .EQ. 0) MATL = SAVMAT
	SAVMAT = MATL
	USING MATL SELECT
	(0),	BEGIN
		MATL = 1	;GALV
		XCALL OUTPT (4,24,0,'1',1)
		END
	(.GT. M_MAX),	GOTO MATL
	ENDUSING

	FOR I FROM 1 THRU M_MAX
		BEGIN
		XCALL OUTPT (4+I-1, 30, 1, ' ', 1)
		END

	XCALL OUTPT (4,30,0,FMT_MAT(MATL),1)
	GOTO (CHKTBL),CNGCTL

PC,
	XCALL INPUT (6,24,02,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	PC = ENTRY(1,2)
	IF (PC .EQ. 0)
		BEGIN
		PC = SAVPC
		IF(PC .EQ. 0)PC = 1
		ENTRY(1,1) = PC,	'X'
		XCALL OUTPT (6,24,0,ENTRY(1,1),1)
		END

	CLEAR TBL_KEY
	TBL_KEY(1,2) = 'FC'
	FC_PC = PC
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		XCALL MESAG ('PRESSURE FILE # HAS NOT BEEN SET UP',1)
		GOTO PC
		END

	XCALL OUTPT (6,29,1,FC_DESC,1)
	SAVPC = PC
	GOTO (CHKTBL),CNGCTL
CODE,
	XCALL INPUT (8,24,02,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	CODE = ENTRY(1,2)
	IF (CODE .LE. 0) GOTO CODE
CHKTBL,
	CLEAR TBL_KEY
	TBL_KEY(1,2) = 'RT'
	RT_MATL = MATL
	RT_PC = PC
	RT_CODE = CODE
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
		BEGIN
		SELECT = 2
		CALL DSPREC
		GOTO ANYCNG
		END

	SELECT = 1			;ADD MODE
	CLEAR COPTBL
	rt_matl = matl
	rt_pc = pc
	rt_code = code

RTN,
	XCALL INPUT(10,24,03,00,'A ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	RT_RTN = ENTRY(1,3)
	GOTO (ANYCNG),CNGCTL

DESC,
	XCALL INPUT(10,42,30,00,'A ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	RT_DESC = ENTRY(1,30)
	GOTO (ANYCNG),CNGCTL

DAMP,
	XCALL INPUT (12,24,01,00,'YN',ENTRY,INXCTL,1)
	IF (ENTRY .EQ. 'Y')
	THEN RT_DAMP = 1
	ELSE RT_DAMP = 0
	GOTO (ANYCNG),CNGCTL
CONE,
	XCALL INPUT (14,24,01,00,'YN',ENTRY,INXCTL,1)
	IF (ENTRY .EQ. 'Y')
	THEN RT_CONE = 1
	ELSE RT_CONE = 0
	GOTO (ANYCNG),CNGCTL
SCOP,
	XCALL INPUT (16,24,01,00,'YN',ENTRY,INXCTL,1)
	IF (ENTRY .EQ. 'Y')
	THEN RT_SCOP = 1
	ELSE RT_SCOP = 0
	GOTO (ANYCNG),CNGCTL
SOFF,
	XCALL INPUT (18,24,01,00,'YN',ENTRY,INXCTL,1)
	IF (ENTRY .EQ. 'Y')
	THEN RT_SOFF = 1
	ELSE RT_SOFF = 0
	GOTO (ANYCNG),CNGCTL

ANG,
	XCALL INPUT (20,24,02,00,'# ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	RT_ANG = ENTRY(1,2)
	USING RT_ANG SELECT
	(0), 	NOP
	(45),	NOP
	(90),	NOP
	(),	GOTO ANG
	ENDUSING
	GOTO (ANYCNG),CNGCTL
RTA1,
	XCALL OUTPT (12,40,0,'11.',1)
	XCALL INPUT (12,45,02,00,'A ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	RT_A2(1) = ENTRY(1,2)
	GOTO (ANYCNG),CNGCTL
RTA2,
	XCALL OUTPT (14,40,0,'12.',1)
	XCALL INPUT (14,45,02,00,'A ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	RT_A2(2) = ENTRY(1,2)
	GOTO (ANYCNG),CNGCTL
RTA3,
	XCALL INPUT (16,45,02,00,'A ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	RT_A2(3) = ENTRY(1,2)
	GOTO (ANYCNG),CNGCTL
RTA4,
	XCALL INPUT (18,45,02,00,'A ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	RT_A2(4) = ENTRY(1,2)
	GOTO (ANYCNG),CNGCTL
RTA5,
	XCALL INPUT (20,45,02,00,'A ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	RT_A2(5) = ENTRY(1,2)
	GOTO (ANYCNG),CNGCTL
RTA6,
	XCALL INPUT (22,45,02,00,'A ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	RT_A2(6) = ENTRY(1,2)
	GOTO (ANYCNG),CNGCTL
	
ANYCNG,
	XCALL OUTPT (24,1,1,'FIELD # TO CHANGE            <TAB> = DELETE',1)
	XCALL INPUT (24,20,02,00,'#T',ENTRY,INXCTL,1)
	IF (INXCTL .EQ. 3)
		BEGIN
		XCALL OUTPT (24,1,1,'DELETE, ARE YOU SURE ?',1)
		XCALL INPUT (24,24,01,01,'YN',ENTRY,INXCTL,1)
		GOTO (ANYCNG),INXCTL-1
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,DELETE,LOKCTL)
		GOTO DISPLA
		END

	WHATNO = ENTRY(1,2)
	IF (WHATNO .EQ. 0)
	THEN 	CLEAR CNGCTL
	ELSE 	CNGCTL = 1

	GOTO (PROCES,CNGBR),CNGCTL+1
CNGBR,
	GOTO (MATL, PC, CODE, RTN, DESC, DAMP, CONE, SCOP, SOFF, ANG),WHATNO
	GOTO (RTA1, RTA2, RTA3, RTA4, RTA5, RTA6), WHATNO - 10
	GOTO ANYCNG

PROCES,

	CASE SELECT OF
	BEGINCASE
	1:	BEGIN
		TBLCOD = 'RT'
		RT_MATL = MATL
		RT_PC = PC
		RT_CODE = CODE
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,STORE,LOKCTL)
		END
	2:	XCALL ISIO (CHN182,COPTBL,TBL_KEY,WRITE,LOKCTL)
	ENDCASE
	GOTO DISPLA

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:TBLMNU',1)
	STOP


DSPREC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; DISPLAY RECORD
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ENTRY(1,1) = RT_MATL,	'X'
	XCALL OUTPT (4,24,0,ENTRY(1,1),1)
	XCALL OUTPT (4,30,0,FMT_MAT(RT_MATL),1)

	ENTRY(1,2) = RT_PC,	'ZZ' [LEFT]
	XCALL OUTPT (6,24,0,ENTRY(1,2),1)

	ENTRY(1,3) = RT_CODE,	'ZZZ' [LEFT]
	XCALL OUTPT (8,24,0,ENTRY(1,3),1) 

	XCALL OUTPT (10,24,0,RT_RTN,1)
	XCALL OUTPT (10,42,0,RT_DESC,1)

	XCALL OUTPT (12,24,0,YESNO(RT_DAMP+1),1)

	XCALL OUTPT (14,24,0,YESNO(RT_CONE+1),1)

	XCALL OUTPT (16,24,0,YESNO(RT_SCOP+1),1)

	XCALL OUTPT (18,24,0,YESNO(RT_SOFF+1),1)

	ENTRY(1,2) = RT_ANG,	'ZZ'
	XCALL OUTPT (20,24,0,ENTRY(1,2),1)

	CLEAR I
	FOR ROW FROM 12 THRU 20 BY 2
		BEGIN
		INCR I
		XCALL OUTPT (ROW, 45, 0, RT_A2(I), 1)
		END
	RETURN
;-----------------------------------



;===================================================================
PRINT_TABLE,
;===================================================================
	LINCNT = 66
	PGCNT  = 0
	
PDISP,
	CNGCTL = 
	XCALL OUTPT (1,1,2,'PRINT QUOTE EXPRESS ROUTINES TABLE',1)
	XCALL OUTPT (4,4,0,'1. FOR MATERIAL: ',1)
;                        123456789012345678901234567890
ST_PC,
	XCALL INPUT (4,21,01,00,'#E',ENTRY,INXCTL,1)
	GOTO (PDISP,PDONE),INXCTL
	MATL = ENTRY(1,1)
	IF (MATL.LT.1 .OR. MATL.GT.9) MATL = 1
	XCALL OUTPT (4,30,1,FMT_MAT(matl),1)
P_ANY,
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (P_PRINT,P_CNGBR),CNGCTL + 1
P_CNGBR,
	GOTO (ST_PC),WHATNO
	GOTO P_ANY

P_PRINT,

	HD0_MAT = FMT_MAT(MATL)

	CLEAR TBL_KEY
	TBLCOD = 'RT'
	RT_MATL = MATL
;;;	TR_SLSM = ST_PC
	FIND (CHN182,COPTBL,TBL_KEY)[ERR=PLOOP]

PLOOP,
	XCALL IOS (CHN182,COPTBL,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (TBLCOD .NE. 'RT') GOTO EOF
	IF (RT_MATL .NE. MATL) GOTO EOF
;;;	IF (RT_PC .LT. ST_PC) GOTO PLOOP
;;;	IF (RT_PC .GT. EN_PC) GOTO EOF


	PLINE (1,2)  = RT_PC,	'ZX'
	PLINE (5,7)  = RT_CODE,	'ZZX'
	PLINE(10,12) = RT_RTN
	PLINE(15,44) = RT_DESC
;;;	PLINE(46,49) = C_SHORT(RT_MATL)
	PLINE(53,53) = YESNO(RT_DAMP+1)
	PLINE(61,61) = YESNO(RT_CONE+1)
	PLINE(69,69) = YESNO(RT_CONE+1)
	PLINE(75,75) = YESNO(RT_SOFF+1)
	PLINE(78,79) = RT_ANG,	'ZZ'

;PC CODE  RTN  DESC                           MATL  DMPR  CONICL  SCOOP  S-OFF
;XX  XXX  AAA  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA AAAA   Y       Y       Y     Y
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7         8
	CALL PRINT

	CLEAR J
	FOR I FROM 15 THRU 30 BY 3
		BEGIN
		INCR J
		PLINE(I,I+1) = RT_A2(J)
		END
	CALL PRINT
	GOTO PLOOP

EOF,
	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
	PRNTON  = 0

PDONE,
	GOTO MENU

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (PRNTON .EQ. 0) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD0,HD1,HD
&		,LG,LG,LG,0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;-------------------------------------------------------------
PRNTON,
	SPLFIL (5,6) = 'EF'
	LPSW = 1		;PRINT,SPOOL, OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	PRNTON = 1
	RETURN
;-------------------------------------------------------------
;===================================================================


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1,'SU',182,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 1

	OPNOK = 1
	RETURN
;----------------------------------------------------

CLOSE,
	CLOSE CHN182
	RETURN
;----------------------------------------------------
	
	


