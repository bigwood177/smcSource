;VVCMNT.CP

;	COP TABLE MAINTENANCE
;	VULCAN PARAMETERS
;
RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'


RECORD	CHANNEL
	CHN182	,D2
	chnwrk	,d2

RECORD	PRINT
	TITLE	,A*,	'VULCAN PARAMETERS'
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


RECORD	HD1
		,A*,'  KEY    F3     SB    SR    W16   W18-20  W22-26'


RECORD	VARS
	COL	,D2
	J	,D2
	F3	,D5
	A4	,A4
	A5	,A5
	A6	,A6
	A30	,A30
	ALPHA	,A12
	OPNOK	,D1
	BLANKS	,A30
	DEPT	,D4
	DESC	,A30
	LOC	,A20	
	ST_DPT	,D4
	EN_DPT	,D4
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
	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'VULCAN PARAMETERS TABLE',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO CLOSE

MENU,
	CLEAR DEPT
	XCALL OUTPT (1,1,2,'VULCAN PARAMETERS TABLE',1)
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
	XCALL OUTPT (1,1,2,'VULCAN PARAMETERS TABLE',1)
	XCALL OUTPT ( 4,4,0,' 1. KEY',1)
	XCALL OUTPT ( 6,4,0,' 2. F3',1)

	XCALL OUTPT ( 8,4,0,' 3. SB',1)
	XCALL OUTPT ( 9,4,0,' 4. SR',1)

	XCALL OUTPT (13,4,0,' 5. W16',1)
	XCALL OUTPT (14,4,0,' 6. W18-20',1)
	XCALL OUTPT (15,4,0,' 7. W22-26',1)
	

	IF (DEPT .NE. 0) 
		BEGIN
		A4 = DEPT,	'ZZZX' [LEFT]	
		XCALL OUTPT (4,13,0,A4,1)
		XCALL OUTPT (4,18,0,DESC,1)
		GOTO F3
		END
DEPT,
	XCALL INPUT (4,13,04,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	DEPT = ENTRY(1,4)
	CLEAR TBL_KEY
	TBLCOD = 'VD'
	VD_KEY = DEPT
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		XCALL MESAG ('KEY has not been setup',1)
		GOTO DISPLA
		END

	DESC = VD_DESC
	XCALL OUTPT (4,19,0,DESC,1)
F3,
	XCALL INPUT (6,13,05,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,DEPT),INXCTL
	F3 = ENTRY(1,5)

	CLEAR TBL_KEY
	TBLCOD = 'MM'
	TBLCOD = 'M3'
	MM_KEY = F3
	XCALL ISIO (CHNwrk, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		XCALL MESAG ('F3 MEMO NOT IN TABLE',1)
		GOTO F3
		END

	XCALL OUTPT (6,19,1,MM_SHORT,1)

	CLEAR TBL_KEY
	TBLCOD = 'V1'
	V1_KEY = DEPT
	V1_F3 = F3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0)
		BEGIN
		SELECT = 2
		CALL DSPREC
		GOTO ANYCNG
		END

	SELECT = 1
	CLEAR COPTBL
SB,
	XCALL INPUT (08,13,05,00,'#-',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	V1_SB = ENTRY(1,5)
	A6 = V1_SB,	'X.XXX-'
	XCALL OUTPT (8,13,1,A6,1)
	GOTO (ANYCNG),CNGCTL
SR,
	XCALL INPUT (09,13,05,00,'#-',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	V1_SR = ENTRY(1,5)
	A6 = V1_SR,	'X.XXX-'
	XCALL OUTPT (9,13,1,A6,1)
	GOTO (ANYCNG),CNGCTL
		
W16,
	COL = 8
	FOR J FROM 1 THRU 4
		BEGIN
		COL = COL + 9	
		XCALL INPUT (13,COL,05,00,'#-',ENTRY,INXCTL,1)
		GOTO (DISPLA),INXCTL
		IF (ENTRY .NE. A30) 	V1_16(J) = ENTRY(1,5)
		A6 = V1_16(J),	'X.XXX-'
		XCALL OUTPT (13,COL,1,A6,1)
		END
	GOTO (ANYCNG),CNGCTL
W1820,
	COL = 8
	FOR J FROM 1 THRU 4
		BEGIN
		COL = COL + 9
		XCALL INPUT (14,COL,05,00,'#-',ENTRY,INXCTL,1)
		GOTO (DISPLA),INXCTL
		IF (ENTRY .NE. A30) V1_1820(J) = ENTRY(1,5)
		A6 = V1_1820(J),	'X.XXX-'
		XCALL OUTPT (14,COL,1,A6,1)
		END
	GOTO (ANYCNG),CNGCTL
W2226,
	COL = 8
	FOR J FROM 1 THRU 4
		BEGIN
		COL = COL + 9
		XCALL INPUT (15,COL,05,00,'#-',ENTRY,INXCTL,1)
		GOTO (DISPLA),INXCTL
		IF (ENTRY .NE. A30) V1_2226(J) = ENTRY(1,5)
		A6 = V1_2226(J),	'X.XXX-'
		XCALL OUTPT (15,COL,1,A6,1)
		END
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
	GOTO (DEPT, F3),WHATNO
	GOTO (SB, SR, W16, W1820, W2226), WHATNO-2
	GOTO ANYCNG

PROCES,
	CASE SELECT OF
	BEGINCASE
	1:	BEGIN
		TBLCOD = 'V1'
		V1_KEY = DEPT
		V1_F3 = F3
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,STORE,LOKCTL)
		END
	2:	XCALL ISIO (CHN182,COPTBL,TBL_KEY,WRITE,LOKCTL)
	ENDCASE
	GOTO DISPLA

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:VULMNU',1)
	STOP


DSPREC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; DISPLAY RECORD
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	A6 = V1_SB,	'X.XXX-'
	XCALL OUTPT (8,13,1,A6,1)

	A6 = V1_SR,	'X.XXX-'
	XCALL OUTPT (9,13,1,A6,1)

	COL = 8
	FOR J FROM 1 THRU 4
		BEGIN
		COL = COL + 9
		A6 = V1_16(J),	'X.XXX-'
		XCALL OUTPT (13,COL,1,A6,1)
		END

	COL = 8
	FOR J FROM 1 THRU 4
		BEGIN
		COL = COL + 9
		A6 = V1_1820(J),	'X.XXX-'
		XCALL OUTPT (14,COL,1,A6,1)
		END

	COL = 8
	FOR J FROM 1 THRU 4
		BEGIN
		COL = COL + 9
		A6 = V1_2226(J),	'X.XXX-'
		XCALL OUTPT (15,COL,1,A6,1)
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
	XCALL OUTPT (1,1,2,'PRINT VULCAN DESCRIPTIONS TABLE',1)
	XCALL OUTPT (4,4,0,'1. STARTING KEY',1)
	XCALL OUTPT (6,4,0,'2. ENDING   KEY',1)
ST_DPT,
	XCALL INPUT (4,27,04,00,'#E',ENTRY,INXCTL,1)
	GOTO (PDISP,PDONE),INXCTL
	ST_DPT = ENTRY(1,4)
	IF (ST_DPT .EQ. 0)
		BEGIN
		EN_DPT = 9999
		XCALL OUTPT (4,25,1,'ALL',1)
		XCALL OUTPT (6,25,1,' ',1)
		GOTO P_ANY
		END
	GOTO (P_ANY),CNGCTL
EN_DPT,
	XCALL INPUT (6,25,04,00,'# ',ENTRY,INXCTL,1)
	GOTO (PDISP),INXCTL
	EN_DPT = ENTRY(1,4)
	IF (EN_DPT .EQ. 0)
		BEGIN
		EN_DPT = ST_DPT
		A4 = EN_DPT,	'ZZZX' [LEFT]
		XCALL OUTPT (6,25,0,A4,1)
		END
P_ANY,
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (P_PRINT,P_CNGBR),CNGCTL + 1
P_CNGBR,
	GOTO (ST_DPT,EN_DPT),WHATNO
	GOTO P_ANY

P_PRINT,
	CLEAR TBL_KEY
	TBLCOD = 'V1'
	VD_KEY = ST_DPT
	FIND (CHN182,COPTBL,TBL_KEY)[ERR=PLOOP]

PLOOP,
	XCALL IOS (CHN182,COPTBL,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (TBLCOD .NE. 'V1') GOTO EOF
	IF (VD_KEY .LT. ST_DPT) GOTO PLOOP
	IF (VD_KEY .GT. EN_DPT) GOTO EOF


	PLINE (2,4) = V1_KEY, 'ZZZX'	;DD_DEPT
	PLINE (7,11) = V1_F3,	'ZZZZX'
	PLINE (14,18) = V1_SB,	'X.XXX'
	PLINE (20,24) = V1_SR,	'X.XXXX'
	PLINE (27,31) = V1_16,	'X.XXX'
	PLINE (36,40) = V1_1820,	'X.XXX'
	PLINE (44,48) = V1_2226,	'X.XXX'

;  KEY    F3     SB    SR    W16   W18-20  W22-26
; ZZZX ZZZZX  X.XXX X.XXX  X.XXX    X.XXX   X.XXX
;123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5
	CALL PRINT
	GOTO PLOOP

EOF,
	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
	PRNTON  = 0

PDONE,
	GOTO MENU

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (PRNTON .EQ. 0) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD,HD
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

	switch = 5
	xcall files (2, 'SI', 182, switch)
	if (switch .eq. 9) return
	chnwrk = 2

	OPNOK = 1
	RETURN
;----------------------------------------------------

CLOSE,
	if (chn182) CLOSE CHN182
	if (chnwrk) close chnwrk
	RETURN
;----------------------------------------------------
	


