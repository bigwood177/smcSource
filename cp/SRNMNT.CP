;SRNMNT.CP
;	COP TABLE MAINTENANCE
;	SERVER NAME AND IP ADDRESS
;
RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'


RECORD	CHANNEL
	CHN182	,D2


RECORD	PRINT
	TITLE	,A*,	'SERVER NAME AND IP ADDR'
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
		,A*,	' CMP  SERVER NAME          IP ADDRESS'


RECORD	VARS
	A3	,A3
	A20	,A20
	ALPHA	,A20
	OPNOK	,D1
	BLANKS	,A30
	CMP	,A3
	SERVER	,A20
	IP	,A15
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
	XCALL OUTPT (1,1,2,TITLE,1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO CLOSE

MENU,
	XCALL OUTPT (1,1,2,TITLE,1)
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
	XCALL OUTPT (1,1,2,TITLE,1)
	XCALL OUTPT ( 4,4,0,'1. COMPANY',1)
	XCALL OUTPT ( 6,4,0,'2. SERVER NAME',1)
;;;	XCALL OUTPT ( 6,40,0,'EXACTLY 9 CHARACTERS',1)
	XCALL OUTPT ( 8,4,0,'3. IP ADDRESS',1)

CMP,
	XCALL INPUT (4,24,03,00,'AE',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	CMP = ENTRY(1,3)
	CLEAR TBL_KEY
	TBLCOD = 'SR'
	SR_CMP = CMP
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
	THEN	BEGIN
		SELECT = 2
		CALL DSPREC
		GOTO ANYCNG
		END
	ELSE	BEGIN
		SELECT = 1
		CLEAR COPTBL
		SR_CMP = CMP
		END

	GOTO (ANYCNG),CNGCTL

SERVER,
	XCALL INPUT (6,24,12,00,'AE',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	SR_SERVER = ENTRY(1,12)
;;;	IF (%TRIM(SR_SERVER) .NE. 9) GOTO SERVER
	GOTO (ANYCNG), CNGCTL

IP,
	XCALL INPUT (8,24,15,00,'AE',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	SR_IP = ENTRY(1,15)
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
	GOTO (CMP, SERVER, IP),WHATNO
	GOTO ANYCNG

PROCES,
	CASE SELECT OF
	BEGINCASE
	1:	BEGIN
		TBLCOD = 'SR'
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

	XCALL OUTPT (4,21,1,SR_CMP,1)
	XCALL OUTPT (6,24,1,SR_SERVER,1)
	XCALL OUTPT (8,24,1,SR_IP,1)


	RETURN
;-----------------------------------



;===================================================================
PRINT_TABLE,
;===================================================================
	LINCNT = 66
	PGCNT  = 0
	
PDISP,
	CNGCTL = 
	XCALL OUTPT (1,1,2,TITLE,1)
P_PRINT,
	CLEAR TBL_KEY
	TBLCOD = 'SR'
	FIND (CHN182,COPTBL,TBL_KEY)[ERR=PLOOP]

PLOOP,
	XCALL IOS (CHN182,COPTBL,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (TBLCOD .NE. 'SR') GOTO EOF

	CLEAR PLINE
	PLINE (2,4) = SR_CMP
	PLINE (7,26) = SR_SERVER
	PLINE (28,42) = SR_IP


; CMP  SERVER NAME          IP ADDRESS
; AAA  AAAAAAAAAAAAAAAAAAAA AAAAAAAAAAAAAAA  
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

	OPNOK = 1
	RETURN
;----------------------------------------------------

CLOSE,
	CLOSE CHN182
	RETURN
;----------------------------------------------------
	


