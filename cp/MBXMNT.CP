;MBXMNT.CP
;	COP TABLE MAINTENANCE
;	PRINTER MAILBOXES FOR A SALESMAN
;
RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'


RECORD	CHANNEL
	CHN182	,D2


RECORD	PRINT
	TITLE	,A*,	'PRINTER MAILBOXES FOR A SALESMAN TABLE'
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
		,A*,	'SLSMAN  MBOX'

RECORD	VARS
	ALPHA	,A12
	OPNOK	,D1
	BLANKS	,A15
	MBOX	,D1
	SLSMAN	,D2
	ST_MAN	,D2
	EN_MAN	,D2
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
	XCALL OUTPT (1,1,2,'PRINTER MAILBOXES FOR A SALESMAN TABLE',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO CLOSE

MENU,
	XCALL OUTPT (1,1,2,'PRINTER MAILBOXES FOR A SALESMAN TABLE',1)
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
	XCALL OUTPT (1,1,2,'PRINTER MAILBOXES FOR A SALESMAN TABLE',1)
	XCALL OUTPT ( 4,4,0,'1. SALESMAN #',1)
	XCALL OUTPT ( 6,4,0,'2. MAILBOX',1)
SLSMAN,
	XCALL INPUT (4,24,02,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	SLSMAN = ENTRY(1,2)
	GOTO (CHKTBL),CNGCTL
MBOX,
	XCALL INPUT (6,24,01,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	MBOX = ENTRY(1,1)
CHKTBL,
	CLEAR TBL_KEY
	TBL_KEY(1,2) = 'MB'
	MB_SLSM = SLSMAN
	MB_MBOX = MBOX
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
		BEGIN
		SELECT = 2
		CALL DSPREC
		GOTO ANYCNG
		END

	SELECT = 1			;ADD MODE
	CLEAR COPTBL

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
	GOTO (SLSMAN, MBOX),WHATNO
	GOTO ANYCNG

PROCES,
	CASE SELECT OF
	BEGINCASE
	1:	BEGIN
		TBLCOD = 'MB'
		MB_SLSM = SLSMAN
		MB_MBOX = MBOX
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
	ENTRY(1,2) = MB_SLSM,	'XX'
	XCALL OUTPT (4,24,1,ENTRY(1,2),1)
	XCALL OUTPT (6,24,1,MB_MBOX,1)

	RETURN
;-----------------------------------



;===================================================================
PRINT_TABLE,
;===================================================================
	LINCNT = 66
	PGCNT  = 0
	
PDISP,
	CNGCTL = 
	XCALL OUTPT (1,1,2,'PRINT MAILBOXES FOR A SALESMAN TABLE',1)
	XCALL OUTPT (4,4,0,'1. STARTING SLSMAN',1)
	XCALL OUTPT (6,4,0,'2. ENDING   SLSMAN',1)
ST_MAN,
	XCALL INPUT (4,27,02,00,'#E',ENTRY,INXCTL,1)
	GOTO (PDISP,PDONE),INXCTL
	ST_MAN = ENTRY(1,2)
	IF (ST_MAN .EQ. 0)
		BEGIN
		EN_MAN = 99
		XCALL OUTPT (4,25,1,'ALL',1)
		XCALL OUTPT (6,25,1,' ',1)
		GOTO P_ANY
		END
	GOTO (P_ANY),CNGCTL
EN_MAN,
	XCALL INPUT (6,25,02,00,'# ',ENTRY,INXCTL,1)
	GOTO (PDISP),INXCTL
	EN_MAN = ENTRY(1,2)
	IF (EN_MAN .EQ. 0)
		BEGIN
		EN_MAN = ST_MAN
		ENTRY(1,2) = EN_MAN, 'ZX'
		XCALL OUTPT (6,25,0,ENTRY(1,2),1)
		END
P_ANY,
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (P_PRINT,P_CNGBR),CNGCTL + 1
P_CNGBR,
	GOTO (ST_MAN,EN_MAN),WHATNO
	GOTO P_ANY

P_PRINT,
	CLEAR TBL_KEY
	TBLCOD = 'MB'
	MB_SLSM = ST_MAN
	FIND (CHN182,COPTBL,TBL_KEY)[ERR=PLOOP]

PLOOP,
	XCALL IOS (CHN182,COPTBL,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (TBLCOD .NE. 'MB') GOTO EOF
	IF (MB_SLSM .LT. ST_MAN) GOTO PLOOP
	IF (MB_SLSM .GT. EN_MAN) GOTO EOF


	PLINE (3,4) = TBLKEY(1,2)	;MB_SLSM
	PLINE (11,11) = MB_MBOX
;SLSMAN  TERR
;  XX     AA
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7         8
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
	
	


