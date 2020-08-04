;  NUVRPT.PO / POR - D11
;
;		CREATED: 22-JUN-95
;		AUTHOR:  DIANA K. SCHEPPA
;
;		VENDORY INVENTORY REPORT BY COMMODITY CODE & NUAIRE PART #
;
; REVISIONS:
;
;**********************************************************************
;		PURCHASE ORDER PROCESSING
;**********************************************************************
;:
;		COMPILE & LINK PER DOCUMENTATION INSTALLATION NOTES.
;
;
;
RECORD VIPART
	.INCLUDE 'DEF:RD061A.DEF'

RECORD ITMMAS
	.INCLUDE 'DEF:RD041A.DEF'

RECORD ITMCTL
	.INCLUDE 'DEF:RD041B.DEF'

RECORD ITMCDX
	.INCLUDE 'DEF:RD239A.DEF'

RECORD PLINE
 		,A132
RECORD LINE1,X
	KPART	,A15
		,A5
	KVNO	,A6
		,A5
	KVPART	,A20

RECORD HDR1
		,A13,	'NUAIRE PART #'
		,A7
		,A8,	'VENDOR #'
		,A3
		,A13,	'VENDOR PART #'

RECORD LEGEND
		,A1
		,A20,'COMMODITY CODES FROM'
		,A1
	SNO	,A4
		,A3
		,A2,'TO'
		,A3
	ENO	,A4

RECORD VKEY
		,A15

RECORD
	CNGCTL	,D1
	WHATNO	,D1
	TNUM	,D3
	TMPREC	,D5
	ALL	,A12
	BLANKS	,A15
	BRACKS	,A30,']]]]]]]]]]]]]]]]]]]]]]]]]]]]]]'
	BSEND	,D5
	BSMID	,D5
	CNTR	,D3
	DSPSW	,D1
	EOF	,D1
	IDFLAG	,D1
	LINCNT	,D2,	60
	LINCTL	,D1
	LOKCTL	,D1
	LPSW	,D1
	NOABRT	,D1,	1
	PGCNT	,D3,	000
	PRTCTL	,D3
	PRTTYP	,A1
	READ	,D1,	0
	WRITE	,D1,1
	RPTNUM	,D3
	SPLFIL	,A14
	SRCOPT	,D1
	SRCCTL	,D1
	SWITCH	,D1
	TITLE	,A36,	'VENDOR/INVENTORY REPORT BY COMM CODE'
	TODAY	,D6
	V	,D1
	WTARG	,D1,	2
	RECNO	,D5
	NORECS	,D5
	INXCTL	,D1
	ENTRY	,A15
	SCODE	,A4
	ECODE	,A4
	YN	,D1


PROC
	XCALL TERID (V)
	XCALL OUTPT (2,1,2,'PRINT VENDOR/INVENTORY REPORT BY COMMODITY CODE',1)
	LINCNT = 60
	PGCNT = 0

GETSNO,
	XCALL OUTPT (4,20,2,'PLEASE ENTER STARTING COMMODITY CODE',V)
	XCALL INPUT (4,60,4,0,'AE',ENTRY,INXCTL,V)
	GO TO (GETSNO,ENDOFF), INXCTL
	SCODE = ENTRY (1,4)
	SNO = SCODE
GETENO,
	XCALL OUTPT (5,20,2,'PLEASE ENTER ENDING COMMODITY CODE',V)
	XCALL INPUT (5,60,4,0,'A ',ENTRY,INXCTL,V)
	GO TO (GETSNO,GETSNO), INXCTL
	ECODE = ENTRY (1,4)
	IF (ECODE.EQ.'    ') ECODE = SCODE
	XCALL OUTPT(5,60,0,ECODE,V)
	ENO = ECODE
	IF (ECODE.EQ.'  ') CALL ALLITM
	GOTO START
ALLITM,
	ALL = 'ALL CODES'
	SCODE=
	ECODE='ZZZZ'
	SNO = ALL
	ENO = 
	XCALL OUTPT(5,60,0,ALL,V)
	RETURN 
START,
	YN = INXCTL
	CNGCTL=2
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (GETSNO),CNGCTL
	XCALL WATE (4,V)
	XCALL RDATE (TODAY)
	SPLFIL(5,6) = 'N2'

	LPSW = 1
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GO TO EXIT
	IF (LPSW.EQ.2) WTARG = 4
	XCALL WATE (WTARG,V)
OPNFIL,
	SWITCH = 1
	XCALL FILES (2,'SI',61,SWITCH)			;FILE # 61 -- VIPART
	IF (SWITCH.EQ.9) GOTO ENDOFF
	
	SWITCH = 5
	XCALL FILES (3,'I',239,SWITCH)			;FILE # 239 -- ITMCDX

	SWITCH = 5
	XCALL FILES (4,'I',41,SWITCH)			;FILE # 41 -- ITMMAS

	LOKCTL = 1
	XCALL IO (4,ITMCTL,1,READ,LOKCTL)
	BSEND = ORG041
PRTRPT,
	XCALL WATE (2,V)
	SRCCTL = 
	XCALL SERCH(3,ITMCDX,SCODE,1,4,BSEND,BSMID,SRCCTL,4,5,10,0,0,0,0)
	GOTO (DONE),SRCCTL
	RECNO = BSMID
FNDFST,
	DECR RECNO
	IF (RECNO.LT.2) GOTO READ
	XCALL IO (3,ITMCDX,RECNO,READ,LOKCTL)
	IF (ICOMCD.EQ.SCODE) GOTO FNDFST
READ,
	INCR RECNO
	LOKCTL = 1
	XCALL IO (3,ITMCDX,RECNO,READ,LOKCTL)
	IF (ITMCDX.EQ.']]]]]]') GOTO DONE
	IF (LOKCTL .EQ. 2) GOTO DONE
	IF (ICOMCD.GT.ECODE) GO TO DONE

PRLOOP,
	VKEY = ICTMNO
	XCALL ISIO (2,VIPART,VKEY,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GOTO READ
	IF (VPART.EQ.']]]DEL') GOTO READ
CONTIN,
	KVNO = IVEN
	KVPART = VPART
	KPART = IPART
	CALL PRINT
RNEXT,
	XCALL IOS (2,VIPART,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GOTO DONE
	IF (VPART.EQ.']]]DEL') GOTO RNEXT
	IF (IPART.NE.ICTMNO) GOTO READ
	GOTO CONTIN
DONE,
	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	XCALL WATE (3,V)
	GO TO EXIT
PRINT,
	PRTCTL = 132
	LINCTL = 1
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,'NO HDR',' ',
&		LEGEND,'NO LEGEND',' ',LINCTL,132,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN

EXIT,
	XCALL FILES (2,'SI',61,4)
	XCALL FILES (3,'I',239,4)
	XCALL FILES (4,'I',31,4)
;**********************************************************************
ENDOFF,
	XCALL PGCHN ('PO:VIMENU',1)
;**********************************************************************

END

