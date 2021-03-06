;  POPRNT / POR - D11
;
;		PRINT PURCHASE ORDERS ON SPECIAL PURCHASE ORDER FORMS
;
;	(01) 10/04/95 DKS - MAKE P.O. FILES ISAM
;
RECORD HEADER	
	.INCLUDE 'DEF:RD151A.DEF'

RECORD POLINE	
	.INCLUDE 'DEF:RD152A.DEF'

RECORD TABLE	
	.INCLUDE 'DEF:RD153A.DEF'

RECORD DUMTBL	
	.INCLUDE 'DEF:RD153B.DEF'

RECORD VENDOR	
	.INCLUDE 'DEF:RD011A.DEF'

RECORD DUMVEN	
	.INCLUDE 'DEF:RD011B.DEF'

RECORD VENIDX	
	.INCLUDE 'DEF:RD012A.DEF'
RECORD INVCTL
	.INCLUDE 'DEF:RD041B.DEF'

RECORD KEYPO
	KEY1	,D6
	KEY2	,D2

RECORD PLINE
		,A80

RECORD LINE1,X
		,A63
	KPONUM	,A6
	KDASH	,A1
	KRLNUM	,A2
		,A3
	KPGNUM	,A2
		,A3

RECORD LINE2,X
		,A61
	KPODTE	,A8
		,A3
	KCXCDE	,A6
		,A2
RECORD LINE3,X
		,A71
	KCHDTE	,A8
		,A1
RECORD LINE4,X
		,A12
	KVNADR	,A30
		,A8
	KSHPTO	,A30
RECORD LINE6,X
		,A2
	KORTYP	,A7
		,A2
	KBUYER	,A16
		,A2
	KACKNO	,A3
		,A2
	KCONFM	,A3
		,A2
	KTERMS	,A10
		,A2
	KFOB	,A9
		,A2
	KSPVIA	,A15
		,A2
	KCLPPD	,A1
RECORD LINE7,X
		,A2
	KLNNUM	,A3
		,A2
	KQYORD	,A7
		,A2
	KUOFM	,A2
		,A2
	KOURNO	,A13
		,A2
	KVENNO	,A17
		,A2
	KPRICE	,A11
		,A2
	KRQDTE	,A8
		,A2
	KCXLIN	,A3
RECORD LINE8,X
		,A7
	KBLTYP	,A4
		,A9
	KLNOTE	,A30
		,A30
RECORD LINE10,X
		,A20
	KNOTE1	,A30
		,A2
	KNOTE2	,A27

RECORD CTL
	ROW	,D2
		,A1
	COL	,D2
		,A1
	MAX	,D2
		,A1
	MIN	,D2
		,A1
	TYPE	,A2

RECORD TBLKEY
	TKEYNO	,D2
	TKEYCD	,A2
	TKEYSQ	,D1

RECORD RANGE1
	STPONO	,D6
	STRLNO	,D2
	ENPONO	,D6
	ENRLNO	,D2

RECORD RANGE2
	STPODA	,D6
	ENPODA	,D6
	ORDTYP	,A1
	ISSTYP	,A1

RECORD	CHARS
	STX	,A1
	SOH	,A1
	ETX	,A1
	ENQ	,A1
	SEL	,A1
	CR	,A1
	FF	,A1
	DLE	,A1
	NAK	,A1
	ETB	,A1
	CAN	,A1
	ESC	,A1
	AFF	,A3
	CANCEL	,A5
	ADVANCE	,A5
	LFORMAT	,A5
	LPRINT	,A3

RECORD	E_BOLD
	E_ESC	,A1
		,A4,	'(s3B'	;bold
RECORD	E_MED
	M_ESC	,A1
		,A4,	'(s0B'	;medium

RECORD	LVARS
	LBL_DATA,A20
	TMPREC	,A20
	TL	,D2
	NUMLBL	,D2
	PRTLBL	,A1
	I	,D3

RECORD	VARS
	BLANKS	,A31
	BRACKS	,A30,	']]]]]]]]]]]]]]]]]]]]]]]]]]]]]]'
	BSMID	,D5
	CNGCTL	,D1
	DSPSW	,D1
	ENTRY	,A30
	EOF	,D1
	EPO	,D1
	HDATE	,D6
	HRECNO	,D5
	INXCTL	,D1
	ISSCDE	,3A1,	'N','C','X'
	ISSNAM	,3A8,	'ORIGINAL','CHANGE  ','CANCEL  '
	KLNCNT	,D2
	LINCNT	,D2,	60
	LINCTL	,D1
	LOKCTL	,D1
	LPSW	,D1
	LRECNO	,D5
	MUSTPT	,D1
	MSGCTL	,D1
	N	,D2
	NOABRT	,D1,	1
	PGCNT	,D3,	000
	PGNUM	,D2
	PRTCTL	,D3
	PRTTYP	,A1
	READ	,D1,	0
	RECNO	,D5
	RPTNUM	,D3
	SPLFIL	,A14
	SRCOPT	,D1
	SRCCTL	,D1
	SWITCH	,D1
	TITLE	,A2
	TODAY	,D6
	TYPCDE	,3A1,	'N','B','R'
	TYPNAM	,3A7,	'NORMAL ','BLANKET','RELEASE'
	V	,D1
	VADR	,5A35		;(01) WAS 4A31		
	WHATNO	,D2
	RJUST	,D1

PROC
	XCALL TERID (V)
	XCALL ASCII (27, E_ESC)
	M_ESC = E_ESC

	XCALL OUTPT (2,1,2,'\',1)
	XCALL WATE (4,V)
	XCALL RDATE (TODAY)

	SWITCH = 1
	XCALL FILES (10,'I',41,SWITCH)
	IF (SWITCH.EQ.9) GO TO INUSE1
	LOKCTL = 1
	XCALL IO (10,INVCTL,1,READ,LOKCTL)
	RJUST = JSTIFY
	XCALL FILES (10,'I',41,4)

	SWITCH = 5
	XCALL FILES (1,'SI',151,SWITCH)		; FILE 151, PORHDR.
	IF (SWITCH.EQ.9) GO TO INUSE1

	SWITCH = 5
	XCALL FILES (2,'SI',152,SWITCH)		; FILE 152, PORLIN.
	IF (SWITCH.EQ.9) GO TO INUSE2

	SWITCH = 1
	XCALL FILES (3,'I',153,SWITCH)		; FILE 153, PORTBL.
	IF (SWITCH.EQ.9) GO TO INUSE3

	LOKCTL = NOABRT
	XCALL IO (3,DUMTBL,1,READ,LOKCTL)

	SWITCH = 1
	XCALL FILES (4,'I',011,SWITCH)		; FILE 011, VENMAS.
	IF (SWITCH.EQ.9) GO TO INUSE4

	LOKCTL = NOABRT
	XCALL IO (4,DUMVEN,1,READ,LOKCTL)

	SWITCH = 1
	XCALL FILES (5,'I',012,SWITCH)		; FILE 012, VENIDX.
	IF (SWITCH.EQ.9) GO TO INUSE5

	CALL ASCII
	OPEN (13,O,'BAR_P2:')
BEGIN,
	RANGE1 =
	RANGE2 =
	CNGCTL =
	XCALL OUTPT (2,1,2,'\',1)
	XCALL OUTPT ( 4,25,0,'1. STARTING P/O NUMBER ',V)
	XCALL OUTPT ( 5,25,0,'2. ENDING P/O NUMBER ',V)
	XCALL OUTPT ( 6,25,0,'3. STARTING P/O DATE ',V)
	XCALL OUTPT ( 7,25,0,'4. ENDING P/O DATE ',V)
	XCALL OUTPT ( 8,25,0,'5. ORDER TYPE ',V)
	XCALL OUTPT ( 9,25,0,'6. ISSUE TYPE ',V)
	XCALL OUTPT (10,25,0,'7. PRINT LABELS ?',V)
STARPO,
	CTL = '04,49,06,00,#E'
	CALL INPUT
	GO TO (BEGIN,EXIT), INXCTL
	IF (ENTRY.EQ.BLANKS) GO TO ALLPO
	STPONO = ENTRY(1,6)
	ENTRY(1,6) = STPONO,'ZZZZXX'
	XCALL OUTPT (4,49,0,ENTRY(1,6),V)
	XCALL OUTPT (4,55,0,'-',V)

	CTL = '04,56,02,00,# '
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO BEGIN
	STRLNO = ENTRY(1,2)
	ENTRY(1,2) = STRLNO,'XX'
	XCALL OUTPT (4,56,0,ENTRY(1,2),V)
	IF (ENPONO.EQ.999999.AND.ENRLNO.EQ.99) GO TO ENDNO2
	IF (CNGCTL.NE.1) GO TO ENDNO2
	IF (ENPONO.LT.STPONO) GO TO MSG1
	IF (ENPONO.EQ.STPONO.AND.ENRLNO.LT.STRLNO) GO TO MSG1
	GO TO ANYCNG
ENDNO,
	IF (ENPONO.EQ.999999.AND.ENRLNO.EQ.99) GO TO STARPO
ENDNO2,
	CTL = '05,49,06,00,# '
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO BEGIN
	IF (ENTRY.EQ.BLANKS) ENTRY(1,6) = STPONO
	ENPONO = ENTRY(1,6)
	ENTRY(1,6) = ENPONO,'ZZZZXX'
	XCALL OUTPT (5,49,0,ENTRY(1,6),V)
	XCALL OUTPT (5,55,0,'-',V)

	CTL = '05,56,02,00,# '
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO BEGIN
	IF (ENTRY.EQ.BLANKS) ENTRY(1,2) = STRLNO,'XX'
	ENRLNO = ENTRY(1,2)
	ENTRY(1,2) = ENRLNO,'XX'
	XCALL OUTPT (5,56,0,ENTRY(1,2),V)
	IF (ENPONO.LT.STPONO) GO TO MSG1
	IF (ENPONO.EQ.STPONO.AND.ENRLNO.LT.STRLNO) GO TO MSG1
ENDNO3,
	GO TO (ANYCNG), CNGCTL
STDTE,
	CTL = '06,49,06,00,D '
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO BEGIN
	IF (ENTRY.EQ.BLANKS) GO TO ALLDTE
	HDATE = ENTRY(1,6)
	STPODA(1,2) = HDATE(5,6)
	STPODA(3,6) = HDATE(1,4)
	IF (ENPODA.EQ.999999) GO TO ENDTE2
	IF (CNGCTL.NE.1) GO TO ENDTE2
	IF (ENPODA.LT.STPODA) GO TO MSG2
	GO TO ANYCNG
ENDTE,
	IF (ENPODA.EQ.999999) GO TO STDTE
ENDTE2,
	CTL = '07,49,06,00,D '
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO BEGIN
	IF (ENTRY.EQ.BLANKS) GO TO SAMEDA
	HDATE = ENTRY(1,6)
	ENPODA(1,2) = HDATE(5,6)
	ENPODA(3,6) = HDATE(1,4)
	IF (ENPODA.LT.STPODA) GO TO MSG2
ENDTE3,
	GO TO (ANYCNG),CNGCTL
ORDER,
	XCALL OUTPT (8,49,0,'         1 = NORMAL ',V)
	XCALL OUTPT (9,58,0,'2 = BLANKET ',V)
	XCALL OUTPT (10,58,0,'3 = RELEASE ',V)
	CTL = '08,49,01,00,# '
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO BEGIN
	IF (ENTRY.EQ.BLANKS) GO TO ALLORD
	N = ENTRY
	IF (N.LT.1.OR.N.GT.3) GO TO ORDER
	ORDTYP = TYPCDE(N)
	XCALL OUTPT (8,49,1,TYPNAM(N),V)
ORDER2,
	XCALL OUTPT (9,58,1,'\',V)
	XCALL OUTPT (10,58,1,'\',V)
	GO TO (ANYCNG), CNGCTL

ISSUE,
	XCALL OUTPT (9,49,0,'         1 = ORIGINAL ',V)
	XCALL OUTPT (10,58,0,'2 = CHANGE ',V)
	XCALL OUTPT (11,58,0,'3 = CANCEL ',V)
	CTL = '09,49,01,00,# '
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO BEGIN
	IF (ENTRY.EQ.BLANKS) GO TO ALLISS
	N = ENTRY
	IF (N.LT.1.OR.N.GT.3) GO TO ISSUE
	ISSTYP = ISSCDE(N)
	XCALL OUTPT (9,49,1,ISSNAM(N),V)
ISSUE2,
	XCALL OUTPT (10,58,1,'\',V)
	XCALL OUTPT (11,58,1,'\',V)
	GO TO (ANYCNG), CNGCTL

PRTLBL,
	CTL = '10,49,01,00,YN'
	CALL INPUT
	PRTLBL = ENTRY(1,1)

	CNGCTL =
ANYCNG,
	WHATNO =
	XCALL ANYCN (CNGCTL,WHATNO)
	IF (CNGCTL.EQ.0) GO TO PROCES
CNGBR,
	GO TO (STARPO,ENDNO,STDTE,ENDTE,ORDER,ISSUE,PRTLBL), WHATNO
BADCNG,
	CNGCTL = 3
	GO TO ANYCNG
;**********************************************************************
ALLPO,
	XCALL OUTPT (4,49,1,'ALL',V)
	XCALL OUTPT (5,49,1,'\',V)
	STPONO =
	STRLNO =
	ENPONO = 999999
	ENRLNO = 99
	GO TO ENDNO3
;**********************************************************************
MSG1,
	XCALL MESAG ('STARTING/ENDING PO NUMBERS ARE OUT OF ORDER',1)
	ENPONO = 999999
	ENRLNO = 99
	GO TO STARPO
;**********************************************************************
ALLDTE,
	XCALL OUTPT (6,49,1,'ALL',V)
	XCALL OUTPT (7,49,1,'\',V)
	STPODA =
	ENPODA = 999999
	GO TO ENDTE3
;**********************************************************************
MSG2,
	XCALL MESAG ('STARTING/ENDING PO DATES ARE OUT OF ORDER',1)
	ENPODA = 999999
	GO TO STDTE
;**********************************************************************
SAMEDA,
	IF (STPODA.EQ.0) GO TO STDTE
	ENPODA = STPODA
	ENTRY(1,5) = STPODA(3,6), 'XX/XX'
	ENTRY(6,8) = STPODA(1,2), '/XX'
	XCALL OUTPT (ROW,COL,0,ENTRY(1,8),V)
	GO TO (ANYCNG), CNGCTL
	GO TO ORDER
;**********************************************************************
ALLORD,
	XCALL OUTPT (8,49,1,'ALL',V)
	ORDTYP =
	GO TO ORDER2
;**********************************************************************
ALLISS,
	XCALL OUTPT (9,49,1,'ALL',V)
	ISSTYP =
	GO TO ISSUE2
;**********************************************************************
;	PRINT CONTROL ROUTINE
;**********************************************************************
PROCES,
	KEYPO = 
	LOKCTL = 1
	XCALL ISIO (1,HEADER,KEYPO,READ,LOKCTL)
	XCALL WATE (3,V)
	CALL READHD		;READ FIRST IN-RANGE HEADER & LINE
	IF (EOF) GO TO NONE

	LPSW = 3
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GO TO BEGIN
	XCALL WATE (2,V)

	CALL PORUN		;PRINT ALL PURCHASE ORDERS

	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	
	XCALL OUTPT (2,1,2,'\',1)
	XCALL OUTPT (6,26,0,'ARE ALL PURCHASE ORDERS OK ?',V)
	CTL = '06,58,01,01,YN'
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO ALLOK
	XCALL OUTPT (7,26,0,'PAPER RE-ALIGNMENT NEEDED ?',V)
	CTL = '07,58,01,00,YN'
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO REALGN
	XCALL OUTPT (8,26,0,'REPRINT SAME PURCHASE ORDERS ?',V)
	CTL = '08,58,01,00,YN'
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO PROCES
	GO TO BEGIN
;**********************************************************************
REALGN,
	XCALL WATE (4,V)
	CALL CLOSES
	XCALL PGCHN ('PO:POALGN',1)
;**********************************************************************
ALLOK,
	XCALL WATE (4,V)
	CLOSE 13
	CLOSE 1
	CLOSE 2
	XCALL FILES (3,'I',153,4)
	XCALL FILES (4,'I',011,4)
	XCALL FILES (5,'I',012,4)
	MSGCTL = 5
	XCALL SNMSG (RANGE1,MSGCTL)
	MSGCTL = 2
	XCALL SNMSG (RANGE2,MSGCTL)
	XCALL PGCHN ('PO:POPUPD',1)
;**********************************************************************
NONE,
	XCALL MESAG ('NO PURCHASE ORDERS MEET PRINT REQUIREMENTS.',2)
	GO TO BEGIN
;**********************************************************************
;**********************************************************************
;	EACH PASS PRINTS ONE PURCHASE ORDER:
;**********************************************************************
PORUN,
	PGNUM = 1
	SRCOPT = 4
	BSMID = 1
	SRCCTL = 1
	XCALL SERCH (5,VENIDX,HVENDR,1,4,ORG011,BSMID,SRCCTL,SRCOPT,6,10,
&			0,0,0,0)
	IF (SRCCTL.EQ.0) GO TO FOUND
	NAME = HVENDR
	GO TO PORUN2
FOUND,
	LOKCTL = NOABRT
	XCALL IO (4,VENDOR,IRC011,READ,LOKCTL)
PORUN2,
	CALL PAGES		; PRINT ALL PAGES OF ONE P/O

	CALL READHD		; READ NEXT IN-RANGE HEADER & LINE
	IF (EOF) RETURN
	GO TO PORUN
;**********************************************************************
;	EACH PASS PRINTS ONE PAGE:
;**********************************************************************
PAGES,
	LINCNT = 1

	DISPLAY (14, E_BOLD)
	PLINE (23,50) = "P U R C H A S E   O R D E R"
	KPONUM = HPONUM,'ZZZZXX'
	KDASH = '-'
	KRLNUM = HRLNUM,'XX'
	KPGNUM = PGNUM,'XX'
;;;	LINCTL = 2	;SSQ 7-7-99
	LINCTL = 1
	CALL PRINT
	DISPLAY (14, E_MED)

	LINCTL = 0
	PLINE(5,75) = 'SHEET METAL CONNECTORS, INC.'
	KPODTE(1,4) = 'DATE'
	KPODTE(7,14) = HPODTE, 'XX/XX/XX'
	CALL PRINT
	PLINE(5,75) = '5850 MAIN STREET N.E. '
	CALL PRINT

	PLINE(5,75) = 'MINNEAPOLIS MN 55432-5439'

	IF (HCXCDE.EQ.'C') KCXCDE = 'CHANGE'
	IF (HCXCDE.EQ.'X') KCXCDE = 'CANCEL'
	LINCTL = 0
	CALL PRINT

	PLINE(5,75) = '(612) 572-0000   FAX (612) 572-1100'
	IF (HCXCDE.EQ.'C'.OR.HCXCDE.EQ.'X') KCHDTE = TODAY,'XX/XX/XX'
;;;	LINCTL = 4
	LINCTL = 3		;SSQ 7-7-99
	CALL PRINT

	TKEYNO = 1
	TKEYCD = HSHPTO
	VADR(1) = NAME
	VADR(2) = ADD1
	VADR(3) = ADD2
;;;	VADR(4) = ADD3		;(01)
	ENTRY =
	ENTRY(1,15) = CITY
	N = 16
FIND,
	N = N - 1
	IF (N.GT.1.AND.ENTRY(N,N).EQ.' ') GO TO FIND
	ENTRY(N+1,N+1) = ','
	ENTRY(N+3,N+4) = STATE
	ENTRY(N+6,N+15) = ZIP
	VADR(5) = ENTRY				;(01)  WAS 4
	IF (VADR(1).EQ.BLANKS) CALL MOVE1
	IF (VADR(2).EQ.BLANKS) CALL MOVE2
	IF (VADR(3).EQ.BLANKS) CALL MOVE3
	IF (VADR(4).EQ.BLANKS) CALL MOVE4	;(01)
	
	LINCTL = 0
	N = 1
ADLOOP,
	KVNADR = VADR(N)
	TKEYSQ = N
	CALL GETTBL
	KSHPTO = TDESC
	CALL PRINT
	INCR N
	IF (N.LE.4) GO TO ADLOOP

;;;	XCALL LINFD (3)
;;;	LINCNT = LINCNT + 3
	XCALL LINFD (2)
	LINCNT = LINCNT + 2

	DISPLAY (14, E_BOLD)
	KORTYP = 'TYPE'
	KACKNO = 'ACK'
	KCONFM = 'CNF'
	KBUYER = 'BUYER'
	KTERMS = 'TERMS'
	KFOB   = 'FOB'
	KSPVIA = 'SHIP-VIA'
	KCLPPD = '*'
	CALL PRINT
	DISPLAY (14, E_MED)

	IF (HPOTYP.EQ.'N') KORTYP = 'NORMAL'
	IF (HPOTYP.EQ.'B') KORTYP = 'BLANKET'
	IF (HPOTYP.EQ.'R') KORTYP = 'RELEASE'

	TBLKEY =
	TKEYNO = 2
	TKEYCD = HBUYER
	CALL GETTBL
	KBUYER = TDESC

	IF (HACKNW.EQ.'Y') KACKNO = 'YES'
	IF (HACKNW.EQ.'N') KACKNO = ' NO'

	IF (HCONFR.EQ.'Y') KCONFM = 'YES'
	IF (HCONFR.EQ.'N') KCONFM = ' NO'

	TBLKEY =
	TKEYNO = 4
	TKEYCD = HTERMS
	CALL GETTBL
	KTERMS = TDESC

	TBLKEY =
	TKEYNO = 5
	TKEYCD = HFOB
	CALL GETTBL
	KFOB = TDESC

	TBLKEY =
	TKEYNO = 3
	TKEYCD = HSHPVI
	CALL GETTBL
	KSPVIA = TDESC

	IF (HCOLPP.EQ.'C') KCLPPD = 'C'
	IF (HCOLPP.EQ.'P') KCLPPD = 'P'
;;;	LINCTL = 3
	LINCTL = 2
	CALL PRINT

	LINCTL = 1
	DISPLAY (14, E_BOLD)
	KLNNUM = 'L/I'
	KQYORD = '    QTY'
	KUOFM  = 'UM'
	KOURNO(1,16) ='ITEM/DESCRIPTION'
	KPRICE = '      PRICE'	
	KRQDTE = 'REQUIRED'
	KCXLIN = 'STS'
	CALL PRINT
	DISPLAY (14, E_MED)

	CALL LINES

	LINCNT = LINCNT + 6
	KLNCNT = 60 - LINCNT
	IF (LINCNT.LT.60) XCALL LINFD (KLNCNT)

	LINCNT = 1		;NEW
	TKEYNO = 6
	N = 1
NTLOOP,
	IF (HPONCD(N).EQ.BLANKS) GO TO SKIP
	TKEYCD = HPONCD(N)
	CALL GETTBL
	KNOTE1 = TDESC
SKIP,
	KNOTE2 = HPONTE(N)
	LINCTL = 0
	CALL PRINT
	INCR N
	IF (N.LE.3) GO TO NTLOOP

	XCALL LINFD (0)


	IF (EOF.OR.EPO) RETURN

	INCR PGNUM
	GO TO PAGES
;**********************************************************************
MOVE1,
	VADR(1) = VADR(2)
MOVE2,
	VADR(2) = VADR(3)
MOVE3,
	VADR(3) = VADR(4)
MOVE4,
	VADR(4) = VADR(5)		;(01)
	VADR(5) = '                              *'	;(01) WAS 4
	IF (VADR(1).EQ.BLANKS) GO TO MOVE1
	IF (VADR(2).EQ.BLANKS) GO TO MOVE2
	RETURN
;**********************************************************************
;	EACH PASS PRINTS ONE LINE ITEM:
;**********************************************************************
LINES,
	KLNNUM = LLINNO,'XXX'
	KQYORD = LQTYOR,'ZZZ,ZZX'
	KUOFM = LITMUM
	KOURNO = LITMNO
	IF (RJUST) XCALL LEFTJ (KOURNO(1,15),15)
	KVENNO = LVITMN
	IF (HPRTPR.NE.'N') KPRICE =(LEXCST)#1,'ZZZ,ZZX.XXX'
	KRQDTE = LDTERQ,'XX/XX/XX'
	IF (HCXCDE.NE.'C'.AND.HCXCDE.NE.'X') GO TO NEXT1
	KCXLIN = 'CANCEL'
	IF (HCXCDE.EQ.'C'.AND.LCXCDE.EQ.'C') KCXLIN = 'CHANGE'
	IF (HCXCDE.EQ.'C'.AND.LINSTS.EQ.'N'.AND.LCXCDE.NE.'X') KCXLIN = 'NEW   '
NEXT1,
	LINCTL = 0
	CALL PRINT
	IF (HPOTYP.NE.'B') GO TO ITMNAM
	IF (LBLKTP.EQ.'F') KBLTYP = 'FIRM'
	IF (LBLKTP.EQ.'E') KBLTYP = 'EST.'
	IF (LBLKTP.EQ.'M') KBLTYP = 'MAX.'
	IF (LBLKTP.EQ.'N') KBLTYP = 'MIN.'
ITMNAM,
	KLNOTE = LITMDS
	CALL PRINT

	TBLKEY =
	TKEYNO = 6
	IF (LPONCD(1).EQ.BLANKS) GO TO NEXT2
	TKEYCD = LPONCD(1)
	CALL GETTBL
	KLNOTE = TDESC
	CALL PRINT
NEXT2,
	IF (LPONCD(2).EQ.BLANKS) GO TO NEXT3
	TKEYCD = LPONCD(2)
	CALL GETTBL
	KLNOTE = TDESC
	CALL PRINT
NEXT3,
	KLNOTE = LPONTE(1)
	IF (KLNOTE.NE.BLANKS) CALL PRINT
	KLNOTE = LPONTE(2)
	IF (KLNOTE.NE.BLANKS) CALL PRINT
	KLNOTE = LPONTE(3)
	IF (KLNOTE.NE.BLANKS) CALL PRINT
	KLNOTE = LPONTE(4)
	IF (KLNOTE.NE.BLANKS) CALL PRINT
	XCALL LINFD (1)
	INCR LINCNT

;;===============================================
	IF (PRTLBL.EQ.'Y' .AND. LILABN .GT. 0) CALL PRINT_BAR_LABEL
;;===============================================
GETLIN,
	CALL NXTREC
	IF (EOF.OR.EPO) RETURN
	IF (LINCNT.GE.50) RETURN		;06/07/94 DKS (WAS 54)
	GO TO LINES
;**********************************************************************
;	READ ONE IN-RANGE HEADER RECORD AND ONE IN-RANGE LINE RECORD
;**********************************************************************
READHD,
	EOF =
REREAD,
	LOKCTL = 1
	XCALL IOS (1,HEADER,READ,LOKCTL)
	IF (LOKCTL.NE.0) GOTO ENDFIL
	IF (HPONUM.LT.STPONO) GO TO REREAD
	IF (HPONUM.GT.ENPONO) GO TO ENDFIL
	IF (HPONUM.EQ.STPONO.AND.HRLNUM.LT.STRLNO) GO TO REREAD
	IF (HPONUM.EQ.ENPONO.AND.HRLNUM.GT.ENRLNO) GO TO ENDFIL
	HDATE(1,2) = HPODTE(5,6)
	HDATE(3,6) = HPODTE(1,4)
	IF (HDATE.LT.STPODA.OR.HDATE.GT.ENPODA) GO TO REREAD
	IF (ORDTYP.NE.' '.AND.ORDTYP.NE.HPOTYP) GO TO REREAD
	IF (HCXCDE.EQ.' ') HCXCDE = 'N'
	IF (ISSTYP.NE.' '.AND.ISSTYP.NE.HCXCDE) GO TO REREAD
	IF (HCXCDE.NE.'N'.AND.HPOSTS.EQ.'P') GO TO TRYIT
	IF (HPOSTS.NE.'R') GO TO REREAD
TRYIT,
	CALL READLN
	IF (EOF) RETURN
	IF (EPO) GO TO REREAD
	RETURN
ENDFIL,
	EOF = 1
	RETURN
;**********************************************************************
;	READ ONE IN-RANGE LINE RECORD
;**********************************************************************
READLN,
	EOF =
	EPO =
REPEAT,
	KEY1 = HPONUM
	KEY2 = HRLNUM
	LOKCTL = 1
	XCALL ISIO (2,POLINE,KEYPO,READ,LOKCTL)
CONTIN,
	IF (LOKCTL.EQ.2) GOTO TOOFAR
	IF (LOKCTL.EQ.1) GOTO EXIT2
	IF (LPONUM.LT.HPONUM) GO TO NXTREC
	IF (LPONUM.GT.HPONUM) GO TO TOOFAR
	IF (LRLNUM.LT.HRLNUM) GO TO NXTREC
	IF (LRLNUM.GT.HRLNUM) GO TO TOOFAR
	IF (HPOSTS.EQ.'P'.AND.HCXCDE.EQ.'C') GO TO CNGORD
	RETURN
NXTREC,
	LOKCTL = 1
	XCALL IOS (2,POLINE,READ,LOKCTL)
	GOTO CONTIN
CNGORD,
	IF (LINSTS.EQ.'N') RETURN
	IF (LCXCDE.EQ.'C'.OR.LCXCDE.EQ.'X') RETURN
	GO TO NXTREC
TOOFAR,
	EPO = 1
	RETURN
EXIT2,
	EOF = 1
	RETURN
;**********************************************************************
;	TABLE SEARCH, INPUT, PRINT, CLOSES, IN-USE, AND EXIT ROUTINES
;**********************************************************************
GETTBL,
	SRCOPT = 4
	BSMID = 1
	SRCCTL = 2
	XCALL SERCH (3,TABLE,TBLKEY,1,5,ORG153,BSMID,SRCCTL,SRCOPT,
&			6,11,0,0,0,0)
	IF (SRCCTL.EQ.1) TABLE =
	RETURN
;**********************************************************************
INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,V)
	RETURN
;**********************************************************************
PRINT,
	PRTCTL = 78
	XCALL LPOUT (LINCNT,PGCNT,PLINE(3,80),TITLE,'NO HDR',' ',' ',
&		'NO LEGEND',' ',' ',LINCTL,80,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;**********************************************************************
CLOSES,
	XCALL FILES (1,'SI',151,4)
	XCALL FILES (2,'SI',152,4)
	XCALL FILES (3,'I',153,4)
	XCALL FILES (4,'I',011,4)
	XCALL FILES (5,'I',012,4)
	RETURN
;**********************************************************************
INUSE5,
	XCALL FILES (4,'I',011,4)
INUSE4,
	XCALL FILES (3,'I',153,4)
INUSE3,
	XCALL FILES (2,'SI',152,4)
INUSE2,
	XCALL FILES (1,'SI',151,4)
INUSE1,
	XCALL PGCHN ('PO:POMENU',1)
;**********************************************************************
EXIT,
	XCALL OUTPT (2,1,2,'\',1)
	XCALL OUTPT (5,23,0,'PURCHASE ORDER PRINT CYCLE COMPLETED.',V)
	XCALL OUTPT (7,23,0,'PLEASE MOUNT REGULAR PAPER ON PRINTER,',V)
	XCALL OUTPT (8,23,0,'AND TYPE "DONE".',V)
DONER,
	CTL = '08,47,04,04,A '
	CALL INPUT
	IF (ENTRY.NE.'DONE') GO TO DONER
	XCALL WATE (4,V)
	CALL CLOSES
	XCALL PGCHN ('PO:POMENU',1)
;**********************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              INTERMEC 3440 BAR CODE LABELS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRINT_BAR_LABEL,
	NUMLBL = LILABN		;# LABELS TO  PRINT
	IF (NUMLBL .LE. 0) NUMLBL = 1	;4-29-97 TEMP.
	IF (NUMLBL .LE. 0) RETURN

	PUTS (13,ADVANCE)

;; Select label format 1...

	PUTS (13,LFORMAT)

;; Clear data from format...

	FOR I FROM 1 THRU NUMLBL
	  BEGIN
	  PUTS (13,CANCEL)

	  LBL_DATA(1,1) = 'I'
	  LBL_DATA(2,14) = LITMNO(1,13)
	  CALL LBL_FIELD
	  LBL_DATA(1,1) = 'J'
	  LBL_DATA(2,7) = LPONUM, 'ZZZZZZ' [LEFT]
	  CALL LBL_FIELD

	  LBL_DATA = LITMNO(1,13)
	  CALL LBL_FIELD
	  LBL_DATA = LPONUM, 'ZZZZZZ' [LEFT]
	  CALL LBL_FIELD

	  PUTS (13,LPRINT)
	  END
	
	RETURN
;;=====================================================
;;=====================================================

LBL_FIELD,	;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PRINT 1 LABEL FIELD
		;;;;;;;;;;;;;;;;;;;;;;;;
	TL = %TRIM(LBL_DATA)
	TMPREC(1,1) = STX
	TMPREC(2,TL+1) = LBL_DATA(1,TL)
	TL = TL + 2
	TMPREC(TL,TL) = CR
	INCR TL
	TMPREC(TL,TL) = ETX
	PUTS (13,TMPREC(1,TL) )
	CLEAR LBL_DATA
	RETURN
;------------------------------------

ASCII,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; DEFINE ASCII CHAR.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ASCII(1,SOH)
	XCALL ASCII(2,STX)
	XCALL ASCII(3,ETX)
	XCALL ASCII(5,ENQ)
	XCALL ASCII(7,SEL)
	XCALL ASCII(13,CR)
	XCALL ASCII(12,FF)
	XCALL ASCII(16,DLE)
	XCALL ASCII(21,NAK)
	XCALL ASCII(23,ETB)
	XCALL ASCII(24,CAN)
	XCALL ASCII(27,ESC)

;; Select Advance mode...
;;; SWR$ + ESC$ + "C1" + EWR$
	ADVANCE(1,1) = STX
	ADVANCE(2,2) = ESC
	ADVANCE(3,4) = 'C1'
	ADVANCE(5,5) = ETX

;; Seledt Label format 2...
;; SWR$ + ESC$ + "E2" + EWR$
	LFORMAT(1,1) = STX
	LFORMAT(2,2) = ESC
	LFORMAT(3,4) = 'E3'
	LFORMAT(5,5) = ETX

;; Clear all data from current format...
;; SWR$ + CAN$ + EWR$
	CANCEL(1,1) = STX
	CANCEL(2,2) = CAN
	CANCEL(3,3) = ETX

;; Label print command...
	LPRINT(1,1) = STX
	LPRINT(2,2) = ETB
	LPRINT(3,3) = ETX

	RETURN
;---------------------------------------

END

