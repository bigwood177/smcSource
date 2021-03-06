;  POVANL / POR - D11
;
;
;
;		PRINT VENDOR PERFORMANCE ANALYSIS
;
RECORD RD011A	
	.INCLUDE 'DEF:RD011A.DEF'

RECORD RD012A	
	.INCLUDE 'DEF:RD012A.DEF'

RECORD HDR1
		,A93
		,A11,	'PERCENTAGES'
		,A3
		,A9,	'AVERAGE %'
		,A4
		,A9,	'-AVERAGE-'
		,A3

;**********************************************************************
RECORD HDR2
		,A51
		,A38,	'------------ YEAR TO DATE ------------'
		,A4
		,A5,	'LINES'
		,A1
		,A4,	'LAST'
		,A3
		,A4,	'COST'
		,A1
		,A5,	'ITEMS'
		,A3
		,A3,	'L/T'
		,A2
		,A4,	'DAYS'
		,A4


;**********************************************************************
RECORD HDR3
		,A8,	'VENDOR #'
		,A2
		,A11,	'VENDOR NAME'
		,A18
		,A9,	'TELEPHONE'
		,A3
		,A11,	'$ PURCHASED'
		,A2
		,A8,	'VOUCHERS'
		,A1
		,A4,	'P/OS'
		,A1
		,A5,	'LINES'
		,A2
		,A4,	'LATE'
		,A4
		,A4,	'LATE'
		,A2
		,A4,	'YEAR'
		,A1
		,A6,	'VARNCE'
		,A1
		,A6,	'REJECT'
		,A2
		,A4,	'DAYS'
		,A1
		,A4,	'LATE'
		,A4


;**********************************************************************
RECORD TITLE
		,A30,	'VENDOR PERFORMANCE ANALYSIS   '

;**********************************************************************
RECORD CLEAR1
	TUSECD	,A2
	STRTNO	,A4
	ENDNO	,A4
	TCOMCD	,A4
	TACTIV	,A1
	USERSW	,D1
	MORESW	,D1
	ACTVSW	,D1
	COMMSW	,D1

;**********************************************************************
RECORD CLEAR2
	LLPCNT	,D4
	VRNCPC	,D4
	REJPCT	,D4

;**********************************************************************
RECORD CONTRL
	ROW	,D2
		,A1
	COL	,D2
		,A1
	MAX	,D2
		,A1
	MIN	,D2
		,A1
	TYPE	,A2

;**********************************************************************
RECORD WORK
	BRAKTS	,A10,	']]]]]]]]]]'
	BLANKS	,A4
	CNGCTL	,D1
	CTR	,D1
	DECMAL	,D4
	ENTRY	,A4
	DSPW	,D1
	FORMAT	,A1
	INXCTL	,D1
	GO3SW	,D1
	LINCNT	,D2
	LINCTL	,D1
	LSLAVG	,D3
	LOKCTL	,D1
	LPSW	,D1
	NOABRT	,D1,	1
	PGCNT	,D6,	000000
	PLINE	,A132
	PRTCTL	,D3
	PRTTYP	,A1
	READ	,D1,	0
	ROWPLS	,D2
	RPTNUM	,D3
	SPLFIL	,A14
	SWITCH	,D1
	TEXT	,A3
	V	,D1
	WHATNO	,D2
	WTARG	,D1,2
	XRECNO	,D5
	ZEROSW	,D1

;**********************************************************************
PROC
;**********************************************************************
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'PRINT VENDOR PERFORMANCE ANALYSIS',1)
	XCALL WATE (1,V)
	SPLFIL (5,6) = 'N6'

	SWITCH = 1
	XCALL FILES (1,'I',12,SWITCH)	;VENDOR INDEX FILE
	IF (SWITCH.EQ.9) GO TO INUSE1

	SWITCH = 1
	XCALL FILES (2,'I',11,SWITCH)	;VENDOR MASTER FILE
	IF (SWITCH.EQ.9) GO TO CLOSE1

;**********************************************************************
BEGIN,
	LPSW = 1
	PGCNT =
	ZEROSW =
	LINCNT = 60
	CNGCTL =
	CLEAR1 =
	RD011A =
	RD012A =
	XCALL OUTPT (1,1,2,'PRINT VENDOR PERFORMANCE ANALYSIS',V)
	XCALL OUTPT (2,1,0,'ACCEPT SELECTION CRITERIA',1)
	XCALL OUTPT (4,20,0,'PLEASE ENTER:',V)
	XCALL OUTPT (5,25,0,'1. VENDOR NUMBER',V)
	XCALL OUTPT (5,50,0,'THRU',V)
	XCALL OUTPT (6,25,0,'2. USER CODE',V)
	XCALL OUTPT (7,25,0,'3. COMMODITY CODE',V)
	XCALL OUTPT (8,25,0,'4. RESTRICT BY PERCENTAGES ?',V)

VENNUM,
	CONTRL = '05,43,04,00,AE'
	CALL INPUT
	GO TO (BEGIN,ENDOFF), INXCTL
	IF (ENTRY.EQ.BLANKS) GO TO VENALL
	XCALL FRMAT (ENTRY(1,4),4)
	STRTNO = ENTRY
	XCALL OUTPT (ROW,COL,0,STRTNO,V)
	IF (ENDNO.EQ.']]]]') GO TO ENDVN2
	IF (CNGCTL.NE.1) GO TO ENDVN2
	IF (ENDNO.LT.STRTNO) GO TO ORDMSG
	GO TO ANYCN1
ENDVN,
	IF (ENDNO.EQ.']]]]') GO TO VENNUM
ENDVN2,
	CONTRL = '05,55,04,00,A '
	CALL INPUT
	IF (INXCTL) GO TO BEGIN
	XCALL FRMAT (ENTRY(1,4),4)
	ENDNO = ENTRY
	IF (ENTRY.EQ.BLANKS) ENDNO = STRTNO
	XCALL OUTPT (5,55,0,ENDNO,V)
	IF (ENDNO.LT.STRTNO) GO TO ORDMSG
	IF (CNGCTL) GO TO ANYCN1

USECD,
	USERSW =
	CONTRL = '06,55,02,00,A '
	CALL INPUT
	TUSECD = ENTRY
	GO TO (BEGIN) ,INXCTL
	IF (ENTRY.EQ.BLANKS)  CALL USEALL
	IF (CNGCTL) GO TO ANYCN1

COMMCD,
	COMMSW =
	CONTRL = '07,55,04,00,A '
	CALL INPUT
	IF (INXCTL) GO TO BEGIN
	IF (ENTRY.EQ.BLANKS) CALL COMALL
	TCOMCD = ENTRY
	IF (CNGCTL) GO TO ANYCN1

RSTRCT,
	MORESW =
	CONTRL = '08,55,01,00.YN'
	CALL INPUT
	IF (ENTRY.EQ.'Y') MORESW = 1
	GO TO ANYCN1

USEALL,
	USERSW = 1
	XCALL OUTPT (6,55,0,'ALL',V)
	RETURN

VENALL,
	STRTNO =
	ENDNO = ']]]]'
	XCALL OUTPT (5,43,1,'\',V)
	XCALL OUTPT (5,55,0,'ALL',V)
	IF (CNGCTL) GO TO ANYCN1
	GO TO USECD

COMALL,
	COMMSW = 1
	XCALL OUTPT (7,55,0,'ALL',V)
	RETURN

ACTVAL,
	ACTVSW = 1
	XCALL OUTPT (8,55,0,'ALL',V)
	RETURN

;**********************************************************************
ANYCN1,
	XCALL ANYCN (CNGCTL,WHATNO)
	IF (CNGCTL) GO TO (VENNUM,USECD,COMMCD,RSTRCT) , WHATNO
	IF (WHATNO.GT.4) GO TO ANYCN1
	IF (MORESW) CALL SCREN2
	GO TO PROCES

;**********************************************************************
ORDMSG,
	XCALL MESAG ('STARTING/ENDING VENDOR NUMBERS ARE OUT OF ORDER',1)
	ENDNO = ']]]]'
	GO TO VENNUM
;**********************************************************************
SCREN2,
	CLEAR2 =
	CNGCTL =
	XCALL OUTPT (2,1,2,'RESTRICT BY PERCENTAGES',1)
	XCALL OUTPT (2,20,0,'PLEASE ENTER:',V)
	XCALL OUTPT (3,25,0,'1. RESTRICT BY % LINES LATE ?',V)
	XCALL OUTPT (4,25,0,'2. CUTOFF PERCENT',V)
	XCALL OUTPT (5,25,0,'3. INCLUDE VENDORS       AND',V)
	XCALL OUTPT (6,25,0,'4. RESTRICT BY % COST VARIANCE ?',V)
	XCALL OUTPT (7,25,0,'5. CUTOFF PERCENT',V)
	XCALL OUTPT (8,25,0,'6. INCLUDE VENDORS       AND',V)
	XCALL OUTPT (9,25,0,'7. RESTRICT BY % ITEMS REJECTED ?',V)
	XCALL OUTPT (10,25,0,'8. CUTOFF PERCENT',V)
	XCALL OUTPT (11,25,0,'9. INCLUDE VENDORS       AND',V)

ONE,
	CONTRL = '03,60,01,00,AE'
	CALL INPUT
	GO TO (SCREN2,BEGIN), INXCTL
	IF (ENTRY.NE.'Y'.AND.ENTRY.NE.'N'.AND.ENTRY.NE.BLANKS) GO TO ONE
	FORMAT = ENTRY
	IF (ENTRY.EQ.BLANKS) FORMAT = 'N'
	XCALL OUTPT (3,60,0,FORMAT,V)
	IF (ENTRY.EQ.'N'.OR.ENTRY.EQ.BLANKS) GO TO FOUR

TWO,
	CONTRL = '04,60,03,00,# '
	CALL INPUT
	IF (INXCTL) GO TO SCREN2
	LLPCNT(2,4) = ENTRY
	TEXT = LLPCNT,'ZZX'
	CALL FILLIN
	CALL LEGEND
	IF (GO3SW) GO TO THREE
	IF (CNGCTL) GO TO ANYCN2

THREE,
	CONTRL = '05,60,01,00,# '
	CALL INPUT
	IF (INXCTL) GO TO SCREN2
	IF (ENTRY.GT.'2'.OR.ENTRY.LT.'1') GO TO THREE
	LLPCNT(1,1) = ENTRY
	ROW = 5
	CALL SHOW
	CALL CLRLGD
	IF (CNGCTL) GO TO ANYCN2

FOUR,
	CONTRL = '06,60,01,00,YN'
	CALL INPUT
	GO TO (FIVE,SEVEN), INXCTL

FIVE,
	CONTRL = '07,60,03,01,# '
	CALL INPUT
	IF (INXCTL) GO TO SCREN2
	VRNCPC(2,4) = ENTRY
	TEXT = VRNCPC,'ZZX'
	CALL FILLIN
	CALL LEGEND
	IF(GO3SW) GO TO SIX
	IF (CNGCTL) GO TO ANYCN2

SIX,
	CONTRL = '08,60,01,01,# '
	CALL INPUT
	VRNCPC(1,1) = ENTRY
	IF (ENTRY.GT.'2'.AND.ENTRY.LT.'1') GO TO SIX
	IF (INXCTL) GO TO SCREN2
	ROW = 8
	CALL SHOW
	CALL CLRLGD
	IF (CNGCTL) GO TO ANYCN2

SEVEN,
	CONTRL = '09,60,01,00,YN'
	CALL INPUT
	GO TO (EIGHT,ANYCN2), INXCTL

EIGHT,
	CONTRL = '10,60,03,01,#'
	CALL INPUT
	IF (INXCTL) GO TO SCREN2
	REJPCT(2,4) = ENTRY
	TEXT = REJPCT,'ZZX'
	CALL FILLIN
	CALL LEGEND
	IF (GO3SW) GO TO NINE
	IF (CNGCTL) GO TO ANYCN2

NINE,
	CONTRL = '11,60,01,01,# '
	CALL INPUT
	IF (INXCTL) GO TO SCREN2
	IF (ENTRY.GT.'2'.AND.ENTRY.LT.'1') GO TO NINE
	REJPCT(1,1) = ENTRY
	ROW = 11
	CALL CLRLGD
	CALL SHOW
	GO TO ANYCN2

SHOW,
	IF (ENTRY.EQ.'1') XCALL OUTPT (ROW,54,1,'BELOW',V)
	IF (ENTRY.EQ.'2') XCALL OUTPT (ROW,54,1,'ABOVE',V)
	RETURN

FILLIN,
	ROWPLS = ROW + 1
	XCALL OUTPT (ROWPLS,44,0,'   %',V)
	XCALL OUTPT (ROWPLS,44,0,TEXT,V)
	RETURN

LEGEND,
	XCALL OUTPT (9,69,0,'LEGEND:',V)
	XCALL OUTPT (10,66,0,'1 = BELOW',V)
	XCALL OUTPT (11,66,0,'2 = ABOVE',V)
	RETURN

CLRLGD,
	XCALL OUTPT (9,69,1,'\',V)
	XCALL OUTPT (10,66,1,'\',V)
	XCALL OUTPT (11,66,1,'\',V)
	RETURN

ABORT1,
	GO3SW = 1
	LLPCNT =
	XCALL OUTPT (4,60,1,'\',V)
	XCALL OUTPT (5,44,1,'\',V)
	XCALL OUTPT (5,50,0,'AND',V)
	GO TO ONE

ABORT4,
	GO3SW = 1
	VRNCPC =
	XCALL OUTPT (7,60,1,'\',V)
	XCALL OUTPT (8,44,1,'\',V)
	XCALL OUTPT (8,50,0,'AND',V)
	GO TO FOUR

ABORT7,
	GO3SW = 1
	REJPCT =
	XCALL OUTPT (10,60,1,'\',V)
	XCALL OUTPT (11,44,1,'\',V)
	XCALL OUTPT (11,50,0,'AND',V)
	GO TO SEVEN

;**********************************************************************
ANYCN2,
	GO3SW =
	XCALL ANYCN (CNGCTL,WHATNO)
	IF (CNGCTL) GO TO (ABORT1,TWO,THREE,ABORT4,FIVE,SIX,ABORT7,EIGHT,NINE),WHATNO
	IF (WHATNO.GT.9) GO TO ANYCN2
;**********************************************************************
PROCES,
	XRECNO = 1
	CALL READ
	IF (ZEROSW) GO TO NONE
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GO TO ENDOFF
	IF (LPSW.EQ.2) WTARG = 4
	XCALL WATE (WTARG,V)
	CALL WRITE

LOOP,
	CALL READ
	IF (ZEROSW) GO TO WRAPUP
	CALL WRITE
	GO TO LOOP

WRAPUP,
	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	XCALL WATE (3,V)
	IF (USERSW.AND.ENDNO.EQ.']]]]'.AND.COMMSW.AND.MORESW.NE.1)
&	GO TO ENDOFF
	GO TO BEGIN

ENDOFF,
	XCALL FILES (2,'I',11,4)	;VENDOR MASTER FILE
CLOSE1,
	XCALL FILES (1,'I',12,4)	;VENDOR INDEX FILE
INUSE1,
	XCALL PGCHN ('PO:POMENU',1)

;**********************************************************************

READ,
	INCR XRECNO
	LOKCTL = NOABRT
	XCALL IO (1,RD012A,XRECNO,READ,LOKCTL)
	IF (RD012A.EQ.BRAKTS) GO TO EOF
	IF (IRC011.EQ.00000) GO TO READ
;**	IF (RD012A.EQ.BRAKTS) GO TO EOF
	IF (IVENNO.LT.STRTNO) GO TO READ
	IF (IVENNO.GT.ENDNO) GO TO READ
RDNEXT,
	LOKCTL = NOABRT
	XCALL IO (2,RD011A,IRC011,READ,LOKCTL)
;;	IF (ACTIVE.EQ.BLANKS) GO TO READ
	CTR =
	IF (USERSW) GO TO CTRLOP
	IF (USERCD.NE.TUSECD) GO TO READ
CTRLOP,
	IF (COMMSW) GO TO FILTAA
	INCR CTR
	IF (CTR.GT.5) GO TO READ
	IF (COMMCD(CTR).EQ.BLANKS) GO TO CTRLOP
	IF (COMMCD(CTR).NE.TCOMCD) GO TO CTRLOP

FILTAA,
;;	IF (ACTVSW) GO TO MORE
;;	IF (ACTIVE.NE.TACTIV) GO TO READ

MORE,
	IF (MORESW.NE.1) GO TO GOBACK

	IF (LNSYTD.EQ.0) GO TO PRIOR
	LSLAVG = (LSLYTD*100)/LNSYTD
	IF (LLPCNT(1,1).EQ.0) GO TO FILTAV
	IF (LLPCNT(1,1).EQ.1) GO TO GTLNSL
	IF (LLPCNT(1,1).EQ.2) GO TO LTLNSL

GTLNSL,
	IF (LSLAVG.GT.LLPCNT(2,4) )GO TO READ
	GO TO FILTAV

LTLNSL,
	IF (LSLAVG.LT.LLPCNT(2,4) ) GO TO READ

PRIOR,
	GO TO (PRIRGT,PRIRLT), LLPCNT(1,1)

PRIRGT,
	IF (LSLPYR.GT.LLPCNT(2,4)) GO TO READ
	GO TO FILTAV

PRIRLT,
	IF (LSLPYR.LT.LLPCNT(2,4)) GO TO READ
	GO TO FILTAV

FILTAV,
	GO TO (FILTAR,GTVRNC,LTVRNC), VRNCPC(1,1) + 1

GTVRNC,
	IF (AVPVAR.GT.VRNCPC(2,4)) GO TO READ
	GO TO FILTAR

LTVRNC,
	IF(AVPVAR.LT.VRNCPC(2,4)) GO TO READ

FILTAR,
	GO TO (GOBACK,GTREJ,LTREJ), REJPCT(1,1) + 1

GTREJ,
	IF (AVPREJ.GT.REJPCT(2,4)) GO TO READ
	GO TO GOBACK

LTREJ,
	IF (AVPREJ.LT.REJPCT(2,4)) GO TO READ

GOBACK,
	RETURN

EOF,
	ZEROSW = 1
	RETURN

;**********************************************************************
WRITE,

	PLINE =
	IF (ACTIVE.EQ.'2') PLINE(2,2) = '*'
	PLINE(4,7)	= VENNO
	PLINE(11,35)	= NAME
	PLINE(37,48)	= TELNO
	PLINE(52,65)	= BILYTD,'ZZ,ZZZ,ZZX.XX-'
	PLINE(67,70)	= NOVYTD
	PLINE(74,76)	= POSYTD
	PLINE(80,83)	= LNSYTD
	PLINE(87,89)	= LSLYTD
	PLINE(95,97)	= '  0'
	IF (LNSYTD.NE.0) PLINE(95,97) = (LSLYTD*100)/LNSYTD
	PLINE(100,102)	= LSLPYR
	PLINE(109,111)	= AVPVAR
	PLINE(114,116)	= AVPREJ
	PLINE(121,123)	= AVLDTM
	PLINE(126,128)	= AVDSLA

	CALL PRINT
	RETURN

;**********************************************************************
PRINT,
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,HDR2,HDR3,'NO LEGEND',
&	' ',' ',LINCTL,0,0,0,LPSW,RPTNUM,PRTTYP)
	RETURN

;**********************************************************************
INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,V)
	RETURN
;**********************************************************************
NONE,
	XCALL MESAG ('NO RECORDS FOR THIS RANGE OF PARAMETERS',2)
	GO TO BEGIN
;**********************************************************************

END

