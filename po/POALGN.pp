;  POALGN / POR - D11
;
;	(01) 10/04/95 DKS - MAKE P.O. FILES ISAM
;
;
;
;
;
;		PRINT ALIGNMENT PAGES UNTIL THE SPECIAL PURCHASE
;		ORDER FORMS ARE PROPERLY ALIGNED.
;
;
;;;RECORD DUMHDR
;;;	.INCLUDE 'DEF:RD151B.DEF'

RECORD PLINE
		,A80
RECORD LINE1
		,A63
		,A14,	'XXXXXX-XX   XX'
		,A13
RECORD LINE2
		,A61
		,A17,	'XX/XX/XX   XXXXXX'
		,A2
RECORD LINE3
		,A71
		,A8,	'XX/XX/XX'
		,A1
RECORD LINE4
		,A12
		,A30,	'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
		,A8
		,A30,	'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
RECORD LINE5
		,A12
		,A30,	'XXXXXXXXXXXXXXX, XX XXXXX-XXXX'
		,A8
		,A30,	'XXXXXXXXXXXXXXX, XX XXXXX-XXXX'
RECORD LINE6
		,A30,	'   XXXXXXX  XXXXXXXXXXXXXXXX X'
		,A30,	'XX  XXX  XXXXXXXXXX  XXXXXXXXX'
		,A20,	'  XXXXXXXXXXXXXXX  X'
RECORD LINE7
		,A30,	'  XXX  XXX,XXX  XX  XXXXXXXXXX'
		,A30,	'XXX  XXXXXXXXXXXXXXXXX  XXX,XX'
		,A20,	'X.XXX  XX/XX/XX  XXX'
RECORD LINE8
		,A7
		,A4,	'XXXX'
		,A9
		,A30,	'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
		,A30
RECORD LINE9
		,A20
		,A30,	'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
		,A30
RECORD LINE10
		,A20
		,A30,	'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
		,A2
		,A28,	'XXXXXXXXXXXXXXXXXXXXXXXXXXXX'

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

RECORD
	ENTRY	,A30
	INXCTL	,D1
	LINCNT	,D2,	60
	LOKCTL	,D1
	LPON	,D1
	LPSW	,D1
	MSGCTL	,D1
	N	,D1
	NOABRT	,D1,	1
	NXTPGM	,A9,	'PO:POPRNT'	; NEXT PROGRAM
	PGCNT	,D3,	001
	PRTCTL	,D3
	PRTTYP	,A1
	READ	,D1,	0
	RPTNUM	,D3
	SPLFIL	,A14
	SWITCH	,D1
	TITLE	,A5
	V	,D1

PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'PRINT PURCHASE ORDERS',1)
	XCALL OUTPT (2,1,0,'ALIGN SPECIAL PURCHASE ORDER FORMS',1)
	XCALL WATE (4,V)
	CALL OPENUP
	XCALL OUTPT (5,24,2,'PLEASE MOUNT PURCHASE ORDER FORMS,',V)
	XCALL OUTPT (6,24,0,'AND TYPE "DONE".',V)
INDONE,
	CTL = '06,47,04,00,AE'
	CALL INPUT
	GO TO (ENDOFF,ENDOFF), INXCTL
	IF (ENTRY.NE.'DONE') GO TO INDONE
ANOTHR,
	LPON =
	XCALL OUTPT (2,1,2,'\',V)
	XCALL OUTPT (6,28,0,'PRINT ALIGNMENT FORM ?',V)
ANOT2,
	CTL = '06,52,01,00,AE'
	CALL INPUT
	GO TO (EXIT,EXIT),INXCTL
	IF (ENTRY.EQ.'Y') GO TO OPENLP
	IF (ENTRY.NE.'N'.AND.ENTRY.NE.' ') GO TO ANOT2
	XCALL WATE (4,V)
	IF (LPON) XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	GO TO CHKSRT
OPENLP,
	LPSW = 3
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GO TO ENDOFF
	LPON = 1
;**********************************************************************
PNTPAG,
	XCALL WATE (2,V)
	LINCNT = 1

	PLINE = LINE1
	CALL PRINT
	XCALL LINFD (2)
	PLINE = LINE2
	CALL PRINT
	PLINE = LINE3
	CALL PRINT
	XCALL LINFD (4)
	PLINE = LINE4
	CALL PRINT
	PLINE = LINE4
	CALL PRINT
	PLINE = LINE4
	CALL PRINT
	PLINE = LINE5
	CALL PRINT
	XCALL LINFD (3)
	PLINE = LINE6
	CALL PRINT
	XCALL LINFD (3)
	N = 0
POITEM,
	INCR N
	IF (N.GT.5) GO TO PNOTES
	PLINE = LINE7
	CALL PRINT
	PLINE = LINE8
	CALL PRINT
	PLINE = LINE9
	CALL PRINT
	PLINE = LINE9
	CALL PRINT
	PLINE = LINE9
	CALL PRINT
	XCALL LINFD (1)
	GO TO POITEM
PNOTES,
	XCALL LINFD (2)
	PLINE = LINE10
	CALL PRINT
	PLINE = LINE10
	CALL PRINT
	PLINE = LINE10
	CALL PRINT
	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	GO TO ANOTHR
;**********************************************************************
OPENUP,
	SWITCH = 3
	XCALL FILES (1,'SU',151,SWITCH)		;FILE 151, PROTECT PORHDR FILE
	IF (SWITCH.EQ.9) GO TO INUSE1

	SWITCH = 5
	XCALL FILES (1,'SI',151,SWITCH)		;OPEN FOR INPUT
	IF (SWITCH.EQ.9) GO TO INUSE2

	SWITCH = 3
	XCALL FILES (2,'SU',152,SWITCH)		;FILE 152, PROTECT PORLIN FILE
	IF (SWITCH.EQ.9) GO TO INUSE2

	SWITCH = 5
	XCALL FILES (2,'SI',152,SWITCH)		;OPEN FOR INPUT
	IF (SWITCH.EQ.9) GO TO ENDOFF

	CLOSE 1
	CLOSE 2
	RETURN

CHKSRT,
	XCALL PGCHN (NXTPGM,1)

;**********************************************************************
INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,V)
	RETURN
;**********************************************************************
PRINT,
	PRTCTL = 80
	XCALL LPOUT (LINCNT,PGCNT,PLINE(3,80),TITLE,'NO HDR',' ',' ',
&		'NO LEGEND',' ',' ',0,80,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;**********************************************************************
EXIT,
	IF (LPON) XCALL LPOFF (LPSW,SPLFIL,PGCNT)
ENDOFF,
	XCALL WATE (3,V)
	XCALL FILES (2,'SU',152,4)
INUSE2,
	XCALL FILES (1,'SU',151,4)
INUSE1,
	XCALL PGCHN ('PO:POMENU',1)
;**********************************************************************

END

