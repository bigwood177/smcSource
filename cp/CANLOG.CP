;CANLOG.CP
;		CREATED FROM ORDNUM.CP 12-27-99 SSQ
;
; ORDNUM.COP - PROGRAM TO PRINT A JOURNAL OF ORDER NUMBERS FROM 
;		3 SOURCES - ORDHDR / ORDHST / CANLOG
;
;
RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


RECORD ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'
RECORD ORDCTL
	.INCLUDE 'DEF:RD176B.DEF'
RECORD REDFIL
		,A4
	FILNAM	,A10
RECORD HDR1
	,A50,'----- ORDER -----   SL   -------- CUSTOMER -------'
	,A50,'--------------   P.O.           SHIP     STATUS --'
	,A32,'--------------------------------'
RECORD HDR2
	,A50,'NUMBER     DATE     MN   NUMBER   NAME            '
	,A50,'                 NUMBER         DATE     FL SOURCE'
	,A32,'    TYPE     MESSAGE            '

RECORD	DIS
	II	,D6

RECORD	VARS
	STDAT	,D8
	ENDAT	,D8
	ORDT	,A1
	V	,D1
	TITLE	,A*,	'CANCELLED ORDER REGISTER'
	PLINE	,A132
	LPSW	,D1
	SPLFIL	,A14
	LINCNT	,D2,62
	PGCNT	,D3
	RPTNUM	,D3
	PRTTYP	,A1
	LPARG	,D1
	SWITCH	,D1
	WRCNT	,D5
	READ	,D1,	0
	WRITE	,D1,	1
	LOKCTL	,D1
	FILE	,D1
	LSTORD	,D6
	DTMASK	,A8,	'XX/XX/XX'
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	ENTRY	,A30
	FSTOPN	,D6,	0
	RECNO	,D5
	STORDR	,D6
PROC 
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'CANCELLED ORDER NUMBER JOURNAL',1)
	XCALL FFILE (68,REDFIL,SWITCH)
	FILNAM = 'CANWRK'
ASK,
	XCALL OUTPT (10,20,0,'BUILD OR PRINT <P> ?',1)
	XCALL INPUT (10,45,01,00,'AE',ENTRY,INXCTL,1)
	GOTO (ASK,ABORT), INXCTL
	IF (ENTRY.EQ.' ') ENTRY = 'P'
	IF (ENTRY.NE.'P'.AND.ENTRY.NE.'B') GOTO ASK
	IF (ENTRY.NE.'B') GOTO PRINTIT
DISPLA,
	XCALL OUTPT (1,1,2,'CANCELLED ORDER NUMBER JOURNAL',1)
	CLEAR CNGCTL
	XCALL OUTPT (4,6,0,'1. START DATE',1)
	XCALL OUTPT (6,6,0,'2. END   DATE',1)
	XCALL OUTPT (8,6,0,'3. ORDER TYPE (O/E/B)',1)
STDAT,
	XCALL INPUT (4, 21,08,05,'DE',ENTRY,INXCTL,1)
	GOTO (DISPLA,ABORT),INXCTL
	STDAT = ENTRY(1,8)
	GOTO (ANYCN),CNGCTL
ENDAT,
	XCALL INPUT (6, 21,08,05,'D ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	ENDAT = ENTRY(1,8)
	GOTO (ANYCN), CNGCTL
ORDT,
	XCALL INPUT (8,28, 01, 00, 'A ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	ORDT = ENTRY(1,1)
	IF (ORDT.NE.'O'.AND.ORDT.NE.'E'.AND.ORDT.NE.'B') GOTO ORDT
ANYCN,
	IF (ENDAT .LT. STDAT)
		BEGIN
		XCALL MESAG ('END DATE LESS THAN START DATE',1)
		GOTO STDAT
		END
	XCALL ANYCN(CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR), CNGCTL+1
CNGBR,
	GOTO (STDAT, ENDAT, ORDT), WHATNO
	GOTO ANYCN
PROCES,
	XCALL WATE (4,1)
	CLEAR II
BUILD,
	OPEN (1,O,FILNAM)
	ORDCTL =
	LOKCTL = 1
	XCALL IOS (1,ORDCTL,WRITE,LOKCTL)
	WRCNT = 1

OF3,
	XCALL FILES (4,'I',176,4)
	XCALL OUTPT (2,1,1,'PROCESSING CANCELLED ORDER LOG: CANLOG',1)
	SWITCH = 1
	XCALL FILES (4,'I',177,SWITCH)
	IF (SWITCH.EQ.9)
	BEGIN
	  XCALL MESAG ('CANLOG FILE NOT AVAILABLE - CANNOT RUN THIS REPORT',1)
	  GOTO ABORT
	END
	FILE = 3
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	GOTO RDLOOP

RDLOOP,
	INCR II
	IF (II/250*250 .EQ. II) XCALL OUTPT (1,70,1,DIS,1)
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.NE.0) GOTO EOF
	IF (ORDHDR.EQ.']]]]]]') GOTO EOF
	IF (OORDNO.EQ.0) GOTO RDLOOP
	IF (OORDDT.LT.STDAT .OR. OORDDT.GT.ENDAT) GOTO RDLOOP
	IF (ORDT.NE.'B' .AND. OLOC(1,1).NE.ORDT) GOTO RDLOOP

	OLOC(2,2) = FILE
	LOKCTL = 1
	XCALL IOS (1,ORDHDR,WRITE,LOKCTL)
	INCR WRCNT
	GOTO RDLOOP
EOF,
;;;	GOTO (OF2,OF3), FILE
	XCALL FILES (4,'I',177,4)
	XCALL FILL (']', ORDHDR)

	LOKCTL = 1
	XCALL IOS (1,ORDHDR,WRITE,LOKCTL)
	LOKCTL = 1
	XCALL IOS (1,ORDHDR,WRITE,LOKCTL)
	CLOSE 1
	OPEN (1,U,FILNAM)
	LOKCTL = 1
	XCALL IO (1,ORDCTL,1,READ,LOKCTL)
	REC176 = WRCNT
	MAX176 = WRCNT+1
	ORG176 = WRCNT
	ORDCTL (100,105) = 'ORDNUM'
	LOKCTL = 1	
	XCALL IO (1,ORDCTL,1,WRITE,LOKCTL)
	CLOSE 1
	XCALL OUTPT (2,1,1,'SORTING TEMP FILE',1)
	SORT (INPUT=FILNAM,RECORD=ORDHDR,KEY=(OORDNO/A,OLOC/D))
	XCALL MESAG ('READY TO PRINT',2)
PRINTIT,
	OPEN (1,I,FILNAM)
	LOKCTL = 1
	XCALL IOS (1,ORDHDR,READ,LOKCTL)
	IF (ORDHDR(100,105).NE.'ORDNUM')
	BEGIN
	  XCALL MESAG ('FILE MUST BE REBUILT',2)
	  CLOSE 1
	  GOTO ASK
	END
	XCALL OUTPT (2,1,1,'OUTPUT LIST',1)
	LPSW = 1	; PRINT, SPOOL OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GO TO CLOSES
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	LSTORD = -1
	RECNO = 1
OUTLP,
	INCR RECNO
	LOKCTL = 1
	XCALL IO (1,ORDHDR,RECNO,READ,LOKCTL)
	IF (ORDHDR.EQ.']]]]]]') GOTO EOF2
	IF (OORDNO.EQ.0) GOTO OUTLP
	IF (FSTOPN.EQ.0.AND.OLOC.EQ.'O ')
	BEGIN
	  PLINE = '-------- FIRST OPEN ORDER --------------------------------------------'
	  CALL PRINT
	  FSTOPN = OORDNO
	END
OLP2,
;;;	IF (LSTORD.NE.-1.AND.OORDNO.GT.LSTORD+1)
;;;	BEGIN
;;;	  PLINE (1,6) = LSTORD+1
;;;	  PLINE (114,129) = 'BREAK IN SEQUENCE'
;;;	  CALL PRINT
;;;	  LSTORD = LSTORD + 1
;;;	  GOTO OLP2
;;;	END
	PLINE (1,6) = OORDNO

	XCALL DATE8(OORDDT, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE (10,17) = D_OUT, DTMASK
	PLINE (21,22) = OSLMAN
	PLINE (26,31) = OCUSNO
	PLINE (35,64) = OCUSNM
	PLINE (68,77) = OPONO

	XCALL DATE8(OSHDAT, D_OUT, D_OUTR, D_FMT, D_SW)
	IF (D_OUT.NE.0) PLINE (81,88) = D_OUT, DTMASK
	PLINE (92,93) = OLOC
	USING OLOC(2,2) SELECT
	  ('1'),	PLINE (95,104) = 'OPEN      '
	  ('2'),	PLINE (95,104) = 'INVOICED  '
	  ('3'),	PLINE (95,104) = 'CANCELLED ' 
	ENDUSING
	USING OLOC(1,1) SELECT
	  ('E'),	PLINE (105,113) = 'ESTIMATE '
	  ('O'),	PLINE (105,113) = 'ORDER    '
	ENDUSING

	IF (OLOC(2,2).EQ.'2'.AND.(OSHDAT.NE.0.AND.OINVDT.NE.0)) 
&		PLINE (114,129) = 'SHIPPED & BILLED'
	IF (OLOC(2,2).EQ.'1'.AND.(OSHDAT.NE.0.AND.OINVDT.NE.0)) 
&		PLINE (114,129) = 'INVOICE IN PROC '
	IF (OSHDAT.NE.0.AND.OINVDT.EQ.0) PLINE (114,132) = 'SHIPPED ONLY    ***'
	IF (OSHDAT.EQ.0.AND.OINVDT.NE.0) PLINE (114,132) = 'BILLED  ONLY    ***'
	IF (LSTORD.EQ.OORDNO) PLINE (114,132) = 'DUPLICATE       ***'
	CALL PRINT
	LSTORD = OORDNO
	GOTO OUTLP
PRINT,
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HDR1,HDR2,' ','NO LEGEND',
&		' ',' ',0,0,132,0,LPSW,RPTNUM,PRTTYP)
	RETURN
EOF2,
	PLINE =
	CALL PRINT
	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
CLOSES,
	CLOSE 1
ABORT,
	XCALL PGCHN ('CP:SPCFUN',1)
END

