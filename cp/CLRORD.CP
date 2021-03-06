;  CLRORD / COP 
;
;		CLEARS ORDER FILE AFTER INVOICES ARE PRINTED
;
;	4/26/96 - CHANGED TO REPORT ERRORS (DETHIS NOW CLEARS RECORDS)
;

RECORD ORDHDR		
		.INCLUDE 'DEF:RD044A.DEF'
RECORD ORDLIN		
		.INCLUDE 'DEF:RD045A.DEF'
RECORD	,X
		.INCLUDE 'DEF:RD045D.DEF'
RECORD DUCACC
		.INCLUDE 'DEF:RD175A.DEF'
RECORD ERRFIL
		,A*,	'WRK:\CLRORD\'
		,A1,'E'
	DATE	,D6
		,A1,	'_'
	TIME	,D6
		,A4,'.ERR'
RECORD
	SWITCH	,D1
	ENDORD	,D1
	V	,D1
	READ	,D1,	0
;;;	WRITE	,D1,	1
;;;	DELETE	,D1,	3
	LOKCTL	,D1
	KORDNO	,D6
	PLINE	,A132
PROC
	XCALL TERID(V)
	XCALL OUTPT (1,1,1,'CHECKING OPEN ORDER FILES',V)
	XCALL RDATE(DATE)
	XCALL FLAGS (100,1)
	ONERROR GETTIME
GETTIME,
	XCALL TIME (TIME)
	OPEN (13,O,ERRFIL)
	OFFERROR
	XCALL FLAGS (100,0)
OPEN1,
	SWITCH = 5
	XCALL FILES (4,'SI',44,SWITCH)		;FILE # 44 -- ORDHDR FILE
OPEN2,
	SWITCH = 5
	XCALL FILES (5,'SI',45,SWITCH)		;FILE # 45 -- ORDLIN FILE
OPEN3,
	SWITCH = 5
	XCALL FILES (6,'SI',175,SWITCH)		;FILE # 175 -- DUCACC FILE
OPENLP,
;;;	OPEN (14,O,'LPA1:')
HDLOOP,
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.NE.0) GO TO EOFORD
	IF (OFLAG.NE.3) GO TO HDLOOP
	KORDNO = OORDNO
FINDLN,
	LORDNO = KORDNO
	LOKCTL = 1
	XCALL ISIO (5,ORDLIN,LORDNO,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.LORDNO.NE.KORDNO) GOTO FINDDU
LNLOOP,
	IF (LFLAG.EQ.3)
	BEGIN
	  LOKCTL = 1
;;;	  XCALL ISIO (5,ORDLIN,ORDKEY,DELETE,LOKCTL)
	  PLINE = 'ERROR: ORDLIN RECORD FOUND IN CLRORD: '
	  PLINE (39,80) = ORDLIN
;;;	  WRITES (14,PLINE)
	  WRITES (13,PLINE)
	  PLINE =
	END
	LOKCTL = 1
	XCALL IOS (5,ORDLIN,READ,LOKCTL)
	IF (LOKCTL.EQ.0.AND.LORDNO.EQ.OORDNO) GOTO LNLOOP
FINDDU,
	DUCTOR = KORDNO
	LOKCTL = 1
	XCALL ISIO (6,DUCACC,DUCTOR,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.KORDNO) GOTO EORDR
DULOOP,
	IF (FLG175.EQ.3)
	BEGIN
	  LOKCTL = 1
;;;	  XCALL ISIO (6,DUCACC,KEY175,DELETE,LOKCTL)
	  PLINE = 'ERROR: DUCACC RECORD FOUND IN CLRORD: '
	  PLINE (39,80) = DUCACC
;;;	  WRITES (14,PLINE)
	  WRITES (13,PLINE)
	  PLINE =
	END
	LOKCTL = 1
	XCALL IOS (6,DUCACC,READ,LOKCTL)
	IF (LOKCTL.EQ.0.AND.DUCTOR.EQ.KORDNO) GOTO DULOOP

EORDR,
	LOKCTL = 1
;;;	XCALL ISIO (4,ORDHDR,OORDNO,DELETE,LOKCTL)
	  PLINE = 'ERROR: ORDHDR RECORD FOUND IN CLRORD: '
	  PLINE (39,80) = ORDHDR
;;;	  WRITES (14,PLINE)
	  WRITES (13,PLINE)
	  PLINE =
	GOTO HDLOOP
EOFORD,
	PLINE = 'CLRORD ERROR REPORTING COMPLETED'
	WRITES (13,PLINE)
;;;	FORMS (14,0)
;;;	CLOSE 14
	CLOSE 13
	CLOSE 6
	CLOSE 5
	CLOSE 4
;;;> no longer needed>>	xcall pgchn ('cp:pstism',1)
	XCALL PGCHN ('CP:UNPRBL',1)
END
