;ARQMNU.CP
;	QUOTE MANAGEMENT
;

RECORD	VARS
	CNGCTL	,D1
	WHATNO	,D2
	TODAY	,D8
	XDATE	,D8
	ENTRY	,A30
	SELECT	,D1
	STORDR	,D8
	ENORDR	,D8
	INXCTL	,D1
	NITE	,D1	;1= NITE BATCH
	V	,D1

PROC 
	XCALL TERID (V)
	XCALL RDAT8 (TODAY)
DISPLA,
	XCALL OUTPT (1,1,2,'QUOTE MANAGEMENT',V)
	XCALL OUTPT (5,20,0,'PLEASE SELECT APPLICATION',V)
	XCALL OUTPT (6,25,0,'1. ARCHIVE QUOTES',V)
	XCALL OUTPT (7,25,0,'2. ARCHIVED QUOTE REPORT',V)
	XCALL OUTPT (8,25,0,'3. QUOTE TO ORDER REPORT',V)
	XCALL OUTPT (9,25,0,'4. OPEN ESTIMATES BY SALESREP',V)
	XCALL OUTPT(10,25,0,'5. ORDER/QUOTE STATISTICS',V)
INPUT,
	XCALL INPUT (5,47,1,1,'#E',ENTRY,INXCTL,V)
	GO TO (END), INXCTL-1

	SELECT = ENTRY(1,1)
	IF (SELECT.LT.1.OR.SELECT.GT.5) GO TO INPUT
	USING SELECT SELECT
	(1),	XCALL PGCHN ('CP:ARQMNT',1)
	(),	BEGIN
		CALL REPORTS
		GOTO DISPLA
		END
	ENDUSING

END,
	XCALL PGCHN ('CP:OEMNU',1)

REPORTS,
	NITE = 0
	XCALL OUTPT (1,1,2,'QUOTE MANAGEMENT',V)
	
ASK,
	XCALL OUTPT (12,20,0,'START DATE:             END DATE:       ',1)
;                             01234567890123456789012345678901234
	XCALL INPUT (12,32,08,00,'D ',ENTRY,INXCTL,1)
	GOTO (ASK), INXCTL
	STORDR = ENTRY
	IF (STORDR.EQ.0) 
		BEGIN
		STORDR = TODAY
		XDATE(1,4) = STORDR(5,8)
		XDATE(5,8) = STORDR(1,4)
		ENTRY(1,10) = XDATE,	'ZX/XX/XXXX'
		XCALL OUTPT (12,32,0,ENTRY(1,10),1)
		END

	XCALL INPUT (12,54,08,00,'D ',ENTRY,INXCTL,1)
	GOTO (ASK), INXCTL
	ENORDR = ENTRY
	IF (ENORDR.EQ.0) 
		BEGIN
		ENORDR = STORDR
		XDATE(1,4) = ENORDR(5,8)
		XDATE(5,8) = ENORDR(1,4)
		ENTRY(1,10) = XDATE,	'ZX/XX/XXXX'
		XCALL OUTPT (12,54,0,ENTRY(1,10),1)
		END
	IF (ENORDR.LT.STORDR) 
	BEGIN
	  XCALL MESAG ('ENDING DATE MUST BE GREATER OR EQUAL TO STARTING DATE',1)
	  GOTO ASK
	END

	CNGCTL = 2
	XCALL ANYCN(CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR),CNGCTL + 1
CNGBR,
	GOTO ASK

PROCES,

	USING SELECT SELECT
	(2),	BEGIN
		XCALL OUTPT (1,1,2,'ARCHIVED QUOTE REPORT',1)
		XCALL ARQRP (STORDR, ENORDR, NITE)
		END
	(3),	BEGIN
		XCALL OUTPT (1,1,2,'QUOTE TO ORDER REPORT',1)
		XCALL QUTRP (STORDR, ENORDR, NITE)
		END
	(4),	BEGIN
		XCALL OUTPT (1,1,2,'OPEN ESTIMATES BY SALESREP',1)
		XCALL SLMES (STORDR, ENORDR, NITE)
		END
	(5),	BEGIN
		XCALL OUTPT (1,1,2,'OPEN ESTIMATES BY SALESREP',1)
		XCALL OUTPT(1,1,2,'ORDER/QUOTE STATISTICS',1)
		XCALL QUTR2 (STORDR, ENORDR, NITE)
		END
	ENDUSING

	RETURN
;-------------------------------------

	STOP

END



