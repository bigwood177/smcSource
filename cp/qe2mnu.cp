;QE2MNU.CP
;	NEW QUOTE EXPRESS TABLES MENU

RECORD	VARS
	PGM	,D1
	ROW	,D2
	ENTRY	,A30
	INXCTL	,D1
	WHATNO	,D2
	SELECT	,D1
	CNGCTL	,D1
	V	,D1

PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'PRICE DUCT TABLES',1)

MENU,
	XCALL OUTPT (1,1,2,'PRICE DUCT TABLES',1)
	XCALL OUTPT (3,9,0,'PLEASE SELECT APPLICATION',1)
	XCALL OUTPT (5,15,0,'1. F2-NOTE EXCEPTIONS',1)
	XCALL OUTPT (6,15,0,'2. MATERIAL SPECIFIC CODES',1)
MINPUT,
	XCALL INPUT (3,36,1,1,'#E',ENTRY,INXCTL,1)
	GOTO (MINPUT,ENDOFF), INXCTL
	PGM = ENTRY(1,1)

	USING PGM SELECT
	(1),	XCALL PGCHN ('CP:QE1MNT',1)
	(2),	XCALL PGCHN ('CP:QE2MNT',1)
;	(),	GOTO MINPUT
	ENDUSING
ENDOFF,
	XCALL PGCHN ('CP:TBLMNU',1)
	END



