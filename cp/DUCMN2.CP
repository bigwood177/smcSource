;DUCMN2.CP
;
; DUCT REPORTS
;
;ducmnu.cp
;PRCMNU.CP

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

MENU,
	XCALL OUTPT (1,1,2,'DUCT REPORTS',1)
	XCALL OUTPT (3,9,0,'PLEASE SELECT APPLICATION',1)
	XCALL OUTPT (5,15,0,'1. DUCT PRODUCTION REPORT',1)
	XCALL OUTPT (6,15,0,'2. DUCT PRODUCTION HISTORY',1)
	XCALL OUTPT (7,15,0,'3. DUCT USAGE REPORT',1)
MINPUT,
	XCALL INPUT (3,36,1,1,'#E',ENTRY,INXCTL,1)
	GOTO (MINPUT,ENDOFF), INXCTL
	PGM = ENTRY(1,1)

	USING PGM SELECT
	(1),	XCALL PGCHN ('CP:DPTRPT',1)
	(2),	XCALL PGCHN ('CP:DUCHST',1)
	(3),	XCALL PGCHN ('CP:DUCUSG',1)
	(),	GOTO MINPUT
	ENDUSING
ENDOFF,
	XCALL PGCHN ('CP:IMMENU',1)
	END





