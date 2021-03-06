;INTERNAL ROUTINE FOR "SRTCSH"
 
RECORD	FFILE
	FILNUM	,D3
	CLCTL	,D1
	REDFIL	,A14
 
 
RECORD	HEADER
		,A7,	'DBSORT '
	HFILE	,A14
 
RECORD	VARS
	XFIL	,D3,	006

	FILNUM = 6		;CASH00
	XCALL FFILE (FILNUM,REDFIL,CLCTL)	;GET FILE SPEC
	HFILE = REDFIL
 
	XCALL OUTPT (2,1,2,HEADER,1)
 
	SORT (IN=REDFIL,
&		RECORD=SRTREC,
&		KEY = (CCUSNO,CRCTDT,CCHKNO,CAPLNO)
&		)
 
	RETURN
	END
 
