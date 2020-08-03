;prcmat.cp
;
;	SET PRICES FOR materials based on galv price & mult
;

RECORD	STPRIC
	.INCLUDE 'DEF:STPRIC.DEF'

RECORD	STPRI1
	.INCLUDE 'DEF:STPRI1.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	FILPRC
	F_DEV	,A3
		,A1,	':'
	F_NAME	,A6
		,A1,	'.'
	F_EXT	,A3


RECORD	VARS
	XTYP	,4A2,	'ST','SS','CS','FT'
	XCON	,4A2,	'MN','S4','S9','RD'
	I	,D6
	J	,D6
	K	,D6
	L	,D6
	ERR	,D1
	MULT	,D4
	PRICE	,D5
	SWITCH	,D1
	V	,D1
;
PROC
	XCALL TERID(V)
	XCALL OUTPT (1,1,2,'RECALCULATE PRICE MATRIX - for material',1)


	XCALL FFILE(1,FILPRC,SWITCH)	;SSQ 9-9-03
	F_NAME = 'XTPRIC'		;SSQ 9-9-03
	F_EXT = 'ISM'			;SSQ 9-9-03

	OPEN(1,SU,FILPRC)		;matl tables
	open (2, si, filprc)		;galv mat tables

	xcall files (3,'SI', 182, SWITCH)

	CLEAR COPTBL
	TBL_KEY = 'CF'
	READ (3, COPTBL, TBL_KEY)	;PARAMETERS

	FIND (1,STPRIC,^first)
LOOP,
	READS (1,STPRIC,EOF)
	if (tmat .eq. 'G') goto loop	;will look up galv record below...
;;;	if (tmat .eq. 'P') goto loop	;skip PVC

	stpri1 = stpric
	tmat1 = "G"			;galv...
	read (2, stpri1, st_key1) [err=loop]	;no galv record
		

	
	using tmat select
	('A'),	mult = alumul
	('R'),	mult = pgrmul
	('S'),	mult = sstmul
	('P'),	mult = pvsmul		;7-12-17
	(),	goto loop		;?? mat - skip
	endusing
	

	for i from 1 thru 80 array(i) = (array1(i) * mult)#2
	write (1, stpric, st_key)
	goto loop


;-------------------------------------------------------------
		
EOF,
	CLOSE 1
	close 2
	CLOSE 3
	XCALL MESAG('material PRICES HAVE BE UPDATED',1)
	XCALL PGCHN ('CP:CFGMNT',1)
	STOP
;==================================================
;--------------------------------------------------

	

	END




