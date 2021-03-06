;LINPRT.COP


RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	PARAMS
	.INCLUDE 'DEF:PARAM.DEF'

RECORD	LPKEY
	K_CODE	,A2
	K_KEY	,D1
	K_FILL	,A14

RECORD	HD1
	,A*,	'  LINER               PRICE'

RECORD	PRINT
	TITLE	,A*,	'DUCT PRICE TABLE'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PLINE	,A80
	PRNTON	,D1
	LINCNT	,D2,60
	PGCNT	,D6
	LPSW	,D2
	SPLFIL	,A14
	RPTNUM	,D3
	PRTTYP	,A1
	PRTCTL	,D3,080
	LPARG	,D1
	PRNTSW	,D1
	PRTCTR	,D1

RECORD	VARS
	I	,D6
	PDESC	,A15
	ENTRY	,A30
	INXCTL	,D1
	WHATNO	,D2
	CNGCTL	,D1
	STG	,D3
	ENG	,D3
	READ	,D1	,0
	WRITE	,D1	,1
	LOKCTL	,D1
	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'PRINT LINER PRICE TABLE',1)
	SWITCH = 5
	XCALL FILES (1,'SI',182,SWITCH)
	IF (SWITCH .EQ. 9) XCALL PGCHN ('CP:TBLMNU',1)

	CALL READ_LINER

	CLEAR PRNTON
PROCES,
	CLEAR LPKEY
	K_CODE = 'LP'
	K_KEY = 0

	FIND (1,COPTBL,LPKEY) [ERR=LOOP]
LOOP,		
	XCALL IOS (1,COPTBL,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (TBLCOD .NE. K_CODE) GOTO EOF

	PLINE (3,3) = LP_KEY,	'X'
	IF (LP_KEY.GT.0 .AND. LP_KEY.LE.9)
	THEN	BEGIN
		PDESC = DLINER(LP_KEY)
		PLINE (6,20) = PDESC(3,15)
		END
	ELSE	PLINE (6,20) = '*OUT OF RANGE*'

	PLINE (23,31) = LP_PRICE,	'Z,ZZX.XXX'
	CALL PRINT
	GOTO LOOP

EOF,
ENDOFF,
	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
	XCALL FILES (1,'SI',182,4)
	XCALL PGCHN ('CP:TBLMNU',1)

PRINT,
	IF (PRNTON .EQ. 0) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD,HD
&		,LG,LG,LG,0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;------------------------------------------------------------------------------

PRNTON,
	SPLFIL (5,6) = 'EF'
	LPSW = 1		;PRINT,SPOOL, OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	PRNTON = 1
	RETURN
;------------------------------------------------------------------------------
READ_LINER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; GET LINER DESCRIPTIONS FROM LINER PRICE TABLE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR COPTBL
	TBLCOD = 'LP'
	FOR I FROM 1 THRU 9
		BEGIN
		LP_KEY = I
		XCALL ISIO (1, COPTBL, TBL_KEY, READ, LOKCTL)
		IF (TBLCOD .NE. 'LP') EXITLOOP
		clear entry
		entry(1,1) = i,'X'
		entry(2,2) = '.'
		entry(4,15) = lp_liner
		DLINER(I) = entry(1,15)
		END

	RETURN
END
;==============================================================================
;  LINER               PRICE
;  X  AAAAAAAAAAAAAAA  ZZ,ZZX.XX
;123456789012345678901234567890123456789012345678901234567890123456789012345678
;         1         2         3         4         5         6         7        



