;phyrpt.dbl
;
record	phyinv
	.include 'def:rd196a.def'
;
record	coptbl
	.include 'def:rd182a.def'
;
RECORD	HD1
		,A*,	'ITEM            DESCRIPTION                      '
		,A*,	'    COUNT    PRICE     TOTAL    PCT   INV-CALC'

RECORD	CHANNEL
	CHN196	,D2
	CHN182	,D2

RECORD	PRINT
	TITLE	,A*,	'PHYSICAL INVENTORY COST %'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PRNTON	,D1
	LINCNT	,D2,60
	PGCNT	,D6
	LPSW	,D2
	SPLFIL	,A14
	RPTNUM	,D3
	PRTTYP	,A1
	PRTCTL	,D3,132
	LPARG	,D1
	PRNTSW	,D1
	PRTCTR	,D1

RECORD	PBUFF
	PLINE	,A132

RECORD	VARS
	OPNOK	,D1
	i	,d6
	TOTAMT	,D12
	TOTVAL	,D12
	MM_DES1	,A30
	MM_DES2	,A30
	MM_DES3	,A30

	EXT	,D12
	VAL	,D12
	DAMPR	,D1
	ZERO	,A5,	'00000'
	DM	,D3
	LN	,D3
	DESC1	,A90
	SWITCH	,D1
	V	,D1
;
proc
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'PHYSICAL INVENTORY VALUATION',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	CLEAR TOTAMT, TOTVAL
LOOP,
	READS (CHN196, PHYINV, EOF)

	CALL GET_MEMOS
	CALL GET_MULT

	PLINE (1,15) = P_ITEM
	PLINE (17,46) = P_DESC
	PLINE (48,58) = P_COUNT,	'ZZZ,ZZX.XXX'
	PLINE (60,67) = P_PRICE,	'ZZZX.XXX'

;;;	EXT = (P_COUNT*P_PRICE)#1
	EXT = (P_COUNT*P_PRICE)#4
	PLINE (69,78) = EXT,	'ZZZ,ZZX.XX'
;;;	PLINE (81,81) = '.'
	PLINE (82,84) = PH_PCT,	'ZZX' [LEFT]

	VAL = (EXT * PH_PCT)#2
	PLINE (85,94) = VAL,	'ZZZ,ZZX.XX'
	CALL PRINT

	TOTAMT = TOTAMT + EXT
	TOTVAL = TOTVAL + VAL

	PLINE ( 1,3) = P_F1
	PLINE ( 5,7) = P_F2
	PLINE (9,13) = P_F3

	IF (DAMPR) 
	THEN	DESC1 = 'DAMPER'
	ELSE	CLEAR DESC1

	DM = 90			;# OF CHAR IN DESC1
	LN = %TRIM(DESC1)
	LN = LN + 1
	if (P_F1 .ne. zero)
		begin
		DESC1(LN,LN) = '*'
		INCR LN
		end
	DESC1 (LN,DM) = MM_DES1
	LN = %TRIM(DESC1)
	LN = LN + 1
	if (P_F2 .ne. zero)
		begin
		DESC1(LN,LN) = '*'
		INCR LN
		end
	DESC1 (LN,DM) = MM_DES2
	LN = %TRIM(DESC1)
	LN = LN + 1
	if (P_F3 .ne. zero)
		begin
		DESC1(LN,LN) = '*'
		INCR LN
		end
	DESC1 (LN,DM) = MM_DES3

	PLINE (17,106) = DESC1
	CALL PRINT

	GOTO LOOP


PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (PRNTON .EQ. 0) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD,HD
&		,LG,LG,LG,0,132,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;-------------------------------------------------------------
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
;-------------------------------------------------


EOF,
	PLINE (55,60) = 'TOTALS'
	PLINE (62,74) = TOTAMT,	'ZZ,ZZZ,ZZX.XX'
	PLINE (78,90) = TOTVAL,	'ZZ,ZZZ,ZZX.XX'
	CALL PRINT

	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
	PRNTON = 0
ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:PHYMNU',1)
	STOP


GET_MEMOS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR MM_DES1, MM_DES2, MM_DES3

	CLEAR TBLKEY
	TBLCOD = 'M1'
	MM_KEY = P_F1
	READ (CHN182, COPTBL, TBL_KEY) [ERR=NO_F1]
	MM_DES1 = MM_SHORT
NO_F1,
	CLEAR TBLKEY
	TBLCOD = 'M2'
	MM_KEY = P_F2
	READ (CHN182, COPTBL, TBL_KEY) [ERR=NO_F2]
	MM_DES2 = MM_SHORT
NO_F2,
	CLEAR TBLKEY
	TBLCOD = 'M3'
	MM_KEY = P_F3
	READ (CHN182, COPTBL, TBL_KEY) [ERR=NO_F3]
	MM_DES3 = MM_SHORT
NO_F3,
	RETURN
;-------------------------------------------------------

GET_MULT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBLKEY
	TBLCOD = 'PH'
	PH_CAT = P_CAT
	READ (CHN182, COPTBL, TBL_KEY) [ERR=NO_MULT]
	RETURN

NO_MULT,
	PH_PCT = 0
	RETURN
;-------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

;;;	OPEN (1,SI,'SMC:COPTBL.SMM')
	SWITCH = 5
	XCALL FILES (1, 'SI', 182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 1

;;;	OPEN (2,SI,'SMC:PHYINV.SMM')
	SWITCH = 5
	XCALL FILES (2, 'SI', 196, SWITCH)	;196 - PHYINV.ISM
	IF (SWITCH .EQ. 9) RETURN
	CHN196 = 2

	OPNOK = 1

	RETURN
;--------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN182) CLOSE CHN182
	IF (CHN196) CLOSE CHN196

	RETURN
;--------------------------------------------

;ITEM            DESCRIPTION                      COUNT    PRICE     TOTAL
;AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA ZZZ,ZZX ZZZX.XXX ZZZ,ZZX.XX  .XXX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
;AAA AAA AAAAA   AAAAAAAAAAAA AAAAAAAAAAAA AAAAAAAAAAAA


;ZZZ,ZZX.XX
;1234567890123456789012345678901234567890123456789012
;         9         0         1         2         3
