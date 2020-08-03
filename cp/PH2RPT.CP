;ph2rpt.dbl	by product cat

;phyrpt.dbl
;
record	phyinv
	.include 'def:rd196a.def'
;
record	coptbl
	.include 'def:rd182a.def'
;
record	invmas
	.include 'def:rd041a.def'

RECORD	HD1
		,A*,	'ITEM            DESCRIPTION                      '
		,A*,	' IN-INV       COUNT    PRICE     TOTAL   PCT   INV-CALC'

RECORD	HD2
		,A*,	'PRODUCT CAT: '
	HD2_C	,A2

RECORD	CHANNEL
	CHN041	,D2
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

RECORD	TOTALS
	TOTAMT	,D12
	TOTVAL	,D12
	CTAMT	,D12
	CTVAL	,D12

RECORD	VARS
	OPNOK	,D1
	KEY	,A15
	A3	,A3
	ITMRFA	,A6
	IVFLAG	,D1
	VANFLG	,D1
	VANRFA	,A6
	PCT	,D3
	X_PRICE	,D8
	SAVCAT	,A2
	A2	,A2
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	STCAT	,A2
	ENCAT	,A2
	i	,d6
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

DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'PHYSICAL INVENTORY VALUATION',1)
	XCALL OUTPT (4,4,0,'1. START PRD CAT: ',1)
	XCALL OUTPT (6,4,0,'2. END   PRD CAT: ',1)
STCAT,
	XCALL INPUT (4,23,02,00,'AE',ENTRY,INXCTL,1)
	GOTO (DISPLA,ENDOFF),INXCTL
	STCAT = ENTRY(1,2)
	IF (STCAT .EQ. A2)
		BEGIN
		ENCAT = 'ZZ'
		XCALL OUTPT (4,23,1,'ALL',1)
		XCALL OUTPT (6,23,1,' ',1)
		CNGCTL = 1
		END
	GOTO (ANYCNG),CNGCTL
ENCAT,
	XCALL INPUT (6,23,02,00,'AE',ENTRY,INXCTL,1)
	GOTO (DISPLA,DISPLA),INXCTL
	ENCAT = ENTRY(1,2)
	IF (ENCAT .EQ. A2)
		BEGIN
		ENCAT = STCAT
		XCALL OUTPT (6,23,1,ENCAT,1)
		END
	GOTO (ANYCNG),CNGCTL
ANYCNG,
	XCALL ANYCN (CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR),CNGCTL+1
CNGBR,
	GOTO (STCAT, ENCAT), WHATNO
	GOTO ANYCNG
PROCES,
	CLEAR TOTAMT, TOTVAL, CTAMT, CTVAL
	SAVCAT = '-1'
	FIND (CHN196, PHYINV, ^FIRST,KEYNUM:1) [ERR=LOOP]	;BY PRDCAT/ITEM
LOOP,
	READS (CHN196, PHYINV, EOF)
	IF (P_CAT .LT. STCAT) GOTO LOOP
	IF (P_CAT .GT. ENCAT) GOTO EOF
	IF (P_CAT .NE. SAVCAT) CALL NEWCAT

	CALL GET_MEMOS
	CALL GET_MULT
	CALL TBL_PRICE

	PLINE (1,15) = P_ITEM
	PLINE (17,46) = P_DESC
	pline (48,48) = '['
	PLINE (49,56) = P_ONH,	'ZZZ,ZZX-'
	pline (57,57) = ']'
	PLINE (58,68) = P_COUNT,	'ZZZ,ZZX.XXX'
	PLINE (70,77) = P_PRICE,	'ZZZX.XXX'

;;;	EXT = (P_COUNT*P_PRICE)#1
	EXT = (P_COUNT*P_PRICE)#4
	PLINE (79,88) = EXT,	'ZZZ,ZZX.XX'
	PLINE (91,91) = '.'
	PLINE (92,94) = PCT,	'ZZX' [LEFT]

	VAL = (EXT * PCT)#2
	PLINE (95,104) = VAL,	'ZZZ,ZZX.XX'
	CALL PRINT

	CTAMT = CTAMT + EXT
	CTVAL = CTVAL + VAL

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

NEWCAT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (SAVCAT .EQ. '-1') GOTO ENDCAT

	PLINE (44,60) = 'PRD CAT XX TOTALS'
	PLINE (52,53) = SAVCAT
;                      12345678901234567890
;                               5         6
	PLINE (72,84) = CTAMT,	'ZZ,ZZZ,ZZX.XX'
	PLINE (88,100) = CTVAL,	'ZZ,ZZZ,ZZX.XX'
	CALL PRINT

	LINCNT = 66
	TOTAMT = TOTAMT + CTAMT
	TOTVAL = TOTVAL + CTVAL
ENDCAT,
	SAVCAT = P_CAT
	HD2_C = P_CAT
	CLEAR CTAMT, CTVAL

	RETURN
;-------------------------------------------------------------

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (PRNTON .EQ. 0) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD2,HD
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
	CALL NEWCAT

	PLINE (55,60) = 'TOTALS'
	PLINE (72,84) = TOTAMT,	'ZZ,ZZZ,ZZX.XX'
	PLINE (88,100) = TOTVAL,	'ZZ,ZZZ,ZZX.XX'
	CALL PRINT

	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
	PRNTON = 0
ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:PHYMNU',1)
	STOP

TBL_PRICE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;look up itmmas record...

	key = p_item
	xcall gtitm (key, p_f1, p_f2, p_f3, itmrfa, ivflag, vanflg, vanrfa)

	read (1, invmas, rfa:itmrfa) [err=no_matl]

	if (imat2 .eq. a3) return	;no mat'l code

; change "price" to be price per unit counted.  itmmas.weight is lbs/unit,
; so qty counted * weight is the weight of the qty counted.  the price in
; the "I2" table is price/lb, so qty*weight*i2_price is the total amount
; for the qty counted.  Therefore, (weight*i2_price) is the price for the
; qty counted. 

; look up price/lb for this material in table...
	clear tbl_key
	tblcod = 'I2'
	i2_mat = imat2
	i2_ga = iga
	read (chn182, coptbl, tbl_key) [err=no_matl]

	x_price = (weight * i2_price)#2	; itmmas.weight is lbs/unit counted,
				; so (weight*i2_price) is price/unit counted
	P_PRICE = X_PRICE
no_matl,
	return
;------------------------------------------------


RETURN
;-------------------------------------------------------

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
	PCT = PH_PCT
	RETURN

NO_MULT,
	PCT = 0
	RETURN
;-------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

;;;	OPEN (1,SI,'SMC:COPTBL.SMM')
	SWITCH = 5
	XCALL FILES (18, 'SI', 182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 18

;;;	OPEN (2,SI,'SMC:PHYINV.SMM')
	SWITCH = 5
	XCALL FILES (2, 'SI', 196, SWITCH)	; 196 - PHYINV.ISM
	IF (SWITCH .EQ. 9) RETURN
	CHN196 = 2

	SWITCH = 5
	XCALL FILES (1, 'SI', 041, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 1

	OPNOK = 1

	RETURN
;--------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN182) CLOSE CHN182
	IF (CHN196) CLOSE CHN196
	IF (CHN041) CLOSE CHN041

	RETURN
;--------------------------------------------

;ITEM            DESCRIPTION                      COUNT    PRICE     TOTAL
;AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA [zzz,zzx] ZZZ,ZZX ZZZX.XXX ZZZ,ZZX.XX  .XXX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
;AAA AAA AAAAA   AAAAAAAAAAAA AAAAAAAAAAAA AAAAAAAAAAAA


;ZZZ,ZZX.XX
;1234567890123456789012345678901234567890123456789012
;         9         0         1         2         3

