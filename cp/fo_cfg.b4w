subroutine fo_cfg
	chn182,	d
	P_in,	a
	P_itm	,a
	P_mult	,d
	P_descr	,a
	P_err	,d	;1=error
	p_segs	,a	;XXYY, OPTIONAL

RECORD DESCRS
Spiral_Pipe,	a*,	"XX.XxYY gaGA x BB'LNG FLT-OVAL"

ReducerR,	a*,	'XX.XxYY / BB.BxCC gaGA REDUCER'
ReducerE,	a*,	'XX.XxYY / BB.Bxcc gaGA ECC.RED'

End_Caps,	a*,	'XX.XxYY gaGA FLT-OVAL END CAP'

Saddle9,	a*,	'XX.XxYY ON BB gaGA 90 SW.SADD'
Saddle4,	a*,	'XX.XxYY ON BB gaGA 45 SW.SADD'

Elbows,	a*,	'XX.XxYY gaGA 90 FLT-OVAL ELBOW'
Angles,	a*,	'XX.XxYY gaGA 45 FLT-OVAL ANGLE'

Connectors,	a*,	'XX.XxYY gaGA CONNECTOR'

Hangers,	a*,	'XX.XxYY FLT-OVAL DBL.ROD HANGR'
;Flanges and Clamps
EFI,		a*,	'XX.XxYY EZ-FLANGE INSTALLED'
EB,		a*,	'XX.XxYY BARREL CLAMP'
EFS,		a*,	'XX.XxYY EZ-FLANGE SET'
EF,		a*,	'XX.XxYY EZ-FLANGE'


RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	SCX
	DPND	,D6
	DSIZ	,D3
	DGA	,D2
	SP_SIZE	,D3		;SPIRAL SIZE
	WT_SIZE	,D3		;SPIRAL SIZE FOR WEIGHT 10-24-18
	INCH	,D3

record	segs
	s_xxyy	,d4
	s_bbcc	,d4

record	vars
	KITMNO	,A15
	GD	,D1	;1=GOT DOT
	RDR	,D1	;1= IS RD REDUCER
	BLANKS	,A30
	fx_key	,a5
	TDESCR	,A30
	T_MULT	,D3
	T_ITEM	,A15
	T1	,D6
	T2	,D6
	XXYY	,A4
	BBCC	,A4
	XX	,D2
	YY	,D2
	BB	,D2
	CC	,D2
	GA	,D2
	AXX	,A2
	AYY	,A2
	ABB	,A2
	ACC	,A2
	AGA	,A2
	FLAG	,D1
	LOKCTL	,D1
	READ	,D1,0
;
proc
	CLEAR P_ERR, RDR, XXYY, BBCC
	CLEAR BB, CC, XX, YY
	CLEAR FLAG

	CLEAR COPTBL
	TBLCOD = 'FO'

	using P_in select
	('FOSG'),	CALL SPIRAL
	('FOR','FOER'),	CALL REDUCER
	('FOEC'),	CALL END_CAPS
	('FOSW'),	CALL SADDLES
;this has to come before elbows...
	('FOEFI'),	CALL EFI	
	('FOEB'),	CALL EB	
	('FOEFS'),	CALL EFS
	('FOEF'),	CALL EF
	('FOE'),	CALL ELBOWS	;AND ANGLES
	('FOC'),	CALL CONNECTORS
	('FOH'),	CALL HANGERS
	(),		NOP
	endusing

	P_itm = T_ITEM
	P_descr = TDESCR
	if (FO_MULT .GT. 0)
	THEN	P_mult = FO_MULT
	ELSE	P_mult = 100
	P_err = FLAG

	ONERROR NOTNUM
	s_xxyy = xxyy
	s_bbcc = bbcc
	OFFERROR

	IF (%PASSED(P_SEGS)) P_SEGS = SEGS

	xreturn
NOTNUM,
	P_ERR = 1
	XRETURN
;=======================================================

SPIRAL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FOSGXXYY = SG DIA GA
;Spiral_Pipe,	a*,	"XX.XxYY gaGA x 10'LNG FLT-OVAL"

	FX_KEY = 'FOSG'
	XXYY = P_IN(5,8)
	CALL XXYY
	IF (FLAG) RETURN

	IF (P_IN(9,9) .EQ. '+')
	THEN	BEGIN
		INCH = 1
		BBCC = P_IN(10,11)
		END
	ELSE	BEGIN
		INCH = 0
		BBCC = P_IN(9,10)
		END

;;;	BBCC = P_IN(9,10)
	CALL BBCC
	IF (FLAG) RETURN
	
	CALL READ_TABLE			;READ COPTBL RECORD
	IF (FLAG) RETURN

	IF (INCH)
	THEN	T_ITEM = 'SG' + ^A(FO_DIA) + ^A(FO_GA) + '+' + ^A(BB)
	ELSE	T_ITEM = 'SG' + ^A(FO_DIA) + ^A(FO_GA) + ^A(BB)
	TDESCR = SPIRAL_PIPE

	CALL REPLACE

	IF (INCH) XCALL REPLC (TDESCR, "'", '"', FLAG)
	RETURN
;---------------------------------------------------------



REDUCER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FORXXYYBBCC  = XXxYY/BBxCC GAga Reducer
;FORXXYYBBRD  = XXxYY/BB" GAga Reducer
;12345678901
;ReducerR,	a*,	'XX.XxYY / BB.BxCC gaGA REDUCER'
;ReducerE,	a*,	'XX.XxYY / BB.Bxcc gaGA ECC.RED'

	USING P_IN SELECT
	('FOR'),	BEGIN
			XXYY = P_IN(4,7)
			BBCC = P_IN(8,11)
			FX_KEY = 'FOR'
			TDESCR = REDUCERR
			END
	('FOER'),	BEGIN
			XXYY = P_IN(5,8)
			BBCC = P_IN(9,12)
			FX_KEY = 'FOER'
			TDESCR = REDUCERE
			END
	ENDUSING

	IF (%INSTR (1, P_IN, 'RD') ) 
	THEN RDR = 1
	ELSE RDR = 0

	IF (RDR) XCALL REPLC (TDESCR, 'xCC', ' RD', FLAG)

	CALL XXYY
	IF (FLAG) RETURN

	CALL READ_TABLE			;READ COPTBL RECORD

	T_ITEM = FO_REF			;SMC ITEM NUMBER

	AXX = FO_XX, 'XX'
	AYY = FO_YY, 'ZX' [LEFT]
	AGA = FO_GA, 'XX'

	XCALL REPLC (TDESCR, 'XX.X', FO_AXX, FLAG)
	IF (FLAG) RETURN

	XCALL REPLC (TDESCR, 'YY', AYY, FLAG)
	IF (FLAG) RETURN

	XCALL REPLC (TDESCR, 'ga', AGA, FLAG)
	IF (FLAG) RETURN

	CALL BBCC
	ACC = CC, 'ZZ' [LEFT]
	ABB = BB, 'XX'

;Need a 2nd look up to validate BB & CC and to see if BB is BB.B
	FO_XX = BB
	FO_YY = CC
	FO_KEY = FX_KEY
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (RDR)
	THEN	IF (FO_XX .NE. BB) GOTO BAD_END
	ELSE	IF (LOKCTL .NE. 0) GOTO BAD_END
		

	IF (%INSTR (1, P_IN, 'RD') ) ACC =

	IF (ABB .NE. BLANKS) XCALL REPLC (TDESCR, 'BB.B', FO_AXX, FLAG)
	IF (FLAG) RETURN
	USING P_IN SELECT
	('FOER'),	IF (ACC .NE. BLANKS) XCALL REPLC (TDESCR, 'cc', ACC, FLAG)
	('FOR'),	IF (ACC .NE. BLANKS) XCALL REPLC (TDESCR, 'CC', ACC, FLAG)
	ENDUSING

	RETURN

BAD_END,
	TDESCR = 'NO COPTBL RECORD FOR BB/CC'
	CLEAR T_ITEM
	FLAG = 1
	RETURN
;--------------------------------------------------------

END_CAPS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FOECXXYY  = XX x YY GAga Flt-Oval End Cap
;12345678901
;End_Caps,	a*,	'XX.XxYY gaGA FLT-OVAL END CAP'
	
	FX_KEY = P_IN(1,4)

	XXYY = P_IN(5,8)
	CALL XXYY
	IF (FLAG) RETURN
	BBCC =

	CALL READ_TABLE			;READ COPTBL RECORD
	IF (FLAG) RETURN

	T_ITEM = FO_REF			;SMC ITEM NUMBER
	TDESCR = END_CAPS

	CALL REPLACE
	RETURN
;--------------------------------------------------------

SADDLES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FOSWAXXYY  = XX x YY on BB GAga 90 Swedge Saddle
;12345678901
;Saddle9,	a*,	'XX.XxYY ON BB gaGA 90 SW.SADD'
;Saddle4,	a*,	'XX.XxYY ON BB gaGA 45 SW.SADD'
	
	FX_KEY = P_IN(1,5)

	XXYY = P_IN(6,9)
	CALL XXYY
	IF (FLAG) RETURN

	BBCC = P_IN(10,11)
	CALL BBCC
	CALL READ_TABLE			;READ COPTBL RECORD
	IF (FLAG) RETURN

	T_ITEM = FO_REF			;SMC ITEM NUMBER

	USING P_IN(5,5) SELECT
	('4'),	TDESCR = SADDLE4
	('9'),	TDESCR = SADDLE9
	(),	BEGIN
		FLAG = 1
		RETURN
		END
	ENDUSING

	CALL REPLACE
	RETURN
;--------------------------------------------------------

ELBOWS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FOEXXYY90GA  = XX x YY GAga 90 Flt-Oval Elbow
;12345678901
;Elbows,	a*,	'XX.XxYY gaGA 90 FLT-OVAL ELBOW'
;Angles,	a*,	'XX.XxYY gaGA 45 FLT-OVAL ANGLE'
	
	FX_KEY = P_IN(1,3)
	FX_KEY(4,4) = P_IN(8,8)	;4 OR 9

	XXYY = P_IN(4,7)
	CALL XXYY
	IF (FLAG) RETURN

	BBCC = 
	CALL READ_TABLE			;READ COPTBL RECORD
	IF (FLAG) RETURN

	T_ITEM = FO_REF			;SMC ITEM NUMBER

	USING P_IN(8,8) SELECT
	(4),	TDESCR = ANGLES
	(9),	TDESCR = ELBOWS
	(),	BEGIN
		FLAG = 1
		RETURN
		END
	ENDUSING

	CALL REPLACE
	RETURN
;--------------------------------------------------------
CONNECTORS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FOECXXYY  = XX x YY GAga Flt-Oval End Cap
;12345678901
;End_Caps,	a*,	'XX.XxYY gaGA FLT-OVAL END CAP'
	
	FX_KEY = P_IN(1,3)	;FOC

	XXYY = P_IN(4,7)
	CALL XXYY
	IF (FLAG) RETURN

	BBCC =

	CALL READ_TABLE			;READ COPTBL RECORD
	IF (FLAG) RETURN

	T_ITEM = FO_REF			;SMC ITEM NUMBER
	TDESCR = CONNECTORS

	CALL REPLACE
	RETURN
;--------------------------------------------------------
HANGERS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FOECXXYY  = XX x YY GAga Flt-Oval End Cap
;12345678901
;End_Caps,	a*,	'XX.XxYY gaGA FLT-OVAL END CAP'
	
	FX_KEY = P_IN(1,3)	;FOH

	XXYY = P_IN(4,7)
	CALL XXYY
	IF (FLAG) RETURN

	BBCC =

	CALL READ_TABLE			;READ COPTBL RECORD
	IF (FLAG) RETURN

	T_ITEM = FO_REF			;SMC ITEM NUMBER
	TDESCR = HANGERS

	FO_GA = 				;NO GAUGE
	CALL REPLACE
	RETURN
;--------------------------------------------------------
EFI,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EFI,		a*,	'XX.XxYY EZ-FLANGE INSTALLED'
	
	FX_KEY = P_IN(1,5)

	XXYY = P_IN(6,9)
	CALL XXYY
	IF (FLAG) RETURN
	BBCC =

	CALL READ_TABLE			;READ COPTBL RECORD
	IF (FLAG) RETURN

	T_ITEM = FO_REF			;SMC ITEM NUMBER
	TDESCR = EFI

	FO_GA = 				;NO GAUGE
	CALL REPLACE
	RETURN
;--------------------------------------------------------
EB,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EB,		a*,	'XX.XxYY BARREL CLAMP'
	
	FX_KEY = P_IN(1,4)

	XXYY = P_IN(5,8)
	CALL XXYY
	IF (FLAG) RETURN
	BBCC =

	CALL READ_TABLE			;READ COPTBL RECORD
	IF (FLAG) RETURN

	T_ITEM = FO_REF			;SMC ITEM NUMBER
	TDESCR = EB

	FO_GA = 				;NO GAUGE
	CALL REPLACE
	RETURN
;--------------------------------------------------------
EFS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EFS,		a*,	'XX.XxYY EZ-FLANGE SET'
	
	FX_KEY = P_IN(1,5)

	XXYY = P_IN(6,9)
	CALL XXYY
	IF (FLAG) RETURN
	BBCC =

	CALL READ_TABLE			;READ COPTBL RECORD
	IF (FLAG) RETURN

	T_ITEM = FO_REF			;SMC ITEM NUMBER
	TDESCR = EFS

	FO_GA = 				;NO GAUGE
	CALL REPLACE
	RETURN
;--------------------------------------------------------
EF,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EF,		a*,	'XX.XxYY EZ-FLANGE'
	
	FX_KEY = P_IN(1,4)

	XXYY = P_IN(5,8)
	CALL XXYY
	IF (FLAG) RETURN
	BBCC =

	CALL READ_TABLE			;READ COPTBL RECORD
	IF (FLAG) RETURN

	T_ITEM = FO_REF			;SMC ITEM NUMBER
	TDESCR = EF

	FO_GA = 				;NO GAUGE
	CALL REPLACE
	RETURN
;--------------------------------------------------------

XXYY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL ALPDC (XXYY(1,2), XX, FLAG)
	IF (FLAG) RETURN
	XCALL ALPDC (XXYY(3,4), YY, FLAG)
	RETURN
;---------------------------------------------------------

BBCC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL ALPDC (BBCC(1,2), BB, FLAG)
	IF (FLAG) RETURN
	XCALL ALPDC (BBCC(3,4), CC, FLAG)
	RETURN
;---------------------------------------------------------

READ_TABLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FO_XX = XX
	FO_YY = YY
	FO_KEY = FX_KEY
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		TDESCR = 'NO COPTBL RECORD'
		CLEAR T_ITEM
		FLAG = 1
 		RETURN
		END
	IF (%INSTR(1, FO_AXX, '.')) 
	THEN GD = 1
	ELSE GD = 0
	RETURN
;-----------------------------------------------------------------------

REPLACE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	AXX = FO_XX, 'XX'
	AYY = FO_YY, 'ZX' [LEFT]
	ABB = BB, 'ZZ'
	ACC = CC, 'ZZ' [LEFT]
	AGA = FO_GA, 'ZZ'

	XCALL REPLC (TDESCR, 'XX.X', FO_AXX, FLAG)

;;;	IF (GD)
;;;	THEN	XCALL REPLC (TDESCR, 'XX', FO_AXX, FLAG)
;;;	ELSE	XCALL REPLC (TDESCR, 'XX', AXX, FLAG)
	IF (FLAG) RETURN

	XCALL REPLC (TDESCR, 'YY', AYY, FLAG)
	IF (FLAG) RETURN

	IF (AGA .NE. BLANKS) XCALL REPLC (TDESCR, 'ga', AGA, FLAG)
	IF (FLAG) RETURN

	IF (ABB .NE. BLANKS) XCALL REPLC (TDESCR, 'BB', ABB, FLAG)
	IF (FLAG) RETURN
	IF (ACC .NE. BLANKS) XCALL REPLC (TDESCR, 'CC', ACC, FLAG)
	IF (FLAG) RETURN

	RETURN
;----------------------------------------------------------------------
;from scrnx
;;;SPIRAL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	IF (KITMNO.EQ.'SX' .AND. MAT_SEL.NE.GALV)
;;;		BEGIN
;;;		XCALL OLMSG(WND_1, 23,"DON'T SELECT MATERIAL",1)
;;;		CONFIG_ERROR = 1
;;;		RETURN
;;;		END
;;;
;;;	IF (KITMNO(7,7).EQ.'+')		;PROCESS INCHES
;;;	THEN	BEGIN
;;;		 ONERROR SPIRAL_ERROR
;;;		INCH = KITMNO(8,10)
;;;		 OFFERROR
;;;		TDESCR = '__"-__GA X ___"LONG SPIRL ____'
;;;		TDESCR(12,14) = INCH,'ZZX'
;;;		DPND = (INCH*100) / 12
;;;		IF (DPND(5,6).NE.0) 
;;;		THEN DSIZ = DPND(1,4) + 1 
;;;		ELSE DSIZ = DPND(1,4)
;;;		END
;;;	ELSE	BEGIN
;;;		INCH = 
;;;		 ONERROR SPIRAL_ERROR
;;;		DSIZ = KITMNO(7,8)
;;;		 OFFERROR
;;;		if (dsiz.le.0) goto spiral_error		;3-14-13 per bill
;;;		TDESCR = '__"-__GA X __''SPIRAL PIPE ____'
;;;		TDESCR(12,13) = DSIZ,'ZX'
;;;		DSIZ = KITMNO(7,8)
;;;		END
;;;
;;;	IF (DSIZ.GT.30) GOTO SPIRAL_ERROR
;;;
;;;	TDESCR(1,2) = KITMNO(3,4)	;DIA
;;;	TDESCR(5,6) = KITMNO(5,6)	;GA

;;;	IF (MAT_SEL .EQ. ALUM)			;SSQ 5-12-03
;;;		BEGIN
;;;		DGA = KITMNO(5,6)	;GA
;;;		USING DGA SELECT
;;;		(18),	TDESCR(5,8) = '.063'
;;;		(20),	TDESCR(5,8) = '.050'
;;;		(22),	TDESCR(5,8) = '.040'
;;;		(24),	TDESCR(5,8) = '.032'
;;;		ENDUSING
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SSQ 8-7-02
;;;	LITMNO(7,15) = 
;;;	SP_SIZE = DSIZ
;;;	WT_SIZE = DSIZ		;10-24-18

; 5-12-03: force price to zero if alum or sst...
;;;	IF (KITMNO.EQ.'SG') 
;;;	THEN	BEGIN
;;;		USING MAT_SEL SELECT
;;;		(GALV),	TDESCR(27,30) = 'GALV'
;;;		(ALUM),	BEGIN
;;;			TDESCR(27,30) = 'ALUM'
;;;			SP_SIZE = 0		;FORCE PRICE TO ZERO
;;;			END
;;;		(SST),	BEGIN
;;;			TDESCR(27,30) = 'SST'
;;;			SP_SIZE = 0		;FORCE PRICE TO ZERO
;;;			END
;;;		(PGRP),	TDESCR(27,30) = 'PGRP'
;;;		(VIN),	BEGIN
;;;			TDESCR(27,30) = 'PVS'
;;;			SP_SIZE = 0		;8-2-17 SSQ
;;;			END
;;;
;;;		ENDUSING
;;;
;;;		if (clvl1.gt.0)
;;;		then	xf1 = f1_key(clvl1)
;;;		else	xf1 = 0
;;;
;;;		USING XF1 SELECT
;;;		(80 THRU 83), SP_SIZE = 0	;FORCE PRICE TO ZERO
;;;		ENDUSING
;;;		END
;;;
;;;	ELSE	TDESCR(27,30) = 'PVS '
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;	XCALL ISIO (1, INVMAS, LITMNO, READ, LOKCTL)	;need this for price
;;;	IF (LOKCTL .NE. 0) GOTO SPIRAL_ERROR		;bad item
;;;	lpwgt = ip_wgt					;4-23-18 this is the correct item for weight...
;;;	RETURN
;;;
;;;SPIRAL_ERROR,
;;;	OFFERROR
;;;	CONFIG_ERROR = 1
;;;	XCALL W_DISP(WND_1, WD_POS,23,1,WD_CLR, WDC_LIN)
;;;	DISPLAY (15,'ITEM CODE ENTERED: ',KITMNO,     '/ ITEM CODE CALCULATED: ',LITMNO      )
;;;	XCALL OLMSG(WND_1,23,'INVALID ITEM NUMBER FOR SPIRAL',2)
;;;	RETURN
;----------------------------------------------------------------

	end
