;cfg_f.pc
.subroutine cfg_f			;flat oval configurator
	litmno		,a
	cfg_price	,d
	tdesc		,a
	config_error	,D
	mat_sel		,d
	st_guage	,a

; NOTE that internal routine "get_price" provides an
;	exit point from routine !!

;	9-06-07 added RW (welded reducers)
;
;	configurator for new items
;
RECORD	FILPRC
	F_DEV	,A3
		,A1,	':'
	F_NAME	,A6
		,A1,	'.'
	F_EXT	,A3

RECORD	STPRIC
	.INCLUDE 'DEF:STPRIC.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CHANNEL
	CHN182	,D2,	17
	CHNSTP	,D2,	9

RECORD	MATERIALS
	.INCLUDE 'DEF:CFGMAT.DEF'

RECORD
	SEGS	,A12			;first 2 dig are gauge
RECORD,X
	GA	,D2
	SARA	,5D2

RECORD	VARS
	DIFF	,D6		;10-8-14
	fake_ga	,d2
	a2	,a2
	SADDL	,D1		;1=is saddle, 0=not saddle
	N_PRICE	,D10
	N_NUM	,D10
	N_DIG	,D1
	SCON	,A2			;"TCON" FOR SADDLES
	D12	,D12
	CMP_PRICE	,D8		;COMPONENT PRICE
	MAX_SEG	,D2,	05
	KITMNO	,A15
	SAVKEY	,A7
	MA	,D2
	B1	,D2
	B2	,D2
	R1	,D2
	R2	,D2
	IDX	,D2
	I	,D2
	MAT	,A1
;;;	GA	,D2
	DG	,D2
	BF	,D1
	ITYPE	,A30
	IMAT	,A1
	LN	,D2
	ST	,D2
	READ	,D1,0
	LOKCTL	,D1
	SWITCH	,D1
.proc

	KITMNO = LITMNO

	clear fake_ga
	if (st_guage .ne. a2) 
		begin
		xcall alpdc (st_guage,fake_ga,bf)
		if (bf.eq.1) clear fake_ga
		end
	clear st_guage

;;;	IMAT = 'G'
	USING MAT_SEL SELECT
	(GALV),	IMAT = 'G'
	(ALUM),	IMAT = 'A'
	(PGRP),	IMAT = 'R'
	(SST),	IMAT = 'S'
	(VIN),	IMAT = 'P'	;SSQ 7-12-12
	ENDUSING

	CLEAR CONFIG_ERROR, SADDL
	clear dg, segs		;ssq 4-25-18

;read in multipliers in case they are needed...	
	CLEAR TBL_KEY
	TBLCOD = 'CF'
	XCALL ISIO(CHN182, COPTBL, TBL_KEY, READ, LOKCTL)

;;;	USING KITMNO(1,2) SELECT
	USING KITMNO SELECT
	('BN'),				CALL BN		;BULLNOSE TEE
	('C9','C4','CV9','CV4'),	CALL CROSS	;CROSS
	('CT','CC','CTV','CCV'),	CALL TEE	;CONICAL TEE/CROSS
	('T4','T9','TV4','TV9'),	CALL TEE	;TEE
	('ERW'),			CALL W_REDU	;4-10-19
	('ER'),				CALL REDU	;ECCENTRIC REDUCER
	('R16' THRU 'R26'),		CALL REDU
	('RV16' THRU 'RV26'),		CALL REDU
	('RW16' THRU 'RW20'),		CALL W_REDU
	('STC','STT'),			CALL SHOE
	('S'),				CALL SADDLE
	('P'),				CALL PP		;PAIR/PANTS
	(),				CONFIG_ERROR = 1;02-08-07 just in case..
	ENDUSING

	if (config_error .eq. 2) xreturn	;11-14-13

	IF (CFG_PRICE .EQ. 0) CONFIG_ERROR = 1
	IF (CONFIG_ERROR .EQ. 0) 
	THEN	CALL MAKE_DESC
	ELSE	BEGIN
		TDESC = "** ERROR **"
		CFG_PRICE = 0
	;;;	XCALL MESAG('CONFIGURATOR ERROR',1)
		END
	CALL ROUND_NICKLE		;round up to nearest nickle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-3-03 ssq these are now in price table...

;;;	USING MAT_SEL SELECT		;MATERIAL MULT
;;;	(ALUM),	CFG_PRICE = (CFG_PRICE * ALUMUL)#2
;;;	(SST),	CFG_PRICE = (CFG_PRICE * SSTMUL)#2
;;;	(PGRP),	CFG_PRICE = (CFG_PRICE * PGRMUL)#2
;;;	ENDUSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XRETURN
;=================================================================
SADDLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SADDL = 1		;11-15-05 SSQ
	ST = 2
	USING LITMNO SELECT
	('SWV'),	BEGIN
			IMAT = 'P'
			SCON = 'SS'
			ITYPE = 'SWDG SADD'
			KITMNO = LITMNO(4,15)
			ST = 4
			END
	('SCV'),	BEGIN
			IMAT = 'P'
			SCON = 'CS'
			ITYPE = 'CON/SADDL'
			KITMNO = LITMNO(4,15)
			ST = 4
			END
	('SC'),		BEGIN
			SCON = 'CS'
			IF (IMAT.EQ.'G' .AND. MAT_SEL.EQ.GALV)
			THEN	ITYPE = 'CONICAL SADDL'
			ELSE	ITYPE = 'CON/SADDL'
			KITMNO = LITMNO(3,15)
			ST = 3
			END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	('STC'),	BEGIN
;;;			CLEAR SADDL
;;;			SCON = 'STC'
;;;			ITYPE = 'SHOE TAP CRSS'
;;;			KITMNO = LITMNO(4,15)
;;;			ST = 4
;;;			xcall alpdc(kitmno(1,1), dg, bf)
;;;			goto (cross_error),bf
;;;			segs = kitmno(2,11)
;;;			SEGS = KITMNO(2,11)
;;;			goto saddl_logic
;;;			END
;;;	('STT'),	BEGIN
;;;			CLEAR SADDL
;;;			SCON = 'STT'
;;;			ITYPE = 'SHOE TAP TEE'
;;;			KITMNO = LITMNO(4,15)
;;;			ST = 4
;;;			xcall alpdc(kitmno(1,1), dg, bf)
;;;			goto (cross_error),bf
;;;			segs = kitmno(2,11)
;;;			goto saddl_logic
;;;			END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	('ST'),		BEGIN
			SCON = 'ST'
			ITYPE = 'SHOE TAP'
			KITMNO = LITMNO(3,15)
			ST = 3
			END
	('SW'),		BEGIN
			IF (IMAT.EQ.'G' .AND. MAT_SEL.EQ.GALV)
			THEN	ITYPE = 'SWEDGE SADDLE'
			ELSE	ITYPE = 'SWDG SADD'
			SCON = 'SS'
			KITMNO = LITMNO(3,15)
			ST = 3
			END
	('S4','S9'),		BEGIN
			ITYPE = 'SADDLE'
			SCON = 'FT'
			KITMNO = LITMNO(2,15)
			ST = 2
			END
	('SV'),		BEGIN
			IMAT = 'P'
			SCON = 'FT'
			ITYPE = 'SADDLE'
			KITMNO = LITMNO(3,15)
			ST = 3
			END
	(),		BEGIN
			CONFIG_ERROR = 1
			GOTO SADDLE_ERROR
			END
	ENDUSING


; kitmno now has everything prior to degree stripped off...

	XCALL ALPDC(KITMNO(1,1), DG, BF)
	GOTO (SADDLE_ERROR),BF

	SEGS = KITMNO(2,7)
saddl_logic,
	CALL PARSE_SEGS
	GOTO (SADDLE_ERROR),BF
	MA = SARA(2)
	B1 = SARA(1)

;validation rules...
	IF (MA.EQ.0 .OR. B1.EQ.0) GOTO SADDLE_ERROR
;;;	IF (B1.LT.1 .OR. B1.GT.50) GOTO SADDLE_ERROR	;7-25-13
	IF (B1.LT.1 .OR. B1.GT.99) GOTO SADDLE_ERROR	;7-25-13
	IF (MA.LT.1 .OR. MA.GT.99) GOTO SADDLE_ERROR
	IF (B1 .GT. MA) GOTO SADDLE_ERROR
	IF (IMAT .EQ. 'P' .AND. GA.EQ.26) GOTO SADDLE_ERROR

	IF (LITMNO .EQ. 'SC9')	;PER SM 10-8-14
		BEGIN
		DIFF = MA - B1
		IF (DIFF .LT. 2) GOTO SADDLE_ERROR
		END


	CALL MAIN		;MAIN
	CFG_PRICE =		;PRICING IS BASED ON BRANCH
	
	USING DG SELECT
	(9),	TCON = 'S9'
	(4),	TCON = 'S4'
	(),	GOTO SADDLE_ERROR
	ENDUSING

	IDX = B1
	TTYP = SCON
	CALL GET_PRICE		;per stevem 1-15-03 price is based on main only

	CFG_PRICE = CFG_PRICE * 10	;.XXX
	LITMNO(ST+1,15) = 
	RETURN

SADDLE_ERROR,

	CONFIG_ERROR = 1
	RETURN
;-------------------------------------------------
SHOE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SADDL = 1		;11-15-05 SSQ
	ST = 2

	USING LITMNO SELECT
	('STC'),	BEGIN
			CLEAR SADDL
			SCON = 'ST'
			ITYPE = 'SHOE TAP CRSS'
			KITMNO = LITMNO(4,15)
			ST = 4
			END
	('STT'),	BEGIN
			CLEAR SADDL
			SCON = 'ST'
			ITYPE = 'SHOE TAP TEE'
			KITMNO = LITMNO(4,15)
			ST = 4
			END
	(),		BEGIN
			CONFIG_ERROR = 1
			GOTO SHOE_ERROR
			END
	ENDUSING


; kitmno now has everything prior to degree stripped off...

;;;	XCALL ALPDC(KITMNO(1,1), DG, BF)
;;;	GOTO (SHOE_ERROR),BF
;;;	SEGS = KITMNO(2,11)

	SEGS = KITMNO
	CALL PARSE_SEGS
	GOTO (SHOE_ERROR),BF
	MA = SARA(1)
	R1 = SARA(2)
	B1 = SARA(3)
	B2 = SARA(4)

;validation rules...
	IF (MA.EQ.0 .OR. B1.EQ.0) GOTO SHOE_ERROR
	IF (B1.LT.1 .OR. B1.GT.50) GOTO SHOE_ERROR
	IF (MA.LT.1 .OR. MA.GT.99) GOTO SHOE_ERROR
	IF (B1 .GT. MA) GOTO SHOE_ERROR
	IF (IMAT .EQ. 'P' .AND. GA.EQ.26) GOTO SHOE_ERROR


	CALL MAIN		;MAIN

	
;;;	USING DG SELECT
;;;	(9),	TCON = 'S9'
;;;	(4),	TCON = 'S4'
;;;	(),	GOTO SHOE_ERROR
;;;	ENDUSING
	
	IF (R1 .LT. MA)
		BEGIN
		IDX = MA	;PRICING BASED ON DIM OF MAIN
		USING GA SELECT
		(16,18,20),	CALL W_REDUCER		;8-23-18
		(),		CALL REDUCER
		ENDUSING
		END

	TCON = 'S9'		;ALWAYS 90
	IDX = B1
	TTYP = SCON
	CALL GET_PRICE		

	IDX = B2
	TTYP = SCON
	CALL GET_PRICE		

	CFG_PRICE = CFG_PRICE * 10	;.XXX
	LITMNO(ST,15) = 
	RETURN

SHOE_ERROR,

	CONFIG_ERROR = 1
	RETURN
;-------------------------------------------------

BN,
;BN926101012 = 90deg 26ga 12main 12branch(90) 12reducer 12 reducer
;12345678901
	ST = 3
;;;	ITYPE = 'BULLNOSE TEE'
	ITYPE = 'BULLNOSE'		;ssq 6-11-03 so "pgr" will fit
	KITMNO = LITMNO
	IF (KITMNO(3,3) .EQ. 'V')
		BEGIN
		IMAT = 'P'		;PVS
		KITMNO(3,15) = LITMNO(4,15)
		ST = 4
		END



	KITMNO = LITMNO(ST,15)
	XCALL ALPDC(KITMNO(1,1), DG, BF)
;;;	IF (DG.NE.4 .AND. DG.NE.9) BF = 1
	IF (DG.NE.9) BF = 1			;NO 45
	GOTO (BN_ERROR),BF
	SEGS = KITMNO(2,11)

	CALL PARSE_SEGS
	GOTO (BN_ERROR),BF

	R1 = SARA(1)
	R2 = SARA(2)
	MA = SARA(3)
	B1 = MA			;BRANCH (SADDLE) SAME AS MAIN

;validation rules...
;;;	IF (R1 .GT. MA) GOTO BN_ERROR
	IF (R1 .GT. MA-1) GOTO BN_ERROR
	IF (R2 .GT. MA-1) GOTO BN_ERROR
	IF (MA.EQ.0 .OR. R1.EQ.0 .OR. R2.EQ.0) GOTO BN_ERROR
	CALL MAIN		;MAIN
	
	IDX = MA

	USING GA SELECT
	(16,18,20),	BEGIN
			CALL W_REDUCER		;8-23-18
			CALL W_REDUCER		;8-23-18
			END
	(),		BEGIN
			CALL REDUCER
			CALL REDUCER
			END
	ENDUSING

;;;	CALL REDUCER
;;;	CALL REDUCER

	USING DG SELECT
	(9),	TCON = 'S9'
	(4),	TCON = 'S4'
	(),	GOTO BN_ERROR
	ENDUSING

	IDX = B1
	CALL GET_PRICE
	CFG_PRICE = CFG_PRICE * 10	;.XXX
	LITMNO(ST+1,15) = 

	RETURN
BN_ERROR,
	CONFIG_ERROR = 1
	RETURN
;-------------------------------------------------

TEE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; TEES AND CONICAL TEES
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CT926121008 = 90deg 26ga 12main 10reducer 8branch,
;12345678901
	USING LITMNO SELECT
	('CT'),	ST = 3
	('CC'),	ST = 3
	('T'),	BEGIN
		ITYPE = 'TEE'
		ST = 2
		IF(LITMNO.EQ.'T4' .OR. LITMNO.EQ.'TV4') ITYPE = 'LATERAL'	;SSQ 08-31-04
		END
	ENDUSING
	KITMNO = LITMNO(ST,15)

	IF (KITMNO(1,1) .EQ. 'V')
		BEGIN
		IMAT = 'P'		;PVS
		ST = ST + 1
		KITMNO = LITMNO(ST,15)
		END

	USING LITMNO SELECT
	('CT'),	BEGIN
		IF (IMAT.EQ.'G' .AND. MAT_SEL.EQ.GALV)
		THEN	ITYPE = 'CONICAL TEE'
		ELSE	ITYPE = 'CONI.TEE'
		END
	('CC'),	BEGIN
		IF (IMAT.EQ.'G' .AND. MAT_SEL.EQ.GALV)
		THEN	ITYPE = 'CONICL CROSS'
		ELSE	ITYPE = 'CONICL'
		END
	ENDUSING
; kitmno now has everything prior to degree stripped off...

	XCALL ALPDC(KITMNO(1,1), DG, BF)
	IF (DG.NE.4 .AND. DG.NE.9) BF = 1
	GOTO (CT_ERROR),BF

	USING LITMNO SELECT
	('CT','CC'),	IF(DG .EQ. 4) GOTO CT_ERROR	;NO 45
	ENDUSING

	SEGS = KITMNO(2,11)
tee_logic,
	CALL PARSE_SEGS
	GOTO (CT_ERROR),BF
	MA = SARA(1)
	R1 = SARA(2)
	B1 = SARA(3)
	B2 = SARA(4)

;validation rules...
	IF (LITMNO.NE.'CC' .AND. B2.GT.0) GOTO CT_ERROR		;12-23-13 TOO MANY SEGS
	IF (MA.EQ.0 .OR. R1.EQ.0 .OR. B1.EQ.0) GOTO CT_ERROR
	IF (R1 .GT. MA) GOTO CT_ERROR
;;;	IF (R1 .NE. MA) GOTO ERROR2	;11-14-13
	IF (B1 .GT. MA) GOTO CT_ERROR
	
	USING LITMNO SELECT
	('CC'),		BEGIN
			IF(B1 .GT. MA-2) GOTO CT_ERROR
			IF(B2 .EQ. 0) GOTO CT_ERROR
			END
	('CT'),		BEGIN
			IF(B1 .GT. MA-2) GOTO CT_ERROR
			IF(B2 .NE. 0) GOTO CT_ERROR
			END
	ENDUSING

	CALL MAIN		;MAIN
	
	IDX = MA
;;;	CALL REDUCER
	IF (MA .GT. R1) 		;>>CALL REDUCER
		BEGIN
		USING GA SELECT
		(16,18,20),	CALL W_REDUCER		;8-23-18
		(),		CALL REDUCER
		ENDUSING
		END

	
	TCON = 'S4'		;FOR BOTH 90deg and 45deg
	IF (LITMNO(1,1) .EQ. 'T')
		BEGIN
		IF(DG .EQ. 9) TCON = 'S9'
		END

	IDX = B1
	CALL GET_PRICE
	IDX = B2
	CALL GET_PRICE

	CFG_PRICE = CFG_PRICE * 10	;.XXX

	LITMNO(ST+1,15) =			;RETURN PART
	RETURN

CT_ERROR,
	CONFIG_ERROR = 1
	RETURN
;-------------------------------------------------

REDU,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; REDUCERS AND ECCENTRIC REDUCERS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ER261210 = 26ga 12main 10reducer
;12345678

	USING LITMNO SELECT
	('E'),		BEGIN
			IF(ITYPE.EQ.'G' .AND. MAT_SEL.EQ.GALV)
			THEN	ITYPE = 'ECCENTRIC REDUCER'
			ELSE	ITYPE = 'ECCENTRIC'
		;;;	ITYPE = 'ECCENTRIC REDUCER'
		;;;	ITYPE = 'ECCENTRIC '
			ST = 3
			END
	('R'),		BEGIN
			ITYPE = 'REDUCER'
			ST = 2
			END
	ENDUSING

	KITMNO = LITMNO(ST,15)

	IF (KITMNO(1,1) .EQ. 'V')
		BEGIN
		IMAT = 'P'		;PVS
		ST = ST + 1
		KITMNO = LITMNO(ST,15)
		END

	USING LITMNO SELECT
	('E'),		BEGIN
			IF(ITYPE.EQ.'G' .AND. MAT_SEL.EQ.GALV)
			THEN	ITYPE = 'ECCENTRIC REDUCER'
			ELSE	ITYPE = 'ECCENTRIC'
		;;;	ITYPE = 'ECCENTRIC REDUCER'
		;;;	ITYPE = 'ECCENTRIC '
			END
	ENDUSING

	SEGS = KITMNO
	CALL PARSE_SEGS
	GOTO (ER_ERROR),BF
	MA = SARA(1)
	R1 = SARA(2)
	DG =		;1/15/03 SSQ

	ln = %trim (litmno)
	if (ln .eq. 5)	;pressed part  R1612
		begin
		ma = ga		;there is no ga
		r1 = sara(1)
		sara(1) = ma
		sara(2) = r1
		ga = 24
		ST_GUAGE = GA		;SAVE THE GA
		end

;validation rules...
	IF (MA.EQ.0 .OR. R1.EQ.0) GOTO ER_ERROR
	IF (R1 .GT. MA-1) GOTO ER_ERROR
;;;	IF (IMAT.EQ.'P' .AND. GA.EQ.26) GOTO ER_ERROR	;removed per SM 6-23-03

;------------------------------------------------
; ssq 5/12/05: don't call "main", not using price, invalid if no price in 
;		"main" matrix".
;;;	CALL MAIN		;MAIN

; ssq 5/12/05: but still need to setup key...
	CLEAR CFG_PRICE, ST_KEY
	TTYP = 'FT'			;FITTINGS

	TMAT = IMAT
	TGA = GA			;GAUGE
	IDX = MA
;------------------------------------------------
	cfg_price =		;price is just reducer price
	IDX = MA
	CALL REDUCER

	USING LITMNO SELECT
	('E'),	CFG_PRICE = (CFG_PRICE*ECRMUL)#1		;.XXX
	(),	CFG_PRICE = CFG_PRICE * 10			;.XXX
	ENDUSING

	LITMNO(ST+2,15) = 		;R26
	RETURN

ER_ERROR,
	CONFIG_ERROR = 1
	RETURN
;-------------------------------------------------
W_REDU,		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; WELDED REDUCERS AND ECCENTRIC REDUCERS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;WR201210 = 20ga 12main 10reducer
;EWR201210 = 20ga 12main 10reducer
;123456789

	USING LITMNO SELECT
	('E'),		BEGIN
			IF(ITYPE.EQ.'G' .AND. MAT_SEL.EQ.GALV)
			THEN	ITYPE = 'WELD ECCENTRIC REDUCER'
			ELSE	ITYPE = 'WELD ECCENTRIC'
			ST = 4
			END
	('R'),		BEGIN
			ITYPE = 'WELD REDUCER'
			ST = 3
			END
	ENDUSING

	KITMNO = LITMNO(ST,15)


	SEGS = KITMNO
	CALL PARSE_SEGS
	GOTO (ERW_ERROR),BF
	MA = SARA(1)
	R1 = SARA(2)
	DG =		;1/15/03 SSQ

;validation rules...
	IF (MA.EQ.0 .OR. R1.EQ.0) GOTO ERW_ERROR
	IF (R1 .GT. MA-1) GOTO ERW_ERROR

;------------------------------------------------
; ssq 5/12/05: don't call "main", not using price, invalid if no price in 
;		"main" matrix".
;;;	CALL MAIN		;MAIN

; ssq 5/12/05: but still need to setup key...
	CLEAR CFG_PRICE, ST_KEY
	TTYP = 'FT'			;FITTINGS

	TMAT = IMAT
	TGA = GA			;GAUGE
	IDX = MA
;------------------------------------------------
	cfg_price =		;price is just reducer price
	IDX = MA
	CALL W_REDUCER

	USING LITMNO SELECT
	('E'),	CFG_PRICE = (CFG_PRICE*ECRMUL)#1		;.XXX
	(),	CFG_PRICE = CFG_PRICE * 10			;.XXX
	ENDUSING

	LITMNO(ST+2,15) = 		;R26
	RETURN

ERW_ERROR,
	CONFIG_ERROR = 1
	RETURN
;-------------------------------------------------

CROSS,
;C92612100808 = 20ga 12main 12 reducer(12->10) 8branch(90) 8 branch(90)
;123456789012

	ST = 2
	ITYPE = 'CROSS'
	IF (KITMNO(2,2) .EQ. 'V')
		BEGIN
		IMAT = 'P'		;PVS
		KITMNO(2,15) = LITMNO(3,15)
		ST = 3
		END

	XCALL ALPDC(KITMNO(2,2), DG, BF)
	GOTO(CROSS_ERROR),BF

	SEGS = KITMNO(3,12)
cross_logic,
	CALL PARSE_SEGS
	GOTO(CROSS_ERROR),BF
	MA = SARA(1)
	R1 = SARA(2)
	B1 = SARA(3)
	B2 = SARA(4)

;validation rules:
	IF (MA.EQ.0 .OR. R1.EQ.0 .OR. B1.EQ.0 .OR. B2.EQ.0) GOTO CROSS_ERROR
	IF (R1 .GT. MA) GOTO CROSS_ERROR
;;;	IF (R1 .NE. MA) GOTO ERROR2
	IF (B1 .GT. MA) GOTO CROSS_ERROR
	IF (B2 .GT. MA) GOTO CROSS_ERROR
	IF (IMAT.EQ.'P' .AND. GA.EQ.26) GOTO CROSS_ERROR

	CALL MAIN		;MAIN

	IF (R1 .LT. MA)
		BEGIN
		IDX = MA	;USE LARGER DIM. FOR REDUCER (10->12 use 12)
		USING GA SELECT
		(16,18,20),	CALL W_REDUCER		;8-23-18
		(),		CALL REDUCER
		ENDUSING
	;;;	CALL REDUCER
		END

	USING DG SELECT
	(9),	TCON = 'S9'
	(4),	TCON = 'S4'
	(),	GOTO CROSS_ERROR
	ENDUSING

	IDX = B1
	CALL GET_PRICE		;BRANCH 1

	IDX = B2		;BRANCH 2
	CALL GET_PRICE

	CFG_PRICE = CFG_PRICE * 10	;TO .XXX
	LITMNO(ST+1,15) = 
	RETURN

CROSS_ERROR,
	CONFIG_ERROR = 1
	RETURN
;--------------------------------------------------------
ERROR2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; 11-14-13 Cross/Tee main & reducer must be the same.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CONFIG_ERROR = 2
	RETURN
;-------------------------------------------------

PP,
;Pair/Pants (90 & 45 DEG)
;P920140808 = 20ga 14main 8branch(45) 8branch(45)
;1234567890

	ST = 2
	ITYPE = 'PAIR/PANTS'
	IF (KITMNO(2,2) .EQ. 'V')
		BEGIN
		IMAT = 'P'		;PVS
		KITMNO(2,15) = LITMNO(3,15)
		ST = 3
		END

	XCALL ALPDC(KITMNO(2,2), DG, BF)
	GOTO (PP_ERROR),BF

	SEGS = KITMNO(3,10)
	CALL PARSE_SEGS
	GOTO (PP_ERROR),BF
	MA = SARA(1)
	B1 = SARA(2)
	B2 = SARA(3)

;validation rules:
	IF (DG.EQ.9 .AND. GA.gt.20) GOTO PP_ERROR
;;;	IF (DG.EQ.9 .AND. GA.NE.20) GOTO PP_ERROR
;;;	IF (B1.GT.MA-1 .OR. B2.GT.MA-1) GOTO PP_ERROR
	IF (B1.GT.MA .OR. B2.GT.MA) GOTO PP_ERROR
	IF (MA.EQ.0 .OR. B1.EQ.0 .OR. B2.EQ.0) GOTO PP_ERROR

	CALL MAIN			;PRICE THE MAIN

	TCON = 'S4'
	IDX = B1
	CALL GET_PRICE

	TCON = 'S4'
	IDX = B2
	CALL GET_PRICE

	USING DG SELECT
	(9),	CFG_PRICE = (CFG_PRICE*PP9MUL)#1		;.XXX
	(4),	CFG_PRICE = (CFG_PRICE*PP4MUL)#1		;.XXX
	ENDUSING
	LITMNO(ST+1,15) = 
	RETURN
PP_ERROR,
;;;	XCALL MESAG('PP ERROR',1)
	CONFIG_ERROR = 1
	RETURN
;------------------------------------------
MAIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; does some initialization, then
	;;; prices "main" ...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR CFG_PRICE, ST_KEY
	TTYP = 'FT'			;FITTINGS

	TMAT = IMAT

	TGA = GA			;GAUGE
	
;;;	if (litmno .eq. 'S') return
	if (saddl .eq. 1) return	;ssq 1-15-05

	TCON = 'MN'
	IDX = MA
	CALL GET_PRICE
	RETURN
;-------------------------------------------
REDUCER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; price the reducer ...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TCON = 'RD'
	CALL GET_PRICE
	RETURN
;-------------------------------------------

W_REDUCER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; price the welded reducer ...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TCON = 'RW'
	CALL GET_PRICE
	RETURN
;-------------------------------------------

GET_PRICE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; look up price in table, if price is zero,
		;;; dimension is invalid, EXIT subroutine cfg
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (IDX .LE. 0) RETURN

	XCALL FFILE(1,FILPRC,SWITCH)	;SSQ 9-9-03
	F_NAME = 'XTPRIC'		;SSQ 9-9-03
	F_EXT = 'ISM'
	OPEN(9,SI,FILPRC)		;SSQ 9-9-03
;;;	OPEN(9,SI,'SMC:XTPRIC.ISM')
	SAVKEY = ST_KEY
	XCALL ISIO(CHNSTP, STPRIC, ST_KEY, READ, LOKCTL)
	close 9
	IF (LOKCTL .NE. 0) GOTO GP_RETURN
	CMP_PRICE = ARRAY(IDX)		;COMPONENT PRICE (1/9/03 SSQ)
	IF (CMP_PRICE .LE. 0)		;INVALID IF PRICE IS ZERO
		BEGIN
		CONFIG_ERROR = 1
		CFG_PRICE = 0
		XRETURN			;EXIT FROM SUBROUTINE
		END

	CFG_PRICE = CFG_PRICE + ARRAY(IDX)
GP_RETURN,
	ST_KEY = SAVKEY 	;RESTORE ORIG KEY
	RETURN
;----------------------------------------

PARSE_SEGS,	;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ALPDC(SEGS, D12, BF)

	using fake_ga select
	(16,18,20,22,24,26),	ga = fake_ga		;12-28-09
	endusing

	IF (GA.LT.16 .OR. GA.GT.26) BF = 1		;ERROR
	ST_GUAGE = GA					;SAVE THE GA
	IF (IMAT .EQ. 'P') ST_GUAGE =
	RETURN
;----------------------------------------

MAKE_DESC,	;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TDESC
	ST = 1
	FOR I FROM 1 THRU MAX_SEG
		BEGIN
		IF(SARA(I) .NE. 0)
			BEGIN
		;;;	if (litmno.eq.'S')
			if (saddl .eq. 1)		;ssq 11-15-05
			then	begin			;saddles
				TDESC(ST,ST+1) = SARA(I), 'XX' 
				ST = %TRIM(TDESC) + 1
				if (i .eq. 1)
					begin	
					TDESC(ST,ST+3) = ' ON '
					ST = ST + 3
					end
				st = st + 1
				end
			else	begin
				TDESC(ST,ST+1) = SARA(I), 'ZX' [LEFT]
				ST = %TRIM(TDESC) + 1
				TDESC(ST,ST) = '/'
				INCR ST
				end
			END
		END
	ST = ST -1
	TDESC(ST,ST) = ' '
	INCR ST
	USING DG SELECT
	(9),	TDESC(ST,ST+1) = '90'
	(4),	TDESC(ST,ST+1) = '45'
	ENDUSING

	ST = %TRIM(TDESC) + 2

;;;	if (fake_ga .ne. 0)
;;;	then	TDESC(ST,ST+1) = fake_ga, 'XX'
;;;	else	TDESC(ST,ST+1) = GA, 'XX'

	TDESC(ST,ST+1) = GA, 'XX'

	TDESC(ST+2,ST+3) = 'GA'
	TDESC(ST+5,30) = ITYPE
	ST = %TRIM(TDESC) + 2
	if (st .gt. 30) st = 30		;can't exceed 30
	USING MAT_SEL SELECT
	(ALUM),	TDESC(ST,30) = 'ALUM'
	(SST),	TDESC(ST,30) = 'SST'
	(PGRP),	TDESC(ST,30) = 'PGRP'
	(VIN),	TDESC(28,30) = 'PVS'	;ssq 7-12-17
	ENDUSING

;;;	IF (IMAT .EQ. 'P') TDESC(ST,30) = 'PVS'
	IF (IMAT .EQ. 'P') TDESC(28,30) = 'PVS'	;for now

	RETURN
;----------------------------------------

ROUND_NICKLE,

	return	;3-2-11 per bill

	N_PRICE = CFG_PRICE#1	;.XXX -> .XX
	N_NUM = N_PRICE/10*10
	N_DIG = N_PRICE - N_NUM		;ENDING DIGIT .XX

	USING N_DIG SELECT
	(0),	RETURN		;ALREADY ENDS IN ZERO
	(.LE. 5),	N_PRICE = N_NUM + 5	;.05
	(.GT. 5),	N_PRICE = N_NUM + 10	;.10
	ENDUSING
	
	CFG_PRICE = N_PRICE * 10	;.XX -> .XXX

	RETURN
;----------------------------------------

.end
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9-4-03 now alpdc.cp
;;;. subroutine alpdc
;;;	alpha	,a
;;;	dec	,d
;;;	bf	,d
;;;.proc
;;;	onerror not_num
;;;	dec = alpha
;;;	offerror
;;;	bf = 0
;;;	return
;;;not_num,
;;;	offerror
;;;	dec =
;;;	bf = 1
;;;	return
;;;.end
