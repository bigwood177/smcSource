;cfg_w.cp	;1-21-15
;	get weight for configured items.
;	called from scrnx.cp right after "xcall cfg" so
;	no need to test for config errors or change litmno
;

;cfg.dbl
.subroutine cfg_w
	litmno		,a
	cfg_weight	,d
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

RECORD	TWEGHT
	.INCLUDE 'DEF:TWEGHT.DEF'

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
	chnwgt	,d2
	opnok	,d1
	fake_ga	,d2
	DIFF	,D6		;10-8-14
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
	DG	,D2
	BF	,D1
	IMAT	,A1
	LN	,D2
	ST	,D2
	READ	,D1,0
	LOKCTL	,D1
	SWITCH	,D1
.proc

	clear cfg_weight

	call opens
	if (.not. opnok) goto endoff


	KITMNO = LITMNO
	clear fake_ga
	if (st_guage .ne. a2) 
		begin
		xcall alpdc (st_guage,fake_ga,bf)
		if (bf.eq.1) clear fake_ga
		end
	clear st_guage


	USING MAT_SEL SELECT
	(GALV),	IMAT = 'G'
	(ALUM),	IMAT = 'A'
	(PGRP),	IMAT = 'R'
	(SST),	IMAT = 'S'
	ENDUSING

	CLEAR CONFIG_ERROR, SADDL

;read in multipliers in case they are needed...	
	CLEAR TBL_KEY
	TBLCOD = 'CF'
	XCALL ISIO(CHN182, COPTBL, TBL_KEY, READ, LOKCTL)

	USING KITMNO SELECT
	('BN'),				CALL BN		;BULLNOSE TEE
	('C9','C4','CV9','CV4'),	CALL CROSS	;CROSS
	('CT','CC','CTV','CCV'),	CALL TEE	;CONICAL TEE/CROSS
	('T4','T9','TV4','TV9'),	CALL TEE	;TEE
	('ERW'),			CALL W_REDU	;SSQ 4-10-19
	('ER'),				CALL REDU	;ECCENTRIC REDUCER
	('R16' THRU 'R26'),		CALL REDU
	('RV16' THRU 'RV26'),		CALL REDU
	('RW16' THRU 'RW20'),		CALL W_REDU
	('STC','STT'),			CALL SHOE
	('S'),				CALL SADDLE
	('P'),				CALL PP		;PAIR/PANTS
	(),				CONFIG_ERROR = 1;02-08-07 just in case..
	ENDUSING

endoff,
	call close


	XRETURN

;=================================================================
SADDLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SADDL = 1		;11-15-05 SSQ
	ST = 2
	USING LITMNO SELECT
	('SWV'),	BEGIN
			IMAT = 'P'
			SCON = 'SS'
			KITMNO = LITMNO(4,15)
			ST = 4
			END
	('SCV'),	BEGIN
			IMAT = 'P'
			SCON = 'CS'
			KITMNO = LITMNO(4,15)
			ST = 4
			END
	('SC'),		BEGIN
			SCON = 'CS'
			KITMNO = LITMNO(3,15)
			ST = 3
			END
	('ST'),		BEGIN
			SCON = 'ST'
			KITMNO = LITMNO(3,15)
			ST = 3
			END
	('SW'),		BEGIN
			SCON = 'SS'
			KITMNO = LITMNO(3,15)
			ST = 3
			END
	('S4','S9'),		BEGIN
			SCON = 'FT'
			KITMNO = LITMNO(2,15)
			ST = 2
			END
	('SV'),		BEGIN
			IMAT = 'P'
			SCON = 'FT'
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


	CALL MAIN		;MAIN
	cfg_weight =		;PRICING IS BASED ON BRANCH
	
	USING DG SELECT
	(9),	WCON = 'S9'
	(4),	WCON = 'S4'
	(),	GOTO SADDLE_ERROR
	ENDUSING

	IDX = B1
	WTYP = SCON
	CALL GET_PRICE		;per stevem 1-15-03 price is based on main only

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
			KITMNO = LITMNO(4,15)
			ST = 4
			END
	('STT'),	BEGIN
			CLEAR SADDL
			SCON = 'ST'
			KITMNO = LITMNO(4,15)
			ST = 4
			END
	(),		BEGIN
			CONFIG_ERROR = 1
			GOTO SHOE_ERROR
			END
	ENDUSING


; kitmno now has everything prior to degree stripped off...


	SEGS = KITMNO
	CALL PARSE_SEGS
	GOTO (SHOE_ERROR),BF
	MA = SARA(1)
	R1 = SARA(2)
	B1 = SARA(3)
	B2 = SARA(4)


	CALL MAIN		;MAIN

	
	IF (R1 .LT. MA)
		BEGIN
		IDX = MA	;PRICING BASED ON DIM OF MAIN
		CALL REDUCER
		END

	WCON = 'S9'		;ALWAYS 90
	IDX = B1
	WTYP = SCON
	CALL GET_PRICE		

	IDX = B2
	WTYP = SCON
	CALL GET_PRICE		

	RETURN

SHOE_ERROR,

	CONFIG_ERROR = 1
	RETURN
;-------------------------------------------------

BN,
;BN926101012 = 90deg 26ga 12main 12branch(90) 12reducer 12 reducer
;12345678901
	ST = 3
	KITMNO = LITMNO
	IF (KITMNO(3,3) .EQ. 'V')
		BEGIN
		IMAT = 'P'		;PVS
		KITMNO(3,15) = LITMNO(4,15)
		ST = 4
		END



	KITMNO = LITMNO(ST,15)
	XCALL ALPDC(KITMNO(1,1), DG, BF)
	IF (DG.NE.9) BF = 1			;NO 45
	GOTO (BN_ERROR),BF
	SEGS = KITMNO(2,11)

	CALL PARSE_SEGS
	GOTO (BN_ERROR),BF

	R1 = SARA(1)
	R2 = SARA(2)
	MA = SARA(3)
	B1 = MA			;BRANCH (SADDLE) SAME AS MAIN

	CALL MAIN		;MAIN
	
	IDX = MA
	CALL REDUCER
	CALL REDUCER

	USING DG SELECT
	(9),	WCON = 'S9'
	(4),	WCON = 'S4'
	(),	GOTO BN_ERROR
	ENDUSING

	IDX = B1
	CALL GET_PRICE

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

; kitmno now has everything prior to degree stripped off...

	XCALL ALPDC(KITMNO(1,1), DG, BF)
	IF (DG.NE.4 .AND. DG.NE.9) BF = 1
	GOTO (CT_ERROR),BF


	SEGS = KITMNO(2,11)
tee_logic,
	CALL PARSE_SEGS
	GOTO (CT_ERROR),BF
	MA = SARA(1)
	R1 = SARA(2)
	B1 = SARA(3)
	B2 = SARA(4)

	

	CALL MAIN		;MAIN
	
	IDX = MA
;;;	CALL REDUCER
	IF (MA .GT. R1) CALL REDUCER

	
	WCON = 'S4'		;FOR BOTH 90deg and 45deg
	IF (LITMNO(1,1) .EQ. 'T')
		BEGIN
		IF(DG .EQ. 9) WCON = 'S9'
		END

	IDX = B1
	CALL GET_PRICE
	IDX = B2
	CALL GET_PRICE

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
			ST = 3
			END
	('R'),		BEGIN
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
		end

;------------------------------------------------
; ssq 5/12/05: don't call "main", not using price, invalid if no price in 
;		"main" matrix".
;;;	CALL MAIN		;MAIN

; ssq 5/12/05: but still need to setup key...
	CLEAR cfg_weight, WT_KEY
	WTYP = 'FT'			;FITTINGS

	WMAT = IMAT
	WGA = GA			;GAUGE
	IDX = MA
;------------------------------------------------
	cfg_weight =		;price is just reducer price
	IDX = MA
	CALL REDUCER

;;;	USING LITMNO SELECT
;;;	('E'),	CFG_PRICE = (CFG_PRICE*ECRMUL)#1		;.XXX
;;;	(),	CFG_PRICE = CFG_PRICE * 10			;.XXX
;;;	ENDUSING

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
			ST = 4
			END
	('R'),		BEGIN
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


;------------------------------------------------
; ssq 5/12/05: don't call "main", not using price, invalid if no price in 
;		"main" matrix".
;;;	CALL MAIN		;MAIN

; ssq 5/12/05: but still need to setup key...
	CLEAR cfg_weight, WT_KEY
	WTYP = 'FT'			;FITTINGS

	WMAT = IMAT
	WGA = GA			;GAUGE
	IDX = MA
;------------------------------------------------
	cfg_weight =		;price is just reducer price
	IDX = MA
	CALL W_REDUCER

;;;	USING LITMNO SELECT
;;;	('E'),	CFG_PRICE = (CFG_PRICE*ECRMUL)#1		;.XXX
;;;	(),	CFG_PRICE = CFG_PRICE * 10			;.XXX
;;;	ENDUSING

	RETURN

ERW_ERROR,
	CONFIG_ERROR = 1
	RETURN
;-------------------------------------------------

CROSS,
;C92612100808 = 20ga 12main 12 reducer(12->10) 8branch(90) 8 branch(90)
;123456789012

	ST = 2
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


	CALL MAIN		;MAIN

	IF (R1 .LT. MA)
		BEGIN
		IDX = MA	;USE LARGER DIM. FOR REDUCER (10->12 use 12)
		CALL REDUCER
		END

	USING DG SELECT
	(9),	WCON = 'S9'
	(4),	WCON = 'S4'
	(),	GOTO CROSS_ERROR
	ENDUSING

	IDX = B1
	CALL GET_PRICE		;BRANCH 1

	IDX = B2		;BRANCH 2
	CALL GET_PRICE

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


	CALL MAIN			;PRICE THE MAIN

	WCON = 'S4'
	IDX = B1
	CALL GET_PRICE

	WCON = 'S4'
	IDX = B2
	CALL GET_PRICE

;;;	USING DG SELECT
;;;	(9),	CFG_PRICE = (CFG_PRICE*PP9MUL)#1		;.XXX
;;;	(4),	CFG_PRICE = (CFG_PRICE*PP4MUL)#1		;.XXX
;;;	ENDUSING

	RETURN
PP_ERROR,
	CONFIG_ERROR = 1
	RETURN
;------------------------------------------
MAIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; does some initialization, then
	;;; prices "main" ...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR cfg_weight, WT_KEY
	WTYP = 'FT'			;FITTINGS

	WMAT = IMAT

	WGA = GA			;GAUGE
	
	if (saddl .eq. 1) return	;ssq 1-15-05

	WCON = 'MN'
	IDX = MA
	CALL GET_PRICE
	RETURN
;-------------------------------------------
REDUCER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; price the reducer ...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WCON = 'RD'
	CALL GET_PRICE
	RETURN
;-------------------------------------------

W_REDUCER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; price the welded reducer ...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WCON = 'RW'
	CALL GET_PRICE
	RETURN
;-------------------------------------------

GET_PRICE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; look up price in table, if price is zero,
		;;; dimension is invalid, EXIT subroutine cfg
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (IDX .LE. 0) RETURN

	SAVKEY = WT_KEY
	XCALL ISIO(chnwgt, TWEGHT, WT_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO GP_RETURN

	cfg_weight = cfg_weight + W_ARRAY(IDX)

GP_RETURN,
	WT_KEY = SAVKEY 	;RESTORE ORIG KEY
	RETURN
;----------------------------------------

PARSE_SEGS,	;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ALPDC(SEGS, D12, BF)

	using fake_ga select
	(16,18,20,22,24,26),	ga = fake_ga		;12-28-09
	endusing

	IF (GA.LT.16 .OR. GA.GT.26) BF = 1		;ERROR
	ST_GUAGE = GA					;5-15-17 GOTS TO HAVE THIS - SCRNX
	IF (IMAT .EQ. 'P') ST_GUAGE =			;5-15-17

	RETURN
;----------------------------------------


opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL FFILE(1,FILPRC,SWITCH)	;SSQ 9-9-03
;;;	F_NAME = 'XTPRIC'		;SSQ 9-9-03
	F_NAME = 'XTWGHT'		;SSQ 6-13-16

	F_EXT = 'ISM'
	OPEN(9,SI,FILPRC)		;SSQ 9-9-03

	chnwgt = 9
	opnok = 1
	return
;----------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (chnwgt) close chnwgt
	return
;----------------------------------------

.end
			

