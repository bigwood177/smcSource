;cfg.dbl
.subroutine cfg2
	litmno		,a
	rsegs		,a

; 3-02-11 ssq: heto's
; 9-06-07 ssq: added welded reducers (rw)
; 10-18-17 ssq: add shoe tap tees, STC
;
;	configurator for new items
;


RECORD
	SEGS	,A12			;first 2 dig are gauge
RECORD,X
	GA	,D2
	SARA	,5D2

RECORD	VARS
	SCON	,A2			;"TCON" FOR SADDLES
	D12	,D12
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
	DG	,D2
	BF	,D1
	ITYPE	,A30
	LN	,D2
	ST	,D2
	READ	,D1,0
	LOKCTL	,D1

.proc

	KITMNO = LITMNO

	CLEAR SEGS

;;;	USING KITMNO(1,2) SELECT
	USING KITMNO SELECT				;4-11-18 (SOME ARE 1ST 3 CHAR)
	('BN'),				CALL BN		;BULLNOSE TEE
	('C9','C4','CV9','CV4'),	CALL CROSS	;CROSS
	('CT','CC','CTV','CCV'),	CALL TEE	;CONICAL TEE/CROSS
	('T4','T9','TV4','TV9'),	CALL TEE	;TEE
	('STT','STC'),			CALL SHOE	;4-11-18
	('ER','ERV'),			CALL REDU	;ECCENTRIC REDUCER
	('ERW16' THRU 'ERW20'),		CALL REDU
	('PD'),				CALL ELBOW	;10-07-03
	('E0' THRU 'E9'),		CALL ELBOW
	('ED'),				CALL DIFORM	;10-26-04
;;;	('ED','EG'),			CALL ELBOW	;8-26
	('EG'),				CALL ELBOW	;10-07-03
	('A0' THRU 'A9'),		CALL ELBOW
	('EV','EA','ES'),		CALL ELBOW
	('AV','AA','AS'),		CALL ELBOW
	('GA','GE'),			CALL ELBOW
	('H'),				CALL HETO
	('R08' THRU 'R26'),		CALL REDU	;3-24-09 inclued pressed reducers
;;;	('R16' THRU 'R26'),		CALL REDU
	('RV16' THRU 'RV26'),		CALL REDU
	('RW16' THRU 'RW20'),		CALL REDU
;;;	('SG'),				CALL PIPE
	('SG','SX'),			CALL PIPE	;SSQ 9-30-03
	('SRK'),			CALL SR		;S-T-R 9-28-11
	('SR0' thru 'SR9'),		CALL SR		;S-T-R 9-28-11
	('S'),				CALL SADDLE
	('P4','P9'),			CALL PP		;PAIR/PANTS
	('PV4'),			CALL PP		;SSQ 10-07-03
	ENDUSING

	rsegs = segs
	XRETURN
;=================================================================
HETO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; HETO'S
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; see routine CATG

	return

;H08 = 08" heto
;HD08 = 08" heto w/ damper
;HE08 = 08" heto
;HO08 = 08" heto
;12345678

	CLEAR SEGS

	KITMNO = LITMNO

	USING KITMNO(1,2) SELECT
	('HD'),	XCALL ALPDC (KITMNO(3,4), SARA(1), BF)
	('HE'),	XCALL ALPDC (KITMNO(3,4), SARA(1), BF)
	('HO'),	XCALL ALPDC (KITMNO(3,4), SARA(1), BF)
	(),	XCALL ALPDC (KITMNO(2,3), SARA(1), BF)
	ENDUSING

	IF (BF) CLEAR SEGS

	RETURN
;-------------------------------------------------

ELBOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; ELBOWS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GG329022 = 32" 22ga 90deg elbow
;E329022  = 32" 22ga 90deg elbow
;12345678

	KITMNO = LITMNO(2,15)		;SKIP 1ST CHAR
	IF (KITMNO(1,1) .GT. '9') KITMNO = LITMNO(3,15) ;SKIP NEXT CHAR


;;;	SEGS = KITMNO
	SEGS = KITMNO(1,6)	;ssq 10-07-03
	CALL PARSE_SEGS
	CLEAR SEGS
	if (bf) return			;not numeric
	GA = KITMNO(5,6)
	SARA(1) = KITMNO(1,2)
	SARA(2) = SARA(1)		;MAY NEED TO COUNT TWICE
	RETURN
;-------------------------------------------------

DIFORM,		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; DIFORM
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ED0345 = 03" 45deg DIFORM
;12345678

	KITMNO = LITMNO(3,15)		;SKIP 1ST 2 CHAR
	if (litmno .eq. 'EDA') kitmno = litmno(4,15)
	if (litmno .eq. 'EDS') kitmno = litmno(4,15)
	SEGS = KITMNO
	CALL PARSE_SEGS
	CLEAR SEGS
	SARA(1) = KITMNO(1,2)
	SARA(2) = SARA(1)		;MAY NEED TO COUNT TWICE
	RETURN
;-------------------------------------------------
PIPE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PIPE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SG3222 = 32" 22ga spiral pipe
;12345678

	KITMNO = LITMNO(3,15)		;SKIP 1ST CHAR

	SEGS = KITMNO
	CALL PARSE_SEGS
	CLEAR SEGS
	GA = KITMNO(3,4)
	SARA(1) = KITMNO(1,2)
	SARA(2) = SARA(1)		;MAY NEED TO COUNT TWICE
	RETURN
;-------------------------------------------------
SR,		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; square to round
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SR10-22-15
;12345678

	CLEAR SEGS
	LN = %trim(LITMNO)
	IF (LN .LT. 4) RETURN
	SARA(1) = LITMNO(LN-1,LN)	
	SARA(2) = SARA(1)		;MAY NEED TO COUNT TWICE
	SARA(1) = 
	RETURN
;-------------------------------------------------
SADDLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ST = 2
	USING LITMNO SELECT
	('SWV'),	ST = 4
	('SCV'),	ST = 4
	('SC'),		ST = 3
	('ST'),		ST = 3
	('SW'),		ST = 3
	('S4','S9'),	ST = 2
	('SV'),		ST = 3
	ENDUSING

	if (%trim(litmno) .le. 6)		;10-28-09
	then	kitmno = '26' + litmno(st,15)	;pressed saddles...
	else	kitmno = litmno(st,15)

;;;	KITMNO = LITMNO(ST,15)

; kitmno now has everything prior to degree stripped off...
	SEGS = KITMNO(2,7)

	CALL PARSE_SEGS
	if (bf) 
		begin
		clear segs
		return			;not numeric
		end

	MA = SARA(2)
	B1 = SARA(1)

;;;	SARA(1) = MA	;SSQ 10-07-03
	SARA(2) = B1	;ssq 9-9-03 just 1st dim
	SARA(1) = 	;SSQ 10-07-03
	RETURN
;-------------------------------------------------

BN,
;BN926101012 = 90deg 26ga 12main 12branch(90) 12reducer 12 reducer
;12345678901
	ST = 3
	IF (KITMNO(3,3) .EQ. 'V')	ST = 4

	KITMNO = LITMNO(ST,15)
	SEGS = KITMNO(2,11)

	CALL PARSE_SEGS

	R1 = SARA(1)
	R2 = SARA(2)
	MA = SARA(3)
	B1 = MA			;BRANCH (SADDLE) SAME AS MAIN

	SARA(1) = MA
	SARA(2) = R1
	SARA(3) = R2
	SARA(4) = B1

	RETURN
;-------------------------------------------------
SHOE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; SHOE TAP TEES AND CROSSES
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STT26121008 = 90deg 26ga 12main 10reducer 8branch,
;12345678901

	ST = 4		;FOR BOTH TEES & CROSSES
	KITMNO = LITMNO(ST,15)

	SEGS = KITMNO

	CALL PARSE_SEGS
	MA = SARA(1)
	R1 = SARA(2)
	B1 = SARA(3)
	B2 = SARA(4)

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
	('T'),	ST = 2
	('STT'),ST = 4
	ENDUSING
	KITMNO = LITMNO(ST,15)

	IF (KITMNO(1,1) .EQ. 'V')
		BEGIN
		ST = ST + 1
		KITMNO = LITMNO(ST,15)
		END

; kitmno now has everything prior to degree stripped off...

	SEGS = KITMNO(2,11)

	CALL PARSE_SEGS
	MA = SARA(1)
	R1 = SARA(2)
	B1 = SARA(3)
	B2 = SARA(4)

	RETURN
;-------------------------------------------------

REDU,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; REDUCERS AND ECCENTRIC REDUCERS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ERW201210 = 20ga 12main 10reducer - welded
;ER261210 = 26ga 12main 10reducer
;12345678

;;;	USING LITMNO SELECT
;;;	('E'),		ST = 3
;;;	('R'),		ST = 2
;;;	ENDUSING

	USING LITMNO SELECT
	('ERW'),	ST = 4
	('ER'),		ST = 3
	('RW'),		ST = 3
	('R'),		ST = 2
	ENDUSING


	KITMNO = LITMNO(ST,15)

	IF (KITMNO(1,1) .EQ. 'V')
		BEGIN
		ST = ST + 1
		KITMNO = LITMNO(ST,15)
		END

	SEGS = KITMNO
	CALL PARSE_SEGS
	MA = SARA(1)
	R1 = SARA(2)
	DG =		;1/15/03 SSQ
	ln = %trim (litmno)

; 12-15-08 Pressed reducers
	if (ln .eq. 5)	;pressed part  R1612
		begin
		ma = ga		;there is no ga
		r1 = sara(1)
		sara(1) = ma
		sara(2) = r1
		ga = 24
		end

	RETURN
;-------------------------------------------------

CROSS,
;C92612100808 = 20ga 12main 12 reducer(12->10) 8branch(90) 8 branch(90)
;123456789012

	ST = 3
	IF (LITMNO .EQ. 'STC') ST = 4
	IF (KITMNO(2,2) .EQ. 'V')ST = 4
	KITMNO = LITMNO(ST,15)

	SEGS = KITMNO
	CALL PARSE_SEGS
	MA = SARA(1)
	R1 = SARA(2)
	B1 = SARA(3)
	B2 = SARA(4)

	RETURN
;--------------------------------------------------------

PP,
;Pair/Pants (90 & 45 DEG)
;P920140808 = 20ga 14main 8branch(45) 8branch(45)
;1234567890

	ST = 2
	IF (KITMNO(2,2) .EQ. 'V')ST = 3

	KITMNO = LITMNO(ST,15)

	SEGS = KITMNO(2,10)	;10-30-03 ssq
	CALL PARSE_SEGS
	MA = SARA(1)
	B1 = SARA(2)
	B2 = SARA(3)

	RETURN
;------------------------------------------

PARSE_SEGS,	;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ALPDC(SEGS, D12, BF)
	RETURN
;----------------------------------------


.end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 9-4-03 ssq now alpdc.cp
			
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

