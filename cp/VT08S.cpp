SUBROUTINE VT08S
	PNUM	,D
	LINE	,A	;ORDER L/I
	CHN182	,D	;CHANNEL FOR COPTBL
	MEMDAT	,A	;CPMEMO
	ERR	,D	;1 => ERROR DURING CONVERSION

; Routine for Shoe Taps
;
; STC 24 14 10 10 10
; AAA GA MA MB BC BD
;     |  |  |  |  |_ Branch D
;     |  |  |  |____ Branch C
;     |  |  |_______ Main B
;     |  |__________ Main A
;     |_____________ Gauge
;
;
common	vulcan
	dampr	,d1
	is_2	,d1	;1 = w2file has data
	is_3	,d1	;1 = w3file has data
	chnw2	,d2	;-2
	chnw3	,d2	;-3
	vdkey	,d4	; VD coptbl key
	m1_note	,a12
	m2_note	,a12
	m3_note	,a12

common	pressed_reducer
	p_file_data	,d1	;1=vt08s has written data to 'spl:"

record	pipeg			;welded pipe gauge table
	.include 'def:pipeg.def'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.def'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CPMEMO
	.INCLUDE 'DEF:CPMEMO.DEF'

RECORD	PARAM
	LT	,D6
	WR	,2D6
	WL	,D6
	DL	,D6
	ST	,D6
	WT	,D6
	WB	,D6
	QTY	,D6
	GAUGE	,D2
	AR	,2D3
	AT	,D2
	SB	,D2
	SR	,D2
	OW	,D10
	OD	,D6
	R	,D2
	A	,D2
	NG	,D2
	L	,D6
	LL	,D6
	W	,D6
	L5	,D6
	GSEAM	,A30
	LSEAM	,A30
	SSEAM	,A30

RECORD	V_LINE1
	VLINE	,A80

RECORD	ARAY
	WD4	,4D4

RECORD	BODY
		,A10		;GA MA MB BC BD
RECORD,X
	AGA	,A2
	AMA	,A2
	AMB	,A2
	ABC	,A2
	ABD	,A2

record	p_list
	p_reducer	,a15
	p_qty		,a6
			,a2
	p_f3		,a30

RECORD	VARS
	ezjr_t	,d1
	ezjr_b	,d1
	S2_CN1	,A15
	S2_CN2	,A15
	S2_CN3	,A15
	S2_CN4	,A15

	vxkey	,d4	;working vdkey	4/2/09

	spiral	,d1,	1	;f3 key for spiral ends
	xga	,d2
	matl	,d1	;f1 matl
	vmat	,a5	;f1 mm_vmat
	t_ezjr	,d1
	is_ezjr	,d1
	chnwrk	,d2
	L_LINE	,A80
	IS_SPIRL	,D1	;1 = SPIRAL END
	LKEY	,A5		;LONG F3 MEMO KEY
	V_KEY	,D2
	IS_PRESSED	,D1	;1= IS A PRESSED REDUCER
	IS_CROSS	,D1
	IS_TEE		,D1
	REDUCER	,A15		;created reducer item #
	I	,D6
	J	,D6
	A1	,A1
	A4	,A4
	WRKITM	,A15
	CNT	,D3
	LF3x	,D5		;THIS IS A FIELD IN RD045A.NEW...
	CODE	,A3
	VINYL	,D1		;1=IS VINYL
	AN	,A1
	GA	,D2
	MA	,D2
	MB	,D2
	BC	,D2
	BD	,D2
	BX	,D2		;WHICHEVER BRANCH
	BF	,D1
	DIF	,D8
	SZ1	,D2
	SZB	,D2
	SZT	,D2
	ERROR	,D1
	LN	,D6
	D4	,D4
	READ	,D1,0
	LOKCTL	,D1
;
PROC

	ERR = 1			;ASSUME ERROR

	PNUM = PNUM + 1			;once for each line

	ORDLIN = LINE

	CPMEMO = MEMDAT

	CLEAR IS_PRESSED, IS_CROSS, IS_TEE

	CLEAR GA, MA, MB, BC, BD, AN, VINYL, t_ezjr


	CLEAR TBLKEY
	TBLCOD = 'V2'		;vulcan descriptions
	V2_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	S2_CN1 = V2_CN1
	S2_CN2 = V2_CN2
	S2_CN3 = V2_CN3
	S2_CN4 = V2_CN4


	WRKITM = LCFGIM
	FOR J FROM 1 THRU 4	;STRIP OFF "V" AND ANGLE (4 or 9)
		BEGIN
		A1 = LCFGIM(J,J)
		USING A1 SELECT
		('V'),		BEGIN
				VINYL = 1
				wrkitm(j,j) = '~'
			;;;	CALL SHIFT
				END
		('4','9'),	BEGIN
				IF (AN.EQ.' ')
					BEGIN
					AN = A1
					wrkitm(j,j) = '~'
				;;;	CALL SHIFT
					END
				END
		ENDUSING

		END
	for j from 1 thru 4
		begin
		i = %instr(1,wrkitm, '~')
		if (i .gt. 1) wrkitm(i,15) = wrkitm(i+1,15)
		end

	VMAT = 'GALV'
	IF (VINYL) VMAT = 'PVS'

	USING WRKITM SELECT
	('ST'),		BEGIN
			CODE = WRKITM(1,3)	;STC,STT
			BODY = WRKITM(4,13)
			END

	('CC','CT'),	BEGIN
			CODE = WRKITM(1,2)	;CC,CT
			BODY = WRKITM(3,12)
			END

	('C','T'),	BEGIN
			CODE = WRKITM(1,1)	;C,T
			BODY = WRKITM(2,11)
			END
	ENDUSING
	
	LN = %TRIM(CODE)
	USING CODE(LN,LN) SELECT
	('C'),	CALL MMBB		;CROSS
	('T'),	CALL MMB		;TEE
	ENDUSING

	if (ga .eq. 26) ga = 24		;6-19-08

;;;	clear matl
	if (vinyl .eq. 1)	;6-11-09
	then	matl = 5
	else	matl = 1		
	CLEAR COPTBL
	TBLCOD = 'M1'
	MM_KEY = LF1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO SKIP_F1_MATL
	IF (MM_VULC .EQ. 2) XRETURN	;6-24-08
	MATL = MM_MATL
	VMAT = MM_VMAT

SKIP_F1_MATL,


	IF (MB .LT. MA)
	THEN	BEGIN
		REDUCER = 'R' + AGA + AMA + AMB
		CALL PRESSED_REDUCER
		END	

	ELSE	CLEAR REDUCER


	CODE = CODE(1,%trim(code) ) + AN


	clear p_list

; end treatment...
	CLEAR COPTBL
	TBLCOD = 'MM'
	MM_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0) 
		begin
		p_f3 = mm_short
		GOTO GOT_TBL
		end


; if no/bad end treatment, check for default keys...
	CLEAR COPTBL
	TBLCOD = 'IK'
	IK_ITEM = LCFGIM
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL
GOT_TBL,
;------------------------------------
; removed ssq: 2-12-09
;;;
;;;	if (is_pressed) 
;;;		begin
;;;		p_reducer = reducer
;;;		p_qty = lqtyor,	'ZZZZZX'
;;;		writes (33,p_list)
;;;		clear p_list
;;;		p_file_data = 1
;;;		end
;------------------------------------

;
; Create the output file...
;
	p_f3 = mm_long

	CLEAR VLINE
	WRITES (chnwrk, VLINE)

	VLINE = '// ' + 'vt08s: ' + LCFGIM
	if (dampr) vline = vline(1,%trim(vline)) + ' damper'
	WRITES (chnwrk, VLINE)

	if (m1_note .ne. '   ')
		begin
		vline = '// ' + 'F1: ' + m1_note
		writes (chnwrk, vline)
		end

	if (m2_note .ne. '   ')
		begin
		vline = '// ' + 'F2: ' + m2_note
		writes (chnwrk, vline)
		end

	if (m3_note .ne. '   ')
		begin
		vline = '// ' + 'F3: ' + m3_note
		writes (chnwrk, vline)
		end

	vline = '// ' + 'pipe gauge: ' + lpipeg
	writes (chnwrk, vline)

	WRITES (chnwrk, '//Straight Body')
	WRITES (chnwrk, 'FN = 1000-i;')


	IF (BD .GT. BC) BC = BD		;BC will be larger of BD & BD

	USING CODE SELECT
	('STC','STT'),			BEGIN
					IF(CODE.EQ.'STT') V_KEY = 10
					IF(CODE.EQ.'STC') V_KEY = 12
					L = BC + 9
					L_LINE = L,	'L= ZZ;' [left]		;
					AT = 90
					END

	('T4','C4'),			BEGIN
					IF (CODE.EQ.'T') V_KEY = 9
					IF (CODE.EQ.'C') V_KEY = 11
				;;;	L = BC + 9
					CALL F3_MEMO
					L_LINE = L5,	'L= ZZ.XXX;' [left]		;
					AT = 45
					END

	('CC9','CT9'),			BEGIN
					IF(CODE.EQ.'CT9') V_KEY = 10
					IF(CODE.EQ.'CC9') V_KEY = 12
					L = BC + 7
					L_LINE = L,	'L= ZZ;' [left]		;
					AT = 90
					END

	('C9','T9'),			BEGIN
					IF (CODE.EQ.'T') V_KEY = 9
					IF (CODE.EQ.'C') V_KEY = 11
					L = BC + 6
					L_LINE = L,	'L= ZZ;' [left]		;
					AT = 90
					END
	ENDUSING

	vxkey = vdkey
	CALL COM_SEC

	CLEAR TBLKEY
	V1_KEY = V_KEY
	call get_v1_table

	W = ((MA * 1000) + WD4(1))
	VLINE = W, 'W= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, L_LINE)
	

	VLINE = 'PRCT='
	VLINE(7,11) = VMAT	
	LN = %TRIM(VLINE) + 1
	VLINE(LN,LN) = ';'

	writes (chnwrk, vline)

	WRITES (chnwrk, 'FLOOR= "";')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; read once at start of routine...

;;;	CLEAR TBLKEY
;;;	TBLCOD = 'V2'		;vulcan descriptions
;;;	V2_KEY = LF3
;;;	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
;;;	IF (LOKCTL .NE. 0) CLEAR COPTBL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; if f3 note is e-z flange and following conditions are true, change to jr...
	clear t_ezjr, is_ezjr
	IF (MA.GT.5 .AND. MA.LT.24) t_ezjr = 1
	IF (MB.GT.5 .AND. MB.LT.24) t_ezjr = 1
	IF (BC.GT.5 .AND. BC.LT.24) t_ezjr = 1
	IF (BD.GT.5 .AND. BD.LT.24) t_ezjr = 1

	IF (t_ezjr .and. lf3.eq.15)
		begin
		S2_CN1 = 'JR E-Z FLANGE'
		is_ezjr = 1
		end
; if f3 note is e-z flange and following conditions are true, change to jr...

	VLINE = 'CONN= B;CN= "'
	LN = %TRIM (VLINE)
	INCR LN
	VLINE(LN,60) = S2_CN1
	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)


	if (t_ezjr .and. S2_CN2.EQ.'EZ FLANGE') 
		begin
		S2_CN2 = 'JR EZ FLANGE'
		is_ezjr = 1
		end

	VLINE = 'CONN= T;CN= "'
	LN = %TRIM (VLINE)
	INCR LN
	VLINE(LN,60) = S2_CN2
	using ga select
;;;	(16 thru 20),	if (ma.gt.mb) vline(ln,60) = 'BUTT WELD'
	(16 thru 20),	if (ma.gt.mb.and. .not.vinyl) vline(ln,60) = 'BUTT WELD'
	endusing

	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, 'END;')
	CLEAR VLINE
	WRITES (chnwrk, VLINE)

; Taps...
	

	WR(1) = ((BC * 1000) + WD4(3))	;1st Tap...
	WR(2) = ((BD * 1000) + WD4(4))	;2nd Tap...

	AR(1) = 0	;9-18-07
	AR(2) = 180	;9-18-07

	USING CODE SELECT	;round boot taps, conical taps
	('STC'),	BEGIN
			FOR J FROM 1 THRU 2	CALL STC
			IF (MA.GT.MB)		CALL CONR
			END

	('CC4'),	BEGIN
			FOR J FROM 1 THRU 2	CALL CC4
			IF (MA.GT.MB) 		CALL CONR
			END		

	('CC9'),	BEGIN
			FOR J FROM 1 THRU 2	
				BEGIN
				USING J SELECT
				(1),	BX = BC
				(2),	BX = BD
				ENDUSING
				CALL CC9
				END
			IF (MA.GT.MB) 		CALL CONR
			END		

	('STT'),	BEGIN
			J = 1			;ONLY 1 TAP
			CALL STC
			IF (MA.GT.MB) 		CALL CONR
			END

	('C4'),		BEGIN
			FOR J FROM 1 THRU 2	CALL CC4
			IF (MA.GT.MB) 		CALL CONR
			END

	('C9'),		BEGIN	
			FOR J FROM 1 THRU 2	CALL C9
			IF (MA.GT.MB) 		CALL CONR
			END

	('T4'),		BEGIN
			J = 1			;ONLY 1 TAP
			CALL CC4
			IF (MA.GT.MB) 		CALL CONR
			END

	('T9'),		BEGIN	
			J = 1			;ONLY 1 TAP
			CALL C9
			IF (MA.GT.MB) 		CALL CONR
			END
	('CT4'),	BEGIN
			J = 1			;ONLY 1 TAP
			CALL CC4
			IF (MA.GT.MB) 		CALL CONR
			END

	('CT9'),	BEGIN	
			J = 1			;ONLY 1 TAP
			BX = BC
			CALL CC9
			IF (MA.GT.MB) 		CALL CONR
			END
	ENDUSING


	CLEAR VLINE
	WRITES (chnwrk, VLINE)

	clear err

	XRETURN

;========================================
get_v1_table,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TBLCOD = 'V1'
	V1_F3 = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	FOR J FROM 1 THRU 4
		USING GA SELECT
		(16),		WD4(J) = V1_16(J)
		(18 THRU 20),	WD4(J) = V1_1820(J)
		(22 THRU 26),	WD4(J) = V1_2226(J)
		(),		WD4(J) =
		ENDUSING

	if (lf3 .ne. spiral)	return
; override vulcan parameters with pipe gauge data...

	using ga select		;create index to pipeg data from gauge
	(20),	j = 1
	(18),	j = 2
	(16),	j = 3
	(),	return		;outside range of valid pipeg data
	endusing

	using lpipeg select
	('26'),	wd4(1) = -g26(j)
	('24'),	wd4(1) = -g24(j)
	('22'),	wd4(1) = -g22(j)
	('20'),	wd4(1) = -g20(j)
	('18'),	wd4(1) = -g18(j)
	('16'),	wd4(1) = -g16(j)

	('W20'),	wd4(1) = -w20(j)
	('W18'),	wd4(1) = -w18(j)
	('W16'),	wd4(1) = -w16(j)
	endusing
	
	set wd4(2), wd4(3), wd4(4) = wd4(1)

	return
;-----------------------------------------------------------------

F3_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	L = (BC*1414)#3 + 9	; value for EZ Flange Tap
	L5 = (BC*1414) + 9000	; value for EZ Flange Tap
	LT = 7000		;.xxx
; F3=1 is spiral end...
	LKEY = LF3, 'XXXXX'
	IF (LF3 .EQ. 1) GOTO IS_SPIRAL

; any note ending in "33" is SP/SP and should use the spiral value
	LN = %TRIM(LKEY)
	IF (LN .LT. 4) RETURN	
	IF (LKEY(LN-1,LN) .NE. '33') RETURN

IS_SPIRAL,
;;;	L = (BC*1414)#3 + 6	; value for Spiral End Tap
	L5 = (BC*1414) + 6000	; value for Spiral End Tap
	LT = 3000		; .xxx

	RETURN
;----------------------------------------

SHIFT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRKITM(J,15) = WRKITM(J+1,15)
	LN = %TRIM(WRKITM) + 1
	WRKITM(LN,LN) = 	

	RETURN
;----------------------------------------

MMBB,	;CROSS: MAIN-A MAIN-B BRANCH-C BRANCH-D

	XCALL ALPDC (AGA, GA, BF)
	IF (BF) XRETURN

	using ga select
	(16 thru 20),	begin
			chnwrk = chnw2
			is_2 = 1
			end
	(22 thru 26),	begin
			chnwrk = chnw3
			is_3 = 1
			end
	endusing

	XCALL ALPDC (AMA, MA, BF)
	IF (BF) XRETURN

	XCALL ALPDC (AMB, MB, BF)
	IF (BF) XRETURN

	XCALL ALPDC (ABC, BC, BF)
	IF (BF) XRETURN

	XCALL ALPDC (ABD, BD, BF)
	IF (BF) XRETURN

	RETURN
;----------------------------------------

MMB,	;TEE: MAIN-A MAIN-B BRANCH-C 

	XCALL ALPDC (AGA, GA, BF)
	IF (BF) XRETURN

	using ga select
	(16 thru 20),	begin
			chnwrk = chnw2
			is_2 = 1
			end
	(22 thru 26),	begin
			chnwrk = chnw3
			is_3 = 1
			end
	endusing

	XCALL ALPDC (AMA, MA, BF)
	IF (BF) XRETURN

	XCALL ALPDC (AMB, MB, BF)
	IF (BF) XRETURN

	XCALL ALPDC (ABC, BC, BF)
	IF (BF) XRETURN

	RETURN
;----------------------------------------
CC4,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Round Lateral Tap
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRITES (chnwrk, '//Round Lateral Tap')
	WRITES (chnwrk, 'FN = 51-i;')

	vxkey = vdkey
	CALL COM_SEC

	WL = WR(J) 	;9-18-07

	VLINE = WR(J),	'WR= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)
	
	if (dampr .and. v_key.eq.9) lt = wr(j)

	VLINE = LT,	'LT= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)
	
	CALL FINISH_CROSS

	RETURN
;-----------------------------------------------------

C9,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Round Straight Tap
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRITES (chnwrk, '//Round Straight Tap')
	WRITES (chnwrk, 'FN = 51-i;')

	WL = WR(J) 	;9-18-07

	vxkey = vdkey
	CALL COM_SEC

	VLINE = WR(J),	'WR= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	lt = 3000					;default value
	if (dampr .and. v_key.eq.9) lt = wr(j)#3 * 1000	;8-30-07

	VLINE = LT,	'LT= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	CALL FINISH_CROSS


	RETURN
;-----------------------------------------------------

CC9,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Round Conical Tap
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRITES (chnwrk, '//Round Conical Tap')
	WRITES (chnwrk, 'FN = 53-i;')

	vxkey = vdkey
	CALL COM_SEC

	WL = WR(J) + 1000

	LT = (BX*10/2)#1
	LT = LT + 1
	LT = LT *1000					;.XXX

	VLINE = WL,	'WL= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	VLINE = WR(J),	'WR= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, 'SR= 0;')

	VLINE = LT,	'LT= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	CALL FINISH_CROSS


	RETURN
;-----------------------------------------------------

CONR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Round Concentric Reducer
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (is_pressed) return		;8-21-07

	WRITES (chnwrk, '//Round Concentric Reducer')
	WRITES (chnwrk, 'FN = 315-i;')

	using ga select	;4-27-09
	(16 thru 20),	vxkey = 3	;welded reducers 4-27-09
	(),		if (mb.lt.16)
			then vxkey = 4	;H dept	6-4-09
			else vxkey = 2	;gorelock 6-4-09
;;;	(),		vxkey = 2	;gorelock reducers 4-2-09
	endusing

	CALL COM_SEC

	SZB = MA
	SZT = MB
	; code from vt02s.cp
	clear ezjr_b, ezjr_t				;2-21-08
	if (lf3.ne.15 .and. lf3.ne.16) goto no_ezjr	;2-21-08

	;if ez flange, change to jr ez flange
	if (szb.le.24)	ezjr_b = 1
	if (szt.le.24)	ezjr_t = 1
no_ezjr,
;-------------------------------

	CLEAR TBLKEY
	TBLCOD = 'V1'

	USING GA SELECT
	(18 THRU 22),	V1_KEY = 2		; F dept
	(24 THRU 26),	V1_KEY = 4		; H dept
	(),		V1_KEY = 4		; default to H dept
	ENDUSING

	call get_v1_table


	SB = 		;always zero for reducers
	SR = 		;always zero for reducers

	WB = (SZB * 1000) 			;.XXX
	if (ga.ge.16 .and. ga.le.20) WB = W
	VLINE = WB,	'WB= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	VLINE = 'SB= 0;'
	WRITES (chnwrk, VLINE)

	WT = ((SZT * 1000) + WD4(2))		;.XXX
	VLINE = WT,	'WT= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	VLINE = 'ST= 0;'
	WRITES (chnwrk, VLINE)

;Total Reduction/Body Length:
	DIF = (WB - WT)#3
	USING GA SELECT
;;;	(18 thru 22),	USING DIF SELECT
	(16 thru 26),	USING DIF SELECT
			(1),		L=3
			(2,3),		L=4
			(4,5,6),	L=DIF+2
			(7,8),		L=DIF+3
			(9,10,11),	L=DIF+4
			(12,13,14),	L=DIF+5
			(15,16),	L=22
			(17,18),	L=25
			(19),		L=26
			(20,21,22),	L=DIF+8
			(),		L=31
			ENDUSING

;;;	(24 thru 26),	USING DIF SELECT
;;;			(1),		L = 2
;;;			(2,3),		L = 3
;;;			(4,5,6),	L=DIF+1
;;;			(7,8),		L=DIF+2
;;;			(9,10,11),	L=DIF+3
;;;			(12,13,14),	L=DIF+4
;;;			(15,16),	L=21
;;;			(17,18),	L=24
;;;			(19),		L=25
;;;			(20,21,22),	L=DIF+7
;;;			(),		L=30
;;;			ENDUSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			

	ENDUSING
		
	L = L * 1000
	IF (EZJR_T) L = L + 625
	IF (EZJR_B) L = L + 625
	IF (CODE .EQ. 'ER') L = (DIF*25)#1

	VLINE = L,	'L= ZX.XXX;' [left]

;;;	VLINE = L,	'L= ZX;' [left]
	WRITES (chnwrk, VLINE)

	DIF = (WB - WT)		;don't round

	OW = (DIF*10/2)#1
	VLINE = OW,		'OW= ZX.XXX;' [left]
	WRITES (chnwrk, VLINE)

	VLINE = OW,		'OD= ZX.XXX;' [left]
	WRITES (chnwrk, VLINE)
	
vline= 'PRCT= CATALOG SETUP Design Build S & D KD - ASMBL Mult Rfcmt Galvanized;'
        ;2345678901234567890123456789012345678901234567890123456789012345678901234567890
        ;        1         2         3         4         5         6         7

	VLINE(62,72) = VMAT
	ln = %trim(vline) + 1
	vline(ln,ln) = ';'
	writes (chnwrk, vline)


	CLEAR TBLKEY
	TBLCOD = 'V2'		;vulcan descriptions
	V2_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	VLINE = 'CONN= B;CN= "'
	LN = %TRIM (VLINE)
	INCR LN
;;;	VLINE(LN,60) = S2_CN1
	VLINE(LN,60) = 'LARGE END'
;;;	if (ga.ge.16 .and. ga.le.20) vline(ln,60) = 'BUTT WELD'
	if (ga.ge.16 .and. ga.le.20 .and. vinyl.eq.0) vline(ln,60) = 'BUTT WELD'
	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)


	if (t_ezjr .and. S2_CN2.EQ.'EZ FLANGE') 
		begin
		S2_CN2 = 'JR EZ FLANGE'
		is_ezjr = 1
		end

	VLINE = 'CONN= T;CN= "'
	LN = %TRIM (VLINE)
	INCR LN
	VLINE(LN,60) = S2_CN2
	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)


	WRITES (chnwrk, 'END;')

	CLEAR VLINE
	WRITES (chnwrk, VLINE)


	RETURN
;-----------------------------------------------------

STC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Round Boot Tap
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRITES (chnwrk, '//Round Boot Tap')
	WRITES (chnwrk, 'FN = 54-i;')

	vxkey = vdkey
	CALL COM_SEC

	DL = WR(J) ;;;>+ 3000	;8-7-07 added 3000
	WL = WR(J) + 3000

	VLINE = WL,	'WL= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	using code select
	('STT','STC','ST9'),	BEGIN
				VLINE = DL,	'DL= ZZ.XXX;' [left]		;.XXX
				WRITES (chnwrk, VLINE)
				END
	endusing

	VLINE = WR(J),	'WR= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, 'SR= 0;')
	WRITES (chnwrk, 'LT= 4;')

	CALL FINISH_CROSS

	RETURN
;-----------------------------------------------------

FINISH_CROSS,	;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------
	IF (AN.EQ.'4' .AND. is_EZJR)
	THEN	OW = 6000
	ELSE	BEGIN
		LL = L * 1000
		if (an .eq. '4')
		then	ow = ((L5*1000 - (wr*1414))/2)#3
	;;;	else	OW = ((LL-WR)*10/2)#1
		else	OW = ((LL-WL)*10/2)#1
		END

	VLINE = OW,	'OW= ZX.XXX;' [left]
	WRITES (chnwrk, VLINE)

;;;	ELSE	WRITES (chnwrk, 'OW= 3;')
;----------------------------------------------

	WRITES (chnwrk, 'OD= 0;')

	VLINE = AT,	'AT= ZX;' [left]
	WRITES (chnwrk, VLINE)

	VLINE = AR(J),	'AR= ZZX;' [left]
	WRITES (chnwrk, VLINE)


	VLINE = 'PRCT='
	VLINE(7,11) = VMAT	
	LN = %TRIM(VLINE) + 1
	VLINE(LN,LN) = ';'

	writes (chnwrk, vline)

	WRITES (chnwrk, 'FLOOR= ;')

	USING GA SELECT
	(16 THRU 20),	VLINE = 'CONN=L;CN= "BUTT WELD"; END;'
	(22 THRU 26),	VLINE = 'CONN=L;CN= "ELBOWLOCK"; END;'
	ENDUSING

	if (vinyl)	VLINE = 'CONN=L;CN= "ELBOWLOCK"; END;'
	WRITES (chnwrk, VLINE)

	if (t_ezjr .and. S2_CN3.EQ.'EZ FLANGE') S2_CN3 = 'JR EZ FLANGE'
	VLINE = 'CONN= R;CN= "'
	LN = %TRIM (VLINE)
	INCR LN
	VLINE(LN,60) = S2_CN3
	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, 'END;')

	CLEAR VLINE
	WRITES (chnwrk, VLINE)

	RETURN
;-----------------------------------------------------

COM_SEC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; COMMON TO EACH SECTION
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	call get_seam		;4-2-09

	VLINE = PNUM,	'PNUM = ZZX;' [LEFT]
	WRITES (chnwrk, VLINE)

	VLINE = 'QTY= '
	VLINE (6,11) = LQTYOR,	'ZZZZZZ;' [LEFT]
	WRITES (chnwrk, VLINE)

	VLINE = 'ADINFO='
	VLINE (9,38) = LDESCR
	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,LN) = ';'
	WRITES (chnwrk, VLINE)


	VLINE = 'METALN= "'
	VLINE (10,13) = VMAT
	LN = %TRIM(VLINE) + 1
	VLINE (LN,80) = ' SPIRAL FITT";'
	WRITES (chnwrk, VLINE)

	XGA = GA
	IF (MATL .NE. 2) GOTO NOT_ALUM	
	USING GA SELECT
	(16),	XGA = 63
	(18),	XGA = 50
	(20),	XGA = 40
	(22),	XGA = 32
	(24),	XGA = 24
	(26),	XGA =
	ENDUSING

NOT_ALUM,

	VLINE = 'GAUGE= '
	VLINE(8,10) = XGA,	'XX;' 
	WRITES (chnwrk, VLINE)

	VLINE = LSEAM
	WRITES (CHNWRK, VLINE)

	VLINE = GSEAM
	WRITES (chnwrk, vline)

	VLINE = SSEAM
	WRITES (chnwrk, vline)
	
	RETURN
;--------------------------------------------------------------

get_seam,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;4-2-09
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR COPTBL
	TBLCOD = 'V3'
;;;	V3_KEY = vdkey
	V3_KEY = vxkey
	V3_GA = GA
	using matl select	;for now default to galv unless alum...
	(2),	V3_MAT = MATL
	(5),	V3_MAT = MATL	;vinyl 6-11-09
	(),	V3_MAT = 1
	endusing

	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0) 
	THEN	BEGIN
		GSEAM = 'GSEAM= ' + '"' + V3_GSEAM(1,%trim(V3_GSEAM)) + '";'
		LSEAM = 'LSEAM= ' + '"' + V3_LSEAM(1,%trim(V3_LSEAM)) + '";'
		SSEAM = 'SSEAM= ' + '"' + V3_SSEAM(1,%trim(V3_SSEAM)) + '";'
		END

	ELSE	CLEAR GSEAM, LSEAM, SSEAM

	return
;-------------------------------------------------------


PRESSED_REDUCER,	;;;;;;;;;;;;;;;;;

	is_pressed = 0
	CLEAR COPTBL
	TBLCOD = 'PP'
	IK_ITEM = REDUCER
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) 
	THEN	CLEAR REDUCER
	ELSE	IS_PRESSED = 1

	RETURN
;----------------------------------------

	END

		
