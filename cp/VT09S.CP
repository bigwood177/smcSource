; 9-08-10: there could be 2 reducers: bc-ma, bc-mb, i am assuming that both
; reducers are the same size, eg: BN 9 24 08 08 10
;			R1 = 10 08
;			R2 = 10 08
; but, the reducers might be different sizes: eg: BN 9 24 06 08 10
;			R1 = 10 08
;			R2 = 10 06
;
;vt09s.cp	;bullnose tee
SUBROUTINE VT09S
	PNUM	,D
	LINE	,A	;ORDER L/I
	CHN182	,D	;CHANNEL FOR COPTBL
	MEMDAT	,A	;CPMEMO
	ERR	,D	;1 => ERROR DURING CONVERSION

; Routine for Shoe Taps
;
; BN 9 26 09 09 12
; AAA  GA MA MB BC 
;      |  |  |  |  
;      |  |  |  |____ Branch C
;      |  |  |_______ Main B
;      |  |__________ Main A
;      |_____________ Gauge
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
	d_br1	,d1	;1 = damper on branch 1	(3-20-17)
	d_br2	,d1	;1 = damper on branch 2	(3-20-17)
	org_item	,a15		;original value of ordlin.lcfgim

common	pressed_reducer
	p_file_data	,d1	;1=vt08s has written data to 'spl:"

EXTERNAL FUNCTION
	SSKEY	,D

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
	WR	,D6
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

record
	keyseg		,a5
record,x
	f3ks		,5d1

RECORD	VARS
	btr		,a1	;B/T/R - to control seams
	bcont		,a15	;body cont
	all_ss		,d1
	is_r		,d1
	brt		,a1	;B/R/T for get_seam
	key17		,a17
	a15		,a15
	d5		,d5
	T_KEY		,A4
	T_GA		,D1	;1=22 thru 26, 2=16 thru 20
	T_DIM		,D2	;dimension to use for table look-up
	SS_BLS		,A15	;BODY
	SS_BGS		,A15
	SS_BSS		,A15
	SS_BCONB	,A15
	SS_BCONT	,A15

	SS_TLS		,2A15	;TAP
	SS_TGS		,2A15
	SS_TSS		,2A15
	SS_TCONB	,2A15
	SS_TCONT	,2A15

	SS_RLS		,A15	;REDUCER
	SS_RGS		,A15
	SS_RSS		,A15
	SS_RCONB	,A15
	SS_RCONT	,2A15
	GS		,A15
	LS		,A15
	SS		,A15
	SS_W		,D6	;ZZX.XXX (WB, W, WL)
	SS_L		,D6	;ZZX.XXX (WT, L, DL)
	SS_WL		,2D6	;ZZX.XXX (WB, W, WL)
	SS_DL		,2D6	;ZZX.XXX (WT, L, DL)
	SS_WR		,2D6	;ZZX.XXX (WR)
	SS_SR		,2D6
	SS_LT		,2D6	;ZZX.XXX (LT)
	SS_OW		,2D6
	SS_OD		,2D6
	SS_AT		,2D6
	SS_AR		,2D6
	SS_LB		,2D6	;ZZX.XXX (LB)
	SS_WB		,D6	;ZZX.XXX (WB, W, WL)
	SS_SB		,D6
	SS_WT		,2D6	;ZZX.XXX (WB, W, WL)
	SS_ST		,2D6
	SS_RL		,D6	;L for reducer
;-
	BL	,D6		;Body length	9-8-10
	sav_pp	,d1
	ladd	,d4
	RDQTY	,D6
	S2_CN1	,A15
	S2_CN2	,A15
	S2_CN3	,A15
	S2_CN4	,A15

	vxkey	,d4	;working vdkey	4/2/09

	spiral	,d1,	4	;f3 key for spiral ends
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

	RDQTY = 1

	ORDLIN = LINE

	CPMEMO = MEMDAT

	CLEAR IS_PRESSED, IS_CROSS, IS_TEE, sav_pp

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

	VMAT = 'GALV'
	IF (VINYL) VMAT = 'PVS'
	
	CODE = 'BN9'
	BODY = WRKITM(4,11)

	CALL MMB	;TEE

	d5 = lf3
	keyseg = d5,	'ZZZZX' [left]
	if (d5 .eq. 1) keyseg = '11111'

;;;	if (ma .gt. mb)
	if (bc .gt. mb)
	then	IS_R = 1			;is reducer
	else	IS_R = 0			;not a reducer

	call check_ss			;safety seal values...

	if (ga .eq. 26) ga = 24		;6-19-08

	matl = 1		
	CLEAR COPTBL
	TBLCOD = 'M1'
	MM_KEY = LF1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO SKIP_F1_MATL
	IF (MM_VULC .EQ. 2) XRETURN	;6-24-08
	MATL = MM_MATL
	VMAT = MM_VMAT
	if (lf1 .eq. 68) matl = 6	;G90
	sav_pp = m1_pp

SKIP_F1_MATL,

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear reducer		;7-13-09
	is_pressed = 0


	clear p_list

; end treatment...
	CLEAR COPTBL
	TBLCOD = 'M3'
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

;
; Create the output file...
;
	p_f3 = mm_long

	CLEAR VLINE
	WRITES (chnwrk, VLINE)

	vline = '// key: ' + t_key
	writes (chnwrk, vline)
	clear vline

	VLINE = '// ' + 'vt09s: ' + LCFGIM
	if (dampr) vline = vline(1,%trim(vline)) + ' damper'
	WRITES (chnwrk, VLINE)

	VLINE = '// BULLNOSE TEE'
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

	V_KEY = 9
	L = BC + 6
	BL = L * 1000		;9-8-10

	if (ss_l .gt. 0) bl = ss_l
	l_line = BL ,'L=ZX.XXX;' [left]

;;;	if (ss_l .gt. 0) bl = ss_l
;;;	then	l_line = ss_l ,'L=ZX.XXX;' [left]
;;;	else	L_LINE = L,	'L= ZZ;' [left]		;
;;;	L_LINE = L,	'L= ZZ;' [left]		;
	AT = 90

	vxkey = vdkey
	btr = 'B'
	CALL COM_SEC

	CLEAR TBLKEY
	V1_KEY = V_KEY
	call get_v1_table

	W = ((BC * 1000) + WD4(1))

	if (ss_w .ne. 0) w = ss_w
	VLINE = W, 'W= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, L_LINE)
	

	VLINE = 'PRCT='
	VLINE(7,11) = VMAT	
	LN = %TRIM(VLINE) + 1
	VLINE(LN,LN) = ';'

	writes (chnwrk, vline)

	WRITES (chnwrk, 'FLOOR= "";')

;-
; safety seal conns:
	if (ss_bconb .ne. a15) s2_cn1 = ss_bconb
	if (ss_bcont .ne. a15) s2_cn2 = ss_bcont


;;;	using ga select
;;;	(16 thru 20),	if (is_r .and. .not.vinyl) S2_CN2 = 'BUTT WELD'
;;;	(22 thru 26),	if (is_r .and. .not.vinyl) S2_CN2 = 'ELBOWLOCK'
;;;	endusing
;-

; if f3 note is e-z flange and following conditions are true, change to jr...
	clear t_ezjr, is_ezjr
	IF (MA.GT.5 .AND. MA.LT.24) t_ezjr = 1
	IF (MB.GT.5 .AND. MB.LT.24) t_ezjr = 1
	IF (BC.GT.5 .AND. BC.LT.24) t_ezjr = 1

;;;	IF (t_ezjr .and. lf3.eq.15)
	IF (t_ezjr .and. lf3.eq.3)
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
	(16 thru 20),	if (ma.gt.mb) vline(ln,60) = 'BUTT WELD'
	endusing

	bcont = s2_cn2		;9-15-10 save 

	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, 'END;')
	CLEAR VLINE
	WRITES (chnwrk, VLINE)

; Taps...
	

	WR = ((BC * 1000) + WD4(3))	;1st Tap...

;;;;	AR(1) = 0	;9-18-07
;;;;	AR(2) = 180	;9-18-07

;--------------------------------------------------------------------		
;;;; 2-13-17 using same logic as side of tap for tee bodies (vt08s.cp)

	using ga select	;12-08-14
	(22 thru 26),	using BC select
			(.le. 14),	ar(1) = 180
			(),		ar(1) = 0
			endusing
	endusing

;;;	AR(1) = 180	;12-10-14 unless BC le 3, then 0

;----------------------------------------------------------------------

	if (matl .eq. 2) ar(1) = 0	;4-1-15 - not for alum
	if (bc .le. 3) ar(1) = 0
	J = 1			;ONLY 1 TAP
	CALL C9

	IF (MA .EQ. MB) RDQTY = 2	;Both reducers are the same..

	IF (BC .GT. MA)
		BEGIN
		CLEAR IS_PRESSED

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; 6-28-10 burn pressed reducers...
	
	;;;	IF (sav_pp .le. 1)	;GALV
	;;;	THEN	BEGIN
	;;;		REDUCER = 'R' + AGA + ABC + AMA
	;;;		CALL PRESSED_REDUCER
	;;;		END	
	;;;	ELSE	CLEAR REDUCER
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		SZB = BC
		SZT = MA
		I = 1
		CALL CONR
		if (rdqty .eq. 2) goto done_red	;no need to process 2nd redu..
		END

	IF (BC .GT. MB)
		BEGIN
		CLEAR IS_PRESSED

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; 6-28-10 burn pressed reducers...
	
	;;;	IF (sav_pp.le.1)
	;;;	THEN	BEGIN
	;;;		REDUCER = 'R' + AGA + ABC + AMB
	;;;		CALL PRESSED_REDUCER
	;;;		END	
	;;;	ELSE	CLEAR REDUCER
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		SZB = BC
		SZT = MB
		I = 2
		CALL CONR
		END
done_red,

	CLEAR VLINE
	WRITES (chnwrk, VLINE)

	clear err

	XRETURN

;========================================
get_v1_table,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TBLCOD = 'V1'
;;;	V1_F3 = LF3
	v1_f3 = %sskey(lf3)

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

	L5 = (BC*1414) + 9000	; value for EZ Flange Tap
	LT = 7000		;.xxx
; F3=1 is spiral end...
	LKEY = LF3, 'XXXXX'
;;;	IF (LF3 .EQ. 1) GOTO IS_SPIRAL
	IF (LF3 .EQ. 4) GOTO IS_SPIRAL

; any note ending in "33" is SP/SP and should use the spiral value
;;;	LN = %TRIM(LKEY)
;;;	IF (LN .LT. 4) RETURN	
;;;	IF (LKEY(LN-1,LN) .NE. '33') RETURN

IS_SPIRAL,
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


C9,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Round Straight Tap
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRITES (chnwrk, '//Round Straight Tap')
	WRITES (chnwrk, 'FN = 51-i;')

	WL = WR 	;9-18-07
	IF (SS_WL .GT. 0) WL = SS_WL

	vxkey = vdkey
	btr = 'T'
	CALL COM_SEC

	IF (SS_WR .GT. 0) WR = SS_WR
	VLINE = WR,	'WR= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	lt = 3000					;default value
	if (dampr .and. v_key.eq.9) lt = wr(j)#3 * 1000	;8-30-07
	
	IF (SS_LT .GT. 0) LT = SS_LT
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

	using ga select	;7-13-09
	(16 thru 20),	vxkey = 3	;welded reducers 4-27-09
	(),		begin
			vxkey = 4
			if (szb .ge. 18) vxkey = 2	;gorelock 7-13-09
			if (szt .lt. 8) vxkey = 4	;elbowlock 7-13-09
			end
	endusing

	btr = 'R'
	CALL COM_SEC

	; code from vt02s.cp


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

	WB = W		;always!  9-8-10
;;;	WB = (SZB * 1000) 			;.XXX
;;;	if (ga.ge.16 .and. ga.le.20) WB = W

	IF (SS_WB .GT. 0) WB = SS_WB
	VLINE = WB,	'WB= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	if (ss_sb .gt. 0) sb = ss_sb
	VLINE = 'SB= 0;'
	WRITES (chnwrk, VLINE)

	WT = ((SZT * 1000) + WD4(2))		;.XXX
	IF (SS_WT(I) .GT. 0) WT = SS_WT(I)
	VLINE = WT,	'WT= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	VLINE = 'ST= 0;'
	WRITES (chnwrk, VLINE)


	using matl select
	(2,4,6),	begin
		vline = 'S1= 3.437;'
		writes (chnwrk, vline)
		vline = 'S2= 3.437;'
		writes (chnwrk, vline)

		ladd = 3265
		end

	(),	ladd = 0
	endusing


;Total Reduction/Body Length:
	DIF = (WB - WT)#3
	USING GA SELECT
	(18 thru 26),	USING DIF SELECT
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

	ENDUSING

	L = L*1000 + LADD		;12-16-09

	VLINE = L,	'L= ZX.XXX;' [left]
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
	VLINE(LN,60) = bcont	;reducer.bottom = body.top -always!
;;;	VLINE(LN,60) = S2_CN2	;reducer.bottom = body.top -always!
;;;	VLINE(LN,60) = 'LARGE END'
	if (ga.ge.16 .and. ga.le.20) vline(ln,60) = 'BUTT WELD'
	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)


	if (t_ezjr .and. S2_CN2.EQ.'EZ FLANGE') 
		begin
		S2_CN2 = 'JR EZ FLANGE'
		is_ezjr = 1
		end

	if (ss_rcont(i) .ne. a15) s2_cn2 = ss_rcont(i)
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


FINISH_CROSS,	;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------
	IF (AN.EQ.'4' .AND. is_EZJR)
	THEN	OW = 6000
	ELSE	BEGIN
		LL = L * 1000
		if (an .eq. '4')
		then	ow = ((L5*1000 - (wr*1414))/2)#3
		else	OW = ((BL-WR)*10/2)#1
	;;;	else	OW = ((LL-WL)*10/2)#1
		END

	VLINE = OW,	'OW= ZX.XXX;' [left]
	WRITES (chnwrk, VLINE)

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
	VLINE (6,11) = (rdqty*LQTYOR),	'ZZZZZZ;' [LEFT]
	WRITES (chnwrk, VLINE)

	VLINE = 'ADINFO='
	if (dampr)
	then	VLINE (9,47) = LDESCR(1,%trim(ldescr)) + ' W/DAMPER'
	else	VLINE (9,38) = LDESCR

;;;	VLINE (9,38) = LDESCR
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
	V3_KEY = vxkey
	V3_GA = GA

;;;	using matl select	;for now default to galv unless alum...
;;;	(2),	V3_MAT = MATL
;;;	(),	V3_MAT = 1
;;;	endusing

; 01-08-15 ssq: sst & pg = galv

	using matl select	;default to galv unless alum or pvs ...
	(2,5),	V3_MAT = MATL
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

	using btr select
	('T'),	if (bc .le. 3)
		begin
		LSEAM = 'LSEAM= .5 LAP'
		SSEAM = 'SSEAM= .5 LAP'
		end
	('R'),	if (ma.le.3 .or. mb.le.3)
		begin
		LSEAM = 'LSEAM= .5 LAP'
		SSEAM = 'SSEAM= .5 LAP'
		end
	endusing

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

check_ss,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;f3ks(1) = MA
;f3ks(2) = MB
;f3ks(3) = BC

	all_ss = 1	;assume entire key is ss
	for j from 1 thru 4
		begin
		if (f3ks(j) .le. 0) exitloop
		if (f3ks(j) .ne. 1) 
			begin
			all_ss = 0
			exitloop
			end
		end

	clear t_key

	clear ss_wb, ss_wt, ss_w, ss_l, ss_sb, ss_st, ss_rl

	clear ss_bls, ss_bgs, ss_bss, ss_bcont, ss_bconb
	clear ss_tls, ss_tgs, ss_tss, ss_tcont, ss_tconb

	for j from 1 thru 2
		begin
		clear ss_wt(j)		;9-8-10
		clear ss_rcont(j)
		clear ss_rconb(j)
		clear ss_wl(j)
		clear ss_dl(j)
		clear ss_wr(j)
		clear ss_lt(j)
		clear ss_lb(j)
		clear ss_wr(j)
		clear ss_sr(j)
		clear ss_ow(j)
		clear ss_od(j)
		clear ss_at(j)
		clear ss_ar(j)

		clear ss_bls(j)
		clear ss_bgs(j)
		clear ss_bss(j)
		clear ss_bcont(j)
		clear ss_bconb(j)
		end	


	LN = %TRIM(CODE)
	T_KEY = CODE(1,LN)
;;;	IF (IS_R)	T_KEY = CODE(1,LN) + 'R'

	using ga select
	(22 thru 26),	T_GA = 1
	(16 thru 20),	T_GA = 2
	endusing

; set the key...

	CLEAR KEY17
	CLEAR TBLKEY
	TBLCOD = 'S2'		;safety seal parameters
	S2_KEY = T_KEY
	S2_MAT = 'G'		;for now...
	S2_GA = T_GA

	S2_TYPE = 'B'		;Body
	S2_DIM = BC		;9-9-10 this is the body (and tap) for BN
	S2_R = 1		;seams
	KEY17 = TBL_KEY		;SAVE THE KEY

	if (f3ks(3) .ne. 1) goto skip_body

; Body...
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_BLS = S2_LSEAM
	SS_BGS = S2_GSEAM
	SS_BSS = S2_SSEAM
	SS_BCONB = S2_CONB
	SS_BCONT = S2_CONT

	KEY17 = TBL_KEY
	S2_R = 2		;values
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_W = S2_W	;ZZX.XXX (WB, W, WL)

	TBL_KEY = KEY17
	S2_R = 2		;values
	S2_DIM = BC
	IF (BD .GT. BC) S2_DIM = BD
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_L = S2_L	;ZZX.XXX (WT, L, DL)

skip_body,
; Reducer Bottom - same as body, and never safety seal.
;
; Reducer-1 Top...

	if (f3ks(1) .ne. 1) goto check_red2
	IF (MA .GE. BC) goto check_red2		; 9-08-10

	TBL_KEY = KEY17
	S2_TYPE = 'R'		;REDUCER
	S2_DIM = MA		;TOP
	S2_R = 1		;SEAMS
	KEY17 = TBL_KEY		;SAVE THE KEY
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_RLS = S2_LSEAM
	SS_RGS = S2_GSEAM
	SS_RSS = S2_SSEAM
	SS_RCONT(1) = S2_CONT

	TBL_KEY = KEY17 
	S2_R = 2		;values
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_WT(1) = S2_WT	;ZZX.XXX (WT, L, DL)
	SS_ST(1) = S2_ST

check_red2,
; Reducer-2 top...
;-
	IF (MB .GE. BC) goto skip_red		; 9-08-10

	TBL_KEY = KEY17
	S2_TYPE = 'R'		;REDUCER
	S2_DIM = MB		;TOP
	S2_R = 1		;SEAMS
	KEY17 = TBL_KEY		;SAVE THE KEY
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_RLS = S2_LSEAM
	SS_RGS = S2_GSEAM
	SS_RSS = S2_SSEAM
	SS_RCONT(2) = S2_CONT
	

	TBL_KEY = KEY17 
	S2_R = 2		;values
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_WT(2) = S2_WT	;ZZX.XXX (WT, L, DL)
	SS_ST(2) = S2_ST
;-

skip_red,
	if (f3ks(3) .ne. 1) goto skip_tap1	;tap same as body

	TBL_KEY = KEY17
	S2_TYPE = 'T'		;TAP
	S2_DIM = BC
	S2_R = 1		;SEAMS
	KEY17 = TBL_KEY		;SAVE THE KEY
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_TLS(1) = S2_LSEAM
	SS_TGS(1) = S2_GSEAM
	SS_TSS(1) = S2_SSEAM
	SS_TCONB(1) = S2_CONB
	SS_TCONT(1) = S2_CONT

	KEY17 = TBL_KEY
	S2_R = 2		;values
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_WL(1) = S2_WL	;ZZX.XXX (WB, W, WL)
	SS_DL(1) = S2_DL	;ZZX.XXX (WT, L, DL)
	SS_WR(1) = S2_WR	;ZZX.XXX (WR)
	SS_SR(1) = S2_SR	;ZZX.XXX (WR)
	SS_LT(1) = S2_LT	;ZZX.XXX (LT)
	SS_OW(1) = S2_OW	;ZZX.XXX (LT)
	SS_OD(1) = S2_OD	;ZZX.XXX (LT)
	SS_AT(1) = S2_AT	;ZZX.XXX (LT)
	SS_AR(1) = S2_AR	;ZZX.XXX (LT)
	SS_LB(1) = S2_LB	;ZZX.XXX (LB)

	if (.not. all_ss) clear ss_ow(1)
skip_tap1,

	return
;-------------------------------------------------------
	END


