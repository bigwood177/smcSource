SUBROUTINE VT03S	; Saddles
	PNUM	,D
	LINE	,A	;ORDER L/I
	CHN182	,D	;CHANNEL FOR COPTBL
	MEMDAT	,A	;CPMEMO
	ERR	,D	;1 => ERROR DURING CONVERSION

; Routine for Saddles
; S 4 26 10 12
; A |  |  |  |_ sz-b
;   |  |  |____ sz-t
;   |  |_______ gauge
;   |________ angle
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

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.def'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CPMEMO
	.INCLUDE 'DEF:CPMEMO.DEF'

RECORD	PARAM
	QTY	,D6
	GAUGE	,D2
	LT	,D6
	LB	,D6
	SB	,D2
	WL	,D6
	DL	,D6
	WR	,D6
	R	,D2
	WB	,D6
	WT	,D6
	OW	,D6
	OD	,D6
	PRCT	,A80
	GSEAM	,A30
	LSEAM	,A30
	SSEAM	,A30
	SEG	,A30

RECORD	V_LINE1
	VLINE	,A80

RECORD	ARAY
	WD4	,4D4

record
	keyseg		,a5
record,x
	f3ks		,5d1

RECORD	VARS
	CMPCOD	,A3		;11-10-20
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
	SS_RCONT	,A15
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
	SS_WT		,D6	;ZZX.XXX (WB, W, WL)
	SS_ST		,D6
	SS_RL		,D6	;L for reducer
	xga	,d2
	matl	,d1	;f1 matl
	vmat	,a5	;f1 mm_vmat
	vinyl	,d1
	ezjr	,d1
	diff	,d6
	chnwrk	,d2
	SDL_KEY	,D1	;5=Saddle, 6=Conical Saddle, 7=Shoe Tap
	SDL_TYP	,A2	;
	KITMNO	,A15
	CNT	,D3
	J	,D6
	LF3x	,D5		;THIS IS A FIELD IN RD045A.NEW...
	CODE	,A5
	SZB	,D2
	SZT	,D2
	AN	,D2
	aan	,A1
	GA	,D2
	BF	,D1
	ERROR	,D1
	LN	,D6
	D4	,D4
	READ	,D1,0
	LOKCTL	,D1
;
PROC
	XCALL WHO (CMPCOD)
	ERR = 1			;ASSUME ERROR

	ORDLIN = LINE

	CPMEMO = MEMDAT

	if (%trim(lcfgim) .le. 4) return	;too short  8-21-07

; S 4 26 10 12
; A |  |  |  |_ sz-b
;   |  |  |____ sz-t
;   |  |_______ gauge
;   |________ angle
;


;-
	SDL_TYP = LCFGIM(1,2)

	clear vinyl

	USING LCFGIM SELECT
	('SWV','SCV'),		begin
				KITMNO = LCFGIM(4,15)
				code = lcfgim(1,3)
				vinyl = 1
				end
	('SV'),			BEGIN
				KITMNO = LCFGIM(3,15)
				code = lcfgim(1,2)
				VINYL = 1
				END
	('SC','ST','SW'),	begin
				KITMNO = LCFGIM(3,15)
				code = lcfgim(1,2)
				end
	('S4','S9'),		begin
				KITMNO = LCFGIM(2,15)
				code = lcfgim(1,1)
				end
	(),		BEGIN		;error
			END
	ENDUSING


; kitmno now has everything prior to degree stripped off...

	XCALL ALPDC(KITMNO(1,1), AN, BF)
	IF (BF) XRETURN

	XCALL ALPDC (KITMNO(2,3), GA, BF)
	IF (BF) XRETURN

	aan = an,'X'
	if (an .gt. 0) code = code(1,%trim(code)) + aan

	if (ga .eq. 26) ga = 24		;5-26-10

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

	XCALL ALPDC (KITMNO(4,5), SZT, BF)	;TAP
	IF (BF) XRETURN

	XCALL ALPDC (KITMNO(6,7), SZB, BF)	;BODY
	IF (BF) XRETURN

	if (ga .eq. 26) ga = 24		;6-19-08

	vmat = 'GALV'
	if (vinyl) vmat = 'PVS'

	MATL = 1		;GALV	4-2-09 SSQ

     	CLEAR COPTBL
	TBLCOD = 'M1'
	MM_KEY = LF1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO SKIP_F1_MATL
	IF (MM_VULC .EQ. 2) XRETURN	;6-24-08
	MATL = MM_MATL
	VMAT = MM_VMAT

SKIP_F1_MATL,

;	CLEAR COPTBL
;	TBLCOD = 'V3'
;	V3_KEY = vdkey
;	V3_GA = GA
;	using matl select	;for now default to galv unless alum...
;	(2),	V3_MAT = MATL
;	(),	V3_MAT = 1
;	endusing

	d5 = lf3
	keyseg = d5,	'ZZZZX' [left]
	if (d5 .eq. 1) keyseg = '11111'

	call check_ss

	CLEAR COPTBL
	TBLCOD = 'V3'
	V3_KEY = vdkey
	V3_GA = GA

; 12-17-14 ssq: use actual material
; 01-08-15 ssq: sst & pg = galv

;;;	v3_mat = matl
	using matl select	;for now default to galv unless alum...
	(2,5),	V3_MAT = MATL
	(),	V3_MAT = 1
	endusing


;;;	using matl select	;for now default to galv unless alum...
;;;	(2),	V3_MAT = MATL
;;;	(),	V3_MAT = 1
;;;	endusing

;-
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0) 
	THEN	BEGIN
		GS = V3_GSEAM
		LS = V3_LSEAM
		SS = V3_SSEAM
		END

	ELSE	CLEAR GSEAM, LSEAM, SSEAM


	if (szt.le.24 .or. szb.le.24)	;if ez flange, change to jr ez flange
	then	ezjr = 1
	else	clear ezjr


; end treatment...
	CLEAR COPTBL
	TBLCOD = 'M3'
	MM_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0) GOTO GOT_TBL

; if no/bad end treatment, check for default keys...
	CLEAR COPTBL
	TBLCOD = 'IK'
	IK_ITEM = KITMNO
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

GOT_TBL,

;
; Create the output file...
;
	USING SDL_TYP SELECT
	('SW','S4','S9','SV'),	SDL_KEY = 5	;ORIG. SADDLE TABLE
	('SC'),			SDL_KEY = 6	;CONICAL SADDLE
	('ST'),			SDL_KEY = 8	;SHOE TAPS
	(),			SDL_KEY = 5	;DEFAULT
	ENDUSING

	CLEAR TBLKEY
	TBLCOD = 'V1'
	V1_KEY = SDL_KEY	; saddles
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


	VLINE = '// ' + 'vt03s: ' + LCFGIM
	if (dampr) vline = vline(1,%trim(vline)) + ' damper'
	WRITES (chnwrk, VLINE)
	CLEAR VLINE

	vline = '// key: ' + t_key
	writes (chnwrk, vline)
	clear vline

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

	WRITES (chnwrk, '//Saddle Body')

	USING SDL_TYP SELECT
	('S4','S9'),	WRITES (chnwrk, 'FN = 1300-i;')
	('SV','SV'),	WRITES (chnwrk, 'FN = 1300-i;')
	('SW'),		WRITES (chnwrk, 'FN = 1200-i;')
	('SC'),		WRITES (chnwrk, 'FN = 1200-i;')
	('ST'),		WRITES (chnwrk, 'FN = 1200-i;')
	ENDUSING
	
	PNUM = PNUM + 1
	VLINE = PNUM,	'PNUM = ZZX;' [LEFT]
	WRITES (chnwrk, VLINE)

	VLINE = 'QTY= '
	VLINE (6,11) = LQTYOR,	'ZZZZZZ;' [LEFT]
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

	brt = 'B'
	call get_seam		;in case ss

	VLINE = LSEAM
	WRITES (CHNWRK, VLINE)

	VLINE = GSEAM
	WRITES (chnwrk, vline)

	VLINE = SSEAM
	WRITES (chnwrk, vline)

	VLINE = SZB,	'W= ZX;' [left]
	IF (SS_W .GT. 0) VLINE = SS_W,	'W= ZX.XXX;' [LEFT]	;SS

	WRITES (chnwrk, VLINE)

	VLINE = 'L= 80;'
	IF (SS_L .GT. 0) VLINE = SS_L,	'L= ZX.XXX;' [LEFT]	;SS

	WRITES (chnwrk, VLINE)

	PRCT = 'PRCT= '
	PRCT (7,11) = VMAT
	LN = %TRIM(PRCT) + 1
	VLINE (LN,80) = ';'

	WRITES (chnwrk, PRCT)

	USING SDL_TYP SELECT
	('SC','ST'),	WRITES (chnwrk, 'FLOOR= "";')
	ENDUSING

	WRITES (chnwrk, 'END;')

;--- Tap:

	CLEAR VLINE
	WRITES (chnwrk, VLINE)

	USING AN SELECT
	(4),	VLINE = '//Round Lateral Tap'
	(9),	VLINE = '//Round Straight Tap'
	ENDUSING

	USING SDL_TYP SELECT
	('SC'),	VLINE = '//Round Conical Tap'
	('ST'),	VLINE = '//Round Boot Tap'
	ENDUSING
	WRITES (chnwrk, VLINE)

	VLINE = 'FN = 51-i;'

	USING SDL_TYP SELECT
	('SC'),	VLINE = 'FN = 53-i;'
	('ST'),	VLINE = 'FN = 54-i;'
	ENDUSING
	WRITES (chnwrk, VLINE)
	
	VLINE = PNUM,	'PNUM = ZZX;' [LEFT]
	WRITES (chnwrk, VLINE)

	VLINE = 'QTY= '
	VLINE (6,11) = LQTYOR,	'ZZZZZZ;' [LEFT]
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


	VLINE = 'GAUGE= '
	VLINE(8,10) = GA,	'XX;' 
	WRITES (chnwrk, VLINE)

	j = 1
	brt = 'T'
	call get_seam

	VLINE = LSEAM
	WRITES (CHNWRK, VLINE)

	VLINE = GSEAM
	WRITES (chnwrk, vline)

	VLINE = SSEAM
	WRITES (chnwrk, vline)

; Table look-up move to top of routine...

	WR = ((SZT * 1000) + WD4(1))		;.XXX
	IF (SS_WR .GT. 0) WR = SS_WR		;SS

	CLEAR DL
	USING SDL_TYP SELECT
	('SC'),	BEGIN
		WL = WR + 500			;12-17-14
	;;;	WL = WR + 1000			;1.000
		END
	('ST'),	BEGIN
		DL = WR ;>>>+ 3000		;8-7-07 added 3000
		WL = WR + 3000			;3.000
		END
	ENDUSING


	DIFF = (SZb - SZt)

;-------------------------------------------------------------------

;;;	USING SDL_TYP SELECT
;;;	('SC'),	IF (DIFF .LT. 4) WL = WR + 500	;Round Conical Tap'
;;;	ENDUSING
;-------------------------------------------------------------------

	IF (SS_WL .GT. 0) WL = SS_WL		;SS

;12-17-14 ssq: this got more complicated:
;
	if (sdl_typ .ne. 'SC') goto not_sc

;;;	using szt select
;;;	(.le.4),	if (diff.ge.2) wl = wl + 500
;;;	(5 thru 10),	if (diff.ge.3) wl = wl + 500
;;;	(11 thru 16),	if (diff.ge.4) wl = wl + 500
;;;	(17 thru 24),	if (diff.ge.6) wl = wl + 500
;;;	endusing

	using szt select
	(.le. 4),	if (diff.ge.2)	wl = wl + 500
	(5 thru 10),	if (diff.ge.3)	wl = wl + 500
	(11 thru 16),	if (diff.ge.4)	wl = wl + 500
	(17 thru 28),	if (diff.ge.6)	wl = wl + 500
	(29 thru 38),	if (diff.ge.8)	wl = wl + 500
	(39 thru 40),	if (diff.ge.10)	wl = wl + 500
	endusing

not_sc,
	VLINE = WL,	'WL=ZZ.XXX;' [left]
	WRITES (chnwrk, VLINE)		

	IF (SS_DL .GT. 0) DL = SS_DL		;SS

	IF (DL .GT. 0)
		BEGIN
		VLINE = DL, 	'DL= ZZ.XXX;' [left]
		WRITES (chnwrk, VLINE)
		END

	IF (SS_WR .GT. 0) WR = SS_WR		;SS
	VLINE = WR,	'WR= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)



	USING SDL_TYP SELECT
	('SC','ST'),	WRITES (chnwrk, 'SR= 0;')
	ENDUSING

	using t_key select
	('SW9*'),	LT = (WR/2) + 5875
	('SW9'),	LT = 5938
	('SWV9'),	LT = 5938
	('S9*'),	LT = (WR/2) + 5875
	('S9'),		LT = 4000
	('SV9'),	LT = 4000
	('SW4*'),	LT = 5938
	('SW4'),	LT = 5938
	('SWV4'),	LT = 5938
	('S4*'),	LT = 5938
	('S4'),		LT = 4000
	('SV4'),	LT = 4000
	('ST9'),	LT = 4000		;3-28-16
	(),		LT = 3000
	endusing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 11-3-10 replaced by above...
;;;	USING SDL_TYP SELECT
;;;	('SC'),	LT = 3250
;;;	('ST'),	LT = 4188	;> back to 4188
;;;	('SW'),	USING AN SELECT
;;;		(9),	BEGIN
;;;			LT = 4188	;> 12-21-09 back to 4188 per RB
;;;			IF(DAMPR) 
;;;			  BEGIN
;;;			  LT = WR#3*1000
;;;			  IF (SZT.EQ.3 .OR. SZT.EQ.4) LT=4750
;;;			  END
;;;			END
;;;		(),	LT = 4750	; 45 always 4.750
;;;		ENDUSING
;;;	(),	LT = 3000
;;;	ENDUSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

	IF (SS_LT .GT. 0) LT = SS_LT		;SS

	VLINE = LT,	'LT= ZX.XXX;'	[left]
	WRITES (chnwrk, VLINE)


	IF (SS_LB .GT. 0) 
	THEN	BEGIN
		LB = SS_LB		;SS
		VLINE = LB,	'LB= ZX.XXX;' [LEFT]
		END

	ELSE	USING SDL_TYP SELECT
		('SC'),	VLINE = 'LB= 0;'
		('ST'),	VLINE = 'LB= 0;'
		('SW'),	VLINE = 'LB= 0;'
		(),	VLINE = 'LB= 2.5;'	
		ENDUSING

	WRITES (chnwrk, VLINE)

	VLINE = 'OW= 0;'		;always 0
	IF (CMPCOD.EQ.'ROC') VLINE = 'OW= 9;'	;11-10-20 BRANDON
	WRITES (chnwrk, VLINE)

	VLINE = 'OD= 0;'		;always 0
	WRITES (chnwrk, VLINE)

	USING AN SELECT
	(4),	WRITES (chnwrk, 'AT= 45;')
	(9),	WRITES (chnwrk, 'AT= 90;')
	ENDUSING

	USING SDL_TYP SELECT
	('ST'),	IF (AN .EQ. 9) WRITES (chnwrk, 'AR= 90;')
	ENDUSING

	WRITES (chnwrk, PRCT)

	CLEAR TBLKEY
	TBLCOD = 'V2'		;vulcan descriptions
	V2_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	USING SDL_TYP SELECT
	('SC'),	VLINE = 'CONN= L;CN= "'
	('SW'),	VLINE = 'CONN= L;CN= "'
	('ST'),	VLINE = 'CONN= L;CN= "'
	('S'),	VLINE = 'CONN= L;CN= "'
	(),	VLINE = 'CONN= B;CN= "'
	ENDUSING

	LN = %TRIM (VLINE)
	INCR LN
	USING SDL_TYP SELECT
;;	('S4','S9'),	VLINE(LN,60) = 'elbowlock'
	('S4','S9'),	if(t_ga .eq. 2)
			then	VLINE(LN,60) = 'buttweld'
			else	VLINE(LN,60) = 'elbowlock'
	('SV'),		VLINE(LN,60) = 'elbowlock'
	(),	VLINE(LN,60) = '5/8 swedge'
	ENDUSING

	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)

	USING SDL_TYP SELECT
	('SC'),	VLINE = 'CONN= R;CN= "'
	('SW'),	VLINE = 'CONN= R;CN= "'
	('ST'),	VLINE = 'CONN= R;CN= "'
	('S'),	VLINE = 'CONN= R;CN= "'
	(),	VLINE = 'CONN= T;CN= "'
	ENDUSING
	LN = %TRIM (VLINE)

	if (ezjr .and. v2_cn2.eq.'EZ FLANGE') V2_CN2 = 'JR EZ FLANGE'
	INCR LN
	VLINE(LN,60) = V2_CN2
	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)


	WRITES (chnwrk, 'END;')

	CLEAR VLINE
	WRITES (chnwrk, VLINE)

	clear err
	XRETURN

get_seam,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	using brt select
	('B'),	BEGIN
		IF (SS_BLS .NE. A15) LS = SS_BLS
		IF (SS_BGS .NE. A15) GS = SS_BGS
		IF (SS_BSS .NE. A15) SS = SS_BSS
		END
	('R'),	BEGIN
		IF (SS_RLS .NE. A15) LS = SS_RLS
		IF (SS_RGS .NE. A15) GS = SS_RGS
		IF (SS_RSS .NE. A15) SS = SS_RSS
		END
	('T'),	BEGIN
		IF (SS_TLS(J) .NE. A15) LS = SS_TLS(J)
		IF (SS_TGS(J) .NE. A15) GS = SS_TGS(J)
		IF (SS_TSS(J) .NE. A15) SS = SS_TSS(J)
		END
	endusing

	if (ga.ge.16 .and. ga.le.20) goto skip_5lap				;6-21-18
	
	using brt select
	('T'),	if (szt .le. 3)
		begin
		LS = '.5 LAP'
		SS = '.5 LAP'
		end
	('B'),	if (szb.le.3)
		begin
		LS = '.5 LAP'
		SS = '.5 LAP'
		end
	endusing

SKIP_5LAP,
	GSEAM = 'GSEAM= ' + '"' + GS(1,%trim(GS)) + '";'
	LSEAM = 'LSEAM= ' + '"' + LS(1,%trim(LS)) + '";'
	SSEAM = 'SSEAM= ' + '"' + SS(1,%trim(SS)) + '";'
	
	return
;--------------------------------------------------------


check_ss,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear t_key

	clear ss_wb, ss_wt, ss_w, ss_l, ss_sb, ss_st, ss_rl

	clear ss_bls, ss_bgs, ss_bss, ss_bcont, ss_bconb
	clear ss_tls, ss_tgs, ss_tss, ss_tcont, ss_tconb

	for j from 1 thru 2
		begin
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

	;;;	clear ss_bls(j)
	;;;	clear ss_bgs(j)
	;;;	clear ss_bss(j)
	;;;	clear ss_bcont(j)
	;;;	clear ss_bconb(j)
		end	


	LN = %TRIM(CODE)
	T_KEY = CODE(1,LN)
	IF (dampr)	T_KEY = CODE(1,LN) + '*'

	using ga select
	(22 thru 26),	T_GA = 1
	(16 thru 20),	T_GA = 2
	endusing

;BODY...

	CLEAR KEY17
	CLEAR TBLKEY
	TBLCOD = 'S2'		;safety seal parameters
	S2_KEY = T_KEY
	S2_MAT = 'G'		;for now...
	S2_GA = T_GA

	S2_TYPE = 'B'		;Body
	S2_DIM = SZB
	S2_R = 1		;seams
	KEY17 = TBL_KEY		;SAVE THE KEY

	j = 1			;for f3 note segs (f3ks) check...
	if (f3ks(j) .ne. 1) goto skip_body

	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_BLS = S2_LSEAM
	SS_BGS = S2_GSEAM
	SS_BSS = S2_SSEAM
	SS_BCONB = S2_CONB
	SS_BCONT = S2_CONT

	tbl_key = key17
;;;	KEY17 = TBL_KEY
	S2_R = 2		;values
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_W = S2_W	;ZZX.XXX (WB, W, WL)

;;;	TBL_KEY = KEY17
;;;	S2_R = 2		;values
;;;	S2_DIM = BC
;;;	IF (BD .GT. BC) S2_DIM = BD
;;;	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
;;;	IF (LOKCTL .NE. 0) CLEAR COPTBL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SS_L = S2_L	;ZZX.XXX (WT, L, DL)

skip_body,

	incr j
	if (f3ks(j) .ne. 1) goto skip_tap1

	TBL_KEY = KEY17
	S2_TYPE = 'T'		;TAP
	S2_DIM = SZT
	S2_R = 1		;SEAMS
	KEY17 = TBL_KEY		;SAVE THE KEY
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_TLS(1) = S2_LSEAM
	SS_TGS(1) = S2_GSEAM
	SS_TSS(1) = S2_SSEAM
	SS_TCONB(1) = S2_CONB
	SS_TCONT(1) = S2_CONT

	tbl_key = key17
;;;	KEY17 = TBL_KEY
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

skip_tap1,

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	IF (BD .LE. 0) return	;2nd Tap?
;;;
;;;	incr j
;;;	if (f3ks(j) .ne. 1) goto skip_tap2
;;;
;;;	TBL_KEY = KEY17
;;;	S2_TYPE = 'T'		;TAP
;;;	S2_DIM = BD
;;;	S2_R = 1		;SEAMS
;;;
;;;	KEY17 = TBL_KEY		;SAVE THE KEY
;;;	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
;;;	IF (LOKCTL .NE. 0) CLEAR COPTBL
;;;
;;;	SS_TLS(2) = S2_LSEAM
;;;	SS_TGS(2) = S2_GSEAM
;;;	SS_TSS(2) = S2_SSEAM
;;;	SS_TCONB(2) = S2_CONB
;;;	SS_TCONT(2) = S2_CONT
;;;
;;;	TBL_KEY= KEY17
;;;	S2_R = 2		;VALUES
;;;	
;;;	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
;;;	IF (LOKCTL .NE. 0) CLEAR COPTBL
;;;
;;;	SS_WL(2) = S2_WL	;ZZX.XXX (WB, W, WL)
;;;	SS_DL(2) = S2_DL	;ZZX.XXX (WT, L, DL)
;;;	SS_WR(2) = S2_WR	;ZZX.XXX (WR)
;;;	SS_SR(2) = S2_SR	;ZZX.XXX (WR)
;;;	SS_LT(2) = S2_LT	;ZZX.XXX (LT)
;;;	SS_OW(2) = S2_OW	;ZZX.XXX (LT)
;;;	SS_OD(2) = S2_OD	;ZZX.XXX (LT)
;;;	SS_AT(2) = S2_AT	;ZZX.XXX (LT)
;;;	SS_AR(2) = S2_AR	;ZZX.XXX (LT)
;;;	SS_LB(2) = S2_LB	;ZZX.XXX (LB)
;;;
;;;skip_tap2,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	return
;-------------------------------------------------------

	END

		
