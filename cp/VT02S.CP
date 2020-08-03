SUBROUTINE VT02S	; Gorelock/Eccentric Reducers
	PNUM	,D
	LINE	,A	;ORDER L/I
	CHN182	,D	;CHANNEL FOR COPTBL
	MEMDAT	,A	;CPMEMO
	ERR	,D	;1 => ERROR DURING CONVERSION

; Routine for Gorelock Reducers
;ERW 24 18 08	- welded eccentric reducer
;ER 24 18 08	- eccentric 
; R 26 10 08	- gorelock
; A |  |  |_ sz-t
;   |  |____ sz-b
;   |_______ gauge
;
;

; 02-14-08 ssq: check bottom & top dim for ezjr...

common	vulcan
	dampr	,d1
	is_2	,d1	;1 = w2file has data
	is_3	,d1	;1 = w3file has data
	chnw2	,d2	;-2
	chnw3	,d2	;-3
	vdkey	,d4	;VD key
	m1_note	,a12
	m2_note	,a12
	m3_note	,a12
	d_br1	,d1	;1 = damper on branch 1	(3-20-17)
	d_br2	,d1	;1 = damper on branch 2	(3-20-17)
	org_item	,a15		;original value of ordlin.lcfgim

EXTERNAL FUNCTION
	SSKEY	,D

record	pipeg
	.include 'def:pipeg.def'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.def'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CPMEMO
	.INCLUDE 'DEF:CPMEMO.DEF'

RECORD	PARAM
	QTY	,D6
	GAUGE	,D2
	SB	,D2
	SR	,D2
	R	,D2
	L	,D6
	WB	,D6
	WT	,D6
	OW	,D6
	OD	,D6
	GSEAM	,A30
	LSEAM	,A30
	SSEAM	,A30


RECORD	V_LINE1
	VLINE	,A80

RECORD	ARAY
	WD4	,4D4


record
	keyseg		,a5
record,x
	f3ks		,5d1

RECORD	VARS
	tl	,d5
	vp_key		,d4	;vulcan parameter (V1) key
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

	LADD	,D4
	burn	,d1	;baby, burn
	vinyl	,d1
	spiral	,d1,	4	;f3 note for spiral ends
	xga	,d2
	matl	,d1	;f1 matl
	vmat	,a5	;f1 matl desc
	ezjr_b	,d1	;1=ez flange jr - bottom
	ezjr_t	,d1	;1=ez flange jr - top
	chnwrk	,d2
	DIF	,D8
	TDIF	,D8	;TRUNCATED DIF
	KITMNO	,A15
	I_DAT	,A15
	CNT	,D3
	J	,D6
	K	,D6
	LF3x	,D5		;THIS IS A FIELD IN RD045A.NEW...
	CODE	,A3
	SZB	,D2
	SZT	,D2
	AN	,D2
	GA	,D2
	BF	,D1
	ERROR	,D1
	LN	,D6
	D4	,D4
	READ	,D1,0
	LOKCTL	,D1
;
PROC

	ERR = 1			;ASSUME ERROR

	ORDLIN = LINE

	CPMEMO = MEMDAT

	KITMNO = LCFGIM


;;;	I_DAT = LCFGIM
;;;	IF (CODE .NE. 'ER') 
;;;	THEN	CODE = LITMNO(1,1)
;;;	ELSE	I_DAT = LCFGIM(2,15)	;SKIP THE "E"


	clear vinyl

	CODE = LITMNO(1,3)
	USING LITMNO(1,3) SELECT
	('RW','RW'),	BEGIN
			CODE = 'RW'
			I_DAT = LCFGIM(3,15)
			END
	('RV'),		BEGIN
			vinyl = 1
			CODE = 'RV'
			I_DAT = LCFGIM(3,15)
			END
	('ERW'),	BEGIN
			CODE = 'ERW'
			I_DAT = LCFGIM(4,15)
			END
	('R1','R2'),	BEGIN
			CODE = 'R'
			I_DAT = LCFGIM(2,15)
			END
	('ER1','ER2'),	BEGIN
			CODE = 'ER'
			I_DAT = LCFGIM(3,15)
			END
	ENDUSING


	XCALL ALPDC (I_DAT(1,2), GA, BF)
	IF (BF) XRETURN

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

	XCALL ALPDC (I_DAT(3,4), SZB, BF)
	IF (BF) XRETURN

	XCALL ALPDC (I_DAT(5,6), SZT, BF)
	IF (BF) XRETURN

	clear ezjr_b, ezjr_t				;2-21-08
;;;	if (lf3.ne.15 .and. lf3.ne.16) goto no_ezjr	;2-21-08
	if (lf3.ne.3 .and. lf3.ne.8) goto no_ezjr

	;if ez flange, change to jr ez flange
	if (szb.le.24)	ezjr_b = 1
	if (szt.le.24)	ezjr_t = 1

no_ezjr,
	if (code .eq. 'RW') goto skip_ga_test	;vdkey set in mainline...
	using ga select
	(16),		vdkey = 3

	(),	begin
		vdkey = 2
	;;;	if(szt.lt.9) vdkey = 4
		if(szt.lt.8) vdkey = 4
		if(szb.lt.8) vdkey = 4
		end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replaced with new rules 3-7-11

;;;	(18 thru 26),	begin
;;;			if (szb .ge. 18) 
;;;			then	vdkey = 2	;7-13-09
;;;			else	vdkey = 4	;7-13-09
;;;
;;;			if (szt .lt. 8) vdkey = 4	;6-18-09 per bill
;;;			end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	endusing

	using vdkey select
	(2),	t_key = 'RF'
	(4),	t_key = 'RH'
	(),	t_key = '**'
	endusing

skip_ga_test,

	VMAT = 'GALV'			;default value
	IF (VINYL) VMAT = 'PVS'

	MATL = 1		;GALV
	IF (VINYL) MATL = 5	;pvs

	clear burn

	CLEAR COPTBL
	TBLCOD = 'M1'
	MM_KEY = LF1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO SKIP_F1_MATL
	IF (MM_VULC .EQ. 2) XRETURN	;6-24-08
	MATL = MM_MATL
	VMAT = MM_VMAT
	if (lf1 .eq. 68) matl = 6 ;G90 12-16-09

SKIP_F1_MATL,

	D5 = LF3
	keyseg = D5, 'ZZZZX' [left]
	tl = %trim(keyseg)
	if (tl .eq. 1) f3ks(2) = f3ks(1)

	using ldept select
	('F'),	t_key = 'RF'
	('H'),	t_key = 'RH'
	('K'),	t_key = 'RK'
	(),	t_key = 'R??'
	endusing
	
	if (code(1,1) .eq. 'E') t_key = 'E' + t_key
	call init_ss
	clear ss_wb, ss_wt, ss_w, ss_l

	for k from 1 thru 2
	  begin
	  using f3ks(k) select
	  (1),	call check_ss
	  (),	call get_v1_table
	  endusing
	  end


	CLEAR COPTBL
	TBLCOD = 'V3'
	V3_KEY = vdkey
	V3_GA = GA
	using matl select	;for now default to galv unless alum...
	(2),	V3_MAT = MATL
	(5),	v3_mat = matl	;or pvs (6/22/09)
	(),	V3_MAT = 1
	endusing

	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0) 
	THEN	BEGIN
		GS=V3_GSEAM
		LS=V3_LSEAM
		SS=V3_SSEAM
		END

	ELSE	CLEAR GS, LS, SS

;-------------------------------------------------------
	using litmno(1,4) select	;6-20-18 per brandon
	('ERW16' THRU 'ERW20'),	NOP
	('RW16' THRU 'RW20'), NOP
	(),	if (szt .le. 3)
		begin
		ss_rls = '.5 LAP'
		ss_rss = '.5 LAP'
		end


	endusing

;;;	if (szt .le. 3)
;;;		begin
;;;		ss_rls = '.5 LAP'
;;;		ss_rss = '.5 LAP'
;;;		end
;-------------------------------------------------------

	if (ss_rgs .ne. a15) gs = ss_rgs
	if (ss_rls .ne. a15) ls = ss_rls
	if (ss_rss .ne. a15) ss = ss_rss

	GSEAM = 'GSEAM= ' + '"' + GS(1,%trim(GS)) + '";'
	LSEAM = 'LSEAM= ' + '"' + LS(1,%trim(LS)) + '";'
	SSEAM = 'SSEAM= ' + '"' + SS(1,%trim(SS)) + '";'
	
;;;	CLEAR LF3
;;;	IF (LMSQ3 .GT. 0) LF3 = F3_KEY(LMSQ3)

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

	VLINE = '// ' + 'vt02s: ' + LCFGIM
	WRITES (chnwrk, VLINE)

;;;	using ldept select
;;;	('F'),	t_key = 'RF'
;;;	('H'),	t_key = 'RH'
;;;	(),	t_key = 'R??'
;;;	endusing
;;;	
;;;	if (code(1,1) .eq. 'E') t_key = 'E' + t_key

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

	vline = '// ' + 'pipe gauge: ' + lpipeg
	writes (chnwrk, vline)

	using code select
	('ER'),	WRITES (chnwrk, '//Round Eccentric Reducer')
	('R'),	WRITES (chnwrk, '//Round Concentric Reducer')
	endusing

	USING SZB SELECT
	(.LT. 20),	WRITES (chnwrk, 'FN = 315-i;')
	(.GE. 20),	WRITES (chnwrk, 'FN = 314-i;')
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

	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,LN) = ';'
	WRITES (chnwrk, VLINE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	using vmat select
;;;	('PGRP'),	BURN = 1
;;;	('S.S.'),	BURN = 1
;;;	('ALUM'),	BURN = 1
;;;	('G-90'),	BURN = 1
;;;	(),		BURN = 0
;;;	endusing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	USING GA SELECT
	(22 THRU 26),	NOP
	(),		BURN = 0
	ENDUSING

	IF (BURN)	WRITES (CHNWRK,'BURN=A;')
	
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

;;;	WRITES (chnwrk, 'LSEAM= "1/2in.Lap Spot";')
;;;	GSEAM = 'GSEAM= ;'

	VLINE = LSEAM
	WRITES (chnwrk, vline)

	VLINE = GSEAM
	WRITES (chnwrk, vline)

	VLINE = SSEAM
	WRITES (chnwrk, vline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7-15-13: WB need uses f3ks(1), WT uses f3ks(2)...

;;;	for k from 1 thru 2
;;;	  begin
;;;	  using f3ks(k) select
;;;	  (1),	nop	;don above> call check_ss
;;;	  (),	call get_v1_table
;;;	  endusing
;;;	  end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	k = 1					;7-15-13
	if (f3ks(k) .ne. 1) call get_v1_table	;7-15-13 for WB


	SB = 		;always zero for reducers
	SR = 		;always zero for reducers

	WB = ((SZB * 1000) + WD4(1))		;.XXX
	if (ss_wb .gt. 0) wb = ss_wb		;ss

	VLINE = WB,	'WB= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	VLINE = 'SB= 0;'
	WRITES (chnwrk, VLINE)

	k = 2					;7-15-13
	if (f3ks(k) .ne. 1) call get_v1_table	;7-15-13 for WT

	WT = ((SZT * 1000) + WD4(2))		;.XXX
	if (ss_wt .gt. 0) wt = ss_wt		;ss

	VLINE = WT,	'WT= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	VLINE = 'ST= 0;'
	WRITES (chnwrk, VLINE)

	
	using matl select
	(2,4,6),	begin		;only alum & sst, g90
		VLINE = 'S1= 3.437;'	;;;>3.625;'
		WRITES (CHNWRK, VLINE)

		VLINE = 'S2= 3.437;'	;;;>3.625;'
		WRITES (CHNWRK, VLINE)
		end
	endusing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; removed 12/16/09
;;;	IF (BURN)
;;;		BEGIN
;;;		WRITES (CHNWRK, 'S1=3.625;')
;;;		WRITES (CHNWRK, 'S2=3.625;')
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Total Reduction/Body Length:
	DIF = WB - WT
;;;	TDIF = DIF#3
	tdif = szb - szt	;4-13-11, per bill, use orig dimensions.
	USING MATL SELECT
	(2,4,6),	LADD = 7250		;7.250 FOR ALUM & SST G90
	(),		LADD = 0
	ENDUSING

	USING GA SELECT				;12-7-09 add 7.25
	(16 thru 26),	USING TDIF SELECT
			(1),		L=3
			(2,3),		L=4
			(4,5,6),	L=TDIF + 2
			(7,8),		L=TDIF + 3
			(9,10,11),	L=TDIF + 4
			(12,13,14),	L=TDIF + 5
			(15,16),	L=22
			(17,18),	L=25
			(19),		L=26
			(20,21,22),	L=TDIF + 8
			(),		L=31
			ENDUSING
	ENDUSING
	
	IF (CODE .EQ. 'ER') 
	THEN	L = (TDIF*25000)#1			;12-16-09
	ELSE	L = L*1000 				;12-16-09
	L = L + LADD					;12-16-09

;;;	only adds 1.25" to length if not welded
	using t_key select				;5/4/16
	('RK'),		nop
	(),		begin 
			if (ezjr_t) L = L + 625
			if (ezjr_b) L = L + 625
			end
	endusing
	
;;;	USING LDEPT SELECT
;;;	('F'),	IF (CODE .EQ. 'ER') L = (DIF*25)#1
;;;	(),	IF (CODE .EQ. 'ER') L = DIF*2
;;;	ENDUSING

;;;	IF (CODE .EQ. 'ER') L = (DIF*25)#1		;9-20-07

	VLINE = L,	'L= ZX.XXX;' [left]
	WRITES (chnwrk, VLINE)

;;;	OW = (DIF/2)#3
	OW = (DIF*10/2)#1
	VLINE = OW,		'OW= ZX.XXX;' [left]
	WRITES (chnwrk, VLINE)

;;;	WB = (DIF/2)#3
;;;	VLINE = WB,		'OD= ZX;' [left]
	OD = (DIF*10/2)#1
	IF (CODE .EQ. 'ER') OD = DIF

	VLINE = OD,		'OD= ZX.XXX;' [left]
	WRITES (chnwrk, VLINE)
	

vline= 'PRCT= CATALOG SETUP Design Build S & D KD - ASMBL Mult Rfcmt Galvanized;'
        ;2345678901234567890123456789012345678901234567890123456789012345678901234567890
        ;        1         2         3         4         5         6         7

	VLINE(62,72) = VMAT
	ln = %trim(vline) + 1
	vline(ln,ln) = ';'
	writes (chnwrk, vline)

;;;	WRITES (chnwrk, 
;;;& 'PRCT= CATALOG SETUP Design Build S & D KD - ASMBL Mult Rfcmt Galvanized;')


	CLEAR TBLKEY
	TBLCOD = 'V2'		;vulcan descriptions
	V2_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	VLINE = 'CONN= B;CN= "'
	LN = %TRIM (VLINE)
	INCR LN
	if (ezjr_b .and. v2_cn1.eq.'EZ FLANGE') V2_CN1 = 'JR EZ FLANGE'
	if (ss_rconb .ne. a15) v2_cn1 = ss_rconb

	VLINE(LN,60) = V2_CN1
	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)

	VLINE = 'CONN= T;CN= "'
	LN = %TRIM (VLINE)
	INCR LN
	if (ezjr_t .and. v2_cn2.eq.'EZ FLANGE') V2_CN2 = 'JR EZ FLANGE'
	if (ss_rcont .ne. a15) v2_cn2 = ss_rcont

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


;================================================
get_v1_table,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR TBLKEY
	TBLCOD = 'V1'
	V1_KEY = VDKEY		;4-6-11 SSQ	
;;;	V1_KEY = VP_KEY	
;;;	V1_F3 = LF3
	v1_f3 = f3ks(k)
;;;	v1_f3 = %sskey(lf3)	;add ss to v-param table... 9-01-10
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	FOR J FROM 1 THRU 4
		USING GA SELECT
		(16),		WD4(J) = V1_16(J)
		(18 THRU 20),	WD4(J) = V1_1820(J)
		(22 THRU 26),	WD4(J) = V1_2226(J)
		(),		WD4(J) =
		ENDUSING

	if (f3ks(k) .ne. spiral) return
;;;	if (lf3 .ne. spiral)	return
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
	
; removed 6-22-09...
;;;	set wd4(2), wd4(3), wd4(4) = wd4(1)

	return
;-----------------------------------------------------------------

init_ss,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear ss_wb, ss_wt, ss_w, ss_l, ss_sb, ss_st, ss_rl

	clear ss_bls, ss_bgs, ss_bss, ss_bcont, ss_bconb
	clear ss_tls, ss_tgs, ss_tss, ss_tcont, ss_tconb
	clear ss_rls, ss_rgs, ss_rss, ss_rcont, ss_rconb

	return
;------------------------------------------------------------

check_ss,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; 8-18-10 - I'm trying to use the same code in each of these
	; 		routines, clearly not all of the variables and
	;		logic below is necessary...


	using t_key select
	('RH','RF'),	nop
	('ERH','ERF'),	nop
	(),		return
	endusing



; t_key already set
;;;	T_KEY = CODE(1,LN)
;;;	IF (IS_R)	T_KEY = CODE(1,LN) + 'R'

	using ga select
	(22 thru 26),	T_GA = 1
	(16 thru 20),	T_GA = 2
	endusing

;;;	j = 1			;for f3 note segs (f3ks) check...
	j = k			;set where check_ss is called...

	CLEAR KEY17
	CLEAR TBL_KEY
	TBLCOD = 'S2'		;safety seal parameters
	S2_KEY = T_KEY
	S2_MAT = 'G'		;for now...
	S2_GA = T_GA
	S2_TYPE = 'R'		;REDUCER


	key17 = tbl_key

	using j select
	(1),	CALL BOTTOM	;S2_DIM = SZB	;BOTTOM
	(2),	CALL TOP	; S2_DIM = SZT	;TOP
	endusing

	RETURN
;----------------------------------------------------------

Bottom,	;---------------------------------------------------
	S2_R = 1		;SEAMS
	S2_DIM = SZB	;BOTTOM

	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_RLS = S2_LSEAM
	SS_RGS = S2_GSEAM
	SS_RSS = S2_SSEAM
	SS_RCONB = S2_CONB

	tbl_key= key17

	S2_DIM = SZB		;BOTTOM
	S2_R = 2		;values
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_WB = S2_WB	;ZZX.XXX (WB, W, WL)
	SS_SB = S2_SB

	return
;-----------------------------------------------------------


top,	;---------------------------------------------------
	TBL_KEY = KEY17

	S2_R = 1		;SEAMS
	KEY17 = TBL_KEY		;SAVE THE KEY
	S2_DIM = SZT	;top

	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_RLS = S2_LSEAM
	SS_RGS = S2_GSEAM
	SS_RSS = S2_SSEAM
	SS_RCONT = S2_CONT

	S2_R = 2		;values
	S2_TYPE = 'R'		;REDUCER
	S2_DIM = SZT		;TOP
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_WT = S2_WT	;ZZX.XXX (WT, L, DL)
	SS_ST = S2_ST

	return
;-------------------------------------------------------

	END

		
