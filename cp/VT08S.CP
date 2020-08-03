SUBROUTINE VT08S
	PNUM	,D
	LINE	,A	;ORDER L/I
	CHN182	,D	;CHANNEL FOR COPTBL
	MEMDAT	,A	;CPMEMO
	ERR	,D	;1 => ERROR DURING CONVERSION

; Routine for Shoe Taps
; 5/1/17: added vt03s code for large (spiral body) tee's
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
	d_br1	,d1	;1 = damper on branch 1	(3-20-17)
	d_br2	,d1	;1 = damper on branch 2	(3-20-17)
	org_item	,a15		;original value of ordlin.lcfgim

common	pressed_reducer
	p_file_data	,d1	;1=vt08s has written data to 'spl:"

EXTERNAL FUNCTION 
	sskey	,d

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
	AR	,2D6
	AT	,D2
	SB	,D2
;;;	SR	,D2
	SR	,2D6
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

RECORD	V_LINE2
		,A2,	'//'
	VL2,	A78

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

record	morearrays
;;;11-6-19>	p45	,9d4,	2000,3000,6000,3828,3000,3000,3000,3000,3000
	p45	,9d4,	2000,3000,3000,3828,3000,3000,3000,3000,3000
	p90	,9d4,	2000,3000,3000,3828,3000,3000,3000,3000,3000
	pred	,9d4,	2000,2000,2000,2000,2000,2000,2000,2000,2000
	f3_ow	,d1	;holds f3(2). used to index 1 of the arrays for value 
			;of "OW".
	f3_x	,d1	;holds f3(1). used to indxe 1 of the arrays for the
			;value of "X" which added to f3_ow yields the amount
			;to add to get "L" or "L5".
	XOW	,D6
	XADD	,D6
	xsum	,d6	;(XX.XXXX) p45(f_ow) + p45(f3_x).  amount to add to get "L"	

RECORD	VARS
	bx_add		,d4	;x.xxx	7-17-19
	BODY_L		,D2	;body length for spiral body tee's
	spc_mat		,d1	;these materials override ar(i) & seams
	L_ADD		,D4	;X.XXX	
	cmp_l		,d6	;xx.xxx - computed "L" value
	big_wr		,d6	;larger wr(j)
	sp_b_add	,d4	;x.xxx	- spiral add-on
	sp_t_add	,d4	;x.xxx	- spiral add-on
	all_ss		,d1	;1= all ss
	EZ_R		,D1	;1 = REDUCER IS EZ FLANGE
	BRT		,A1	;B/R/T for com_sec
	KEY17		,A15	;FOR TBL_KEY
	IS_R		,D1	;1=is reducer
	d5		,d5
	D6		,D6
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
	sav_pp	,d1	;1 = valid mat'l for pressed parts
	ladd	,d4
	x_lf3	,d5
	ezjr_t	,d1
	ezjr_b	,d1
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
	A15	,A15
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
	LBC	,D2		;LARGER OF BC & BD
	BD	,D2
	TEMP_BC	,D2		;TEMP HOLDER FOR LARGER OF TWO TAPS
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

	CLEAR GA, MA, MB, BC, LBC, BD, AN, VINYL, t_ezjr, sav_pp

	using lf1 select		;12-17-14 ssq
	(54,55,59,62,63,66,69), spc_mat = 1
	(),			spc_mat = 0
	endusing


	CLEAR TBLKEY
	TBLCOD = 'V2'		;vulcan descriptions
;;;	v2_key = %sskey(lf3)	;change SS to LE
	V2_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	S2_CN1 = V2_CN1
	S2_CN2 = V2_CN2
	S2_CN3 = V2_CN3
	S2_CN4 = V2_CN4

	IF (S2_CN2 .EQ. 'EZ FLANGE')	;EZ FLANGE REDUCER?
	THEN	EZ_R = 1
	ELSE	EZ_R = 0


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
	
	d5 = lf3
	keyseg = d5,	'ZZZZX' [left]
	if (d5 .eq. 1) keyseg = '11111'
	if (d5 .eq. 3) keyseg = '33333'	;11-3-10
	if (d5 .eq. 2) keyseg = '22222'	;3-7-17
	if (d5 .eq. 4) keyseg = '44444'	;3-7-17

	LN = %TRIM(CODE)
	USING CODE(LN,LN) SELECT
	('C'),	CALL MMBB		;CROSS
	('T'),	CALL MMB		;TEE
	ENDUSING

	IF (BC .LT. BD)			;4/20/16
		BEGIN
		TEMP_BC = BC
		BC = BD
		BD = TEMP_BC
		END

	if (ma .gt. mb)
	then	IS_R = 1			;is reducer
	else	IS_R = 0			;not a reducer

	if (ga .eq. 26) ga = 24		;6-19-08


;;;	clear matl
	if (vinyl .eq. 1)	;6-11-09
	then	matl = 5
	else	matl = 1		
	CLEAR COPTBL
	TBLCOD = 'M1'
	MM_KEY = LF1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
;;;	IF (LOKCTL .NE. 0) GOTO SKIP_F1_MATL
	IF (LOKCTL .NE. 0) 
		begin
		clear coptbl
		GOTO SKIP_F1_MATL
		end
	IF (MM_VULC .EQ. 2) XRETURN	;6-24-08
	MATL = MM_MATL
	VMAT = MM_VMAT
	if (lf1 .eq. 68) matl = 6	;G90
	sav_pp = m1_pp			;1-04-10 save valid pp value

SKIP_F1_MATL,

	CODE = CODE(1,%trim(code) ) + AN

	call check_ss			;check for safety seal values...

	clear p_list

; end treatment...
	CLEAR COPTBL
	TBLCOD = 'M3'
	MM_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0) 
		begin
	;;;	p_f3 = mm_short
		p_f3 = mm_long			;9-21-09 moved here...
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

	is_pressed = 0
	clear reducer

;
; Create the output file...
;
;;;	p_f3 = mm_long			;9-21-09 moved above...

	CLEAR VLINE
	WRITES (chnwrk, VLINE)

;;;	VLINE = '// ' + 'vt08s: ' + LCFGIM
	VLINE = '// ' + 'vt08s: ' + org_item	;3-27-17

	if (dampr) vline = vline(1,%trim(vline)) + ' damper'
	WRITES (chnwrk, VLINE)

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

;-------------------------------------------------------------------
; 5-1-17: check for spiral body tee:

	USING MA SELECT
	(.GE. 38),	IF (BC.GE.20 .OR. BD.GE.20)GOTO SPIRAL_BODY
	ENDUSING
;-------------------------------------------------------------------

	WRITES (chnwrk, '//Straight Body')
	WRITES (chnwrk, 'FN = 1000-i;')

;----------------------------------------------------------------
; 10-14-10: load the values for ow & add, compute xsum...

	f3_ow = f3ks(2)
	if (f3_ow .le. 0) f3_ow = f3ks(1)
	if (f3_ow .le. 0) f3_ow = 1

	f3_x = f3ks(1)
	if (f3_x .le. 0) f3_x = 1


	using t_key select
	('T9','C9','CT9','CC9','BN9','STC','STT'), BEGIN
					     XOW = P90(F3_OW)
					     XADD = P90(F3_X)
					;;;     if (t_key(ln,ln) .eq. 'R') xow = 2000	;moved here 1-2-19
					     if (is_r) xow = 2000	;10-30-19
					     END
	('T4','C4'),	BEGIN
			XOW = P45(F3_OW)
			XADD = P45(F3_X)
		;;;> 1-2-19	if (f3_x .eq. 3) xadd = 3000	;for ezflang
			END
	;;;(),		SET XOW, XADD = 2000	;MUST BE REDUCERS WHICH ARE ALL 2
	endusing
	
	ln = %trim(t_key)
;;;	if (t_key(ln,ln) .eq. 'R') xow = 2000		;reducers

	XSUM = (XOW + XADD)	;X.XXXX

;----------------------------------------------------------------

	brt = 'B'

;;	BK - 4/20/16 THIS IS NOT USED
;;
;;	LBC = BC			;12-28-09
;;	IF (BD .GT. BC) LBC = BD	;12-28-09

	
	clear L_ADD			;10-27-10

	call get_code			;5-2-17: spiral_body needs this too


	if (wr(1) .gt. wr(2))	;this is like "LBC" using modified branch values
	then cmp_l = wr(1)
	else cmp_l = wr(2)

	big_wr = cmp_l

	CMP_L = CMP_L + L_ADD

;check ss...
	if (ss_wr(1) .gt. 0) cmp_l = ss_wr(1)
	if (ss_wl(1) .gt. 0) cmp_l = ss_wl(1)
	if (ss_wr(2) .gt. cmp_l) cmp_l = ss_wr(2)
	if (ss_wl(2) .gt. cmp_l) cmp_l = ss_wl(2)

	L = CMP_L + XSUM

	USING CODE SELECT		;RE-CALCULATE "L" FOR T4 & C4...
	('T4','C4'),		BEGIN
				CALL F3_MEMO
				L = L5		
				END
	ENDUSING


	call bx_add		;7-17-19
	L = L + BX_ADD		;7-17-19

	L_LINE = L,	'L= ZZ.XXX;' [left]		;

	vxkey = vdkey
	CALL COM_SEC

	CLEAR TBLKEY
	V1_KEY = V_KEY

	x_lf3 = lf3
	call get_v1_table

	W = ((MA * 1000) + WD4(1))
	IF (SS_W .GT. 0) W = SS_W		;SS

	VLINE = W, 'W= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, L_LINE)
	

	VLINE = 'PRCT='
	VLINE(7,11) = VMAT	
	LN = %TRIM(VLINE) + 1
	VLINE(LN,LN) = ';'

	writes (chnwrk, vline)

	WRITES (chnwrk, 'FLOOR= "";')


; safety seal conns:
	if (ss_bconb .ne. a15) s2_cn1 = ss_bconb
	if (ss_bcont .ne. a15) s2_cn2 = ss_bcont


	using ga select
	(16 thru 20),	if (is_r .and. .not.vinyl) S2_CN2 = 'BUTT WELD'
	(22 thru 26),	if (is_r .and. .not.vinyl) S2_CN2 = 'ELBOWLOCK'
	endusing

; if f3 note is e-z flange and following conditions are true, change to jr...
	clear t_ezjr, is_ezjr

	IF (MA.GT.5 .AND. MA.LT.24) t_ezjr = 1

;;;	IF (t_ezjr .and. lf3.eq.3)
	IF (t_ezjr .and. f3ks(1).eq.3)
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


	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, 'END;')
	CLEAR VLINE
	WRITES (chnwrk, VLINE)
	GOTO TAPS

get_code,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; get code and vulcan parameters
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;5-2-17 made a routine so it can be called from spiral_body

; 12-23-09 changed "BC" to "LBC" in this using...
	USING CODE SELECT
	('STC','STT'),			BEGIN
					IF(CODE.EQ.'STT') V_KEY = 10
					IF(CODE.EQ.'STC') V_KEY = 12
					L_ADD = 3000
					AT = 90
					END

	('T4','C4'),			BEGIN
					IF (CODE.EQ.'T') V_KEY = 9
					IF (CODE.EQ.'C') V_KEY = 11
				;;;	CALL F3_MEMO
				;;;	L = L5		;10-13-10
					AT = 45
					END

	('CC9','CT9'),			BEGIN
					IF(CODE.EQ.'CT9') V_KEY = 10
					IF(CODE.EQ.'CC9') V_KEY = 12
					L_ADD = 1000
					AT = 90
					END

	('C9','T9'),			BEGIN
					IF (CODE.EQ.'T') V_KEY = 9
					IF (CODE.EQ.'C') V_KEY = 11
					AT = 90
					END
	ENDUSING


; 10-27-10: need wl or wr from taps...
	CLEAR TBLKEY
	V1_KEY = V_KEY
	x_lf3 = lf3
	call get_v1_table
	WR(1) = ((BC * 1000) + WD4(3))	;1st Tap...
	WR(2) = ((BD * 1000) + WD4(4))	;2nd Tap...

	return
;---------------------------------------------------------------------
;--------------------------------------------------------------
; ;Code for Saddle Body from VT03S:
SPIRAL_BODY,


	WRITES (chnwrk, '//Spiral Body')
	WRITES (chnwrk, '//FN = 1200-i;')
	
;;;	PNUM = PNUM + 1
	VLINE = PNUM,	'//PNUM = ZZX;' [LEFT]
	WRITES (chnwrk, VLINE)

	VLINE = 'QTY= '
	VLINE (6,11) = LQTYOR,	'ZZZZZZ;' [LEFT]
	VL2 = VLINE
	WRITES (CHNWRK, V_LINE2)
;;;	WRITES (chnwrk, VLINE)

	VLINE = 'ADINFO='
	if (dampr)
	then	VLINE (9,47) = LDESCR(1,%trim(ldescr)) + ' W/DAMPER'
	else	VLINE (9,38) = LDESCR

	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,LN) = ';'
	VL2 = VLINE
	WRITES (CHNWRK, V_LINE2)
;;;	WRITES (chnwrk, VLINE)

	VLINE = 'METALN= "'
	VLINE (10,13) = VMAT
	LN = %TRIM(VLINE) + 1
	VLINE (LN,80) = ' SPIRAL FITT";'
	VL2 = VLINE
	WRITES (CHNWRK, V_LINE2)
;;;	WRITES (chnwrk, VLINE)

	XGA = GA
	IF (MATL .NE. 2) GOTO NOT_ALUM_SB	
	USING GA SELECT
	(16),	XGA = 63
	(18),	XGA = 50
	(20),	XGA = 40
	(22),	XGA = 32
	(24),	XGA = 24
	(26),	XGA =
	ENDUSING

NOT_ALUM_SB,
	VLINE = 'GAUGE= '
	VLINE(8,10) = XGA,	'XX;' 
	VL2 = VLINE
	WRITES (CHNWRK, V_LINE2)
;;;	WRITES (chnwrk, VLINE)

	brt = 'B'
	call get_seam		;in case ss

	VLINE = LSEAM
	VL2 = VLINE
	WRITES (CHNWRK, V_LINE2)
;;;	WRITES (CHNWRK, VLINE)

	VLINE = GSEAM
	VL2 = VLINE
	WRITES (CHNWRK, V_LINE2)
;;;	WRITES (chnwrk, vline)

	VLINE = SSEAM
	VL2 = VLINE
	WRITES (CHNWRK, V_LINE2)
;;;	WRITES (chnwrk, vline)

; 5-2-17
;;;	VLINE = SZB,	'W= ZX;' [left]
	VLINE = MA,	'W= ZX;' [left]

	IF (SS_W .GT. 0) VLINE = SS_W,	'W= ZX.XXX;' [LEFT]	;SS

	VL2 = VLINE
	WRITES (CHNWRK, V_LINE2)
;;;	WRITES (chnwrk, VLINE)

; 5-2-17
;;;	VLINE = 'L= 80;'
	LBC = BC			;12-28-09
	IF (BD .GT. BC) LBC = BD	;12-28-09

	VLINE = LBC, 'L= ZX' [LEFT]
	IF (SS_L .GT. 0) VLINE = SS_L,	'L= ZX.XXX;' [LEFT]	;SS

	VL2 = VLINE
	WRITES (CHNWRK, V_LINE2)
;;;	WRITES (chnwrk, VLINE)

;-

	VLINE = 'PRCT='
	VLINE(7,11) = VMAT	
	LN = %TRIM(VLINE) + 1
	VLINE(LN,LN) = ';'

	VL2 = VLINE
	WRITES (CHNWRK, V_LINE2)
;;;	writes (chnwrk, vline)
;-

;;;	USING SDL_TYP SELECT
;;;	('SC','ST'),	WRITES (chnwrk, '//FLOOR= "";')
;;;	ENDUSING

	WRITES (chnwrk, '//END;')

	clear vline
	writes (chnwrk, vline)

	call get_code



;--------------------------------------------------------------

TAPS,

; Taps...
	
;;; 10-27-10: now already computed above...
;;;	WR(1) = ((BC * 1000) + WD4(3))	;1st Tap...
;;;	WR(2) = ((BD * 1000) + WD4(4))	;2nd Tap...

	AR(1) = 0	;9-18-07
;;;	AR(2) = 180	;9-18-07


	USING CODE SELECT	;round boot taps, conical taps
	('STC'),	BEGIN
			call set_cross_ar

			FOR J FROM 1 THRU 2	CALL STC
			IF (MA.GT.MB)		CALL CONR
			END


	('CC9'),	BEGIN
			call set_cross_ar

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
		;;;	ar(j) = 180000
			call set_tee_ar
			CALL STC
			IF (MA.GT.MB) 		CALL CONR
			END

	('C4'),		BEGIN
			call set_cross_ar

			FOR J FROM 1 THRU 2	CALL CC4
			IF (MA.GT.MB) 		CALL CONR
			END

	('C9'),		BEGIN	
			call set_cross_ar

			FOR J FROM 1 THRU 2	CALL C9
			IF (MA.GT.MB) 		CALL CONR

			END

	('T4'),		BEGIN
			J = 1			;ONLY 1 TAP
			CALL CC4
			IF (MA.GT.MB) 		CALL CONR
			END

	('T9'),		BEGIN	
			call set_tee_ar
			J = 1			;ONLY 1 TAP
			CALL C9
			IF (MA.GT.MB) 		CALL CONR

			END

	('CT9'),	BEGIN	
			J = 1			;ONLY 1 TAP
			call set_tee_ar
		;;;	ar(j) = 180000
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

set_tee_ar,	;;;;;;;;;;;;;;;;;;;;;;;
;	for T9 & T9R if body is:
;	   4-14 then AR=180
;	  16-24 then AR=0

	using ga select	;12-08-14
	(22 thru 26),	using MA select
			(.le. 14),	ar(1) = 180000
			(),		ar(1) = 0
			endusing
	endusing

	if (spc_mat) ar(1) = 0	;always zero for these materials

	return
;----------------------------------------------

set_cross_ar,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; 12-08-14 ssq: new code for setting value of AR:
; for ga 16-20:
;	crosses tap1 AR=0, tap2 AR=180
;	if not a cross, AR=0
;
; for ga 22-26:
;	crosses tap1 AR=90, tap2 AR=270
;	
;	anything else (not a cross or t9 or t9r) AR=0

	if (ga .ge. 22)
	then begin
	     ar(1) =  90000
	     ar(2) = 270000
	     end
	else begin
	     ar(1) =  0
	     ar(2) = 180000
	     end

	if (spc_mat)
		begin
		ar(1) = 0	;alway zero for these mat.
		ar(2) = 180000	;alway zero for these mat.
		end


	return
;---------------------------------------------

get_v1_table,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TBLCOD = 'V1'
;;;	V1_F3 = LF3
	V1_F3 = x_LF3
;;;	v1_f3 = %sskey(x_lf3)	;change SS to LE
;;;	read (chn182, coptbl, tbl_key)
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

	L5 = (BC*1414) + XSUM	; value for EZ Flange Tap

	USING LF3 SELECT
	(4),	LT = 3000		;SPIRAL END
	(2),	LT = 3000		;LE/LE
	(),	LT = 7000		;ANYTHING ELSE
	ENDUSING

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

;;;	call check_ss		;init vars, check for ss
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

;;;	call check_ss		;init vars, check for ss
	RETURN
;----------------------------------------
CC4,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Round Lateral Tap
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRITES (chnwrk, '//Round Lateral Tap')
	WRITES (chnwrk, 'FN = 51-i;')

	brt = 'T'
	vxkey = vdkey
	CALL COM_SEC

	if (ss_wr(j) .gt. 0) wr(j) = ss_wr(j)

 	WL = WR(J) 	;9-18-07
	if (ss_wl(j) .gt. 0) wl = ss_wl(j)

	VLINE = WR(J),	'WR= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)
	
	lt = 4000				;10-27-10
;;; 8-28-19>	if (mb.lt.ma .and. ga.le.20) lt = lt + 2500		;8-12-19 per brandon

	if (f3ks(3).eq.3 .and. code.eq.'T4') lt = 7000	;EZ 11-3-10
	if (f3ks(3).eq.3 .and. code.eq.'C4') lt = 7000	;1-2-19
	if (dampr .and. v_key.eq.9) lt = wr(j)

	if (ss_lt(j) .gt. 0) lt = ss_lt(j)
	VLINE = LT,	'LT= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)
	
	CALL FINISH_CROSS

	RETURN
;-----------------------------------------------------

C9,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Round Straight Tap
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRITES (chnwrk, '//Round Straight Tap')
; 3-7-17 ssq; change LT calc depending on presence of damper...
	using j select		;3-20-17
	(1),	if (d_br1) writes (chnwrk, '*damper')
	(2),	if (d_br2) writes (chnwrk, '*damper')
	endusing

	WRITES (chnwrk, 'FN = 51-i;')
	brt = 'T'

	if (ss_wr(j) .gt. 0) wr(j) = ss_wr(j)

	WL = WR(J) 	;9-18-07
	if (ss_wl(j) .gt. 0) wl = ss_wl(j)

	vxkey = vdkey
	CALL COM_SEC

	VLINE = WR(J),	'WR= ZZ.XXX;' [left]		;.XXX

	WRITES (chnwrk, VLINE)

; 3-7-17 ssq; change LT calc depending on presence of damper...
	using j select		;3-20-17
	(1),	if (d_br1) goto damper_calc
	(2),	if (d_br2) goto damper_calc
	endusing

;;;	if (dampr) goto damper_calc


	lt = 4000	;10-27-10

	if (f3ks(3).eq.3 .and. code.eq.'T9') lt = 3000	;EZ 11-3-10

;;;3-7-17>>	if (dampr .and. v_key.eq.9) lt = wr(j)#3 * 1000	;8-30-07

	if (ss_lt(j) .gt. 0) lt = ss_lt(j)
	goto done_c9

damper_calc,
	using f3ks(j+2) select	;add depending on end treatment
	(1,3),	L_ADD = 4000
	(2,4),	L_ADD = 6000
	endusing

	using j select
	(1),	lt = (bc*10)#1/2 *1000 +L_ADD
	(2),	lt = (bd*10)#1/2 *1000 +L_ADD
	endusing

done_c9,
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
	brt = 'T'
	vxkey = vdkey
	CALL COM_SEC

;;;	WL = WR(J) + 1000
	WL = WR(J) + 500				;12-17-14
	if (ss_wl(j) .gt. 0) wl = ss_wl(j) 
	

;--------------------------------------------------
;7-17-19 ssq now a sub to be used for both "L" and "WL"
;;;	dif = ma - bx
;;;	using bx select
;;;	(.le. 4),	if (dif.ge.2)	wl = wl + 500
;;;	(5 thru 10),	if (dif.ge.3)	wl = wl + 500
;;;	(11 thru 16),	if (dif.ge.4)	wl = wl + 500
;;;	(17 thru 28),	if (dif.ge.6)	wl = wl + 500
;;;	(29 thru 38),	if (dif.ge.8)	wl = wl + 500
;;;	(39 thru 40),	if (dif.ge.10)	wl = wl + 500
;;;	endusing
;--------------------------------------------------
	CALL BX_ADD
	WL = WL + BX_ADD				;7-17-19


	lt = 3375		;10-27-10

;;;	if (ss_wl(j) .gt. 0) wl = ss_wl(j)
	VLINE = WL,	'WL= ZZ.XXX;' [left]		;.XXX
	
	WRITES (chnwrk, VLINE)

	if (ss_wr(j) .gt. 0) wr(j) = ss_wr(j)
	VLINE = WR(J),	'WR= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	SET SR(1), SR(2) = 0
	IF (SS_SR(J) .GT. 0) SR = SS_SR(J)
	VLINE = SR,	'SR= ZZ.XXX;' [left]
	WRITES (CHNWRK, VLINE)
;;;	WRITES (chnwrk, 'SR= 0;')

	using matl select
	(2,4,6),	WRITES (CHNWRK, 'S1= 3.437')		;12-7-09
	endusing

	if (ss_lt(j) .gt. 0) lt = ss_lt(j)
	VLINE = LT,	'LT= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	CALL FINISH_CROSS


	RETURN
;-----------------------------------------------------

BX_ADD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; USED FOR "L=" AND "WL="
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	dif = ma - bx
	using bx select
	(.le. 4),	if (dif.ge.2)	BX_ADD = 500
	(5 thru 10),	if (dif.ge.3)	BX_ADD = 500
	(11 thru 16),	if (dif.ge.4)	BX_ADD = 500
	(17 thru 28),	if (dif.ge.6)	BX_ADD = 500
	(29 thru 38),	if (dif.ge.8)	BX_ADD = 500
	(39 thru 40),	if (dif.ge.10)	BX_ADD = 500
	endusing

	RETURN
;----------------------------------------------------


CONR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Round Concentric Reducer
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (is_pressed) return		;8-21-07

	WRITES (chnwrk, '//Round Concentric Reducer')
	WRITES (chnwrk, 'FN = 315-i;')
	brt = 'R'

	SZB = MA
	SZT = MB

	IF (GA.LE.20 .AND. SZT.GE.25) EZ_R = 0		;4/13/15 BK FIX DIRECT WELD LATERAL REDUCERS
	
	using ga select	;7-13-09
	(16 thru 20),	vxkey = 3	;welded reducers 4-27-09
	(),		begin
			vxkey = 4
			if (szb .ge. 18) vxkey = 2	;gorelock 7-13-09
			if (szt .lt. 8) vxkey = 4	;elbowlock 7-13-09
			end
	endusing

	CALL COM_SEC


	CLEAR TBLKEY
	TBLCOD = 'V1'

	USING GA SELECT

	(16 thru 20),		V1_KEY = 3	;7-13-09

	(22 THRU 26),		BEGIN
				V1_KEY = 4
				IF (SZB .GE. 18) V1_KEY = 2
				IF (SZT .LT. 8) V1_KEY = 4
				END
	ENDUSING

;-
	CLEAR TBLKEY
	V1_KEY = V_KEY
;-

	x_lf3 = lf3
	call get_v1_table

	WT = (SZT * 1000) 		;.XXX
	IF (.not. EZ_R) WT = WT + WD4(2)		;.XXX
;;;	WT = ((SZT * 1000) + WD4(2))		;.XXX

	T_EZJR = 0
	IF (SZT.GT.5 .AND. SZT.LT.24) T_EZJR = 1


	if (v1_key.eq.2 .or. v1_key.eq.4) 
	begin
	 using lf3 select
	 (4),	x_lf3 = 2	;change spiral to LE
	 (2),	x_lf3 = 4	;change LE to spiral
	 endusing
	end

	CLEAR TBLKEY
	V1_KEY = V_KEY
	call get_v1_table


	SB = 		;always zero for reducers
	SR = 		;always zero for reducers


;-------------------------------------------------------------------
; 8-23-10 this is always true: reducer.wb = body.w
;;;	WB = (SZB * 1000) + wd4(1)		;.XXX
;;;	if (ga.ge.16 .and. ga.le.20) WB = W
;;;	if (ss_wb .gt. 0) wb = ss_wb

	wb = w				;8-23-10: reducer.wb = body.w
;-------------------------------------------------------------------

	VLINE = WB,	'WB= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	IF (SS_SB .GT. 0) SB = SS_SB
	VLINE = 'SB= 0;'
	WRITES (chnwrk, VLINE)

	if (ss_wt .gt. 0) wt = ss_wt
	VLINE = WT,	'WT= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	ST = 0
	IF (SS_ST .GT. 0) ST = SS_ST
	VLINE = ST,	'ST= ZZ.XXX;' [left]
;;;	VLINE = 'ST= 0;'
	WRITES (chnwrk, VLINE)

	ladd = 0					;4-25-16

;;;	using matl select
;;;	(2,4,6),	begin		;alum & sst g90 only...
;;;		VLINE = 'S1= 3.437'			;12-7-09
;;;		WRITES (CHNWRK, VLINE)
;;;		VLINE = 'S2= 3.437'			;12-7-09
;;;		WRITES (CHNWRK, VLINE)
;;;
;;;		ladd = 3625
;;;		end
;;;
;;;	(),	ladd = 0
;;;	endusing

;Total Reduction/Body Length:

	dif = szb - szt
	USING GA SELECT			;add 3.625 to constants below
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
	ENDUSING
		
	L = L*1000 + LADD			;12-16-09

	IF (SS_RL .GT. 0) L = SS_RL
	VLINE = L,	'L= ZX.XXX;' [left]
	WRITES (chnwrk, VLINE)


	DIF = (WB - WT)		;don't round

	OW = (DIF*10/2)#1
;;;	ow = xow		;10-14-10 not here!

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

; bottom...
	VLINE = 'CONN= B;CN= "'
	LN = %TRIM (VLINE)
	INCR LN
	VLINE(LN,60) = S2_CN2	;reducer.bottom = body.top -always!

;;;	IF (SS_RCONB .NE. A15)
;;;	THEN	VLINE(LN,60) = SS_RCONB
;;;	ELSE	VLINE(LN,60) = 'LARGE END'

	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)


; top...
	CLEAR TBLKEY
	TBLCOD = 'V2'		;vulcan descriptions
	V2_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	S2_CN2 = V2_CN2
	if (t_ezjr .and. S2_CN2.EQ.'EZ FLANGE') 
		begin
		S2_CN2 = 'JR EZ FLANGE'
		is_ezjr = 1
		end

	VLINE = 'CONN= T;CN= "'
	LN = %TRIM (VLINE)
	INCR LN


	IF (SS_RCONT .NE. A15)
	THEN	VLINE(LN,60) = SS_RCONT
	ELSE	VLINE(LN,60) = S2_CN2

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
	brt = 'T'
	vxkey = vdkey
	CALL COM_SEC

	DL = WR(J) ;;;>+ 3000	;8-7-07 added 3000
	if (ss_dl(j) .gt. 0) dl = ss_dl(j)

	WL = WR(J) + 3000
	if (ss_wl(j) .gt. 0) wl = ss_wl(j)

	VLINE = WL,	'WL= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	using code select
	('STT','STC','ST9'),	BEGIN
				VLINE = DL,	'DL= ZZ.XXX;' [left]		;.XXX
				WRITES (chnwrk, VLINE)
				END
	endusing

	if (ss_wr(j) .gt. 0) wr(j) = ss_wr(j)
	VLINE = WR(J),	'WR= ZZ.XXX;' [left]		;.XXX
	WRITES (chnwrk, VLINE)

	SET SR(1), SR(2) = 0
	IF (SS_SR(J) .GT. 0) SR = SS_SR(J)
	VLINE = SR,	'SR= ZZ.XXX;' [left]
	WRITES (CHNWRK, VLINE)
;;;	WRITES (chnwrk, 'SR= 0;')
	using matl select
	(2,4,6),	begin
		WRITES (CHNWRK, 'S1= 3.437')			;12-7-09
	;;;	WRITES (CHNWRK, 'LT= 7.625')			;12-7-09
		lt = 7625
		end
	(),	begin
	;;;	WRITES (chnwrk, 'LT= 4;')
		lt =4000
		end
		
	endusing

	if (ss_lt(j) .gt. 0) lt = ss_lt(j)
	vline = lt,	'LT= ZX.XXX;' [Left]
	writes (chnwrk, vline)

	CALL FINISH_CROSS

	RETURN
;-----------------------------------------------------

FINISH_CROSS,	;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------
	T_EZJR = 0
	D6 = WR#3
	IF (D6.GT.5 .AND. D6.LT.24) T_EZJR = 1

	IF (AN.EQ.'4' .AND. is_EZJR) LL = L		;8-18-10 see above w/ same date

;10-14-10:
;-----------------------------------------------------------
;;;	IF (AN.EQ.'4' .AND. is_EZJR)
;;;	THEN	OW = 6000
;;;	ELSE	BEGIN
;;;		LL = L		;8-18-10 see above w/ same date
;;;		if (an .eq. '4')
;;;		then	ow = ((L5*1000 - (wr*1414))/2)#3
;;;		else	OW = ((LL-WL)*10/2)#1
;;;		END
;-----------------------------------------------------------
;ow was set from p90,p45,pred arrays above...

;;;	OW = XOW
	if (wr(1) .gt. wr(2))
	then	big_wr = wr(1)
	else	big_wr = wr(2)

	if (wr(j) .eq. big_wr)
	then	ow = xow		;larger tap
	else	begin			;center smaller tap
;;		dif = (l - wl)		
		dif = %abs(wr(1) - wr(2))
		ow = (dif*10/2)#1 + xadd
		end
		

; if this works, remove all of the ow calcs above
;---------------------------------------------------



	VLINE = OW,	'OW= ZX.XXX;' [left]
	WRITES (chnwrk, VLINE)

	OD = 0
	IF (SS_OD .GT. 0) OD = SS_OD
	VLINE = OD,	'OD= ZX.XXX;' [left]
	WRITES (CHNWRK, VLINE)

	IF (SS_AT(J) .GT.0) AT = SS_AT(J)#3
	VLINE = AT,	'AT= ZX;' [left]
	WRITES (chnwrk, VLINE)

	IF (SS_AR(J) .GT. 0) AR(J) = SS_AR(J)
;;;	VLINE = AR(J),	'AR= ZZX;' [left]
	VLINE = AR(J)#3,	'AR= ZZX;' [left]
	WRITES (chnwrk, VLINE)


	VLINE = 'PRCT='
	VLINE(7,11) = VMAT	
	LN = %TRIM(VLINE) + 1
	VLINE(LN,LN) = ';'

	writes (chnwrk, vline)

	WRITES (chnwrk, 'FLOOR= ;')

	if (ss_tcont(j) .ne. a15) s2_cn3 = ss_tcont(j)


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
		GS = V3_GSEAM
		LS = V3_LSEAM
		SS = V3_SSEAM
		END
	ELSE	CLEAR GS, LS, SS

;;; 12-10-14 ssq: dim .le. 3 overrides tables...
; use ss seam variables...
;;; 2-13-17	CHANGED 20GA OR HEAVIER TO BE BUTTWELD SEAM

	if(GA .le. 20)
	then	begin
		ss_bls = 'BUTTWELD'
		ss_bss = 'BUTTWELD'
		ss_rls = 'BUTTWELD'
		ss_rss = 'BUTTWELD'
		ss_tls(1) = 'BUTTWELD'
		ss_tss(1) = 'BUTTWELD'
		ss_tls(2) = 'BUTTWELD'
		ss_tss(2) = 'BUTTWELD'
		end

	else	using brt select
		('B'),	if (ma .le. 3)
			begin
			ss_bls = '.5 LAP'
			ss_bss = '.5 LAP'
			end
		('R'),	if (mb .le. 3)
			begin
			ss_rls = '.5 LAP'
			ss_rss = '.5 LAP'
			end
		('T'),	using j select
			(1),	if (bc .le. 3)
			begin
			ss_tls(1) = '.5 LAP'
			ss_tss(1) = '.5 LAP'
			end
			(2),	if (bd .le. 3)
			begin
			ss_tls(2) = '.5 LAP'
			ss_tss(2) = '.5 LAP'
			end
			endusing
		endusing

;;; 12-10-14 ssq: dim .le. 3 overrides tables...

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

;8-23-10: this overrides everything...
;12-17-14: material overrides everything...
	if (spc_mat)
		begin
		LS = '.5 LAP'
		SS = '.5 LAP'
		end


	GSEAM = 'GSEAM= ' + '"' + GS(1,%trim(GS)) + '";'
	LSEAM = 'LSEAM= ' + '"' + LS(1,%trim(LS)) + '";'
	SSEAM = 'SSEAM= ' + '"' + SS(1,%trim(SS)) + '";'

	return
;-------------------------------------------------------


PRESSED_REDUCER,	;;;;;;;;;;;;;;;;;

	is_pressed = 0
	if (vinyl) return	;vinyl not pressed...
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

; key seq corresponds to 1-MA, 2-MB, 3-BC, 4-BD

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

	clear ss_rls, ss_rgs, ss_rss, ss_rcont, ss_rconb
	clear ss_bls, ss_bgs, ss_bss, ss_bcont, ss_bconb
	clear ss_tls, ss_tgs, ss_tss

	for j from 1 thru 2
		begin
		clear ss_tcont(j)
		clear ss_tconb(j)

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
	IF (IS_R)	T_KEY = CODE(1,LN) + 'R'

	using ga select
	(22 thru 26),	T_GA = 1
	(16 thru 20),	T_GA = 2
	endusing

;;;	j = 1			;for f3 note segs (f3ks) check...
;;;	if (f3ks(j) .ne. 1) goto skip_body

	CLEAR KEY17
	CLEAR TBLKEY
	TBLCOD = 'S2'		;safety seal parameters
	S2_KEY = T_KEY
	S2_MAT = 'G'		;for now...
	S2_GA = T_GA

	S2_TYPE = 'B'		;Body
	S2_DIM = MA
	S2_R = 1		;seams
	KEY17 = TBL_KEY		;SAVE THE KEY

; body, bottom...
	j = 1			;for f3 note segs (f3ks) check...
	if (f3ks(j) .ne. 1) goto skip_body

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
; 2 - body top

;;;	incr j		;no, same as body
	incr j		;yes, this is body, top

	if (f3ks(j) .ne. 1) goto skip_red1
	IF (MB .GE. MA) goto skip_red		;no reducer

	TBL_KEY = KEY17
	S2_TYPE = 'R'		;REDUCER
	S2_DIM = MA
	S2_R = 1		;SEAMS
	KEY17 = TBL_KEY		;SAVE THE KEY
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_RLS = S2_LSEAM
	SS_RGS = S2_GSEAM
	SS_RSS = S2_SSEAM
	SS_RCONB = S2_CONB

	TBL_KEY = KEY17 
	S2_R = 2		;values
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_WB = S2_WB	;ZZX.XXX (WB, W, WL)
	SS_SB = S2_SB

skip_red1,
; body top
;;;	incr j		
	if (f3ks(j) .ne. 1) goto skip_red
	TBL_KEY = KEY17
	S2_R = 1		;seams
	S2_TYPE = 'R'		;REDUCER
	S2_DIM = MB
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL
	SS_RCONT = S2_CONT


	TBL_KEY = KEY17
	S2_R = 2		;values
	S2_TYPE = 'R'		;REDUCER
	S2_DIM = MB
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_WT = S2_WT	;ZZX.XXX (WT, L, DL)
	SS_ST = S2_ST


skip_red,
; 3 - tap 1
	incr j
	if (f3ks(j) .ne. 1) goto skip_tap1

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
	IF (BD .LE. 0) return	;2nd Tap?

; 4 - tap 2

	incr j
	if (f3ks(j) .ne. 1) goto skip_tap2

	TBL_KEY = KEY17
	S2_TYPE = 'T'		;TAP
	S2_DIM = BD
	S2_R = 1		;SEAMS

	KEY17 = TBL_KEY		;SAVE THE KEY
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_TLS(2) = S2_LSEAM
	SS_TGS(2) = S2_GSEAM
	SS_TSS(2) = S2_SSEAM
	SS_TCONB(2) = S2_CONB
	SS_TCONT(2) = S2_CONT

	TBL_KEY= KEY17
	S2_R = 2		;VALUES
	
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	SS_WL(2) = S2_WL	;ZZX.XXX (WB, W, WL)
	SS_DL(2) = S2_DL	;ZZX.XXX (WT, L, DL)
	SS_WR(2) = S2_WR	;ZZX.XXX (WR)
	SS_SR(2) = S2_SR	;ZZX.XXX (WR)
	SS_LT(2) = S2_LT	;ZZX.XXX (LT)
	SS_OW(2) = S2_OW	;ZZX.XXX (LT)
	SS_OD(2) = S2_OD	;ZZX.XXX (LT)
	SS_AT(2) = S2_AT	;ZZX.XXX (LT)
	SS_AR(2) = S2_AR	;ZZX.XXX (LT)
	SS_LB(2) = S2_LB	;ZZX.XXX (LB)
	if (.not. all_ss) clear ss_ow(2)

skip_tap2,

	return
;-------------------------------------------------------

	END

		
