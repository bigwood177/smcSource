SUBROUTINE VTRTS	;register take-offs
	PNUM	,D
	LINE	,A	;ORDER L/I
	CHN182	,D	;CHANNEL FOR COPTBL
	MEMDAT	,A	;CPMEMO
	ERR	,D	;1 => ERROR DURING CONVERSION

; Routine for Register Take-offs
;
; RT 12 10 18
; AA S1 S2 len
;    |  |  |_len
;    |  |____size 2
;    |_______size 1
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
	WL	,D2
	DL	,D2
	WR	,D2
	DR	,D2
	GSEAM	,A30
	LSEAM	,A30
	SSEAM	,A30

RECORD	V_LINE1
	VLINE	,A80

RECORD	ARAY
	WD4	,4D4

RECORD	VARS
	xga	,d2
	matl	,d1
	vmat	,a5	;f1 mm_vmat
	chnwrk	,d2
	KITMNO	,A15
	CNT	,D3
	J	,D6
	LF3x	,D5		;THIS IS A FIELD IN RD045A.NEW...
	CODE	,A2
	SZ1	,D2
	SZ2	,D2
	SZ3	,D2
	LEN	,D2
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
	CODE = KITMNO(1,2)

	XCALL ALPDC (KITMNO(3,4), SZ1, BF)
	IF (BF) XRETURN

	XCALL ALPDC (KITMNO(5,6), SZ2, BF)
	IF (BF) XRETURN

	XCALL ALPDC (KITMNO(7,8), SZ3, BF)
	IF (BF) XRETURN

	LEN = SZ1
	IF (SZ2 .GT. LEN) LEN = SZ2	
	USING LEN SELECT
	(.GE. 31),	GA = 20
	(13 THRU 30),	GA = 24
	(1 THRU 12),	GA = 26
	ENDUSING

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

	if (ga .eq. 26) ga = 24		;6-19-08

	vmat = 'GALV'
;;;	clear matl
	matl = 1
	
	CLEAR COPTBL
	TBLCOD = 'M1'
	MM_KEY = LF1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO SKIP_F1_MATL
	IF (MM_VULC .EQ. 2) XRETURN	;6-24-08
	MATL = MM_MATL
	VMAT = MM_VMAT

SKIP_F1_MATL,
	CLEAR COPTBL
	TBLCOD = 'V3'
	V3_KEY = vdkey
	V3_GA = GA
	using matl select	;for now default to galv unless alum...
	(2,5),	V3_MAT = MATL	;1-8-15 -added "5"
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


	VLINE = '// ' + 'vtrts: ' + LCFGIM
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

	WRITES (chnwrk, '//Saddle Body')
	WRITES (chnwrk, 'FN = 1200-i')

	
	PNUM = PNUM + 1
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	VLINE = 'METALN= "'
	VLINE (10,13) = VMAT
	LN = %TRIM(VLINE) + 1
	VLINE (LN,80) = ' SPIRAL FITT";'
	WRITES (chnwrk, VLINE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

	WRITES (chnwrk, 'LSEAM= ".5 LAP";')
	WRITES (chnwrk, 'GSEAM= "1/4" elbowlock";')
	WRITES (chnwrk, 'SSEAM= ".5 LAP";')


	VLINE = SZ3,	'W= ZX;' [LEFT]
	WRITES (chnwrk, VLINE)
	
	WRITES (chnwrk, 'L= 80;')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	VLINE = 'PRCT= '
	VLINE (7,11) = VMAT
	LN = %TRIM(VLINE) + 1
	VLINE (LN,80) = ' SPIRAL FITT";'
	WRITES (chnwrk, VLINE)

;;;	USING MATL SELECT
;;;	(2),	WRITES (chnwrk, 'PRCT= "ALUM SPIRAL FITT";')
;;;	(3),	WRITES (chnwrk, 'PRCT= "PAINT GRIP SPIRAL FITT";')
;;;	(4),	WRITES (chnwrk, 'PRCT= "STAINLESS SPIRAL FITT";')
;;;	(),	WRITES (chnwrk, 'PRCT= "GALV SPIRAL FITT";')
;;;	ENDUSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	WRITES (chnwrk, 'END;')

	CLEAR VLINE
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, '//Rectangular Tap')
	WRITES (chnwrk, 'FN= 64-i')

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	VLINE = 'METALN= "'
	VLINE (10,13) = VMAT
	LN = %TRIM(VLINE) + 1
	VLINE (LN,80) = ' SPIRAL FITT";'
	WRITES (chnwrk, VLINE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	VLINE = 'GAUGE= '
	VLINE(8,10) = GA,	'XX;' 
	WRITES (chnwrk, VLINE)

;;;	VLINE = LSEAM
;;;	WRITES (CHNWRK, VLINE)

	WRITES (chnwrk, 'GSEAM= "SM Pittsburg";')
	WRITES (chnwrk, 'SSEAM= ".5 Lap";')

;;;	VLINE = GSEAM
;;;	WRITES (chnwrk, vline)

;;;	VLINE = SSEAM
;;;	WRITES (chnwrk, vline)


	WL = SZ1 + 2
	DL = SZ2 + 2
	WR = SZ1 + 2
	DR = SZ2 + 2
		
	VLINE = WL, 'WL= ZX;' [left]
	WRITES (chnwrk, VLINE)

	VLINE = DL, 'DL= ZX;' [left]
	WRITES (chnwrk, VLINE)

	VLINE = WR, 'WR= ZX;' [left]
	WRITES (chnwrk, VLINE)

	VLINE = DR, 'DR= ZX;' [left]
	WRITES (chnwrk, VLINE)

	WRITES (chnwrk, 'LT= 6;')
	WRITES (chnwrk, 'LB= 0;')
	WRITES (chnwrk, 'OW= 0;')
	WRITES (chnwrk, 'OW= 0;')
	WRITES (chnwrk, 'OD= 0;')
	WRITES (chnwrk, 'AT= 90;')
	WRITES (chnwrk, 'AR= 0;')

	WRITES (chnwrk, 
& 'PRCT= CATALOG SETUP Design Build S & D KD - ASMBL Mult Rfcmt Galvanized;')


	CLEAR TBLKEY
	TBLCOD = 'V2'		;vulcan descriptions
	V2_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	WRITES (chnwrk, 'CONN= L;CN= out flange 1.25"; END;')
	WRITES (chnwrk, 'CONN= R;CN= flange in 1"; END;')
	WRITES (chnwrk, 'END;')


;;;	VLINE = 'CONN= L;CN= "'
;;;	LN = %TRIM (VLINE)
;;;	INCR LN
;;;	VLINE(LN,60) = V2_CN1
;;;	LN = %TRIM(VLINE)
;;;	INCR LN
;;;	VLINE(LN,80) = '"; END;'
;;;	WRITES (chnwrk, VLINE)
;;;
;;;	VLINE = 'CONN= R;CN= "'
;;;	LN = %TRIM (VLINE)
;;;	INCR LN
;;;	VLINE(LN,60) = V2_CN2
;;;	LN = %TRIM(VLINE)
;;;	INCR LN
;;;	VLINE(LN,80) = '"; END;'
;;;	WRITES (chnwrk, VLINE)


;;;	WRITES (chnwrk, 'END;')

	CLEAR VLINE
	WRITES (chnwrk, VLINE)

	clear err
	XRETURN
	END

		

