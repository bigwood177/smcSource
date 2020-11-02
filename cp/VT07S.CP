SUBROUTINE VT07S
	PNUM	,D
	LINE	,A	;ORDER L/I
	CHN182	,D	;CHANNEL FOR COPTBL
	MEMDAT	,A	;CPMEMO
	ERR	,D	;1 => ERROR DURING CONVERSION

; Routine for End Caps
;
; ECS 10 24 S	;9-16-09
; EC 10 24 S
; AA SZ GA
;    |  |_gauge
;    |_______size
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
	SB	,D2
	SR	,D2
	R	,D2
	A	,D2
	NG	,D2
	GSEAM	,A30
	LSEAM	,A30
	SSEAM	,A30


RECORD	V_LINE1
	VLINE	,A80

RECORD	ARAY
	WD4	,4D4

RECORD	VARS
	spc_mat	,d1	;special material for seams
	XITEM	,A15
	vinyl	,d1
	xga	,d2
	matl	,d1	;f1 matl
	vmat	,a5	;f1 mm_vmat
	chnwrk	,d2
	CNT	,D3
	J	,D6
	LF3x	,D5		;THIS IS A FIELD IN RD045A.NEW...
	CODE	,A2
	SZ	,D2
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

	using lf1 select		;1-7-15 ssq
	(54,55,59,62,63,66,69), spc_mat = 1
	(),			spc_mat = 0
	endusing

	CPMEMO = MEMDAT

	clear vinyl
	if (litmno(3,3) .eq. 'V')
	then	begin
		vinyl = 1
		litmno(2,15) = litmno(3,15)
		end
	else	CODE = LITMNO(1,2)

; EC 10 24 S
; AA SZ GA
	XITEM = LITMNO
	IF (XITEM(3,3).LT.'0' .OR. XITEM(3,3).GT.'9') XITEM(3,14) = LITMNO(4,14)
	XCALL ALPDC (XITEM(3,4), SZ, BF)
;;;	XCALL ALPDC (LITMNO(3,4), SZ, BF)
	IF (BF) XRETURN

;;;	IF (SZ.GE.10 .AND. SZ.LE.18) XRETURN	;DON'T EXPORT THESE SIZES

	XCALL ALPDC (XITEM(5,6), GA, BF)
;;;	XCALL ALPDC (LITMNO(5,6), GA, BF)
	IF (BF) XRETURN

	if (ga .eq. 26) ga = 24		;6-19-08

	VMAT = 'GALV'			;default value
	if (vinyl) vmat = 'PVS'		

	clear matl

	CLEAR COPTBL
	TBLCOD = 'M1'
	MM_KEY = LF1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO SKIP_F1_MATL
	IF (MM_VULC .EQ. 2) XRETURN	;6-24-08
	MATL = MM_MATL
	VMAT = MM_VMAT

SKIP_F1_MATL,
;-
	using lf3 select
	(2),		begin
			gseam = 'GSEAM= "1/4" Elbow LK.";'
			sseam = 'SSEAM= ".5"Riv.";'
			lseam = 'LSEAM= "1/2" Rivet";'
			using sz select
			(0 thru 5),	wd4(1) = -437
			(6 thru 13),	wd4(1) = -125
			(14 thru 48),	begin					
					wd4(1) = 	;10-24-18 remove -125
					sseam = 'SSEAM= ".25 STITCH";'
					lseam = 'LSEAM= ".25 STITCH";'
					gseam = 'GSEAM= "1/4" Gore Lock";'
					end
			endusing
			end
	(3,9),	begin
			gseam = 'GSEAM= "Butt Weld";'
			sseam = 'SSEAM= ".5"Riv.";'
			lseam = 'LSEAM= "None";'
			using sz select
			(0 thru 24),	wd4(1) = 1250	;12-15-08
			(.gt.24),	wd4(1) = 2750
			endusing
			end
		
	(),		begin
		;;;	gseam = 'SSEAM= "1/4" Elbow L.";'
			gseam = 'GSEAM= "1/4" Elbow LK.";'
			sseam = 'SSEAM= ".5"Riv.";'
			lseam = 'LSEAM= "1/2" Rivet";'
			using sz select
			(0 thru 7),	wd4(1) = -375
			(8 thru 48),	begin
					wd4(1) = -125
					gseam = 'GSEAM= "1/4" Gore Lock";'
					end
			endusing
			end
	endusing

;;;	if (sz .le. 3)
	if (sz.le.3 .or. spc_mat.eq.1)
		begin
		LSEAM = 'LSEAM= .5 LAP'
		SSEAM = 'SSEAM= .5 LAP'
		end

;-

	using ga select
	(16 thru 20),	begin
			chnwrk = chnw2
			is_2 = 1
			end
	(22 thru 26),	begin
			chnwrk = chnw3
			is_3 = 1
			end
	(),		begin		;9-29-10 alum (or anything else...)
			chnwrk = chnw3
			is_3 = 1
			end
	endusing


; end treatment...
	CLEAR COPTBL
	TBLCOD = 'M3'
	MM_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0) GOTO GOT_TBL

; if no/bad end treatment, check for default keys...
	CLEAR COPTBL
	TBLCOD = 'IK'
	IK_ITEM = LITMNO
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL
	lf3 = ik_f3		;default key...
GOT_TBL,
;
; Create the output file...
;


	VLINE = '// ' + 'vt07s: ' + LCFGIM
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

	WRITES (chnwrk, '//End Cap')
	
	WRITES (chnwrk, 'FN = 351-i;')
	
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	VLINE = 'METALN= "'
	VLINE (10,13) = VMAT
	LN = %TRIM(VLINE) + 1
	VLINE (LN,80) = ' SPIRAL FITT";'
	WRITES (chnwrk, VLINE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;;	WRITES (chnwrk, 'LSEAM= "None";')


;;;	GSEAM = 'GSEAM= ;'
	VLINE = GSEAM
	WRITES (chnwrk, vline)

	VLINE = SSEAM
	WRITES (chnwrk, vline)

	CLEAR TBLKEY
	TBLCOD = 'V1'
;;;	V1_KEY = 1		; gorelock elbows and angles
	V1_KEY = 7		; end caps
	V1_F3 = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;	FOR J FROM 1 THRU 4
;;;		USING GA SELECT
;;;		(16),		WD4(J) = V1_16(J)
;;;		(18 THRU 20),	WD4(J) = V1_1820(J)
;;;		(22 THRU 26),	WD4(J) = V1_2226(J)
;;;		(),		WD4(J) =
;;;		ENDUSING
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	VLINE = ((SZ * 1000) + WD4(1)), 'W= ZZ.XXX;' [left]		;.XXX

;10-28-20: special case for end caps

	USING LITMNO SELECT
	('EC0326S','EC0426S','EC0424S'), IF(LF3.EQ.4) VLINE = ((SZ * 10000) + 625), 'W= ZZ.XXXX;' [left]		;.XXX	
	('EC0326L','EC0426L','EC0424L'), IF(LF3.EQ.2) VLINE = ((SZ * 1000) + 125), 'W= ZZ.XXX;' [left]		;.XXX	
	ENDUSING


	WRITES (chnwrk, VLINE)

;;;	WRITES (chnwrk, 'L= 2;')
	WRITES (chnwrk, 'L= 0;')	;11-18-08
	
;;;	USING MATL SELECT
;;;	(2),	WRITES (chnwrk, 'PRCT= ALUMINUM;')
;;;	(3),	WRITES (chnwrk, 'PRCT= PAINT GRIP;')
;;;	(4),	WRITES (chnwrk, 'PRCT= STAINLESS;')
;;;	(),	WRITES (chnwrk, 'PRCT= GALVANIZED;')
;;;	ENDUSING

	vline = 'PRCT= '
	VLINE(7,80) = VMAT
	ln = %trim(vline) + 1
	vline (ln,ln) = ';'
	writes (chnwrk, vline)


	WRITES (chnwrk, 'FLOOR= "";')

	CLEAR TBLKEY
	TBLCOD = 'V2'		;vulcan descriptions
	V2_KEY = LF3
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR COPTBL

	VLINE = 'CONN= B;CN= "'
	LN = %TRIM (VLINE)
	INCR LN
	VLINE(LN,60) = V2_CN1
	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)


	WRITES (chnwrk, 'END;')

	CLEAR VLINE
	WRITES (chnwrk, VLINE)

	clear err
	XRETURN
	END

		
