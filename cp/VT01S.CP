
SUBROUTINE VT01S
	PNUM	,D
	LINE	,A	;ORDER L/I
	CHN182	,D	;CHANNEL FOR COPTBL
	MEMDAT	,A	;CPMEMO
	ERR	,D	;1 => ERROR DURING CONVERSION

; Routine for Gorelock Elbows and Angles
;
; 7-27-07: also other elbows
; EV 12 90 20 S
; EG 14 90 20
; GA 10 90 24
; AA SZ AN GA
;    |  |  |_gauge
;    |  |____angle
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

record	cfgmat
	.include 'def:cfgmat.def'

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
	SB	,D5
	SR	,D5
	R	,D6
	W	,D6
	WB	,D6
	WR	,D6
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
	xm3_note,	a16	;11-2-20 in case f3 switched
	is_307	,d1		;1=is a 307-i, 0= not a 307-i
	alf3	,a5
	cmpcod	,a3
;;;	spiral	,d1,	1	;f3 note for spiral ends
	spiral	,d1,	4	;f3 note for spiral ends
	XGA	,D2		;WORKING GA
	MATL	,D1		;F1 MATL 
	VMAT	,A5		;F1 MM_VMAT
	FULLSWEEP	,D1
	DEG	,D8
	ANGL	,D8
	BUTWELD	,D1
	vynl	,d1
	ezjr_b	,d1
	ezjr_r	,d1
	seam	,a20
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
	xcall who(cmpcod)

	ERR = 1			;ASSUME ERROR

	ORDLIN = LINE

;;; uncommented this 1-18-17	
	IF (LF3 .EQ. 1) XRETURN	;6-28-10 NO SAFETY SEAL!

	CPMEMO = MEMDAT

	CODE = LITMNO(1,2)

	if (%instr(1, code, 'V'))
	then	vynl = 1
	else	clear vynl

	XCALL ALPDC (LITMNO(3,4), SZ, BF)
	IF (BF) XRETURN

	xm3_note = m3_note			;11-2-20	
	using code select
	('GE'),	if (sz .gt. 36) xreturn		;6-7-16	BK
	('EG'),	using lf3 select		;11-2-20 bk
		(42),	begin
			lf3 = 24
			xm3_note = 'LE/SP - switched'
			end
		(43),	begin
			lf3 = 34
			xm3_note = 'EZ/SP - switched'
			end
		endusing
	endusing
		

	XCALL ALPDC (LITMNO(5,6), AN, BF)
	IF (BF) XRETURN

	XCALL ALPDC (LITMNO(7,8), GA, BF)
	IF (BF) XRETURN

	if (ga .eq. 26) ga = 24		;6-19-08

	VMAT = 'GALV'			;7-18-08
	MATL = 1			;GALV
	IF (VYNL) 
		begin
		matl = 5		;pvs
		VMAT = 'PVS'		;7-18-08
		end

	CLEAR COPTBL
	TBLCOD = 'M1'
	MM_KEY = LF1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO SKIP_F1_MATL
	IF (MM_VULC .EQ. 2) XRETURN	;6-24-08
	MATL = MM_MATL
	VMAT = MM_VMAT			;7-18-08

SKIP_F1_MATL,

	CLEAR COPTBL
	TBLCOD = 'V3'
	V3_KEY = vdkey
	V3_GA = GA

	using matl select	;for now default to galv unless alum...
	(2),	V3_MAT = MATL
	(5),	v3_mat = matl
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

	BUTWELD = 1
	IF (V3_GSEAM .NE. 'BUTTWELD') CLEAR BUTWELD
	IF (V3_LSEAM .NE. 'BUTTWELD') CLEAR BUTWELD
	IF (V3_SSEAM .NE. 'BUTTWELD') CLEAR BUTWELD

	SEAM= '"1/2in.Lap Spot";'

	using code select
	('EV'), if (ga .ne. 20) xreturn		;only process if 20 ga
	('EG'),	SEAM= '"Butt Weld";'
	endusing

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

	IF (LF2 .EQ. 32)	;FULL SWEEP
	THEN	FULLSWEEP = 1
	ELSE	FULLSWEEP = 0


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
GOT_TBL,
;
; Create the output file...
;



	VLINE = '// ' + 'vt01s: ' + LCFGIM
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
		vline = '// ' + 'F3: ' + xm3_note
	;;;	vline = '// ' + 'F3: ' + m3_note
		writes (chnwrk, vline)
		end

	vline = '// ' + 'pipe gauge: ' + lpipeg
	writes (chnwrk, vline)


	USING CODE SELECT
	('GE'),	WRITES (chnwrk, '//Round Elbow')
	('GA'),	WRITES (chnwrk, '//Round Angle')
	ENDUSING
	

;1-14-19 ---------------------------------------------------
; for "GE" or "GA" if F3 = 24 or 42 FN = 307-i

;;;	vline = 'FN = 304-i;'		;default value
;;;	if (matl.eq.2 .or. matl.eq.5) vline = 'FN = 303-i;'
;;;	if (litmno .eq. 'EG') vline = 'FN = 303-i;'
;;;	using lf3 select
;;;	(24),	if (litmno.eq.'GE' .or. litmno.eq.'GA') vline = 'FN = 307-i;'
;;;	(42),	if (litmno.eq.'GE' .or. litmno.eq.'GA') vline = 'FN = 307-i;'
;;;	endusing


	is_307 = 0
	using matl select
	(2,5),	WRITES (chnwrk, 'FN = 303-i;')	;alum
	(),	using litmno select
		('EG'),	if (lf3.eq.24 .or. lf3.eq.42)
			then	begin
				is_307 = 1
				WRITES (chnwrk, 'FN = 307-i;')
				end
			else	WRITES (chnwrk, 'FN = 303-i;')
				
		(),	using cmpcod select
			(),	 WRITES (chnwrk, 'FN = 304-i;')
			endusing

	;;;	(),	WRITES (chnwrk, 'FN = 304-i;')
		endusing
	endusing

;;;	writes (chnwrk,vline)

;1-14-19 ---------------------------------------------------
;;;	using matl select
;;;	(2,5),	WRITES (chnwrk, 'FN = 303-i;')	;alum
;;;	(),	using litmno select
;;;		('EG'),	WRITES (chnwrk, 'FN = 303-i;')
;;;		(),	using cmpcod select		
;;;			(),	 WRITES (chnwrk, 'FN = 304-i;')
;;;			endusing
;;;
;;;	;;;	(),	WRITES (chnwrk, 'FN = 304-i;')
;;;		endusing
;;;	endusing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
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


	if (vynl)
	then	WRITES (chnwrk, 'METALN= "PVS SPIRAL FITT";')
	else	BEGIN
		VLINE = 'METALN= "'
		VLINE (10,13) = VMAT
		LN = %TRIM(VLINE) + 1
		VLINE (LN,80) = ' SPIRAL FITT";'
		WRITES (chnwrk, VLINE)
		END

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

	vline = LSEAM
	WRITES (chnwrk, vline)
	
	VLINE = GSEAM
	WRITES (chnwrk, vline)

	VLINE = SSEAM
	WRITES (chnwrk, vline)


	CLEAR TBLKEY
	TBLCOD = 'V1'
;;	V1_KEY = 1		; gorelock elbows and angles
	using code select
	('EG'),		v1_key = 13
	('GA','GE'),	v1_key = 1
	(),		v1_key = 1
	endusing

	V1_F3 = LF3
	call get_v1_table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
;;;	IF (LOKCTL .NE. 0) CLEAR COPTBL
;;;
;;;	FOR J FROM 1 THRU 4
;;;		USING GA SELECT
;;;		(16),		WD4(J) = V1_16(J)
;;;		(18 THRU 20),	WD4(J) = V1_1820(J)
;;;		(22 THRU 26),	WD4(J) = V1_2226(J)
;;;		(),		WD4(J) =
;;;		ENDUSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; for ez flange .le. 24 add 5/8 (.625) to ends (SR & SB)
	clear ezjr_b, ezjr_r

	using lf3(4,4) select	;LF3(4,4) = bottom end treatment
	(3,9),	if(sz.le.24)	;ez flange .le. 24
		then	begin
			SB = V1_SB 
			SR = V1_SR 
			ezjr_B = 1
			end
		else	clear sb, sr

	(),	begin		;not ez flange
		SB = V1_SB 
		SR = V1_SR 
		end
	endusing

	using sz select		;1-2-10 for swedged rings
	(3 thru 7),	if (lf3 .eq. 5)
				begin
				sb = sb + 625
				sr = sr + 625
				end

	endusing

	using lf3(5,5) select	;LF3(5,5) = right end treatment
	(3,9),	if(sz.le.24) ezjr_r = 1		;ez flange .le. 24

	endusing

	using lf3 select	;lf3=3 or 9 => both bottom and right are ez flange
	(3,9),	if(sz.le.24)begin
		ezjr_b = 1
		ezjr_r = 1
		end
	endusing


	USING SZ SELECT
	(5 THRU 7),	R = 8000
;;;	(6),		R = 6000
;;;	(7),		R = 7000
	(8 THRU 19),	R = 8000
	(20 THRU 40),	R = 10000
	(42 THRU 60),	R = 20000
	(),		R = 1000
	ENDUSING

	IF (CODE .EQ. 'EG') R = SZ*1000
	IF (FULLSWEEP) R = SZ*1000		;12-04-07

;1-14-19 ssq:
	
	W = ((SZ * 1000) + WD4(1))		;.XXX

	goto (do_307), is_307

	VLINE = W,	'W= ZX.XXX;' [LEFT]
	WRITES (chnwrk, VLINE)
	goto done_307

do_307,
;1-7-19 ssq:
	using lf3 select
	(24),	begin
		wr = w - 188
		wb = w
		sr = 2000
		end
	(42),	begin
		wb = w - 188
		wr = w
		sb = 2000
		end
	endusing
;1-7-19 ssq

;8-14-19 ssq
	using an select
	(45,90), using sz select
		 (3 thru 9), if(code.eq.'EG' .and. lf3.eq.24) sb = 2000	;24=le/sp
		 endusing	

	endusing
;8-14-19 ssq

	VLINE = WB,	'WB= ZX.XXX;' [LEFT]
	WRITES (chnwrk, VLINE)

	VLINE = WR,	'WR= ZX.XXX;' [LEFT]
	WRITES (chnwrk, VLINE)
done_307,

	VLINE = SB,	'SB= ZX.XXX;' [LEFT]
	WRITES (chnwrk, VLINE)

	VLINE = SR,	'SR= ZX.XXX;' [LEFT]
	WRITES (chnwrk, VLINE)

	VLINE = R,	'R= ZX.XXX;' [LEFT]
	WRITES (chnwrk, VLINE)

	USING AN SELECT
	(45),	NG = 3
	(90),	NG = 5
	(),	NG = 4		;this should never happen
	ENDUSING

	DEG = (NG-1) * 375
	ANGL = (AN*1000) ;;>- DEG	;12-5-2013 per BillN

	IF (BUTWELD)
	THEN	VLINE = ANGL,	'A= XX.XXX;' [LEFT]
	ELSE	VLINE = AN,	'A= XX;' [LEFT]
	WRITES (chnwrk, VLINE)

	VLINE = NG,	'NG= X;' [LEFT]
	writes (chnwrk, vline)


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


;;;	if (ezjr)
;;;		begin
;;;		v2_cn1 = 'JR EZ FLANGE'
;;;		v2_cn2 = v2_cn1
;;;		end

	
	if (ezjr_b) v2_cn1 = 'JR EZ FLANGE'
		
	
	if (ezjr_r) v2_cn2 = 'JR EZ FLANGE'
		

	VLINE = 'CONN= B;CN= "'
	LN = %TRIM (VLINE)
	INCR LN
	VLINE(LN,60) = V2_CN1
	LN = %TRIM(VLINE)
	INCR LN
	VLINE(LN,80) = '"; END;'
	WRITES (chnwrk, VLINE)

	VLINE = 'CONN= R;CN= "'
	LN = %TRIM (VLINE)
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
;======================================================
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


	END

		
