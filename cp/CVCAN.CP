;cvcan.cp	check vulcan
;
.subroutine	cvcan
	ordln	,a
	ocusno	,d
	chn182	,d
	is_vulcan	,d	;1= this item goes to Vulcan Round Printer

; 01-17-18: check for any dimension over 36
; 06-20-18: check for f3 notes containing 5, 6 or 7
; 10-30-19: special case for eg w/ f3-note = 24

record	ordlin
	.include 'def:rd045a.def'

record	coptbl
	.include 'def:rd182a.def'
;

record	
	segs	,a12
record,x
	ga	,d2
	sara	,5d2

record	vars
	i	,d6
	a5	,a5

.proc
	ordlin = ordln

	
	CLEAR IS_VULCAN

	IF (LSTOKT .EQ. 'S') XRETURN
	IF (OCUSNO .EQ. 5) XRETURN

	USING LDEPT SELECT
	('F','H','K'),	NOP
	(),		XRETURN
	ENDUSING


;---------------------------------------------------
; 6-20-18 ssq:
	a5 = lf3, 'XXXXX'
	if (%instr(1, a5, '5') ) xreturn
	if (%instr(1, a5, '6') ) xreturn
	if (%instr(1, a5, '7') ) xreturn
;---------------------------------------------------

; if notes are "send to vulcan = n" skip item...
	clear tblkey
	tblcod = 'M1'
	mm_key = lf1
	read (chn182, coptbl, tbl_key) [err=nk1]
	if (mm_vulc .eq. 2) return
nk1,
	clear tblkey
	tblcod = 'M2'
	mm_key = lf2
	read (chn182, coptbl, tbl_key) [err=nk2]
	if (mm_vulc .eq. 2) return
nk2,
	clear tblkey
	tblcod = 'M3'
	mm_key = lf3
	read (chn182, coptbl, tbl_key) [err=nk3]
	if (mm_vulc .eq. 2) return
nk3,

	USING LITMNO SELECT
	('BN'),				IS_VULCAN = 1	;CALL BN	;BULLNOSE TEE
	('C4020','C4420','C4620'),	NOP
	('C4820','C4220'),		NOP
	('C9','C4','CV9','CV4'),	IS_VULCAN = 1	;CALL SHOE_TAP		;CROSS
	('CT','CC','CTV','CCV'),	IS_VULCAN = 1	;CALL SHOE_TAP		;TEE	;CONICAL TEE/CROSS
	('T4','T9','TV4','TV9'),	IS_VULCAN = 1	;CALL SHOE_TAP		;TEE
	('EC'),				IS_VULCAN = 1	;NOP;	CALL END_CAP
;;;	('EG'),				IS_VULCAN = 1	;CALL ELBOW	
	('EG'),				if (lf3.ne.24) IS_VULCAN = 1	;10-30-19	
	('EV'),				IS_VULCAN = 1	;CALL ELBOW
	('AV'),				IS_VULCAN = 1	;CALL ELBOW

	('GA','GE'),		IF (LF3.NE.1) IS_VULCAN = 1 	;IF COMPLETE SEAL THEN NOT VULCAN 2-1-17
;;;	('GA','GE'),			IS_VULCAN = 1	;CALL ROUND_ELBOW_ANGLE

	('ER16' THRU 'ER26'),		IS_VULCAN = 1	;CALL ECC_REDUCER
	('ERW16' THRU 'ERW26'),		IS_VULCAN = 1	;CALL ECC_REDUCER
	('R16' THRU 'R26'),		IS_VULCAN = 1	;CALL ROUND_CONCENTRIC_REC
	('RW16' THRU 'RW26'),		IS_VULCAN = 1	;CALL ROUND_CONCENTRIC_REC
	('RV16' THRU 'RV26'),		IS_VULCAN = 1	;CALL REDU
	('RT','RTV'),			NOP;	CALL REG_TAKE_OFF
	('STC','STT'),			IS_VULCAN = 1	;CALL SHOE_TAP		;SHOE TAP CROSS
	('SC0' THRU 'SC8'),		NOP;	STORM COLLARS
	('SR'),				NOP;	SQUARE-TO-ROUND 4-20-15
	('STW'),			NOP;	PER BRANDON 2-1-17
	('SWF'),			NOP;	ST STL WIRE FEED 3-24-09
	('S'),				IS_VULCAN = 1	;CALL SADDLE
	ENDUSING


;1-17-18 check for seg > 36

	clear segs
	xcall cfg2 (lcfgim, segs)
	for i from 1 thru 5
		begin
		if (sara(i) .gt. 36) is_vulcan = 0
		end

	XRETURN				
;----------------------------------------------
.end

