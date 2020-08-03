SUBROUTINE MKDEC
	ORDNO	,D
;MAKDEC.CP
;	create export (order.dec) file for coil line
;DDEPT 5-29-14

RECORD	DUCTO	;DEBUG FILE
	.INCLUDE 'DEF:DUCTO.DEF'

RECORD	DUCACC
	.INCLUDE 'DEF:RD175A.DEF'

RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.DEF'

RECORD	DUCFIL
		,A4,	'SPL:'
		,A1,	'D'
	WRORD	,A6
		,A4,	'.ISM'

RECORD	DEC_FILE
	DF_ORDER	,D6
			,A4,	'.DEC'

RECORD	DBG_FILE
	DB_ORDER	,D6
			,A4,	'.DBG'

RECORD	BUF
	DATA	,A512
;
RECORD	CHANNEL
	CHN175	,D2
	CHNWRK	,D2
	CHNDBG	,D2
	CHNDUC	,D2

RECORD	VARS
	temp	,a25	;for punch pattern processing
	savpath	,a512
	d_path	,a512
	sts	,d6
	cmpcod	,a3
	STAT	,D1
	CHAN	,D2
	ctr	,d6
	OPNOK	,D1
	big	,d6
	lit	,d6
	J_ONLY	,D1
	C_ONLY	,D1
	JCJ	,D1
	JCCJ	,D1
	WRK_JOINT	,D6
	XON	,D1
	I	,D6
	K	,d6
	A1	,A1
	T1CNT	,D6
	T2CNT	,D6
	HLCNT	,D6
	D10	,D10
	FIELD	,A10
	BMAX	,D3,	512
	BL	,D6
	FL	,D6
	TL	,D6
	LN	,D6
	ENTRY	,A30
	INXCTL	,D1
	LOKCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	READ	,D1,0
	SWITCH	,D1
	V	,D1
;
PROC
	xcall who(cmpcod)
	using cmpcod select
	('SMC','TST'),	NOP
	(),		XRETURN	;only SMC for now...
	endusing

;-----------------------------------------------------
; this code will cause the work files to be opened in the users
; local "spl:" directory...

	savpath = %syn_getdir
	xcall getlog ('SPL', d_path, ln)	;logical for spl...
	sts = %syn_setdir (d_path)		;set current directory to spl...
;-----------------------------------------------------

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	find (chn175, ducacc, ordno) [err=endoff]

	DF_ORDER = ORDNO
	OPEN (CHNWRK, O, DEC_FILE)

;Create output file... file sent to coil line needs to be in same
; order as work order...

	WRORD = ORDNO
	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (39, SU, DUCFIL)
	CHNDUC = 39

w_loop,
	reads (chn175, ducacc, w_eof)
	if (ductor .ne. ordno) goto w_eof

	XCALL DDEPT (GAUGE,SIZE3,DUTYPE,WD_DEPT,STY)
;;;	USING STY SELECT
;;;	(1,4,5),	WD_DEPT='P'		;TDC
;;;	(2,3,6),	WD_DEPT='O'		;S&D
;;;	ENDUSING

; ssq 6-4-01 force duct work to end
	USING WD_DEPT(1,1) SELECT
	('O'),	WD_DEPT(1,1) = 'Y'
	('P'),	WD_DEPT(1,1) = 'Z'
	ENDUSING

	WDUTYPE = DUTYPE	; Type of duct 
	WSTY = 10-STY		; Style		descending
	WLINER = LINER		; Liner 
	WSEAM = SEAM		; Seam  
	WSEAL = SEAL		; Seal
	WGAUGE = GAUGE		; Gauge ( 26,24,22,20,18 )
	WSIZE3 = 100-SIZE3	; LENGTH	descending
	WSIZE1 = 1000000-SIZE1	; SLIP		descending
	WTHICK = THICK		; Dec. Thickness .024-24g .032-22g .040-20g .050-18g
	WCAT = CAT		; Catagory 
	WJOINT = JOINT		; Number of joints ( quantity)
	WSIZE2 = 1000000-SIZE2	; DRIVE		descending	

	WLOCAT = LOCAT
	WSQFEET = SQFEET
	WPOUNDS = POUNDS
	WSQFLIN = SQFLIN
	WGPRICE = GPRICE
	WLINPRC = LINPRC
	WDPUNCH = DPUNCH
	if (wdpunch .eq. '0') clear wdpunch	;ssq 6-28-05
	WDSEQ = DSEQ	
	
	wd_cngd = dcngd
	STORE(CHNDUC,DUCWRK,WD_KEY)

	goto w_loop
w_eof,


	DB_ORDER = ORDNO

	find (chnduc, ducwrk, ^first) [err=loop]
loop,
	reads (chnduc, ducwrk, eof)

	WSIZE3 = 100-WSIZE3	; LENGTH 	restore original value
	WSIZE1 = 1000000-WSIZE1	; SLIP		restore original value
	WSIZE2 = 1000000-WSIZE2	; DRIVE		restore original value	
	WSTY = 10-WSTY		; Style		restore original value

	ctr = 0				;1st pass...
pc4,					;4-pc needs 2 line...
	incr ctr

	using wcat select
	(2),	D10 = wJOINT		;wraparound
	(),	D10 = wJOINT * 2
	endusing

	CALL MAK_FIELD

; 11-3-10 print in order input per Brandon...
;;;	if (wsize1.gt.wsize2)
;;;	then	begin
;;;		big = wsize1
;;;		lit = wsize2
;;;		end
;;;	else	begin
;;;		big = wsize2
;;;		lit = wsize1
;;;		end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	D10 = big#1		;.xxx to .xx
	D10 = wsize1#1		;.xxx to .xx
	if (wcat.eq.3 .and. ctr.eq.2) d10 = lit#1
	CALL MAK_FIELD

;;;	D10 = lit#1		;.xxx to .xx
	D10 = wsize2#1
	if (wcat.eq.3) d10 = 0
	CALL MAK_FIELD

	D10 = wGAUGE
	CALL MAK_FIELD

	D10 = wCAT	;duct type
	USING wCAT SELECT
	(1),	D10 = 3	;L-shape
	(2),	D10 = 5	;Wraparound
	(3),	D10 = 2	;4-peice
	(4),	D10 = 
	ENDUSING
	CALL MAK_FIELD

	D10 = wSIZE3 - 2	;coil width: 1=3ft, 2=4ft, 3=5ft, 4=6ft
	CALL MAK_FIELD

	D10 = 1		;bead used, always 1
	CALL MAK_FIELD

	USING wSEAM SELECT	;lock type
	(1),	D10 = 2
	(2),	D10 = 1
	(),	D10 = 3		;anything else
	ENDUSING
	CALL MAK_FIELD

	D10 = 1			;seal used?, always 1
	CALL MAK_FIELD
	
	USING wSTY SELECT
	(1),	D10 = 2		;S/D
	(2),	D10 = 1		;TDC
	(3,7),	D10 = 5		;None
	(4,5,6),D10 = 4		;Other
	ENDUSING
	CALL MAK_FIELD

	USING wLINER SELECT
	(2,5),		D10 = 1		;1/2 "
	(1,3,7),	D10 = 2		;1"
	(6,8),		D10 = 3		;2"
	(4),		D10 = 5		;None
	ENDUSING
	CALL MAK_FIELD

	USING wLINER SELECT
	(4),	D10 = 2		;glue used
	(),	D10 = 1
	ENDUSING
	CALL MAK_FIELD
	
	USING wCAT SELECT
	(3),	D10 = 2		;brake used; 4-piece
	(),	D10 = 1
	ENDUSING
	CALL MAK_FIELD

	D10 = 1			;pin type = 1 always
	CALL MAK_FIELD
	
	D10 = 0			; die used

	CALL PUNCH_PATTERN

	D10 = 0			;die used - set below
	IF (J_ONLY) D10 = 1	;edge
	IF (C_ONLY) D10 = 2	;center
	IF (JCJ)    D10 = 3	;all
	IF (JCCJ)   D10 = 4	;diamond
	CALL MAK_FIELD

	D10 = 1			;diameter of hole (always 1 for now)
	CALL MAK_FIELD

;;;	D10 = T1CNT		;leg1
	D10 = T2CNT		;11-3-10 per Brandon
	CALL MAK_FIELD
	
;;;	D10 = T2CNT		;leg2
	D10 = T1CNT		;11-3-10 per Brandon
	CALL MAK_FIELD
	
	TL = %TRIM(BUF)
	IF (BUF(TL,TL) .EQ. ',') TL = TL - 1	;strip off last comma
	WRITES (CHNWRK, BUF(1,TL) )

	CLEAR BUF

	if (wcat.eq.3 .and. ctr.eq.1) goto pc4
	GOTO LOOP
EOF,
	close chnwrk

	xcall ftp (ordno)

	CLOSE CHNDUC
	XCALL DELET (DUCFIL)
;
ENDOFF,
	CALL CLOSE

	sts = %syn_setdir (savpath)		;set current directory to spl...

	XRETURN
;;;	STOP

MAK_FIELD,	;;;;;;;;;;;;;;;;;;;;;;
	
	FIELD = D10,	'ZZZZZZZZZX,' [LEFT]
	FL = %TRIM(FIELD)

	BL = %TRIMZ(BUF)
	INCR BL			;start pos
	FL = BL + FL -1		;end pos
	BUF(BL,FL) = FIELD

	RETURN
;-------------------------------------

PUNCH_PATTERN,	;;;;;;;;;;;;;;;;;;;;;;
; check elements of punch pattern

	clear k, temp
	for i from 1 thru 25
		begin
		using wdpunch(i,i) select
		('J','j','C','c'),	begin
					incr k
					temp(k,k) = wdpunch(i,i)
					end
		endusing
		end
	upcase temp

	clear c_only, j_only, jcj, jccj		;10-25-10
	clear t1cnt, t2cnt, hlcnt		;10-25-10

	IF (%INSTR(1,temp,'C') ) c_only = 1
	IF (%INSTR(1,temp,'J') ) j_only = 1
	IF (%INSTR(1,temp,'C') ) clear j_only
	IF (%INSTR(1,temp,'J') ) clear c_only
	IF (%INSTR(1,temp,'JCJ') ) 
		BEGIN
		JCJ = 1
		SET J_ONLY, C_ONLY = 0
		END

	IF (%INSTR(1,temp,'JCCJ') ) 
		BEGIN
		JCCJ = 1
		SET J_ONLY, C_ONLY = 0
		END

	WRK_JOINT = wJOINT
;------------------------------------------------------
;;; ssq 4-12-05 ssq: don't count rangle as part of
;	punch pattern.  ie in "WRJ" don't count the "J"

	TL = %INSTR(1, wDPUNCH, 'WR')
	IF (TL .GT. 0) 
	THEN	TL = TL + 2	;pos of char in question...
	ELSE 	TL = 0
	
;------------------------------------------------------

CNTTRD,
	LN=%TRIM(wDPUNCH)
	FOR I FROM 1 THRU LN
		BEGIN
		IF (I .EQ. TL)
		THEN	CLEAR A1	;4-12-05, don't count rangle
		ELSE	A1=wDPUNCH(I,I)

	;;;	if (tbl_punch.eq.1 .and. A1 .eq. 'J') clear A1	;per sm

		upcase a1		;02-21-08 new tables
		IF(A1.EQ.'X')XON=1
		USING XON SELECT
		(0),	IF(A1.EQ.'C' .OR. A1.EQ.'J') INCR T2CNT
		(1),	IF(A1.EQ.'C' .OR. A1.EQ.'J') INCR T1CNT
		ENDUSING
		END
;;; 11-3-10>	T1CNT = T1CNT * WRK_JOINT
;;; 11-3-10>	T2CNT = T2CNT * WRK_JOINT
	HLCNT = T1CNT + T2CNT
	RETURN
;-------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK, chn175, chnwrk
	
	FOR CHAN FROM 20 THRU 90
		BEGIN
		XCALL CHNOPN (CHAN, STAT)
		IF (STAT .EQ. 0) 
		  BEGIN
		  SWITCH = 5
		  IF (.NOT. CHN175) 
		  THEN	BEGIN
			XCALL FILES (CHAN,'SI',175,SWITCH)
			IF (SWITCH.EQ.9) RETURN
			CHN175 = CHAN
			END	
		  ELSE	BEGIN
			CHNWRK = CHAN
			OPNOK = 1
			RETURN
			END	
		  END
		END


	RETURN
;-------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	IF (CHN175) CLOSE CHN175
	IF (CHNWRK) CLOSE CHNWRK

	RETURN
;-------------------------------------

END
	

