;mak_prc.dbl	production version
;
;	update price from configurator
;
record	itmmas
	.include 'def:rd041a.def'
;
record	itmkey
	.include 'def:rd041k.def'

record	ordlin
	.include 'def:rd045a.def'	;not updated but need variable names

record	out
	o_item	,a15
		,a1
	o_f1	,a3
		,a1
	o_f2	,a3
		,a1
	o_f3	,a5	
	o_old	,a11
	o_new	,a11


record	dis
	ii	,d6
;

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	DMPRIC
	.INCLUDE 'DEF:DMPRIC.DEF'

record	cfgmat
	.include 'def:cfgmat.def'

RECORD TPRICE
	ARRAY	,80D5

RECORD	TOFF
	RT_SZ1	,D2
		,A1,	'/'
	RT_SZ2	,D2
		,A4,	' ON '
	RT_DIA	,D2
		,A*,	'"RD '
	RT_GA	,D2
		,A*,	'GA '
	RT_REST	,A10	
	

RECORD	DMPDAT
	DMAT	,A1
	DHT	,D3
	DLEN	,D3
	DHARA	,3D3		;ARRAY OF SECTION HEIGHTS
	DLARA	,3D3		;ARRAY OF SECTION LENGTHS
	DITEM	,A15

RECORD TEEDAT
	TGUAG	,D2
	TMAIN	,D2
	TREDU	,D2
	TBRAN	,D2
	TDESCR	,A30

RECORD	SADDLE
	SANGL	,A2
	SGUAG	,A2
	SBRAN	,A2
	SMAIN	,A2
		,A1	;IN CASE "DAMPER" (*)
RECORD,X
		,A4	;ANGLE, GAUGE
	STAR	,A1	;FOR DAMPER
	ST_BRAN	,A2
	ST_MAIN	,A2


record	channel
	chn182	,d2
	chn041	,d2
	chnpp	,d2

RECORD	VDAMPR
	DAMP		,D2	;DAMPER DIMENSIONS
	DAMPER_PRICE	,3D8
	SAVE_DP		,3D8
	FL	,D2	;
	MAXFLEN	,D2
	BAD_DIGIT	,D1
	PARTNO	,A15
	PFF	,A15

record	pp_rec
	p_item	,a15

record	vars
	opnok	,d1
	p_mult	,d4
	wrkfil	,a14,	'spl:ppxref.ism'
	vanitm	,a15
	savprc	,d9
	itmrfa	,a6

	ik1	,d3
	ik2	,d3
	ik3	,d5
	dec	,d10
	entry	,a30
	inxctl	,d1
	ctr	,d1
	cmx	,d3	;x.xx
	read	,d1,0
	lokctl	,d1
	i	,d6
	blanks	,a25
	dga	,d2
	st_guage	,d2
	y	,d3
	
	kitmno	,a15
	prppn	,d6
	wtpin	,d6
	dpnd	,d6
	dsiz	,d3
	ra_size	,d3
	sp_size	,d3
	inch	,d3
	dash	,d3
	decmal	,d18
	N_PRICE	,D10
	N_NUM	,D10
	N_DIG	,D1
	D_SQFT	,D6
	D_MIN	,D5	;MIN PRICE
	D_HV	,A1	;(Y/N)
	D_FM	,A1
	D_SL	,A1
	DDIM	,D3
	TEMP	,D6
	TEMP2	,D6
	QLEN	,D3
	QHT	,D3
	DQTY	,D3
	lbad_digit	,d1
	mat_sel	,d1
	j	,d6
	x	,d6
	config_price	,d8
	config_error	,d1
	pidx	,d6
	tl	,d6
	wrangl	,a12	
	RTLEN	,D2
	RTVAL	,A6
	VYNL	,D1
	switch	,d1
	v	,d1

proc
;;;	open (15,i,'tt:')
	xcall terid (v)

	xcall outpt (1,1,2,'PRICE INVENTORY',1)
	xcall outpt (4,4,0,'Are you sure?',1)
	xcall input (4,20,01,01,'YN',entry,inxctl,1)
	if (inxctl .eq. 2)	xcall pgchn ('cp:immenu',1)

	call opens
	if (.not. opnok) goto endoff


	call make_pp

; set default values...
	p_mult =	;matl is included in price table
	cmx = 100		;contractor
	ctr = 1
;---------------------------------------------------
;
	reads (1, itmmas, eof)
	ii = 1
loop,
	incr ii
	if (ii/500*500 .eq. ii) display(15,$scr_pos(1,70),dis)
	reads (1, itmmas, eof, getrfa:itmrfa)

	if (icfg .ne. 1) goto loop	;not config

	mat_sel = galv		;default for now...

	if (itemno .eq. 'S9*0810')	;;; .and. if1 .eq. '064')
		BEGIN
		CTR = 1
		END

	find (chnpp, pp_rec, itemno) [err=not_pp]
	goto loop				;pressed part

not_pp,
	ik1 = if1
	ik2 = if2
	ik3 = if3

	clear p_mult
	if (ik1 .gt. 0)		;can this f1 note be assoc. w/ a pressed part?
		begin
		clear tbl_key
		tblcod = 'M1'
		mm_key = ik1
		read (chn182, coptbl, tbl_key) [err=not_pressed]
		mat_sel = m1_matl	;actual material 
		if (mat_sel.lt.1 .or. mat_sel.gt.4) mat_sel = galv
	;;;	p_mult = mm_mult	;matl is in price table
		
		if (mm_pp .ne. 1) goto not_pressed
		end
	if (ik2 .gt. 0) goto not_pressed	;f2 notes are never pressed

	if (ik3 .gt. 0)		;can this f3 note be assoc. w/ a pressed part?
		begin
		clear tbl_key
		tblcod = 'M3'
		mm_key = ik3
		read (chn182, coptbl, tbl_key) [err=not_pressed]
		if (mm_pp .ne. 1) goto not_pressed
		end

	dec = ik1 + ik2 + ik3
	if (dec .eq. 0)		;is the item a pressed part?
		begin
		clear tbl_key
		tblcod = 'PP'	
		pp_item = itemno
		find (chn182, coptbl, tbl_key) [err=not_pressed]
	;;;>>>	goto loop		;don't change price for pressed parts
		end
not_pressed,	

	clear ordlin
	litmno = itemno
	lprice = price
	call find_damper

	call config
	call cfg_price_routine

;;; 10-16-07 damper price added in scrnx
	if (ldampr) call add_damper_price	
	if (p_mult .gt. 100) call add_matl_mult

;;;	if (lprice.gt.0 .and. lprice.ne.price)
	if (lprice.ne.price)
		begin
		o_item = itemno
		o_f1 = if1
		o_f2 = if2
		o_f3 = if3
		o_old = price,	'ZZZ,ZZZ.XXX'
		o_new = lprice,	'ZZZ,ZZZ.XXX'
		writes (10, out)

		price = lprice
		write (1, itmmas, itemno)

		end

	goto loop
eof,
endoff,
	call close
	lpque ('mak_prc.dat')
	xcall pgchn ('cp:immenu',1)

	stop
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CONFIG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; BRANCH TO PRODUCT CONFIGURATORS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	kitmno = litmno

	TDESCR=LDESCR			;SAVE IN CASE USER HAS CHANGED
	CONFIG_PRICE = LPRICE		;ssq 10/16/02 moved here

	USING LITMNO SELECT		;SSQ 5-27-03
	('SG','SX'),	IF(%TRIM(LITMNO) .LT. 7) RETURN
	(),		IF(%TRIM(LITMNO) .LT. 5) RETURN
	ENDUSING

	CLEAR CONFIG_ERROR

	USING LITMNO SELECT
	('C4020','C4420','C4620'),	RETURN	;SSQ 2-19-03
	('C4820','C4220 '),		RETURN	;SSQ 2-19-03
	('WR'),				CALL R_ANGLE
	('R16' THRU 'R26'),		CALL CALL_CFG
	('RV16' THRU 'RV26'),		CALL CALL_CFG
	('T4','T9','TV4','TV9'),	CALL CALL_CFG
	('C9','C4','CV9','CV4'),	CALL CALL_CFG
	('CT','CC','CTV','CCV'),	CALL CALL_CFG
	('ER'),				CALL CALL_CFG
	('BN'),				CALL CALL_CFG	
	('P4','P9','PV4'),			CALL CALL_CFG

	('S9','S4','SV','SW','SC','ST'),CALL CALL_CFG
	('SG','SX'),			CALL SPIRAL
	('RT','RTV'),			CALL R_TAKEOFF
	('OB','PB','SBD','BDD'),	CALL CFG_DAMPER	;BALANCING DAMPER
	('SLF','SLB'),			CALL CFG_DAMPER	;LOUVERS
	ENDUSING
	RETURN
;----------------------------------------------------

CALL_CFG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; internal routine so I can see parameter list...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	call get_van	;in case we need the vanilla price...

	XCALL CFG(LITMNO,CONFIG_PRICE,TDESCR,CONFIG_ERROR,MAT_SEL,ST_GUAGE)
;;;	IF (CONFIG_ERROR .EQ. 1) 
	IF (CONFIG_ERROR) 	;11-14-13
		begin
		writes (12, itmmas)
		end
	RETURN
;----------------------------------------------------

;====================================================
;----------- PRODUCT CONFIGURATOR ROUTINES ----------
;====================================================
CFG_DAMPER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PRELIMINARY CALCS FOR ALL DAMPERS...
		;;; AND LOUVERS (SSQ 12-16-03
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	CLEAR DMPDAT, CONFIG_PRICE, CONFIG_ERROR, TDESCR

	DITEM = LITMNO(1,3)
	DMAT = LITMNO(4,4)		;MATERIAL

	USING DMAT SELECT		;NO MATERIAL IF GALV
	('A','S'),	DITEM = LITMNO
	('U','P'),	DITEM = LITMNO		;COLOR CLAD, PG
	('0' THRU '9'),	DITEM(5,15) = LITMNO(4,14)	;ssq 2-18-03
	(),		GOTO DAMPER_ERROR
	ENDUSING

	TL = %INSTR(1,DITEM,'X')			;FIND THE X

	ONERROR DAMPER_ERROR
	IF (TL .LE. 1) 
	THEN	BEGIN
		DLEN = DITEM(5,6)
		DHT = DITEM(7,8)
		END
	ELSE	BEGIN
		DLEN = DITEM(4,TL-1)
		DHT = DITEM(TL+1,%TRIM(DITEM))
		END
	OFFERROR

	IF (DLEN.LE.99)
	THEN	BEGIN
		TDESCR(1,2) = DLEN,	'ZX'
		TDESCR(3,3) = 'X'
		TDESCR(4,6) = DHT,	'ZZX' [LEFT]
		END
	ELSE	BEGIN
		TDESCR(1,3) = DLEN,	'ZZX'
		TDESCR(4,4) = 'X'
		TDESCR(5,7) = DHT,	'ZZX' [LEFT]
		END

;148x148 PARALLEL BLADE STS STL
;148x148 STD LOUVER FLG
;123456789012345678901234567890


	USING DMAT SELECT		;this get's overriden in lvr
	('A'),	TDESCR(24,30) = 'ALUM'
	('S'),	TDESCR(24,30) = 'STS STL'
	('S'),	TDESCR(24,30) = 'SS'
	('U'),	TDESCR(24,30) = 'CLAD'
	('P'),	TDESCR(24,30) = 'PGRP'
	ENDUSING

	USING DITEM SELECT
	('OB','PB'),	GOTO SMP	;BALANCING
	('SB'),		GOTO SBD	;SINGLE BLADE
	('BD'),		GOTO BDD	;BACKDRAFT
	('SLF','SLB'),	GOTO LVR	;LOUVER
	ENDUSING

DAMPER_ERROR,
	OFFERROR
	writes (12, itmmas)
	CONFIG_ERROR = 1
	RETURN
;----------------------------------------------------------

SMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; BALANCING DAMPER CONFIGURATOR
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (DHT.LT.6) GOTO DAMPER_ERROR	;10/28/02 per SM
	IF (DLEN.LT.6) GOTO DAMPER_ERROR	;10/28/02 per SM

	IF (DITEM(1,1) .EQ. 'P')
	THEN	TDESCR(9,22) = 'PARALLEL BLADE'
	ELSE	TDESCR(10,22) = 'OPPOSED BLADE'

	IF (DITEM(3,3) .EQ. 'S') TDESCR(24,30) = ' SEALED'

;;;	FL_NAME = 'DMPRIC'		;SSQ 9-9-03
;;;	OPEN(9,I,FILPRC)		;SSQ 9-9-03
	OPEN(9,I,'SMC:DMPRIC.SMC')
	IF (DITEM(3,3) .EQ. 'N')
	THEN	READ(9,DMPRIC,1)	;NON-SEALED MATRIX
	ELSE	READ(9,DMPRIC,2)	;SEALED MATRIX
	CLOSE 9

	DDIM = 48
	CALL GET_DAMPER_SECTIONS

	DITEM(5,15) = 
	CLEAR CONFIG_PRICE

	FOR I FROM 1 THRU 3
		BEGIN
		USING DHARA(I) SELECT		;FIND THE MATRIX ROW
		(01 THRU 11),	X = 1
		(12 THRU 18),	X = 2
		(19 THRU 24),	X = 3
		(25 THRU 32),	X = 4
		(33 THRU 40),	X = 5
		(41 THRU 48),	X = 6
		(),		X = 0
		ENDUSING
		FOR J FROM 1 THRU 3
		IF (X.GT.0 .AND. DLARA(J).GT.0)
			BEGIN
			USING DLARA(J) SELECT		;FIND THE MATRIX COLUMN
			(01 THRU 18),	Y = 1
			(19 THRU 24),	Y = 2
			(25 THRU 30),	Y = 3
			(31 THRU 36),	Y = 4
			(37 THRU 42),	Y = 5
			(43 THRU 48),	Y = 6
			(),		Y = 0
			ENDUSING
	
			PIDX = (X-1)*6 + Y		;GET THE INDEX

			IF (PIDX.GT. 0)
&			CONFIG_PRICE = CONFIG_PRICE+(DM_PRICE(PIDX)*DM_MULT)#1	;TO .XXX
			END
		END


	USING MAT_SEL SELECT
	(ALUM),	CONFIG_PRICE = (CONFIG_PRICE * 200)#2	;ALUM
	(SST),	CONFIG_PRICE = (CONFIG_PRICE * 225)#2	;STAINLESS
	ENDUSING

	CALL ROUND_NICKLE		;ROUND CONFIG_PRICE UP TO NEAREST .05
	
	LITMNO = DITEM(1,4)
	RETURN
;----------------------------------------------------------

SBD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; SINGLE BLADE DAMPER
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (DHT.LT.04 .OR. DHT.GT.12) GOTO DAMPER_ERROR
	IF (DLEN.LT.04 .OR. DLEN.GT.48)GOTO DAMPER_ERROR
	
	TDESCR(7,20) = 'SINGLE BLADE'

	CLEAR TBL_KEY
	TBLCOD = 'SB'
	READ (CHN182,COPTBL,TBL_KEY)[ERR=DAMPER_ERROR]
	CONFIG_PRICE = (DLEN*DHT)*SB_PCUN + SB_QUAD
	IF (CONFIG_PRICE .LT. SB_MIN) CONFIG_PRICE = SB_MIN

	USING DMAT SELECT
	('A'),	CONFIG_PRICE = (CONFIG_PRICE*SB_AMUL)#2
	('S'),	CONFIG_PRICE = (CONFIG_PRICE*SB_SMUL)#2
	ENDUSING

	CONFIG_PRICE = (CONFIG_PRICE*SB_PMUL)#1		;.XXX
	LITMNO = DITEM(1,4)
	CALL ROUND_NICKLE		;ROUND CONFIG_PRICE UP TO NEAREST .05
	RETURN
;----------------------------------------------------------
LVR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; LOUVERS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	return		;later
;----------------------------------------------------------

BDD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; BACKDRAFT DAMPER
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	RETURN
;------------------------------------------------------

ROUND_NICKLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	N_PRICE = CONFIG_PRICE#1	;.XXX -> .XX
	N_NUM = N_PRICE/10*10
	N_DIG = N_PRICE - N_NUM		;ENDING DIGIT .XX

	USING N_DIG SELECT
	(0),	RETURN		;ALREADY ENDS IN ZERO
	(.LE. 5),	N_PRICE = N_NUM + 5	;.05
	(.GT. 5),	N_PRICE = N_NUM + 10	;.10
	ENDUSING
	
	CONFIG_PRICE = N_PRICE * 10	;.XX -> .XXX
	RETURN
;----------------------------------------------------------

R_TAKEOFF,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	USING LITMNO(3,3) SELECT
	('V'),		BEGIN			;VINYL
			RTVAL = LITMNO(4,9)
			CLEAR LITMNO(4,15)
			VYNL = $TRUE
			END
	('.'),		RETURN			;"RT." is a valid part #
	(),		BEGIN			;NOT VINYL
			RTVAL = LITMNO(3,8)
			CLEAR LITMNO(3,15)
			VYNL = $FALSE
			END
	ENDUSING

	RTLEN = %TRIM(RTVAL)
	IF(RTLEN .LT. 6) GOTO R_TAKEOFF_ERROR

	ONERROR R_TAKEOFF_ERROR
	RT_SZ1 = RTVAL(1,2)
	RT_SZ2 = RTVAL(3,4)
	RT_DIA = RTVAL(5,6)
	OFFERROR

	IF(RT_SZ2 .GT. RT_DIA) GOTO R_TAKEOFF_ERROR

; find the greater of SZ1 & SZ2 to determin the gauge...
	RTLEN = RT_SZ1
	IF(RT_SZ2 .GT. RTLEN) RTLEN = RT_SZ2

	IF (VYNL .EQ. $TRUE)
	THEN	BEGIN
		USING RTLEN SELECT
		(.GE. 25),	RT_GA = 20
		(17 THRU 24),	RT_GA = 22
		(1 THRU 16),	RT_GA = 24
		(),		GOTO R_TAKEOFF_ERROR
		ENDUSING
		IF (MAT_SEL.NE.GALV .AND. RT_GA.GT.24) RT_GA = 24
		LITMNO(4,5) = RT_GA,	'XX'
		RT_REST = 'PVS SADDLE'
		END
	ELSE	BEGIN
		USING RTLEN SELECT
		(.GE. 31),	RT_GA = 22
		(13 THRU 30),	RT_GA = 24
		(1 THRU 12),	RT_GA = 26
		(),		GOTO R_TAKEOFF_ERROR
		ENDUSING
		IF(MAT_SEL.NE.GALV .AND. RT_GA.GT.24) RT_GA = 24
		LITMNO(3,4) = RT_GA,	'XX'
		RT_REST = 'REG. T.O.'
		END

	TDESCR = TOFF

	vanitm = litmno
	call get_van		;get the price for the vanilla item

	RETURN

R_TAKEOFF_ERROR,
	OFFERROR
;;;	XCALL OLMSG(WND_1, 23,'INVALID REGISTER TAKEOFF PART NUMBER',1)
	writes (12, itmmas)

	CONFIG_ERROR = 1
	RETURN
;----------------------------------------------------------

R_ANGLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRANGL = LITMNO(4,15)
	XCALL INSTR (1,WRANGL,'-',DASH)
	XCALL INSTR (1,WRANGL,'+',INCH)	;SSQ 6-1-98

	IF (DASH.EQ.0.AND.INCH.EQ.0) GOTO R_ANGLE_ERROR		;NEITHER
	IF (DASH.NE.0.AND.INCH.NE.0) GOTO R_ANGLE_ERROR		;BOTH
	IF (WRANGL(8,12).NE.BLANKS)  GOTO R_ANGLE_ERROR
	LITMNO(4,15) = 

	TDESCR = WRANGL(1,7)	
	IF (DASH) 
		BEGIN
		 ONERROR R_ANGLE_ERROR
		TDESCR(DASH,DASH) = '/'
		DSIZ = WRANGL(1,DASH-1)
		DECMAL = WRANGL(DASH+1,DASH+3)
		 OFFERROR
		RA_SIZE = DSIZ + DECMAL
		IF (DECMAL .LE. 0) GOTO R_ANGLE_ERROR	;3-26-99 SSQ
		END
	IF (INCH) 
		BEGIN
		TDESCR(INCH,INCH) = '"'
		 ONERROR R_ANGLE_ERROR
		RA_SIZE = WRANGL(1,INCH-1)
		 OFFERROR
		IF (WRANGL(INCH+1,12) .NE. BLANKS) GOTO R_ANGLE_ERROR	;2-17-99
		END


	vanitm = litmno
	call get_van		;get the price for the vanilla item

	RETURN

R_ANGLE_ERROR,
	OFFERROR
;;;	XCALL OLMSG(WND_1, 23,'INVALID R-ANGLE PART NUMBER',1)
	writes (12, itmmas)
	CONFIG_ERROR = 1
	RETURN
;----------------------------------------------------------


SPIRAL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (KITMNO.EQ.'SX' .AND. MAT_SEL.NE.GALV)
		BEGIN
;;;		XCALL OLMSG(WND_1, 23,"DON'T SELECT MATERIAL",1)
		CONFIG_ERROR = 1
		RETURN
		END
 
	IF (KITMNO(7,7).EQ.'+')		;PROCESS INCHES
	THEN	BEGIN
		 ONERROR SPIRAL_ERROR
		INCH = KITMNO(8,10)
		 OFFERROR
		TDESCR = '__"-__GA X ___"LONG SPIRL ____'
		TDESCR(12,14) = INCH,'ZZX'
		DPND = (INCH*100) / 12
		IF (DPND(5,6).NE.0) 
		THEN DSIZ = DPND(1,4) + 1 
		ELSE DSIZ = DPND(1,4)
		END
	ELSE	BEGIN
		INCH = 
		 ONERROR SPIRAL_ERROR
		DSIZ = KITMNO(7,8)
		 OFFERROR
		TDESCR = '__"-__GA X __''SPIRAL PIPE ____'
		TDESCR(12,13) = DSIZ,'ZX'
		DSIZ = KITMNO(7,8)
		END

	IF (DSIZ.GT.30) GOTO SPIRAL_ERROR

	TDESCR(1,2) = KITMNO(3,4)	;DIA
	TDESCR(5,6) = KITMNO(5,6)	;GA

	IF (MAT_SEL .EQ. ALUM)			;SSQ 5-12-03
		BEGIN
		DGA = KITMNO(5,6)	;GA
		USING DGA SELECT
		(18),	TDESCR(5,8) = '.063'
		(20),	TDESCR(5,8) = '.050'
		(22),	TDESCR(5,8) = '.040'
		(24),	TDESCR(5,8) = '.032'
		ENDUSING
		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SSQ 8-7-02
	LITMNO(7,15) = 
	SP_SIZE = DSIZ

; 5-12-03: force price to zero if alum or sst...
	IF (KITMNO.EQ.'SG') 
	THEN	USING MAT_SEL SELECT
		(GALV),	TDESCR(27,30) = 'GALV'
		(ALUM),	BEGIN
			TDESCR(27,30) = 'ALUM'
			SP_SIZE = 0		;FORCE PRICE TO ZERO
			END
		(SST),	BEGIN
			TDESCR(27,30) = 'SST'
			SP_SIZE = 0		;FORCE PRICE TO ZERO
			END
		(PGRP),	TDESCR(27,30) = 'PGRP'
		ENDUSING
	ELSE	TDESCR(27,30) = 'PVS '


	vanitm = litmno
	call get_van		;get the price for the vanilla item

	RETURN

SPIRAL_ERROR,
	OFFERROR
	CONFIG_ERROR = 1
;;;	XCALL W_DISP(WND_1, WD_POS,23,1,WD_CLR, WDC_LIN)
	DISPLAY (15,'ITEM CODE ENTERED: ',KITMNO,     '/ ITEM CODE CALCULATED: ',LITMNO      )
;;;	XCALL OLMSG(WND_1,23,'INVALID ITEM NUMBER FOR SPIRAL',2)
	RETURN
;----------------------------------------------------------------


GET_VAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; LOOK UP VANILLA ITEM TO GET PRICE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	savprc = 
	clear itmkey
	k_item = vanitm
	k_f1 = '000'
	k_f2 = '000'
	k_f3 = '00000'

	read (1, itmmas, itmkey, keynum:1) [err=no_van]
	savprc = price
no_van,
	read (1, itmmas, rfa:itmrfa)	;back to the original record

	RETURN
;----------------------------------------------------------------


FIND_DAMPER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR LBAD_DIGIT

	PARTNO = LITMNO

	FOR I FROM 1 THRU 3
		BEGIN
		CLEAR DAMPER_PRICE(I)
		CLEAR SAVE_DP(I)
		END

	MAXFLEN = %TRIM(PARTNO)
	FOR I FROM 1 THRU 3
		BEGIN
		XCALL INSTR(1, PARTNO, "*", FL)
		IF (.NOT. FL) 		EXITLOOP
		PARTNO(FL, MAXFLEN) = PARTNO(FL+1, MAXFLEN)
		CALL D_SIZE
		IF (BAD_DIGIT)
			BEGIN
;;;			XCALL OLMSG(WND_1,23,'damper configuration',2)
			RETURN
			END
		END

	LITMNO = PARTNO
	RETURN

D_SIZE,	;---------------------------------------
	CLEAR BAD_DIGIT

	ONERROR BADDIG
	DAMP = PARTNO(FL, FL+1)
	OFFERROR

;check damper price...
	CLOSE 9

;;;	FL_NAME = 'SDPRIC'		;SSQ 9-9-03
;;;	OPEN(9,I,FILPRC)		;SSQ 9-9-03
	OPEN (9,I,'SMC:SDPRIC.SMC')
	XCALL IO (9,TPRICE,1,READ,LOKCTL)
	IF(DAMP.LE.80) 
	THEN	DAMPER_PRICE(I) = ARRAY(DAMP)*10
	ELSE	CLEAR DAMPER_PRICE(I)

	CLOSE 9
	IF (DAMPER_PRICE(I) .LE. 0)
		BEGIN
;;;		XCALL OLMSG(WND_1,23,'Zero or missing damper price',2)
		GOTO BADDIG
		END
	LDAMPR = 1
	RETURN
BADDIG,
	CLEAR DAMPER_PRICE(I)
	BAD_DIGIT = 1
	RETURN

GET_DAMPER_SECTIONS,
; new logic - look up dimensions in table...


	DHARA(1) = DHT
	CLEAR TBL_KEY
	TBLCOD = 'SZ'
	SZ_KEY = DHT	;FIND DIM'S FOR THIS HGHT
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
		BEGIN
		DHARA(1) = SZ_S1
		DHARA(2) = SZ_S2
		DHARA(3) = SZ_S3
		END

	DLARA(1) = DLEN
	CLEAR TBL_KEY
	TBLCOD = 'SZ'
	SZ_KEY = DLEN	;FIND DIM'S FOR THIS HGHT
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
		BEGIN
		DLARA(1) = SZ_S1
		DLARA(2) = SZ_S2
		DLARA(3) = SZ_S3
		END
	RETURN
;--------------------------------------------------------

ADD_DAMPER_PRICE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FOR I FROM 1 THRU 3 
		BEGIN
		LPRICE = LPRICE + DAMPER_PRICE(I)
		SAVE_DP(I) = DAMPER_PRICE(I)	;SSQ 12/23/02
		DAMPER_PRICE(I) = 		;ssq 10/16/02
		END
	if (itemno(1,2) .eq. 'SW')
		begin
		entry = 'hi'
		end
	RETURN
;---------------------------------------------------	

CFG_PRICE_ROUTINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;===================================================
; ------- Configuration Price Adjustments -------
	USING LITMNO SELECT
	('WR'),		BEGIN		;R_ANGLE
			IF (DASH)
			   BEGIN
			   WTPIN = ((WEIGHT*1000)/120)
			   DPND = (WTPIN * RA_SIZE)#3
			   PRPPN = ((savprc*1000)/WEIGHT)
			   LPRICE = (PRPPN * DPND)#3
			   LPRICE = ((PRPPN * DPND)#3 * CMX)#3 *10	;SSQ 9-16-04
			   END
			IF (INCH)
			   BEGIN
			   WTPIN = ((WEIGHT*10000)/120)/2
			   DPND = (WTPIN * RA_SIZE)#3
			   PRPPN = ((savprc*1000)/WEIGHT)
			   LPRICE = (PRPPN * DPND)#4
			   LPRICE = ((PRPPN * DPND)#4 * CMX)#3 *10
			   END
			END
;;;	('SP','SPV'),	BEGIN	;SPIRAL PIPE	;SSQ 8-7-02
	('SG','SX'),	BEGIN	;SPIRAL PIPE
	  		LPRICE = (SP_SIZE * savprc)
	  		LPRICE = ((SP_SIZE * savprc) * CMX)#3 * 10
			LUOFM = 'EA'
			END
	('RT','RTV'),			LPRICE = (savprc*(RT_SZ1+RT_SZ2)*CMX)#3 * 10
	('T4','T9','TV4','TV9'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('C9','C4','CV9','CV4'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('CT','CC','CTV','CCV'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('ER'),				LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('BN'),				LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('P4','P9','PV4'),		LPRICE = (CONFIG_PRICE * CMX)#3 * 10

	('R16'  THRU  'R26'),		LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('RV16' THRU 'RV26'),		LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('S9','S4','SV','SW','SC','ST'), LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('OB','PB','SBD','BDD'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('SLF','SLB'),			LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	ENDUSING

	RETURN
;---------------------------------------------------	
make_pp,
;Create presssed parts work file...
	XCALL ISAMC (WRKFIL, 15, 1, 'START=1, LENGTH=15, NODUPS, ASCEND')
	OPEN (33, SU, WRKFIL)
	CHNPP = 33

	clear coptbl
	tblcod = 'PP'
	find (chn182, coptbl, tbl_key) [err=mp_loop]
mp_loop,
	reads (chn182, coptbl, mp_eof)
	if (tblcod .ne. 'PP') return
	p_item = pp_part
	store (chnpp, pp_rec, p_item) [err=mp_loop]
	goto mp_loop
mp_eof,
	return
;-----------------------------------------------------
ADD_MATL_MULT,	;;;;;;;;;;;;;;;;;;;;;;;;;

	LPRICE=(SAVPRC*P_MULT)#2
	CONFIG_PRICE=LPRICE
	CALL ROUND_NICKLE
	LPRICE=CONFIG_PRICE
	RETURN
;---------------------------------------------------	
opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

	switch = 5
	xcall files (1, 'SU', 41, switch)	;41 = itmmas
	if (switch .eq. 9) return
	chn041 = 1

;;;	open (1,su,'smc:itmmas.smm')
;;;	open (1,su,'tst:itmmas.tsm')
	open (10,o,'mak_prc.dat')
	o_old = 'Old' [right]
	o_new = 'New' [right]
	writes (10,out)

	open (12,o,'prc_err.dat')
	open (17,si,'smc:coptbl.smm')

	chn182 = 17

	opnok = 1
	return
;--------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (chn041) close chn041
	if (chn182) close chn182
	close 10
	close 12

	return
;--------------------------------------------
