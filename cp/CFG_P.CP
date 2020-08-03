;cfg_p.cp	;config price routine

.subroutine cfg_p
	p_item	,a		;incoming item
	p_f1	,a
	p_f2	,a
	p_f3	,a
	p_price	,d		;price returned
	p_err	,d	

RECORD	INVMAS
	.INCLUDE 'DEF:RD041A.DEF'

RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	DMPRIC
	.INCLUDE 'DEF:DMPRIC.DEF'

RECORD	FILPRC
	FL_DEV	,A3
		,A1,	':'
	FL_NAME	,A6
		,A1,	'.'
	FL_EXT	,A3

RECORD	MATERIALS
	.INCLUDE 'DEF:CFGMAT.DEF'

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
	

RECORD	VDAMPR
	DAMP		,D2	;DAMPER DIMENSIONS
	DAMPER_PRICE	,3D8
	SAVE_DP		,3D8
	FL		,D2	;
	MAXFLEN		,D2
	BAD_DIGIT	,D1
	PARTNO		,A15
	PFF		,A15

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
	chnstp	,d2
	chninv	,d2

record	vals
	litmno		,a15
	lprice		,d8
	luofm		,a2
	ldescr		,a30
	ldampr		,d1
	cmx		,d3		;contractor multiplier
	config_price	,d8
	config_error	,d1
	mat_sel		,d1
;	st_guage	,d2
	st_guage	,a2


	
record	vars
	chn	,d3
	opnok	,d1
	i	,d6
	bf	,d1
	key	,a15
	lokctl	,d1
	read	,d1,0
	decmal	,d18
	cfg_item	,d1
	ivflag	,d1	;0=found, 1=not found, 2=abort, 3=bad price
	kitmno	,a15
	blanks	,a30
	RTLEN	,D2
	RTVAL	,A6
	VYNL	,D1
	DPND	,D6
	DSIZ	,D3
	DGA	,D2
	RA_SIZE	,D3		;R_ANGLE SIZE
	SP_SIZE	,D3		;SPIRAL SIZE
	WTPIN	,D6
	PRPPN	,D6
	WRANGL	,A12
	INCH	,D3
	DASH	,D2
	BADTPR	,D1
	switch	,d1
;
.proc
	
	call opens
	if (.not. opnok) goto endoff

	litmno = p_item

	clear config_error, ivflag
	cmx = 100

	call get_mat

	call item

	call cfg_price_routine
	
	if(config_error .or. ivflag) 
	then	p_err = 1
	else	begin
		p_err = 0
		p_price = lprice
		end

endoff,
	call close
	xreturn
;=============================================================

get_mat,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	MAT_SEL = 1		;GALV
	CLEAR TBL_KEY
	TBLCOD = 'M1'
	XCALL ALPDC (P_F1, MM_KEY, BF)
	IF (BF .NE. 0) RETURN
	IF (MM_KEY .LE. 0) RETURN

	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .eq. 0) MAT_SEL = MM_MATL	;2-9-17
;;;	IF (LOKCTL .NE. 0) MAT_SEL = MM_MATL


	IF (MAT_SEL.LT.1 .OR. MAT_SEL.GT.4) MAT_SEL = 1
	return
;---------------------------------------------------------------

item,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; re-price  item using scrnx logic...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (config_error .or. ivflag) return

	kitmno = litmno

	CLEAR TDESCR
	SP_SIZE = 1				;in case sph

	
	USING LITMNO SELECT
	('WR'),				CFG_ITEM = 1
	('RW16' THRU 'RW20'),		CFG_ITEM = 1
	('R16' THRU 'R26'),		CFG_ITEM = 1
	('RV16' THRU 'RV26'),		CFG_ITEM = 1
	('T4','T9','TV4','TV9'),	CFG_ITEM = 1
	('C4020','C4420','C4620'),	CFG_ITEM = 2	;SSQ 10-14-03
	('C4820','C4220 '),		CFG_ITEM = 2	;SSQ 10-14-03
	('C9','C4','CV9','CV4'),	CFG_ITEM = 1
	('CT','CC','CTV','CCV'),	CFG_ITEM = 1
	('ER'),				CFG_ITEM = 1
	('BN'),				CFG_ITEM = 1	
	('P4','P9','PV4'),			CFG_ITEM = 1
	('S400' THRU 'S415'),		CFG_ITEM = 2	;5-26-11 PRESSED SADDLES
	('S900' THRU 'S915'),		CFG_ITEM = 2	;5-26-11 PRESSED SADDLES
	('S9','S4','SV','SW','SC','ST'),	CFG_ITEM = 1
	('SG','SX'),			CFG_ITEM = 1
	('RT','RTV'),			CFG_ITEM = 1
	('OB','PB','SBD','BDD'),	
					BEGIN
					CFG_ITEM = 1
					GOTO CALL_CONFIG
					END
	('SLF','SLB'),			BEGIN		;LOUVERS
					CFG_ITEM = 1
					GOTO CALL_CONFIG
					END
	(),				CFG_ITEM = 2	;item not configurable
	ENDUSING

	GOTO (REG_ITEM),CFG_ITEM -1

	CALL GETITM			;check if cfg item is in itmmas...
;;;	GOTO (SKIP_CFG,CALL_CONFIG,ITEM),IVFLAG+1	;(found,not-found,abort)
CALL_CONFIG,
	CALL CONFIG			;call the configurator
	GOTO (ITEM), CONFIG_ERROR

REG_ITEM,
	CALL GETITM
	GOTO (ITEM, ITEM, ITEM),IVFLAG
;;;	GOTO (BADITM, ITEM, ITEM),IVFLAG

SKIP_CFG,

	return
;-----------------------------------------------------------

GETITM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR IVFLAG

	USING LITMNO SELECT
	  ('CM'),		BEGIN
				KEY = LITMNO(1,3)
			;;;	CALL GI_TEMP
				END

	  ('CR0' thru 'CR9'),	BEGIN
				KEY = LITMNO(1,4)
			;;;	CALL GI_TEMP		;READ ITMMAS
				END

	  (),			KEY = LITMNO
	ENDUSING


	CLEAR ITMKEY
	K_ITEM = KEY
	K_F1 = '000'
	K_F2 = '000'
	K_F3 = '00000'

	read (chninv, invmas, itmkey, keynum:1) [err=no_item]
	lprice = price	;for non-cfg items
	p_price = price
	return
no_item,
	ivflag = 1
	return
	
;----------------------------------------------------

CONFIG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; BRANCH TO PRODUCT CONFIGURATORS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
	('RW16' THRU 'RW20'),		CALL CALL_CFG
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

; if it gets this far, then all *'s need to be stripped from itemno, since
; it can't be existing itmmas item...

	IF (LDAMPR) litmno = PARTNO

	XCALL CFG(LITMNO,CONFIG_PRICE,TDESCR,CONFIG_ERROR,MAT_SEL,ST_GUAGE)
;;;	IF (CONFIG_ERROR .EQ. 1) XCALL OLMSG(WND_1, 23,'INVALID CONFIGURATION',2)
	RETURN
;----------------------------------------------------
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
			   PRPPN = ((PRICE*1000)/WEIGHT)
			   LPRICE = (PRPPN * DPND)#3
			   LPRICE = ((PRPPN * DPND)#3 * CMX)#3 *10	;SSQ 9-16-04
			   END
			IF (INCH)
			   BEGIN
			   WTPIN = ((WEIGHT*10000)/120)/2
			   DPND = (WTPIN * RA_SIZE)#3
			   PRPPN = ((PRICE*1000)/WEIGHT)
			   LPRICE = (PRPPN * DPND)#4
			   LPRICE = ((PRPPN * DPND)#4 * CMX)#3 *10
			   END
			END
	('SG','SX'),	BEGIN	;SPIRAL PIPE
	  		LPRICE = (SP_SIZE * PRICE)
	  		LPRICE = ((SP_SIZE * PRICE) * CMX)#3 * 10
			LUOFM = 'EA'
			END
	('RT','RTV'),			LPRICE = (PRICE*(RT_SZ1+RT_SZ2)*CMX)#3 * 10
	('T4','T9','TV4','TV9'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('C9','C4','CV9','CV4'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('CT','CC','CTV','CCV'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('ER'),				LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('BN'),				LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('P4','P9','PV4'),		LPRICE = (CONFIG_PRICE * CMX)#3 * 10

	('RW16'  THRU  'RW20'),		LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('R16'  THRU  'R26'),		LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('RV16' THRU 'RV26'),		LPRICE = (CONFIG_PRICE * CMX)#3 * 10

	('S400' THRU 'S415'),		nop	;5-26-11 PRESSED SADDLES
	('S900' THRU 'S915'),		nop	;5-26-11 PRESSED SADDLES
	('S9','S4','SV','SW','SC','ST'), LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('OB','PB','SBD','BDD'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('SLF','SLB'),			LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	ENDUSING

	RETURN
;-----------------------------------------------------------------
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
	XCALL ISIO (chninv, INVMAS, LITMNO, READ, LOKCTL)	;need this for price
	IF (LOKCTL .NE. 0) GOTO R_TAKEOFF_ERROR		;bad item
	RETURN

R_TAKEOFF_ERROR,
	OFFERROR
;;;	XCALL OLMSG(WND_1, 23,'INVALID REGISTER TAKEOFF PART NUMBER',1)
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

	XCALL ISIO (chninv, INVMAS, LITMNO, READ, LOKCTL)	;need this for price
	IF (LOKCTL .NE. 0) GOTO R_ANGLE_ERROR		;bad item
	RETURN

R_ANGLE_ERROR,
	OFFERROR
;;;	XCALL OLMSG(WND_1, 23,'INVALID R-ANGLE PART NUMBER',1)
	CONFIG_ERROR = 1
	RETURN
;----------------------------------------------------------


SPIRAL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (KITMNO.EQ.'SX' .AND. MAT_SEL.NE.GALV)
		BEGIN
	;;;	XCALL OLMSG(WND_1, 23,"DON'T SELECT MATERIAL",1)
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


	XCALL ISIO (chninv, INVMAS, LITMNO, READ, LOKCTL)	;need this for price
	IF (LOKCTL .NE. 0) GOTO SPIRAL_ERROR		;bad item
	RETURN

SPIRAL_ERROR,
	OFFERROR
	CONFIG_ERROR = 1
;;;	XCALL W_DISP(WND_1, WD_POS,23,1,WD_CLR, WDC_LIN)
;;;	DISPLAY (15,'ITEM CODE ENTERED: ',KITMNO,     '/ ITEM CODE CALCULATED: ',LITMNO      )
;;;	XCALL OLMSG(WND_1,23,'INVALID ITEM NUMBER FOR SPIRAL',2)
	RETURN
;----------------------------------------------------------------

FIND_DAMPER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;03/08/07 isam: now itmmas.item contains the *'s and is priced to reflect
;;;		the fact that there are dampers. just set ldampr.

	if (%instr(1, litmno, "*") )
	then	ldampr = 1
	else	ldampr = 0

;;;	return	;10-16-07 get partno but leave litmno alone
;------------------------------------------------------------------

	CLEAR LDAMPR, BAD_DIGIT

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
	;;;		XCALL OLMSG(WND_1,23,'damper configuration',2)
			RETURN
			END
		END

;;;	LITMNO = PARTNO
	RETURN

D_SIZE,	;---------------------------------------
	CLEAR BAD_DIGIT

	ONERROR BADDIG
	DAMP = PARTNO(FL, FL+1)
	OFFERROR

;check damper price...
	CLOSE chnstp

	FL_NAME = 'SDPRIC'		;SSQ 9-9-03
	OPEN(chnstp,I,FILPRC)		;SSQ 9-9-03
	XCALL IO (chnstp,TPRICE,1,READ,LOKCTL)
	IF(DAMP.LE.80) 
	THEN	DAMPER_PRICE(I) = ARRAY(DAMP)*10
	ELSE	CLEAR DAMPER_PRICE(I)

	CLOSE chnstp
	IF (DAMPER_PRICE(I) .LE. 0)
		BEGIN
	;;;	XCALL OLMSG(WND_1,23,'Zero or missing damper price',2)
		GOTO BADDIG
		END
	LDAMPR = 1
	RETURN
BADDIG,
	CLEAR DAMPER_PRICE(I)
	BAD_DIGIT = 1
	CLEAR LDAMPR
	RETURN
;----------------------------------------------------------------

CFG_DAMPER,	;LOUVERS

	return	;later
;---------------------------------------------------------------

opens,
	clear opnok

	for chn from 20 thru 100
		begin
		if (.not. %chopen(chn)) exitloop
		end
	chninv = chn
	switch = 5
	xcall files (chninv, 'SI', 041, switch)
	if (switch .eq. 9) return

	for chn from chninv thru 100
		begin
		if (.not. %chopen(chn)) exitloop
		end
	chnstp = chn

	for chn from chnstp thru 100
		begin
		if (.not. %chopen(chn)) exitloop
		end
	chn182 = chn
	switch = 5
	xcall files (chn182, 'SI', 182, switch)
	if (switch .eq. 9) return

	opnok = 1
	return
close,
	if (chninv) close chninv
	if (chnstp) close chnstp
	if (chn182) close chn182
	return

.end

