subroutine g_item
	p_item		,a
	p_itmmas	,a	;return the record?
	p_ordlin	,a
	p_cpmemo	,a	;
	p_matsel	,d
	p_lmsq1		,d
	p_lmsq2		,d
	p_lmsq3		,d
	p_ga		,d	;need for lpipga
	p_qty		,d	;qtyord from original line
	p_chn041	,d	;optional ch for itmmas
	p_chn182	,d	;optional ch for coptbl


record	invmas
	.include 'def:rd041a.def'
record	itmkey
	.include 'def:rd041k.def'

record	ordlin	
	.include 'def:rd045a.def'

record	coptbl
	.include 'def:rd182a.def'

record	cpmemo
	.include 'def:cpmemo.def'

RECORD	FILPRC
	FL_DEV	,A3
		,A1,	':'
	FL_NAME	,A6
		,A1,	'.'
	FL_EXT	,A3

RECORD	
	D_LONG	,A90
	D_SHORT	,A12
	D_KEY	,D5		;SSQ 7-31-03
	D_F6M	,D1		;SSQ 10-13-07
	D_CDPT	,D1		;SSQ 12-26-07
	D_IK	,D1		;SSQ 05-15-08
	D_SS	,D1		;SSQ 05-13-10
RECORD,X
	DL_ARA	,3A30		;ssq 1-03-08, not sure why this was 31...

RECORD	MATERIALS
	.INCLUDE 'DEF:CFGMAT.DEF'


RECORD TPRICE
	ARRAY	,80D5

RECORD	VDAMPR
	DAMP		,D2	;DAMPER DIMENSIONS
	DAMPER_PRICE	,3D8
	SAVE_DP		,3D8
	FL	,D2	;
	MAXFLEN	,D2
	BAD_DIGIT	,D1
	PARTNO	,A15
	PFF	,A15

RECORD TEEDAT
	TGUAG	,D2
	TMAIN	,D2
	TREDU	,D2
	TBRAN	,D2
	TDESCR	,A30

RECORD
	F3_TMP	,D4
RECORD,X
	F3_VAL	,4D1	;VALUE OF EACH F3 SEG
RECORD
	F3_WORK	,D4
RECORD,X	
	F3_ARA	,4D1
RECORD
	SEGS	,A12			;first 2 dig are gauge
RECORD,X
	GA	,D2
	SARA	,5D2


record	channel
	chn182	,d2, 17
	chn041	,d2


record	vars
	A2	,A2
	A4	,A4
	A5	,A5
	A15	,A15
	D2	,D2
	ivflag	,d1
	xf1	,d3
	witmno	,a15
	a_lf1	,a3
	a_lf2	,a3
	a_lf3	,a5
	f1_tmp	,d3
	f2_tmp	,d3
	f2_tmem2	,a30
	no_ss	,d1		;1=safety seal not allowed
	custom	,d1		;1=custom (f2=31) note running
	D5	,D5
	ss_running	,d1	;1=ss not turned on, but is running
	SS_ON	,D1		;1=SAFETY SEAL NOTE IS RUNNING
	save_ss_on	,d1	;save status of "ss_on" for f5_pop 1-27-14
	turn_off_ss	,d1	;default notes can't turn ss_on 
	SS_ERR	,D1		;1=BAD DIMENSION FOR SAFETY SEAL
	LN	,D3
	TN	,D3
	vanflg	,d1
	spiral	,d1,	1	;f3 note for spiral
	cmpcod	,a3
	is_pressed	,d1	;1 = is a pressed part
	SWFAB	,D1		;1=no exact match, set dept to prdcat 1-18-08
	CDPT	,D1		;1=change dept if this <F2> note running
	CDPT1	,D1		
	CDPT2	,D1		
	
	M_MAX	,D2		;input max for memos, see "INSERT,"
	EM_CONT	,D1		;0=no exact match, continue/ 
				;1=no exact match, don't continue
	DF1	,D3		;default key1 (from coptbl)
	DF2	,D3		;default key2
	DF3	,D5		;default key3
	TOP	,D2		;TOP OF REDUCER
	BOT	,D2		;BOTTOM OF REDUCER
	l_pp		,a15	;save pressed part #
	exact_match	,d1	;exact match w/ all notes
	is_fake		,d1	;1 = found in fake item table
	fake_ga		,d2
	fake_descr	,a30	;description of fake item
	fake_price	,d8	;price of fake item 12-28-09
	van_price	,d8
	van_wgt		,d6	;production weight
	van_descr	,a30	;description of vanilla item
	van_cat		,a2	;prdcat for vanilla item
	van_isafes	,a1	;isafes Y/N
	vanitm		,d1	;1 = item w/ zero notes was found
	vanrfa		,a6	;rfa of vanilla item
	defrfa		,a6	;rfa of default item
	girfa		,a6	;see "trec"
	cmrfa		,a6	;for 'CR..'
	is_dist	,d1	;1 = is a dist
	C_ITEM	,A15	;FOR CFG() OR CFG2()
	KEYCOD	,A2
	savcfg	,d1	;save icfg 5-1-07
	BF	,A1	;BOX/FLANGE  SSQ 1-24-06
	CMX	,D3	;X.XX non-contractor multiplier
	RI	,D6
	str	,a20
	IS_15		,D1	;NOTE .15 IS RUNNING (EZ FLANGE)

	F6A	,5A4,	'GALV','ALUM','SST ','PGRP','PVS '

	BAD_MAT	,D1			;INVALID PART FOR THIS MAT'L
	MAT_SEL	,D1			;MATERIAL SELECTED
	MAT_MUL	,D4			;MATERIAL MULTIPLIER
	DDIM	,D3
	TL	,D6
	TEMP	,D6
	TEMP2	,D6
	DQTY	,D3
	CFG_ON	,D1
	CFG_ITEM	,D1	;1=CONFIGURABLE ITEM, 2=REGULAR ITEM
	ENTDES	,A30	;OVER-RIDE DESCRIPTION
	ABSQTY	,D6
	RTLEN	,D2
	RTVAL	,A6
	VYNL	,D1
	X_MEMO	,A90
	P_MULT	,D3
	X_MULT	,D3
	SAVMLT	,D3
	SAV_OP	,D8
	SAVPRC	,D8
	TODAY	,D8
	SL	,D2		;SEARCH LENGTH
	CMTFLG	,D1
	COMIT	,D1,0
	UN_COMIT,D1,1
	CNG_LI	,D1
	NEWITEM	,D1		;new item# when changing line items
	LINRFA		,A6
	MM_CODE		,D5		;ssq 7-31-03
	V_LEVEL		,D1
	KEY_FOUND	,D1
	KF1		,D3	;used by getitm
	KF2		,D3	;used by getitm
	KF3		,D5	;used by getitm
	XL1		,D2
	XL2		,D2
	XL3		,D2
	CLVL1		,D2
	CLVL2		,D2
	CLVL3		,D2
	CLVL4		,D2
	SAVL1		,D2
	SAVL2		,D2
	SAVL3		,D2
	SAVL4		,D2
	F_KEY	,D3
	T_REC	,D5
	VINYL		,D1
	CONFIG_ERROR	,D1
	A_GUAGE		,A2
	ST_GUAGE	,A2
	IS_DAMPER	,D1
	PIDX	,D5
	I	,D5
	J	,D5
	X	,D5
	Y	,D5
	NUMITM	,D2
	ORIG_PRICE	,D8
	CONFIG_PRICE	,D8
	JUSTIF	,D1
	WRKDAT	,D6
	OPTION	,D1
	ENTRY	,A36
	INXCTL  ,D1
	CNGCTL	,D1
	WHATNO	,D2
	DECMAL	,D18
	KEY   	,A15
	ORDNOA	,A6
	ODISCA	,A2
	RECNO	,D5
	CTR	,D2
	MSG	,A17
	ALPHA	,A10
	BLANKS	,A25
	BLANK30	,A30
	BLANK1	,A80
	BADBIL	,D1
	BRACKS	,A15,	']]]]]]]]]]]]]]]'
	READ	,D1,	0	;USED FOR IO SUBROUTINE
	WRITE	,D1,	1	;USED FOR IO SUBROUTINE
	STORE	,D1,	2	;USED FOR ISIO SUBROUTINE
	LOKCTL	,D1
	SAVITM	,A15	;SAVE THE LAST ITEM NUMBER *** NOTE SIZE ***
	SYSTEM	,D1
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
	NUMASK	,A7,'ZZ,ZZZ-'
	KITMNO	,A15
	SWITCH	,D1

proc

	chn041 = p_chn041
	chn182 = p_chn182

	cmx = 100		;non-contractor
	cpmemo = p_cpmemo
	mat_sel = p_matsel

	clvl3 = f3_idx		;current f3 note

	goto process_item

baditm,
item,
	;this means there was an error!
	xreturn
;======================================================

process_item,

	ORDLIN =

	LMSQ1 = P_LMSQ1
	IF (LMSQ1) LF1 = F1_KEY(LMSQ1)


	LMSQ3 = P_LMSQ3
	IF (LMSQ3) LF3 = F3_KEY(LMSQ3)

	LMSQ2 = P_LMSQ2
	IF (LMSQ2) LF2 = F2_KEY(LMSQ2)

	LPIPEG = P_GA

	IF (CLVL3.GT.0)			;SSQ 9-9-03
	then	BEGIN
		IF (F3_KEY(CLVL3).EQ.3)		;3-31-10
		THEN	IS_15 = 1
		ELSE	IS_15 = 0
		END
	else	is_15 = 0
	


	CLEAR CFG_ON, CFG_ITEM
	CLEAR BADTPR, LITMWT, LTXFLG,  DASH, INCH
	clear lpwgt
	CLEAR ST_GUAGE
	LITMNO = p_item
	LCFGIM = LITMNO			;SSQ 7-29-03 SAVE ITEM#

;------------------------------------------------------------
; 11-18-09: check the fake item table...

	clear is_fake, fake_descr, fake_price, fake_ga

	clear coptbl
	tblcod = 'FP'
	fp_item = litmno
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .eq. 0)
		begin
		clear itmkey
		k_item = litmno
		k_f1 = '000'
		k_f2 = '000'
		k_f3 = '00000'
		read (CHN041, invmas, itmkey, keynum:1) [err=no_f_item1]
		fake_price = price	;12-28-09	use price from item input
	no_f_item1,
		litmno = fp_part	;real item
		fake_descr = fp_desc	;fake description
		fake_ga = fp_ga		;fake ga
		is_fake = 1
		end

;------------------------------------------------------------

	CALL FIND_DAMPER
	GOTO (ITEM),BAD_DIGIT

	IF (LITMNO.EQ.BLANKS) GO TO ITEM


	KITMNO = LITMNO					;;;

	CLEAR TDESCR, CONFIG_ERROR
	SP_SIZE = 1				;in case sph
	
	
	USING LCFGIM SELECT	;in case litmno replaced by pressed part
	('RW16' THRU 'RW20'),		CFG_ITEM = 1
	('T4','T9','TV4','TV9'),	CFG_ITEM = 1
	('C4020','C4420','C4620'),	CFG_ITEM = 2	;SSQ 10-14-03
	('C4820','C4220 '),		CFG_ITEM = 2	;SSQ 10-14-03
	('C9','C4','CV9','CV4'),	CFG_ITEM = 1
	('CT','CC','CTV','CCV'),	CFG_ITEM = 1


	('S9','S4','SV','SW','SC','ST'),	CFG_ITEM = 1
	('SG','SX'),			CFG_ITEM = 1
	(),				CFG_ITEM = 2	;item not configurable
	ENDUSING



;-------------------------------------------------------
; DEFAULT KEY CODE:
;	light-up default keys unless:
;	a) a f1 note is running , or
;	b) a f3 note is already running.

	clear tbl_key
	tblcod = 'IK'
	ik_item = litmno
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
;todo	if (lokctl .ne. 0) call chk_pvs	;1-8-15 special case for vinyl
	if (lokctl .ne. 0) goto skip_ik	;still no match

	clear df1, df2, df3		;these will be the default keys
	clear d_long			; 7-19-16
		
	df3 = ik_f3			;always need this
	df1 = ik_f1

	if (clvl1 .gt. 0) df1 = f1_key(clvl1)
	if (df1 .eq. 0) df1 = ik_f1	;don't replace f1-memo

	if (clvl2 .gt. 0)
		begin
	 	df2 = f2_key(clvl2)
		d_long = f2_memol(clvl2)	;7-19-16
		end

	if (df2 .eq. 0) df2 = ik_f2	;don't replace f2-memo

	if (clvl3 .gt. 0) df3 = f3_key(clvl3)
	if (df3 .eq. 0) df3 = ik_f3	;don't replace f3-memo


	if (ik_f2 .gt. 0)		;5-16-13
		begin
		f2_tmp = df2
		f2_tmem2 = dl_ara(2)	;7-19-16
		call check_f2_note
		end


skip_ik,
	GOTO (REG_ITEM),CFG_ITEM -1

	XCALL CFG2 (PARTNO, SEGS)		;get ga & segs


	CALL GETITM
	IF (EXACT_MATCH .and. IS_PRESSED)
		BEGIN
		CFG_ITEM = 2	;Pressed is not cfg
		GOTO SKIP_CFG		;pressed parts
		END

	IF (EXACT_MATCH .and. SAVCFG.EQ.2) 
		BEGIN
		CFG_ITEM = 2	;this will prevent re-pricing
		GOTO SKIP_CFG	;not cfg'd
		END

	IF (CFG_ITEM.EQ.1 .AND. MAT_SEL.NE.GALV) GOTO CALL_CONFIG


CALL_CONFIG,
	CALL CONFIG			;call the configurator
	GOTO (ITEM), CONFIG_ERROR
	IF (EXACT_MATCH.OR.IVFLAG.EQ.0) GOTO SKIP_CFG	;2-12-08

	XCALL ISIO (CHN041, INVMAS, LITMNO, READ, LOKCTL)	;just read instead of getitm
	IF (LOKCTL .NE. 0) lokctl = 0	;todo: some kind of error

	GOTO SKIP_CFG

REG_ITEM,
	CALL GETITM
	GOTO (BADITM, ITEM, ITEM),IVFLAG

SKIP_CFG,
	LPWGT = VAN_WGT		;6-28-17

	LITMNO = ITEMNO
	SAVITM = ITEMNO

	IF (TDESCR .EQ. BLANKS)		;was desc created in configurator ?
	THEN	LDESCR = van_DESCR
	ELSE	LDESCR = TDESCR

;fill in ordlin...
	LQTYOR = P_QTY
	LPRDCD = PRDCAT
	LITMWT = WEIGHT
	LDEPT = USRDEF
	LSRTSQ = PRICCD			;3-29-00 SSQ
	LUOFM = SUOFM

	LPRICE = van_PRICE	;DEFAULT TO ITMMAS PRICE
	LPRICE = (van_PRICE*CMX)#3*10	;SSQ 9-16-04
	call CFG_PRICE_ROUTINE

;---
	USING ST_GUAGE SELECT
	('20','18','16'),
		BEGIN
		LDEPT = 'K'
		LPRDCD = 'K3'
		END
	ENDUSING

	USING LCFGIM SELECT	;3-17-15
	('T420', 'T920'),	LDEPT = 'K'
	('T418', 'T918'),	LDEPT = 'K'
	('T416', 'T916'),	LDEPT = 'K'
	ENDUSING

	USING LITMNO SELECT
	('RW20','RW18','RW16'),	LDEPT = 'K'
	ENDUSING
;---
	p_itmmas = invmas
	p_ordlin = ordlin
	xreturn				;this is the end
;--------------------------------------------------------------------
;====================================================================


CONFIG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; BRANCH TO PRODUCT CONFIGURATORS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TDESCR=LDESCR			;SAVE IN CASE USER HAS CHANGED
	CONFIG_PRICE = LPRICE		;ssq 10/16/02 moved here
	IF(CFG_ON.EQ.1) RETURN		;ALREADY CONFIGRED
	CFG_ON = 1


	CLEAR CONFIG_ERROR

	USING LITMNO SELECT
	('C4020','C4420','C4620'),	RETURN	;SSQ 2-19-03
	('C4820','C4220 '),		RETURN	;SSQ 2-19-03
	('RW16' THRU 'RW20'),		CALL CALL_CFG
	('T4','T9','TV4','TV9'),	CALL CALL_CFG
	('C9','C4','CV9','CV4'),	CALL CALL_CFG
	('CT','CC','CTV','CCV'),	CALL CALL_CFG

	('S9','S4','SV','SW','SC','ST'),CALL CALL_CFG
	('SG','SX'),			CALL SPIRAL
	ENDUSING
	RETURN
;----------------------------------------------------

CALL_CFG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; internal routine so I can see parameter list...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; if it gets this far, then all *'s need to be stripped from itemno, since
; it can't be existing itmmas item...

	IF (LDAMPR) litmno = PARTNO
	clear vanitm		;if we got this far we're not using the vanitm...

	if (fake_ga .gt. 0) st_guage = fake_ga, 'XX'
	witmno = litmno		;2-2-15
	XCALL CFG(LITMNO,CONFIG_PRICE,TDESCR,CONFIG_ERROR,MAT_SEL,ST_GUAGE)
	IF (CONFIG_ERROR) 
	then	begin
		config_error = 0	;todo - error		using config_error select
		end
	else	xcall cfg_w (witmno, lpwgt, config_error,mat_sel,st_guage)
	RETURN
;----------------------------------------------------

GETITM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; NOW A ROUTINE, LOOK UP ITEM IN ITMMAS...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vanitm = item w/ all note keys = zero
; exact_match = exact match


	CLEAR IVFLAG, VANITM, EXACT_MATCH, SAVCFG
	CLEAR DF1, DF2, DF3
	clear swfab
	clear van_price
	clear van_descr
	clear van_cat
	clear van_isafes
	clear van_wgt

; read itmmas based on partial key for matching items below...
; 11-29-07: this partial items don't have f-note keys, so after gi_temp
; there's nothing else to do but exit...
	KEY = LITMNO


; Configured items, check if vanilla item exists...
	CLEAR ITMKEY
	K_ITEM = LITMNO
	K_F1 = '000'
	K_F2 = '000'
	K_F3 = '00000'

	read (CHN041, invmas, itmkey, keynum:1, getrfa:vanrfa) [err=no_itm_2]
	vanitm = 1				;vanilla item was found
	savcfg = icfg				;remember if cfg or not.
	van_price = price
	van_wgt = ip_wgt			;6-28-17 production weight
	girfa = vanrfa				;in case this is the item to update
	van_descr = descr			;11-16-09
	van_cat = prdcat
	van_isafes = isafes			;11-08-10

	if (is_fake .eq. 1) 
		begin
		van_descr = fake_descr	;11-18-09
		van_price = fake_price	;12-28-09
		end

	if (inn .eq. 1) 
		begin
		exact_match = 1
		goto gi_done		;ignore key values
		end


; check for exact match...
no_itm_2,
; first, check for default keys...
	CLEAR TBL_KEY		;is there a default key for this item?
	TBLCOD = 'IK'		
	IK_ITEM = LITMNO
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NO_DEFAULT]
	DF1 = IK_F1		;note that these values will 
	DF2 = IK_F2		;be overridden if notes are
	DF3 = IK_F3		;running

;----------------------------------------

; Configured items, check if default-vanilla item exists...
	CLEAR ITMKEY
	K_ITEM = LITMNO
	K_F1 = DF1,	'XXX'
	K_F2 = DF2,	'XXX'
	K_F3 = DF3,	'XXXXX'
; if item w/ default key exists - even better than vanilla...

	read (CHN041, invmas, itmkey, keynum:1, getrfa:defrfa) [err=no_default]
	vanitm = 1				;vanilla item was found
	savcfg = icfg				;remember if cfg or not.
	girfa = defrfa				;in case this is the item to update
	if (inn .eq. 1) goto gi_done		;ignore key values
;----------------------------------------
no_default,
; note that some notes are info only, ex: f1=60 (all mat exposed). don't use
; these notes as part of key, and they don't change dept...

;-------------------------------------------------------------------------
; 11-02-09: default note changes:

	clear kf1, kf2, kf3	;these will be f-key values when done...

	if (clvl1 .le. 0)
	then	kf1 = df1	;no key entered, used default
	else	if (f1_ik(clvl1).eq.1)
		then	kf1 = 0			;key entered not valid for look-up
		else	kf1 = f1_key(clvl1)	;use key entered
	if (clvl2 .le. 0)
	then	kf2 = df2	;no key entered, used default
	else	if (f2_ik(clvl2).eq.1)
		then	kf2 = 0			;key entered not valid for look-up
		else	kf2 = f2_key(clvl2)	;use key entered
	if (clvl3 .le. 0)
	then	kf3 = df3	;no key entered, used default
	else	if (f3_ik(clvl3).eq.1)
		then	kf3 = 0			;key entered not valid for look-up
		else	kf3 = f3_key(clvl3)	;use key entered

	using kf1 select
	(080),	kf1 = 081
	(082),	kf2 = 083
	endusing

	if (kf3 .eq. 9) kf3 = 3	;ssq 5-18-10

; now kf1, kf2 & kf3 should be look-up keys for this item...

	k_f1 = kf1, 'XXX'
	k_f2 = kf2, 'XXX'
	k_f3 = kf3, 'XXXXX'

;-------------------------------------------------------------------------
	

	READ (CHN041, INVMAS, ITMKEY, KEYNUM:1, getrfa:girfa) [ERR=NO_ITM_3]

	savcfg = icfg				;remember if cfg or not.
	exact_match = 1
	goto gi_done

no_itm_3,

	clear girfa
	ivflag = 1			;not found
	clear invmas			;9-6-2011
	if (vanitm .ne. 1) goto gi_done
	read (CHN041, invmas, rfa:vanrfa) [err=no_vrfa]	;re-read vanilla item
	clear ivflag					;back to vanilla item
	swfab = 1					;new item, fabricate

gi_done,
	RETURN
;----------------------------------------------------------
no_vrfa,
	ivflag = 1
	RETURN

;-----------------------------------------------------------------

GI_TEMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ISIO (CHN041, INVMAS, KEY, READ, LOKCTL)
	read (CHN041, invmas, KEY, getrfa:girfa) [err=git_bad]	

	exact_match = 1
	RETURN

GIT_BAD,
	IVFLAG = 1
	RETURN
;-----------------------------------------------------------------

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
		if (dsiz.le.0) goto spiral_error		;3-14-13 per bill
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
	THEN	BEGIN
		USING MAT_SEL SELECT
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
		(VIN),	BEGIN
			TDESCR(27,30) = 'PVS'
			SP_SIZE = 0		;8-2-17 SSQ
			END

		ENDUSING

		if (clvl1.gt.0)
		then	xf1 = f1_key(clvl1)
		else	xf1 = 0

		USING XF1 SELECT
		(80 THRU 83), SP_SIZE = 0	;FORCE PRICE TO ZERO
		ENDUSING
		END

	ELSE	TDESCR(27,30) = 'PVS '


	XCALL ISIO (CHN041, INVMAS, LITMNO, READ, LOKCTL)	;need this for price
	IF (LOKCTL .NE. 0) GOTO SPIRAL_ERROR		;bad item
	RETURN

SPIRAL_ERROR,
	OFFERROR
	CONFIG_ERROR = 1
	RETURN
;----------------------------------------------------------------

FIND_DAMPER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;03/08/07 isam: now itmmas.item contains the *'s and is priced to reflect
;;;		the fact that there are dampers. just set ldampr.

	if (%instr(1, litmno, "*") )
	then	ldampr = 1
	else	ldampr = 0

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
		IF (FL .GE. MAXFLEN)	;11-14-13
			BEGIN
		;;;	XCALL OLMSG(WND_1,23,'damper configuration',2)
			bad_digit = 1
			RETURN
			END

		PARTNO(FL, MAXFLEN) = PARTNO(FL+1, MAXFLEN)
		CALL D_SIZE
		IF (BAD_DIGIT)
			BEGIN
		;;;	XCALL OLMSG(WND_1,23,'damper configuration',2)
			RETURN
			END
		END

	RETURN

D_SIZE,	;---------------------------------------
	CLEAR BAD_DIGIT

	ONERROR BADDIG
	DAMP = PARTNO(FL, FL+1)
	OFFERROR

;check damper price...
	CLOSE 9
	XCALL FFILE(1,FILPRC,SWITCH)	;SSQ 9-9-03

	FL_NAME = 'SDPRIC'		;SSQ 9-9-03
	OPEN(9,I,FILPRC)		;SSQ 9-9-03
	XCALL IO (9,TPRICE,1,READ,LOKCTL)
	IF(DAMP.LE.80) 
	THEN	DAMPER_PRICE(I) = ARRAY(DAMP)*10
	ELSE	CLEAR DAMPER_PRICE(I)

	CLOSE 9
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
check_f2_note,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; if this note is in the f2-memo array select it...
	for j from 1 thru f_max
		begin
		d_long = f2_memol(j)			;7-19-16 2nd line
		if (f2_key(j).eq.f2_tmp .and. dl_ara(2).eq.f2_tmem2 ) exitloop	;f2 key exists
		end
	if (j .gt. f_max) goto add_f2_note
	clvl2 = j		;4-25-13 switch back
;;;	pi2 = -f2_seq(clvl2)
	return
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
add_f2_note,
	v_level = 2
	entry = f2_tmp,	'ZZZZX' [left]
	call get_memo_table
	if (.not. key_found)
		begin
	;;;	xcall olmsg (w_id, 24, 'memo not in table',2)
		return
		end

	IF(F2_NUM .GE. F_MAX)
		BEGIN
	;;;	XCALL OLMSG(W_ID, 23, 'Cannot exceed 10 memos',1)
		RETURN
		END

	incr f2_num
	f2_idx = f2_num
	f2_memol(f2_idx) = mm_long
	f2_memos(f2_idx) = mm_short
	f2_key(f2_idx) = mm_key

	return
;--------------------------------------------------------
GET_MEMO_TABLE,	;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR KEY_FOUND

	ONERROR NOT_KEY
	USING ENTRY(1,1) SELECT
	('.'),	MM_CODE = ENTRY(2,3)
	('9'),	MM_CODE = ENTRY(1,5)
	(),	mm_code = entry		;2-01-10 don't need "."
	ENDUSING
	OFFERROR
MEMO_KEY,
	CLEAR TBL_KEY
	KEYCOD(1,1) = 'M'
	KEYCOD(2,2) = V_LEVEL, 'X'
	TBLCOD = KEYCOD
	MM_KEY = MM_CODE
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_KEY]
	KEY_FOUND = 1		;PER STEVE M 1-18-00
	RETURN
NOT_KEY,
	OFFERROR
	RETURN
;------------------------------------------------------------

CFG_PRICE_ROUTINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;===================================================
; ------- Configuration Price Adjustments -------
	USING LITMNO SELECT
	('SG','SX'),	BEGIN	;SPIRAL PIPE
	  		LPRICE = (SP_SIZE * PRICE(1))
	  		LPRICE = ((SP_SIZE * PRICE(1)) * CMX)#3 * 10
			LPWGT = (SP_SIZE * IP_WGT)			;SSQ 5-30-17
			LUOFM = 'EA'
			END
	('C4020','C4420','C4620'),	nop	;SSQ 6-12-18
	('C4820','C4220 '),		nop	;SSQ 6-12-18
	('T4','T9','TV4','TV9'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('C9','C4','CV9','CV4'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('CT','CC','CTV','CCV'),	LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('BN'),				LPRICE = (CONFIG_PRICE * CMX)#3 * 10

	('RW16'  THRU  'RW20'),		LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	('S9','S4','SV','SW','SC','ST'), LPRICE = (CONFIG_PRICE * CMX)#3 * 10
	ENDUSING

	RETURN
;---------------------------------------------------	
