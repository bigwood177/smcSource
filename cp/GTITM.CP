SUBROUTINE GTITM	;see getitm, from scrnx.cp
	ITEM	,A
	F1	,A
	F2	,A
	F3	,A
	RFA	,A
	FLAG	,D	;0=NOT FOUND, 1=FOUND
	VANFLG	,D	;1= FOUND VANILLA ITEM
	VANRFA	,A	
	CMPNY	,A	;OPTIONAL COMPANY CODE
	RVANRFA	,A	;REAL VAN RFA (NO NOTES) 12-19-18 FOR PH2LBS (OPTIONAL)

RECORD	INVMAS
	.INCLUDE 'DEF:RD041A.DEF'
RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CHANNEL
	CHN041	,D2
	CHN182	,D2

RECORD	VARS
	CMPCOD	,A3
	D5	,D5
	dec	,d18
	XL1	,D3
	XL2	,D3
	XL3	,D5

	flg	,d1
	KEY	,A15
	ZRO	,A5,	'00000'
	A2	,A2
	A5	,A5
	OPNOK	,D1
	CHAN	,D6
	I	,D6
	GIRFA	,A6
	DF1	,D3
	DF2	,D3
	DF3	,D5
	REDFIL	,A14
	CLCTL	,D1

;
PROC
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF


; get an itmmas rfa using the followin logic:
; check for vanilla item (vanitm = item w/ all note keys = zero),
; if vanitm found and no-notes then you've got the item.
;
; check for default keys for this item. if incoming keys are blank, read
; item using default keys.  If item found then you've got the item.
;
; if the incoming keys are not blank, try for an exact match using the 
; incoming keys.  If f3 is blank, and there is a default f3-key, substitute
; the default f3-key. If item found then you've got the item.

; if none of the above work, you did not find the item.


	FLAG = 1		;assume item found
	CLEAR VANFLG		;assume vanilla item not found
	CLEAR RFA, vanrfa

	CLEAR DF1, DF2, DF3	;default keys (numeric)

; read itmmas based on partial key for matching items below...
; these partial items don't have f-note keys, so after 
; reading the item, there's nothing else to do but exit...
	USING ITEM SELECT
	('CMAB','CMGB','CMSB'),	GOTO SKIP_KEY		;1-18-12 BOX
	('CM'),			KEY = ITEM(1,3)
	('CR0' thru 'CR9'),	KEY = ITEM(1,4)
	('JEB0' THRU 'JEB9'),	KEY = ITEM(2,15)	;SKIP THE "J"
	('JEF0' THRU 'JEF9'),	KEY = ITEM(2,15)	;SKIP THE "J"
     	('JEFI0' THRU 'JEFI9'),	KEY = ITEM(2,15)	;SKIP THE "J"
	('JJG'),		KEY = ITEM(2,15)	;SKIP THE "J"
	('JTG'),		KEY = ITEM(2,15)	;SKIP THE "J"

	(),			GOTO SKIP_KEY
	ENDUSING
; 10-21-09 include f1 note...
	CLEAR ITMKEY
	K_ITEM = KEY
	K_F1 = F1
	K_F2 = '000'
	K_F3 = '00000'

	read (chn041, invmas, itmkey, keynum:1, getrfa:rfa) [err=GIT_BAD]

;;;	READ (CHN041, INVMAS, KEY, GETRFA:RFA) [ERR=GIT_BAD]	
	CALL CLOSE
	XRETURN			;GOT THE ITEM!

;===============================================================
SKIP_KEY,
; Configured items, check if vanilla item exists...
	CLEAR ITMKEY
	K_ITEM = ITEM
	K_F1 = '000'
	K_F2 = '000'
	K_F3 = '00000'

	read (chn041, invmas, itmkey, keynum:1, getrfa:rfa) [err=no_itm_2]
	vanflg = 1	;got the vanilla item
	vanrfa = rfa	;6-18-09
	IF (%PASSED(RVANRFA) ) RVANRFA = RFA		;12-19-18
	if (inn .eq. 1) 
		BEGIN
		CALL CLOSE
		xreturn		;got the item!
		END
;===============================================================


no_itm_2,
; check for exact match,  first, check for default keys...
	CLEAR TBL_KEY		;is there a default key for this item?
	TBLCOD = 'IK'		
	IK_ITEM = ITEM
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NO_DEFAULT]
	DF1 = IK_F1		;note that these values will 
	DF2 = IK_F2		;be overridden if notes are
	DF3 = IK_F3		;running


; Configured items, check if default-vanilla item exists...
	CLEAR ITMKEY
	K_ITEM = ITEM
	K_F1 = DF1,	'XXX'
	K_F2 = DF2,	'XXX'
	K_F3 = DF3,	'XXXXX'
; if item w/ default key exists - even better than vanilla...

	read (chn041, invmas, itmkey, keynum:1, getrfa:rfa) [err=no_default]
	vanflg = 1		;got the vanilla/default item
	vanrfa = rfa

	if (inn .eq. 1) 
		BEGIN
		CALL CLOSE
		xreturn		;got the item!
		END
;===============================================================

no_default,
	XL1 = F1
	XL2 = F2
	XL3 = F3

;--------------------------------------------------
; 11-2-09: substitute default keys if key input is zero...

	IF (XL1 .EQ. 0) XL1 = DF1
	IF (XL2 .EQ. 0) XL2 = DF2
	IF (XL3 .EQ. 0) XL3 = DF3

;;;	K_F1 = F1
;;;	K_F2 = F2
;;;	K_F3 = F3
;;;	IF (K_F3 .EQ. '00016') K_F3 = '00015'
;;;	using f1 select
;;;	('080'),	k_f1 = '081'
;;;	('082'),	k_f1 = '083'
;;;	endusing

	IF (XL3 .EQ. 9) XL3 = 3		;ssq 5-18-10
;;;	IF (XL3 .EQ. 16) XL3 = 15

	using xl1 select
	(080),	xl1 = 081
	(082),	xl1 = 083
	endusing

;note that some f-notes are not used as keys (f2=60 for example), so
;eliminate these keys from consideration...

	if (xl1 .eq. 0) goto chk_f2

	clear tbl_key
	tblcod = 'M1'
;;;	xcall alpdc (k_f1, mm_key, flg)
	mm_key = xl1
	read (chn182,coptbl,tbl_key)[err=chk_f2]
	if (mm_ik .eq. 1) 	;not used as key
		begin
		clear xl1 
		end
chk_f2,
	if (xl2 .eq. 0) goto chk_f3

	clear tbl_key
	tblcod = 'M2'
;;;	xcall alpdc (k_f2, mm_key, flg)
	mm_key = xl2
	read (chn182,coptbl,tbl_key)[err=chk_f3]
	if (mm_ik .eq. 1)	;not used as key
		begin
		clear xl2 
		end

chk_f3,
; well, this doesn't apply to f3-notes.

; 11-2-09 default notes inserted above
;
	K_F1 = XL1,	'XXX'
	K_F2 = XL2,	'XXX'
	K_F3 = XL3,	'XXXXX' 

;;;;if no notes running use default keys...
;;;	dec = xl1 + xl2 + xl3
;;;	if (dec .eq. 0)
;;;		BEGIN
;;;		K_F1 = DF1,	'XXX'
;;;		K_F2 = DF2,	'XXX'
;;;		K_F3 = DF3,	'XXXXX'
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;if just the <f3> note is zero, use the default f3 key...
;;;	IF (F3 .EQ. A5) K_F3 = DF3,	'XXXXX'

	READ (chn041, INVMAS, ITMKEY, KEYNUM:1, getrfa:rfa) [ERR=GIT_BAD]

	CALL CLOSE
	XRETURN			;GOT THE ITEM!
;===============================================================


GIT_BAD,
	CLEAR FLAG	;ITEM NOT FOUND
	CALL CLOSE
	XRETURN
;-----------------------------------------------------------------


ENDOFF,
	CALL CLOSE
	RETURN

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; passing cmpny allows you to look up roc inventory while in smc,
; see cpyroc...

	IF (%PASSED(CMPNY) )		;10-12-09 
	THEN	CMPCOD = CMPNY
	ELSE	XCALL WHO (CMPCOD)

	CLEAR OPNOK

	XCALL FFILC (CMPCOD, 41, REDFIL, CLCTL)
;;;	XCALL FFILE (41, REDFIL, CLCTL)
	REDFIL(14,14) = 'M'

	FOR CHAN FROM 50 THRU 100	IF (.NOT. %CHOPEN(CHAN)) EXITLOOP
	IF (CHAN .GE. 100) RETURN

	CHN041 = CHAN
	OPEN (CHN041, SI, REDFIL)

	XCALL FFILE (182, REDFIL, CLCTL)
	REDFIL(14,14) = 'M'

	FOR CHAN FROM 50 THRU 100	IF (.NOT. %CHOPEN(CHAN)) EXITLOOP
	IF (CHAN .GE. 100) RETURN

	CHN182 = CHAN
	OPEN (CHN182, SI, REDFIL)

	OPNOK = 1

	RETURN
;------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN041) CLOSE CHN041
	IF (CHN182) CLOSE CHN182
	RETURN
;------------------------------------------------
