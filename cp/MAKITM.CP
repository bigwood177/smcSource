;MAKITM.CP
;
RECORD	INVMAS
	.INCLUDE 'DEF:RD041A.DEF'
RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.DEF'
;
RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CHANNEL
	CHN041	,D2
	CHN045	,D2
	CHN182	,D2
;
RECORD	VARS
	OPNOK	,D1
	SRCCTL	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DEC	,D18
	ERR	,D1
	XF1	,D3
	XF2	,D3
	XF3	,D5
	LITEM	,A15
	KEY	,A15
	A15	,A15
	I	,D6
	SWITCH	,D1
	V	,D1
;
PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'ADD LINES TO INVENTORY',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
LOOP,
	LOKCTL = 1
	XCALL IOS (CHN045, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF

	CALL FNDINV
	IF (SRCCTL .EQ. 0) GOTO LOOP		;ALREAD EXISTS

	XCALL ADITM (ORDLIN, INVMAS, CHN041, ERR)
	GOTO LOOP
EOF,
ENDOFF,
	CALL CLOSE
	STOP


FNDINV,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	LITEM = LITMNO
	IF (LCFGIM .NE. A15) LITEM = LCFGIM	;use item input if not blank

	USING LITEM SELECT
	('CR0' thru 'CR9'),	KEY = LITEM(1,4)
	('CM'),			KEY = LITEM(1,3)
	('EDS0' THRU 'EDS9'),	KEY = LITEM(1,9)
	(),			KEY = LITEM
	ENDUSING

; check if vanilla item exists...
	CLEAR ITMKEY
	K_ITEM = KEY
	K_F1 = '000'
	K_F2 = '000'
	K_F3 = '00000'

	read (1, INVMAS, ITMKEY, KEYNUM:1) [ERR=I_err]
	if (inn .eq. 1) goto gi_done		;ignore key values
;-------------------------------------------

	XF1 = LF1
	XF2 = LF2
	XF3 = LF3
	DEC = XF1 + XF2 + XF3
	IF (DEC .EQ. 0)		;check for default notes...
		BEGIN
		CLEAR TBL_KEY
		TBLCOD = 'IK'
		IK_ITEM = KEY
		LOKCTL = 1
		XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
		IF (LOKCTL .EQ. 0)
			BEGIN
			XF1 = IK_F1
			XF2 = IK_F2
			XF3 = IK_F3
			END
		END

	CLEAR ITMKEY
	K_ITEM = KEY
	K_F1 = XF1,	'XXX'
	K_F2 = XF2,	'XXX'
	K_F3 = XF3,	'XXXXX'

	read (1, INVMAS, ITMKEY, KEYNUM:1) [ERR=I_err]
gi_done,
	clear srcctl
	return
I_err,
	srcctl = 1
	return
;----------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1, 'SU', 41, SWITCH)		;INVMAS
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 1

	SWITCH = 5
	XCALL FILES (5, 'SI', 82, SWITCH)		;INVLIN
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 5

	SWITCH = 5
	XCALL FILES (8, 'SI', 182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 8

	OPNOK = 1
	RETURN
;-------------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	IF (CHN041) CLOSE CHN041
	IF (CHN045) CLOSE CHN045
	IF (CHN182) CLOSE CHN182

	RETURN
;-------------------------------------------------------
