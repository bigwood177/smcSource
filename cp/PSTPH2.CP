;PSTPH2.CP
;	POST SCANNED DATA W/OUT CLEARIN ALL QTYONH
;
;PSTPHY.CP
;
;	POST PHYSICAL INVENTORY TO ITMMAS BY DEPT.
;
RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.def'
;
RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	PHYINV
	.INCLUDE 'DEF:RD196A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	ERR_REC
	ER_KEY	,A27		;(ITMKEY)
		,A1
		,A14,	'DID NOT UPDATE'

RECORD	CHANNEL
	CHN041	,D2
	CHN182	,D2
	CHN196	,D2
	CHNERR	,D2

RECORD	DIS
	II	,D6
;
RECORD	VARS
	OPNOK	,D1
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	DEPT	,A2	
	DEC	,D18
	XF1	,D3
	XF2	,D3
	XF3	,D5
	READ	,D1,0
	WRITE	,D1,1
	LOKCTL	,D1
	SWITCH	,D1
	V	,D1
;
PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'POST CYCLE COUNT TO ITMMAS',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'POST CYCLE COUNT TO ITMMAS',1)
	XCALL OUTPT (12,4,1,'ARE YOU SURE',1)
	XCALL INPUT (12,20,01,01,'YN',ENTRY,INXCTL,1)
	GOTO (ENDOFF),INXCTL-1

PROCES,
	find (chn196, phyinv, ^first) [err=eof]	
LOOP,

	READS (CHN196, PHYINV, EOF) [ERR = EOF]

	CLEAR ITMKEY
	K_ITEM = P_ITEM
	K_F1 = P_F1
	K_F2 = P_F2
	K_F3 = P_F3

	READ (CHN041, ITMMAS, ITMKEY, KEYNUM:1) [ERR = NO_ITEM]
	
;;;	QTYONH = P_COUNT
	QTYONH = P_COUNT#3

	XCALL ISIO (CHN041, ITMMAS, ITEMNO, WRITE, LOKCTL)
	IF (LOKCTL .NE. 0) CALL ERR_1
	GOTO LOOP


EOF,
	CLOSE CHNERR

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:PHYMNU',1)
;
;======================================================================
;
ERR_1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NO_ITEM,
	ER_KEY = ITMKEY

	WRITES (CHNERR, ERR_REC)

	RETURN
;--------------------------------------------------


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	CLEAR OPNOK
	
	SWITCH = 5
	XCALL FILES (1, 'SU', 041, SWITCH)	;41 - ITMMAS
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 1

	SWITCH = 5
	XCALL FILES (2, 'SI', 182, SWITCH)	;182 - COPTBL
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 2

	SWITCH = 5
	XCALL FILES (3, 'SI', 196, SWITCH)	;196 - PHYINV
	IF (SWITCH .EQ. 9) RETURN
	CHN196 = 3

	open (5, o, 'smc:phyerr.dat')
	chnerr = 5

	OPNOK = 1
	RETURN
;------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN041) CLOSE CHN041
	IF (CHN182) CLOSE CHN182
	IF (CHN196) CLOSE CHN196

	RETURN
;------------------------------------------

