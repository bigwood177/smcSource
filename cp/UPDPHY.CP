;UPDPHY.CP
;
;	CLEAR ITMMAS.SMM & UPDATE FROM PYINV.SMM
;
RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.def'

RECORD	PHYINV
	.INCLUDE 'DEF:PHYINV.DEF'
;
RECORD	DIS
	II	,D6
;
RECORD	CHANNEL
	CHN041	,D2
	CHNPHY	,D2
;
RECORD	VARS
	OPNOK	,D1
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D1
	READ	,D1,0
	WRITE	,D1,1
	SWITCH	,D1
	V	,D1

;
PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'POST PHYSICAL TO INVENTORY',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
ENDOFF,
	CALL CLOSE
	STOP

;===============================================

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1,'SU',041,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 1

	OPEN (2,SU,'SMC:PHYINV.SMM')
	CHNPHY = 2

	OPNOK = 1
	RETURN
;-----------------------------------------------

CLOSE,
	IF (CHN041) CLOSE CHN041
	IF (CHNPHY) CLOSE CHNPHY

	RETURN
;-----------------------------------------------
