;dptque.cp
;mailque.cp
; email version of fax-que
;
;FAXQUE.CP
.MAIN MAILQUE
;
;	MAINTAIN quote QUE for dept I.
;

	.DEFINE POOLSIZE	,25000
	.DEFINE WNDCHNL		,15
	.DEFINE MAXWINS		,10
	.INCLUDE 'WND:WINDOWS.DEF'

	.INCLUDE 'DEF:HPSUB.DEF'


	.INCLUDE 'DEF:RD175D.DEF'


record	coptbl
	.include 'def:rd182a.def'


RECORD	COINFO
	.INCLUDE 'DEF:COINFO.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	DPTQUE
	.INCLUDE 'DEF:DPTQUE.DEF'


RECORD	POP
	.INCLUDE 'DEF:FAXPOP.DEF'


RECORD	F_FILE
		,A4,	'SPL:'
		,A1,	'F'
	F_TIME	,D6
		,A4,	'.FAX'



RECORD	WN_NAME
		,A5,	'DPTQU'
	WN_TNMBR,D4
RECORD	WARS
	WND_1	,D4
	W_ID	,D4
	

RECORD	CHANNEL
	CHN044	,D2
	CHN045	,D2
	CHN182	,D2
	CHNFAX	,D2
	CHNFXD	,D2
	CHN139	,D2
	chn084	,d2

RECORD	BUFFER
		,A100

RECORD	FILE4
	F4_DEV	,A3
		,A1,	':'
	F4_NAME	,A4
		,A1,	'.'
	F4_EXT	,A3

RECORD	FILPRC
	FL_DEV	,A3
		,A1,	':'
	FL_NAME	,A6
		,A1,	'.'
	FL_EXT	,A3

RECORD	VARS
	LOCAL	,D1	;1=PRINT LOCAL
	cmpcod	,a3
	NO_DOLLARS	,D1
	C_CODE	,A3		;COMPANY CODE
	B_ERR	,D1
	CP_INFO	,D1
	J	,i4
	PD_PHONE	,A12	;'XXX-XXX-XXXX'
	OPNOK	,D1
	lincnt	,d3
	PLINE	,A96
	TODAY	,D6
	Lrow	,D2
	Lcol	,D3
	DECMAL	,D18
	BLANKS	,A30
	F_SEND	,D1
	BADCUS	,D1
	q_quote	,d6
	q_name	,a25
	PHONEDATE	,A10
	LETTERDATE	,A10
	STAT	,D3
	II	,D6	
	SAVRFA	,A6
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	XDATE	,D6
	USRDAT	,D6
	SENT	,A1
	SWITCH	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	V	,D1

.PROC

	XCALL TERID (V)
	XCALL RDATE(TODAY)
	CALL INIT_WINDOW

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF


DISPLA,
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL W_DISP(WND_1,WD_POS,4,4,'SHOW PRINTED WORK ORDERS ?')
	XCALL W_UPDT
	XCALL WINPT (W_ID,4,29,01,00,'YE',ENTRY,INXCTL)
	SENT = ENTRY(1,1)
	IF (INXCTL.EQ.2 .AND. SENT.NE.'N') GOTO ENDOFF
	XCALL W_UPDT

	CALL GET_QUE
	IF (NUMARA .EQ. 0) GOTO DISPLA

	DLINE = '  ORDER#     DATE  CUST# NAME                           STS'
	XCALL TTSTS(STAT)
	IF (STAT) READS(15,ENTRY)
	XCALL FXPOP (POP)
	IF (PI .EQ. 0) GOTO DISPLA
	GOTO (PROCES,ADDNEW,DELETE),P_ACTION
	GOTO DISPLA

;--------------------------------------------------------------
PROCES,
	SAVRFA = PARRFA(PI) 
	READ (CHNFAX,DPTQUE,RFA:SAVRFA)[ERR=NO_FAX_RECORD]
	XCALL ISIO (CHN044,ORDHDR,FAX_ORDER,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO NO_ORDER_RECORD


	LOCAL = 1				;FOR TESTING
	XCALL DEPTI (OORDNO, LOCAL, "I")
	GOTO UPDATE_DPTQUE


UPDATE_DPTQUE,
	FAX_STATUS = 1
	CLOSE 22

EOF_CFS,
	IF (%CHOPEN(22) ) CLOSE 22
; write back DPTQUE record...
	WRITE (CHNFAX,DPTQUE,RFA:SAVRFA) [ERR=BAD_WRITE]

	GOTO ENDOFF
;------------------------------------

BAD_WRITE,
	XCALL W_DISP(WND_1,WD_POS,20,1,"Error Writing faxque Record !!")
	XCALL WINPT (W_ID,4,01,00,00,'A ',ENTRY,INXCTL)
	GOTO DISPLA

;--------------------------------------------------------------
;--------------------------------------------------------------
DELETE,
	SAVRFA = PARRFA(PI) 
	READ (CHNFAX,DPTQUE,RFA:SAVRFA)[ERR=NO_FAX_RECORD]
	DELETE(CHNFAX,FAX_ORDER) [ERR=BAD_DELETE]
	GOTO DISPLA
BAD_DELETE,
	XCALL W_DISP(WND_1,WD_POS,20,1,"Error Deleting Record !!")
	XCALL WINPT (W_ID,4,01,00,00,'A ',ENTRY,INXCTL)
	GOTO DISPLA
;--------------------------------------------------------------
ADDNEW,
	GOTO DISPLA
;--------------------------------------------------------------

NO_ORDER_RECORD,
	XCALL W_DISP(WND_1,WD_POS,20,1,"CAN'T FIND ORDER RECORD !!")
	XCALL WINPT (W_ID,4,01,01,00,'A ',ENTRY,INXCTL)
	GOTO ENDOFF
NO_FAX_RECORD,
	XCALL W_DISP(WND_1,WD_POS,20,1,"CAN'T FIND FAX RECORD !!")
	XCALL WINPT (W_ID,4,01,01,00,'A ',ENTRY,INXCTL)
ENDOFF,
	CALL CLOSE
	XCALL FLAGS (7000000)	;suppress program termination message
	STOP
;=========================================================
;=========================================================

GET_QUE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR NUMARA, II
;;;	FIND (CHNFAX,FAXFIL,^FIRST) [ERR=EXIT_QUE]
	FIND (CHNFAX,DPTQUE,^FIRST,KRF:1) [ERR=EXIT_QUE]
QLOOP,
	READS (CHNFAX,DPTQUE,EXIT_QUE,GETRFA:SAVRFA) [ERR=EXIT_QUE]
	IF (SENT.EQ.'N' .AND. FAX_STATUS.EQ.1) GOTO QLOOP

	XCALL ISIO (CHN044,ORDHDR,FAX_ORDER,READ,LOKCTL)
	IF (LOKCTL .NE. 0) 
		BEGIN
		OCUSNO = 
		OCUSNM = '***ORDER NOT FOUND ***'
		GOTO QLOOP		;SSQ 7-2-99
		END
	CLEAR DLINE
	DLINE(1,6) = FAX_ORDER,	'ZZZZZX'
	DLINE(8,15) = FAX_DATE,	'XX/XX/XX'
	DLINE(17,22) = OCUSNO,	'ZZZZZX'
	DLINE (24,53) = OCUSNM
	USING FAX_STATUS SELECT
	(0),	DLINE(56,59) = "Open"
	(1),	DLINE(56,59) = "Sent"
	(2),	DLINE(56,69) = "ERR"
	ENDUSING

	INCR II
	IF (II .GT. MAXARA) 
		BEGIN
		II = MAXARA
		GOTO EXIT_QUE
		END
	PARRFA(II) = SAVRFA
	PARRY(II) = DLINE
	GOTO QLOOP
	
EXIT_QUE,
	NUMARA = II
	RETURN
;--------------------------------------------------
;--------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	XCALL FFILE(1,FILPRC,SWITCH)	;SSQ 01-13-04
	FL_EXT(3,3) = 'M'



	XCALL FILES (4,'SI',044,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 4

	xcall files (25,'SI',045,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 25


;;;	XCALL FILES (17,'SI',182,SWITCH)
;;;	CHN182 = 17
	XCALL FILES (3,'SI',182,SWITCH)
	CHN182 = 3

	XCALL OECO (CHN182,COINFO)		;SSQ 11-21-03


	OPEN (6,SU,'SPL:DPTQUE.ISM')
	CHNFAX = 6

	XCALL FFILE(41,FILPRC,SWITCH)	;SSQ 11-02-04
	FL_EXT(3,3) = 'M'
	FL_NAME = 'FXDATA'
	OPEN (7,SU,FILPRC)
;;;	OPEN (7,SU,'SMC:FXDATA.ISM')
	CHNFXD = 7

	XCALL FILES (9,'SI',139,SWITCH)
	CHN139 = 9

	switch = 5
	xcall filec ('SMC', 84, 'SI', 084, switch)
	chn084 = 84

	XCALL WHO(C_CODE)
	OPNOK = 1
	RETURN
;-----------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045
	IF (CHNFAX) CLOSE CHNFAX
	IF (CHNFXD) CLOSE CHNFXD
	if (chn084) close chn084
	RETURN
;-----------------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	XCALL W_INIT(POOLSIZE,WNDCHNL,MAXWINS)

	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)
		END
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	xcall u_logwnd(wnd_1)

	W_ID = WND_1

;; POP info...

	MAXARA = 40		;9-19-97 NOT MORE THAN 1 FULL WINDOW
	PLEN = 60
	NUMROW = 18
	WX = 
	WY = 10
	POP_WID(1,5) = "FXPOP"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "QUOTES"
	RETURN
;ORDER#     DATE  CUST# NAME                           STS
;XXXXXX XX/XX/XX XXXXXX AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA  X
;1234567890123456789012345678901234567890123456789012345678
;         1         2         3         4         5

.END





