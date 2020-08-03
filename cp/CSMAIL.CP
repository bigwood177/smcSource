;csmail.cp
;TESTE2.DBL
;
;	5-29-18 ssq: make cusmas isam

.INCLUDE 'WND:TOOLS.DEF'
.INCLUDE 'WND:WINDOWS.DEF'

RECORD	CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD001B.DEF'

;;;RECORD	CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'

RECORD	SHIPTO
	.INCLUDE 'DEF:RD171A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CPOP		
	.INCLUDE 'DEF:POP1.DEF'

RECORD	SPOP		
	.INCLUDE 'DEF:POP1A.DEF'

RECORD	WN_NAME
		,A5,	'EMAIL'
	WN_TNMBR,D4

RECORD	WARS
	WND_1	,D4
	W_ID	,D4

RECORD	CHANNEL
	CHN182	,D2
	CHN001	,D2
	CHN002	,D2
	CHN171	,D2

RECORD SKEY
	XCUSNO	,D6
	SHPNO	,D4


RECORD	VARS
	last_seq	,d3
	KEY	,A6
	BSEND	,D6
	BSMID	,D6
	SRCCTL	,D1
	OPNOK	,D1
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D1
	NOCUS	,D1
	NOSHP	,D1
	READ	,D1,0
	WRITE	,D1,1
	LOKCTL	,D1
	savrfa	,a6
	a1	,a1
	CUST	,D6
	SHIP	,D4
	XNBR	,D3
	XNAM	,A10
	XCUST	,D6
	I	,D6
	SWITCH	,D1
	V	,D1
;
PROC
	open (15,i,'tt:')

	CALL INIT_WINDOW

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
DISPLA,
	unlock chn182
	CLEAR NOCUS, NOSHP

	XCALL W_DISP (WND_1, WD_CLEAR)
	XCALL W_DISP (WND_1, WD_POS, 1, 1, 'CUSTOMER EMAIL')
	XCALL W_DISP (WND_1, WD_POS, 4, 4, '1. CUSTOMER:')
	XCALL W_DISP (WND_1, WD_POS, 6, 4, '2. SHIP-TO:')

CUST,
	XCALL WINPT (WND_1, 4, 20, 06, 00, '#E', ENTRY, INXCTL)
	GOTO (DISPLA, ENDOFF), INXCTL
	CUST = ENTRY(1,6)
	CALL GETCUS
	GOTO (CUST),NOCUS	
	XCALL W_DISP (WND_1, WD_POS, 4,30, NAME)
	GOTO (ANYCNG), CNGCTL
SHIP,
	XCALL WINPT (WND_1, 6, 20, 04, 00, '#E', ENTRY, INXCTL)
	GOTO (DISPLA, ENDOFF), INXCTL
	SHIP = ENTRY(1,4)
	IF (SHIP .GT. 0) CALL GETSHP
	XCALL W_DISP (WND_1, WD_POS, 6,30, SHTONA)

	GOTO (SHIP), NOSHP
	GOTO (ANYCNG), CNGCTL
ANYCNG,
	XCALL WANCN (WND_1, 24, CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR), CNGCTL +1
CNGBR,
	GOTO (CUST, SHIP), WHATNO
	GOTO ANYCNG

PROCES,
	IF (SHIP .GT. 0) GOTO CSPOP

	clear xnbr
	xcall cont (cust, xnbr)
	goto displa

CSPOP,

	CALL GET_CUST

	DLINE = 'CUSTOMER EMAIL'

LOOP,
	CALL GET_SHIP
	aDLINE = 'SHIP-TO'

	XCALL CSPOP (CPOP, SPOP)

	USING P_ACTION SELECT
	(1),	BEGIN
		CALL SET
		CALL SELECTED
		END

	(2),	BEGIN		;INSERT
		CALL SET
		CALL INSERT_new
		END

	(3),	BEGIN		;DELETE
		CALL SET
		CALL DELETE
		END
	(4),	BEGIN
		CALL GET_MORE
		IF (NUMARA.EQ.0) CALL GET_CUST
		END
	(),	GOTO displa
	ENDUSING

	GOTO LOOP

GETSHP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	NOSHP  = 1
	XCUSNO = CUST
	SHPNO = SHIP
	XCALL ISIO (CHN171,SHIPTO,SKEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0) 
	THEN	CLEAR SHTONA
	ELSE	CLEAR NOSHP

	RETURN
;-----------------------------------------------------

GETCUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	NOCUS = 1
	
	XCALL ISIO (CHN001, CUSMAS, CUST, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		THEN	CLEAR CUSMAS
		ELSE	CLEAR NOCUS
		
		
;;;	KEY = CUST, 'XXXXXX'
;;;	XCALL SERCH (CHN002,CUSIDX,KEY,1,6,BSEND,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	IF (SRCCTL .NE. 0)
;;;		BEGIN
;;;		NAME = 
;;;		RETURN
;;;		END

;;;	LOKCTL = 1
;;;	XCALL IO (chn001,CUSMAS,IRC001,READ,LOKCTL)
;;;	IF (LOKCTL .NE. 0) RETURN

;;;	clear nocus

	RETURN
;------------------------------------------------------
endoff,
FINI,
	CALL CLOSE

	XCALL FLAGS (7000000)
	STOP

SET,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	DLINE = PARRY(PI)
	XNBR =  DLINE(1,3)
	XNAM =  DLINE(12,21)

	RETURN
;--------------------------------------------

insert_new,
	xnbr = -1		;insert
	xcall cont (cust, xnbr)
	reads (15,a1)
	call get_cust
	return

SELECTED,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (PFLAGS(PI) .EQ. 1) CALL INSERT
	IF (PFLAGS(PI) .EQ. 0) CALL DELETE
	RETURN
;--------------------------------------------

INSERT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR TBL_KEY
	TBLCOD = 'E3'
	E3_CUST = CUST
	E3_SHIP = SHIP
	E3_NBR = XNBR
	FIND (CHN182, COPTBL, TBL_KEY) [ERR=NO_CUST]

	RETURN			;ALREADY IN TABLE
NO_CUST,
	CLEAR COPTBL
	TBLCOD = 'E3'
	E3_CUST = CUST
	E3_SHIP = SHIP
	E3_SEQ = LAST_SEQ + 1

	E3_NBR = XNBR
	E3_FNAM = XNAM

	STORE (CHN182, COPTBL, TBL_KEY)
	
	RETURN
;--------------------------------------------
DELETE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'E3'
	E3_CUST = CUST
	E3_SHIP = SHIP
	find (CHN182, COPTBL, TBL_KEY) [ERR=de_loop]
de_loop,
	xcall ios (chn182, coptbl, read, lokctl)
	if (lokctl .ne. 0) goto not_del
	if (tblcod .ne. 'E3') goto not_del
	if (e3_cust .ne. cust) goto not_del
	if (e3_ship .ne. ship) goto not_del
	if (e3_nbr .ne. xnbr) goto de_loop

	DELETE(CHN182)

NOT_DEL,			;NOT IN TABLE
	RETURN
;--------------------------------------------
GET_MORE,
	read (CHN182, COPTBL, RFA:PARRFA(numara) )

	CALL CLEAR_CUST_ARRAY
	GOTO GC_LOOP


GET_CUST,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CALL CLEAR_CUST_ARRAY

	CLEAR TBL_KEY
	TBLCOD = 'E2'
	E2_CUST = CUST
	FIND (CHN182, COPTBL, TBL_KEY) [ERR=GC_LOOP]
	
GC_LOOP,
	READS (CHN182, COPTBL, GC_EOF, GETRFA:SAVRFA)
	IF (TBLCOD .NE. 'E2') GOTO GC_EOF
	if (e2_cust .ne. cust) goto gc_eof
	clear dline
	DLINE (1,3) = E2_NBR,	'ZZX'
	DLINE (5,10) = E2_CUST,	'ZZZZZX'
	DLINE (12,21) = E2_FNAM
	DLINE (23,45) = E2_MAIL
	INCR I
	PARRY (I) = DLINE
	PARRFA (I) = SAVRFA
	if (i .lt. maxara) GOTO GC_LOOP	
GC_EOF,
	NUMARA = I
	DLINE = 'CUSTOMER EMAIL'
	RETURN
;--------------------------------------------

CLEAR_CUST_ARRAY,

	CLEAR NUMARA
	FOR I FROM 1 THRU 20
		BEGIN
		CLEAR PARRY(I)
		CLEAR PARRFA(I)
		CLEAR PFLAGS(I)
		END
	CLEAR I
	RETURN
;--------------------------------------------


GET_SHIP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR aNUMARA
	FOR I FROM 1 THRU 20
		BEGIN
		CLEAR aPARRY(I)
		CLEAR aPARRFA(I)
		CLEAR aPFLAGS(I)
		END

	CLEAR I
	CLEAR TBL_KEY
	TBLCOD = 'E3'
	E3_CUST = CUST
	E3_SHIP = SHIP

	FIND (CHN182, COPTBL, TBL_KEY) [ERR=GS_LOOP]
	
GS_LOOP,
	READS (CHN182, COPTBL, GS_EOF)
	IF (TBLCOD .NE. 'E3') GOTO GS_EOF
	if (e3_cust .ne. cust) goto gs_eof
	if (e3_ship .ne. ship) goto gs_eof
	clear dline
	DLINE (1,3) = E3_NBR,	'ZZX'
	DLINE (5,14) = E3_FNAM
	DLINE (50,53) = E3_SEQ		;WE'LL NEED THIS LATER
	INCR I
	aPARRY (I) = DLINE
	LAST_SEQ = E3_SEQ		;KEEP TRACK OF LAST SEQ# USED

	GOTO GS_LOOP	
GS_EOF,
	aNUMARA = I
	RETURN
;--------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (18, 'SU', 182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN

	CHN182 = 18

	SWITCH = 5
	XCALL FILES (1,'SI',001, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN001 = 1

;;;	SWITCH = 5
;;;	XCALL FILES (2, 'I', 002, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN002 = 2

	SWITCH = 5
	XCALL FILES (4, 'SI', 171, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN171 = 4


;;;	XCALL IO (CHN001, CUSMAS, 1, READ, LOKCTL)
;;;	BSEND = ORG001

	OPNOK = 1

	RETURN
;--------------------------------------------
CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN001) CLOSE CHN001
;;;	IF (CHN002) CLOSE CHN002
	IF (CHN171) CLOSE CHN171
	IF (CHN182) CLOSE CHN182

	RETURN
;--------------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	xcall u_start("lib:smc_wnd",,,,,,,200)
	xcall e_sect("Customer Email",D_CAPTION)
	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)
;;;		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,23,78)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'Test Cust / Ship-To',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL W_UPDT
	W_ID = WND_1
	xcall u_logwnd(wnd_1)

;; POP info...

	MAXARA = 20
	PLEN = 46
	NUMROW = 15
	WX = 10
	WY = 5
	POP_WID(1,5) = "CSPOP"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "Customer Email"

;; POP info...

	aMAXARA = 20
	aPLEN = 14
	aNUMROW = 15
	aWX = 10
	aWY = 60
	aPOP_WID(1,5) = "SHPOP"
	aPOP_WID(6,8) = WN_TNMBR,	'XXX'
	aPOP_TITLE = "Ship-To Email"

	RETURN
;--------------------------------------------



