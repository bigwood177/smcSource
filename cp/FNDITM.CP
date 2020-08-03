;fnditm.cp
;
;	item look-up
;	note: this assumes we have key 4 (descr/itemno)

	.include 'wnd:windows.def'

RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.DEF'

RECORD	POP
	.INCLUDE 'DEF:POP40.DEF'

RECORD	HLINE
	J	,D6

RECORD	CHANNEL
	CHN041	,D1

RECORD	WN_NAME
		,A5,	'FNDIM'
	WN_TNMBR,D4


RECORD	WARS
	W_ID	,D4
	WND_1	,D4

RECORD	VARS
	OPNOK	,D1
	TL	,D6
	XSI	,A1
	XDESCR	,A30
	DT	,A20
	ACHAR	,A1
	XFLAG	,A1
	TSTAT	,D1
	SAVRFA	,A6
	I	,D6
	ENTRY	,A30
	INXCTL	,D1
	WHATNO	,D1
	CNGCTL	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3

	SWITCH	,D1
	V	,D1
;
PROC
	XCALL TERID (V)
	CALL INIT_WINDOW
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

DISPLA,

	CLEAR CNGCTL

	XCALL W_DISP(W_ID, WD_CLEAR)
	XCALL W_DISP(W_ID, WD_POS, 1, 1, 'ITEM LOOK-UP')
	XCALL W_DISP(W_ID, WD_POS, 3, 4, 'STARTS WITH OR INCLUDES (S/I)')
	XCALL W_DISP(W_ID, WD_POS, 4, 4, 'DESCRIPTION:')
	
XSI,
	XCALL WINPT(W_ID, 3, 36, 01, 00, 'AE', ENTRY, INXCTL)
	GOTO (DISPLA, ENDOFF),INXCTL
	XSI= ENTRY(1,1)
	IF (XSI .EQ. ' ') XSI = 'S'
	USING XSI SELECT
	('S','I'),	NOP
	(),		BEGIN
			XCALL WNMSG (w_id,23,'ENTER "S" OR "I"' ,1)
			GOTO DISPLA
			END
	ENDUSING

	XCALL W_DISP(W_ID, WD_POS, 3,36,XSI)


XDESCR,
	XCALL WINPT(W_ID, 4, 16, 30, 00, 'AE', ENTRY, INXCTL)
	GOTO (DISPLA, ENDOFF), INXCTL
	XDESCR = ENTRY(1,30)
	
	IF (XDESCR .EQ. '  ')
		BEGIN
		XCALL WNMSG (w_id,23,"DON'T LEAVE BLANK",1)
		GOTO XDESCR
		END

	CALL GET_ITEMS			;POP

	IF (I .EQ. 0) 
		BEGIN
		XCALL WNMSG(w_id, 23, 'NO MATCHES FOUND',1)
		GOTO DISPLA
		END

	NUMARA = I


	DO BEGIN
	  XCALL TTSTS (TSTAT)
	  IF (TSTAT) xcall w_disp(wnd_1,wd_accept,achar)
	END UNTIL (TSTAT.EQ.0)

	DLINE = 'DESCRIPTION                    ITEM #'
	XCALL POP40 (POP)

	USING P_ACTION SELECT
	(),	GOTO DISPLA		;NEXT ORDER
	ENDUSING


ENDOFF,
	CALL CLOSE
	xcall u_finish
	XCALL FLAGS (7000000)
	STOP



GET_ITEMS,
	for i from 1 thru maxara clear parry(i) 
	clear i, numara, j

	TL = %TRIM (XDESCR)
	DESCR = XDESCR
	USING XSI SELECT
	('S'),	FIND (CHN041, ITMMAS, DESCR, KRF:3) [ERR=LOOP]
	('I'),	FIND (CHN041, ITMMAS, ^FIRST, KRF:3) [ERR=LOOP]
	ENDUSING
LOOP,
	INCR J
	USING XSI SELECT
	('I'),	IF (J/500*500.EQ.J) XCALL W_DISP(W_ID, WD_POS, 60,1, HLINE)
	ENDUSING

	XCALL IOS (CHN041, ITMMAS, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF

	USING XSI SELECT
	('S'),	IF (DESCR(1,TL) .GT. XDESCR(1,TL) ) GOTO EOF
	('I'),	IF (.NOT. %INSTR(1,DESCR,XDESCR(1,TL) ) )GOTO LOOP
	ENDUSING

	DLINE(1,30) = DESCR
	DLINE(32,46) = ITEMNO

	INCR I
	PARRY(I) = DLINE
	IF (I .LT. MAXARA) GOTO LOOP
EOF,

	RETURN
;DESCRIPTION                    ITEM #
;AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA AAAAAAAAAAAAAAA
;1234567890123456789012345678901234
;         1         2         3
;--------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	CLEAR OPNOK
	SWITCH = 5


	XCALL FILES (4,'SI',041,SWITCH)		;041-ITMMAS.ISM
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 4


	OPNOK = 1
	RETURN
;---------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN041) CLOSE CHN041
	RETURN
;---------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall u_start(,,,,,,,99)

	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (WND_1.EQ.0)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,23,78)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'Job-in',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,2,2)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1
;; POP info...

	MAXARA = 40
	PLEN = 48
	NUMROW = 15
	WX = 12
	WY = 4
	POP_WID(1,5) = "IMPOP"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "ITEM LOOK-UP"
	RETURN
;-----------------------------------------------------------------


