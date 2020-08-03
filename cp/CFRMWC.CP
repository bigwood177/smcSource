;CFRMWC.CP	WILL-CALL
;CONFRM.CP
;SHIPMENT CONFIRMATIONS
;
; 12-23-19: use "9C" instead of "WC" to look up 
;		confirmation names for cash customers

	.include 'wnd:tools.def'
	.include 'wnd:windows.def'

	.include 'def:hpsub.def'

RECORD	CONFRM
	.INCLUDE 'DEF:RD158A.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

record	coptbl
	.include 'def:rd182a.def'

record	pop1
	.include 'def:pop1.def'


RECORD	funkey
	.include 'def:funkey.def'

record
	DT	,A20
RECORD,X
	YYYY	,D4
	MM	,D2
	DD	,D2
	HH	,D2
	MIN	,D2

RECORD	ADT
	AYYYY	,A4
		,A1, '/'
	AMM	,A2
		,A1, '/'
	ADD	,A2
		,A1, ' '
	AHH	,A2
		,A1, ':'
	AMIN	,A2


RECORD	CHANNEL
	CHN044	,D2
	CHN158	,D2
	CHN182	,D2

record	wvars
	wnd_1	,d4
	w_id	,d4
	w_2	,d4

RECORD	VARS
	wc_code	,a2	;either WC or 9C
	xcust	,d6	;either ocusno or omicus
	f_key	,d3
	stat	,d1
	dec1	,d3
	got_names	,d1
	TDATE	,D8		;CCYYMMDD
	TTIME	,D6		;HH:MM:SS
	TS	,D1
	CMPCOD	,A3
	OPNOK	,D1
	A1	,A1
	BS	,A1	; BACKSPACE 8
	EN	,A1	; END 127
	CW	,A1	; ^W 23
	PGM_SRC	,A1
	XORD	,D6
	SELECT	,D1
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	LOKCTL	,D1
	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID (V)
	call init_window

	xcall w_disp (wnd_1, wd_clear, 'Shipping Confirmation - Will Call')
	

;;;	XCALL OUTPT (1,1,2,'SHIPPING CONFIRMATION - WILL CALL',1)
;
	XCALL ASCII (8, BS)
	XCALL ASCII (127, EN)
	XCALL ASCII (23, CW)


	XCALL WHO(CMPCOD)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
	PGM_SRC = 'W'	;WILL CALL

DISPLA,
	xcall w_disp (wnd_1, wd_clear, 'Shipping Confirmation - Will Call')
	XCALL w_disp (wnd_1, wd_pos, 4,4,'1. ORDER #')
	XCALL w_disp (wnd_1, wd_pos, 8,4,'2. PICKED UP BY:')

ORD,
	XCALL winpt (wnd_1, 4, 15, 08, 00, 'AE', ENTRY, INXCTL)
	
	GOTO (DISPLA, ENDOFF), INXCTL
	IF (ENTRY(1,2) .EQ. 'S~') 
		BEGIN
		ENTRY = ENTRY(3,30)
		XCALL w_disp (wnd_1, wd_pos, 4,15, ENTRY)
		END
	
	xcall alpdc (entry(1,6), xord, switch)
	goto (ord), switch
	XCALL ISIO (CHN044, ORDHDR, XORD, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		XCALL wnmsg (wnd_1, 24, 'ORDER NOT ON FILE',1)
		GOTO DISPLA
		END

;12-23-19 for cash-cust...
	if (omicus .gt. 0)
	then	begin
		wc_code = '9C'		;cash customer
		xcust = omicus
		end
	else	begin
		wc_code = 'WC'		;regular customer
		xcust = ocusno
		end



	XCALL w_disp (wnd_1, wd_pos, 4,25,OCUSNM)

	read (chn158, confrm, xord, keynum:1) [err=not_already]
	XCALL wnmsg (wnd_1, 24, 'ALREADY CONFIRMED',1)
	SELECT = 2
	CALL DSPREC
	GOTO ANYCNG

not_already,

	CLEAR CONFRM
	SELECT = 1
	call cust_list
	if (.not. got_names .or. numara.lt.1) goto conf
	DO	BEGIN
		XCALL TTSTS (STAT)
		IF (STAT) ACCEPT (15,DEC1)
		END
	UNTIL (STAT .EQ. 0)

	dline = 'Names'
	xcall pop1 (pop1)
	using p_action select
	(1),	begin
		if (pi.lt.1) pi = 1
		dline = parry(pi)
		sp_conf = dline
		xcall w_disp (wnd_1, wd_pos, 8, 21, sp_conf)
		goto anycng
		end
	(2),	goto conf
	(),	goto displa
	endusing

CONF,
	xcall w_disp (wnd_1, wd_pos,24,1, wd_clr, wdc_lin, '<F1> = Names')

;;;	XCALL winpt (wnd_1, 8, 21, 20, 00, 'A ', ENTRY, INXCTL)
	XCALL winpt (wnd_1, 8, 21, 20, 00, 'A ', ENTRY, INXCTL, f_key)
	GOTO (DISPLA),INXCTL
	using f_key select
	(F_01), goto not_already
	endusing

	SP_CONF = ENTRY(1,20)
	GOTO (ANYCNG), CNGCTL

ANYCNG,
	XCALL wancn (wnd_1, 24, CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR), CNGCTL + 1
CNGBR,
	GOTO (ORD, CONF), WHATNO
	GOTO ANYCNG

DSPREC,
	ENTRY(1,6) = SP_ORD, 'XXXXXX'
	XCALL w_disp (wnd_1, wd_pos, 8, 21,  SP_CONF)
	
	DT(1,12) = SP_DT
	AYYYY = YYYY,	'XXXX'
	AMM = MM,	'XX' [left]
	ADD = DD,	'XX' [left]
	AHH = HH,	'ZX'
	AMIN = MIN,	'XX'
	XCALL w_disp (wnd_1, wd_pos, 8, 48, ADT)
	xcall w_updt
	RETURN
	
PROCES,
	USING SELECT SELECT
	(1),	BEGIN
		DT = %DATETIME
		SP_DT = DT(1,12)
		SP_ORD = XORD
		SP_SRC = PGM_SRC
		SP_CUST = OCUSNO
		SP_MNBR = OC_NBR
		XCALL ISIO (CHN158, CONFRM, SP_ORD, STORE, LOKCTL)
		END
	(2),	BEGIN
		XCALL ISIO (CHN158, CONFRM, SP_ORD, WRITE, LOKCTL)
		END

	ENDUSING
	
	clear coptbl
;;;	tblcod = 'WC'
;;;	wc_cust = ocusno

	tblcod = wc_code
	wc_cust = xcust

	wc_key = sp_conf
	wc_name = sp_conf
	switch = 5
	xcall files (27,'SU',182,switch)
	xcall isio (27, coptbl, tbl_key, store, lokctl)
	close 27

	CALL TIME_STAMP

	IF (OC_NBR .LE. 0) goto displa

	XCALL w_disp (wnd_1, wd_pos, 24,1,'SEND CONFIRMATION NOW <N> ?')
	XCALL winpt (wnd_1, 24,31,01,00,'YN',ENTRY,INXCTL)
	IF (INXCTL.EQ.1) CALL CONFIRM

	GOTO DISPLA

cust_list,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	for numara from 1 thru maxara
		begin
		clear parry(numara),parrfa(numara),pflags(numara)
		end

	clear numara

	clear tbl_key
;;;	tblcod = 'WC'
;;;	wc_cust = ocusno

	tblcod = wc_code
	wc_cust = xcust
	find (chn182, coptbl, tbl_key) [err=cl_loop]
cl_loop,
	xcall ios (chn182, coptbl, read, lokctl)
	if (lokctl .ne. 0) goto cl_eof
	if (tblcod .ne. 'WC') goto cl_eof
	if (wc_cust .ne. ocusno) goto cl_loop

	dline = wc_name
	incr numara
	parry(numara) = dline

;;;	goto cl_loop	;removed 12-23-19
	if (numara .lt. maxara) goto cl_loop
cl_eof,
	got_names = 1
	return
no_wc,
	clear got_names
return
;--------------------------------------------------

TIME_STAMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; RECORD DATE AND TIME ORDER PRINTED.
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TS = 1		;set the timestamp
	SWITCH = 5
	XCALL FILES (44, 'SU', 044, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	READ(44, ORDHDR, XORD) [ERR=TS_ERR]
	IF (TS .EQ. 1)
	THEN	BEGIN
		XCALL RDAT8(TDATE)
		XCALL TIME(TTIME)
		OSHDTE = TDATE
		OSHTIM = TTIME(1,4)
		END
	ELSE	BEGIN
		OSHDTE =
		OSHTIM =
		END

	WRITE(44, ORDHDR, XORD)
	CLOSE 44
	RETURN
TS_ERR,
	XCALL wnmsg (wnd_1, 24, 'Another user is updating this order',1)
	RETURN
;---------------------------------------------------------

ENDOFF,
	CALL CLOSE
	xcall flags (7000000)
	STOP
;

CONFIRM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SEND AN IMMEDIATE CONFIRMATION
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL CONFR1 (CMPCOD, XORD)
	call init_window
	RETURN
;--------------------------------------------------



OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (24, 'SI', 044, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 24

	SWITCH = 5
	XCALL FILES (25, 'SU', 158, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN158 = 25

	SWITCH = 5
	XCALL FILES (87,'SI',182, SWITCH)
	IF (SWITCH.EQ.9) RETURN
	CHN182 = 87

	OPNOK = 1

	RETURN
;----------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	IF (CHN044) CLOSE CHN044
	IF (CHN158) CLOSE CHN158
	IF (CHN182) CLOSE CHN182

	RETURN
;----------------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL U_START("LIB:SMC_WND",,,,,,,100)
	XCALL E_SECT("Will Call Confermations", D_CAPTION)
	XCALL W_PROC(WP_FIND,WND_1,'TTCINF')
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,'TTCINF',24,78)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'CC-TEST',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL W_UPDT
	W_ID = WND_1
	xcall u_logwnd(wnd_1)


	MAXARA = 20
	PLEN = 22
	NUMROW = 10
	WX = 10
	WY = 20
	POP_WID(1,5) = "CONFN"
	POP_TITLE = "Names"
	RETURN
