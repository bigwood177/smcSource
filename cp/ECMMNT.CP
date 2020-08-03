;ecmmnt.cp
;		invoice email address's
;eemmnt.cp
;
;		customer e-mail addresses
;	5-29-18 ssq: make cusmas isam
;
;
	.include 'wnd:windows.def'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD001B.DEF'

;;;RECORD	CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'

record	funkey
	.include 'def:funkey.def'

record	pop
	.include 'def:pop1.def'

RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN182	,D2


RECORD	PRINT
	TITLE	,A*,	'INVOICE EMAIL ADDRESS'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PLINE	,A132
	PRNTON	,D1
	LINCNT	,D2,60
	PGCNT	,D6
	LPSW	,D2
	SPLFIL	,A14
	RPTNUM	,D3
	PRTTYP	,A1
	PRTCTL	,D3,132
	LPARG	,D1
	PRNTSW	,D1
	PRTCTR	,D1

RECORD	HD1
		,A*,	'CUST #    KEY      E-MAIL'

RECORD	WN_NAME
		,A5,	'ECKEY'
	WN_TNMBR,D4

RECORD	WARS
	W_ID	,D4
	WND_1	,D4

RECORD	VARS
	prim	,2a1,	'P','C'
	dprim	,d1
	OPNOK	,D1
	sav_mail	,a80
	achar	,a1
	tstat	,d1
	savrfa	,a6
	f_key	,d3
	ST_CUST	,D6
	EN_CUST	,D6
	BLANKS	,A15
	PGM	,D1
	ROW	,D2
	CUST	,D6
	SKEY	,A6
	EKEY	,A9
	TL	,D2
	NUMITM	,D2
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	ENTRY	,A30
	INXCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	LOKCTL	,D1
	WHATNO	,D2
	SELECT	,D1
	CNGCTL	,D1
	I	,D3
	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID (V)

	call init_window

	XCALL w_disp (wnd_1,wd_pos, 1,1,wd_clr,wdc_eow,'INVOICE EMAIL ADDRESS')

	CALL OPENS
	IF (.NOT. OPNOK) GOTO CLOSE

MENU,
	CLEAR PRNTON
	XCALL w_disp (wnd_1,wd_pos, 1,1,wd_clr,wdc_eow,'INVOICE EMAIL ADDRESS')
	XCALL w_disp (wnd_1,wd_pos, 3,9,'PLEASE SELECT APPLICATION')
	XCALL w_disp (wnd_1,wd_pos, 5,15,'1. TABLE MAINTENANCE')
	XCALL w_disp (wnd_1,wd_pos, 6,15,'2. PRINT TABLE')
MINPUT,
	XCALL wINPT (w_id,3,36,1,1,'#E',ENTRY,INXCTL)
	GOTO (MINPUT,ENDOFF), INXCTL
	PGM = ENTRY(1,1)
	GOTO (DISPLA,PRINT_TABLE),PGM
	GOTO MINPUT

DISPLA,
	CLEAR CNGCTL
	XCALL w_disp (wnd_1,wd_pos, 1,1,wd_clr,wdc_eow,'INVOICE EMAIL ADDRESS')
	XCALL w_disp (wnd_1,wd_pos, 4,05,'1. CUST #')
	XCALL w_disp (wnd_1,wd_pos, 6,05,'2. Primary or CC (P or C)')
	XCALL w_disp (wnd_1,wd_pos, 8,05,'3. KEY                  <F1> = FIND')
	XCALL w_disp (wnd_1,wd_pos,10,05,'4. EMAIL')
	
CUST,
	XCALL wINPT (w_id,04,17,06,00,'#E',ENTRY,INXCTL)
	GOTO (DISPLA,MENU),INXCTL
	CUST = ENTRY(1,6)
	
	XCALL ISIO (CHN001, CUSMAS, CUST, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
	THEN	BEGIN
		XCALL wnmsg (wnd_1, 24, 'CUST NOT ON FILE',1)
		GOTO CUST
		END
	ELSE	XCALL w_disp (wnd_1,wd_pos, 04,25,wd_clr,wdc_eol,NAME)
	
;;;	SKEY = CUST,	'XXXXXX'
;;;	XCALL SERCH (CHN002,CUSIDX,SKEY,1,6,BSEND,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	IF (SRCCTL.NE.0 .OR. IRC001.EQ.0)
;;;	THEN	BEGIN
;;;		XCALL MESAG ('CUST NOT ON FILE',1)
;;;		GOTO CUST
;;;		END
;;;	ELSE	BEGIN
;;;		XCALL IO (CHN001,CUSMAS,IRC001,READ,LOKCTL)
;;;		XCALL w_disp (wnd_1,wd_pos, 04,25,wd_clr,wdc_eol,NAME)
;;;		END

	xcall em2 (cust, wnd_1)
	goto displa

prim,
	xcall winpt (w_id,06,33,1,1,'A ',entry,inxctl)
	goto (displa),inxctl
	using entry(1,1) select
	('P'),	dprim = 1
	('C'),	dprim = 2
	(),	goto prim
	endusing
	goto (anycng),cngctl

EKEY,
	XCALL wINPT (w_id, 8,17,08,00,'A ',ENTRY,INXCTL,f_key)
	using f_key select
	(f_01),	nop
	(exit_key),	goto displa
	(end_key),	goto displa
	(),		begin
			EKEY = ENTRY(1,8)
			end
	endusing


	CLEAR TBL_KEY
	TBLCOD= 'EC'
	ec_CUST = CUST
	ec_prim = dprim
	ec_KEY = EKEY
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
	then	BEGIN
		SELECT = 2
		sav_mail = ec_email
		CALL DSPREC
		GOTO ANYCNG
		END

	else	begin
		SELECT = 1			;ADD MODE
		CLEAR COPTBL
		if (cngctl)
			begin
			ec_email = sav_mail 
			XCALL w_disp (wnd_1,wd_pos, 11,1,ec_EMAIL)
			xcall w_updt
			end
		end

	goto (anycng),cngctl

EMAIL,
	XCALL wINPT (w_id,11,1,80,00,'A ',ec_EMAIL,INXCTL)
	sav_mail = ec_email

ANYCNG,
	XCALL w_disp (wnd_1,wd_pos, 24,1,wd_clr,wdc_eol,'FIELD # TO CHANGE            <TAB> = DELETE')
	XCALL wINPT (w_id,24,20,02,00,'#T',ENTRY,INXCTL)
	IF (INXCTL .EQ. 3)
		BEGIN
		XCALL w_disp (wnd_1,wd_pos, 24,1,wd_clr,wdc_lin,'DELETE, ARE YOU SURE ?')
		XCALL wINPT (w_id,24,24,01,01,'YN',ENTRY,INXCTL)
		GOTO (ANYCNG),INXCTL-1
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,DELETE,LOKCTL)
		GOTO DISPLA
		END

	WHATNO = ENTRY(1,2)
	IF (WHATNO .EQ. 0)
	THEN 	CLEAR CNGCTL
	ELSE 	CNGCTL = 1

	GOTO (PROCES,CNGBR),CNGCTL+1
CNGBR,
	GOTO (CUST, prim, EKEY, EMAIL),WHATNO
	GOTO ANYCNG
	
PROCES,
	CASE SELECT OF
	BEGINCASE
	1:	BEGIN
		TBLCOD = 'EC'
		ec_CUST = CUST
		ec_prim = dprim
		ec_KEY = EKEY
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,STORE,LOKCTL)
		END
	2:	XCALL ISIO (CHN182,COPTBL,TBL_KEY,WRITE,LOKCTL)
	ENDCASE
	GOTO DISPLA

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:emmMNU',1)
	XCALL PGCHN ('CP:TBLMNU',1)
	STOP


DSPREC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; DISPLAY RECORD
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ENTRY(1,6) = ec_CUST,	'ZZZZZX' [LEFT]
	XCALL w_disp (wnd_1,wd_pos, 4,17,ENTRY(1,6))

	using ec_prim select
	(1),	nop
	(2),	nop
	(),	ec_prim = 2
	endusing

	XCALL w_disp (wnd_1,wd_pos, 6,17,prim(ec_prim))
	XCALL w_disp (wnd_1,wd_pos, 8,17,ec_KEY)
	XCALL w_disp (wnd_1,wd_pos, 11,1,ec_EMAIL)
	xcall w_updt
	RETURN
;-----------------------------------

getdpt,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear i, tbl_key
	tblcod = 'EC'
	ec_cust = cust
	find (chn182, coptbl, tbl_key) [err=gd_loop]
gd_loop,
	reads (chn182, coptbl, getrfa:savrfa) [err=gd_eof]
	if (tblcod .ne. 'EC') goto gd_eof
	if (ec_cust .ne. cust) goto gd_eof

	clear dline
	dline (1,9) = ec_key
	dline (11,70) = ec_email

	incr i
	parry(i) = dline
	parrfa(i) = savrfa
	goto gd_loop

gd_eof,
	return
;---------------------------------------

;===================================================================
PRINT_TABLE,
;===================================================================
	LINCNT = 66
	PGCNT  = 0
	
PDISP,
	CNGCTL = 
	XCALL w_disp (wnd_1,wd_pos, 1,1,wd_clr,wdc_eow,'PRINT EMAIL TABLE')
	XCALL w_disp (wnd_1,wd_pos, 4,4,'1. STARTING CUST #')
	XCALL w_disp (wnd_1,wd_pos, 6,4,'2. ENDING   CUST #')
ST_CUST,
	XCALL wINPT (w_id,4,25,06,00,'#E',ENTRY,INXCTL)
	GOTO (PDISP,PDONE),INXCTL
	ST_CUST = ENTRY(1,6)
	IF (ST_CUST .EQ. 0)
		BEGIN
		EN_CUST = 999999
		XCALL w_disp (wnd_1,wd_pos, 4,25,wd_clr,wdc_eol,'ALL')
		XCALL w_disp (wnd_1,wd_pos, 6,25,wd_clr,wdc_eol,' ')
		GOTO P_ANY
		END
	GOTO (P_ANY),CNGCTL
EN_CUST,
	XCALL wINPT (w_id,6,25,06,00,'# ',ENTRY,INXCTL)
	GOTO (PDISP),INXCTL
	EN_CUST = ENTRY(1,6)
	IF (EN_CUST .EQ. 0)
		BEGIN
		EN_CUST = ST_CUST
		ENTRY(1,6) = ST_CUST,'ZZZZZX' [LEFT]
		XCALL w_disp (wnd_1,wd_pos, 6,25,ENTRY(1,6))
		xcall w_updt
		END
P_ANY,
	XCALL wANCN (w_id, 24,CNGCTL,WHATNO)
	GOTO (P_PRINT,P_CNGBR),CNGCTL + 1
P_CNGBR,
	GOTO (ST_CUST,EN_CUST),WHATNO
	GOTO P_ANY

P_PRINT,
	CLEAR TBL_KEY
	TBLCOD = 'EC'
	ec_CUST = ST_CUST
	FIND (CHN182,COPTBL,TBL_KEY)[ERR=PLOOP]

PLOOP,
	XCALL IOS (CHN182,COPTBL,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (TBLCOD .NE. 'EC') GOTO EOF
	IF (ec_CUST .LT. ST_CUST) GOTO PLOOP
	IF (ec_CUST .GT. EN_CUST) GOTO EOF


	PLINE (1,6) = ec_CUST,	'ZZZZZX'
	pline (8,8) = prim (ec_prim)
	PLINE (10,17) = ec_KEY
	PLINE (20,100) = ec_EMAIL
	CALL PRINT
	GOTO PLOOP

EOF,
	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
PDONE,
	GOTO MENU

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (PRNTON .EQ. 0) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD,HD
&		,LG,LG,LG,0,132,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;-------------------------------------------------------------
PRNTON,
	SPLFIL (5,6) = 'EF'
	LPSW = 1		;PRINT,SPOOL, OR DISPLAY

;;;	XCALL LPON (LPSW,SPLFIL)
	XCALL LPONw (LPSW,SPLFIL,w_id)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	PRNTON = 1
	RETURN
;-------------------------------------------------------------
;===================================================================


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (18,'SU',182,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 18
	
	SWITCH = 5
	XCALL FILES (1,'SI',001,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN001 = 1

;;;	SWITCH = 5
;;;	XCALL FILES (2, 'I', 002, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN002 = 2

;;;	XCALL IO (CHN001,CUSMAS,1,READ,LOKCTL)
;;;	BSEND = ORG001

	OPNOK = 1
	RETURN
;----------------------------------------------------

CLOSE,
	CLOSE CHN001
;;;	CLOSE CHN002
	CLOSE CHN182
	RETURN
;----------------------------------------------------
INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall u_start(,,,,,,,99)

	XCALL TNMBR (WN_TNMBR)
;;;	XCALL W_INIT(POOLSIZE,WNDCHNL,MAXWINS)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (WND_1.EQ.0)
		BEGIN
	;;;	XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,22,78)
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)	;full size
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'Email',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,2,2)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1
;; POP info...

	MAXARA = 20
	PLEN = 70
	NUMROW = 15
	WX = 12
	WY = 4
	POP_WID(1,5) = "EMAIL"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "Email Addr"
	RETURN
;-----------------------------------------------------------------




