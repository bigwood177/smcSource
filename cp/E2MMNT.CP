;E2MMNT.CP
;
;		customer E2 e-mail addresses
;
;	5-29-18 ssq: make cusmas isam
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
	TITLE	,A*,	'CUSTOMER E2 EMAIL ADDRESS'
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
		,A*,	'CUST #  KEY      E-MAIL'

RECORD	WN_NAME
		,A5,	'EMKEY'
	WN_TNMBR,D4

RECORD	WARS
	W_ID	,D4
	WND_1	,D4

RECORD	VARS
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
	EKEY	,D3
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

	XCALL w_disp (wnd_1,wd_pos, 1,1,wd_clr,wdc_eow,'CONFIRMATION EMAIL ADDRESS')

	CALL OPENS
	IF (.NOT. OPNOK) GOTO CLOSE

MENU,
	CLEAR PRNTON
	XCALL w_disp (wnd_1,wd_pos, 1,1,wd_clr,wdc_eow,'CONFIRMATION EMAIL ADDRESS')
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
	XCALL w_disp (wnd_1,wd_pos, 1,1,wd_clr,wdc_eow,'CONFIRMATION EMAIL ADDRESS')
	XCALL w_disp (wnd_1,wd_pos, 4,05,'1. CUST #')
	XCALL w_disp (wnd_1,wd_pos, 6,05,'2. NBR                  <F1> = FIND')
	XCALL w_disp (wnd_1,wd_pos, 8,05,'3. EMAIL')
	XCALL w_disp (wnd_1,wd_pos,10,05,'4. FIRST NAME')
	XCALL w_disp (wnd_1,wd_pos,12,05,'5. LAST NAME')
	XCALL w_disp (wnd_1,wd_pos,14,05,'6. CELL/DIRECT')
	XCALL w_disp (wnd_1,wd_pos,16,05,'7. DEFAULT')
	
	
CUST,
	XCALL wINPT (w_id,04,17,06,00,'#E',ENTRY,INXCTL)
	GOTO (DISPLA,MENU),INXCTL
	CUST = ENTRY(1,6)

	XCALL ISIO (CHN001, CUSMAS, CUST, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
	THEN	BEGIN
			XCALL MESAG ('CUST NOT ON FILE',1)
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

EKEY,
	XCALL wINPT (w_id, 6,17,03,00,'# ',ENTRY,INXCTL,f_key)
;;;	GOTO (DISPLA),INXCTL
	using f_key select
	(f_01),	nop
	(exit_key),	goto displa
	(end_key),	goto displa
	(),		begin
			EKEY = ENTRY(1,3)
			goto reg
			end
	endusing

;-
	CALL GETDPT			;POP

d_pop,
	IF (I .EQ. 0) 
		BEGIN
		XCALL WNMSG(w_id, 24,'No Emails',1)
		GOTO DISPLA
		END

	NUMARA = I

	DO BEGIN
	  XCALL TTSTS (TSTAT)
	  IF (TSTAT) xcall w_disp(wnd_1,wd_accept,achar)
	END UNTIL (TSTAT.EQ.0)

	DLINE = 'Nbr       Email Addr'
	XCALL POP1 (POP)

	USING P_ACTION SELECT
	(1),	BEGIN
		SAVRFA = PARRFA(PI)
		READ (CHN182, COPTBL, RFA:SAVRFA) [ERR=DISPLA]
		UNLOCK CHN182
		ekey = e2_nbr
		entry(1,3) = ekey, 'ZZX' [left]
		XCALL w_disp (w_id,wd_pos, 6,17, entry(1,3) )
		xcall w_updt
		END
	(6),	begin
		call more_dpt
		goto d_pop
		end

	(),	GOTO DISPLA		;NEXT ORDER
	ENDUSING

;-
reg,
	CLEAR TBL_KEY
	TBLCOD= 'E2'
	E2_CUST = CUST
	E2_NBR = EKEY
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
	then	BEGIN
		SELECT = 2
		sav_mail = e2_mail
		CALL DSPREC
		GOTO ANYCNG
		END

	else	begin
		SELECT = 1			;ADD MODE
		CLEAR COPTBL
		if (cngctl)
			begin
			e2_mail = sav_mail 
			XCALL w_disp (wnd_1,wd_pos, 9,1,E2_MAIL)
			xcall w_updt
			end
		end

	goto (anycng),cngctl

E2_MAIL,
	XCALL wINPT (w_id,8,17,50,00,'A ',E2_MAIL,INXCTL)
	sav_mail = e2_mail
	GOTO (ANYCNG),CNGCTL

E2_FNAM,
	xcall winpt (w_id,10,21,10,00,'A ',entry,inxctl)
	goto (displa),inxctl
	e2_fnam = entry
	goto (anycng),cngctl
	
E2_LNAM,
	xcall winpt (w_id,12,21,15,00,'A ',entry,inxctl)
	goto (displa),inxctl
	e2_lnam = entry(1,15)
	goto (anycng),cngctl

E2_CELL,
	xcall winpt (w_id,14,21,10,00,'# ',entry,inxctl)
	goto (displa),inxctl
	e2_cell = entry(1,10)
	entry(1,12) = e2_cell, 'ZZZ ZZX XXXX' 
	goto (anycng),cngctl

E2_DFLT,
	xcall winpt (w_id,16,17,01,00,'YN',entry,inxctl)
	e2_dflt = inxctl
	goto (anycng),cngctl


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
	GOTO (CUST, EKEY, E2_MAIL,E2_FNAM,E2_LNAM,E2_CELL,e2_dflt),WHATNO
	GOTO ANYCNG
	
PROCES,
	CASE SELECT OF
	BEGINCASE
	1:	BEGIN
		TBLCOD = 'E2'
		E2_CUST = CUST
		E2_NBR = EKEY
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
	ENTRY(1,6) = E2_CUST,	'ZZZZZX' [LEFT]
	XCALL w_disp (wnd_1,wd_pos, 4,17,ENTRY(1,6))
	ENTRY(1,3) = E2_NBR,	'ZZX' [LEFT]
	XCALL w_disp (wnd_1,wd_pos, 6,17,ENTRY(1,3))
	XCALL w_disp (wnd_1,wd_pos, 8,17,E2_MAIL)
	XCALL w_disp (wnd_1,wd_pos,10,21,e2_fnam)
	xcall w_disp (wnd_1,wd_pos,12,21,e2_lnam)
	entry(1,12) = e2_cell,	'ZZZ ZZX XXXX'
	xcall w_disp (wnd_1,wd_pos,14,21,entry(1,12))
	using e2_nbr select
	(1),	xcall w_disp(wnd_1,wd_pos,16,17,'Y')
	(),	xcall w_disp(wnd_1,wd_pos,16,17,'N')
	endusing

	xcall w_updt
	RETURN
;-----------------------------------
more_dpt,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	call clr_dpt
	find (chn182, coptbl, rfa:savrfa) [err=md_f_err]
md_f_err,
	goto gd_loop



getdpt,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	call clr_dpt

	clear i, tbl_key
	tblcod = 'E2'
	e2_cust = cust
	find (chn182, coptbl, tbl_key) [err=gd_loop]
gd_loop,
	reads (chn182, coptbl, getrfa:savrfa) [err=gd_eof]
	if (tblcod .ne. 'E2') goto gd_eof
	if (e2_cust .ne. cust) goto gd_eof

	clear dline
	dline (1,3) = e2_nbr,	'ZZX'
	dline (5,14) = e2_fnam
	dline (16,25) = e2_lnam
	dline (27,38) = e2_cell, 'ZZZ ZZX XXXX'
	dline (40,80) = e2_mail

	incr i
	parry(i) = dline
	parrfa(i) = savrfa
	if (i .lt. maxara) goto gd_loop

gd_eof,
	return
;---------------------------------------

clr_dpt,	;;;;;;;;;;;;;;;;;;;;;;;;
	for i from 1 thru maxara
		begin
		clear parry(i), pflags(i), parrfa(i)
		end
	clear i
	return
;---------------------------------------

;===================================================================
PRINT_TABLE,
;===================================================================
	LINCNT = 66
	PGCNT  = 0
	
PDISP,
	CNGCTL = 
	XCALL w_disp (wnd_1,wd_pos, 1,1,wd_clr,wdc_eow,'PRINT Customer Email TABLE')
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
	xcall files (6,'SI',182,5)

	CLEAR TBL_KEY
	TBLCOD = 'E2'
	E2_CUST = ST_CUST
	FIND (6,COPTBL,TBL_KEY)[ERR=PLOOP]

PLOOP,
	XCALL IOS (6,COPTBL,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (TBLCOD .NE. 'E2') GOTO EOF
	IF (E2_CUST .LT. ST_CUST) GOTO PLOOP
	IF (E2_CUST .GT. EN_CUST) GOTO EOF


	clear dline
	pline (1,6) = e2_cust,	'ZZZZZZ'
	pline (8,10) = e2_nbr,	'ZZX'
	pline (12,21) = e2_fnam
	pline (23,32) = e2_lnam
	pline (34,45) = e2_cell, 'ZZZ ZZZ ZZZZ'
	pline (47,80) = e2_mail
	CALL PRINT
	GOTO PLOOP

EOF,
	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
PDONE,
	close 6
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
	maxara = 15
	PLEN = 70
	NUMROW = 15
	WX = 12
	WY = 4
	POP_WID(1,5) = "EMAIL"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "Email Addr"
	RETURN
;-----------------------------------------------------------------



