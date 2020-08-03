;emcmis.cp	- cash cust emails
;emcmnt.cp
; customer email maintenance
; combination of emails from coptbl for "E2" & "EM"
; smc:emailc.smm is used for cont.cp subroutine; customer contact emails.
;	5-29-18 ssq: make cusmas isam

	.include 'wnd:tools.def'
	.include 'wnd:windows.def'
;
record	emailc
	.include 'def:rd084a.def'
;

record	cusmas
	.include 'def:rd001a.def'
record	cusctl
	.include 'def:rd001b.def'

;;;record	cusidx
;;;	.include 'def:rd002a.def'

RECORD	PRINT
	TITLE	,A*,	'CASH CUSTOMER CONTACT EMAIL ADDRESS'
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
	,A*,	'CUST #     F-NAME     L-NAME     CELL         EMAIL'

record	channel
	chn001	,d2
	chn002	,d2
	chn084	,d2
	chn182	,d2

record	Xvars
	w_id	,d4
	wnd_1	,d4
	wn_name	,a5,	'e_mnt'
	nbr	,d3

record	vars
	pgm	,d2
	st_cust	,d6
	en_cust	,d6
	func	,d1
	opnok	,d1
	custok	,d1
	mailok	,d1
	cust	,d6
	key	,a6
	select	,d1
	entry	,a50
	inxctl	,d1
	cngctl	,d1
	whatno	,d2
	bsmid	,d6
	srcctl	,d1
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	delete	,d1,3
	switch	,d1
	v	,d1
;
proc
	xcall terid (v)
	call init_window

;
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF


MENU,
	CLEAR PRNTON
	XCALL w_disp (wnd_1,wd_pos, 1,1,wd_clr,wdc_eow,'CASH CUSTOMER EMAIL ADDRESS')
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
	XCALL W_DISP(WND_1,WD_CLEAR)
	xcall outpt (1,1,2,'CASH CUSTOMER CONTACT EMAIL',1)

	XCALL w_disp (wnd_1, wd_pos, 4,4,'1. CUSTOMER #')

e_cust,
	xcall winpt (wnd_1, 4,20,06,00,'#E',entry,inxctl)
	goto (displa, endoff),inxctl
	cust = entry(1,6)
	call get_cust
	if (.not. custok) goto e_cust
	cust = entry(1,6)

	clear nbr
	func = 2		;maint
	xcall emcmn (cust, nbr, chn084, func, w_id)	;2-16-15 - opt
;;;	xcall emcmn (cust, nbr, chn084, func)
	goto displa

	
get_cust,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear custok

	XCALL ISIO (CHN001, CUSMAS, CUST, READ, LOKCTL)
	IF (LOKCTL .NE. 0) RETURN

	custok = 1
	return
;---------------------------------------------
ENDOFF,
	CALL CLOSE
	xcall pgchn ('cp:mismnu',1)
	STOP



;===================================================================
PRINT_TABLE,
;===================================================================
	LINCNT = 66
	PGCNT  = 0
	
PDISP,
	CNGCTL = 
	XCALL w_disp (wnd_1,wd_pos, 1,1,wd_clr,wdc_eow,'PRINT Cash Customer Email TABLE')
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
	xcall files (6,'SI',103,5)

	CLEAR emailc
	e_cust = st_cust

	FIND (6,emailc,e_key)[ERR=PLOOP]

PLOOP,
	XCALL IOS (6,emailc,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (E_CUST .LT. ST_CUST) GOTO PLOOP
	IF (E_CUST .GT. EN_CUST) GOTO EOF


	clear pline
	pline (1,6) = e_cust,	'ZZZZZZ'
	pline (8,10) = e_nbr,	'ZZX'
	pline (12,21) = e_fnam
	pline (23,32) = e_lnam
	pline (34,45) = e_cell, 'ZZZ ZZZ ZZZZ'
	pline (47,80) = e_mail
	CALL PRINT
	GOTO PLOOP
;CUST #     F-NAME     L-NAME     CELL         EMAIL
;XXXXXX ZZX AAAAAAAAAA AAAAAAAAAA ZZZ ZZZ ZZZZ AAAA
;12345678901234567890123456789012345678901234567890
;         1         2         3         4

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

	XCALL LPONw (LPSW,SPLFIL,w_id)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	PRNTON = 1
	RETURN
;-------------------------------------------------------------
;===================================================================

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

	switch = 1
	xcall files (1, 'SI', 101, switch)		;101 - MISCUS
	if (switch .eq. 9) return
	chn001 = 1


	switch = 5
	xcall files (8,'SU', 103, switch)		;103 - MISEMC
	if (switch .eq. 9) return
	chn084 = 8


	opnok = 1
	RETURN
;-------------------------------------------
CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	switch = 4
	xcall files (chn001, 'SI', 101, switch)
	if (chn084) close chn084

	RETURN
;-------------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL U_START("LIB:SMC_WND",,,,,,,100)

;;;	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'CUSTOMER CONTACT EMAIL',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1
	xcall u_logwnd(wnd_1)

	RETURN
;-------------------------------------------------------------------
END

