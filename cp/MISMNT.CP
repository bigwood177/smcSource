;MISMNT.CP
; MISC CASH CUSTOMER MAINTENANCE
;  CUSMNT / AR 
;
;
;		CUSTOMER FILE MAINTENANCE
;
;	12-04-96 SSQ:	SAVE CUSTOMER NAME IN SORT NAME WHEN DELETING,
;			DON'T FORCE REORG OF CUSMAS
;	04-18-97 SSQ:	EXPANDED CUSMAS 549 -> 557 BYTES.
;	12-27-10 ssq:	windowize...
; 	5-15-18: ssq convert cusmas to isam

	.include 'wnd:windows.def'

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR



RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD CUSMAS		; 
		.INCLUDE 'DEF:RD001A.DEF'
RECORD CUSCTL		; 
		.INCLUDE 'DEF:RD001B.DEF'
RECORD SALMAN		; 
		.INCLUDE 'DEF:RD054A.DEF'
RECORD AROPEN		; 
		.INCLUDE 'DEF:RD003B.DEF'
RECORD ARTERM		;
		.INCLUDE 'DEF:RD170A.DEF'
RECORD ARTCTL		;
		.INCLUDE 'DEF:RD170B.DEF'
RECORD ARTCDE		;
		.INCLUDE 'DEF:RD169A.DEF'
RECORD ATXCTL		;
		.INCLUDE 'DEF:RD169B.DEF'
RECORD TRWCOM
	TCOM	,A28
RECORD CTL
	ROW	,D2
		,A1
	COL	,D2
		,A1
	MAX	,D2
		,A1
	MIN	,D2
		,A1
	TYPE	,A2
RECORD SNDMSG
		,A3,	'AR:'
	PRGNAM	,A6,	'CUSCNT'
	RCNT	,D5
	OCNT	,D5
RECORD NXTPRG
	MSGPRG	,A9
RECORD MSGES
	TXCDNF	,A20,  	'TAX CODE NOT ON FILE'
	TRCDNF	,A21,	'TERM CODE NOT ON FILE'


record	funkey
	.include 'def:funkey.def'

;;;record	pop
;;;	.include 'def:pop1.def'

RECORD	POPAR
	.INCLUDE 'DEF:OEPOP.DEF'

RECORD	WN_NAME
		,A5,	'EMKEY'
	WN_TNMBR,D4

RECORD	WARS
	W_ID	,D4
	WND_1	,D4
	w_em	,d4

RECORD	CHANNEL
	CHN101	,D3
	CHN182	,D3
	CHN169	,D3
	CHN170	,D3

RECORD	VARS
	OPNOK	,D1
	F_KEY	,D3
	SHIP_SRCH	,D1
	SRCH_KEY	,A25
	SAVCUS	,D6
	was_email	,d1	;changed email, don't display 2nd screen.
	I	,D6
	A1	,a1
	A6	,A6
	A25	,A25
	EM_FOUND	,D1
	D1	,D1
	XDATE	,D8
	OPTION	,D1
	ENTRY	,A66
	INXCTL  ,D1
	CNGCTL	,D1
	WHATNO	,D2
	lastno	,d2
	DECMAL	,D18
	SELECT	,D1
	KEY   	,A6
	BSEND	,D5
	BS169	,D5
	BS170	,D5
	BSMID	,D5
	BSMID1	,D5
	SRCCTL	,D1
	MAXCNT	,D5
	MSGCTL	,D1
	YESNO	,2A1,	'Y','N'
	STATEMENT	,3A1,	'P','N','E'
	SWITCH	,D1,	1
	V	,D1
	APHONE	,A12
	TCHEND	,D3
	SAVEND	,D5
	AROMID	,D5
	READ	,D1,	0
	WRITE	,D1,	1
	STORE	,D1,	2
	LOKCTL	,D1
	SCRN	,D1,	0
	TCHAR	,D3
	DCHAR	,D3
	TODAY	,D6
	ALPHA	,A12
	ACCEPT	,D1
	SYSTEM	,D1
	TLMASK	,A12,'ZZZ-ZZZ-ZZZZ'
	BLANKS	,A25

PROC
	XCALL TERID (V)
	XCALL ENVRN (SYSTEM)
	XCALL RDATE (TODAY)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	call init_window

DISPLA,
	UNLOCK 1
	CNGCTL =
	CUSMAS =
	SCRN = 1
	was_email = 0
	XCALL w_disp (wnd_1, wd_pos, 1,1,wd_clr, wdc_eow)
	XCALL w_disp (wnd_1, wd_pos, 1,1,'MISC CASH CUSTOMERS')
	
	XCALL w_disp (wnd_1, wd_pos, 4,9,'1. CUST #')
	XCALL w_disp (wnd_1, wd_pos, 6,9,'2. NAME')
	XCALL w_disp (wnd_1, wd_pos, 8,9,'3. ADD-1')
	XCALL w_disp (wnd_1, wd_pos,10,9,'4. ADD-2')
	XCALL w_disp (wnd_1, wd_pos,12,9,'5. CITY')
	XCALL w_disp (wnd_1, wd_pos,14,9,'6. STATE')
	XCALL w_disp (wnd_1, wd_pos,14,29,'7. ZIP')
	XCALL w_disp (wnd_1, wd_pos,16,9,'8. PHONE')
	XCALL w_disp (wnd_1, wd_pos,18,9,'9. SALESMAN')
	XCALL w_disp (wnd_1, wd_pos,20,8,'10. TERRITORY')

	XCALL w_disp (wnd_1, wd_pos, 22,8,'11. FIN CHG?')
	XCALL w_disp (wnd_1, wd_pos, 4,50,'12. CUST TYPE')
	XCALL w_disp (wnd_1, wd_pos, 6,50,'13. SALES-MTD')
	XCALL w_disp (wnd_1, wd_pos, 8,50,'14. SALES-YTD')
	XCALL w_disp (wnd_1, wd_pos, 10,50,'15. COST-MTD')
	XCALL w_disp (wnd_1, wd_pos, 12,50,'16. COST-YTD')
	XCALL w_disp (wnd_1, wd_pos, 14,50,'17. CREDT-LMT')
	XCALL w_disp (wnd_1, wd_pos, 16,50,'18. TERMS')
	XCALL w_disp (wnd_1, wd_pos, 18,50,'19. STATEMENT?')
	XCALL w_disp (wnd_1, wd_pos, 20,50,'20. EMAIL 0=NO,1=YES,2=BOTH')
	XCALL w_disp (wnd_1, wd_pos, 21,50,'21. email address')
	XCALL w_disp (wnd_1, wd_pos, 22,50,'22. TAX CODE')
	IF (SELECT.NE.1) XCALL w_disp (wnd_1, wd_pos, 4,8,'*')
CUSTNO,
	XCALL w_disp (wnd_1, wd_pos, 24,1,'<F1> = NAME SEARCH, <F2> = SEARCH ALL FIELDS')
	CTL = '04,22,06,01,#E'
	CALL INPUT
	GO TO (DISPLA,ENDOFF), INXCTL

	USING F_KEY SELECT
	(F_01, F_02),	BEGIN
			CALL ALPHA_SEARCH
			GOTO (DISPLA,DISPLA),INXCTL
			XCALL W_DISP (WND_1, WD_POS, 4,22, ENTRY(1,6) )
			END
	ENDUSING

	CUSNO = ENTRY (1,6)
	SAVCUS = CUSNO
	IF (CUSNO.EQ.0) GO TO CUSTNO
	
	XCALL ISIO (1, CUSMAS, CUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
	THEN	BEGIN
		XCALL wnmsg(wnd_1,24,'CUSTOMER NOT ON FILE',1)
		GOTO CUSTNO
		END
	ELSE	BEGIN
		SELECT = 2
		GOTO FNDREC
		END

	

	GO TO (ANYCNG), CNGCTL

NAMER,
	CTL = '06,22,25,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	NAME = ENTRY (1,25)
	GO TO (ANYCNG), CNGCTL
ADRS1,
	CTL = '08,22,25,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	ADD1 = ENTRY (1,25)
	GO TO (ANYCNG), CNGCTL
ADRS2,
	CTL = '10,22,21,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	ADD2 = ENTRY (1,21)
	GO TO (ANYCNG), CNGCTL
ADRS3,
	CTL = '12,22,15,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	CITY = ENTRY (1,15)
	GO TO (ANYCNG), CNGCTL
ADRS4,
	CTL = '14,22,02,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	STATE = ENTRY (1,2)
	GO TO (ANYCNG), CNGCTL
ZCODE,
	CTL = '14,37,10,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	ZIP = ENTRY (1,10)
	GO TO (ANYCNG), CNGCTL
PHONE,
	CTL = '16,22,10,00,# '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	PHONE = ENTRY (1,10)
	APHONE = PHONE, TLMASK
	XCALL w_disp (wnd_1, wd_pos, 16,22,APHONE)
	GO TO (ANYCNG), CNGCTL
SLSMEN,
	XCALL w_disp (wnd_1, wd_pos,18,26,BLANKS(1,22))
	CTL = '18,22,02,01,# '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	SLSMAN = ENTRY (1,2)
	IF (SLSMAN.LT.1) GO TO SLSMEN
	LOKCTL = 1
	XCALL IO (4,SALMAN,SLSMAN,READ,LOKCTL)
	IF (SLSNM.EQ.']]]]]]'.OR.SLSNM.EQ.']]]DEL') GO TO BADSLM
	XCALL w_disp (wnd_1, wd_pos,18,26,SLSNM(1,22))
	GO TO (ANYCNG), CNGCTL
TERR,
	CTL = '20,22,02,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	TERR = ENTRY (1,2)
	GO TO (ANYCNG), CNGCTL

;----------------------------------
FCHFLG,
	CTL = '22,22,01,00,YN'
	CALL INPUT
	FCHFLG = INXCTL
	GOTO (ANYCNG), CNGCTL


TYPE,
	CTL = '04,66,02,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	CUSCD = ENTRY (1,2)
	GO TO (ANYCNG), CNGCTL
SALESM,
	CTL = '06,66,10,00,$-'
	CALL INPUT
	GO TO (DISPLA), INXCTL
	SALMTD = ENTRY (1,10)
	GO TO (ANYCNG), CNGCTL
SALESY,
	CTL = '08,66,10,00,$-'
	CALL INPUT
	GO TO (DISPLA), INXCTL
	SALYTD = ENTRY (1,10)
	GO TO (ANYCNG), CNGCTL
COSMTD,
	CTL = '10,66,10,00,$-'
	CALL INPUT
	GO TO (DISPLA), INXCTL
	COSMTD = ENTRY (1,10)
	GO TO (ANYCNG), CNGCTL
COSYTD,
	CTL = '12,66,10,00,$-'
	CALL INPUT
	GO TO (DISPLA), INXCTL
	COSYTD = ENTRY (1,10)
	GO TO (ANYCNG), CNGCTL
CRDLMT,
	CTL = '14,66,06,00,# '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	CRDLMT = ENTRY (1,7)
	GO TO (ANYCNG), CNGCTL
TERMS,
	xcall w_disp(wnd_1, wd_pos, 17,66,wd_clr,wdc_eol)
	CTL = '16,66,01,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	TERMS = ENTRY (1,1)
	IF (TERMS.EQ.BLANKS) GO TO TERMS
	BS170 = ORG170
	SRCCTL = 2
	XCALL SERCH (5,ARTERM,TERMS,1,1,BS170,BSMID1,SRCCTL,4,2,7,0,0,0,0)
	IF (SRCCTL.NE.1) GO TO DISTRM
	MSGCTL = 1
	XCALL wnmsg (wnd_1,24,TRCDNF,MSGCTL)
	GO TO TERMS
DISTRM,
	XCALL w_disp (wnd_1, wd_pos, 17,66,ARTRDS)
	GO TO (ANYCNG), CNGCTL
STMFLG,
;;;	XCALL w_disp (wnd_1, wd_pos, 19,54,'N=NO, E=EMAIL, P=PRINT')
;;;	CTL = '18,66,01,00,A '
;;;	CALL INPUT
;;;	USING ENTRY(1,1) SELECT	
;;;	('N'),	STMFLG = 2	;NONE
;;;	('E'),	STMFLG = 3	;EMAIL
;;;	('P'),	STMFLG = 1	;PRINT
;;;	(),	GOTO STMFLG
;;;	ENDUSING
;;;
;;;	XCALL w_disp (wnd_1, wd_pos, 19,54,wd_clr, wdc_lin)
	GO TO (ANYCNG), CNGCTL
cemail,
	CTL = '20,79,01,00,# '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	cemail = entry(1,1)
	using cemail select
	(0,1,2),	nop
	(),		goto cemail
	endusing

	balmth = 'O'
	if (cngctl) was_email = 1
	GO TO (ANYCNG), CNGCTL
	
	using cemail select
	(1,2),	nop
	(), goto taxflg
	endusing

emaddr,
;;;	xcall w_proc(wp_place,w_em,1,1)	
;;;	xcall w_disp(w_em,wd_clear)
;;;
;;;	XCALL w_disp (w_em,wd_pos, 1,1,wd_clr,wdc_eow,'CUSTOMER EMAIL ADDRESS')
;;;	XCALL w_disp (w_em,wd_pos, 4,05,'1. CUST #')
;;;	XCALL w_disp (w_em,wd_pos, 6,05,'2. Primary or CC (P or C)')
;;;
;;;	XCALL w_disp (w_em,wd_pos, 8,05,'3. KEY                  ')
;;;	XCALL w_disp (w_em,wd_pos,10,05,'4. EMAIL')
;;;
;;;	a6 = cusno, 'ZZZZZX' [left]
;;;	xcall w_disp (w_em,wd_pos,4,17,a6)
;;;	xcall w_disp (w_em,wd_pos,4,25,name)
;;;
;;;	xcall em2(cusno, w_em)
;;;
;;;	xcall w_disp(w_em, wd_clear)
;;;	xcall w_proc(wp_remove, w_em)
;;;	xcall w_updt

	goto (anycng),cngctl

TAXFLG,
	xcall w_disp(wnd_1, wd_pos, 23,50,wd_clr, wdc_eol)
	CTL = '22,66,03,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	TAXFLG =  ENTRY
	IF (TAXFLG.EQ.BLANKS) GO TO ANYCNG
	BS169 = ORG169
	srcctl = 4
	XCALL SERCH (6,ARTCDE,TAXFLG,1,3,BS169,BSMID1,SRCCTL,4,4,9,0,0,0,0)
	IF (SRCCTL.NE.1) GO TO DISTAX
	MSGCTL = 1
	XCALL wnmsg (wnd_1, 24, TXCDNF,MSGCTL)
	GO TO TAXFLG
DISTAX,
	XCALL w_disp (wnd_1, wd_pos, 23,50,ARTDSC)
	GO TO ANYCNG
BADSLM,
	XCALL wnmsg (wnd_1, 24,'SALESMAN NOT ON FILE',1)
	GO TO SLSMEN

DISPL2,						;;;SCRN2
	SCRN = 2
;9-23-13 >>>	if (lastno .eq. 21) goto proces		;3-3-11
	XCALL w_disp (wnd_1, wd_pos, 3,1,wd_clr,wdc_eow)
	XCALL w_disp (wnd_1, wd_pos,  4,20,'   CUSTOMER ')
	ALPHA(1,6) = CUSNO,'ZZZZZX'
	XCALL w_disp (wnd_1, wd_pos,  4,32,ALPHA(1,6))
	XCALL w_disp (wnd_1, wd_pos,  4,40,NAME)
	XCALL w_disp (wnd_1, wd_pos,  6,20,'1. SORT NAME ')
	XCALL w_disp (wnd_1, wd_pos,  8,20,'2. A/P CONTACT')
	XCALL w_disp (wnd_1, wd_pos,  9,20,'3.     PHONE')
	XCALL w_disp (wnd_1, wd_pos,  9,55,'4. FAX PHONE')
	XCALL w_disp (wnd_1, wd_pos, 11,20,'5. AUTH PURCH-1')
	XCALL w_disp (wnd_1, wd_pos, 12,20,'6.           -2')
	XCALL w_disp (wnd_1, wd_pos, 13,20,'7.           -3')
	XCALL w_disp (wnd_1, wd_pos, 14,20,'8.           -4')
	XCALL w_disp (wnd_1, wd_pos, 16,20,'9. DATE STARTED')
	XCALL w_disp (wnd_1, wd_pos, 17,19,'10. LAST SALE DT')
	XCALL w_disp (wnd_1, wd_pos, 18,19,'11. TRW COMMENT CODE')
	XCALL w_disp (wnd_1, wd_pos, 19,19,'12. TRW CR DATE')
	XCALL w_disp (wnd_1, wd_pos, 20,19,'13. A/R COMMENTS')
	XCALL w_disp (wnd_1, wd_pos, 21,01,'14. EMAIL: ')
	IF (SELECT.GT.1) GOTO DSPSC2
ESORNA,
	CTL = '06,40,20,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	ESORNA = ENTRY
	IF (ESORNA.EQ.BLANKS)
	BEGIN
	  ESORNA = NAME
	  XCALL w_disp (wnd_1, wd_pos, ROW,COL,ESORNA)
	END
	GOTO (ANYCNG), CNGCTL
EAPNA,
;;;	CTL = '08,40,25,00,A '
;;;	CALL INPUT
;;;	GOTO (DISPLA), INXCTL
;;;	EAPNA = ENTRY
	GOTO (ANYCNG), CNGCTL
EAPPHO,
	CTL = '09,40,10,00,# '
	XCALL w_disp (wnd_1, wd_pos, 24,1,'FIELD DEFAULTS TO MAIN PHONE NUMBER')
	CALL INPUT
	XCALL w_disp (wnd_1, wd_pos, 24,1,wd_clr, wdc_lin)
	GOTO (DISPLA), INXCTL
	EAPPHO = ENTRY
	IF (EAPPHO.EQ.0) EAPPHO = PHONE
	IF (EAPPHO(1,3).EQ.0.AND.ENTRY.NE.BLANKS) EAPPHO(1,3) = 612
	ALPHA = EAPPHO,TLMASK
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,ALPHA)
	GOTO (ANYCNG), CNGCTL
EFAX,
	CTL = '09,68,10,00,# '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	EFAX = ENTRY
	IF (EFAX(1,3).EQ.0.AND.ENTRY.NE.BLANKS) EFAX(1,3) = 612
	ALPHA = EFAX,TLMASK
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,ALPHA)
	GOTO (ANYCNG), CNGCTL
ECONT1,
	CTL = '11,40,22,00,A '
	IF (CNGCTL.AND.ECONT(1).NE.BLANKS)
	BEGIN
	  CALL ACCEPT
	  GOTO (DISPLA,ANYCNG), ACCEPT
	END
	CALL INPUT
	GOTO (DISPLA), INXCTL
	ECONT(1) = ENTRY
	GOTO (ANYCNG), CNGCTL
ECONT2,
	CTL = '12,40,22,00,A '
	IF (CNGCTL.AND.ECONT(2).NE.BLANKS)
	BEGIN
	  CALL ACCEPT
	  GOTO (DISPLA,ANYCNG), ACCEPT
	END
	CALL INPUT
	GOTO (DISPLA), INXCTL
	ECONT(2) = ENTRY
	GOTO (ANYCNG), CNGCTL
ECONT3,
	CTL = '13,40,22,00,A '
	IF (CNGCTL.AND.ECONT(3).NE.BLANKS)
	BEGIN
	  CALL ACCEPT
	  GOTO (DISPLA,ANYCNG), ACCEPT
	END
	CALL INPUT
	GOTO (DISPLA), INXCTL
	ECONT(3) = ENTRY
	GOTO (ANYCNG), CNGCTL
ECONT4,
	CTL = '14,40,22,00,A '
	IF (CNGCTL.AND.ECONT(4).NE.BLANKS)
	BEGIN
	  CALL ACCEPT
	  GOTO (DISPLA,ANYCNG), ACCEPT
	END
	CALL INPUT
	GOTO (DISPLA), INXCTL
	ECONT(4) = ENTRY
	GOTO (ANYCNG), CNGCTL
ESTART,
	CTL = '16,40,08,00,D '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	XDATE = ENTRY(1,8)

	XCALL DATE8(XDATE, ESTART, D_OUTR, D_FMT, D_SW)
	IF (ESTART.EQ.0)
	BEGIN
	  ESTART = TODAY
	  ENTRY(1,8) = ESTART, 'XX/XX/XX'
	  XCALL w_disp (wnd_1, wd_pos, ROW, COL, ENTRY(1,8))
	  ;;;DECMAL(1,6) = ESTART
	  ;;;CALL DSPDTE
	END
	GOTO (ANYCNG), CNGCTL
ELAST,
	CTL = '17,40,08,00,D '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	XDATE = ENTRY(1,8)

	XCALL DATE8(XDATE, ELAST, D_OUTR, D_FMT, D_SW)

	GOTO (ANYCNG), CNGCTL
ECOMNT,
	CTL = '18,40,02,00,# '
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,wd_clr, wdc_eol)
	CALL INPUT
	GOTO (DISPLA), INXCTL
	ECOMNT = ENTRY
	IF (ECOMNT.LT.0.OR.ECOMNT.GT.89) GOTO ECOMNT
	OPEN (3,I,'SMC:TRWCOM.SMC')
	READ (3,TRWCOM,ECOMNT+1)
	CLOSE 3
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,TCOM)
	GOTO (ANYCNG), CNGCTL
ETRWDT,
	CTL = '19,40,08,00,D '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	XDATE = ENTRY(1,8)

	XCALL DATE8(XDATE, ETRWDT, D_OUTR, D_FMT, D_SW)

	GOTO (ANYCNG), CNGCTL
ARCOM,
	CTL = '20,40,20,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	ARCOM = ENTRY
	GOTO ANYCNG

EMAIL,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	CLEAR TBL_KEY
;;;	TBLCOD = 'EM'
;;;	EM_CUST = CUSNO
;;;	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
;;;	IF (LOKCTL .EQ. 0) 
;;;	THEN	EM_FOUND = 1
;;;	ELSE	BEGIN
;;;		CLEAR COPTBL
;;;		EM_FOUND = 0
;;;		END
;;;
;;;
;;;	CTL = '21,13,66,00,A '
;;;	CALL INPUT
;;;	GOTO (DISPLA),INXCTL
;;;	em_email = entry(1,66)
;;;
;;;	IF (EM_FOUND .EQ. 1)
;;;	THEN	XCALL ISIO (CHN182, COPTBL, TBL_KEY, WRITE, LOKCTL)
;;;	ELSE	BEGIN
;;;		TBLCOD = 'EM'
;;;		EM_CUST = CUSNO
;;;		store (chn182, coptbl, tbl_key)
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	GOTO ANYCNG

PROCES,
	GOTO CHANGE
;;;	GO TO (ADD,CHANGE), SELECT
FNDREC,
DSPSC1,
	XCALL w_disp (wnd_1, wd_pos, 24,33,wd_clr,wdc_eol)
	XCALL w_disp (wnd_1, wd_pos, 4,30,'BAL:')
	DECMAL = OSTDCR
	CTL = '04,35,08'
	CALL DSPDLR
	XCALL w_disp (wnd_1, wd_pos, 6,22,NAME)
	XCALL w_disp (wnd_1, wd_pos, 8,22,ADD1)
	XCALL w_disp (wnd_1, wd_pos,10,22,ADD2)
	XCALL w_disp (wnd_1, wd_pos,12,22,CITY)
	XCALL w_disp (wnd_1, wd_pos,14,22,STATE)
	XCALL w_disp (wnd_1, wd_pos,14,37,ZIP)
	APHONE = PHONE, TLMASK
	IF (APHONE(1,3).EQ.BLANKS) APHONE (4,4) = BLANKS
	IF (APHONE(5,7).EQ.BLANKS) APHONE (8,8) = BLANKS
	XCALL w_disp (wnd_1, wd_pos, 16,22,APHONE)
	DECMAL = SLSMAN
	CTL = '18,22,02'
	CALL DSPNUM
	XCALL w_disp (wnd_1, wd_pos, 20,22,TERR)
	IF (FCHFLG .LE. 0) FCHFLG = 2	;NO
	XCALL w_disp (wnd_1, wd_pos, 22,22,YESNO(FCHFLG))

	XCALL w_disp (wnd_1, wd_pos, 4,66,CUSCD)
	DECMAL = SALMTD
	CTL = '06,66,10'
	CALL DSPDLR
	DECMAL = SALYTD
	ROW = 8
	CALL DSPDLR
	DECMAL = COSMTD
	ROW = 10
	CALL DSPDLR
	DECMAL = COSYTD
	ROW = 12
	CALL DSPDLR
	CTL = '14,66,07'
	DECMAL = CRDLMT
	CALL DSPNUM
	BS170 = ORG170
	XCALL SERCH (5,ARTERM,TERMS,1,1,BS170,BSMID1,SRCCTL,4,2,7,0,0,0,0)
	IF (SRCCTL.EQ.1) ARTRDS = 'TERMS NOT ON FILE'
	XCALL w_disp (wnd_1, wd_pos, 16,66,TERMS)
	XCALL w_disp (wnd_1, wd_pos, 17,66,ARTRDS)
;;;	XCALL w_disp (wnd_1, wd_pos, 18,66,STATEMENT(STMFLG))
	a1 = cemail
	XCALL w_disp (wnd_1, wd_pos, 20,79,a1)
	IF (TAXFLG.EQ.BLANKS) GO TO WHTNXT
	BS169 = ORG169
	srcctl = 4
	XCALL SERCH (6,ARTCDE,TAXFLG,1,3,BS169,BSMID1,SRCCTL,4,4,9,0,0,0,0)
	IF (SRCCTL.EQ.1) ARTDSC = 'TAX CODE NOT ON FILE'
	XCALL w_disp (wnd_1, wd_pos, 22,66,TAXFLG)
	XCALL w_disp (wnd_1, wd_pos, 23,50,ARTDSC)
	GOTO WHTNXT
DSPSC2,
	CTL = '06,40,20,00,A '
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,ESORNA)
	CTL = '08,40,25,00,A '
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,EAPNA)
	CTL = '09,40,10,00,# '
	ALPHA = EAPPHO,TLMASK
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,ALPHA)
	CTL = '09,68,10,00,# '
	ALPHA = EFAX,TLMASK
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,ALPHA)
	CTL = '11,40,22,00,L '
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,ECONT(1))
	CTL = '12,40,22,00,L '
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,ECONT(2))
	CTL = '13,40,22,00,L '
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,ECONT(3))
	CTL = '14,40,22,00,A '
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,ECONT(4))

	CTL = '16,40,06,00,D '
	ENTRY(1,8) = ESTART, 'XX/XX/XX'
	XCALL w_disp (wnd_1, wd_pos, ROW, COL, ENTRY(1,8))

	CTL = '17,40,06,00,D '
	ENTRY(1,8) = ELAST, 'XX/XX/XX'
	XCALL w_disp (wnd_1, wd_pos, ROW, COL, ENTRY(1,8))

	IF (ECOMNT.EQ.0) ECOMNT =
	OPEN (13,I,'SMC:TRWCOM.SMC')
	READ (13,TRWCOM,ECOMNT+1)
	CLOSE 13
	XCALL w_disp (wnd_1, wd_pos, 18,40,TCOM)

	ENTRY(1,8) = ETRWDT, 'XX/XX/XX'
	XCALL w_disp (wnd_1, wd_pos, 19, 40, ENTRY(1,8))

	XCALL w_disp (wnd_1, wd_pos, 20,40,ARCOM)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	CLEAR TBL_KEY
;;;	TBLCOD = 'EM'
;;;	EM_CUST = CUSNO
;;;	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
;;;	IF (LOKCTL .EQ. 0) 
;;;	THEN	EM_FOUND = 1
;;;	ELSE	BEGIN
;;;		CLEAR COPTBL
;;;		EM_FOUND = 0
;;;		END
;;;
;;;	XCALL w_disp (wnd_1, wd_pos, 21,13,EM_EMAIL)
;;;	xcall w_updt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WHTNXT,
	IF (SELECT.NE.3) GO TO ANYCNG
	IF (SCRN.EQ.1) XCALL wnmsg(wnd_1,24,'PRESS CR FOR SECOND SCREEN',2)
	IF (SCRN.EQ.1) GOTO DISPL2
	XCALL w_disp (wnd_1, wd_pos, 24,1,wd_clr,wdc_lin,'OK TO DELETE ?')
	CTL = '24,17,01,00,YN'
	CALL INPUT
	IF (INXCTL.EQ.1) GOTO DELETE
	GOTO DISPLA
ANYCNG,
	xcall w_updt
	lastno = whatno				;save so can exit after email
	XCALL wANCN (wnd_1, 24,CNGCTL,WHATNO)
	IF (SCRN.EQ.1) GO TO (DISPL2,CNGBR1,DISPLA), CNGCTL + 1
	IF (SCRN.EQ.2) GO TO (PROCES,CNGBR2,DISPLA), CNGCTL + 1
CNGBR1,
	GO TO (CUSTNO,NAMER,ADRS1,ADRS2,ADRS3,ADRS4,ZCODE,PHONE,SLSMEN,TERR,
&		FCHFLG,TYPE,SALESM,SALESY,COSMTD,COSYTD,CRDLMT,TERMS,
&		STMFLG,cemail,emaddr,TAXFLG), WHATNO
	GOTO BADCNG
CNGBR2,
	GOTO (ESORNA,EAPNA,EAPPHO,EFAX,ECONT1,ECONT2,ECONT3,ECONT4,ESTART,ELAST,
&		ECOMNT,ETRWDT,ARCOM,EMAIL), WHATNO
BADCNG,
	CNGCTL = 3
	GO TO ANYCNG
CHANGE,
	LOKCTL = 1
	XCALL ISIO (1,CUSMAS,CUSNO,WRITE,LOKCTL)
	IF (SELECT .EQ. 2) GOTO DISPLA
	
GETCHR,
	XCALL wnmsg(wnd_1,24,'CUSTOMER DELETED',2)
FREBUF,
	GO TO DISPLA
DELETE,
	IF (SALYTD.EQ.0) GO TO CHKARO
	XCALL wnmsg
&	 (wnd_1, 24, 'CUSTOMER HAS NON-ZER0 SALES YEAR-TO-DATE. DELETION NOT ALLOWED',1)
	GO TO DISPLA
CHKARO,
	XCALL SERCH (3,AROPEN,CUSNO,16,21,SAVEND,AROMID,SRCCTL,4,8,15,0,0,0,0)
	IF (SRCCTL) GO TO DELET1
	XCALL wnmsg(wnd_1,24,'CUSTOMER HAS ACTIVE ACCOUNT.  DELETION NOT ALLOWED',1)
	GO TO DISPLA
DELET1,

;;;	XCALL DCPRT(CUSMAS)

;;; SSQ 12-4-96  save name...
	ESORNA = NAME
	NAME = ']]]DEL'
	GO TO CHANGE
INPUT,
	XCALL wINPT (wnd_1,ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,F_KEY)
	RETURN
ACCEPT,
	XCALL FLAGS (00010000)			;DISABLE CHARACTER ECHOING
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,' ')
	ACCEPT (15,TCHAR)
	IF (SYSTEM.NE.1.AND.TCHAR.EQ.13) ACCEPT (15,DCHAR)
	XCALL FLAGS (00000000)			;RE-ENABLE CHARACTER ECHOING
	ACCEPT =
	IF (TCHAR.EQ.23) ACCEPT = 1	;CTRL/W
	IF (TCHAR.EQ.13) ACCEPT = 2	;RETURN
	IF (TCHAR.EQ.9) ACCEPT = 3	;TAB
	IF (ACCEPT.EQ.0) GOTO ACCEPT
	RETURN
DSPNUM,
	OPTION = 1
	GO TO CALDSP
DSPDTE,
	OPTION = 2
	GO TO CALDSP
DSPDLR,
	OPTION = 3
CALDSP,
	XCALL wDSPL (wnd_1, MAX,ROW,COL,DECMAL,OPTION)
	RETURN

ALPHA_SEARCH,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	XCALL w_disp (wnd_1, wd_pos, 24,1, wd_clr, wdc_eol,'SEARCH STRING: ')
	CTL = '24,16,25,00,AE'
	XCALL wINPT (wnd_1,ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL)
	IF (INXCTL .NE. 0) RETURN
	SRCH_KEY = ENTRY(1,25)

MORE_SHIP,
	CALL GET_MIS
	IF (%TTSTS) XCALL W_DISP(WND_1,WD_ACCEPT,DECMAL)
	IF (NUMARA .GT. 0)
	THEN	BEGIN
		DLINE = '  MISC #  NAME'
		POP_TITLE = "<CR> = MORE CUSTOMERS"
		XCALL OEPOP(POPAR)
		END
	ELSE	BEGIN
		XCALL OLMSG(WND_1,24,"NO MORE MISC CUSTOMERS",1)
		CLEAR ENTRY
		INXCTL = 1
		RETURN
		END

	USING P_ACTION SELECT
	(0),	INXCTL = 1		;<END>
	(1),	BEGIN			;SELECTED
		DLINE = PARRY(PI)	
		ENTRY = DLINE(1,6)	;SHIPTO
		CLEAR INXCTL
		END
	(4),	GOTO MORE_SHIP

	ENDUSING

	RETURN
;------------------------------------------------------

GET_MIS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	USING F_KEY SELECT
	(F_01),	FIND (CHN101, CUSMAS, SRCH_KEY, KRF=1)[ERR=GO_MIS]
	(F_02),	FIND (CHN101, CUSMAS, ^FIRST)[ERR=GO_MIS]
	ENDUSING

GO_MIS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR I
MS_LOOP,
	XCALL IOS (CHN101,CUSMAS,READ,LOKCTL)
	IF (LOKCTL.NE.0) GOTO MS_EOF
	IF (CUSNO .LT. 100) GOTO MS_LOOP	;CUST 0-99 reserved

	USING F_KEY SELECT
	(F_02),	BEGIN			;SEARCH ALL
		SHIP_SRCH = 0		;NO MATCH
		IF (%INSTR(1,NAME,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (%INSTR(1,ADD1,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (%INSTR(1,ADD2,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (%INSTR(1,CITY,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (%INSTR(1,STATE,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (%INSTR(1,ZIP,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (SHIP_SRCH .EQ. 0) GOTO MS_LOOP
		END
	ENDUSING

	CLEAR DLINE
	DLINE(1,6) = CUSNO,	'ZZZZZZ' [LEFT]
	DLINE(8,40) = NAME
	INCR I
	PARRY(I) = DLINE
	
	DLINE (1,25) = ADD1
	DLINE (26,46) = ADD2
	DLINE (47,61) = CITY
	DLINE (63,64) = STATE
	DLINE (66,75) = ZIP
	P_ADD(I) = DLINE
	IF (I .LT. MAXARA) GOTO MS_LOOP
MS_EOF,
	NUMARA = I
	RETURN
;------------------------------------------------	

;======================================================================


ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:MISMNU',1)
LIST,
	CALL CLOSE

	XCALL PGCHN ('CP:MISPRT',1)

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	OPNOK = 0

	SWITCH = 5
	XCALL FILES (1, 'SU', 101, SWITCH)	;101 - MISCUS
	IF (SWITCH .EQ. 9) RETURN
	CHN101 = 1

;;;	SWITCH = 5
;;;	XCALL FILES (17, 'SI', 182, SWITCH)	;182 - COPTBL
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN182 = 17

	SWITCH = 1
	XCALL FILES (5,'I',170,SWITCH)		; ARTERM
	IF (SWITCH.EQ.9) RETURN
	CHN170 = 5
	XCALL IO (5,ARTCTL,1,READ,LOKCTL)

	SWITCH = 1
	XCALL FILES (6,'I',169,SWITCH)		; ARTCDE
	IF (SWITCH.EQ.9) RETURN
	CHN169 = 6

	OPNOK = 1

	RETURN
;----------------------------------------------------------------

CLOSE ,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN101) CLOSE CHN101
	IF (CHN182) CLOSE CHN182
	IF (CHN170) XCALL FILES (CHN170, 'I', 170, 4)
	IF (CHN169) XCALL FILES (CHN169, 'I', 169, 4)

	RETURN
;----------------------------------------------------------------

INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall u_start(,,,,,,,99)

	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (WND_1.EQ.0)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)	;full size
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'Customer Maintenance',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,2,2)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1

;; POP info...

	MAXARA = 10		
	PLEN = 34
	NUMROW = 10
	WX = 8
	WY = 7
	POP_WID(1,5) = "OEPOP"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "CUSTOMER NAMES"

;for email adress's
;;;	XCALL W_PROC(WP_CREATE,W_EM,'email',0,0)	;full size
;;;	XCALL W_DISP(W_EM,WD_CLEAR)


;; POP info...

;;;	MAXARA = 20
;;;	PLEN = 70
;;;	NUMROW = 15
;;;	WX = 12
;;;	WY = 4
;;;	POP_WID(1,5) = "EMAIL"
;;;	POP_WID(6,8) = WN_TNMBR,	'XXX'
;;;	POP_TITLE = "Email Addr"

	RETURN
;-----------------------------------------------------------------


END
