;child.cp
;

	.include 'wnd:windows.def'

record	parent
	.include 'def:rd162a.def'

record	child
	.include 'def:rd163a.def'

record	itmmas
	.include 'def:rd041a.def'

record	itmkey
	.include 'def:rd041k.def'
	
RECORD	POP1
	.INCLUDE 'DEF:POP1.DEF'

RECORD
	TITLE	,A*,	'COMPONENT ITEMS'

RECORD HDR1
		,A4,	'CODE'
		,A2
		,A11,	'PARENT ITEM'

RECORD	PRINT
	NOHDR	,A6,	'NO HDR'
	LINCNT	,D2,	60
	SPLFIL	,A14
	LPONSW	,D1
	LPARG	,D1
	RPTNUM	,D3
	PRTTYP	,A1
	PGCNT	,D6,	000000
	PLINE	,A80
	PRTCTL	,D3,	080
	LPSW	,D2

RECORD	CTL
	MIN	,D2
	MAX	,D2
	TYPE	,A2
	ROW	,D2
	COL	,D2

RECORD	NXT_LINE
	NL_NUM	,A3	
		,A1
	LN_LINE	,A9,	'NEXT SEG:'

record	channels
	chn163	,d2
	chn162	,d2
	chn041	,d2

RECORD	WN_NAME
		,A5,	'COMPO'
	WN_TNMBR,D4

RECORD	WARS
	W_ID	,D4
	WND_1	,D4
	
record	vars
	opnok	,d1
	pgm		,d1
	tstat	,d1
	achar	,a1
	savrfa	,a6
	A15	,A15
	SAVPAR	,A15
	XCODE	,A15
	XPAR,	A15
	XF1		,D3
	XF2		,D3
	XF3		,D5
	XBEG	,D2
	XEND	,D2
	XQTY	,D4
	TL	,D6
	LN	,D6
	XSEG	,A2
	SEGARA	,10A2
	ITEM	,A15
	i	,d6
	j	,d6
	nseg	,d6
	msgctl	,d1
	entry	,a30
	inxctl	,d1
	whatno	,d2
	cngctl	,d1
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	delete	,d1,3
	switch	,d1
	v	,d1
proc
	xcall terid(v)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	call init_window

MENU,
	XCALL W_DISP(WND_1,WD_CLEAR)
	xcall w_disp (wnd_1, wd_pos, 1,1,'COMPONENT ITEM MAINTENANCE')
	xcall w_disp (wnd_1, wd_pos, 3,9,'PLEASE SELECT APPLICATION')
	xcall w_disp (wnd_1, wd_pos, 5,15,'1. TABLE MAINTENANCE')
	xcall w_disp (wnd_1, wd_pos, 6,15,'2. PRINT TABLE')
MINPUT,
	xcall winpt (wnd_1, 3,36,1,1,'#E',ENTRY,INXCTL)
	GOTO (MINPUT,ENDOFF), INXCTL
	PGM = ENTRY(1,1)
	GOTO (DISPLA,PRINT_TABLE),PGM
	GOTO MINPUT

DISPLA,
	CLEAR CNGCTL
	XCALL W_DISP(WND_1,WD_CLEAR)
	xcall w_disp (wnd_1, wd_pos, 1,1,'COMPONENT ITEM MAINTENANCE')
	
	xcall w_disp (wnd_1, wd_pos,  4,4,'1. PARENT ITEM')
	xcall w_disp (wnd_1, wd_pos,  6,4,'2. CODE')
	xcall w_disp (wnd_1, wd_pos,  8,4,'3. F2 NOTE')
	xcall w_disp (wnd_1, wd_pos, 10,4,'4. F3 NOTE')
	xcall w_disp (wnd_1, wd_pos, 12,4,'5. XX-BEG')
	xcall w_disp (wnd_1, wd_pos, 14,4,'6. XX-END')
;;;	xcall w_disp (wnd_1, wd_pos, 12,4,'5. QTY PER')
	
	
PAR,
	XCALL winpt (wnd_1, 4, 19, 15, 00, 'AE', ENTRY, INXCTL)
	GOTO (DISPLA, MENU), INXCTL
	
	XPAR = ENTRY(1,15)
	IF (XPAR .EQ. A15)
		BEGIN
		XPAR = SAVPAR
		xcall w_disp (wnd_1, wd_pos, 4,19,XPAR)
		END
	SAVPAR = XPAR


	XCALL ISIO (chn162, PARENT, XPAR, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		XCALL wnmsg(wnd_1, 24,'PARENT NOT ON FILE!',1)
		GOTO DISPLA
		END

	CALL GET_PARENT

	DO BEGIN
		XCALL TTSTS (TSTAT)
		IF (TSTAT) xcall w_disp(wnd_1,wd_accept,achar)
		END UNTIL (TSTAT.EQ.0)

	DLINE = 'PARENT           COMPONENT'
	IF (NUMARA) 
	THEN	XCALL POP1(POP1)		;FOR MATCHING CODES
	ELSE	P_ACTION = 2			;ADD NEW


	USING P_ACTION SELECT
	(2),	NOP		;;;CALL ADD_NEW
	(3),	BEGIN
			SAVRFA = PARRFA(PI)
			READ (CHN163, CHILD, RFA:SAVRFA) [ERR=DISPLA]
			UNLOCK CHN163
			CALL DELETE
			GOTO DISPLA
			END
	(),	GOTO DISPLA
	ENDUSING

	
CODE,
	xcall winpt (wnd_1, 6,19,15,00,'AE',ENTRY, INXCTL)
	GOTO (DISPLA,MENU),INXCTL
	XCODE = ENTRY(1,15)

F2,
	xcall winpt (wnd_1, 8,19,03,00,'# ',ENTRY,INXCTL)
	GOTO (DISPLA),INXCTL
	XF2 = ENTRY(1,3)
	
F3,
	ROW = 10
	xcall winpt (wnd_1, 10,19,05,00,'# ',ENTRY,INXCTL)
	GOTO (DISPLA),INXCTL
	XF3 = ENTRY(1,5)
	
	CLEAR ITMKEY
	k_item = xcode
	k_f1 = xf1
	k_f2 = xf2
	k_f3 = xf3
	
	XCALL ISIO (CHN041, ITMMAS, ITMKEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0)
	THEN	BEGIN
			MSGCTL = 7
			XCALL wnmsg (wnd_1, 24,"USE THIS ITEM (Y/N)?", MSGCTL)
			IF (MSGCTL .EQ. 1) CALL ADD_NEW
			END
	ELSE	CALL ADD_NEW

XBEG,
	ROW = 12
	XCALL winpt (wnd_1, 12,19,02,00,'# ', ENTRY, INXCTL)
	GOTO (DISPLA),INXCTL
	XBEG = ENTRY(1,2)
XEND,
	ROW = 14
	xcall winpt (wnd_1, 14,19,02,00,'# ',ENTRY,INXCTL)
	GOTO (DISPLA),INXCTL
	XEND = ENTRY(1,2)

QTY,
	ROW = ROW + 2
	xcall w_disp (wnd_1, wd_pos, ROW,4,'5. QTY PER')
	xcall winpt (wnd_1, ROW,19,04,00,'# ',ENTRY,INXCTL)
	GOTO (DISPLA),INXCTL
	XQTY = ENTRY(1,4)



	xcall w_disp (wnd_1, wd_pos, 24,1,'STORE ITEM?')
	xcall winpt (wnd_1, 24,15,01,01,'YN',ENTRY,INXCTL)
	IF (ENTRY(1,1) .EQ. 'N') RETURN


	CLEAR CHILD
	C_PAR = XPAR
	C_CODE = XCODE
	C_F1 = XF1
	C_F2 = XF2
	C_F3 = XF3
	C_BEG = XBEG
	C_END = XEND
	C_QTY = XQTY

	FOR J FROM 1 THRU nseg+1
		BEGIN
		C_SEG(J) = SEGARA(J)		;JUST IN CASE...
		SEGARA(J) =
		END
		
	STORE (chn163, CHILD, C_KEY)
	GOTO DISPLA

ENDOFF,
	CALL CLOSE
	xcall pgchn ('cp:tblmnu', 1)

	STOP

;======================================================================
GET_PARENT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FOR I FROM 1 THRU MAXARA
		BEGIN
		PARRY(I) =
		PARRFA(I) = 
		PFLAGS(I) =
		END

	I = 0

	FIND (CHN163, CHILD, XPAR) [ERR=GP_LOOP]
GP_LOOP,
	XCALL IOS (CHN163, CHILD, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO GP_EOF
	IF (C_PAR .NE. XPAR) GOTO GP_EOF
	DLINE(1,15) = C_PAR
	DLINE(17,32) = C_CODE

	FOR J FROM 1 THRU 4
		BEGIN
		IF (C_SEG(J) .EQ. '  ') EXITLOOP
		TL = %TRIM(DLINE)
		DLINE(TL+1, TL+2) = C_SEG(J)
		END
	

	INCR I
	PARRY(I) = DLINE
	XCALL GETRFA(CHN163, PARRFA(I) )
	GOTO GP_LOOP
	

GP_EOF,
	NUMARA = I
	RETURN
;---------------------------------------------------
DELETE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; DELETE CHILD RECORD
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		xcall w_disp (wnd_1, wd_pos, 24,1,'DELETE THIS COMPONENT?')
		xcall winpt (wnd_1, 24, 27, 01, 01, 'YN', ENTRY, INXCTL)
		GOTO (D_DONE),INXCTL-1
		READ (CHN163, CHILD, RFA=SAVRFA)
		XCALL ISIO (CHN163, CHILD, C_PAR, DELETE, LOKCTL)
		IF (LOKCTL .NE. 0) XCALL wnmsg (wnd_1, 24,'COMPONENT NOT DELETED!!',1)
		
D_DONE,
		
	RETURN
;------------------------------------------------


ADD_NEW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; ADD NEW COMPONENT
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR ITEM
	FOR I FROM 1 THRU 10 CLEAR SEGARA(I)

	ITEM = XCODE

	ROW = 16
	FOR nseg FROM 1 THRU 4			;just 4 segs for components
		BEGIN
		ROW = ROW + 2
		NL_NUM = nseg, 'ZX.' [LEFT]
	GET_SEG,
		xcall w_disp (wnd_1, wd_pos, ROW, 4,  NXT_LINE)
		xcall winpt (wnd_1, ROW, 18, 02, 00, 'AE',ENTRY,INXCTL)

		USING INXCTL SELECT
		(1), EXITLOOP
		(2), EXITLOOP
		ENDUSING

		XSEG = ENTRY(1,2)
		TL = %TRIMZ(XSEG)
		IF (TL .EQ. 0) EXITLOOP

		USING XSEG SELECT
		('*'),	NOP
		('4','9'), NOP
	;;;	('45','90'), NOP
		('S'), NOP
		('GA'), NOP
		('XX'), NOP
		('YY'), NOP
		('ZZ'), NOP
		(),	GOTO GET_SEG
		ENDUSING

		LN = %TRIM(ITEM)
		INCR LN
		ITEM(LN,LN+TL) = XSEG
		xcall w_disp (wnd_1, wd_pos, 4,25,ITEM)
		SEGARA(nseg) = XSEG	
		END


	RETURN
;---------------------------------------------------
PRINT_TABLE,
	FIND (CHN162, PARENT, ^FIRST) [ERR=PT_LOOP]
PT_LOOP,
	XCALL IOS (CHN162, PARENT, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO PT_EOF

	PLINE(1,4) = P_CODE
	PLINE(7,21) = P_PAR
	CALL PRINT
	
	FIND (CHN163, CHILD, P_PAR) [ERR=CLOOP]
CLOOP,
	XCALL IOS (CHN163, CHILD, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO C_EOF
	IF (C_PAR .NE. P_PAR) GOTO C_EOF
	
	PLINE (5,8) = C_QTY, 'ZZZX'
	PLINE (9,9) = '-'

	PLINE (11,26) = C_CODE
	FOR I FROM 1 THRU 4
		BEGIN
		IF (C_SEG(I) .EQ. '  ') EXITLOOP
		TL = %TRIM(PLINE)
		PLINE(TL+1, TL+2) = C_SEG(I)
		END
	
	TL = %TRIM(PLINE)
	IF (C_BEG .GT. 0)
		BEGIN
		TL = TL + 2
		PLINE(TL,TL+3) = C_BEG, '(XX,'
		PLINE(TL+4,TL+6) = C_END, 'XX)'
		TL = %TRIM(PLINE)
		END

	TL = TL + 5
	IF (C_F2 .GT. 0) PLINE (TL,TL+4) = C_F2, 'F2=XX'

	TL = %TRIM(PLINE)
	IF (C_F3 .GT. 0) PLINE (TL+3,TL+7) = C_F3, 'F3=XX'
	CALL PRINT
	GOTO CLOOP
C_EOF,
	GOTO PT_LOOP

PT_EOF,
	IF (LPONSW.EQ.1) XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	LPONSW = 0
	LINCNT = 60
	PGCNT =
	GOTO MENU
PRINT,
	IF (LPONSW.EQ.0) CALL PRNTON
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,NOHDR,NOHDR,
&			'NO LEGEND',' ',' ',1,80,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
PRNTON,
	SPLFIL (5,6) = 'EL'
	LPSW = 1	; PRINT, SPOOL OR DISPLAY
	XCALL LPONw (LPSW,SPLFIL, wnd_1)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPONSW = 1
	RETURN
;*****	
	
	OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	OPNOK = 0

;;;	OPEN (1, SI, 'PARENT.ISM')
	switch = 5
	xcall files (1, 'SI', 162, switch)	;162 = parent.ism
	if (switch .eq. 9) return
	chn162 = 1
	
;;;	OPEN (2, SU, 'CHILDX.ISM')
	switch = 5
	xcall files (2, 'SU', 163, switch)
	if (switch .eq. 9) return

	chn163 = 2
	
;;;	OPEN (3, SI, 'SMC:ITMMAS.SMM')
	xcall files (3, 'SI', 41, switch)
	if (switch .eq. 9) return
	CHN041 = 3
 	
	OPNOK = 1
	RETURN
;--------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (chn163) CLOSE chn163
	IF (chn162) CLOSE chn162
	IF (CHN041) CLOSE CHN041

	RETURN
;--------------------------------------------------

INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall u_start(,,,,,,,99)

	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (WND_1.EQ.0)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'COMPONENT ITEM MAINTENANCE',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,2,2)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1
;; POP info...

	MAXARA = 20
	PLEN = 32
	NUMROW = 5
	WX = 4
	WY = 30
	POP_WID(1,5) = "PCPOP"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "COMPONENT ITEMS"
	RETURN
;-----------------------------------------------------------------

END
