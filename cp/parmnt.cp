;parmnt.cp
;
	.include 'wnd:windows.def'

record	parent
	.include 'def:rd162a.def'

;
RECORD	POP1
	.INCLUDE 'DEF:POP1.DEF'


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
	CHN162	,d2
RECORD
	TITLE	,A*,	'PARENT ITEMS'

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

RECORD	WN_NAME
		,A5,	'PAREN'
	WN_TNMBR,D4

RECORD	WARS
	W_ID	,D4
	WND_1	,D4

record	vars
	opnok	,d1
	PGM	,D3
	TSTAT	,D1
	ACHAR	,A1
	SAVRFA	,A6
	XCODE	,A3
	TL	,D6
	LN	,D6
	XSEG	,A2
	SEGARA	,10A2
	ITEM	,A15
	i	,d6
	j	,d6
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
;;;	xcall outpt (1,1,2,'PARENT ITEM MAINTENANCE',1)
	CALL INIT_WINDOW
;
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

MENU,
	XCALL W_DISP(W_ID, WD_CLEAR)
	XCALL W_DISP(WND_1, WD_POS, 1,1,TITLE)
	XCALL W_DISP(WND_1, WD_POS,3,9,'PLEASE SELECT APPLICATION')
	XCALL W_DISP(WND_1, WD_POS,5,15,'1. TABLE MAINTENANCE')
	XCALL W_DISP(WND_1, WD_POS,6,15,'2. PRINT TABLE')
MINPUT,
	XCALL WINPT (WND_1, 3,36,1,1,'#E',ENTRY,INXCTL)
	GOTO (MINPUT,ENDOFF), INXCTL
	PGM = ENTRY(1,1)
	GOTO (DISPLA,PRINT_TABLE),PGM
	GOTO MINPUT

DISPLA,
	CLEAR CNGCTL
	XCALL W_DISP(W_ID, WD_CLEAR)

	XCALL W_DISP(WND_1, WD_POS,1,1,'PARENT ITEM MAINTENANCE')
	XCALL W_DISP(WND_1, WD_POS,4,4,'1. CODE')
	XCALL WINPT (WND_1, 4, 12,03,00,'AE',ENTRY,INXCTL)
	GOTO (DISPLA,ENDOFF),INXCTL
	XCODE = ENTRY(1,3) 
	
	CALL GET_PARENT

	DO BEGIN
	  XCALL TTSTS (TSTAT)
	  IF (TSTAT) xcall w_disp(wnd_1,wd_accept,achar)
	END UNTIL (TSTAT.EQ.0)

	DLINE = 'CODE PARENT-ITEM'
	IF (NUMARA) 
	THEN	XCALL POP1(POP1)		;FOR MATCHING CODES
	ELSE	P_ACTION = 2			;ADD NEW


	USING P_ACTION SELECT
	(2),	CALL ADD_NEW
	(3),	BEGIN
		SAVRFA = PARRFA(PI)
		READ (CHN162, PARENT, RFA:SAVRFA) [ERR=DISPLA]
		UNLOCK CHN162
		CALL DELETE
		END
	(),	GOTO DISPLA
	ENDUSING

	GOTO DISPLA

ENDOFF,
	CALL CLOSE
	xcall pgchn ('cp:tblmnu',1)
	STOP

GET_PARENT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FOR I FROM 1 THRU MAXARA
		BEGIN
		PARRY(I) =
		PARRFA(I) = 
		PFLAGS(I) =
		END

	I = 0

	FIND (CHN162, PARENT, XCODE, KRF=1) [ERR=GP_LOOP]
GP_LOOP,
	XCALL IOS (CHN162, PARENT, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO GP_EOF
	IF (P_CODE .NE. XCODE) GOTO GP_EOF
	DLINE(1,3) = P_CODE
	DLINE(6,20) = P_PAR
	INCR I
	PARRY(I) = DLINE
	XCALL GETRFA(CHN162, PARRFA(I) )
	GOTO GP_LOOP
	

GP_EOF,
	NUMARA = I
	RETURN
;---------------------------------------------------

ADD_NEW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; ADD NEW PARENT
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR ITEM
	FOR I FROM 1 THRU 10 CLEAR SEGARA(I)

	ITEM = XCODE

	ROW = 4
	FOR I FROM 1 THRU 10
		BEGIN
		ROW = ROW + 2
		NL_NUM = I, 'ZX.' [LEFT]
	GET_SEG,
		XCALL W_DISP(WND_1, WD_POS, ROW, 4, NXT_LINE)
		XCALL WINPT (WND_1,ROW, 18, 02, 00, 'AE',ENTRY,INXCTL)

		USING INXCTL SELECT
		(1), EXITLOOP
		(2), EXITLOOP
		ENDUSING

		XSEG = ENTRY(1,2)
		TL = %TRIMZ(XSEG)
		IF (TL .EQ. 0) EXITLOOP

		USING XSEG SELECT
		('*'),	NOP
		('45','90'), NOP
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
		XCALL W_DISP(WND_1, wd_pos,4,25,ITEM)
		SEGARA(I) = XSEG	
		END


	XCALL W_DISP(WND_1, WD_POS,24,1,'STORE ITEM?')
	XCALL WINPT (WND_1,24,15,01,01,'YN',ENTRY,INXCTL)
	IF (ENTRY(1,1) .EQ. 'N') RETURN


	CLEAR PARENT
	P_PAR = ITEM
	P_CODE = XCODE
	FOR J FROM 1 THRU I+1	P_SEG(J) = SEGARA(J)		;JUST IN CASE...
		
	XCALL ISIO (CHN162, PARENT, P_PAR, STORE, LOKCTL)
	IF (LOKCTL .NE. 0) XCALL WNMSG (WND_1, 23,'RECORD NOT STORED!',1)

	RETURN
;---------------------------------------------------

DELETE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL ISIO (CHN162, PARENT, P_PAR, DELETE, LOKCTL)
	IF (LOKCTL .NE. 0) XCALL WNMSG (WND_1, 23,'RECORD NOT DELETED!',1)

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
	XCALL LPONw (LPSW,SPLFIL,wnd_1)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPONSW = 1
	RETURN
;************************************************************************


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	OPNOK = 0

	SWITCH = 5
	XCALL FILES (1, 'SU', 162, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN162 = 1

	
	OPNOK = 1
	RETURN
;--------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN162) CLOSE CHN162

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
	XCALL W_BRDR(WND_1,WB_TITLE,'PARENT ITEM MAINTENANCE',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,2,2)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1
;; POP info...

	MAXARA = 20
	PLEN = 22
	NUMROW = 5
	WX = 4
	WY = 30
	POP_WID(1,5) = "PPPOP"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "PARENT ITEMS"
	RETURN
;-----------------------------------------------------------------


END
