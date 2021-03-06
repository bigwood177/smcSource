;RINMNT.CP
;
	.include 'wnd:tools.def'
	.include 'wnd:windows.def'

RECORD	ROCINV
	.INCLUDE 'DEF:RD091A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	POP
	.INCLUDE 'DEF:POP1.DEF'		

RECORD	FUNKEY
	.INCLUDE 'DEF:FUNKEY.DEF'

RECORD	HD1
		,A*,	'ITEM #          DESCRIPTION                    MAT'
		,A*,	'   ON-HAND   COMMIT ON-ORDR'

RECORD	CTL
	ROW	,D2
		,A1
	COL	,D2
		,A1
	MAX	,D2
		,A1
	MIN	,D2
		,A1
	TYPE	,A2

RECORD	CHANNELS
	CHN091	,D2
	CHN182	,D2

RECORD	WRKLIN
	ARO	,A1
	REST	,A56

RECORD	PRNT_VARS
	LINCNT	,D3
	PGCNT	,D5
	TITLE	,A*,	'MANUFACTURED INVENTORY PRINT-OUT'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PRTCTL	,D3
	RPTNUM	,D3
	PRTTYP	,D3	
	SPLFIL	,A14
	LPSW	,D1
	LPARG	,D1
	PRNTON	,D1
	PLINE	,A132

RECORD	PVARS
	LI		,D2
	VAL		,D1
	SAVE_ROW	,D2
	FIRST_ROW	,D2
	LAST_ROW	,D2
	WROW		,D2
	FROW		,D2	
	WLEN		,D2
	TITLEN		,D2
	STAT_KEY	,D2
	STAT		,D2
	DEC		,D18

RECORD	WVARS
	FACTOR	,D18
	W_ID	,D4
	WND_1	,D4
	WND_2	,D4
	WND_F1	,D4
	WND_F2	,D4
	WND_F3	,D4
	WN_NAME	,A6,	'RINMNT'
	W1_NAME	,A6
	W2_NAME	,A6

RECORD	VARS
	XDATE	,D8
	window_on	,d1
	BLANKS	,A15
	MATDES	,A4
	ST_ITEM	,A15
	EN_ITEM	,A15
	PGM	,D2
	LN	,D6
	OPNOK	,D1
	LINRFA	,A6
	I	,D6
	MEMO_KEY	,D5
	X_ITEM	,A15
	ITEM_SELECTED	,D1
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	LOKCTL	,D1
	ALPHA	,A9
	ADATE	,A10
	F_KEY	,D3
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	SWITCH	,D1
	V	,D1
;
PROC
	XCALL TERID(V)

	clear window_on

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
MENU,
	if (window_on) xcall w_exit

	XCALL OUTPT (1,1,2,'MANUFACTURED INVENTORY MAINTENANCE',1)
	XCALL OUTPT (3,9,0,'SELECT APPLICATION',1)
	XCALL OUTPT (5,15,0,'1. INVENTORY MAINTENANCE',1)
	XCALL OUTPT (6,15,0,'2. INVENTORY PRINT-OUT',1)

;;;	XCALL W_DISP(WND_1,WD_CLEAR)
;;;	XCALL W_DISP(WND_1,WD_POS,1,1,'MANUFACTURED INVENTORY MAINTENANCE')
;;;	XCALL W_DISP(WND_1,WD_POS,3,9,'SELECT APPLICATION')
;;;	XCALL W_DISP(WND_1,WD_POS,5,15,'ITEM MAINTENANCE')
;;;	XCALL W_DISP(WND_1,WD_POS,6,15,'INVENTORY PRINT-OUT')
	XCALL INPUT (03,29,01,00,'#E',ENTRY,INXCTL,1)
	GOTO (MENU,ENDOFF),INXCTL
	PGM = ENTRY(1,1)
	GOTO (MAINT,PRINT_OUT),PGM
	GOTO MENU

MAINT,
	
	CALL INIT_WINDOW
DISPLA,
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL W_DISP(WND_1,WD_POS,1,1,'MANUFACTURED INVENTORY MAINTENANCE')
	CLEAR CNGCTL
	XCALL W_DISP(WND_1,WD_POS,5,5,'1. ITEM')
;;;	XCALL W_DISP(WND_1,WD_POS,5,32,'<F1>=Search')
	XCALL W_DISP(WND_1,WD_POS,7,5,'2. QTY ON HAND')
	XCALL W_DISP(WND_1,WD_POS,9,5,'3. QTY COMMITTED')
	XCALL W_DISP(WND_1,WD_POS,11,5,'4. QTY ON ORDER')
	XCALL W_DISP(WND_1,WD_POS,13,5,'5. REORDER QTY')
	XCALL W_DISP(WND_1,WD_POS,15,5,'   LAST ORDER:')
R_ITEM,
	CTL = '05,15,15,00,AE'
	CALL INPUT
	GOTO (DISPLA,RE_MENU),INXCTL
	X_ITEM = ENTRY(1,15)
	CALL POP_ITEMS
	IF (.NOT.ITEM_SELECTED) GOTO DISPLA
	LINRFA = PARRFA(LI)
	READ (CHN091, ROCINV, RFA=LINRFA)
	CALL DSPITM
	XCALL W_PROC(WP_REMOVE, WND_F3)
	XCALL W_PROC(WP_REMOVE, WND_F2)
	GOTO ANYCNG

R_QTYH,
	CTL = '07,25,06,00,# '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	R_QTYH = ENTRY(1,6)
	GOTO ANYCNG
R_QTYC,
	CTL = '09,25,06,00,# '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	R_QTYC = ENTRY(1,6)
	GOTO ANYCNG
R_QTYO,
	CTL = '11,25,06,00,# '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	R_QTYO = ENTRY(1,6)
	GOTO ANYCNG
R_REO,
	CTL = '13,25,06,00,# '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	R_REO = ENTRY(1,6)
	GOTO ANYCNG
ANYCNG,
;;;	XCALL ANYCN(CNGCTL,WHATNO)
	XCALL WANCN(W_ID,23,CNGCTL,WHATNO)
	GOTO(PROCES,CNGBR),CNGCTL+1
CNGBR,
	GOTO (R_ITEM,R_QTYH,R_QTYC,R_QTYO,R_REO),WHATNO
	GOTO ANYCNG
PROCES,
	WRITE (CHN091, ROCINV, LINRFA)
	GOTO DISPLA
	
PRINT_OUT,
	PRNTON = 0
	LINCNT = 66

	XCALL OUTPT (2,1,2,'PRINT-OUT',1)
	XCALL OUTPT (5,4,0,'1. STARTING ITEM #',1)
	XCALL OUTPT (6,4,0,'2. ENDING   ITEM #',1)
ST_ITEM,
	XCALL INPUT (5,25,15,00,'AE',ENTRY,INXCTL,1)
	GOTO (PRINT_OUT,P_EOF),INXCTL
	ST_ITEM = ENTRY(1,15)
	IF (ST_ITEM .EQ. BLANKS)
		BEGIN
		EN_ITEM = ']]]]]]'
		XCALL OUTPT (5,25,1,'ALL',1)
		GOTO P_ANY
		END
EN_ITEM,
	XCALL INPUT (6,25,15,00,'AE',ENTRY,INXCTL,1)
	GOTO (PRINT_OUT,P_EOF),INXCTL
	EN_ITEM = ENTRY(1,15)
	IF (EN_ITEM .EQ. BLANKS)
		BEGIN
		EN_ITEM = ST_ITEM
		XCALL OUTPT (6,25,0,EN_ITEM,1)
		END
P_ANY,
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (P_PROC, P_CNGBR), CNGCTL+1
P_CNGBR,
	GOTO (ST_ITEM,EN_ITEM),WHATNO
	GOTO P_ANY
P_PROC,
	FIND (CHN091,ROCINV,ST_ITEM) [ERR=P_LOOP]
P_LOOP,
	XCALL IOS (CHN091,ROCINV,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO P_EOF
	IF (R_ITEM .GT. EN_ITEM) GOTO P_EOF
	CALL GET_MEMO

	PLINE(1,15) = R_ITEM
	PLINE(17,46) = R_DESC
	PLINE(48,53) = MATDES
	PLINE(55,61) = R_QTYH,	'ZZZ,ZZX-'
	PLINE(63,69) = R_QTYC,	'ZZZ,ZZX'
	PLINE(71,77) = R_QTYO,	'ZZZ,ZZX'
	PLINE(79,85) = R_REO,	'ZZZ,ZZX'
	PLINE(88,93) = R_ORDN,	'ZZZZZX'
	CALL PRINT
	CALL PRINT
	GOTO P_LOOP

GET_MEMO,
	USING R_MAT SELECT
	(1),	MATDES = 'GALV'
	(2),	MATDES = 'ALUM'
	(3),	MATDES = 'PGRP'
	(4),	MATDES = 'SST'
	(),	MATDES = '????'
	ENDUSING

	IF (R_F1KEY) 
		BEGIN
		CALL GET_F1_MEMO
		PLINE(1,30) = MM_LONG
		END

	IF (R_F2KEY)
		BEGIN
		CALL GET_F2_MEMO
		PLINE(%TRIM(PLINE)+2, %TRIM(PLINE)+32) = MM_LONG
		END

	IF (R_F3KEY)
		BEGIN
		CALL GET_F3_MEMO
		PLINE(%TRIM(PLINE)+2, %TRIM(PLINE)+32) = MM_LONG
		END

	IF (%TRIM(PLINE) ) CALL PRINT
	
	RETURN

P_EOF,
	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
	GOTO MENU

PRINT,	
	IF (PRNTON .EQ. 0) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD,HD
&		,LG,LG,LG,0,0,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;-------------------------------------------------------------
PRNTON,
	SPLFIL (5,6) = 'EF'
	LPSW = 1		;PRINT,SPOOL, OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	PRNTON = 1
	RETURN
;-------------------------------------------------------------
;
;AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
;ITEM #          DESCRIPTION                    MAT    ON-HAND  COMMIT ON-ORDR
;AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA AAAAAA ZZZ,ZZX ZZZ,ZZX ZZZ,ZZX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890��
;         1         2         3         4         5         6         7

RE_MENU,
	IF (WND_1)
		BEGIN
		XCALL W_PROC(WP_REMOVE,WND_1)
		XCALL W_PROC(WP_DELETE,WND_1)
		END

	IF (WND_2)
		BEGIN
		XCALL W_PROC(WP_REMOVE,WND_2)
		XCALL W_PROC(WP_DELETE,WND_2)
		END

	IF (WND_F2)
		BEGIN
		XCALL W_PROC(WP_REMOVE,WND_F2)
		XCALL W_PROC(WP_DELETE,WND_F2)
		END

	IF (WND_F3)
		BEGIN
		XCALL W_PROC(WP_REMOVE,WND_F3)
		XCALL W_PROC(WP_DELETE,WND_F3)
		END

	XCALL W_EXIT

	GOTO MENU
;=====================================================

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:ROCMNU',1)
	XCALL FLAGS(7000000)
	STOP
;====================================================
;====================================================

POP_ITEMS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; READ NUMARA ROCINV RECORDS INTO ARRAY
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL W_PROC(WP_PLACE, WND_F2, 22, 4)
	XCALL W_DISP(WND_F2, WD_CLEAR)

	XCALL W_PROC(WP_PLACE, WND_F3, 22, 40)
	XCALL W_DISP(WND_F3, WD_CLEAR)

	CALL GET_ITEMS
;-
	WLEN = 55
	SAVE_ROW = 0
	LI = 1
RE_PAGE,
	IF (NUMARA .EQ. 0) GOTO F5_RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	IF (NUMARA .EQ. 1)	;ONLY 1 ITEM
;;;		BEGIN
;;;		PI = LI
;;;		ITEM_SELECTED = 1
;;;		GOTO F5_RETURN
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	NUMROW = 10
	MAXARA = NUMROW

	LAST_ROW = NUMROW+1
	IF (NUMROW .GT. NUMARA) LAST_ROW = NUMARA+1
	INCR LAST_ROW

	IF (WND_2)
		BEGIN
		XCALL W_PROC(WP_REMOVE,WND_2)
		XCALL W_PROC(WP_DELETE,WND_2)
		END
	XCALL W_PROC(WP_CREATE, WND_2, 'SCROL',LAST_ROW, 57)
	XCALL W_BRDR(WND_2,WB_TITLE,'Inventory Data',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE, WND_2, 08, 0)
	XCALL W_DISP(WND_2, WD_CLEAR)
	DLINE = '        ITEM#           DESCRIPTION                     OH-HAND'
	XCALL W_DISP(WND_2,WD_POS,1,1,DLINE(1,WLEN))
	XCALL W_DISP(WND_2,WD_HLINE,2,1,WLEN+2)
	XCALL W_UPDT	

	CLEAR ARO
	WROW = 2
	FOR I FROM 1 THRU LAST_ROW-2
		BEGIN
		INCR WROW
		REST = PARRY(I)
		XCALL W_DISP(WND_2, WD_POS, WROW, 1, WRKLIN)
		END
	XCALL W_UPDT

	CLEAR VAL
	WROW = SAVE_ROW	
	CLEAR SAVE_ROW	
	IF (WROW .LE. 0) WROW = 3	;SSQ 12-16-99

	FIRST_ROW = 3

	CALL DSPLIN

	XCALL TTSTS(STAT)
	IF (STAT) XCALL W_DISP(WND_2,WD_ACCEPT,F_KEY)

DSPLOP,
	IF (SAVE_ROW .NE. 0)	CALL CLEAR_LAST_LINE

	WROW = WROW + VAL
	IF (WROW .GT. LAST_ROW) CALL SCROLL_DN
	IF (WROW .LT. FIRST_ROW) CALL SCROLL_UP

	LI = LI + VAL
	IF (LI .LE. 0) LI = 1
	IF (LI .GT. NUMARA) LI = NUMARA

	CALL DSPLIN
	CALL PINPUT
	USING F_KEY SELECT
	(EXIT_KEY, CR_KEY),	BEGIN
			CLEAR PI
			CLEAR P_ACTION
			END
	(UP_ARO),	BEGIN
			VAL = - 1
			GOTO DSPLOP
			END
	(DN_ARO),	BEGIN
			VAL =  1
			GOTO DSPLOP
			END
	(PAGE_UP),	BEGIN
			CALL OL_PAGE_UP
			IF (NUMARA .GT. 0) GOTO RE_PAGE
			END
	(PAGE_DN),	BEGIN
			CALL OL_PAGE_DN
			IF (NUMARA .GT. 0) GOTO RE_PAGE
			END
	(RGHT_ARO),	BEGIN
			PI = LI
			ITEM_SELECTED = 1
			END
	(DEL_KEY),	BEGIN
			GOTO DSPLOP
			END
	ENDUSING
	
DONE_F5,
	IF (WND_2)
		BEGIN
		XCALL W_PROC(WP_REMOVE,WND_2)
		XCALL W_PROC(WP_DELETE,WND_2)
		CLEAR WND_2
		END
	XCALL W_UPDT

F5_RETURN,
	RETURN


DSPLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; move arrow, highlight selected line
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ARO = '>'
	REST = PARRY(LI)
	XCALL W_DISP (WND_2,WD_ATTR,ATTR_SET+ATTR_RVRS)
	XCALL W_DISP (WND_2,WD_POS,WROW,1,WRKLIN)
	XCALL W_DISP (WND_2,WD_POS,WROW,1)
	SAVE_ROW = WROW
;display any memos for current item...
	READ (CHN091, ROCINV, RFA=PARRFA(LI))

	CALL DISPLAY_MEMO_WINDOWS
	RETURN
;----------------------------------------------------------------------
DISPLAY_MEMO_WINDOWS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;look up in coptbl

	IF (R_F1KEY .GT. 0)
		BEGIN
		CALL GET_F1_MEMO
		END

	IF (R_F2KEY .GT. 0)
	THEN	CALL GET_F2_MEMO
	ELSE	CLEAR MM_LONG
	XCALL W_DISP(WND_F2,WD_POS,1,1,MM_LONG)
	XCALL W_AREA(WND_F2,WA_COLOR,5)

	IF (R_F3KEY .GT. 0)
	THEN	CALL GET_F3_MEMO
	ELSE	CLEAR MM_LONG
	XCALL W_DISP(WND_F3,WD_POS,1,1,MM_LONG)
	XCALL W_AREA(WND_F3,WA_COLOR,6)

	XCALL W_UPDT

	RETURN
;----------------------------------------------------------------------

CLEAR_LAST_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; CLEAR HI-LITE ON PREV LINE
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR ARO
	XCALL W_DISP (WND_2,WD_ATTR,ATTR_CLR+ATTR_RVRS)
	XCALL W_DISP (WND_2,WD_POS,SAVE_ROW,1,WRKLIN)
	RETURN
;----------------------------------------------------------------------
OL_PAGE_DN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PAGE FORWARD NUMARA RECORDS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR SAVE_ROW
;;;	WROW = 1
	WROW = 3
	LI = 1
	FIND (CHN091, ROCINV, RFA=PARRFA(NUMARA)) [ERR=GI_INIT]
	GOTO GI_INIT

OL_PAGE_UP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PAGE BACKWARDS NUMARA RECORDS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR SAVE_ROW
;;;	WROW = 1
	WROW = 3
	LI = 1
; read reverse NUMARA records...
	FIND (CHN091, ROCINV, RFA=PARRFA(1)) [ERR=GI_INIT]
	FOR I FROM 1 THRU MAXARA+1
		BEGIN
		READS (CHN091, ROCINV, GI_INIT, REVERSE) [ERR=GI_INIT]
		IF (R_ITEM(1,LN) .NE. X_ITEM(1,LN) ) GOTO GI_INIT
	;;;	IF (R_ITEM .NE. X_ITEM) GOTO GI_INIT
		END
	GOTO GI_INIT
;------------------------------------------------------------------
PINPUT,	;;;;;;;;;;;;;;;;;;;;;;;
	;;; FUNCTION KEY INPUT
	;;;;;;;;;;;;;;;;;;;;;;;

	XCALL W_DISP(WND_2,WD_ACCEPT,STAT_KEY)
	XCALL TTSTS(STAT)
	IF (STAT) 
	THEN	XCALL W_DISP(WND_2,WD_ACCEPT,F_KEY)
	ELSE	F_KEY = STAT_KEY

	CASE F_KEY OF
	BEGINCASE
	079:	F_KEY = 027
	133:	F_KEY = 027
	008:	F_KEY = 027		;<BS>
	010:	F_KEY = 013		;<CR>
	ENDCASE

	RETURN

GETCHR,	
	XCALL W_DISP(WND_2,WD_ACCEPT,ALPHA)
	XCALL DECML (ALPHA, DEC)
	RETURN
;----------------------------------------------------------------------
SCROLL_DN,
	WROW = LAST_ROW
	IF (LI .EQ. NUMARA) RETURN
	XCALL W_AREA(WND_2,WA_SCROLL,WAS_UP,1)
	RETURN

SCROLL_UP,
	WROW = FIRST_ROW
	IF (LI .EQ. 1) RETURN
	XCALL W_AREA(WND_2,WA_SCROLL,WAS_DOWN,1)
	RETURN
;-

GET_ITEMS,
	CLEAR I, ITEM_SELECTED
	LN = %TRIM(X_ITEM)

	R_KEY = X_ITEM
	FIND (CHN091, ROCINV, R_KEY) [ERR=GI_INIT, EOF=RI_EOF]

GI_INIT,
	FOR I FROM 1 THRU MAXARA CLEAR PARRY(I), PARRFA(I), PFLAGS(I)
	CLEAR I

READ_ITEMS,
	XCALL IOS (CHN091, ROCINV, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO RI_EOF
	IF (R_ITEM(1,LN) .NE. X_ITEM(1,LN) ) GOTO RI_EOF
	INCR I
	XCALL GETRFA(CHN091, LINRFA)
	PARRFA(I) = LINRFA
	DLINE(1,15) = R_ITEM
	DLINE(17,46) = R_DESC
	DLINE(48,56) = R_QTYH,	'ZZZZ,ZZX-'
;;;	DLINE(57,64) = R_QTYC,	'ZZZZ,ZZX'

;       ITEM#           DESCRIPTION                     OH-HAND
;       AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA ZZZZ,ZZX ZZZZ,ZZX 
;	1234567890123456789012345678901234567890123456789012345678901234567890
;                1         2         3         4         5         6          7
	PARRY(I) = DLINE

	IF (I .LT. MAXARA) GOTO READ_ITEMS	
RI_EOF,
	UNLOCK CHN091
	NUMARA = I
	RETURN
;--------------------------------------------

DSPITM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CNGCTL = 1
	XCALL W_DISP(W_ID,WD_POS,5,15,R_ITEM)
	IF (%INSTR(1,R_ITEM,"*") )XCALL W_DISP(W_ID,WD_POS,5,26,'Damper')
	XCALL W_DISP(W_ID,WD_POS,5,32,' ')
	XCALL W_DISP(W_ID,WD_POS,5,33,R_DESC)
	xcall w_updt

	ALPHA = R_QTYH,	'ZZZZ,ZZX-' [left]
	XCALL W_DISP(W_ID,WD_POS,7,25,ALPHA)

	ALPHA = R_QTYC,	'ZZZZ,ZZX' [left]
	XCALL W_DISP(W_ID,WD_POS,9,25,ALPHA)

	ALPHA = R_QTYO,	'ZZZZ,ZZX' [left]
	XCALL W_DISP(W_ID,WD_POS,11,25,ALPHA)

	ALPHA = R_REO,	'ZZZZ,ZZX' [left]
	XCALL W_DISP(W_ID,WD_POS,13,25,ALPHA)

	ALPHA = R_ORDN,	'ZZZZZX'
	XCALL W_DISP(W_ID,WD_POS,15,25,ALPHA)

	XDATE(1,4) = R_DATE(5,8)
	XDATE(5,8) = R_DATE(1,4)
	ADATE = XDATE,	'ZX/XX/XXXX'
	XCALL W_DISP(W_ID,WD_POS,15,33,ADATE)

	IF (R_F1KEY .GT. 0)
		BEGIN
		CALL GET_F1_MEMO
		XCALL W_DISP(W_ID,WD_POS,3,15,MM_LONG)
		END
	IF (R_F2KEY .GT. 0)
		BEGIN
		CLEAR COPTBL
		TBLCOD = 'MM'
		MM_KEY = R_F2KEY
		XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
		IF (LOKCTL.NE.0) MM_LONG = '*** NOT IN TABLE ***'
		XCALL W_DISP(W_ID,WD_POS,4,1,MM_LONG)
		END
	IF (R_F3KEY .GT. 0)
		BEGIN
		CLEAR COPTBL
		TBLCOD = 'MM'
		MM_KEY = R_F3KEY
		XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
		IF (LOKCTL.NE.0) MM_LONG = '*** NOT IN TABLE ***'
		XCALL W_DISP(W_ID,WD_POS,4,33,MM_LONG)
		END

	XCALL W_UPDT
	RETURN
;--------------------------------------------

GET_F1_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MEMO_KEY = R_F1KEY
	CALL READ_MEMO_TABLE
	RETURN
;-------------------------------------------------------------------
GET_F2_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MEMO_KEY = R_F2KEY
	CALL READ_MEMO_TABLE
	RETURN
;-------------------------------------------------------------------
GET_F3_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MEMO_KEY = R_F3KEY
	CALL READ_MEMO_TABLE
	RETURN
;-------------------------------------------------------------------

READ_MEMO_TABLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR COPTBL
	TBLCOD = 'MM'
	MM_KEY = MEMO_KEY
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL.NE.0) MM_LONG = '*** NOT IN TABLE ***'
	RETURN
;-------------------------------------------------------------------

INPUT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL WINPT(W_ID,ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,F_KEY)
	RETURN
;---------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
;;;	XCALL FILES (1,'SU',091,SWITCH)
	open (1, su, 'roc:rocinv.rom')

	IF (SWITCH .EQ. 9) RETURN
	CHN091 = 1

	SWITCH = 5
	XCALL FILES (2,'SI',182,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 2

	OPNOK = 1
	RETURN
;---------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN091) CLOSE CHN091
	IF (CHN182) CLOSE CHN182
	RETURN
;---------------------------------------------
INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall w_init(0,15,10)

	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'Manufactured Inventory',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1
;-
;Pallets...
	XCALL W_PROC(WP_PALET, 4, 1, 0)		;1=BLUE
	XCALL W_PROC(WP_PALET, 5, 2, 0)		;2=GREEN
	XCALL W_PROC(WP_PALET, 6, 3, 0)		;3=CYAN
	XCALL W_PROC(WP_PALET, 7, 4, 7)		;4=RED
	XCALL W_PROC(WP_PALET, 8, 7, 0)		;7=WHITE
	XCALL W_PROC(WP_PALET, 9, 0, 7)		;4=BLACK ON WHITE
	XCALL W_PROC(WP_PALET,10, 6, 0)		;6=YELLOW
;-

	XCALL W_PROC(WP_CREATE, WND_F2, 'F2NOT',1, 30)
	XCALL W_BRDR(WND_F2,WB_TITLE,'Inventory Data',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE, WND_F2, 22, 4)
	XCALL W_DISP(WND_F2, WD_CLEAR)

	XCALL W_PROC(WP_CREATE, WND_F3, 'F3NOT',1, 30)
	XCALL W_BRDR(WND_F3,WB_TITLE,'Inventory Data',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE, WND_F3, 22, 40)
	XCALL W_DISP(WND_F3, WD_CLEAR)

;Pop info...
	NUMARA = 10
	PLEN = 60
	NUMROW = 10
	MAXARA = NUMROW
	WX = 10
	WY = 5
	POP_WID(1,5) = "items"
	POP_TITLE = "Inventory"

	window_on = 1
	RETURN
	END
