SUBROUTINE ARQ		;ARCHIVED QUOTES
	ORDER	,D	;ORDER NUMBER
	USET	,D	;1=ARCHIVE, 0=RESTORE (UN-ARCHIVE)
	WROW	,D	;WINDOW PLACEMENT
	WCOL	,D	;WINDOW PLACEMENT
	CHN182	,D	;COPTBL CHANNEL

	.INCLUDE 'WND:WINDOWS.DEF'

;
RECORD	ARQDAT
	.INCLUDE 'DEF:RD083A.DEF'
;
RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'
;
RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'
;
RECORD	RPOP
	.INCLUDE 'DEF:POP1.DEF'

RECORD	FUNKEY
	.INCLUDE 'DEF:FUNKEY.DEF'

RECORD	W_VARS
	A_WID	,A8,	'ARCQUOTE'
	A_TITLE	,A*,	'ARCHIVED QUOTES'
	W_ID	,D4
	X	,D2
	Y	,D2

RECORD	VARS
	OPNOK	,D1
	AORD	,A6
	SAVRFA	,A6
	F_KEY	,D3
	ABORT	,D1
	CNGCTL	,D1
	WHATNO	,D2
	INXCTL	,D1
	ENTRY	,A30
	X_RC	,A3
	X_DATE	,D8
	X_NOTE	,2A30
	DATIME	,A20
	CHN083	,D3
	CHN044	,D3
	I	,D6
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELET	,D1,3
	SWITCH	,D1
;
PROC
	CALL OPENS

	IF (.NOT. OPNOK) 
		BEGIN
		USET = 9		;ERROR
		GOTO ENDOFF
		END

	CALL INIT_WINDOW

	XCALL ISIO (CHN044, ORDHDR, ORDER, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO ENDOFF
	IF (USET .EQ. 0) GOTO PROCES
	
	AORD = OORDNO, 'ZZZZZX'
DISPLA,
	CLEAR CNGCTL

	XCALL W_DISP (W_ID, WD_CLEAR)
	XCALL W_DISP (W_ID, WD_POS, 1,17, AORD)
	XCALL W_DISP (W_ID, WD_POS, 1,24, OCUSNM)
	XCALL W_DISP (W_ID, WD_POS, 1,4, '   ORDER #')
	XCALL W_DISP (W_ID, WD_POS, 3,4, '1. REASON           <F1=LOOK-UP>')
	XCALL W_DISP (W_ID, WD_POS, 5,4, '2. COMMENTS:')

RC,
	XCALL WINPT (W_ID, 3, 17, 03, 00, 'AE', ENTRY, INXCTL, F_KEY)
	GOTO (DISPLA, ENDOFF), INXCTL
	IF (F_KEY .EQ. F_01)
		BEGIN
		CALL RC_POP
		GOTO (DISPLA), ABORT
		ENTRY(1,3) = QN_COD
		XCALL W_DISP (W_ID, WD_POS, 3,17, ENTRY(1,3) )
		END

	X_RC = ENTRY(1,3)
	CLEAR COPTBL
	TBLCOD = 'QN'
	QN_COD = X_RC
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0)
	THEN	BEGIN
		XCALL W_DISP (W_ID, WD_POS, 3,21, QN_DESC)
		END
	ELSE	BEGIN
		XCALL WNMSG (W_ID, 8, 'CODE NOT IN TABLE', 2)
		GOTO DISPLA
		END

	XCALL W_UPDT
	GOTO (ANYCN), CNGCTL
NOTE,
	XCALL WINPT (W_ID, 5, 18, 30, 00, 'A ', ENTRY, INXCTL)
	GOTO (DISPLA),INXCTL
	X_NOTE(1) = ENTRY(1,30)
	XCALL WINPT (W_ID, 6, 18, 30, 00, 'A ', ENTRY, INXCTL)
	GOTO (DISPLA),INXCTL
	X_NOTE(2) = ENTRY(1,30)
	GOTO ANYCN
ANYCN,
	XCALL WANCN (W_ID, 8, CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR),CNGCTL + 1
CNGBR,
	GOTO (RC, NOTE),WHATNO
	GOTO ANYCN

PROCES,

	XCALL ISIO (CHN044, ORDHDR, ORDER, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO NO_UPDATE

	USING USET SELECT
	(0),	BEGIN	;UNSET
		XCALL ISIO (CHN083, ARQDAT, ORDER, READ, LOKCTL)
		IF (LOKCTL .EQ. 0) XCALL ISIO (CHN083, ARQDAT, ORDER, DELET, LOKCTL)
		END

	(1),	BEGIN	;ARCHIVE
		CLEAR ARQDAT
		DATIME = %DATETIME
		AQ_DATE = DATIME(1,8)	;YYYYMMDD
		AQ_ORD = ORDER
		AQ_SLMN = OSLMAN
		AQ_RC = X_RC
		AQ_NOTE(1) = X_NOTE(1)
		AQ_NOTE(2) = X_NOTE(2)
		LOKCTL = 1
		XCALL ISIO (CHN083, ARQDAT, AQ_ORD, STORE, LOKCTL)		
		IF (LOKCTL.NE.0) GOTO NO_UPDATE
		END
	ENDUSING

	OQARC = USET
	XCALL ISIO (CHN044, ORDHDR, ORDER, WRITE, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO NO_UPDATE

ENDOFF,
	CALL CLOSE
	IF (W_ID)
		BEGIN
		XCALL W_PROC(WP_REMOVE,W_ID)
		XCALL W_PROC(WP_DELETE,W_ID)
		XCALL W_UPDT
		END
	XRETURN

NO_UPDATE,
	USET = 8
	GOTO ENDOFF

;====================================

RC_POP,
	ABORT = 1			;ASSUME NO CODE
	CALL GET_RC
	IF (NUMARA .LE. 0) RETURN	;NOTHING IN TABLE

	DLINE = 
	XCALL POP1 (RPOP)
	IF (PI .LE. 0) RETURN		;ABORT

	USING P_ACTION SELECT
	(1),	BEGIN
		READ (CHN182, COPTBL, RFA:PARRFA(PI) ) [ERR=NO_TBL]
		CLEAR ABORT
		END

	ENDUSING
	RETURN
NO_TBL,
	CLEAR COPTBL
	RETURN
GET_RC,
	CLEAR NUMARA
	CLEAR COPTBL
	TBLCOD = 'QN'
	FIND (CHN182, COPTBL, TBL_KEY) [ERR=GRC_LOOP]
GRC_LOOP,
	READS (CHN182, COPTBL, GRC_EOF, GETRFA:SAVRFA)
	IF (TBLCOD .NE. 'QN') GOTO GRC_EOF
	DLINE (1,3) = QN_COD
	DLINE (5,34) = QN_DESC

	INCR NUMARA
	PARRY(NUMARA) = DLINE
	PARRFA(NUMARA) = SAVRFA

	GOTO GRC_LOOP

GRC_EOF,
	RETURN

;------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK, CHN044, CHN083

	CHN044 = %syn_freechn(20,100)	;between 20 -100
	xcall files (chn044, 'SU', 044, SWITCH)
	IF (SWITCH .EQ. 9) RETURN

	CHN083 = %syn_freechn(chn044, 100)
	xcall files (chn083, 'SU', 083, switch)
	if (switch .eq. 9) return
	
	OPNOK = 1
	RETURN
;------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN044) CLOSE CHN044
	IF (CHN083) CLOSE CHN083
	RETURN
;------------------------------------

INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;
;;; place window...
	X = WROW
	Y = WCOL

	CLEAR W_ID

	XCALL W_PROC(WP_FIND,W_ID,A_WID)
	IF (.NOT. W_ID) XCALL W_PROC(WP_CREATE,W_ID,A_WID,9,60)

	XCALL W_BRDR(W_ID,WB_TITLE,A_TITLE,WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,W_ID,X,Y)
	XCALL W_DISP(W_ID,WD_CLEAR)

;;;	RETURN
;; POP info...

	MAXARA = 10		;9-19-97 NOT MORE THAN 1 FULL WINDOW
	PLEN = 35
	NUMROW = 10
	WX = 8
	WY = 7
	POP_WID(1,5) = "RCPOP"
;;;	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "ARCHIVE REASON CODES"
	RETURN
;-------------------------------------------------------------------

