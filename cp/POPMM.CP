;will need to rename source from popmm -> popm
;POPM.DBL	double window
;		AUTHOR: SHERWOOD S. QUIRING
;		POPUP WINDOW ROUTINE FOR MEMOS
;
;		if pi is neg, display pop-up window and exit.

SUBROUTINE	POPM
	POPAR	,A		;POP UP WINDOW DATA
	NO2	,D		;1=no double window (optional)	
	.INCLUDE 'DEF:WINDOWS.DEF'

RECORD	POPARA
	.INCLUDE 'MPOP.DEF'

RECORD	FUNKEY
	.INCLUDE 'DEF:FUNKEY.DEF'

RECORD	WRKLIN
	ARO	,A1
		,A1
	REST	,A80

record	wvars
	w2	,d4
	w2_id	,a8

RECORD	VARS
	NO_DUB	,D1
	A30	,A30
	A80	,A80
	SELECT	,D3
	STAT	,D1
	STAT_KEY	,D3
	DEC	,D6
	ALPHA	,A3
	WHDR	,A80
	F_KEY	,D3
	I	,D5
	J	,D5
	VAL	,D1
	WROW	,D2
	WCOL	,D2
	FIRST_ROW	,D2
	LAST_ROW	,D2
	SAVE_ROW	,D2
	X	,D2
	Y	,D2
	HT	,D2
	LN	,D2
	MAXROW	,D2
	WLEN	,D2
	TITLEN	,D2
PROC
	IF (%PASSED(NO2) )		;SSQ 4-26-07
	THEN	NO_DUB = NO2
	ELSE	CLEAR NO_DUB

	POPARA = POPAR		;POP UP DATA

	CALL DSPWND		;DISPLAY WINDOW

	CLEAR VAL
	CLEAR SAVE_ROW
	CLEAR P_ACTION
	FIRST_ROW = 1
;;;	WROW = 1
;;;	I = 1
;--------------------------------------------
	WROW = 0
	I = 0
	USING PI SELECT
	(.LT.0),	SELECT = -PI
	(0),		SELECT = 1
	(.GT.0),	SELECT = PI
	ENDUSING
	FOR J FROM 1 THRU SELECT
		BEGIN
		IF (SAVE_ROW .NE. 0)	CALL CLEAR_LAST_LINE
		INCR WROW
		INCR I
		IF (WROW .GT. LAST_ROW) CALL SCROLL_DN
		CALL DSPLIN
		END
	I = SELECT
;--------------------------------------------
;;;	CALL DSPLIN
	IF (PI .LT. 0) RETURN

DSPLOP,
	IF (SAVE_ROW .NE. 0)	CALL CLEAR_LAST_LINE

	WROW = WROW + VAL
	IF (WROW .GT. LAST_ROW) CALL SCROLL_DN
	IF (WROW .LT. FIRST_ROW) CALL SCROLL_UP

	I = I + VAL
	IF (I .LE. 0) I = 1
	IF (I .GT. NUMARA) I = NUMARA

	CALL DSPLIN

	CALL INPUT
	USING F_KEY SELECT
	(EXIT_KEY),	BEGIN
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
	(RGHT_ARO),	BEGIN
			P_ACTION = 1
			PI = I		;SAVE SELECTED ARRAY EL.
			END
	(LEFT_ARO),	BEGIN
			P_ACTION = 1
			PI = I		;SAVE SELECTED ARRAY EL.
			END
	(CR_KEY),	BEGIN
			P_ACTION = 1	;next page
			PI = I		;SAVE SELECTED ARRAY EL.
			END
	(INS_KEY),	BEGIN
			P_ACTION = 2	;INSERT NEW
			CLEAR PI
			END
	(F_01, F_02, F_03),		BEGIN
			CLEAR PI
			XCALL W_PROC(WP_REMOVE,POP_WND)
			XCALL W_PROC(WP_DELETE,POP_WND)
			xcall w_proc(wp_remove, w2)
			xcall w_proc(wp_delete, w2)
			CLEAR P_ACTION
			END
	ENDUSING
	
	POPAR = POPARA		;POP UP DATA

	XCALL W_UPDT
	RETURN

DSPLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; move arrow, highlight selected line
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	XCALL W_DISP(w2,WD_CLEAR)
	XCALL W_UPDT

	ARO = '>'
	A80 = PARRY(I)
	REST = A80(1,30)
;;;	REST = PARRY(I)
	XCALL W_DISP (POP_WND,WD_ATTR,ATTR_CLR+ATTR_RVRS)
	XCALL W_DISP (POP_WND,WD_POS,WROW,1,WRKLIN(1,WLEN))
	XCALL W_DISP (POP_WND,WD_POS,WROW,1)
	SAVE_ROW = WROW
	XCALL W_AREA (POP_WND, WA_COLOR, PALET)
	XCALL W_UPDT

	IF (NO_DUB) RETURN

	A30 = A80(1,30)
	XCALL W_DISP (W2, WD_POS, 1,1, A30)
	A30 = A80(31,60)
	XCALL W_DISP (W2, WD_POS, 2,1, A30)
	A30 = A80(61,80)
	XCALL W_DISP (W2, WD_POS, 3,1, A30)
	XCALL W_UPDT	
	RETURN
;----------------------------------------------------------------------

CLEAR_LAST_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; CLEAR HI-LITE ON PREV LINE
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR ARO
	IF (I.GT.0)
		BEGIN
		IF (PFLAGS(I).EQ.1)
		THEN	WRKLIN(WLEN,WLEN) = '*'
		ELSE	CLEAR WRKLIN(WLEN,WLEN)
;;; 3-1-07		PARRY(I) = REST(1,WLEN)
		END
	XCALL W_DISP (POP_WND,WD_ATTR,ATTR_SET+ATTR_RVRS)
;;;	XCALL W_DISP (POP_WND,WD_ATTR,ATTR_CLR+ATTR_RVRS)
	XCALL W_DISP (POP_WND,WD_POS,SAVE_ROW,1,WRKLIN(1,WLEN))
	RETURN
;----------------------------------------------------------------------
INPUT,	;;;;;;;;;;;;;;;;;;;;;;;
	;;; FUNCTION KEY INPUT
	;;;;;;;;;;;;;;;;;;;;;;;

	XCALL W_DISP(POP_WND,WD_ACCEPT,STAT_KEY)
	XCALL TTSTS(STAT)
	IF (STAT) 
	THEN	XCALL W_DISP(POP_WND,WD_ACCEPT,F_KEY)
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
	XCALL W_DISP(POP_WND,WD_ACCEPT,ALPHA)
	XCALL DECML (ALPHA, DEC)
	RETURN

;----------------------------------------------------------------------
;----------------------------------------------------------------------

;;; WINDOW SCROLLING ROUTINES
SCROLL_DN,
	WROW = LAST_ROW
	IF (I .EQ. NUMARA) RETURN
	XCALL W_AREA(POP_WND,WA_SCROLL,WAS_UP,1)
	RETURN

SCROLL_UP,
	WROW = FIRST_ROW
	IF (I .EQ. 1) RETURN
	XCALL W_AREA(POP_WND,WA_SCROLL,WAS_DOWN,1)
	RETURN
;--------------------------

DSPWND,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; DISPLAY WINDOW, FILL W/ DATA
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; determine height & length of window, x & y coordinates
;;; to center window on screen.
	LAST_ROW = NUMROW 
	IF (NUMROW .GT. NUMARA) LAST_ROW = NUMARA 

	HT = LAST_ROW		;HEIGHT OF WINDOW, LEAVE ROOM FOR HEADER
	LN = PLEN + 2		;LENGHT OF WINDOW
	
	IF (WX .LE. 0)
	THEN	X = (24-HT)/2		;X CO-ORDINATE
	ELSE	X = WX
	IF (WY .LE. 0)
	THEN	Y = (80-LN)/2		;Y CO-ORDINATE
	ELSE 	Y = WY

;;; place window...
	XCALL W_PROC(WP_FIND,POP_WND,POP_WID)
;;;	IF (.NOT. POP_WND) XCALL W_PROC(WP_CREATE,POP_WND,POP_WID,HT,LN)
	IF (POP_WND) 
		BEGIN
		XCALL W_PROC(WP_REMOVE,POP_WND)
		XCALL W_PROC(WP_DELETE,POP_WND)
		END
	XCALL W_PROC(WP_CREATE,POP_WND,POP_WID,HT,LN)

	TITLEN = %TRIM(POP_TITLE)
	IF (TITLEN .GT.0)
&	 XCALL W_BRDR(POP_WND,WB_TITLE,POP_TITLE(1,TITLEN),WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,POP_WND,X,Y)
	XCALL W_DISP(POP_WND,WD_CLEAR)
;;;	XCALL W_DISP (POP_WND,WD_ATTR,ATTR_CLR+ATTR_RVRS)
	XCALL W_DISP (POP_WND,WD_ATTR,ATTR_SET+ATTR_RVRS)
	XCALL W_UPDT
;;; fill window w/ data...
	CLEAR ARO
	WLEN = LN
	WROW = 0
	FOR I FROM 1 THRU LAST_ROW
		BEGIN
		INCR WROW
		DLINE = PARRY(I)
	;;;	REST = DLINE(1,PLEN)		;SSQ 3-1-07
		REST = DLINE(1,30)		;SSQ 3-1-07
		XCALL W_DISP(POP_WND,WD_POS,WROW,1,WRKLIN(1,WLEN) )
		END
	XCALL W_AREA (POP_WND, WA_COLOR, PALET)
	XCALL W_UPDT

;2nd window....
	W2_ID = 'POPM-2'
	XCALL W_PROC(WP_FIND,W2,W2_ID)
	IF (W2) 
		BEGIN
		XCALL W_PROC(WP_REMOVE,W2)
		XCALL W_PROC(WP_DELETE,W2)
		END
	XCALL W_PROC(WP_CREATE,W2,W2_ID,3,30)
;;;	XCALL W_PROC(WP_CREATE,W2,W2_ID,HT,LN)

	XCALL W_PROC(WP_PLACE,W2,X+3,Y+22)
	XCALL W_DISP(W2,WD_CLEAR)
;;;	XCALL W_DISP (W2,WD_ATTR,ATTR_SET+ATTR_RVRS)
	XCALL W_UPDT

	RETURN
;---------------------------------------------------------------------


