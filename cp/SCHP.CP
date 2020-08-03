;SCHP.CP
;POP1.MAN
;		AUTHOR: SHERWOOD S. QUIRING
;		POPUP WINDOW ROUTINE

;		11-dec-02 ssq: 	added palet option

SUBROUTINE	SCHP
	POPAR	,A		;POP UP WINDOW DATA

	.INCLUDE 'DEF:WINDOWS.DEF'

RECORD	POPARA
	.INCLUDE 'DEF:POPSCH.DEF'

RECORD	FUNKEY
	.INCLUDE 'DEF:FUNKEY.DEF'

record	
	red_key	,d3,	701	;R
	bold_key,d3,	702	;B

RECORD	WRKLIN
	ARO	,A1
		,A1
	WSEQ	,A3
		,A1
	REST	,A80

RECORD	VARS
	fara	,d6		;index of first non-9 element
	lara	,d6		;index of last non-9 element
	num9	,d5		;number of completed orders in array
	paletB	,d2,	15	;default = Black on white
	A3	,A3
	NEWSEQ	,D3
	STAT	,D1
	STAT_KEY	,D3
	DEC	,D6
	ALPHA	,A3
	WHDR	,A80
	F_KEY	,D3
	I	,D5
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
	POPARA = POPAR		;POP UP DATA


	CALL DSPWND		;DISPLAY WINDOW

	CLEAR VAL
	CLEAR SAVE_ROW
	FIRST_ROW = 3
	WROW = 3
	I = 1
	i = fara
	CALL DSPLIN

DSPLOP,
	IF (SAVE_ROW .NE. 0)	CALL CLEAR_LAST_LINE

	WROW = WROW + VAL
	IF (WROW .GT. LAST_ROW) CALL SCROLL_DN
	IF (WROW .LT. FIRST_ROW) CALL SCROLL_UP
skip9,
	I = I + VAL
	IF (I .LT. fara) I = fara		;1st non-9
	IF (I .GT. lara) I = lara		;last non-9
	if (parcpl(i) .eq. 9) goto skip9

;;;	IF (I .LE. 0) I = 1
;;;	IF (I .GT. NUMARA) I = NUMARA


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
			PFLAGS(I) = 1
			CALL GET_SEQ
			END
	(LEFT_ARO),	BEGIN
			P_ACTION = 1
			PI = I		;SAVE SELECTED ARRAY EL.
			PFLAGS(I) = 0
			CALL GET_SEQ
			END
	(CR_KEY),	BEGIN
			P_ACTION = 4	;next page
			PI = I		;SAVE SELECTED ARRAY EL.
			END
	(PAGE_DN),	BEGIN
			P_ACTION = 6	;next page
			PI = I		;SAVE SELECTED ARRAY EL.
			END
	(INS_KEY),	BEGIN
			P_ACTION = 2	;ADD NEW
			PI = I
			END
	(DEL_KEY),	BEGIN
			P_ACTION = 3	;DELETE
			PI = I
			END
	(HOME_KEY),	BEGIN
			P_ACTION = 5	;PRINT
			END
	(RED_KEY),	BEGIN		;R,r ;RED: HOT
			IF (PARCLR(I)=='R')
			THEN	PARCLR(I)=	;RED: HOT
			ELSE	PARCLR(I)='R'	
			GOTO DSPLOP
			END
	(BOLD_KEY),	BEGIN		;B,b
			IF (PARBLD(I) .EQ. 1)
			THEN PARBLD(I) =
			ELSE PARBLD(I) = 1	;BOLD:running late
			GOTO DSPLOP
			END
	ENDUSING
	
	POPAR = POPARA		;POP UP DATA

	XCALL W_PROC(WP_REMOVE,POP_WND)
	XCALL W_PROC(WP_DELETE,POP_WND)
	XCALL W_UPDT
	RETURN
;-----------------------------------------------

GET_SEQ,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; change seq #
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL W_DISP (POP_WND, WD_POS, WROW, 2, WD_READS, A3)
	XCALL ALPDC (A3, NEWSEQ, STAT)
	GOTO (GET_SEQ),STAT
;;;	IF (NEWSEQ .GT. I) GOTO GET_SEQ
;;;	IF (NEWSEQ .GT. NUMARA) GOTO GET_SEQ
	IF (NEWSEQ .GT. NUMARA) NEWSEQ = NUMARA	;23-18-20

	NEW_SEQ = NEWSEQ
	OLD_SEQ = I

	RETURN
;-----------------------------------------------


DSPLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; move arrow, highlight selected line
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	ARO = '>'
	REST = PARRY(I)
	
	XCALL W_DISP (POP_WND,WD_ATTR,ATTR_SET+ATTR_RVRS)

	XCALL W_DISP (POP_WND,WD_POS,WROW,1,WRKLIN(1,WLEN))
	XCALL W_DISP (POP_WND,WD_POS,WROW,1)
	SAVE_ROW = WROW
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
		PARRY(I) = REST(1,WLEN)
		END
	XCALL W_DISP (POP_WND,WD_ATTR,ATTR_CLR+ATTR_RVRS)
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
;;;	ELSE	F_KEY = STAT_KEY
	ELSE	BEGIN
		USING STAT_KEY SELECT
		(82,114), STAT_KEY = RED_KEY
		(66,98),  STAT_KEY = BOLD_KEY
		ENDUSING
		F_KEY = STAT_KEY
		END

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
	if (i .eq. lara) return		;5-31-19
	IF (I .EQ. NUMARA) RETURN
	XCALL W_AREA(POP_WND,WA_SCROLL,WAS_UP,1)
	XCALL W_DISP(POP_WND,WD_POS,1,1,WD_CLR,WDC_EOL )
	XCALL W_DISP(POP_WND,WD_POS,1,1,WHDR(1,PLEN) )
	XCALL W_DISP(POP_WND,WD_HLINE,2,1,PLEN+2)
	RETURN

SCROLL_UP,
	WROW = FIRST_ROW
	if (i .eq. fara) return		;5-31-19
	IF (I .EQ. 1) RETURN
	XCALL W_AREA(POP_WND,WA_SCROLL,WAS_DOWN,1)
	XCALL W_DISP(POP_WND,WD_POS,1,1,WD_CLR,WDC_EOL )
	XCALL W_DISP(POP_WND,WD_POS,1,1,WHDR(1,PLEN) )
	XCALL W_DISP(POP_WND,WD_HLINE,2,1,PLEN+2)
	RETURN
;--------------------------

DSPWND,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; DISPLAY WINDOW, FILL W/ DATA
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; determine height & length of window, x & y coordinates
;;; to center window on screen.
	LAST_ROW = NUMROW + 1
	IF (NUMROW .GT. NUMARA) LAST_ROW = NUMARA + 1
	INCR LAST_ROW		;SSQ 6-17-98

	HT = LAST_ROW		;HEIGHT OF WINDOW, LEAVE ROOM FOR HEADER
	LN = PLEN + 2		;LENGHT OF WINDOW
	
	LN = LN + 1		;FOR "*"

	WHDR = DLINE

	IF (WX .LE. 0)
	THEN	X = (24-HT)/2		;X CO-ORDINATE
	ELSE	X = WX
	IF (WY .LE. 0)
	THEN	Y = (80-LN)/2		;Y CO-ORDINATE
	ELSE 	Y = WY

;;; place window...
	XCALL W_PROC(WP_FIND,POP_WND,POP_WID)
	IF (.NOT. POP_WND) XCALL W_PROC(WP_CREATE,POP_WND,POP_WID,HT,LN)
	TITLEN = %TRIM(POP_TITLE)
	IF (TITLEN .GT.0)
&	 XCALL W_BRDR(POP_WND,WB_TITLE,POP_TITLE(1,TITLEN),WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,POP_WND,X,Y)
	XCALL W_DISP(POP_WND,WD_CLEAR)
	XCALL W_DISP (POP_WND,WD_ATTR,ATTR_CLR+ATTR_RVRS)
	XCALL W_DISP(POP_WND,WD_POS,1,1,WHDR(1,PLEN) )
	XCALL W_DISP(POP_WND,WD_HLINE,2,1,PLEN+2)
;;; fill window w/ data...

	CLEAR ARO
	WLEN = LN
	WROW = 2

;;;	FOR I FROM 1 THRU LAST_ROW-2
; array's might contain 9's, read thru entire array skipping 9's
	clear fara, lara, num9
	i = 0
	do forever					;5-31-19
		BEGIN
		incr i					;5-31-19
		clear dline				;5-31-19
		if (parry(i) .eq. dline) exitloop	;5-31-19
		if (parcpl(i) .eq. 9)	;5-29-19 only open lines
		then	incr num9
		else	begin
			if (fara.le.0)fara=i		;first non-9
			lara = i			;save last non-9
			INCR WROW
			DLINE = PARRY(I)
			REST = DLINE(1,PLEN)
			XCALL W_DISP(POP_WND,WD_POS,WROW,1,WRKLIN(1,WLEN) )
			end
		END

	numara = numara + num9		;need all array elements for scrolling
	RETURN
;---------------------------------------------------------------------



