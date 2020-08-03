;OOSDST.CP
;
;	STAND ALONE OUT OF STOCK LOOK-UP PGM
;
	.DEFINE POOLSIZE	,25000
	.DEFINE WNDCHNL		,15
	.DEFINE MAXWINS		,10
	.INCLUDE 'WND:WINDOWS.DEF'



RECORD	POP
	.INCLUDE 'DEF:POP1.DEF'

RECORD	WN_NAME
		,A5,	'OOSDS'
	WN_TNMBR,D4

RECORD	WARS
	WND_1	,D4
	W_ID	,D4

RECORD	OOSMSG
	.INCLUDE 'DEF:OOSMSG.DEF'

RECORD	FILOOS
	FS_DEV	,A3
		,A1,	':'
	FS_NAME	,A6
		,A1,	'.'
	FS_EXT	,A3

RECORD	CHANNEL
	CHNOOS	,D2

RECORD	VARS
	OPNOK	,D1
	ENTRY	,A30
	INXCTL	,D1
	READ	,D1,0
	LOKCTL	,D1
	XDATE	,D6
	SAVRFA	,A6
	OPT	,D1
	CLCTL	,D1
	I	,D6
	V	,D1

PROC
	XCALL TERID(V)
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	CALL INIT_WINDOW
DISPLA,
	XCALL W_DISP(WND_1,WD_POS,1,1,'OUT OF STOCK')
	CALL GET_OOS
NEXT_POP,
	IF (NUMARA .LE. 0) GOTO ENDOFF
	DLINE = '      DATE ORDER#     CUSTOMER             ITEM #          QTY-ORD  QTY-AVL'
	XCALL OOPOP(POP)	
	IF(PI.EQ.0) GOTO ENDOFF

	USING P_ACTION SELECT
	(4),	BEGIN			;NEXT PAGE
		CALL DELETE_LINES	;CHECK FOR LINES MARKED FOR DELETION
		CLEAR NUMARA, I
		CALL OOS_LOOP
		GOTO NEXT_POP
		END
	ENDUSING

	CALL DELETE_LINES
	GOTO DISPLA
;===================================================================
ENDOFF,
	CALL CLOSE
	XCALL FLAGS (7000000)	;suppress program termination message
	STOP
;=========================================================
;=========================================================

GET_OOS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR NUMARA, I
	FIND (CHNOOS,OOSMSG,^LAST) [ERR=EXIT_OOS]
OOS_LOOP,
	READS (CHNOOS,OOSMSG,EXIT_OOS,REVERSE,GETRFA:SAVRFA)
	IF(F_FLAG.EQ.9)GOTO OOS_LOOP	;MARKED FOR DELETION

	XDATE(1,4)=F_DATE(5,8)
	XDATE(5,6)=F_DATE(3,4)
	DLINE(1,8)=XDATE,	'XX/XX/XX'
	DLINE(10,15)=F_ORD,	'ZZZZZX'
;;;	DLINE(17,19)=F_SLS
	DLINE(21,40)=F_NAME
	DLINE(42,56)=F_ITEM
	DLINE(58,64)=F_QTYOR,	'ZZZ,ZZX'
	DLINE(67,74)=F_QTYAV,	'ZZZ,ZZX-'
	INCR I
	PARRY(I)=DLINE
	PARRFA(I)=SAVRFA
	IF(I.LT.MAXARA) GOTO OOS_LOOP

EXIT_OOS,
	NUMARA = I
	RETURN
;------------------------------------------------------

DELETE_LINES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;;delete selected items
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FOR I FROM 1 THRU NUMARA
		BEGIN
		IF(PFLAGS(I).EQ.1)
			BEGIN
			READ(CHNOOS, OOSMSG, F_DATE, RFA:PARRFA(I))
			DELETE (CHNOOS)
			END
		END
	RETURN
;-----------------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL FFILE (45, FILOOS, CLCTL)

;;;	OPEN(19, SU, 'SMC:OOSMSG.ISM')
	FS_NAME = 'OOSMSG'
	FS_EXT(3,3) = 'M'
	OPEN(19,SU,FILOOS)

	CHNOOS = 19
;;;	OPEN(1,SU,'SMC:OOSMSG.ISM')
;;;	CHNOOS = 1
	OPNOK=1
	RETURN
;------------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLOSE CHNOOS
	RETURN
;------------------------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL W_INIT(POOLSIZE,WNDCHNL,MAXWINS)

	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)
		END
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1

;; POP info...

	MAXARA = 20		;9-19-97 NOT MORE THAN 1 FULL WINDOW
	PLEN = 76
	NUMROW = 18
	WX = 4
	WY = 1
	POP_WID(1,5) = "FXPOP"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "OUT OF STOCK"
	RETURN
;--------------------------------------------------

;    DATE ORDER# SLM CUSTOMER             ITEM #          QTY-ORD  QTY-AVL
;XX/XX/XX ZZZZZX AAA AAAAAAAAAAAAAAAAAAAA AAAAAAAAAAAAAAA ZZZ,ZZX  ZZZ,ZZX- 
;123456789012345678901234567890123456789012345678901234567890123456789012345
;         1         2         3         4         5         6         7
