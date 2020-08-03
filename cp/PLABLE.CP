; IN PROCES - CHANGE PIC TO ALPHA
;PLABLE.CP 
;
; Print Product Labels
;
	.include 'wnd:windows.def'

RECORD	FKEYS
	.INCLUDE 'DEF:FUNKEY.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.def'
RECORD,X
	.INCLUDE 'DEF:RD041B.def'

RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	MATERIAL
	.INCLUDE 'DEF:CFGMAT.DEF'

RECORD	POP
	.INCLUDE 'DEF:POP40.DEF'

RECORD	POP6
	.INCLUDE 'DEF:POP6.DEF'

RECORD	MPOP1
	.INCLUDE 'def:mpop1.DEF'

RECORD	MPOP2
	.INCLUDE 'def:mpop2.DEF'

RECORD	MPOP3
	.INCLUDE 'def:mpop3.DEF'

RECORD	KEY_DSP
	KD1	,A3
		,A1
	KD2	,A3
		,A1
	KD3	,A5

RECORD	F_DESC	
	MM_DES1	,A12
	MM_B1	,A1
	MM_DES2	,A12
	MM_B2	,A1
	MM_DES3	,A13		;need a13 for "complete seal"

RECORD	CHANNEL
	CHN041	,D2
	CHN042	,D2
	CHN182	,D2

RECORD
	,	a30,	'90 DIE FORMED                 ';1
	,	a30,	'45 DIE FORMED                 ';2
	,	a30,	'END CAP                       ';3
	,	a30,	'EZ TAP COLLAR                 ';4
	,	a30,	'EZ TAP COLLAR DAMPER          ';5
	,	a30,	'EZ TAP COLLAR STAND OFF       ';6
	,	a30,	'EZ TAP CONICAL                ';7
	,	a30,	'EZ TAP CONICAL DAMPER         ';8
	,	a30,	'EZ TAP CONICAL STAND OFF      ';9
	,	a30,	'HET                           ';10
	,	a30,	'HET DAMPER                    ';11
	,	a30,	'HET STANDOFF                  ';12
	,	a30,	'CONCENTRIC REDUCER            ';13
	,	a30,	'90 PRESSED SADDLE             ';14
	,	a30,	'90 PRESSED SADDLE DAMPER      ';15
	,	a30,	'90 PRESSED SADDLE STANDOFF    ';16
	,	a30,	'45 SWEDGED SADDLE             ';17
	,	a30,	'ROUND PIPE HANGER             ';18
	,	a30,	'SINGLE ROD HANGER             ';19
	,	a30,	'DOUBLE ROD HANGER             ';20
	,	a30,	'** NO PICTURE **              ';21
RECORD	PICA,x
	P_NAME	,21A30

RECORD	WARS
	WND_1	,D4
	WND_2	,D4
	WND_6	,D4
	WND_P	,D4
	WND_M	,D4
	W_ID	,D4
	WN_NAME	,A6
	W2_NAME	,A6
	W6_NAME	,A6
	WLEN	,D2

RECORD	VARS
	batfil	,a*,	'pfile.bat'		;9-27-17
	l_des3	,a13		;for complete seal
	m_keyx	,a2
	blanks	,a20
	SELECT	,D1
	DAMPR	,D1
	DAMPER	,A10
	ZERO	,A5,	'00000'
	ALPHA	,A1
	F2	,D5
	F3	,D5
	MM_CODE	,D5
	V_LEVEL	,D1
	KEY_FOUND	,D1
	F_KEY	,D3
	J	,D6
	I	,D6
	PIC	,A4
	OPNOK	,D1
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	SRCCTL	,D1
	BSEND	,D6
	BSMID	,D6
	READ	,D1,0
	LOKCTL	,D1
	ITEM	,A15
	FLN	,D3
	LN	,D3
	DM	,D3
	DESC1	,A90
	DESC2	,A30
	QTY	,D6
	NUM	,D6
	BOXSIZ	,A30
	SWITCH	,D1
	V	,D1
;
PROC
	XCALL TERID(V)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	call init_window

DISPLA,
	CLEAR CNGCTL
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL W_DISP (WND_1,WD_POS, 1,1,'PRINT PRODUCT LABELS')
	XCALL W_DISP (WND_1,WD_POS, 4,4,'1. ITEM #')
	XCALL W_DISP (WND_1,WD_POS, 6,4,'2. INFO:')

	XCALL W_DISP (WND_1,WD_POS,08,4,'3. CARTON/BOX SIZE:')
	XCALL W_DISP (WND_1,WD_POS,10,4,'4. QTY PER BOX')
	XCALL W_DISP (WND_1,WD_POS,12,4,'5. PICTURE CODE')
	XCALL W_DISP (WND_1,WD_POS,14,4,'6. NUMBER OF LABELS')
	
ITEM,
	CALL POP_KEY
	USING LOKCTL SELECT
	(1),	GOTO ENDOFF		;1=ABORT
	(2),	BEGIN
		XCALL WNMSG (WND_1, 24, 'ITEM NOT ON FILE' ,1)
		GOTO DISPLA
		END
	ENDUSING

	BOXSIZ = IBXSIZ
	QTY = IBXQTY
	PIC = IPIC
	XCALL W_DISP (WND_1,WD_POS,08,25,BOXSIZ)

	ENTRY(1,4) = QTY,	'ZZZX' [LEFT]
	XCALL W_DISP (WND_1,WD_POS,10,25,ENTRY(1,4) )

	XCALL W_DISP (WND_1,WD_POS,12,25,PIC)

	CLEAR MM_B1, MM_B2
	IF (IF1.NE.ZERO .AND. IF2.NE. ZERO) MM_B1 = '/'
	IF (IF2.NE.ZERO .AND. IF3.NE. ZERO) MM_B2 = '/'
	XCALL W_DISP (W_ID, WD_POS, 5,46, F_DESC)

	IF (%INSTR (1, ITEMNO, '*') )
	THEN	DAMPR = 1
	ELSE	DAMPR = 0

	XCALL W_DISP (W_ID, WD_POS, 4,15, ITEMNO)
	CLEAR KEY_DSP
	KD1 = IF1
	KD2 = IF2
	KD3 = IF3
	XCALL W_DISP (W_ID, WD_POS, 4,32,KEY_DSP)
	XCALL W_DISP (W_ID, WD_POS, 5, 15, DESCR)

	CLEAR DESC1

	USING SUOFM SELECT
	('IN'),	DESC1 = 'Counted by the inch'
	('BL'),	DESC1 = 'Counted by the blank'
	ENDUSING

	IF (DAMPR) DESC1 = 'DAMPER'

	DM = 90			;# OF CHAR IN DESC1
	LN = %TRIM(DESC1)
	LN = LN + 1
	if (if1 .ne. zero)
		begin
		DESC1(LN,LN) = '*'
		INCR LN
		end
	DESC1 (LN,DM) = MM_DES1
	LN = %TRIM(DESC1)
	LN = LN + 1
	if (if2 .ne. zero)
		begin
		DESC1(LN,LN) = '*'
		INCR LN
		end
	DESC1 (LN,DM) = MM_DES2
	LN = %TRIM(DESC1)
	LN = LN + 1
	if (if3 .ne. zero)
		begin
		DESC1(LN,LN) = '*'
		INCR LN
		end
	DESC1 (LN,DM) = MM_DES3

	XCALL W_DISP (W_ID, WD_POS, 6,15, DESC1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	CALL GET_JPEG_NAME
;;;	IF (PIC .GT. 0)
;;;			BEGIN
;;;			ENTRY(1,4) = PIC,	'ZZZX' [LEFT]
;;;			XCALL W_DISP(W_ID, WD_POS, 14,25,ENTRY(1,4) )
;;;			XCALL W_DISP(W_ID, WD_POS, 15,25,P_NAME(PIC))
;;;			END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	goto num
;;;	IF (IF1.NE.ZERO .OR. IF2.NE.ZERO .OR. IF3.NE.ZERO) GOTO QTY
;;;	IF (DAMPR) GOTO BOXSIZ

DESC1,
	XCALL WINPT (W_ID, 6,15,30,00,'A ',ENTRY,INXCTL)
	GOTO (DISPLA),INXCTL
	DESC1 = ENTRY(1,30)
	GOTO (ANYCN),CNGCTL

BOXSIZ,
	XCALL WINPT (W_ID, 8,25,30,00,'A ',ENTRY,INXCTL)
	GOTO (DISPLA),INXCTL
	BOXSIZ = ENTRY(1,30)
	GOTO (ANYCN),CNGCTL
QTY,
	XCALL WINPT (W_ID, 10,25,06,00,'# ',ENTRY,INXCTL)
	GOTO (DISPLA),INXCTL
	QTY = ENTRY(1,6)
	GOTO (ANYCN),CNGCTL
PIC,
	XCALL WINPT (W_ID,12,25,04,00,'A ',ENTRY,INXCTL)
	PIC = ENTRY(1,4)
;;;	IF (PIC.LT.1 .OR. PIC.GT.21) GOTO PIC
;;;	XCALL W_DISP(W_ID, WD_POS, 15,25,P_NAME(PIC))
;;;	XCALL W_UPDT	
	GOTO (ANYCN),CNGCTL

NUM,
	XCALL WINPT (W_ID,14,25,02,00,'# ',ENTRY,INXCTL)
	GOTO (DISPLA),INXCTL
	NUM = ENTRY(1,2)
	if (num .eq. 0)
		begin
		num = 1
		xcall w_disp (w_id, wd_pos,14,25,'1  ')
		end
	GOTO ANYCN

ANYCN,
	cngctl = 1
	if (boxsiz .eq. blanks) goto boxsiz
	if (pic .eq. blanks) goto pic
	XCALL WANCN(W_ID, 24, CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR),CNGCTL + 1
CNGBR,
	GOTO (ITEM, DESC1, BOXSIZ, QTY, PIC, NUM), WHATNO
	GOTO DISPLA
PROCES,
	XCALL LBLPD (ITMMAS, CHN182, QTY, NUM, DESC1, BOXSIZ, PIC, batfil)
	GOTO DISPLA

;========================================================
POP_KEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Find by item # & f_keys
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR MM_DES1, MM_DES2, MM_DES3
	XCALL W_PROC(WP_PLACE, WND_2, 4, 4)
P_DISPLA,
	CLEAR CNGCTL
	XCALL W_DISP(WND_2, WD_CLEAR)
	XCALL W_DISP(WND_2, WD_POS, 1,1, '1. ITEM #')
	XCALL W_DISP(WND_2, WD_POS, 2,1, '2. F1_KEY')
	XCALL W_DISP(WND_2, WD_POS, 3,1, '3. F2_KEY')
	XCALL W_DISP(WND_2, WD_POS, 4,1, '4. F3_KEY')
P_ITEM,
	XCALL WINPT (WND_2,1,13, 15, 00, 'AE', ENTRY, INXCTL)
	GOTO (P_DISPLA, P_ENDOFF),INXCTL
	K_ITEM = ENTRY(1,15)
	GOTO (P_ANY),CNGCTL
P_F1,
	XCALL WINPT (WND_2,2,13, 3, 00, '# ', ENTRY, INXCTL)
	GOTO (P_DISPLA),INXCTL
	MM_CODE = ENTRY(1,3)
	K_F1 = MM_CODE,	'XXX'

	m_keyx = 'M1'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		XCALL W_DISP (WND_2, WD_POS, 2,20, MM_SHORT)
		MM_DES1 = MM_SHORT
		END
	ELSE	CLEAR MM_DES1
	GOTO (P_ANY),CNGCTL
P_F2,
	XCALL WINPT (WND_2,3,13, 3, 00, '# ', ENTRY, INXCTL)
	GOTO (P_DISPLA),INXCTL
	MM_CODE = ENTRY(1,3)
	K_F2 = MM_CODE,	'XXX'

	m_keyx = 'M2'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		XCALL W_DISP (WND_2, WD_POS, 3,20, MM_SHORT)
		MM_DES2 = MM_SHORT
		END
	ELSE	CLEAR MM_DES2

	GOTO (P_ANY),CNGCTL
P_F3,
	XCALL WINPT (WND_2,4,13, 5, 00, '# ', ENTRY, INXCTL)
	GOTO (P_DISPLA),INXCTL
	MM_CODE = ENTRY(1,5)
	K_F3 = MM_CODE,	'XXXXX'

	m_keyx = 'M3'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		XCALL W_DISP (WND_2, WD_POS, 4,20, MM_SHORT)
		MM_DES3 = MM_SHORT
		IF(MM_SHORT.EQ.'COMPLETESEAL') MM_DES3='COMPLETE SEAL'
		END
	ELSE	CLEAR MM_DES3

	GOTO (P_ANY),CNGCTL
P_ANY,	
	XCALL WANCN (WND_2, 6, CNGCTL, WHATNO)
	GOTO (P_PROCES, P_CNGBR),CNGCTL+1
P_CNGBR,
	GOTO (P_ITEM, P_F1, P_F2, P_F3),WHATNO
	GOTO P_ANY


P_PROCES,
;;;	XCALL W_DISP (W_ID, WD_POS, 4,15, KITMNO)
;;;	KITMNO = K_ITEM
	CLEAR K_MAT
	XCALL W_PROC (WP_REMOVE, WND_2)
	GOTO EXIT_KEY

P_ENDOFF,
	LOKCTL = 1			;ABORT
	XCALL W_PROC (WP_REMOVE, WND_2)
	RETURN
;------------------------------------------------------------

MEMO_KEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY

;;;	TBLCOD = 'MM'
	TBLCOD = m_keyx
	MM_KEY = MM_CODE
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_KEY]
	KEY_FOUND = 1		
	RETURN
NOT_KEY,
	KEY_FOUND = 0
	RETURN
;-------------------------------------------------------------


EXIT_KEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	LOKCTL = 0
	READ (CHN041, ITMMAS, ITMKEY, KEYNUM:1) [ERR=P_BAD_KEY]	;KRF = 2
	GOTO P_KEY_OK
P_BAD_KEY,
	CLEAR ITMMAS
;;;	KITMNO = K_ITEM
	LOKCTL = 2		;key not same
P_KEY_OK,
	CLEAR INXCTL, SELECT

	USING LOKCTL SELECT
	(0),	BEGIN
		SELECT = 2		;item exists, change mode
		CNGCTL = 1		;change mode
		END

	(1),	INXCTL = 1		;ABORT

	(),	BEGIN
		IF1 = K_F1
		IF2 = K_F2
		IF3 = K_F3
		IMAT = K_MAT
		SELECT = 1		;add new item
		END
	ENDUSING

	RETURN
;--------------------------------------------------------------
;==============================================================

;------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this code not used ?
;;;F_POP,
;;;	CALL LOAD_MEMO_POP
;;;	IF (NUMARA .EQ. 0) RETURN
;;;	DLINE = "  NO  MEMO"
;;;;;;	READS (15,ALPHA)
;;;	XCALL POP40(POP)
;;;	USING P_ACTION SELECT
;;;	(1, 4),	BEGIN
;;;		DLINE = PARRY(PI)
;;;		MM_CODE = DLINE (1,5)	;ssq 7-31-03
;;;		GOTO MEMO_KEY
;;;		END
;;;	ENDUSING
;;;
;;;	RETURN
;;;LOAD_MEMO_POP,
;;;	CLEAR TBL_KEY, KEY_FOUND
;;;	TBLCOD = 'MM'
;;;	CLEAR MM_KEY
;;;	FIND (CHN182, COPTBL, TBL_KEY) [ERR=LM_FIND]
;;;LM_FIND,
;;;	CLEAR I
;;;	FOR J FROM 1 THRU MAXARA
;;;		BEGIN
;;;		XCALL IOS (CHN182, COPTBL, READ, LOKCTL)
;;;		IF (LOKCTL .NE. 0) GOTO NO_MORE_TBL
;;;		IF (TBLCOD .NE. 'MM') GOTO NO_MORE_TBL
;;;		IF (MM_VALID.EQ.0 .OR. MM_VALID.EQ.V_LEVEL)
;;;		   BEGIN
;;;		   CLEAR DLINE
;;;		   DLINE (1,5) = MM_KEY,	'ZZZZX' [LEFT]
;;;		   DLINE (7,37) = MM_LABEL
;;;		   INCR I
;;;		   PARRY(I) = DLINE
;;;		   END
;;;		END
;;;NO_MORE_TBL,
;;;	NUMARA = I
;;;	RETURN
;;;----------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;

get_jpeg_name,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	using itemno select
;;;	('BO06' THRU 'BO20'),	BEGIN
;;;				PIC = 9
;;;				END
;;;
;;;	('E04' THRU 'E24'),		BEGIN
;;;					PIC= 4
;;;					END
;;;	('EC03265' THRU 'EC1822S'),	BEGIN
;;;					PIC= 3
;;;					END
;;;	('EC04' THRU 'EC2024'),		BEGIN
;;;					PIC= 7
;;;					END
;;;	('ECD05' THRU 'ECD2024'),	BEGIN
;;;					PIC= 8
;;;					END
;;;	('ED0345' THRU 'ED1245'),	BEGIN
;;;					PIC= 2
;;;					END
;;;	('ED0390' THRU 'ED1290'),	BEGIN
;;;					PIC= 1
;;;					END
;;;
;;;	('R22' THRU 'R26'),		BEGIN
;;;					PIC= 13
;;;					END
;;;
;;;	('S926*' THRU 'S926*'),		BEGIN
;;;					PIC= 15
;;;					END
;;;	('S922' THRU 'S926'),		BEGIN
;;;					PIC= 14
;;;					END
;;;	('SPH03' THRU 'SPH30'),		BEGIN
;;;					PIC= 19
;;;					END
;;;	('SPH06D' THRU 'SPH96D'),	BEGIN
;;;					PIC= 20
;;;					END
;;;	('SW422' THRU 'SW426'),		BEGIN
;;;					PIC= 17
;;;					END
;;;
;;;	('H04' THRU 'H2424'),		BEGIN
;;;					PIC= 10
;;;					END
;;;	('HD04' THRU 'HD2424'),		BEGIN
;;;					PIC= 11
;;;					END
;;;	('HH03' THRU 'HH36'),		BEGIN
;;;					PIC= 18
;;;					END
;;;	('HO06' THRU 'HO18'),		BEGIN
;;;					PIC= 12
;;;					END
;;;
;;;	(),				BEGIN
;;;					PIC= 21
;;;					END
;;;	endusing

	return

;------------------------------------------------
OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1,'SI',041,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 1

	SWITCH = 5
	XCALL FILES (3,'SI',182,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 3


	OPNOK = 1
	RETURN
;---------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN041) CLOSE CHN041
	IF (CHN182) CLOSE CHN182
	RETURN
;---------------------------------------------


;========================================================

init_window,

	WN_NAME = 'PLABL'

	xcall w_init(0,15,10)
;;;	XCALL W_INIT(POOLSIZE,WNDCHNL,MAXWINS)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'PRODUCT LABELS',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1

;-
	W2_NAME = 'FKEY'
	XCALL W_PROC(WP_FIND, WND_2, W2_NAME)
	IF (.NOT. WND_2) XCALL W_PROC(WP_CREATE,WND_2,W2_NAME,6,41)
	XCALL W_BRDR(WND_2,WB_TITLE,'Item Keys',
&		WB_TPOS,WBT_TOP,WBT_CENTER)
;-

;Pallets...
	XCALL W_PROC(WP_PALET, 4, 1, 0)		;1=BLUE
	XCALL W_PROC(WP_PALET, 5, 2, 0)		;2=GREEN
	XCALL W_PROC(WP_PALET, 6, 3, 0)		;3=CYAN
	XCALL W_PROC(WP_PALET, 7, 4, 7)		;4=RED
	XCALL W_PROC(WP_PALET, 8, 7, 0)		;7=WHITE
	XCALL W_PROC(WP_PALET, 9, 0, 7)		;4=BLACK ON WHITE
	XCALL W_PROC(WP_PALET,10, 6, 0)		;6=YELLOW
;POP40:
	MAXARA = 40
	PLEN = 48
	NUMROW = 10
	WX = 19
	WY = 18
	POP_WID(1,5) = "MM_TBL"
	POP_TITLE = "CANNED MEMOS"

;POP-M2:
	MAXARA2 = 10		
	PLEN2 = 34
	NUMROW2 = 4
	WX2 = 19
	WY2 = 4
	POP_WID2(1,5) = "F2WIN"
	POP_TITLE2 = "LEVEL-2 MEMOS"

;POP-M3:
	MAXARA3 = 10	
	PLEN3 = 34
	NUMROW3 = 4
	WX3 = 19
	WY3 = 44
	POP_WID3(1,5) = "F3WIN"
	POP_TITLE3 = "LEVEL-3 MEMOS"

;POP-MATERIAL:
	MAXARA6 = 4	
	NUMARA6 = 4
	PLEN6 = 15
	NUMROW6 = 4
	WX6 = 1
	WY6 = 48
	POP_WID6(1,5) = "F6WIN"
	POP_TITLE6 = "MATERIAL"
	
	FOR I FROM 1 THRU 4	PARRY6(I) = C_DES(I)
;;;	PARRY6(1) = "GALVANIZED"
;;;	PARRY6(2) = "ALUMINUM"
;;;	PARRY6(3) = "PAINT GRIP"
;;;	PARRY6(4) = "STAINLESS STEEL"


	RETURN

;---------------------------------------



ENDOFF,
	CALL CLOSE
	STOP
	END

