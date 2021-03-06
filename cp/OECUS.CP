SUBROUTINE	OECUS
	HEADER	,A		;ORDER HEADER RECORD	
	TMPREC	,A		;TMPCUS

	; 11-18-19 ssq: miscus mod

.INCLUDE 'WND:WINDOWS.DEF'

EXTERNAL FUNCTION
	NORM	,A

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	TMPCUS
	.INCLUDE 'DEF:RD139A.DEF'

RECORD	MISCUS
	.INCLUDE 'DEF:RD001A.DEF'	;MISCUS SAME FORMAT AS CUSMAS

RECORD	MISCTL
	.INCLUDE 'DEF:RD101B.DEF'

RECORD	ARTCDE
	.INCLUDE 'DEF:RD169A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD169B.DEF'

RECORD	POPAR
	.INCLUDE 'DEF:OEPOP.DEF'

RECORD	FUNKEY
	.INCLUDE 'DEF:FUNKEY.DEF'

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

RECORD	CHANNEL
	CHN139	,D2
	CHN169	,D2
	CHN101	,D3,	101

RECORD	WVARS
	W1_NAME	,A7
	WND_1	,D4
	W_ID	,D4

RECORD	WN_NAME
		,A5,	'EMKEY'
	WN_TNMBR,D4

RECORD	VARS
	F_KEY	,D3
	SHIP_SRCH,D1
	HAS_MIS		,D1
	HAS_TMP		,D1
	MIS_SRCH	,D1
	SRCH_KEY	,A20
	GOT_MISCUS	,D1
	ROUTINE	,A32
	LN	,D6
	A30	,A30
	GETTAX	,D1
	OPNOK	,D1
	I	,D6
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	SWITCH	,D1
	SELECT 	,D1
	SRCCTL	,D1
	BSEND	,D6
	BSMID	,D6
	KEY	,A3
	DECMAL	,D18

PROC
	ORDHDR = HEADER			;11-18-19
	XCALL MODNAME (1, ROUTINE)	;GET CALLING ROUTINE
	LN = %TRIM(ROUTINE)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO FINI
	CALL INIT_WINDOW

BEGIN,
	CALL PAINT

	CLEAR HAS_MIS, HAS_TMP
	READ(CHN139, TMPCUS, OORDNO) [ERR=NEWCUS]		;ORDER IS CUSNO
	HAS_TMP = 1						;TMPCUS EXISTS!

	IF (OMICUS .GT. 0)
		BEGIN
		XCALL ISIO (CHN101, MISCUS, OMICUS, READ, LOKCTL)	;READ MISCUS

		IF (LOKCTL .NE. 0) 
			BEGIN
			UNLOCK CHN101
			GOTO NEWCUS
			END
		HAS_MIS = 1				;MISCUS EXISTS!
		CALL MIS_TO_TEMP			;IN CASE SOMETHING CHANGED
		END

	SELECT = 2					;EXISTING TMP CUST
	GOTO DIS_CUS
NEWCUS,
	SELECT = 1					;ADD NEW TMP CUST
	CALL GET_MISCUS
	IF (GOT_MISCUS)
		BEGIN
		HAS_MIS = 1				;NOW WE HAVE A MISCUS
		CALL MIS_TO_TEMP
		SELECT = 2
		GOTO DIS_CUS
		END
	
DISPLA,
	CLEAR GETTAX
	CLEAR CNGCTL
	CLEAR TMPCUS
	T_CUSNO = OORDNO
NAME,
	CTL = '01,15,25,00,AE'
	CALL INPUT
	GOTO (BEGIN,ENDOFF),INXCTL
	T_NAME = ENTRY(1,25)
	IF (T_NAME .EQ. A30) GOTO NAME
	GOTO (ANYCNG),CNGCTL
ADD1,
	CTL = '02,15,25,00,AE'
	CALL INPUT
	GOTO (DISPLA,ENDOFF),INXCTL
	T_ADD1 = ENTRY(1,25)
	IF (T_ADD1 .EQ. A30) GOTO ADD1
	GOTO (ANYCNG),CNGCTL
ADD2,
	CTL = '03,15,21,00,A '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	T_ADD2 = ENTRY(1,21)
	GOTO (ANYCNG),CNGCTL
CITY,
	CTL = '04,15,15,00,A '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	T_CITY = ENTRY(1,15)
	IF (T_CITY .EQ. A30) GOTO CITY
	GOTO (ANYCNG),CNGCTL
STATE,
	CTL = '05,15,02,00,A '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	T_STATE = ENTRY(1,2)
	IF (T_STATE .EQ. '  ') GOTO STATE
	SRCCTL = 2
	XCALL SERCH (CHN169,ARTCDE,T_STATE,1,2,BSEND,BSMID,SRCCTL,4,4,9,0,0,0,0)
	IF (SRCCTL.NE.0) 
	THEN	BEGIN
		XCALL WNMSG(WND_1,44,'NO TAX CODE',1)
		CLEAR T_TAXFLG
		GOTO STATE
		END
	ELSE	T_TAXFLG = T_STATE
	GOTO (TAXFLG),CNGCTL
	GOTO (ANYCNG),CNGCTL
ZIP,
	CTL = '06,15,10,AA,A '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	T_ZIP = ENTRY(1,10)
	IF (T_ZIP .EQ. A30) GOTO ZIP
	GOTO (ANYCNG),CNGCTL
TAXFLG,
	CTL = '07,15,03,00,A '
;;;	IF (T_TAXFLG .EQ. '  ')
	IF (T_TAXFLG .EQ. '  ' .or. cngctl)
		BEGIN
		CALL INPUT
		GOTO (DISPLA),INXCTL
		T_TAXFLG = ENTRY(1,3)
		SRCCTL = 2
		XCALL SERCH (CHN169,ARTCDE,T_TAXFLG,1,3,BSEND,BSMID,SRCCTL,4,4,9,0,0,0,0)
		IF (SRCCTL.NE.0) 
			BEGIN
			XCALL WNMSG(WND_1,44,'INVALID TAX CODE',1)
			GOTO TAXFLG
			END
		END

	XCALL W_DISP(WND_1,WD_POS,ROW,COL+5, ARTDSC)
	XCALL W_UPDT

	GOTO (ANYCNG),CNGCTL	
PHONE,
	CTL = '08,15,10,00,# '
	XCALL W_DISP(WND_1, WD_POS, ROW,COL, WD_CLR, WDC_EOL)
	CALL INPUT
	GOTO (DISPLA),INXCTL
	T_PHONE = ENTRY(1,10)
	ENTRY(1,12) = T_PHONE,	'ZZZ XXX XXXX' [LEFT]
	XCALL W_DISP(WND_1, WD_POS, ROW, COL, ENTRY(1,12))
	XCALL W_UPDT
	GOTO (ANYCNG),CNGCTL
FAX,
	CTL = '09,15,10,00,# '
	XCALL W_DISP(WND_1, WD_POS, ROW,COL, WD_CLR, WDC_EOL)
	CALL INPUT
	GOTO (DISPLA),INXCTL
	T_FAX = ENTRY(1,10)
	ENTRY(1,12) = T_FAX,	'ZZZ XXX XXXX' [LEFT]
	XCALL W_DISP(WND_1, WD_POS, ROW, COL, ENTRY(1,12))
	XCALL W_UPDT
	GOTO (ANYCNG),CNGCTL
;;;EMAIL,
;;;	CTL = '10,15,30,00,A '
;;;	CALL INPUT
;;;	GOTO (DISPLA),INXCTL
;;;	T_EMAIL = ENTRY(1,30)
	GOTO ANYCNG

DIS_CUS,
	XCALL W_DISP(WND_1, WD_POS, 1, 15, T_NAME)
	XCALL W_DISP(WND_1, WD_POS, 2, 15, T_ADD1)
	XCALL W_DISP(WND_1, WD_POS, 3, 15, T_ADD2)
	XCALL W_DISP(WND_1, WD_POS, 4, 15, T_CITY)
	XCALL W_DISP(WND_1, WD_POS, 5, 15, T_STATE)
	XCALL W_DISP(WND_1, WD_POS, 6, 15, T_ZIP)
	XCALL W_DISP(WND_1, WD_POS, 7, 15, T_TAXFLG)

	ENTRY(1,12) = T_PHONE,	'ZZZ XXX XXXX' [LEFT]
	XCALL W_DISP(WND_1, WD_POS, 8, 15, ENTRY(1,12))

	ENTRY(1,12) = T_FAX,	'ZZZ XXX XXXX' [LEFT]
	XCALL W_DISP(WND_1, WD_POS, 9, 15, ENTRY(1,12))

;;;	XCALL W_DISP(WND_1, WD_POS,10, 15, T_EMAIL)
;;;	IF (ROUTINE(1,LN) .NE. 'SCRN1') GOTO ANYCNG

	XCALL W_DISP(WND_1, WD_POS, 15,1, '<F1> = START OVER WITH NEW CUST')
	XCALL WINPT(W_ID, 15,33, 01,00,'AE',ENTRY,INXCTL,F_KEY)

	XCALL W_DISP(WND_1, WD_POS,15,1, WD_CLR, WDC_LIN)
	XCALL W_UPDT

	USING F_KEY SELECT
	(F_01),	BEGIN
		OMICUS =		;START OVER W/ NEW CUST
		XCALL ISIO (CHN139, TMPCUS, OORDNO, READ, LOKCTL)
		IF (LOKCTL.EQ.0) XCALL ISIO (CHN139, TMPCUS, OORDNO, DELETE, LOKCTL)
		GOTO BEGIN		;START OVER
		END
	ENDUSING

;-----------------------------------------------------------------
ANYCNG,
	XCALL WANCN(W_ID,15,CNGCTL,WHATNO)
	GOTO (PROCES,CNGBR), CNGCTL+1
CNGBR,
;;;	GOTO (NAME,ADD1,ADD2,CITY,STATE,ZIP,TAXFLG,PHONE,FAX,EMAIL),WHATNO
	GOTO (NAME,ADD1,ADD2,CITY,STATE,ZIP,TAXFLG,PHONE,FAX),WHATNO
	GOTO ANYCNG

PROCES,
	OMICUS = CUSNO		;SAVE NEW MISCUS.CUSNO
	CALL TEMP_TO_MIS	;
	CUSNO = OMICUS		;CUSNO cleared in temp_to_mis

	USING SELECT SELECT
	(1),	BEGIN
		XCALL ISIO (CHN139, TMPCUS, T_CUSNO, STORE, LOKCTL)
		;IF (.not.HAS_MIS)
		;	BEGIN
		;	MISKEY = 
		;	XCALL ISIO (CHN101, MISCTL, MISKEY, READ, LOKCTL)
		;	INCR REC101
		;	XCALL ISIO (CHN101, MISCTL, MISKEY, WRITE, LOKCTL)

		;	CUSNO = REC101
		;	CUSCD = 'EM'	;EMAIL
		;	XCALL ISIO (CHN101, MISCUS, CUSNO, STORE, LOKCTL)
		;	END		
		END
	(2),	BEGIN
		IF (HAS_TMP)
		THEN	XCALL ISIO (CHN139, TMPCUS, T_CUSNO, WRITE, LOKCTL)
		ELSE	XCALL ISIO (CHN139, TMPCUS, T_CUSNO, STORE, LOKCTL)
	;;;	IF (HAS_MIS) write (CHN101, MISCUS, CUSNO)
		IF (HAS_MIS) XCALL ISIO (CHN101, MISCUS, CUSNO, WRITE, LOKCTL)
		END
	ENDUSING

;1-22-20 changed:
	IF (.not.HAS_MIS)
		BEGIN
		MISKEY = 
		XCALL ISIO (CHN101, MISCTL, MISKEY, READ, LOKCTL)
		INCR REC101
		XCALL ISIO (CHN101, MISCTL, MISKEY, WRITE, LOKCTL)

		CUSNO = REC101
		CUSCD = 'EM'	;EMAIL
		XCALL ISIO (CHN101, MISCUS, CUSNO, STORE, LOKCTL)
		END		

	GOTO FINISH

ENDOFF,
	IF (ROUTINE(1,LN) .EQ. 'SCRN1')
		BEGIN
		XCALL ISIO (CHN139, TMPCUS, OORDNO, READ, LOKCTL)
		IF (LOKCTL .EQ. 0) XCALL ISIO (CHN139, TMPCUS, OORDNO, DELETE, LOKCTL)
		OMICUS = 	;FORCE RE-START
		END

FINISH,
	TMPREC = TMPCUS		;RETURN DATA
	OMICUS = CUSNO		;SAVE NEW MISCUS.CUSNO
	OCUSNM = NAME		;MISCUS.NAME
	HEADER = ORDHDR		;11-18-19

	CALL CLOSE
FINI,
	XCALL W_PROC(WP_DELETE, W_ID)
	xcall w_updt
	XRETURN
;================================================

MAK_MISCUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR MISCUS
	RETURN
;------------------------------------------------

GET_MISCUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	GOT_MISCUS = 0
	CLEAR MISCUS, TMPCUS
	XCALL W_DISP (WND_1, WD_POS, 15, 1, WD_CLR, WDC_LIN, 'SEARCH:')
	XCALL WINPT (WND_1, 15, 12, 20, 00, 'AE', ENTRY, INXCTL)
	IF (INXCTL .NE. 0) RETURN

	
;;;	SRCH_KEY = %NORM (ENTRY(1,20))
	SRCH_KEY = ENTRY(1,20)
	ESORNA = SRCH_KEY
	CALL GET_MIS
D_MIS,
	IF (%TTSTS) XCALL W_DISP(WND_1, WD_ACCEPT, DECMAL)
	IF (NUMARA .GT. 0)
	THEN	BEGIN
		DLINE = 'NUM  NAME'
		POP_TITLE = '<CR> = MORE CASH CUSTOMERS'
		XCALL OEPOP(POPAR)
		END
	ELSE	BEGIN
		XCALL OLMSG (WND_1, 15, 'NO MORE CASH CUSTOMERS',1)
		XCALL W_DISP(WND_1, WD_POS, 15,1, WD_CLR, WDC_EOL,'ADD NEW CASH CUSTOMER?')
		XCALL WINPT (WND_1, 15,25,01,01,'YN',ENTRY,INXCTL)
		IF (INXCTL .EQ. 1)
		THEN 	RETURN
		ELSE	GOTO GET_MISCUS
		END

	USING P_ACTION SELECT
	(0),	INXCTL = 1		;<END>
	(1),	BEGIN			;SELECTED
		DLINE = PARRY(PI)
		CUSNO = DLINE(1,6)
		XCALL ISIO (CHN101, MISCUS, CUSNO, READ, LOKCTL)
		GOT_MISCUS = 1
		END
	(2),	NOP			;INSERT
	(4),	BEGIN
		CALL MORE_MIS
		GOTO D_MIS
		END
;;;	(4),	GOTO MORE_MIS
	ENDUSING

	RETURN
;-------------------------------------------------------


GET_MIS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	READ (CHN101, MISCUS, ^FIRST)[ERR=MORE_MIS]	;SKIP THE HEADER
;;;	FIND (CHN101, MISCUS, ESORNA)[ERR=MORE_MIS]
MORE_MIS,
	I = 0
GM_LOOP,
	XCALL IOS (CHN101, MISCUS, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO GM_EOF
	;SEARCH ALL
	SHIP_SRCH = 0		;NO MATCH
	IF (%INSTR(1,NAME,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
	IF (%INSTR(1,ADD1,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
	IF (%INSTR(1,ADD2,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
	IF (%INSTR(1,CITY,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
	IF (%INSTR(1,STATE,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
	IF (%INSTR(1,ZIP,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
	IF (SHIP_SRCH .EQ. 0) GOTO GM_LOOP


	CLEAR DLINE
	DLINE(1,6) = CUSNO, 'ZZZZZZ' [LEFT]
	DLINE(8,34) = NAME
	INCR I
	PARRY(I) = DLINE

	DLINE(1,25) = ADD1
	DLINE(26,46) = ADD2
	DLINE(47,61) = CITY
	DLINE(63,64) = STATE
	DLINE(66,75) = ZIP
	P_ADD(I) = DLINE

	IF (I .LT. MAXARA) GOTO GM_LOOP
GM_EOF,
	NUMARA = I
	RETURN
;------------------------------------------------



MIS_TO_TEMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR TMPCUS
	T_CUSNO = OORDNO
	T_NAME = NAME
	T_ADD1 = ADD1
	T_ADD2 = ADD2
	T_CITY = CITY
	T_ZIP = ZIP
	T_STATE = STATE
	T_TAXFLG = TAXFLG
	T_PHONE = PHONE
	T_SLSMAN = SLSMAN
	T_FAX = EFAX
	T_ESORNA = ESORNA
	T_EMAIL = EAPNA			;USE THIS FOR EMAIL
	RETURN
;------------------------------------------------

TEMP_TO_MIS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	CLEAR MISCUS

	T_ESORNA = %NORM(T_NAME)

	NAME = T_NAME
	ADD1 = T_ADD1
	ADD2 = T_ADD2
	CITY = T_CITY
	STATE = T_STATE
	ZIP = T_ZIP
	TAXFLG = T_TAXFLG
	PHONE = T_PHONE
	EFAX= T_FAX
	ESORNA = T_ESORNA
	EAPNA = T_EMAIL
	
	RETURN
;------------------------------------------------

PAINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL W_DISP(WND_1, WD_CLEAR)
	XCALL W_DISP(WND_1, WD_POS, 1,3,'1. NAME')
	XCALL W_DISP(WND_1, WD_POS, 2,3,'2. ADD1')
	XCALL W_DISP(WND_1, WD_POS, 3,3,'3. ADD2')
	XCALL W_DISP(WND_1, WD_POS, 4,3,'4. CITY')
	XCALL W_DISP(WND_1, WD_POS, 5,3,'5. STATE')
	XCALL W_DISP(WND_1, WD_POS, 6,3,'6. ZIP')
	XCALL W_DISP(WND_1, WD_POS, 7,3,'7. TAX CODE')
	XCALL W_DISP(WND_1, WD_POS, 8,3,'8. PHONE #')
	XCALL W_DISP(WND_1, WD_POS, 9,3,'9. FAX #')
;;;	XCALL W_DISP(WND_1, WD_POS,10,2,'10. E-MAIL')
	XCALL W_UPDT

	RETURN
;------------------------------------------

INPUT,
	XCALL WINPT(W_ID,ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL)

	RETURN
;------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK
	SWITCH = 5
	XCALL FILES(33,'SU',139,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN139 = 33

	SWITCH = 5
	XCALL FILES (34,'I',169,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN169 = 34

	XCALL IO (CHN169,ARTCDE,1,READ,LOKCTL)
	BSEND = ORG169

	OPNOK = 1
	RETURN
;------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN139) CLOSE CHN139
	IF (CHN169) CLOSE CHN169
	RETURN
;------------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	W1_NAME = 'OECUS'
	XCALL W_PROC(WP_FIND, WND_1, W1_NAME)
	IF (.NOT. WND_1) XCALL W_PROC(WP_CREATE,WND_1,W1_NAME,15,45)
	XCALL W_BRDR(WND_1,WB_TITLE,'Cash Customer Information',
&		WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE, WND_1, 8, 8)
	XCALL W_DISP(WND_1, WD_CLEAR)

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
	RETURN
;-------------------------------------------------------------------
