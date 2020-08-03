;Harris.cp
;

	.DEFINE POOLSIZE	,25000
	.DEFINE WNDCHNL		,15
	.DEFINE MAXWINS		,10

	.INCLUDE 'WND:WINDOWS.DEF'

RECORD	WORK
	HDOCNO	,D6
	HCUSNO	,D6
	HDOCDT	,D8
	HAAMT	,D8
	HTAX	,D8
	HJOB	,A10
	HNAME	,A10

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

;
RECORD	POP
	.INCLUDE 'DEF:POP800.DEF'

RECORD	LPOP
	.INCLUDE 'DEF:LPOP.DEF'

RECORD	FKEYS
	.INCLUDE 'DEF:FUNKEY.DEF'

RECORD	CHANNELS
	CHN044	,D2
	CHNWRK	,D2

RECORD	W2_LINE
	W2_DATE		,A8
			,A1
	W2_DOC		,A6
			,A1
	W2_JOB		,A10
			,A1
	W2_AMT		,A12
			,A1
	W2_EXT		,A12


RECORD	WRKLIN
	ARO	,A1
	REST	,A53

;    DATE INVOIC      JOB     AMOUNT
;XX/XX/XX ZZZZZX AAAAAAAA ZZZ,ZZX.XX
;1234567890123456789012345678901234567890
;         1         2         3         4
RECORD	PVARS
	IVFLAG	,D1
	LI		,D2
	VAL		,D1
	SAVE_ROW	,D2
	FIRST_ROW	,D2
	LAST_ROW	,D2
	WROW		,D2
	FROW		,D2	;FOR <F5> MEMOS
	WLEN		,D2
	TITLEN		,D2
	STAT_KEY	,D2
	STAT		,D2
	DEC		,D18


RECORD	PRINT
	SPLFIL	,A14,	'SPL:HARRIS.SPL'
	TITLE	,A*,	'HARRIS REMITTANCE'
	NOHDR	,A*,	'NO HDR'
	LEG	,A*,	'NO LEGEND'
	LINCNT	,D2,60
	LNFEED	,D1
	LPSW	,D1
	PGCNT	,D3
	PLINE	,A80
	PRTCTL	,D3,	080
	PRTTYP	,A1
	RPTNUM	,D3

RECORD	HD1
	,A*,	'ORDER#  CUSTOMER      SALE AMT       TAX        TOTAL'


;ORDER#  CUSTOMER      SALE AMT       TAX        TOTAL
;ZZZZZX  AAAAAAAAAA  ZZZ,ZZX.XX- Z,ZZX.XX-  ZZZ,ZZX.XX-
;123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6


RECORD	CUSTOT
	CAMT	,D10
	CTAX	,D10
	SAVCUS	,D6
	TTAMT	,D10
	TTAX	,D10


RECORD	WVARS
	WND_1	,D4
	C_WND	,D4
	WN_NAME	,A6,	'HARRIS'

	W_ID	,D4

	W1_NAME	,A6
	WND_2	,D4
	W2_NAME	,A6
	WND_4	,D4	;CNG_LI
	W4_NAME	,A6

RECORD	TOTAL
	T_AAMT	,D10
	T_EXT	,D10


RECORD	VARS
	OPNOK	,D1
	K_SPEC1	,A30
	K_SPEC2	,A42
	WRKFIL	,A14,	'SPL:HARRIS.ISM'
	LINRFA	,A6
	CNT	,D6
	F5_POP_ON	,D1
	F_KEY	,D3
	XORD	,D6
	H_CUSNO	,D6
	KEY2	,A12
	SRCCTL	,D1
	BSEND	,D6
	BSMID	,D6
	ALPHA	,A10
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	MSGCTL	,D1
	READ	,D1,0
	LOKCTL	,D1
	XDATE	,D6
	I	,D6

	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID (v)
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
	CALL INIT_WINDOW

	CALL LOAD_LINE_ITEMS			;LOAD ANY EXISTING ORDERS...

DISPLA,
	XCALL W_DISP(WND_1,WD_CLEAR)
	CLEAR W2_LINE
	W2_AMT = T_AAMT,	'ZZZ,ZZX.XX'
	W2_EXT = (T_AAMT),	'ZZZ,ZZX.XX-'
	XCALL W_DISP(WND_1, WD_POS, 20,1, W2_LINE)

	XCALL W_DISP(WND_1, WD_POS, 4,4, 'ORDER#')
	XCALL W_DISP(WND_1, WD_POS, 4,22,'<F5> = EDIT')

	XCALL WINPT (WND_1, 4,12,06,00,'#E',ENTRY,INXCTL, F_KEY)
	GOTO (DISPLA,ENDOFF),INXCTL
	IF (F_KEY .EQ. F_05)
		BEGIN
		CALL F5_POP
		GOTO DISPLA
		END

	XORD = ENTRY(1,6)

	XCALL ISIO (CHNWRK, WORK, XORD, READ, LOKCTL)
	IF (LOKCTL .EQ. 0)
		BEGIN
		MSGCTL = 1
		XCALL WNMSG (WND_1, 23, 'Already entered', MSGCTL)
		GOTO DISPLA
		END
	XCALL ISIO (CHN044, ORDHDR, XORD, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO NOT_FOUND
	USING OCUSNO SELECT
	(33550, 34850,49828),	NOP
	(),			GOTO NOT_FOUND
	ENDUSING

	
	XDATE(1,4) = OINVDT(5,8)
	XDATE(5,6) = OINVDT(3,4)
	W2_DATE = XDATE,	'ZX/XX/XX'
	W2_DOC = OORDNO,	'ZZZZZX'

	W2_AMT = (OSALE + OMISC + OFRGHT+ OTAX(1)+OTAX(2)+OTAX(3)),		'ZZZ,ZZX.XX'
	W2_JOB = OPONO
	W2_EXT = (OSALE + OMISC + OFRGHT+ OTAX(1)+OTAX(2)+OTAX(3)),	'ZZZ,ZZX.XX-'
	XCALL W_DISP(WND_1, WD_POS, 4,20,W2_LINE)


ISLNOK,
	CNGCTL = 2
	XCALL WANCN (WND_1, 24, CNGCTL, WHATNO)
	GOTO (DISPLA), CNGCTL
	
LINEOK,	
	XCALL W_AREA(WND_2, WA_SCROLL, WAS_UP, 1)

	XCALL W_DISP(WND_2, WD_POS, 14, 1, W2_LINE)
	XCALL W_UPDT

	HDOCNO = OORDNO
	HCUSNO = OCUSNO
	HNAME = OCUSNM
	HDOCDT = OINVDT
	HAAMT = (OSALE + OMISC + OFRGHT)
	HTAX = OTAX(1)+OTAX(2)+OTAX(3)
	HJOB = OPONO

	;STORE TO FILE OR ADD TO ARRAY...
	STORE (CHNWRK, WORK, HDOCNO) ;[ERR=BAD_REC]
	T_AAMT = T_AAMT + HAAMT + HTAX


	GOTO DISPLA

NOT_FOUND,

	MSGCTL = 1
	XCALL WNMSG (WND_1, 23, 'Not Found', MSGCTL)
 	GO TO DISPLA

BAD_REC,
	XCALL WNMSG (WND_1, 23, 'Error Storing Record',MSGCTL)
	GOTO DISPLA


F5_POP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Inquire mode, hi-lite memos
	;;; associated w/ each line...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	F5_POP_ON = 1		;DON'T ALLOW MULTIPLE ENTRY INTO F5_POP

	CALL GET_LINES

	SAVE_ROW = 1
	LI = 1
;wham
RE_PAGE,
	IF (NUMARA .EQ. 0) GOTO F5_RETURN
	LAST_ROW = NUMROW
	IF (NUMROW .GT. NUMARA) LAST_ROW = NUMARA

	XCALL W_PROC(WP_REMOVE,WND_2)
	XCALL W_PROC(WP_DELETE,WND_2)
	XCALL W_PROC(WP_CREATE, WND_2, 'SCROL',LAST_ROW, 54)
	XCALL W_BRDR(WND_2,WB_TITLE,'Review Lines',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE, WND_2, 8, 0)
	XCALL W_DISP(WND_2, WD_CLEAR)
	WLEN = 54

	CLEAR ARO
	CLEAR WROW
	FOR I FROM 1 THRU LAST_ROW
		BEGIN
		INCR WROW
		DLINE = PARRY(I)
		REST = DLINE
		XCALL W_DISP(WND_2, WD_POS, WROW, 1, WRKLIN(1,PLEN+1))
		END

	XCALL W_UPDT

	CLEAR VAL
	WROW = SAVE_ROW	
	CLEAR SAVE_ROW	
	IF (WROW .LE. 0) WROW = 1	;SSQ 12-16-99

	FIRST_ROW = 1
	CALL DSPLIN

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
			CALL CNG_LI		;CHANGE THIS LI
			IF (NUMARA .GT. 0) 
				BEGIN
				if (%ttsts)READS (15, ENTRY)
				GOTO RE_PAGE
				END
			END
	(DEL_KEY),	BEGIN
			CALL DEL_LI
			CALL GET_LINES
			GOTO RE_PAGE
			END
	ENDUSING
	
DONE_F5,
	XCALL W_UPDT
	CALL LOAD_LINE_ITEMS


	CLEAR WORK


F5_RETURN,
	F5_POP_ON = 0		;DON'T ALLOW MULTIPLE ENTRY INTO F5_POP

	RETURN
;;;control point here..


DSPLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; move arrow, highlight selected line
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ARO = '>'
	REST = PARRY(LI)
	XCALL W_DISP (WND_2,WD_ATTR,ATTR_SET+ATTR_RVRS)
	XCALL W_DISP (WND_2,WD_POS,WROW,1,WRKLIN(1,WLEN))
	XCALL W_DISP (WND_2,WD_POS,WROW,1)
	SAVE_ROW = WROW
;display any memos for current item...
;;;	READ (CHNWRK, WORK, RFA=PARRFA(LI))


	RETURN

CLEAR_LAST_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; CLEAR HI-LITE ON PREV LINE
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR ARO
	XCALL W_DISP (WND_2,WD_ATTR,ATTR_CLR+ATTR_RVRS)
	XCALL W_DISP (WND_2,WD_POS,SAVE_ROW,1,WRKLIN(1,WLEN))
	RETURN
;----------------------------------------------------------------------

ENDOFF,

	XCALL W_PROC(WP_REMOVE, WND_1)
	XCALL W_PROC(WP_DELETE,WND_1)

	XCALL W_PROC(WP_REMOVE, WND_2)
	XCALL W_PROC(WP_DELETE,WND_2)
	XCALL W_UPDT


	XCALL OUTPT (1,1,2,'HARRIS REMITTANCE',1)
	XCALL OUTPT (12,4,0,'PRINT REPORT <Y>?',1)
	XCALL INPUT (12,24,01,00,'YY',ENTRY,INXCTL,1)
	IF (ENTRY(1,1) .EQ. 'Y') CALL PRINT_REPORT

	CALL CLOSE

	XCALL OUTPT (1,1,2,'HARRIS REMITTANCE',1)
	XCALL OUTPT (12,4,1,'CLEAR DATA?',1)
	XCALL INPUT (12,24,01,01,'YN',ENTRY,INXCTL,1)
	IF (ENTRY(1,1) .EQ. 'Y') XCALL DELET (WRKFIL)


	XCALL FLAGS (7000000)
	STOP
;=================================================================

PRINT_REPORT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL LPON (LPSW,SPLFIL)

	SAVCUS = -1
	FIND (CHNWRK, WORK, ^FIRST, KRF:1) [ERR=PR_LOOP]
PR_LOOP,
	XCALL IOS (CHNWRK, WORK, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO PR_EOF
	IF (HCUSNO .NE. SAVCUS) CALL NEWCUS

	PLINE(1,6) = HDOCNO,	'ZZZZZX'
	PLINE(9,18) = HNAME
	PLINE(21,31) = HAAMT,	'ZZZ,ZZX.XX-'
	PLINE(33,41) = HTAX,	'Z,ZZX.XX-'
	PLINE(44,54) = (HAAMT+HTAX),	'ZZZ,ZZX.XX-'
	CALL PRINT
	
	CAMT = CAMT + HAAMT
	CTAX = CTAX + HTAX
	GOTO PR_LOOP

PR_EOF,
	CALL NEWCUS

	PLINE(21,31) = '==========='
	PLINE(33,41) = '==========='
	PLINE(44,54) = '==========='
	CALL PRINT

	PLINE(21,31) = TTAMT,	'ZZZ,ZZX.XX-'
	PLINE(33,41) = TTAX,	'Z,ZZX.XX-'
	PLINE(44,54) = (TTAMT+TTAX),	'ZZZ,ZZX.XX-'
	CALL PRINT

	XCALL LPOFF (LPSW,SPLFIL,PGCNT)

	RETURN
;----------------------------------------------------------------

NEWCUS,
	IF (SAVCUS .EQ. -1) GOTO ENDCUS
	PLINE(21,31) = '___________'
	PLINE(33,41) = '__________'
	PLINE(44,54) = '___________'
	CALL PRINT

	PLINE(21,31) = CAMT,	'ZZZ,ZZX.XX-'
	PLINE(33,41) = CTAX,	'Z,ZZX.XX-'
	PLINE(44,54) = (CAMT+CTAX),	'ZZZ,ZZX.XX-'
	CALL PRINT
	CALL PRINT

	TTAMT = TTAMT + CAMT
	TTAX = TTAX + CTAX

ENDCUS,
	SAVCUS = HCUSNO
	CLEAR CAMT, CTAX
	RETURN
;------------------------------------------------------------------

PRINT,
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,NOHDR,NOHDR,
&		LEG,' ',' ',0,80,PRTCTL,0,LPSW,RPTNUM,PRTTYP)

	RETURN
;-----------------------------------------------------------------

CLEAR_DATA,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	RETURN
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
;----------------------------------------------------------------------

;;; WINDOW SCROLLING ROUTINES
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
;--------------------------
OL_PAGE_DN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PAGE FORWARD NUMARAL RECORDS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR SAVE_ROW
	WROW = 1
	LI = 1
	FIND (CHNWRK, WORK, RFA=PARRFA(NUMARA)) [ERR=GL_INIT]
	GOTO GL_INIT

OL_PAGE_UP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PAGE BACKWARDS NUMARAL RECORDS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR SAVE_ROW
	WROW = 1
	LI = 1
; read reverse numaral records...
	FIND (CHNWRK, WORK, RFA=PARRFA(1)) [ERR=GL_INIT]
	FOR I FROM 1 THRU MAXARA
		BEGIN
		READS (CHNWRK, WORK, GL_INIT, REVERSE) [ERR=GL_INIT]
		END
	GOTO GL_INIT
;------------------------------------------------------------------

GET_LINES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; READ NUMARAL WORK RECORDS INTO ARRAY
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR I
	FIND (CHNWRK, WORK, ^FIRST) [ERR=GL_INIT, EOF=RL_EOF]

GL_INIT,
	FOR I FROM 1 THRU MAXARA CLEAR PARRY(I), PARRFA(I), PFLAGS(I)
	CLEAR I

READ_LINES,
	XCALL IOS (CHNWRK, WORK, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO RL_EOF
	INCR I
	XCALL GETRFA(CHNWRK, LINRFA)
	PARRFA(I) = LINRFA
	CALL MAKE_W2
	PARRY(I) = W2_LINE

	IF (I .LT. MAXARA) GOTO READ_LINES	
RL_EOF,
	UNLOCK CHNWRK
	NUMARA = I
	RETURN
;------------------------------------------------------------

LOAD_LINE_ITEMS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL W_PROC(WP_REMOVE,WND_2)
	XCALL W_PROC(WP_DELETE,WND_2)
	CALL CREATE_WND2

	FIND (CHNWRK, WORK, ^FIRST) [ERR=LLI_EOF]
LLI_LOOP,
	XCALL IOS (CHNWRK, WORK, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO LLI_EOF

	CALL MAKE_W2
	XCALL W_AREA(WND_2, WA_SCROLL, WAS_UP, 1)
	XCALL W_DISP(WND_2, WD_POS, 14, 1, W2_LINE)
	XCALL W_UPDT
	T_AAMT = T_AAMT + HAAMT + HTAX
	GOTO LLI_LOOP

LLI_EOF,
	RETURN
;------------------------------------------------------------

CNG_LI,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	RETURN
;-----------------------------------------------------------

DEL_LI,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	READ (CHNWRK, WORK, RFA=PARRFA(LI)) [ERR=GL_INIT]
	DELETE (CHNWRK)
	CALL LOAD_LINE_ITEMS

	RETURN
;-----------------------------------------------------------
MAKE_W2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XDATE(1,4) = HDOCDT(5,8)
	XDATE(5,6) = HDOCDT(3,4)
	W2_DATE = XDATE,	'ZX/XX/XX'
	W2_DOC = HDOCNO,	'ZZZZZX'
	W2_AMT = (HAAMT+HTAX),		'ZZZ,ZZX.XX'
	W2_JOB = HJOB
	W2_EXT = (HAAMT+HTAX),	'ZZZ,ZZX.XX-'

	RETURN
;-----------------------------------------------------------

;===========================================================

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK
	SWITCH = 5
	XCALL FILES (4,'SI',184,SWITCH)	;184 - SLHHDR
	IF (SWITCH .EQ. 9) RETURN

	CHN044 = 4

	ONERROR NO_FILE
	OPEN (5,SU,WRKFIL)
	OFFERROR

	GOTO FILE_OK

NO_FILE,
	OFFERROR
	;CREATE SPL:HARRIS.ISM

	K_SPEC1 = 'start=1, len=6, nodups, ascend'
;                  12345=789012345678901234567890

	K_SPEC2 = 'start=7:1, len=6:6, nodups, ascend'
;                  123456789012345678901234567890123456789012
	XCALL ISAMC (WRKFIL, 56, 2, K_SPEC1, K_SPEC2)

;;;	XCALL ISAMC (WRKFIL, 56, 1,'START=1, LENGTH=6 NODUPS, ASCEND')
	OPEN (5,SU,WRKFIL)
FILE_OK,
	CHNWRK = 5
	
	OPNOK = 1
	RETURN
;-----------------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN044
	CLOSE CHNWRK
;;;	XCALL DELET (WRKFIL)
	RETURN
;-----------------------------------------------------------


;===========================================================

INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,24,78)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'HARRIS REMITTANCE',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL W_UPDT
	W_ID = WND_1


	W2_NAME = 'SCROL'
	W4_NAME = 'CNGLI'


	XCALL W_INIT(POOLSIZE,WNDCHNL,MAXWINS)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'HARRIS INVOICES',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1

	CALL CREATE_WND2


;CNGLI:
	MAXARA = 10
	PLEN = 52
	NUMROW = 10
	WXL = 8
	WYL = 1
	POP_WIDL(1,5) = "F4WIN"
	POP_TITLEL = "Select/Unselect Memos"

	RETURN

;---------------------------------------
CREATE_WND2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL W_PROC(WP_CREATE, WND_2, 'SCROL',10, 54)
	XCALL W_BRDR(WND_2,WB_TITLE,'INVOICES',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE, WND_2, 8, 0)
	XCALL W_DISP(WND_2, WD_CLEAR)
	WLEN = 52
	RETURN
;--------------------------------------------------------

