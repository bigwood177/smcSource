SUBROUTINE MSHP
	CUST		,D
	SHIPN		,D4	;SHIPTO NUMBER
	MODE		,D	;1=ADD, 2=CHANGE, 3=DELETE
	TAXFLG		,A	;CUSMAS.TAXFLG
	NAME		,A	;CUSMAS.NAME
	CASH_CUST	,D	;1= CASH CUSTOMER

.INCLUDE 'WND:WINDOWS.DEF'

RECORD	SHIPTO
	.INCLUDE 'DEF:RD171A.DEF'

RECORD	ARTCDE
	.INCLUDE 'DEF:RD169A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD169B.DEF'

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

RECORD	ATAX
		,A1,	'('
	DTAX	,A3
		,A1,	')'

RECORD	CHANNEL
	CHN169	,D2,	33
	CHN171	,D2,	30

RECORD	VARS
	routine	,a32
	A30	,A30
	OPNOK	,D1
	W_ID	,D4
	T_CODE	,A3
	ENTRY	,A30
	INXCTL	,D1
	WHATNO	,D2
	CNGCTL	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	SWITCH	,D1

PROC
	CALL INIT_WINDOW
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	xcall modname(1, routine)		;calling routine

RE_START,
	CLEAR SHIPTO

	USING MODE SELECT
	(1),	BEGIN
		CLEAR SHIPTO
		SHCSNO = CUST 
		GOTO DISPLA
		END
	(2),	BEGIN
		SHCSNO = CUST 
		SHTONO = SHIPN 
		READ (CHN171,SHIPTO,SHTKEY) [ERR=ENDOFF]
		CALL PAINT
		CALL DSP_SHIPTO
		GOTO ANYCNG
		END
	ENDUSING

	GOTO ENDOFF

DISPLA,
	CLEAR CNGCTL
	CALL PAINT

SHTONO,
	IF (MODE .NE. 1) GOTO ANYCNG
	CTL = '01,16,04,00,#E'
	CALL INPUT
	GOTO(DISPLA,ENDOFF),INXCTL
	SHTONO = ENTRY(1,4)
	SHCSNO = CUST 
	LOKCTL = 1
	XCALL ISIO (CHN171,SHIPTO,SHTKEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)	;ALREADY EXISTS
		BEGIN
		XCALL WNMSG(W_ID,13,'SHIP-TO ALREADY ON FILE',1)
		GOTO RE_START
		END

	CLEAR SHIPTO
	SHCSNO = CUST
	SHTONO = ENTRY(1,4)
	SHIPN = SHTONO
	GOTO (ANYCNG), CNGCTL
	SHTONA = NAME				;9-9-03 SSQ
	XCALL W_DISP(W_ID,WD_POS,2,16,SHTONA)	;9-9-03 SSQ
	XCALL W_UPDT				;9-9-03 SSQ
	GOTO SHTAD1				;9-9-03 SSQ
SHTONA,
	CTL = '02,16,30,00,A '
	CALL INPUT
	GOTO(DISPLA),INXCTL
	SHTONA = ENTRY(1,30)
	GOTO (ANYCNG), CNGCTL
SHTAD1,
	CTL = '03,16,30,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	SHTAD1 = ENTRY(1,30)
	GOTO (ANYCNG), CNGCTL
SHTAD2,
	CTL = '04,16,30,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	SHTAD2 = ENTRY(1,30)
	GOTO (ANYCNG), CNGCTL
SHTCTY,
	CTL = '05,16,16,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	SHTCTY = ENTRY(1,16)
	GOTO (ANYCNG), CNGCTL
SHTST,
	CTL = '06,16,02,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	SHTST = ENTRY(1,2)
	IF (SHTST .EQ. '  ') GOTO SHTST
	T_CODE = SHTST
	CALL CHK_TAXCDE
	GOTO(SHTST),SRCCTL
	GOTO (ANYCNG), CNGCTL
SHTZIP,
	CTL = '07,16,10,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	SHTZIP = ENTRY(1,10)
	GOTO (ANYCNG), CNGCTL
SHTOTC,
	CTL = '08,16,03,00,A '
	XCALL W_DISP(W_ID,WD_POS,ROW,COL+5,A30)
	DTAX = TAXFLG
	XCALL W_DISP(W_ID,WD_POS,ROW,COL+5,ATAX)
	XCALL W_UPDT
	CALL INPUT
	GOTO (DISPLA), INXCTL
	SHTOTC = ENTRY(1,3)
;;;	IF (SHTOTC .EQ. '   ') GOTO SHTOTC
	IF (SHTOTC .EQ. '   ') 
		BEGIN
		SHTOTC = TAXFLG
		XCALL W_DISP(W_ID,WD_POS,ROW,COL,SHTOTC)
		END
	XCALL W_UPDT

	T_CODE = SHTOTC
	CALL CHK_TAXCDE
	GOTO(SHTOTC),SRCCTL
	XCALL W_DISP(W_ID,WD_POS,ROW,COL+5,ARTDSC)
	GOTO (ANYCNG), CNGCTL
SHPHON,
	CTL = '09,16,10,00,# '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	SHPHON = ENTRY(1,10)
	ENTRY(1,12) = SHPHON,	'ZZZ XXX XXXX' [LEFT]
	XCALL W_DISP(W_ID,WD_POS,ROW,COL,ENTRY(1,12))
	GOTO (ANYCNG), CNGCTL
SHFAX,
	CTL = '10,16,10,00,# '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	SHFAX = ENTRY(1,10)
	ENTRY(1,12) = SHFAX,	'ZZZ XXX XXXX' [LEFT]
	XCALL W_DISP(W_ID,WD_POS,ROW,COL,ENTRY(1,12))
	GOTO (ANYCNG),CNGCTL

ANYCNG,
	XCALL WANCN(W_ID,12,CNGCTL,WHATNO)
	GOTO (PROCES,CNGBR),CNGCTL + 1
CNGBR,
	GOTO (SHTONO,SHTONA,SHTAD1,SHTAD2,SHTCTY,SHTST,SHTZIP,
&			SHTOTC,SHPHON,SHFAX),WHATNO
	GOTO ANYCNG

PROCES,
	upcase routine
	USING ROUTINE SELECT
	('SCRN1'),		SHPGM = 'A'
	('ORDCN'),		SHPGM = 'C'
	('MAIN$BLADIN'),	SHPGM = 'S'
	ENDUSING

	USING MODE SELECT
	(1),	begin
		shcngd = 1		;added
		xcall rdat8 (shodat)
		STORE(CHN171,SHIPTO,SHTKEY) ;;[ERR=ENDOFF]
		end
	(2),	begin
		shcngd = 2		;changed
		WRITE(CHN171,SHIPTO,SHTKEY) ;;[ERR=ENDOFF]
		end
	ENDUSING

ENDOFF,
	XCALL W_PROC(WP_DELETE, W_ID)
	CLOSE CHN171
	CLOSE CHN169
	XRETURN

DSP_SHIPTO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ENTRY(1,4) = SHTONO,	'ZZZZ' [LEFT]
	XCALL W_DISP(W_ID,WD_POS,1,16,ENTRY(1,4) )

	XCALL W_DISP(W_ID,WD_POS,2,16,SHTONA)
	XCALL W_DISP(W_ID,WD_POS,3,16,SHTAD1)
	XCALL W_DISP(W_ID,WD_POS,4,16,SHTAD2)
	XCALL W_DISP(W_ID,WD_POS,5,16,SHTCTY)
	XCALL W_DISP(W_ID,WD_POS,6,16,SHTST)
	XCALL W_DISP(W_ID,WD_POS,7,16,SHTZIP)
	XCALL W_DISP(W_ID,WD_POS,8,16,SHTOTC)

	T_CODE = SHTOTC
	CALL CHK_TAXCDE
	IF (SRCCTL.EQ.0) XCALL W_DISP(W_ID,WD_POS,8,21,ARTDSC)

	ENTRY(1,12) = SHPHON,	'ZZZ XXX XXXX'
	XCALL W_DISP(W_ID,WD_POS,9,16,ENTRY(1,12) )

	ENTRY(1,12) = SHFAX,	'ZZZ XXX XXXX'
	XCALL W_DISP(W_ID,WD_POS,10,16,ENTRY(1,12) )

	RETURN
;---------------------------------------------------------

PAINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL W_DISP(W_ID,WD_CLEAR)
	XCALL W_DISP(W_ID,WD_POS, 1,1,' 1. NO')
	XCALL W_DISP(W_ID,WD_POS, 2,1,' 2. NAME')
	XCALL W_DISP(W_ID,WD_POS, 3,1,' 3. ADDR-1')
	XCALL W_DISP(W_ID,WD_POS, 4,1,' 4. ADDR-2')
	XCALL W_DISP(W_ID,WD_POS, 5,1,' 5. CITY')
	XCALL W_DISP(W_ID,WD_POS, 6,1,' 6. STATE')
	XCALL W_DISP(W_ID,WD_POS, 7,1,' 7. ZIP')
	XCALL W_DISP(W_ID,WD_POS, 8,1,' 8. TAX CODE')
	XCALL W_DISP(W_ID,WD_POS, 9,1,' 9. PHONE #')
	XCALL W_DISP(W_ID,WD_POS,10,1,'10. FAX#')
	RETURN
;---------------------------------------------------------

CHK_TAXCDE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; VALIDATE TAX CODE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SRCCTL = 2
	XCALL SERCH (CHN169,ARTCDE,T_CODE,1,3,BSEND,BSMID,SRCCTL,4,4,9,0,0,0,0)
	IF (SRCCTL.NE.0) XCALL WNMSG(W_ID,14,'INVALID STATE/TAX CODE',1)
	RETURN
;----------------------------------------------------------

CHK_SHIPTO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; CHECK IF SHIPTO ALREADY EXISTS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	LOKCTL = 1
	XCALL ISIO (CHN171,SHIPTO,SHTKEY,READ,LOKCTL)
	
	RETURN
;----------------------------------------------------------

INPUT,
	XCALL WINPT(W_ID,ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL)
	RETURN
;----------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	IF (CASH_CUST)
	THEN	XCALL FILES (30,'SU',102,SWITCH)	;MISSHP
	ELSE	XCALL FILES (30,'SU',171,SWITCH)	;SHIPTO

	SWITCH = 5
	XCALL FILES (33,'I',169,SWITCH)
	LOKCTL = 1
	XCALL IO (CHN169,ARTCDE,1,READ,LOKCTL)
	BSEND = ORG169
	OPNOK = 1

	RETURN
;-----------------------------------------------

INIT_WINDOW,
	XCALL W_PROC(WP_FIND,W_ID,'MSHP')
	IF (.NOT. W_id) XCALL W_PROC(WP_CREATE,W_ID,'MSHP',13,50)
	XCALL W_BRDR(W_ID, WB_TITLE, 'SHIP-TO INFO',
&		 WB_TPOS, WBT_TOP, WBT_CENTER)
;;;	XCALL W_PROC(WP_PLACE,W_ID,5,5)
	XCALL W_PROC(WP_PLACE,W_ID,12,2)
	XCALL W_DISP(W_ID,WD_CLEAR)
	RETURN

