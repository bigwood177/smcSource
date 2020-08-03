;RECENT.COP	isam
;
;		AUTHOR: SHERWOOD S. QUIRING
;		DATE  : 23-MAY-1997
;
;		INVENTORY RECEIPTS ENTRY & EDIT
;
	.include 'wnd:windows.def'

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR

RECORD
	CHN042	,D2
	CHNOLD	,D2
;--------------------------------------------


RECORD	RECTRX
	.INCLUDE 'DEF:RD191A.def'

RECORD	DUMTRX
	.INCLUDE 'DEF:RD191B.def'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'


RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.def'

RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'


RECORD	PRINT
	TITLE	,A18
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PLINE	,A80
	PRNTON	,D1
	LINCNT	,D2,60
	PGCNT	,D6
	LPSW	,D2
	SPLFIL	,A14
	RPTNUM	,D3
	PRTTYP	,A1
	PRTCTL	,D3,080
	LPARG	,D1
	PRNTSW	,D1
	PRTCTR	,D1

RECORD	HD1
		,A*,	' SEQ # ITEM #                         OLD OH   RECVD  NEW OH       DATE   REF #'
;;;RECORD	HD1
;;;		,A*,	' SEQ # ITEM #                            QTY'
;;;		,A*,	'       DATE   REF #'


RECORD	CHANNEL
	CHN191	,D2
	CHN041	,D2
	CHN151	,D2
	CHN152	,D2
	CHN182	,D2

record	itm_err
	ie_item	,a15
	ie_f1	,a3
		,a1
	ie_f2	,a3
		,a1
	ie_f3	,a5
		,a1
		,a*,	'not found'


RECORD	WVARS
	W_ID	,D4
	WND_1	,D4
	WND_2	,D4
	WN_TNMBR	,D4
	WN_NAME		,A6,	'RECENT'
	W2_NAME		,A6

RECORD	KEY_DSP
	KD1	,A3
		,A1
	KD2	,A3
		,A1
	KD3	,A5

record	errmsg
	er_item	,a15
		,a*,	' not updated'

RECORD	VARS
	key	,a15
	litem	,a15
	itmrfa	,a6
	vanrfa	,a6
	ivflag	,d1
	vanflg	,d1

	MM_DES1		,A12
	MM_DES2		,A12
	MM_DES3		,A12
	A12		,A12
	KSEQ		,D6
	KITMNO		,A15
	IN_TABLE	,D1	;SSQ 9-10-03
	KEY_FOUND	,D1
	MM_CODE	,D5
	F_KEY	,D3

	OPNOK	,D1
	REM	,D8
	FILE_NAME	,A80
	FL	,D3	;LENGTH OF FILE_NAME
	XITEM	,A15
	SELECT	,D1
	OPT	,D1
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	LOKCTL	,D1
	WHATNO	,D2
	SRCCTL	,D1
	BSEND	,D5
	BSMID	,D5
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	TODAY	,D8
	TDATE	,D6
	STAT	,D1
	SWITCH	,D1
	V	,D1
;
PROC
	XCALL TERID (V)

	XCALL RDATE(TDATE)
	XCALL DATE8(TDATE, D_OUT, TODAY, D_FMT, D_SW)
RESTART,
	call init_window
	XCALL W_DISP (WND_1, WD_CLEAR)
	XCALL W_DISP (WND_1, WD_POS, 1,1,'INVENTORY RECEIPTS ENTRY')

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
MENU,
	XCALL W_DISP (WND_1, WD_CLEAR)
	XCALL W_DISP (WND_1, WD_POS, 1,1,'INVENTORY RECEIPTS ENTRY')
	XCALL W_DISP (WND_1, WD_POS, 4,20,'PLEASE SELECT APPLICATION')
	XCALL W_DISP (WND_1, WD_POS, 6,25,'1. RECEIPTS ENTRY')
	XCALL W_DISP (WND_1, WD_POS, 8,25,'2. PRINT RECEIPTS EDIT LIST')
	XCALL W_DISP (WND_1, WD_POS,10,25,'3. POST RECEIPTS')
	XCALL WINPT (W_ID, 4,47,01,00,'AE',ENTRY,INXCTL)
	GOTO (MENU,ENDOFF),INXCTL
	OPT = ENTRY(1,1)
	GOTO (DISPLA,EDIT_LIST,POST),OPT
	GOTO MENU

DISPLA,
	CLEAR CNGCTL, RECTRX
	XCALL W_DISP (WND_1, WD_CLEAR)
	XCALL W_DISP (WND_1, WD_POS, 1,1,'INVENTORY RECEIPTS ENTRY')
	XCALL W_DISP (WND_1, WD_POS, 4,5,'1. SEQ #')
	XCALL W_DISP (WND_1, WD_POS, 6,5,'2. ITEM #')
	XCALL W_DISP (WND_1, WD_POS, 8,5,'3. QTY')
	XCALL W_DISP (WND_1, WD_POS,10,5,'4. REF #')
R_SEQ,
	XCALL WINPT (W_ID, 4, 16, 06, 00, '#E', ENTRY, INXCTL)
	GOTO (DISPLA,MENU),INXCTL
	KSEQ = ENTRY(1,6)
	IF (KSEQ .EQ. 0) GOTO ADD

	XCALL ISIO (CHN191, RECTRX, KSEQ, READ, LOKCTL)
	IF (LOKCTL .EQ. 0) 
	THEN	GOTO CHANGE		;EXISTING REC
	ELSE	BEGIN
		XCALL WNMSG (W_ID, 24, 'SEQ # NOT FOUND',1)
		GOTO R_SEQ
		END

ADD,
	RH_KEY = '!!!!!!'

	LOKCTL = 1
	XCALL ISIO (CHN191, DUMTRX, RH_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		XCALL WNMSG (W_ID, 24, 'BAD HEADER RECORD',1)
		GOTO ENDOFF
		END
	
	INCR RH_NEXT
	LOKCTL = 1
	XCALL ISIO (CHN191, DUMTRX, RH_KEY, WRITE, LOKCTL)

	SELECT = 1	 		;ADD MODE
	CLEAR RECTRX
	KSEQ = RH_NEXT

	ENTRY(1,6) = KSEQ, 'ZZZZZX' [LEFT]
	XCALL W_DISP (W_ID, WD_POS, 4,16, ENTRY(1,6))
	XCALL W_UPDT

R_ITEM,
	CALL POP_KEY
	GOTO(DISPLA,MENU),INXCTL

	XCALL W_DISP (WND_1, WD_POS, 6,16, K_ITEM)

	CLEAR KEY_DSP
	KD1 = K_F1
	KD2 = K_F2
	KD3 = K_F3
	XCALL W_DISP (W_ID, WD_POS, 7,16,KEY_DSP)

	XCALL W_DISP (WND_1, WD_POS, 6,32,DESCR)
	IF (MM_DES3.NE.A12) XCALL W_DISP (WND_1, WD_POS, 7,43, MM_DES3)
	IF (MM_DES2.NE.A12) XCALL W_DISP (WND_1, WD_POS, 7,30, MM_DES2)
	IF (MM_DES1.NE.A12) XCALL W_DISP (WND_1, WD_POS, 6,63, MM_DES1)
	XCALL W_UPDT

R_QTY,
	XCALL WINPT (W_ID, 8,16,07,00,'#-',ENTRY,INXCTL)
	GOTO (DISPLA),INXCTL
	R_QTY = ENTRY(1,7)
	GOTO (ANYCNG),CNGCTL
R_PONO,
;;;	XCALL W_DISP (WND_1, WD_POS, 10,16,WD_CLR,WDC_EOL,' ')
	XCALL WINPT (W_ID, 10,16,06,00,'# ',ENTRY,INXCTL)
	GOTO (DISPLA),INXCTL
	R_PONO = ENTRY(1,6)
	GOTO ANYCNG

;----------------------------------------------------------------
CHANGE,
	SELECT = 2		;CHANGE MODE
	ENTRY(1,6) = KSEQ,	'ZZZZZX' [LEFT]
	XCALL W_DISP (WND_1, WD_POS, 4,16, ENTRY(1,6))

	XCALL W_DISP (WND_1, WD_POS, 6,16,R_ITEM)

	CLEAR KEY_DSP
	KD1 = R_F1
	KD2 = R_F2
	KD3 = R_F3
	XCALL W_DISP (W_ID, WD_POS, 7,16,KEY_DSP)

	XCALL W_DISP (WND_1, WD_POS, 6,32,DESCR)
	ENTRY(1,7) = R_QTY,	'ZZZZZX-' [LEFT]
	XCALL W_DISP (WND_1, WD_POS, 8,16,ENTRY(1,7))
	ENTRY(1,6) = R_PONO,	'ZZZZZX' [LEFT]
	XCALL W_DISP (WND_1, WD_POS, 10,16,ENTRY(1,6))
	GOTO ANYCNG

ANYCNG,
	XCALL WANCN (WND_1, 23, CNGCTL, WHATNO)
	GOTO (PROCES,CNGBR),CNGCTL+1
CNGBR,
	GOTO (ANYCNG,R_ITEM,R_QTY,R_PONO),WHATNO
	GOTO ANYCNG
PROCES,
	CASE SELECT OF
	BEGINCASE
	1:	BEGIN		;ADD MODE
		R_SEQ = KSEQ
		R_ITEM = K_ITEM
		R_MAT =
		R_F1 = K_F1
		R_F2 = K_F2
		R_F3 = K_F3
		R_DATE = TODAY
		XCALL ISIO (CHN191,RECTRX,R_SEQ,STORE,LOKCTL)
		IF (LOKCTL .NE. 0) XCALL WNMSG (WND_1, 24,'ERROR STORING RECORD',1)
		END
	2:	BEGIN
		XCALL ISIO (CHN191,RECTRX,R_SEQ,WRITE,LOKCTL)
		IF (LOKCTL .NE. 0) XCALL WNMSG (WND_1, 24, 'ERROR WRITING RECORD',1)
		END
	ENDCASE

	goto displa
	GOTO MENU

ENDOFF,
	CALL CLOSE
;;;	XCALL PGCHN ('ITM:ITM',1)	;7-10-07: temperary
	XCALL PGCHN ('CP:IMMENU',1)
	STOP
;===========================================================================
POP_KEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Find by item # & f_keys
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR MM_DES1, MM_DES2, MM_DES3
	XCALL W_PROC(WP_PLACE, WND_2, 6, 4)
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

	CLEAR TBL_KEY
	TBLCOD = 'M1'

	MM_CODE = ENTRY(1,3)
	K_F1 = MM_CODE,	'XXX'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		XCALL W_DISP (WND_2, WD_POS, 2,20, MM_SHORT)
		MM_DES1 = MM_SHORT
		END
	ELSE	MM_DES1 =

	GOTO (P_ANY),CNGCTL
P_F2,
	XCALL WINPT (WND_2,3,13, 3, 00, '# ', ENTRY, INXCTL)
	GOTO (P_DISPLA),INXCTL

	CLEAR TBL_KEY
	TBLCOD = 'M2'

	MM_CODE = ENTRY(1,3)
	K_F2 = MM_CODE,	'XXX'
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

	CLEAR TBL_KEY
	TBLCOD = 'M3'

	MM_CODE = ENTRY(1,5)
	K_F3 = MM_CODE,	'XXXXX'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		XCALL W_DISP (WND_2, WD_POS, 4,20, MM_SHORT)
		MM_DES3 = MM_SHORT
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
	XCALL W_DISP (W_ID, WD_POS, 4,15, KITMNO)
	KITMNO = K_ITEM
	CLEAR K_MAT
	XCALL W_PROC (WP_REMOVE, WND_2)
EXIT_KEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	LOKCTL = 0
	READ (CHN041, ITMMAS, ITMKEY, KEYNUM:1) [ERR=P_BAD_KEY]	;KRF = 2
	GOTO P_KEY_OK
P_BAD_KEY,
	CLEAR ITMMAS
	KITMNO = K_ITEM
	XCALL WNMSG (W_ID, 6, 'Item not found',1)
	GOTO P_ANY
P_KEY_OK,
	CLEAR INXCTL
	RETURN

P_ENDOFF,
	INXCTL = 2
	LOKCTL = 1			;ABORT
	XCALL W_PROC (WP_REMOVE, WND_2)
	RETURN
;------------------------------------------------------------

MEMO_KEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	CLEAR TBL_KEY
;;;	TBLCOD = 'MM'
	MM_KEY = MM_CODE
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_KEY]
	KEY_FOUND = 1		
	RETURN
NOT_KEY,
;;;	XCALL WNMSG (WND_2, 6, 'MEMO NOT FOUND',1)
	KEY_FOUND = 0
	RETURN
;-------------------------------------------------------------
;===========================================================================

;;;GET_ITEM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	XCALL SERCH (CHN042,ITMIDX,R_ITEM,1,15,BSEND,BSMID,SRCCTL,4,16,20,0,0,0,0)
;;;	IF (SRCCTL .OR. IRC041.LE.0) 
;;;	THEN	SRCCTL = 1
;;;	ELSE	XCALL IO (CHN041,ITMMAS,IRC041,READ,LOKCTL)
;;;	RETURN
;------------------------------------------------------

EDIT_LIST,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TITLE = 'RECEIPTS EDIT LIST'
	LPSW = 1		;PRINT,SPOOL, OR DISPLAY

	xcall u_finish

	CALL PRINT_TRX

	CALL CLOSE
	GOTO RESTART
;;;	GOTO MENU
;------------------------------------------------------
POST,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TITLE = 'RECEIPTS REGISTER'
	LPSW = 4		;MAY BE AUTO-SPOOLED

	xcall u_finish

	CALL PRINT_TRX
	IF (PRNTON .EQ. 0) GOTO MENU

	CALL CLOSE

	CALL OPN_PROTECT
	IF (.NOT. OPNOK) 
		BEGIN
		XCALL MESAG ('RECTRX FILE IN USE, RECEIPTS NOT POSTED !!',1)
		CALL CLOSE
		GOTO RESTART
		END

;;;	FIND (CHN191,RECTRX,^FIRST)[ERR=EOF_POST]
	read (CHN191,RECTRX,^FIRST)[ERR=EOF_POST]	;header record

POST_LOOP,
	XCALL IOS (CHN191,RECTRX,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF_POST
;;; 7-24-08	IF (R_QTY .EQ. 0) GOTO POST_LOOP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; 9-23-09: use this routine...

;-----------------------------------------------------------------
	LITEM = R_ITEM


	USING LITEM SELECT
	('CR0' thru 'CR9'),	KEY = LITEM(1,4)
	('CM'),			KEY = LITEM(1,3)
	('EDS0' THRU 'EDS9'),	KEY = LITEM(1,9)
	('JEB0' THRU 'JEB9'),	KEY = LITEM(2,15)	;SKIP THE "J"
	('JEF0' THRU 'JEF9'),	KEY = LITEM(2,15)	;SKIP THE "J"
	('JEFI0' THRU 'JEFI9'),	KEY = LITEM(2,15)	;SKIP THE "J"
	('JJG'),		KEY = LITEM(2,15)	;SKIP THE "J"
	('JTG'),		KEY = LITEM(2,15)	;SKIP THE "J"
	(),			KEY = LITEM
	ENDUSING

	xcall gtitm (key, r_f1, r_f2, r_f3, itmrfa, ivflag, vanflg, vanrfa)

	read (chn041, itmmas, rfa:itmrfa) [err=i_err]
;-----------------------------------------------------------------
	
;;;	CLEAR ITMKEY
;;;	K_ITEM = R_ITEM
;;;	K_MAT = R_MAT
;;;	K_F1 = R_F1
;;;	K_F2 = R_F2
;;;	K_F3 = R_F3
	
;;;	READ (CHN041, ITMMAS, ITMKEY, KEYNUM:1) [ERR=POST_LOOP]	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	QTYONH = QTYONH + R_QTY
	QTYONO = QTYONO - R_ORD		;4-17-08
;;; 2-7-08	QTYONO = QTYONO - R_QTY
	IF (QTYONO.LT.0) QTYONO = 0
	XCALL ISIO (CHN041,ITMMAS,ITEMNO,WRITE,LOKCTL)

	GOTO POST_LOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
i_err,
	ie_item = r_item
	ie_f1 = r_f1
	ie_f2 = r_f2
	ie_f3 = r_f3
	xcall mesag (itm_err,1)
	goto post_loop



EOF_POST,
;; clear rectrx file...
	READ (CHN191, DUMTRX, ^FIRST) 	;SKIP HEADER
EP_LOOP,
	XCALL IOS (CHN191, RECTRX, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EP_EOF
	DELETE (CHN191)
	GOTO EP_LOOP
EP_EOF,

	CALL CLOSE
	GOTO RESTART
;------------------------------------------------------
PRINT_TRX,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR PRNTON, PGCNT
	LINCNT = 66
	FIND (CHN191,RECTRX,^FIRST)[ERR=EOF_PRINT]
	XCALL IOS (CHN191,RECTRX,READ,LOKCTL)	;header
	
	SPLFIL (5,6) = 'EF'
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) RETURN
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	PRNTON = 1
	
PLOOP,
	XCALL IOS (CHN191,RECTRX,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF_PRINT
;;;7-24-08	IF (R_QTY .EQ. 0) GOTO PLOOP
	call get_item

	PLINE (38,44) = QTYONH,	'ZZZ,ZZX'
	PLINE (54,60) = (QTYONH+R_QTY), 'ZZZ,ZZX'
pl_err,
	PLINE (1,6) = R_SEQ,	'ZZZZZX'
	PLINE (8,22) = R_ITEM
	PLINE (24,26) = R_F1
	PLINE (28,30) = R_F2
	PLINE (32,36) = R_F3
	PLINE (46,52) = R_QTY,	'ZZZ,ZZX-'
	XCALL DATE8(R_DATE, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE (62,71) = D_FMT
	PLINE (74,79) = R_PONO,	'ZZZZZX'
	CALL PRINT
	GOTO PLOOP
; SEQ # ITEM #                         OLD OH   RECVD  NEW OH       DATE   REF #
;ZZZZZX AAAAAAAAAAAAAAA AAA AAA AAAAA ZZZ,ZZX ZZZ,ZZX ZZZ,ZZX XX-XX-XXxx  ZZZZZZ
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7

EOF_PRINT,
	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
	RETURN
;------------------------------------------------------
get_item,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; added 10-10-18

	LITEM = R_ITEM

	USING LITEM SELECT
	('CR0' thru 'CR9'),	KEY = LITEM(1,4)
	('CM'),			KEY = LITEM(1,3)
	('EDS0' THRU 'EDS9'),	KEY = LITEM(1,9)
	('JEB0' THRU 'JEB9'),	KEY = LITEM(2,15)	;SKIP THE "J"
	('JEF0' THRU 'JEF9'),	KEY = LITEM(2,15)	;SKIP THE "J"
	('JEFI0' THRU 'JEFI9'),	KEY = LITEM(2,15)	;SKIP THE "J"
	('JJG'),		KEY = LITEM(2,15)	;SKIP THE "J"
	('JTG'),		KEY = LITEM(2,15)	;SKIP THE "J"
	(),			KEY = LITEM
	ENDUSING
	xcall gtitm (key, r_f1, r_f2, r_f3, itmrfa, ivflag, vanflg, vanrfa)
	clear itmmas
	read (chn041, itmmas, rfa:itmrfa) [err=PL_err]
	return
;-------------------------------------------------------------------------


PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD,HD
&		,LG,LG,LG,0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;------------------------------------------------------

GET_PO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR REM
	RETURN
	
;;;	CLEAR REM, POLKEY
;;;	LPONUM = R_PONO
;;;	FIND(CHN152,PORLIN,POLKEY) [ERR=GP_LOOP]
;;;GP_LOOP,
;;;	LOKCTL = 1
;;;	XCALL IOS (CHN152,PORLIN,READ,LOKCTL)
;;;	IF (LOKCTL .NE. 0) RETURN
;;;	IF (LPONUM .NE. R_PONO) RETURN
;;;	IF (LITMNO .NE. R_ITEM) GOTO GP_LOOP
;;;	LQTYRC = R_QTY
;;;	REM = LQTYOR ;;;>- LQTYRC
;;;	LINSTS = 'C'
;;;	XCALL ISIO (CHN152,PORLIN,POLKEY,WRITE,LOKCTL)	
;;;	RETURN
;------------------------------------------------------


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1,'SU',191,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN191 = 1

	XCALL FILES (2,'SI',041,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 2


	SWITCH = 5
	XCALL FILES (8, 'SI', 182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 8


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	SWITCH = 5
;;;	XCALL FILES (21,'I',041,SWITCH)	;ITMMAS.SMC
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHNOLD = 21
;;;
;;;	SWITCH = 5
;;;	XCALL FILES (22,'I',042,SWITCH)	;ITMIDX
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN042 = 22
;;;
;;;	LOKCTL = 1
;;;	XCALL IO (CHNOLD, OLDMAS, 1, READ, LOKCTL)
;;;	BSEND = ORG041
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	OPNOK = 1
	RETURN
;------------------------------------------------------

OPN_PROTECT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1,'SU',191,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN191 = 1

	SWITCH = 1
	XCALL FILES (2,'SU',041,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 2


	SWITCH = 5
	XCALL FILES (21,'U',041,SWITCH)	;OLD ITMMAS
	CHNOLD = 21

	SWITCH = 5
	XCALL FILES (22,'I',042,SWITCH)	;ITMIDX
	CHN042 = 22

	OPNOK = 1
	RETURN
;------------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN191) XCALL FILES (CHN191,'SU',191,4)
	IF (CHN041) XCALL FILES (CHN041,'SI',041,4)
	IF (CHN182) CLOSE CHN182
	IF (CHNOLD) CLOSE CHNOLD
	IF (CHN042) CLOSE CHN042

	RETURN
;------------------------------------------------------

INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL U_START("LIB:SMC_WND",,,,,,,100)
	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,24,78)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'ORDER MAINTENANCE',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL W_UPDT
	W_ID = WND_1
	xcall u_logwnd(wnd_1)


	W2_NAME = 'FKEY'
	XCALL W_PROC(WP_FIND, WND_2, W2_NAME)
	IF (.NOT. WND_2) XCALL W_PROC(WP_CREATE,WND_2,W2_NAME,6,41)
	XCALL W_BRDR(WND_2,WB_TITLE,'Item Keys',
&		WB_TPOS,WBT_TOP,WBT_CENTER)
		
	RETURN

END

; SEQ # ITEM #                            QTY     DATE   REF #
;ZZZZZX AAAAAAAAAAAAAAA AAA AAA AAAAA ZZZ,ZZX XX-XX-XX  ZZZZZZ
;1234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
