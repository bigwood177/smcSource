;JOBHST.CP
;
;	job/shop order
;

	.INCLUDE 'WND:WINDOWS.DEF'

RECORD	JOBHDR
	.INCLUDE 'DEF:RD071A.DEF'
;
RECORD	JOBLIN
	.INCLUDE 'DEF:RD072A.DEF'

RECORD ITMMAS		;
	.INCLUDE 'DEF:RD041A.def'
RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	POP
	.INCLUDE 'DEF:POP1.DEF'

RECORD	FKEYS
	.INCLUDE 'DEF:FUNKEY.DEF'

RECORD CTL
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
	CHN041	,D2
	CHN182	,D2
	CHN071	,D2
	CHN072	,D2
	CHNTMP	,D2


record	jlkey2
	j2itm	,a15
	j2if1	,a3
	j2if2	,a3
	j2if3	,a5


RECORD	WVARS
	W_ID	,D4
	WND_1	,D4
	WND_2	,D4
	WN_TNMBR	,D4
	WN_NAME		,A6,	'IMFMNT'
	W2_NAME		,A6

RECORD	KEY_DSP
	KD1	,A3
		,A1
	KD2	,A3
		,A1
	KD3	,A5

RECORD	VARS
	achar	,a1
	savrfa	,a6
	tstat	,d1
	xdate	,d6
	i_date	,d8
	SELECT	,D1
	m_keyx	,a2
	BF		,D1
	DS		,D4
	DI		,D4
	I		,D6
	D_ITEM		,A15
	A12		,A12
	KEY_FOUND	,D1
	MM_CODE	,D5
	MM_DES1	,A12		;SHORT DESC F1
	MM_DES2	,A12		;SHORT DESC F2
	MM_DES3	,A12		;SHORT DESC F3
	F_KEY	,D3
	DECMAL	,D18
	KEY	,A15
	KITMNO	,A15
	OPNOK	,D1
	OPTION	,D1
	ENTRY	,A31
	INXCTL  ,D1
	CNGCTL	,D1
	MSGCTL	,D1
	WHATNO	,D2
	KEY1	,A20
	FULL	,D1
	OPENED	,D1
	SWITCH	,D1,	1
	SAVREC	,D5
	V	,D1
	READ	,D1,	0			;IO READ FLAG FOR I-O
	WRITE	,D1,	1			;IO WRITE
	STORE	,D1,	2
	DELETE	,D1,	3
	LOKCTL	,D1				;RECORD-LOCKED INDICATOR
;
PROC 
	XCALL TERID (V)
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	CALL INIT_WINDOW

DISPLA,
	ITMMAS =
	CNGCTL =
DISPL2,
	XCALL W_DISP (W_ID, WD_CLEAR)
	XCALL W_DISP (W_ID, WD_POS, 4,2,'ITEM #')
	XCALL W_DISP (W_ID, WD_POS, 5,2,'DESCR')

	XCALL W_DISP (W_ID, WD_POS, 7,2,'ON-HAND')
	XCALL W_DISP (W_ID, WD_POS, 8,2,'ALLOCATD')
	XCALL W_DISP (W_ID, WD_POS, 9,2,'ON-ORDER')
	XCALL W_DISP (W_ID, WD_POS,10,2,'ORD UPTO')
	XCALL W_DISP (W_ID, WD_POS,11,2,'REO LVL')

	XCALL W_DISP (W_ID, WD_POS, 7,48,'QTY SOLD MTD')
	XCALL W_DISP (W_ID, WD_POS, 8,48,'QTY SOLD YTD')
ITMNUM,
	CALL POP_KEY

	GO TO (DISPLA,ENDOFF), INXCTL

	XCALL W_DISP (W_ID, WD_POS, 4,15,KITMNO)
	CLEAR KEY_DSP
	KD1 = IF1
	KD2 = IF2
	KD3 = IF3
	XCALL W_DISP (W_ID, WD_POS, 4,32,KEY_DSP)
	xcall w_updt
	
	call get_memo_keys

	ITEMNO = KITMNO

	XCALL W_DISP (W_ID, WD_POS, 24,1,WD_CLR, WDC_LIN)
	CTL =

	CLEAR KEY_DSP
	KD1 = IF1
	KD2 = IF2
	KD3 = IF3
	XCALL W_DISP (W_ID, WD_POS, 4,15,ITEMNO)
	XCALL W_DISP (W_ID, WD_POS, 4,32,KEY_DSP)

	XCALL W_DISP (W_ID, WD_POS, 6,15,DESCR)
	IF (MM_DES3.NE.A12) 	XCALL W_DISP (W_ID, WD_POS, 6,46, MM_DES3)
	IF (MM_DES2.NE.A12) 	XCALL W_DISP (W_ID, WD_POS, 6,59, MM_DES2)

	ENTRY(1,11) = QTYONH,	'ZZ,ZZZ,ZZX-' [LEFT]
	XCALL W_DISP (W_ID, WD_POS, 7,15,ENTRY(1,11))

	ENTRY(1,11) = QTYCOM,	'ZZ,ZZZ,ZZX-' [LEFT]
	XCALL W_DISP (W_ID, WD_POS, 8,15, ENTRY(1,11))

	ENTRY(1,11) = QTYONO,	'ZZ,ZZZ,ZZX-' [LEFT]
	XCALL W_DISP (W_ID, WD_POS, 9,15, ENTRY(1,11) )

	ENTRY(1,11) = ORDUPT,	'ZZ,ZZZ,ZZX-' [LEFT]
	XCALL W_DISP (W_ID, WD_POS, 10,15, ENTRY(1,11) )

	ENTRY(1,11) = REOLVL,	'ZZ,ZZZ,ZZX-' [LEFT]
	XCALL W_DISP (W_ID, WD_POS, 11,15, ENTRY(1,11) )

	ENTRY(1,10) = QTYMTD,	'ZZ,ZZZ,ZZX' [LEFT]
	XCALL W_DISP (W_ID, WD_POS, 7,68, ENTRY(1,10) )

	ENTRY(1,10) = QTYYTD,	'ZZ,ZZZ,ZZX' [LEFT]
	XCALL W_DISP (W_ID, WD_POS, 8,68, ENTRY(1,10) )

	xcall w_updt

	call get_lines
	IF (I .EQ. 0) 
		BEGIN
		XCALL WNMSG(w_id, 24,'No Fab Tickets',1)
		GOTO DISPLA
		END

	NUMARA = I

	DO BEGIN
	  XCALL TTSTS (TSTAT)
	  IF (TSTAT) xcall w_disp(wnd_1,wd_accept,achar)
	END UNTIL (TSTAT.EQ.0)

	DLINE = "  TICKET ISSUED ON      QTY   REC'D ON     QTY"

	XCALL POP1 (POP)



;;;	XCALL WNMSG (W_ID, 23, 'OK',1)
	goto displa

;-------------------------------------------------------------

;==============================================================
;==================== INTERNAL ROUTINES =======================

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
;;;	XCALL WNMSG (WND_2, 6, 'MEMO NOT FOUND',1)
	KEY_FOUND = 0
	RETURN
;-------------------------------------------------------------

EXIT_KEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	LOKCTL = 0
	READ (CHN041, ITMMAS, ITMKEY, KEYNUM:1) [ERR=P_BAD_KEY]	;KRF = 2
	GOTO P_KEY_OK


P_BAD_KEY,

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
get_lines,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	for i from 1 thru maxara
		begin
		clear parry(i)
		clear parrfa(i)
		end

	i = 0

	clear jlkey2
	j2itm = itemno
	j2if1 = '999'

	find (chn072, joblin, jlkey2, keynum:1) [err=gd_loop]
gd_loop,
	reads (chn072, joblin, getrfa:savrfa) [err=gd_eof]
	if (jlitm .ne. j2itm) goto gd_eof
	if (jlsts .ne. 'R') goto gd_loop
	if (jlif1 .ne. if1) goto gd_loop
	if (jlif2 .ne. if2) goto gd_loop
	if (jlif3 .ne. if3) goto gd_loop
	
	if (jlnum .ne. jhnum) 
		begin
		xcall isio (chn071, jobhdr, jlnum, read, lokctl)
		if (lokctl .ne. 0) clear jobhdr
		end

	clear dline
	dline(1,6) = jhnum,	'ZZZZZX'

	;did not start recording date recv'd until 2-25-11
	i_date = jhrldt
	if (i_date .le. 0) i_date = jhshdt
	xdate(1,4) = i_date(5,8)
	xdate(5,6) = i_date(3,4)

	dline (9,16) = xdate,	'ZX/XX/XX'
	dline (19,25) = jlqty,	'ZZZ,ZZX'

	xdate(1,4) = jlrcdt(5,8)
	xdate(5,6) = jlrcdt(3,4)

	dline (29,36) = xdate,	'ZX/XX/XX'
	dline (38,44) = jlrcv,	'ZZZ,ZZX'

;TICKET ISSUED ON      QTY   REC'D ON     QTY
;ZZZZZX  ZX/XX/XX  ZZZ,ZZX   ZX/XX/XX ZZZ,ZZX
;1234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
	incr i
	parry(i) = dline
	parrfa(i) = savrfa
	if (i .lt. maxara) goto gd_loop

gd_eof,
	return
;--------------------------------------------------------------


;==============================================================


INPUT,
	XCALL WINPT (W_ID, ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL, F_KEY)
	RETURN
ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:JOBMNU',1)


GET_MEMO_KEYS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ALPDC (IF1, MM_CODE, BF)

	m_keyx = 'M1'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		MM_DES1 = MM_SHORT
		END
	ELSE	CLEAR MM_DES1

	XCALL ALPDC (IF2, MM_CODE, BF)
	m_keyx = 'M2'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		MM_DES2 = MM_SHORT
		END
	ELSE	CLEAR MM_DES2

	XCALL ALPDC (IF3, MM_CODE, BF)
	m_keyx = 'M3'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		MM_DES3 = MM_SHORT
		END
	ELSE	CLEAR MM_DES3

	RETURN
;-------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5

	XCALL FILES (1,'SI',41,SWITCH)		;FILE # 41 -- ITMMAS FILE
	IF (SWITCH.EQ.9) RETURN
	CHN041 = 1
	
	XCALL FILES (2,'SI',71,SWITCH)		;FILE # 71 -- JOBHDR
	IF (SWITCH .EQ. 9) RETURN
	CHN071 = 2

	XCALL FILES (3, 'SI', 72, SWITCH)	;FILE # 72 -- JOBLIN
	IF (SWITCH .EQ. 9) RETURN
	CHN072 = 3
	
	
NOTOPN,
	OPNOK = 1
	RETURN
;----------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN041) CLOSE CHN041
	IF (CHN071) CLOSE CHN071
	IF (CHN072) CLOSE CHN072

	RETURN
;----------------------------------------------------

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

;;;	XCALL W_PROC(WP_PLACE, WND_2, 4, 4)
;;;	XCALL W_DISP(WND_2, WD_CLEAR)

	MAXARA = 20
	PLEN = 47
	NUMROW = 7
	WX = 13
	WY = 
	POP_WID = 'fabtik'
	POP_TITLE = 'Fab Tickets'
		
	RETURN

END
