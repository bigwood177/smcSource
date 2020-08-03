;jobmnt.cp
;	job/shop order
;
; 05-dec-18 ssq: add to ordsch as type 'J'
; 28-oct-19 ssq: allocate component inventory via xcom.cp



	.INCLUDE 'WND:WINDOWS.DEF'
	.INCLUDE 'DEF:HPSUB.DEF'

RECORD	DPTSTS
	.INCLUDE 'DEF:RD193A.DEF'
	
RECORD	NEWORD
	.INCLUDE 'DEF:RD088A.DEF'

RECORD	COPCTL
	.INCLUDE 'DEF:RD060A.DEF'

RECORD	JOBHDR
	.INCLUDE 'DEF:RD071A.DEF'
;
RECORD	JOBLIN
	.INCLUDE 'DEF:RD072A.DEF'
;
RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.DEF'

RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'
;
RECORD	KEYS
	.INCLUDE 'DEF:FUNKEY.DEF'

RECORD	LPOP
	.INCLUDE 'DEF:LPOP.DEF'

RECORD	W2_LINE
	W2_ITEM		,A15
			,A1
	W2_F1		,A3
			,A1
	W2_F2		,A3
			,A1
	W2_F3		,A5
			,A1
	W2_DESCR	,A30
	W2_QTY		,A6
;
RECORD	WRKLIN
	ARO	,A1
;;;		,A1
	REST	,A80


RECORD	HDR1
		,A*,	'   Qty  Item No.        Description                            Actual'

RECORD	PRINT
	LINCNT	,D2,	60
	LPONSW	,D1
	LPARG	,D1
	RPTNUM	,D3
	PRTTYP	,A1
	PGCNT	,D6,	000000
	PLINE	,A132
	PRTCTL	,D3
	LPSW	,D2

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT

RECORD	RESET
	R_CHAR	,A1
		,A1,	'E'
RECORD	HP_V
	H_CHAR	,A1		;<ESC>
		,A4,	"&l8C"	;PAGE EJECT

RECORD	SPLFIL
		,A4,	'SPL:'
		,A1,	'S'
	SPORD	,A6
		,A4,	'.SPL'

RECORD	DEPT_LINE
		,A*,	'DEPARTMENT "'
	D_LET	,A3

RECORD	WVARS
	W_ID	,D4
	WND_1	,D4
	WND_K	,D4	;POP_KEY
	WND_2	,D4	;LINE ITEMS
	WN_NAME	,A6,	'JOBMNT'
	W2_NAME	,A6,	'KEYSXX'
	W3_NAME	,A6,	'SCROLL'
	WN_TNMBR	,D2

RECORD	CHANNEL
	CHN041	,D2
	CHN060	,D2
	CHN071	,D2
	CHN072	,D2
	CHN182	,D2
	CHN088	,D2
	CHN162	,D2
	CHN163	,D2
	CHN193	,D2

RECORD	VARS
	savdpt		,a2	;12-5-18
	cfg_item	,d1
	partno		,a15
	maxflen		,d3
	flag		,d1
	fl			,d3
	rfa			,a6
	st_guage	,d2
	b_data	,a100
	b_strng	,a100
	m_keyx	,a2
	OPNOK	,D1
	BOXQTY	,D6	;NUMBER OF BOXES
	BOLD_ON	,D1
	A40	,A40
	TL	,D3
	LL	,D3
	LROW	,D2
	CMPCOD	,A3
	NEW_ITEM	,D1	;1 = ADD MODE, 0 = CHANGE MODE
	I	,D6
	LINRFA	,A6
	LI	,D2

	WROW	,D2
	VAL	,D6
	SAVE_ROW	,D2
	FIRST_ROW	,D2
	LAST_ROW	,D2
	F5_POP_ON	,D1		;DON'T ALLOW MULTIPLE ENTRY INTO F5_POP
	F_KEY		,D3
	STAT_KEY	,D3
	STAT		,D1
	WLEN	,D2
	FIRST	,D1
	NXTJOB	,D6
	D6	,D6
	A6	,A6
	A10	,A10
	XDATE	,D8
	TODAY	,D8
	MM_CODE	,D5
	MM_DES1	,A12		;SHORT DESC F1
	MM_DES2	,A12		;SHORT DESC F2
	MM_DES3	,A12		;SHORT DESC F3
	KEY_FOUND	,D1
	H_SEL	,D1	;1=ADD, 2=CHANGE
	L_SEL	,D1	;1-ADD, 2=CHANGE
	MODE	,A14
	DECMAL	,D18
	DEC	,D18
	ALPHA	,A10
	KEY	,A15
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	TMPORD	,D6
	JOB	,D6
	QTY	,D6
	KITMNO	,A15
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	SWITCH	,D1
;
PROC
	open (15,i,'tt:')
	XCALL ASCII (27, E_CHAR)
	H_CHAR = E_CHAR
	R_CHAR = E_CHAR
	
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	XCALL RDAT8 (TODAY)

	CALL INIT_WINDOW
HEADER,
	FIRST = 1
	W_ID = WND_1
H_DISPLA,
	CLEAR CNGCTL
	XCALL W_DISP(W_ID, WD_CLEAR)
	XCALL W_DISP(W_ID, WD_POS, 2,01, '1. TICKET #')
	XCALL W_DISP(W_ID, WD_POS, 2,21, '2. DEPT')
	XCALL W_DISP(W_ID, WD_POS, 2,34, '3. DUE DATE')
	XCALL W_UPDT
JHNUM,
	XCALL WINPT (W_ID, 02, 13, 06, 00, '#E' ,ENTRY, INXCTL)
	GOTO (H_DISPLA, ENDOFF), INXCTL
	JOB = ENTRY(1,6)
	IF (JOB .EQ. 0)
	THEN	BEGIN
		CALL DEFLNX
		H_SEL = 1	;ADD MODE
		CLEAR JHNUM
		JHNUM = JOB
		END
	ELSE	BEGIN
		LOKCTL = 1
		XCALL ISIO (CHN071, JOBHDR, JOB, READ, LOKCTL)
		IF (LOKCTL .NE. 0) GOTO JHNUM
		H_SEL = 2
		CALL DSP_HDR
		GOTO H_ANY
		END

	GOTO (H_ANY),CNGCTL
	
JHDEPT,
	XCALL WINPT (W_ID, 02, 29, 02, 00, 'A ',ENTRY,INXCTL)
	GOTO (H_DISPLA),INXCTL
	JHDEPT = ENTRY(1,2)
	
	CLEAR TBL_KEY
	TBLCOD = 'DD'
	DD_DEPT = JHDEPT
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		XCALL OLMSG (W_ID, 24, 'INVALID DEPARTMENT',1)
		GOTO JHDEPT
		END
	
	GOTO (H_ANY),CNGCTL
JHSHDT,
	XCALL WINPT (W_ID, 02, 46, 08, 00, 'D ',ENTRY, INXCTL)
	GOTO (H_DISPLA), INXCTL
	JHSHDT = ENTRY(1,8)
	IF (JHSHDT .EQ. 0)
		BEGIN
		JHSHDT = TODAY
		XDATE(1,4) = JHSHDT(5,8)
		XDATE(5,8) = JHSHDT(1,4)
		A10 = XDATE,	'ZX/XX/XXXX'
		XCALL W_DISP (W_ID, WD_POS, 2, 46, A10)
		END
	GOTO (H_ANY),CNGCTL

H_ANY,
	XCALL WANCN (W_ID, 23, CNGCTL, WHATNO)
	GOTO (H_PROCES, H_CNGBR),CNGCTL+1
H_CNGBR,
	GOTO (JHNUM, JHDEPT, JHSHDT), WHATNO
	GOTO H_ANY
H_PROCES,
	USING H_SEL SELECT
	(1),	XCALL ISIO (CHN071, JOBHDR, JHNUM, STORE, LOKCTL)
	(2),	XCALL ISIO (CHN071, JOBHDR, JHNUM, WRITE, LOKCTL)
	ENDUSING

	XCALL ISIO (CHN071, JOBHDR, JHNUM, READ, LOKCTL)
;;;	UNLOCK CHN071

LINES,
	W_ID = WND_1
	CALL FIND_LINE_ITEMS

L_DISPLA,
	NEW_ITEM = 1
	CALL POP_KEY
	GOTO (L_DISPLA, END_LIN),INXCTL

	L_SEL = 1
	CLEAR JOBLIN
	
	JLNUM = JHNUM

	JLSEQ = JHSEQ
	JHSEQ = JHSEQ + 1

	JLITM = ITEMNO
	JLIF1 = IF1
	JLIF2 = IF2
	JLIF3 = IF3
	JLDES = DESCR
	JLBOX = IBXQTY	;QTY/BOX
	JLQTY = QTY

	CALL GET_WEIGHT						;9-12-18
	
	USING L_SEL SELECT
	(1),	BEGIN
		LOKCTL = 1
		XCALL ISIO (CHN072, JOBLIN, JLKEY, STORE, LOKCTL)
		END
	(2),	BEGIN
		LOKCTL = 1
		XCALL ISIO (CHN072, JOBLIN, JLKEY, WRITE, LOKCTL)
		END
	ENDUSING

;----------------------------------------------------
	XCALL W_AREA(WND_2, WA_SCROLL, WAS_UP, 1)
	CALL MAKE_W2
	XCALL W_DISP(WND_2, WD_POS, 14, 1, W2_LINE)
	XCALL W_UPDT
;----------------------------------------------------
	GOTO L_DISPLA
END_LIN,
;;;	write (chn071, jobhdr, jhnum)
	XCALL ISIO (CHN071, JOBHDR, JHNUM, WRITE, LOKCTL)
	XCALL W_PROC(WP_REMOVE,WND_2)

	XCALL W_DISP (WND_1, WD_POS, 23,1,WD_CLR, WDC_LIN,'PRINT TICKET? <Y>')
	XCALL WINPT (WND_1, 23,21, 01,00, 'YY',ENTRY, INXCTL)
	GOTO (JOB_PRINT),INXCTL
	GOTO HEADER
;-------------------------------------------------------

GET_WEIGHT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; SCRNX CODE TO GET LINE ITEM WEIGHT
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear jlwgt
	
	call check_cfg
	goto (reg_item),cfg_item-1
	
	call find_damper
	
	xcall cfg_w (partno, jlwgt, flag ,1,st_guage)
write_out,
	return
;-------------------------------------------------------	
reg_item,

	CLEAR ITMKEY
	K_ITEM = jlitm
	K_F1 = '000'
	K_F2 = '000'
	K_F3 = '00000'

	read (chn041, itmmas, itmkey, keynum:1, getrfa:rfa) [err=write_out]

	if (ip_um .eq. '$')
	then	begin	
			CLEAR TBL_KEY
			TBLCOD='DD'
			TBLKEY=USRDEF
			read (chn182, coptbl, tbl_key) [err=write_out]				
			jlwgt =(DD_DLR*PRICE)#2
			end
	else	jlwgt = ip_wgt
	
return
;-----------------------------------------------------------------
	
FIND_DAMPER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	PARTNO = jlitm

	MAXFLEN = %TRIM(PARTNO)
	FOR I FROM 1 THRU 3
		BEGIN
		XCALL INSTR(1, PARTNO, "*", FL)
		IF (.NOT. FL) 		EXITLOOP
		PARTNO(FL, MAXFLEN) = PARTNO(FL+1, MAXFLEN)
		END

	RETURN
;----------------------------------------------------------------	
check_cfg,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear cfg_item
	
; ssq 10-17-18 don't used pressed parts...

;;;	CLEAR TBL_KEY
;;;	TBLCOD = 'PP'
;;;	PP_ITEM = jlitm
;;;	READ (chn182,COPTBL,TBL_KEY) [ERR=NOT_PRESSED]
;;;	jlitm = pp_part		;item # for pressed part
;;;	cfg_item = 2
;;;	return
	
not_pressed,
	
	USING jlitm SELECT	;in case litmno replaced by pressed part
	('GA','GE'),				CFG_ITEM = 2
	('WR'),						CFG_ITEM = 1
	('J35'),					CFG_ITEM = 1
	('RW16' THRU 'RW20'),		CFG_ITEM = 1
	('R16' THRU 'R26'),			CFG_ITEM = 1
	('RV16' THRU 'RV26'),		CFG_ITEM = 1
	('T4','T9','TV4','TV9'),	CFG_ITEM = 1
	('C0324' THRU 'C25'),		CFG_ITEM = 2

	('C4020','C4420','C4620'),	CFG_ITEM = 2	;SSQ 10-14-03
	('C4820','C4220 '),			CFG_ITEM = 2	;SSQ 10-14-03
	('C9','C4','CV9','CV4'),	CFG_ITEM = 1
	('CT','CC','CTV','CCV'),	CFG_ITEM = 1

	('EC'),						CFG_ITEM = 2	;EC06-bad, ED0624S-good

	('ER'),						CFG_ITEM = 1
	('BN'),						CFG_ITEM = 1	
	('P4','P9','PV4'),			CFG_ITEM = 1
	('S9','S4','SV','SW','SC','ST'),	CFG_ITEM = 1
	('SG','SX'),				begin
								CFG_ITEM = 1
								end
	('RT','RTV'),				CFG_ITEM = 1
	('SR0' thru 'SR9'),			CFG_ITEM = 1
	('SRK0' thru 'SRK9'),		CFG_ITEM = 1
	('OB','PB','SBD','BDD'),	CFG_ITEM = 3	
	('SLF','SLB'),				CFG_ITEM = 3		;LOUVERS
	(),				CFG_ITEM = 2	;item not configurable
	ENDUSING

return			
;-------------------------------------------------------
;==============================================================

JOB_PRINT,	
	;re-read the job
	XCALL ISIO (CHN071, JOBHDR, JHNUM, READ, LOKCTL)	;2-24-11

	SPORD = JHNUM,'XXXXXX'
	OPEN (14, O, SPLFIL)

	LINCNT = 1
	XCALL HP (14,hpDOTS, 0)
	XCALL HP (14,hpTHICK,3)

	XCALL WHO(CMPCOD)

	CLEAR TBL_KEY
	TBLCOD = 'CO'
	C1_CODE = CMPCOD
	C1_REC = 1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) C1_NAME = ' *** NOT ON FILE ***'

	XCALL HP(14,hpFONT,hpBOLD)
	XCALL HP (14,hpFONT,hp5CPI+hpBOLD)
	TL = %TRIM(C1_NAME)
	LL = 38 - TL
	LL = LL/2
	IF (LL .LE. 0) LL = 1
	PLINE (LL,80) = C1_NAME

	CALL PRINT
	CALL PRINT

	XCALL HP (14,hpFONT,hp6CPI+hpBOLD)
	PLINE (10,27) = 'FABRICATION TICKET'
	PLINE (29,34) = JHNUM,	'ZZZZZX'
	CALL PRINT
	CALL PRINT

	D_LET = JHDEPT
	TL = %TRIM(DEPT_LINE)
	INCR TL
	DEPT_LINE(TL,TL) = '"'

	PLINE (16,80) = DEPT_LINE
	CALL PRINT
	CALL PRINT

	XDATE(1,4) = JHSHDT(5,8)
	XDATE(5,8) = JHSHDT(1,4)
	PLINE (16,19) = 'DUE:'
	PLINE (21,30) = XDATE,	'ZX/XX/XXXX'
	CALL PRINT

	CALL PRINT

;;;	XCALL HP (14,hpFONT,hpMEDIUM)
	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	pline = hdr1
	call print
	call print

	FIND (CHN072, JOBLIN, JHNUM) [ERR=JP_LOOP, EOF=JP_EOF]
JP_LOOP,
	XCALL IOS (CHN072, JOBLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO JP_EOF
	IF (JLNUM .NE. JHNUM) GOTO JP_EOF

	PLINE (1,6) = JLQTY,	'ZZZZZX'
	PLINE (9,23) = JLITM
	PLINE (25,54) = JLDES
	if (%instr(1,jlitm,'*')) pline (56,61) = '*DAMPR'
;;;	PLINE (56,75) = '____________________'
	PLINE (63,77) = '____________________'
	CALL PRINT
	CALL JP_MEMOS
	CALL PRINT
	IF (BOLD_ON)	XCALL HP(14,hpFONT,hpMEDIUM)

	USING JLSTS SELECT
	('R','C'),	NOP		;previously processed

	(' '),		BEGIN
			IF(JLSTS.NE.'P') CALL UPDITM		;allocate parent & componets first time print
			JLSTS = 'P'	;printed
			XCALL IO (CHN072, JOBLIN, JLKEY, WRITE, LOKCTL)
		;;;	CALL UPDITM
			END
	ENDUSING

	GOTO JP_LOOP

;ZZZZZX  AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA __________
;        XXX XXX XXXXX   AAAAAAAAA                                ZZZX BOXES
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
JP_EOF,
	JHRLDT = TODAY		;2-24-11
	JHSTS = 'P'		;12-5-18
	XCALL ISIO (CHN071, JOBHDR, JHNUM, WRITE, LOKCTL)	;2-24-11
	CALL PRINT
	CALL PRINT
	CALL PRINT
	XCALL HP (14,hpFONT,hp10CPI+hpBOLD)
	PLINE (28,80) = 'Fabrication Time'
	CALL PRINT
	CALL PRINT
;;;	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)

	CLEAR TBL_KEY
	TBLCOD = 'OP'
	OP_DEPT = JHDEPT
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
OP_LOOP,
	IF (OP_WRK)
		BEGIN
		call print
		B_DATA(1,2) = 'J~'
		B_DATA(3,4) = jhDEPT
		b_data(5,5) = '~'
		b_data(6,8) = OP_SEQ,	'XXX'
	
		XCALL B128(B_DATA, B_STRNG, 70)
		pline(1,20) = b_strng
		
		xcall of128
		END

	USING OP_SEQ SELECT
	(.LT. 90),	BEGIN		;REGULAR OPERATIONS
			PLINE(31,50) = OP_DESC
			PLINE (52,72) = '____________________'
			CALL PRINT
			END

	(),		BEGIN		;BOTTOM OF PAGE INSTRUCTIONS
			TL = %TRIM(OP_DESC)
			LL = (87 - TL)/2
			IF (LL.LE.0) LL = 1
			PLINE(LL,80) = OP_DESC
			IF (LINCNT.LT.50) LINCNT = 51
			END
	ENDUSING

	
	CALL PRINT
	XCALL IOS (CHN182, COPTBL, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO END_OP
	IF (TBLCOD .NE. 'OP') GOTO END_OP
	IF (OP_DEPT .NE. JHDEPT) GOTO END_OP
	GOTO OP_LOOP
END_OP,

	lincnt = 60
	XDATE(1,4) = TODAY(5,8)
	XDATE(5,8) = TODAY(1,4)

	PLINE (65,74) = XDATE,	'ZX/XX/XXXX'
	CALL PRINT

	lincnt = 55

;;;	XCALL HP (14,hpDOTS, 0)

	B_DATA(1,2) = 'J~'
	B_DATA(3,8) = jhnum, 'XXXXXX'
	b_data(9,9) = '~'
	if (jhdept(2,2) .eq. 'V')
	then	b_data(10,11) = jhdept
	else	b_data(10,11) = jhdept(1,1)

	XCALL B128(B_DATA, B_STRNG, 70)
	pline(21,40) = b_strng

	xcall of128
	call print

	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)


	display(14,eject)	;12-30-02
	display(14,reset)	;01-02-03
	CLOSE 14
	LPQUE (SPLFIL, copies:2, delete)

	CALL MAK_NEWORD	;12-5-18
	CALL MAK_DPTSTS	;12-19-18
	
	GOTO HEADER
;-------------------------------------------------------
mak_neword,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear neword
	savdpt = jhDEPT
	
	clear tbl_key
	tblcod = 'DD'
	tblkey = savdpt
	xcall isio (chn182,coptbl,tbl_key,read,lokctl)
	if (lokctl .ne. 0)
		begin
		clear coptbl
		end

	if (dd_sch)
		begin
		new_dpt = savdpt(1,1)

		xcall rdate(d6)
		new_dat = d6(1,4)	;mm/dd
		xcall time(d6)
		new_tim = d6(1,4)	;hh:mm

		new_ord = jhnum
		new_stat = 0
		new_type = 'J'		;job 12-5-18

		xcall isio (chn088, neword, new_key, store, lokctl)
		end
	RETURN
;------------------------------------------	RETURN

MAK_DPTSTS,

	CLEAR DPTSTS
	S_ORDN = JHNUM
	S_DEPT = '**'		;header
	XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)


; create a DPTSTS record for the fab ticket.  If there are
; already records in DPTSTS file, they will not be over-written.
	CLEAR DPTSTS
	S_ORDN = JHNUM


	S_DEPT(1,1) = JHDEPT

	XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)
	
	return
;------------------------------------------

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL HP (14,hpPOS,Lrow+LINCNT,1,0,PLINE)
	PLINE =
	INCR LINCNT
	RETURN
;-------------------------------------------------------

JP_MEMOS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR A40

	MM_CODE = JLIF1
	IF (MM_CODE .LE. 0) GOTO M2

	m_keyx = 'M1'
	CALL MEMO_KEY
	IF (KEY_FOUND) A40 = MM_SHORT
M2,
	MM_CODE = JLIF2
	IF (MM_CODE .LE. 0) GOTO M3

	m_keyx = 'M2'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
		BEGIN
		TL = %TRIM(A40)
		INCR TL
		A40(TL,40) = MM_SHORT
		END
M3,
	MM_CODE = JLIF3
	IF (MM_CODE .LE. 0) GOTO M_DONE

	m_keyx = 'M3'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
		BEGIN
		TL = %TRIM(A40)
		INCR TL
		A40(TL,40) = MM_SHORT
		END
M_DONE,	
	PLINE (09,11) = JLIF1
	PLINE (13,15) = JLIF2
	PLINE (17,21) = JLIF3

	PLINE (25,80) = A40

	CLEAR BOLD_ON
	IF (JLBOX .GT. 0)
		BEGIN
		BOXQTY = JLQTY/JLBOX
		IF (BOXQTY*JLBOX .NE. JLQTY) BOXQTY = BOXQTY + 1
		XCALL HP(14,hpFONT,hpBOLD)
		PLINE(66,69) = BOXQTY, 'ZZZZ'
		PLINE(71,75) = 'Boxes'
		BOLD_ON = 1
		END

	RETURN
;-------------------------------------------------------

UPDITM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SWITCH = 5
	XCALL FILES (24, 'SU', 041, SWITCH)
	IF (SWITCH .EQ. 9) RETURN

	XCALL XCOM(JLITM, JLIF1, JLIF2, JLIF3, JLQTY, JLRCV, 1, CHN162, CHN163, 24)

	CLEAR ITMKEY
	K_ITEM = JLITM
	K_F1 = JLIF1
	K_F2 = JLIF2
	K_F3 = JLIF3
	READ (24,ITMMAS,ITMKEY,KEYNUM:1) [ERR=NO_ITM] ;KRF = 2
	QTYONO = QTYONO + JLQTY	
	WRITE (24, ITMMAS, ITEMNO)
	CLOSE 24
	RETURN
NO_ITM,
	XCALL OLMSG (WND_1, 24, "Can't Update Item",1)

	CLOSE 24
	RETURN
;-------------------------------------------------------



FIND_LINE_ITEMS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; CHECK FOR EXISTING LINE ITEMS
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CALL LOAD_LINE_ITEMS
	RETURN
;-----------------------------------------------------------

LOAD_LINE_ITEMS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; LOAD EXISTIN LINE ITEMS INTO WND_2
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; restore current line items...
	XCALL W_PROC(WP_REMOVE,WND_2)
	XCALL W_PROC(WP_DELETE,WND_2)
	CALL CREATE_WND2
;;;	CALL RESTORE_WND2

	FIND (CHN072, JOBLIN, JHNUM) [ERR=RES_LOOP, EOF=RES_EOF]
RES_LOOP,
	LOKCTL = 1
	XCALL IOS (CHN072,JOBLIN,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO RES_EOF
	IF (JLNUM .NE. JHNUM) GOTO RES_EOF

	XCALL W_AREA(WND_2, WA_SCROLL, WAS_UP, 1)
	CALL MAKE_W2
	XCALL W_DISP(WND_2, WD_POS, 14, 1, W2_LINE)
	XCALL W_UPDT

	GOTO RES_LOOP
RES_EOF,
;;;	UNLOCK CHN071
	RETURN
;----------------------------------------------------

MAKE_W2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR W2_LINE

	W2_ITEM = JLITM
	W2_F1 = JLIF1
	W2_F2 = JLIF2
	W2_F3 = JLIF3
	W2_DESCR = JLDES
	W2_QTY = JLQTY

	RETURN
;----------------------------------------------------

ENDOFF,
	CALL CLOSE
	xcall pgchn ('cp:jobmnu',1)
;;	xcall pgchn ('cp:immenu',1)
	STOP
;============================================================

POP_KEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Find by item # & f_keys
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR MM_DES1, MM_DES2, MM_DES3

;;;	IF (WND_K)
;;;		BEGIN
;;;		XCALL W_PROC(WP_REMOVE,WND_K)
;;;		XCALL W_PROC(WP_DELETE,WND_K)
;;;		END
	IF (.NOT. WND_K) CALL CREATE_WNDK

	XCALL W_PROC(WP_PLACE, WND_K, 4, 4)
P_DISPLA,
	CLEAR CNGCTL
	XCALL W_DISP(WND_K, WD_CLEAR)
	XCALL W_DISP(WND_K, WD_POS, 1,1, '1. ITEM #')
	XCALL W_DISP(WND_K, WD_POS, 2,1, '2. F1_KEY')
	XCALL W_DISP(WND_K, WD_POS, 3,1, '3. F2_KEY')
	XCALL W_DISP(WND_K, WD_POS, 4,1, '4. F3_KEY')
	XCALL W_DISP(WND_K, WD_POS, 5,1, '5. QTY')

	IF (.NOT. NEW_ITEM)	; ie, called from cng_li...
		BEGIN
		XCALL W_DISP (WND_K, WD_POS, 1, 13, K_ITEM)
		XCALL W_DISP (WND_K, WD_POS, 2, 13, K_F1)
		XCALL W_DISP (WND_K, WD_POS, 3, 13, K_F2)
		XCALL W_DISP (WND_K, WD_POS, 4, 13, K_F3)

		A6 = QTY, 'ZZZZZX' [LEFT]
		XCALL W_DISP (WND_K, WD_POS, 5, 13, A6)
		XCALL W_UPDT
		GOTO P_ANY
		END
P_ITEM,
	XCALL WINPT (WND_K,1,13, 15, 00, 'AE', ENTRY, INXCTL, F_KEY)
	GOTO (P_DISPLA, P_ENDOFF),INXCTL

	USING F_KEY SELECT
	(F_05),	BEGIN
		IF (.NOT. F5_POP_ON) 
			BEGIN
			CALL F5_POP
			NEW_ITEM = 1	;BACK TO ADD MODE
			GOTO POP_KEY
			END
		GOTO P_DISPLA
		END
	ENDUSING

	K_ITEM = ENTRY(1,15)
	GOTO (P_ANY),CNGCTL
P_F1,
	XCALL WINPT (WND_K,2,13, 3, 00, '# ', ENTRY, INXCTL)
	GOTO (P_DISPLA),INXCTL
	MM_CODE = ENTRY(1,3)
	K_F1 = MM_CODE,	'XXX'

	m_keyx = 'M1'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		XCALL W_DISP (WND_K, WD_POS, 2,20, MM_SHORT)
		MM_DES1 = MM_SHORT
		END
	ELSE	CLEAR MM_DES1
	GOTO (P_ANY),CNGCTL
P_F2,
	XCALL WINPT (WND_K,3,13, 3, 00, '# ', ENTRY, INXCTL)
	GOTO (P_DISPLA),INXCTL
	MM_CODE = ENTRY(1,3)
	K_F2 = MM_CODE,	'XXX'

	m_keyx = 'M2'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		XCALL W_DISP (WND_K, WD_POS, 3,20, MM_SHORT)
		MM_DES2 = MM_SHORT
		END
	ELSE	CLEAR MM_DES2

	GOTO (P_ANY),CNGCTL
P_F3,
	XCALL WINPT (WND_K,4,13, 5, 00, '# ', ENTRY, INXCTL)
	GOTO (P_DISPLA),INXCTL
	MM_CODE = ENTRY(1,5)
	K_F3 = MM_CODE,	'XXXXX'

	m_keyx = 'M3'
	CALL MEMO_KEY
	IF (KEY_FOUND) 
	THEN	BEGIN
		XCALL W_DISP (WND_K, WD_POS, 4,20, MM_SHORT)
		MM_DES3 = MM_SHORT
		END
	ELSE	CLEAR MM_DES3

	GOTO (P_ANY),CNGCTL
P_QTY,
	XCALL WINPT (WND_K,5,13, 6, 00, '# ',ENTRY, INXCTL)
	GOTO (P_DISPLA),INXCTL
	QTY = ENTRY(1,6)
	GOTO (P_ANY),CNGCTL
P_ANY,	
	XCALL WANCN (WND_K, 6, CNGCTL, WHATNO)
	GOTO (P_PROCES, P_CNGBR),CNGCTL+1
P_CNGBR,
	GOTO (P_ITEM, P_F1, P_F2, P_F3, P_QTY),WHATNO
	GOTO P_ANY


P_PROCES,
	XCALL W_DISP (W_ID, WD_POS, 4,15, KITMNO)
	KITMNO = K_ITEM
	CLEAR K_MAT
	XCALL W_PROC (WP_REMOVE, WND_K)
	GOTO EXIT_KEY


EXIT_KEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	LOKCTL = 0
	READ (CHN041, ITMMAS, ITMKEY, KEYNUM:1) [ERR=P_BAD_KEY]	;KRF = 2
	RETURN
P_BAD_KEY,
	XCALL OLMSG (W_ID, 24, 'ITEM NOT FOUND',1)
	GOTO POP_KEY


P_ENDOFF,
	LOKCTL = 1			;ABORT
	XCALL W_PROC (WP_REMOVE, WND_K)
;;;	XCALL W_PROC (WP_DELETE, WND_K)
	xcall w_updt
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
;;;	XCALL WNMSG (WND_K, 6, 'MEMO NOT FOUND',1)
	KEY_FOUND = 0
	RETURN
;-------------------------------------------------------------
DEFLNX,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (FIRST.EQ.0) GO TO NEXT
	LOKCTL = 0
	XCALL IOW (CHN060,COPCTL,1,READ,LOKCTL,W_ID)	
	IF (LOKCTL) GO TO ENDOFF
	IF (LSTJOB.EQ.999999) LSTJOB =
	INCR LSTJOB
	NXTJOB = LSTJOB
	LOKCTL = 1
	XCALL IOW (CHN060,COPCTL,1,WRITE,LOKCTL,W_ID)	;SSQ 11/20/02
	FIRST =
NEXT,
;;;	DEFFLG = 1
	A6 = NXTJOB, 'XXXXXX'
	JOB = NXTJOB
	XCALL W_DISP(WND_1,WD_POS,2,13,A6)
	XCALL W_UPDT
	RETURN
;-------------------------------------------------

DSP_HDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	A6 = JHNUM,	'ZZZZZX' [LEFT]
	XCALL W_DISP (W_ID, WD_POS, 2, 13, A6)

	XCALL W_DISP (W_ID, WD_POS, 2, 29, JHDEPT)

	XDATE(1,4) = JHSHDT(5,8)
	XDATE(5,8) = JHSHDT(1,4)
	A10 = XDATE,	'XX/XX/XXXX'
	XCALL W_DISP (W_ID, WD_POS, 2, 46, A10)

	RETURN
;-------------------------------------------------------------
F5_POP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Inquire mode, hi-lite memos
	;;; associated w/ each line...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	F5_POP_ON = 1		;DON'T ALLOW MULTIPLE ENTRY INTO F5_POP

	CALL GET_LINES

	SAVE_ROW = 1
	LI = 1
RE_PAGE,
	IF (NUMARAL .EQ. 0) GOTO F5_RETURN
	LAST_ROW = NUMROWL
	IF (NUMROWL .GT. NUMARAL) LAST_ROW = NUMARAL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	CALL RESTORE_WND2

	XCALL W_PROC(WP_REMOVE,WND_2)
	XCALL W_PROC(WP_DELETE,WND_2)
	CALL CREATE_WND2

	XCALL W_PROC(WP_PLACE, WND_2, 8, 2)
	XCALL W_DISP(WND_2, WD_CLEAR)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR ARO
	CLEAR WROW
	FOR I FROM 1 THRU LAST_ROW
		BEGIN
		INCR WROW
		REST = PARRYL(I)
		XCALL W_DISP(WND_2, WD_POS, WROW, 1, WRKLIN(1,PLENL+2))
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
	IF (LI .GT. NUMARAL) LI = NUMARAL

	CALL DSPLIN
	CALL PINPUT
	USING F_KEY SELECT
	(EXIT_KEY, CR_KEY),	BEGIN
			CLEAR PIL
			CLEAR P_ACTIONL
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
			IF (NUMARAL .GT. 0) GOTO RE_PAGE
			END
	(PAGE_DN),	BEGIN
			CALL OL_PAGE_DN
			IF (NUMARAL .GT. 0) GOTO RE_PAGE
			END
	(RGHT_ARO),	BEGIN
			PIL = LI
			CALL CNG_LI		;CHANGE THIS LI
			IF (NUMARAL .GT. 0) 
				BEGIN
				if (%ttsts)READS (15, ENTRY)
				GOTO RE_PAGE
				END
			END
	(DEL_KEY),	BEGIN
			CALL DEL_LI
			GOTO DSPLOP
			END
	ENDUSING
	
DONE_F5,
	XCALL W_UPDT
	CALL LOAD_LINE_ITEMS

	CLEAR JOBLIN
;;;	CLEAR PI2, PI3

F5_RETURN,
	F5_POP_ON = 0		;DON'T ALLOW MULTIPLE ENTRY INTO F5_POP
	RETURN

DSPLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; move arrow, highlight selected line
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	CLEAR CLVL1, CLVL2, CLVL3, CLVL4
	ARO = '>'
	REST = PARRYL(LI)
	XCALL W_DISP (WND_2,WD_ATTR,ATTR_SET+ATTR_RVRS)
	XCALL W_DISP (WND_2,WD_POS,WROW,1,WRKLIN(1,WLEN))
	XCALL W_DISP (WND_2,WD_POS,WROW,1)
	SAVE_ROW = WROW
;display any memos for current item...
	READ (CHN072, JOBLIN, RFA=PARRFAL(LI))

;;;	CALL DISPLAY_MEMO_WINDOWS
	RETURN
;----------------------------------------------------------------------
CLEAR_LAST_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; CLEAR HI-LITE ON PREV LINE
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR ARO
	XCALL W_DISP (WND_2,WD_ATTR,ATTR_CLR+ATTR_RVRS)
	XCALL W_DISP (WND_2,WD_POS,SAVE_ROW,1,WRKLIN(1,WLEN))
	RETURN
;----------------------------------------------------------------------
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

;;; WINDOW SCROLLING ROUTINES
SCROLL_DN,
	WROW = LAST_ROW
	IF (LI .EQ. NUMARAL) RETURN
	XCALL W_AREA(WND_2,WA_SCROLL,WAS_UP,1)
	RETURN

SCROLL_UP,
	WROW = FIRST_ROW
	IF (LI .EQ. 1) RETURN
	XCALL W_AREA(WND_2,WA_SCROLL,WAS_DOWN,1)
	RETURN
;--------------------------

;===================================================================
ADD_LI,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	RETURN
DEL_LI,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	RETURN
;--------------------------------------------------
CNG_LI,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; ALLOW CHANGES TO SELECTED L/I
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	READ (CHN072, JOBLIN, RFA=PARRFAL(PIL)) ;;dbg [ERR=CNG_LI_ERR]
;input...
;;;	CALL NEW_ITEM
	CLEAR ITMKEY
	K_ITEM = JLITM
	K_F1 = JLIF1
	K_F2 = JLIF2
	K_F3 = JLIF3
	QTY = JLQTY
	NEW_ITEM = 0	;CHANGE MODE
	CALL POP_KEY

	JLITM = ITEMNO
	JLIF1 = IF1
	JLIF2 = IF2
	JLIF3 = IF3
	JLDES = DESCR
	JLQTY = QTY

	XCALL ISIO (CHN072, JOBLIN, JLKEY, WRITE, LOKCTL)
; refresh array
	FOR I FROM 1 THRU NUMARAL
		BEGIN
		READ (CHN072, JOBLIN, RFA=PARRFAL(I) )
		CALL MAKE_W2
		PARRYL(I) = W2_LINE
		END
	RETURN
OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK
	
	SWITCH = 5
	XCALL FILES (1, 'SU', 071, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN071 = 1

	SWITCH = 5
	XCALL FILES (2, 'SU', 072, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN072 = 2

	SWITCH = 5
	XCALL FILES (6, 'U', 060, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN060 = 6

	SWITCH = 5
	XCALL FILES (17, 'SI', 182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 17

	SWITCH = 5
	XCALL FILES (4, 'SI', 041, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 4

	SWITCH = 5
	XCALL FILES (8, 'SU', 088, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN088 = 8
	
	SWITCH = 5
	XCALL FILES (19, 'SU', 193, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN193 = 19

	SWITCH = 5
	XCALL FILES (62, 'SU', 162, SWITCH)	;;162 - PARENT
	IF (SWITCH .EQ. 9) RETURN
	CHN162 = 62

	SWITCH = 5
	XCALL FILES (63, 'SU', 163, SWITCH)	; 163 - CHILDX
	IF (SWITCH .EQ. 9) RETURN
	CHN163 = 63

	OPNOK = 1

	RETURN
;-------------------------------------------------------------
OL_PAGE_DN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PAGE FORWARD NUMARAL RECORDS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR SAVE_ROW
	WROW = 1
	LI = 1
	FIND (CHN072, JOBLIN, RFA=PARRFAL(NUMARAL)) [ERR=GL_INIT]
	GOTO GL_INIT

OL_PAGE_UP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PAGE BACKWARDS NUMARAL RECORDS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR SAVE_ROW
	WROW = 1
	LI = 1
; read reverse numaral records...
	FIND (CHN072, JOBLIN, RFA=PARRFAL(1)) [ERR=GL_INIT]
	FOR I FROM 1 THRU MAXARAL
		BEGIN
		READS (CHN072, JOBLIN, GL_INIT, REVERSE) [ERR=GL_INIT]
		IF (JLNUM.NE.JHNUM) GOTO GL_INIT
		END
	GOTO GL_INIT
;------------------------------------------------------------------

GET_LINES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; READ NUMARAL JOBLIN RECORDS INTO ARRAY
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR I
	CLEAR JLKEY
	JLNUM = JHNUM
	FIND (CHN072, JOBLIN, JLKEY) [ERR=GL_INIT, EOF=RL_EOF]

GL_INIT,
	FOR I FROM 1 THRU MAXARAL CLEAR PARRYL(I), PARRFAL(I), PFLAGSL(I)
	CLEAR I

READ_LINES,
	XCALL IOS (CHN072, JOBLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO RL_EOF
	IF (JLNUM.NE.JHNUM) GOTO RL_EOF
;;;	IF (SEARCH_ON .AND. (LITMNO(1,SL).NE.S_ITEM(1,SL)) ) GOTO READ_LINES
	INCR I
	XCALL GETRFA(CHN072, LINRFA)
	PARRFAL(I) = LINRFA
	CALL MAKE_W2
	PARRYL(I) = W2_LINE

	IF (I .LT. MAXARAL) GOTO READ_LINES	
RL_EOF,
	UNLOCK CHN072
	NUMARAL = I
	RETURN
;---------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN041) CLOSE CHN041
	IF (CHN060) CLOSE CHN060
	IF (CHN071) CLOSE CHN071
	IF (CHN072) CLOSE CHN072
	IF (CHN182) CLOSE CHN182
	IF (CHN088) CLOSE CHN088
	IF (CHN162) CLOSE CHN162
	IF (CHN163) CLOSE CHN163
	IF (CHN193) CLOSE CHN193

	RETURN
;-------------------------------------------------------------
;=================================================================
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
	XCALL W_BRDR(WND_1,WB_TITLE,'Fabrication Tickets',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL W_UPDT
	W_ID = WND_1
;;;	xcall u_logwnd(wnd_1)

	CALL CREATE_WNDK	;POP_KEY WINDOW		
	CALL CREATE_WND2	;LINE ITEMS
;;;	RETURN
;CNGLI:
	MAXARAL = 10
	PLENL = 65
	NUMROWL = 10
	WXL = 8
	WYL = 1
	POP_WIDL(1,5) = "F4WIN"
	POP_TITLEL = "Select/Unselect Memos"
	RETURN

CREATE_WNDK,
	W2_NAME = 'FKEY'
	XCALL W_PROC(WP_FIND, WND_K, W2_NAME)
	IF (.NOT. WND_K) XCALL W_PROC(WP_CREATE,WND_K,W2_NAME,7,41)
	XCALL W_BRDR(WND_K,WB_TITLE,'Item Keys',
&		WB_TPOS,WBT_TOP,WBT_CENTER)
	RETURN
;--------------------------------------------------------

CREATE_WND2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL W_PROC(WP_CREATE, WND_2, 'SCROL',10, 67)
	XCALL W_BRDR(WND_2,WB_TITLE,'LINE ITEMS',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
RESTORE_WND2,
	XCALL W_PROC(WP_PLACE, WND_2, 8, 2)
	XCALL W_DISP(WND_2, WD_CLEAR)
	WLEN = 67
	RETURN
;--------------------------------------------------------
;
