;PRCVR.cp
; COVER SHEET dept status

;sales copy of "material for all depts" which is now call the "main" copy.
;main copy prints to will-call.
;this copy (sales copy). prints locally.
;
; 5-30-18 ssq: make cusmas isam
; 3-28-19: Express orders
; 5-6-19:  print to sc_que in carrier table
; 01-28-20 ssq: IC sb based on consolidated qty

SUBROUTINE	PRCVR
	ORDNO	,D
	LOCAL	,D	;1= print on local printer
	PB	,A	;printed by (optional)

	.include 'def:hpsub.def'

EXTERNAL FUNCTION
	TRN3	,D
EXTERNAL FUNCTION
	IS_IC	,D		;SPLIT DEPT C INTO C AND IC

EXTERNAL FUNCTION		;1-28-20
	IS_IC2	,D

record	key3
	k3_ord	,d6
	k3_dept	,a2
	k3_item	,a15
	k3_desc	,a30

record	icitem			;1-28-20
	ic_item	,a15
	ic_qty	,d6
	
RECORD	ICFIL
		,A4,	'SPL:'
		,A1,	'C'
		OCORD	,A6
		,A4,	'.ISM'


RECORD	ORDCM2
	.INCLUDE 'DEF:RD135A.DEF'

RECORD	CRHOLD
	.INCLUDE 'DEF:RD195A.DEF'

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


;;;GLOBAL DUCK	
	.INCLUDE 'DEF:RD175D.DEF'
;;;ENDGLOBAL

RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.DEF'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PCL code for IDX C8800 printer

record	reset
	re	,a1	;esc
		,a1,	'E'

record	l1
	e1	,a1	;esc
		,a*,	'*v6W'
	lx	,8a1
;;;		,a*,	' 020108080808'
record	idx_pal
	e2	,a1	;esc
		,a*,	'*v'
	ipcR	,d3	;color 1 
		,a*,	'a'
	ipcG	,d3	;color 2
		,a*,	'b'
	ipcB	,d3	;color 3
		,a*,	'c'
	ipidx	,d2	
		,a*,	'I'


record	idx_sel
	e5	,a1	;esc
		,a*,	'*v'
	isidx	,d2
		,a*,	'S'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
record	bot_lin
		,a29
		,a*,	'   QTY U/M                           SIZE     WGHT'

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT

RECORD	BOLD
	E_CHA2	,A1
		,A*,	"(s3B"

RECORD	MEDIUM
	E_CHA3	,A1
		,A*,	'(s0B'

RECORD	BOTFIL
		,A4,	'SPL:'
		,A1,	'B'
	BTORD	,A6
		,A4,	'.ISM'

RECORD	WRKFIL
		,A4,	'SPL:'
		,A1,	'W'
	WFORD	,A6
		,A4,	'.ISM'
RECORD	OUTFIL
		,A4,	'SPL:'
		,A1,	'X'
	OFORD	,A6
		,A4,	'.ISM'

RECORD	DUCFIL
		,A4,	'SPL:'
		,A1,	'D'
	WRORD	,A6
		,A4,	'.ISM'

RECORD	SPLFIL
		,A4,	'SPL:'
		,A1,	'S'
	SPORD	,A6
		,A4,	'.SPL'

RECORD	BOTWRK
	B_PWI	,D1	;0=PRINT DETAIL AT TOP, 1=NOT PACKED W/, 2=PACKED W/ OR INSTALLED
	B_DEPT	,A2	;PRINT THIS DEPT AT BOTTOM OF COVER SHEET
	B_PRT	,A1	;Y = PRINT DETAIL AT TOP OF ORDER, N=DON'T PRINT DETAIL
		,A6	;filler
RECORD,X
	B_KEY	,A3
	
RECORD	F_MEMOS
	.INCLUDE 'def:CPMEMO.DEF'

RECORD	DPTLOC
	.INCLUDE 'DEF:RD194A.DEF'
		
RECORD	DPTSTS
	.INCLUDE 'DEF:RD193A.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.def'
RECORD,X
	.INCLUDE 'DEF:RD045M.def'
RECORD,X
	.INCLUDE 'DEF:RD045D.def'


RECORD	CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'
RECORD	DUMCUS
	.INCLUDE 'DEF:RD001B.DEF'
;;;RECORD	CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'

RECORD	SALMAN
	.INCLUDE 'DEF:RD054A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	DEPTMNTS
	DPTS	,20A2
	MAXDPT	,D2,	20
	NUMDPT	,D2
	ROCKON	,D1

RECORD PARAM
	.INCLUDE 'DEF:PARAM2.DEF'


RECORD
	MAXDUC	,D3,100
	QTY	,D5
	ITM	,A15
	DES	,A30
	PRC	,D7
	COMPA	,A15
	COMPB	,A15

RECORD ARRAYS
	SDUCRC	,100A32	;SUMMARIZED DUCT CONFIGURATIONS   (SEE DUCREC BELOW)
	LINSQF	,9D6	;SUMMARIZE LINER SQUARE FEET

RECORD DUCREC
	DUCCFG	,D7	;CONFIGURATION CODE
	DUCSQF	,D7	;SQ FEET OF MATERIAL
	DUCPND	,D7	;POUNDS OF MATERIAL
	DUCGPR	,D7	;GAUGE PRICE

RECORD ACCREC
	.INCLUDE 'DEF:ACCREC.DEF'


RECORD	LINE			
	WDEPT	,A2
	WSEQ1	,D2		;reversed
	WSEQ2	,D2
	WSEQ3	,D2	
	WSQNO	,A2
	WITEM	,A15
	WDESC	,A90
	WTYPE	,A1		;L=LINE, M=MEMO
	WQTY	,D5
	WUM	,A2
	wpcat	,a2		;prdcat ssq 4-11-06
	wtag	,d2		;lmsq4 12-26-07
	wcngd	,d1		;changed 7-23-09

RECORD	MEMO,X
	MDEPT	,A2
	MSEQ	,D6
	MSQNO	,A2
	MITEM	,A15
	MMEMO	,3A30
		,A1
		,D5
		,A2
		,a2		;ssq 4-11-06
		,a2		;ssq 12-26-07
		,a1
RECORD	,X
		,D2
		,D6
		,A2
		,A15
	M_LONG	,A63
		,A27
		,A1
		,D5
		,A2
		,a2		;ssq 4-11-06
		,a2		;ssq 12-26-07
		,a1
RECORD,X
	W_KEY	,A55

RECORD,X
		,a2		;not reversed
		,A2
	COMSEQ	,A4
;---------------------------
RECORD	LINE2			
	WDEPT2	,A2
	WSEQ12	,D2		
	WSEQ22	,D2
	WSEQ32	,D2	;REVERSE
	WSQNO2	,A2
	WITEM2	,A15
	WDESC2	,A30
	WADD2	,A60	;FILLER
	WTYPE2	,A1		;L=LINE, M=MEMO
	WQTY2	,D5
	WUM2	,A2
	wpcat2	,a2		;ssq 4-11-06
	wtag2	,d2		;ssq 12-26-07
	wcng2	,d1		;ssq 7-23-09
RECORD,X
	W_KEY2	,A55

RECORD,X
		,a2		;not reversed
		,A2
	COMSEQ2	,A4
;---------------------------

RECORD	H_LINE
;;;	,A*,	'DATE ENTERED    SLS-REP     PO NUMBER   JOB NUMBER'
	,A*,	'DATE ENTERED  SLS-REP       PO NUMBER   JOB NUMBER'
	,A*,	'  SHIP VIA         SHIP DATE'

RECORD	H_LINE2
	,A*,	'   QTY  ITEM NUMBER     DESCRIPTION                     UM'

RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN044	,D2
	CHN045	,D2
	CHN054	,D2
	CHN135	,D3
	CHN175	,D2
	CHN182	,D2
	CHNWRK	,D2
	CHNOUT	,D2
	CHNDUC	,D2
	CHNBOT	,D2
	CHN193	,D2
	CHN194	,D2

RECORD	PAGEOF
	P1	,A2
		,A4,	' OF '
	P2	,A2


RECORD	ORDFMT
	OF_ORD	,A6
		,A1,	'.'
	OF_NUM	,D1
		,A1
	OF_LOC	,A1
RECORD,X
		,A5
	LDIG	,D1


RECORD	PVARS
	is_ic_item	,d1		;1-28-20
	totqty		,d6		;1-28-20
	savitm		,a15	;1-28-20
	chnic		,d2		;1-28-20
	TEMP	,A2
	MEMO_ON	,D1
	MAXLIN	,D2,	57	;2-6-14
	LINCNT	,D2
	PAG	,D2
	NPAG	,D3
	TPAG	,D3
	SAVPAG	,D3
	LP_ON	,D1
	pakdpt	,a2
	PLINE	,A85		;ssq 7-23-14
	SAVLIN	,A85
	DPTLIN	,A85
	BLANKS	,A30
	BLANK90	,A90
	TMPDSC	,A56
	REM	,D2
	DASHES	,A30,	'------------------------------'
	UNDER	,A58
	DECMAL	,D18
	NUMASK	,A8,	'ZZZZZZX-'

RECORD
	TM2	,A90
	TM3	,A90
RECORD,X
	TM2A	,3A30
	TM3A	,3A30

;---------------------------------
; for f4 tags... 1-03-08
RECORD
	D_LONG	,A90
RECORD,X
	DL_ARA	,3A30
;---------------------------------

RECORD	DATIME
	DATE	,A10
		,A2
	TIME	,A8

RECORD	KEYSP
	KEY_SPEC	,A*,	'START=1:126, LENGTH=55:2, DUPS, ASCEND'

RECORD	VARS
	ic_lbs	,d12
	c_lbs	,d12
	c_wgt	,d12
	C_PCS	,D6
	IC_PCS	,D6
	print_que	,a12	;sc_cvrs print que (carrier table SC)
	printed_by	,a30	;printed by
	lin_dpt		,a2
	from_o		,d1
	from_p		,d1
	got_o		,d1
	got_p		,d1
	printed_o	,d1
	printed_p	,d1
	no_stage	,d1	;1= no staging for this dept
	printing_bot	,d1	;1= printing dptsts detail at bottom of cover-sheet
	bot_first	,d1	;first time 
	W_PASS	,D1	;1 OR 2
	W_PRINT	,A1	;Y/N print lines on Cover Sheet?
	sav_wdp	,a2	;for duct dept
	sr_qty	,2d5	;dept k, m
	lrm	,d6
        barfil  ,a14,   'cp:c128s16.bar'
	b_data	,a100
	b_strng	,a100
	splfl2	,a14
	prow	,d3
	changed_lines	,d1
	is_vulcan	,d1	;10-02-08
	xdept		,a2	;10-02-08
	hex0	,a1
	hex1	,a1
	hex2	,a1
	hex8	,a1
	W2	,A2
	WX_KEY	,A57
	CMPCOD	,A3	;SSQ 9-12-06
	I	,D6	
	bold_on	,d1
	sav_bold	,d1
	XPOL1	,A12
	XPOL2	,A12
	DT	,A20
	D	,D8
	T	,D6
	LNAM	,A25
	SNAM	,A12
	SINT	,A3
	GOTP	,D1
	GOTO	,D1
	XDPT	,D2
	SZ1	,D3
	SZ2	,D3
	A2	,A2
	FACT	,D4
	SAVJOINT	,D5
	SAVFEET		,D6
	SAVFLIN		,D6
	SAVLBS		,D6
	OPNOK	,D1
	FIRST_PAGE	,D1
	SV_NAME	,A15
	E_DATE	,A10		;DATE ENTERED
	SH_DATE	,A10		;SHIP DATE
	CONFIG	,D7
	CSZ	,A30
	KEY	,A6
	BSMID	,D6
	BSEND	,D6
	SRCCTL	,D1
	ACCUOM	,A2
	J	,D5
	LL	,D2
	TL	,D2
	SEQNO	,D2
	SAVSEQ	,D2
	SAVSQ1	,D2
	SAVSQ2	,D2
	SAVSQ3	,D2
	SAVDPT	,A2
	SAVCOM	,A4
	SAVKEY	,A55
	XSAVKEY	,A57
	SAVQTY	,D5
	MULTLINE	,D5
	SKIP_LINE	,D1		;SKIP THIS LINE IF BLOCK MEMO
	STAT	,D3
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	LOKCTL	,D1
	SWITCH	,D1

PROC

	clear printed_by
	if (%passed(pb) ) printed_by = pb		;
	if (printed_by .eq. 'WOOD') local = 1		;6-24-19 so it will print local for me

	SWITCH = 5
	XCALL FILES (40, 'SI', 195, SWITCH)
	read (40, CRHOLD, ORDNO) [ERR=NOT_ON_HOLD]
	goto is_on_hold
NOT_ON_HOLD,
	clear crhold
is_on_hold,
	CLOSE 40

	USING CH_FLAG SELECT
	('3'),	XRETURN		;Nothing prints for 555 crdlmt...	
	('4'),	XRETURN		;4-1-15 ssq: COD	
	ENDUSING

	XCALL WHO (CMPCOD)

	XCALL ASCII (27, E_CHAR)
	E_CHA2 = E_CHAR
	E_CHA3 = E_CHAR

	CLEAR ROCKON			;SSQ 4-27-03
	CLEAR BLANKS, BLANK90, ARRAYS, bold_on, sav_bold
	FOR I FROM 1 THRU MAXDPT CLEAR DPTS(I)

	MEMO_ON = 1

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	clear coptbl
	tblcod = 'SC'		;carrier table
	sc_scac = oscac
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (sc_cvrs.eq.'Y') print_que = sc_que
	

	call read_liner		;2-22-17


	CLEAR ORDCM2
	C2_ORD = ORDNO
	C2_SEQ = 0

	LOKCTL = 0
	XCALL ISIO (CHN135, ORDCM2, C2_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR ORDCM2

	CALL LOAD_IC			;1-28-20

	CALL LOAD_MEMOS
	CALL LOAD_WORK
	CALL CONSOLIDATE
	CALL GET_DEPTS

	CALL LOAD_DUCT
	CALL CONS_DUCT

	CALL LOAD_BOT

;;; Add memos to output file...
	CLEAR SAVCOM, SAVSQ1, SAVDPT

	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_BM]

BM_LOOP,
	READS (CHNWRK, LINE, EOF_BM)

	USING WDEPT(1,1) SELECT
	('F','H','K'),	IF (WDEPT .NE. SAVDPT) CALL NEWDPT
	(),		IF (WDEPT(1,1) .NE. SAVDPT(1,1)) CALL NEWDPT
	ENDUSING
;;;	IF (WDEPT(1,1) .NE. SAVDPT(1,1)) CALL NEWDPT

	IF (WSEQ1 .NE. SAVSQ1) CALL NEWSQ1
	IF (COMSEQ .NE. SAVCOM) CALL NEWCOM

	GOTO BM_LOOP
EOF_BM,
	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_FIN]
FIN_LOOP,
	READS (CHNWRK, LINE, EOF_FIN)
	STORE (CHNOUT, LINE, W_KEY)
	GOTO FIN_LOOP
EOF_FIN,
	CLOSE CHNWRK

	CLEAR SAVPAG, LP_ON
START_PRINT,
	CLEAR TPAG, PAG
	NPAG = SAVPAG
	OPEN (14, O, SPLFIL)


	call colors		;setup for oki color printer...

	printing_bot =			;set to 1 when printing bottom
	FIRST_PAGE = 1
	CALL PRTHDR

;-----------------------------------------------------------
; 4-12-17 to get top and bottom correct for lines and duct...
	CLOSE CHNOUT
	OPEN (CHNOUT, SI, OUTFIL)
	XCALL FILL ('_', UNDER)
	SAVDPT = -1

;;;	bot_first =			;trigger first h-line
;;;	W_PASS = 1			;Print any detail on the first pass
;;;PASS2,	
	CALL WRTMEM		;INTERNAL ROUTINE TO WRITE WORKSHEETS
	clear from_o, from_p
	CALL GETDUC		;PROCESS DUCTWORK

	CALL PRINT_BOT		;5-22-17
;;;	incr w_pass
;;;	if (w_pass .le. 2) goto pass2

;-----------------------------------------------------------

	SAVPAG = TPAG
	bold_on = 0		;ssq 3-30-06	

	IF (C_LBS+IC_LBS .GT.0)
		BEGIN
		call print
		call print
		display (14, bold)
		PLINE = 'C + IC TOTAL PCS:'
		PLINE (18,21) = (C_PCS+IC_PCS), 'ZZZX'
		PLINE (27,36) = 'TOTAL LBS:'
		PLINE (37,43) = (C_LBS+IC_LBS)#2, 'ZZZ,ZZX'
		CALL PRINT
		display (14, medium)
		END

	LRM = maxlin - lincnt -3
	if (lrm .lt. 1) lrm=1
	for i from 1 thru LRM writes (14, '   ')

	pline = datime
	call print

	pline = 'Printed by:'
	pline(14,54) = printed_by

	pline (50,80) = 'Loaded by __________________________'
	call print

	DISPLAY(14,MEDIUM)
	display(14, EJECT)


	CLOSE 14
	close 46

	INCR LP_ON
	IF(LP_ON .LE. 1) GOTO START_PRINT


	if (local .eq. 1) 
		begin
        	lpque (barfil)                  ;cp:c128s16.bar - insure barcode loaded
		lpque (splfil, delete)
		goto done_ptest	;nothing else to do
		end


	USING CMPCOD SELECT
	('SMC'),	begin
       			lpque (barfil,lpnum:print_que)  ;cp:c128s16.bar - insure barcode loaded
			lpque (splfil, lpnum:print_que, delete)
       		;;;	lpque (barfil)                  ;cp:c128s16.bar - insure barcode loaded
		;;;	lpque (splfil, delete)
			end		;see rules for SMC below...

	('TST'),	BEGIN
       			lpque (barfil,lpnum:print_que)  ;cp:c128s16.bar - insure barcode loaded
			lpque (splfil, lpnum:print_que, delete)
       		;;;	lpque (barfil)                  ;cp:c128s16.bar - insure barcode loaded
		;;;	lpque (splfil, delete)
			goto done_ptest	;nothing else to do
			END

	('ROC'),	BEGIN
		   	lpque (barfil, lpnum:"ROC_STEVEG")                ;cp:c128s16.bar - insure barcode loaded
			LPQUE (SPLFIL, LPNUM:"ROC_STEVEG", DELETE)
			GOTO DONE_PTEST
			END

	(),		goto done_ptest					;Rockford & SMP don't get a Main Copy
	ENDUSING



done_ptest,
	CLOSE CHNOUT
	CLOSE CHNWRK
	CLOSE CHNBOT
	XCALL DELET (WRKFIL)
	XCALL DELET (OUTFIL)
	XCALL DELET (BOTFIL)
	CLOSE CHNDUC
	XCALL DELET(DUCFIL)
	close chnic
	xcall delet (icfil)
	
ENDOFF,
	call close

;;;	CLOSE CHN045
;;;	CLOSE CHN175
;;;	CLOSE CHNWRK

	XRETURN

;================================================


NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVDPT = WDEPT
	RETURN
;------------------------------------------

NEWSQ1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear savcom		;7-30-09 solves prob. if f1 changes but f2+f3
				;are same as prev line.
	SAVSQ1 = WSEQ1
	IF (WSEQ1 .LE. 0) RETURN
	CLEAR LINE2
	WSEQ12 = SAVSQ1
	CLEAR WSEQ22, WSEQ32	
	WITEM2 = '   F1'
	WTYPE2 = 'M'
;;;	WDESC2 = F1_MEMOL(WSEQ12)
	WDESC2(1,90) = F1_MEMOL(WSEQ12)	;1-17-08
	WDEPT2 = SAVDPT
	STORE (CHNOUT, LINE2, W_KEY)

	RETURN
;------------------------------------------
NEWCOM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; COMBINATION OF SEQ2 & SEQ3
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVCOM = COMSEQ
	CLEAR TM2, TM3
	IF (WSEQ2 .NE. 0) TM2 = F2_MEMOL(WSEQ2)
	IF (WSEQ3 .NE. 0) TM3 = F3_MEMOL(WSEQ3)


	FOR J FROM 1 THRU 3
		BEGIN
		CLEAR LINE

		WSEQ1 = SAVSQ1
		COMSEQ = SAVCOM
		WITEM = '   MB'		;8-30-00 SSQ
		WITEM = 
		WTYPE = 'M'
		WDEPT = SAVDPT
		WDESC(1,30) = TM2A(J)
		WDESC(34,63) = TM3A(J)
		IF (WDESC .EQ. BLANK90) NEXTLOOP
		IF (TM3A(J).NE.BLANKS) 
			BEGIN
			WDESC(32,32) = '*'
			END
		WITEM (10,10) = J,'X'
		STORE (CHNOUT, LINE, W_KEY)
		END
	RETURN
;------------------------------------------

LOAD_IC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; dept IC SB based on 
		;;; consolidated qty
		;;;;;;;;;;;;;;;;;;;;;;;;;;;

	k3_ord = oordno
	k3_dept = 'C '
	k3_item =
	k3_desc =

	savitm = '-1'

	find (chn045, ordlin, key3, krf=3) [err=ic_loop]
ic_loop,
	reads (chn045, ordlin, eof_ic)
	if (lordno .ne. oordno) goto eof_ic
	if (ldept .ne. 'C') goto ic_loop
	if (lcfgim .ne. savitm) call newitm
	if (.not. %is_ic2(ordlin) ) goto ic_loop
	totqty = totqty + lqtyor
	goto ic_loop

eof_ic,
	call newitm
	return

newitm,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (savitm .eq. '-1') goto outitm
	using totqty select
	(0),		goto outitm	;no ic items
	(.gt.9),	goto outitm	;qty more than 9
	(),		begin
			ic_item = savitm
			ic_qty = totqty
			xcall isio  (chnic, icitem, ic_item, store, lokctl)
			end
	endusing


outitm,
	savitm = lcfgim
	totqty =
	return
;--------------------------------------------

LOAD_MEMOS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Read thru ordlin file, and
		;;; load memo arrays
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR F1_NUM, F2_NUM, F3_NUM, F4_NUM
	FOR J FROM 1 THRU F_MAX
		BEGIN
		CLEAR F1_MEMOS(J)
		CLEAR F2_MEMOS(J)
		CLEAR F3_MEMOS(J)
		CLEAR F4_MEMOS(J)

		CLEAR F1_MEMOL(J)
		CLEAR F2_MEMOL(J)
		CLEAR F3_MEMOL(J)
		CLEAR F4_MEMOL(J)

		clear f1_key(j)
		END

	FIND (CHN045, ORDLIN, ORDNO) [ERR=MLOOP]
MLOOP,
	READS (CHN045, ORDLIN, EOF_M)
	IF (LINSEQ .NE. 0) GOTO EOF_M
	IF (LMSQ1 .GT. 0) 
		BEGIN
		F1_MEMOS(LMSQ1) = M_SHORTD
		F1_MEMOL(LMSQ1) = M_LDESCR
		F1_KEY(LMSQ1) = M_KEY
		END
	IF (LMSQ2 .GT. 0) 
		BEGIN
		F2_MEMOS(LMSQ2) = M_SHORTD
		F2_MEMOL(LMSQ2) = M_LDESCR
		END
	IF (LMSQ3 .GT. 0) 
		BEGIN
		F3_MEMOS(LMSQ3) = M_SHORTD
		F3_MEMOL(LMSQ3) = M_LDESCR
		END
	IF (LMSQ4 .GT. 0) 
		BEGIN
		F4_MEMOS(LMSQ4) = M_SHORTD
		F4_MEMOL(LMSQ4) = M_LDESCR
		END
	GOTO MLOOP
EOF_M,
	RETURN
;-------------------------------------------------

LOAD_WORK,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Move non-memo line items
		;;; into work file
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear c_lbs, ic_lbs, c_pcs, ic_pcs
	clear changed_lines
	for i from 1 thru 2	clear sr_qty(i)

	FIND (CHN045, ORDLIN, ORDNO) [ERR=LOOP]
LOOP,
	READS (CHN045, ORDLIN, EOF)
	IF (LORDNO .NE. ORDNO) GOTO EOF
	IF (LTYPE .EQ. 'M') GOTO LOOP
	IF (LPRDCD .EQ. 'Z ') GOTO LOOP	;SSQ 3/22/05 skip priceduct.com

	CLEAR TBL_KEY		;SSQ 11-9-04
	TBLCOD = 'EX'
	TBLKEY = EX_ITEM = LITMNO
	FIND (CHN182,COPTBL,TBL_KEY) [ERR=LCON2]	;continue if NOT in table
	GOTO LOOP
LCON2,
	WTYPE = 'L'
	WSQNO = LSRTSQ
	wdept = ldept(1,1)
;;;8-19-19>	CALL CHECK_VULCAN		;7-29-08: go to vulcan?
;;;8-19-19>	IF (IS_VULCAN)	WDEPT(2,2) = 'V'
;;;	if (wdept.eq.'C' .and. %is_ic(ordlin)) wdept = 'IC'
;-
	if (ldept .eq. 'C')
		begin
		call c_lbs				;6-19-19
		is_ic_item = 0	;assume not an IC item	;1-28-20
		if (%is_ic(ordlin)) 
			begin
			xcall isio (chnic, icitem, lcfgim, read, lokctl) ;1-28-20
			if (lokctl.eq.0) is_ic_item = 1
			end
		if (is_ic_item)
		then	begin
			wdept = 'IC'
			ic_lbs = ic_lbs + c_wgt
			ic_pcs = ic_pcs + lqtyor
			end
		else	begin
			wdept = 'C'
			c_lbs = c_lbs + c_wgt
			c_pcs = c_pcs + lqtyor
			end
		end

;-

	A2 = LMSQ1, 'XX'
	WSEQ1 = A2
	A2 = LMSQ2, 'XX'
	WSEQ2 = A2
	A2 = LMSQ3, 'XX'
	WSEQ3 = A2
	A2 = LMSQ4, 'XX'
	WTAG = A2			;ssq 12-26-07

	WITEM = LITMNO
	using witem select
	('JEB','JEF','JJG','JTG'), WITEM = LITMNO(2,15)	;SKIP THE "J"
	endusing

	WDESC = LDESCR
	IF (LDAMPR) WITEM(10,15) = 'Damper'
	WQTY = LQTYOR
	WUM = LUOFM

	wcngd = lcngd
	if (wcngd .eq. 1) changed_lines = 1
	STORE (CHNWRK, LINE, W_KEY)
	GOTO LOOP

EOF,
	RETURN
;----------------------------------------------

CONSOLIDATE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Consolidate line items for the
		;;; same part #
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_CON]
	SAVKEY = '***'
	XSAVKEY = '***'
CONLOOP,
	READS(CHNWRK, LINE, EOF_CON)
	W2 = WTAG, 'XX'
	WX_KEY = W_KEY + W2
	IF (WX_KEY .NE. XSAVKEY)
	THEN	CALL NEWKEY
	ELSE	BEGIN
		INCR MULTLINE
		SAVQTY = SAVQTY + WQTY
		DELETE (CHNWRK)
		END
	GOTO CONLOOP

NEWKEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (XSAVKEY .EQ. '***') GOTO OUTKEY
	IF (MULTLINE .EQ. 0) GOTO OUTKEY
	READ (CHNWRK, LINE, XSAVKEY)
	WQTY = SAVQTY

	W2 = WTAG, 'XX'				;1-24-08 need to re-set key
	WX_KEY = W_KEY + W2
	WRITE (CHNWRK, LINE, XSAVKEY)
OUTKEY,
	SAVKEY = W_KEY
	XSAVKEY = WX_KEY
	SAVQTY = WQTY
	CLEAR MULTLINE
	RETURN
;-----------------------------------------------

EOF_CON,
	CALL NEWKEY
	RETURN
;-----------------------------------------------
LOAD_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; LOAD DUCTWORK INTO WORKFILE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Note: only the ductwork data is moved into the work file,
; it is assumed that all the acc info has already been moved
; to the ordlin file...
	CLEAR GOTP, GOTO
	FIND(CHN175,DUCACC,ORDNO) [ERR=LD_LOOP]
LD_LOOP,
	LOKCTL = 1
	XCALL IOS (CHN175,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.ORDNO) GOTO LD_EOF

	XCALL DDEPT (GAUGE,SIZE3,DUTYPE,WD_DEPT,STY)
	USING WD_DEPT SELECT
	('P'),	GOTP = 1
	('O'),	GOTO = 1
	ENDUSING


	WDUTYPE = DUTYPE	; Type of duct 
	WSTY = 10-STY		; Style		descending
	WLINER = LINER		; Liner 
	WSEAM = SEAM		; Seam  
	WSEAL = SEAL		; Seal
	WGAUGE = GAUGE		; Gauge ( 26,24,22,20,18 )
	WSIZE3 = 100-SIZE3	; LENGTH	descending
	WSIZE1 = 1000000-SIZE1	; SLIP		descending
	WTHICK = THICK		; Dec. Thickness .024-24g .032-22g .040-20g .050-18g
	WCAT = CAT		; Catagory 
	WJOINT = JOINT		; Number of joints ( quantity)
	WSIZE2 = 1000000-SIZE2	; DRIVE		descending	

	WLOCAT = LOCAT
	WSQFEET = SQFEET
	WPOUNDS = POUNDS
	WSQFLIN = SQFLIN
	WGPRICE = GPRICE
	WLINPRC = LINPRC
	WDPUNCH = DPUNCH
	WDSEQ = DSEQ	

	wd_cngd = dcngd
	if (wd_cngd .eq. 1) changed_lines = 1

	STORE(CHNDUC,DUCWRK,WD_KEY)

	GOTO LD_LOOP
LD_EOF,
	IF(GOTP)
		BEGIN
		FOR I FROM 1 THRU MAXDPT
			BEGIN
			IF(DPTS(I).EQ.'P') EXITLOOP
			IF(DPTS(I).EQ.' ')
				BEGIN
				IF(NUMDPT.GE.MAXDPT)EXITLOOP
				NUMDPT=NUMDPT+1
				DPTS(NUMDPT) = 'P'
				EXITLOOP
				END
			END
		END
	IF(GOTO)
		BEGIN
		FOR I FROM 1 THRU MAXDPT
			BEGIN
			IF(DPTS(I).EQ.'O') EXITLOOP
			IF(DPTS(I).EQ.' ')
				BEGIN
				IF(NUMDPT.GE.MAXDPT)EXITLOOP
				NUMDPT=NUMDPT+1
				DPTS(NUMDPT) = 'O'
				EXITLOOP
				END
			END
		END
	RETURN
;-----------------------------------------------
CONS_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; CONSOLIDATE DUCTWORK
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR MULTLINE, SAVJOINT, SAVFEET, SAVFLIN, SAVLBS
	FIND(CHNDUC,DUCWRK,^FIRST) [ERR=EOF_CD]
	SAV_WD_KEY = '***'

CD_LOOP,
	READS(CHNDUC,DUCWRK,EOF_CD)
	IF(WD_KEY .NE. SAV_WD_KEY)
	THEN	CALL WD_NEWKEY
	ELSE	BEGIN
		INCR MULTLINE
		SAVJOINT = SAVJOINT + WJOINT
		SAVFEET = SAVFEET + WSQFEET
		SAVFLIN = SAVFLIN + WSQFLIN
		SAVLBS = SAVLBS + WPOUNDS
		DELETE(CHNDUC)
		END
	GOTO CD_LOOP		
EOF_CD,
	CALL WD_NEWKEY
	RETURN

WD_NEWKEY,
	IF(SAV_WD_KEY .EQ. '***') GOTO OUT_WDKEY
	IF(MULTLINE .EQ. 0) GOTO OUT_WDKEY
	READ(CHNDUC,DUCWRK,SAV_WD_KEY)
	WJOINT = SAVJOINT
	WSQFEET = SAVFEET
	WPOUNDS = SAVLBS
	WSQFLIN = SAVFLIN
	WRITE(CHNDUC,DUCWRK,SAV_WD_KEY)

OUT_WDKEY,
	SAV_WD_KEY = WD_KEY
	SAVJOINT = WJOINT
	SAVFEET = WSQFEET
	SAVFLIN = WSQFLIN
	SAVLBS = WPOUNDS

	CLEAR MULTLINE
	RETURN
;-----------------------------------------------

WRTMEM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; ROUTINE TO FORMAT AND PRINT WO'S
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Only print dept summary (DPTLOC) info on 2nd pass

	CLEAR PRINTED_O, PRINTED_P
	SAVDPT = -1

	FIND (CHNOUT, LINE, ^FIRST) [ERR=W_EOF]
W_LOOP,
	READS (CHNOUT, LINE, W_EOF)

	USING WDEPT(1,1) SELECT
	('F','H','K'),	IF (WDEPT .NE. SAVDPT) CALL W_NEWDPT
	('IC'),		IF (WDEPT .NE. SAVDPT) CALL W_NEWDPT
	(),		IF (WDEPT(1,1) .NE. SAVDPT(1,1)) CALL W_NEWDPT
	ENDUSING

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	USING W_PASS SELECT
;;;	(1),	IF (W_PRINT .NE. 'Y') GOTO W_LOOP	;this prints at the bottom
;;;	(2),	 GOTO W_LOOP				;detail only prints on first pass
;;;	ENDUSING

	IF (W_PRINT .EQ. 'N')	GOTO W_LOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (WTYPE .EQ. 'M')
	THEN	BEGIN
		bold_on = 1
		IF (MEMO_ON .EQ. 0)
			BEGIN
			CLEAR PLINE
			CALL PRINT
			MEMO_ON = 1
			END
		IF (MITEM.EQ.'   F1') 
		THEN	BEGIN
			using f1_key(wseq1) select
			(61),	isidx = 8	;8, Green
			(64),	isidx = 5	;5, Orange
			(65),	isidx = 3	;3, Yellow
			(66),	isidx = 0	;0, Blue
			(68),	isidx = 2	;2, Pink
			(80),	isidx = 4	;4, Purple
			(81),	isidx = 6	;6, Gray
			(82),	isidx = 8	;7, green stevem 12-12-13
		;;;	(82),	isidx = 7	;7, Gold
			(83),	isidx = 9	;9, Beige
			(),	isidx = 1	;1, black
			endusing

			display (14, idx_sel)
			FOR J FROM 1 THRU 3
				BEGIN
				TMPDSC = MMEMO(J)
				CALL MAIN_MEMO
				PLINE (8,54) = TMPDSC
				IF (MMEMO(J) .NE. BLANKS) CALL PRINT
				END
			isidx = 1	;1 => black
			display (14, idx_sel)
			END
		ELSE	BEGIN
			XCALL HP (14, hpFONT, hpBOLD)	
			prow = lincnt + 3
			isidx = 0	;0, blue
			display (14, idx_sel)
			xcall hp (14,hpPOS, prow, 7, 0, m_long(1,30))
			isidx = 1	;1, black
			display (14, idx_sel)
			xcall hp (14,hpPOS, prow, 38, 0, m_long(31,63))
			clear pline
			CALL PRINT
			isidx = 1	;color 1, black
			display (14, idx_sel)
			XCALL HP (14, hpFONT, hpMEDIUM)	
			GOTO W_LOOP
			END

		END
	ELSE	BEGIN
		using wum select
		('RL','BG','BX'), bold_on = 1
		(),		bold_on = 0
		endusing


		if (wcngd) bold_on = 1	;7-23-09

		MEMO_ON = 0
		CLEAR PLINE
		PLINE(1,7) = WQTY,	'ZZ,ZZX-'
		PLINE(9,23) = WITEM
		PLINE(25,54) = WDESC
		PLINE(57,58) = WUM
		IF (WTAG)
			BEGIN
			d_long = f4_memol(wtag)
			PLINE (59,64) = BOLD
		;;;	PLINE (65,79) = F4_MEMOS(WTAG)
		;;;	pline (65,79) = dl_ara(1)
			pline (65,85) = dl_ara(1)
			if(dl_ara(2).ne.blanks)
				begin
				call print	;current line
				pline(50,79) = dl_ara(2)
				call print
				if (dl_ara(3).ne.blanks)
					begin
					pline(50,79) = dl_ara(3)
					call print
					end
				end

			END
		CALL PRINT

		bold_on = 0
		IF (WTAG .or. wcngd) 
			begin
			DISPLAY(14,MEDIUM)	;SSQ 12-26-07
			end
		END

	GOTO W_LOOP

;   QTY  ITEM NUMBER     DESCRIPTION                     UM
;ZZZZZZ  AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA  AA
;1234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
;====================================================
MAIN_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;
	TL = %TRIM(TMPDSC)
	REM = 46 - TL
	REM = REM/2
	IF (REM.GT.0)
		BEGIN
		CLEAR TMPDSC
		TMPDSC(1,REM) = DASHES
		TMPDSC(REM+1,REM+TL) = MMEMO(J)
		TMPDSC(REM+1+TL,46) = DASHES
		END
	RETURN
;-----------------------------------------
W_EOF,

	RETURN
;===============================================

WD_NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; FOR DUCT
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'DD'
	TBLKEY = WD_DEPT
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		DD_Desc = "* NOT ON FILE *"
		END

	CLEAR DPTLOC
	L_ORDN = ORDNO
	L_DEPT = WD_DEPT
	XCALL ISIO (CHN194, DPTLOC, L_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR DPTLOC
		L_PRT = 'N'
		END
	W_PRINT = L_PRT


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (W_PRINT .EQ. 'N') GOTO WDD_END

;;;	USING W_PASS SELECT
;;;	(1),	IF (W_PRINT.NE.'Y') GOTO WDD_END
;;;	(2),	BEGIN
;;;		if (wd_dept.eq.'O' .and. printed_o) goto wdd_end
;;;		if (wd_dept.eq.'P' .and. printed_p) goto wdd_end
;;;		if (from_o .or. from_p) goto wdd_end		;would have already printed bottom at wrtmem:w_newdpt
;;;		lin_dpt = wdept
;;;		wdept = wd_dept
;;;		CALL PRINT_BOT
;;;		wdept = lin_dpt
;;;		GOTO WDD_END
;;;		END
;;;	ENDUSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	XCALL FILL ('=', PLINE(8,53))
	PLINE (22,37) = ' DUCTWORK DEPT '
	pline (38,39) = wd_dept
	SAV_WDP = WD_DEPT
	display (14, bold)

	CALL PRINT
	display (14, medium)

WDD_END,
	SAV_WDP = WD_DEPT

	RETURN
;--------------------------------------------------------------

W_NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR FROM_O, FROM_P

;;;	USING SAVDPT SELECT			;5-17-17
	USING WDEPT SELECT			;5-17-17
	('O'),		BEGIN
			FROM_O = 1
			CALL GETDUC
			PRINTED_O = 1
			END
	('P'),		BEGIN
			FROM_P = 1
			CALL GETDUC
			PRINTED_P = 1
			END
	ENDUSING

	CLEAR TBL_KEY
	TBLCOD = 'DD'
	TBLKEY = WDEPT
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		DD_Desc = "* NOT ON FILE *"
		END


	CLEAR DPTLOC
	L_ORDN = ORDNO
	L_DEPT = WDEPT
	XCALL ISIO (CHN194, DPTLOC, L_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		begin
 		CLEAR DPTLOC
		L_PRT = 'N'		;MUST DEFAULT TO "N"
		END

	W_PRINT = L_PRT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (W_PRINT .EQ. 'N') GOTO WD_END

;;;	USING W_PASS SELECT
;;;	(1),	IF (W_PRINT.NE.'Y') GOTO WD_END
;;;	(2),	BEGIN
;;;		CALL PRINT_BOT
;;;		GOTO WD_END
;;;		END
;;;	ENDUSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



	call print		;8-10-09
	display (14, bold)
	PLINE(27,28) = WDEPT
	PLINE(30,30) = '-'
	PLINE(32,80) = dd_DESC
	CALL PRINT
	display (14, medium)
WD_END,
	SAVDPT = WDEPT
	RETURN
;------------------------------------

LOAD_BOT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FOR I FROM 1 THRU MAXDPT
		BEGIN
		IF (DPTS(I) .EQ. '  ') EXITLOOP
			CLEAR DPTLOC
			L_ORDN = ORDNO
			L_DEPT = DPTS(I)
			XCALL ISIO (CHN194, DPTLOC, L_KEY, READ, LOKCTL)
			IF (LOKCTL .NE. 0)
				BEGIN
				CLEAR DPTLOC
				L_PRT = 'N'
				END
		B_DEPT = DPTS(I)
		B_PRT = L_PRT
		IF (B_PRT .EQ. 'Y') B_PWI = 0	;SORT TO TOP
		IF (%INSTR(1,L_LOC, 'PACKED W/') .OR. %INSTR(1,L_LOC, 'INSTALLED') )
		THEN	B_PWI = 2	;SORT TO BOTTOM
		ELSE	B_PWI = 1	;SORT TO TOP
	
		STORE (CHNBOT, BOTWRK, B_KEY)
		END

	RETURN
;----------------------------------------------------
	
PRINT_BOT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PRINT DEPT SUMMARY (DPTLOC)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	printing_bot = 1
	find (chnbot, botwrk, ^first) [err=bot_eof]			;nothing to print


;;;	printing_bot = 1						;prthdr needs this
;;;	if (bot_first .eq. 0 .and. pag.eq.1)
;;;		begin
		lincnt = lincnt + 3
		XCALL HP (14, hpPOS,Lincnt,0, hpHLINE,80)		;first horiz line
		lincnt = lincnt + 1
		XCALL HP (14, hpPOS,Lincnt,1,0, bot_lin)		;first horiz line

;;;		incr bot_first
;;;		end

BOT_LOOP,
	READS (CHNBOT, BOTWRK, BOT_EOF)

	CLEAR TBL_KEY
	TBLCOD = 'DD'
	TBLKEY = B_DEPT
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		DD_Desc = "* NOT ON FILE *"
		END

	S_ORDN = ORDNO
	S_DEPT = B_DEPT
	XCALL ISIO (CHN193, DPTSTS, S_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR DPTSTS

	CLEAR DPTLOC
	L_ORDN = ORDNO
	L_DEPT = b_DEPT
	XCALL ISIO (CHN194, DPTLOC, L_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR DPTLOC


	if (l_loc .eq. blanks) l_loc = s_comm		;if no staging info

	call print		;8-10-09

	if (lincnt .ge. 53) lincnt = 55	;if at bottom force new page before printing
	display (14, bold)
	PLINE(1,2) = B_DEPT
	PLINE(3,3) = '-'
	PLINE(4,33) = dd_DESC
	IF (S_STAT .LE. 0) PLINE (27,30) = 'OPEN'
	dptlin = pline							;save for new page

	CALL PRINT
	display (14, medium)


PB_LOOP,
	IF (B_PRT .EQ. 'Y')
		BEGIN
		PLINE = 'SEE TOP OF MAIN COPY'
		END
	IF (S_STAT .GT. 0)
		BEGIN
		PLINE(31,36) = L_QTY,	'ZZZZZX'
		PLINE(38,41) = L_UM
		PLINE(43,62) = L_LOC
		PLINE(64,66) = L_SIZ(1), 'ZZZ'
		PLINE(68,70) = L_SIZ(2), 'ZZZ'
		PLINE(72,74) = L_SIZ(3), 'ZZZ'
		PLINE(77,80) = L_WT,	'ZZZX'
		END


	CALL PRINT

	XCALL IOS (CHN194, DPTLOC, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO PB_END
	IF (L_ORDN .NE. ORDNO) GOTO PB_END
	IF (L_DEPT .NE. B_DEPT) GOTO PB_END
	GOTO PB_LOOP

PB_END,
	XCALL HP (14, hpPOS,Lincnt,0, hpHLINE,80)

	GOTO BOT_LOOP
BOT_EOF,

		
	RETURN
;----------------------------------------------------

GETDUC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PROCESS DUCTWORK
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (printed_o .and. printed_p) return
	sav_wdp = -1

	find (chnduc, ducwrk, ^first) [err=nxtduc]
NXTDUC,
	READS(CHNDUC,DUCWRK,GETDUC_EOF) [ERR=GETDUC_EOF]

	USING WD_DEPT SELECT
	('O'),	BEGIN
		IF (PRINTED_O) GOTO NXTDUC		;ALREADY PRINTED DEPT "O"
		IF (FROM_P) GOTO NXTDUC			;FROM WRTMEM:P
		END
	('P'),	BEGIN
		IF (PRINTED_P) GOTO NXTDUC		;ALREADY PRINTED DEPT "P"
		IF (FROM_O) GOTO NXTDUC			;FROM WRTMEM:O
		END
	ENDUSING


	IF (WD_DEPT.NE.SAV_WDP) call wd_newdpt		;4-12-17


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (W_PRINT .EQ. 'N') GOTO NXTDUC

;;;	USING W_PASS SELECT
;;;	(1),	IF (W_PRINT .NE. 'Y') GOTO NXTDUC	;this prints at the bottom
;;;	(2),	 GOTO NXTDUC				;detail only prints on first pass
;;;	ENDUSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CONDUC,
	WSTY = 10 - WSTY
	WSIZE3 = 100 - WSIZE3
	WSIZE2 = 1000000 - WSIZE2
	WSIZE1 = 1000000 - WSIZE1

	SZ1 = %TRN3(WSIZE1)
	SZ2 = %TRN3(WSIZE2)

	CALL PRTDUC
SUMDUC,
	CONFIG(1,3) = WGAUGE
	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3) CONFIG(1,3) = WTHICK	;ssq 3-21-06
;;;	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3) CONFIG(1,3) = THICK	;ssq 3-21-06
	CONFIG(4,4) = WDUTYPE
	CONFIG(5,5) = WCAT
	CONFIG(6,6) = WSTY
	CONFIG(7,7) = WSEAM

		; Check to see if the configuration has all ready 
		; been on this order
	I =
	DO BEGIN
	  INCR I
	  DUCREC = SDUCRC(I)
	END
	UNTIL (DUCCFG.EQ.CONFIG.OR.DUCCFG.EQ.0.OR.I.GE.MAXDUC)
	IF (I.GE.MAXDUC) GOTO BADCON

	DUCCFG = CONFIG
	DUCSQF = DUCSQF + WSQFEET
	DUCPND = DUCPND + WPOUNDS
	DUCGPR = WGPRICE
	SDUCRC(I) = DUCREC

	IF (WLINER.GE.1.AND.WLINER.LE.9.AND.WLINER.NE.4)
&		LINSQF(WLINER) = LINSQF(WLINER) + WSQFLIN

	GOTO NXTDUC

GETDUC_EOF,
	CALL PRTSDU
NO_DUCT,
	RETURN
;---------------------------------------------------

BADCON,
	FORMS (14,2)
	PLINE (32,60) = 'DUCT INFORMATION IS '	
	CALL PRINT
	GOTO ERRMSG
ERRMSG,
	PLINE (32,60) = 'PRESENTLY OVERFLOWING THE'
	CALL PRINT
	PLINE (32,60) = 'ARRAY BOUNDARIES.  PLEASE CALL'
	CALL PRINT
	PLINE (32,60) = 'SOFTWARE SUPPORT TO CORRECT THE'
	CALL PRINT
	PLINE (32,60) = 'PROBLEM'
	CALL PRINT
	RETURN
;---------------------------------------------------

PRTDUC,		;Print duct record


	IF ((LINCNT+3).GT.MAXLIN) CALL PRTHDR
	if (wd_cngd) bold_on = 1
	PLINE (1,7) = WJOINT,NUMASK
	PLINE (8,9) = 'JT'
	IF (WDUTYPE.NE.2.AND.WDUTYPE.NE.3)
	BEGIN
	  PLINE (13,14) = WGAUGE,'XX'
	  PLINE (15,16) = 'GA'
	END
	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3)
	BEGIN
	  PLINE (13,16) = WTHICK,'.XXX'
	END
	IF(WSIZE1/1000.EQ.SZ1 .AND.WSIZE2/1000.EQ.SZ2)
	THEN	BEGIN
		PLINE (18,20) = SZ1,'ZZX'
		PLINE (21,21) = 'X'
		PLINE (22,24) = SZ2,'ZZX'
		PLINE (25,25) = 'X'
		PLINE (26,27) = WSIZE3,'ZX'
		PLINE (31,34) = DTYPE(WDUTYPE)
		PLINE (36,41) = DCAT(WCAT)
		PLINE (43,45) = DSTY(WSTY)
		PLINE (48,50) = DSEAM(WSEAM)
		IF (WSEAL.EQ.1) PLINE (52,55) = 'SEAL'
		END
	ELSE	BEGIN
		PLINE (18,24) = WSIZE1,'ZZX.XXX'
		PLINE (25,25) = 'X'
		PLINE (26,32) = WSIZE2,'ZZX.XXX'
		PLINE (33,33) = 'X'
		PLINE (34,35) = WSIZE3,'ZX'
		PLINE (39,42) = DTYPE(WDUTYPE)
		PLINE (45,50) = DCAT(WCAT)
		PLINE (52,54) = DSTY(WSTY)
		PLINE (57,59) = DSEAM(WSEAM)
		IF (WSEAL.EQ.1) PLINE (61,64) = 'SEAL'
		END
	CALL PRINT

	PLINE (  1,7  ) = WPOUNDS,NUMASK
	PLINE (  8,8  ) = '#'
	PLINE ( 12,17 ) = WSQFLIN,NUMASK
	PLINE ( 19,23 ) = 'SQ FT'
	PLINE (30,44) = '     NO LINER '
;;;	IF (WLINER.GE.1.AND.WLINER.LE.8.AND.WLINER.NE.4)
	IF (WLINER.GE.1.AND.WLINER.LE.9.AND.WLINER.NE.4)
	BEGIN
	  PLINE (31,37) = DLINER( WLINER )
	  PLINE (38,42) = 'LINER'
	END
	PLINE (43,54) = WLOCAT
	CALL PRINT

	bold_on = 0

	CALL PRINT
	RETURN


PRTSDU,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;Print summarized duct from this order
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF ((LINCNT+1).GT.MAXLIN) CALL PRTHDR
	INCR LINCNT
	FOR I FROM 1 THRU MAXDUC
	  BEGIN
	  DUCREC = SDUCRC(I)
	  IF (DUCCFG.EQ.0) GOTO PRTSD2
	  IF ((LINCNT+1).GT.MAXLIN) CALL PRTHDR
	  PLINE (1,7) = DUCPND,NUMASK
	  PLINE (8,8) = '#'
	  PLINE (18,19) = DUCCFG(1,3),'XX'
	  PLINE (21,22) = 'GA'
	  IF (DUCCFG(4,4).EQ.2.OR.DUCCFG(4,4).EQ.3)
	  BEGIN
	    PLINE (18,22) = DUCCFG(1,3),'.XXX'
	  END
	  PLINE (33,36) = DTYPE(DUCCFG(4,4))
	  PLINE (38,43) = DCAT(DUCCFG(5,5))
	  PLINE (45,47) = DSTY(DUCCFG(6,6))
	  PLINE (49,51) = DSEAM(DUCCFG(7,7))
	  CALL PRINT
	  SDUCRC(I) =
	END
PRTSD2,
	FOR I FROM 1 THRU 9
		BEGIN
		IF (LINSQF(I).NE.0.AND.I.NE.4)
			BEGIN
			IF ((LINCNT+1).GT.MAXLIN) CALL PRTHDR
			PLINE (1,7) = LINSQF(I),NUMASK
			PLINE (8,10) = 'SQF'
			PLINE (33,39) = DLINER(I)
			PLINE (40,45) = ' LINER'
			CALL PRINT
			LINSQF(I) =
			END
		END
	RETURN	
;---------------------------------------------

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INCR LINCNT
;;;	IF (LINCNT .GT. 56) 
	IF (LINCNT .GT. 53) 	;2-6-14 paklin
		BEGIN
		SAVLIN = PLINE
		sav_bold = bold_on
		CALL PRTHDR
		PLINE = SAVLIN
		bold_on = sav_bold
		incr lincnt			;8-17-09

		if (printing_bot)
			begin
			writes (14, bot_lin)
			display (14, bold)
			writes (14,dptlin)
			display (14, medium)
			lincnt = lincnt + 2
			end
		END


	if (bold_on .eq. 1) 
		begin
		display(14, bold)
		end

	WRITES (14, PLINE)

	if (bold_on .eq. 1) 
		begin
		display(14, medium)
		end

	CLEAR PLINE
	RETURN
PRINT2,
	INCR LINCNT
	WRITES (14, PLINE)
	CLEAR PLINE
	RETURN
;-----------------------------------------------------

PRTHDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear bot_first			;to position dptsts detail correctly

	IF (FIRST_PAGE .GT. 1) 
		BEGIN
		writes (14, ' ')	;2-6-14
		display(14, EJECT)
		END


	FIRST_PAGE = 2

	CLEAR LINCNT
	CLEAR PLINE
	
	OF_ORD = OORDNO,	'ZZZZZX'
	IF(OPRTF.LE.0)OPRTF=0
	OF_NUM = OPRTF

	IF (LOCAL)
	THEN	OF_LOC = '*'
	ELSE	OF_LOC =

	xcall rbox (6, 0, ldig)	;ldig is last dig of order (ord_fmt)
;	IF (OXPRS .EQ. 'Y') xcall exprs (6,1)	;print "EXPRESS" in read under rbox
	xcall hp (14,hpPOS, 1, 1, 0, ' ')

	display(14, medium)

	DT = %DATETIME
	D = DT(1,8)
	T = DT(9,14)
	DATE = D,	'XXXX-XX-XX'
	TIME = T,	'ZX:XX:XX'
	PLINE = DATIME

	PLINE(23,60) = OCOMNT(1)
	CALL PRINT2

	PLINE(25,60) = OCOMNT(2)
	CALL PRINT2

	PLINE(28,63) = c2_com

	PLINE(1,20) = B_STRNG

	call print2

	CALL PRINT2
	XCALL OF128
	

;added 9-6-16	----------------------------
	using cmpcod select
	('SMC'),	begin
			XCALL HP (14, hpFONT, hpBOLD+hp6CPI)
			isidx = 4	;red 
			display (14, idx_sel)
			display (14, '5850 / 5900')
			isidx =1 	;black
			display (14,idx_sel)
			xcall HP (14, hpFONT,hpMEDIUM+hp10CPI)
			end
	endusing

;------------------------------------------

	display (14, medium)

;;;	IF (OXPRS .EQ. 'Y')
;;;	IF (OXPRS.EQ.'Y' .OR. OXSTK.EQ.'Y') 			;7-20-20
	IF (OXPRS.EQ.'Y' .OR. OXSTK.EQ.'Y' .OR. OXPRF.EQ.'Y') 			;12-30-20
			begin	
			isidx = 4	;red
			display (14, idx_sel)
			display(14, bold)
			end

	using cmpcod select
	('SMC'),	BEGIN
			PLINE(20,44) = '** COVER SHEET **'	;9-6-16
			PLINE(54,63) = ORDFMT			;9-6-16
			END

	(),		BEGIN
			PLINE(28,52) = '** COVER SHEET **'
			PLINE(62,71) = ORDFMT		;12-14-09
			END
	endusing

	writes (14, pline)
;;;	IF (OXPRS .EQ. 'Y')
;;;	IF (OXPRS.EQ.'Y' .OR. OXSTK.EQ.'Y') 			;7-20-20
	IF (OXPRS.EQ.'Y' .OR. OXSTK.EQ.'Y' .OR. OXPRF.EQ.'Y') 			;12-30-20
			begin	
			isidx = 1	;black
			display (14, idx_sel)
			display(14, medium)
			end

	incr lincnt
	clear pline

	FACT = 22
	FOR J FROM 1 THRU NUMDPT  
		BEGIN
		PLINE(FACT, FACT+1) = DPTS(J)
		FACT = FACT + 3
		END
	IF (ROCKON) PLINE(FACT,FACT+4) = '[SMC]'

	CALL PRINT

	INCR PAG
	INCR TPAG

	P1 = PAG,	'ZX'
	P2 = NPAG,	'ZZ' [LEFT]
; later...
	PLINE(61,69) = PAGEOF
	CALL PRINT2
;........................

	PLINE(10,15) = OCUSNO,	'ZZZZZZ' [LEFT]
	PLINE(20,25) = CRDLMT,	'ZZZZZZ' [LEFT]	;3-29-00
	CALL PRINT2

	PLINE(1,8) = 'BILL TO:'
	PLINE(43,50) = 'SHIP TO:'
	PLINE(10,39) = NAME
	PLINE(52,80) = OSHPNM
	CALL PRINT2

	PLINE(10,39) = ADD1
	PLINE(52,80) = OSHAD1
	CALL PRINT2

	PLINE(10,39) = ADD2
	PLINE(52,80) = OSHAD2
	CALL PRINT2

	PLINE(10,39) = CSZ
	PLINE(52,80) = OSHAD3
	CALL PRINT2

	FORMS (14,2)
	WRITES (14, H_LINE)
	PLINE(3,12) = E_DATE
;;;	PLINE(17,26) = SLSNM
	PLINE(15,26) = SNAM

;;;	PLINE(29,38) = OPONO
	XPOL1 = OPONO
	XPOL2 =
	IF (OMETRO .NE. BLANKS)
		BEGIN
		XPOL1 = OMETRO
		XPOL2 = OPONO
		END
	PLINE(28,39) = XPOL1

	PLINE(41,50) = OJOBNO
;;;	PLINE(53,67) = SV_NAME
	PLINE(53,64) = SV_NAME

	USING OCLPPD SELECT
	('C'),	PLINE(66,68)='COL'
	('P'),	PLINE(66,68)='PPD'
	('D'),	PLINE(66,68)='COD'
	ENDUSING

	PLINE(70,79) = SH_DATE
	CALL PRINT2

	PLINE(28,39) = XPOL2
	CALL PRINT2

	if (.not. printing_bot)
		begin
		FORMS(14,1)
		WRITES (14, H_LINE2)
		end

	LINCNT = 17
	RETURN

;DATE ENTERED    SLS-REP     PO NUMBER   JOB NUMBER  SHIP VIA         SHIP DATE
;  XX/XX/XXXX    AAAAAAAAAA  AAAAAAAAAA  AAAAAAAAAA  AAAAAAAAAAAAAAA  XX/XX/XXXX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
;-----------------------------------------------

GET_SCAC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; COP TABLE SCAC LOOK-UP
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'SC'
	TBLKEY = OSCAC
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		SC_NAME = "* NOT ON FILE *"
		END
	SV_NAME = SC_NAME
	RETURN
;-----------------------------------------------------

GET_SALESMAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL SREP(OSLMAN,LNAM,SNAM,SINT)
	RETURN
;-----------------------------------------------------

GET_DEPTS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FIND (CHN045, ORDLIN, OORDNO) [ERR=NXTLIN]
NXTLIN,
	XCALL IOS (CHN045, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF_LIN
	IF (LORDNO .NE. OORDNO) GOTO EOF_LIN
	IF (LROCPO .EQ. 9) 	;SSQ 4-27-04
		BEGIN
		ROCKON = 1		
	;;;	GOTO NXTLIN
		END
	IF (LTYPE .EQ. 'M') GOTO NXTLIN		;SKIP MEMOS

	CLEAR TBL_KEY		;SSQ 11-9-04
	TBLCOD = 'EX'
	TBLKEY = EX_ITEM = LITMNO
	FIND (CHN182,COPTBL,TBL_KEY) [ERR=LCONT]	;continue if NOT in table
	GOTO NXTLIN
LCONT,
	CLEAR I

	XDEPT = LDEPT(1,1)		;1ST CHAR OF DEPT	
;;;8-19-19>	CALL CHECK_VULCAN
;;;8-19-19>	IF (IS_VULCAN) XDEPT(2,2) = 'V'	;For send to vulcan
	if (ldept.eq.'C' .and. %is_ic(ordlin)) xdept = 'IC'
NLI,
	INCR I
;;;	IF (DPTS(I) .EQ. LDEPT(1,1)) GOTO NXTLIN
	IF (DPTS(I) .EQ. XDEPT) GOTO NXTLIN	;10-02-08
	IF (DPTS(I) .EQ. '  ') 
		BEGIN
	;;;	DPTS(I) = LDEPT(1,1)	;FIRST CHAR ONLY 3-31-00
		DPTS(I) = XDEPT		;FIRST CHAR ONLY, UNLESS VULCAN
		NUMDPT = I
		GOTO NXTLIN
		END
	IF (I .LT. MAXDPT) GOTO NLI

EOF_LIN,
	RETURN
;---------------------------------------------

CHECK_VULCAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 7-29-13 use routine cvcan in all routines for consistency
	xcall cvcan (ordlin, ocusno, chn182, is_vulcan)	;7-29-13
	return


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	CHN135 = 35		;5-28-15 SSQ opened in calling routine

	SWITCH = 5
	XCALL CHNOPN (24,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (24,'SI',044,SWITCH)
		END
	CHN044 = 24

	XCALL CHNOPN (25,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (25,'SI',045,SWITCH)
		END
	CHN045 = 25

	XCALL CHNOPN (26,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (26,'SI',175,SWITCH)
		END
	CHN175 = 26

	XCALL CHNOPN (36,STAT)
	IF (STAT .EQ. 0) 
		BEGIN
		XCALL FILES (36,'SI',001,SWITCH)
		END
	CHN001 = 36

;;;	XCALL CHNOPN (37,STAT)
;;;	IF (STAT .EQ. 0)
;;;		BEGIN
;;;		XCALL FILES (37,'I',002,SWITCH)
;;;		END
;;;	CHN002 = 37


	XCALL CHNOPN(38,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (38,'I',054,SWITCH)
		END
	CHN054 = 38

	CHN182 = 17	;COP TABLES

	XCALL ISIO (CHN044, ORDHDR, ORDNO, READ, LOKCTL)
	WFORD = ORDNO,'XXXXXX'
	XCALL DATE8(OORDDT, D_OUT, D_OUTR, E_DATE, D_SW)
;;;	XCALL DATE8(OSHDAT, D_OUT, D_OUTR, S_DATE, D_SW)
	XCALL DATE8(OPROMD, D_OUT, D_OUTR, SH_DATE, D_SW)

	CALL GET_SCAC
	CALL GET_SALESMAN

	XCALL ISIO (CHN001, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CUSMAS =
		NAME = '* CUSTOMER NOT ON FILE *'
		END
	
;;;	XCALL IO (CHN001, DUMCUS, 1, READ, LOKCTL)

;;;	KEY = OCUSNO,'XXXXXX'
;;;	XCALL SERCH (CHN002,CUSIDX,KEY,1,6,ORG001,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	LOKCTL = 1
;;;	CASE SRCCTL OF 
;;;	BEGINCASE
;;;	  0:	XCALL IO (CHN001,CUSMAS,IRC001,READ,LOKCTL)
;;;	  1:	BEGIN
;;;		CUSMAS =
;;;		NAME = '* CUSTOMER NOT ON FILE *'
;;;		END
;;;	ENDCASE

	CSZ(1,15) = CITY
	CSZ(17,18) = STATE
	CSZ(20,29) = ZIP

;Create work file...
	XCALL ISAMC (WRKFIL, 128, 1, KEY_SPEC)
	OPEN (33, SU, WRKFIL)
	CHNWRK = 33

	OFORD = ORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (OUTFIL, 128, 1, KEY_SPEC)
	OPEN (34, SU, OUTFIL)
	CHNOUT = 34

	BTORD = ORDNO,	'XXXXXX'
	XCALL ISAMC (BOTFIL, 10, 1, 'START=1, LENGTH=3, NODUPS, ASCEND')
	OPEN (42, SU, BOTFIL)
	CHNBOT = 42


	WRORD = ORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (39, SU, DUCFIL)
	CHNDUC = 39

	chnic = %SYN_FREECHN(50,99)		;1-29-20
	OCORD = OORDNO, 'XXXXXX'
	XCALL ISAMC (ICFIL, 21, 1, 'START=1, LENGTH=15, NODUPS, ASCEND')
	OPEN (CHNIC, SU, ICFIL)


	SWITCH = 5
	XCALL FILES (93,'SI', 193, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN193 = 93

; opened in calling routine...
;;;	SWITCH = 5
;;;	XCALL FILES (94,'SI',194, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
	CHN194 = 94

	SPORD = ORDNO,'XXXXXX'

	OPNOK = 1

	RETURN
;-------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN044
	CLOSE CHN045
	CLOSE CHN175

	CLOSE CHN001
;;;	CLOSE CHN002
	CLOSE CHN054

	IF (CHN193) CLOSE CHN193
;;;	IF (CHN194) CLOSE CHN194
	if (chnic) close chnic
	RETURN

colors,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
	set re, e1,e2,e5 = e_char
	
	display (14, reset)

; set the color palettes...
	xcall ascii (0, hex0)
	xcall ascii (1, hex1)
	xcall ascii (8, hex8)
	xcall ascii (2, hex2)	;8-07-09

	set lx(1) = hex2	;color space = sRGB
	set lx(2) = hex1
	set lx(3) = hex8
	set lx(4), lx(5), lx(6) = hex0
	
	writes (14,l1)


	clear tbl_key
	tblcod = 'CL'
	find (chn182, coptbl, tbl_key) [err=color_loop]
color_loop,
	xcall ios (chn182, coptbl, read, lokctl)
	if (lokctl .ne. 0) goto eof_color
	if (tblcod .ne. 'CL') goto eof_color	

	ipidx = cl_num
	ipcR = cl_red
	ipcG = cl_grn
	ipcB = cl_blu
	display (14,idx_pal)
	
	goto color_loop

eof_color,

;--------------------------------------
	XCALL HP (14,hpDOTS, 0)

	B_DATA(1,2) = 'S~'
	B_DATA(3,8) = OORDNO, 'XXXXXX'
	XCALL B128(B_DATA, B_STRNG, 70)
	return
;-----------------------------------------------------------

;------------------------------------------------

READ_LINER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; GET LINER DESCRIPTIONS FROM LINER PRICE TABLE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR COPTBL
	TBLCOD = 'LP'
	FOR I FROM 1 THRU 9
		BEGIN
		LP_KEY = I
		XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
		IF (TBLCOD .NE. 'LP') EXITLOOP
		DLINER(I) = lp_liner
		END

	RETURN
;--------------------------------------------------

c_lbs,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; compute weight for c dept items...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; scrnx figures lpwgt * length, don't do it again here...
	c_wgt = lpwgt*lqtyor

	if (lpwnw) c_wgt = c_wgt * 100		;lpwgt is ZZZ,ZZX instead of Z,ZZX.XX

	using lf1 select
	(80,81),	c_wgt = (c_wgt*230)#2
	(82,83),	c_wgt = (c_wgt*270)#2
	(66),		c_wgt = (c_wgt*35)#2
	endusing

	if (litmno .eq. 'SPCC') lqtyor = 0
	if (%instr(1,ldescr,'NET COST') ) lqtyor = 0

	return
;-----------------------------------------------------

	END


