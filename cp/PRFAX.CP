SUBROUTINE PRFAX
;;;	OR_NUMBER	,D
	ORDNO		,D
	WND_1		,D		;PARENT WINDOW
	F_INFO		,A		;FAXINFO
	F_SEND		,D		;0 = ABORT FAX

	.INCLUDE 'DEF:WINDOWS.DEF'
;DDEPT 5-29-14

; Create a Quote output file in Rich-text format.
;
;
;	1-25-05 ssq: lines are wrapping, trim by 1 char (77 to 76).
; 	5-30-18 ssq: make cusmas isam
;
EXTERNAL FUNCTION
	TRN3	,D

.include	'def:rtf.def'

RECORD	COINFO
	.INCLUDE 'DEF:COINFO.DEF'

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


record	pvars
	P_SELECTED	,D1
	stats		,i4
	alpha	,a10

RECORD	SUBJECT
		,A*,	'::S=Quotation #'
	S_QUOTE	,D6

RECORD	FAXINFO
	.INCLUDE 'DEF:FAXINFO.DEF'


RECORD	O_FILE
;;;		,a*,	'spl:'
		,a*,	'\\server-hv01\de63\faxscan\'
		,A1,	'W'
	O_ORDER	,D6
	O_EXT	,A4,	'.txt'


record	buf_line
	BL_CUST	,A6
		,A5
		,A4,	'{\b '		;bold
	BL_REP	,A17
		,A1,	'}'
		,A1
	BL_JOB	,A10
		,A6
	BL_FOB	,A23
		,A1
		,A4,	'{\b '		;bold
	BL_FAB_TIME	,A9
		,A1,	'}'
		,A2
	BL_TERMS	,A6

RECORD CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'
RECORD CUSCTL	,X
	.INCLUDE 'DEF:RD001B.DEF'
;;;RECORD CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'
RECORD	SALMAN
	.INCLUDE 'DEF:RD054A.DEF'

RECORD ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD ORDLIN
	.INCLUDE 'DEF:RD045A.def'
RECORD,X
	.INCLUDE 'DEF:RD045M.def'
RECORD,X
	.INCLUDE 'DEF:RD045D.def'

;;;RECORD	ORDLIN1
;;;	.INCLUDE 'DEF:RD045X.new'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD PARAM
	.INCLUDE 'DEF:PARAM2.DEF'

GLOBAL DUCK
	.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL

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
RECORD	SPLFIL
		,A4,	'SPL:'
		,A1,	'S'
	SPORD	,A6
		,A4,	'.ISM'

RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.DEF'

RECORD	DUCFIL
		,A4,	'SPL:'
		,A1,	'D'
	WRORD	,A6
		,A4,	'.ISM'

RECORD	F_MEMOS
	.INCLUDE 'def:CPMEMO.DEF'

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

RECORD	SAVE_LINE
;;;		,A132	;SAME LENGTH AS LINE
		,A133	;SAME LENGTH AS LINE

;---------------------------
RECORD	FOR_ISAMC
	KEYSPEC,	A*,	'START=1:117, LEN=56:8, DUPS, ASCEND'

RECORD	LINE			
	WSS	,D1
	WSEQ1	,D2		;reversed
	WSEQ2	,D2
	WSEQ3	,D2	
	WDEPT	,A2
	WSQNO	,A2
	WITEM	,A15
	WDESC	,A90
	WPRICE	,D8
	WTYPE	,A1		;L=LINE, M=MEMO
	WQTY	,D5
	WUM	,A2
	WCPFLG	,D1
RECORD	MEMO,X
		,D1
	MSEQ	,D6
	MDEPT	,A2
	MSQNO	,A2
	MITEM	,A15
	MMEMO	,3A30
		,D8
		,A1
		,D5
		,A2
		,D1
RECORD	,X
		,D1
		,D6
		,A2
		,A2
		,A15
	M_LONG	,A63
		,A27
		,D8
		,A1
		,D5
		,A2
		,D1
RECORD,X
	W_KEYA	,A56		;FIRST PART OF KEY
		,A60
	W_KEYB	,A8		;PRICE
RECORD,X
		,D1
		,D2
	COMSEQ	,D4
		,A2		;reversed
;---------------------------
RECORD	LINE2			
	WSS2	,D1
	WSEQ12	,D2		
	WSEQ22	,D2
	WSEQ32	,D2	;REVERSE
	WDEPT2	,A2
	WSQNO2	,A2
	WITEM2	,A15
	WDESC2	,A30
	WADD2	,A60	;FILLER
	WPRICE2	,D8
	WTYPE2	,A1		;L=LINE, M=MEMO
	WQTY2	,D5
	WUM2	,A2
	WCPFLGW	,D1
RECORD,X
	W_KEY2A	,A56		;FIRST PART OF KEY
		,A60
	W_KEY2B	,A8		;PRICE

RECORD,X
		,D1
		,D2
	COMSEQ2	,D4
		,A2		;not reversed
;---------------------------


RECORD
	INIT	,20A3,	'JJM','PDS','LAM','DEG','JTH',
&			'DLH','JIM','SJM','CJM','BIL',
&			'MAW','PAT','BJZ','JPV','MJM',
&			'JWF','GJK','   ','   ','   '

RECORD	PBUF
	PLINE	,A132
record,x
		,A12
	RLINE	,A120
record	
	ffone	,d10
record,x
	ff_area	,d3
	ff_exch	,d3
	ff_num	,d4

RECORD	CHANNEL
	CHN182	,D2
	CHN044	,D2
	CHN045	,D2
	CHN054	,D2
	CHN175	,D2
	CHNWRK	,D2
	CHNOUT	,D2
	CHNDUC	,D2
RECORD
	TM2	,A90
	TM3	,A90
RECORD,X
	TM2A	,3A30
	TM3A	,3A30

RECORD	NEWVARS
	COMPANY	,A3
	SZ1		,D3
	SZ2		,D3
	SAVJOINT	,D5
	SAVFEET		,D6
	SAVFLIN		,D6
	SAVLBS		,D6
	SAVLIN	,A132
	A2	,A2
	BLANK90	,A90
	TMPDSC	,A62
	REM	,D2
	A6A	,A6
	M_RFA	,A6
	M_ON	,D1
	MEMO_ON	,D1
	TL	,D2
	LL	,D2
	DASHES	,A30,	'------------------------------'
	UNDER	,A58
	J	,D5
	SEQNO	,D2
	SAVSEQ	,D2
	SAVSQ1	,D2
	SAVSQ2	,D2
	SAVSQ3	,D2
	SAVDPT	,A2
	SAVCOM	,D4
	W_KEY	,A63
	W_KEY2	,A63
	SAVKEY	,A63
	SAVQTY	,D5
	MULTLINE	,D5

RECORD	VARS
	A5	,A5
	savss	,d1
	cmpcod	,a3
	i	,d6
	fax_string	,a200
	f_work		,a10
	f_number	,a11
	LINCNT	,D2
	A_FRT	,A6
	NEG_ZERO	,D1,1
	ZERO		,D1,0
	X_FRT		,D1
	STAT	,D1
	ORDER	,D6
	LOKCTL	,D1
	READ	,D1
	Lrow	,D2
	Lcol	,D3
	DECMAL	,D18
	TOTPRC	,D8
	BLANKS	,A30
	CONFIG	,D7
	PAGE	,D2
	KEY	,A6
	BSMID	,D5
	SRCCTL	,D1
	SWITCH	,D1
	ORGCUS	,D5
	ORGSHV	,D5
	V	,D1
	ACCUOM	,A2
	LFEED	,D2
	DTMASK	,A8,	'XX/XX/XX'
	NUMASK	,A8,	'ZZZZZZX-'
	MAXLIN	,D2
	ENTRY	,A30
	INXCTL	,D1
	NO_DOLLARS	,D1
	P_OPT		,D1
PROC
	xcall who(cmpcod)

	faxinfo = f_info

;; moved to here ssq 1-29-04...
	XCALL CHNOPN(17,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (17,'SI',182,SWITCH)
		END
	CHN182 = 17	;COP TABLES

	XCALL OECO (CHN182,COINFO)	;SSQ 11-21-03
;; moved to here ssq 1-29-04...


ASK,
	XCALL W_DISP(WND_1, WD_POS, 17, 1, ' 1  All Prices')
	XCALL W_DISP(WND_1, WD_POS, 18, 1, '(2) Total Prices Only')
	XCALL W_DISP(WND_1, WD_POS, 19, 1, ' 3  No Prices')

	XCALL W_DISP(WND_1,WD_POS,16,4,"Price Option:        <END> to abort")
	XCALL WINPT(wnd_1,16,20,01,00,'AE',ENTRY,INXCTL)
	IF (ENTRY(1,1) .EQ. ' ')
	   BEGIN
	   ENTRY(1,1) = '2'
	   XCALL W_DISP(WND_1, WD_POS, 16, 20, ENTRY(1,1))
	   XCALL W_UPDT
	   END

	IF (INXCTL .EQ. 2)
		BEGIN
		CLEAR F_SEND		;ABORT
		RETURN			;RETURN TO FAXQUE
		END

	USING ENTRY(1,1) SELECT
	('1'),	P_OPT = 1
	('2'),	P_OPT = 2
	('3'),	P_OPT = 3
	(),	GOTO ASK
	ENDUSING

	call print_setup
	if (.not. p_selected) return

	CALL OPENS
	call read_liner			;2-27-17
	F_SEND = 1			;FAX WILL BE SENT
;----------------------------------
	MEMO_ON = 1
	CLEAR M_RFA, M_ON
	CALL LOAD_MEMOS
	CALL LOAD_WORK
	CALL CONSOLIDATE

	CALL LOAD_DUCT
	CALL CONS_DUCT
;----------------------------------
	MAXLIN = 34
	CLEAR TOTPRC, ARRAYS, DUCREC, ACCREC
	FOR I FROM 1 THRU MAXACC	SACCRC (I) =
	PAGE = 1

	CALL PRTHDR_FIRST

;========================================
;;; Add memos to output file...
	CLEAR SAVCOM, SAVSQ1, SAVDPT
	CLEAR SAVSS

	FIND (CHNWRK, LINE, ^FIRST)[ERR=EOF_BM]

BM_LOOP,
	READS (CHNWRK, LINE, EOF_BM)
	IF (WDEPT .NE. SAVDPT) CALL NEWDPT
	IF (WSEQ1 .NE. SAVSQ1) CALL NEWSQ1
	IF (COMSEQ .NE. SAVCOM) CALL NEWCOM

	GOTO BM_LOOP
EOF_BM,
	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_FIN]
FIN_LOOP,
	READS (CHNWRK, LINE, EOF_FIN)

	W_KEY = W_KEYA + W_KEYB
	STORE (CHNOUT, LINE, W_KEY)
	GOTO FIN_LOOP
EOF_FIN,
	CLOSE CHNWRK
	CALL WRTMEM		;INTERNAL ROUTINE TO WRITE WORKSHEETS
	CALL GETDUC		;PROCESS DUCTWORK
	CALL ENDORD

	CLOSE 14
;;;	LPQUE (SPLFIL, copies:1, delete)

ENDOFF,
	CLOSE CHN045
	CLOSE CHNWRK
	CLOSE CHNDUC
	XCALL DELET(DUCFIL)
	RETURN

;================================================

NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVDPT = WDEPT
	RETURN
;------------------------------------------

NEWSQ1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVSQ1 = WSEQ1
	SAVSS = WSS
	IF (WSEQ1 .LE. 0) RETURN
	CLEAR LINE2

	WSS2 = SAVSS

	WSEQ12 = SAVSQ1
	CLEAR WSEQ22, WSEQ32
	WITEM2 = '   F1'
	WTYPE2 = 'M'
	WDESC2 = F1_MEMOL(WSEQ12)
	WDEPT2 = SAVDPT

	W_KEY = W_KEYA + W_KEYB
	STORE (CHNOUT, LINE2, W_KEY)

	RETURN
;------------------------------------------
NEWCOM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; COMBINATION OF SEQ2 & SEQ3
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVCOM = COMSEQ
	SAVSS = WSS

	CLEAR TM2, TM3
	IF (WSEQ2 .NE. 0) TM2 = F2_MEMOL(WSEQ2)
	IF (WSEQ3 .NE. 0) TM3 = F3_MEMOL(WSEQ3)


	FOR J FROM 1 THRU 3
		BEGIN
		CLEAR LINE

		WSS = SAVSS
		WSEQ1 = SAVSQ1
		COMSEQ = SAVCOM
		WITEM = 
		WTYPE = 'M'
		WDEPT = SAVDPT
		WDESC(1,30) = TM2A(J)
		WDESC(34,63) = TM3A(J)
		IF (WDESC .EQ. BLANK90) NEXTLOOP
		IF (TM2A(J).NE.BLANKS) WITEM(4,5) = 'F2'
		IF (TM3A(J).NE.BLANKS) 
			BEGIN
			WITEM(4,5) = 'F3'
			WDESC(32,32) = '*'
			END
		IF (TM2A(J).NE.BLANKS .AND. TM3A(J).NE.BLANKS) 
			BEGIN
			WITEM(4,8) = 'F2&F3'
			END
		WITEM (10,10) = J,'X'

		W_KEY = W_KEYA + W_KEYB
		STORE (CHNOUT, LINE, W_KEY)
		END
		
	RETURN
;------------------------------------------


LOAD_MEMOS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Read thru ordlin file, and
		;;; load memo arrays
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR F1_NUM, F2_NUM, F3_NUM
	FOR J FROM 1 THRU F_MAX
		BEGIN
		CLEAR F1_MEMOS(J)
		CLEAR F2_MEMOS(J)
		CLEAR F3_MEMOS(J)

		CLEAR F1_MEMOL(J)
		CLEAR F2_MEMOL(J)
		CLEAR F3_MEMOL(J)
		END

	FIND (CHN045, ORDLIN, ORDNO) [ERR=MLOOP]
MLOOP,
	READS (CHN045, ORDLIN, EOF_M)
	IF (LINSEQ .NE. 0) GOTO EOF_M
	IF (LMSQ1 .GT. 0) 
		BEGIN
		F1_MEMOS(LMSQ1) = M_SHORTD
		F1_MEMOL(LMSQ1) = M_LDESCR
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
	GOTO MLOOP
EOF_M,
	RETURN
;-------------------------------------------------

LOAD_WORK,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Move non-memo line items
		;;; into work file
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FIND (CHN045, ORDLIN, ORDNO) [ERR=LOOP]
LOOP,
	READS (CHN045, ORDLIN, EOF)
	IF (LORDNO .NE. ORDNO) GOTO EOF
	IF (LTYPE .EQ. 'M') GOTO LOOP
	IF (LQTYOR .EQ. 0) GOTO LOOP	;SSQ 3/24/00

	WTYPE = 'L'
;;;	WDEPT = LDEPT
	WDEPT = LPRDCD(1,1)
	A2 = LMSQ1,	'XX'
	WSEQ1 = A2

	A2 = LMSQ2,	'XX'
	WSEQ2 = A2

	A2 = LMSQ3,	'XX'
	WSEQ3 = A2

	WSS = 0
	A5 = LF3,	'XXXXX'
	IF (%INSTR(1, A5, '1')) WSS = 1	;CONTAINS SS

	WSQNO = LSRTSQ
	WITEM = LITMNO
	using witem select
	('JEB','JEF','JJG','JTG'), WITEM = LITMNO(2,15)	;SKIP THE "J"
	endusing

	WDESC = LDESCR
	IF (LDAMPR) WITEM(10,15) = 'Damper'
	WQTY = LQTYOR
	WUM = LUOFM
	WPRICE = LPRICE
	WCPFLG = LCPFLG

	W_KEY = W_KEYA + W_KEYB
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
CONLOOP,
	READS(CHNWRK, LINE, EOF_CON)
;;;	IF (W_KEY .NE. SAVKEY)


	W_KEY = W_KEYA + W_KEYB
	IF (W_KEY.NE.SAVKEY .OR. WCPFLG)
	THEN	CALL NEWKEY
	ELSE	BEGIN
		INCR MULTLINE
		SAVQTY = SAVQTY + WQTY
		DELETE (CHNWRK)
		END
	GOTO CONLOOP

NEWKEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (SAVKEY .EQ. '***') GOTO OUTKEY
	IF (MULTLINE .EQ. 0) GOTO OUTKEY

	READ (CHNWRK, LINE, SAVKEY)
	W_KEY = W_KEYA + W_KEYB		;ssq 6-23-03
	WQTY = SAVQTY
	WRITE (CHNWRK, LINE, SAVKEY)
OUTKEY,
	SAVKEY = W_KEY
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

	FIND(CHN175,DUCACC,ORDNO) [ERR=LD_LOOP]
LD_LOOP,
	LOKCTL = 1
	XCALL IOS (CHN175,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.ORDNO) GOTO LD_EOF

	XCALL DDEPT (GAUGE,SIZE3,DUTYPE,WD_DEPT,STY)
;;;	USING STY SELECT
;;;	(1,4,5),	WD_DEPT='P'		;TDC
;;;	(2,3,6),	WD_DEPT='O'		;S&D
;;;	ENDUSING

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
	STORE(CHNDUC,DUCWRK,WD_KEY)

	GOTO LD_LOOP
LD_EOF,
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
	CLOSE CHNOUT
	OPEN (CHNOUT, SI, OUTFIL)
	XCALL FILL ('_', UNDER)
	SAVDPT = -1

	SAVSS = 0
W_LOOP,
	READS (CHNOUT, LINE, W_EOF)
	IF (WTYPE.EQ.'L' .AND. WQTY.EQ.0) GOTO W_LOOP		;SKIP OF QTY=0
	IF (WDEPT .NE. SAVDPT) CALL W_NEWDPT
	IF (WSS .NE. SAVSS)
		BEGIN
		CALL SET_BOLD
		clear pline
		call print
	;;;	PLINE(25,70) = '   Safety Seal is a self sealing'
	;;;	PLINE(25,70) = '     EZ Seal is a self sealing'
		PLINE(25,70) = '  Complete Seal is a self sealing'
		call print
		pline(25,70) = '    spiral pipe connection that'
		call print
		pline(25,70) = '    meets SMACNA Leakage Class 3'
		call print
		savss = wss
		CALL SET_NOBOLD
		savss = wss
		END

	
	IF (WTYPE .EQ. 'M')
	THEN	BEGIN
		CALL SET_BOLD
		IF(M_ON.EQ.0) 
			BEGIN
			XCALL GETRFA(CHNOUT, M_RFA)	;SAVE THE RFA OF 1ST MEMO
			M_ON = 1
			END
		IF (MITEM.EQ.'   F1') 
		THEN	FOR J FROM 1 THRU 3
			BEGIN
			TMPDSC = MMEMO(J)
			CALL MAIN_MEMO
			PLINE (8,70) = TMPDSC
			IF (MMEMO(J) .NE. BLANKS) CALL MEMO_PRINT
			END
		ELSE	BEGIN
			PLINE(8,70) = M_LONG
			CALL MEMO_PRINT
			GOTO W_LOOP
			END

		CALL SET_NOBOLD
		END
	ELSE	BEGIN
		CALL SET_NOBOLD
		CLEAR M_ON
		CALL PRTLIN
		END


	GOTO W_LOOP
;========================================

H_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; NEW PAGE; PRINT ANY ACTIVE MEMOS...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (M_RFA .EQ. A6A) RETURN
	SAVE_LINE = LINE

	OPEN (39, SI, OUTFIL)
	XCALL POSRFA(39, M_RFA)		;GET TO LAST MEMO
HM_LOOP,
	READS (39, LINE, HM_EOF)
	IF (WTYPE .EQ. 'L') GOTO HM_EOF
	call set_bold
	IF (MITEM.EQ.'   F1') 
	THEN	FOR J FROM 1 THRU 3
		BEGIN
		TMPDSC = MMEMO(J)
		CALL MAIN_MEMO
		PLINE (8,70) = TMPDSC
		IF (MMEMO(J) .NE. BLANKS) CALL MEMO_PRINT
		END
	ELSE	BEGIN
		PLINE(8,70) = M_LONG
		CALL MEMO_PRINT
		GOTO HM_LOOP
		END


	GOTO HM_LOOP

HM_EOF,
	CLOSE 39
	CALL SET_NOBOLD
	LINE = SAVE_LINE
	RETURN
;------------------------------------------------

PRTLIN,		; Print out line item detail
	IF (LINCNT+2.GT.MAXLIN) 
		BEGIN
		CALL CONTIN
		IF (MSEQ.NE.0) CALL H_MEMO
		END
	IF (WCPFLG) 
	THEN	BEGIN
		PLINE (67,68) = '**'	
		END
	ELSE	BEGIN
		CLEAR PLINE
		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	PLINE (9,23) = WITEM
	IF (WITEM.EQ.'???') 	PLINE (19,20) = WITEM(4,5)

	PLINE (25,54) = WDESC
	IF (WITEM.NE.'M')
	BEGIN
	  PLINE ( 1,7 ) = WQTY,NUMASK
	  PLINE (61,71) = WPRICE,'ZZZZZZ.XXX-'
	  PLINE (73,74) = WUM
	  DECMAL = (WQTY * WPRICE)#1
	  PLINE (76,87) = DECMAL,'ZZZZ,ZZZ.XX-'
	  TOTPRC = TOTPRC + DECMAL		;INVENTORY ITEMS
	END
	IF (P_OPT .GE. 2)
		BEGIN
		PLINE(61,71) = 
		PLINE(72,87) =
		END

	CALL LINE_PRINT
	RETURN
;----------------------------------------------------------

MAIN_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;
	TL = %TRIM(TMPDSC)
	REM = 50 - TL
	REM = REM/2
	IF (REM.GT.0)
		BEGIN
		CLEAR TMPDSC
		TMPDSC(1,REM) = DASHES
		TMPDSC(REM+1,REM+TL) = MMEMO(J)
		TMPDSC(REM+1+TL,50) = DASHES
		END
	RETURN
;-----------------------------------------
W_EOF,
	CLOSE CHNOUT
	CLOSE CHNWRK
	XCALL DELET (WRKFIL)
	XCALL DELET (OUTFIL)
	RETURN
;===============================================

W_NEWDPT,
	SAVDPT = WDEPT
	RETURN
;------------------------------------

GETDUC,
	READ(CHNDUC,DUCWRK,^FIRST) [ERR=GETDUC_EOF]
	GOTO CONDUC
NXTDUC,
	READS(CHNDUC,DUCWRK,GETDUC_EOF) [ERR=GETDUC_EOF]
CONDUC,
	WSIZE3 = 100-WSIZE3
	WSIZE2 = 1000000-WSIZE2
	WSIZE1 = 1000000-WSIZE1
	WSTY=10-WSTY
	SZ1 = %TRN3(WSIZE1)
	SZ2 = %TRN3(WSIZE2)
	CALL PRTDUC
SUMDUC,
	CONFIG(1,3) = WGAUGE
	IF (WDUTYPE.EQ.2.OR.WDUTYPE.EQ.3) CONFIG(1,3) = WTHICK
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
	RETURN

BADCON,
	GOTO ERRMSG
BADACC,
ERRMSG,
	RETURN

;------------------------------------------

ENDORD,
;;;	CALL PRTSAC
;;;	CALL PRTSDU
	
	A_FRT = OFRGHT
	USING A_FRT SELECT
	('    -0'),	X_FRT = NEG_ZERO
	('     0'),	X_FRT = ZERO
	(),		X_FRT = 9
	ENDUSING

	call set_bold

	LFEED = MAXLIN - lincnt - 6

	IF (LFEED.GT.0) XCALL RTF(14, LN_FEED, LFEED)

	pline = 'Prices shown will be honored for 30 days'	;ssq 5-16-06

	PLINE(61,75) = '   NET AMOUNT:'
	PLINE (76,87) = TOTPRC,'ZZZZ,ZZZ.ZZ-'

	IF (P_OPT .EQ. 3) PLINE (49,88) = 
	CALL PRINT

	CLEAR PLINE
	USING X_FRT SELECT
	(NEG_ZERO),	BEGIN
			PLINE(72,76) = 'ESTIMATED FRT: '
			PLINE(85,87) = 	'N/C'
			END
	(9),		BEGIN
			PLINE(62,76) = 'ESTIMATED FRT: '
			PLINE(78,87) = OFRGHT,	'ZZ,ZZZ.XX-'
			END
	ENDUSING

	pline(1,39) = 'from date of Quotation.  Call factory for current'
;                      12345678901234567890123456789012345678901234567890
;                               1         2                   3         4
;;;	PLINE(10,43) = "Please review all quantities,"
	IF (P_OPT .EQ. 3) PLINE (51,88) = 
	call print
	
	pline = 'pricing after 30 days.'		;ssq 5-16-06
;;;	PLINE(10,43) = "items and gauges. Thank you."
	IF (X_FRT .NE. ZERO)
		BEGIN
		PLINE (60,76) = 'ESTIMATED TOTAL: '
		PLINE (77,87) = (TOTPRC+OFRGHT),'ZZZZ,ZZZ.ZZ-'
		END
	IF (P_OPT .EQ. 3) RLINE (49,88) = 

	call print2

	CALL SET_NOBOLD

ENDUP,

done,
	XCALL RTF(14, FINI)
STOPIT,
	CLOSE 14
	RETURN

;=================================================

CONTIN,
	PLINE (62,76) = '** CONTINUED **'
	CALL PRINT
	IF(PAGE .EQ. 1)
	THEN	LFEED = MAXLIN - LINCNT + 6
	ELSE	LFEED = MAXLIN - LINCNT + 7
	IF (LFEED) LINCNT = MAXLIN

	IF (LFEED.GT.0) XCALL RTF(14, LN_FEED, LFEED)
	IF (PAGE.GT.1)
		BEGIN
		LFEED = 2
		XCALL RTF(14, LN_FEED, LFEED)
		END

;need new page here
;;;	xcall wpr_print(phand, 	DWP_ENDPAGE)

	INCR PAGE

PRTHDR,		; Print the order header information

PRTHDR_FIRST,
	clear  lrow

	XCALL RTF(14, FS, 32)	;LARGE FONT
	call set_bold
	pline(22,95) = "Q U O T A T I O N"
	call print
	LFEED = 2	
;;;	LFEED = 4
	XCALL RTF(14, LN_FEED, LFEED)

	XCALL RTF(14, FS, 20)	;NORMAL FONT
	pline = c_name
;;;	pline = " SHEET METAL CONNECTORS, INC."
	pline(56,90) = "Quotation No."

	pline (70,75) = oordno,	'XXXXXX'
	call print


	call set_noBold
	
	pline = c_add1
;;;	pline = " 5850 MAIN STREET N. E."
	pline(66,69) = "DATE"

	XCALL DATE8(OORDDT, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE (71,81) = D_FMT

	if (page .gt. 1) 
		begin
		clear pline(1,50)
		call print
		MAXLIN = 50
		goto done_hdr
		end

	call print

	pline = c_add2
;;;	pline = " MINNEAPOLIS,  MN 55432"
	call print

	call set_bold
	pline(2,13) = c_ld,	'XXX-XXX-XXXX'
	pline(16,27) = c_loc,	'XXX-XXX-XXXX'	
;;;	pline = " 800-328-1966  763-572-0000"
	call print

	call set_nobold
	pline = ' WEB SITE:'
	pline(12,52) = c_web
;;;	pline = ' WEB SITE: WWW.SMCDUCT.COM'
;                123456789012345678901234567
;                         1         2
	call print

	call set_bold
	pline = " FAX:"
	pline(6,17) = c_fax,	'XXX-XXX-XXXX'
;;;	pline = " FAX:763-572-1100"
	call print

	call set_nobold
	clear pline
;;; 3-31-03 SSQ>>>	PLINE(23,28) = CRDLMT,'ZZZZZZ'
	pline (55,62) = "SHIP TO:"
	call print

	pline(17,55) = ocusnm
	pline(55,90) = oshpnm
	call print

	pline(17,55) = add1
	pline(55,90) = oshad1
	call print

	pline(17,55) = add2
	pline(55,90) = oshad2
	call print

	PLINE (17,31) = CITY
	PLINE (34,35) = STATE
	PLINE (38,42) = ZIP
	PLINE (55,84) = OSHAD3
	call print

;per Steve M...
	call set_bold
	clear pline
	PLINE (17,21) = 'ATTN:'
;;;	PLINE (23,32) = OPONO
	PLINE (23,32) = OATTN		;SSQ 6-22-00
	call print
	call set_nobold

	LFEED =3
	XCALL RTF(14, LN_FEED, LFEED)

;;;	PLINE =  ' Cust #    Sales-Rep         Job #           Freight FOB                Fab Time     Terms'
	PLINE =  ' Cust #    Sales-Rep         Job #           Freight FOB             Fab Time     Terms'
	call print

	BL_CUST = OCUSNO,	'ZZZZZZ'
	BL_JOB = OJOBNO
	IF (OCLPPD.EQ.'C') BL_FOB = 'PLANT - '
	IF (OCLPPD.EQ.'P') BL_FOB = 'DEST. - '
	BL_FOB(9,23) = SC_NAME

	BL_TERMS = 'NET 30'
	BL_REP = slsnm

	BL_FAB_TIME(1,3) = OPROMD,"ZZX"
	BL_FAB_TIME(5,8) = 'DAYS'

	PLINE = BUF_LINE
	call print

;;;	LFEED =3
	LFEED = 2
	XCALL RTF(14, LN_FEED, LFEED)

;;;	XCALL RTF(14, FS, 24)	;NORMAL FONT
	CALL SET_BOLD

	PLINE(3,90) = 'We are pleased to quote on your inquiry as follows:'
	call print

DONE_HDR,
	call set_nobold
;;;	XCALL RTF(14, FS, 24)	;NORMAL FONT

	PLINE = 
&	"   Qty  Item No.        Description                              Price  UM  Ext. Price"
;;;&	"   Qty  Item No.        Description                                 Price UM  Ext. Price"

	call print

	LFEED = 1
	XCALL RTF(14, LN_FEED, LFEED)
	LINCNT = 0
;;;	XCALL RTF(14, FS, 20)	;NORMAL FONT
;;;	XCALL RTF(14, FS, 24)	;NORMAL FONT
	RETURN
;---------------------------------------------------


PRTDUC,		;Print duct record

	IF (LINCNT+3 .GT. MAXLIN) CALL CONTIN
	INCR LINCNT

	LFEED = 1
	XCALL RTF(14, LN_FEED, LFEED)

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
	PLINE (18,20) = SZ1,'ZZX'
	PLINE (21,21) = 'X'
	PLINE (22,24) = SZ2,'ZZX'
	PLINE (25,25) = 'X'
	PLINE (26,27) = WSIZE3,'ZX'
	IF(WSIZE1/1000.NE.SZ1 .OR. WSIZE2/1000.NE.SZ2) PLINE(28,28)='*'
	PLINE (31,34) = DTYPE(WDUTYPE)
	PLINE (36,41) = DCAT(WCAT)
	PLINE (43,45) = DSTY(WSTY)
	PLINE (48,50) = DSEAM(WSEAM)
	PLINE (52,66) = WLOCAT
	PLINE (64,71) = WGPRICE,'ZZZ.XXX-'
	DECMAL = ( WPOUNDS * WGPRICE ) # 1
	PLINE (73,74) = 'LB'
	PLINE (76,87) = DECMAL,'ZZZZ,ZZZ.XX-'
	TOTPRC = TOTPRC + DECMAL		;DUCT

	IF (P_OPT .GE. 2)
		BEGIN
		PLINE(60,88) = 
		PLINE(72,88) =
		END
	CALL line_PRINT

	PLINE (  1,7  ) = WPOUNDS,NUMASK
	PLINE (  8,8  ) = '#'
	PLINE ( 12,17 ) = WSQFLIN,NUMASK
	PLINE ( 19,23 ) = 'SQ FT'
	PLINE (30,44) = '     NO LINER '
	IF (WLINER.GE.1.AND.WLINER.LE.8.AND.WLINER.NE.4)
	BEGIN
	  PLINE (31,37) = DLINER( WLINER )
	  PLINE (38,42) = 'LINER'
	END
	IF (WSEAL.EQ.1) PLINE (44,47) = 'SEAL'
	PLINE (49,63) = WDPUNCH
	PLINE (64,71) = WLINPRC,'ZZZ.XXX-'
	PLINE (74,75) = 'SF'
	DECMAL = ( WLINPRC * WSQFLIN ) # 1
	PLINE (76,87) = DECMAL,'ZZZZ,ZZZ.XX-'
	TOTPRC = TOTPRC + DECMAL		;LINER

	IF (P_OPT .GE. 2)
		BEGIN
		PLINE(65,72) = 
		PLINE(77,88) =
		END
	CALL line_PRINT

	RETURN
;-------------------------------------------------------------

PRTSDU,
		;Print summarized duct from this order
	IF ((LINCNT+1).GT.MAXLIN) CALL CONTIN
	INCR LINCNT

	FOR I FROM 1 THRU MAXDUC
	  BEGIN
	  DUCREC = SDUCRC(I)
	  IF (DUCCFG.EQ.0) GOTO PRTSD2
	  IF (lincnt+1.GT.MAXLIN) CALL CONTIN
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
	  CALL line_PRINT
	  SDUCRC(I) =
	END
PRTSD2,
	FOR I FROM 1 THRU 9
	  BEGIN
	  IF (LINSQF(I).NE.0.AND.I.NE.4)
	  BEGIN
	    IF (LINCNT+1.GT.MAXLIN) CALL CONTIN
	    PLINE (1,7) = LINSQF(I),NUMASK
	    PLINE (8,10) = 'SQF'
	    PLINE (33,39) = DLINER(I)
	    PLINE (40,45) = ' LINER'
	    CALL line_PRINT
	    LINSQF(I) =
	  END
	END
	RETURN	
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
	RETURN
;-----------------------------------------------------

GET_SALESMAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL IO (CHN054,SALMAN,OSLMAN,READ,LOKCTL)
	IF (SLSNM.EQ.']]]]]]' .OR. SLSNM.EQ.']]]DEL' .OR. LOKCTL.NE.0) SLSNM =
	RETURN
;-----------------------------------------------------


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SWITCH = 5
	XCALL CHNOPN(24,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (24,'SI',044,SWITCH)
		CHN044 = 24
		END

	XCALL CHNOPN(25,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (25,'SI',045,SWITCH)
		CHN045 = 25
		END

	XCALL CHNOPN(26,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (26,'SI',175,SWITCH)
		CHN175 = 26
		END

	XCALL CHNOPN(36,STAT)
	IF (STAT .EQ. 0) XCALL FILES (36,'SI',001,SWITCH)

;;;	XCALL CHNOPN(37,STAT)
;;;	IF (STAT .EQ. 0) XCALL FILES (37,'I',002,SWITCH)

	XCALL CHNOPN(38,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (38,'I',054,SWITCH)
		CHN054 = 38
		END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	XCALL CHNOPN(17,STAT)
;;;	IF (STAT .EQ. 0)
;;;		BEGIN
;;;		XCALL FILES (17,'SI',182,SWITCH)
;;;		END
;;;	CHN182 = 17	;COP TABLES
;;;
;;;	XCALL OECO (CHN182,COINFO)	;SSQ 11-21-03
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	LOKCTL = 1
;;;	XCALL IO (36,CUSCTL,1,READ,LOKCTL)
;;;	ORGCUS = ORG001

;Create work file...
;;;	XCALL ISAMC (WRKFIL, 132, 1, 'START=1, LENGTH=56, DUPS, ASCEND')
	XCALL ISAMC (WRKFIL, 133, 1, KEYSPEC)
	OPEN (33, SU, WRKFIL)
	CHNWRK = 33

	OFORD = ORDNO,'XXXXXX'
;Create output file...
;;;	XCALL ISAMC (OUTFIL, 132, 1, 'START=1, LENGTH=56, DUPS, ASCEND')
	XCALL ISAMC (OUTFIL, 133, 1, KEYSPEC)
	OPEN (34, SU, OUTFIL)
	CHNOUT = 34

	WRORD = ORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
;;;	XCALL ISAMC (DUCFIL, 114, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (51, SU, DUCFIL)
	CHNDUC = 51

	SPORD = ORDNO,'XXXXXX'
;;;	OPEN (14, O, SPLFIL)

	OORDNO = ORDNO
	ORDER = ORDNO

	LOKCTL = 1
	XCALL ISIO (CHN044,ORDHDR,OORDNO,READ,LOKCTL)
;;;	IF (LOKCTL.NE.0.OR.OORDNO.NE.ORDER) GOTO INVORD

	XCALL ISIO (36, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CUSMAS = 
		NAME = '* CUSTOMER NOT ON FILE *'
		END
		
;;;	KEY = OCUSNO,'XXXXXX'
;;;	XCALL SERCH (37,CUSIDX,KEY,1,6,ORGCUS,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	LOKCTL = 1
;;;	CASE SRCCTL OF 
;;;	BEGINCASE
;;;	0:	XCALL IO (36,CUSMAS,IRC001,READ,LOKCTL)
;;;	1:	BEGIN
;;;		  CUSMAS =
;;;		  NAME = '* CUSTOMER NOT ON FILE *'
;;;		END
;;;	ENDCASE

	CALL GET_SCAC
	CALL GET_SALESMAN

	RETURN
;-------------------------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLOSE CHN044
	CLOSE CHN045
	CLOSE CHN054
	CLOSE CHN175
	CLOSE 36
;;;	CLOSE 37

	RETURN

;=================================================================
;=================================================================
; Print routines...
;=================================================================
;=================================================================

PRINT_SETUP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Set up printer interface
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; open spool file
	onerror not_num
	ffone = fc_fax
	offerror
	
	using ff_area select
; user will not include area code unless long distance call (JM 7-14-99)
; area codes used for local numbers, 4-25-01
;;;	(0),	f_number = fc_fax			;local
	(0, 612, 651, 763, 952),	f_number = fc_fax		;local
	(),		begin
			f_work = fc_fax			;dec to alpha
			f_number = "1" + f_work		;long distance
			end
	endusing

	goto f_ok
not_num,
	xcall mesag ('Invalid fax number',1)
	p_selected = 0
	return
f_ok,
	p_selected = 1

;create a unique spool file 
	O_ORDER = ORDNO

	open(14,O,O_FILE)
	clear fax_string
	fax_string = "::" + fc_cusname(1,%trim(fc_cusname))
&		+ "," + fc_cont(1,%trim(fc_cont)) + ","
&		+ f_number(1,%trim(f_number))
;;;&		+ "9," + f_number(1,%trim(f_number))
	
	writes (14,fax_string(1,%trim(fax_string)) )
;;;	WRITES (14,'::Computer Services,Sherwood Quiring,5721100')
;;;	WRITES (14,'::H, C=SMCCOVER.RTF')
	XCALL WHO(COMPANY)
	USING COMPANY SELECT
	('SMC'),	WRITES (14,'::H, C=SMCCOVER.RTF')
	('ROC'),	WRITES (14,'::H, C=ROCCOVER.RTF')
	('CAT'),	WRITES (14,'::H, C=CATCOVER.RTF')
	('TST'),	WRITES (14,'::H, C=TSTCOVER.RTF')
	ENDUSING

	S_QUOTE = ORDNO		;QUOTE #
	WRITES (14,SUBJECT)

	XCALL RTF(14, START_RTF)
	call set_bold		;12-7-15
; --------------------- cover page info ------------------------
	FOR I FROM 1 THRU 7
		BEGIN
		PLINE (26,76) = FC_NOTARA(I)
		CALL PRINT
		END

	LFEED = 2
	XCALL RTF(14, LN_FEED, LFEED)

	PLINE (46,66) = 'Thank You,'
	CALL PRINT
	PLINE (46,66) = FC_SALESMAN
	CALL PRINT

	LFEED = 10
	XCALL RTF(14, LN_FEED, LFEED)

	PLINE (31,80) = 'If you should have any questions or concerns'
	CALL PRINT
	LFEED = 1
	XCALL RTF(14, LN_FEED, LFEED)
;;;	PLINE (27,80) = 'Please do not hesitate to call me at (800) 328-1966'
	PLINE (27,80) = 'Please do not hesitate to call me at '
	PLINE (64,75) = C_LD,	'XXX-XXX-XXXX'
	CALL PRINT

	LFEED = 15
	XCALL RTF(14, LN_FEED, LFEED)
	call set_nobold			;ssq 12-7-15
; --------------------- cover page info ------------------------
	return
;---------------------------------------------------------------

print_err,
	display (15,$scr_pos(22,1), "an error occured")
	stop

memo_print,	;here ??
	if (LINCNT+5 .gt. maxlin)
		begin
		call set_nobold
		savlin = pline
		clear pline
		call contin
		pline = savlin
		call set_bold
		end

line_print,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	Lcol = 1
	call print
	return
;--------------------------------------------

print,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; set up the print parameters
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL RTF(14, LPRINT, PLINE)
	INCR LINCNT
	return
;--------------------------------------------

print2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;print the line, don't incr lincnt
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL RTF(14, FORMAT, PLINE)
	return

;--------------------------------------------
;----------------------------------------------------

SET_BOLD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL RTF (14, BOLD)
	RETURN
;----------------------------------------------------
SET_noBOLD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	using cmpcod select
	('CAT'),	return
	('TST'),	return
	endusing
	
	XCALL RTF (14, PLAIN)
	RETURN
;----------------------------------------------------

SET_UNDERLINE,	;*************************************
	RETURN
;----------------------------------------------------
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

END



