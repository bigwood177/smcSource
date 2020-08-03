SUBROUTINE	PRWO2
	ORDNO	,D


; 09-07-06 ssq: removed bolding logic at "print,".
; 09-12-06 ssq: if spee dee or UPS
;			then print to ups printer
;			else print to will-call printer


EXTERNAL FUNCTION
	TRN3	,D

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


GLOBAL DUCK	
	.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL

RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.DEF'

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT

RECORD	BOLD
	E_CHA2	,A1
		,A*,	"(s3B"

RECORD	MEDIUM
	E_CHA3	,A1
		,A*,	'(s0B'

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
	
RECORD	F_MEMOS
	.INCLUDE 'def:CPMEMO.DEF'
		
RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.new'
RECORD,X
	.INCLUDE 'DEF:RD045M.new'
RECORD,X
	.INCLUDE 'DEF:RD045D.new'

;;;RECORD	ORDLIN1
;;;	.INCLUDE 'DEF:RD045X.new'

RECORD	CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'
RECORD	DUMCUS
	.INCLUDE 'DEF:RD001B.DEF'
RECORD	CUSIDX
	.INCLUDE 'DEF:RD002A.DEF'

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
	LINSQF	,8D6	;SUMMARIZE LINER SQUARE FEET

RECORD DUCREC
	DUCCFG	,D7	;CONFIGURATION CODE
	DUCSQF	,D7	;SQ FEET OF MATERIAL
	DUCPND	,D7	;POUNDS OF MATERIAL
	DUCGPR	,D7	;GAUGE PRICE

RECORD ACCREC
	.INCLUDE 'DEF:ACCREC.DEF'


RECORD	LINE			
	WSEQ1	,D2		;reversed
	WSEQ2	,D2
	WSEQ3	,D2	
	WDEPT	,A2
	WSQNO	,A2
	WITEM	,A15
	WDESC	,A90
	WTYPE	,A1		;L=LINE, M=MEMO
	WQTY	,D5
	WUM	,A2
	wpcat	,a2		;prdcat ssq 4-11-06
	wtag	,d2		;lmsq4 12-26-07

RECORD	MEMO,X
	MSEQ	,D6
	MDEPT	,A2
	MSQNO	,A2
	MITEM	,A15
	MMEMO	,3A30
		,A1
		,D5
		,A2
		,a2		;ssq 4-11-06
		,a2		;ssq 12-26-07
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
RECORD,X
	W_KEY	,A55

RECORD,X
		,A2
	COMSEQ	,A4
		,a2		;not reversed
;---------------------------
RECORD	LINE2			
	WSEQ12	,D2		
	WSEQ22	,D2
	WSEQ32	,D2	;REVERSE
	WDEPT2	,A2
	WSQNO2	,A2
	WITEM2	,A15
	WDESC2	,A30
	WADD2	,A60	;FILLER
	WTYPE2	,A1		;L=LINE, M=MEMO
	WQTY2	,D5
	WUM2	,A2
	wpcat2	,a2		;ssq 4-11-06
	wtag2	,d2		;ssq 12-26-07
RECORD,X
	W_KEY2	,A55

RECORD,X
		,A2
	COMSEQ2	,A4
		,a2		;not reversed
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
	CHN175	,D2
	CHN182	,D2
	CHNWRK	,D2
	CHNOUT	,D2
	CHNDUC	,D2

RECORD	PAGEOF
	P1	,A2
		,A4,	' OF '
	P2	,A2


RECORD	ORDFMT
	OF_ORD	,A6
		,A1,	'.'
	OF_NUM	,D1

RECORD	PVARS
	TEMP	,A2
	MEMO_ON	,D1
	MAXLIN	,D2,	57
	LINCNT	,D2
	PAG	,D2
	NPAG	,D3
	TPAG	,D3
	SAVPAG	,D3
	LP_ON	,D1
	PLINE	,A80
	SAVLIN	,A80
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

RECORD	VARS
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
	S_DATE	,A10		;SHIP DATE
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
	LOKCTL	,D1
	SWITCH	,D1

PROC
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

	CALL LOAD_MEMOS
	CALL LOAD_WORK
	CALL CONSOLIDATE
	CALL GET_DEPTS

	CALL LOAD_DUCT
	CALL CONS_DUCT

;;; Add memos to output file...
	CLEAR SAVCOM, SAVSQ1, SAVDPT

	FIND (CHNWRK, LINE, ^FIRST) [ERR=EOF_BM]

BM_LOOP,
	READS (CHNWRK, LINE, EOF_BM)
	IF (WDEPT(1,1) .NE. SAVDPT(1,1)) CALL NEWDPT
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
	FIRST_PAGE = 1
	CALL PRTHDR

	CALL WRTMEM		;INTERNAL ROUTINE TO WRITE WORKSHEETS
	CALL GETDUC		;PROCESS DUCTWORK

	SAVPAG = TPAG
		bold_on = 0		;ssq 3-30-06	
		DISPLAY(14,MEDIUM)
		display(14, EJECT)
	CLOSE 14
	INCR LP_ON
	IF(LP_ON .LE. 1) GOTO START_PRINT

;-------------------------------------------------------
; SSQ 9-12-06: Print all-dept to UPS printer if UPS or Spee Dee,
;		all others go to Will-Call printer...

	USING CMPCOD SELECT
	('SMC','TST'), BEGIN
		       USING OSCAC SELECT
		       ('0','4','7','8','9'),	IF (OPRTF .LE. 1)
	THEN	LPQUE (SPLFIL, LPNUM:"HPLaserJ",DELETE)
	ELSE	LPQUE (SPLFIL, LPNUM:"UPS6P",DELETE)	;pink

		       ('10'),		LPQUE (SPLFIL, LPNUM:'new_blue', DELETE)

		       (),		LPQUE (SPLFIL, copies:1, delete)
	        ;;;       (),		LPQUE (SPLFIL, LPNUM:'new_blue', DELETE)
		       ENDUSING
		       END

	(),	       LPQUE (SPLFIL, copies:1, delete)
	ENDUSING


;;;	IF (OSCAC .EQ. '10')
;;;	THEN	LPQUE (SPLFIL, LPNUM:'new_blue', DELETE)
;;;	ELSE	LPQUE (SPLFIL, copies:1, delete)
;-------------------------------------------------------

	CLOSE CHNOUT
	CLOSE CHNWRK
	XCALL DELET (WRKFIL)
	XCALL DELET (OUTFIL)
	CLOSE CHNDUC
	XCALL DELET(DUCFIL)
ENDOFF,
	CLOSE CHN045
	CLOSE CHNWRK

	RETURN

;================================================

NEWDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SAVDPT = WDEPT
	RETURN
;------------------------------------------

NEWSQ1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
	FIND (CHN045, ORDLIN, ORDNO) [ERR=LOOP]
LOOP,
	READS (CHN045, ORDLIN, EOF)
	IF (LORDNO .NE. ORDNO) GOTO EOF
	IF (LTYPE .EQ. 'M') GOTO LOOP
	IF (LPRDCD .EQ. 'Z ') GOTO LOOP	;SSQ 3/22/05 skip priceduct.com
;;;	IF (LQTYOR .EQ. 0) GOTO LOOP		;SSQ 3/24/00

	CLEAR TBL_KEY		;SSQ 11-9-04
	TBLCOD = 'EX'
	TBLKEY = EX_ITEM = LITMNO
	FIND (CHN182,COPTBL,TBL_KEY) [ERR=LCON2]	;continue if NOT in table
	GOTO LOOP
LCON2,

	WTYPE = 'L'
	WSQNO = LSRTSQ
	WDEPT = LPRDCD(1,1)
	wpcat = ldept			;ssq 4-11-06 save dept

	A2 = LMSQ1, 'XX'
	WSEQ1 = A2
	A2 = LMSQ2, 'XX'
	WSEQ2 = A2
	A2 = LMSQ3, 'XX'
	WSEQ3 = A2
	WTAG = LMSQ4			;ssq 12-26-07

	WITEM = LITMNO
	using witem select
	('JEB','JEF','JJG','JTG'), WITEM = LITMNO(2,15)	;SKIP THE "J"
	endusing

	WDESC = LDESCR
	IF (LDAMPR) WITEM(10,15) = 'Damper'
	WQTY = LQTYOR
	WUM = LUOFM
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
	READ (CHNWRK, LINE, SAVKEY)
	WQTY = SAVQTY
	WRITE (CHNWRK, LINE, SAVKEY)
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

	USING STY SELECT
	(1,4,5),	BEGIN
			WD_DEPT='P'		;TDC
			GOTP=1
			END			
	(2,3,6),	BEGIN
			WD_DEPT='O'		;S&D
			GOTO=1
			END
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
	CLOSE CHNOUT
	OPEN (CHNOUT, SI, OUTFIL)
	XCALL FILL ('_', UNDER)
	SAVDPT = -1

W_LOOP,
	READS (CHNOUT, LINE, W_EOF)
	IF (WDEPT(1,1) .NE. SAVDPT(1,1)) CALL W_NEWDPT
	IF (WTYPE .EQ. 'M')
	THEN	BEGIN
	;;;	DISPLAY(14,BOLD)
		bold_on = 1
		IF (MEMO_ON .EQ. 0)
			BEGIN
			CLEAR PLINE
			CALL PRINT
			MEMO_ON = 1
			END
		IF (MITEM.EQ.'   F1') 
		THEN	FOR J FROM 1 THRU 3
			BEGIN
			TMPDSC = MMEMO(J)
			CALL MAIN_MEMO
			PLINE (8,54) = TMPDSC
			IF (MMEMO(J) .NE. BLANKS) CALL PRINT
			END
		ELSE	BEGIN
			PLINE(8,70) = M_LONG
			CALL PRINT
			GOTO W_LOOP
			END

		END
	ELSE	BEGIN
		using wum select
		('RL','BG','BX'), bold_on = 1
		(),		bold_on = 0
		endusing

; dept is prdcat(1,1), wpcat is ldept...
		if (wpcat .eq. 'B') bold_on = 1
		if (wpcat .eq. 'A') bold_on = 1		;04-04-06 ssq
		if (wpcat .eq. 'D') bold_on = 1		;04-04-06 ssq
; this is really cat:D, dept:I... ssq 4-11-06
		if (wdept.eq.'D' .and. wpcat.eq.'I') bold_on = 1

	;;;	DISPLAY(14,MEDIUM)
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
			pline (65,79) = dl_ara(1)
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
		IF (WTAG) DISPLAY(14,MEDIUM)	;SSQ 12-26-07
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
;;;	CLOSE CHNOUT
;;;	CLOSE CHNWRK
;;;	XCALL DELET (WRKFIL)
;;;	XCALL DELET (OUTFIL)
	RETURN
;===============================================

W_NEWDPT,
	SAVDPT = WDEPT
	RETURN
;------------------------------------

GETDUC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PROCESS DUCTWORK
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	WDEPT = 'O'		;DUCTWORK

	READ(CHNDUC,DUCWRK,^FIRST) [ERR=NO_DUCT]
	IF ((LINCNT+3) .GE. MAXLIN) CALL PRTHDR
	CALL PRINT
	XCALL FILL ('=', PLINE(8,53))
	PLINE (26,35) = ' DUCTWORK '
	CALL PRINT

	GOTO CONDUC
NXTDUC,
	READS(CHNDUC,DUCWRK,GETDUC_EOF) [ERR=GETDUC_EOF]
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

	IF (WLINER.GE.1.AND.WLINER.LE.8.AND.WLINER.NE.4)
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
;;;	INCR LINCNT		;SSQ 12-4-96

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
	IF (WLINER.GE.1.AND.WLINER.LE.8.AND.WLINER.NE.4)
	BEGIN
	  PLINE (31,37) = DLINER( WLINER )
	  PLINE (38,42) = 'LINER'
	END
	PLINE (43,54) = WLOCAT
	CALL PRINT

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
	FOR I FROM 1 THRU 8
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
;;;	IF (LINCNT .GT. 57) 
	IF (LINCNT .GT. 56) 
		BEGIN
		SAVLIN = PLINE
		sav_bold = bold_on
		CALL PRTHDR
		PLINE = SAVLIN
		bold_on = sav_bold
		END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ssq 9-7-06: removed bold items

;;;	if (bold_on .eq. 1)
;;;	then 	display(14, bold)
;;;	else	display(14, medium)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	WRITES (14, PLINE)
	CLEAR PLINE
	RETURN
PRINT2,
	INCR LINCNT
	WRITES (14, PLINE)
	CLEAR PLINE
	RETURN
;-----------------------------------------------------

PRTHDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (FIRST_PAGE .GT. 1) 
		BEGIN
;;;		WRITES (14, EJECT)
		display(14, EJECT)
		END
	FIRST_PAGE = 2

	CLEAR LINCNT
	CLEAR PLINE

;-----------------
	DT = %DATETIME
	D = DT(1,8)
	T = DT(9,14)
	DATE = D,	'XXXX-XX-XX'
	TIME = T,	'XX:XX:XX'
	PLINE = DATIME
;-----------------
	PLINE(32,67) = OCOMNT(1)
	CALL PRINT2
	PLINE(32,67) = OCOMNT(2)
	CALL PRINT2
	CALL PRINT2

	PLINE(22,62) = 'MATERIAL FOR ALL DEPARTMENTS'
;;;	PLINE(62,67) = OORDNO,	'ZZZZZZ'
	OF_ORD = OORDNO,	'ZZZZZX'
	IF(OPRTF.LE.0)OPRTF=0
	OF_NUM = OPRTF
	PLINE(62,69) = ORDFMT

	CALL PRINT2

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
;;;	forms (14,1)
;........................

	FORMS (14,1)

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

	PLINE(70,79) = S_DATE
	CALL PRINT2

	PLINE(28,39) = XPOL2
	CALL PRINT2

	FORMS(14,1)
	WRITES (14, H_LINE2)
	LINCNT = 16
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
;;;	XCALL IO (CHN054,SALMAN,OSLMAN,READ,LOKCTL)
;;;	IF (SLSNM.EQ.']]]]]]' .OR. SLSNM.EQ.']]]DEL' .OR. LOKCTL.NE.0) SLSNM =
	RETURN
;-----------------------------------------------------

GET_DEPTS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	DUCTOR = ORDNO
;;;	LOKCTL = 1
;;;	XCALL ISIO (CHN175,DUCACC,DUCTOR,READ,LOKCTL)
;;;	IF (LOKCTL.EQ.0 .AND. DUCTOR.EQ.ORDNO) 
;;;		BEGIN
;;;		DPTS(1) = 'O'
;;;		NUMDPT = 1
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FIND (CHN045, ORDLIN, OORDNO) [ERR=NXTLIN]
NXTLIN,
	XCALL IOS (CHN045, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF_LIN
	IF (LORDNO .NE. OORDNO) GOTO EOF_LIN
	IF (LROCPO .EQ. 9) 	;SSQ 4-27-04
		BEGIN
		ROCKON = 1		
		GOTO NXTLIN
		END
	IF (LTYPE .EQ. 'M') GOTO NXTLIN		;SKIP MEMOS
;-
	CLEAR TBL_KEY		;SSQ 11-9-04
	TBLCOD = 'EX'
	TBLKEY = EX_ITEM = LITMNO
	FIND (CHN182,COPTBL,TBL_KEY) [ERR=LCONT]	;continue if NOT in table
	GOTO NXTLIN
LCONT,
;-
	CLEAR I
NLI,
	INCR I
;;;	IF (DPTS(I) .EQ. LDEPT) GOTO NXTLIN
	IF (DPTS(I) .EQ. LDEPT(1,1)) GOTO NXTLIN
	IF (DPTS(I) .EQ. '  ') 
		BEGIN
		DPTS(I) = LDEPT(1,1)	;FIRST CHAR ONLY 3-31-00
		NUMDPT = I
		GOTO NXTLIN
		END
	IF (I .LT. MAXDPT) GOTO NLI

EOF_LIN,
	RETURN
;---------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

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
		XCALL FILES (36,'I',001,SWITCH)
		END
	CHN001 = 36

	XCALL CHNOPN (37,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (37,'I',002,SWITCH)
		END
	CHN002 = 37


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
	XCALL DATE8(OPROMD, D_OUT, D_OUTR, S_DATE, D_SW)

	CALL GET_SCAC
	CALL GET_SALESMAN

	XCALL IO (CHN001, DUMCUS, 1, READ, LOKCTL)

	KEY = OCUSNO,'XXXXXX'
	XCALL SERCH (CHN002,CUSIDX,KEY,1,6,ORG001,BSMID,SRCCTL,4,7,11,0,0,0,0)
	LOKCTL = 1
	CASE SRCCTL OF 
	BEGINCASE
	  0:	XCALL IO (CHN001,CUSMAS,IRC001,READ,LOKCTL)
	  1:	BEGIN
		CUSMAS =
		NAME = '* CUSTOMER NOT ON FILE *'
		END
	ENDCASE

	CSZ(1,15) = CITY
	CSZ(17,18) = STATE
	CSZ(20,29) = ZIP

;Create work file...
	XCALL ISAMC (WRKFIL, 127, 1, 'START=1, LENGTH=55, DUPS, ASCEND');ssq 4-11-06
;;;	XCALL ISAMC (WRKFIL, 125, 1, 'START=1, LENGTH=55, DUPS, ASCEND');ssq 4-11-06
	OPEN (33, SU, WRKFIL)
	CHNWRK = 33

	OFORD = ORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (OUTFIL, 127, 1, 'START=1, LENGTH=55, DUPS, ASCEND');ssq 4-11-6
;;;	XCALL ISAMC (OUTFIL, 125, 1, 'START=1, LENGTH=55, DUPS, ASCEND');ssq 4-11-6
	OPEN (34, SU, OUTFIL)
	CHNOUT = 34

	WRORD = ORDNO,'XXXXXX'
;Create output file...
	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (39, SU, DUCFIL)
	CHNDUC = 39

	SPORD = ORDNO,'XXXXXX'
;;;	OPEN (14, O, SPLFIL)

	OPNOK = 1

	RETURN
;-------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN044
	CLOSE CHN045
	CLOSE CHN175
	RETURN
	END
