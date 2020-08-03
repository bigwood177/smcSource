SUBROUTINE SCRN5	;isam
;
;
;	SCRN5.COP
;
;
;		This subroutine reads the records just added to the duct
;		accessory file (DUCACC, from SCRN3 ),and totals up the
;		duct by type.  Then they enter the price per pound
;		they want the duct to be.  Then the price is written back
;		to the DUCACC file
;
;
;	17-JAN-1997 SSQ:	ADD DUCT/LINER PRICE LOOKUP
;	25-JUL-1997 SSQ:	LOSING INFORMATION IN SACCRC ARRAY IF NEW
;				LINE ITEMS ADDED.
;
;	24-mar-1999 ssq:	losing price info if changing from snap-lock
;				seam to pittsburg during mass price change.
;				"reads" doesn't work if you 
;				delete and store records to ducacc file.
;				
;	14-JUL-1999 SSQ:	SCROLLING ACC SCREEN
;
;	23-JUL-1999 SSQ:	Total line item prices and request estimated
;				freight.
;	13-AUG-99 SSQ:		REMOVE FREIGHT MOD
;	27-DEC-99 SSQ:		LPRDCD, LDEPT UPDATES FOR DEPT/WO
;	20-AUG-01 SSQ:		cornermatic corner allocation
;	22-OCT-01 SSQ:		mark line item as taxable if itmmas
;				item is taxable (itmmas.txflag = "Y")
;	24-mar-03 ssq:		add gprice & linprc to config
;	01-dec-10 ssq:		split tierods over 120" into 2 pieces
;	6-17-20: ordlin.lpwnw = 1 => ordlin.lpwgt is XXX,XXX instead of X,XXX.XX


	OORDNO	,D	;  Duct order number
	FULL	,D
	SELECT	,D
	OLOC	,A
	FIL045	,A
	ORGINV	,D
	TAXFLG	,A
	E_FRGHT	,D
	CUSTCD	,A
	XRUNTOT	,D
	XTAXTOT	,D
	CONTR	,D	;1=CONTRACTOR 2-29-16
;
;

;=========================
external function 
	trn3,	d
;=========================

GLOBAL DUCK	;;;,INIT
		.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL

GLOBAL ORDL
RECORD ORDLIN	; ORDER LINE FILE
	.INCLUDE 'DEF:RD045A.def'
RECORD	,X
	.INCLUDE 'DEF:RD045D.def'
RECORD	,X
	.INCLUDE 'DEF:RD045M.def'
ENDGLOBAL

;;;GLOBAL PAR
RECORD PARAM
	.INCLUDE 'DEF:PARAM.DEF'
;;;ENDGLOBAL

GLOBAL IMS
RECORD INVMAS
	.INCLUDE 'DEF:RD041A.def'
RECORD DUMINV,X
	.INCLUDE 'DEF:RD041B.def'
;;;RECORD ITMIDX 
;;;	.INCLUDE 'DEF:RD042A.DEF'
RECORD
	FIL041	,A14		;ITMMAS FILE NAME
ENDGLOBAL

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD
	MAXARY	,D3,	100	;MAXIMUM ARRAY ELEMENTS
; changed element size to match ducrec, 3-24-99 ssq
;;;	SDUCRC	,100A32		;DUCT CONFIGURATION RECORDS (SEE DUCREC BELOW)
;;;	SDUCRC	,100A29		;DUCT CONFIGURATION RECORDS (SEE DUCREC BELOW)
	SDUCRC	,100A45		;DUCT CONFIGURATION RECORDS (SEE DUCREC BELOW)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; now must match type and price...		SSQ 3-24-03
	LINSQF	,9D6	; This is just total of the types of liner sqft
	LINPR	,9D8	; This is price of each type of liner

	LINCFG	,D9	;TYPE+PRICE
	LIDX	,D4
	LMAX	,D3,	024
	LINKEY	,24D9	; TYPE+PRICE
	LXSQF	,24D6	; This is just total of the types of liner sqft
	LXPR	,24D8	; This is price of each type of liner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	QTY	,D5
	ITM	,A15
	DES	,A30
	PRC	,D7
RECORD DUCREC
;;;	DUCCFG	,D7	;CONFIGURATION CODE
	DUCCFG	,D23	;CONFIGURATION CODE
	DUCSQF	,D7	;SQ FEET OF MATERIAL
	DUCPND	,D7	;POUNDS OF MATERIAL
	DUCGPR	,D7	;GAUGE PRICE
	NEWSEAM	,D1	;MASS CHANGE SEAM TYPE
RECORD ACCREC
	.INCLUDE 'DEF:ACCREC.DEF'
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
RECORD
	ACOMPA	,A15
	ACOMPB	,A15
RECORD	,X
	COMPA	,D7
		,A8
	COMPB	,D7
		,A8
RECORD	D_ORD
		,A7,	'ORDER: '
	DORD	,A6

record	feet
	fq	,a6
		,a*,	'ft'

RECORD	VARS
	CMX	,D3	;XX.X	;2-29-16
	bf	,d1
	len	,d3
	len1	,d3
	len2	,d3
	I	,D6	;5-18-06 (was in rd175d.def)
	ROLLS	,D6
	DUCLPR	,D8	;ACTUAL LINER PRICE ENTERED IN SCRN3 SSQ 3-24-03
	SZ1	,D3
	SZ2	,D3

	A_FRT	,A7
	X_FRT		,D1
	NEG_ZERO	,D1,	1
	ZERO		,D1,	0

	DUC_RFA	,A6		;3-24-99 SSQ
	TBL_PRICE_FOUND	,D1
	NUMITM	,D2
	AIDX	,D3		;SACCRC ARRAY INDEX
	TSTAT	,D1
	SWITCH	,D1
	CHN182	,D2
	V	,D1
	READ	,D1,	0
	WRITE	,D1,	1
	STORE	,D1,	2
	DELETE	,D1,	3
	LOKCTL	,D1
	CNGCTL	,D1
	INXCTL	,D1
	WHATNO	,D2
	DECMAL	,D18
	EXEPRC	,D8
;;;	CONFIG	,D7
	CONFIG	,D23
	PPRICE	,D8
	TOTPRC	,D8
	MATTOT	,D7
	LINTOT	,D7
	ACCTOT	,D7
	TAXTOT	,D8
	RUNTOT	,D8	;ORDLIN TOTAL PRICE
	FREGHT	,D7	;ESTIMATED FREIGHT
	ALPHA	,A20
	ENTRY	,A20
	DLRMSK	,A11,	'ZZZ,ZZZ.XX-'
	NUMMSK	,A7,	'ZZZZZX-'
	OPTION	,D1
	BLANKS	,A10
	ZIPFIN	,D1
	J	,D1
	START	,D3
	BSMID	,D5
	BSEND	,D5
	KEY	,A15
	SRCCTL	,D1
;
PROC
	IF (CONTR .EQ. 1)	;SSQ 2-29-16
	THEN	CMX = 100
	ELSE	CMX = 120

	FREGHT = E_FRGHT

	CALL GET_NEG_ZERO

	TBL_PRICE_FOUND = 0
	NUMITM = 4
	CHN182 = 17	;OPENED IN ORDADD

	CALL READ_LINER			;fill param.dliner from coptbl "LP"
			
	V = 1
	for i from 1 thru maxacc	SACCRC(I) =
	for i from 1 thru maxary	SDUCRC(I) =

	for i from 1 thru 9
		BEGIN
		LINSQF(I) =
		LINPR(I) =
		END

	for i from 1 thru LMAX
		BEGIN
		LXSQF(I) =
		LXPR(I) = 
		LINKEY(I) =
		END
FNDDUC,
	DUCTOR = OORDNO
	LOKCTL = 1
	XCALL ISIO (5,DUCACC,DUCTOR,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.OORDNO) GOTO NOFIND
	GOTO SUMMAR
NXTDUC,
	LOKCTL = 1
	XCALL IOS (5,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.OORDNO) GOTO NOMORE
SUMMAR,
	SZ1=%TRN3(SIZE1)
	SZ2=%TRN3(SIZE2)

;;;	CONFIG(1,3) = GAUGE
	clear config
	CONFIG(2,3) = GAUGE
;;;	IF (DUTYPE.EQ.2.OR.DUTYPE.EQ.3) CONFIG(1,3) = THICK
	IF (DUTYPE.EQ.2) CONFIG(1,3) = THICK			;2-18-19 should have been changed for SS mod
	CONFIG(4,4) = DUTYPE
	CONFIG(5,5) = CAT
	CONFIG(6,6) = STY
	CONFIG(7,7) = SEAM
	CONFIG(8,15) = GPRICE		;SSQ 3-24-03
	CONFIG(16,23) = LINPRC		;SSQ 3-24-03
		; Check to see if the configuration has already 
		; been on this order
	I =
	DO BEGIN
	  INCR I
	  DUCREC = SDUCRC(I)
	END
	UNTIL (DUCCFG.EQ.CONFIG.OR.DUCCFG.EQ.0.OR.I.GE.MAXARY)
	IF (I.GE.MAXARY) GOTO BADCON

UPDDUC,
	DUCCFG = CONFIG
	DUCSQF = DUCSQF + SQFEET
	DUCPND = DUCPND + POUNDS
	DUCGPR = GPRICE
	SDUCRC(I) = DUCREC


;-
	LIDX =
	DO	BEGIN
		LINCFG(1,1) = LINER
		LINCFG(2,9) = LINPRC
		INCR LIDX
		END
	UNTIL (LINCFG.EQ.LINKEY(LIDX).OR.LINKEY(LIDX).EQ.0.OR.LIDX.GE.LMAX)
	IF (LIDX.GE.LMAX) GOTO BADCON
;-
	IF (LINER.GE.1.AND.LINER.LE.9.AND.LINER.NE.4)
		BEGIN
		LINSQF(LINER) = LINSQF(LINER) + SQFLIN
		LINPR(LINER) = LINPRC
		LXSQF(LIDX) = LXSQF(LIDX) + SQFLIN
		LXPR(LIDX) = LINPRC
		LINKEY(LIDX) = LINCFG
		END

					;Summarize accesories
	IF(TR1QTY)	CALL AC_TR1
	IF(TR2QTY)	CALL AC_TR2
	IF(HLQTY)	CALL AC_HL

	IF(ACC.EQ.2)GOTO NXTDUC		;No accessories

	IF(SLIPS)	CALL AC_SLIPS
	IF(DRIVES)	CALL AC_DRIVES
	IF(TCORN)	CALL AC_TCORN
	IF(TNBQTY)	CALL AC_TNB
	IF(TGAQTY)	CALL AC_TGA
	IF(TCLQTY)	CALL AC_TCL
	IF(TBAQTY)	CALL AC_TBA
	IF(TBNQTY)	CALL AC_TBN
	IF(CMQTY)	CALL AC_CM
	GOTO NXTDUC

AC_SLIPS,	;;;;;;;;;;;;;;;;;;;
	QTY = SLIPS
	ITM = SLPITM
	PRC = SLPPRC
	DES = SLPDES
	CALL SUMACC
	RETURN
;----------------------------------

AC_DRIVES,	;;;;;;;;;;;;;;;;;;;
	QTY = DRIVES
	ITM = DRVITM
	PRC = DRVPRC
	DES = DRVDES
	CALL SUMACC
	RETURN
;----------------------------------
AC_TCORN,	;;;;;;;;;;;;;;;;;;;
	QTY = TCORN
	ITM = CORITM
	PRC = CORPRC
	DES = CORDES
	CALL SUMACC
	RETURN
;----------------------------------
AC_TNB,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	QTY = TNBQTY
	ITM = TNBITM
	PRC = TNBPRC
	DES = TNBDES
	CALL SUMACC
	RETURN
;----------------------------------
AC_TGA,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	QTY = TGAQTY
	ITM = TGAITM
	PRC = TGAPRC
	DES = TGADES
	CALL SUMACC
	RETURN
;----------------------------------
AC_TCL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	QTY = TCLQTY
	ITM = TCLITM
	PRC = TCLPRC
	DES = TCLDES
	CALL SUMACC
	RETURN
;----------------------------------
AC_TBA,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	QTY = TBAQTY / (SZ1 + SZ2)
	ITM = TBAITM
	ITM(7,8) = SZ1,'XX'
	IF (SZ1.GT.99) ITM(7,9) = SZ1,'XXX'
	PRC = TBAPRC * SZ1
	DES = TBADES
	DES(9,12) = SZ1,'ZZX"'
	CALL SUMACC
	QTY = TBAQTY / (SZ1 + SZ2)
	ITM = TBAITM
	ITM(7,8) = SZ2,'XX'
	IF (SZ2.GT.99) ITM(7,9) = SZ2,'XXX'
	PRC = TBAPRC * SZ2
	DES = TBADES
	DES(9,12) = SZ2,'ZZX"'
	CALL SUMACC
	RETURN
;----------------------------------
AC_TBN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	QTY = TBNQTY
	ITM = TBNITM
	PRC = TBNPRC
	DES = TBNDES
	CALL SUMACC
	RETURN
;----------------------------------
AC_TR1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	QTY = TR1QTY
	ITM = TR1ITM
	PRC = TR1PRC
	DES = TR1DES
	call long_rod
	CALL SUMACC
	RETURN
;----------------------------------
AC_TR2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	QTY = TR2QTY
	ITM = TR2ITM
	PRC = TR2PRC
	DES = TR2DES
	call long_rod
	CALL SUMACC
	RETURN
;----------------------------------
AC_HL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	QTY = HLQTY
	ITM = HLITM
	PRC = HLPRC
	DES = HLDES
	CALL SUMACC
	RETURN
;----------------------------------
AC_CM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	QTY = CMQTY
	ITM = CMITM
	PRC = CMPRC
	DES = CMDES
	CALL SUMACC
	RETURN
;----------------------------------
long_rod,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; split long tierods
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall alpdc (itm(4,6), len, bf)
	if (bf) return			;not numeric

	if (len .le. 120) return	;don't need to split

	len1 = len/2
	len2 = len - len1

	itm(4,6) = len1,	'ZXX' [left]
	accitm = itm		;the stud...
	call lr_item
	if (srcctl.eq.0) call sumacc

	itm(4,6) = len2,	'ZXX' [left]
	accitm = itm		;the stud...
	call lr_item
	if (srcctl.eq.0) call sumacc

	accitm = 'CDS'		;the stud...
	call lr_item
	return
;-------------------------------------------------

lr_item,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	call getitm
	if (srcctl .eq. 0)
	then	begin
		prc = price
		des = descr
		itm = accitm
		end
	else	qty = 0		;don't update sumacc again...

	return
;-------------------------------------------------
SUMACC,
		; Check to see if the accessories have already 
		; been on this order
	I =
	DO BEGIN
	  INCR I
	  ACCREC = SACCRC(I)
	END
	UNTIL (ACCITM.EQ.ITM.OR.ACCITM.EQ.BLANKS.OR.I.GE.MAXACC)
	IF (I.GE.MAXACC) 
	BEGIN
	  CALL BADACC
	  GOTO NXTDUC
	END
	ACCQTY = ACCQTY + QTY
	ACCITM = ITM
	ACCPRC = PRC
	ACCDES = DES
	SACCRC(I) = ACCREC
	RETURN

NOMORE,
;
;	BUBBLE SORT SDUCRC ARRAY
;
;;;	close 5		;10-5-16
	unlock 5	;10-11-16


	XCALL OUTPT (2,1,1,'Sort Duct Configuration Array',1)
	for i from 1 thru maxary
	  BEGIN
	  DUCREC = SDUCRC(I)
	  IF (DUCCFG.EQ.0) GOTO SRTACC
	  COMPA = DUCCFG
	  DUCREC = SDUCRC(I+1)
	  IF (DUCCFG.EQ.0) GOTO SRTACC
	  COMPB = DUCCFG
	  IF (COMPA.GT.COMPB)
	  BEGIN
	    SDUCRC(I+1) = SDUCRC(I)
	    SDUCRC(I) = DUCREC
	    I = I - 2
	    IF (I.LT.0) I = 0
	  END
	END
	GOTO BADCON
;
;	BUBBLE SORT SACCRC ARRAY
;
SRTACC,
	XCALL OUTPT (2,1,1,'Sort Accessory Array',1)
;;;	FOR I = 1 STEP 1 UNTIL MAXACC
;;;	DO BEGIN
	for i from 1 thru maxacc
	  BEGIN
	  ACCREC = SACCRC(I)
	  IF (ACCITM.EQ.BLANKS) GOTO SHOW
	  ACOMPA = ACCITM
	  ACCREC = SACCRC(I+1)
	  IF (ACCITM.EQ.BLANKS) GOTO SHOW
	  ACOMPB = ACCITM
	  IF (ACOMPA.GT.ACOMPB)
	  BEGIN
	    SACCRC(I+1) = SACCRC(I)
	    SACCRC(I) = ACCREC
	    I = I - 2
	    IF (I.LT.0) I = 0
	  END
	END
	GOTO BADCON

SHOW,
;	Display the configurations that were matched for the order
;	Then display the total square feet of each liner
;
	DORD = OORDNO,	'ZZZZZX' [LEFT]
	XCALL OUTPT(1,1,0,D_ORD,1)

	CASE SELECT OF
	BEGINCASE
	0:	XCALL OUTPT (2,1,1,'Summarize Duct Order',1)
	4:	XCALL OUTPT (2,1,1,'Mass Change',1)
	5:	XCALL OUTPT (2,1,1,'Resummarize Accessories',1)
	ENDCASE
	MATTOT =
	ROW = 4
	XCALL OUTPT (ROW,1,2,'Pricing for Ductwork',V)
;;;	DISPLAY (15,27,'[6;21r')
	ROW = 5
	for i from 1 thru maxary
	  BEGIN
	  DUCREC = SDUCRC(I)
	  IF (DUCCFG.EQ.0) GOTO ENDSHO

	  INCR ROW
	  IF (ROW.GE.22)
	  BEGIN
	    XCALL OUTPT (21,1,0,'\',1)
	    DISPLAY (15,10,10,10,10)
	    ROW = 18
	  END
	  ALPHA(1,4) = DUCCFG(1,3),'ZZZX'
	  ALPHA(5,10) = ' Gauge'
;;;	  IF (DUCCFG(4,4).EQ.2.OR.DUCCFG(4,4).EQ.3)
	  IF (DUCCFG(4,4).EQ.2)				;2-18-19
	  BEGIN
	    ALPHA(1,4) = DUCCFG(1,3),'.XXX'
	    ALPHA(5,10) = ' Thick'
	  END
	  XCALL OUTPT (ROW,2,0,ALPHA(1,10),V)
	  XCALL OUTPT (ROW,14,0,DTYPE(DUCCFG(4,4)),V)
	  XCALL OUTPT (ROW,31,0,DCAT(DUCCFG(5,5)),V)
	  XCALL OUTPT (ROW,48,0,DSTY(DUCCFG(6,6)),V)
	  XCALL OUTPT (ROW,65,0,DSEAM(DUCCFG(7,7)),V)
	  ALPHA (1,7) = DUCSQF,NUMMSK
	  ALPHA (8,14) = DUCPND,NUMMSK
	  INCR ROW
	  XCALL OUTPT (ROW,6,0,'Square feet ',V)
	  XCALL OUTPT (0,0,0,ALPHA(1,7),V)
	  INCR ROW
	  XCALL OUTPT (ROW,6,0,'Duct Pounds ',V)
	  XCALL OUTPT (0,0,0,ALPHA(8,14),V)
	  INCR ROW

;=========================================================
	  XCALL OUTPT (ROW,6,0,'      Price per pound  ',V)
	  PPRICE = DUCGPR
	  ALPHA(1,9) = PPRICE, 'ZZZZZ.XXX'
	  XCALL OUTPT (ROW,30,0,ALPHA(1,9),V)
	  DECMAL = ( PPRICE * DUCPND ) # 1
	  ALPHA(1,11) = DECMAL,DLRMSK
	  XCALL OUTPT (ROW,40,0,'Total price =',V)
 	  XCALL OUTPT (0,0,0,ALPHA(1,11),V)
	  DO BEGIN
	     IF (CNGCTL.EQ.1 .OR.PPRICE.EQ.0)
		BEGIN
		CTL(4,14) = '30,06,01,#T'
;;;		XCALL OUTPT (ROW,6,0,'Enter price per pound  ',V)
		XCALL OUTPT (ROW,6,1,'Enter price per pound             <TAB> = Table look-up',V)
		CALL INPUT
		CASE INXCTL OF
		BEGINCASE
		0:	PPRICE = ENTRY
	
		3:	BEGIN
			CLEAR TBL_KEY
			TBLCOD = 'DP'	
			DP_GAUGE = DUCCFG(1,3)
			DP_TYPE = DUCCFG(4,4)
			DP_CAT = DUCCFG(5,5)
			DP_STYLE = DUCCFG(6,6)
			CALL GET_TABLE
			IF (LOKCTL.NE.0) CLEAR DP_PRICE
			PPRICE = DP_PRICE
			END
		ENDCASE
		ALPHA(1,9) = PPRICE, 'ZZZZZ.XXX'
		XCALL OUTPT (ROW,30,0,ALPHA(1,9),V)
		DECMAL = ( PPRICE * DUCPND ) # 1
		ALPHA(1,11) = DECMAL,DLRMSK
		XCALL OUTPT (ROW,40,0,'Total price =',V)
		XCALL OUTPT (0,0,0,ALPHA(1,11),V)
		END

DSPPRC,
	    IF (CNGCTL.EQ.2) 
	    BEGIN
	      NEWSEAM =
	      for j from 1 thru 2 XCALL OUTPT (ROW+J+1,30,0,DSEAM(J),1)
	      INCR ROW
	      XCALL OUTPT (ROW,6,1,'  Enter new seam type  ',V)
	      CTL(4,14) = '30,01,01,# '
	      CALL INPUT
	      ROW = ROW - 1
	      GOTO (DSPPRC), INXCTL
	      NEWSEAM = ENTRY
	      IF (NEWSEAM.LT.0.OR.NEWSEAM.GT.2) GOTO DSPPRC
	      IF (NEWSEAM.EQ.DUCCFG(7,7)) NEWSEAM =
	      IF (NEWSEAM) XCALL OUTPT (ROW+1,30,0,DSEAM(NEWSEAM),V)
	      IF (NEWSEAM.EQ.0) XCALL OUTPT (ROW+1,30,0,'No Change',V)
	      for j from 1 thru 2 XCALL OUTPT (ROW+J+1,30,1,'\',1)
	    END

	    XCALL OUTPT (24,1,1,'ANY CHANGE <N> ? .   1-MASS PRICE    2-SEAM',1)
	    XCALL INPUT (24,18,01,00,'#E',ENTRY,INXCTL,1)
	    CNGCTL = ENTRY
	    IF (INXCTL.EQ.2) CNGCTL = -1
	    IF (CNGCTL.EQ.0 .AND. PPRICE.EQ.0)
		BEGIN
		XCALL OUTPT (24,1,1,'Leave price per pound zero ?',1)
		XCALL INPUT (24,31,01,01,'YN',ENTRY,INXCTL,1)
		CNGCTL = INXCTL - 1
		END
	  END UNTIL (CNGCTL.EQ.0)
;=========================================================
	  IF (NEWSEAM) INCR ROW

	  MATTOT = MATTOT + DECMAL
	  DUCGPR = PPRICE
	  SDUCRC(I) = DUCREC

	END
ENDSHO,
;
; Display the total square feet of liner by type of liner.
;
;;;	DISPLAY (15,27,'[1;24r')	;Reset scrolling region
	LINTOT =
	ROW = 4
	XCALL OUTPT (ROW,1,2,'Pricing for Liner',V)
;;;	for i from 1 thru 8
	for i from 1 thru lmax		;ssq 4-9-03
	 BEGIN
;;;	 IF ( LINSQF(I).NE.0 )
	 IF (LXSQF(I).NE.0 )		;SSQ 4-9-03
	 BEGIN
	    ROW = ROW + 2
;;;	    ALPHA(1,7) = LINSQF(I),NUMMSK
	    ALPHA(1,7) = LXSQF(I),NUMMSK	;SSQ 4-9-03
	    XCALL OUTPT (ROW,2,0,ALPHA(1,7),V)
	    XCALL OUTPT (0,0,0,' Square feet of ',V)
		LINCFG = LINKEY(I)
		LIDX = LINCFG(1,1)
;;;	    XCALL OUTPT (0,0,0,DLINER(I),V)
	    XCALL OUTPT (0,0,0,DLINER(LIDX),V)	;SSQ 4-9-03
	    INCR ROW
	    DO BEGIN
	       XCALL OUTPT (ROW,6,0,'      Price per square foot  ',V)
	       CTL(4,14) = '34,06,00,#T'
	       IF (CNGCTL)
	       THEN BEGIN
		    CALL INPUT
		    CASE INXCTL OF
		    BEGINCASE
		   0:	PPRICE = ENTRY
		   3:	BEGIN
			CLEAR TBL_KEY
			TBLCOD = 'LP'	
			LP_KEY = I
			CALL GET_TABLE
			IF (LOKCTL.NE.0) CLEAR DP_PRICE
			PPRICE = DP_PRICE
			END
		   ENDCASE
		END
		ELSE	BEGIN
			XCALL OUTPT (ROW,6,0,'      Price per square foot  ',V)
	;;;		PPRICE = LINPR(I)
			PPRICE = LXPR(I)
			END
		ALPHA(1,7) = PPRICE, 'ZZZ.XXX'
		XCALL OUTPT (ROW,34,0,ALPHA(1,7),V)
	;;;	DECMAL = ( PPRICE * LINSQF(I)) # 1
		DECMAL = ( PPRICE * LXSQF(I)) # 1
		ALPHA(1,11) = DECMAL,DLRMSK
		XCALL OUTPT (ROW,43,0,'Liner price =',V)
		XCALL OUTPT (0,0,0,ALPHA(1,11),V)
		CNGCTL = 4
		CALL ANYCNG
	     END 
	     UNTIL (CNGCTL.EQ.0)


;;;	  LINPR (I) = PPRICE	; Set the price for this configuration 
	  LXPR (I) = PPRICE	; Set the price for this configuration 
				; to the correct liner
;;;	  LINTOT = LINTOT + ( ( LINPR(I) * LINSQF(I) ) #1)
	  LINTOT = LINTOT + ( ( LXPR(I) * LXSQF(I) ) #1)

	 END
	END
;
; SUMMARIZE ACCESSORY INFORMATION & ALLOW CHANGES
;
RE_SUM,
	ACCTOT =
	ZIPFIN = 1
	ROW = 4
	START = 1
	XCALL OUTPT (ROW,1,2,'Summarized Accessories',V)
	ROW = ROW + 2
	XCALL OUTPT (ROW,1,0,'---------------- ITEM -------------------------  TOTAL      UNIT   EXTENDED',1)
	INCR ROW
	XCALL OUTPT (ROW,1,0,'ITEM NUMBER      DESCRIPTION                       QTY     PRICE      PRICE',1)

SUMLP,
	for aidx from start thru maxacc
	   BEGIN		;=============================================*
	   ACCREC = SACCRC(AIDX);					      *
	   IF (ACCITM.EQ.BLANKS) GOTO ENDACC	;			      *
	   INCR ROW				;			      *
	   IF (ROW.GE.22)			;			      *
	      BEGIN				;			      *
	      XCALL OUTPT (21,1,0,'\',1)	;			      *
	      DISPLAY (15,10)			;			      *
	      ROW = 21				;			      *
	      END				;			      *
DSPACC,						;			      *
	   CNGCTL =				;			      *
	   XCALL OUTPT (ROW,1,0,ACCITM,1)	;			      *
	   XCALL OUTPT (ROW,18,0,ACCDES,1)	;			      *
		if (accitm .eq. 'TG')
			begin
			fq = accqty, 'ZZZZZZZZZX'
		;;;	xcall mesag (feet,1)
			xcall outpt (row,5,0,feet,1)
			LUOFM = 'RL'	
			ROLLS = (accqty*100/50)#2	;50' ROLLS
			IF (accqty .GT. (ROLLS*50)) ROLLS = ROLLS + 1
			accqty = ROLLS
			end

	   ALPHA(1,6) = ACCQTY,NUMMSK		;			      *
	   XCALL OUTPT (ROW,50,0,ALPHA(1,6),1)	;			      *
	   ALPHA(1,9) = ACCPRC,'ZZZZ.XXX-'	;			      *
	   XCALL OUTPT (ROW,58,0,ALPHA(1,9),1)	;			      *
	   DO	BEGIN	;------------------------------------------------     *
	     	IF (SELECT.EQ.0.OR.CNGCTL);				|     *
	           BEGIN	;--------------------------------	|     *
	           IF (CNGCTL) CASE WHATNO OF;			|	|     *
	           BEGINCASE;					|	|     *
	           1:	BEGIN;					|	|     *
		;;;	CTL (4,14) = '50,05,00,# '	;	|	|     *
			CTL (4,14) = '50,05,00,#-'	;	|	|     *
			CALL INPUT			;	|	|     *
			GOTO (DSPACC), INXCTL		;	|	|     *
			ACCQTY = ENTRY			;	|	|     *
			ALPHA(1,6) = ACCQTY,NUMMSK	;	|	|     *
			XCALL OUTPT (ROW,50,0,ALPHA(1,6),1);	|	|     *
			CALL NEW_PRICE		;		|	|     *
			END			;		|	|     *
	           2:	BEGIN			;		|	|     *
			CTL (4,14) = '58,07,00,# ';		|	|     *
			CALL INPUT	;			|	|     *
			GOTO (DSPACC), INXCTL;			|	|     *
			ACCPRC = ENTRY;				|	|     *
			ALPHA(1,9) = ACCPRC,'ZZZZ.XXX-'	;	|	|     *
			XCALL OUTPT (ROW,58,0,ALPHA(1,9),1);	|	|     *
			END;					|	|     *
	           ENDCASE;					|	|     *
	    	   END		;-------------------------------|	|     *
	   	DECMAL = (ACCQTY * ACCPRC ) #1	;			|     *
	   	ALPHA(1,11) = DECMAL, DLRMSK	;			|     *
	   	XCALL OUTPT (ROW,69,0,ALPHA(1,11),1);			|     *
	   	IF (ZIPFIN) ;						|     *
		DO	BEGIN					;	|     *
			XCALL OUTPT (24,1,0,'Any Change ? .   
&	                   1. Qty  2. Price  <F12> Zip Finish',1);	|     *
			XCALL INPUT (24,14,01,00,'#E',ENTRY,INXCTL,1);	|     *
			WHATNO = ENTRY				;	|     *
			IF (INXCTL.EQ.2) ZIPFIN =		;	|     *
			END ;						|     *
			UNTIL (WHATNO.GE.0.AND.WHATNO.LE.2)	;	|     *
	   	IF (WHATNO) 					;	|     *
	   	THEN	CNGCTL = 1;					|     *
	   	ELSE	CNGCTL =	;				|     *
	   	END 	;-----------------------------------------------|     *
	   	UNTIL (CNGCTL.EQ.0)	;				      *
;									      *
	   ACCTOT = ACCTOT + DECMAL	;				      *
	   SACCRC(AIDX) = ACCREC	;				      *
	   END		;=====================================================*

	GOTO ENDACC
;
;	end accessory summarize, CHECK FOR ADDING ACCESSORIES
;		
ABOACC,
	XCALL OUTPT (ROW,1,1,'\',1)
	ROW = ROW - 1
ENDACC,
	ACCREC =	
	XCALL OUTPT (24,1,1,'Add additional accessories <N> ? .',1)
	XCALL INPUT (24,34,01,00,'YN',ENTRY,INXCTL,1)
	IF (INXCTL.NE.1) GOTO FINSUM
	INCR ROW
	IF (ROW.GE.22)	CALL SHIFT
;-----------------------------------------

ACCITM,
	CTL (4,14) = '01,15,00,AE'
	CALL INPUT
	GOTO (ABOACC,ABOACC), INXCTL
	IF (ENTRY.EQ.BLANKS) GOTO ABOACC
	ACCITM = ENTRY
;***************************************************
;GET ITEM FROM INVENTORY
;***************************************************
;;;	BSEND = ORGINV
;;;	KEY = ACCITM
;;;	XCALL SERCH (2,ITMIDX,KEY,1,15,ORGINV,BSMID,SRCCTL,4,16,20,0,0,0,0)
;;;	IF (SRCCTL.OR.(IRC041.LE.0))

	LOKCTL = 1
	XCALL ISIO (1, INVMAS, ACCITM, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
	BEGIN
	  XCALL MESAG ('ITEM NOT FOUND',2)
	  INVMAS = 
;;;;	  ITMIDX = 
	  GOTO ACCITM
	END

;;;	LOKCTL = 1
;;;	XCALL IO (1,INVMAS,IRC041,READ,LOKCTL)
	CALL TBL_PRICE		;SSQ 7-18-97

	ACCDES = DESCR
	ACCPRC = PRICE
	ACCSTK = STOCK
	ACCCAT = PRDCAT
	ACCTAX = TXFLAG
;***************************************************
	XCALL OUTPT (ROW,18,0,ACCDES,1)
;;;	CTL (4,14) = '50,05,00,# '
	CTL (4,14) = '50,05,00,#-'
	CALL INPUT
	GOTO (ABOACC), INXCTL
	ACCQTY = ENTRY
	CALL NEW_PRICE		;SSQ 7-18-97

	ALPHA(1,9) = ACCPRC,'ZZZZ.XXX-'
	XCALL OUTPT (ROW,58,0,ALPHA(1,9),1)

	ZIPFIN = 1
	ROW = ROW - 1

	START = AIDX
	SACCRC(AIDX) = ACCREC
	GOTO SUMLP

;
;	DISPLAY FINAL TOTAL SUMMARY
;
FINSUM,
	CALL RUNTOT		;LINE ITEM TOTAL
	ROW = 4
	XCALL OUTPT (ROW,1,2,'           Duct work total = ',V)
	ALPHA (1,11) = MATTOT,DLRMSK
	XCALL OUTPT (0,0,0,ALPHA(1,11),V)
	INCR ROW
	XCALL OUTPT (ROW,1,2,'               Liner total = ',V)
	ALPHA (1,11) = LINTOT,DLRMSK
	XCALL OUTPT (0,0,0,ALPHA(1,11),V)
	INCR ROW
	XCALL OUTPT (ROW,1,2,'         Accessories total = ',V)
	ALPHA (1,11) = ACCTOT,DLRMSK
	XCALL OUTPT (0,0,0,ALPHA(1,11),V)
	INCR ROW
	XCALL OUTPT (ROW,1,2,'                             ',V)
	XCALL OUTPT (0,0,0,'-----------',V)
	INCR ROW
	XCALL OUTPT (ROW,1,2,'Duct/Liner/Accessory total   ',V)
	TOTPRC = MATTOT + LINTOT + ACCTOT
	TAXTOT = TAXTOT + TOTPRC		;SSQ 4-11-02
	ALPHA (1,11) = TOTPRC,DLRMSK
	XCALL OUTPT (0,0,0,ALPHA(1,11),V)

;---------------------------------------------------------
	INCR ROW
	XCALL OUTPT (ROW,1,2,'           Line item total = ',V)
	ALPHA (1,11) = RUNTOT,DLRMSK
	XCALL OUTPT (0,0,0,ALPHA(1,11),V)
	INCR ROW
	XCALL OUTPT (ROW,1,2,'                             ',V)
	XCALL OUTPT (0,0,0,'-----------',V)
	INCR ROW
	XCALL OUTPT (ROW,1,2,'               Order total   ',V)
	TOTPRC = TOTPRC + RUNTOT
	ALPHA (1,11) = TOTPRC,DLRMSK
	XCALL OUTPT (0,0,0,ALPHA(1,11),V)
	ROW = ROW + 2

;---------------------------------------------------------
UPDATE,
	XCALL OUTPT (2,1,1,'Update Duct Pricing Information',1)
	DUCTOR = OORDNO
;---------------------------------------------------------

	FIND (5, DUCACC, DUCTOR) [ERR=ENDUPD]
UPDNXT,
;---------------------------------------------------------
; 3-24-99 SSQ
;;;	READS (5, DUCACC, ENDUPD, GETRFA:DUC_RFA) [ERR=ENDUPD]
	READS (5,DUCACC,ENDUPD,GETRFA:DUC_RFA) [LOCK=UPN_LOCKED]
	IF (DUCTOR .NE. OORDNO) GOTO ENDUPD
;---------------------------------------------------------
UPDARY,
	CONFIG(1,3) = GAUGE
;;;	IF (DUTYPE.EQ.2.OR.DUTYPE.EQ.3) CONFIG(1,3) = THICK
	IF (DUTYPE.EQ.2) CONFIG(1,3) = THICK			;2-18-19
	CONFIG(4,4) = DUTYPE
	CONFIG(5,5) = CAT
	CONFIG(6,6) = STY
	CONFIG(7,7) = SEAM
	CONFIG(8,15) = GPRICE		;SSQ 3-24-03
	CONFIG(16,23) = LINPRC		;SSQ 3-24-03
		; Find this configuration in array
	I =
	DO BEGIN
	  INCR I
	  DUCREC = SDUCRC(I)
	END
	UNTIL (DUCCFG.EQ.CONFIG.OR.DUCCFG.EQ.0.OR.I.GE.MAXARY)
	IF (I.GE.MAXARY) GOTO BADCON

	IF (DUCCFG .EQ. 0) GOTO UPDNXT		;SSQ 3-26-99 - assume these
				;are records stored as a result of seam change

	GPRICE = DUCGPR

;----------------------------------------------

;;;	LINPRC = DUCCFG(16,23)		;LINER PRICE PART OF KEY SSQ 3-24-03
	LINCFG(1,1) = LINER
	LINCFG(2,9) = DUCCFG(16,23)
	LIDX =
	DO	BEGIN
		INCR LIDX
		END
	UNTIL (LINCFG.EQ.LINKEY(LIDX).OR.LINKEY(LIDX).EQ.0.OR.LIDX.GE.LMAX)
	IF (LIDX .LT. LMAX) LINPRC = LXPR(LIDX)
;;;	IF (LINER) LINPRC = LINPR(LINER)
;----------------------------------------------

;;;	IF (ACC.NE.1) GOTO WRTDUC	;if no accessories - skip clearing
;Clear out accessories before adding SACCRC array to ORDLIN
	LICRE = 1	;l/i was created
	ACC = 2
	SLPITM =
	SLPDES =
	SLIPS =
	SLPPRC =
	DRVITM =
	DRVDES =
	DRIVES =
	DRVPRC =
	CORITM =
	CORDES =
	TCORN =
	CORPRC =
	TNBITM =
	TNBDES =
	TNBQTY =
	TNBPRC =
	TGAITM =
	TGADES =
	TGAQTY =
	TGAPRC =
	TCLITM =
	TCLDES =
	TCLQTY =
	TCLPRC =
	TBAITM =
	TBADES =
	TBAQTY =
	TBAPRC =
	TBAITM =
	TBADES =
	TBAQTY =
	TBAPRC =
	TBNITM =
	TBNDES =
	TBNQTY =
	TBNPRC =
	TR1QTY =
	TR1ITM =
	TR1PRC =
	TR1DES =
	TR2QTY =
	TR2ITM =
	TR2PRC =
	TR2DES =
	HLQTY =
	HLITM =
	HLPRC =
	HLDES =
	CMQTY =
	CMITM =
	CMPRC =
	CMDES =
	
WRTDUC,
	IF (NEWSEAM.NE.0.AND.NEWSEAM.NE.SEAM)
	THEN BEGIN
; store record first...
	  SEAM = NEWSEAM
	  LOKCTL = 1
	  XCALL ISIO (5,DUCACC,KEY175,STORE,LOKCTL)

; read back the orignial record & delete it, leaving ptr in correct
; position for next "reads"...
	  FIND (5, DUCACC, RFA:DUC_RFA) ;;;[ERR=ENDUPD]
	  READS (5, DUCACC, ENDUPD) ;;;[ERR=ENDUPD]
	  LOKCTL = 1
	  XCALL ISIO (5,DUCACC,KEY175,DELETE,LOKCTL)
	END
	ELSE BEGIN
	  LOKCTL = 1
	  XCALL ISIO (5,DUCACC,KEY175,WRITE,LOKCTL)
	END
	GOTO UPDNXT

UPN_LOCKED,

	XCALL MESAG ("Clearing Duct Acc - Record locked",2)
	GOTO UPDNXT
;----------------------------------------------------------

ENDUPD,
;;;;;;;;	;update SACCRC array to ORDLIN file
;;;
	XCALL OUTPT (2,1,1,'Update Accessory Array to ORDLIN',1)
	CLOSE 5
	OPEN (12,SU,FIL045)
	LINSEQ = 900
	IF (SELECT.EQ.5) LINSEQ = 940
	for i from 1 thru maxacc
	  BEGIN
	  ACCREC = SACCRC(I)
	  IF (ACCITM.NE.BLANKS.AND.ACCQTY.NE.0)
	  BEGIN
	  clear ordlin		;3-22-00 ssq
	    LTYPE = 'L'		;3-24-00 SSQ
	    LORDNO = OORDNO	; ORDER NUMBER
	    INCR LINSEQ		; LINE PRINT SEQUENCE

	    if (accitm .eq. 'J35')
		begin
		lcfgim = accitm
		accitm(4,15) =
		end

	    LITMNO = ACCITM	; ITEM NUMBER
	    CALL GETITM		; GET ITEM FROM ITMMAS

	    LDESCR = ACCDES	; ITEM DESCRIPTION
	    LQTYOR = ACCQTY	; QUANTITY ORDERED
	    LQTYSH =		; QUANTITY SHIPPED
	    LQTYBO =		; QUANTITY BACK ORDERED
	    LLOC = OLOC		; LOCATION (ORD/EST)
	    LPRDCD = PRDCAT	; PRODUCT CATEGORY
	    LSRTSQ = PRICCD	; SORT CODE 5-3-00 SSQ
;;;	    IF (LPRDCD.EQ.'  ') LPRDCD = '11'
	    IF (LPRDCD.EQ.'  ') LPRDCD = 'O'	;DUCTWORK
	    LDEPT = USRDEF
	    IF(LDEPT .EQ. '  ')	    LDEPT = 'O'		;DUCTWORK
	    LSTOKT = STOCK	; STOCKED ITEM FLAG (S=STOCKED, N=NON-STOCKED)
	    IF (LSTOKT.EQ.'  ') LSTOKT = 'N'
	    LUOFM = SUOFM	; UNIT OF MEASURE from itmmas

;;;	    IF (ACCITM.EQ.'TG') LUOFM = 'FT'	;SSQ 7-18-97
	    IF (ACCITM.EQ.'TG') 		;SSQ 4-04-06
		BEGIN
		LUOFM = 'RL'	
;;;		ROLLS = (LQTYOR*100/50)#2	;50' ROLLS
;;;		IF (LQTYOR .GT. (ROLLS*50)) ROLLS = ROLLS + 1
;;;		LQTYOR = ROLLS
		END
	    LPRICE = ACCPRC	; PRICE ($XX,XXX.XXX)
	    LPRICE = (LPRICE*CMX)#3*10	;2-29-16

	    LODISC =		; ORDER DISCOUNT (XX%)
	    LDISC =		; LINE DISCOUNT (XX%)
	    LSTATS = 		; LINE STATUS (1=BACKORDER STATUS, B/Oable 
	    			;	       2=BACKORDER STATUS, Non-B/Oable
	    LCOST =		; UNIT COST ($XX,XXX.XXX)
	    LFLAG =		; SELECT FOR BILLING FLAG (0=NOT SEL, 1=SEL
	    		       	;       2=INVOICED, 3=REMOVED FROM FILE)
	    LITMWT =		; ITEM WEIGHT (X,XXX.XX)
	    LPWGT = IP_WGT	; 6-1-20 PRODUCTION WEIGHT

;-
	CLEAR LPWNW
	IF (IP_UM .EQ. '$') 	;7-10-17
		BEGIN
		CLEAR TBL_KEY
		TBLCOD='DD'
		TBLKEY=USRDEF
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
		IF (.NOT.LOKCTL)
			BEGIN
			LPWGT=(DD_DLR*LPRICE)#4
			LPWNW = 1			;XXX,XXX
			END
		END
;-

	    LTXFLG = 0		; ITEM TAX FLAG (0=NON-TAXABLE, 1=TAXABLE)
;;;	    IF (TAXFLG.NE.'   '.AND.TXFLAG.EQ.'Y') LTXFLG = 1
	    IF (TXFLAG.EQ.'Y') LTXFLG = 1	;SSQ 10-22-01

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ssq 8-21-03 these fields now filler...
;;;	    LEXSDT =		; EXPECTED SHIP DATE (YYMMDD)
;;;	    LPRMDT =		; PROMISED DATE	(YYMMDD)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	   
	    LOKCTL = 1
	    XCALL ISIO (12,ORDLIN,ORDKEY,STORE,LOKCTL)

	key = accitm		;11-4-13: read below expectes key

; cornermatic corners...
	IF (ACCITM(1,2) .EQ. 'CM') 
		BEGIN
		KEY = ACCITM(1,3)

	;;	XCALL SERCH (2,ITMIDX,KEY,1,15,ORGINV,BSMID,SRCCTL,4,16,20,0,0,0,0)
	;;;	IF (SRCCTL.OR.(IRC041.LE.0))
		
		XCALL ISIO (1, INVMAS, KEY, READ, LOKCTL)
		IF (LOKCTL .NE. 0)
			BEGIN
			XCALL MESAG ('error updating corner',2)
			END
	;;;	ELSE	BEGIN
	;;;		LOKCTL = 1
	;;;		XCALL IO (1,INVMAS,IRC041,READ,LOKCTL)
	;;;		END
		END		;SSQ 8-22-01, yeah, gots to have it here!
;-

	    IF (LSTOKT.EQ.'S' .AND. LLOC.EQ.'O')	;SSQ 10-28-98
		BEGIN
		CLOSE 1
		OPEN (1, SU, FIL041)

		LOKCTL = 1
		XCALL ISIO (1, INVMAS, KEY, READ, LOKCTL)	;re-read
		QTYCOM = QTYCOM + LQTYOR
		XCALL ISIO (1,INVMAS,KEY,WRITE,LOKCTL)	;ISAM
	;;;	XCALL IO (1,INVMAS,IRC041,WRITE,LOKCTL)	;SSQ 7-18-97
		CLOSE 1

		OPEN (1, SI, FIL041)
		END
	  END
	END
	CLOSE 12
;;;;;;;;
	XRUNTOT = TOTPRC
	XTAXTOT = TAXTOT

	XCALL MESAG ('*',2)
	RETURN
;;;

RUNTOT,	;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Add line items
	;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR RUNTOT,TAXTOT
	OPEN (12,SI,FIL045)
	FIND (12, ORDLIN, OORDNO) [ERR=N_ORD]
N_ORD,
	XCALL IOS (12, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF_LIN
	IF (LORDNO .NE. OORDNO) GOTO EOF_LIN
	IF (LTYPE .EQ. 'M') GOTO N_ORD		;SKIP MEMOS

	DECMAL = ((LQTYOR)*LPRICE)#1
&		-(((LQTYOR*LPRICE)#1*LDISC)#2)
	EXEPRC = DECMAL - ((DECMAL*LODISC)#2)
	
	RUNTOT = RUNTOT + EXEPRC
	IF(LTXFLG) TAXTOT = TAXTOT + EXEPRC
	GOTO N_ORD
EOF_LIN,
	CLOSE 12
	RETURN
;------------------------------------

GETITM,
;;;	BSEND = ORGINV
;;;	KEY = ACCITM
;;;	XCALL SERCH (2,ITMIDX,KEY,1,15,ORGINV,BSMID,SRCCTL,4,16,20,0,0,0,0)
;;;	IF (SRCCTL.OR.(IRC041.LE.0))

	XCALL ISIO (1, INVMAS, ACCITM, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
	BEGIN
	  XCALL MESAG ('ITEM NOT FOUND',2)
	  SRCCTL = 1
	  INVMAS = 
	  ;;;ITMIDX = 
	  RETURN
	END
;;;	LOKCTL = 1
;;;	XCALL IO (1,INVMAS,IRC041,READ,LOKCTL)
	CALL TBL_PRICE		;SSQ 7-18-97
	RETURN
NOFIND,
;;;	close 5		;10-5-16
	unlock 5	;10-11-16

	IF (SELECT.EQ.2) XCALL MESAG ('No duct on file for this order',2)
;Added 4/11/02 ssq - need these totals if cc order...
	CALL RUNTOT
	XRUNTOT = RUNTOT
	XTAXTOT = TAXTOT
	RETURN
BADCON,
	XCALL MESAG ('The configuration for this item is bad',2)
	STOP
BADACC,
	XCALL MESAG ('Acc limit of 100 recs exceeded - CANNOT PROCESS ACCURATELY',1)
	RETURN
INPUT,
	XCALL INPUT( ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1 )
	RETURN

ANYCNG,
	XCALL ANYCN (CNGCTL,WHATNO)
	RETURN

GET_TABLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CHN182 = 17	;OPENED IN ORDADD

	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL.NE.0) XCALL MESAG ('PRICE NOT IN TABLE',1)
	RETURN
;------------------------------------------------------------	

TBL_PRICE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; READ COP PRICE TABLE INTO CORE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-----------------------------------------------
;;; 5-16-97 SSQ: QTY BREAK PRICING:

	CLEAR TBL_KEY, TBL_PRICE_FOUND
	TBLCOD = 'I1'
	I1_KEY = ACCITM
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0) 
		BEGIN
		TBL_PRICE_FOUND = 1
		PRICE = I1_PRICE(1)		;SAVE THE 1ST PRICE
		END
	RETURN
;-----------------------------------------------
;-----------------------------------------------
NEW_PRICE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; LOOK UP PRICE BY QTY
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (TBL_PRICE_FOUND)
	   BEGIN
	   FOR I FROM 1 THRU NUMITM
		BEGIN
		IF (ACCQTY.LT.I1_QTY(I))
		    BEGIN
		    ACCPRC = I1_PRICE(I)
		    ALPHA(1,9) = ACCPRC,'ZZZZ.XXX-'
		    XCALL OUTPT (ROW,58,0,ALPHA(1,9),1)
		    EXITLOOP
		    END
		END
	   END
	RETURN
;-----------------------------------------------

SHIFT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL OUTPT (24,1,0,'\',1)
	XCALL OUTPT (10,10,-1,'CNTRL',1)
	XCALL OUTPT (21,1,1,'\',1)
	XCALL OUTPT (22,1,1,'\',1)
	XCALL OUTPT (23,1,1,'\',1)
	XCALL OUTPT (24,1,1,'\',1)
	XCALL OUTPT (1,1,1,'ORDER ENTRY & EDITING',1)
	XCALL OUTPT (2,1,1,'Summarized Duct Order',1)
	XCALL OUTPT (3,1,1,'\',1)
	XCALL OUTPT (4,1,1,'Summarized Accessories',V)
	XCALL OUTPT (5,1,1,'\',1)
	XCALL OUTPT (6,1,1,'---------------- ITEM -------------------------  TOTAL      UNIT   EXTENDED',1)
	XCALL OUTPT (7,1,0,'ITEM NUMBER      DESCRIPTION                       QTY     PRICE      PRICE',1)

	ROW = 21
	RETURN
;-----------------------------------------------

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
		clear entry
		entry(1,1) = i,'X'
		entry(2,2) = '.'
		entry(4,15) = lp_liner
		DLINER(I) = entry(1,15)
		END

	RETURN
;--------------------------------------------------------------------

GET_NEG_ZERO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	A_FRT = FREGHT
	USING A_FRT SELECT
	('    -0'),	X_FRT = NEG_ZERO
	('     0'),	X_FRT = ZERO
	(),		X_FRT = 9
	ENDUSING

	RETURN
;--------------------------------------------
END
;;;;;;;;;;;;;;;;;
;XXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  XXXXX-  XXXX.XXX-  XXXXXX.XX-
;XXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  XXXXX-  XXXX.XXX-  XXXXXX.XX-
