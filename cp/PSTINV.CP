;  PSTINV / COP 	isam
;
;
;
;		POSTS INVOICE AMOUNTS TO INVENTORY MASTER FILE
;
;	11-FEB-03 SSQ:	for each CD5.. tierod use 2 CD50 conduit locks
;	22-MAR-07 SSQ: 	check for default note codes.
;	15-Jun-20 ssq: flat oval

RECORD	OCT17
	.include 'def:rd045o.def'
;;;	ORD17	,D6	;don't relieve inventory for these orders

RECORD	FILEN		;wip orders...
	F_NAME	,A14

RECORD	COPTBL
		.INCLUDE 'DEF:RD182A.DEF'
RECORD ORDLIN		
		.INCLUDE 'DEF:RD045A.def'
RECORD,X
		.INCLUDE 'DEF:RD045D.def'
RECORD INVMAS		
		.INCLUDE 'DEF:RD041A.def'
record	itmkey
	.include 'def:rd041k.def'

RECORD DUCACC
		.INCLUDE 'DEF:RD175A.DEF'
RECORD	CHANNEL
	CHN182	,D2
	CHN082	,D2		;FOR items not found in inventory...

RECORD	VARS
	mat	,d1
	j35cnt	,d8		;qty of j35 frames galv
	j35Acnt	,d8		;qty of j35 frames alum
	j35Scnt	,d8		;qty of j35 frames sst
	j35qty	,d8
	itmrfa	,a6
	vanrfa	,a6
	ivflag	,d1
	vanflg	,d1
	A15	,A15
	LITEM	,A15		;ORDLIN ITEM # = LCFGIM
	DEC	,D18
	XF1	,D5
	XF2	,D5
	XF3	,D5
	CD5QTY	,D8		;1/2" CONDUIT LOCKS
	CD7QTY	,D8		;3/4" CONDUIT LOCKS
	KEY	,A15
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	ENDLIN	,D1
	EXEPRC	,D8
	EXECST	,D8
	DECMAL	,D18
	CTR	,D2
	LOCTNS	,D2
	INVORG 	,D5
	RECNO	,D5
	SAVTYP	,D1
	SWITCH	,D1
	ALPHA	,A9
	V	,D1
	ERRMSG	,A58
	ERRMS5	,A20	,'PRESS C TO CONTINUE.'
	ENTRY	,A2
	INXCTL	,D1
	COL2	,D2
	READ	,D1	,0
	WRITE	,D1	,1
	STORE	,D1	,2
	LOKCTL	,D1
	QTY	,D5	;;;
	PRC	,D8	;;;
	WGT	,D6	;;;
	EXTPRC	,D8	;;;
	UOFM	,A2	;;;
	XITMNO	,A15	;;;
	RESTRT	,A7	;;;
PROC
	XCALL TERID(V)
	XCALL OUTPT (1,1,1,'UPDATE INVENTORY FILE',V)
	XCALL WATE (4,V)
	SWITCH = 1
	XCALL SNMSG (RESTRT,SWITCH)
	IF (SWITCH.EQ.9)
	BEGIN
	  SWITCH = 3
	  XCALL SNMSG (' ',SWITCH)
	END
	CALL OPENS
	CLEAR CD5QTY, CD7QTY

	clear j35cnt, j35Acnt, j35Scnt
NXTITM,
	CALL GETLIN
	IF (ENDLIN) GO TO END
	CALL UPDINV
	GO TO NXTITM
END,
	SWITCH = 4
	CLOSE 1
	CLOSE 6
	CLOSE 7				;SSQ 10-18-05
	CLOSE CHN182
	if (chn082) close chn082	;ssq 3-22-07

	XCALL PGCHN ('CP:PSTSLH',1)
;
OPENS,
	XCALL FFILE (45, F_NAME, SWITCH)
	F_NAME(5,10) = 'PHYDEP'
	F_NAME(14,14) = 'M'
	open (7, si, f_name)
;;;	OPEN (7, SI, 'SMC:OCT17.ISM')		;orders in process on 10/17/05

	SWITCH = 5
	XCALL FILES (5,'SI',45,SWITCH)		;FILE # 45 -- ORDLIN FILE
	XCALL FILES (6,'SU',175,SWITCH)		;FILE # 175 -- DUCTOR FILE
	XCALL FILES (1,'SU',41,SWITCH)		;FILE # 41 -- INVMAS FILE
	XCALL FILES (8,'SI',182,SWITCH)		;FILE # 182 -- COPTBL
	CHN182 = 8
	xcall files (9,'SU',082,switch)		;file # 82 -- invlin.ism
	chn082 = 9

	RETURN
;
GETLIN,
	IF (RESTRT.EQ.'RESTART') GOTO EOFLIN
	LOKCTL = 1
	XCALL IOS (5,ORDLIN,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GO TO EOFLIN
	IF (LFLAG.NE.2) GO TO GETLIN
;;;	IF (LITMNO.EQ.'M') GOTO GETLIN
	IF (LITMNO.EQ.'M      ') GOTO GETLIN	;4-11-11, can't skip items starting w/ "m"

	IF (LTYPE .EQ. 'M') GOTO GETLIN		;MEMO
;;; 2-24-10 >>>	IF (LROCPO .EQ. 9) GOTO GETLIN		;SSQ 5-4-04
	RETURN
EOFLIN,
	ENDLIN = 1
;----------------------------------------
;;; 2-12-03 ssq: conduit locks, if any

	IF (CD5QTY.NE.0)
		BEGIN
		KEY = 'CD50'
		LQTYSH = CD5QTY
		CALL UPDCD
		END
	IF (CD7QTY.NE.0)
		BEGIN
		KEY = 'CD70'
		LQTYSH = CD7QTY
		CALL UPDCD
		END


;;;	RETURN
	GOTO DUCT	;;;
;
UPDINV,
	CALL FNDINV
	IF (SRCCTL) GOTO NOITEM
	CALL SETINV
	RETURN

NOITEM,
	lokctl = 1
	xcall isio (chn082, ordlin, ordkey, store, lokctl)

	ERRMSG =
	ERRMSG = 'ITEM                 IS NOT FOUND IN'
;                 1234567890123456789012345678901234567890
	ERRMSG(6,20) = LITMNO
	ERRMSG(50,55) = LORDNO, 'ZZZZZX'
;;;	CALL BADLC1
	RETURN

FNDINV,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; 9-23-09: use this routine...


;-----------------------------------------------------------------
	LITEM = LITMNO

;;; 11/11/09: no, don't use item input - use the actual item;;;
;;;	IF (LCFGIM .NE. A15) LITEM = LCFGIM	;use item input if not blank

	USING LITEM SELECT
	('CR0' thru 'CR9'),	KEY = LITEM(1,4)
	('CM'),			KEY = LITEM(1,3)
	('EDS0' THRU 'EDS9'),	KEY = LITEM(1,9)
	('JEB0' THRU 'JEB9'),	KEY = LITEM(2,15)	;SKIP THE "J"
	('JEF0' THRU 'JEF9'),	KEY = LITEM(2,15)	;SKIP THE "J"
	('JEFI0' THRU 'JEFI9'),	KEY = LITEM(2,15)	;SKIP THE "J"
	('JJG'),		KEY = LITEM(2,15)	;SKIP THE "J"
	('JTG'),		KEY = LITEM(2,15)	;SKIP THE "J"
	('FOSG'),		KEY = 'FOSG'		;FO SPIRAL
	('FOR'),		KEY = 'FOR'		;FO REDUCER
	('FOEC'),		KEY = 'FOEC'		;FO END CAP
	('FOSW'),		KEY = 'FOSW'		;FO SADDLE

	('FOEF'),		KEY = 'FOEF'		;FO EZ FLANGE
	('FOEFI'),		KEY = 'FOEFI'		;FO EZ FLANGE INSTALLED
	('FOEB'),		KEY = 'FOEB'		;FO BARREL CLAMPS
	('FOEF'),		KEY = 'FOEF'		;FO EZ FLANGE
	('FOEFS'),		KEY = 'FOEFS'		;FO EZ FLANGE SET

	('JFOEF'),		KEY = 'FOEF'		;FO EZ FLANGE
	('JFOEFI'),		KEY = 'FOEFI'		;FO EZ FLANGE INSTALLED
	('JFOEB'),		KEY = 'FOEB'		;FO BARREL CLAMPS
	('JFOEF'),		KEY = 'FOEF'		;FO EZ FLANGE
	('JFOEFS'),		KEY = 'FOEFS'		;FO EZ FLANGE SET

	('FOE'),		KEY = 'FOE'		;FO ELBOW OR ANGLE
	('FOH'),		KEY = 'FOH'		;FO HANGER
	(),			KEY = LITEM
	ENDUSING

	xcall gtitm (key, lf1, lf2, lf3, itmrfa, ivflag, vanflg, vanrfa)
	if (.not. ivflag) goto i_err	;no exact match	;6-8-15

	read (1, invmas, rfa:itmrfa) [err=i_err]
	
gi_done,
	using litmno select
	('J35A'),	J35ACNT = J35ACNT + LQTYSH
	('J35S'),	J35SCNT = J35SCNT + LQTYSH
	('J35'),	J35CNT = J35CNT + LQTYSH
	endusing

	clear srcctl
	return
I_err,
	srcctl = 1
	return
;---------------------------------------------------------------	

SETINV,
;	read record at "fndinv"
SKIP,
	USING LITEM SELECT
	('CD506' THRU 'CD548'),	CD5QTY = CD5QTY + LQTYSH*2	;conduit locks
	('CD701' THRU 'CD799'),	CD7QTY = CD7QTY + LQTYSH*2	;conduit locks
	ENDUSING

	QTYMTD = QTYMTD + LQTYSH	;UPDATE QTY SOLD MONTH TO DATE
	QTYYTD = QTYYTD + LQTYSH	;UPDATE QTY SOLD YEAR TO DATE

; ssq 9-13-12
; using "usemtd" fields to hold rolling 12 month avg use"
;;;	USEMTD = USEMTD + LQTYSH	;UPDATE USAGE MONTH TO DATE
;;;	USEYTD = USEYTD + LQTYSH	;UPDATE USAGE YEAR TO  DATE

	IF (LITEM.EQ.'???') AVGCST = LCOST
	DECMAL = ((LQTYSH*LPRICE) - ((LDISC*LQTYSH*LPRICE)#2))#1
	EXEPRC = DECMAL - ((DECMAL*LODISC)#2)
	EXECST = (LQTYSH*AVGCST)#1
	SLSMTD = SLSMTD + EXEPRC
	SLSYTD = SLSYTD + EXEPRC
	CSTMTD = CSTMTD + EXECST
	CSTYTD = CSTYTD + EXECST

;----------------------------------------------------------
; any orders in this file should not relieve inventory...
	FIND (7, OCT17, LORDNO) [ERR=O_OK]
	CLEAR LQTYSH
O_OK,
;----------------------------------------------------------

;----------------------------------------------------------
; 10-21-09: for ez flange installed (efi) update "regular" ez flange (ef)


	USING LITEM SELECT
	('JEFI'),	KEY = LITEM(2,3) + LITEM(5,15)
	('EFI'),	KEY = LITEM(1,2) + LITEM(4,15)
	(),		GOTO SKIP_EZ
	ENDUSING

; write back the mtd, ytd sales record ...
	LOKCTL = 1
	WRITE (1, INVMAS, ITMKEY)		;DEBUG
;;;	XCALL ISIO (1,INVMAS,ITMKEY,WRITE,LOKCTL)

; get the "uninstalled" record for on-hand and committed...
	XCALL GTITM (KEY, LF1, LF2, LF3, ITMRFA, IVFLAG, VANFLG, VANRFA)
	READ (1, INVMAS, RFA:ITMRFA) [ERR=EZ_ERR]

SKIP_EZ,
;----------------------------------------------------------
	if (lf2 .eq. 31)  goto skip_onh	;11-2-09: don't update qty's for 
					;custom notes per billn
;;;	IF (LSTOKT.EQ.'S') 
	IF (STOCK.EQ.'S') 
	BEGIN
	  QTYCOM = QTYCOM - LQTYOR
	  IF (QTYCOM.LE.0) QTYCOM = 
	END

	QTYONH = QTYONH - LQTYSH
skip_onh,
	LOKCTL = 1
	WRITE (1, INVMAS, ITMKEY)		;DEBUG
;;;	XCALL ISIO (1,INVMAS,ITMKEY,WRITE,LOKCTL)
;;;	xcall comlg(LITEM, itemno, -lqtyor, lordno, lloc, "pstinv")

ez_err,	  
	RETURN
;--------------------------------------------------------------------

UPDCD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	READ(1, INVMAS, KEY) [ERR=NO_CD]
	goto cd_ok
no_cd,
	srcctl = 1
	return
cd_ok,
	QTYMTD = QTYMTD + LQTYSH	;UPDATE QTY SOLD MONTH TO DATE
	QTYYTD = QTYYTD + LQTYSH	;UPDATE QTY SOLD YEAR TO DATE

;;;	USEMTD = USEMTD + LQTYSH	;UPDATE USAGE MONTH TO DATE
;;;	USEYTD = USEYTD + LQTYSH	;UPDATE USAGE YEAR TO  DATE

	DECMAL = (LQTYSH*PRICE)#1	;inventory price
	EXECST = (LQTYSH*AVGCST)#1
	SLSMTD = SLSMTD + EXEPRC
	SLSYTD = SLSYTD + EXEPRC
	CSTMTD = CSTMTD + EXECST
	CSTYTD = CSTYTD + EXECST
	QTYONH = QTYONH - LQTYSH
	QTYCOM = QTYCOM - LQTYOR
	IF (QTYCOM.LE.0) QTYCOM = 

	WRITE (1, INVMAS, ITEMNO)	;DEBUG

;;;	LOKCTL = 1
;;;	XCALL IO (1,INVMAS,IRC041,WRITE,LOKCTL)
	RETURN		
;----------------------------------------

;;;									   ;;;
;	UPDATES DUCT INFORMATION TO ITMMAS				     ;
;	IF NO RECORD FOUND FOR DUCT CONFIGURATION, THIS PROGRAM ADDS IT	     ;
;	TO ITMMAS							     ;
;;;									   ;;;

DUCT,
	if (j35cnt .gt. 0)
		begin
		itemno = 'DFRC'
		j35qty = j35cnt*4
		call j35_inv
		end

	if (j35Acnt .gt. 0)
		begin
		itemno = 'DFRCA'
		j35qty = j35Acnt*4
		call j35_inv
		end

	if (j35Scnt .gt. 0)
		begin
		itemno = 'DFRCS'
		j35qty = j35Scnt*4
		call j35_inv
		end
	
	GOTO DUCT2

j35_inv,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	xcall isio (1, invmas, itemno, read, lokctl)
	if (lokctl .eq. 0) 
		begin
		qtyonh = qtyonh - j35qty
		qtymtd = qtymtd + j35qty
		qtyytd = qtyytd + j35qty
		xcall isio (1, invmas, itemno, write, lokctl)
		end
	return
;----------------------------------------------------------------
DUCT2,
	DUCTOR = 
	clear ordlin		;isam: don't want lf1, lf2, lf3 set
NXTDUC,
	LOKCTL = 1
	XCALL IOS (6,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0) RETURN
	IF (FLG175.NE.2) GOTO NXTDUC
	IF (RST175.EQ.-1) GOTO NXTDUC	;rst175 is d1, not d2
;;;	IF (RST175.EQ.99) GOTO NXTDUC
	GOTO (LINER,A1,A2,A3,A4,A5,A6,A7,A8), RST175-1
	XITMNO =
	XITMNO(1,4) = GAUGE,'XXGA'
	IF (DUTYPE.EQ.2.OR.DUTYPE.EQ.3) XITMNO(1,4) = THICK,'.XXX'
	XITMNO(5,7) = SIZE1,'XXX'
	XITMNO(8,9) = SIZE2,'XX'
	XITMNO(10,11) = SIZE3,'XX'
	XITMNO(12,12) = DUTYPE
	XITMNO(13,13) = CAT
	XITMNO(14,14) = STY
	XITMNO(15,15) = SEAM
;;;	LITMNO = XITMNO
	LITMNO = 'DUCT'		;;;CHANGED 2/20/90 - DO NOT SAVE INDIV DUCT 
				;;;  CONFIG IN ITMMAS
	CALL FNDINV

	QTY = JOINT
	EXTPRC = ( POUNDS * GPRICE ) # 1
	RST175 = 1

	IF ( SRCCTL.NE.0 ) THEN
	BEGIN
	  PRC = GPRICE
	  WGT = POUNDS / JOINT
	  UOFM = 'LB'
	  CALL ADDITM
	END
	  		; Update inventory file here 
	ELSE CALL UPDITM


LINER,
	XITMNO =
	XITMNO(1,5) = 'LINER'
	XITMNO(6,6) = LINER,'X'
	LITMNO	= XITMNO
	CALL FNDINV

	QTY = SQFLIN
	EXTPRC = (SQFLIN * LINPRC) # 1
	RST175 = 2

	IF ( SRCCTL.NE.0 ) THEN
	BEGIN
	  PRC = LINPRC
	  WGT = 
	  UOFM = 'SF'
	  CALL ADDITM
	END
	  		; Update inventory file here 
	ELSE CALL UPDITM

A1,
	IF (SLIPS.NE.0)
	BEGIN
	  XITMNO = SLPITM
	  LITMNO = XITMNO
	  CALL FNDINV
	  QTY = SLIPS
	  EXTPRC = (SLIPS * SLPPRC) # 1
	  RST175 = 3
	  IF ( SRCCTL.NE.0 ) THEN
	  BEGIN
	    PRC = SLPPRC
	    WGT = 
	    UOFM = 'EA'
	    CALL ADDITM
	  END
	  ELSE CALL UPDITM
	END

A2,
	IF (DRIVES.NE.0)
	BEGIN
	  XITMNO = DRVITM
	  LITMNO = XITMNO
	  CALL FNDINV
	  QTY = DRIVES
	  EXTPRC = (DRIVES * DRVPRC) # 1
	  RST175 = 4
	  IF ( SRCCTL.NE.0 ) THEN
	  BEGIN
	    PRC = DRVPRC
	    WGT = 
	    UOFM = 'EA'
	    CALL ADDITM
	  END
	  ELSE CALL UPDITM
	END

A3,
	IF (TCORN.NE.0)
	BEGIN
	  XITMNO = CORITM
	  LITMNO = XITMNO
	  CALL FNDINV
	  QTY = TCORN
	  EXTPRC = (TCORN * CORPRC) # 1
	  RST175 = 5
	  IF ( SRCCTL.NE.0 ) THEN
	  BEGIN
	    PRC = CORPRC
	    WGT = 
	    UOFM = 'EA'
	    CALL ADDITM
	  END
	  ELSE CALL UPDITM
	END

A4,
	IF (TNBQTY.NE.0)
	BEGIN
	  XITMNO = TNBITM
	  LITMNO = XITMNO
	  CALL FNDINV
	  QTY = TNBQTY
	  EXTPRC = (TNBQTY * TNBPRC) # 1
	  RST175 = 6
	  IF ( SRCCTL.NE.0 ) THEN
	  BEGIN
	    PRC = TNBPRC
	    WGT = 
	    UOFM = 'EA'
	    CALL ADDITM
	  END
	  ELSE CALL UPDITM
	END

A5,
	IF (TGAQTY.NE.0)
	BEGIN
	  XITMNO = TGAITM
	  LITMNO = XITMNO
	  CALL FNDINV
	  QTY = TGAQTY
	  EXTPRC = (TGAQTY * TGAPRC) # 1
	  RST175 = 7
	  IF ( SRCCTL.NE.0 ) THEN
	  BEGIN
	    PRC = TGAPRC
	    WGT = 
	    UOFM = 'IN'
	    CALL ADDITM
	  END
	  ELSE CALL UPDITM
	END

A6,
	IF (TCLQTY.NE.0)
	BEGIN
	  XITMNO = TCLITM
	  LITMNO = XITMNO
	  CALL FNDINV
	  QTY = TCLQTY
	  EXTPRC = (TCLQTY * TCLPRC) # 1
	  RST175 = 8
	  IF ( SRCCTL.NE.0 ) THEN
	  BEGIN
	    PRC = TCLPRC
	    WGT = 
	    UOFM = 'EA'
	    CALL ADDITM
	  END
	  ELSE CALL UPDITM
	END

A7,
	IF (TBAQTY.NE.0)
	BEGIN
	  XITMNO = TBAITM
	  LITMNO = XITMNO
	  CALL FNDINV
	  QTY = TBAQTY
	  EXTPRC = (TBAQTY * TBAPRC) # 1
	  RST175 = 9
	  IF ( SRCCTL.NE.0 ) THEN
	  BEGIN
	    PRC = TBAPRC
	    WGT = 
	    UOFM = 'EA'
	    CALL ADDITM
	  END
	  ELSE CALL UPDITM
	END

A8,
	IF (TBNQTY.NE.0)
	BEGIN
	  XITMNO = TBNITM
	  LITMNO = XITMNO
	  CALL FNDINV
	  QTY = TBNQTY
	  EXTPRC = (TBNQTY * TBNPRC) # 1
;;;	  RST175 = 10
	  RST175 = 9			;11-29-07: not sure what else to do
	  IF ( SRCCTL.NE.0 ) THEN
	  BEGIN
	    PRC = TBNPRC
	    WGT = 
	    UOFM = 'EA'
	    CALL ADDITM
	  END
	  ELSE CALL UPDITM
	END

;;;	RST175 = 99
	RST175 = -1			;ssq 11-29-07
	LOKCTL = 1
	XCALL ISIO (6,DUCACC,KEY175,WRITE,LOKCTL)

	GOTO NXTDUC

ADDITM,
	ERRMSG( 1,15 ) = XITMNO
	ERRMSG( 16,50) = 'ITEM NOT IN INVENTORY, ADDING ...  ' ;;
	DISPLAY (15,13,10,ERRMSG)

			;SET UP ITEM RECORD
	INVMAS =
	ITEMNO = XITMNO
	DESCR = 'DUCT CONFIGURATION'
	PRDCAT = '11'		;PRODUCT CATEGORY 11 IS DUCT
	PRICE = PRC
	WEIGHT = WGT
	SUOFM = UOFM
	BOCODE = 0
	TXFLAG = 'Y'
	
	if1 = '000'
	if2 = '000'
	if3 = '00000'
	store (1, invmas, itemno)	;debug
	read (1, invmas, itemno)	;debug
	clear srcctl


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;			;GET RECORD NUMBER
;;;	LOKCTL = 1
;;;	XCALL IO (1,DUMINV,1,READ,LOKCTL)
;;;	INCR REC041
;;;			;CHECK FOR FULL
;;;	IF (REC041.GE.MAX041) GOTO ITMFUL
;;;
;;;			;SET UP INDEX RECORD
;;;	INVIDX =
;;;	IITMNO = ITEMNO
;;;	IRC041 = REC041
;;;	IPRCAT = PRDCAT
;;;
;;;			;WRITE INDEX RECORD
;;;	LOKCTL = 1
;;;	XCALL IO (2,INVIDX,REC041,WRITE,LOKCTL)
;;;
;;;			;WRITE HEADER BACK
;;;	LOKCTL = 1
;;;	XCALL IO (1,DUMINV,1,WRITE,LOKCTL)
;;;	
;;;	UNLOCK 1
;;;	UNLOCK 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
UPDITM,
	IF (SRCCTL.EQ.0)
	BEGIN
	  LOKCTL = 1
;;;	  XCALL IO ( 1,INVMAS,IRC041,READ,LOKCTL )
	END

			;UPDATE QTY & DOLLARS SOLD
	QTYMTD = QTYMTD + QTY
	QTYYTD = QTYYTD + QTY
	SLSMTD = SLSMTD + EXTPRC
	SLSYTD = SLSYTD + EXTPRC

			;WRITE DATA RECORD BACK
	write (1, invmas, itemno)	;debug

;;;	LOKCTL = 1
;;;	XCALL IO ( 1,INVMAS,IRC041,WRITE,LOKCTL )
	RETURN

;;;
BADLC1,
	XCALL OUTPT (2,1,2,'\',1)
	XCALL OUTPT (7,7,-1,'CNTRL',V)
	XCALL OUTPT (7,0,-1,'CNTRL',V)
	XCALL OUTPT (4,10,0,ERRMSG,V)
	XCALL OUTPT (8,10,0,ERRMS5,V)
INPTYN,
;;;	XCALL INPUT (8,48,1,1,'A ',ENTRY,INXCTL,V)
;;;	GOTO (INPTYN,INPTYN), INXCTL
;;;	IF (ENTRY(1,1).NE.'C') GOTO INPTYN
;;;	XCALL WATE (4,V)
	IF (SRCCTL) RETURN
	GOTO SKIP
END
