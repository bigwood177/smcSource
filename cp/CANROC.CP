;CANROC.CP
;  CANCEL / COP 
;
;	5/22/02 ssq: added CANHDR, CANLIN, & CANDUC.ISM

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


RECORD	ORDCM2
	.INCLUDE 'DEF:RD135A.DEF'

RECORD SKEY
	XCUSNO	,D6
	SHPNO	,D4

RECORD SHIPTO
		.INCLUDE 'DEF:RD171A.DEF'
RECORD SHPCTL	,X
		.INCLUDE 'DEF:RD171B.DEF'

RECORD ORDHDR
		.INCLUDE 'DEF:RD044A.DEF'
RECORD ORDLIN
		.INCLUDE 'DEF:RD045A.def'
RECORD	,X
		.INCLUDE 'DEF:RD045D.def'
RECORD CANCTL
		.INCLUDE 'DEF:RD177B.DEF'
RECORD DUCACC
		.INCLUDE 'DEF:RD175A.DEF'

RECORD ITMMAS
		.INCLUDE 'DEF:RD041A.def'
record	itmkey
	.include 'def:rd041k.def'

;;;RECORD ,X
;;;		.INCLUDE 'DEF:RD041B.DEF'
;;;RECORD ITMIDX
;;;		.INCLUDE 'DEF:RD042A.DEF'

RECORD COPCTL
		.INCLUDE 'DEF:RD060A.DEF'
RECORD ARTERM
		.INCLUDE 'DEF:RD170A.DEF'
RECORD ARTCTL
		.INCLUDE 'DEF:RD170B.DEF'


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
	CHN135	,D3
	CHN144	,D2
	CHN145	,D2
	CHN146	,D2

RECORD	VARS
	GOT_CM2	,D1
	CNGCTL	,D1
	WHATNO	,D2
	TCHAR	,D1
	SAVV	,D1
	OPNOK	,D1
	L	,2D1
	OPTION	,D1
	ENTRY	,A35
	INXCTL  ,D1
	DECMAL	,D18
	MASK	,A6,'XXXXXX'
	RECNO	,D5
	ITMMAX	,D5
	KEY   	,A15
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	BLANKS	,A25
	ALPHA	,A8
	ORGINV	,D5
	SWITCH	,D1
	CTR	,D2
	LOCTNS	,D2
	SAVTYP	,D1
	BADBIL	,D1
	V	,D1
	READ	,D1,	0
	WRITE	,D1,	1
	STORE	,D1,	2
	DELETE	,D1,	3
	LOKCTL	,D1

PROC
	XCALL TERID (V)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF


RDCTL,
;;;	LOKCTL = 1
;;;	XCALL IO (1,ITMMAS,1,READ,LOKCTL)
;;;	ORGINV = ORG041
;;;	UNLOCK 1

	LOKCTL = 1
	XCALL IO (3,COPCTL,1,READ,LOKCTL)

DISPLA,
	UNLOCK 4
	UNLOCK 5
	UNLOCK 6
	UNLOCK 1
	LOKCTL = 1
	XCALL IO (10,ARTCTL,1,READ,LOKCTL)


	XCALL OE1 (DSTFLG,V)
	XCALL OUTPT (2,1,0,'CANCEL',1)
	XCALL OUTPT (2,3,0,'*',V)
	XCALL OUTPT (2,50,0,'3. CANCELED DATE',V)

ORDNUM,
	CTL = '02,16,06,01,#E'
	CALL INPUT
	GO TO (ORDNUM,END), INXCTL
	OORDNO = ENTRY
	IF (OORDNO.EQ.0) GO TO ORDNUM
SRCH,
	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)
	IF (LOKCTL) GO TO NOFIND

	CTL = '02,38'
	DECMAL(1,8) = OORDDT
	CALL DSPDTE
	CTL = '03,16,06'
	DECMAL = OCUSNO
	CALL DSPNUM
	XCALL OUTPT (3,24,0,OCUSNM,V)
	CTL = '04,16,02'
	DECMAL = OSLMAN
	CALL DSPNUM
	XCALL OUTPT (4,33,0,OLOC,V)

	XCALL OUTPT (8,53,0,OSCAC,1)


	XCALL OUTPT (4,70,0,OPONO,V)
;;;	CTL = '05,16,02'
;;;	DECMAL = ODISC
;;;	CALL DSPNUM
;;;	XCALL OUTPT (10,18,0,'%',1)
	SRCCTL = 2
	BSEND = ORG170
	XCALL SERCH (10,ARTERM,OTERMS,1,1,BSEND,BSMID,SRCCTL,1,2,7,0,0,0,0)
	XCALL OUTPT (10,33,0,OTERMS,1)
	GO TO (TRMNFD), SRCCTL
	XCALL OUTPT (11,33,0,ARTRDS,1)
	GO TO TERMOK
TRMNFD,
	XCALL OUTPT (11,33,0,'TERM NOT ON FILE',1)
TERMOK,
	XCALL OUTPT (10,53,0,OCLPPD,1)
	XCALL OUTPT (5,70,0,OJOBNO,V)
DSPADD,
	CLEAR ORDCM2
	C2_ORD = OORDNO
	C2_SEQ = 0

	LOKCTL = 0
	XCALL ISIO (CHN135, ORDCM2, C2_KEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0) 
	THEN	GOT_CM2 = 1
	ELSE	BEGIN
		GOT_CM2 = 0
		CLEAR ORDCM2
		END


	ALPHA (1,4) = OSHPTO,'ZZZZ'
	XCALL OUTPT (6,16,0,ALPHA(1,4),V)
	XCALL OUTPT (13,13,0,OSHPNM,1)
	XCALL OUTPT (14,13,0,OSHAD1,1)
	XCALL OUTPT (15,13,0,OSHAD2,1)
	XCALL OUTPT (16,13,0,OSHAD3,1)
	XCALL OUTPT (10,19,0,OCOMNT(1),V)
	XCALL OUTPT (11,19,0,OCOMNT(2),V)
	XCALL OUTPT (23,19,0,C2_COM,1)

;;;	IF (DSTFLG.NE.'Y') GO TO RIGHT1
	IF (DSTFLG.NE.'Y') GO TO ANYCNG
	ALPHA = OARACT, 'XXXX-XXX'
	XCALL OUTPT (11,73,0,ALPHA,V)
;-
	GOTO ANYCNG
;-----------------------------------------------------
OPROMD,
	CTL = '02,68,08,00,D '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	OPROMD = ENTRY
	GOTO ANYCNG

PONUM,
	CTL = '04,70,10,00,AT'
	CALL INPUT
	GO TO (DISPLA), INXCTL
	IF(INXCTL .EQ. 3)	;<TAB>
		BEGIN
		CALL OMETRO		;GET METRO PROJ #
		IF(CNGCTL.EQ.0)GOTO PONUM
		ENTRY = OPONO
		XCALL OUTPT(4,70,0,OPONO,V)
		END

	OPONO = ENTRY
	GO TO ANYCNG

JOBNUM,
	CTL = '05,70,10,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	OJOBNO = ENTRY
	GOTO ANYCNG

;-
SHIPTO,
	XCALL OUTPT (13,12,1,'\',1)
	XCALL OUTPT (14,12,1,'\',1)
	XCALL OUTPT (15,12,1,'\',1)
	XCALL OUTPT (16,12,1,'\',1)
	CTL = '06,16,04,00,N '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	OSHPTO = ENTRY
	IF (ENTRY.EQ.BLANKS) GO TO DFBLTO
	IF (OSHPTO.EQ.9999) GO TO SHIPNM


	XCUSNO = OCUSNO
	SHPNO = OSHPTO
	XCALL ISIO (8,SHIPTO,SKEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO BADSHP

	OSHPNM = SHTONA
	OSHAD1 = SHTOAD(1)
	OSHAD2 = SHTOAD(2)
	OSHAD3 = SHTOAD(3)
	OTAXFL = SHTOTC		;SSQ 4/24/02
	CALL DSPSHP
	GO TO ANYCNG

DFBLTO,
;;;	IF (NAME.NE.OCUSNM) CALL GETCUS
;;;	OSHPNM = NAME
;;;	OSHAD1 = ADD1
;;;	OSHAD2 = ADD2
;;;	TMPCTY = CITY
;;;	TMPSTA = STATE
;;;	TMPZIP = ZIP
;;;	OSHAD3 = TMPAD3
	CALL DSPSHP
	GO TO ANYCNG
SHIPNM,
	CTL = '13,12,30,00,A '
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	GO TO (DISPLA), INXCTL
	OSHPNM = ENTRY
	OSHFLG = 'C'		;USER CHANGED SHIP TO
	IF (OSHPTO.NE.9999) GO TO ANYCNG
SHIPA1,
	CTL = '14,12,30,00,A '
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	GO TO (DISPLA), INXCTL
	OSHAD1 = ENTRY
	OSHFLG = 'C'		;USER CHANGED SHIP TO
	IF (OSHPTO.NE.9999) GO TO ANYCNG
SHIPA2,
	CTL = '15,12,30,00,A '
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	GO TO (DISPLA), INXCTL
	OSHAD2 = ENTRY
	OSHFLG = 'C'		;USER CHANGED SHIP TO
	IF (OSHPTO.NE.9999) GO TO ANYCNG
SHIPA3,
	CTL = '16,12,30,00,A '
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	GO TO (DISPLA), INXCTL
	OSHAD3 = ENTRY
	OSHFLG = 'C'		;USER CHANGED SHIP TO
	GO TO ANYCNG
DSPSHP,
	XCALL OUTPT (13,12,0,OSHPNM,1)
	XCALL OUTPT (14,12,0,OSHAD1,1)
	XCALL OUTPT (15,12,0,OSHAD2,1)
	XCALL OUTPT (16,12,0,OSHAD3,1)
	RETURN
BADSHP,
	XCALL MESAG('SHIP-TO NUMBER NOT FOUND',1)
	GO TO SHIPTO
;-
	
	
COMENT,
	XCALL OUTPT (11,19,0,BLANKS,V)
	CTL = '10,19,35,00,AT'
	CALL INPUT
	GO TO (DISPLA,DEFCOM,DEFCOM), INXCTL
	OCOMNT(1) = ENTRY
	ROW = 11
	CALL INPUT
	GO TO (DISPLA), INXCTL
	OCOMNT(2) = ENTRY
	GO TO ANYCNG
defcom,
;;;	ocomnt(1) = ordcom(1)
;;;	ocomnt(2) = ordcom(2)

	XCALL OUTPT (20,19,0,OCOMNT(1),1)
	XCALL OUTPT (21,19,0,OCOMNT(2),1)

	GO TO ANYCNG
OMETRO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; GET METRO PROJ. NO.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL OUTPT(7,58,0,'METRO:',1)
	CTL = '07,66,12,00,A '

	IF (OMETRO .NE. BLANKS)GOTO OM_DISP

	SAVV = V
	V = 1
	CALL INPUT
	V = SAVV
	GOTO (OM_ABORT),INXCTL
	OMETRO = ENTRY(1,12)
	GOTO OM_ABORT

OM_DISP,
	XCALL OUTPT (ROW,COL,1,OMETRO,1)

	CALL ACCEPT		
	USING TCHAR SELECT
	(21),	BEGIN			;^U
		OMETRO =
		GOTO OMETRO
		END
	(13),	GOTO OM_ABORT		;<CR>
	ENDUSING
	GOTO OM_DISP

OM_ABORT,
	RETURN
;------------------------------------------
ACCEPT,
	XCALL FLAGS (00010000)			;DISABLE CHARACTER ECHOING
	XCALL OUTPT(ROW,COL+13,0,' ',1)
	ACCEPT (15,TCHAR)
	XCALL FLAGS (00000000)			;RE-ENABLE CHARACTER ECHOING
	RETURN

;-----------------------------------------------------
ANYCNG,
	XCALL ANYCN (CNGCTL, WHATNO)
	GOTO (UPDAT, CNGBR), CNGCTL + 1
CNGBR,
	GOTO (NOC,NOC,OPROMD,NOC,NOC,NOC,NOC,PONUM,NOC,NOC,
&	      NOC,JOBNUM,SHIPTO,SHIPNM,SHIPA1,SHIPA2,SHIPA3,
&	      COMENT,NOC,NOC,NOC), WHATNO
NOC,
	XCALL BEEP
	GOTO ANYCNG
UPDAT,
	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,WRITE,LOKCTL)
	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)
RIGHT1,
;-
	XCALL OUTPT (12,1,1,'OK TO CANCEL ?',V)
	CTL = '12,17,01,01,YN'
	CALL INPUT
	GO TO (PROCES), INXCTL
	UNLOCK 4
	GO TO DISPLA
NOFIND,
	XCALL MESAG('ORDER NOT FOUND',1)
	GO TO DISPLA
PROCES,
	XCALL WATE(3,V)

	CALL SAVE_ORDER			;SAVE CANCELLED ORDER
	IF (.NOT. OPNOK)
		BEGIN
		XCALL MESAG ('COULD NOT SAVE DATA, ORDER NOT CANCELLED!',1)
		GOTO DISPLA
		END

	LOKCTL = 1
	XCALL IO (7,CANCTL,1,READ,LOKCTL)
	INCR REC177

	LOKCTL = 1
	XCALL IO (7,CANCTL,1,WRITE,LOKCTL)
	LOKCTL = 1
	XCALL IO (7,ORDHDR,REC177,WRITE,LOKCTL)

	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,DELETE,LOKCTL)

	IF (GOT_CM2)
		BEGIN
		LOKCTL = 1
		XCALL ISIO (CHN135, ORDCM2, C2_KEY, DELETE, LOKCTL)
		END

	LOKCTL = 1
	LORDNO = OORDNO
	LINSEQ =
	XCALL OUTPT (24,1,1,'Cancelling line items',1)
	XCALL ISIO (5,ORDLIN,ORDKEY,READ,LOKCTL)
;;;	IF (LOKCTL.EQ.3) GO TO CANCLD
;;;	IF (LOKCTL.EQ.2 .AND. LORDNO.NE.OORDNO) GO TO CANCLD
	IF (LOKCTL.EQ.3) GO TO CANDUC				;;;
	IF (LOKCTL.EQ.2 .AND. LORDNO.NE.OORDNO) GO TO CANDUC	;;;
	GO TO ADJUST
NOCANDO,
	UNLOCK 7
	XCALL FILES (7,'U',177,4)
NOCAN2,
	XCALL MESAG ('CANNOT CANCEL ORDER',2)
	GOTO END
NXTLIN,
	LOKCTL = 1
	XCALL IOS (5,ORDLIN,READ,LOKCTL)
;;;	IF (LOKCTL.EQ.2) GO TO CANCLD
;;;	IF (LORDNO.NE.OORDNO) GO TO CANCLD
	IF (LOKCTL.EQ.2) GO TO CANDUC		;;;
	IF (LORDNO.NE.OORDNO) GO TO CANDUC	;;;
	GO TO ADJUST
CANDUC,
	XCALL OUTPT (24,1,1,'Cancelling duct and accessory items',1)
	DUCTOR = OORDNO
	XCALL ISIO (6,DUCACC,DUCTOR,READ,LOKCTL)
	IF (LOKCTL.EQ.3) GOTO CANCLD
	IF (LOKCTL.EQ.2.AND.DUCTOR.NE.OORDNO) GOTO CANCLD
	GOTO DELDUC
NXTDUC,
	LOKCTL = 1
	XCALL IOS (6,DUCACC,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GOTO CANCLD
	IF (DUCTOR.NE.OORDNO) GOTO CANCLD
DELDUC,
	LOKCTL = 1
	XCALL ISIO (6,DUCACC,KEY175,DELETE,LOKCTL)
	GOTO NXTDUC
CANCLD,
	XCALL MESAG('ORDER CANCELLED',2)
	GO TO DISPLA
ADJUST,
	LOKCTL = 1
	XCALL ISIO (5,ORDLIN,ORDKEY,DELETE,LOKCTL)
;;;	IF (LSTOKT.NE.'S') GO TO NONSTK
	IF (LSTOKT.NE.'S'.OR.LLOC.EQ.'E') GO TO NXTLIN		;;;
;;;
;	UNCOMMIT INVENTORY
;;;
	BSEND = ORGINV
	USING LITMNO SELECT
;;;	('X','CD'),		BEGIN
	('X'),		BEGIN
				  KEY = LITMNO(1,3)
				  KEY(4,4) = '0'
				END
	('CR0' thru 'CR9'),	KEY = LITMNO(1,4)
	('CM'),			KEY = LITMNO(1,3)
	('EDS0' THRU 'EDS9'),	KEY = LITMNO(1,9)
	(),			KEY = LITMNO
	ENDUSING

	CLEAR ITMKEY
	K_ITEM = KEY
	K_F1 = LF1,	'XXX'
	K_F2 = LF2,	'XXX'
	K_F3 = LF3,	'XXXXX'

	READ (1, ITMMAS, ITMKEY, KEYNUM:1) [ERR=I_LOCKED]
	GOTO I_OK
I_LOCKED,
	GOTO NXTLIN
I_OK,

;;;	XCALL SERCH (2,ITMIDX,KEY,1,15,BSEND,BSMID,SRCCTL,4,16,20,0,0,0,0)
;;;	GO TO (NXTLIN), SRCCTL
;;;	LOKCTL = 1
;;;	XCALL IO (1,ITMMAS,IRC041,READ,LOKCTL)

	QTYCOM = QTYCOM - LQTYOR		;;;
	IF (QTYCOM.LT.0) QTYCOM =		;;;

	LOKCTL = 1
	XCALL ISIO (1,ITMMAS,ITMKEY,WRITE,LOKCTL)

;;;	xcall comlg(litmno, itemno, -lqtyor, lordno, lloc, "scrn2")
	  
	GO TO NXTLIN

ENDOFF,
END,
	XCALL WATE(3,V)
	CALL CLOSE
	xcall flags(7000000)
	stop
;===================================================================

SAVE_ORDER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	OPEN (17,SU,'ROC:CANHDR.ROM')
	CHN144 = 17

	OPEN (18,SU,'ROC:CANLIN.ROM')
	CHN145 = 18

	OPEN (19,SU,'ROC:CANDUC.ROM')
	CHN146 = 19

	OPNOK = 1

	XCALL ISIO (CHN144, ORDHDR, OORDNO, STORE, LOKCTL)

	FIND (5, ORDLIN, OORDNO) [ERR=SO_LLOP]
SO_LLOP,
	XCALL IOS (5, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO SO_DUCT
	IF (LORDNO .NE. OORDNO) GOTO SO_DUCT
	XCALL ISIO (CHN145, ORDLIN, ORDKEY, STORE, LOKCTL)
	GOTO SO_LLOP
SO_DUCT,
	FIND (6, DUCACC, OORDNO) [ERR=SO_DLOP]
SO_DLOP,
	XCALL IOS (6, DUCACC, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO FINI_SO
	IF (DUCTOR .NE. OORDNO) GOTO FINI_SO
	XCALL ISIO (CHN146, DUCACC, DUCTOR, STORE, LOKCTL)
	GOTO SO_DLOP
FINI_SO,

	IF (CHN144) CLOSE CHN144
	IF (CHN145) CLOSE CHN145
	IF (CHN146) CLOSE CHN146

	RETURN
;------------------------------------------------------


INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,V)
	RETURN
DSPNUM,
	OPTION = 1
	GO TO CALDSP
DSPDTE,
	XCALL DATE8(DECMAL(1,8), D_OUT, D_OUTR, D_FMT, D_SW)
	XCALL OUTPT (ROW, COL, 0, D_FMT, V)
	RETURN
;;;	OPTION = 2
CALDSP,
	XCALL DSPLY(MAX,ROW,COL,DECMAL,OPTION,V)
	RETURN

OPENS,
	CLEAR OPNOK
	
	OPEN (1, SU,'ROC:ITMMAS.ROM')
	OPEN (3, I,'ROC:COPCTL.ROC')
	OPEN (4,SU,'ROC:ORDHDR.ROM')
	OPEN (5,SU,'ROC:ORDLIN.ROM')
	OPEN (6,SU,'ROC:DUCACC.ROM')
	OPEN (7, U,'ROC:CANLOG.ROC')
	OPEN (8,SI,'ROC:SHIPTO.ROM')
	OPEN (10,I,'ROC:ARTERM.ROC')

	OPEN (35, SU, 'ROC:ORDCM2.ROM')
	CHN135 = 35

	OPNOK = 1
	RETURN
;------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE 1
;;;	CLOSE 2
	CLOSE 3
	CLOSE 4
	CLOSE 5
	CLOSE 6
	CLOSE 7
	CLOSE 8
	CLOSE 10
	IF (CHN135) CLOSE CHN135

	RETURN
;------------------------------------------
END

