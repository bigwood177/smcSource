SUBROUTINE S_CHANGE		;isam
	S_OORDNO	,D
	INET		,A	;"I" = INTERNET ORDER
	CCORD		,A	;"C" = Changed to CC order

; 6-04-18 ssq: make cusmas isam
; 3-28-19 ssq: express order mod
; 12-4-19 ssq: miscus mod

	.include 'wnd:windows.def'
	.include 'wnd:tools.def'

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR

EXTERNAL FUNCTION		;12-28-20
	IS_PRF	,A

EXTERNAL FUNCTION		;3-28-19
	IS_EXP	,A

EXTERNAL FUNCTION		;7-20-20
	IS_STOCK	,A


EXTERNAL FUNCTION
	GU_NAME	,A

GLOBAL ORDH
RECORD ORDHDR   	
		.INCLUDE 'DEF:RD044A.DEF'
ENDGLOBAL

GLOBAL ORDL
RECORD ORDLIN		
		.INCLUDE 'DEF:RD045A.def'
RECORD	,X
		.INCLUDE 'DEF:RD045D.def'
RECORD	,X
		.INCLUDE 'DEF:RD045M.def'
ENDGLOBAL

GLOBAL DUCK
	.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL

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

;;;GLOBAL PAR	
;;;RECORD PARAM
;;;	.INCLUDE 'DEF:PARAM.DEF'
;;;ENDGLOBAL
;----- END GLOBALS -----


	.INCLUDE 'DEF:HPSUB.DEF'

RECORD	CRHOLD
	.INCLUDE 'DEF:RD195A.DEF'

RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'


record	coptbl
	.include 'def:rd182a.def'

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT

RECORD	FUNKEY
	.INCLUDE 'DEF:FUNKEY.DEF'

RECORD	FAXFIL
	.INCLUDE 'DEF:FAXFIL.DEF'

RECORD	WRKKEY
	.INCLUDE 'DEF:RD183A.DEF'


RECORD
	.INCLUDE 'DEF:RD001W.DEF'

RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN041	,D2
	CHN042	,D2
	CHN044	,D2
	CHN045	,D2	;SSQ 9-4-03
	CHN060	,D2
	CHN084	,D2	;SSQ 9-16-13
	CHN135	,D3	;SSQ 5-20-15
	CHN137	,D2
	CHN166	,D2
	CHN182	,D2
	CHN183	,D2
	CHN101	,D3	;MISCUS 11-18-19
	CHN102	,D3	;MISSHP 11-18-19

RECORD	D_ORD
		,A7,	'ORDER: '
	DORD	,A6

RECORD	SPLFIL
		,A4,	'SPL:'
	SPF_ORD	,A6
		,A4,	'.SPL'

RECORD	PBUF
		,A10
	PLINE	,A50

RECORD	VARS
	PRFAB_ORD	,A1	;12-28-20 Pre fab order (Y/N)
	XPRS_ORD,A1	;3-28-10 Express order (Y/N)
	STK_ORD	,A1	;7-20-20 Stock order (Y/N)
	a_lf1	,a3
	a_lf2	,a3
	a_lf3	,a5
	onedpt	,a2	;for printing a single dept
	taxcc	,d1	;1=tax code changed in oecc2 (4-29-13)
	wnd_1	,d4
	A15	,A15
	local	,d1
	GTIRFA	,A6
	GTIFLG	,D1
	vanrfa	,a6
	vanflg	,d1
	distr	,d1	;6-12-07  1=cust is distr
	HAS_I	,D1	;1 = l/i w/ dept "I" is on order
	batfil	,a10
	numlbl	,d2,	04
;;;	CCORD	,A1
	CMPCOD	,A3
	ROCPO	,D1
	F_KEY	,D3
	TODAY	,D6
	XDATE	,D6
	CUSTCD	,A2
	U_NAME	,A25
	OPNOK	,D1
	OORSEQ	,D3
	DUCSEQ	,D3
	CREDIT	,D8
	CONTR	,D1	;1=CONTRACTOR
	ODATE	,D8
	MAXRC2	,D5
	RUNTOT	,D8
	TAXTOT	,D8

	X_FRT	,D1
	NEG_ZERO	,D1,1
	ZERO		,D1,0
	A_FRT	,A7
	E_FRGHT	,D7
	C_FRGHT	,D7	;CC CALC FREIGHT	3-4-20
	EN_FRT	,D7	;USER ENTERED FREIGHT	3-4-20
	SAVFRT	,D7
	SAVLOC	,A2
	O_QTY	,D6
	SCUSTP	,A2
	FULL	,D1
	RE_PRICE,D1	;1 = ORDER RE-PRICED
	ORGINV	,D5
	MAXINV	,D5
	INXCTL  ,D1
	ENTRY	,A30
	ANS	,A1
	CNGCTL	,D1
	WHATNO	,D2
	ENTFLG	,D1
	PRICES	,D2
	LOCTNS	,D2
	SAVTYP	,D1
	PMAX	,D2
	SWITCH	,D1
	D	,D1
	V	,D1
;;;	TAXFLG	,D1
	TAXFLG	,A3		;SSQ 9-7-01
	READ	,D1,	0
	WRITE	,D1,	1
	STORE	,D1,	2
	DELETE	,D1,	3
	LOKCTL	,D1
	CTR	,D2
	FILNAM	,A14	;;;
	FIL045	,A14	;;;
	FIL174	,A14
	SELECT	,D1	;;;
	SAVSEQ	,D3
	OEFLAG	,D1	;0=NO CHANGE, 1=ORD TO EST, 2=EST TO ORD
	KEY	,A15
	ITEM	,A15
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	DECMAL	,D10

PROC
;;;	XCALL TERID(V)
	XCALL WHO(CMPCOD)

	U_NAME = %GU_NAME		;GET USER LOGON
	V = 2

;;;	xcall u_start("lib:smc_wnd",,,,,,,100)
	xcall u_start("lib:smc_wnd",,,,,,,200)
	xcall e_sect("Change",D_CAPTION,D_CLREOL, D_POSITION,1,5)

	CALL OPENS
	XCALL FFILE (174,FIL174,SWITCH)	;;;

DISPLA,
	CLEAR INXCTL
;;;	XCALL OUTPT (1,67,1,'\',V)
	XCALL OUTPT (1,1,2,'ORDER ENTRY & EDITING',1)
	IF (INET .NE. 'I') GOTO NOT_INET
	IF (ORDTYP .EQ. INET) GOTO NOT_INET	;ALREADY MARKED AS "I"
	XCALL OUTPT (4,4,0,'Change to Internet Order?',1)
	XCALL INPUT (4,31,01,01,'YN',ENTRY,INXCTL,1)
;;;	GOTO (NOT_INET), INXCTL-1
;;;	ORDTYP = INET
	USING INXCTL SELECT
	(1),	ORDTYP = INET		;change to internet order
	(2),	INET =			;not internet
	ENDUSING
	
NOT_INET,
	IF (CCORD .EQ. 'C') OCCRD = 'C'
	CCORD = OCCRD		;SAVE VALUE SSQ 12-8-05
	OORDNO = S_OORDNO
	XCALL ORDCN (ORDHDR,SCUSTP,INXCTL,CNGCTL,INET,V,TAXFLG,
& OEFLAG,CUSTCD,distr,wnd_1)

	taxcc = otaxcc		;10-3-13 this may have changed in ordcn...

;;;	XCALL ORDCN (ORDHDR,SCUSTP,INXCTL,CNGCTL,INET,V,TAXFLG,OEFLAG,CUSTCD,distr)
;;;	XCALL ORDCN (ORDHDR,SCUSTP,INXCTL,CNGCTL,D,V,TAXFLG,OEFLAG,CUSTCD)
	IF (INXCTL.EQ.2) GO TO ENDOFF
DISPL2,
	XCALL FFILE (45,FIL045,SWITCH)	;;;
	FIL045 (14,14) = 'M'
	OPEN (5,SU,FIL045)		;;;
	CHN045 = 5			;SSQ 9-4-03
	IF (OEFLAG .GT. 0) CALL CNGOE1	;ORD/EST FLAG CHANGED TO O 

DISP2A,
	SAVSEQ = ORDSEQ
;;;	CONTR = 1		;9-16-04, ALWAYS 1 IN CHANGE
	CONTR = OCONTR		;3-21-11, NOW THIS IS SAVED IN ORDADD...

	XCALL SCRNX (SCUSTP,PCODES,PDISCS,PMAX,DCODES,DDISCS,DMAX,TYPSYS,
&		ORDSEQ,OORDNO,TAXFLG,MAXRC2,ORGINV,LOCTNS,PRICES,
&		RE_PRICE,ODISC,OLOC,OCUSNM,TAXTOT,OPRTF,CONTR,ODATE,CUSTCD,distr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 9-27-10: call befor exiting scrnx...
;;;	XCALL OEJR(OORDNO,CHN041,CHN042,CHN045,runtot, taxtot,oloc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLOSE CHN045				;SSQ 9-4-03

	IF (SAVSEQ.EQ.ORDSEQ) GOTO DISPL3		;;;
	XCALL OUTPT (24,1,1,'UPDATING ORDER SEQUENCE',1);;;
	SAVSEQ = ORDSEQ					;;;
	LOKCTL = 1					;;; 6/8/93
	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)	;;; 6/8/93
	ORDSEQ = SAVSEQ					;;;
	LOKCTL = 1					;;;
	XCALL ISIO (4,ORDHDR,OORDNO,WRITE,LOKCTL)	;;;


DISPL3,
	XCALL OUTPT (3,1,2,'\',1)	;;;
	XCALL FFILE (175,FILNAM,SWITCH)	;;;
	FILNAM (14,14) = 'M'		;;;
	OPEN (5,SU,FILNAM)		;;;
	SELECT =
	DUCSEQ=ODSEQ

;;;	XCALL SCRN3 (OORDNO,TAXFLG,ORGINV,RE_PRICE,OCUSNM,ODATE,OLOC,FIL174,DUCSEQ,
;;;&			CUSTCD)

	XCALL SCRN3 (OORDNO,TAXFLG,ORGINV,RE_PRICE,OCUSNM,ODATE,OLOC,FIL174,DUCSEQ,
&			CUSTCD, oprtf)
	XCALL u_finish

	E_FRGHT = OFRGHT
;;;	XCALL SCRN5 (OORDNO,FULL,SELECT,OLOC,FIL045,ORGINV,TAXFLG,E_FRGHT,
;;;&			CUSTCD)

;-
	XCALL SCRN5 (OORDNO,FULL,SELECT,OLOC,FIL045,ORGINV,TAXFLG,E_FRGHT,
&			CUSTCD,RUNTOT,TAXTOT,CONTR)	;SSQ 2-29-16 added contr
	
	XPRS_ORD = %IS_EXP (OORDNO, CHN182)	;is this an "Express" order? 3-28-19
	STK_ORD = %IS_STOCK (OORDNO, CHN182)	;is this a "Stock" order? 7-20-20
	PRFAB_ORD = %IS_PRF (OORDNO, CHN182)	;is this a "Pre fab" order? 12-28-20

	EN_FRT = -9			;3-4-20
	CALL RE_WRITE

	CLOSE 5

ASK_PRINT,
	CLEAR LOCAL
	XCALL OUTPT (2,1,2,' ',1)
	DORD = OORDNO, 'ZZZZZX' [LEFT]
	XCALL OUTPT(1,1,1,D_ORD,1)
	IF(CUSTCD .EQ. 'NQ')	XCALL OUTPT (1,64,0,"Don't Fax/Email",1)

	USING OLOC SELECT
	('O'),	XCALL OUTPT (24,1,1,'Reprint Work Order (Y/N/F/R/L/B) ? ',1)
	('E'),	XCALL OUTPT (24,1,1,'Reprint Quote (Y/N/F/R) ? ',1)
	ENDUSING

	xcall outpt (25,1,1,'Yes/No/Freight/Reprice/Local/Blues Only  A=Abort Printing',1)

;;;	XCALL INPUT (24,30,01,00,'A ',ENTRY,INXCTL,1)
;;;	XCALL INPTK (24,34,01,00,'A ',ENTRY,INXCTL,1,F_KEY)
	XCALL INPTK (24,34,03,00,'A ',ENTRY,INXCTL,1,F_KEY)

	clear onedpt			;7-29-13

	USING F_KEY SELECT
	(F_05),	BEGIN
		CLEAR CUSTCD
		GOTO ASK_PRINT
		END
	(F_09),	BEGIN
		CUSTCD = 'NQ'
		CALL RE_WRITE
		GOTO ASK_PRINT
		END
	ENDUSING

	xcall outpt (25,1,1,' ',1)

	ANS = ENTRY(1,1)
	USING ANS SELECT
	('.'),	BEGIN			;PRINT W/O FOR SINGLE DEPT
		ONEDPT = ENTRY(2,3)
		LOCAL = 1		;PRINT LOCALLY
		XCALL PRWOD (OORDNO,local,ONEDPT)
		GOTO NOT_WO
		END
	('A'),	GOTO NOT_WO		;SKIP ALL PRINT QUESTIONS
	('B'),	INXCTL = 1		;BLUES ONLY
	('C'),	BEGIN
		XCALL PDES2(OORDNO)	;PRINT ON LOCAL PRINTER
		GOTO NOT_WO
		END
	('F'),	BEGIN
		SAVFRT = OFRGHT
		E_FRGHT = OFRGHT
		CALL FREIGHT
		IF (E_FRGHT .NE. SAVFRT) CALL RE_WRITE
		GOTO ASK_PRINT
		END
	('N'),	NOP
	('R'),	BEGIN
		SAVFRT = E_FRGHT
		CCORD = 'R'		;12-6-05 SSQ CASH RECEIPT
		XCALL OECC2(ORDHDR, RUNTOT, TAXTOT, E_FRGHT, OCCRD, TAXCC)
		TAXFLG = OTAXFL		;SAVE FOR RE-WRITE SSQ 5/6/02
		IF (E_FRGHT .NE. SAVFRT) EN_FRT = E_FRGHT	;MUST HAVE CHANGED
		CALL RE_WRITE
		GOTO ASK_PRINT
		END
	('L'),	BEGIN
		INXCTL = 1
		LOCAL = 1
		END
	('Y'),	INXCTL = 1
	(' '),	BEGIN
		INXCTL = 2
		XCALL OUTPT (24,34,1,'N',1)
		END
	(),	GOTO ASK_PRINT
	ENDUSING


; shipping lables...
	USING OSCAC SELECT
	('0','4'),	NUMLBL = 0	;SPEE-DEE, UPS
	(),		NUMLBL = 4	;
	ENDUSING

;-------------------------------------------------------------
; 4-4-17 ssq: for new rockford printer:
	USING CMPCOD SELECT
	('SMC','TST'),	BATFIL = 'UPSFIL.BAT'	;ssq 4-4-17
	('ROC'),	BATFIL = 'BROC.BAT'		
	(),		NUMLBL = 0	;NO LABLES
	ENDUSING
	

;-------------------------------------------------------------

	IF (INXCTL.EQ.1) 
	THEN	BEGIN
		USING OLOC SELECT
		('O'),	BEGIN
			XCALL FIND_I (OORDNO, HAS_I)
			XCALL PDORD (OORDNO)
			XCALL PRDUC(OORDNO, OCUSNM)	;here 8-31-09
			XCALL MKDEC(OORDNO)		;coil line labels
			if (ans .ne. 'B')
			  begin
				if (has_i .AND. oprtf.le.0)
				  begin
				  XCALL LBLSH (OORDNO, CHN182, NUMLBL, BATFIL)	;ssq 7/11/06
				  end
			  XCALL PRWOD (OORDNO,local)
			  XCALL PRWO3 (OORDNO)
			  XCALL PRWO2 (OORDNO,local)
			  XCALL PRWOS (OORDNO, LOCAL)
			  end

			IF (OSHFLG.EQ.'C') CALL PRSHP

			XCALL FILES (40,'SU',195, SWITCH)
			XCALL ISIO (40, CRHOLD, OORDNO, READ, LOKCTL)
			IF (LOKCTL.EQ.0)
				BEGIN
				CH_ANS = ANS
				XCALL ISIO (40, CRHOLD, OORDNO, WRITE, LOKCTL)
				END
			CLOSE 40

			END
		('E'),	BEGIN
			XCALL PDEST(OORDNO)	
			XCALL PRDUC(OORDNO, OCUSNM)	;moved 8-31-09
			END
		ENDUSING
	;;;	XCALL PRDUC(OORDNO, OCUSNM)	;moved 8-31-09
		GOTO NOT_WO
		END
	ELSE	LOCAL = 1		;PRINT WORKORDERS ON LOCAL PRINTER...

	IF (OLOC .EQ. 'E') GOTO NOT_WO

	XCALL OUTPT (24,1,1,'Print Dept Work Orders ? ',1)
	XCALL INPUT (24,28,01,00,'YN',ENTRY,INXCTL,1)
	IF (ENTRY(1,1) .EQ. 'Y')
		BEGIN
		XCALL PRWOD (OORDNO,local)
		XCALL PRWO3 (OORDNO)
		XCALL PRWO2 (OORDNO,local)
		XCALL PRWOS (OORDNO, LOCAL)
		;;;XCALL PRcvr (OORDNO, LOCAL)
		GOTO ASK_LBL
	;;;	GOTO NOT_WO
		END
		
	XCALL OUTPT (24,1,1,'Print Material for all Depts ?',1)
;;;	XCALL INPUT (24,36,01,00,'YN',ENTRY,INXCTL,1)
	XCALL INPUT (24,36,01,00,'A ',ENTRY,INXCTL,1)
	if (entry(1,1) .eq. ' ') entry(1,1) = 'N'
	ANS = ENTRY(1,1)
	IF (ANS .EQ. 'Y')
		BEGIN
		XCALL PRWO2 (OORDNO,local)
		XCALL PRWOS (OORDNO, LOCAL)
		END
	IF (ANS .EQ. 'S') XCALL PRWOS(OORDNO, LOCAL)
ASK_LBL,
;;;	using cmpcod select
;;;	('SMC','TST'), XCALL PDLBL (OORDNO)	;DUCT LABLES 5-07-09
;;;	endusing

	XCALL OUTPT (24,1,1,'Reprint Shipping Labels (N) ? ',1)
	XCALL INPUT (24,39,01,00,'YN',ENTRY,INXCTL,1)
	IF (INXCTL .EQ. 2) GOTO NO_LBL

	XCALL LBLSH (OORDNO, CHN182, NUMLBL, BATFIL)	;ssq 7/11/06

NOT_WO,
NO_LBL,
	USING OCCRD SELECT
	('C'),		BEGIN
			XCALL OECC2(ORDHDR, RUNTOT, TAXTOT, E_FRGHT, OCCRD, TAXCC)
			TAXFLG = OTAXFL		;SAVE FOR RE-WRITE SSQ 5/6/02
			CALL RE_WRITE
			END
	ENDUSING

	GOTO ENDOFF
;========================================================

FREIGHT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR ENTRY, CNGCTL
	XCALL OUTPT (22,10,2,'FREIGHT',1)
	CALL GET_NEG_ZERO
	CALL D_FRT
	GOTO ANYF
FRGHT2,
	XCALL INPUT (22,18,07,00,'$ ',ENTRY,INXCTL,1)
	IF (INXCTL) RETURN
	E_FRGHT = ENTRY(1,7)
	EN_FRT = E_FRGHT
	IF (E_FRGHT.EQ.0) SAVFRT = 1	;FORCE RE_WRITE
	CALL GET_NEG_ZERO
	CALL D_FRT
ANYF,
	CNGCTL = 2
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (FRGHT2),CNGCTL
	RETURN

D_FRT,
	USING X_FRT SELECT
	(NEG_ZERO),	ENTRY(1,3) = 'N/C'
	(9),		ENTRY(1,9) = E_FRGHT,	'ZZ,ZZX.XX' [LEFT]
	ENDUSING
	XCALL OUTPT (22,18,1,ENTRY(1,9),1)
	RETURN
;----------------------------------------------------

RE_WRITE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	LOKCTL = 1					;SSQ 7-23-99
	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)	;SSQ 7-23-99
	
	OXPRS = XPRS_ORD		;SSQ 3-28-19 
	OXSTK = STK_ORD			;SSQ 7-20-20
	OXPRF = PRFAB_ORD		;SSQ 12-28-20

;-
	using oscac select	;added to s_change 2-24-20
	('1','10','WC'),	nop	
	(),			IF (OCUSNO.EQ.10)		;11-27-19 CASH CUST BUT NOT WILL-CALL
				BEGIN
				IF (EN_FRT .GE. 0)
				THEN	E_FRGHT = EN_FRT	;3-4-20 USER FREIGHT WINS
				ELSE	BEGIN
					C_FRGHT = (RUNTOT*20)#2	;FRT IS 20%
					IF (C_FRGHT .LT. 1000) C_FRGHT = 1000	;MIN $10.00
					IF (E_FRGHT .LT. C_FRGHT) E_FRGHT = C_FRGHT	;3-2-20
					END
				END
	endusing

;-

	OFRGHT = E_FRGHT
	ODSEQ = DUCSEQ		;DUCT SEQ

;ordcn will change otaxcc to 0 if new cust or ship-to was entered.
	if (otaxcc.ne.1) OTAXFL = TAXFLG		;SSQ 4-29-13
;;;	OTAXFL = TAXFLG		;SSQ 5/6/02

	IF (TAXCC.EQ.1) OTAXCC = TAXCC		;SSQ 4-29-13
	OCCRD = CCORD		;SSQ 12/6/05
	IF (CUSTCD .EQ. 'NQ') OCUSCD = 'NF'		;SSQ 12-19-01
	LOKCTL = 1					;SSQ 7-23-99
	XCALL ISIO (4,ORDHDR,OORDNO,WRITE,LOKCTL)	;SSQ 7-23-99

	IF (CUSTCD .EQ. 'NQ')
		BEGIN
		;;; write FAXFIL record...
		OPEN (40,SU,'SPL:FAXFIL')
		CLEAR FAXFIL
		FAX_ORDER = OORDNO
		XCALL RDATE(TODAY)
		XDATE(1,2) = TODAY(5,6)
		XDATE(3,6) = TODAY(1,4)
		FAX_DATE = XDATE		;01-04-16
	;;;	FAX_DATE = TODAY
		FAX_SLSMAN = OSLMAN
		FAX_STATUS = 0		;OPEN
		FAX_OTYPE = 'O'		
		STORE (40,FAXFIL,FAX_ORDER) [ERR=BAD_FAX]
	BAD_FAX,
		CLOSE 40
		END

	RETURN
;---------------------------------------------------------

ENDOFF,
	CALL CLOSE
	IF (FULL) XCALL MESAG
&	('THE "ORDLIN" FILE IS NOW FULL - REORGANIZE BEFORE CONTINUING',2)

	XCALL u_finish
	RETURN
;;;	XCALL PGCHN ('CP:SSQMNU',1)
;-------- END OF SUBROUTINE -------------


CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN044) CLOSE 4
;;;	IF (CHN042) XCALL FILES (CHN042, 'I', 42, 4)
;;;	IF (CHN041) XCALL FILES (CHN041, 'U', 41, 4)

	IF (CHN182) CLOSE CHN182
	IF (CHN183) CLOSE CHN183
	IF (CHN060) CLOSE CHN060
	IF (CHN084) CLOSE CHN084	;9-16-13
	IF (CHN137) CLOSE CHN137
	IF (CHN135) CLOSE CHN135	;5-20-15

	IF (CHN002) XCALL FILES (CHN002, 'I', 02, 4)
	IF (CHN001) XCALL FILES (CHN001, 'SI', 01, 4)

	IF (CHN166) CLOSE CHN166
	IF (CHN101) CLOSE CHN101
	IF (CHN102) CLOSE CHN102

	RETURN
;-----------------------------------------------


;
;
OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (4,'SU',44,SWITCH)		;FILE # 44 -- ORDHDR FILE
	IF (SWITCH.EQ.9) RETURN
	CHN044 = 4

	CHN041 = 1

	SWITCH = 5				;SSQ 5-16-97
	XCALL FILES (17,'SI',182,SWITCH)	;FILE #182 -- COPTBL
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 17

	XCALL FILES (18,'SI',183,SWITCH)	;FILE #183 -- ROLO.ISM
	IF (SWITCH .EQ. 9) RETURN
	CHN183 = 18


	SWITCH = 1
;;;	XCALL FILES (7,'I',02,SWITCH)		;FILE # 02 -- CUSIDX FILE
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN002 = 7

	XCALL FILES (6,'SI',01,SWITCH)		;FILE # 01 -- CUSMAS FILE
	IF (SWITCH.EQ.9) RETURN
	CHN001 = 6


	SWITCH = 5
	XCALL FILES (3,'I',60,SWITCH)
	IF (SWITCH.EQ.9) RETURN
	CHN060 = 3

	SWITCH = 5
	XCALL FILES (29,'SU',137,SWITCH)	;FILE # 137 CCINFO.ISM
	IF (SWITCH .EQ. 9) RETURN
	CHN137 = 29


	SWITCH = 5
	XCALL FILES (16,'SI',166,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN166 = 16

	SWITCH = 5
	XCALL FILEC ('SMC',84,'SI',084, SWITCH)	;EMAILC.SMM
	CHN084 = 84

	SWITCH = 5
	XCALL FILES (135, 'SU', 135, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN135 = 135

	SWITCH = 5
	XCALL FILES (101, 'SU', 101, SWITCH)	;FILE #101 MISCUS
	CHN101 = 101

	SWITCH = 5
	XCALL FILES (102, 'SU', 102, SWITCH)	;FILE #102 MISSHP
	CHN102 = 102

	OPNOK = 1
	RETURN
;--------------------------------------------------


CNGOE1,
	xcall w_disp (wnd_1, wd_pos, 23, 1, wd_clr, wdc_lin)
	xcall w_disp (wnd_1, wd_pos, 24, 1, wd_clr, wdc_lin)
;;;	xcall wnmsg (wnd_1, 24, 'im here',1)
;;;	XCALL WHO(CMPCOD)
CNG_DIS,
	ROCPO = 0
	IF (OEFLAG.NE. 1 .OR. CMPCOD.NE.'ROC') GOTO CNG_CON
	XCALL w_disp (wnd_1, wd_pos, 3,1,wd_clr, wdc_eow)
	XCALL w_disp (wnd_1, wd_pos, 4,4,'FILL ORDER FROM SMC?')
	XCALL winpt (wnd_1, 4,26,01,01,'YN',ENTRY,INXCTL)
	IF (INXCTL .EQ. 1)	ROCPO = 9
	CNGCTL = 2
	XCALL wancn (wnd_1, 24, CNGCTL,WHATNO)
	GOTO (CNG_DIS),CNGCTL

CNG_CON,
	LORDNO = OORDNO
;4-19-00 SSQ: delete any zero qty estimate line items...
	FIND(5,ORDLIN,LORDNO)[ERR=CNGOEL]
CNGOEL,
	LOKCTL = 1
	XCALL IOS (5,ORDLIN,READ,LOKCTL)
	IF (LORDNO.NE.OORDNO.OR.LOKCTL.NE.0) RETURN
	IF (LTYPE .EQ. 'M') GOTO CNGOEL
	IF (LQTYOR .EQ. 0)
		BEGIN
		XCALL ISIO (5, ORDLIN, ORDKEY, DELETE, LOKCTL)
		GOTO CNGOEL
		END

	LLOC = OLOC
	LROCPO = ROCPO		;SSQ 6-3-04

	LOKCTL = 1
	XCALL ISIO (5,ORDLIN,ORDKEY,WRITE,LOKCTL)
;;;	IF (LSTOKT.EQ.'S') CALL COMMIT
	CALL COMMIT	;5-13-08 gtitm may return different item...
	GOTO CNGOEL

NOFORL,
	XCALL wnmsg (wnd_1, 24, 'NO LINE ITEMS FOUND FOR THIS ORDER',2)
	RETURN

COMMIT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	XCALL OUTPT (0,0,0,' ... COMMITTING INVENTORY',1)

	KEY = LITMNO

	USING OEFLAG SELECT
	(1),	O_QTY = LQTYOR		;FROM EST TO ORDER
	(2),	O_QTY = -LQTYOR		;FROM ORDER TO EST
	ENDUSING

;--------------------
; 10-21-09 ef, efi mod

	ITEM = LITMNO
;;;	IF (LCFGIM .NE. A15) ITEM = LCFGIM

	IF (LCFGIM .NE. A15) 
		begin
		ITEM = LCFGIM

		clear coptbl		;1-25-10 fake part...
		tblcod = 'FP'
		fp_item = item
		xcall isio (chn182, coptbl, tbl_key, read, lokctl)
		if (lokctl .eq. 0) item = fp_part		;real item
		end

	USING ITEM SELECT
	('JEFI'),	KEY = ITEM(2,3) + ITEM(5,15)
	('EFI'),	KEY = ITEM(1,2) + ITEM(4,15)
	(),		KEY = ITEM
	ENDUSING

	a_lf1 = lf1, 'XXX'
	a_lf2 = lf2, 'XXX'
	a_lf3 = lf3, 'XXXXX'
	xcall gtitm (KEY, a_lf1, a_lf2, a_lf3, gtirfa, gtiflg, vanflg, vanrfa)


; 5-13-08 ssq: use this subroutine to find the correct item to allocate...
;;; 10-21-09>	xcall gtitm (litmno, lf1, lf2, lf3, gtirfa, gtiflg, vanflg, vanrfa)
;--------------------

	if (gtiflg .eq. 0) goto itm_err			;no item found

	close 1
	open (1, su, fil041)

	read (1, invmas, rfa:gtirfa) [err=itm_err]
	
	if (stock .ne. 'S') goto done_commit

	decmal = qtyonh-qtycom+qtyono
	if (o_qty.gt.decmal .and. oeflag.eq.1)
		begin
		xcall w_disp (wnd_1,wd_pos, 23,1,'item: ')
		xcall w_disp (wnd_1, wd_pos,23, 7,litmno,'   ',ldescr)
		xcall wnmsg (wnd_1, 24, 'warning: qty ordered exceeds available inventory',2)
		end

	qtycom = qtycom + o_qty

	write (1, invmas, rfa:gtirfa) [err=done_commit]

done_commit,			;add error mesaage
	
	close 1
	open (1, si, fil041)

	unlock 1
	return
;------------------------------------------------------------

ITM_ERR,
	SRCCTL = 1
	INVMAS =
	XCALL OUTPT (0,0,0,' ... FAILED, ITMIDX RECORD NOT FOUND',1)
	CLOSE 1
	OPEN (1, SI, FIL041)
	RETURN
;---------------------------------------------------


GET_NEG_ZERO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	A_FRT = E_FRGHT
	USING A_FRT SELECT
	('    -0'),	X_FRT = NEG_ZERO
	('     0'),	X_FRT = ZERO
	(),		X_FRT = 9
	ENDUSING

	RETURN
;--------------------------------------------

PRSHP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PRINT SHIP-TO ADDRESS CHANGED PAGE
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	RETURN		;SSQ 2-20-03

	SPF_ORD = OORDNO,	'XXXXXX'

	OPEN(14,O,SPLFIL)
	PLINE = 'THIS SHIP-TO ADDRESS HAS CHANGED!     ORDER:XXXXXX'
;                12345678901234567890123456789012345678901234567890
	PLINE (45,50) = OORDNO,	'XXXXXX'
	CALL PRINT
	PLINE(1,6) = OCUSNO,'ZZZZZX'
	CALL PRINT
	PLINE = OSHPNM
	CALL PRINT
	PLINE = OSHAD1
	CALL PRINT
	PLINE = OSHAD2
	CALL PRINT
	PLINE = OSHAD3
	CALL PRINT

	xcall ascii(27,e_char)
	display(14,eject)
	CLOSE 14
	LPQUE (SPLFIL,DELETE)
	RETURN
PRINT,
	WRITES(14,PBUF)
	PLINE =
	RETURN
;---------------------------------------------------

END


	
