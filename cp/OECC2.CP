SUBROUTINE OECC2		;CC ORDER SUMMARY
	ORDATA	,A	;ORDHDR
	RUNTOT	,D	;ORDER SUBTOTAL
	TAXTOT	,D	;ORDER TAXABLE TOTAL
	FREGHT	,D	;FREIGHT
	CTYPE	,A	;R = RECEIPT (FOR CASH INVOICE)
	TAXCC	,D	;1 = TAX CODE CHANGED IN THIS ROUTINE (4-29-13)

;6-1-16 ssq: only display last 4 digits of cc#
;6-1-16 ssq: print order header info
;8-3-16 ssq: print on oki_ar tray 5=pink
; 5-30-18 ssq: make cusmas isam

EXTERNAL FUNCTION
	R1	,D

.include 'def:hpsub.def'

record	tray
	te	,a1	;esc
		,a*,	'&l'
	trah	,d1,	5
		,a*,	'H'

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT

RECORD	HP_PROG
	HP_ESC	,A1	;ESC
	HP_BOLD	,A4,	'(s#B'

RECORD	TOP_MARGIN
	TM_ESC	,A1
		,A4,	'&l0E'

RECORD	LEFT_MARGIN
	LM_ESC	,A1
		,A4,	'&a0L'

RECORD	ORDFMT
	OF_ORD	,A6
		,A1,	'.'
	OF_NUM	,D1


RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR

RECORD	DATIME
	DATE	,A10
		,A2
	TIME	,A8

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'

RECORD	CUSCTL
	.INCLUDE 'DEF:RD001B.DEF'

;;;RECORD	CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'

RECORD	COPCTL
	.INCLUDE 'DEF:RD060A.DEF'

RECORD	COINFO
	.INCLUDE 'DEF:COINFO.DEF'

RECORD	SALMAN
	.INCLUDE 'DEF:RD054A.DEF'

RECORD	ARTCDE		;AR TAX CODES
	.INCLUDE 'DEF:RD169A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD169B.DEF'

RECORD	CCINFO
	.INCLUDE 'DEF:RD137A.DEF'

RECORD	CCTRAN
	.INCLUDE 'DEF:RD138A.DEF'

RECORD	TMPCUS
	.INCLUDE 'DEF:RD139A.DEF'

RECORD	ORDCM2
	.INCLUDE 'DEF:RD135A.DEF'

RECORD	CUST_KEY
	CK_CUST		,D6
	CK_NUMBR	,D16

RECORD	ALPH_KEY
	AK_ALPH		,A25
	AK_NUMBR	,D16

RECORD	DIG4_KEY
	DK_DIG4		,D4
	DK_NUMBR	,D16
	DK_TYPE		,A1

RECORD
		,A8,	'MVDARXXX'
		,A20,	'MASTER CARD         '
		,A20,	'VISA                '
		,A20,	'DISCOVER            '
		,A20,	'AMERICAN EXPRESS    '
		,A20,	'CASH                '
		,A20,	'INVALID             '
		,A20,	'INVALID             '
		,A20,	'INVALID             '
;                        12345678901234567890
RECORD,X
	CTYP	,8A1
	CDES	,8A20

RECORD	
	MMYY	,D4
RECORD,X
	MM	,D2
	YY	,D2

RECORD	CTL
	ROW	,D2
		,A1
	COL	,D2
		,A1
	MAX	,D2
		,A1
	MIN	,D2
		,A1
	TYPE	,A2

RECORD	PBUF
	PLINE	,A95

RECORD	CHANNEL
	CHN044	,D2,04
	CHN054	,D2
	CHN060	,D2,03
	CHN137	,D2,29
	CHN138	,D2
	CHN139	,D2
	CHN169	,D2
	chn182	,d2
	chn135	,d3

RECORD	CCKEY
	CCCUS	,D6
	CCNUM	,D16


record	print
	page	,d4
	lincnt	,d4
	maxlin	,d2
	dt	,a20	;for date/time
	d	,d8
	t	,d6
	HPR		,D4	;ROW FOR CALL HP
	STAT	,D1
	DTMASK	,A8,	'XX/XX/XX'
	NUMASK	,A8,	'ZZZZZZX-'
	POLX1	,A12
	POLX2	,A12
	SLM	,D3		;length of salesmans name
	SREC	,D5
	S_NAME	,A12
	orgcus	,d6
	a6	,a6
	distr	,d1
	B_DATA	,A100
	B_STRNG	,A100

RECORD	VARS
	cash_string	,a30
	tl	,d6
	X_NUMBR	,D16	;decrpted CC number
	X_CCNUM	,D16	;decrypted version of CCNUM
	routine	,a32
	wc_code	,a3	;7-16-12 will call tax code
	NON_TAX	,D1	;FOR NON-TAX TAX CODES
	MSGCTL	,D1
	cmpcod	,a3
	IS_CASH	,D1
	LNAM	,A25
	SNAM	,A12
	SINT	,A3
	SEXT	,D10
	OPNOK	,D1
	BADDAT	,D1
	I	,D5
	SAVIDX	,D4
	TODAY	,D6
	UPDATE	,D1
	DESC_OK	,D1
	F_KEY	,D3
	SELECT	,D1
	ERR	,D1
	SAVRFA	,A6
	WND1	,D4
	CCDAT	,D4
	CCTYP	,A1
	BADTAX	,D1
	TAXAMT	,D10
	TAX	,3D10
	TAXFRT	,D10
	XDAT1	,D4	;YYMM
	XDAT2	,D4	;YYMM
	XDATE	,D8
	ENTRY	,A30
	BLANKS	,A30
	CNGCTL	,D1
	INXCTL	,D1
	WHATNO	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	LOKCTL	,D1
	SRCCTL	,D1
	BSEND	,D6
	BSMID	,D6
	KEY	,A6
	SWITCH	,D1
	V	,D1
;
PROC
	V = 1

	xcall who (cmpcod)			;10-14-08
	xcall modname (1, routine)

	XCALL ASCII(27,HP_ESC)
	E_CHAR = HP_ESC
	TM_ESC = HP_ESC
	LM_ESC = HP_ESC
	te = hp_esc				;8-3-16

	CLEAR IS_CASH
	IF (%PASSED(CTYPE))
		BEGIN
		IF (CTYPE .EQ. 'R') IS_CASH = 1
		END

	XCALL RDATE(TODAY)
	ORDHDR = ORDATA
	XCALL ISIO (CHN044,ORDHDR,OORDNO,READ,LOKCTL)

	chn135 = 135		;opened in calling program
	clear ordcm2
	c2_ord = oordno
	c2_seq = 0

	lokctl = 0
	xcall isio (chn135, ordcm2, c2_key, read, lokctl)
	if (lokctl .ne. 0) clear ordcm2

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
	call open_splfil



	
DISPLA,	
	XCALL ISIO(CHN138,CCTRAN,OORDNO,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		IF (IS_CASH)
		THEN	BEGIN
			CLEAR CCTRAN
			CT_ORDER = OORDNO
			CT_TYPE = 'R'		;CASH
			CT_EXDAT = TODAY(5,6) + 1
			XCALL ISIO(CHN138,CCTRAN,OORDNO,STORE,LOKCTL)
			XCALL ISIO(CHN138,CCTRAN,OORDNO,READ,LOKCTL)
			END
		ELSE	BEGIN
			XCALL MESAG("CAN'T FIND CCTRAN",1)
			GOTO ENDOFF
			END
		END

; save original credit card info...
	CCNUM = CT_NUMBR
	X_CCNUM = %R1 (CCNUM, 'D')

	CCDAT = CT_EXDAT
;;;	CCTYP = CT_TYPE

	CALL PAINT
	CALL DISP_CC
	GOTO FRT

	CLEAR CNGCTL
	CALL PAINT
NUMBR,
	CTL = '03,21,16,00,#E'
	CALL INPUT
	GOTO(DISPLA,ENDOFF),INXCTL
	IF (ENTRY .EQ. BLANKS)
		BEGIN
		ENTRY(1,16) = X_CCNUM,	'XXXXXXXXXXXXXXXX' [LEFT]
		END
	X_NUMBR = ENTRY(1,16)
	CT_NUMBR = %R1(X_NUMBR, 'E')	;encrypt
	ENTRY(1,19) = X_NUMBR,	'XXXX-XXXX-XXXX-XXXX'
	XCALL OUTPT (ROW,COL,1,ENTRY(1,19),1)
	GOTO ANYCNG

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;TYPE,
;;;	CTL = '04,21,01,00,A '
;;;	CALL INPUT
;;;	GOTO(DISPLA),INXCTL
;;;	IF (ENTRY .EQ. BLANKS) 
;;;		BEGIN
;;;		ENTRY(1,1) = CCTYP
;;;		XCALL OUTPT (ROW,COL,0,CCTYP,1)
;;;		END
;;;	CT_TYPE = ENTRY(1,1)
;;;	CALL GET_CC_DESC
;;;	IF (DESC_OK) 
;;;	THEN	XCALL OUTPT(ROW,24,0,CDES(I),1)
;;;	ELSE	BEGIN
;;;		XCALL MESAG('Invalid Type',1)
;;;		GOTO TYPE
;;;		END
;;;	GOTO ANYCNG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EXDAT,
	CTL = '04,21,04,04,# '
	CALL INPUT
	GOTO(DISPLA),INXCTL
	IF (ENTRY .EQ. BLANKS)	ENTRY(1,4) = CCDAT,	'XXXX'
	MMYY = ENTRY(1,4)
	IF (MM.LT.0 .OR. MM.GT.12)
		BEGIN
		XCALL MESAG('DATE FORMAT IS MMYY',1)
		GOTO EXDAT
		END

	CT_EXDAT = MMYY
	ENTRY(1,5) = CT_EXDAT,	'XX/XX'
	XCALL OUTPT (ROW,COL,0,ENTRY(1,5),1)
	CALL CHKDAT
	GOTO(EXDAT),BADDAT
	GOTO ANYCNG
CVV,
	CTL = '05,21,04,00,# '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	CT_CVV = ENTRY(1,4)
	GOTO ANYCNG
BCD,
	CTL = '06,21,06,00,# '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	CT_ADDR = ENTRY(1,6)
	GOTO ANYCNG
ZIP,
	CTL = '07,21,05,00,# '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	CT_ZIP = ENTRY(1,5)
	GOTO ANYCNG
TAXCDE,
	CTL = '10,21,03,00,A '
	CALL INPUT
	GOTO(DISPLA),INXCTL
	IF (ENTRY .EQ. BLANKS)
		BEGIN
		ENTRY(1,3) = OTAXFL
		XCALL OUTPT (ROW,COL,0,OTAXFL,1)
		END
	OTAXFL = ENTRY(1,3)
	CALL CALC_TAX
	IF(BADTAX) GOTO TAXCDE

	XCALL OUTPT (ROW,COL,0,OTAXFL,1)
	TAXCC = 1	;4-29-13
	GOTO ANYCNG
TAX,
	CTL = '11,21,08,00,$-'
	CALL INPUT
	GOTO(DISPLA),INXCTL
	TAXAMT = ENTRY(1,8)
	GOTO ANYCNG
FRT,
	CTL = '12,21,08,00,$-'
	CALL INPUT
	GOTO(DISPLA,ENDOFF),INXCTL
	CT_FRT = ENTRY(1,8)
	FREGHT = CT_FRT
	OFRGHT = FREGHT
	CALL CALC_TAX		;IN CASE FREGHT IS TAXABLE
	GOTO ANYCNG

DISP_CC,
;;;	XCALL OUTPT (4,21,0,CT_TYPE,1)
; removed 8-29-16
;;;	CALL GET_CC_DESC				
;;;	IF (DESC_OK) XCALL OUTPT(4,24,0,CDES(I),1)

	IF (IS_CASH) GOTO SKIP_CC

	ENTRY(1,19) = X_CCNUM,	'XXXX-XXXX-XXXX-XXXX'
	XCALL OUTPT (3,21,0,ENTRY(1,19),1)
	ENTRY(1,5) = CT_EXDAT	,'XX/XX'
	XCALL OUTPT (4,21,0,ENTRY(1,5),1)
	CALL CHKDAT

	ENTRY(1,4) = CT_CVV, 'XXXX' ;[LEFT]
	XCALL OUTPT (5,21,0,ENTRY(1,4),1)

	ENTRY(1,6) = CT_ADDR, 'ZZZZZX' [LEFT]
	XCALL OUTPT (6,21,0,ENTRY(1,6),1)

	ENTRY(1,5) = CT_ZIP, 'XXXXX'
	XCALL OUTPT (7,21,0,ENTRY(1,5),1)

SKIP_CC,
	ENTRY(1,11) = RUNTOT,	'ZZZ,ZZX.XX-'
	XCALL OUTPT (9,21,0,ENTRY(1,11),1)

;2-13-20: ssq added here per jes
	if (otaxfl(1,2) .eq. 'MN') call chktax	;2-13-20

	XCALL OUTPT (10,21,0,OTAXFL,1)
	CALL CALC_TAX

	ENTRY(1,11) = FREGHT,	'ZZZ,ZZZ.XX-'
	XCALL OUTPT (12,21,0,ENTRY(1,11),1)

ANYCNG,
	ENTRY(1,11) = (RUNTOT + TAXAMT + FREGHT), 'ZZZ,ZZX.XX-'
	XCALL OUTPT (13,21,0,ENTRY(1,11),1)
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (PROCES,CNGBR),CNGCTL+1
CNGBR,
	GOTO (NUMBR,EXDAT,CVV,BCD,ZIP,TAXCDE,TAX,FRT), WHATNO
	GOTO ANYCNG
PROCES,
	CALL CHKDAT
	GOTO (ANYCNG),BADDAT

	CT_SUB = RUNTOT
	CT_FRT = FREGHT
	CT_TAX = TAXAMT
	XCALL ISIO(CHN138, CCTRAN, OORDNO, WRITE, LOKCTL)
	IF (LOKCTL .NE. 0) XCALL MESAG ('ERROR WRITING CCTRAN',1)

; set billing fields in ordhdr...
	XCALL IO(CHN060,COPCTL,1,READ,LOKCTL)	;DEFAULT ACCOUNTS
	UNLOCK CHN060

	FOR I FROM 1 THRU 3
		BEGIN
		OTAX(I) = TAX(I)
		OTXACT(I) = ARTGLN(I)	;TAX ACCOUNTS
		END

	OFRGHT = FREGHT
	OFRACT = DEFRET
	otaxcc = taxcc		;10-3-13 insure this gets updated
	OSALE = RUNTOT 
	XCALL ISIO (CHN044,ORDHDR,OORDNO,WRITE,LOKCTL)
	ORDATA = ORDHDR
	CCCUS = OCUSNO		;SSQ 6-17-02	
;May need to change ccinfo data...
	XCALL ISIO (CHN137,CCINFO,CCKEY,READ,LOKCTL)
	IF(CI_NUMBR .NE. CT_NUMBR)CALL UPD_CCINFO
;;;	IF(CI_TYPE .NE. CT_TYPE) CALL UPD_CCINFO
	IF(CI_EXDAT.NE. CT_EXDAT) CALL UPD_CCINFO
	IF(CI_CVV .NE. CT_CVV) CALL UPD_CCINFO
	IF(CI_ADDR .NE. CT_ADDR) CALL UPD_CCINFO
	IF(CI_ZIP .NE. CT_ZIP) CALL UPD_CCINFO

	IF (OLOC .EQ. 'E') GOTO ENDOFF		;DON'T PRINT QUOTES

;7-11-16 ssq: print header then print cc

		MSGCTL = 7
		XCALL MESAG ('PRINT CC SHEET?', MSGCTL)
		IF (MSGCTL .EQ. 0) 
		THEN	BEGIN
			CALL PRTHDR
			CALL PRT_CC
			END
		ELSE	CLOSE 14

;----------------------------------------------------------
; 6-22-15: alway ask...
;;;	IF (OPRTF .GT. 0)
;;;	THEN	BEGIN
;;;		MSGCTL = 7
;;;		XCALL MESAG ('PRINT CC SHEET?', MSGCTL)
;;;		IF (MSGCTL .EQ. 0) CALL PRT_CC
;;;		END
;;;	
;;;	ELSE	CALL PRT_CC
;----------------------------------------------------------

ENDOFF,
	CALL CLOSE
	XRETURN
;=========================================================

CHKDAT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; CHECK EXPIRATION DATE
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR BADDAT
	IF (IS_CASH) RETURN

	XDAT1(1,2) = CT_EXDAT(3,4)
	XDAT1(3,4) = CT_EXDAT(1,2)
	XDAT2(1,2) = TODAY(5,6)
	XDAT2(3,4) = TODAY(1,2)
	IF (XDAT2 .GT. XDAT1) 
		BEGIN
		XCALL MESAG('Card has Expired!',1)
		BADDAT = 1
		END
	RETURN
;--------------------------------------------

UPD_CCINFO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CI_NUMBR .NE. CT_NUMBR)
	THEN	BEGIN
		XCALL ISIO (CHN137,CCINFO,CCNUM,DELETE,LOKCTL)	;ORIGINAL
		CI_NUMBR = CT_NUMBR
		;;;CI_TYPE = CT_TYPE
		CI_EXDAT = CT_EXDAT
		CI_CVV = CT_CVV
		CI_ADDR = CT_ADDR
		CI_ZIP = CT_ZIP
		XCALL ISIO (CHN137,CCINFO,CI_NUMBR,STORE,LOKCTL)
		END
	ELSE	BEGIN
		CI_TYPE = CT_TYPE
		CI_EXDAT = CT_EXDAT
		CI_CVV = CT_CVV
		CI_ADDR = CT_ADDR
		CI_ZIP = CT_ZIP
		XCALL ISIO (CHN137,CCINFO,CI_NUMBR,WRITE,LOKCTL)
		END
	RETURN
;---------------------------------------------------------

;===============================
PRTHDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Print this info on every page...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	INCR PAGE

	DISPLAY(14,TOP_MARGIN)

	DISPLAY(14,LEFT_MARGIN)

	DT = %DATETIME
	D = DT(1,8)
	T = DT(9,14)
	DATE = D,	'XXXX-XX-XX'
	TIME = T,	'XX:XX:XX'

;top of order...
	IF(PAGE.EQ.1)
		BEGIN
		XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)	
		xcall HP (14, hpPOS,1,60,0, DATIME)

		XCALL HP (14, hpPOS,1,16,0, OCOMNT(1))
		XCALL HP (14, hpPOS,2,16,0, OCOMNT(2))
		END

	XCALL HP (14,hpFONT,hp12CPI+hpBOLD)
	XCALL HP (14, hpPOS,2,79,0,'  ORDER NO. ')

	IF(PAGE.EQ.1)		;8-15-01 SSQ
		BEGIN
		XCALL HP (14, hpPOS,5,78,0,'   Ship Date ')
		END
	XCALL HP (14,hpFONT,hp10CPI)

		XCALL HP (14, hpPOS,3,32,0, C2_COM)

	OF_ORD = OORDNO,	'ZZZZZX'
	IF(OPRTF.LE.0)OPRTF=0
	OF_NUM = OPRTF+1
	PLINE(1,8) = ORDFMT

	XCALL HP (14, hpPOS,3,68,0,PLINE(1,8))	;ORDER #

	XCALL HP (14,hpFONT,hp10CPI+hpBOLD)
	XCALL DATE8(OPROMD, D_OUT, D_OUTR, D_FMT, D_SW)
	IF(PAGE.EQ.1)		;8-15-01 SSQ
		BEGIN
		XCALL HP (14, hpPOS,6,66,0, D_FMT)
		END
	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	

;company name & address...
	XCALL HP (14,hpFONT,hp8CPI+hpBOLD)

	XCALL HP (14, hpPOS,3,1,0, C_NAME)
	xcall HP (14, hpPOS,3,40,0,'Credit Card')

	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	

	XCALL HP (14, hpPOS,4,1,0, C_ADD1)
	XCALL HP (14, hpPOS,5,1,0, C_ADD2)

	XCALL HP (14,hpFONT,hpBOLD)

	PLINE = %STRING(C_LOC,'ZZZ-ZZX-XXXX') + '  ' + %STRING(C_LD,'ZZZ-ZZX-XXXX')
	XCALL HP (14, hpPOS,6,1,0, PLINE(1,%TRIM(PLINE)))

	PLINE = 'FAX:' + %STRING(C_FAX,'ZZZ-ZZX-XXXX')
	XCALL HP (14, hpPOS,7,1,0, PLINE(1,%TRIM(PLINE)))
	CLEAR PLINE
	if (ordtyp .eq. 'I')
		begin
		xcall HP (14, hpPOS,4,30,0,'Internet Order')
		end

	HPR = 10		;FOR SUB_HDR
	MAXLIN = 46

	CALL SUB_HDR

	LINCNT =
	RETURN



SUB_HDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	
	CLEAR PLINE
;-
	IF (OMICUS .NE. 0)	;CASH-CUST 12-23-19
		BEGIN
		cash_string = '*** MISC CASH'
		cash_string(15,20) = omicus, 'ZZZZZX'
		tl = %trim(cash_string)
		cash_string(tl+2,tl+4) = '***'
		clear pline
		pline (9,35) = cash_string

		XCALL HP (14,hpPOS,HPR-2,2,0,pline)
		clear pline
		END
;-

	PLINE(1,6)=OCUSNO			;7-28-14
	PLINE(8,19) = PHONE, 'ZZZ ZZX-XXXX'	;7-28-14
	PLINE(22,27)=CRDLMT,	'ZZZZZZ'	;7-28-14
	XCALL HP (14,hpPOS,HPR-1,1,0,PLINE(1,27))

	CLEAR PLINE(8,19)	;7-28-14
	
	XCALL HP (14,hpFONT,hp12CPI+hpBOLD)
	XCALL HP (14,hpPOS,HPR,1,0,'SOLD')
	XCALL HP (14,hpPOS,HPR,51,0,'SHIP')

	XCALL HP (14,hpPOS,HPR+1,2,0,'TO')
	XCALL HP (14,hpPOS,HPR+1,52,0,'TO')
	XCALL HP (14,hpPOS,HPR,8,0,OCUSNM)

	XCALL HP (14,hpPOS,HPR,58,0,OSHPNM)

	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	XCALL HP (14,hpPOS,HPR+1,8,0,ADD1)
	XCALL HP (14,hpPOS,HPR+2,8,0,ADD2)
	CLEAR PLINE
	PLINE(1,15) = CITY
	PLINE(17,18) = STATE
	PLINE(20,29) = ZIP
	XCALL HP (14,hpPOS,HPR+3,8,0,PLINE(1,30))
	XCALL HP (14,hpPOS,HPR+1,58,0,OSHAD1)
	XCALL HP (14,hpPOS,HPR+2,58,0,OSHAD2)
	XCALL HP (14,hpPOS,HPR+3,58,0,OSHAD3)

	XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)
	XCALL HP (14, hpPOS,HPR+3,0, hpHLINE,164)	;
	XCALL HP (14, hpPOS,HPR+6,0, hpHLINE,164)	;


	XCALL HP (14,hpFONT,hp12CPI+hpBOLD)
	XCALL HP (14, hpPOS,HPR+3,13, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,26, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,38, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,49, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,60, hpVLINE,6)	

	XCALL HP (14, hpPOS,HPR+3,83, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,87, hpVLINE,6)	
	XCALL HP (14, hpPOS,HPR+3,91, hpVLINE,6)	

;first header...
	PLINE = 'Sales-Rep     Cust P.O.    Cust Job      Entered   Shipped   Shipped Via
&           Ppd Col Cod'

	POLX1 = OPONO
	POLX2 =
	IF (OMETRO .NE. BLANKS)
		BEGIN
		POLX1 = OMETRO
		POLX2 = OPONO
		END

	XCALL HP (14,hpPOS,HPR+4,0,0,PLINE(1,%TRIM(PLINE)) )
	
	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)
	CLEAR PLINE

	SREC = OSLMAN
	IF(SREC.LT.1 .OR. SREC.GT.99) SREC = 1
	XCALL IO (CHN054, SALMAN, SREC, READ, LOKCTL)
	SLM = %TRIM(SLSNM)
	IF(SLM .LE. 12) 
	THEN	S_NAME = SLSNM
	ELSE	S_NAME = SLSAD1

	XCALL HP (14,hpFONT,hpBOLD)
	XCALL HP (14,hpPOS,HPR+5,0,0,S_NAME)
	XCALL HP (14,hpFONT,hpMEDIUM)

	PLINE ( 15,26 ) = POLX1
	PLINE ( 28,37 ) = OJOBNO

	XCALL DATE8(OORDDT, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE ( 41,48 ) = D_OUT,DTMASK		; PROMISE DATE
	CALL GET_SCAC
	PLINE (62,77) = SC_NAME		;ssq 10-17-01

	CASE OCLPPD OF 
	  BEGINCASE
	    'P':	PLINE ( 85,85 ) = 'X'
	    'C':	PLINE ( 89,89 ) = 'X'
	    'D':	PLINE ( 93,93 ) = 'X'
	  ENDCASE
	XCALL HP (14,hpPOS,HPR+5,0,0,PLINE)
	CLEAR PLINE

	PLINE ( 15,26 ) = POLX2
	XCALL HP (14,hpPOS,HPR+6,0,0,PLINE)
	CLEAR PLINE
	
	
	XCALL HP (14,hpFONT,hp10CPI+hpMEDIUM)
	CLEAR PLINE
	CALL PRINT		

	RETURN
;------------------------------------------------------------
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

FNDCUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR DISTR
	
	XCALL ISIO (36, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
	THEN	BEGIN
			CUSMAS =
			NAME = '* CUSTOMER NOT ON FILE *'
			END
	ELSE	BEGIN
			A6 = CRDLMT,	'XXXXXX'
			IF (%INSTR(1, A6, '6') )
			THEN	DISTR = 1
			ELSE	DISTR = 0
			END
	RETURN
		
;;;	IF (OCUSNO.NE.CUSNO)
;;;	BEGIN	
;;;	  KEY = OCUSNO,'XXXXXX'
;;;	  XCALL SERCH (37,CUSIDX,KEY,1,6,ORGCUS,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	  LOKCTL = 1
;;;	  CASE SRCCTL OF 
;;;	  BEGINCASE
;;;	  0:	BEGIN
;;;		XCALL IO (36,CUSMAS,IRC001,READ,LOKCTL)
;;;		A6 = CRDLMT,	'XXXXXX'
;;;		IF (%INSTR(1, A6, '6') )
;;;		THEN	DISTR = 1
;;;		ELSE	DISTR = 0
;;;		END
;;;	  1:	BEGIN
;;;		  CUSMAS =
;;;		  NAME = '* CUSTOMER NOT ON FILE *'
;;;		END
;;;	  ENDCASE
;;;	END
;;;	RETURN
;--------------------------------------------------

OPEN_SPLFIL,	;;;;;;;;;;;;;;;;;;;;;;;


;;;	SPORD = OORDNO,'XXXXXX'
;;;	OPEN (14, O, SPLFIL)
	OPEN(14,O,'SPL:OECC2.SPL')

	XCALL HP (14,hpDOTS, 0)		;moved here 2/15/10
	XCALL HP (14,hpTHICK,3)		;moved here 2/15/10

	using cmpcod select
	('SMC'),	writes (14, tray)
	('ROC'),	if (ocusno.ne.10) writes (14, tray)
	('CAT'),	writes (14, tray)
	endusing



	B_DATA(1,2) = 'S~'
	B_DATA(3,8) = OORDNO, 'XXXXXX'

	XCALL B128(B_DATA, B_STRNG, 70)
	XCALL OF128

	RETURN
;--------------------------------------

;===============================
PRT_CC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL SREP(OSLMAN,LNAM,SNAM,SINT,SEXT)


	if (is_cash) goto skip_pcc
	PLINE = 'Credit Card #'
	PLINE(17,36) = X_CCNUM,	'XXXX-XXXX-XXXX-XXXX'	
	CALL PRINT

;;;	PLINE = 'Type'
;;;	PLINE(17,37) = CDES(SAVIDX)
;;;	CALL PRINT

	PLINE = 'Expiration Date'
	PLINE(17,21) = CT_EXDAT,		'XX/XX'
	CALL PRINT

	PLINE = 'Security Code'
	PLINE (17,20) = CT_CVV,	'XXXX'
	CALL PRINT

	PLINE = 'Billing Address'
	PLINE (17,22) = CT_ADDR,	'ZZZZZX'
	CALL PRINT

	PLINE = 'Zip Code'
	PLINE (17,22) = CT_ZIP,	'XXXXX'
	CALL PRINT

skip_pcc,
	PLINE = 'Order Sub Total'
	PLINE(21,31) = RUNTOT,		'ZZZ,ZZX.XX-'
	CALL PRINT
	PLINE = 'Tax Code:'
	PLINE(11,14) = OTAXFL
	PLINE(16,46) = ARTDSC
	CALL PRINT
	PLINE = 'Tax'
	PLINE(21,31) = TAXAMT,	'ZZZ,ZZX.XX-'
	CALL PRINT
	PLINE = 'Shipping & Handling'
	PLINE(21,31) = FREGHT,	'ZZZ,ZZX.XX-'
	CALL PRINT

	HP_BOLD(3,3) = '3'	;BOLD
	PLINE = HP_PROG
	CALL PRINT
	PLINE = 'Order Total'
	PLINE(21,31) = (RUNTOT+TAXAMT+FREGHT),	'ZZZ,ZZX.XX-'
	CALL PRINT
	HP_BOLD(3,3) = '0'	;MEDIUM
	PLINE = HP_PROG
	CALL PRINT
	PLINE = 'Auth # ___________________________'
	CALL PRINT

	IF (OCUSNO .EQ. 10) CALL TMP_CUS
	WRITES(14,EJECT)
	CLOSE 14

	onerror no_que
	using cmpcod select
	('SMC'),	lpque ('spl:oecc2.spl', lpnum:'smc_ar')
	('ROC'),	if (ocusno.eq.10)
			then	lpque ('spl:oecc2.spl', lpnum:'oki_lisam')
			else	lpque ('spl:oecc2.spl', lpnum:'smc_ar')
	('TST'),	goto no_que
	('CAT'),	lpque ('spl:oecc2.spl', lpnum:'oki_ar')
;;;	('CAT'),	goto no_que
	endusing
	offerror


	return
no_que,
	offerror
	LPQUE ('SPL:OECC2.SPL')
	RETURN
TMP_CUS,
	SWITCH = 5
	XCALL FILES (39,'SI',139,SWITCH)
	LOKCTL = 1
	XCALL ISIO (39,TMPCUS,OORDNO,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO NO_TMP_CUS

	CALL PRINT
	PLINE = T_NAME
	CALL PRINT
	PLINE = T_ADD1
	CALL PRINT
	PLINE = T_ADD2
	CALL PRINT
	PLINE = T_CITY
	PLINE(17,18) = T_STATE
	PLINE(20,29) = T_ZIP
	CALL PRINT
NO_TMP_CUS,
	CLOSE 39
	RETURN
;------------------------------------------------------
PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRITES(14,PBUF)
	CLEAR PLINE
	WRITES(14,PBUF)
	RETURN
;------------------------------------------------------

PAINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL OUTPT (2,1,2,' ',1)

	XCALL OUTPT( 3,5,0,'1. NUMBER',1)
	XCALL OUTPT( 4,5,0,'2. EXP DATE',1)
	XCALL OUTPT( 5,5,0,'3. SEC CODE',1)
	XCALL OUTPT( 6,5,0,'4. BILL ADR',1)
	XCALL OUTPT( 7,5,0,'5. ZIP CODE',1)

	XCALL OUTPT( 9,5,0,'   SUB-TOTAL',1)
	XCALL OUTPT(10,5,0,'6. TAX CODE',1)
	XCALL OUTPT(11,5,0,'7. TAX AMOUNT',1)
	XCALL OUTPT(12,5,0,'8. FREIGHT',1)
	XCALL OUTPT(13,5,0,'   ORDER TOTAL',1)

	RETURN
;-------------------------------------------------------

CALC_TAX,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	xcall who (cmpcod)			;10-14-08
;;;	if (cmpcod .eq. 'ROC') goto skphen	;7-16-12
	if (otaxfl(1,2) .eq. 'MN') call chktax	;10-14-08
skphen,
	CLEAR BADTAX 
	SRCCTL = 2
	XCALL SERCH (CHN169,ARTCDE,OTAXFL,1,3,BSEND,BSMID,SRCCTL,4,4,9,0,0,0,0)
	IF (SRCCTL.EQ.0) 
		BEGIN
		XCALL OUTPT (8,30,0,ARTDSC,1)	;;;
		USING OTAXFL(1,1) SELECT
		('W'),	TAXFRT = OFRGHT			;WISC
		('N'),	TAXFRT = OFRGHT			;NEB
		('S'),	TAXFRT = OFRGHT			;SSQ 9-15-10
		('M'),	TAXFRT = OFRGHT			;9-3-14
		(),	TAXFRT =
		ENDUSING

	;;;9-3-14	IF (OTAXFL(1,2) .EQ. 'MN') TAXFRT = OFRGHT	;SSQ 1-9-02
		END
SKP1,
	IF (SRCCTL.OR.OTAXFL.EQ.'   ')
		BEGIN
		XCALL MESAG('INVALID TAX CODE',1)
		BADTAX = 1
		RETURN
		END	

	TAX(1) = ((TAXTOT+TAXFRT) * ARTPRT(1) / 100) # 3
	TAX(2) = ((TAXTOT+TAXFRT) * ARTPRT(2) / 100) # 3
	TAX(3) = ((TAXTOT+TAXFRT) * ARTPRT(3) / 100) # 3

	TAXAMT = TAX(1) + TAX(2) + TAX(3)

	ENTRY(1,11) = TAXAMT,	'ZZZ,ZZX.XX-'
	XCALL OUTPT (11,21,0,ENTRY(1,11),1)
	RETURN
;-------------------------------------------------------

CHKTAX,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; NEED TO CHANGE TAX CODE FOR WILL CALL
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 4-29-13: don't override user changed tax code. First time thru, in
; add mode go thru this logic, but not in change mode.

	if (routine .ne. 'S_ORDADD') return	;only in add mode

	using cmpcod select
	('CAT'),	wc_code = 'MNH'		;2-13-20
	('SMC'),	wc_code = 'MNT'
	('ROC'),	wc_code = 'ILR'
	(),		wc_code = 'MNT'
	endusing

	using otaxfl select
	('MNN','WIN','SDN','IAN'),	NON_TAX = 1
	(),				NON_TAX = 0
	endusing

	USING OSCAC SELECT
	('1 '),	IF(.NOT. NON_TAX) OTAXFL = wc_code
	('10'),	IF(.NOT. NON_TAX) OTAXFL = wc_code
	('WC' ),	IF(.NOT. NON_TAX) OTAXFL = wc_code
	ENDUSING

;;;	USING OSCAC SELECT
;;;	('1 '),	IF(.NOT. NON_TAX) OTAXFL = 'MNT'
;;;	('10'),	IF(.NOT. NON_TAX) OTAXFL = 'MNT'
;;;	('WC' ),	IF(.NOT. NON_TAX) OTAXFL = 'MNT'
;;;	ENDUSING
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	RETURN
;---------------------------------------------------------

GET_CC_DESC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR DESC_OK

	FOR I FROM 1 THRU 8
		BEGIN
		IF (CTYP(I).EQ.'X')EXITLOOP
		IF (CTYP(I).EQ.CT_TYPE) 
			BEGIN
			DESC_OK = 1
			SAVIDX = I
			EXITLOOP
			END
		END
	RETURN
;-------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK
	SWITCH = 5
	XCALL FILES (33,'SU',138,SWITCH)	;CCTRAN
	IF (SWITCH .EQ. 9) RETURN
	CHN138 = 33

	XCALL FILES (34,'I',169,SWITCH)		;ARTCDE
	IF(SWITCH .EQ. 9) RETURN
	CHN169 = 34

	XCALL IO (CHN169,ARTCDE,1,READ,LOKCTL)
	IF (LOKCTL .NE. 0) RETURN
	BSEND = ORG169

	CHN182 = 17	;COP TABLES
	XCALL OECO(CHN182,COINFO)	;SSQ 11-13-03

	CHN054 = 27
	XCALL CHNOPN(27,STAT)
	IF(STAT .EQ. 0)
		BEGIN
		XCALL FILES (27,'I',054,SWITCH)
		END

	XCALL CHNOPN (36,STAT)
	IF (STAT .EQ. 0) XCALL FILES (36,'SI',001,SWITCH)

;;;	XCALL CHNOPN (37,STAT)
;;;	IF (STAT .EQ. 0)XCALL FILES (37,'I',002,SWITCH)

;;;	LOKCTL = 1
;;;	XCALL IO (36,CUSCTL,1,READ,LOKCTL)
;;;	ORGCUS = ORG001

	CALL FNDCUS

	OPNOK = 1
	RETURN
;-------------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN138) CLOSE CHN138
	IF (CHN169) CLOSE CHN169

	CLOSE 36
;;;	CLOSE 37

	RETURN
;-------------------------------------------------------

INPUT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL INPUT(ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,V)

	RETURN
;------------------------------------


