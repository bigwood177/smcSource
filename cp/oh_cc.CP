
;OH_CC.CP - on hold credit card orders - print this instead of PDORD -
; 		combines the top of PDORD w/ cc print-out
; 5-30-18 ssq: make cusmas isam


SUBROUTINE OH_CC
	ORDNO	,D

EXTERNAL FUNCTION
	TRN3	,D
	R1	,D

EXTERNAL FUNCTION
	GU_NAME	,A

	.INCLUDE 'DEF:HPSUB.DEF'


RECORD	ORDCM2
	.INCLUDE 'DEF:RD135A.DEF'


RECORD	B3OF9
	E_ESC	,A1
		,A*,	'(80X'

RECORD	COINFO
	.INCLUDE 'DEF:COINFO.DEF'

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


RECORD	CRHOLD
	.INCLUDE 'DEF:RD195A.DEF'

RECORD	C1_LINE
	C1_TYPE	,A1
		,A1
		,A15,	'XXXX-XXXX-XXXX-'
	C1_NUM	,D4

RECORD	C2_LINE
		,A*,	'EXP '
	C2_DAT	,A5	;XX/XX
		,A3
		,A*,	'AUTH '
	C2_AUTH	,A6
	

RECORD	P_EJECT
	PJ_ESC	,A1
		,A1,	'&'
		,A1,	'l'
		,A1,	'0'
		,A1,	'H'
		
record	tray
	te	,a1	;esc
		,a*,	'&l'
	trah	,d1
		,a*,	'H'
	
RECORD	M_BOX
	MB_ESC	,A1
		,A1,	'&'
		,A1,	'l'
	MBOX	,A2
RECORD	M_BOX2
		,A1,	'g'
	TRA	,A2
	MB_END	,A3


RECORD	TOP_MARGIN
	TM_ESC	,A1
		,A4,	'&l0E'

RECORD	LEFT_MARGIN
	LM_ESC	,A1
		,A4,	'&a0L'
;-

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


RECORD	SALMAN
	.INCLUDE 'DEF:RD054A.DEF'

RECORD	COPCTL
	.INCLUDE 'DEF:RD060A.DEF'

RECORD CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'

RECORD CUSCTL	,X
	.INCLUDE 'DEF:RD001B.DEF'

;;;RECORD CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'

RECORD ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'


RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'


RECORD	SPLFIL
		,A4,	'SPL:'
		,A1,	'O'
	SPORD	,A6
		,A4,	'.SPL'
	
RECORD	F_MEMOS
	.INCLUDE 'def:CPMEMO.DEF'
		


RECORD	PTEST
		,A80
RECORD,X
	P_TST	,15A5

RECORD
	INIT	,20A3,	'JJM','PDS','LAM','DEG','JTH',
&			'DLH','JIM','SJM','CJM','BIL',
&			'MAW','PAT','BJZ','JPV','MJM',
&			'JWS','GJK','   ','   ','   '
RECORD	FF
	NEG_ZERO	,D1,1
	ZERO		,D1,0
	X_FRT		,D1
	A_FRT		,A6


RECORD	CHANNEL
	CHN182	,D2
	CHN044	,D2
	CHN054	,D2
	CHN135	,D3
	CHN138	,D2
	CHN175	,D2
	CHN195	,D2
	chn079	,d2
	chn160	,d2

RECORD
	TM2	,A90
	TM3	,A90
RECORD,X
	TM2A	,3A30
	TM3A	,3A30

RECORD	LP_BOLD
	B_ESC	,A1
		,A1,	'%'
	L_BOLD	,A1

RECORD	LP_MEDIUM
	M_ESC	,A1
		,A1,	'%'
	L_MED	,A1


RECORD	ORDFMT
	OF_ORD	,A6
		,A1,	'.'
	OF_NUM	,D1


RECORD	HP_PROG
	HP_ESC	,A1	;ESC
	HP_BOLD	,A4,	'(s#B'

RECORD	DATIME
	DATE	,A10
		,A2
	TIME	,A8

RECORD	VARS
	RUNTOT	,D8
	TAXAMT	,D8
	FREGHT	,D8

	X_NUMBR	,D16	;decrypted cc #


	bad_blue	,d1	;oki_blue not working
	sr_qty	,d5	;dept k,m
	sr_dlr	,d8	;dept k,m
	cmpcod	,a3
	SAVSS	,D1
        barfil  ,a14,   'cp:c128s16.bar'
        exeprc  ,d18
	A5	,A5
	A6	,A6
	distr	,d1	;1 = cust is distributor
	A12	,A12
	on_hold	,d1	;1 = order on hold
	I	,D6
	dt	,a20	;for date/time
	d	,d8
	t	,d6
	B_DATA	,A100
	B_STRNG	,A100
	YELLOW	,D2	;Tray number for yellow paper
	BLUE	,D2	;Tray number for blue paper
	U_NAME	,A25
	TRA_B	,D2
	TRA_Y	,D2
	TRA_W	,D1
	cd_rfa	,a6
	POLX1	,A12
	POLX2	,A12
	SLM	,D3		;length of salesmans name
	SREC	,D5
	S_NAME	,A12
	LL	,D2
	S_LL	,D2
	AR_PRINTER	,A13	;FOR ON HOLD
	PRINTER_NAME	,A13	;3-1-10 to match coptbl records.
	LP_ON		,D1	;first pass just to count pages
	NPAG		,D4
	HPR		,D4	;ROW FOR CALL HP


	t_cnt	,d4
	VL	,D3,	082
	VL2	,D3
	SZ1	,D3
	SZ2	,D3

	SKPLIN	,D2
	NO_DUCT	,D1
	A2	,A2
	SAVQTY	,D6
	STAT	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DECMAL	,D18
	PLINE	,A95
	S_PLINE	,A95
	LONG_LINE	,A110
	S_LINCNT	,D3
	S_PAGE		,D3
	LINCNT	,D3
	TOTPRC		,D8
	SAV_TOTPRC	,D8
	BLANKS	,A30
	SAVJOINT	,D5
	SAVFEET		,D6
	SAVFLIN		,D6
	SAVLBS		,D6
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
	J	,D5
	TL	,D3
	SEQNO	,D2
	SKIP_LINE	,D1		;SKIP THIS LINE IF BLOCK MEMO
	MEMO_ON		,D1
	BLANK90		,A90
	TMPDSC		,A30
	REM		,D4
	DASHES		,A30,	'------------------------------'
	LFEED	,D2
	DTMASK	,A8,	'XX/XX/XX'
	NUMASK	,A8,	'ZZZZZZX-'
	MAXLIN	,D2

PROC

	switch = 5
	xcall files (40, 'SI', 195, SWITCH)
	CHN195 = 40
	XCALL ISIO (CHN195, CRHOLD, ORDNO, READ, LOKCTL)
	CLOSE 40

	IF (LOKCTL .NE. 0) XRETURN		;NOT ON HOLD
	IF (CH_FLAG .LT. '2') XRETURN		;


	
	cmhpLPI = 12	;2-15-10 ssq: this is critical for spacing, don't
			;know how the value changes to 6...

	xcall who (cmpcod)

	chn135 = 135		;opened in calling program
	clear ordcm2
	c2_ord = ordno
	c2_seq = 0

	lokctl = 0
	xcall isio (chn135, ordcm2, c2_key, read, lokctl)
	if (lokctl .ne. 0) clear ordcm2


	U_NAME = %GU_NAME
	UPCASE U_NAME
	XCALL ASCII(27,E_ESC)
	te = e_esc

	LM_ESC=e_ESC
	TM_ESC=e_ESC
	HP_ESC = e_esc

	CALL OPENS
	call open_splfil


	USING U_NAME SELECT		;6/27/03 SSQ
	('WCALL'),	CALL WCALL
	('CATHYM'),	CALL SMP
	(),		CALL SMC
	ENDUSING

	IF (OSCAC .EQ. '10') CALL WCALL

	CALL GET_SCAC

	using cmpcod select
	('SMC'),if (sc_que.ne.'JESSIE') call wcall	;12-27-11 all non-dist to w/c
	('TST'),if (sc_que.ne.'JESSIE') call wcall	;12-27-11 all non-dist to w/c
	endusing


	TRA_B = BLUE
	TRA_Y = YELLOW

	CALL PRTHDR
	CALL PRINT_CC

	CLOSE 14


	USING CMPCOD SELECT
	('CAT'),	AR_PRINTER = PRINTER_NAME	;DON'T PRINT ON DI'S
	(),		AR_PRINTER = 'SMC_AR'		;DI'S PRINTER
	ENDUSING


	lpque (barfil, lpnum:AR_PRINTER)              ;cp:c128s16.bar - insure barcode loaded
	LPQUE (SPLFIL, LPNUM:AR_PRINTER, DELETE)			;DIANNA
;;;	lpque (splfil)	;debug

	LOKCTL = 1
	XCALL ISIO (CHN044,ORDHDR,ORDNO,WRITE,LOKCTL)


ENDOFF,
	CLOSE CHN138
;;;	CLOSE CHN195
	close chn079
	close chn160

	XRETURN

;================================================

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

;;;	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	
;;;	XCALL HP (14, hpPOS,63,1,0, ORDCOM(1))
;;;	XCALL HP (14, hpPOS,64,1,0, ORDCOM(2))

;;;	PLINE(40,80) = B_STRNG
;;;	XCALL HP (14,hpPOS,8,20,0,PLINE(20,80))
;;;	XCALL OF128

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


;;;	XCALL HP (14,hpFONT,hp12CPI+hpMEDIUM)	

	
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
;;;PLINE='12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345'

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

PRINT_CC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	if (is_cash) goto skip_pcc
	PLINE = 'Credit Card #'
	PLINE(17,36) = X_NUMBR,	'XXXX-XXXX-XXXX-XXXX'	
	CALL PRINT

	PLINE = 'Expiration Date'
	PLINE(17,21) = CT_EXDAT,		'XX/XX'
	CALL PRINT

	PLINE = 'Security Code'
	PLINE (17,20) = CT_CVV,	'ZZZX'
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
	WRITES(14,PLINE)
	CLEAR PLINE
	WRITES(14,PLINE)
	RETURN
;------------------------------------------------------




FNDCUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR DISTR

	XCALL ISIO (36, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
	THEN	BEGIN
			A6 = CRDLMT,	'XXXXXX'
			IF (%INSTR(1, A6, '6') )
			THEN	DISTR = 1
			ELSE	DISTR = 0
			END
	ELSE	BEGIN
			CUSMAS =
			NAME = '* CUSTOMER NOT ON FILE *'
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



;PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	WRITES (14,PLINE)
;
;	PLINE =
;	INCR LINCNT
;	RETURN
;--------------------------------------------------

;===============================================

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ASCII(27, B_ESC)
	XCALL ASCII(27,PJ_ESC)
	L_BOLD = 'G'		;START ITALIC
	M_ESC = B_ESC
	L_MED = 'H'		;END ITALIC
	SWITCH = 5

;---------------------------------------------------
; ORDHDR already opened on ch 4 in calling routines...
	CHN044 = 4


	CHN175 = 26
	XCALL CHNOPN (26,STAT)
	IF (STAT .EQ. 0)
		BEGIN
		XCALL FILES (26,'SI',175,SWITCH)
		END

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


	switch = 5
	xcall files (79, 'SU', 079, switch)
	chn079 = 79

	switch = 5
	xcall files (60, 'SU', 160, switch)
	chn160 = 60

;;;	switch = 5
;;;	xcall files (40, 'SI', 195, SWITCH)
;;;	CHN195 = 40

	CHN182 = 17	;COP TABLES

	XCALL OECO(CHN182,COINFO)	;SSQ 11-13-03


	LOKCTL = 1
	XCALL ISIO (CHN044,ORDHDR,ORDNO,READ,LOKCTL)
;;;	IF (LOKCTL.NE.0.OR.OORDNO.NE.ORDNO) GOTO INVORD


;;;	LOKCTL = 1
;;;	XCALL IO (36,CUSCTL,1,READ,LOKCTL)
;;;	ORGCUS = ORG001

	CALL FNDCUS

	LOKCTL = 1
	XCALL IO(3,COPCTL,1,READ,LOKCTL)

	SWITCH = 5
	XCALL FILES (51,'SI',138,SWITCH)
	CHN138 = 51

	XCALL ISIO (CHN138,CCTRAN,OORDNO,READ,LOKCTL)
	IF (LOKCTL .NE. 0) CCTRAN =
	X_NUMBR = %R1(CT_NUMBR,'D')	;decrypt

	RUNTOT = CT_SUB
	FREGHT = CT_FRT
	TAXAMT = CT_TAX
	RETURN
;----------------------------------------------------------

OPEN_SPLFIL,	;;;;;;;;;;;;;;;;;;;;;;;


	SPORD = ORDNO,'XXXXXX'
	OPEN (14, O, SPLFIL)
	XCALL HP (14,hpDOTS, 0)		;moved here 2/15/10
	XCALL HP (14,hpTHICK,3)		;moved here 2/15/10



	B_DATA(1,2) = 'S~'
	B_DATA(3,8) = OORDNO, 'XXXXXX'

	XCALL B128(B_DATA, B_STRNG, 70)
	XCALL OF128

	RETURN
;--------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN044

	CLOSE CHN175
	CLOSE CHN138
;;;	CLOSE CHN195
	close chn079
	close chn160

	RETURN
;----------------------------------------------------------

SMC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;4=TRAY1, 1=TRAY2, 5=TRAY3

	PRINTER_NAME = 'SMC_BLUE'

;switched 2-3-12:
	blue = 4
	yellow = 5
	RETURN
;----------------------------------------------------------

SMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;4=TRAY1, 1=TRAY2, 5=TRAY3

	PRINTER_NAME= 'OKI_SMP'

;for now use same settings as smc... ssq 7/10/03
	BLUE = 5	;TRAY 2 IS BLUE
	YELLOW = 20	;TRAY 3 IS YELLOW

	RETURN
;----------------------------------------------------------

WCALL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;4=TRAY1, 1=TRAY2, 5=TRAY3


	using cmpcod select
	('SMC','TST'),	begin
		PRINTER_NAME= 'SMC_BLUE'
		BLUE = 4	;TRAY 1 IS BLUE
		YELLOW = 5	;TRAY 2 IS YELLOW
		end
	(),	begin
		PRINTER_NAME= 'new_blue'
		BLUE = 4	;TRAY 1 IS BLUE
		YELLOW = 1	;TRAY 2 IS YELLOW
		end	
	endusing

	RETURN
;----------------------------------------------------------

END

