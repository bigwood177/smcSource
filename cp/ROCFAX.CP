;ROCFAX.CP
;BATFAX.CP
;
;	FAX A BATCH OF TODAY'S ORDERS.
;
; 5-30-18 ssq: make cusmas isam

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	BATFAX
	.INCLUDE 'DEF:RD147A.DEF'
;
RECORD	BFXERR
	.INCLUDE 'DEF:BFXERR.DEF'

RECORD	FAXINFO
	.INCLUDE 'DEF:FAXINFO.DEF'

RECORD	SHIPTO
	.INCLUDE 'DEF:RD171A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD171B.DEF'

RECORD	CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD001B.DEF'

;;;RECORD	CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	SKEY
	S_CUST	,D6
	S_SHIP	,D4


RECORD	M_BOX
	MB_ESC	,A1
		,A1,	'&'
		,A1,	'l'
	MBOX	,A2
RECORD	M_BOX2
		,A1,	'g'
	TRA	,A1
		,A1,	'h'
		,A1,	'8'
		,A1,	'C'	;vertical spacing
RECORD	TOP_MARGIN
	TM_ESC	,A1
		,A4,	'&l0E'

RECORD	LEFT_MARGIN
	LM_ESC	,A1
		,A4,	'&a0L'

RECORD	SPLFIL
		,A4,	'SPL:'
		,A1,	'O'
	SPORD	,A6
		,A4,	'.SPL'
	
RECORD	HD1
,	A*,'SLS ORDER# CUSTOMER             TELEPHONE     FAX          STATUS'

RECORD	PRINT
	TITLE	,A*,	'Rockford FAX STATUS REPORT'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PRNTON	,D1
	LINCNT	,D2,60
	PGCNT	,D6
	LPSW	,D2
	RPTNUM	,D3
	PRTTYP	,A1
	PRTCTL	,D3,080
	LPARG	,D1
	PRNTSW	,D1
	PRTCTR	,D1
	
RECORD	PBUF
	PLINE	,A132

RECORD	FXSPOOL
;;;		,a*,	'\\serversbs\de63\faxscan\'
		,a*,	'\\server-hv01\de63\faxscan\'
		,A1,	'F'
	O_ORDER	,D6
		,A1,	'.'
	O_EXT	,A3,	'txt'

RECORD	BUFFER
		,A100


RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN044	,D2
	CHN171	,D2
	CHN147	,D2
	FAXCHN	,D2
	ERRCHN	,D2
	CHN182	,D2
	CHNRET	,D2		;RETRY

RECORD	VARS
	no_errors	,d1
	LOKCTL	,D1
	READ	,D1,0
	CMPCOD	,A3
	RETFIL		,A14,	'ROC:BATFAX.RET'
	RETRY		,D1
	CNTERR		,D4
	ERR_BUFFER	,A100

	PRINTER_NAME	,A4
	SINT	,A3
	LNAM	,A25
	SNAM	,A12
	OPNOK	,D1
	TODAY	,D8
	B_ERR	,D1	;ERROR STATUS
	FAXNUM	,D10
	SAVKEY	,D10	;SAVE CUST KEY
	BSENDS	,D6	;SHIPTO
	BSENDC	,D6	;CUSMAS
	BSMID	,D6
	SRCCTL	,D1
	SWITCH	,D1
;
PROC
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	xcall who (cmpcod)
;;;	clear coptbl
;;;	tblcod = 'SR'
;;;	sr_cmp = cmpcod
;;;	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
;;;	if (lokctl .ne. 0)
;;;		begin
;;;		xcall mesag('no coptbl server record',1)
;;;		goto endoff
;;;		end
;;;
;;;	sv_ser = sr_server

	XCALL RDAT8(TODAY)	

	CLEAR RETRY
RETRY,
	SAVKEY = -1
	CLEAR CNTERR
LOOP,
	READS (CHN147, BATFAX, EOF)

	READ (CHN044, ORDHDR, BFX_ORDER) [ERR=LOOP]	;SKIP OF NOT FOUND

	FC_DATE(1,4) = OORDDT(5,8)
	FC_DATE(5,6) = OORDDT(3,4)
	FC_CUSNAME = OCUSNM

	IF (BFX_CUSKEY .NE. SAVKEY) CALL NEW_CUST

	IF (FC_FAX .EQ. 0) 
		BEGIN
		BUFFER = "No phone number"
		CALL FAX_ERROR		;COULD NOT SENT FAX, UPDATE LOG
		GOTO LOOP
		END

	;create spool file of order in rtf format...
	XCALL BFORD(BFX_ORDER, B_ERR, CHANNEL, 'ROC')

	GOTO LOOP

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD,HD
&		,LG,LG,LG,0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;-------------------------------------------------------------

EOF,
;;;	if (savkey .eq. -1) goto report	;nothing in batfax file
	if (savkey .eq. -1) 
		begin
		call report
		goto endoff
		end

	WRITES(FAXCHN,'}')		;TERMINATE RTF FORMAT
	CLOSE FAXCHN

	CLOSE CHNRET
	CALL REPORT			;2-19-03

;;;	INCR RETRY			
	retry = 9		;to test 2-20-03
	IF (RETRY .LE. 1)		; JUST RETRY ONCE...
		BEGIN
		CLOSE CHN147		;CLOSE BATFAX.ROC
		OPEN (CHN147,I,RETFIL)	;NEW PROCESS THE ERROR FILE
		RETFIL(14,14) = '2'	;2ND RETRY FILE
		OPEN (CHNRET,O,RETFIL)

		clear bfxerr
	;;;	CLOSE ERRCHN
		OPEN(ERRCHN,O,'ROC:FAXERR.DAT')	;NEW ERROR FILE

		GOTO RETRY
		END
	

ENDOFF,
	XCALL FLAGS (7000000)	;suppress program termination message
	STOP


REPORT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; 2-19-03 now a routine
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	no_errors = 1

	PRINTER_NAME = 'BLUE'
	SPORD = 'FAXRPT'
	OPEN (14, O, SPLFIL)

	MBOX= '1'	;STEVE'S MB

	XCALL ASCII(27,MB_ESC)
	LM_ESC=MB_ESC
	TM_ESC=MB_ESC

	TRA=4,'X'	;4 IS TRAY 4

	clear bfxerr
	CLOSE ERRCHN
	OPEN(ERRCHN,I,'ROC:FAXERR.DAT')
R_LOOP,
	READS(ERRCHN, BFXERR, R_EOF)
	XCALL SREP(BE_SMAN,LNAM,SNAM,SINT)
;-

	PLINE(1,3) = SINT
	PLINE(5,10) = BE_ORDER,	'XXXXXX'
	PLINE(12,17) = BE_CUST,	'ZZZZZX'
	PLINE(19,39) = BE_NAME
	PLINE(41,52) = BE_PHONE,	'ZZZ ZZX-XXXX'
	PLINE(55,66) = BE_FAX,		'ZZZ ZZX-XXXX'
	PLINE(68,88) = BE_MSG

	CALL PRINT
;-

;;;	PLINE(1,3) = SINT
;;;	PLINE(5,10) = BE_ORDER,	'XXXXXX'
;;;	PLINE(12,31) = BE_NAME
;;;	PLINE(33,44) = BE_PHONE,	'ZZZ ZZZ ZZZZ'
;;;	PLINE(47,58) = BE_FAX,		'ZZZ ZZZ ZZZZ'
;;;	PLINE(60,80) = BE_MSG
;;;	CALL PRINT
	clear no_errors			;at least one error printed
	GOTO R_LOOP
R_EOF,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (no_errors)	return	;10-3-13

;;;	if (no_errors)
;;;		begin
;;;		pline = "No Errors Reported"
;;;		call print
;;;		end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE ERRCHN
	CLOSE 14
	LPQUE (SPLFIL)		;BLUE sb the default now (12-12-01)
	RETURN
;---------------------------------------------------------------

NEW_CUST,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (SAVKEY .EQ. -1) GOTO OUT_CUST
	WRITES(FAXCHN,'}')		;TERMINATE RTF FORMAT
	CLOSE FAXCHN			;CLOSE OFF CURRENT FAX SPOOL FILE
	CALL CHECK_FAX_STATUS

OUT_CUST,
	XCALL ISIO (CHN001, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR FAXNUM
	FAXNUM = EFAX				;FAX # FROM CUSMAS
	CALL GET_SHIPTO
	IF (SHFAX .GT. 0) FAXNUM = SHFAX	;FAX # FROM SHIP-TO
	
	
;;;	XCALL SERCH (CHN002,CUSIDX,OCUSNO,1,6,BSENDC,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	IF (SRCCTL.NE.0 .OR. IRC001.LE.0)
;;;	THEN	CLEAR FAXNUM			;NO CUSTOMER
;;;	ELSE	BEGIN
;;;		READ(CHN001,CUSMAS,IRC001)
;;;		FAXNUM = EFAX				;FAX # FROM CUSMAS
;;;		CALL GET_SHIPTO
;;;		IF (SHFAX .GT. 0) FAXNUM = SHFAX	;FAX # FROM SHIP-TO
;;;		END

	SAVKEY = BFX_CUSKEY

	FC_FAX = FAXNUM	
	O_ORDER = BFX_ORDER
	O_EXT = "TXT"
	XCALL BFFAX(FAXINFO,FXSPOOL,FAXCHN,'ROC')	;OPEN NEW SPOOL FILE
;;;	XCALL BFFAX(FAXINFO,FXSPOOL,FAXCHN)	;OPEN NEW SPOOL FILE
				;ADD COVER PAGE TO SPOOL FILE OF ORDERS,
; set up the error record in case fax does not go...
	BE_DATE = BFX_DATE
	BE_CUST = BFX_CUST
	BE_SHIP = BFX_SHIP
	BE_SMAN = BFX_SLSMAN
	BE_FAX = FAXNUM
	BE_MSG = BUFFER
	BE_NAME = NAME
	BE_PHONE = PHONE
	BE_ORDER = BFX_ORDER

	RETURN
;--------------------------------------------
GET_SHIPTO,
	CLEAR SHFAX		;ASSUME ERROR
	S_CUST = OCUSNO
	S_SHIP = OSHPTO
;;;	XCALL SERCH (CHN171,SHIPTO,SKEY,1,10,BSENDS,BSMID,SRCCTL,4,11,16,0,0,0,0)
;;;	IF (SRCCTL) RETURN
	READ(CHN171,SHIPTO,SKEY) [ERR=NO_SHPTO]
	RETURN			;SSQ 9-4-02
NO_SHPTO,
	CLEAR SHFAX		;SSQ 9-4-02
	RETURN	
;------------------------------------------

CHECK_FAX_STATUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; faxmaker will create either an ".OK"
			;;; or ".ERR" file...
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	goto fax_ok	;skip check during testing...
CFS_LOOP,
	SLEEP 2			
	O_EXT = 'OK'
	OPEN (22,I,FXSPOOL)[ERR=CHK_ERR]
CFS_READ,
	READS (22,BUFFER,EOF_CFS)
	IF (.NOT. %INSTR(1, BUFFER ,"Fax sent successfully") )GOTO CFS_READ
fax_ok,
	BFX_STATUS = 1		;SUCCESS
;;;	WRITE (CHN147, BATFAX, BFX_KEY)
	CLOSE 22
;;;2/20/03 debug	XCALL DELET(FXSPOOL)	;DELETE THE FILE
	RETURN

EOF_CFS,
	CLOSE 22
	BUFFER = 'CHECK FAXSCAN DIR FOR "OK" FILE ...'
	CALL FAX_ERROR
;;;	XCALL DELET(O_FILE)	;SAVE THE FILE FOR INSPECTION...
	RETURN

CHK_ERR,
	O_EXT = 'ERR'
	OPEN(22,I,FXSPOOL) [ERR=CFS_LOOP]
ERR_READ,
	READS (22, BUFFER, EOF_FER)
	IF (%INSTR(1, BUFFER, "Description:") ) GOTO EOF_FER
	GOTO ERR_READ
EOF_FER,
	close 22		;ssq 12-11-01
;;;	ERR_BUFFER = BUFFER(13,100)	;SKIP "Description:"
	ERR_BUFFER = BUFFER(35,100)	;SKIP "Description:"
	BUFFER = ERR_BUFFER
	CALL FAX_ERROR
	RETURN
;----------------------------------------------------------


FAX_ERROR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;SET THE STATUS RECORD ERROR FLAG 
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	BFX_STATUS = 2		;ERROR


UPDATE_STATUS,
	IF (BFX_STATUS .EQ. 1) BUFFER = 'SENT'
	;error fields set at "out_cust" above
	BE_FAX = FAXNUM
	BE_MSG = BUFFER
	WRITES (ERRCHN, BFXERR)
	
	INCR CNTERR			;# OF BAD FAXES
	WRITES (CHNRET, BATFAX)		;RETRY FILE
	RETURN
;------------------------------------------
OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK
	
;;;	OPEN(1,I,'ROC:CUSMAS.ROC')
	OPEN(1,I,'SMC:CUSMAS.SMM')
	CHN001 = 1

;;;	OPEN(2,'I','ROC:CUSIDX.ROC')
;;;	OPEN(2,'I','SMC:CUSIDX.SMC')
;;;	CHN002 = 2

	OPEN(4,SI,'ROC:ORDHDR.ROM')
	CHN044 = 4

	OPEN(5,SI,'ROC:SHIPTO.ROM')
	CHN171 = 5

	OPEN(17,SI,'ROC:COPTBL.ROM')
	CHN182 = 17	;COP TABLES

	OPEN(6,I,'ROC:BATFAX.ROC')
	CHN147 = 6

	FAXCHN = 7
	ERRCHN = 8
	OPEN(ERRCHN,O,'ROC:FAXERR.DAT')

;;;	READ (CHN001,CUSMAS,1)
;;;	BSENDC = ORG001

	OPEN(10,O,RETFIL)	;RETRY FILE (ROC:BATFAX.RET)
	CHNRET = 10

	OPNOK = 1
	RETURN
;------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN001
	CLOSE CHN002
	CLOSE CHN044
	CLOSE CHN171
	CLOSE CHN147
	CLOSE FAXCHN
	CLOSE ERRCHN

	RETURN
;------------------------------------------
;SLS ORDER# CUSTOMER             TELEPHONE     FAX          STATUS
;AAA ZZZZZX AAAAAAAAAAAAAAAAAAAA XXX XXX XXXX  XXX XXX XXXX AAAAAAAAAAAAAAAAAAAA
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7         8

