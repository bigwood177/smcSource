;UPSEDT.CP


RECORD UPSDATA
	.INCLUDE 'DEF:UPSDAT.DEF'
		
RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'


RECORD TITLE
		,A30
RECORD HDR1
		,A*,	'SHIP TO:                        ORDER SHIP TO:'
		,A*,	'                          AMOUNT'


RECORD	TR_LIN
	TR_TRACK	,A20
			,A*,	'  PKG WT: '
	TR_WGT		,A8

RECORD	CHANNEL
	CHNUPS	,D2
	CHN044	,D2
	CHNHST	,D2

RECORD	VARS
	I	,D2
	J	,D2
	SAVTRACK	,A8
	OPNOK	,D1

	LSTORD	,D6
	HDR	,A6,	'NO HDR'
	LEG	,A9,	'NO LEGEND'
	U_BUFF	,3A30
	O_BUFF	,3A30
	X_BUFF	,3A30
	BLANKS	,A30
	ACTCNT	,D4
	RECNO	,D4
	STRTCT	,A2
	ENDCAT	,A2
	KEY   	,A2
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	LINCNT	,D2,	60
	SPLFIL	,A14
	LPONSW	,D1
	LPARG	,D1
	RPTNUM	,D3
	PRTTYP	,A1
	PGCNT	,D6,	000000
	PLINE	,A132
	PRTCTL	,D3
	ENTRY	,A30
	INXCTL	,D1
	DECMAL	,D4
	CNGCTL	,D1
	WHATNO	,D1
	SWITCH	,D1
	LPSW	,D2
	V	,D1
	READ	,D1	,0
	WRITE	,D1	,1
	LOKCTL	,D1
	TODAY	,D8

PROC
	XCALL TERID (V)
	XCALL OUTPT (2,1,1,'UPS/SPEE-DEE DATA UPLOAD REGISTER',1)
	LSTORD = -1
BEGIN,
	CALL OPENS
	IF (.NOT. OPNOK) GOTO PRINT_ERROR

	SAVTRACK = '***'

	XCALL RDAT8(TODAY)
	FIND (7, UPSDATA, TODAY, KRF=1) [ERR=LOOP]
LOOP,
	READS (7, UPSDATA, EOF)
	IF (UDATE .NE. TODAY) GOTO EOF

	IF (SAVTRACK .NE. TRACK(1,8) ) CALL NEW_TRACK


	IF (ORDER .EQ. LSTORD) GOTO PKG_ONLY

	XCALL FILL ('_', PLINE(1,80))
	CALL PRINT
	LSTORD = ORDER

	CALL GET_ORDER

	CALL BUFFER

	PLINE (1,30) = UNAME
	PLINE (32,37) = ORDER,	'ZZZZZX'
	PLINE (40,78) = OSHPNM
	PLINE (70,78) = TOTAMT	,'ZZ,ZZZ.XX'
	CALL PRINT
		
	PLINE (1,30) = U_BUFF(1)
	PLINE (40,78) = O_BUFF(1)
	IF (VOID .EQ. 'Y') PLINE (70,78) = '* VOID *'
	CALL PRINT

	PLINE (1,30) = U_BUFF(2)
	PLINE (40,78) = O_BUFF(2)
	CALL PRINT

	PLINE (1,30) = U_BUFF(3)
	PLINE (40,78) = O_BUFF(3)
	CALL PRINT

PKG_ONLY,
	TR_TRACK = TRACK
	TR_WGT = PKGWGT
	PLINE = TR_LIN
	CALL PRINT
;;;	XCALL FILL ('_', PLINE(1,80))
;;;	CALL PRINT

	GOTO LOOP



BUFFER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	U_BUFF(1) = UADD1
	U_BUFF(2) = UADD2

	ENTRY(1,15) = UCITY
	ENTRY(17,18) = USTAT
	ENTRY(20,30) = UZIP
	
	U_BUFF(3) = ENTRY

	O_BUFF(1) = OSHAD1
	O_BUFF(2) = OSHAD2
	O_BUFF(3) = OSHAD3

	CLEAR J
	FOR I FROM 1 THRU 3
		BEGIN
		IF (O_BUFF(I) .EQ. BLANKS)NEXTLOOP
		INCR J
		O_BUFF(J) = O_BUFF(I)
		END

	IF (J .LT. 3) CLEAR O_BUFF(3) 

	CLEAR J
	FOR I FROM 1 THRU 3
		BEGIN
		IF (U_BUFF(I) .EQ. BLANKS)NEXTLOOP
		INCR J
		U_BUFF(J) = U_BUFF(I)
		END

	IF (J .LT. 3) CLEAR U_BUFF(3) 

	RETURN
;------------------------------------

GET_ORDER,	;;;;;;;;;;;;;;;;;;;;;;
	READ (CHN044, ORDHDR, ORDER)[ERR=NO_ORDER]
	RETURN
NO_ORDER,
	READ (CHNHST, ORDHDR, ORDER)[ERR=NO_HIST]
	RETURN
NO_HIST,
	CLEAR ORDHDR
	RETURN
;------------------------------------

PRINT_ERROR,	;;;;;;;;;;;;;;;;;;;;;
	RETURN
;------------------------------------

PRINT,
	IF (LPONSW.EQ.0) CALL PRNTON
	PRTCTL = 80
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,HDR,HDR,
&			LEG,LEG,LEG,0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
PRNTON,
	SPLFIL (5,6) = 'EL'
	LPSW = 1	; PRINT, SPOOL OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GOTO PRNTON

	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	LPONSW = 1
	RETURN
;************************************************************************
NEW_TRACK,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; NEW TRACKING
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	LINCNT = 66

	CLEAR LSTORD

	SAVTRACK = TRACK(1,8)

	USING SAVTRACK SELECT
	('SP002714'),	BEGIN
			TITLE = 'SMC SPEE-DEE UPLOAD REGISTER'
			CALL OPEN_SMC
			END
	('SP016995'),	BEGIN
			TITLE = 'ROC SPEE-DEE UPLOAD REGISTER'
			CALL OPEN_ROC
			END
	('1Z560570'),	BEGIN
			TITLE = 'SMC UPS UPLOAD REGISTER'
			CALL OPEN_SMC
			END
	('1Z560570'),	BEGIN
			TITLE = 'ROC UPS UPLOAD REGISTER'
			CALL OPEN_ROC
			END
	ENDUSING

	RETURN

OPEN_SMC,
	CLOSE 4
	CLOSE 5
	OPEN (4, SI, 'smc:ORDHDR.smM')
	CHN044 = 4

	OPEN (5, SI, 'smc:SLHHDR.smM')
	CHNHST = 5

	RETURN

OPEN_ROC,
	CLOSE 4
	CLOSE 5

	OPEN (4, SI, 'roc:ORDHDR.roM')
	CHN044 = 4

	OPEN (5, SI, 'roc:SLHHDR.roM')
	CHNHST = 5

	RETURN
;------------------------------------------------------------

EOF,
	CLOSE 4
	CLOSE 5
	CLOSE 7
	IF (LPONSW.EQ.1) XCALL LPOFF (LPSW,SPLFIL,PGCNT)

	STOP 'CP:UPSMNU'

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	OPEN (7, SI, 'smc:UPSDAT.ISM')
	CHNUPS = 7
	OPNOK = 1
	RETURN
;--------------------------------------------
	END

;SHIP TO:                        ORDER SHIP TO:                          AMOUNT
;AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA ZZZZZX AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA ZZ,ZZZ.XX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7         8
