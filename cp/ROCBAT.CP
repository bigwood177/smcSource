;ROCBAT.CP	ROCKFORD
;MAKBAT.DBL
;
; 5-30-18 ssq: make cusmas isam

RECORD	CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD001B.DEF'
;;;RECORD	CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'
;
RECORD	BATFAX
	.INCLUDE 'DEF:RD147A.DEF'
;
RECORD	DIS
	II	,D6

RECORD	VARS
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	TODAY	,D8
	READ	,D1,0
	LOKCTL	,D1
	I	,D5
;
PROC
	OPEN(15,I,'TT:')
	XCALL RDAT8(TODAY)
;
	OPEN(1,SI,'ROC:ORDHDR.ROM')
	OPEN(2,O,'ROC:BATFAX.ROC')
;;;	OPEN(4,I,'ROC:CUSMAS.ROC')
;;;	OPEN(5,I,'ROC:CUSIDX.ROC')


	OPEN(4,SI,'SMC:CUSMAS.SMM')
;;;	OPEN(5,I,'SMC:CUSIDX.SMC')

;;;	READ (4,CUSMAS,1)
;;;	BSEND = ORG001
	
;
	CLEAR II
LOOP,
	INCR II
	READS (1,ORDHDR,EOF)
	IF (OLOC .NE. 'O') GOTO LOOP
	IF (OCUSCD .EQ. 'NF') GOTO LOOP		;MARK AS DONT FAX
	IF (OORDDT .NE. TODAY) GOTO LOOP
O_OK,
	XCALL ISIO (4, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO LOOP
	
;;;	XCALL SERCH (5,CUSIDX,OCUSNO,1,6,BSEND,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	GO TO (LOOP), SRCCTL
;;;	IF (IRC001 .LE. 0) GOTO LOOP
;;;	READ (4,CUSMAS,IRC001)
	IF (CUSCD .NE. 'FX') GOTO LOOP

	BFX_CUST = OCUSNO
	BFX_SHIP = OSHPTO
	BFX_DATE = OORDDT
	BFX_ORDER = OORDNO
	BFX_SLSMAN = OSLMAN
	BFX_STATUS = 0
	WRITES (2,BATFAX)
	GOTO LOOP
EOF,
	CLOSE 1
	CLOSE 2
	SORT (IN='ROC:BATFAX.ROC' ,RECORD=BATFAX, KEY = (BFX_KEY))
	XCALL FLAGS (7000000)	;suppress program termination message
	STOP
	END
	

