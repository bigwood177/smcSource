;MAKITM.PO
;
RECORD	ITMALP
	.INCLUDE 'DEF:RD167A.DEF'

RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.DEF'

RECORD	DIS
	I	,D6
		,a2
	J	,d5

RECORD	VARS
	BLANKS	,A20
	XNAME	,A25
	X	,D2
	Y	,D2
	A1	,A1
	STAT	,D1
	V	,D1

PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'POPULATE "SMC:ITMALP.SMM"',1)
	OPEN (1,I,'SMC:ITMMAS.SMC')

	OPEN (2,SU,'SMC:ITMALP.SMM')
	xcall outpt (2,1,1,'clear file',1)
cl_loop,
	READS (2,ITMALP,EOF_CL) 
	DELETE (2,A_ITEMNO)
	GOTO CL_LOOP
EOF_CL,
	xcall outpt (2,1,1,'rebuild index',1)

	READS (1,ITMMAS,EOF)
	I = 1
LOOP,
	INCR I
	IF (I/100*100 .EQ. I) XCALL OUTPT (1,60,1,DIS,1)
	READS (1,ITMMAS,EOF)
	IF (ITMMAS .EQ. ']]]]]]') GOTO EOF
	IF (DESCR .EQ. ']]]DEL') GOTO LOOP

	read (2,ITMALP, ITEMNO) [err=mak_record]
	if (A_ITEMNO .EQ. ITEMNO) goto loop

mak_record,
	incr j
	A_ITEMNO = ITEMNO
	A_DESCR = DESCR
	STORE (2,ITMALP,A_ITEMNO) ;[err=loop]
	GOTO LOOP
EOF,
	CLOSE 1
	CLOSE 2
	STOP
	END



