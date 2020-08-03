;emcprg.cp
;	purge emailc.ism emails thru a date
;

RECORD	TMPREC
	T_CUS	,D6
	T_NBR	,D3
RECORD,X
	T_KEY	,D9

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'
;
RECORD	EMAILC
	.INCLUDE 'DEF:RD084A.DEF'

RECORD	DIS
	II	,D6

RECORD	VARS
	OPNOK	,D1
	O_FILES	,4A14,	'SMC:ORDHDR.SMM','SMC:SLHHDR.SMM','SMC:SLHHXX.SMM','SMC:SLHHXX.SMM'
	FILEN	,A14
	WRKFIL	,A14,	'SPL:EMCPRG.ISM'
	I	,D6
	J	,D6
	XDATE	,D8
	YR	,D2
	ENTRY	,A30
	INXCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	LOKCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'PURGE CUSTOMER CONTACT EMAILS OVER TWO YEARS OLD',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'PURGE CUSTOMER CONTACT EMAILS OVER TWO YEARS OLD',1)
	XCALL OUTPT (4,4,1,'PURGE THRU DATE                (01/01/20XX)',1)
	XCALL INPUT (4,23,08,00,'DE',ENTRY,INXCTL,1)
	GOTO (DISPLA,ENDOFF),INXCTL
	XDATE = ENTRY(1,8)
	YR = ENTRY(3,4)
	
	XCALL ANYCN (CNGCTL, WHATNO)
	GOTO (DISPLA),CNGCTL


;as ever, depends on if this is running just before year end or just after...
	
	FILEN = O_FILES(3)
	FILEN(9,10) = '19'
	O_FILES(3) = FILEN

	FILEN = O_FILES(4)
	FILEN(9,10) = '18'
	O_FILES(4) = FILEN

	FOR I FROM 1 THRU 4
		BEGIN
		CALL PROCESS_FILE
		END

	CLOSE 1
	CALL PRG_EMAILC

ENDOFF,
	STOP


PROCESS_FILE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FILEN = O_FILES(I)
	OPEN (2,SI, FILEN)
	XCALL OUTPT (I,59,1,FILEN,1)
	CLEAR II
	FIND (2, ORDHDR, ^FIRST) [ERR=LOOP]
LOOP,
	INCR II
	IF (II/1000*1000 .EQ. II) DISPLAY (15, $SCR_POS(I,74), DIS)
	READS (2, ORDHDR, EOF)
	IF (OC_NBR .LE. 0) GOTO LOOP		;NO EMAIL


	CLEAR TMPREC

	T_CUS = OCUSNO
	T_NBR= OC_NBR

	STORE (1, TMPREC, T_KEY) [ERR=LOOP]
	GOTO LOOP
EOF,
	CLOSE 2

	RETURN
;---------------------------------------------------

PRG_EMAILC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR II
	DISPLAY (15, $SCR_POS(6,59), 'SMC:EMAILC.SMM')

	OPEN (1,SI,WRKFIL)		;SPL:EMCPRG.ISM
	OPEN (2,SU,'SMC:EMAILC.SMM')	;EMAIL FILE
	OPEN (3,O,'EMCPRG.DAT')

	FIND (2, EMAILC, ^FIRST) [ERR=PE_LOOP]
PE_LOOP,
	INCR II
	IF (II/100*100 .EQ. II) DISPLAY (15, $SCR_POS(6,74), DIS)
	XCALL IOS (2, EMAILC, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO PE_EOF
	
	CLEAR TMPREC
	T_CUS = E_CUST
	T_NBR = E_NBR
	FIND (1, TMPREC, T_KEY) [ERR=PURGE]	;PURGE IF NOT FOUND
	GOTO PE_LOOP				;ELSE SKIP
PURGE,
	XCALL ISIO (2, EMAILC, E_KEY, DELETE, LOKCTL)
	WRITES (3, EMAILC)

	GOTO PE_LOOP
PE_EOF,
	CLOSE 1
	CLOSE 2
	CLOSE 3

	RETURN
;---------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK

	XCALL ISAMC (WRKFIL,9,1,'START=1,LENGTH=9,NAME=MAILK,NODUPS,ASCEND')
	OPEN (1,SU,WRKFIL)
	
	OPNOK = 1
	RETURN
;-------------------------------------------------

CLOSE,
	CLOSE 1
	RETURN
;-------------------------------------------------

END

