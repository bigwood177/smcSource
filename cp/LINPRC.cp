;DUCPRC.COP
;
;		AUTHOR: SHERWOOD S. QUIRING
;		DATE  : 18-DEC-1996
;
RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	PARAMS
	.INCLUDE 'DEF:PARAM.DEF'

RECORD	LPKEY
	K_CODE	,A2
	K_KEY	,D1
	K_FILL	,A14

RECORD	CHANNEL
	CHN182	,D2

RECORD	DLINE
		,A6
	DND	,D1
		,A2,	'. '
	DD	,A15
		,A4
	DNP	,A2
		,A2,	'. '
	DP	,A10

RECORD	D_ARRAY
	D_LINER	,9A15
	D_PRICE	,9D8

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


RECORD	VARS
	I	,D5
	J	,D5
	SELECT	,D1
	OPNOK	,D1
	ENTRY	,A30
	INXCTL	,D1
	WHATNO	,D2
	CNGCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	LOKCTL	,D1
	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID (V)
	V = 1

	XCALL OUTPT (1,1,2,'LINER PRICING TABLE',1)
	CALL OPENS
	IF (.NOT. OPNOK)
		BEGIN
		CALL CLOSE
		XCALL PGCHN ('CP:CPMENU',1)
		END


DISPLA,
	CLEAR CNGCTL
	ROW = 4

	FOR I FROM 1 THRU 9
		BEGIN
		CLEAR TBL_KEY
		TBLCOD = 'LP'
		LP_KEY = I
		XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
		IF (LOKCTL .NE. 0) CLEAR COPTBL

		DND = I
		DD = LP_LINER

		DNP = (I + 9),	'XX'
		DP = LP_PRICE,	'ZZ,ZZX.XXX' [LEFT]
		XCALL OUTPT (ROW+I, 1, 0, DLINE, 1)
		D_LINER(I) = LP_LINER
		D_PRICE(I) = LP_PRICE
		END


ANYCNG,
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (PROCES,CNGBR),CNGCTL + 1
CNGBR,
	USING WHATNO SELECT
	(1 THRU 9),	BEGIN
			I = WHATNO
			GOTO LP_LINER
			END
	(10 THRU 18),	BEGIN
			I = WHATNO - 9
			GOTO LP_PRICE
			END
	ENDUSING

	GOTO ANYCNG

LP_LINER,
	if (whatno .eq. 4) goto anycng		;hard coded to be NONE in scrn3
	CTL = '06,10,12,00,A '
	ROW = WHATNO + 4
	CALL INPUT
	GOTO (DISPLA),INXCTL
	D_LINER(I) = ENTRY(1,12)
	GOTO ANYCNG

LP_PRICE,
	if (whatno .eq. 13) goto anycng		;none
	CTL = '06,33,08,00,# '
	ROW = WHATNO-9 + 4

	CALL INPUT
	GOTO (DISPLA),INXCTL
	D_PRICE(I) = ENTRY(1,8)
	ENTRY(1,10) = D_PRICE(I),	'ZZ,ZZX.XXX' [LEFT]
	XCALL OUTPT (ROW,33,0,ENTRY(1,10),1)
	GOTO ANYCNG

PROCES,

	FOR I FROM 1 THRU 9
		BEGIN
		CLEAR TBL_KEY
		TBLCOD = 'LP'
		LP_KEY = I
		XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
		IF (LOKCTL .NE. 0) nextloop
		LP_PRICE = D_PRICE(I)
		LP_LINER = D_LINER(I)
		XCALL ISIO (CHN182, COPTBL, TBL_KEY, WRITE, LOKCTL)
		END



ENDOFF,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CALL CLOSE
	XCALL PGCHN ('CP:TBLMNU',1)
;------------------------------------

INPUT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL INPUT(ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	RETURN
;--------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1,'SU',182,SWITCH)		;182 - COPTBL
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 1
	OPNOK = 1
	RETURN
;------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN182) CLOSE CHN182
	RETURN
;------------------------------------

