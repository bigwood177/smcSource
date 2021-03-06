;DPRICE.CP
; ENTER PRICING ARRAY FOR DAMPERS
;
;
RECORD TPRICE
	ARRAY	,80D5

RECORD HDR1
		,A40,'SZ"    PRICE        SZ"    PRICE        '
		,A40,'SZ"    PRICE        SZ"    PRICE        '
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
RECORD
	I	,D2
		,A1,'"'
RECORD	,X
	AI	,A3

RECORD	FILPRC
	FL_DEV	,A3
		,A1,	':'
	FL_NAME	,A6
		,A1,	'.'
	FL_EXT	,A3

RECORD	VARS
	OPT	,D1
	DTYPE	,D2
	INXCTL	,D2
	ENTRY	,A6
	CNGCTL	,D2
	WHATNO	,D2
	READ	,D1,0
	WRITE	,D1,1
	LOKCTL	,D1
	DLMASK	,A6,'ZZZ.XX'
	V	,D1
	ENTALL	,D1
	LINCNT	,D2
	PGCNT	,D3
	TITLE	,A20,	'PRICING ARRAY REPORT'
	PLINE	,A80
	RPTNUM	,D3
	PRTTYP	,A1
	SPLFIL	,A14
	LPARG	,D1
	LPSW	,D1
	SWITCH	,D1
;
PROC
	XCALL TERID (V)

	XCALL FFILE(1,FILPRC,SWITCH)	;SSQ 01-13-04
	FL_NAME = 'SDPRIC'
	OPEN (1,U,FILPRC)		;SSQ 01-13-04
;;;	OPEN (1,U,'SMC:SDPRIC.SMC')

MENU,
	XCALL OUTPT (1,1,2,'PRICING MATRIX FOR VOLUME DAMPERS',1)
	XCALL OUTPT (10,10,0,'ENTER OPTION:',1)
	XCALL OUTPT (12,10,0,'1. MAINTAIN PRICES',1)
	XCALL OUTPT (14,10,0,'2. PRINT PRICES',1)
	XCALL INPUT (10,24,01,00,'#E',ENTRY,INXCTL,1)
	GOTO (MENU,ENDOFF),INXCTL
	OPT = ENTRY(1,1)
	USING OPT SELECT
	(1),	GOTO DISPLA
	(2),	GOTO PRTFIL
	(),	GOTO MENU
	ENDUSING

DISPLA,
	XCALL OUTPT (1,1,2,'PRICING MATRIX FOR VOLUME DAMPERS',1)
	XCALL OUTPT (1,40,1,'\',1)
	LOKCTL = 1
	DTYPE = 1
	XCALL IO (1,TPRICE,DTYPE,READ,LOKCTL)
	INXCTL =
	CNGCTL =

	XCALL OUTPT (3,1,0,HDR1,1)

	FOR I FROM 1 THRU 80
	BEGIN
	  CALL SETCUP
	  XCALL OUTPT (ROW,COL,1,AI,1)
	  XCALL OUTPT (0,0,0,'   ',1)
	  ENTRY(1,6) = ARRAY(I),DLMASK
	  XCALL OUTPT (0,0,0,ENTRY(1,6),1)
	END
	I =
	GOTO ANYCNG
INPLP,
	INCR I
	IF (I.GT.80) GOTO ANYCNG
GETPRC,
	CTL = '00,00,05,00,$X'
	CALL SETCUP
	COL = COL + 6
	CALL INPUT
	GOTO (DISPLA,ANYCNG,CLRALL), INXCTL
	ARRAY (I) = ENTRY
	ENTRY(1,6) = ARRAY(I),DLMASK
	XCALL OUTPT (ROW,COL,0,ENTRY(1,6),1)
	IF (ENTALL) GOTO INPLP
	GOTO ANYCNG
ANYCNG,
	ENTALL =
	XCALL ANYCN (CNGCTL,WHATNO)
	IF (CNGCTL.EQ.1)
	BEGIN
	  IF (WHATNO.GT.80) GOTO ANYCNG
	  I = WHATNO
	  GOTO GETPRC
	END
	LOKCTL = 1
	XCALL IO (1,TPRICE,DTYPE,WRITE,LOKCTL)
	GOTO MENU

ENDOFF,
	CLOSE 1
	XCALL PGCHN ('CP:SPCFUN',1)

CLRALL,
	ENTALL =
	XCALL OUTPT (24,1,1,'CLEAR AND REENTER ALL PRICES IN THIS CATEGORY <N> ? ',1)
	CTL = '24,53,01,00,YN'
	CALL INPUT
	IF (INXCTL.NE.1) GOTO ANYCNG
	FOR I FROM 1 THRU 80
	BEGIN
	  CALL SETCUP
	  ARRAY(I) =
	  ENTRY(1,6) = ARRAY(I),DLMASK
	  XCALL OUTPT (ROW,COL,1,AI,1)
	  XCALL OUTPT (0,0,0,'   ',1)
	  XCALL OUTPT (0,0,0,ENTRY(1,6),1)
	END
	I=
	ENTALL = 1
	XCALL OUTPT (24,1,1,'\',1)
	GOTO INPLP
SETCUP,
	IF (I.GE.1.AND.I.LE.20)
	BEGIN
	  ROW = 4+((I-1)*1)
	  COL = 01
	END
	IF (I.GE.21.AND.I.LE.40)
	BEGIN
	  ROW = 4+((I-20-1)*1)
	  COL = 21
	END
	IF (I.GE.41.AND.I.LE.60)
	BEGIN
	  ROW = 4+((I-40-1)*1)
	  COL = 41
	END
	IF (I.GE.61.AND.I.LE.80)
	BEGIN
	  ROW = 4+((I-60-1)*1)
	  COL = 61
	END
	RETURN
INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	RETURN
PRTFIL,
	DTYPE = 1
	LPSW = 1
	SPLFIL (5,6) = 
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GO TO ENDPFI
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	LINCNT = 62
	PGCNT =
	XCALL IO (1,TPRICE,DTYPE,READ,LOKCTL)
	IF (LINCNT+23.GT.60) LINCNT = 62
	PLINE (1,30) = 'DAMPER PRICING'
	CALL PRINT
	FOR I FROM 1 THRU 20
	  BEGIN
	    PLINE ( 1, 3) = AI
	    PLINE ( 7,12) = ARRAY(I), DLMASK
	    I = I + 20
	    PLINE (21,23) = AI
	    PLINE (27,32) = ARRAY(I), DLMASK
	    I = I + 20
	    PLINE (41,43) = AI
	    PLINE (47,52) = ARRAY(I), DLMASK
	    I = I + 20
	    PLINE (61,63) = AI
	    PLINE (67,72) = ARRAY(I), DLMASK
	    I = I - 60
	    CALL PRINT
	  END
	XCALL LINFD (2)
	LINCNT = LINCNT + 2
	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
ENDPFI,
	GOTO DISPLA

PRINT,
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,' ',' ',
&		'NO LEG',' ',' ',0,80,80,0,LPSW,RPTNUM,PRTTYP)
	RETURN
END


