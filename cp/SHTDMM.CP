;  SHTDMM / COP
;	SHIP-TO DATES/PURGE
;
; 5-30-18 ssq: make cusmas isam
;
;
RECORD SHIPTO
		.INCLUDE 'DEF:RD171A.DEF'
RECORD CUSCTL
		.INCLUDE 'DEF:RD001B.DEF'
RECORD CUSMAS
		.INCLUDE 'DEF:RD001A.DEF'
;;;RECORD CUSIDX
;;;		.INCLUDE 'DEF:RD002A.DEF'
RECORD ARTCDE
		.INCLUDE 'DEF:RD169A.DEF'
RECORD ARTCTL
		.INCLUDE 'DEF:RD169B.DEF'
record	ordhdr
	.include 'def:rd044a.def'
;
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

RECORD TITLE
		,A*,	'SHIP-TO PURGE REPORT'
RECORD LEGND1
		,A17,	'RANGE: CUSTOMERS '
	LCUST1	,A6
		,A7,	' THRU  '
	LCUST2	,A6
		,A5
		,A*, 'PURGE THRU: '
	LPDAT	,A10

RECORD HDR1
		,A1
		,A4,	'CUST'
		,A3
		,A12,	'BILL-TO NAME'
		,A20
		,A7,	'SHIP-TO'
		,A2
		,A12,	'SHIP-TO NAME'
		,A20
		,A7,	'SHIP-TO'
		,A3
		,A11,	'DESCRIPTION'
RECORD HDR2
		,A2
		,A2,	'NO'
		,A4
		,A15,	'BILL-TO ADDRESS'
		,A19
		,A2,	'NO'
		,A5
		,A15,	'SHIP-TO ADDRESS'
		,A17
		,A8,	'TAX CODE'

RECORD	PRINT
	LINCNT	,D2
	LPARG   ,D1
	LPONSW	,D1
	LPSW	,D1
	MAXCNT  ,D5
	PGCNT	,D3
	PLINE	,A132
	PRTTYP  ,A1
	RECCNT  ,D5
	RECNO   ,D5
	RPTNUM  ,D3
	SAVCUS	,D6
	SPLFIL  ,A14


RECORD MSGES
	TXCDNF	,A20,  	'TAX CODE NOT ON FILE'
	MSGCTL	,D1
RECORD KEY
	XCUSNO	,D6
	SHPNO	,D4

RECORD	ATAX
		,A1,	'('
	DTAX	,A3
		,A1,	')'

RECORD	CHANNELS
	CHN001	,D2
	CHN002	,D2
	CHN169	,D2
	CHN171	,D2

record	dis
	d_file	,a14
		,a2
	ii	,d6

record	ss_vars
	i	,d6
	j	,d6
	cmpcods	,3a3, 'SMC','ROC','CAT'
	filen	,5a14,	
&	'smc:ordhdr.smm','smc:slhhdr.smm','smc:slhh15.smm','smc:slhh14.smm',
&	'smc:slhh13.smm'

;;;&	'smc:ordhdr.smm','smc:slhhdr.smm','smc:slhh13.smm','smc:slhh12.smm',
;;;&	'smc:slhh11.smm'

record	wrkfil
	wdef	,a3
		,a5
	wyr	,d2
		,a1
	wext	,a3


RECORD	VARS
	OPNOK	,D1
	TODAY	,D8
	CMPCOD	,A3
	STCUS	,D6
	ENCUS	,D6
	STDAT	,D8
	ENDAT	,D8
	A10	,a10
	xdate	,d8
	ALPHA   ,A2
	BS171   ,D5
	BSEND	,D5
	BSMID	,D5
	CKEY	,A6
	CNGCTL	,D1
	ENTRY	,A30
	INXCTL	,D1
	LOKCTL	,D1
	OPNFLG  ,D1
	PAYDEP  ,D2
	READ	,D1	,0
	STORE	,D1	,2
	DELETE	,D1	,3
	RECCNT  ,D5
	SELECT  ,D1
	SORT    ,A1
	SRCCTL	,D1
	SWITCH	,D1
	TMPCOD	,A1
	V       ,D1
	WHATNO  ,D2
	WRITE	,D1	,1
	WRKDEP  ,A2
	WRKSAV  ,D5
	YEAR	,D2

PROC
	XCALL TERID(V)
;;;	XCALL WHO(CMPCOD)	
	XCALL RDAT8(TODAY)
	YEAR = TODAY(3,4)
MENU,
	XCALL OUTPT (1,1,2,'SHIP-TO PURGE APPS',V)
	XCALL OUTPT (4,20,0,'PLEASE SELECT APPLICATION',V)
	XCALL OUTPT (5,25,0,'1. SET SHIP-TO LAST USED DATE',V)
	XCALL OUTPT (6,25,0,'2. PURGE REPORT',V)
	XCALL OUTPT (7,25,0,"3. PURGE SHIP-TO'S THRU A DATE",V)
	CTL = '08,47,01,01,#E'
	CALL INPUT
	GO TO (MENU,ENDOFF), INXCTL
	SELECT = ENTRY
	USING SELECT SELECT
	(1),	CALL SSDAT
	(2),	CALL REPORT
	(3),	CALL PURGE
	(),	GOTO MENU
	ENDUSING

	GOTO MENU


SSDAT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	set date of last order in ship-to file
;


	xcall files (5, 'SU', 171, switch)
	if (switch .eq. 9) 
		begin
		xcall mesag ("can't open files",1)
		return
		end

	for j from 1 thru 3
	   begin
	   cmpcod = cmpcods(j)

	   for i from 1 thru 5
		begin
		d_file = filen(i)
		wrkfil = d_file
		wdef = cmpcod
		if(i.ge.3) wyr = (year-i)+2
		using cmpcod select
		('ROC'),	wext = 'rom'
		('CAT'),	wext = 'cam'
		('TST'),	wext = 'tsm'
		endusing

		open (1,si,wrkfil)
		d_file = wrkfil
		call set_date
		end
	   end

	close 1
	close 5
	return

set_date,
	clear ii
	find (1, ordhdr, ^first) [err=loop]
loop,
	incr ii
	if (ii/1000*1000 .eq. ii) display (15,$scr_pos(1,50), dis)
	reads (1, ordhdr, eof)
	if (oloc .ne. 'O') goto loop
	if (oshpto.le.0 .or. oshpto.ge.9998) goto loop

	shcsno = ocusno
	shtono = oshpto
	read (5, shipto, shtkey) [err=no_shipto]
	if (shodat .ge. oorddt) goto loop

	shodat = oorddt
	write (5, shipto, shtkey)

no_shipto,
	goto loop	
eof,
	close 1
	return

;----------------------------------------------------


REPORT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CALL OPENS
	IF (.NOT. OPNOK)
		BEGIN
		XCALL MESAG ("CAN'T OPEN FILES",1)
		RETURN
		END

RE_REPORT,
	XCALL OUTPT (2,1,2,'PURGE REPORT',1)
	clear lponsw

	CALL GET_CRIT
	IF (INXCTL) 
		BEGIN
		CALL CLOSE
		RETURN
		END

	
	LCUST1 = STCUS, 'ZZZZZX'
	LCUST2 = ENCUS, 'ZZZZZX'

	XDATE(1,4) = STDAT(5,8)
	XDATE(5,8) = STDAT(1,4)
	LPDAT = XDATE, 'ZX/XX/XXXX'
	LINCNT = 60
	PGCNT =

	CLEAR SHIPTO
	SHCSNO = STCUS
	find(chn171,SHIPTO,SHTKEY) [ERR=RECORD]

RECORD,
	XCALL IOS (CHN171,SHIPTO,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO END_REPORT
	IF (SHCSNO .GT. ENCUS) GOTO END_REPORT
	IF (SHODAT .GT. STDAT) GOTO RECORD

NXTCUS,
	SAVCUS = SHCSNO
	CALL GETCUS
	CALL FMTLIN
NXTSHP,
	XCALL IOS (CHN171,SHIPTO,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO END_REPORT
	
	IF (SHCSNO.GT.ENCUS) GOTO END_REPORT
	IF (SHODAT .GT. STDAT) GOTO NXTSHP
	
	IF (SAVCUS.NE.SHCSNO) GO TO NXTCUS
	CALL FMTSHP
	GO TO NXTSHP
FMTLIN,
	PLINE(1,6) = SHCSNO , 'XXXXXX'
	PLINE(9,38) = NAME
	PLINE(42,45) = SHTONO , 'XXXX'
	PLINE(50,79) = SHTONA
	PLINE(84,86) = SHTOTC
	CALL GTTXDS
	PLINE(92,121) = ARTDSC
	CALL PRINT
	PLINE =
	PLINE(9,38) = ADD1
	PLINE(50,79) = SHTOAD(1)
	PLINE(82,85) = ' PH:'
	PLINE(87,99) = SHPHON,	'ZZZ XXX-XXXX'
	CALL PRINT
	PLINE =
	PLINE(9,30) = ADD2
	PLINE(50,79) = SHTOAD(2)
	PLINE(82,85) = 'FAX:'
	PLINE(87,99) = SHFAX,	'ZZZ XXX-XXXX'
	CALL PRINT
	PLINE =
	PLINE(9,23) = CITY
	PLINE(25,26) = STATE
	PLINE(29,38) = ZIP
	PLINE(50,79) = SHTOAD(3)

	XDATE(1,4) = SHODAT(5,8)
	XDATE(5,8) = SHODAT(1,4)
	PLINE(82,86) = 'LAST:'
	PLINE(88,97) = XDATE,	'ZX/XX/XXXX'
	CALL PRINT
	PLINE =
	CALL PRINT
	RETURN
FMTSHP,
	PLINE(42,45) = SHTONO , 'XXXX'
	PLINE(50,79) = SHTONA
	PLINE(84,86) = SHTOTC
	CALL GTTXDS
	PLINE(92,121) = ARTDSC
	CALL PRINT
	PLINE =
	PLINE(50,79) = SHTOAD(1)
	PLINE(82,85) = ' PH:'
	PLINE(87,99) = SHPHON,	'ZZZ XXX-XXXX'
	CALL PRINT
	PLINE =
	PLINE(50,79) = SHTOAD(2)
	PLINE(82,85) = 'FAX:'
	PLINE(87,99) = SHFAX,	'ZZZ XXX-XXXX'
	CALL PRINT
	PLINE =
	PLINE(50,79) = SHTOAD(3)
	XDATE(1,4) = SHODAT(5,8)
	XDATE(5,8) = SHODAT(1,4)
	PLINE(82,86) = 'LAST:'
	PLINE(88,97) = XDATE,	'ZX/XX/XXXX'
	CALL PRINT
	PLINE =
	CALL PRINT
	RETURN
END_REPORT,
	IF (LPONSW.EQ.0) XCALL MESAG ('NO RECORDS WITHIN RANGE',1)
	IF (LPONSW.EQ.0) GO TO RE_REPORT
	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	GO TO RE_REPORT	

PRINT,
	IF (LPONSW.EQ.0) GO TO PRNTON
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,HDR2,'NO HDR',LEGND1
& 		,'NO LEGEND','NO LEGEND',0,132,132,1,LPSW,RPTNUM,PRTTYP)
	RETURN
PRNTON,
	SPLFIL (5,6) = 'EZ'
	LPSW = 1
	XCALL LPON (LPSW,SPLFIL)
;;;	IF (LPSW.EQ.0) GO TO ENDOFF
	IF (LPSW.EQ.0) RETURN

	LPONSW = 1
	GO TO PRINT
;********************************************************************************
GETCUS,

	XCALL ISIO (CHN001, CUSMAS, SHCSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO NOTFND
	
;;;	SRCCTL = 3		;COP36
;;;	XCALL SERCH (CHN002,CUSIDX,SHCSNO,1,6,ORG001, BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	IF (SRCCTL.EQ.1) GO TO NOTFND
;;;	LOKCTL = 1
;;;	XCALL IO (CHN001,CUSMAS,IRC001,READ,LOKCTL)
;;;	GO TO FOUND
NOTFND,
	NAME = '**** CUSTOMER NOT FOUND ***'
	ADD1 =
	ADD2 =
	CITY =
	STATE =
	ZIP =
FOUND,	
	RETURN
;********************************************************************************
GTTXDS,
	IF (SHTOTC.EQ.'   ') ARTDSC =
	IF (SHTOTC.EQ.'   ') RETURN
	SRCCTL = 4
	XCALL SERCH (CHN169,ARTCDE,SHTOTC,1,3,ORG169,BSMID,SRCCTL,4,4,9,0,0,0,0)
	IF (SRCCTL.EQ.1) ARTDSC = ' ***** TAX CODE NOT FOUND **** '
	RETURN
;********************************************************************************


;----------------------------------------------------

PURGE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SWITCH = 5
	XCALL FILES (4,'SU',171, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN171 = 4


	XCALL OUTPT (2,1,2,"PURGE SHIP-TO'S THRU A DATE",1)
	CALL GET_CRIT
	IF (INXCTL) RETURN

	CLEAR SHIPTO
	SHCSNO = STCUS
	find(chn171,SHIPTO,SHTKEY) [ERR=P_LOOP]

P_LOOP,
	XCALL IOS (CHN171,SHIPTO,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO END_PURGE
	IF (SHCSNO .GT. ENCUS) GOTO END_PURGE
	IF (SHODAT .GT. STDAT) GOTO P_LOOP

	XCALL ISIO (CHN171, SHIPTO, SHTKEY, DELETE, LOKCTL)
	GOTO P_LOOP

END_PURGE,
	XCALL MESAG ("OLD SHIP-TO'S PURGED FROM FILE",1)
	CLOSE CHN171

	RETURN
;----------------------------------------------------

GET_CRIT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR CNGCTL
;;;	XCALL OUTPT (1,1,2,'SHIP-TO PURGE APPS',V)
	XCALL OUTPT (4,4,0,'1. START CUST',1)
	XCALL OUTPT (5,4,0,'2. END   CUST',1)
	XCALL OUTPT (7,4,0,'3. PURGE THRU DATE',1)
STCUS,
	XCALL INPUT (4,24,06,00,'#E',ENTRY,INXCTL,1)
	GOTO (GET_CRIT,GC_END),INXCTL
	STCUS = ENTRY(1,6)
	IF (STCUS .EQ. 0)
		BEGIN
		ENCUS = 999999
		XCALL OUTPT (4,24,1,'ALL',1)
		XCALL OUTPT (5,24,1,' ',1)
		GOTO (GC_ANY),INXCTL
		GOTO STDAT
		END
	GOTO (GC_ANY),INXCTL
ENCUS,
	XCALL INPUT (5,24,06,00,'#E',ENTRY,INXCTL,1)
	GOTO (GET_CRIT,GC_END),INXCTL
	ENCUS = ENTRY(1,6)
	IF (ENCUS .EQ. 0)
		BEGIN
		ENCUS = STCUS
		ENTRY(1,6) = ENCUS, 'ZZZZZX' [LEFT]
		XCALL OUTPT (5,24,1,ENTRY(1,6),1)
		END
	GOTO (GC_ANY),INXCTL
STDAT,
	XCALL INPUT (7,24,08,00,'D ',ENTRY,INXCTL,1)
	GOTO (GET_CRIT),INXCTL
	STDAT = ENTRY(1,8)	
	GOTO (GC_ANY),INXCTL

GC_ANY,
	XCALL ANYCN (CNGCTL, WHATNO)
	GOTO (GC_END, GC_CNG),CNGCTL+1
GC_CNG,
	GOTO (STCUS, ENCUS, STDAT),WHATNO
	GOTO GC_ANY
GC_END,
	RETURN
;----------------------------------------------------

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:SHTMNU',1)


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK
	
	SWITCH = 1
	XCALL FILES (1,'SI', 001, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN001 = 1

;;;	SWITCH = 1
;;;	XCALL FILES (2,'I',002, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN002 = 2

	SWITCH = 1
	XCALL FILES (3,'I',169,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN169 = 3

	SWITCH = 5
	XCALL FILES (4,'SI',171, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN171 = 4


;;;	LOKCTL = 1
;;;	XCALL IO (CHN001,CUSCTL,1,READ,LOKCTL)
	LOKCTL = 1
	XCALL IO (CHN169,ARTCTL,1,READ,LOKCTL)

	OPNOK = 1
	RETURN
;-----------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN001) XCALL FILES (CHN001, 'SI', 001, 4)
;;;	IF (CHN002) XCALL FILES (CHN002, 'I', 002, 4)
	IF (CHN169) XCALL FILES (CHN169, 'I', 169, 4)
	IF (CHN171) CLOSE CHN171

	RETURN
;-----------------------------------------------------------

;**********************************************************************
INPUT,
	XCALL INPUT(ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	RETURN
;********************************************************************************
END
