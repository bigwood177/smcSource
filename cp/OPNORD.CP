;OPNORD.CP	;isam
;
;		PGM TO PRINT ORDER EDIT LIST
;
;
;
SUBROUTINE	OPNORD	;converted to subroutine 6-16-14
	NITE	,D	;1=NITE BATCH, 0=FROM MENU

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR

record	oocmnt
	ooc_ord	,d6
	ooc_not	,a25


RECORD ORDHDR   	
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	BLHEAD
	.INCLUDE 'DEF:RD178A.DEF'

RECORD	DPTSTS
	.INCLUDE 'DEF:RD193A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD TITLE
		,A*,	'OPEN ORDER REPORT'
		,a1
	t_user	,a10


RECORD	HD1
		,A*,	'OPEN ORDERS AS OF '
	H1_SDATE	,A10

RECORD	HD2
		,A*,	'ORDER# SLM ----------- CUSTOMER ----------- '
		,A*,	'SHIP VIA           -------- OPEN DEPTS -------- '
		,A*,	'PAST DUE'


RECORD PBUF
	PLINE	,A132

record	arays
	c1	,11d2,	61,64,67,70,73,76,79,82,85,88,91
	c2	,11d2,	62,65,68,71,74,77,80,83,86,89,92

RECORD	WRKLIN
	W_DATE	,D8
	W_ORDER	,D6
	W_SREP	,A3
	W_SC_NAME	,A4
	W_PLINE	,A132


RECORD	CHANNEL
	CHN044	,D2
	CHN182	,D2
	CHN178	,D2
	chn193	,d2


RECORD	VARS
	dow	,d1		;day of week = clear opnord.smm on sunday.
	S_KEY	,A4		;sort key
	SORT_BY	,D1		;1=ship-via, 2=saleman
	SAVE_SC_NAME	,A4
	SAVE_REP	,A3
;;;	wrkfil	,a14,	'spl:opnord.wrk'
	wrkfil	,a14,	'smc:opnord.wrk'
	txtfil	,a14,	'spl:opnord.txt'
	i	,d6
	done	,d1
	days	,d4
	wk1	,d6
	wk2	,d6
	LG1	,A9,	'NO LEGEND'
	HDR	,A6,	'NO HDR'
	OPNOK	,D1
	TODAY	,D8
	TDATE	,D6
	TODAA	,A10
	WHATNO	,D2
	CNGCTL	,D1
	S_VIA	,A1
	X_SCAC	,A4
	SCAC	,A4
	ADCNT2	,D4
	SPLFIL	,A14
	LPARG	,D1
	LOKCTL	,D1
	LINCNT	,D2
	PGCNT	,D6
;-
	tbl_scac	,a4
	MAN	,D2	;SALESMAN NUMBER
	LNAM	,A25
	SNAM	,A12
	SINT	,A3
;-
	PAGENO	,D2
	PRTCTL	,D2
	ENTRY	,A30
	INXCTL	,D1
	STDAT	,D8
	ENDAT	,D8
	XDATE	,D8
	BLANKS	,A25
	RPTNUM	,D3
	PRTTYP	,A1
	SWITCH	,D1
	LPSW	,D2
	V	,D1
	READ	,D1	,0
	WRITE	,D1	,1
	PRNTSW	,D1

PROC
	XCALL TERID (V)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	XCALL RDATE(TDATE)
	XCALL DATE8(TDATE, D_OUT, TODAY, TODAA, D_SW)

	t_user = %syn_uname		;3-28-16 to debug

BEGIN,

	SORT_BY = 1		;DEFAULT TO SHIP-VIA
DISPLA,
	IF (NITE) GOTO SKIP_DISPLA

	CNGCTL =
	XCALL OUTPT (1,1,2,'ORDER ENTRY & EDITING',V)
	XCALL OUTPT (2,1,0,'OPEN ORDER REPORT',1)

	XCALL OUTPT (4,4,1,'SORT BY:',1)
	XCALL OUTPT (5,4,0,'1. SHIP-VIA',1)
	XCALL OUTPT (6,4,0,'2. SALES REP',1)

	XCALL INPUT (4,15,01,01,'#E',ENTRY,INXCTL,1)
	GOTO (displa,ENDOFF),INXCTL
	SORT_BY = ENTRY(1,1)
	USING SORT_BY SELECT
	(1,2),	NOP
	(),	GOTO DISPLA
	ENDUSING

	CNGCTL = 2
	XCALL ANYCN (CNGCTL,WHATNO)
	GOTO (DISPLA),CNGCTL

SKIP_DISPLA,
	H1_SDATE = TODAA

RDHDR,
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GO TO ENDHDR
	IF (OLOC.NE."O") GOTO RDHDR
	IF (OFLAG .GT. 0) GOTO RDHDR	;SELECTED
	IF (OSHDTE .GT. 0) GOTO RDHDR	;SHIPPED	
	
	LOKCTL = 1
	XCALL ISIO (CHN178, BLHEAD, OORDNO, READ, LOKCTL)
	if (lokctl .eq. 0)
		begin
		IF (BHSHPD .GT. 0) GOTO rdhdr	;ALREADY SHIPPED
		end
	
	
	if (opromd .le. 0) opromd = oorddt
	xcall bdat8 (today, opromd, wk1, wk2, days)

	CALL GET_SCAC

	XCALL SREP(OSLMAN,LNAM,SNAM,SINT)

	read (2, oocmnt, oordno) [err=no_note]
	goto note_ok
no_note,
	clear oocmnt
note_ok,

	PLINE (1,6) = OORDNO,	'ZZZZZX'
	PLINE (8,10) = SINT	
	PLINE (12,17) = OCUSNO, 'ZZZZZX'
	PLINE (19,43) = OCUSNM
	PLINE (45,59) = SC_NAME

	CALL OPN_OPS

	PLINE (94,96) = DAYS, 'ZZZX'
;;;	PLINE (101,104) = DAYS, 'ZZZX'

;;;	PLINE (106,132) = OOC_NOT
	PLINE (98,132) = ocomnt(1)		;5-13-19
	CALL WRITE_WORK

	GOTO RDHDR

OPN_OPS,
	done = 1
	i = 0
	find (chn193, dptsts, oordno) [err=oo_loop]
oo_loop,
	reads (chn193, dptsts, oo_eof)
	if (s_ordn .ne. oordno) goto oo_eof
	if (s_dept .eq. '**') goto oo_loop	;6-21-17 header
	if (s_stat .ge. 3) goto oo_loop		;complete
	clear done
	incr i
	pline (c1(i), c2(i)) = s_dept
	if (i .lt. 10) goto oo_loop
;;;	goto oo_loop
	
oo_eof,
	if (done) pline (66,76) = 'DONE'
	return




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WRITE_WORK,
	CLEAR WRKLIN

	using oscac select
	('2 ','5 ','X033'),	oscac = 'A'
	('0 ', '4 '),		oscac = 'B'
	('1 ', '10 ', 'WC'),	oscac = 'C'
	(),			oscac = 'D'
	endusing

	W_DATE = OPROMD
	W_ORDER = OORDNO
	W_SC_NAME = oscac

	W_SREP = SINT
	W_PLINE = PLINE
	WRITES (10, WRKLIN)

	CLEAR PLINE
	RETURN

PRINT,
	IF (PRNTSW.NE.1) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD2,HDR,LG1,
&		LG1,'NO LEGEND',0,0,132,0,LPSW,RPTNUM,PRTTYP)

	RETURN
;*************************************************************************
PRNTON,
	SPLFIL (5,6) = 'EA'
	LPSW = 9	; local compressed

	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GO TO ENDOFF
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
;;;	XCALL WATE (LPARG,V)
	lincnt = 66
	pgcnt = 0
	PRNTSW = 1
	RETURN
LPOFF,
; 4-2-18: decided not to do this
;;;	IF (NITE) 
;;;		begin
;;;		close 14
;;;		xcall renam (1, splfil, txtfil)
;;;		goto lp_done
;;;		end

	if (nite)
	then	XCALL LPOFF (LPSW,SPLFIL,PGCNT,'SMC_BLUE')
	else	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
lp_done,
	PGCNT =
	LINCNT = 60
	PRNTSW =
	RETURN
;*************************************************************************
ENDHDR,
	CLOSE 10
	USING SORT_BY SELECT
	(1),	SORT (IN=WRKFIL, RECORD=WRKLIN,	KEY=(W_SC_NAME,W_DATE,W_ORDER) )
	(2),	SORT (IN=WRKFIL, RECORD=WRKLIN,	KEY=(W_SREP,W_DATE,W_ORDER) )
	ENDUSING

	OPEN (10,I,WRKFIL)
	CLEAR SAVE_SC_NAME
EH_LOOP,
	READS (10, WRKlin, EH_EOF)
	using sort_by select
	(1),	IF (W_SC_NAME .NE. SAVE_SC_NAME) LINCNT = 66
	(2),	IF (W_SREP .NE. SAVE_REP) LINCNT = 66
	endusing

	PLINE = W_PLINE
	CALL PRINT
	SAVE_SC_NAME = W_SC_NAME
	SAVE_REP = W_SREP

	GOTO EH_LOOP
EH_EOF,
	CLOSE 10

GETOUT,
	IF (PRNTSW) CALL LPOFF

ENDOFF,
	CALL CLOSE
;;;	XCALL PGCHN ('CP:coprpt',1)

	dow = %wkday
	if (dow .eq. 1) 
		begin
		xcall isclr(2, 'smc:opnord.smm', lokctl)
		end

	xcall flags (7000000)
	stop

PRNTOF,
	LINCNT = 66
	IF (PRNTSW.EQ.0) XCALL MESAG ('NO RECORDS FIT THESE PARAMETERS',1)
	IF (PRNTSW.EQ.1) CALL LPOFF
	PRNTSW =
	PGCNT = 0
	GOTO BEGIN

GET_SCAC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; COP TABLE SCAC LOOK-UP
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'SC'
	TBLKEY = oscac
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		SC_NAME = "* NOT ON FILE *"
		END
	RETURN
;-----------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK

	open (2,si,'smc:opnord.smm')	;notes file

	SWITCH = 5

	XCALL FILES (4,'SI',44,SWITCH)		;FILE # 44 -- ORDHDR FILE
	IF (SWITCH.EQ.9) RETURN
	CHN044 = 4


	SWITCH = 5
	XCALL FILES (7, 'SI', 178, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN178 = 7

	XCALL FILES (9,'SI',182,SWITCH)		;COPTBL
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 9

	SWITCH = 5
	XCALL FILES (8, 'SI', 193, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN193 = 8

	open (10,o,wrkfil)

	OPNOK = 1

	RETURN
;-----------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	IF (CHN044) CLOSE CHN044
	IF (CHN182) CLOSE CHN182
	IF (CHN178) CLOSE CHN178
	if (chn193) close chn193
	close 2
	RETURN
;-----------------------------------------------------

END

;123456 AAA XXXXXX AAAAAAAAAAAAAAAAAAAAAAAAA SHIP VIA AA AA AA AA AA AA AA AA AA AA AA    ZZX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345
;         1         2         3         4         5         6         7         8          9
