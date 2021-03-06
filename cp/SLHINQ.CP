; SLHINQ.COP
;
;	SALES HISTORY INQUIRY PROGRAM
;
; 6-04-18 ssq: make cusmas isam
;
RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR



RECORD SLSHST
		.INCLUDE 'DEF:RD055A.DEF'
RECORD SLHCTL	,X
		.INCLUDE 'DEF:RD055B.DEF'
RECORD CUSMAS
		.INCLUDE 'DEF:RD001A.DEF'
RECORD CUSCTL	,X
		.INCLUDE 'DEF:RD001B.DEF'
;;;RECORD CUSIDX
;;;		.INCLUDE 'DEF:RD002A.DEF'
RECORD ITMMAS
		.INCLUDE 'DEF:RD041A.DEF'
RECORD ITMCTL	,X
		.INCLUDE 'DEF:RD041B.DEF'
RECORD ITMIDX
		.INCLUDE 'DEF:RD042A.DEF'
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
	
RECORD	SH_FILE
		,A11	;'SMC:SLSHST.'
	SH_Y	,A1
	SH_EXT	,A2

RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN041	,D2
	CHN042	,D2
	CHN055	,D2


RECORD	PRINT
	TITLE	,A*,	'CATEGORIES/DEPARTMENTS'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PLINE	,A132
	PRNTON	,D1
	LINCNT	,D2,60
	PGCNT	,D6
	LPSW	,D2
	SPLFIL	,A14
	RPTNUM	,D3
	PRTTYP	,A1
	PRTCTL	,D3,080
	LPARG	,D1
	PRNTSW	,D1
	PRTCTR	,D1

RECORD	HD2
	,a*, '--------------- ITEM -------------------'
	,a*, '------    LAST    ---- ACCUMULATED ---- '

RECORD	HD3
	,A*, 'NUMBER          DESCRIPTION             '
	,A*, '         SL DATE      QTY       SL $    '

RECORD	HD1
	,A*, 'FOR CUSTOMER '
	H3_CUST	,A6
	,A1
	H3_NAME	,A30
		
RECORD	VARS
	CHNSPL	,D2
	PRINTING	,D1	;1 = PRINT INSTEAD OF DISPLAYING
	YEAR	,D2
	BADYEAR	,D1
	OPNOK	,D1
	V	,D1
	SWITCH	,D1
	ITMKEY	,A15
	CATKEY	,A2
	CUSKEY	,D6
	ORG41	,D5
	ORG01	,D5
	ORG55	,D5
	RECNO	,D5
	INXCTL	,D1
	ENTRY	,A15
	I	,D2
	LOKCTL 	,D1
	READ	,D1,0
	SRCCTL	,D1
	BSMID	,D5
	KEY	,A21
	BLANKS	,A15	
	ALPHA	,A15
	DTMASK	,A8,	'XX/XX/XX'
	NUMASK	,A8,	'ZZZ,ZZX-'
	DLMASK	,A12,	'ZZZZ,ZZZ.XX-'
	HSTOT	,D9
	LSTCUS	,D6
;
PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'SALES HISTORY INQUIRY',1)
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

DISPLA,
;;;	CLEAR CHNSPL
	clear prnton

	XCALL OUTPT (1,1,2,'SALES HISTORY INQUIRY',1)
	XCALL OUTPT (2,10,0,'YEAR    :             <CR> = CURRENT YEAR',1)
	XCALL OUTPT (3,10,0,'CUSTOMER: ',1)
	XCALL OUTPT (4,10,0,'CATEGORY: ',1)
	XCALL OUTPT (5,10,0,'ITEM NO.: ',1)
	XCALL OUTPT (5,40,0,'(PARTIAL ITEM NUMBERS ALLOWED)',1)
	XCALL OUTPT (6,1,0,'--------------- ITEM -------------------',1)
	XCALL OUTPT (0,0,0,'------    LAST    ---- ACCUMULATED ---- ',1)
	XCALL OUTPT (7,1,0,'NUMBER          DESCRIPTION             ',1)
	XCALL OUTPT (0,0,0,'         SL DATE      QTY       SL $    ',1)
;


ASK0,
	CTL = '02,20,02,00,#E'
	CALL INPUT
	GOTO (DISPLA,ENDOFF),INXCTL
	YEAR = ENTRY(1,2)
;;;	IF (YEAR .EQ. 0) GOTO ASK1
	CALL YEAR_SLSHST
	IF (BADYEAR) GOTO DISPLA
ASK1,
	CTL = '03,20,06,00,#E'
	CALL INPUT
	GOTO (DISPLA,ENDOFF), INXCTL
	CUSKEY = ENTRY
	IF (CUSKEY.EQ.0)
	BEGIN
	  XCALL OUTPT (ROW,COL,0,'ALL',1)
	  GOTO ASK2
	END
	CALL FNDCUS
	XCALL OUTPT (ROW,40,0,NAME,1)
	h3_cust = cuskey, 'ZZZZZX'
	h3_name = name
ASK2,
	CTL = '04,20,02,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	CATKEY = ENTRY
	IF (CATKEY.NE.BLANKS) 
	BEGIN
	  ITMKEY =
	  XCALL OUTPT (ROW+1,COL,0,'ALL',1)
	  GOTO FNDHST
	END
	XCALL OUTPT (ROW,COL,0,'ALL',1)
ASK3,
	CTL = '05,20,15,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	ITMKEY = ENTRY
	IF (ITMKEY.EQ.BLANKS) 
	BEGIN
	  XCALL OUTPT (ROW,COL,1,'ALL',1)
	  GOTO FNDHST
	END
	FOR I FROM 15 THRU 1 BY -1
	BEGIN
	  IF (ITMKEY(I,I).NE.' ') GOTO E3
	END
E3,
	XCALL OUTPT (ROW,COL,1,ITMKEY(1,I),1)
	CALL FNDITM
	XCALL OUTPT (ROW,40,0,DESCR,1)

FNDHST,
	CALL OPN_SPOOL_FILE
	HSTOT =
	ROW = 7
	RECNO = 1
	LSTCUS = -1
	IF (CUSKEY.EQ.0) GOTO HSTLP
	IF (ITMKEY.EQ.BLANKS) THEN CALL FNDHS1 ELSE CALL FNDHS2
	IF (SRCCTL) GOTO NOSLH
	RECNO = BSMID
	GOTO DSPHRC
HSTLP,
	LOKCTL = 1
	INCR RECNO
	XCALL IO (5,SLSHST,RECNO,READ,LOKCTL)
	IF (SLSHST.EQ.']]]]]]]]]]') GOTO ENDHST
DSPHRC,
	IF (CUSKEY.NE.0.AND.HCUSNO.NE.CUSKEY) GOTO ENDHST
	IF (CATKEY.NE.BLANKS.AND.CATKEY.NE.HPRDCD) GOTO HSTLP
	IF (ITMKEY.NE.BLANKS) IF (ITMKEY(1,I).NE.HITMNO(1,I)) GOTO HSTLP
	INCR ROW
	IF (ROW.GT.22) 
	BEGIN
	  CALL NXTPAG
	  IF (INXCTL.EQ.2) GOTO DISPLA
	END
	IF (CUSKEY.EQ.0.AND.HCUSNO.NE.LSTCUS)
	BEGIN
	  KEY(1,6) = HCUSNO,'XXXXXX'
	  CALL FNDCU2
	  IF (ROW+1.GE.22) CALL NXTPAG
	  IF (INXCTL.EQ.2) GOTO DISPLA
	  ALPHA(1,9) = HCUSNO,'ZZZZZX   '
	  XCALL OUTPT (ROW,1,0,ALPHA(1,9),1)
	  XCALL OUTPT (ROW,10,0,NAME,1)
	  INCR ROW
	  LSTCUS = HCUSNO

	  h3_cust = key(1,6)
	  h3_name = name
	  lincnt = 66
	END
	XCALL OUTPT (ROW,1,0,HITMNO,1)
	KEY(7,21) = HITMNO
	CALL FNDIT2
	XCALL OUTPT (ROW,17,0,DESCR,1)

	XCALL DATE8(HINVDT, D_OUT, D_OUTR, D_FMT, D_SW)
	XCALL OUTPT (ROW,49,0,D_FMT,1)

	PLINE(1,80) = HITMNO
	PLINE(17,80) = DESCR
	PLINE(49,80) = D_FMT

	ALPHA(1,8) = HQTY,NUMASK
	XCALL OUTPT (ROW,59,0,ALPHA(1,8),1)
	PLINE(59,80) = ALPHA(1,8)

	ALPHA(1,12) = HSALE,DLMASK
	XCALL OUTPT (ROW,69,0,ALPHA(1,12),1)
	PLINE(69,80) = ALPHA(1,12)

	CALL PRINT

	HSTOT = HSTOT + HSALE
	XCALL OUTPT (23,62,0,'TOTAL: ',1)
	ALPHA(1,12) = HSTOT,DLMASK
	XCALL OUTPT (23,69,0,ALPHA(1,12),1)

	GOTO HSTLP
NXTPAG,
	ROW = 08
	XCALL OUTPT (24,1,1,'<RETURN> FOR NEXT PAGE / <BS> TO ESCAPE',1)
	XCALL INPUT (24,42,01,00,'#E',ENTRY, INXCTL,1)
	XCALL OUTPT (ROW,1,2,'\',1)
	IF (INXCTL.EQ.2) RETURN
	RETURN
ENDHST,
	PLINE(62,80) = 'TOTAL: '
	ALPHA(1,12) = HSTOT,DLMASK
	PLINE(69,80) = ALPHA(1,12)
	CALL PRINT

	close 14
	clear chnspl

;;;	XCALL MESAG ('NO MORE RECORDS TO FIT THESE PARAMETERS',2)
	xcall outpt (24,1,1,'Print these results?',1)
	xcall input (24,41,01,01,'YN',entry,inxctl,1)

	if (inxctl.eq. 1) lpque (splfil)

	GOTO DISPLA
FNDHS1,
	XCALL SERCH (5,SLSHST,KEY(1,6),1,6,ORG55,BSMID,SRCCTL,1,1,6,0,0,0,0)
	RETURN
FNDHS2,
	XCALL SERCH (5,SLSHST,KEY(1,6+I),1,6+I,ORG55,BSMID,SRCCTL,1,1,6,0,0,0,0)
	RETURN
NOSLH,
	XCALL MESAG ('NO SALES HISTORY RECORDS FIT THESE PARAMETERS',1)
	GOTO DISPLA
FNDCUS,
	KEY(1,6) = CUSKEY,'XXXXXX'
FNDCU2,
	XCALL ISIO (1, CUSMAS, CUSKEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) NAME = 'CUSTOMER NOT ON FILE'
	RETURN
	
;;;	SRCCTL =
;;;	IF (CUSIDX(1,6).EQ.KEY(1,6)) RETURN
;;;	XCALL SERCH (2,CUSIDX,KEY(1,6),1,6,ORG01,BSMID,SRCCTL,1,7,11,0,0,0,0)
;;;	IF (SRCCTL)
;;;	BEGIN
;;;	  CUSMAS =
;;;	  NAME = 'CUSTOMER NOT ON FILE'
;;;	  RETURN
;;;	END
;;;	LOKCTL = 1
;;;	XCALL IO (1,CUSMAS,IRC001,READ,LOKCTL)
;;;	RETURN
FNDITM,
	KEY(7,21) = ITMKEY

FNDIT2,
	srcctl =
	IF (itemno.EQ.KEY(7,21)) RETURN
	xcall isio (3, itmmas, key(7,21), read, lokctl)
	if (lokctl .ne. 0)
	BEGIN
	  ITMMAS = 
	  DESCR = 'NOT FOUND OR PARTIAL ITEM NO.'
	  RETURN
	END
	return
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	SRCCTL =
;;;	IF (ITMIDX(1,15).EQ.KEY(7,21)) RETURN
;;;	XCALL SERCH (4,ITMIDX,KEY(7,21),1,15,ORG41,BSMID,SRCCTL,1,16,20,0,0,0,0)
;;;	IF (SRCCTL)
;;;	BEGIN
;;;	  ITMMAS = 
;;;	  DESCR = 'NOT FOUND OR PARTIAL ITEM NO.'
;;;	  RETURN
;;;	END
;;;	LOKCTL = 1
;;;	XCALL IO (3,ITMMAS,IRC041,READ,LOKCTL)
;;;	RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	RETURN


ENDOFF,
	CALL CLOSES

ABORT,
	XCALL PGCHN ('CP:CPMENU',1)
	STOP

OPN_SPOOL_FILE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHNSPL) CLOSE CHNSPL
	splfil = 'SPL:SLHINQ.SPL'
	open (14,o,splfil)
	chnspl = 14

	RETURN
;-------------------------------------------------
PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD2,HD3
&		,LG,LG,LG,0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;-------------------------------------------------------------



YEAR_SLSHST,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; OPEN SLSHST FILE FOR SELECTED YEAR
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR BADYEAR 

	CLOSE CHN055
	XCALL FFILE (55, SH_FILE, SWITCH)
	IF (YEAR .NE. 0)
		BEGIN
		SH_Y = 'Y'
		SH_EXT = YEAR, 'XX'
		END

	ONERROR BADFILE
	OPEN (CHN055, I, SH_FILE)
	OFFERROR
	RETURN

BADFILE,
	OFFERROR
	BADYEAR = 1
	RETURN
;------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; OPEN FILES...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK

;;;	SWITCH = 1
;;;	XCALL FILES (2,'I',002,SWITCH)		;FILE 002 - CUSIDX
;;;	IF (SWITCH.EQ.9) RETURN
;;;	CHN002 = 2

	XCALL FILES (1,'SI',001,SWITCH)		;FILE 001 - CUSMAS
	IF (SWITCH.EQ.9) RETURN
	CHN001 = 1

;;;	XCALL FILES (4,'SI',042,SWITCH)		;FILE 042 - ITMIDX
;;;	IF (SWITCH.EQ.9) RETURN
;;;	CHN042 = 4

	XCALL FILES (3,'SI',041,SWITCH)		;FILE 041 - ITMMAS
	IF (SWITCH.EQ.9) RETURN
	CHN041 = 3

	XCALL FILES (5,'I',055,SWITCH)		;FILE 055 - SLSHST
	IF (SWITCH.EQ.9) RETURN
	CHN055 = 5
RDHDR,
;;;	LOKCTL = 1
;;;	XCALL IO (1,CUSCTL,1,READ,LOKCTL)
;;;	ORG01 = ORG001
;;;	LOKCTL = 1
;;;	XCALL IO (3,ITMCTL,1,READ,LOKCTL)
;;;	ORG41 = ORG041
	LOKCTL = 1
	XCALL IO (5,SLHCTL,1,READ,LOKCTL)
	ORG55 = ORG055

	OPNOK = 1
	RETURN
;-------------------------------------------------------
CLOSES,
	SWITCH = 4

;;;	IF (CHN002) XCALL FILES (2,'I',002,SWITCH)	;FILE 002 - CUSIDX
	IF (CHN001) XCALL FILES (1,'SI',001,SWITCH)	;FILE 001 - CUSMAS
	IF (CHN042) XCALL FILES (4,'I',042,SWITCH)	;FILE 042 - ITMIDX
	IF (CHN041) XCALL FILES (3,'SI',041,SWITCH)	;FILE 041 - ITMMAS
	IF (CHN055) XCALL FILES (5,'I',055,SWITCH)	;FILE 055 - SLSHST
	RETURN
;-------------------------------------------------------
END
