;filpr3.cp
;
; slhpr3.cp never converted to isam???
;
; SLHPR3.COP
;
;	SALES HISTORY PRINT BY category / item
;
;	5-29-18 ssq: make cusmas isam
;
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
	
RECORD SLHFIL
		,A11,	'SMC:SLSSUM.'
	SLHYR	,D2
		,A1

RECORD SLHTMP
		,A14,	'SMC:SLHWRK.SMC'
RECORD LEG1
		,A18,	'FOR SALES YEAR: 20'
	LSLHYR	,D2
RECORD LEG3
		,A10,	'CUSTOMER: '
	LCUSNO	,A6
		,A3
	LCUSNA	,A25
RECORD HDR1
		,A40,'-------------------- ITEM --------------'
		,A45,'------------  LAST SL  -------- SALE --------'
RECORD HDR2
		,A40,'CAT  NUMBER          DESCRIPTION        '
		,A45,'                 DATE      QTY       DOLLARS '
RECORD ITMSUB
	ITMSUBQ	,D6
	ITMSUB$	,D10

record	splfil
		,a4,	'RPT:'
		,A1,	'U'		;FILPR1 IS "R"
	R_CUST	,A6
		,A4,	'.TXT'
RECORD
	IS_CURRENT_YEAR	,D1
	DASH	,A12,	'------------'
	V	,D1
	SWITCH	,D1
	ITMKEY	,A15
	CATKEY	,A2
;	CUSKEY	,D6
	ORG41	,D5
	ORG01	,D5
	ORG55	,D5
	REC55	,D5
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
	LSTCUS	,D6,	-1
	LSTCAT	,A2,	']]'
	LSTITM	,A15,	'-1'
	LINCNT	,D2,60
	PGCNT	,D3
	PRTCTL	,D2
	LPSW	,D2
	LPARG	,D1
	PRTTYP	,A1
	RPTNUM	,D3
	PLINE	,A132
;;;	SPLFIL	,A14
	TITLE	,A54,	'SALES HISTORY SUMMARY BY PRODUCT CAT / ITEM'
	GRTOT	,D12
	CUSSUB	,D12
	CATSUB	,D12
	DECML	,D2
	RESTART	,D6,	0
PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,TITLE,1)
ASK0,
	XCALL OUTPT (12,20,0,'ENTER YEAR TO PROCESS OR <ENTER> FOR CURRENT FILE',1)
	XCALL INPUT (12,70,02,00,'#X',ENTRY,INXCTL,1)
	GOTO (ASK0,ABORT,OPEN), INXCTL
	SLHYR = ENTRY
	LSLHYR = SLHYR
	IF (ENTRY .EQ. BLANKS)
	THEN	IS_CURRENT_YEAR = 1
	ELSE	IS_CURRENT_YEAR = 0

;;;	IF (SLHYR.NE.0.AND.SLHYR.LT.92) 
;;;	BEGIN
;;;	  XCALL MESAG ('1992 WAS THE FIRST YEAR DATA WAS COLLECTED',1)
;;;	  GOTO ASK0
;;;	END
OPEN,
;;;	SWITCH = 1
;;;	XCALL FILES (2,'I',002,SWITCH)		;FILE 002 - CUSIDX
;;;	IF (SWITCH.NE.9) GOTO OPEN2
;;;	GOTO ABORT
OPEN2,
	XCALL FILES (1,'SI',001,SWITCH)		;FILE 001 - CUSMAS
	IF (SWITCH.NE.9) GOTO OPEN3
	CALL CLOSE1
	GOTO ABORT
OPEN3,
	XCALL FILES (4,'I',042,SWITCH)		;FILE 042 - ITMIDX
	IF (SWITCH.NE.9) GOTO OPEN4
	CALL CLOSE2
	GOTO ABORT
OPEN4,
	XCALL FILES (3,'I',041,SWITCH)		;FILE 041 - ITMMAS
	IF (SWITCH.NE.9) GOTO OPEN5
	CALL CLOSE3
	GOTO ABORT
OPEN5,
	SWITCH = 6
	XCALL FILES (5,'I',55,SWITCH)
	IF (SWITCH.EQ.9)
	BEGIN
	  CALL CLOSE4
	  GOTO ABORT
	END
	SWITCH = 1
;;;	IF (SLHYR.EQ.0) XCALL FFILE (55,SLHFIL,SWITCH)	;FILE 055 - SLSHST
;;;	IF (IS_CURRENT_YEAR) XCALL FFILE (55,SLHFIL,SWITCH)	;FILE 055 - SLSHST
	XCALL FFILE (55,SLHFIL,SWITCH)	;FILE 055 - SLSHST
	IF (.not. IS_CURRENT_YEAR)
		BEGIN
		SLHFIL(12,12) = 'Y'
		SLHFIL(13,14) = LSLHYR,'XX'
		END

	XCALL FFILE (57,SLHTMP,SWITCH)
	XCALL OUTPT (2,1,1,'BUILD TEMP FILE - ',1)
	XCALL OUTPT (0,0,0,SLHTMP,1)
	RESTART =
	IF (INXCTL.EQ.3) 
	BEGIN
	  XCALL OUTPT (14,20,00,'ENTER RESTART CUSTOMER CODE: ',1)
	  XCALL INPUT (14,50,06,00,'# ',ENTRY,INXCTL,1)
	  RESTART = ENTRY
	  INXCTL =
	  IF (RESTART) INXCTL = 3
	  IF (INXCTL.EQ.3) GOTO SKIPB
	END
	OPEN (5,O,SLHTMP)
	OPEN (6,I,SLHFIL)
	READ (6,SLHCTL,1)
	REC55 = REC055
RDLP1,
	READS (6,SLSHST,ENLP1)
	IF (SLSHST.EQ.']]]]]]]]]]') GOTO ENLP1
	IF (HCUSNO.EQ.0) GOTO RDLP1
;;;		INCR RECNO
;;;		IF (RECNO.GT.500) 
;;;		BEGIN
;;;		  READ (6,SLSHST,REC55+1)
;;;		  GOTO ENLP1
;;;		END
	ONERROR NODEC
	DECML = HPRDCD
	OFFERROR
	HPRDCD = DECML
	WRITES (5,SLSHST)
	GOTO RDLP1
NODEC,
	WRITES (5,SLSHST)
	GOTO RDLP1	
ENLP1,
	WRITES (5,SLSHST)
	CLOSE 6
	CLOSE 5
	XCALL OUTPT (2,1,1,'SORTING',1)
	SORT (INPUT=SLHTMP,RECORD=SLSHST/F,KEY=(HPRDCD,HITMNO),OUTPUT=SLHTMP)
	XCALL OUTPT (2,1,1,'\',1)
SKIPB,
	OPEN (5,I,SLHTMP)
OPEN6,
	LPSW = 1		; PRINT, SPOOL OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.NE.0) 
	BEGIN
	  LPARG = 2
	  IF (LPSW.EQ.2) LPARG = 4
	  XCALL WATE (LPARG,V)
	  GOTO RDHDR
	  END
	CALL CLOSE5
	GO TO ENDOFF
CLOSES,
	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
CLOSE5,
	XCALL FILES (5,'I',055,4)
	XCALL DELET (5,SLHTMP)
CLOSE4,
	XCALL FILES (3,'I',042,4)
CLOSE3,
	XCALL FILES (4,'I',041,4)
CLOSE2,
	XCALL FILES (1,'I',001,4)
CLOSE1,
	XCALL FILES (2,'I',002,4)
	RETURN
ENDOFF,
	CALL CLOSES
ABORT,
	XCALL PGCHN ('CP:CPMENU',1)
	STOP
RDHDR,
	LOKCTL = 1
	XCALL IO (1,CUSCTL,1,READ,LOKCTL)
	ORG01 = ORG001
	LOKCTL = 1
	XCALL IO (3,ITMCTL,1,READ,LOKCTL)
	ORG41 = ORG041
	LOKCTL = 1
	XCALL IO (5,SLHCTL,1,READ,LOKCTL)
	ORG55 = ORG055
DISPLA,
	CATSUB =
	CUSSUB =
	ITMSUB =
	GRTOT =
	LINCNT = 62
	XCALL OUTPT (1,1,2,TITLE,1)
	XCALL OUTPT (4,10,0,'CATEGORY: ',1)
	XCALL OUTPT (5,10,0,'ITEM NO.: ',1)
	XCALL OUTPT (5,40,0,'(PARTIAL ITEM NUMBERS ALLOWED)',1)
ASK1,
;	CTL = '03,20,06,00,#E'
;	CALL INPUT
;	GOTO (DISPLA,ENDOFF), INXCTL
;	CUSKEY = ENTRY
;	IF (CUSKEY.EQ.0)
;	BEGIN
;	  XCALL OUTPT (ROW,COL,0,'ALL',1)
;	  GOTO ASK2
;	END
;	CALL FNDCUS
;	XCALL OUTPT (ROW,40,0,NAME,1)
ASK2,
	CTL = '04,20,02,00,AE'
	CALL INPUT
	GOTO (DISPLA,ENDOFF), INXCTL
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
	ROW = 7
	RECNO = 1
	LSTCUS = -1
;	IF (CUSKEY.EQ.0) GOTO HSTLP
	IF (ITMKEY.EQ.BLANKS.AND.CATKEY.EQ.BLANKS) GOTO HSTLP
	SRCCTL = 1
	IF (CATKEY.NE.BLANKS) CALL FNDHS1	;ONE CAT AUTO ALL ITEMS
	IF (CATKEY.EQ.BLANKS.AND.ITMKEY.NE.BLANKS) CALL FNDHS2

	IF (SRCCTL) GOTO NOSLH
	RECNO = BSMID
	GOTO DSPHRC
HSTLP,
	LOKCTL = 1
	INCR RECNO
	XCALL IO (5,SLSHST,RECNO,READ,LOKCTL)
	IF (LOKCTL) SLSHST = ']]]]]]]]]]'
	IF (SLSHST.EQ.']]]]]]]]]]') GOTO ENDHST
;	IF (HCUSNO.GE.RESTART) RESTART =
	IF (LSLHYR.EQ.0) LSLHYR = HINVDT(5,6)
DSPHRC,
;	IF (CUSKEY.NE.0.AND.HCUSNO.NE.CUSKEY) GOTO ENDHST


	IF (CATKEY.NE.BLANKS.AND.CATKEY.NE.HPRDCD) GOTO HSTLP
	IF (ITMKEY.NE.BLANKS) IF (ITMKEY(1,I).NE.HITMNO(1,I)) GOTO HSTLP

	IF (LSTITM.EQ.'-1') LSTITM = HITMNO
	IF (LSTCAT.EQ.']]') LSTCAT = HPRDCD

	IF (HITMNO.NE.LSTITM) CALL ITMSUB

	IF (CATKEY.EQ.'  '.AND.HPRDCD.NE.LSTCAT)
	BEGIN
	  IF (LSTCAT.NE.']]') CALL CATSUB
	  LSTCAT = HPRDCD
	END

;	IF (CUSKEY.EQ.0.AND.HCUSNO.NE.LSTCUS)
;	BEGIN
;	  IF (LSTCUS.NE.-1.AND.HPRDCD.EQ.LSTCAT) CALL CATSUB
;	  IF (LSTCUS.NE.-1) CALL CUSSUB
;	  KEY(1,6) = HCUSNO,'XXXXXX'
;	  CALL FNDCU2
;	  LCUSNO = HCUSNO,'ZZZZZX'
;	  LCUSNA = NAME
;	  LSTCUS = HCUSNO
;	END


	ITMSUBQ = ITMSUBQ + HQTY
	ITMSUB$ = ITMSUB$ + HSALE
	LSTITM = HITMNO

	GOTO HSTLP


ITMSUB,
	KEY(3,17) = LSTITM
	CALL FNDIT2
	PLINE (2,3) = LSTCAT
	PLINE (6,20) = LSTITM
	PLINE (22,52) = DESCR

	PLINE (64,71) = ITMSUBQ,NUMASK
	PLINE (74,85) = ITMSUB$,DLMASK
	CALL PRINT
	CATSUB = CATSUB + ITMSUB$
	ITMSUB =
	RETURN
CATSUB,
	PLINE (74,85) = DASH
	CALL PRINT
	PLINE (41,70) = 'PRODUCT CATEGORY XX SUBTOTAL: '
	PLINE (58,59) = LSTCAT
	PLINE (74,85) = CATSUB, DLMASK	
	CALL PRINT
	CALL LINFD
;	CUSSUB = CUSSUB + CATSUB
	GRTOT = GRTOT + CATSUB		;
	CATSUB = 
	RETURN
;CUSSUB,
;	PLINE (74,85) = DASH
;	CALL PRINT
;	PLINE (52,70) = 'CUSTOMER SUBTOTAL: '
;	PLINE (74,85) = CUSSUB, DLMASK	
;	CALL PRINT
;	LINCNT = 62
;	IF (LPSW.EQ.-4)
;	BEGIN
;	  LPSW = LPSW * -1
;	  SLEEP 2
;	END
;	GRTOT = GRTOT + CUSSUB
;	CUSSUB = 
;	RETURN
GRTOT,
	LEG3 = 'GRAND TOTAL PAGE'
	PLINE (74,85) = DASH
	CALL PRINT
	PLINE (52,70) = 'GRAND TOTALS: '
	PLINE (72,85) = GRTOT, DLMASK		;xx,xxx,xxx.xx-
	CALL PRINT
	GRTOT =
	RETURN
ENDHST,
;	IF (CUSKEY.EQ.0.AND.CATKEY.EQ.BLANKS.AND.ITMKEY.EQ.BLANKS.AND.
;&		SLSHST.EQ.']]]]]]]]]]')
	IF (CATKEY.EQ.BLANKS.AND.ITMKEY.EQ.BLANKS.AND.
&		SLSHST.EQ.']]]]]]]]]]')
	BEGIN
	  CALL ITMSUB
	  CALL CATSUB
;	  CALL CUSSUB
	  CALL GRTOT
	  GOTO ENDOFF
	END
	CALL ITMSUB
	CALL CATSUB
;	CALL CUSSUB
	XCALL MESAG ('NO MORE RECORDS TO FIT THESE PARAMETERS',2)
	GOTO DISPLA
FNDHS1,
	KEY(1,2) = CATKEY
	XCALL SERCH (5,SLSHST,KEY(1,2),52,53,ORG55,BSMID,SRCCTL,4,1,6,0,0,0,0)
	RETURN
FNDHS2,
	KEY(1,2) = PRDCAT
	KEY(3,2+I) = ITMKEY
	XCALL SERCH (5,SLSHST,KEY(1,2+I),52,53,ORG55,BSMID,SRCCTL,4,1,6,7,6+I,0,0)
	RETURN
NOSLH,
	XCALL MESAG ('NO SALES HISTORY RECORDS FIT THESE PARAMETERS',1)
	GOTO DISPLA
;FNDCUS,
;	KEY(1,6) = CUSKEY,'XXXXXX'
;FNDCU2,
;	SRCCTL =
;	IF (CUSIDX(1,6).EQ.KEY(1,6)) RETURN
;	XCALL SERCH (2,CUSIDX,KEY(1,6),1,6,ORG01,BSMID,SRCCTL,4,7,11,0,0,0,0)
;	IF (SRCCTL)
;	BEGIN
;	  CUSMAS =
;	  NAME = 'CUSTOMER NOT ON FILE'
;	  RETURN
;	END
;	LOKCTL = 1
;	XCALL IO (1,CUSMAS,IRC001,READ,LOKCTL)
;	RETURN
FNDITM,
	KEY(3,17) = ITMKEY
FNDIT2,
	SRCCTL =
	IF (ITMIDX(1,15).EQ.KEY(3,17)) RETURN
	XCALL SERCH (4,ITMIDX,KEY(3,17),1,15,ORG41,BSMID,SRCCTL,4,16,20,0,0,0,0)
	IF (SRCCTL)
	BEGIN
	  ITMMAS = 
	  DESCR = 'NOT FOUND OR PARTIAL ITEM NO.'
	  RETURN
	END
	LOKCTL = 1
	XCALL IO (3,ITMMAS,IRC041,READ,LOKCTL)
	RETURN
INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	RETURN
PRINT,
	IF (RESTART) PLINE =
	IF (RESTART) RETURN
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,HDR2,'NO HDR',
&			LEG1,' ',LEG3,0,0,132,1,LPSW,RPTNUM,PRTTYP)
	RETURN
LINFD,
	IF (RESTART) RETURN
	XCALL LINFD (1)
	INCR LINCNT
	RETURN
END
;-------------------- ITEM --------------------------  LAST SL  -------- SALE --------
;CAT  NUMBER          DESCRIPTION                         DATE      QTY       DOLLARS
; XX  XXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XX/XX/XX  XXX,XXX-  XXXX,XXX.XX-


