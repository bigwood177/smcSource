;PRTBO.PO
;		Print backorder PO's

SUBROUTINE	prtBo
	POHD	,A		;PORHDR
	CHANS	,A		;FILE CHANNELS


; PRINT PO...


	.INCLUDE 'DEF:RTF.DEF'

RECORD	COINFO
	.INCLUDE 'DEF:COINFO.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CHANNEL
	CHN011	,D2
	CHN012	,D2
	CHN027	,D2
	CHN041	,D2
	CHN042	,D2
	CHN151	,D2
	CHN152	,D2
	CHN153	,D2
	CHN149	,D2
	CHN182	,D2		;COP TABLES

RECORD	POSHIP
	.INCLUDE 'DEF:RD149A.DEF'

RECORD HEADER	
	.INCLUDE 'DEF:RD151A.DEF'

RECORD POLINE	
	.INCLUDE 'DEF:RD152A.DEF'

RECORD TABLE	
	.INCLUDE 'DEF:RD153A.DEF'

RECORD DUMTBL	
	.INCLUDE 'DEF:RD153B.DEF'

RECORD VENDOR	
	.INCLUDE 'DEF:RD011A.DEF'

RECORD DUMVEN	
	.INCLUDE 'DEF:RD011B.DEF'

RECORD VENIDX	
	.INCLUDE 'DEF:RD012A.DEF'

RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.DEF'
RECORD	ITMCTL
	.INCLUDE 'DEF:RD041B.DEF'
RECORD	ITMIDX
	.INCLUDE 'DEF:RD042A.DEF'

RECORD KEYPO
	KEY1	,D6
	KEY2	,D2

RECORD	XLINE
		,A120
RECORD PLINE
		,A80

RECORD,X
	KLNNUM	,A3
		,A1
	KORD	,A7
		,A1
	KRCV	,A7
		,A1
	KBO	,A7
		,A2
	KOURNO	,A13
		,A35
		,A3

RECORD LINE1,X
		,A63
	KPONUM	,A6
	KDASH	,A1
	KRLNUM	,A2
		,A3
	KPGNUM	,A2
		,A3

RECORD LINE2,X
		,A58
	KPODTE	,A8
		,A6
	KCXCDE	,A6
		,A2
RECORD LINE3,X
		,A71
	KCHDTE	,A8
		,A1
RECORD LINE4,X
		,A12
	KVNADR	,A30
		,A8
	KSHPTO	,A30
RECORD LINE6,X
		,A2
	KORTYP	,A7
		,A2
	KBUYER	,A16
		,A2
	KACKNO	,A3
	;;;	,A2
	KCONFM	,A5
		,A2
	KTERMS	,A10
		,A2
	KFOB	,A9
		,A2
	KSPVIA	,A15
		,A2
	KCLPPD	,A1
RECORD LINE7,X
		,A2
		,A3
;;;	KLNNUM	,A3
		,A2
	KQYORD	,A7
		,A2
		,A13
;;;	KOURNO	,A13
		,A2
	KVENNO	,A17
		,A2
	KPRICE	,A11
		,A2
	KUOFM	,A2
		,A2
	KRQDTE	,A10
	KCXLIN	,A3
RECORD LINE8,X
		,A7
	KBLTYP	,A4
		,A9
	KLNOTE	,A30
		,A30
RECORD LINE10,X
		,A20
	KNOTE1	,A30
		,A2
	KNOTE2	,A27

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

RECORD TBLKY
	TKEYNO	,D2
	TKEYCD	,A2
	TKEYSQ	,D1

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT
RECORD	E_BOLD
	E_ESC	,A1
		,A4,	'(s3B'	;bold
RECORD	E_MED
	M_ESC	,A1
		,A4,	'(s0B'	;medium

RECORD	SPLFIL
		,A4,	'SPL:'
		,A1,	'P'
	SPPO	,D6
		,A3,	'.PO'

RECORD	FAXFIL
		,A*,	'SPL:'
		,A1,	'P'
	SFAX	,D6
		,A4,	'.FAX'


record	buf
		,a500


record	values
	r_bold	,a*,	'\b '
	r_plain	,a*,	'\plain\f3\fs20\cf0 '

record	t_bold
	besc	,a1
		,a*,	'(s3B'
record	t_plain
	pesc	,a1
		,a*,	'(s0B'


RECORD	VARS
	F_CTR	,D4
	EXTPRC	,D10
	TOTAMT	,D10
	SAVBUF	,A500
	bl	,d4
	sts	,d1	;success flag
	r_flg	,d1	;was anything replaced?
	XDATE	,D8
	BLANKS	,A31
	BSMID	,D5
	CNGCTL	,D1
	ENTRY	,A30
	EOF	,D1
	HDATE	,D6
	INXCTL	,D1
	ISSCDE	,3A1,	'N','C','X'
	ISSNAM	,3A8,	'ORIGINAL','CHANGE  ','CANCEL  '
	KLNCNT	,D2
	LINCNT	,D2,	60
	LINCTL	,D1
	LOKCTL	,D1
	LPSW	,D1
	N	,D2
	PGCNT	,D3,	000
	PGNUM	,D2
	PRTCTL	,D3
	PRTTYP	,A1
	READ	,D1,	0
	WRITE	,D1,	1
	RECNO	,D5
	RPTNUM	,D3
	SRCCTL	,D1
	SWITCH	,D1
	TITLE	,A2
	TODAY	,D8
	TYPCDE	,3A1,	'N','B','R'
	TYPNAM	,3A7,	'NORMAL ','BLANKET','RELEASE'
	VADR	,5A35		;(01) WAS 4A31		
	SHPTO	,5A30
	WHATNO	,D2
	RJUST	,D1

PROC
	CALL SETUP		;PORHDR, PORLIN, VENMAS, READ HEADERS

	CLEAR TOTAMT		;SSQ 7/10/02

	CALL READLN			; GET FIRST PO LINE
	IF (.NOT. EOF) CALL PAGES	; PRINT ALL PAGES OF ONE P/O

	CLOSE 14

	LPQUE (SPLFIL)			;original


;;;	CLOSE CHN041
;;;	SWITCH = 5
;;;	XCALL FILES (CHN041, 'I', 041, SWITCH)

	HPOSTS = 'P'		;change status to Printed
	POHD = HEADER
	XRETURN
;**********************************************************************

CON_FAX,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Convert spool file to fax file...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ASCII(27,BESC)
	PESC = BESC
	E_CHAR = BESC

	OPEN(14,I,SPLFIL)
	OPEN(16,O,FAXFIL)

LOOP,
	READS(14,BUF,EOF)
	CLEAR R_FLG
	XCALL REPLC(BUF, T_BOLD, R_BOLD, STS)
	R_FLG = STS
	XCALL REPLC(BUF, T_PLAIN, R_PLAIN, STS)
	R_FLG = R_FLG + STS
;;;	IF(R_FLG.GT.0)
;;;		BEGIN
;;;		CLEAR SAVBUF
;;;		SAVBUF(2,500) = BUF
;;;		BUF = SAVBUF
;;;		END
	BL = %TRIM(BUF)
	IF (BL .EQ. 1) GOTO PAR
	WRITES(16, BUF(1,%TRIM(BUF)) )
PAR,
	WRITES(16,'\par')
	GOTO LOOP
EOF,
;;;	WRITES(14,EJECT)
	CLOSE 14
	CLOSE 16
	RETURN
;------------------------------------------


PAGES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PRINT THE PO...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	LINCNT = 1

	DISPLAY (14, E_BOLD)
	PLINE (23,50) = "P U R C H A S E   O R D E R"
	PLINE (61,63) = "PO#"
	KPONUM = HPONUM,'ZZZZXX'
	KDASH = '-'
	KRLNUM = HRLNUM,'XX'
	KPGNUM = PGNUM,'XX'
	LINCTL = 0
	CALL PRINT
	PLINE(32,51) = 'Back Order'
;;;	PLINE(32,51) = 'Original'
	CALL PRINT
	DISPLAY (14, E_MED)

	LINCTL = 0
	PLINE(5,75) = C_NAME

	KPODTE(1,3) = 'JOB'
	KPODTE(10,17) = HJOB,'ZZZZZZ'
	CALL PRINT

	PLINE(5,75) = C_ADD1

	KPODTE(1,8) = 'DATE REQ'
	XDATE(1,4) = HPODTE(5,8)
	XDATE(5,8) = HPODTE(1,4)
	KPODTE(10,19) = XDATE, 'XX/XX/XXXX'
	CALL PRINT

	PLINE(5,75) = C_ADD2

	LINCTL = 0
	CALL PRINT

	PLINE(5,75) = %STRING(C_LOC,'XXX-XXX-XXXX') + '  FAX ' + 
&		%STRING(C_FAX,'XXX-XXX-XXXX')
	LINCTL = 3		;SSQ 7-7-99
	CALL PRINT
	LINCTL = 0

	KBUYER = HVENDR		;SSQ 7/10/02
	KTERMS = "SHIP TO:"
	CALL PRINT

	IF(HSHPTO .EQ. '99') 
	THEN	BEGIN
		SHONUM = HPONUM
		SHLNUM = HRLNUM
		XCALL ISIO (CHN149,POSHIP,SHKEY,READ,LOKCTL)
		IF (SHKEY .EQ. PORKEY) 
			THEN	USING SHCODE SELECT
				('S'),	KSHPTO = ;;;>'SHIP TO'
				('D'),	KSHPTO = 'DROP SHIP'
				('W'),	KSHPTO = 'WILL CALL'
				ENDUSING
			ELSE	BEGIN
				KSHPTO = ;;;>'SHIP TO'
				CLEAR POSHIP
				SHADD1 = '*** NOT ON FILE ***'
				END
		END
	ELSE	KSHPTO = ;;;>'SHIP TO'

	DISPLAY (14, E_BOLD)
	CALL PRINT
	DISPLAY (14, E_MED)

	TKEYNO = 1
	TKEYCD = HSHPTO
	VADR(1) = NAME
	VADR(2) = ADD1
	VADR(3) = ADD2
	ENTRY =
	ENTRY(1,15) = CITY
	N = 16
FIND,
	N = N - 1
	IF (N.GT.1.AND.ENTRY(N,N).EQ.' ') GO TO FIND
	ENTRY(N+1,N+1) = ','
	ENTRY(N+3,N+4) = STATE
	ENTRY(N+6,N+15) = ZIP
	VADR(4) = ENTRY				;(01)  WAS 4
	IF (VADR(1).EQ.BLANKS) CALL MOVE1
	IF (VADR(2).EQ.BLANKS) CALL MOVE2
	IF (VADR(3).EQ.BLANKS) CALL MOVE3
	IF (VADR(4).EQ.BLANKS) CALL MOVE4	;(01)
	
	LINCTL = 0
	IF(HSHPTO .EQ. '99') 
	THEN	BEGIN
		SHPTO(1) = SHADD1
		SHPTO(2) = SHADD2
		SHPTO(3) = SHADD3
		SHPTO(4) = SHADD4
		END
	ELSE	BEGIN
		SHPTO(1) =
		SHPTO(2) = 
		SHPTO(3) = 
		SHPTO(4) = 
		END

	N = 1
ADLOOP,
	KVNADR = VADR(N)
	IF(HSHPTO.EQ.'99')
		BEGIN
		KSHPTO = SHPTO(N)
		GOTO PRTSHP
		END

	TKEYSQ = N
	CALL GETTBL
	KSHPTO = TDESC
PRTSHP,
	CALL PRINT
	INCR N
	IF (N.LE.4) GO TO ADLOOP

	KBUYER = TELNO
	CALL PRINT

	DISPLAY (14, E_BOLD)
	KACKNO(1,11) = 'CONFIRMING'
	CALL PRINT

	KORTYP = 	;;;>'TYPE'
	KACKNO = 	;;;>'ACK'
	PLINE(3,10) = 'PO DATE'
	KCONFM = 'ORDER'
	KBUYER = 'ATTENTION'
	KTERMS = 'TERMS'
	KFOB   = 'FOB'
	KSPVIA = 'SHIP-VIA'
	PLINE(74,80) = 'COL/PPD'
	CALL PRINT
	DISPLAY (14, E_MED)

	XDATE(1,4) = TODAY(1,4)
	XDATE(5,6) = TODAY(7,8)
	PLINE(3,10) = XDATE(1,6),	'XX/XX/XX'

	KBUYER(1,20) = HCONTC		;VENDOR CONTACT

	IF (HCONFR.EQ.'Y') KCONFM = 'YES'
	IF (HCONFR.EQ.'N') KCONFM = ' NO'

	TBLKY =
	TKEYNO = 4
	TKEYCD = HTERMS
	CALL GETTBL
	KTERMS = TDESC

	TBLKY =
	TKEYNO = 5
	TKEYCD = HFOB
	CALL GETTBL
	KFOB = TDESC

	CLEAR TBL_KEY
	TBL_KEY = 'P1'
	P1_CODE = HSHPVI
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
	THEN	BEGIN
		TBL_KEY = 'SC'
		SC_SCAC = P1_SCAC
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
		IF (LOKCTL .EQ. 0) 
		THEN	KSPVIA = SC_NAME
		ELSE	KSPVIA = "* NOT ON FILE *"
		END
	ELSE	KSPVIA = "* NOT ON FILE *"
		

	IF (HCOLPP.EQ.'C') KCLPPD = 'C'
	IF (HCOLPP.EQ.'P') KCLPPD = 'P'
	LINCTL = 2
	CALL PRINT

	LINCTL = 1
	DISPLAY (14, E_BOLD)
	KLNNUM = 'L/I'

;----------------------------------------------------
;;;	KQYORD = '    QTY'

	KORD = '    ORD'
	KRCV = '    RCV'
	KBO  = '    B/O'
;----------------------------------------------------
	KUOFM  = 'UM'
	KOURNO(1,16) ='ITEM/DESCRIPTION'
	KPRICE = '      PRICE'	
	KRQDTE = '     TOTAL'
	KCXLIN = 	;;;>'STS'
	CALL PRINT
	DISPLAY (14, E_MED)

	CALL LINES
	IF(EOF)
		BEGIN
		KPRICE = 'TOTAL'
		KRQDTE = TOTAMT,	'ZZZ,ZZX.XX'
		LINCTL = 0
		CALL PRINT
		END
	
	LINCNT = LINCNT + 6

	KLNCNT = 60 - LINCNT
	IF (LINCNT.LT.60) XCALL LINFD (KLNCNT)

	LINCNT = 1		;NEW
	TKEYNO = 6
	N = 1
NTLOOP,
	IF (HPONCD(N).EQ.BLANKS) GO TO SKIP
	TKEYCD = HPONCD(N)
	CALL GETTBL
	KNOTE1 = TDESC
SKIP,
	KNOTE2 = HPONTE(N)
	LINCTL = 0
	CALL PRINT
	INCR N
	IF (N.LE.3) GO TO NTLOOP

	KSHPTO = 'PURCHASING DEPT'
	CALL PRINT

	TBLKY =
	TKEYNO = 2
	TKEYCD = HBUYER
	CALL GETTBL
	KSHPTO = TDESC
	CALL PRINT

	XCALL LINFD (0)

	IF (EOF) RETURN

	INCR PGNUM
	GO TO PAGES
;**********************************************************************

MOVE1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	VADR(1) = VADR(2)
MOVE2,
	VADR(2) = VADR(3)
MOVE3,
	VADR(3) = VADR(4)
MOVE4,
	VADR(4) = VADR(5)		
	VADR(5) = '                              *'
	IF (VADR(1).EQ.BLANKS) GO TO MOVE1
	IF (VADR(2).EQ.BLANKS) GO TO MOVE2
	RETURN
;---------------------------------------------

LINES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PRINT LINE ITEMS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	KLNNUM = LLINNO,'XXX'

;----------------------------------------------------
;;;	KQYORD = LQTYOR,'ZZZ,ZZX'

	KORD = LQTYOR,	'ZZZ,ZZX'
	KRCV = LQTYRC,	'ZZZ,ZZX'
	KBO = (LQTYOR-LQTYRC),	'ZZZ,ZZX'
;----------------------------------------------------

	KUOFM = LITMUM
	KOURNO = LITMNO
	IF (RJUST) XCALL LEFTJ (KOURNO(1,15),15)
;;;	KVENNO = LVITMN
	IF (HPRTPR.NE.'N') KPRICE =(LEXCST),'ZZ,ZZX.XXXX'

	EXTPRC = (LEXCST*LQTYOR)#2
	KRQDTE = EXTPRC,	'ZZZ,ZZX.XX'

	TOTAMT = TOTAMT + EXTPRC	;SSQ 7/10/02
NEXT1,
	LINCTL = 0
	CALL PRINT

ITMNAM,
	KOURNO(1,30) = LITMDS
;;;	KLNOTE = LITMDS
	CALL PRINT

	TBLKY =
	TKEYNO = 6
	IF (LPONCD(1).EQ.BLANKS) GO TO NEXT2
	TKEYCD = LPONCD(1)
	CALL GETTBL
	KLNOTE = TDESC
	CALL PRINT
NEXT2,
	IF (LPONCD(2).EQ.BLANKS) GO TO NEXT3
	TKEYCD = LPONCD(2)
	CALL GETTBL
	KLNOTE = TDESC
	CALL PRINT
NEXT3,
	KLNOTE = LPONTE(1)
	IF (KLNOTE.NE.BLANKS) CALL PRINT
	KLNOTE = LPONTE(2)
	IF (KLNOTE.NE.BLANKS) CALL PRINT
	KLNOTE = LPONTE(3)
	IF (KLNOTE.NE.BLANKS) CALL PRINT
	KLNOTE = LPONTE(4)
	IF (KLNOTE.NE.BLANKS) CALL PRINT
	XCALL LINFD (1)
	INCR LINCNT

	CALL NXTREC
	IF(EOF) RETURN
	IF(LINCNT .GE. 50) RETURN

	GO TO LINES

;**********************************************************************
;	READ ONE IN-RANGE LINE RECORD
;**********************************************************************
READLN,
	EOF =
REPEAT,
	KEY1 = HPONUM
	KEY2 = HRLNUM
	LOKCTL = 1
	XCALL ISIO (CHN152,POLINE,KEYPO,READ,LOKCTL)
CONTIN,
	IF (LOKCTL.EQ.2) GOTO TOOFAR
	IF (LOKCTL.EQ.1) GOTO TOOFAR
	IF (LPONUM.LT.HPONUM) GO TO NXTREC
	IF (LPONUM.GT.HPONUM) GO TO TOOFAR
	IF (LRLNUM.LT.HRLNUM) GO TO NXTREC
	IF (LRLNUM.GT.HRLNUM) GO TO TOOFAR
	GOTO UPDATES
NXTREC,
	LOKCTL = 1
	XCALL IOS (CHN152,POLINE,READ,LOKCTL)
	GOTO CONTIN
CNGORD,
	IF (LINSTS.EQ.'N') GOTO UPDATES
	IF (LCXCDE.EQ.'C'.OR.LCXCDE.EQ.'X') GOTO UPDATES
	GO TO NXTREC

UPDATES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; no updates in this version...			12-20-05 ssq
;update inventory record...
;;;	CALL UPDINV	
;update poline record
;;;	CALL UPDPOL
	RETURN
;------------------------------------------------------------


TOOFAR,
	EOF = 1
	RETURN
;-------------------------------------------------------


;**********************************************************************
;	TABLE SEARCH, INPUT, PRINT, CLOSES, IN-USE, AND EXIT ROUTINES
;**********************************************************************
GETTBL,
	BSMID = 1
	SRCCTL = 2
	XCALL SERCH (CHN153,TABLE,TBLKY,1,5,ORG153,BSMID,SRCCTL,4,
&			6,11,0,0,0,0)
	IF (SRCCTL.EQ.1) TABLE =
	RETURN
;**********************************************************************
PRINT,
	PRTCTL = 78
	XCALL LPOUT (LINCNT,PGCNT,PLINE(3,80),TITLE,'NO HDR',' ',' ',
&		'NO LEGEND',' ',' ',LINCTL,80,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;**********************************************************************
SETUP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; MISC SET UP STUFF
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	PGNUM = 1

	CHANNEL = CHANS

	LOKCTL = 1
	XCALL IO (CHN041,ITMCTL,1,READ,LOKCTL)

;open itmmas in update mode...
;;;	CLOSE CHN041
;;;
;;;	SWITCH = 5
;;;	XCALL FILES (CHN041,'U',041,SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN

	HEADER = POHD		;CURRENT HEADER
	SPPO = HPONUM
	SFAX = HPONUM

	OPEN(14,O,SPLFIL)

	XCALL ASCII (27, E_ESC)
	M_ESC = E_ESC

	XCALL RDATE (TODAY)
	XDATE(1,4) = TODAY(1,4)
	XDATE(5,6) = 20
	XDATE(7,8) = TODAY(5,6)
	TODAY = XDATE

	LOKCTL = 1
	XCALL IO (CHN153,DUMTBL,1,READ,LOKCTL)

	LOKCTL = 1
	XCALL IO (CHN011,DUMVEN,1,READ,LOKCTL)


	NAME = HVENDR
	BSMID = 1
	SRCCTL = 1
	XCALL SERCH (CHN012,VENIDX,HVENDR,1,4,ORG011,BSMID,SRCCTL,4,6,10,
&			0,0,0,0)
	IF (SRCCTL.NE.0 .OR. IRC011.LE.0) RETURN
	LOKCTL = 1
	XCALL IO (CHN011,VENDOR,IRC011,READ,LOKCTL)

	XCALL OECO(CHN182,COINFO)	;SSQ 3-11-04
	RETURN
;----------------------------------------------------

END



