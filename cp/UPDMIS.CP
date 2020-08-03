;UPDMIS - PROGRAM TO DO MISC THINGS IN INVOICE POSTING
;
;	such as Freight Summary Report
;	4/26/96: ADDED B/L summary & BLHEAD status code update 
;		 (3=reported on freight summary, eligible to move to hist file)
;
;
;	MODS: 3-NOV-88		BY: DDDD
;

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


RECORD ORDHDR
		.INCLUDE 'DEF:RD044A.DEF'
;;;RECORD ORDLIN
;;;		.INCLUDE 'DEF:RD045A.DEF'

RECORD	COPTBL
		.INCLUDE 'DEF:RD182A.DEF'

RECORD BLHEAD
		.INCLUDE 'DEF:RD178A.DEF'
RECORD HDR1
		,A50,'INVOICE      ------ CUSTOMER ---------------------'
		,A21,'---------------------'
RECORD HDR2
		,A50,'NUMBER       NO.    NAME                        SH'
		,A50,'IP TO NAME              ---- FREIGHT -----   SHP D'
		,A14,'ATE   SHIP VIA'
RECORD HDR3
		,A50,'      B/L #                                       '
		,A50,'                         BIL/LAD   INVOICE   B/L D'
		,A32,'ATE   B/L CARRIER     PRO NUMBER'
RECORD LEG1
		,A33,'FOR INVOICE POSTING BATCH DATED: '
	LBATDT	,A10
RECORD ASHIPNO
	AORDNO	,A6
		,A1,'.'
	AHDRN	,A1
		,A1,'.'
	ASHPN	,A2

RECORD	VARS
	REDFIL	,A14
	CLCTL	,D1
	CHN182	,D2,	18
	SCAC	,A4
	LOKCTL	,D1
	V	,D1
	SWITCH	,D1
	SRCCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	BSMID	,D5
	NUMMSK	,A6,'XXXXXX'
	DATMSK	,A8,'XX/XX/XX'
	DLRMSK	,A10,'ZZ,ZZZ.XX-'
	BSEND	,D5
	PLINE	,A132
	LINCNT	,D2,60
	PGCNT	,D3
	LPSW	,D1
	RPTNUM	,D3
	PRTTYP	,A1
	TITLE	,A39,'OUTGOING FREIGHT AND B/L SUMMARY REPORT'
	LPARG	,D1
	SPLFIL	,A14
	TOTFRT	,D7,0000000
	TOTBLF	,D7,0000000
	LEG2	,A6,'NO LEG'
	LEG3	,A40,'NO LEG'
RECORD	,X
	ALOKCTL	,A1
PROC
	XCALL TERID (V)
;--------------------------------------------------
	XCALL FFILE (44, REDFIL, CLCTL)
	REDFIL(5,14) = 'INVOIC.WRK'
	OPEN (8, I, REDFIL)
;--------------------------------------------------

	XCALL FILES (4,'SI',44,5)		;ORDHDR - ORDER HEADER FILE
;;;	XCALL FILES (5,'SI',45,5)		;ORDLIN - ORDER LINE FILE
	XCALL FILES (28,'SU',178,5)		;BLHEAD - B/L HEADER FILE
	XCALL FILES (18,'SI',182,5)		;COPTBL - COP TABLES FILE

OPENLP,
	LPSW = 4	; MAY BE AUTO-SPOOLED
	SPLFIL (5,6) = 'E2'
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0)
	BEGIN
	 XCALL MESAG('CANNOT ABORT THIS PRINTOUT - PLEASE RESELECT PRINTER',2)
	 GOTO OPENLP
	END
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)

BEGIN1,
	XCALL OUTPT (1,1,1,TITLE,V)
BEGIN,
	LOKCTL = 1
	XCALL IOS (8,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.NE.0) GOTO EOF
	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)
	IF (LOKCTL.NE.0) GOTO BEGIN
	IF (OCUSNM.EQ.']]]CANCEL') GOTO BEGIN
	IF (OFLAG.NE.2) GOTO BEGIN
	IF (OFRGHT.NE.0.OR.OCLPPD.EQ.'P') CALL PRTIT
	GOTO BEGIN
PRTIT,

	SCAC = OSCAC
	CALL GET_SCAC

	XCALL DATE8(OINVDT, D_OUT, D_OUTR, LBATDT, D_SW)
;;;	LBATDT = OINVDT,DATMSK
	PLINE (1,6) = OINVNO,NUMMSK
	PLINE (13,18) = OCUSNO,NUMMSK
	PLINE (21,45) = OCUSNM
	PLINE (49,78) = OSHPNM
	PLINE (84,93) = OFRGHT,DLRMSK	;XX,XXX.XX-

	XCALL DATE8(OSHDAT, D_OUT, D_OUTR, D_FMT, D_SW)
	PLINE (96,105) = D_FMT
;;;	PLINE (107,121) = SHPVDS
	PLINE (107,121) = SC_NAME
	IF (LINCNT+3.GE.60) LINCNT = 62
	CALL PRINT
	TOTFRT = TOTFRT + OFRGHT
	CALL GETBL
	XCALL LINFD (1)
	INCR LINCNT
	RETURN
;;;
;	GET B/L FOR SUMMARY INFORMATION
;;;
GETBL,
	BHORDN = OORDNO
	LOKCTL = 1
	XCALL ISIO (28,BLHEAD,BHORDN,READ,LOKCTL)
	IF (LOKCTL.OR.BHORDN.NE.OORDNO) RETURN
	DO BEGIN
	    SCAC = BHSCAC
	    CALL GET_SCAC
	    AORDNO = BHORDN
	    AHDRN = BHHDRN
	    ASHPN = BHSHPN
	    PLINE (7,11) = ASHIPNO(7,11)
	    PLINE (74,83) = BHFRTAM,DLRMSK

	    XCALL DATE8(BHBILD, D_OUT, D_OUTR, D_FMT, D_SW)
	    PLINE (96,105) = D_FMT
	    PLINE (107,121) = SC_NAME
	    PLINE (107,121) = SC_NAME
	    PLINE (123,132) = BHPRON
	    CALL PRINT
	    TOTBLF = TOTBLF + BHFRTAM
	    BHSTATS = 4				;STATUS 4 = POSTING IN PROCESS
	    LOKCTL = 1
	    XCALL ISIO (28,BLHEAD,KEY178,WRITE,LOKCTL)
	    IF (LOKCTL.NE.0) 
	      BEGIN
	      XCALL OUTPT (23,1,1,ALOKCTL,1)
	      DISPLAY (15,':  ',BLHEAD(1,9))
	      XCALL MESAG ('PROBLEM WRITING RECORD STATUS',2)
	      END
	  LOKCTL = 1
	  XCALL IOS (28,BLHEAD,READ,LOKCTL)
	END UNTIL (LOKCTL.OR.BHORDN.NE.OORDNO)
	RETURN

PRINT,
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,HDR2,HDR3,
&		LEG1,LEG2,LEG3,0,132,0,1,LPSW,RPTNUM,PRTTYP)
	RETURN

;-----------------------------------------------------------
GET_SCAC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; COP TABLE SCAC LOOK-UP
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'SC'
	TBLKEY = SCAC
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		SC_NAME = "* NOT ON FILE *"
		END
	RETURN
;-----------------------------------------------------

;-----------------------------------------------------------
EOF,
	PLINE (84,92) = '---------'
	CALL PRINT
	PLINE (59,73) = 'TOTAL FREIGHT: '
	PLINE (74,83) = TOTBLF,DLRMSK
	PLINE (84,93) = TOTFRT,DLRMSK
	CALL PRINT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; following removed 10-16-98 ssq
;;;CHKBL,
;;;	LEG3 = LEG1
;;;	LEG2 =
;;;	LEG1 = 'B/L OPEN STATUS FLAG SUMMARY'
;;;	LINCNT = 62
;;;	READ (28,BLHEAD,^FIRST) [EOF=ENDOFF, LOCK=CHKBL]
;;;BLLP,
;;;	DO BEGIN
;;;	  IF (BHSTATS.NE.0.AND.BHSTATS.NE.4)
;;;	  BEGIN
;;;	    AORDNO = BHORDN
;;;	    AHDRN = BHHDRN
;;;	    ASHPN = BHSHPN
;;;	    PLINE (1,11) = ASHIPNO
;;;	    PLINE (21,45) = BHCUSNM
;;;	    PLINE (49,78) = BHSHPNM
;;;	    PLINE (96,103) = BHSHPD,DATMSK
;;;	    CALL PRINT
;;;	    PLINE (15,20) = 'STAT: '
;;;	    USING BHSTATS SELECT
;;;	      (0),	PLINE(21,31) = 'X OPEN '
;;;	      (1),	PLINE(21,31) = 'X SELECTED'
;;;	      (2),	PLINE(21,31) = 'X PRINTED'
;;;	      (3),	PLINE(21,31) = 'X COMPLETE'
;;;	      (4),	PLINE(21,31) = 'X POSTING'
;;;	      (9),	PLINE(21,31) = 'X HISTORY'
;;;	      () ,	PLINE(21,31) = 'X UNDEFINED'
;;;	    ENDUSING
;;;	    PLINE (21,21) = BHSTATS,'X'
;;;	    PLINE (96,103) = BHBILD,DATMSK
;;;	    PLINE (107,121) = SC_NAME
;;;	    PLINE (123,132) = BHPRON
;;;	    CALL PRINT
;;;	    XCALL LINFD (1)
;;;	    INCR LINCNT
;;;	  END
;;;	  LOKCTL = 1
;;;	  XCALL IOS (28,BLHEAD,READ,LOKCTL)
;;;	END UNTIL (LOKCTL.NE.0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ENDOFF,
	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
	CLOSE 8
	CLOSE 9
	CLOSE 5
	CLOSE 4
	XCALL PGCHN ('CP:PSTINV',1)
END 	
;INVOICE      ------ CUSTOMER --------------------------------------------
;NUMBER       NO.    NAME                        SHIP TO NAME                         FREIGHT   SHP DATE   SHIP VIA
;      B/L #                                                                          CHARGED   B/L DATE   B/L CARRIER     PRO NUMBER
;
;XXXXXX       XXXXX  XXXXXXXXXXXXXXXXXXXXXXXXX   XXXXXXXXXXXXXXXXXXXXXXXXX   XX,XXX.XX   XX/XX/XX   XXXXXXXXXXXXXXX
;      .X.XX                                                                                         XX/XX/XX   XXXXXXXXXXXXXXX
;      .X.XX                                                                                         XX/XX/XX   XXXXXXXXXXXXXXX

