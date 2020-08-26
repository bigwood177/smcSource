SUBROUTINE SCRN1	
;
;	14-dec-01 ssq: 	don't fax mod.
; 5-30-18 ssq: make cusmas isam
;
;
;		HANDLES FIRST SCREEN OF ADD MODE OF ORDER ENTRY
;
	ORDNO	,D
	DISC	,D
	OOLOC	,A
	CUSNM	,A
	INET	,A	;SSQ 5-4-05 I = internet order
	INXCTL	,D
	TXFLG	,A
	CUSTP2	,A
	CCORD	,A
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	PCODES	,A
;;;	PDISCS	,D
;;;	PMAX	,D
;;;;;;;;;;;;;;;;;;;;;;;;;;;
	LSTDTE	,D
	FULL	,D
	V	,D
;;;	CREDIT	,D
	CONTR	,D		;1 = USE CONTRACTOR PRICE
	ODATE	,D
	S_ESTIMA	,D
	ABORT	,D			;USER ABORT FLAG
	CUSTCD	,A			;FAX/
	DISTR	,D1			;6/12/07 1=distributor

	.DEFINE POOLSIZE	,25000
	.DEFINE WNDCHNL		,15
	.DEFINE MAXWINS		,10
	.INCLUDE 'WND:WINDOWS.DEF'

EXTERNAL FUNCTION
	GU_NAME	,A

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR

record	ch_err
	.include 'def:ch_err.def'	;2-8-17


RECORD	ORDCM2
	.INCLUDE 'DEF:RD135A.DEF'	;5-20-15

RECORD	EMAILC
	.INCLUDE 'DEF:RD084A.DEF'

RECORD	CRHOLD
	.INCLUDE 'DEF:RD195A.DEF'

RECORD	ROLO
	.INCLUDE 'DEF:RD183A.DEF'

RECORD	POPAR
	.INCLUDE 'DEF:OEPOP.DEF'

RECORD	TMPCUS
	.INCLUDE 'DEF:RD139A.DEF'	

RECORD	CCINFO
	.INCLUDE 'DEF:RD137A.DEF'

RECORD	CCTRAN
	.INCLUDE 'DEF:RD138A.DEF'

RECORD	WVARS
	SHIP_SRCH	,D1	;1 = FOUND SERCH KEY 
	SRCH_KEY	,A30	;SHIP-TO SEARCH KEY
	A35		,A35
	disp_em		,d1	;display <F3> = email
	cash_cust	,d1	;is cust# 2,3,5,10
	wstat	,d1
	POBOX	,D1
	TODAY	,D6
	CHN101	,D3,	101
	CHN195	,D2,	40		;OPEN & CLOSE IN THIS ROUTINE	
	CHN182	,D2,	17		;FROM ORDADD
	CHN183	,D2,	18		;ROLO2
	CHN135	,D3,	135		;ORDCM2 5-20-15
	CHN137	,D2,	29
	CHN138	,D2,	50
	CHN084	,D2,	84
	MAXTYP	,D2,	05
	W_ID	,D4
	WND_1	,D4

RECORD	WN_NAME
		,A5,	'SCRN1'
	WN_TNMBR,D4

RECORD	FUNKEY
	.INCLUDE 'DEF:FUNKEY.DEF'

RECORD	MISCUS
		.INCLUDE 'DEF:RD001M.DEF'
RECORD CUSMAS  		
		.INCLUDE 'DEF:RD001A.DEF'
RECORD ,X		
		.INCLUDE 'DEF:RD001B.DEF'
;;;RECORD CUSIDX  		
;;;		.INCLUDE 'DEF:RD002A.DEF'
RECORD TORDHD   	
		.INCLUDE 'DEF:RD044A.DEF'
RECORD TMPHDR		
		.INCLUDE 'DEF:RD044E.DEF'
RECORD COPCTL
		.INCLUDE 'DEF:RD060A.DEF'
RECORD CUSALP
		.INCLUDE 'DEF:RD166A.DEF'
RECORD ARTERM
		.INCLUDE 'DEF:RD170A.DEF'
RECORD ARTCTL
		.INCLUDE 'DEF:RD170B.DEF'
RECORD SHIPTO
		.INCLUDE 'DEF:RD171A.DEF'
RECORD SHPCTL
		.INCLUDE 'DEF:RD171B.DEF'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

;;;record	mail_rec
;;;		,a2	;E2
;;;	mr_cust	,d6
;;;	mr_key	,d4
;;;		,a5
;;;		,a30
;;;	mr_fnam	,a10
;;;	mr_lnam	,a15
;;;	mr_cell	,d10
;;;	mr_mail	,a65

record	mail_rec
		,a2	;E2
	mr_cust	,d6
	mr_key	,d3
		,a6
		,a30
	mr_dflt	,d1
	mr_fnam	,a10
	mr_lnam	,a15
	mr_cell	,d10
	mr_mail	,a64

record	mail_fmt
	mf_text	,a60

RECORD TMPAD3
	TMPCTY	,A15
		,A1
	TMPSTA	,A2
		,A2
	TMPZIP	,A10
RECORD SKEY
	XCUSNO	,D6
	SHPNO	,D4
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
RECORD MESAGE
		,A5,'THE "'
	FILNAM	,A6
		,A18,'" FILE IS NOW FULL'


RECORD	VARS
	savmis	,d6	;need new email and ship-to if miscus changes
	bad_tmp_cus	,d1
	A30	,A30
	cr_flag	,d1	;number of 5's in crdlmt...
	A6	,A6
	UNAME	,A25
	CMPCOD	,A3
	FK_DESC	,A48
	XCUST	,D6	;CUST # FOR CCINF
	OPT	,D1	;OPTION FOR CCINF
	C_WND	,D4	;WINDOW FOR CCINF
	LNAM	,A25
	SNAM	,A12
	SINT	,A3
	LEN	,D4
	UPDATE	,D1
	R_TYPE	,D1
	TSTAT	,D1
	CHN166	,D2,16
	START_NAME	,A25
	F_KEY	,D3
	SYSTEM	,D1
	DCHAR	,D3
	TCHAR	,D3
	FIRST	,D1,1
	L	,2D1
	OPTION	,D1
	ENTRY	,A36
	CNGCTL	,D1
	WHATNO	,D2
	DECMAL	,D18
	NXTORD	,D6
	TMPORD	,D6
	XORDR	,D6
	MASK	,A6,'XXXXXX'
	ORGCUS	,D5
	DEFFLG	,D1
	KEY   	,A6
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	CTR	,D2
	BLANKS	,A25
	MAXINV	,D5
	ALPHA	,A8
	COUNT	,D1
	COL2	,D2
	ROW2	,D2
	READ	,D1,	0
	WRITE	,D1,	1
	STORE	,D1,	2
	DELETE	,D1,	3
	LOKCTL	,D1
	SWITCH	,D1
	FILENM	,A14
		,A5
	ESTIMA	,D1
	I	,D2

PROC
	XCALL WHO(CMPCOD)	;which company ssq 3/2/04
	UNAME = %GU_NAME	;4-26-07 ssq
	UPCASE UNAME

	CLEAR ABORT
	CLEAR ORDCM2		;SSQ 7-27-15
	CLEAR TORDHD		;SSQ 7-12-00
	clear c_wnd		;cc window number
	CONTR = 1		;SSQ 9-16-04

	LOKCTL = 0
	XCALL IO (3,COPCTL,1,READ,LOKCTL)
	IF (LOKCTL) GO TO ABORT_NW
	UNLOCK 3

	CALL INIT_WINDOW
	ESTIMA = S_ESTIMA
	TMPORD = ORDNO

;;;DISPLA,
	INXCTL =
	CNGCTL =
	disp_em =
	XCALL OE1W (DSTFLG,W_ID)
	IF (ESTIMA.EQ.1) XCALL W_DISP(WND_1,WD_POS,2,10,'*ESTIMATE*')
	IF (CCORD.EQ.'C') XCALL W_DISP(WND_1,WD_POS,2,10,'*CREDIT CARD*')
	IF (INET.EQ.'I')  XCALL W_DISP(WND_1,WD_POS,2,10,'*INTERNET*')
	DEFFLG =

	CTL = '04,16,06,00,#X'
	ENTRY(1,6) = ORDNO,	'ZZZZZX' [LEFT]
	XCALL W_DISP(WND_1, WD_POS, ROW, COL, ENTRY(1,6))

;---------------------------------------------------
	OCCRD = CCORD		;CC ORDER?
	ORDTYP = INET		;I = internet order
;---------------------------------------------------

DATE,
	CTL = '04,38,08,00,D '
	OORDDT = LSTDTE		;SSQ 6-6-01

	DECMAL(1,8) = OORDDT
	ODATE = OORDDT
	CALL DSPDTE
	GO TO (ANYCNG), CNGCTL
PDATE,
	CTL = '04,68,08,00,D '
	IF (ESTIMA) 
	BEGIN
	  XCALL W_DISP(WND_1,WD_POS,4,53,'EST DEL DAYS   ')
	  TYPE = '# '
	END
	CALL INPUT
	GOTO (DISPLA), INXCTL
	OPROMD = ENTRY
	GOTO (ANYCNG), CNGCTL
CUSNUM,
	clear cr_flag		;3-15-10
	clear cash_cust		;2-27-12

	XCALL W_DISP(WND_1,WD_POS,24,2,WD_CLR,WDC_EOL,'<F1> = ALPHA LOOK-UP')
	CTL = '06,16,06,01,# '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	USING F_KEY SELECT
	(F_01),	BEGIN
		CALL ALPHA_LOOKUP
		XCALL W_DISP(WND_1,WD_POS,24,1,WD_CLR,WDC_EOL)
		GOTO (CUSNUM,CUSNUM),INXCTL
		XCALL W_DISP(WND_1,WD_POS,6,16,ENTRY(1,6) )
		END

	(UP_ARO), GOTO PDATE
	ENDUSING

	XCALL W_DISP(WND_1,WD_POS,24,1,WD_CLR,WDC_EOL)
	OCUSNO = ENTRY
	if (ocusno.eq.3 .and. estima.ne.1)
		begin
		xcall olmsg (wnd_1,24,'QUOTES ONLY FOR CUSTOMER 3',2)
		goto cusnum
		end

	XCALL ISIO (6, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO BADCUS
	
;;;	LOKCTL = 1
;;;	XCALL IO (6,CUSMAS,1,READ,LOKCTL)
;;;	ORGCUS = ORG001
;;;	BSEND = ORGCUS
;;;;;;	PMAX = NUMDSC
;;;	KEY = OCUSNO, MASK
;;;	XCALL SERCH (7,CUSIDX,KEY,1,6,BSEND,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	GO TO (BADCUS), SRCCTL
;;;	LOKCTL = 1
;;;	XCALL IO (6,CUSMAS,IRC001,READ,LOKCTL)

	IF (CUSCD.EQ.'**') GOTO BADCUS

	XCALL W_DISP(WND_1,WD_POS,6,24,NAME)
	IF (CMPCOD.EQ.'SMC' .AND. SLSMAN.EQ.50)
		BEGIN
		XCALL W_DISP(WND_1,WD_POS,24,1,'ROCKFORD CUSTOMER - CONTINUE WITH ORDER?')
		XCALL WINPT(W_ID,24,43,01,01,'YN',ENTRY,INXCTL)
		GOTO (CUSNUM),INXCTL-1
		END


	a6 = crdlmt, 'XXXXXX'
	if (%instr(1, a6, '6') )
	then	distr = 1
	else	distr = 0

;moved here 11-23-16
;2-15-17: moved back below in case cust is changed from one that was on-hold to one that isn't
;;;	a6 = crdlmt, 'XXXXXX'		
;;;	if (%instr(1, a6, '5') )	call on_hold	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 11-7-16 per jess - just need one "6" to be a dist... 
;;;	if ((cmpcod.eq.'SMC'.or.cmpcod.eq.'TST').and.(uname.ne.'BILLN'.and.%instr(1,a6,'666')) )
	if ((cmpcod.eq.'SMC'.or.cmpcod.eq.'TST').and.distr.eq.1 )

		BEGIN
		XCALL W_DISP(WND_1,WD_POS,24,1,'Distributor, Refer to Dist. Rep - Continue with order?')
		XCALL WINPT(W_ID,24,58,01,01,'YN',ENTRY,INXCTL)
		GOTO (CUSNUM),INXCTL-1
		END
		
	using cusno select		;2-27-12 ssq
	(2,3,5,10),	cash_cust = 1
	(),		cash_cust = 0
	endusing

	CUSTP2 = CUSCD
	OCUSNM = NAME
	OCUSCD = CUSCD
	TXFLG = TAXFLG
	OTAXFL = TAXFLG		;SSQ 4/11/02
; change taxcode from IL to ILR for Rockford orders, unless code is "IRL"
	IF (OORDNO.LT.300000 .AND. OTAXFL(1,2).EQ.'IL' .AND. 
&		OTAXFL(3,3).NE.'N') OTAXFL(3,3) = 'R'

	xcall w_disp (wnd_1, wd_pos,5,65,otaxfl)	;2-27-12
	xcall w_updt

;------------------------------------------------
; not installed
goto skip_oc_nbr
; get contact info every time:
;	using ocusno select
;	(2,3,5,10),	nop
;	(),	begin
;		clear oc_nbr
;		do begin
;		   xcall cont (ocusno, oc_nbr)
;		   end
;		until (oc_nbr.gt.0)
;		end
;	endusing
;	call disp_cont
;------------------------------------------------
skip_oc_nbr,

	CTL = '06,68,06'			;;;
	DECMAL = CRDLMT				;;; ADDED DS 1/7/87
	CALL DSPNUM	
			;;;
	if (distr .and. cr_flag.eq.0) goto skip_c_msg	;11-23-16 don't display if only 6's


	IF (CRDLMT.NE.0.AND.CRDLMT.NE.7) 				;;;
		BEGIN
		XCALL W_DISP(WND_1,WD_POS,24,1,'NOTE CREDIT CODE AND CHECK WITH AR')
		XCALL W_DISP(WND_1,WD_BELL)
		XCALL OLMSG(WND_1,24,'NOTE CREDIT CODE AND CHECK WITH AR',2)	;;;
		END

skip_c_msg,
	ORDSEQ = 01

; display rolodex info if there are sales notes
	read (chn183,rolo,ocusno)[err=bad_rolo]
	unlock chn183				;9-28-16

	XCALL TRIM(R_SALES,LEN)
	IF (LEN .GT. 1)	CALL ROLODEX
	goto finis
bad_rolo,

FINIS,
	CLEAR TMPCUS, CCINFO
	CALL ORG_ORD	;GET ORIGINAL ORDER #
	CALL TEMP_CUS
	goto (cusnum),bad_tmp_cus

	CALL C_CARD		;GET CC INFO IF NEC.
	IF (OPT .EQ. 9) GOTO DISPLA	;ABORT

	ALPHA = CRDLMT
	IF (%INSTR(1,ALPHA,'123') )CALL ASK_CONTR

	IF (OCUSNO .EQ. 10)
		BEGIN
	;;;	CONTR = 2			;11-27-19 CASH CUST NOT CONT
						;8-24-20 - ask, per brandon
		if (cngctl .eq. 1)
			begin
			if (omicus.ne.savmis) 
			then	goto shipto
			else	goto anycng
			end
		GOTO SALMAN			;11-27-19 CASH CUST
		END

; 8-24-20 ssq: ask for cust 10, too
;;;	ALPHA = CRDLMT
;;;	IF (%INSTR(1,ALPHA,'123') )CALL ASK_CONTR


;;;	GO TO (ANYCNG), CNGCTL
	GO TO SALMAN
BADCUS,
	XCALL OLMSG(WND_1,24,'CUSTOMER NOT FOUND',1)
	GO TO CUSNUM
BADSHV,
	XCALL OLMSG(WND_1,24,'SHIP-VIA CODE NOT FOUND',1)
	GO TO SHIPV
BADTRM,	
	XCALL OLMSG(WND_1,24,'TERM CODE NOT FOUND',1)
	GO TO TERMS
BADSHP,
	XCALL OLMSG(WND_1, 24,'SHIP-TO NUMBER NOT FOUND',1)
	GO TO SHIPTO
FULL,
	UNLOCK 4
	FILNAM = 'ORDHDR'
	XCALL OLMSG (WND_1, 24,MESAGE,1)
	FULL = 1
	RETURN

ASK_CONTR,	;;;;;;;;;;;;;;;;
	XCALL W_DISP(WND_1,WD_POS,24,1,WD_CLR,WDC_EOL,'CONTRACTOR?')
	XCALL WINPT(W_ID,24,14,01,01,'YN',ENTRY,INXCTL)
	CONTR = INXCTL
	RETURN
;-------------------------------

SALMAN,
	CTL = '08,16,02,01,N '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	IF (F_KEY .EQ. UP_ARO) GOTO CUSNUM
	OSLMAN = ENTRY
	IF (ENTRY.EQ.BLANKS.AND.CNGCTL) GO TO SALMAN
	IF (ENTRY.EQ.BLANKS) OSLMAN = SLSMAN

	XCALL SREP(OSLMAN,LNAM,SNAM,SINT)
	IF(LNAM.EQ.BLANKS .OR. LNAM.EQ.']]]]]]') 
		BEGIN
		XCALL BEEP
		GOTO SALMAN
		END
	XCALL W_DISP(W_ID,WD_POS,ROW+1,7,SNAM)
	XCALL W_UPDT
	DECMAL = OSLMAN
	CALL DSPNUM
	GO TO (ANYCNG), CNGCTL
LOCA,
	CTL = '08,35,01,00,A '
;;;	CALL INPUT
;;;	GOTO (DISPLA), INXCTL

	CLEAR ENTRY		;SSQ 6-6-01

	IF (ESTIMA.AND.ENTRY(1,1).EQ.' ') ENTRY(1,2) = 'E'
	IF (ENTRY(1,1).EQ.' ') ENTRY(1,1) = 'O'
	OLOC = ENTRY(1,1)
	XCALL W_DISP(WND_1,WD_POS,ROW,COL,OLOC)
	IF (OLOC.NE.'E'.AND.OLOC.NE.'O') GOTO LOCA
	IF (OLOC .EQ. 'E')
	THEN	BEGIN
		XCALL W_DISP(WND_1,WD_POS,23,3,'21. ATTN:')
		XCALL W_DISP(WND_1,WD_POS,23,14,OATTN)
		END
	ELSE	XCALL W_DISP(WND_1,WD_POS,23,1,WD_CLR,WDC_EOL)
	XCALL W_UPDT
	if (ocusno.eq.3 .and. oloc.eq.'O')
		begin
		xcall olmsg (wnd_1,24,'QUOTES ONLY FOR CUSTOMER 3',2)
		goto loca
		end

	GOTO (ANYCNG), CNGCTL
	GOTO SHIPV
GDLOC,
	OLOC = ENTRY
	GOTO (ANYCNG), CNGCTL
SHIPV,
	XCALL W_DISP(WND_1,WD_POS,9,53,'               ')
	CTL = '08,53,04,00,A '
	CALL INPUT
	GOTO (DISPLA),INXCTL
	IF (ENTRY(1,4) .EQ. '    ') 
		BEGIN
		ENTRY(1,4) = 'WC'
		XCALL W_DISP(WND_1,WD_POS,8,53,'WC')
		END
	OSCAC = ENTRY(1,4)
	CALL GET_SCAC
	IF (LOKCTL) GOTO SHIPV
	XCALL W_DISP(WND_1,WD_POS,9,53,SC_NAME)
	XCALL W_DISP(W_ID,WD_POS,8,59,'8. PO #')

	GO TO (ANYCNG), CNGCTL
PONUM,
	CTL = '08,68,10,00,AT'
	CALL INPUT
	GOTO (DISPLA), INXCTL
	IF (F_KEY.EQ.F_01 .OR. INXCTL.EQ.3)
		BEGIN
		CALL OMETRO		;GET METRO PROJ #
		IF(CNGCTL.EQ.0)GOTO PONUM
		ENTRY = OPONO
		XCALL W_DISP(W_ID,WD_POS,8,68,OPONO)
		END

	OPONO = ENTRY
	OATTN = OPONO		;SSQ 11-7-03
	XCALL W_DISP(WND_1,WD_POS,23,14,OATTN)
	GOTO (ANYCNG), CNGCTL
DISC,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3-14-97 DISABLE THIS CODE...
	ODISC =

	GOTO (ANYCNG), CNGCTL
TERMS,
	OTERMS = TERMS 	; DEFAULT TO CUSTOMERS TERMS
	IF(CCORD .EQ. 'C') OTERMS = 'C'		;CREDIT CARD

	USING OTERMS SELECT
	('4','5','6','C','O'), XCALL OLMSG (WND_1, 24, 'REQUIRES PAYMENT IN ADVANCE',1)
	(), IF (CRDLMT .EQ. 5555) XCALL OLMSG (WND_1, 24, 'REQUIRES PAYMENT IN ADVANCE',1)
	ENDUSING

;;;	OTRMSD = 				;3-21-2011

COLPPD,
;;;	IF(CNGCTL.EQ.0 .AND. SC_PPD.EQ.'Y')
	IF(CNGCTL.EQ.0)
		BEGIN
		IF(SC_PPD.EQ.'Y')
		THEN	ENTRY = 'P'
		ELSE	ENTRY = 'C'
		GOTO PPD
		END

	CTL = '10,53,01,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	IF (ENTRY.EQ.' ') ENTRY = 'C'
PPD,
	OCLPPD = ENTRY
	XCALL W_DISP(WND_1,WD_POS,10,53,OCLPPD)
	IF (OCLPPD.NE.'C'.AND.OCLPPD.NE.'D'.AND.OCLPPD.NE.'P') GO TO COLPPD
	GO TO (ANYCNG), CNGCTL
JOBNUM,
	CTL = '10,68,10,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	OJOBNO = ENTRY
	GOTO (ANYCNG), CNGCTL
SHIPTO,
	if (CUSCD(1,1).EQ.'E') disp_em = 1
;;;	if (CUSCD.EQ.'EM') disp_em = 1

	SWITCH = 5
	if (cash_cust)
	then	XCALL FILES (8,'SI',102,SWITCH)
	else	XCALL FILES (8,'SI',171,SWITCH)

	XCALL W_DISP(WND_1,WD_POS,13,12,WD_CLR,WDC_EOL)
	XCALL W_DISP(WND_1,WD_POS,14,12,WD_CLR,WDC_EOL)
	XCALL W_DISP(WND_1,WD_POS,15,12,WD_CLR,WDC_EOL)
	XCALL W_DISP(WND_1,WD_POS,16,12,WD_CLR,WDC_EOL)
	CTL = '12,16,04,00,N '
	FOR I = 1 STEP 1 UNTIL 6  DO  IF (CRDLMT(I,I).EQ.7) MIN = 1
	IF (MIN.EQ.1) XCALL W_DISP(WND_1,WD_POS,12,24,'MUST ENTER A SHIP-TO CODE, OR 0 FOR HOME OFFICE')
	XCALL W_DISP(WND_1,WD_POS,24,1,WD_CLR,WDC_EOL,'<F1> = DISPLAY SHIP-TO LIST')
	XCALL W_DISP(WND_1,WD_POS,24,33,'<F4> = SEARCH')
	CALL INPUT
	IF (MIN.EQ.1) XCALL W_DISP(WND_1,WD_POS,12,24,WD_CLR,WDC_EOL)
	XCALL W_DISP(WND_1,WD_POS,24,1,WD_CLR,WDC_EOL)
	IF (INXCTL)
	BEGIN
	  CLOSE 8
	  GOTO DISPLA
	END

;--------------------
;;;	IF (F_KEY .EQ. F_01) 
	USING F_KEY SELECT
	(F_01, F_04),	BEGIN
			CALL M_SHIPTO
			GOTO(SHIPTO),INXCTL		;ABORT
			XCALL W_DISP(WND_1,WD_POS,ROW,COL,ENTRY(1,4))
			END		
	ENDUSING
;--------------------

	OSHPTO = ENTRY
	IF (ENTRY.EQ.BLANKS.OR.OSHPTO.EQ.0) 
	BEGIN
	  CLOSE 8
	  GO TO DFBLTO
	END
	IF (OSHPTO.EQ.9999) 
	BEGIN
	  CLOSE 8
	  GO TO SHIPNM
	END

	using cash_cust select
	(1),	xcusno = omicus
	(),	xcusno = ocusno
	endusing

;;;	XCUSNO = OCUSNO
	SHPNO = OSHPTO
	XCALL ISIO (8,SHIPTO,SKEY,READ,LOKCTL,wnd_1)
	CLOSE 8
	IF (LOKCTL .NE. 0) GOTO BADSHP

	OSHPNM = SHTONA
	OSHAD1 = SHTOAD(1)
	OSHAD2 = SHTOAD(2)
	OSHAD3 = SHTOAD(3)
	OTAXFL = SHTOTC		;SSQ 4/24/02
; change taxcode from IL to ILR for Rockford orders, unless code is "IRL"
	IF (OORDNO.LT.500000 .AND. OTAXFL(1,2).EQ.'IL' .AND. 
&		OTAXFL(3,3).NE.'N') OTAXFL(3,3) = 'R'

	CALL DSPSHP

	goto chk_cs_mail
	GO TO (ANYCNG), CNGCTL

;;;	GO TO COMENT

DFBLTO,
	IF (T_CUSNO.EQ.ORDNO)
	THEN	BEGIN			;TEMP CUST ENTERED
		OSHPNM = T_NAME
		OSHAD1 = T_ADD1
		OSHAD2 = T_ADD2
		TMPCTY = T_CITY
		TMPSTA = T_STATE
		TMPZIP = T_ZIP
		OSHAD3 = TMPAD3
		END
	ELSE	BEGIN
		OSHPNM = NAME
		OSHAD1 = ADD1
		OSHAD2 = ADD2
		TMPCTY = CITY
		TMPSTA = STATE
		TMPZIP = ZIP
		OSHAD3 = TMPAD3
		END

	CALL DSPSHP
	if (cngctl.eq.1 .and. omicus.ne.savmis) goto chk_cs_mail
	GO TO (ANYCNG), CNGCTL

	goto chk_cs_mail
	GO TO COMENT
SHIPNM,
	using oloc select
	('O'),if(.not.cash_cust) goto (coment,anycng),cngctl+1	
	endusing
	CTL = '13,12,30,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	OSHPNM = ENTRY
	OSHFLG = 'C'			;changed the ship-to
	GO TO (ANYCNG), CNGCTL
SHIPA1,
	using oloc select
	('O'),if(.not.cash_cust) goto (coment,anycng),cngctl+1	
	endusing
	CTL = '14,12,30,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	OSHAD1 = ENTRY
	OSHFLG = 'C'			;changed the ship-to
	GO TO (ANYCNG), CNGCTL
SHIPA2,
; 4-2-12 can change this line per steve m
;;;	using oloc select
;;;	('O'),if(.not.cash_cust) goto (coment,anycng),cngctl+1	
;;;	endusing

	CTL = '15,12,30,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	OSHAD2 = ENTRY
	OSHFLG = 'C'			;changed the ship-to
	GO TO (ANYCNG), CNGCTL
SHIPA3,
	using oloc select
	('O'),if(.not.cash_cust) goto (coment,anycng),cngctl+1	
	endusing

	CTL = '16,12,30,00,A '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	OSHAD3 = ENTRY
	OSHFLG = 'C'			;changed the ship-to
	GO TO (ANYCNG), CNGCTL
;----------------------------------
;6-18-12: cust/ship-to email
chk_cs_mail,
	using ocuscd select
	('EM','E2'),	begin
			if (cash_cust)
			then	xcall con2 (omicus, oshpto, oc_nbr, cash_cust)		;get email address
			else	xcall con2 (ocusno, oshpto, oc_nbr, cash_cust)		;get email address
			call disp_cont
			end
	endusing

	goto (anycng),cngctl
;----------------------------------


COMENT,
;;;	CTL = '19,16,35,00,AT'
	CTL = '19,19,35,00,AT'
	CALL INPUT
	GO TO (DISPLA,DEFCOM,DEFCOM), INXCTL
;5-15-19>>	IF (ENTRY.EQ.BLANKS.AND.CNGCTL.EQ.0) GO TO DEFCOM
	OCOMNT(1) = ENTRY
	GO TO (ANYCNG), CNGCTL
COM2,
	CTL = '20,19,35,00,AT'
	CALL INPUT
	GO TO (DISPLA), INXCTL
	OCOMNT(2) = ENTRY
	GO TO (ANYCNG), CNGCTL
COM3,
	CTL = '21,19,35,00,AT'
	CALL INPUT
	GO TO (DISPLA), INXCTL
	C2_COM = ENTRY
	GO TO (ANYCNG), CNGCTL
	GO TO ARACCT
DEFCOM,
	XCALL W_DISP(WND_1,WD_POS,19,16,OCOMNT(1))
	XCALL W_DISP(WND_1,WD_POS,20,16,OCOMNT(2))
	GO TO (ARACCT,ANYCNG), CNGCTL+1
ARACCT,
	OARACT = DEFARA
;;;	XCALL W_DISP(WND_1,WD_POS,19,71,' ')
;;;	CALL ASSACC
OATTN,
	IF(OLOC .NE. 'E') GOTO ANYCNG
	XCALL W_DISP(WND_1,WD_POS,23,14,OATTN)
	XCALL W_UPDT
	IF (CNGCTL .EQ. 1)
		BEGIN
		CTL = '23,14,20,00,A '
		CALL INPUT
		GOTO(DISPLA),INXCTL
		OATTN = ENTRY(1,20)
		END
	GOTO ANYCNG

ASSACC,
	OARACT = DEFARA
	ALPHA = OARACT, 'XXXX-XXX'
	XCALL W_DISP(WND_1,WD_POS,19,71,ALPHA)
	RETURN
DSPSHP,
	XCALL W_DISP(WND_1,WD_POS,13,12,OSHPNM)
	XCALL W_DISP(WND_1,WD_POS,14,12,OSHAD1)
	XCALL W_DISP(WND_1,WD_POS,15,12,OSHAD2)
	XCALL W_DISP(WND_1,WD_POS,16,12,OSHAD3)
	xcall w_updt
	RETURN
DEFLT1,
	IF (ENTRY.EQ.BLANKS) ENTRY(1,1) = '1'
	XCALL W_DISP(WND_1,WD_POS,ROW,COL,ENTRY(1,1))
	RETURN
CNGBR,
;;;	GOTO (BADCNG,DATE,PDATE,CUSNUM,SALMAN,LOCA,SHIPV,PONUM,DISC,TERMS,
;;;	GOTO (BADCNG,DATE,PDATE,CUSNUM,SALMAN,LOCA,SHIPV,PONUM,BADCNG,BADCNG,
;;;&	      COLPPD,JOBNUM,SHIPTO,SHIPNM,SHIPA1,SHIPA2,SHIPA3,
;;;&	      COMENT,BADCNG,BADCNG,OATTN), WHATNO

	GOTO (BADCNG,DATE,PDATE,CUSNUM,SALMAN,LOCA,SHIPV,PONUM,BADCNG,BADCNG,
&	      COLPPD,JOBNUM,SHIPTO,SHIPNM,SHIPA1,SHIPA2,SHIPA3,
&	      COMENT,COM2,COM3,OATTN), WHATNO
BADCNG,
	XCALL OLMSG(WND_1,24,' ',6)
	GOTO ANYCNG

PROCES,
	CALL CHECK_SHIPTO
	IF (POBOX)
		BEGIN
		XCALL OLMSG(WND_1,24,"Can't ship SPE-DEE or UPS if PO BOX",1)
		GOTO ANYCNG
		END

	LOKCTL = 1
	OFLAG =
	OORDNO = TMPORD

	OKEYDT = OORDDT
	OCONTR = CONTR		;3-21-11

	using ocusno select
	(3,10),	nop
	(),	omicus =	;this is only for cash-cust, incase cust changes
	endusing

	LOKCTL = 1
	XCALL ISIO (4,TORDHD,OORDNO,STORE,LOKCTL)
	IF (LOKCTL.EQ.4) GO TO DUPHDR
	IF (LOKCTL.EQ.5) GO TO FULL

	IF (C2_COM .NE. A35)
		BEGIN
		C2_ORD = OORDNO
		C2_SEQ = 0
		XCALL ISIO (CHN135, ORDCM2, C2_KEY, STORE, LOKCTL)
		END

	CALL C_TRAN

; 11-23-16 moved to top...
; 2-15-17 moved back, orders getting on hold if cust is changed
	a6 = crdlmt, 'XXXXXX'		
	if (%instr(1, a6, '5') )	call on_hold	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ORDNO = OORDNO
	DISC = ODISC
	OOLOC = OLOC
	CUSNM = OCUSNM
	CUSTCD = OCUSCD
	XCALL W_PROC(WP_DELETE,WND_1)
	IF (C_WND) XCALL W_PROC(WP_DELETE,C_WND)
	CLEAR C_WND
	RETURN
;=====================================================

DUPHDR,
	XCALL OLMSG(WND_1, 24,'**RECORD NOT ADDED**PREVIOUSLY ADDED BY ANOTHER USER',1)
	RETURN
displa,
ABORT,
	XCALL W_PROC(WP_DELETE,WND_1)
ABORT_NW,
	INXCTL = 2
	ABORT = 1		;SSQ 7-5-00 USER ABORT
END,
	RETURN

disp_cont,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	xcall w_disp (wnd_1, wd_pos, 7,1, wd_clr, wdc_eol)
	if (oc_nbr .le. 0) return

	call read_cont
	xcall w_disp (wnd_1, wd_attr, attr_set+attr_bold)
	xcall w_disp (wnd_1, wd_pos, 7,1, mf_text(1, %trim(mf_text)) )
	xcall w_updt
	xcall w_disp (wnd_1, wd_attr, attr_set+attr_clr+attr_bold)

	return
;------------------------------------------------------------------

INPUT,
	if (disp_em) XCALL W_DISP(WND_1,WD_POS,24,66,'<F3> = EMAIL')
	XCALL WINPT (W_ID,ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,F_KEY)
	wstat = f_key
	using f_key select
	(f_02),	begin
		call rolodex
		goto input
		end
	(f_03),	begin
		using ocusno select
		(2,3,5,10),	xcall con2 (omicus, oshpto, oc_nbr, cash_cust)
		(.gt.0),	begin
				xcall con2 (ocusno, oshpto, oc_nbr, cash_cust)
		;;;		call disp_cont
				end
		endusing
		call disp_cont

		goto input
		end
	endusing

;;;	IF (F_KEY .EQ. F_02) 
;;;		BEGIN
;;;		CALL ROLODEX
;;;		GOTO INPUT
;;;		END
	RETURN

ANYCNG,
	savmis = omicus
	IF (OCUSCD .EQ. 'NQ') 
;;;	THEN	XCALL W_DISP(W_ID,WD_POS,1,70,"Don't Fax")
	THEN	XCALL W_DISP(W_ID,WD_POS,1,64,"Don't Fax/Email")
	ELSE	XCALL W_DISP(W_ID,WD_POS,1,64,"          ")
	XCALL W_UPDT
	FK_DESC = "<F9> = no fax/email <F3>=Email"
;;;3-4-13	FK_DESC = "<F9> = don't fax"
;;;	FK_DESC = "<F3> = Email"

;;;	IF (OCCRD.NE.' ') FK_DESC(18,31) = '<F1> = CC INFO'
	IF (OCCRD.EQ.'C') FK_DESC(18,31) = '<F1> = CC INFO'

	IF (T_CUSNO.EQ.ORDNO) FK_DESC(33,48) = '<F2> = CUST'
	XCALL WANFK (W_ID,24,CNGCTL,WHATNO,F_KEY,FK_DESC)
;"<F9> = don't fax",)
;12345678901234567890123456789012345678901234567890
	USING F_KEY SELECT
	(F_01),	BEGIN
		CALL CC_CHANGE
		END

	(F_02),	BEGIN
		CALL TEMP_CUS
		END

	(f_03),	begin
		using ocusno select
		(2,3,5,10),	xcall con2 (omicus, oshpto, oc_nbr, cash_cust)
		(.gt.0),	begin
				xcall con2 (ocusno, oshpto, oc_nbr, cash_cust)
				end
		endusing
		call disp_cont

		goto anycng
		end
	(F_03),	BEGIN
		OCUSCD = CUSCD
		GOTO ANYCNG
		END
	(F_05),	BEGIN
		OCUSCD = CUSCD
		GOTO ANYCNG
		END

	(F_09),	BEGIN
		OCUSCD = 'NQ'
		GOTO ANYCNG
		END
	ENDUSING

	GO TO (PROCES,CNGBR,DISPLA), CNGCTL + 1
DSPNUM,
	OPTION = 1
	GO TO CALDSP
DSPDTE,
	XCALL DATE8(DECMAL(1,8), D_OUT, D_OUTR, D_FMT, D_SW)
	XCALL W_DISP(W_ID, WD_POS, ROW, COL, D_FMT)
	XCALL W_UPDT
	RETURN
;;;------------------
;;;	OPTION = 2
;;;	GO TO CALDSP
DSPDLR,
	OPTION = 3
CALDSP,
;;;	XCALL DSPLY(MAX,ROW,COL,DECMAL,OPTION,V)
	XCALL WDSPL(W_ID,MAX,ROW,COL,DECMAL,OPTION)
	XCALL W_UPDT
	RETURN

OMETRO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; GET METRO PROJ. NO.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL W_DISP(W_ID,WD_POS,7,58,'12DPO:')
	CTL = '07,66,12,00,A '

	IF (OMETRO .NE. BLANKS)GOTO OM_DISP

	CALL INPUT
	GOTO (OM_ABORT),INXCTL
	OMETRO = ENTRY(1,12)
	GOTO OM_ABORT

OM_DISP,
	XCALL W_DISP(W_ID,WD_POS,ROW,COL,OMETRO)
	XCALL W_UPDT

	CALL ACCEPT		
	USING TCHAR SELECT
	(21),	BEGIN			;^U
		OMETRO =
		GOTO OMETRO
		END
	(13),	GOTO OM_ABORT		;<CR>
	ENDUSING
	GOTO OM_DISP

OM_ABORT,
;;;	XCALL W_DISP(W_ID,WD_POS,7,58,WD_CLR,WDC_EOL)
;;;	XCALL W_UPDT
	RETURN
;------------------------------------------

ACCEPT,
	XCALL FLAGS (00010000,1)
	XCALL W_DISP(W_ID, WD_ACCEPT, TCHAR)
	IF (TCHAR.EQ.10) GOTO ACCEPT
	XCALL FLAGS (00010000,0)
	RETURN
;-------------------------------------------------------


ROLODEX,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	UPDATE = 1
	R_TYPE = 1	;SALES
	XCALL ROLDX(CUSMAS,UPDATE,3,6,R_TYPE,CHN183)

	RETURN
;-------------------------------------------------------

ORG_ORD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; FREIGHT CLAIM, GET ORIGINAL ORDER #
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (OCUSNO .NE. 999000)
		BEGIN
	;;;	XCALL W_DISP(WND_1,WD_POS,7,4,WD_CLR,WDC_EOL)
		XCALL W_DISP(WND_1,WD_POS,7,58,WD_CLR,WDC_EOL)
		RETURN
		END

	XCALL W_DISP(WND_1,WD_POS,7,4,'   ORIGINAL ORDER')
	CTL = '07,23,06,00,# '
	CALL INPUT
	XORDR = ENTRY(1,6)
	OBIN = XORDR,	'XXXXXX'
	RETURN
;-------------------------------------------------------

TEMP_CUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear miscus
	bad_tmp_cus = 0
	IF(OCUSNO.EQ.3 .OR. OCUSNO.EQ.10 .OR. OCUSNO.EQ.58455)
	THEN	BEGIN
		oordno = ordno			;11-20-19 oecus needs this for miscus...
		XCALL OECUS(TORDHD, TMPCUS)
		if (omicus .le. 0)
			begin
			bad_tmp_cus = 1
			return
			end

		OTAXFL = T_TAXFLG		;SSQ 5/6/02
		; change taxcode from IL to ILR for Rockford orders, unless code is "IRL"
		IF (OORDNO.LT.500000 .AND. OTAXFL(1,2).EQ.'IL' .AND. 
&		OTAXFL(3,3).NE.'N') OTAXFL(3,3) = 'R'
		if (t_name .eq. a30) bad_tmp_cus = 1
		XCALL ISIO (CHN101, MISCUS, OMICUS, READ, LOKCTL)	;READ MISCUS
		unlock chn101
		ocuscd = mi_cuscd
		XCALL W_DISP(WND_1,WD_POS,6,24,WD_CLR,WDC_EOL,OCUSNM)	;FROM OMICUS
		XCALL W_UPDT
		END
	ELSE	BEGIN
		SWITCH = 5
		XCALL FILES(33,'SU',139,SWITCH)
		IF (SWITCH .NE. 9)
			BEGIN 
			XCALL ISIO (33, TMPCUS, OORDNO, READ, LOKCTL)
			IF (LOKCTL.EQ.0) XCALL ISIO (33, TMPCUS, OORDNO, DELETE, LOKCTL)
			END
		CLOSE 33
		END

	RETURN
;-------------------------------------------------------
CC_CHANGE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	OPT = 2
	GOTO CC_CHECK
C_CARD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	OPT = 4				;FIND BY CUST #
	IF (T_CUSNO .GT. 0)
	THEN	BEGIN
	;;;	XCUST = T_CUSNO		;FROM TMPCUS
		XCUST = OCUSNO		;FROM ORDHDR
		CI_ALPH = T_NAME
		END
	ELSE	BEGIN
		XCUST = OCUSNO		;FROM CUSMAS
		CI_ALPH = NAME
		END
CC_CHECK,
;;;	IF (CCORD .EQ. ' ')
	IF (CCORD .NE. 'C')
		BEGIN
		OPT = 0
		RETURN
		END

;----------------------------------------------------------------------
; 3-14-11
; this line of code prevents problem that had been occurring when
; cc window occluded the input area for comments.  only happened if
; rolodex window poped up.  not sure why that caused a problem and 
; not sure why this works...
	wstat = %w_info(WIF_OCLFLG,w_id)	;need this line, too!
	xcall w_proc(WP_REMOVE, W_ID, WP_MOVE,W_ID,0,0)
;----------------------------------------------------------------------
;;;	XCALL CCIN2(C_WND, CHN137, XCUST, CCINFO, OPT)
	IF (CASH_CUST)
	THEN	XCALL CCIN2(C_WND, CHN137, OMICUS, CCINFO, OPT, CASH_CUST)
	ELSE	XCALL CCIN2(C_WND, CHN137, XCUST, CCINFO, OPT, CASH_CUST)

	RETURN
;-------------------------------------------------------

C_TRAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CCORD .NE. 'C') RETURN
	CLEAR CCTRAN
	CT_ORDER = OORDNO
	CT_NUMBR = CI_NUMBR
	CT_TYPE = CI_TYPE
	CT_EXDAT = CI_EXDAT
	CT_CVV = CI_CVV		;6-13-16
	CT_ADDR = CI_ADDR	;6-13-16
	CT_ZIP = CI_ZIP		;6-13-16

	STORE (CHN138, CCTRAN, CT_ORDER)
	;;;XCALL ISIO (CHN138, CCTRAN, CT_ORDER, STORE, LOKCTL)
	RETURN
;-------------------------------------------------------

ALPHA_LOOKUP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; ALPHA CUSTOMER LOOK-UP
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL W_DISP(WND_1,WD_POS,24,2,WD_CLR,WDC_EOL,'CUST NAME:')
	CTL = '24,13,25,00,AE'
	CALL INPUT
	GOTO (ALPHA_RETURN,ALPHA_RETURN),INXCTL
	START_NAME = ENTRY(1,25)
	FIND (CHN166,CUSALP,START_NAME) [ERR=MORE_NAMES]
	CLEAR LOKCTL
MORE_NAMES,
	CALL GET_NAMES
	XCALL TTSTS(TSTAT)
	IF (TSTAT) XCALL W_DISP(WND_1,WD_ACCEPT,DECMAL)
;;;	IF (TSTAT) XCALL W_DISP(WND_1,WD_READS,ENTRY)
	IF (NUMARA .GT. 0)
	THEN	BEGIN
		DLINE = '  CUST #  NAME'
		POP_TITLE = "<CR> = MORE NAMES"
		XCALL OEPOP (POPAR)
		END
	ELSE	BEGIN
		XCALL OLMSG(WND_1,24,'NO MORE NAMES',1)
		INXCTL = 2
		RETURN
		END

	CASE P_ACTION OF
	BEGINCASE
	0:	INXCTL = 2		;<END>
	1:	BEGIN
		DLINE = PARRY(PI)
	;;;	ENTRY(1,6) = DLINE(1,6)
		ENTRY = DLINE(1,6)
		CLEAR INXCTL		
		END
	4:	GOTO MORE_NAMES
	ENDCASE
ALPHA_RETURN,
	RETURN
;------------------------------------------------

GET_NAMES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; GET NEXT 20 NAMES FROM CUSALP
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR I
GN_LOOP,
	XCALL IOS (CHN166,CUSALP,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO GN_EOF
	CLEAR DLINE
	DLINE(1,6) = A_CUSNO,	'ZZZZZZ' [LEFT]
	DLINE(8,32) = A_NAME
	INCR I
	PARRY(I) = DLINE
	
	DLINE (1,25) = A_ADD1
	DLINE (26,46) = A_ADD2
	DLINE (47,61) = A_CITY
	DLINE (63,64) = A_STATE
	DLINE (66,75) = A_ZIP
	P_ADD(I) = DLINE
	IF (I .LT. MAXARA) GOTO GN_LOOP
GN_EOF,
	NUMARA = I
	IF (I.EQ.0 .OR. I.EQ.MAXARA) RETURN
	
	FOR I FROM NUMARA+1 THRU MAXARA	CLEAR PARRY(I)
	RETURN
;------------------------------------------------

M_SHIPTO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SHIP_SRCH = 0
	IF (F_KEY .EQ. F_04)
		BEGIN
		XCALL W_DISP(W_ID,WD_POS,24,1,'SEARCH STRING:')
		XCALL WINPT(W_ID,24,16,30,00,'AE',ENTRY,INXCTL)
		IF (INXCTL .NE. 0) 
			BEGIN
			CLEAR ENTRY
			RETURN
			END
		SRCH_KEY = ENTRY(1,30)
		SHIP_SRCH = 1
		END

	CLEAR ENTRY, INXCTL

	if (cash_cust)
	then	xcusno = omicus
	else	xcusno = ocusno

;;;	XCUSNO = OCUSNO
	SHPNO = 	;;;>OSHPTO
	FIND(8,SHIPTO,SKEY)[ERR=MORE_SHIP]
MORE_SHIP,
	CALL GET_MSHIP
	IF (%TTSTS) XCALL W_DISP(WND_1,WD_ACCEPT,DECMAL)
;;;	IF (%TTSTS) XCALL W_DISP(WND_1,WD_READS,ENTRY)
	IF (NUMARA .GT. 0)
	THEN	BEGIN
		DLINE = '  SHIP  NAME'
		POP_TITLE = "<CR> = MORE SHIP-TO'S"
		XCALL OEPOP(POPAR)
		END
	ELSE	BEGIN
		XCALL OLMSG(WND_1,24,"NO MORE SHIP-TO'S",1)
		XCALL W_DISP(WND_1,WD_POS,24,1,'ADD NEW SHIP-TO?')
		XCALL WINPT(W_ID,24,19,01,01,'YN',ENTRY,INXCTL)
		IF(INXCTL.EQ.1)
		THEN	CALL M_ADD_SHIP
		ELSE	BEGIN
			CLEAR ENTRY
			INXCTL = 1
			RETURN
			END
		XCALL W_DISP(WND_1,WD_POS,24,1,WD_CLR,WDC_EOL)
		END

	USING P_ACTION SELECT
	(0),	INXCTL = 1		;<END>
	(1),	BEGIN			;SELECTED
		DLINE = PARRY(PI)	
		ENTRY = DLINE(1,4)	;SHIPTO
		OSHPTO = ENTRY(1,4)
	;;;	XCALL MSHP (OCUSNO,OSHPTO,2)
	;;;	XCALL MSHP (OCUSNO,OSHPTO,2,TAXFLG)
	;;;	XCALL MSHP (OCUSNO,OSHPTO,2,TAXFLG,OCUSNM)
		IF (CASH_CUST)
		THEN	XCALL MSHP (OMICUS,OSHPTO,2,MI_TAXFLG,MI_NAME, CASH_CUST)
		ELSE	XCALL MSHP (OCUSNO,OSHPTO,2,TAXFLG,OCUSNM, CASH_CUST)
		CLEAR INXCTL
		END
	(2),	CALL M_ADD_SHIP
	(4),	GOTO MORE_SHIP

	ENDUSING

	RETURN
;------------------------------------------------

GET_MSHIP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR I
MS_LOOP,
	XCALL IOS (8,SHIPTO,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO MS_EOF
;;;	USING CASH_CUST SELECT
;;;	(1),	IF (SHCSNO .NE. OMICUS) GOTO MS_EOF
;;;	(),	IF (SHCSNO .NE. OCUSNO) GOTO MS_EOF
;;;	ENDUSING

	IF (LOKCTL.NE.0 .OR. SHCSNO.NE.XCUSNO) GOTO MS_EOF
;;;	IF (LOKCTL.NE.0 .OR. SHCSNO.NE.OCUSNO) GOTO MS_EOF

	USING F_KEY SELECT
	(F_04),	BEGIN
		SHIP_SRCH = 0		;NO MATCH
		IF (%INSTR(1,SHTONA,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (%INSTR(1,SHTAD1,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (%INSTR(1,SHTAD2,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (%INSTR(1,SHTCTY,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (%INSTR(1,SHTST,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (%INSTR(1,SHTZIP,SRCH_KEY(1,%TRIM(SRCH_KEY)))) SHIP_SRCH = 1
		IF (SHIP_SRCH .EQ. 0) GOTO MS_LOOP
		END
	ENDUSING

	CLEAR DLINE
	DLINE(1,4) = SHTONO,	'ZZZZ' [LEFT]
	DLINE(6,32) = SHTONA
	INCR I
	PARRY(I) = DLINE
	
	DLINE (1,25) = SHTAD1
	DLINE (26,46) = SHTAD2
	DLINE (47,61) = SHTCTY
	DLINE (63,64) = SHTST
	DLINE (66,75) = SHTZIP
	P_ADD(I) = DLINE
	IF (I .LT. MAXARA) GOTO MS_LOOP
MS_EOF,
	NUMARA = I
	RETURN
;------------------------------------------------
M_ADD_SHIP,
;;;	XCALL MSHP (OCUSNO,OSHPTO,1,TAXFLG)	;add
;;;	XCALL MSHP (OCUSNO,OSHPTO,1,TAXFLG,OCUSNM)	;add
		IF (CASH_CUST)
		THEN	XCALL MSHP (OMICUS,OSHPTO,1,MI_TAXFLG,MI_NAME, CASH_CUST)
		ELSE	XCALL MSHP (OCUSNO,OSHPTO,1,TAXFLG,OCUSNM, CASH_CUST)
	ENTRY(1,4) = OSHPTO
	CLEAR INXCTL
	RETURN
;------------------------------------------------
on_hold,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	a6 = crdlmt, 'XXXXXX'		;6-12-07 ssq
	if (%instr(1, a6, '5555') ) 	;4-1-15 ssq
		begin
		cr_flag = 4		;cod
		goto oh_fin
		end

	if (%instr(1, a6, '555') ) 
		begin
		cr_flag = 3
		goto oh_fin
		end

	if (%instr(1, a6, '55') ) 
		begin
		cr_flag = 2
		goto oh_fin
		end

	if (%instr(1, a6, '5') ) 
		begin
		cr_flag = 1
		goto oh_fin
		end

oh_fin,
	SWITCH = 5
	XCALL FILES (CHN195, 'SU', 195, SWITCH)

	IF (SWITCH .EQ. 9)
	THEN	XCALL OLMSG(WND_1,24,"CAN'T OPEN CRHOLD",2)	;;;

	ELSE	BEGIN
		CLEAR CRHOLD
		CH_ORD = ORDNO
		CH_FLAG = cr_flag		;ORDER ENTRY credit hold
		XCALL ISIO (CHN195, CRHOLD, CH_ORD, STORE, LOKCTL)
		CLOSE CHN195
		END

; 2-08-17 temp code to log orders written to crhold
	onerror che_opn
	open (26,a,'smc:ch_err.dat')		;
	che_ord = ordno
	che_cus = ocusno
	che_nam = name
	che_pgm = 'scrn1'
	che_cde = crdlmt
	writes (26, ch_err)
	close 26
che_opn,
	offerror
	return
;------------------------------------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	XCALL W_INIT(POOLSIZE,WNDCHNL,MAXWINS)
;;;	xcall u_start("lib:smc_wnd",,,,,,,35)
	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,24,78)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'ORDER ADD',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1
	xcall u_logwnd(wnd_1)

;; POP info...

	MAXARA = 10		;9-19-97 NOT MORE THAN 1 FULL WINDOW
	PLEN = 34
	NUMROW = 10
	WX = 8
	WY = 7
	POP_WID(1,5) = "OEPOP"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "CUSTOMER NAMES"
	RETURN
;-------------------------------------------------------------------

GET_SCAC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; COP TABLE SCAC LOOK-UP
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'SC'
	TBLKEY = OSCAC
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		CLEAR COPTBL
		SC_NAME = "* NOT ON FILE *"
		END
	RETURN
;-----------------------------------------------------

CHECK_SHIPTO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; CHECK SHIP-TO FOR "PO BOX"
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR POBOX

	USING OSCAC SELECT
	('0','4','8','9'),GOTO UPS_SPEEDY
	(),	RETURN		;DON'T CARE
	ENDUSING

UPS_SPEEDY,
	POBOX = 1	;ASSUME CONTAINS "PO " OR "BOX"...
	IF (%INSTR(1,OSHPNM,'PO ') .OR. %INSTR(1,OSHPNM,'BOX')) RETURN
	IF (%INSTR(1,OSHAD1,'PO ') .OR. %INSTR(1,OSHAD1,'BOX')) RETURN
	IF (%INSTR(1,OSHAD2,'PO ') .OR. %INSTR(1,OSHAD2,'BOX')) RETURN
	IF (%INSTR(1,OSHAD3,'PO ') .OR. %INSTR(1,OSHAD3,'BOX')) RETURN

	CLEAR POBOX
	RETURN
;--------------------------------------------------
read_cont,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 12-2-19: for cash_cust chn084 is opened to smc:misemc.smm

	clear emailc
	if (cash_cust)
	then	begin
		close chn084
		switch = 5
		xcall files (chn084, 'SU', 103, switch)
		e_cust = omicus
		end
	else	e_cust = cusno

	e_nbr = oc_nbr

	xcall isio (chn084, emailc, e_key, read, lokctl,wnd_1)
	if (lokctl .ne. 0) clear emailc
	
	clear mail_fmt, mf_text
	mf_text = e_fnam
	mf_text(%trim(mf_text)+2, 60) = e_lnam
	mf_text(%trim(mf_text)+2, 60) = e_mail

	if (cash_cust)
		begin
		close chn084
		switch = 5
		xcall filec ('SMC', chn084, 'SU', 084, switch)		
		end

	return

END
