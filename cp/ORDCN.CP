SUBROUTINE ORDCN
;
;  ORDCN / COP 
;
; (01)	12-11-98 ssq: oeflag necc. reset going from 1 order to next,
;			this caused allocation problems !
; 5-30-18 ssq: make cusmas isam
; 12-4-19 ssq: miscus mod
;
;		ORDER-HEADER CHANGE SUBROUTINE
;
	ORDHDR	,A
	SCUSTP	,A
	INXCTL	,D
	CNGCTL	,D
	INET	,A	;"I" = internet order
	V	,D
	TXFLG	,A
	OEFLAG	,D
	CUSTCD	,A		;FAX/
	distr	,d		;1=cust is a distr
	c_wnd	,d

;4-26-11: if crdlmt does not contain '123' set ocontr = 1 (contractor)
;2-20-14: update arqdat.ism if quote converted to order.

GLOBAL ORDH
RECORD TORDHD   	
		.INCLUDE 'DEF:RD044A.DEF'
ENDGLOBAL


	.include 'wnd:windows.def'

record	ch_err
	.include 'def:ch_err.def'

RECORD	ORDCM2
	.INCLUDE 'DEF:RD135A.DEF'

RECORD	ARQDAT
	.INCLUDE 'DEF:RD083A.DEF'

RECORD	CUSALP
	.INCLUDE 'DEF:RD166A.DEF'

RECORD	FUNKEY
	.INCLUDE 'DEF:FUNKEY.DEF'

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


record	popar
	.include 'def:oepop.def'

RECORD	CRHOLD
	.INCLUDE 'DEF:RD195A.DEF'

RECORD	TMPCUS
	.INCLUDE 'DEF:RD139A.DEF'

RECORD CUSMAS		
		.INCLUDE 'DEF:RD001A.DEF'
RECORD ,X		
		.INCLUDE 'DEF:RD001B.DEF'

RECORD	MISCUS
		.INCLUDE 'DEF:RD101A.DEF'

;;;RECORD CUSIDX		
;;;		.INCLUDE 'DEF:RD002A.DEF'
RECORD COPCTL
		.INCLUDE 'DEF:RD060A.DEF'
RECORD ARTERM
		.INCLUDE 'DEF:RD170A.DEF'
RECORD ARTCTL	,X
		.INCLUDE 'DEF:RD170B.DEF'
RECORD SHIPTO
		.INCLUDE 'DEF:RD171A.DEF'
RECORD SHPCTL	,X
		.INCLUDE 'DEF:RD171B.DEF'


RECORD	EMAILC
	.INCLUDE 'DEF:RD084A.DEF'

RECORD	COPTBL
		.INCLUDE 'DEF:RD182A.DEF'

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

RECORD	CCTRAN
	.INCLUDE 'DEF:RD138A.DEF'

RECORD	C1_LINE
		,A*,	'* Credit Card *   Auth:'
	C1_AUTH	,A6

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

RECORD	ORDFMT
	OF_ORD	,A6
		,A1, '.'
	OF_NUM	,D1


RECORD	SHIP22
	XSHPNM	,A30
	XSHAD1	,A30
	XSHAD2	,A30
	XSHAD3	,A30


record	wvars
	W_ID	,D4
	WND_1	,D4

RECORD	WN_NAME
		,A5,	'ORDCN'
	WN_TNMBR,D4

RECORD	VARS
	savmis	,d6
	CALL_SHPTO	,D1	;12-30-19 FOR CASH CUST
	CHN101	,D3,	101	;12-4-19
	CHN166	,D2,	16
	CHN182	,D2,	17
	CHN195	,D2
	CHN084	,D2,	84
	CHN135	,D3,	135	;5-20-15
	CHN083	,D3
	bad_tmp_cus	,d1	;12-4-19
	A6	,A6
	A30	,a30
	chnlok	,d2,	97
	SHIP_SRCH	,D1
	SRCH_KEY	,A30
	GOT_CM2	,D1
	SAVCM3	,A35		;5-20-15
	disp_em	,d1
	wstat	,d1
	palet1		,d2
	BAD_SHIPTO	,D1
	cash_cust	,d1	;2,3,5,10
	TSTAT	,D1
	START_NAME	,A25
	i	,d6
	cr_flag	,d1
	ON_CC	,D1
	FK_DESC	,A48
	POBOX	,D1
	F_KEY	,D3
	SAVV	,D1
	TCHAR	,D3
	LNAM	,A25
	SNAM	,A12
	SINT	,A3
	TODAY	,D8
	TDATE	,D6
	XORDR	,D6
	ALPHA	,A8
	L	,2D1
	ORDNOA	,A6
	OPTION	,D1
	ENTRY	,A36
	WHATNO	,D2
	TMSARR	,5D2,05,10,15,20,25		;SET TO TERMS DISCOUNTS
						;ALLOWED FOR EACH CUST
	CTR	,D2
	DECMAL	,D18
	KEY   	,A16
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	BLANKS	,A35
	MASK	,A6,'XXXXXX'
	COUNT	,D1
	READ	,D1	,0
	WRITE	,D1	,1
	store	,d1	,2
	delet	,d1	,3
	LOKCTL	,D1
	SWITCH	,D1	;;;
	FILENM	,A14	;;;
	CNGCUS	,D1	,0
	SAVLOC	,A1
;
PROC
	call init_window

DISPLA,
	CNGCUS =
	LOKCTL = 1
	OEFLAG = 0	;(01) SSQ 12-11-98

	XCALL IO (3,COPCTL,1,READ,LOKCTL)

	UNLOCK 3
;;;	D =
	CALL DISPL1
	IF (ON_CC .EQ. 1) XRETURN		;CC ALREADY RUN

;;;	IF (OFLAG.EQ.1) GOTO SELCTD		;1-2-01 don't allow
	IF (OFLAG.ge.1) GOTO SELCTD		;6-3-11 gets changed to 2 in invoic

	XCALL w_disp(wnd_1,wd_pos,24,1,wd_clr,wdc_lin,'RIGHT ORDER <Y> ?')
	CTL = '24,20,01,00,YY'
	CALL INPUT
	IF (INXCTL.EQ.2) RETURN

	CALL CNG1

	IF (INXCTL.EQ.1) GO TO DISPLA
	GO TO (DISPLA), CNGCTL

;;;	IF (OEFLAG .EQ. 1) 
	USING OEFLAG SELECT
	(1),	BEGIN			;SSQ 7-30-99
		XCALL RDATE(TDATE)
		XCALL DATE8(TDATE, D_OUT, TODAY, D_FMT, D_SW)
		OORDDT = TODAY		;QUOTE BECAME ORDER
		OKEYDT = TODAY
		call getcus		;12-06-10 in case crdlmt changed
		clear cr_flag
		a6 = crdlmt, 'XXXXXX'		
		if (%instr(1, a6, '5') )	call on_hold	
		call quote_2_order	;update arqdat
		END

	(2),	call order_2_quote	;2-26-14
	ENDUSING


	using ocusno select
	(3,10),	nop
	(),	omicus =	;in case cust changes from cash
	endusing

	LOKCTL = 1
	XCALL ISIO (4,TORDHD,OORDNO,WRITE,LOKCTL)
	
	IF (GOT_CM2)
	THEN	USING C2_COM SELECT
		(.EQ. SAVCM3),	NOP		;NOTHING CHANGED
;;;		(.EQ. BLANKS),	XCALL ISIO (CHN135, ORDCM2, C2_KEY, DELET, LOKCTL)
		(.EQ. BLANKS),	begin
				xcall isio (chn135, ordcm2, c2_key, read, lokctl)
				 delete (CHN135)
				end
		(),		BEGIN
				savcm3 = c2_com
				lokctl = 0
				xcall isio (chn135, ordcm2, c2_key, read, lokctl)
				c2_com = savcm3 
				XCALL ISIO (CHN135, ORDCM2, C2_KEY, WRITE, LOKCTL)
				END
		ENDUSING
	ELSE	USING C2_COM SELECT
		(.NE. BLANKS),	BEGIN
				C2_ORD = OORDNO
				C2_SEQ = 0
				XCALL ISIO (CHN135, ORDCM2, C2_KEY, STORE, LOKCTL)
				END
		ENDUSING



END,
	ORDHDR = TORDHD

	c_wnd = wnd_1

	RETURN

SELCTD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL wnmsg(wnd_1,24,'ORDER SELECTED - MUST UNSELECT FIRST',1)
	INXCTL =2
	RETURN		;to calling routine
;--------------------------------------------

QUOTE_2_ORDER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR CHN083
	CHN083 = %SYN_FREECHN (20,100)
	SWITCH = 5
	XCALL FILES (CHN083, 'SU', 083, SWITCH)

	CLEAR ARQDAT
	LOKCTL = 0
	XCALL ISIO (CHN083, ARQDAT, OORDNO, READ, LOKCTL)

	USING LOKCTL SELECT
	(0),	BEGIN		;RECORD ALREADY ON FILE - ARCHIVED?
		AQ_RC = 'QO'
		AQ_DATE = OORDDT
		AQ_SLMN = OSLMAN
		XCALL ISIO (CHN083, ARQDAT, OORDNO, WRITE, LOKCTL)
		END

	(),	BEGIN
		CLEAR ARQDAT
		AQ_ORD = OORDNO
		AQ_SLMN = OSLMAN
		AQ_RC = 'QO'		;RESERVED FOR QUOTE TO ORDER
		AQ_DATE = OORDDT	;SHOULD BE TODAY'S DATE
		XCALL ISIO (CHN083, ARQDAT, OORDNO, STORE, LOKCTL)
		END
	ENDUSING
	
	CLOSE CHN083
	RETURN
;-----------------------------------------------------
ORDER_2_QUOTE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR CHN083
	CHN083 = %SYN_FREECHN (20,100)
	SWITCH = 5
	XCALL FILES (CHN083, 'SU', 083, SWITCH)

	CLEAR ARQDAT
	LOKCTL = 0
	XCALL ISIO (CHN083, ARQDAT, OORDNO, READ, LOKCTL)

	USING LOKCTL SELECT
	(0),	BEGIN		;RECORD ALREADY ON FILE - ARCHIVED?
		XCALL ISIO (CHN083, ARQDAT, OORDNO, DELET, LOKCTL)
		END

	ENDUSING
	
	CLOSE CHN083
	RETURN
;-----------------------------------------------------

;;;SELECTED,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		;;; SELECTED FOR BILLING
;;;		;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	XCALL w_disp(wnd_1,wd_pos,24,1,wd_clr,wdc_lin,'ORDER SELECTED, CONTINUE ANYWAY?')
;;;	xcall winpt(wnd_1,24,36,01,01,'YN',entry,inxctl)
;;;	IF (INXCTL .EQ. 2) XRETURN
;;;	RETURN
;-------------------------------------------


DISPL1,
	using ocuscd select
	('E'),	disp_em = 1
	(),	disp_em = 0
	endusing

	UNLOCK 1
	CNGCTL =
	XCALL OE1w(DSTFLG,wnd_1)
;;;	XCALL OE1(DSTFLG,V)
	XCALL w_disp(wnd_1,wd_pos,2,1,'CHANGE')
	IF (OCCRD .EQ. 'C')
		BEGIN
		CALL GET_CCTRAN
		C1_AUTH = CT_AUTH,	'ZZZZZZ'
		XCALL W_DISP(WND_1,WD_POS,2,10,C1_LINE)
		END

	IF (ORDTYP .EQ. 'I') XCALL w_disp(wnd_1,wd_pos,2,10,wd_clr,wdc_eol,' *INTERNET*')
	IF (OCCRD .EQ. 'R') XCALL w_disp(wnd_1,wd_pos,2,10,wd_clr,wdc_eol,' *CASH*')

	XCALL w_disp(wnd_1,wd_pos,4,3,'*')
	XCALL w_disp(wnd_1,wd_pos,6,3,'*')

	CTL = '04,16,06,01,#E'
	ENTRY(1,6) = OORDNO,	'ZZZZZX' [LEFT]
	XCALL w_disp(wnd_1,wd_pos,ROW, COL, ENTRY(1,6))

	LOKCTL = 0
;;;	read (4, tordhd, oordno)
	XCALL ISIO (4,TORDHD,OORDNO,READ,LOKCTL,wnd_1,chnlok)
	IF (LOKCTL.EQ.1) 
		begin
		inxctl = 2		;abort
		xreturn		;ssq 3-4-04
		end
	IF (LOKCTL.GE.2) GO TO NOFIND

	
	clear ordcm2
	c2_ord = oordno
	c2_seq = 0

	lokctl = 0
	xcall isio (chn135, ordcm2, c2_key, read, lokctl)
	unlock chn135		;1-13-2016 ssq !
	if (lokctl .eq. 0) 
	then	got_cm2 = 1
	else	begin
		clear got_cm2		;no cm2 on file
		clear ordcm2
		end

	savcm3 = c2_com


	ORDTYP = INET		;SSQ 6-7-05
DISPO,
	OF_ORD = OORDNO,	'ZZZZZX' [LEFT]
	IF(OPRTF.LE.0)OPRTF=0
	OF_NUM = OPRTF
	XCALL w_disp(wnd_1,wd_pos,ROW, COL,  ORDFMT)

	SAVLOC = OLOC
	CTL = '04,38'
	DECMAL(1,8) = OORDDT
	CALL DSPDTE
	CTL = '04,68'
	IF (OLOC.EQ.'E')  
	THEN BEGIN
	  XCALL w_disp(wnd_1,wd_pos,4,53,'EST DEL DAYS   ')
	  DECMAL = OPROMD
	  CALL DSPNUM
	  XCALL w_disp(wnd_1,wd_pos,23,3,wd_clr,wdc_eol,'21. ATTN:')
	  XCALL w_disp(wnd_1,wd_pos,23,14,OATTN)
	END
	ELSE BEGIN
	  DECMAL(1,8) = OPROMD
	  CALL DSPDTE
	END
	CTL = '06,16,06'
	DECMAL = OCUSNO
	CALL DSPNUM
	XCALL w_disp(wnd_1,wd_pos,6,24,OCUSNM)

	IF (OCUSNO .EQ. 999000) 
	THEN	BEGIN
		XCALL w_disp(wnd_1,wd_pos,7,4,wd_clr,wdc_eol,'   ORIGINAL ORDER:')
		XCALL w_disp(wnd_1,wd_pos,7,23,OBIN)
		END
	ELSE	BEGIN
		CLEAR OBIN
		XCALL w_disp(wnd_1,wd_pos,7,4,wd_clr,wdc_eol,' ')
		END

	call getcus		;5-18-10
	CTL = '06,68,06'	;5-18-10
	DECMAL = CRDLMT		;5-18-10
	CALL DSPNUM		;5-18-10
	

	call disp_cont
;----------------------------------------------------------
; not installed
;;;	if (oc_nbr.gt.0 .and. %trim(mf_text).gt.1) 
;;;		begin
;;;		xcall w_disp (wnd_1, wd_attr, attr_set+attr_bold)
;;;		xcall w_disp (wnd_1, wd_pos, 7,1, mf_text(1, %trim(mf_text)) )
;;;		xcall w_updt
;;;		xcall w_disp (wnd_1, wd_attr, attr_set+attr_clr+attr_bold)
;;;		end
;----------------------------------------------------------
	
	xcall w_disp (wnd_1, wd_pos,5,65,otaxfl)	;2-27-12
	xcall w_updt

	CTL = '08,16,02'
	DECMAL = OSLMAN
	CALL DSPNUM

	XCALL SREP(OSLMAN,LNAM,SNAM,SINT)
	XCALL w_disp(wnd_1,wd_pos,9,9,SNAM)

	XCALL w_disp(wnd_1,wd_pos,8,35,OLOC)

	XCALL w_disp(wnd_1,wd_pos,8,53,OSCAC)
	CALL GET_SCAC
	XCALL w_disp(wnd_1,wd_pos,9,53,SC_NAME)

	IF (OMETRO .NE. BLANKS)
		BEGIN
		XCALL w_disp(wnd_1,wd_pos,7,58,'12DPO:')
		XCALL w_disp(wnd_1,wd_pos,7,66,OMETRO)
		END
	XCALL w_disp(wnd_1,wd_pos,8,68,OPONO)

	XCALL w_disp(wnd_1,wd_pos,10,53,OCLPPD)
	XCALL w_disp(wnd_1,wd_pos,10,68,OJOBNO)
DSPADD,
	ALPHA (1,4) = OSHPTO,'ZZZZ'
	XCALL w_disp(wnd_1,wd_pos,12,16,ALPHA(1,4))
	XCALL w_disp(wnd_1,wd_pos,13,13,OSHPNM)
	XCALL w_disp(wnd_1,wd_pos,14,13,OSHAD1)
	XCALL w_disp(wnd_1,wd_pos,15,13,OSHAD2)
	XCALL w_disp(wnd_1,wd_pos,16,13,OSHAD3)
	
	XCALL w_disp(wnd_1,wd_pos,19,19,OCOMNT(1))
	XCALL w_disp(wnd_1,wd_pos,20,19,OCOMNT(2))
	XCALL w_disp(wnd_1,wd_pos,21,19,C2_COM)

	SWITCH = 5		;SSQ 12-19-01
	XCALL FILES (20,'SI',195,SWITCH)
	CHN195 = 20
;;;	FIND (CHN195,CRHOLD,OORDNO) [ERR=NOT_ON_HOLD]
	read (CHN195,CRHOLD,OORDNO) [ERR=NOT_ON_HOLD]	;4-1-15
	IF (CH_FLAG .EQ. 'C') 
	THEN	BEGIN
		XCALL wnmsg(wnd_1,24,'Already on Credit Card, No Changes Allowed!',1)
		ON_CC = 1
		XRETURN
		END
	ELSE	BEGIN
		ON_CC = 0
		using ch_flag select
		('4'),	XCALL wnmsg(wnd_1,24,'This order on credit hold - COD',1)
		(),	XCALL wnmsg(wnd_1,24,'THIS ORDER ON CREDIT HOLD',1)
		endusing
		END

NOT_ON_HOLD,
	CLOSE CHN195
	RETURN
;-------------------------------------------

NOFIND,
	UNLOCK 4
	XCALL wnmsg(wnd_1,24,'ORDER NOT FOUND',1)
	GO TO DISPL1
;-------------------------------------------
;-------------------------------------------

CNG1,
	INXCTL =
	GO TO ANYCNG

DATE,
	CTL = '04,38,08,05,D '
	CALL INPUT
	GO TO (NDSRCH), INXCTL
	OORDDT = ENTRY
	OKEYDT = OORDDT
	GO TO ANYCNG
PDATE,
	CTL = '04,68,08,00,D '
	IF (OLOC.EQ.'E')
	BEGIN
	  XCALL w_disp(wnd_1,wd_pos,4,53,'EST DEL DAYS   ')
	  TYPE = '# '
	END
	CALL INPUT
	GOTO (DISPLA), INXCTL
	OPROMD = ENTRY
	GO TO ANYCNG
CUSNUM,
	XCALL W_DISP (WND_1,WD_POS,24,2,WD_CLR,WDC_EOL,'<F1> = ALPHA LOOK-UP')
	CNGCUS = 1
	CTL = '06,16,06,01,# '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	USING F_KEY SELECT
	(F_01),		BEGIN
			CALL ALPHA_LOOKUP
			XCALL W_DISP(WND_1,WD_POS,24,1,WD_CLR,WDC_EOL)
			GOTO (CUSNUM,CUSNUM),INXCTL
			XCALL W_DISP(WND_1,WD_POS,6,16,ENTRY(1,6))
			END
	ENDUSING

	XCALL W_DISP(WND_1,WD_POS,24,1,WD_CLR,WDC_EOL)
	OCUSNO = ENTRY
	CALL GETCUS
	XCALL w_disp(wnd_1,wd_pos,6,24,NAME)
	OCUSNM = NAME
	OCUSCD = CUSCD
	TXFLG = TAXFLG
	CTL = '06,68,06'			;;;
	
	A6 = CRDLMT,	'XXXXXXX'
	IF (%INSTR(1,A6,'6') )
	THEN	DISTR = 1
	ELSE	DISTR = 0

	clear cr_flag
;-------------------------------------------------
; if cust changes may have to put on-hold, or 
; take off hold...

	call on_hold		;9-10-12
;;;	a6 = crdlmt, 'XXXXXX'		
;;;	if (%instr(1, a6, '5') )	call on_hold	
;-------------------------------------------------

	DECMAL = CRDLMT				;;; ADDED DS 1/7/87
	CALL DSPNUM
	if (distr .and. cr_flag.eq.0) goto skip_c_msg	;11-23-16 don't display if only 6's

	IF (CRDLMT.NE.0) 						
&	  XCALL wnmsg(wnd_1,24,'NOTE CREDIT CODE AND CHECK WITH AR',2)	

skip_c_msg,

	CALL TEMP_CUS			;SSQ 3-25-08
	goto (cusnum), bad_tmp_cus	;12-4-19

	otaxcc = 0	;4-29-13 force new tax code logic in oecc2

	IF(OCUSNO.NE.999000) 
		BEGIN
		IF (SAVMIS.NE.OMICUS) GOTO SHIPTO	;NEED A NEW SHIP-TO
		GOTO ANYCNG
		END
;;;	IF(OCUSNO.NE.999000) GOTO ANYCNG
	XCALL w_disp(wnd_1,wd_pos,7,4,wd_clr,wdc_eol,'   ORIGINAL ORDER:')
	xcall winpt(wnd_1,7,23,06,00,'# ',entry,inxctl)
;;;	XCALL INPUT (7,23,06,00,'# ',ENTRY,INXCTL,1)
	GOTO(DISPLA),INXCTL
	XORDR = ENTRY(1,6)
	OBIN = XORDR,	'XXXXXX'
	GOTO ANYCNG

SALMAN,
	CTL = '08,16,02,00,N '
	CALL INPUT
	GO TO (NDSRCH), INXCTL
	OSLMAN = ENTRY

	XCALL SREP(OSLMAN,LNAM,SNAM,SINT)
	IF(LNAM.EQ.BLANKS .OR. LNAM.EQ.']]]]]]') GOTO SALMAN
	XCALL w_disp(wnd_1,wd_pos,9,9,SNAM)
	GOTO ANYCNG
LOCA,
	CTL = '08,35,02,00,A '
	CALL INPUT
	GOTO (DISPLA), INXCTL
	if (ocusno.eq.3 .and. entry(1,1).ne.'E')
		begin
		xcall olmsg (wnd_1,24,'QUOTES ONLY FOR CUSTOMER 3',2)
		XCALL w_disp(wnd_1,wd_pos,ROW,COL,OLOC)
		goto anycng
		end

	IF (ENTRY(1,2).EQ.'  ') ENTRY(1,2) = 'O '
	OLOC = ENTRY
	XCALL w_disp(wnd_1,wd_pos,ROW,COL,OLOC)
	IF (OLOC.NE.'E'.AND.OLOC.NE.'O') GOTO LOCA

	OEFLAG = 0			;6-1-98 SSQ
	IF (SAVLOC.EQ.'E'.AND.ENTRY.EQ.'O') OEFLAG = 1
	IF (SAVLOC.EQ.'O'.AND.ENTRY.EQ.'E') OEFLAG = 2
	GOTO ANYCNG
PONUM,
	CTL = '08,68,10,00,AT'
	CALL INPUT
	GO TO (NDSRCH), INXCTL
	IF(INXCTL .EQ. 3)	;<TAB>
		BEGIN
		CALL OMETRO		;GET METRO PROJ #
		IF(CNGCTL.EQ.0)GOTO PONUM
		ENTRY = OPONO
		XCALL w_disp(wnd_1,wd_pos,8,68,OPONO)
		END

	OPONO = ENTRY
	GO TO ANYCNG
SHIPV,
	XCALL w_disp(wnd_1,wd_pos,9,53,'               ')
	CTL = '08,53,04,00,A '
	CALL INPUT
	GO TO (NDSRCH), INXCTL
	OSCAC = ENTRY(1,4)
	CALL GET_SCAC
	IF (LOKCTL .NE. 0) GOTO SHIPV
	XCALL w_disp(wnd_1,wd_pos,9,53,SC_NAME)

	XCALL w_disp(wnd_1,wd_pos,8,59,'8. PO #')
	GO TO ANYCNG
COLPPD,
	CTL = '10,53,01,00,A '
	CALL INPUT
	GO TO (NDSRCH), INXCTL
	OCLPPD = ENTRY
	IF (OCLPPD.NE.'C'.AND.OCLPPD.NE.'P'.AND.OCLPPD.NE.'D' ) GO TO COLPPD
	GO TO ANYCNG

DISC,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SSQ 3-14-97 DISABLE DISC CODE...

;;;	CTL = '05,16,02,00,# '
;;;	CALL INPUT
;;;	GO TO (NDSRCH), INXCTL
;;;	ODISC = ENTRY
;;;	XCALL DSPLY (2,10,16,ODISC,1,1)
;;;	XCALL w_disp(wnd_1,wd_pos,10,18,'%')
;;;	D = 1		;D = 1 INDICATES A NEW ORDER DISCOUNT FOR LINCN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	GOTO ANYCNG
TERMS,
	XCALL w_disp(wnd_1,wd_pos,11,33,'               ',1)
	CTL = '10,33,01,00,A '
	CALL INPUT
	GO TO (NDSRCH), INXCTL
	OTERMS = ENTRY
	IF (OTERMS.EQ.' ') GO TO TERMS
	XCALL FFILE (170,FILENM,SWITCH)
	OPEN (10,I,FILENM)
	LOKCTL = 1
	XCALL IO (10,ARTCTL,1,READ,LOKCTL)
	SRCCTL = 2
	BSEND = ORG170
	XCALL SERCH (10,ARTERM,OTERMS,1,1,BSEND,BSMID,SRCCTL,1,2,7,0,0,0,0)
	GO TO (BADTRM), SRCCTL
	close 10		;ssq 5-19-98

;;;	OTRMSD = ARTRDP
	XCALL w_disp(wnd_1,wd_pos,11,33,ARTRDS)
	GO TO ANYCNG
JOBNUM,
	CTL = '10,68,10,00,A '
	CALL INPUT
	GO TO (NDSRCH), INXCTL
	OJOBNO = ENTRY
	GOTO ANYCNG
SHIPTO,
	switch = 5
	if (cash_cust)
	then	xcall files (8,'SI',102,switch)	;misshp
	else	xcall files (8,'SI',171,switch)	;shipto

	XCALL w_disp(wnd_1,wd_pos,13,12,wd_clr,wdc_eol)
	XCALL w_disp(wnd_1,wd_pos,14,12,wd_clr,wdc_eol)
	XCALL w_disp(wnd_1,wd_pos,15,12,wd_clr,wdc_eol)
	XCALL w_disp(wnd_1,wd_pos,16,12,wd_clr,wdc_eol)
	CTL = '12,16,04,00,N '

	XCALL W_DISP(WND_1,WD_POS,24,1,wd_clr, wdc_eol,'<F1> = DISPLAY SHIP-TO LIST')
	XCALL W_DISP(WND_1,WD_POS,24,33,'<F4> = SEARCH')

	CALL INPUT
	xcall w_disp(wnd_1,wd_pos,24,1,wd_clr,wdc_eol)
	if (inxctl)
		begin
		close 8
		goto ndsrch
		end

;;;	if (f_key .eq. f_01)
	using f_key select
	(f_01, f_04),	begin
			call m_shipto
			goto (shipto),inxctl
			xcall w_disp(wnd_1,wd_pos,row,col,entry(1,4))
			end

	endusing

;;;	GO TO (NDSRCH), INXCTL

	OSHPTO = ENTRY
	IF (ENTRY.EQ.BLANKS) 
		begin
		close 8
		GO TO DFBLTO
		end
	IF (OSHPTO.EQ.9999) 
		begin
		close 8
		GO TO SHIPNM
		end

;;;	SWITCH = 5
;;;	XCALL FILES (8,'SI',171,SWITCH)


	using cash_cust select
	(1),	xcusno = omicus
	(),	xcusno = ocusno
	endusing

;;;	XCUSNO = OCUSNO
	SHPNO = OSHPTO
	XCALL ISIO (8,SHIPTO,SKEY,READ,LOKCTL)
	CLOSE 8
	IF (LOKCTL .NE. 0) GOTO BADSHP

	OSHPNM = SHTONA
	OSHAD1 = SHTOAD(1)
	OSHAD2 = SHTOAD(2)
	OSHAD3 = SHTOAD(3)
	OTAXFL = SHTOTC		;SSQ 4/24/02
	otaxcc = 0		;ssq 4-29-13

	if (oordno.lt.500000 .and. otaxfl(1,2).eq.'IL' .and.
&		otaxfl(3,3).ne.'N') otaxfl(3,3) = 'R'
	CALL DSPSHP
	IF (CALL_SHPTO) RETURN		;12-30-19 CASH CUST

	GO TO ANYCNG

DFBLTO,
	IF (NAME.NE.OCUSNM) CALL GETCUS

	if (t_cusno.eq.oordno)
	then	begin
		OSHPNM = T_NAME
		OSHAD1 = T_ADD1
		OSHAD2 = T_ADD2
		TMPCTY = T_CITY
		TMPSTA = T_STATE
		TMPZIP = T_ZIP
		OSHAD3 = TMPAD3
		end
	else	begin
		OSHPNM = NAME
		OSHAD1 = ADD1
		OSHAD2 = ADD2
		TMPCTY = CITY
		TMPSTA = STATE
		TMPZIP = ZIP
		OSHAD3 = TMPAD3
		end

	CALL DSPSHP
	GO TO ANYCNG
SHIPNM,
	using oloc select
	('O'),	if(.not.cash_cust) goto anycng
	endusing
	CTL = '13,12,30,00,A '
	xcall winpt(wnd_1,row,col,max,min,type,entry,inxctl)
	GO TO (NDSRCH), INXCTL
	OSHPNM = ENTRY
	OSHFLG = 'C'		;USER CHANGED SHIP TO
	IF (OSHPTO.NE.9999) GO TO ANYCNG
SHIPA1,
	using oloc select
	('O'),	if(.not.cash_cust) goto anycng
	endusing
	CTL = '14,12,30,00,A '
	xcall winpt(wnd_1,row,col,max,min,type,entry,inxctl)
	GO TO (NDSRCH), INXCTL
	OSHAD1 = ENTRY
	OSHFLG = 'C'		;USER CHANGED SHIP TO
	IF (OSHPTO.NE.9999) GO TO ANYCNG
SHIPA2,
; 4-2-12 ok to change this line per steve m
;;;	using oloc select
;;;	('O'),	if(.not.cash_cust) goto anycng
;;;	endusing

	CTL = '15,12,30,00,A '
	xcall winpt(wnd_1,row,col,max,min,type,entry,inxctl)
	GO TO (NDSRCH), INXCTL
	OSHAD2 = ENTRY
	OSHFLG = 'C'		;USER CHANGED SHIP TO
	IF (OSHPTO.NE.9999) GO TO ANYCNG
SHIPA3,
	using oloc select
	('O'),	if(.not.cash_cust) goto anycng
	endusing
	CTL = '16,12,30,00,A '
	xcall winpt(wnd_1,row,col,max,min,type,entry,inxctl)
	GO TO (NDSRCH), INXCTL
	OSHAD3 = ENTRY
	OSHFLG = 'C'		;USER CHANGED SHIP TO
	GO TO ANYCNG
COMENT,
	XCALL w_disp(wnd_1,wd_pos,22,16,BLANKS)
	CTL = '19,19,35,00,AT'
	CALL INPUT
	GO TO (NDSRCH,DEFCOM,DEFCOM), INXCTL
	OCOMNT(1) = ENTRY
	GO TO ANYCNG
COM2,
	CTL = '20,19,35,00,AT'
	CALL INPUT
	GO TO (NDSRCH), INXCTL
	OCOMNT(2) = ENTRY
	GO TO ANYCNG
COM3,
	CTL = '21,19,35,00,AT'
	CALL INPUT
	GO TO (NDSRCH), INXCTL
	C2_COM = ENTRY
	GO TO ANYCNG
defcom,
	ocomnt(1) = ordcom(1)
	ocomnt(2) = ordcom(2)

	XCALL w_disp(wnd_1,wd_pos,19,19,OCOMNT(1))
	XCALL w_disp(wnd_1,wd_pos,20,19,OCOMNT(2))

	GO TO ANYCNG
ARACCT,
	IF (DSTFLG.NE.'Y' .OR. MLARFG.NE.'Y') GO TO ANYCNG
	XCALL w_disp(wnd_1,wd_pos,20,72,wd_clr,wdc_eol,)
	XCALL GETAC (10,73,OARACT,INXCTL,V)
	GO TO (NDSRCH), INXCTL
	GOTO ANYCNG
OATTN,
	IF (OLOC .NE. 'E') GOTO ANYCNG
	CTL = '23,14,20,00,A '
	CALL INPUT
	GOTO (NDSRCH),INXCTL
	OATTN = ENTRY(1,20)
	GOTO ANYCNG	
ENDORD,
	CALL CHECK_SHIPTO
	IF (POBOX)
		BEGIN
		XCALL wnmsg(wnd_1,24,"Can't ship SPE-DEE or UPS if PO BOX",1)
		GOTO ANYCNG
		END

	CALL CHECK_SHIP22
	IF (BAD_SHIPTO) 
		begin
		XCALL wnmsg(wnd_1,24,"Must Enter Valid Ship-To     ",1)
		GOTO ANYCNG
		end


	SCUSTP =

	XCALL ISIO (6, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO NDSRCH
	
;;;	LOKCTL = 1
;;;	XCALL IO (6,CUSMAS,1,READ,LOKCTL)
;;;	BSEND = ORG001
;;;	KEY(1,6) = OCUSNO, MASK
;;;	XCALL SERCH(7,CUSIDX,KEY(1,6),1,6,BSEND,BSMID,SRCCTL,1,7,11,0,0,0,0)
;;;	IF (SRCCTL.EQ.1) GOTO NDSRCH	

;;;	LOKCTL = 1
;;;	XCALL IO (6,CUSMAS,IRC001,READ,LOKCTL)

;2-3-12 ssq: get tax code from ship-to:
	if (oshpto .gt. 0)
		begin
		switch = 5
		using cash_cust select			;12-9-19 cash-cust
		(1),	begin
			xcall files (8,'SI',102,switch)
			SCUSTP = MI_CUSCD
			XCUSNO = OMICUS
			end
		(),	begin
			xcall files (8,'SI',171,switch)
			SCUSTP = CUSCD
			XCUSNO = OCUSNO
			end
		endusing

		XCALL ISIO (8,SHIPTO,SKEY,READ,LOKCTL)
		CLOSE 8
		IF (LOKCTL .EQ. 0) OTAXFL = SHTOTC		;SSQ 5-2-13

	if (oordno.lt.500000 .and. otaxfl(1,2).eq.'IL' .and.
&		otaxfl(3,3).ne.'N') otaxfl(3,3) = 'R'

		taxflg = otaxfl
		end

	TXFLG = TAXFLG

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Added cust=3 or 10 test 6/24/02...
	;removed per stevem 2-3-2012
;;;	IF (OCUSNO.NE.3 .AND. OCUSNO.NE.10)OTAXFL = TAXFLG	;SSQ 4/11/02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------------------------------------------------------------------
; for orders prior to 3/21/11 ocontr did not get set.  this is a problem
; when orders are re-priced.
	ALPHA = CRDLMT
	IF (.not. %INSTR(1,ALPHA,'123') ) ocontr = 1		;4-26-11
;------------------------------------------------------------------------

NDSRCH,
	CUSTCD = OCUSCD
	RETURN
DEFLT1,
	IF (ENTRY.EQ.BLANKS) ENTRY(1,1) = '1'
	XCALL w_disp(wnd_1,wd_pos,ROW,COL,ENTRY(1,1))
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
	IF (TSTAT) XCALL W_DISP(WND_1,WD_READS,ENTRY)
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
;-------------------------------------------------------
M_SHIPTO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SHIP_SRCH = 0
	IF (F_KEY .EQ. F_04)
		BEGIN
		XCALL W_DISP(W_ID, WD_POS, 24,1,'SEARCH STRING:')
		XCALL WINPT (W_ID, 24,16,30,00,'AE',ENTRY,INXCTL)
		IF (INXCTL .NE. 0)
			BEGIN
			CLEAR ENTRY
			RETURN
			END
		SRCH_KEY = ENTRY(1,30)
		SHIP_SRCH = 1
		END
		
	CLEAR ENTRY, INXCTL

	using cash_cust select
	(1),	xcusno = omicus
	(),	xcusno = ocusno
	endusing
;;;	XCUSNO = OCUSNO
	SHPNO = 	;;;>OSHPTO
	FIND(8,SHIPTO,SKEY)[ERR=MORE_SHIP]
MORE_SHIP,
	CALL GET_MSHIP
	IF (%TTSTS) XCALL W_DISP(WND_1,WD_READS,ENTRY)
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
	IF (LOKCTL.NE.0 .OR. SHCSNO.NE.XCUSNO) GOTO MS_EOF	;for cash-cust
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
;-------------------------------------------------------
on_hold,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	cr_flag = 0		;9-10-12

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
	THEN	XCALL wnmsg(wnd_1,24,"CAN'T OPEN CRHOLD",2)	;;;

	ELSE	BEGIN
		;if record already exists - delete it!
		xcall isio (chn195, crhold, oordno, read, lokctl)
		if (lokctl .eq. 0) xcall isio (chn195,crhold,oordno,delet, lokctl)		;existing record for this order
		if (cr_flag .eq. 0) return	;this cust not on hold

		CLEAR CRHOLD
		CH_ORD = OORDNO
		CH_FLAG = cr_flag		;ORDER ENTRY credit hold
		XCALL ISIO (CHN195, CRHOLD, CH_ORD, STORE, LOKCTL)
		CLOSE CHN195
		END
; 2-08-17 temp code to log orders written to crhold
	onerror che_opn
	open (26,a,'smc:ch_err.dat')		;
	che_ord = oordno
	che_cus = ocusno
	che_nam = name
	che_pgm = 'ordcn'
	che_cde = crdlmt
	writes (26, ch_err)
	close 26
che_opn,
	offerror

	return
;------------------------------------------------------------------
;-------------------------------------------------------

TEMP_CUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear bad_tmp_cus
	IF(OCUSNO.EQ.3 .OR. OCUSNO.EQ.10 .OR. OCUSNO.EQ.58455)
		BEGIN
		XCALL OECUS(TORDHD, TMPCUS)
		if (omicus .le. 0)
			begin
			bad_tmp_cus = 1
			return
			end
		
		if(otaxcc.ne.1) OTAXFL = T_TAXFLG		;SSQ 4--29=13
	;;;	OTAXFL = T_TAXFLG		;SSQ 5/6/02
; change taxcode from IL to ILR for Rockford orders, unless code is "IRL"
	IF (OORDNO.LT.500000 .AND. OTAXFL(1,2).EQ.'IL' .AND. 
&		OTAXFL(3,3).NE.'N') OTAXFL(3,3) = 'R'
		XCALL W_DISP(WND_1,WD_POS,6,24,WD_CLR,WDC_EOL,OCUSNM)	;FROM OMICUS
		xcall w_updt
		IF (T_CUSNO.EQ.OORDNO)
			BEGIN			;TEMP CUST ENTERED
			OSHPNM = T_NAME
			OSHAD1 = T_ADD1
			OSHAD2 = T_ADD2
			TMPCTY = T_CITY
			TMPSTA = T_STATE
			TMPZIP = T_ZIP
			OSHAD3 = TMPAD3
			if (.not. cash_cust) call dspshp
			END

;		if (cash_cust)
;		then	xcall con2 (omicus, oshpto, oc_nbr, cash_cust)		;get email address
;		else	xcall con2 (ocusno, oshpto, oc_nbr, cash_cust)		;get email address
;		call disp_cont
;		if (omicus.ne.savmis)
;			begin
;			call_shpto = 1
;			call shipto
;			end

		END
	RETURN
;-------------------------------------------------------

CLROE4,
	XCALL w_disp(wnd_1,wd_pos,6,60,wd_clr,wdc_eol)
	XCALL w_disp(wnd_1,wd_pos,7,60,wd_clr,wdc_eol)
	XCALL w_disp(wnd_1,wd_pos,8,60,wd_clr,wdc_eol)
	XCALL w_disp(wnd_1,wd_pos,9,60,wd_clr,wdc_eol)
	XCALL w_disp(wnd_1,wd_pos,10,60,wd_clr,wdc_eol)
	RETURN

ANYCNG,
	savmis = omicus
	CLEAR CALL_SHPTO							;12-30-19
	IF (OCUSCD .EQ. 'NQ') 
	THEN	XCALL w_disp(wnd_1,wd_pos,1,64,wd_clr,wdc_eol,"Don't Fax/Email")
	ELSE	XCALL w_disp(wnd_1,wd_pos,1,64,wd_clr,wdc_eol)

;3-4-13	FK_DESC = "<F9> = don't fax"
	FK_DESC = "<F3> = Email  <F9> = Don't Fax/Email"
;;;;	FK_DESC = "<F3> = Email"
;;;;	IF (OCCRD.NE.' ') FK_DESC(18,33) = '<F1> = fax info'
	XCALL WANFK(wnd_1,24,CNGCTL,WHATNO,F_KEY,FK_DESC)
	USING F_KEY SELECT
	(F_02),	BEGIN
		CALL TEMP_CUS
		END

	(f_03),	begin
		using ocusno select
	;;;	(2,3,5,10),	nop
		(2,3,5,10),	xcall con2 (omicus, oshpto, oc_nbr, cash_cust)
		(.gt.0),	xcall con2 (ocusno, oshpto, oc_nbr, cash_cust)
		endusing
		call disp_cont
		goto anycng
		end
	(F_05),	BEGIN
		OCUSCD = CUSCD
		GOTO ANYCNG
		END

	(F_09),	BEGIN
		OCUSCD = 'NQ'
		GOTO ANYCNG
		END
	ENDUSING

	GOTO (ENDORD,CNGBR,ENDORD), CNGCTL+1
CNGBR,
	GOTO (BADCNG,DATE,PDATE,CUSNUM,SALMAN,LOCA,SHIPV,PONUM,badcng,badcng,
&	      COLPPD,JOBNUM,SHIPTO,SHIPNM,SHIPA1,SHIPA2,SHIPA3,
&	      COMENT,com2,com3,OATTN), WHATNO

;;;	GOTO (BADCNG,DATE,PDATE,CUSNUM,SALMAN,LOCA,SHIPV,PONUM,badcng,badcng,
;;;&	      COLPPD,JOBNUM,SHIPTO,SHIPNM,SHIPA1,SHIPA2,SHIPA3,
;;;&	      COMENT,badcng,BADCNG,OATTN), WHATNO

BADCNG,
	XCALL BEEP
	GOTO ANYCNG

OMETRO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; GET METRO PROJ. NO.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL w_disp(wnd_1,wd_pos,7,58,'METRO:')
	CTL = '07,66,12,00,A '

	IF (OMETRO .NE. BLANKS)GOTO OM_DISP

	SAVV = V
	V = 1
	CALL INPUT
	V = SAVV
	GOTO (OM_ABORT),INXCTL
	OMETRO = ENTRY(1,12)
	GOTO OM_ABORT

OM_DISP,
	XCALL w_disp(wnd_1,wd_pos,ROW,COL,wd_clr,wdc_eol,OMETRO)

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
	RETURN
;------------------------------------------
ACCEPT,
	XCALL FLAGS (00010000)			;DISABLE CHARACTER ECHOING
	XCALL w_disp(wnd_1,wd_pos,ROW,COL+13,' ')
	ACCEPT (15,TCHAR)
	XCALL FLAGS (00000000)			;RE-ENABLE CHARACTER ECHOING
	RETURN

DSPNUM,
	OPTION = 1
	GOTO CALDSP
DSPDTE,
	XCALL DATE8(DECMAL(1,8), D_OUT, D_OUTR, D_FMT, D_SW)
	XCALL w_disp(wnd_1,wd_pos,ROW, COL, D_FMT)
	RETURN
DSPDLR,
	OPTION = 3
CALDSP,
	xcall wdspl(wnd_1,max,row,col,decmal,option)
;;;	XCALL DSPLY(MAX,ROW,COL,DECMAL,OPTION,V)
	RETURN
INPUT,
;;;	xcall winpt(wnd_1,row,col,max,min,type,entry,inxctl,f_key)
	if (disp_em) XCALL W_DISP(WND_1,WD_POS,24,66,'<F3> = EMAIL')
	XCALL WINPT (W_ID,ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,F_KEY)
	wstat = f_key
	using f_key select
;;;	(f_02),	begin
;;;		call rolodex
;;;		goto input
;;;		end
	(f_03),	begin
		using ocusno select
	;;;	(2,3,5,10),	nop
		(2,3,5,10),	xcall con2 (omicus, oshpto, oc_nbr, cash_cust)
		(.gt.0),	xcall con2 (ocusno, oshpto, oc_nbr, cash_cust)
		endusing
		call disp_cont

		goto input
		end
	endusing
	RETURN
;------------------------------------------------------------------

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

read_cont,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	clear tbl_key
;;;	tblcod = 'E2'
;;;	e2_cust = cusno
;;;	e2_nbr = oc_nbr
;;;	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
;;;	if (lokctl .ne. 0) clear coptbl
;;;	mail_rec = coptbl
	
	if (cash_cust)
	then	begin
		close chn084
		switch = 5
		xcall files (chn084, 'SU', 103, switch)
		e_cust = omicus
		end
	else	e_cust = cusno

	e_nbr = oc_nbr
	xcall isio (chn084, emailc, e_key, read, lokctl)
	if (lokctl .ne. 0) clear emailc
	

	clear mail_fmt, mf_text
	mf_text = mr_fnam
	mf_text(%trim(mf_text)+2, 60) = e_lnam
	mf_text(%trim(mf_text)+2, 60) = e_mail

	if (cash_cust)
		begin
		close chn084
		switch = 5
		xcall filec ('SMC', chn084, 'SU', 084, switch)		
		end
	return
;------------------------------------------------------------------

DSPSHP,
	XCALL w_disp(wnd_1,wd_pos,13,12,OSHPNM)
	XCALL w_disp(wnd_1,wd_pos,14,12,OSHAD1)
	XCALL w_disp(wnd_1,wd_pos,15,12,OSHAD2)
	XCALL w_disp(wnd_1,wd_pos,16,12,OSHAD3)

	IF (OMICUS .NE. SAVMIS)
		BEGIN
		xcall con2 (omicus, oshpto, oc_nbr, cash_cust)		;get email address
		call disp_cont
		END
	

	RETURN
BADSHV,
	XCALL wnmsg(wnd_1,24,'SHIP-VIA CODE NOT FOUND',1)
	GO TO SHIPV
BADTRM,	
	XCALL wnmsg(wnd_1,24,'TERM CODE NOT FOUND',1)
	GO TO TERMS
BADSHP,
	XCALL wnmsg(wnd_1,24,'SHIP-TO NUMBER NOT FOUND',1)
	GO TO SHIPTO

GETCUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	LOKCTL = 1
	XCALL ISIO (6, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		XCALL wnmsg(wnd_1,24,'CUSTOMER NOT FOUND FOR TAX CODE UPDATE',1)
		CUSMAS = 
		NAME = '** CUSTOMER NOT FOUND ***'
		END			
		
	clear miscus
	using cusno select
	(2,3,5,10),	begin
			cash_cust = 1
			if (omicus.gt.0) 
				begin
				xcall isio (chn101,miscus,omicus,read,lokctl)
				if (lokctl.ne.0)
					begin
					clear miscus
					xcall wnmsg(wnd_1,24,'not miscus rec found',1)
					end
				end
			end

	(),		cash_cust = 0
	endusing

	USING OTERMS SELECT
	('4','5','6','C','O'), XCALL WNMSG (WND_1, 24, 'REQUIRES PAYMENT IN ADVANCE',1)
	(), IF (CRDLMT .EQ. 5555) XCALL WNMSG (WND_1, 24, 'REQUIRES PAYMENT IN ADVANCE',1)
	ENDUSING

;----------------------------------------------------------
;;;	if (oc_nbr .gt. 0)
;;;		begin
;;;		clear tbl_key
;;;		tblcod = 'E2'
;;;		ex_cust = cusno
;;;		ex_key = oc_nbr
;;;		xcall isio (chn182, coptbl, tbl_key, read, lokctl)
;;;		if (lokctl .ne. 0) clear coptbl
;;;		mail_rec = coptbl
;;;
;;;		clear mail_fmt
;;;		mf_text = mr_fnam
;;;		mf_text(%trim(mf_text)+2, 60) = mr_lnam
;;;		mf_text(%trim(mf_text)+2, 60) = mr_mail
;;;		end
;----------------------------------------------------------

	RETURN			
;--------------------------------------------------

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
GET_CCTRAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SWITCH = 5
	XCALL FILES (33,'SI',138,SWITCH)
	XCALL ISIO (33,CCTRAN,OORDNO,READ,LOKCTL)
	CLOSE 33
	IF (LOKCTL .NE. 0) CLEAR CCTRAN
	RETURN
;---------------------------------------------------

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
CHECK_SHIP22,
	CLEAR BAD_SHIPTO

	if (cash_cust) return
	if (ocusno .eq. 63340) return		;rockford
	if (oloc .eq. 'E') return		;estimate

	USING OSHPTO SELECT
	(1 THRU 9998),	RETURN	;ASSUME FROM SHIP-TO FILE
	ENDUSING

; if not from ship-to file, it should match cust info...

	BAD_SHIPTO = 1		;ASSUME BAD

	TMPCTY = CITY
	TMPSTA = STATE
	TMPZIP = ZIP

	IF (OSHPNM .NE. NAME) RETURN
	IF (OSHAD1 .NE. ADD1) RETURN
;;;	IF (OSHAD2 .NE. ADD2) RETURN	;4-2-12 can be diff for ups PO #
	IF (OSHAD3 .NE. TMPAD3) RETURN

	CLEAR BAD_SHIPTO	

	RETURN
;--------------------------------------------------
INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;color palets
;Pallets...
	XCALL W_PROC(WP_PALET, 3, 7, 4)		;4=RED
	XCALL W_PROC(WP_PALET, 4, 1, 0)		;1=BLUE
	XCALL W_PROC(WP_PALET, 5, 2, 0)		;2=GREEN
	XCALL W_PROC(WP_PALET, 6, 3, 0)		;3=CYAN
	XCALL W_PROC(WP_PALET, 7, 4, 7)		;4=RED
	XCALL W_PROC(WP_PALET, 8, 7, 0)		;7=WHITE
	XCALL W_PROC(WP_PALET, 9, 0, 7)		;4=BLACK ON WHITE
	XCALL W_PROC(WP_PALET,10, 6, 0)		;6=YELLOW

	palet1 = 3		;01-17-11

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

END
