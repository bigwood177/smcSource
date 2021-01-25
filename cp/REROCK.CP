;REROCK.CP	;RE-COPY ROCKFORD ORDER...	isam
;CPYROC.CP	;COPY ROCKFORD ORDERS TO SMC
;CPYORD.CP
;
;	CREATE NEW ORDER FROM EXISTING ORDER
;
;	4-14-05: if order has already been copied to SMC, delete lines.
;	1-18-21 ssq: ordhdr.odisc has been removed.

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR


RECORD	CUSMAS
	.INCLUDE 'DEF:RD001A.DEF'
RECORD,X
	.INCLUDE 'DEF:RD001B.DEF'
;;;RECORD	CUSIDX
;;;	.INCLUDE 'DEF:RD002A.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.def'
RECORD,X
	.INCLUDE 'DEF:RD045D.def'

RECORD	COPCTL
	.INCLUDE 'DEF:RD060A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	DUCACC
	.INCLUDE 'DEF:RD175A.DEF'

RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.def'

	RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'


RECORD	SNMSG
		,A2,	'CO'	;ID'S THIS PGM
	S_ORD	,D6

RECORD	ITMMSG
	ITM_ITM	,A15
		,A*,	' IS NOT IN ITMMAS FILE'

RECORD	ORDMSG
		,A*,	'ORDER NUMBER: '
	ORD_ORD	,D6
		,A*,	' HAS JUST BEEN UPDATED'

RECORD	PRINT
	U_LINE	,A80
	TITLE	,A*,	'NO TITLE'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PLINE	,A80
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

RECORD	HLINE0
		,A65
		,A6,	'ORDER:'
		,A1
	HL0_ORD	,A8

RECORD	HLINE
	,A*,'DATE ENTERED  SALESMAN    EXT PO NUMBER  JOB NUMBER  SHIP VIA'

RECORD	ORDFMT
	OF_ORD	,A6
		,A1,	'.'
	OF_NUM	,D1

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
	
RECORD	ERRMSG
		,A*,	'Error storing order: '
	ERR_ORD	,D6

RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2	
	CHN041	,D2
	CHN042	,D2
	CHN044	,D2
	CHN045	,D2
	CHN060	,D2
	CHN175	,D2
	CHN182	,D2
	ROC044	,D2	;ROC:ORDHDR
	ROC045	,D2	;ROC:ORDLIN
	ROC175	,D2	;ROC:DUCACC
	roc041	,d2	;roc:itmmas

RECORD TMPAD3
	TMPCTY	,A15
		,A1
	TMPSTA	,A2
		,A2
	TMPZIP	,A10
record	m_msg
	m_item	,a15
		,a1
	m_f1	,a3
		,a1
	m_f2	,a3
		,a1
	m_f3	,a5
		,a1
	m_desc	,a30
		,a1
		,a*, 'not copied!!'

RECORD	VARS
	config_price	,d8
	tdescr	,a30
	config_error	,d1
	mat_sel	,d1
	a2	,a2
	a15	,a15
	top	,d3
	bot	,d3
	kitmno	,a15
	swfab	,d1
	ivflag	,d1
	vanflg	,d1
	itmrfa	,a6
	vanrfa	,a6
	cfg_item	,d1	
	cdpt	,d1
	cdpt1	,d1
	cdpt2	,d1
	st_guage	,a2
	SMCORD	,D6
	ROCORD	,D6
	OPNOK	,D1
	CBSEND	,D5
	PASS	,D2
	HDRHST	,A14
	LINHST	,A14
	DUCHST	,A14
	CUSKEY	,D6
	KEY	,A15
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	ALPHA	,A8
	DECMAL	,D18
	OPTION	,D1
	LNAM	,A25
	SNAM	,A12
	SINT	,A3
	SEXT	,D3
	ABORT	,D1
	I	,D5
	BLANKS	,A30
	XDATE	,D8
	LSTDTE	,D8
	TDATE	,D6
	DCHAR	,D3
	TCHAR	,D3
	XORD	,D6
	ORGORD	,D6
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	LOKCTL	,D1

	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	SWITCH	,D1
	V	,D1

.PROC
	XCALL TERID (V)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	XCALL RDATE (TDATE)
	XCALL DATE8(TDATE, D_OUT, LSTDTE, D_FMT, D_SW)

DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'RE-COPY ROCKFORD ORDER TO SMC',1)
	XCALL OUTPT (2,4,0,'SMC ORDER #',1)
	XCALL OUTPT (4,4,0,'ROCKFORD  #',1)

	XCALL INPUT (2, 20, 06, 00 , '#E', ENTRY, INXCTL, 1)
	GOTO (DISPLA, ENDOFF), INXCTL
	SMCORD = ENTRY(1,6)

	LOKCTL = 1
	XCALL ISIO (CHN044, ORDHDR, SMCORD, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		XCALL MESAG ('SMC ORDER NOT FOUND',1)
		GOTO DISPLA
		END

	ONERROR NOT_NUM
;;;	ROCORD = OMETRO		;ORIGINAL ROCKFORD ORDER #
	ROCORD = OJOBNO		;ORIGINAL ROCKFORD ORDER #
	OFFERROR

	ENTRY(1,6) = ROCORD,	'XXXXXX'
	XCALL OUTPT (4,20,0,ENTRY(1,6),1)
	XCALL MESAG ('This was the original Rockford order # ...',1)
	GOTO CONT
NOT_NUM,
	OFFERROR
	XCALL MESAG ('OJOBNO DOES NOT CONTAIN ROC ORD #',1)
	CLEAR ROCORD
	GOTO DISPLA

CONT,

	CALL GETORD
	IF (ABORT) GOTO DISPLA

	CALL CHKORD			;delete lines if order already exists

	CALL COPY_HEADER
	GOTO (ENDOFF),ABORT

	CALL COPY_LINE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	don't copy duct - per Ryan 5-7-04
;;;	CALL COPY_DUCT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CALL END_ORDER
;=====================================================	

ENDOFF,
	CALL CLOSE
	IF(S_ORD)
		BEGIN
		XCALL SNMSG(SNMSG,5)
		XCALL PGCHN('cp:ORDMNT',1)
		END

	XCALL PGCHN ('cp:OEMNU',1)
	STOP
;=====================================================	
CHKORD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FIND (CHN045, ORDLIN, SMCORD) [ERR=CO_LOOP]
CO_LOOP,
	LOKCTL = 1
	XCALL IOS (CHN045, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO CO_EOF
	IF (LORDNO .NE. SMCORD) GOTO CO_EOF
	
	LQTYOR = -LQTYOR	;reverse
	CALL COMMIT		;uncommit
	XCALL ISIO (CHN045, ORDLIN, ORDKEY, DELETE, LOKCTL) ;delete record

	GOTO CO_LOOP
CO_EOF,
	RETURN
;-----------------------------------------------------


COPY_HEADER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; SMC order already exists...
	RETURN


;;;	LOKCTL = 1
;;;	XCALL IO (CHN060, COPCTL, 1, READ, LOKCTL)
;;;	IF(LSTORD .EQ. 999999) LSTORD = 100000
;;;	INCR LSTORD
;;;	LOKCTL = 1
;;;	XCALL IO (CHN060, COPCTL, 1, WRITE, LOKCTL)
;;;
;;;	OORDNO = LSTORD
;;;
;;;	CLEAR OINVNO, OINVDT, OFLAG, DSAMT, DSCODE, OPRTF, OSHDTE, OSHTIM
;;;	CLEAR OSALE, OMISC, DSAMT, DSCODE
;;;
;;;	OORDDT = LSTDTE		;TODAY
;;;	OKEYDT = LSTDTE		;TODAY
;;;
;;;	OSLMAN = 11		;Eric
;;;	OCUSNO = 63340		;ROCKFORD
;;;	OMETRO = XORD
;;;	OSHPTO = 
;;;	OFRGHT = 
;;;	
;;;	IF (OARACT .NE. 0) OARACT(5,7) = 100	;SSQ 6/29/04
;;;	IF (OFRACT .NE. 0) OFRACT(5,7) = 100	;SSQ 6/29/04
;;;	IF (OMSACT .NE. 0) OMSACT(5,7) = 100	;SSQ 6/29/04
;;;
;;;	XCALL ISIO (CHN044, ORDHDR, OORDNO, STORE, LOKCTL)
;;;	IF (LOKCTL .NE. 0)
;;;		BEGIN
;;;		ERR_ORD = OORDNO
;;;		XCALL MESAG (ERRMSG,1)
;;;		ABORT = 1
;;;		END
;;;	RETURN
;-----------------------------------------------------

COPY_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FIND (ROC045, ORDLIN, ROCORD) [ERR=CL_LOOP]
CL_LOOP,
	XCALL IOS (ROC045, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO CL_EOF
	IF (LORDNO .NE. ROCORD) GOTO CL_EOF

	IF(LTYPE .EQ. 'M') GOTO WRTLIN
	IF(LROCPO .NE. 9) GOTO CL_LOOP		;.XX lines only...	
	CLEAR LQTYSH, LQTYBO, LCOMPS, LFLAG
	CLEAR LROCPO


	xcall gtitm (litmno, lf1, lf2, lf3, itmrfa, ivflag, vanflg, vanrfa)

	clear swfab

	using ivflag select
	(0),	begin			
		if (.not.vanflg) goto iv_err	;no vanilla item
		read (chn041, itmmas, keynum:1, rfa:vanrfa) [err=iv_err]
		swfab = 1
		lprdcd = prdcat
		ldept = usrdef
		call try_cfg	;vanilla item found
		end

	(1),	begin
		read (chn041, itmmas, keynum:1, rfa:itmrfa) [err=iv_err]
		lprdcd = prdcat
		ldept = usrdef
		if (litmno.ne.lcfgim) call try_cfg
		end
	endusing


	if (swfab .eq. 0) CALL COMMIT

	xcall gtitm (litmno,lf1,lf2,lf3, itmrfa, ivflag, vanflg, vanrfa, 'ROC')
	if (ivflag .eq. 0) goto wrtlin

	read (roc041, itmmas, rfa:itmrfa) [err=wrtlin]
	if (stock .eq. 'S')
		begin
		qtyono = qtyono + lqtyor
		write (roc041, itmmas, itmrfa) [err=wrtlin]
		end
	unlock roc041
;;;	CALL COMMIT

WRTLIN,
	LORDNO = SMCORD
	XCALL ISIO (CHN045, ORDLIN, ORDKEY, STORE, LOKCTL)
	GOTO CL_LOOP

iv_err,
	m_item = litmno
	m_f1 = lf1,	'XXX'
	m_f2 = lf2,	'XXX'
	m_f3 = lf3,	'XXXXX'
	m_desc = ldescr
	xcall mesag (m_msg,1)
	goto cl_loop

CL_EOF,
	RETURN
;--------------------------------------------------

try_cfg,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; might be configured item...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	USING LCFGIM SELECT	;in case litmno replaced by pressed part
	('WR'),				CFG_ITEM = 1
	('RW16' THRU 'RW20'),		CFG_ITEM = 1
	('R16' THRU 'R26'),		CFG_ITEM = 1
	('RV16' THRU 'RV26'),		CFG_ITEM = 1
	('T4','T9','TV4','TV9'),	CFG_ITEM = 1
	('C9','C4','CV9','CV4'),	CFG_ITEM = 1
	('CT','CC','CTV','CCV'),	CFG_ITEM = 1
	('ER'),				CFG_ITEM = 1
	('BN'),				CFG_ITEM = 1	
	('P4','P9','PV4'),			CFG_ITEM = 1
	('S9','S4','SV','SW','SC','ST'),	CFG_ITEM = 1
	('SG','SX'),			CFG_ITEM = 1
	('RT','RTV'),			CFG_ITEM = 1
	(),				CFG_ITEM = 2	;item not configurable
	ENDUSING

	GOTO (REG_ITEM),CFG_ITEM -1

; only reason to call cfg is to get "st_guage"...
	kitmno = litmno
	mat_sel = lmat
	XCALL CFG(KITMNO,CONFIG_PRICE,TDESCR,CONFIG_ERROR,MAT_SEL,ST_GUAGE)
	if (config_error) clear st_guage
	
reg_item,
	clear cdpt, cdpt1, cdpt2			;01-17-08
	if (lf1)
		begin
		clear tbl_key
		tblcod = 'M1'
		m1_key = lf1
		xcall isio (chn182,coptbl,tbl_key,read,lokctl)
		if (lokctl .eq. 0) cdpt1 = mm_cdpt
		end
	if (lf2)
		begin
		clear tbl_key
		tblcod = 'M2'
		m2_key = lf2
		xcall isio (chn182,coptbl,tbl_key,read,lokctl)
		if (lokctl .eq. 0) cdpt2 = mm_cdpt
		end

	if (cdpt1.eq.1 .or. cdpt2.eq.1) cdpt = 1

	if (cdpt.eq.1 .or. swfab.eq.1) 
		begin
		ldept = lprdcd	;f2 NOTE W/ CHANGE DEPT FLAG SET 12-26-07
		clear a2
		if(imdpt .ne. a2) ldept = imdpt	;1-18-08
		end


	USING ST_GUAGE SELECT
	('20','18','16'),
		BEGIN
		LDEPT = 'K'
		LPRDCD = 'K3'
		END
	ENDUSING

	USING LITMNO SELECT
	('T420', 'T920'),	LDEPT = 'K'
	('T418', 'T918'),	LDEPT = 'K'
	('T416', 'T916'),	LDEPT = 'K'
	ENDUSING

	USING LITMNO SELECT
	('RW20','RW18','RW16'),	LDEPT = 'K'

	('R18' thru 'R24'),	BEGIN		;2-14-08
				XCALL ALPDC (LCFGIM(4,5), BOT, SWITCH)
				IF (SWITCH) CLEAR BOT
				XCALL ALPDC(LCFGIM(6,7), TOP, SWITCH)
				IF (SWITCH) CLEAR TOP
				IF (BOT.GE.14 .AND. TOP.GE.12)
				THEN	LDEPT = 'F'
				ELSE	LDEPT = 'H'
				END
	('ER18' thru 'ER24'),	BEGIN		;2-14-08
				XCALL ALPDC (LCFGIM(5,6), BOT, SWITCH)
				IF (SWITCH) CLEAR BOT
				XCALL ALPDC(LCFGIM(7,8), TOP, SWITCH)
				IF (SWITCH) CLEAR TOP
				IF (BOT.GE.14 .AND. TOP.GE.12)
				THEN	LDEPT = 'F'
				ELSE	LDEPT = 'H'
				END

	ENDUSING


	return
;------------------------------------------------------


COMMIT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; COMMIT INVENTORY
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3-10-09 alread have rfa from gtitm...

	read (chn041, itmmas, rfa:itmrfa) [err=itm_err]
	if (stock.ne.'S')	return

	qtycom = qtycom + lqtyor
	write (chn041, itmmas, rfa:itmrfa) [err=itm_err]

	return
itm_err,
	xcall mesag (itmmsg,1)
	return
;-----------------------------------------------------


COPY_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FIND (ROC175, DUCACC, ROCORD) [ERR=CD_LOOP]
CD_LOOP,
	XCALL IOS (ROC175, DUCACC, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO CD_EOF
	IF (DUCTOR .NE. ROCORD) GOTO CD_EOF

	CLEAR RST175, FLG175, DCOMPS

	DUCTOR = OORDNO
	XCALL ISIO (CHN175, DUCACC, DUCTOR, STORE, LOKCTL)
	GOTO CD_LOOP

CD_EOF,
	RETURN
;-----------------------------------------------------

END_ORDER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	S_ORD = SMCORD
	ORD_ORD = SMCORD
	XCALL MESAG (ORDMSG,1)
	RETURN
;-----------------------------------------------------

GET_SCAC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; READ SCAC FROM COPTBL
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
;--------------------------------------------------------


GETORD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR ABORT, LOKCTL, PASS
RE_TRY,
	XCALL ISIO (ROC044, ORDHDR, ROCORD, READ, LOKCTL)
	IF (LOKCTL)
		BEGIN
		XCALL MESAG ('ORDER NOT ON FILE',1)
		GOTO ABORT
		END
DISPL1,
	IF (OLOC .EQ. 'E')
		BEGIN
		XCALL MESAG ('This is an estimate! - but go ahead',2)
	;;;	XCALL MESAG ('This is an estimate!',2)
	;;;	GOTO ABORT
		END
		
	XCALL OE1(DSTFLG,V)
	XCALL OUTPT (1,1,0,'RE-COPY ROCKFORD ORDER TO SMC',1)

	ONERROR NO_NUM
;;;	ORGORD = OMETRO
	ORGORD = OJOBNO
NO_NUM,
	OFFERROR

	OF_ORD = OORDNO,	'ZZZZZX' [LEFT]
	IF(OPRTF.LE.0)OPRTF=0
	OF_NUM = OPRTF
	CTL = '02,16'
	XCALL OUTPT(ROW, COL, 0, ORDFMT, V)

	CTL = '02,38'
	DECMAL(1,8) = OORDDT
	CALL DSPDTE
	CTL = '02,68'
	IF (OLOC.EQ.'E')  
	THEN BEGIN
	  XCALL OUTPT (4,53,0,'EST DEL DAYS   ',1)
	  DECMAL = OPROMD
	  CALL DSPNUM
	  XCALL OUTPT (23,3,1,'21. ATTN:',1)
	  XCALL OUTPT (23,14,0,OATTN,1)
	END
	ELSE BEGIN
	  DECMAL(1,8) = OPROMD
	  CALL DSPDTE
	END
	CTL = '03,16,06'
	DECMAL = OCUSNO
	CALL DSPNUM
	XCALL OUTPT (3,24,0,OCUSNM,V)

	CTL = '04,16,02'
	DECMAL = OSLMAN
	CALL DSPNUM

	XCALL SREP(OSLMAN,LNAM,SNAM,SINT,SEXT)
	XCALL OUTPT(9,9,0,SNAM,1)

	XCALL OUTPT (4,35,0,OLOC,V)

	XCALL OUTPT (8,53,0,OSCAC,1)
	CALL GET_SCAC
	XCALL OUTPT (9,53,0,SC_NAME,1)

	XCALL OUTPT (4,69,0,OPONO,V)
;;;	CTL = '05,16,02'
;;;	DECMAL = ODISC
;;;	CALL DSPNUM

	XCALL OUTPT (10,33,0,OTERMS,1)

TERMOK,
	XCALL OUTPT (10,53,0,OCLPPD,1)
	XCALL OUTPT (5,69,0,OJOBNO,V)
DSPADD,
	ALPHA (1,4) = OSHPTO,'ZZZZ'
	XCALL OUTPT (6,16,0,ALPHA(1,4),V)
	XCALL OUTPT (13,13,0,OSHPNM,1)
	XCALL OUTPT (14,13,0,OSHAD1,1)
	XCALL OUTPT (15,13,0,OSHAD2,1)
	XCALL OUTPT (16,13,0,OSHAD3,1)
	
	XCALL OUTPT (20,19,0,OCOMNT(1),1)
	XCALL OUTPT (21,19,0,OCOMNT(2),1)
;;;	IF (DSTFLG.NE.'Y') RETURN
;;;	ALPHA = OARACT, 'XXXX-XXX'
;;;	XCALL OUTPT (10,73,0,ALPHA,V)



	XCALL OUTPT (24,1,1,'RIGHT ORDER?',1)
	XCALL INPUT (24,16,01,01,'YY',ENTRY,INXCTL,1)
	IF(INXCTL .EQ. 2) GOTO ABORT

	XCALL OUTPT (24,1,1,'DO YOU WANT TO RE-COPY THIS ORDER?',1)
	XCALL INPUT (24,37,01,01,'YN',ENTRY,INXCTL,1)
	IF(INXCTL .EQ. 2) GOTO ABORT


;-
	CUSKEY = 63340

	XCALL ISIO (CHN001, CUSMAS, CUSKEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO BADCUS

;;;	XCALL SERCH (CHN002,CUSIDX,CUSKEY,1,6,CBSEND,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	GO TO (BADCUS), SRCCTL
;;;	LOKCTL = 1
;;;	XCALL IO (CHN001,CUSMAS,IRC001,READ,LOKCTL)
		
	OCUSNM = NAME
	OCUSCD = CUSCD
	OTAXFL = TAXFLG		;SSQ 4/11/02
	ORDSEQ = 01
	OTERMS = TERMS

BADCUS,
;;;	XCALL OUTPT (4,4,2,'SHIP TO ROCKFORD <Y>?',1)
;;;	XCALL INPUT (4,28,01,00,'YY',ENTRY,INXCTL,1)
;;;	GOTO (C_CON),INXCTL-1
;;;
;;;	OSHPNM = NAME
;;;	OSHAD1 = ADD1
;;;	OSHAD2 = ADD2
;;;	TMPCTY = CITY
;;;	TMPSTA = STATE
;;;	TMPZIP = ZIP
;;;	OSHAD3 = TMPAD3
C_CON,
	RETURN
;---------------------------------------------------------

ABORT,
	ABORT = 1
	RETURN
;---------------------------------------------------------

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	IF (PRNTON .EQ. 0) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD,HD,HD
&		,LG,LG,LG,0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;-------------------------------------------------------------
PRTHDR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	LINCNT = 2
	PLINE = HLINE0
	CALL PRINT
	CALL PRINT

	PLINE(10,40) = OCUSNM
	PLINE(42,42) ='/'
	PLINE(44,74) = OSHAD1
	CALL PRINT
	CALL PRINT

	PLINE = HLINE
	CALL PRINT
	CALL PRINT

	XDATE(1,4) = OORDDT(5,8)
	XDATE(5,8) = OORDDT(1,4)
	PLINE(3,12) = XDATE,	'XX/XX/XXXX'
	PLINE(14,25)= SNAM
	PLINE(27,29)= SEXT,	'XXX'
	PLINE(31,40)= OPONO
	PLINE(42,51)= OJOBNO
	
	TBLCOD = 'SC'
	SC_SCAC = OSCAC
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL) SC_NAME = '* NOT ON FILE *'

	PLINE(54,80) = SC_NAME
	CALL PRINT
	CALL PRINT

	RETURN
;------------------------------------------------


DSPNUM,
	OPTION = 1
	GOTO CALDSP
DSPDTE,
	XCALL DATE8(DECMAL(1,8), D_OUT, D_OUTR, D_FMT, D_SW)
	XCALL OUTPT (ROW, COL, 0, D_FMT, V)
	RETURN
DSPDLR,
	OPTION = 3
CALDSP,
	XCALL DSPLY(MAX,ROW,COL,DECMAL,OPTION,V)
	RETURN

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 1
	XCALL FILES (1, 'SU', 041, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 1

;;;	XCALL FILES (2, 'I', 042, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN042 = 2

	SWITCH = 5
	XCALL FILES (4, 'SU', 044, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 4

	XCALL FILES (5, 'SU', 045, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 5

	XCALL FILES (7, 'SU', 175, SWITCH)
	IF(SWITCH .EQ. 9) RETURN
	CHN175 = 7

;;;	XCALL FILES (8, 'SI',  182, SWITCH)
	XCALL FILES (17, 'SI',  182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 17			;cfg needs this to be 17

;;;	XCALL FILES (9, 'U', 060, SWITCH)
	XCALL FILES (19, 'U', 060, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN060 = 19			;cfg uses chn 9

	SWITCH = 5
	XCALL FILES (10,'SI',001,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN001 = 10

;;;	XCALL FILES (12,'I',002,SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN002 = 12

;;;	XCALL IO (CHN001, CUSMAS, 1, READ, LOKCTL)
;;;	CBSEND = ORG001

;;;	OPEN(24,SI,'tst:ORDHDR.tsm')
	OPEN(24,SI,'ROC:ORDHDR.ROM')
	ROC044 = 24

;;;	OPEN(25,SI,'tst:ORDLIN.tsm')
	OPEN(25,SI,'ROC:ORDLIN.ROM')
	ROC045 = 25

;;;	OPEN(26,SI,'tst:DUCACC.tsm')
	OPEN(26,SI,'ROC:DUCACC.ROM')
	ROC175 = 26

	open (27,si,'roc:itmmas.rom')
	roc041 = 27

;;;	XCALL IO (CHN041, ITMMAS, 1, READ, LOKCTL)
;;;	IF (LOKCTL .NE. 0) RETURN
;;;
;;;	BSEND = ORG041
	
	OPNOK = 1
	RETURN
;---------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN041) XCALL FILES (CHN041, 'SU', 041, 4)
;;;	IF (CHN042) XCALL FILES (CHN042, 'U', 042, 4)

	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045
	IF (CHN060) CLOSE CHN060
	IF (CHN175) CLOSE CHN175
	IF (CHN182) CLOSE CHN182
	IF (ROC044) CLOSE ROC044
	IF (ROC045) CLOSE ROC045
	IF (ROC175) CLOSE ROC175
	if (roc041) close roc041
	RETURN
;---------------------------------------------
.END




