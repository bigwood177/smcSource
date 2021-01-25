;9-19-06 working on straight duct - .prd matches production version...
;FABCNV.CP
;
;	CONVERT FABSHOP DATA TO SMC ORDER
;	pressure file indicates material
;
;	 5-6-15: read server name from table 'SR'
;	5-29-18 ssq: make cusmas isam
;	6-17-20: ordlin.lpwnw = 1 => ordlin.lpwgt is XXX,XXX instead of X,XXX.XX
;	1-18-21 ssq: ordhdr.odisc has been removed.

	.INCLUDE 'WND:TOOLS.DEF'
	.INCLUDE 'WND:WINDOWS.DEF'

record	ordcm2				;7-15-15
	.include 'def:rd135a.def'	;order comments

record	cusmas
	.include 'def:rd001a.def'
record,x
	.include 'def:rd001b.def'

;;;record	cusidx
;;;	.include 'def:rd002a.def'

record	crhold
	.include 'def:rd195a.def'

External Function
	f_item		,a	;return item #

External Function
	f_roundU	,d	;round up a number

external function
	trn3		,d	;truncate last 3 decmal places

external function
	pde		,d	;map Price Duct End treatment

;--- end of External Functions ---

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT
;---------------------------------------
RECORD	DMPDAT
	DMAT	,A1
	DHT	,D3
	DLEN	,D3
	DHARA	,3D3		;ARRAY OF SECTION HEIGHTS
	DLARA	,3D3		;ARRAY OF SECTION LENGTHS
	DITEM	,A15

; for SSS items SG Pipe SST or Alum
record
	totlen	,d10
	s_adj_len	,d10
	tot_316	,d10
	tot_304	,d10
	tot_alu	,d10
	sstamt	,d10
	aluamt	,d10
	wrkamt	,d10
	s_adj	,d3
	alu_amt	,d10
	sst_amt	,d10
	s304_amt	,d10
	s316_amt	,d10
	ppi	,d10
	s304_lvl	,d3
	s316_lvl	,d3
	alu_lvl		,d3

record
	dia	,d3

	alumdia_18	,40d3	;array of dia
	s304dia_18	,40d3	;array of dia
	s316dia_18	,40d3	;array of dia
	alumtl_18	,40d10	;length in inches by dia
	s304tl_18	,40d10	;length in inches by dia
	s316tl_18	,40d10	;length in inches by dia
	alumprc_18	,40d8	;price by dia
	s304prc_18	,40d8	;price by dia
	s316prc_18	,40d8	;price by dia

	alumdia_20	,40d3	;array of dia
	s304dia_20	,40d3	;array of dia
	s316dia_20	,40d3	;array of dia
	alumtl_20	,40d10	;length in inches by dia
	s304tl_20	,40d10	;length in inches by dia
	s316tl_20	,40d10	;length in inches by dia
	alumprc_20	,40d8	;price by dia
	s304prc_20	,40d8	;price by dia
	s316prc_20	,40d8	;price by dia

	alumdia_22	,40d3	;array of dia
	s304dia_22	,40d3	;array of dia
	s316dia_22	,40d3	;array of dia
	alumtl_22	,40d10	;length in inches by dia
	s304tl_22	,40d10	;length in inches by dia
	s316tl_22	,40d10	;length in inches by dia
	alumprc_22	,40d8	;price by dia
	s304prc_22	,40d8	;price by dia
	s316prc_22	,40d8	;price by dia

	alumdia_24	,40d3	;array of dia
	s304dia_24	,40d3	;array of dia
	s316dia_24	,40d3	;array of dia
	alumtl_24	,40d10	;length in inches by dia
	s304tl_24	,40d10	;length in inches by dia
	s316tl_24	,40d10	;length in inches by dia
	alumprc_24	,40d8	;price by dia
	s304prc_24	,40d8	;price by dia
	s316prc_24	,40d8	;price by dia



; for F3 end treatments...
RECORD	
		,D1,	8
	PD_KEY	,D4
RECORD,X
		,D1
	K4	,4D1
RECORD,X
	TMP_KEY	,D5

RECORD	TOFF
	RT_SZ1	,D2
		,A1,	'/'
	RT_SZ2	,D2
		,A4,	' ON '
	RT_DIA	,D2
		,A*,	'"RD '
	RT_GA	,D2
		,A*,	'GA '
	RT_REST	,A10	

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR

;-------------------
GLOBAL ORDL,	INIT
RECORD ORDLIN		
		.INCLUDE 'DEF:RD045A.def'
RECORD	,X
		.INCLUDE 'DEF:RD045D.def'
RECORD	,X
		.INCLUDE 'DEF:RD045M.def'
ENDGLOBAL


GLOBAL IMS,	INIT

RECORD INVMAS
	.INCLUDE 'DEF:RD041A.DEF'

RECORD DUMINV,X
	.INCLUDE 'DEF:RD041B.DEF'

RECORD
	FIL041	,A14		;ITMMAS FILE NAME
ENDGLOBAL


GLOBAL DUCK,	INIT
	.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL
;------------------------------------------

;;;GLOBAL PAR	,INIT
RECORD PARAM
	.INCLUDE 'DEF:PARAM.DEF'
;;;ENDGLOBAL
;------------------------------------------

RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	F_MEMOS
	.INCLUDE 'DEF:CPMEMO.DEF'

RECORD	COPCTL
	.INCLUDE 'DEF:RD060A.DEF'

RECORD	DUCFIL
	.INCLUDE 'DEF:RD174A.DEF'


RECORD
	MAXARY	,D3,	100	;MAXIMUM ARRAY ELEMENTS
	SDUCRC	,100A45		;DUCT CONFIGURATION RECORDS (SEE DUCREC BELOW)

;;; now must match type and price...		SSQ 3-24-03
	LINSQF	,8D6	; This is just total of the types of liner sqft
	LINPR	,8D8	; This is price of each type of liner

	LINCFG	,D9	;TYPE+PRICE
	LIDX	,D4
	LMAX	,D3,	024
	LINKEY	,24D9	; TYPE+PRICE
	LXSQF	,24D6	; This is just total of the types of liner sqft
	LXPR	,24D8	; This is price of each type of liner

	QTY	,D5
	ITM	,A15
	DES	,A30
	PRC	,D7
RECORD DUCREC
	DUCCFG	,D23	;CONFIGURATION CODE
	DUCSQF	,D7	;SQ FEET OF MATERIAL
	DUCPND	,D7	;POUNDS OF MATERIAL
	DUCGPR	,D7	;GAUGE PRICE
	NEWSEAM	,D1	;MASS CHANGE SEAM TYPE
RECORD ACCREC
	.INCLUDE 'DEF:ACCREC.DEF'

RECORD
	ACOMPA	,A15
	ACOMPB	,A15
RECORD	,X
	COMPA	,D7
		,A8
	COMPB	,D7
		,A8

;;; end of 5-18-06 for scrn5 stuff...


RECORD
	IN_FILE		,A21
	OUT_FILE	,A21


;-- 5-6-15: server name in table...
;;;RECORD	F_PATH
;;;		,A27,	'\\SERVERsbs\DE63\FS_EXPORT\'
;;;	FP_FILE	,A21

RECORD	F_PATH
		,A41,	'\\server-dc01\Trimble\Quote-Vulcan\Round\'
	FP_FILE	,A21

;;;RECORD	F_PATH
;;;		,A200
;;;RECORD
;;;	FP_FILE	,A21
;;;	EX_PATH	,A100


;------------------------------------------------------
record
	WND_D	,D4	;DAMPER
	WD_NAME	,A6

RECORD	WINS
	W_ID	,D4
	WND_1	,D4
	C_WND	,D4

RECORD	WN_NAME
		,A5,	'FABMM'
	WN_TNMBR,D4

RECORD	BUFFER
		,A4096		;FABSHOP DATA BUFFER

RECORD
	.INCLUDE 'DEF:RD001W.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CFGMAT
	.INCLUDE 'DEF:CFGMAT.DEF'


RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'


RECORD	FABDAT
	.include 'def:fabdat.def'

RECORD	FABITM
	F_DESC	,A30


RECORD
	ER1	,A30,	'No COPTBL RTN record          '
;                        123456789012345678901234567890
	ER2	,A30,	'Not in ITMMAS                 '
	ER3	,A30,	'Bad Digit in Damper           '
	ER4	,A30,	'Price Missmatch               '
	ER5	,A30,	'316-SS Spiral: Wrong Acc File '
	ER6	,A30,	'316-SS Spiral: Wrong Button   '
	ER7	,A30,	'316-SS Sprial: Wrong PressureF'
	ER8	,A30
	ER9	,A30,	'Could not open copy work files'
	ER10	,A30
;                        123456789012345678901234567890
RECORD,X
	ER_MSG	,10A30

RECORD
	ERN	,D2
	ERCNT	,D6

RECORD	ERRMSG
		,A*,	'Error storing order: '
	ERR_ORD	,D6


RECORD	SNMSG
		,A2,	'CO'	;ID'S THIS PGM
	S_ORD	,D6

RECORD	ITMMSG
	ITM_ITM	,A15
		,A*,	' IS NOT IN ITMMAS FILE'

RECORD	ORDMSG
		,A*,	'ORDER NUMBER: '
	ORD_ORD	,D6
		,A*,	' HAS JUST BEEN CREATED'



RECORD	TOTALS
	A_GA	,6D2,	16,18,20,22,24,26
	A_WGHT	,6D10
	A_LNDS	,8A30
	A_LNSF	,8D10


RECORD	CHANNELS
	CHNF1	,D2	;FAB SHOP DATA
	CHNF2	,D2	;FABDA2.DDF - DEBUG FILE FOR RDFAB.DBL
	CHNOUT	,D2
;----------------------------------
; these are needed for SCRN1:
	CHN001	,D2	;06
	CHN002	,D2	;07
	CHN060	,D2	;03
	CHN044	,D2	;04
	CHN166	,D2	;16
	CHN182	,D2	;17
	CHN183	,D2	;18
	CHN137	,D2	;29
	CHN138	,D2	;50
	CHN084	,D2	;84
	CHN135	,D3	;135	7-15-15
;----------------------------------

	CHN041	,D2
;;;	CHN042	,D2
	CHN045	,D2
	CHN175	,D2

	CHN_IN	,D2
	CHN_OUT	,D2
	CHN_FD	,D2

RECORD	HDRFIL
		,A4,	'SPL:'
		,A1,	'H'
	H_CUST	,A6

RECORD	LINFIL
		,A4,	'SPL:'
		,A1,	'L'
	L_CUST	,A6

RECORD	DCTFIL
		,A4,	'SPL:'
		,A1,	'D'
	D_CUST	,A6



RECORD	HDR
		,A*,	'   SQFT    WGHT VANE                INCHES    '
		,A*,	'RAIL LINER              LN SQFT'

RECORD	PBUF
	PLINE	,A80


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

RECORD	L_ARA	;note: NO "FI"
	LARA	,10A2,	'FA','FB','FC','FD','FE','FF','FG','FH','FJ','FK'

RECORD	L_DIMS
	DIMS	,10D2

RECORD
	RT_ARA	,6A2
RECORD,X
	RTARA	,A12

RECORD	VDAMPR
	DAMP		,D2	;DAMPER DIMENSIONS
	DAMPER_PRICE	,3D8
	SAVE_DP		,3D8
	FL	,D2	;
	MAXFLEN	,D2
	BAD_DIGIT	,D1
	PARTNO	,A15
	PFF	,A15

RECORD	DMPRIC
	.INCLUDE 'DEF:DMPRIC.DEF'

RECORD TPRICE
	ARRAY	,80D5

RECORD	FILPRC
	FL_DEV	,A3
		,A1,	':'
	FL_NAME	,A6
		,A1,	'.'
	FL_EXT	,A3

; map liner values from description
; 1 = 1" - 1.5#
; 2 = .5" - 2#
; 3 = 1" - 3#
; 4 = None
; 5 = .5" - 3#
; 6 = 2" - 1.5#
; 7 = 1" - 2#
; 8 = 2" - 3#
;
RECORD
	,A13,	'1 in. 1.5 lb.'
	,A13,	'.5 in. 2 lb. '
	,A13,	'1 in. 3 lb.  '
	,A13,	'No Liner     '
	,A13,	'.5 in. 3 lb. '
	,A13,	'2 in. 1.5 lb.'
	,A13,	'1 in. 2 lb.  '
	,A13,	'2 in. 3 lb.  '
;		 1234567890123
RECORD,X
	LINERS	,8A13

RECORD			;LINER ITEM #'S
		,A15,	'DLF1           '	;1" - 1.5#
		,A15,	'DLF1/2         '	;.5" - 2#
		,A15,	'               '
		,A15,	'               '
		,A15,	'               '
		,A15,	'DLF2           '	;2" - 1.5#
		,A15,	'               '
		,A15,	'               '
			;123456789012345
RECORD,X			
	L_ITEM	,8A15

; map values from QE to existing 6 styles:
; 1 = slip/drive
; 2 = TDC
; 3 = Raw - 4 bolt
; 4 = Raw - slip
; 5 = Other S/D
; 6 = Other TDC

RECORD
	S1	,D1	,2		;TDC L'S W/ CORNERS
	S2	,D1	,2		;TDC ASSEMBLED
	S3	,D1	,2		;TDC
	S4	,D1	,2		;TDC
	S5	,D1	,1		;SLIP & DRIVE
	S6	,D1	,1		;STD SLIP & DRIVE
	S7	,D1	,1		;DRIVE SLIP
	S8	,D1	,4		;RAW FOR SLIPS
	S9	,D1	,3		;RAW FOR 4-BOLT
	S10	,D1	,9
	S11	,D1	,9
	S12	,D1	,9
	S13	,D1	,9
	S14	,D1	,9
	S15	,D1	,9
	S16	,D1	,9
	S17	,D1	,9
	S18	,D1	,9
	S19	,D1	,9
	S20	,D1	,9
	S21	,D1	,9
	S22	,D1	,9
RECORD,X
	STYLES	,22D1
record
	enda		,4d1
record,x
	ends		,d4	; to pass fd_f3 from fabdat.def


RECORD
	SEGS	,A12			;first 2 dig are gauge
RECORD,X
	sGA	,D2
	SARA	,5D2

RECORD	VARS
	odisc	,d2
	wnw	,d1		;6-17-20
	x_lpwgt	,d6		;9-12-17
	witmno	,a15
	str	,a20
	N_PRICE	,D10
	N_NUM	,D10
	N_DIG	,D1
	D_OPT	,D1
	D_OPT2	,D1
	D_SFPRIC	,D5
	bf	,a1
	cmpcod	,a3
	D_SQFT	,D6
	D_MIN	,D5	;MIN PRICE
	D_HV	,A1	;(Y/N)
	D_FM	,A1
	D_SL	,A1
	D_CNGBR	,D1
	ddim	,d3
	x	,d5
	y	,d5
	pidx	,d5
	CHN195	,D2
	cr_flag	,d1
	xqty	,d9
	sum_mat	,d3
	savdsc	,a30
	PI6	,D1
	tl	,d6
	EZSS		,D1	;1=F3 ONLY FOR EZ OR SS
	AN		,D2	;ANGLE
	vanflg		,d1
	s_item		,a15	;search item
	l_pp		,a15	;pressed part
	KF1		,D3	;used by getitm
	KF2		,D3	;used by getitm
	KF3		,D5	;used by getitm
	need_f3	,d1
	V_KEY		,D5	; for end treatment
	V_DIG		,D1
	is_pressed	,d1	;1 = pressed part
	distr	,d1
	vanitm		,d1
	exact_match	,d1
	savcfg		,d1
	df1		,d3
	df2		,d3
	df3		,d3
	swfab		,d1
	van_price	,d8
	van_wgt		,d6	;production weight

	van_descr	,a30
	is_fake		,d1
	fake_price	,d8
	fake_descr	,a30
	fake_ga		,d2
	vanrfa		,a6
	defrfa		,a6
	girfa		,a6
	cmrfa		,a6
	xl1		,d2
	xl2		,d2
	xl3		,d2
	clvl1		,d2
	clvl2		,d2
	clvl3		,d2

	TMPSZ	,D3		;FOR SZ1,SZ2
	FIL174	,A14
;;;	V_SIZE1	,D6
;;;	V_SIZE2	,D6
	V_SIZE3	,D6
	diff	,d10
	CALCAT	,D1
	OPNOK	,D1
	LN	,D6
	ORDQTY	,D6
	NUMFTG	,D6		;number of fittings (???L)
	FIL175	,A14
	FIL045	,A14
	RUNTOT	,D10
	SELECT	,D1
	E_FRGHT	,D6
	X_WGHT	,D6
	SZ1	,D3
	SZ2	,D3
	NUMITM	,D2,	04
	ABSQTY	,D8
	RCTITM	,A15
	CFG_ON	,D1
	X_DAMPR	,D1
	IS_15	,D1
	CTR	,D2
	CFG_ITEM	,D1
	PRPPN	,D6
	WTPIN	,D6
	CMX	,D3,	100
	IVFLAG	,D1
	TREC	,D8
	TONH	,D8
	TONO	,D8
	TCOM	,D8
	TREO	,D8
	DECMAL	,D18
	DEC	,D18
	DEC1	,D18
	DEC2	,D18
	DEC3	,D18
	DEC4	,D18
	DGA	,D2
	TDESCR	,A30
	CONFIG_ERROR	,D1
	CONFIG_PRICE	,D8
	BLANKS	,A30
	KITMNO	,A15
	RTLEN	,D2
	RTVAL	,A6
	VYNL	,D1
	INCH	,D3
	DPND	,D6
	DSIZ	,D3
	SP_SIZE	,D3
	RA_SIZE	,D3
	WRANGL	,A12
	DASH	,D2
	IS_CFG	,D1		;1= CFG ITEM
	ORGITM	,A15
	SAVITM	,A15
	C_DESC	,A30		;CONFIG DESC
	F6A	,5A4,	'GALV','ALUM','SST ','PGRP','S.S.'
	BAD_MAT	,D1
	MAT_SEL	,D1
	MAT_MUL	,D4
	P_MULT	,D3
	C_PRICE	,D8		;price SCRNX would have computed...
	C_ERR	,D1
	C_GAUGE	,D2
	XORD	,D6
	KEY	,A16
	CUSKEY	,A6
	RE_PRICE,D1
	PMAX	,D2
	MAXRC2	,D5
	ORGINV	,D5
	LOCTNS	,D2
	PRICES	,D2
	TAXTOT	,D8
	SCUSTP	,A2
	SAVSEQ	,D3
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D5
	FULL	,D1
	INET	,A1
	CCORD	,A1
	TAXFLG2	,D3
	LSTDTE	,D8
	TDATE	,D6
	CONTR	,D1
	CUSTP2	,A2
	ODATE	,D8
	ESTIMA	,D1
	ABORT	,D1
	CUSTCD	,D2
	J_CUST	,D6
;;;	J_JOB	,D10
	J_JOB	,A10
	P_ERROR	,D2	;0 = NO ERRORS PARSING
	F_KEY	,D3
	PC	,D1
	CODE	,D2
	GA	,D2
	DIM1	,D2
	DIM2	,D2
	DIM3	,D2
	ET	,A2
	ITEM	,A15

	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	LOKCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	ENTRY	,A30
	INXCTL	,D1
	A4	,A4
	A5	,A5
	a6	,A6
	A13	,A13
	D18	,D18
	A30	,A30
	I	,D6
	J	,D6
	K	,D6
	SWITCH	,D1
	V	,D1

;
.PROC
	XCALL TERID(V)
	XCALL ASCII (27, E_CHAR)
	xcall who(cmpcod)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	odisc = 			;1-18-21

;;;	CLEAR COPTBL
;;;	TBLCOD = 'SR'
;;;	SR_CMP = CMPCOD
;;;	LOKCTL = 1
;;;	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
;;;	IF (LOKCTL .NE. 0)
;;;		BEGIN
;;;		XCALL MESAG ('NO SERVER NAME FOUND IN TABLE',1)
;;;		GOTO ENDOFF
;;;		END
;;;
;;;	EX_PATH = '\\' + SR_SERVER(1,%TRIM(SR_SERVER)) + '\DE63\FS_EXPORT\'

	CALL INIT_WINDOW

	XCALL RDATE (TDATE)
	XCALL DATE8(TDATE, D_OUT, LSTDTE, D_FMT, D_SW)

DISPLA,
	CLEAR CNGCTL
	XCALL W_DISP(WND_1, WD_CLEAR)
	XCALL W_DISP(WND_1, WD_POS,6,4,'   JOB #')

JOB,
;;;	CTL = '06,16,10,00,#E'
	CTL = '06,16,10,00,AE'
	CALL INPUT
	GOTO (DISPLA,ENDOFF),INXCTL
	J_JOB = ENTRY(1,10)
	GOTO (ANYCNG),CNGCTL
ANYCNG,
	XCALL WANCN(WND_1, 24, CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR),CNGCTL+1
CNGBR,
	GOTO (JOB,JOB),WHATNO
	GOTO ANYCNG
PROCES,

	open (53, o, 'spl:fabout.dat')
	chnout = 53

	open (54, o, 'spl:fabduc.dat')

	PLINE = J_JOB	;;>,	'ZZZZZZZZZZ'
	WRITES (53,PLINE)
	writes (54,pline)	;duct file


;;;	PLINE = '  PC CODE  GA D1    QTY   ITEM#           DESCRIPTON'
	PLINE = 'NUM PC CODE  GA D1    QTY   ITEM#           DESCRIPTON'
	WRITES (53,PLINE)
	CLOSE 53

	WRITES (54, HDR)	;for fittings
	close 54

	CLEAR ERCNT

;-------------------------------------------------------------
; open the export file, export debug (.dat) file, and local "fabdat.ddf" files

	CALL OPEN_X
	IF (.NOT. OPNOK)
		BEGIN
		;work file error message...
		CALL CLOSE_X
		XCALL OLMSG (W_ID, 24, 'Could not open Export files...', 1)
		GOTO DISPLA
		END

; open the hdr and lin work files...

	H_CUST = J_CUST,	'XXXXXX'
	XCALL ISAMC (HDRFIL, 501, 1, 'START=1, LENGTH=6, noDUPS, ASCEND')
	OPEN (4, SU, HDRFIL)		;SPL:HXXXXXX.ISM

	L_CUST = J_CUST,	'XXXXXX'
	XCALL ISAMC (LINFIL, 180, 1, 'START=1, LENGTH=11, DUPS, ASCEND')
;;;	XCALL ISAMC (LINFIL, 180, 1, 'START=1, LENGTH=11, noDUPS, ASCEND')
	OPEN (5, SU, LINFIL)		;SPL:LXXXXXX.ISM

	D_CUST = J_CUST,	'XXXXXX'
	XCALL ISAMC (DCTFIL, 850, 1, 'START=1, LENGTH=21, DUPS, ASCEND')
	OPEN (36, SU, DCTFIL)		;SPL:DXXXXXX.ISM
;-------------------------------------------------------------

	CLEAR S_ORD
	OORDNO = 1	;always order number 1
	INET = 'I'	;internet order
	XCALL SCRN1(OORDNO,ODISC,OLOC,OCUSNM,INET,INXCTL,TAXFLG2,
&      CUSTP2,CCORD,LSTDTE,FULL,V,CONTR,ODATE,ESTIMA,ABORT,CUSTCD,distr)


	XCALL W_DISP(WND_1, WD_CLEAR)
	xcall w_updt
	IF (INXCTL .EQ. 2) GOTO ENDOFF
	if (abort .eq. 1) goto endoff

;-------------------------------------------------------------
; open the export file and parse the data into 'spl:fabdat.ddf'
	XCALL XPARS(CHN_FD, CHN_IN, CHN_OUT, P_ERROR)
;-------------------------------------------------------------

	CALL CLOSE_X			;CLOSE WORK FILES

	ordseq = 1			;start w/ seq # 1

	CLEAR NUMFTG			;NUMBER OF FITTINGS
	CLEAR F_MEMOS			;F3 NOTES
	FOR I FROM 1 THRU 20 F3_KEY(I) = 0
	F3_NUM = 0

	OPEN (CHNF1, I, 'SPL:FABDAT')
	OPEN (CHNF2, O, 'SPL:FABDA2')	;DEBUG FILE FOR RDFAB.DBL
LOOP,
	READS (CHNF1, FABDAT, EOF)	;(FABDAT) FABSHOP EXPORT FILE
	
	CLEAR CLVL3, NEED_F3
	USING FD_PC SELECT
	(10,20),	BEGIN
		;;;	CALL F3_END	;Create f3 notes
			CALL INV
			END

	(30),		CALL RECTANGULAR

	(40),		BEGIN
			CALL CATALOG		;CATALOG ITEM
			CALL SCX		;PROCESS ITEM
			END
	ENDUSING

	WRITES (CHNF2, FABDAT)
	GOTO LOOP

EOF,
	CLOSE CHNF2
	call make_sss			;need SSSP or ASP items?
	call make_memos				;add memo records if any...
	call fit_sum				;summarize fittings

	;re-write ordhdr
	SAVSEQ = ORDSEQ					
	LOKCTL = 1					
	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)	
	ORDSEQ = SAVSEQ					
	OCOMNT(1) = J_JOB				; SAVE JOB #
	LOKCTL = 1					
	XCALL ISIO (4,ORDHDR,OORDNO,WRITE,LOKCTL)	

	SAVSEQ = ORDSEQ
	CONTR = 1		;9-16-04, ALWAYS 1 IN CHANGE
	XCALL SCRNX (SCUSTP,PCODES,PDISCS,PMAX,DCODES,DDISCS,DMAX,TYPSYS,
&		ORDSEQ,OORDNO,TAXFLG2,MAXRC2,ORGINV,LOCTNS,PRICES,
&		RE_PRICE,ODISC,OLOC,OCUSNM,TAXTOT,OPRTF,CONTR,ODATE,CUSTCD,distr)


	IF (ERCNT .GT. 0) 
		BEGIN
		open (chnout, A, 'spl:fabout.dat')
		WRITES (CHNOUT, EJECT)
		CLOSE CHNOUT
		LPQUE('spl:fabout.dat')

		open (54, a, 'spl:fabduc.dat')
		writes (54, eject)
		close 54
	;;; 6-08-09 per brandon >>	lpque ('spl:fabduc.dat')
		END


;---------------------
; scrn5 duct stuff:

	FIL175 = DCTFIL
	FIL045 = LINFIL

	CLEAR E_FRGHT

;---------------------

	XCALL W_DISP(WND_1, WD_POS,23,1,'Copy to live system?')
	XCALL WINPT (WND_1, 23, 24, 01, 01, 'YN', ENTRY, INXCTL, F_KEY)
	GOTO (ENDOFF),INXCTL-1
CPYORD,
	CALL OPEN_CPY
	IF (.NOT. OPNOK)
		BEGIN
		ERN = 9
		CALL ERROR
		GOTO END_COPY
		END

	
	XORD = OORDNO
	CALL COPY_HEADER
	CALL COPY_LINE
	CALL COPY_DUCT

	CALL END_ORDER

	GOTO END_COPY

END_COPY,
	CALL CLOSE_CPY

ENDOFF,
	CALL CLOSE
	IF(S_ORD)
		BEGIN
		XCALL SNMSG(SNMSG,5)
		XCALL PGCHN('CP:ORDMNT',1)
		END

	XCALL FLAGS (7000000)
	STOP
;=================================================
;=================================================


F3_END,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; F3 End Treatment
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; map price duct code to f3 note code
; create "cpmemo.def" data for each fabdat record processed

	CLEAR CLVL3
	v_key = 0
;--------------------------------------------------------
; 10-05-09 SSQ: Special case for BSEC....
;
	if (item .eq. 'BSEC')
	then	v_key = 4			;default to spiral
	else	xcall et (fabdat, segs, mat_sel, v_key)

;;;	xcall et (fabdat, segs, mat_sel, v_key)
;--------------------------------------------------------

	using ezss select
	(1),	using v_key select
		(1),	nop	;safety stock is ok
		(3),	nop	;ez flange is ok
		(),	v_key = 0	;anything else, skip the key
		endusing
	endusing

	IF (V_KEY .LE. 0) call def_f3	;get default f3 key
	IF (V_KEY .LE. 0) RETURN	;still no key

	FOR I FROM 1 THRU F3_NUM			;max number of cpmeno notes
		BEGIN
		IF (F3_KEY(I) .EQ. V_KEY) EXITLOOP
		IF (F3_KEY(I) .EQ. 0) EXITLOOP
		END
	IF (I .GT. 20) RETURN			; max array elements
	IF (F3_KEY(I) .EQ. 0) CALL NEW_F3_KEY	; add new note to array

	CLVL3 = I
	RETURN

NEW_F3_KEY,
	F3_KEY(I) = V_KEY
	
	CLEAR COPTBL
	TBLCOD = 'M3'
	MM_KEY = V_KEY
	LOKCTL = 1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) RETURN

	INCR F3_NUM 
	F3_MEMOL(F3_NUM) = MM_LONG
	F3_MEMOS(F3_NUM) = MM_SHORT
	F3_KEY(F3_NUM) = MM_KEY
	F3_IK(F3_NUM) = MM_IK	
	CLVL3 = F3_NUM
	RETURN
;---------------------------------------------------

def_f3,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; get default f3 key
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear tbl_key
	tblcod = 'IK'
	ik_item = item
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .eq. 0) v_key = ik_f3

	return
;---------------------------------------------------

F2_END,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; F2 End Treatment
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; map price duct code to F2 note code
; create "cpmemo.def" data for each fabdat record processed

	CLEAR CLVL2
	v_key = 0

	call def_F2	;get default F2 key
	IF (V_KEY .LE. 0) RETURN	;still no key

	FOR I FROM 1 THRU F2_NUM			;max number of cpmeno notes
		BEGIN
		IF (F2_KEY(I) .EQ. V_KEY) EXITLOOP
		IF (F2_KEY(I) .EQ. 0) EXITLOOP
		END
	IF (I .GT. 20) RETURN			; max array elements
	IF (F2_KEY(I) .EQ. 0) CALL NEW_F2_KEY	; add new note to array

	CLVL2 = I
	RETURN

NEW_F2_KEY,
	F2_KEY(I) = V_KEY
	
	CLEAR COPTBL
	TBLCOD = 'M2'
	MM_KEY = V_KEY
	LOKCTL = 1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) RETURN

	INCR F2_NUM 
	F2_MEMOL(F2_NUM) = MM_LONG
	F2_MEMOS(F2_NUM) = MM_SHORT
	F2_KEY(F2_NUM) = MM_KEY
	F2_IK(F2_NUM) = MM_IK	
	CLVL2 = F2_NUM
	RETURN
;---------------------------------------------------

def_F2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; get default F2 key
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear tbl_key
	tblcod = 'IK'
	ik_item = item
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .eq. 0) v_key = ik_F2

	return
;---------------------------------------------------

F1_END,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; F1 End Treatment
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; map price duct code to F1 note code
; create "cpmemo.def" data for each fabdat record processed

	MAT_SEL = 1			;default to galv.

	CLEAR CLVL1
	v_key = FD_F1

	IF (V_KEY .LE. 0) call def_F1	;get default F1 key
	IF (V_KEY .LE. 0) RETURN	;still no key

	FOR I FROM 1 THRU F1_NUM			;max number of cpmeno notes
		BEGIN
		IF (F1_KEY(I) .EQ. V_KEY) EXITLOOP
		IF (F1_KEY(I) .EQ. 0) EXITLOOP
		END
	IF (I .GT. 20) RETURN			; max array elements
	IF (F1_KEY(I) .EQ. 0) CALL NEW_F1_KEY	; add new note to array

	CLVL1 = I
	MAT_SEL = f1_f6m(clvl1)

	RETURN

NEW_F1_KEY,
	F1_KEY(I) = V_KEY
	
	CLEAR COPTBL
	TBLCOD = 'M1'
	MM_KEY = V_KEY
	LOKCTL = 1
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) RETURN

	INCR F1_NUM 
	F1_MEMOL(F1_NUM) = MM_LONG
	F1_MEMOS(F1_NUM) = MM_SHORT
	F1_KEY(F1_NUM) = MM_KEY
	F1_IK(F1_NUM) = MM_IK	
	F1_CDPT(F1_NUM) = M1_CDPT
	F1_SS(F1_NUM) = M1_SS
	F1_F6M(F1_NUM) = M1_MATL

	CLVL1 = F1_NUM
	using v_key select
	(64),	s304_lvl = clvl1
	(65),	s316_lvl = clvl1
	(66),	alu_lvl = clvl1
	endusing

	RETURN
;---------------------------------------------------

def_F1,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; get default F1 key
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear tbl_key
	tblcod = 'IK'
	ik_item = item
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .eq. 0) v_key = ik_F1

	return
;---------------------------------------------------

INV,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; ITMMAS items
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR INVMAS, ITEM
	clear cfg_on
	clear ordlin		;4-25-11

; add code here to validate date, ie gauge>16 and <26, code, pc...
	PC = FD_PC
	GA = FD_GA

	ET = FD_ET
;-----------------------------------------------

	CALL GET_RTN_TABLE
	IF (ERN .NE. 0) RETURN	;error reading table

	CALL ORDER_DIMS		;sort dimension data into proper order


	FOR I FROM 1 THRU 6 RT_ARA(I) = RT_A2(I)	;TO PASS ARRAY

	CLEAR IS_CFG

	ITEM = %f_item(RT_RTN, GA, ET, L_DIMS, RT_MATL, RT_DAMP, 
&		RT_CONE, RT_SCOP, RT_SOFF, RT_ANG, RTARA, fd_spl)

;-
	if (item(1,2) .eq. 'SG') call sg_pipe		;5-11-11

	xcall cfg2 (item, segs)
	
; 6-22-11 doesn't look like I'm using need_f3 for anything...
	CLEAR NEED_F3, EZSS

	USING item SELECT	;in case litmno replaced by pressed part
;;;	('BN'),				NEED_F3 = 1	
;;;	('BSEC'),			NEED_F3 = 1
;;;	('C4020','C4420','C4620'),	NEED_F3 = 0	;changed 8-5-08
;;;	('C4820','C4220 '),		NEED_F3 = 0	;changed 8-05-08
;;;	('CV4020','CV4420','CV4620'),	NEED_F3 = 0	;12-15-08
;;;	('CV4820','CV5020','CV4220'),	NEED_F3 = 0
;;;	('C9','C4','CV9','CV4'),	NEED_F3 = 1
;;;	('CT9','CC9','CTV','CCV'),	NEED_F3 = 1
	('C00' thru 'C88'),	BEGIN
				XCALL ALPDC(ITEM(2,3),AN,SWITCH)
				IF(SWITCH.EQ.0)
					BEGIN
				;;;	NEED_F3=1
					EZSS=1
					END
				END

	('E06 ','E08 ','E10 '),	BEGIN
			;;;	NEED_F3 = 1	;ONLY EZ, SS
				EZSS = 1
				END

	('E12 ','E1424','E1624'),	BEGIN
			;;;	NEED_F3 = 1	;ONLY EZ, SS
				EZSS = 1
				END

	('E1824','E2024','E2224'),	BEGIN
			;;;	NEED_F3 = 1	;ONLY EZ, SS
				EZSS = 1
				END

	('E2424'),	BEGIN
			;;;	NEED_F3 = 1	;ONLY EZ, SS
				EZSS = 1
				END


	('EC'),	BEGIN
		tl = %trim(item)
		using item(tl,tl) select
		('S'),	begin
			XCALL ALPDC(ITEM(5,6),AN,SWITCH)
			IF (SWITCH.EQ.0 .AND. AN.GE.16.AND.AN.LE.26) ;ga
			    BEGIN
			    XCALL ALPDC(ITEM(3,4), SARA(1), SWITCH)    ;dia
			    IF (SWITCH.EQ.0)
				BEGIN
			;;;	NEED_F3 = 1
				EZSS = 1
				END
			    END
			end
		('L'),	need_f3 = 0	;large end
		(),	need_f3 = 0	;conical ez tap
		endusing
		END
		
	('ED06','ED0645','ED0690'),	BEGIN
			;;;		NEED_F3=1
					EZSS = 1
					END

	('ED08','ED0845','ED0890'),	BEGIN
			;;;		NEED_F3=1
					EZSS = 1
					END

	('ED10','ED1045','ED1090'),	BEGIN
			;;;		NEED_F3=1
					EZSS = 1
					END

	('ED12','ED1245','ED1290'),	BEGIN
			;;;		NEED_F3=1
					EZSS = 1
					END

	('ED1445','ED1490'),		BEGIN	;can't be ss, spiral instead
			FOR I FROM 1 THRU 4 IF (FD_F3(I).EQ.9) FD_F3(I) = 1
					END
	('ED16','ED18','ED22','ED24'),	BEGIN
			;;;		NEED_F3=1
					EZSS = 1
					END


;;;	('ER0' thru 'ERZ'),		NEED_F3 = 1
;;;	('GA','GE'),			NEED_F3 = 1
	('H06','H08','H10','H12'),	BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

	('H14','H16','H18','H20'),	BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

	('H22','H24'),			BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

	('HD6','HD8','HD10','HD12'),	BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

	('HD14','HD16','HD18','HD20'),	BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

	('HD22','HD24'),		BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END
	('HE6','HE8','HE10','HE12'),	BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

	('HE14','HE16','HE18','HE20'),	BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

	('HE22','HE24'),		BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

	('HO6','HO8','HO10','HO12'),	BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

	('HO14','HO16','HO18','HO20'),	BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

	('HO22','HO24'),		BEGIN
			;;;		NEED_F3 = 1
					EZSS = 1
					END

;;;	('P4','P9','PV4'),		NEED_F3 = 1
;;;	('R16' THRU 'R26'),		NEED_F3 = 1
;;;	('RV16' THRU 'RV26'),		NEED_F3 = 1
;;;	('RW16' THRU 'RW20'),		NEED_F3 = 1
;;;	('SWF','STW'),			NOP		;to prevent next line
;;;	('S9','S4','SV','SW','ST'),	NEED_F3 = 1
;;;	('SC4','SC9'),			NEED_F3 = 1
;;;	('T4','T9','TV4','TV9'),	NEED_F3 = 1
	(),				NEED_F3	= 0
	ENDUSING

	call f1_end
	call f2_end
	call f3_end

;;;	call f3_end
;;;	call f2_end
;;;	call f1_end

;-

SCX,	;catalog items must execute this code to get added to order

	ORGITM = ITEM					;SAVE FOR LCFGIM
	LITMNO = ITEM

; 1-18-10 - fake part...

	clear is_fake, fake_descr, fake_price, fake_ga

	clear coptbl
	tblcod = 'FP'
	fp_item = litmno
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .eq. 0)
		begin
		clear itmkey
		k_item = litmno
		k_f1 = '000'
		k_f2 = '000'
		k_f3 = '00000'
		read (1, invmas, itmkey, keynum:1) [err=no_f_item1]
		fake_price = price	;12-28-09	use price from item input
	no_f_item1,
		litmno = fp_part	;real item
		fake_descr = fp_desc	;fake description
		fake_ga = fp_ga		;fake ga
		is_fake = 1
		end

;------------------------------------------------------------

;------------------------------------

;---------------------------------------------------------
; 4-20-11: now this is set at F1_END,

;;;	MAT_SEL = 1	;hard code GALV for now...
	call f1_end	;8-29-11
;---------------------------------------------------------

;===============================
;scrnx logic
	
	clear c_desc			;5-13-13
	clear x_lpwgt			;9-6-17

	CALL FIND_DAMPER

	GOTO (DAMP_ERR),BAD_DIGIT

;;;	IF (LITMNO.EQ.BLANKS) GO TO ITEM

	KITMNO = LITMNO

	CLEAR TDESCR, CONFIG_ERROR
	SP_SIZE = 1				;in case sph

	USING LITMNO SELECT
	('WR'),				CFG_ITEM = 1
	('R16' THRU 'R26'),		CFG_ITEM = 1
	('RV16' THRU 'RV26'),		CFG_ITEM = 1
	('T4','T9','TV4','TV9'),	CFG_ITEM = 1
	('C4020','C4420','C4620'),	CFG_ITEM = 2	;SSQ 10-14-03
	('C4820','C4220 '),		CFG_ITEM = 2	;SSQ 10-14-03
	('C9','C4','CV9','CV4'),	CFG_ITEM = 1
	('CT','CC','CTV','CCV'),	CFG_ITEM = 1
	('ER'),				CFG_ITEM = 1
	('BN'),				CFG_ITEM = 1	
	('P4','P9','PV4'),			CFG_ITEM = 1
	('S9','S4','SV','SW','SC','ST'),	CFG_ITEM = 1
	('SG','SX'),			CFG_ITEM = 1
	('RT','RTV'),			CFG_ITEM = 1
	('OB','PB','SBD','BDD'),	
					BEGIN
					CFG_ITEM = 1
					GOTO CALL_CONFIG
					END
	('SLF','SLB'),			BEGIN		;LOUVERS
					CFG_ITEM = 1
					GOTO CALL_CONFIG
					END
	(),				CFG_ITEM = 2	;item not configurable
	ENDUSING
	
	GOTO (REG_ITEM),CFG_ITEM -1

;=================================================================
;===== following replace by code taken from scrnx.cp 4-27-09 ...
;------------------------------------------------------------
;;;;SSQ 2-17-03
;;;; if configurable item but material is not galv, cannot be
;;;; stamped part, so don't look up in itmmas - go straight to config...
;;;
;;;	IF(CFG_ITEM.EQ.1 .AND. MAT_SEL.NE.1) GOTO CALL_CONFIG
;;;;------------------------------------------------------------
;;;
;;;	CALL GETITM			;check if cfg item is in itmmas...
;;;	if (is_pressed) goto skip_cfg
;;;
;;;	GOTO (call_config,CALL_CONFIG,BADITM),IVFLAG+1	;(found,not-found,abort)
;;;CALL_CONFIG,
;;;	CALL CONFIG			;call the configurator
;;;	GOTO (BADITM), CONFIG_ERROR
;;;
;============ end of 4-27-09 update
;=================================================================
;==== code added from scrnx...

	CALL GETITM
	IF (EXACT_MATCH .and. IS_PRESSED)
		BEGIN
		CFG_ITEM = 2	;Pressed is not cfg
		GOTO SKIP_CFG		;pressed parts
		END

	IF (EXACT_MATCH .and. SAVCFG.EQ.2) 
		BEGIN
		CFG_ITEM = 2	;this will prevent re-pricing
		GOTO SKIP_CFG	;not cfg'd
		END

	IF (CFG_ITEM.EQ.1 .AND. MAT_SEL.NE.GALV) GOTO CALL_CONFIG

CALL_CONFIG,
	CALL CONFIG			;call the configurator
	GOTO (BADITM), CONFIG_ERROR
	IF (EXACT_MATCH.OR.IVFLAG.EQ.0) GOTO SKIP_CFG	;2-12-08

	XCALL ISIO (1, INVMAS, LITMNO, READ, LOKCTL)	;just read instead of getitm
	IF (LOKCTL .NE. 0) GOTO BADITM	;bad item
	GOTO SKIP_CFG

;========================================================================
REG_ITEM,
	CALL GETITM
	GOTO (BADITM, BADITM, BADITM),IVFLAG

	x_lpwgt = van_wgt
	clear wnw		;6-17-20
SKIP_CFG,
	IF (IP_UM .EQ. '$') 	;9-06-17
		BEGIN
		CLEAR TBL_KEY
		TBLCOD='DD'
		TBLKEY=USRDEF
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
		IF (.NOT.LOKCTL)	;6-17-20
			begin
			x_LPWGT=(DD_DLR*PRICE)#4	;XXX,XXX instead of X,XXX.XX for these
			wnw = 1				;whole lbs
			end

	;;;	IF (.NOT.LOKCTL)x_LPWGT=(DD_DLR*PRICE)#2
		END

	IF (IS_15)	USING PRDCAT SELECT	;SSQ 9-9-03
			('C' THRU 'H'),	NOP
			('K1','K3'),	NOP
			(),	BEGIN
				XCALL OLMSG(WND_1, 23,'INVALID EZ FLANGE ITEM',2)
				GOTO BADITM
			;;;	GOTO ITEM
				END
			ENDUSING

	COL = 1
	LITMNO = ITEMNO
	XCALL W_DISP(W_ID, WD_POS,ROW,COL,ITEMNO)
	IF (LDAMPR) XCALL W_DISP(W_ID,WD_POS,ROW,COL+10,' DAMPER')

;;;	savdsc = descr			;4-20-11
	savdsc = van_descr		;5-31-17
	IF (IS_CFG) savdsc = C_DESC	;use cfg desc. (ssq 5-13-13)


	CALL CHK_F6_MATL		;SSQ 4-30-03
;;;	GOTO (ITEM), BAD_MAT

	IF (LITMNO.EQ.'WR')
	BEGIN
	  LDESCR = savdsc
	  IF (DASH.OR.INCH) LDESCR(1,8) = WRANGL(1,7)		;;;
	  IF (DASH) LDESCR(DASH,DASH) = '/'		;;;
	  IF (INCH) LDESCR(INCH,INCH) = '"'		;;;
	  savdsc = ldescr	;ssq 1-20-10, ldescr gets set to descr below...
	END


	call qty_b_price					;ssq 4-13-06
	c_price = ((price*cmx)#3)*10				;from itmmas

	if (cfg_item.eq.2 .or. cfg_on.eq.0) goto skip_adj

	call cfg_price_routine
skip_adj,

	if (ldampr) call add_damper_price
	if (p_mult .gt. 0) call add_matl_mult

;;;	if (c_price .ne. fd_price) 
	diff =%abs(c_price - fd_price)
	call chk_prc_dif


;===============================
;;;	IF (IS_CFG) DESCR = C_DESC		;use cfg desc.
	IF (IS_CFG) savdsc = C_DESC		;use cfg desc.

	ORDQTY = FD_QTY				;QTY ORDERED

INSR_FIT,
	FD_DESC = savdsc

	CALL WRT_FABOUT

sg_item,
	SAVITM = LITMNO
	X_DAMPR = LDAMPR


	CLEAR ORDLIN

	IF (TXFLAG .EQ. 'Y') LTXFLG = 1
	LDAMPR = X_DAMPR
	LCFGIM = ORGITM
	LORDNO = OORDNO
	LINSEQ = ORDSEQ
	ORDSEQ = ORDSEQ + 1

	lmat = mat_sel			;ssq 5-21-09
	L_CFG = 2
	LITMNO = SAVITM

;;;	LDESCR = DESCR
	LDESCR = savdsc

	LQTYOR = ORDQTY
	LLOC = OLOC
	LPRDCD = PRDCAT
	LUOFM = SUOFM

	using litmno select		;ssq 4-11-06
	('SG','SX'),	luofm = 'EA'
	endusing

	LCOST = AVGCST#1
	LPRICE = FD_PRICE
	LSAVPR = LPRICE
	LITMWT = WEIGHT
	LSTOKT = STOCK
	LDEPT = USRDEF
	LSRTSQ = PRICCD

	LMULT = 0

	LTYPE = 'L'

	LF1 = 0
	LF2 = 0
	LF3 = 0
	LF4 = 0
	LMSQ1 = clvl1
	LMSQ2 = clvl2
	LMSQ3 = CLVL3
	if (clvl3 .gt. 0) LF3 = F3_KEY(CLVL3)
	if (clvl1 .gt. 0) lf1 = f1_key(clvl1)

	lpwgt = x_lpwgt			;ssq 9-12-17
	lpwnw = wnw			;ssq 6-17-20

	XCALL ISIO (5, ORDLIN, ORDKEY, STORE, LOKCTL)
;;;	store (5, ordlin, ordkey)

	RETURN
;---------------------------------------------------
CONFIG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; BRANCH TO PRODUCT CONFIGURATORS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TDESCR=LDESCR			;SAVE IN CASE USER HAS CHANGED
	CONFIG_PRICE = LPRICE		;ssq 10/16/02 moved here
	IF(CFG_ON.EQ.1) RETURN		;ALREADY CONFIGRED
	CFG_ON = 1

	USING LITMNO SELECT		;SSQ 5-27-03
	('SG','SX'),	IF(%TRIM(LITMNO) .LT. 7) RETURN
	(),		IF(%TRIM(LITMNO) .LT. 5) RETURN
	ENDUSING

	CLEAR CONFIG_ERROR

	USING LITMNO SELECT
	('C4020','C4420','C4620'),	RETURN	;SSQ 2-19-03
	('C4820','C4220 '),		RETURN	;SSQ 2-19-03
	('WR'),				CALL R_ANGLE
	('R16' THRU 'R26'),		CALL CALL_CFG
	('RV16' THRU 'RV26'),		CALL CALL_CFG
	('T4','T9','TV4','TV9'),	CALL CALL_CFG
	('C9','C4','CV9','CV4'),	CALL CALL_CFG
	('CT','CC','CTV','CCV'),	CALL CALL_CFG
	('ER'),				CALL CALL_CFG
	('BN'),				CALL CALL_CFG	
	('P4','P9','PV4'),			CALL CALL_CFG

	('S9','S4','SV','SW','SC','ST'),CALL CALL_CFG
	('SG','SX'),			CALL SPIRAL
	('RT','RTV'),			CALL R_TAKEOFF
;uncommented 5-13-13...
	('OB','PB','SBD','BDD'),	CALL CFG_DAMPER	;BALANCING DAMPER
	('SLF','SLB'),			CALL CFG_DAMPER	;LOUVERS
;;;	('OB','PB','SBD','BDD'),	CALL CFG_DAMPER	;BALANCING DAMPER
;;;	('SLF','SLB'),			CALL CFG_DAMPER	;LOUVERS
;----------------------------------------------------------------------
	ENDUSING
	RETURN
;----------------------------------------------------

CALL_CFG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; internal routine so I can see parameter list...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (ldampr) litmno = partno
	clear vanitm

	clear c_gauge			;8-4-10 ssq
	if (fake_ga .gt. 0) c_gauge = fake_ga
	witmno = litmno			;9-6-17
;;;	XCALL CFG(LITMNO,CONFIG_PRICE,TDESCR,CONFIG_ERROR,MAT_SEL,ST_GUAGE)
	XCALL CFG(LITMNO,CONFIG_PRICE,TDESCR,CONFIG_ERROR,MAT_SEL,C_GAUGE)
	IF (CONFIG_ERROR .EQ. 0) 
		BEGIN
		IS_CFG = 1
		C_DESC = TDESCR
		xcall cfg_w (witmno, x_lpwgt, config_error,mat_sel,c_gauge) 
		END
	RETURN
;----------------------------------------------------
;-------------
;;ssq 5-13-13
CFG_DAMPER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PRELIMINARY CALCS FOR ALL DAMPERS...
		;;; AND LOUVERS (SSQ 12-16-03
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	clear exact_match
	ivflag = 1		;will force read of itmmas
	IS_CFG = 1		;5-13-13

	CLEAR DMPDAT, CONFIG_PRICE, CONFIG_ERROR, TDESCR

	DITEM = LITMNO(1,3)
	DMAT = LITMNO(4,4)		;MATERIAL

	USING DMAT SELECT		;NO MATERIAL IF GALV
	('A','S'),	DITEM = LITMNO
	('U','P'),	DITEM = LITMNO		;COLOR CLAD, PG
	('0' THRU '9'),	DITEM(5,15) = LITMNO(4,14)	;ssq 2-18-03
	(),		GOTO DAMPER_ERROR
	ENDUSING

	TL = %INSTR(1,DITEM,'X')			;FIND THE X

	ONERROR DAMPER_ERROR
	IF (TL .LE. 1) 
	THEN	BEGIN
		DLEN = DITEM(5,6)
		DHT = DITEM(7,8)
		END
	ELSE	BEGIN
		DLEN = DITEM(4,TL-1)
		DHT = DITEM(TL+1,%TRIM(DITEM))
		END
	OFFERROR

	IF (DLEN.LE.99)
	THEN	BEGIN
		TDESCR(1,2) = DLEN,	'ZX'
		TDESCR(3,3) = 'X'
		TDESCR(4,6) = DHT,	'ZZX' [LEFT]
		END
	ELSE	BEGIN
		TDESCR(1,3) = DLEN,	'ZZX'
		TDESCR(4,4) = 'X'
		TDESCR(5,7) = DHT,	'ZZX' [LEFT]
		END

;148x148 PARALLEL BLADE STS STL
;148x148 STD LOUVER FLG
;123456789012345678901234567890


	USING DMAT SELECT		;this get's overriden in lvr
	('A'),	TDESCR(24,30) = 'ALUM'
	('S'),	TDESCR(24,30) = 'STS STL'
	('S'),	TDESCR(24,30) = 'SS'
	('U'),	TDESCR(24,30) = 'CLAD'
	('P'),	TDESCR(24,30) = 'PGRP'
	ENDUSING

	USING DITEM SELECT
	('OB','PB'),	GOTO SMP	;BALANCING
	('SB'),		GOTO SBD	;SINGLE BLADE
	('BD'),		GOTO BDD	;BACKDRAFT
	('SLF','SLB'),	GOTO LVR	;LOUVER
	ENDUSING

DAMPER_ERROR,
	OFFERROR
	XCALL OLMSG(WND_1, 23,'INVALID DAMPER PART NUMBER',1)
	CONFIG_ERROR = 1
	RETURN
;----------------------------------------------------------

SMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; BALANCING DAMPER CONFIGURATOR
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	IF (DHT.LT.12) GOTO DAMPER_ERROR	;10/28/02 per SM
;;;	IF (DLEN.LT.12) GOTO DAMPER_ERROR	;10/28/02 per SM
	IF (DHT.LT.6) GOTO DAMPER_ERROR	;10/28/02 per SM
	IF (DLEN.LT.6) GOTO DAMPER_ERROR	;10/28/02 per SM

	IF (DITEM(1,1) .EQ. 'P')
	THEN	TDESCR(9,22) = 'PARALLEL BLADE'
	ELSE	TDESCR(10,22) = 'OPPOSED BLADE'

	IF (DITEM(3,3) .EQ. 'S') TDESCR(24,30) = ' SEALED'

	FL_NAME = 'DMPRIC'		;SSQ 9-9-03
	OPEN(9,I,FILPRC)		;SSQ 9-9-03
;;;	OPEN(9,I,'SMC:DMPRIC.SMC')
	IF (DITEM(3,3) .EQ. 'N')
	THEN	READ(9,DMPRIC,1)	;NON-SEALED MATRIX
	ELSE	READ(9,DMPRIC,2)	;SEALED MATRIX
	CLOSE 9

	DDIM = 48
	CALL GET_DAMPER_SECTIONS

	DITEM(5,15) = 
	CLEAR CONFIG_PRICE

	FOR I FROM 1 THRU 3
		BEGIN
		USING DHARA(I) SELECT		;FIND THE MATRIX ROW
		(01 THRU 11),	X = 1
		(12 THRU 18),	X = 2
		(19 THRU 24),	X = 3
		(25 THRU 32),	X = 4
		(33 THRU 40),	X = 5
		(41 THRU 48),	X = 6
		(),		X = 0
		ENDUSING
		FOR J FROM 1 THRU 3
		IF (X.GT.0 .AND. DLARA(J).GT.0)
			BEGIN
			USING DLARA(J) SELECT		;FIND THE MATRIX COLUMN
			(01 THRU 18),	Y = 1
			(19 THRU 24),	Y = 2
			(25 THRU 30),	Y = 3
			(31 THRU 36),	Y = 4
			(37 THRU 42),	Y = 5
			(43 THRU 48),	Y = 6
			(),		Y = 0
			ENDUSING
	
			PIDX = (X-1)*6 + Y		;GET THE INDEX

			IF (PIDX.GT. 0)
&			CONFIG_PRICE = CONFIG_PRICE+(DM_PRICE(PIDX)*DM_MULT)#1	;TO .XXX
			END
		END

;;;	USING DMAT SELECT
;;;	('A'),	CONFIG_PRICE = (CONFIG_PRICE * 175)#2	;ALUM
;;;	('S'),	CONFIG_PRICE = (CONFIG_PRICE * 225)#2	;STAINLES
;;;	ENDUSING


	USING MAT_SEL SELECT
	(ALUM),	CONFIG_PRICE = (CONFIG_PRICE * 200)#2	;ALUM
	(SST),	CONFIG_PRICE = (CONFIG_PRICE * 225)#2	;STAINLESS
	ENDUSING

	if (cmpcod .ne. 'CAT') CONFIG_PRICE = CONFIG_PRICE + DM_QUAD*10	;SSQ 6-20-08
	CALL ROUND_NICKLE		;ROUND CONFIG_PRICE UP TO NEAREST .05
	
	C_DESC = TDESCR				;5-13-13
	LITMNO = DITEM(1,4)
	RETURN
;----------------------------------------------------------

SBD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; SINGLE BLADE DAMPER
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  changed per MikeM 2-19-03...
;;;	IF (DHT.LT.06 .OR. DHT.GT.12) GOTO DAMPER_ERROR
;;;	IF (DLEN.LT.06 .OR. DLEN.GT.48)GOTO DAMPER_ERROR
	IF (DHT.LT.04 .OR. DHT.GT.12) GOTO DAMPER_ERROR
	IF (DLEN.LT.04 .OR. DLEN.GT.48)GOTO DAMPER_ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	TDESCR(7,20) = 'SINGLE BLADE'

	CLEAR TBL_KEY
	TBLCOD = 'SB'
	READ (CHN182,COPTBL,TBL_KEY)[ERR=DAMPER_ERROR]
	CONFIG_PRICE = (DLEN*DHT)*SB_PCUN + SB_QUAD
	IF (CONFIG_PRICE .LT. SB_MIN) CONFIG_PRICE = SB_MIN

	USING DMAT SELECT
	('A'),	CONFIG_PRICE = (CONFIG_PRICE*SB_AMUL)#2
	('S'),	CONFIG_PRICE = (CONFIG_PRICE*SB_SMUL)#2
	ENDUSING

	C_DESC = TDESCR				;5-13-13

	CONFIG_PRICE = (CONFIG_PRICE*SB_PMUL)#1		;.XXX
	LITMNO = DITEM(1,4)
	CALL ROUND_NICKLE		;ROUND CONFIG_PRICE UP TO NEAREST .05
	RETURN
;----------------------------------------------------------
LVR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; LOUVERS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (DHT.LT.06) GOTO DAMPER_ERROR
	IF (DLEN.LT.06)GOTO DAMPER_ERROR
;;;	D_SQFT = ((DLEN*DHT*1000)/144)#1	 	;SQ FT (.xx)
	D_SQFT = ((DLEN*DHT*1000)/144)		 	;SQ FT (.xxx)

;148x148 STD LOUVER FLG
;123456789012345678901234567890

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; added box/flange 1-24-06 ssq

;;;	USING DITEM SELECT
;;;	('SLB'),	TDESCR(8,30) = 'STD LOUVER BOX'
;;;	('SLF'),	TDESCR(8,30) = 'STD LOUVER FLANGE'
;;;	ENDUSING
	USING DITEM SELECT
	('SLB'),	BEGIN
			TDESCR(8,30) = 'STD LOUVER BOX'
			BF = 'B'
			END
	('SLF'),	BEGIN
			TDESCR(8,30) = 'STD LOUVER FLANGE'
			BF = 'F'
			END
	ENDUSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	USING DMAT SELECT		;this get's overriden in lvr
	('A'),	TDESCR(27,30) = 'ALUM'
	('U'),	TDESCR(27,30) = 'CLAD'
	('P'),	TDESCR(27,30) = 'PGRP'
	ENDUSING

	call get_window_d				;ssq 5-13-13

L_DISPLA,
	CLEAR CNGCTL
	XCALL W_PROC(WP_PLACE, WND_D, 8, 15)
	XCALL W_DISP(WND_D, WD_CLEAR)
	XCALL W_DISP(WND_D,WD_POS,1,1,'CHOOSE ONE OF THE FOLLOWING:')
	XCALL W_DISP(WND_D,WD_POS,2,4,'2. 2" FRAME')
	XCALL W_DISP(WND_D,WD_POS,3,4,'4. 4" FRAME (STD)')
	XCALL W_DISP(WND_D,WD_POS,4,4,'6. 6" FRAME')
	XCALL W_UPDT
L_OPT,
	XCALL WINPT(WND_D, 1, 32, 01, 00, '#', ENTRY, INXCTL)
	D_OPT = ENTRY(1,1)
	IF (D_OPT .EQ. 0) D_OPT = 4

	USING D_OPT SELECT
	(2,4,6),	BEGIN
			ENTRY(1,1) = D_OPT,'X'
			XCALL W_DISP(WND_D,WD_POS,1,32,ENTRY(1,1))
			XCALL W_UPDT
			IF(D_OPT.NE.4) 
				BEGIN
				TDESCR(8,8) =
				TDESCR(9,9) = D_OPT,'X'
				TDESCR(10,10) = '"'
				END
			END
	(),		GOTO L_DISPLA
	ENDUSING

	CNGCTL = 2
	XCALL WANCN(WND_D, 7, CNGCTL, WHATNO)
	GOTO (L_OPT),CNGCTL

	XCALL W_DISP(WND_D, WD_CLEAR)
	XCALL W_PROC(WP_REMOVE, WND_D)
	XCALL W_UPDT

	CLEAR TBL_KEY
	TBLCOD = 'LV'
	TBLKEY = BF		;SSQ 1-24-06
	READ (CHN182,COPTBL,TBL_KEY)[ERR=DAMPER_ERROR]

	IF (DMAT.EQ.'A' .OR. DMAT.EQ.'U')
	THEN	D_MIN = LV_AMIN			;ALUM/COLOR CLAD
	ELSE	D_MIN = LV_GMIN
	D_MIN = D_MIN*10			;to .xxx

	USING D_OPT SELECT
	(2),		USING DMAT SELECT
			('A','U'),	D_SFPRIC = LV_A02
			(),		D_SFPRIC = LV_G02
			ENDUSING
	(4),		USING DMAT SELECT
			('A','U'),	D_SFPRIC = LV_A04
			(),		D_SFPRIC = LV_G04
			ENDUSING
	(6),		USING DMAT SELECT
			('A','U'),	D_SFPRIC = LV_A06
			(),		D_SFPRIC = LV_G06
			ENDUSING
	ENDUSING
;;;	CONFIG_PRICE = (D_SQFT*D_SFPRIC)#1
	CONFIG_PRICE = (D_SQFT*D_SFPRIC)#2	;ssq 12-16-03
	IF (CONFIG_PRICE .LT. D_MIN) CONFIG_PRICE = D_MIN
	CALL ROUND_NICKLE		;ROUND CONFIG_PRICE UP TO NEAREST .05

	C_DESC = TDESCR				;5-13-13
	LITMNO = DITEM(1,4)
	RETURN
;----------------------------------------------------------

BDD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; BACKDRAFT DAMPER
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (DHT.LT.06) GOTO DAMPER_ERROR
	IF (DLEN.LT.06)GOTO DAMPER_ERROR
	D_SQFT = ((DLEN*DHT*1000)/144)#1	 	;SQ FT (.xx)

	TDESCR(7,13) = 'B-DRAFT'

	DDIM = 36
	CALL GET_DAMPER_SECTIONS

	CLEAR TBL_KEY
	TBLCOD = 'BD'
	READ (CHN182,COPTBL,TBL_KEY)[ERR=DAMPER_ERROR]

; after this, price will be .XXX, convert all add-on's to 3 dec places...
;;;	CONFIG_PRICE = (CONFIG_PRICE*BD_PMUL)#1		;.XXX

; Backdraft damper add-on's ...

	call get_window_d				;ssq 5-13-13
D_DISPLA,
	CLEAR CNGCTL
	XCALL W_DISP(WND_D, WD_CLEAR)
	XCALL W_PROC(WP_PLACE, WND_D, 8, 15)
	XCALL W_DISP(WND_D,WD_POS,1,1,'CHOOSE ONE OF THE FOLLOWING:')
	XCALL W_DISP(WND_D,WD_POS,2,4,'1. REGULAR')
	XCALL W_DISP(WND_D,WD_POS,3,4,'2. SPRING LOADED')
	XCALL W_DISP(WND_D,WD_POS,4,4,'3. COUNTER BALANCED')
	XCALL W_UPDT
D_OPT,
	XCALL WINPT(WND_D, 1, 32, 01, 00, '#', ENTRY, INXCTL)
	D_OPT = ENTRY(1,1)
	IF (D_OPT .EQ. 0) D_OPT = 1

	IF (D_OPT.LT.1 .OR. D_OPT.GT.3) GOTO D_DISPLA
	ENTRY(1,1) = D_OPT,'X'
	XCALL W_DISP(WND_D,WD_POS,1,32,ENTRY(1,1))
	XCALL W_UPDT
D_ANY,
	CNGCTL = 2
	XCALL WANCN(WND_D, 7, CNGCTL, WHATNO)
	GOTO (D_OPT),CNGCTL

D2_DISP,
	CLEAR CNGCTL
	XCALL W_DISP(WND_D, WD_CLEAR)
	XCALL W_DISP(WND_D,WD_POS,1,1,'CHOOSE ONE OF THE FOLLOWING:')
	XCALL W_DISP(WND_D,WD_POS,2,4,'1. REGULAR')
	XCALL W_DISP(WND_D,WD_POS,3,4,'2. FLANGED')
	XCALL W_DISP(WND_D,WD_POS,4,4,'3. HIGH VELOCITY')
	XCALL W_UPDT
D2_OPT,
	XCALL WINPT(WND_D, 1, 32, 01, 00, '#', ENTRY, INXCTL)
	D_OPT2 = ENTRY(1,1)
	IF (D_OPT2 .EQ. 0) D_OPT2 = 1
	IF (D_OPT2.LT.1 .OR. D_OPT2.GT.3) GOTO D2_DISP
	ENTRY(1,1) = D_OPT2,'X'
	XCALL W_DISP(WND_D,WD_POS,1,32,ENTRY(1,1))
	XCALL W_UPDT

	CNGCTL = 2
	XCALL WANCN(WND_D, 7, CNGCTL, WHATNO)
	GOTO (D2_OPT),CNGCTL

	XCALL W_DISP(WND_D, WD_CLEAR)
	XCALL W_PROC(WP_REMOVE, WND_D)
	XCALL W_UPDT

	USING D_OPT2 SELECT
	(1),	D_SFPRIC = BD_PCUN		;REGULAR PRICE/SQ FT
	(2),	D_SFPRIC = BD_FM		;FLANGED
	(3),	D_SFPRIC = BD_HV		;HIGH VEL
	ENDUSING


	BD_SL = BD_SL * 10		;.XXX
	BD_MIN = BD_MIN * 10		;.XXX
	CONFIG_PRICE = (D_SQFT*D_SFPRIC)#1

	USING D_OPT SELECT
	(2),	CONFIG_PRICE = CONFIG_PRICE + BD_SL	;SPRING LOADED
	(3),	CONFIG_PRICE = CONFIG_PRICE + BD_SL	;COUNTER BALANCE
	ENDUSING

	IF (CONFIG_PRICE .LT. BD_MIN) CONFIG_PRICE = BD_MIN	;MINIMUM...

	USING DMAT SELECT					;MATERIAL
	('A'),	CONFIG_PRICE = (CONFIG_PRICE*BD_AMUL)#2
	('S'),	CONFIG_PRICE = (CONFIG_PRICE*BD_SMUL)#2
	ENDUSING

	CONFIG_PRICE = (CONFIG_PRICE*BD_PMUL)#2		;.XXX	PROFIT

	LITMNO = DITEM(1,4)
	CALL ROUND_NICKLE		;ROUND CONFIG_PRICE UP TO NEAREST .05

	CLEAR STR
	USING D_OPT SELECT
	(2),	STR = 'SPNG/'
	(3),	STR = 'CB/'
	ENDUSING

	TL = %TRIM(STR)
	USING D_OPT2 SELECT
	(2),	STR = STR(1,TL) + 'FLG'
	(3),	STR = STR(1,TL) + 'HI-VEL'
	ENDUSING
	
	TL = %TRIM(STR)
	IF (STR(TL,TL) .EQ. '/') STR(TL,TL) = 
	TDESCR(15,30) = STR
	C_DESC = TDESCR				;5-13-13

	RETURN
;------------------------------------------------------

ROUND_NICKLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	return	;3-2-11 per bill

	N_PRICE = CONFIG_PRICE#1	;.XXX -> .XX
	N_NUM = N_PRICE/10*10
	N_DIG = N_PRICE - N_NUM		;ENDING DIGIT .XX

	USING N_DIG SELECT
	(0),	RETURN		;ALREADY ENDS IN ZERO
	(.LE. 5),	N_PRICE = N_NUM + 5	;.05
	(.GT. 5),	N_PRICE = N_NUM + 10	;.10
	ENDUSING
	
	CONFIG_PRICE = N_PRICE * 10	;.XX -> .XXX
	RETURN
;----------------------------------------------------------
GET_DAMPER_SECTIONS,
; new logic - look up dimensions in table...


	DHARA(1) = DHT
	CLEAR TBL_KEY
	TBLCOD = 'SZ'
	SZ_KEY = DHT	;FIND DIM'S FOR THIS HGHT
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
		BEGIN
		DHARA(1) = SZ_S1
		DHARA(2) = SZ_S2
		DHARA(3) = SZ_S3
		END

	DLARA(1) = DLEN
	CLEAR TBL_KEY
	TBLCOD = 'SZ'
	SZ_KEY = DLEN	;FIND DIM'S FOR THIS HGHT
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
		BEGIN
		DLARA(1) = SZ_S1
		DLARA(2) = SZ_S2
		DLARA(3) = SZ_S3
		END
	RETURN
;--------------------------------------------------------

;;ssq 5-13-13
;-------------

CHK_F6_MATL,
	clear p_mult
;;;	MAT_SEL = PI6		;already set...

	USING LITMNO SELECT
	('SG','SX'),	NOP
	('RT','RTV'),	NOP
	('WR'),		NOP
	('J35'),	NOP
	(),		IF (CFG_ITEM .NE. 2) RETURN	;MUST BE NON-CFG PART
	ENDUSING
	
	if (mat_sel.eq.galv .and. fd_f1.eq.68) p_mult = 120	;G-90
	if (mat_sel.le.galv .and. lmat.le.1) return	;not set, or, galv

	CLEAR TBL_KEY
	TBLCOD = 'CF'
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_CF_MUL]

	USING MAT_SEL SELECT
;;;	(GALV),	MAT_MUL = 100		;1.00
	(GALV),	IF (FD_F1.EQ.68)
		THEN	MAT_MUL = 120		;1.20 G-90
		ELSE	MAT_MUL = 100		;1.00
	(ALUM),	MAT_MUL = ALUMUL
	(PGRP),	MAT_MUL = PGRMUL
	(SST),	MAT_MUL = SSTMUL
	(VIN),	MAT_MUL = PVSMUL		;7-12-17
	ENDUSING

	I = 1
CF6L,
	A4 = F6A(I)			;CHECK FOR EACH MAT'L
	TL = %INSTR(1,savdsc,A4)
	IF (TL .LE. 1) 
		BEGIN
		INCR I
		IF(I.LE.5) GOTO CF6L
		END

	IF (TL .LE. 1) 
		BEGIN
	;;;	BAD_MAT = 1	;these 2 lines removed 7-25-11
	;;;	XCALL OLMSG(WND_1, 23,'INVALID ITEM FOR CURRENT MATERIAL',2)
		RETURN
		END

	USING MAT_SEL SELECT
	(GALV),	savdsc(TL,TL+3) = 'GALV'
	(ALUM),	savdsc(TL,TL+3) = 'ALUM'
	(PGRP),	savdsc(TL,TL+3) = 'PGRP'
	(SST),	savdsc(TL,TL+3) = 'SST'
	ENDUSING


	P_MULT = MAT_MUL

NOT_CF_MUL,
	RETURN
;----------------------------------------------------

;----------------------------------------------------
ADD_MATL_MULT,	;;;;;;;;;;;;;;;;;;;;;;;;;

	c_PRICE=(c_price*P_MULT)#2
	RETURN
;---------------------------------------------------	


;-
QTY_B_PRICE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'I1'
	I1_KEY = LITMNO
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .NE. 0) RETURN

	ABSQTY = fd_qty				;7-25-06 ssq
;;;	ABSQTY = LQTYOR
	IF(ABSQTY.LT.0) ABSQTY = -ABSQTY	;ABSOLUTE QTY
	FOR I FROM 1 THRU NUMITM
		BEGIN
		IF (ABSQTY.LT.I1_QTY(I))
		    BEGIN
		    price = i1_price(i)
	;;;	    LPRICE = I1_PRICE(I)
	;;;	    LPRICE = (I1_PRICE(I)*CMX)#3 * 10	;SSQ 9-16-04
		    EXITLOOP
		    END
		END
	RETURN
;---------------------------------------------------	
;-

CFG_PRICE_ROUTINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;===================================================
; ------- Configuration Price Adjustments -------
	USING LITMNO SELECT
	('J35'),	BEGIN
			LPRICE = CONFIG_PRICE
			XCALL RNICK (LPRICE)
			END

	('WR'),		BEGIN		;R_ANGLE
			IF (DASH)
			   BEGIN
			   WTPIN = ((WEIGHT*1000)/120)
			   DPND = (WTPIN * RA_SIZE)#3
			   PRPPN = ((PRICE*1000)/WEIGHT)
			   C_PRICE = (PRPPN * DPND)#3
			   C_PRICE = ((PRPPN * DPND)#3 * CMX)#3 *10	;SSQ 9-16-04
			   END
			IF (INCH)
			   BEGIN
			   WTPIN = ((WEIGHT*10000)/120)/2
			   DPND = (WTPIN * RA_SIZE)#3
			   PRPPN = ((PRICE*1000)/WEIGHT)
			   C_PRICE = (PRPPN * DPND)#4
			   C_PRICE = ((PRPPN * DPND)#4 * CMX)#3 *10
			   END
			END
	('SG','SX'),	BEGIN	;SPIRAL PIPE
	  		C_PRICE = (SP_SIZE * PRICE(1))
	  		C_PRICE = ((SP_SIZE * PRICE(1)) * CMX)#3 * 10
			x_LPWGT = (SP_SIZE * IP_WGT)			;SSQ 9-6-17

			LUOFM = 'EA'
			END
;;;	('RT','RTV'),			C_PRICE = (PRICE*(RT_SZ1+RT_SZ2)*CMX)#3 * 10
	('RT','RTV'),			C_PRICE = (config_PRICE*(RT_SZ1+RT_SZ2)*CMX)#3 * 10
	('T4','T9','TV4','TV9'),	C_PRICE = (CONFIG_PRICE * CMX)#3 * 10
	('C9','C4','CV9','CV4'),	C_PRICE = (CONFIG_PRICE * CMX)#3 * 10
	('CT','CC','CTV','CCV'),	C_PRICE = (CONFIG_PRICE * CMX)#3 * 10
	('ER'),				C_PRICE = (CONFIG_PRICE * CMX)#3 * 10
	('BN'),				C_PRICE = (CONFIG_PRICE * CMX)#3 * 10
	('P4','P9','PV4'),		C_PRICE = (CONFIG_PRICE * CMX)#3 * 10

	('R16'  THRU  'R26'),		C_PRICE = (CONFIG_PRICE * CMX)#3 * 10
	('RV16' THRU 'RV26'),		C_PRICE = (CONFIG_PRICE * CMX)#3 * 10
	('S9','S4','SV','SW','SC','ST'), C_PRICE = (CONFIG_PRICE * CMX)#3 * 10
	('OB','PB','SBD','BDD'),	C_PRICE = (CONFIG_PRICE * CMX)#3 * 10
	('SLF','SLB'),			C_PRICE = (CONFIG_PRICE * CMX)#3 * 10
	ENDUSING

	RETURN
;---------------------------------------------------	

ADD_DAMPER_PRICE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (exact_match .and. is_pressed) return	;pressed
	if (exact_match .and. savcfg.eq.2) return	;pressed part input

	FOR I FROM 1 THRU 3 
		BEGIN
		C_PRICE = C_PRICE + DAMPER_PRICE(I)
		SAVE_DP(I) = DAMPER_PRICE(I)	;SSQ 12/23/02
		DAMPER_PRICE(I) = 		;ssq 10/16/02
		END
	RETURN
;---------------------------------------------------	

chk_prc_dif,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (diff .gt. 10)
		begin
		ern = 4		;price mis-match
		a30(1,8) = 'QE/SMC: '
		A30(9,17) = fd_price,	'ZZZZX.XXX'
		A30(19,19) = '/'
		A30(20,28) = c_price,	'ZZZZX.XXX'
		ER4 = A30
		call error
		end
	return
;--------------------------------------------------		

ORDER_DIMS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get the dimension data into proper order...
	CLEAR K
	FOR I FROM 1 THRU 10
	  BEGIN
 	  CLEAR DIMS(I)

	  USING RT_A2(I) SELECT
	  ('  '),	EXITLOOP	;HAVE PROCESSED ALL ELEMENTS
	  ('AN'),	NEXTLOOP	;SKIP ANGLE
	  ('GA'),	NEXTLOOP	;SKIP GAUGE
	  ENDUSING	

	  FOR J FROM 1 THRU 10
		BEGIN
		IF (RT_A2(I) .EQ. LARA(J))
			BEGIN	;INDEX CORRESPONDING TO CURRENT LETTER
			INCR K
			DIMS(K) = FD_DIMS(J)
			EXITLOOP
			END
		END
	  END

	RETURN
;----------------------------------------------------------------

GET_RTN_TABLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR ERN
	CLEAR TBL_KEY
	TBLCOD = 'RT'		;routine table
	RT_MATL = FD_PCM	;mat'l from QE
	RT_PC = FD_PC		;pressure file (indicates material)
	RT_CODE = FD_CODE	;CODE

	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0)
		BEGIN
		ERN = 1
		CALL ERROR
		RETURN
		END
	RETURN
;-------------------------------------------------------------------

GETITM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; NOW A ROUTINE, LOOK UP ITEM IN ITMMAS...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 11-16-09 re-copied from scrnx...

; vanitm = item w/ all note keys = zero
; exact_match = exact match

	call check_pressed

	CLEAR IVFLAG, VANITM, EXACT_MATCH, SAVCFG
	CLEAR DF1, DF2, DF3
	clear swfab
	clear van_price
	clear van_wgt		;9-6-17
	clear van_descr		;1-18-10

; read itmmas based on partial key for matching items below...
; 11-29-07: this partial items don't have f-note keys, so after gi_temp
; there's nothing else to do but exit...
	USING LITMNO SELECT
	  ('CM'),		BEGIN
				KEY = LITMNO(1,3)
				CALL GI_TEMP
				cmrfa = girfa	;12-18-07 for commit
				END
	  ('CR0' thru 'CR9'),	BEGIN
				KEY = LITMNO(1,4)
				CALL GI_TEMP		;READ ITMMAS
				cmrfa = girfa	;12-18-07 for commit
				END
	('JEB0' THRU 'JEB9'),	begin
				KEY = litmno(2,15)	;SKIP THE "J"
				CALL GI_TEMP		;READ ITMMAS
				cmrfa = girfa	;12-18-07 for commit
				end
	('JEF0' THRU 'JEF9'),	begin
				KEY = litmno(2,15)	;SKIP THE "J"
				CALL GI_TEMP		;READ ITMMAS
				cmrfa = girfa	;12-18-07 for commit
				end
	('JEFI0' THRU 'JEFI9'),	begin
				KEY = litmno(2,15)	;SKIP THE "J"
				CALL GI_TEMP		;READ ITMMAS
				cmrfa = girfa	;12-18-07 for commit
				end
	('JJG'),		begin
				KEY = litmno(2,15)	;SKIP THE "J"
				CALL GI_TEMP		;READ ITMMAS
				cmrfa = girfa	;12-18-07 for commit
				end
	('JTG'),		begin
				KEY = litmno(2,15)	;SKIP THE "J"
				CALL GI_TEMP		;READ ITMMAS
				cmrfa = girfa	;12-18-07 for commit
				end
	ENDUSING

	KEY = LITMNO


; Configured items, check if vanilla item exists...
	CLEAR ITMKEY
	K_ITEM = LITMNO
	K_F1 = '000'
	K_F2 = '000'
	K_F3 = '00000'

	read (1, invmas, itmkey, keynum:1, getrfa:vanrfa) [err=no_itm_2]
	vanitm = 1				;vanilla item was found
	savcfg = icfg				;remember if cfg or not.
	van_price = price
	van_descr = descr			;ssq 5-31-17
	van_wgt = ip_wgt			;6-28-17 production weight
	girfa = vanrfa				;in case this is the item to update

	if (is_fake .eq. 1)		;1-18-10
		begin
		van_descr = fake_descr
		van_price = fake_price
		end

	if (inn .eq. 1) 
		begin
		exact_match = 1
		goto gi_done		;ignore key values
		end

; check for exact match...
no_itm_2,
; first, check for default keys...
	CLEAR TBL_KEY		;is there a default key for this item?
	TBLCOD = 'IK'		
	IK_ITEM = LITMNO
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NO_DEFAULT]
	DF1 = IK_F1		;note that these values will 
	DF2 = IK_F2		;be overridden if notes are
	DF3 = IK_F3		;running

;----------------------------------------

; Configured items, check if default-vanilla item exists...
	CLEAR ITMKEY
	K_ITEM = LITMNO
	K_F1 = DF1,	'XXX'
	K_F2 = DF2,	'XXX'
	K_F3 = DF3,	'XXXXX'
; if item w/ default key exists - even better than vanilla...

	read (1, invmas, itmkey, keynum:1, getrfa:defrfa) [err=no_default]
	vanitm = 1				;vanilla item was found
	savcfg = icfg				;remember if cfg or not.
	girfa = defrfa				;in case this is the item to update
	if (inn .eq. 1) goto gi_done		;ignore key values
;----------------------------------------
no_default,
; note that some notes are info only, ex: f1=60 (all mat exposed). don't use
; these notes as part of key, and they don't change dept...

;-------------------------------------------------------------------------
; 11-02-09: default note changes:

	clear kf1, kf2, kf3	;these will be f-key values when done...

	if (clvl1 .le. 0)
	then	kf1 = df1	;no key entered, used default
	else	if (f1_ik(clvl1).eq.1)
		then	kf1 = 0			;key entered not valid for look-up
		else	kf1 = f1_key(clvl1)	;use key entered
	if (clvl2 .le. 0)
	then	kf2 = df2	;no key entered, used default
	else	if (f2_ik(clvl2).eq.1)
		then	kf2 = 0			;key entered not valid for look-up
		else	kf2 = f2_key(clvl2)	;use key entered
	if (clvl3 .le. 0)
	then	kf3 = df3	;no key entered, used default
	else	if (f3_ik(clvl3).eq.1)
		then	kf3 = 0			;key entered not valid for look-up
		else	kf3 = f3_key(clvl3)	;use key entered

	using kf1 select
	(080),	kf1 = 081
	(082),	kf2 = 083
	endusing

	if (kf3 .eq. 16) kf3 = 15

; now kf1, kf2 & kf3 should be look-up keys for this item...

	k_f1 = kf1, 'XXX'
	k_f2 = kf2, 'XXX'
	k_f3 = kf3, 'XXXXX'

;-------------------------------------------------------------------------
	

	READ (1, INVMAS, ITMKEY, KEYNUM:1, getrfa:girfa) [ERR=NO_ITM_3]

	savcfg = icfg				;remember if cfg or not.
	exact_match = 1
	goto gi_done

no_itm_3,
	clear girfa
	ivflag = 1			;not found
	if (vanitm .ne. 1) goto gi_done
	read (1, invmas, rfa:vanrfa) [err=no_vrfa]	;re-read vanilla item
	clear ivflag					;back to vanilla item
	swfab = 1					;new item, fabricate

GI_DONE,
	IF (PRDCAT.EQ.'ZZ'.OR.USRDEF.EQ.'ZZ')
		BEGIN
		IVFLAG = 3
		XCALL OLMSG(WND_1,23,'See Steve M (ZZ)',2)
		END

	price = van_price

	RETURN
no_vrfa,
	ivflag = 1
	RETURN

;-----------------------------------------------------------------

GI_TEMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL ISIO (1, INVMAS, KEY, READ, LOKCTL)
	read (1, invmas, KEY, getrfa:girfa) [err=git_bad]	

	exact_match = 1
	TONH = QTYONH
	TONO = QTYONO
	TCOM = QTYCOM
	TREO = REOLVL
	RETURN

GIT_BAD,
	IVFLAG = 1
;;;	XCALL OLMSG(WND_1,23,'Item has not been set up',2)
	RETURN
;-----------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
check_pressed,
; 11-16-09 copied from scrnx...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;---------------------------------------------------------
; check to see if this is a pressed part...
	clear is_pressed

	IF (CLVL1) 
		BEGIN
		CLEAR TBL_KEY
		TBLCOD = 'M1'
		MM_KEY = F1_KEY(CLVL1)
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
		IF (LOKCTL .NE. 0) GOTO NOT_PRESSED
		IF (MM_PP .NE. 1) GOTO NOT_PRESSED
		END

; free hand F2 notes are OK for pressed parts...
	IF (CLVL2) 
		BEGIN
		CLEAR TBL_KEY
		TBLCOD = 'M2'
		MM_KEY = F2_KEY(CLVL2)
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
		IF(LOKCTL.EQ.0 .AND. MM_PP.NE.1) GOTO NOT_PRESSED
		END

	IF (CLVL3 .and. F3_KEY(CLVL3).NE.0) 	;01-10-08
		BEGIN
		CLEAR TBL_KEY
		TBLCOD = 'M3'
		MM_KEY = F3_KEY(CLVL3)
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
		IF (LOKCTL .NE. 0) GOTO NOT_PRESSED
		IF (MM_PP .NE. 1) GOTO NOT_PRESSED
		END
;;;	if (clvl1.ne.0 .or. clvl2.ne.0 .or. clvl3 .ne. 0) goto not_pressed
; is the item in the pressed part table?

	CLEAR TBL_KEY
	TBLCOD = 'PP'
	PP_ITEM = LITMNO
	READ (CHN182,COPTBL,TBL_KEY) [ERR=NOT_PRESSED]

; note that if the price changes, or a multiplier is used item will
; revert back to item as entered (ie: not pressed).

	is_pressed = 1
	litmno = pp_part		;item # for pressed part
	l_pp = litmno			;save pressed -part itemn#
;;;	xcall w_disp(wnd_1,wd_pos,row,col,litmno)
;;;	xcall w_updt

NOT_PRESSED,
	return
;-----------------------------------------------------------------

FIND_DAMPER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR LDAMPR, BAD_DIGIT

	PARTNO = LITMNO

	FOR I FROM 1 THRU 3
		BEGIN
		CLEAR DAMPER_PRICE(I)
		CLEAR SAVE_DP(I)
		END

	MAXFLEN = %TRIM(PARTNO)
	FOR I FROM 1 THRU 3
		BEGIN
		XCALL INSTR(1, PARTNO, "*", FL)
		IF (.NOT. FL) 		EXITLOOP
		PARTNO(FL, MAXFLEN) = PARTNO(FL+1, MAXFLEN)
		CALL DM_SIZE
		IF (BAD_DIGIT)
			BEGIN
			XCALL OLMSG(WND_1,23,'damper configuration',2)
			RETURN
			END
		END

;;; 12-21-09>	LITMNO = PARTNO
	RETURN

DM_SIZE,	;---------------------------------------
	CLEAR BAD_DIGIT

	ONERROR BADDIG
	DAMP = PARTNO(FL, FL+1)
	OFFERROR

;price coming from Fabshop - don't need to look-up price...
;check damper price...
	CLOSE 9
	FL_NAME = 'SDPRIC'		;SSQ 9-9-03
	OPEN(9,I,FILPRC)		;SSQ 9-9-03
	XCALL IO (9,TPRICE,1,READ,LOKCTL)
	IF(DAMP.LE.80) 
	THEN	DAMPER_PRICE(I) = ARRAY(DAMP)*10
	ELSE	CLEAR DAMPER_PRICE(I)

	CLOSE 9
	IF (DAMPER_PRICE(I) .LE. 0)
		BEGIN
		XCALL OLMSG(WND_1,23,'Zero or missing damper price',2)
		GOTO BADDIG
		END
	LDAMPR = 1
	RETURN
BADDIG,
	CLEAR DAMPER_PRICE(I)
	BAD_DIGIT = 1
	CLEAR LDAMPR
	RETURN
;----------------------------------------------------------------

; END SCRNX LOGIC
;========================================================

; Routines to create item numbers...

RECTANGULAR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PC = 30 RECTANGULAR DUCTWORK & FITTINGS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CALL GET_RTN_TABLE
	IF (ERN .NE. 0) RETURN	;error reading table

	CALL ORDER_DIMS		;sort dimension data
;;;	V_SIZE1 = DIMS(1)
;;;	V_SIZE2 = DIMS(2)
	SZ1 = DIMS(1)
	SZ2 = DIMS(2)

;dbg
;;;	if (sz1.ne.9 .and. sz2.ne.9) return
;dbg
	V_SIZE3 = DIMS(3)
	
	IF (FD_SDC .EQ. 'D')	;QE reversed slip & drive to drive/slip
		BEGIN
		TMPSZ = SZ2
		SZ2 = SZ1
		SZ1 = TMPSZ
		END

	USING RT_RTN(1,1) SELECT
	('F'),		CALL FITTING
	('D'),		IF (V_SIZE3 .LT. 4) 
			THEN	CALL FITTING
			ELSE	CALL DUCT
	ENDUSING

	RETURN
;-----------------------------------------------------------------
DUCT,		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; STRAIGHT DUCTWORK
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;>return	;dbg - just fittings for test
	
	IF (DIMS(4) .EQ. 11) V_SIZE3 = V_SIZE3 + 1	;4' 11"

;;;	V_SIZE1 = FD_SIZ1	;ZZZ.XXX
;;;	V_SIZE2 = FD_SIZ2	;ZZZ.XXX


	SZ1=%TRN3(FD_SIZ1)
	SZ2=%TRN3(FD_SIZ2)

	CLEAR DUCACC
	DUCTOR = OORDNO

	DUTYPE = 1	;default to 1=galv

	STY =	STYLES(FD_STYL)
	FOR I FROM 1 THRU 8
		BEGIN
		A13 = LINERS(I)
		IF (FD_LINR(1,%TRIM(FD_LINR)) .EQ. A13(1,%TRIM(A13))) EXITLOOP
		END

	LINER = I	;Note that I will be 9 if no match found

	SEAM = FD_SEAM
	SEAL = 1	;default to 1=yes
	GAUGE =	FD_GA
	SIZE3 = V_SIZE3
	SIZE1 = FD_SIZ1
	THICK =		;???

	CALL CALC_CAT	;based on size and style
	CAT = CALCAT	

	JOINT = FD_QTY
	SIZE2 = FD_SIZ2
	LOCAT =
	ACC = 1		;default 1=yes

	CALL RECALC	;CALCULATE SQFEET, POUNDS, GPRICE
	CALL CALLIN	;CALCULATE SQFLIN, LINPRC
;;;	SQFEET =
;;;	POUNDS =
;;;	SQFLIN =
;;;	GPRICE =
;;;	LINPRC =

	USING FD_STYL SELECT
	(5,6,7),	BEGIN		;SLIP/DRIVE OR STANDING SLIP & DRIVE
		CALL SLIP
		LITMNO = RCTITM
		CALL GETITM
		IF (IVFLAG.EQ.0)
		  BEGIN
		  SLIPS = ORDQTY
		  SLPITM = RCTITM
		  SLPPRC = PRICE
		  SLPDES = DESCR
		  END

		CALL DRIVE
		LITMNO = RCTITM
		CALL GETITM
		IF (IVFLAG.EQ.0)
		  BEGIN
		  DRIVES = ORDQTY
		  DRVITM = RCTITM
		  DRVPRC = PRICE
		  DRVDES = DESCR
		  END
		END
	ENDUSING


	STORE (36, DUCACC, KEY175)


	RETURN		;(from DUCT,)
;--------------------------------------------

CALC_CAT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; DETERMINE CATEGORY
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR CALCAT

	IF(SZ1.GT.120.OR.SZ2.GT.120)
		BEGIN
	 	CALCAT=5	;OVERSIZE
		GOTO END_CC
		END

	IF(SZ1+SZ2.GT.120)			CALCAT=3	;4-PIECE
	IF(SZ1+SZ2.LE.120)			CALCAT=1	;L-SHAPED
	IF(SZ1+SZ2.LE.30 .AND. (STY.EQ.2.OR.STY.EQ.6))	CALCAT=2	;WRAPAROUND ssq 9/5/02
	IF(SZ1+SZ2.LE.23 .AND. 
&		(STY.EQ.1.OR.STY.EQ.3.OR.STY.EQ.4.OR.STY.EQ.5))	CALCAT=2	;WRAPAROUND

END_CC,

	RETURN	
;-----------------------------------------------------------------

RECALC,
		; Get the parameter information for that gauge

	XCALL FFILE (174,FIL174,SWITCH)	
	OPEN (8,I,FIL174)
	LOKCTL = 1
	XCALL IO (8,DUCFIL,GAUGE,READ,LOKCTL)
	CLOSE 8	

		; Calculate the square feet
	DEC1 =
	DEC2 =
	DEC3 =
	DEC4 =
	SQFEET =
	DEC4 = SIZE3 * 12
		; Get the length in inches

	DEC1 = ( ( SZ1 * DEC4 ) * 2 )
		; DEC1 = square feet of 2 slip sides in inches

	DEC2 = ( ( SZ2 * DEC4 ) * 2 )
		; DEC2 = Square feet of 2 drive sides in inches

	DEC3 = ( DEC1 + DEC2 ) * JOINT
		; Total square feet, is slip sides, plus drive sides
		; times the number of joints

;;;
;	8/19/94 - modify rounding so SQFEET always rounds UP
;;;

 	SQFEET = ( ( DEC3 * 10 ) / 144 )
 		; Convert to square feet

	IF (SQFEET(6,6).EQ.0) THEN SQFEET = SQFEET / 10
	ELSE BEGIN
	  IF (SQFEET.GT.0) SQFEET = SQFEET(1,5) + 1
	  IF (SQFEET.LT.0) SQFEET = (SQFEET(1,5) * -1) - 1
	END
;;;

		;  Calculate the pounds of sheet metal, seam allowance, 
		;  and waste factor.
	DEC1 = 
	DEC2 =
	DEC3 =
	POUNDS =

	DEC1 = ( DUCLBS * SQFEET )
		;  Calculate the total actual number of pounds

	DEC3 = ( DUCSEA * ( SIZE3 * JOINT ) )
	IF ((SZ1+SZ2).GE.120) DEC3 = ( ( DUCSEA * 2 )*( SIZE3 * JOINT ) )
		; Calculate the seam allowance, by the foot, from total length
		; If size1+size2 .ge.120 then add seam allow twice

	DEC2 = ( DUCWAS * (DEC1+DEC3) ) #5
		; Calculate the total waste pounds, based on above acutal #
		; Round back 5 digts cuz of decimal places.
		; Changed to figure waste last cuz waste was not figured on
		; seam allowance and it should be

	POUNDS = ( DEC1 + DEC3 + DEC2 ) #3
		; The total pounds, is the actual number of pounds
		; plus the seam allowance
		; plus the waste
;(01)
	X_WGHT = POUNDS		;use "unadjusted weight for cleat & gasket calc.
;(01)
	IF (DUTYPE.EQ.2) POUNDS = ( ( DEC1 + DEC3 + DEC2 ) / 3 ) #3
		; Aluminum pounds is 1/3 galvanized


	CLEAR TBL_KEY
	TBLCOD = 'DP'
	DP_GAUGE = GAUGE
	DP_TYPE = DUTYPE
	DP_CAT = CAT
	DP_STYLE = STY

	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR DP_PRICE
	GPRICE = DP_PRICE

	RETURN
;------------------------------------------------------------------


CALLIN,		; Calculate the number of square feet of liner for this item
		; re-read parameter file (ssq 10/03/01)...
		; Get the parameter information for that gauge
	OPEN (8,I,FIL174)
	LOKCTL = 1
	XCALL IO (8,DUCFIL,GAUGE,READ,LOKCTL)
	CLOSE 8	

	DEC1 = 
	DEC2 =
	DEC3 =
	SQFLIN =

	DEC1 = ( DLINE * SQFEET ) #2
		; DLINE is a overage % of square feet ( SEE DUCMNT.COP )

	IF (DEC1.GT.0) DEC1 = ( DEC1 + 500 ) #3
	IF (DEC1.LT.0) DEC1 = ( DEC1 - 500 ) #3

	SQFLIN = DEC1 + SQFEET
		; Calculate the waste, and add square feet of duct

	IF (LINER.EQ.4) SQFLIN =

	CLEAR TBL_KEY
	TBLCOD = 'LP'
	LP_KEY = LINER

	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL.NE.0) CLEAR DP_PRICE
	LINPRC = DP_PRICE

	RETURN
;----------------------------------------------------------------
;;; END OF DUCT CALCS ;;;
;================================================================


FITTING,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Fittings
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;return	;dbg - just duct for test

; sum weight by gauge so line items (see ex. below) can be created.
;(24MFA 24GA S&D FITTINGS FOR FULL ASSEMB.
; sum sqft by liner type so  line items can for liner can be created.
;(DLF1 1" DUCT LINER FOR FITTINGS)

; need to accumulate totals for the following:
; ft for each type of liner used
; lbs for each style of fitting used
; number of plasma fittings
; number of canvas connectors (CC)
; number of square-to-round assemblies (SR)
; number of square-to-rounds (SR.)
; 4in single wall 
;	vane (4000A)
;	rail (5000A)
; 4in double wall
;	vane (424A)
;	rail (522A)
; 2in double wall 
;	vane (226A)
;	rail (322A)
; slips
; drives
;
;		
;dbg - print
	
	FD_SIZ1 = SZ1
	FD_SIZ2 = SZ2

	clear pline

	pline (1,7) = fd_sqft #1, 'ZZZX.XX'
	pline (9,15) = fd_wght #1,'ZZZX.XX'
	pline (17,34) = fd_vdes
	pline (36,42) = fd_vnch #1,	'ZZZX.XX'
	pline (44,50) = fd_rail #1,	'ZZZX.XX'
	pline (52,69) = fd_linr
	pline (71,77) = fd_lnsf #1,	'ZZZX.XX'
	pline (79,79) = fd_fttg,	'X'
	open (54, A, 'spl:fabduc.dat')
	writes (54, pline)
	close 54

	if (fd_fttg) numftg = numftg + fd_qty	;number of plasma fittings

;dbg - print

;10-05-06 ssq: need to multiply wght by qty...
	USING FD_GA SELECT
	(16),	A_WGHT(1) = A_WGHT(1) + (FD_WGHT*FD_QTY)
	(18),	A_WGHT(2) = A_WGHT(2) + (FD_WGHT*FD_QTY)
	(20),	A_WGHT(3) = A_WGHT(3) + (FD_WGHT*FD_QTY)
	(22),	A_WGHT(4) = A_WGHT(4) + (FD_WGHT*FD_QTY)
	(24),	A_WGHT(5) = A_WGHT(5) + (FD_WGHT*FD_QTY)
	(26),	A_WGHT(6) = A_WGHT(6) + (FD_WGHT*FD_QTY)
	ENDUSING

	USING RT_RTN(2,3) SELECT
	('CC'),	BEGIN
		LITMNO = 'CC'
		CALL GETITM		;read the ITMMAS record
		D18 = SZ1*2 + SZ2*2 + 6
		D18 = (D18*100/12)#1		;convert to feet
		C_PRICE = (D18 * PRICE)#1		
		diff =%abs((C_PRICE) - fd_price)
		call chk_prc_dif
		ORDQTY = FD_QTY
		CALL INSR_FIT		;create & write a line item
		END
	ENDUSING

	UPCASE FD_LINR
	IF (FD_LINR .EQ. 'NO LINER') GOTO SKIP_LINER

	FOR I FROM 1 THRU 8
		BEGIN
		IF (A_LNDS(I) .EQ. A30)
		THEN	BEGIN
			A_LNDS(I) = FD_LINR
			A_LNSF(I) = (FD_LNSF*FD_QTY)
			EXITLOOP
			END
	
		ELSE	BEGIN
			IF (FD_LINR.EQ.A_LNDS(I)) A_LNSF(I) = A_LNSF(I)+(FD_LNSF*FD_QTY)
			EXITLOOP
			END
		END


SKIP_LINER,
	USING FD_STYL SELECT
	(5,6,7),	BEGIN		;SLIP/DRIVE OR STANDING SLIP & DRIVE
		CALL SLIP
		CALL FIT_ITEM

		CALL DRIVE
		CALL FIT_ITEM
		END
	ENDUSING

	UPCASE FD_VDES
	USING FD_VDES SELECT
	('4IN SINGLE WALL'),	BEGIN
				RCTITM = '4000A'	;VANE ITEM
				decmal = fd_vnch/12	;convert to ft
				ORDQTY = %f_roundU(decmal,3)
				ordqty = ordqty*fd_qty				
				CALL FIT_ITEM

				RCTITM = '5000A'	;RAIL ITEM
				decmal = fd_rail/12	;convert to ft
				ORDQTY = %f_roundU(decmal,3)
				ordqty = ordqty*fd_qty				
				CALL FIT_ITEM
				END
	('4IN DOUBLE WALL'),	BEGIN
				RCTITM = '424A'	;VANE ITEM
				decmal = fd_vnch/12	;convert to ft
				ORDQTY = %f_roundU(decmal,3)
				ordqty = ordqty*fd_qty				
				CALL FIT_ITEM

				RCTITM = '522A'	;RAIL ITEM
				decmal = fd_rail/12	;convert to ft
				ORDQTY = %f_roundU(decmal,3)
				ordqty = ordqty*fd_qty				
				CALL FIT_ITEM
				END
	('2IN DOUBLE WALL'),	BEGIN
				RCTITM = '226A'	;VANE ITEM
				decmal = fd_vnch/12	;convert to ft
				ORDQTY = %f_roundU(decmal,3)
				ordqty = ordqty*fd_qty				
				CALL FIT_ITEM

				RCTITM = '322A'	;RAIL ITEM
				decmal = fd_rail/12	;convert to ft
				ORDQTY = %f_roundU(decmal,3)
				ordqty = ordqty*fd_qty				
				CALL FIT_ITEM
				END
	ENDUSING


	RETURN
;----------------------------------------------------------


FIT_ITEM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	LITMNO = RCTITM
	CALL GETITM		;read the ITMMAS record
	FD_PRICE = PRICE 
	
	CALL INSR_FIT		;create & write a line item
	RETURN
;----------------------------------------------------------

FIT_SUM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (numftg .ne. 0)
		begin
		rctitm = '???L'		;fittings
		ordqty = numftg
		call fit_item
		end

	for i from 1 thru 8
	  begin
	  clear pline

	  if (i .le. 6)
		begin	
		if (a_wght(i) .ne. 0) 
		  begin
		  ordqty = %f_roundU(a_wght(i), 3)

		  CLEAR LITMNO
		  RCTITM(1,2) = A_GA(I), 'XX'
	;;;	  RCTITM(3,5) = 'MFA'
		  RCTITM(3,5) = 'GF'		;SSQ 1-25-07
		  CALL FIT_ITEM
		  end
		end

	  if (a_lnsf(i) .ne. 0)
		  begin
		  for j from 1 thru 8
			begin
			upcase a_lnds(i)
			upcase liners(j)
			if (a_lnds(i) .eq. liners(j)) 
				begin
				rctitm = l_Item(j)
				ordqty = %f_roundu(a_lnsf(i), 3)
				call fit_item		;liner item
				exitloop
				end
			end
		  pline (20,30) = a_lnsf(i)#1,	'ZZZZZZZZ.XX'
		  pline (32,62) = a_lnds(i)
		  end

	  open (54, A, 'spl:fabduc.dat')
	  writes (54, pline)
	  close 54
	  end

	RETURN
;----------------------------------------------------------
;sg.z - create the sss items


make_sss,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	clear alu_amt, s304_amt, s316_amt


	for k from 1 thru 40
		begin
		if (alumtl_18(k) .gt. 0)
			begin
			totlen = alumtl_18(k)
			ppi = alumprc_18(k)
			call sum_alu
			end
		if (s304tl_18(k) .gt. 0)
			begin
			totlen = s304tl_18(k)
			ppi = s304prc_18(k)
			call sum_304
			end
		if (s316tl_18(k) .gt. 0)
			begin
			totlen = s316tl_18(k)
			ppi = s316prc_18(k)
			call sum_316
			end
		if (alumtl_20(k) .gt. 0)
			begin
			totlen = alumtl_20(k)
			ppi = alumprc_20(k)
			call sum_alu
			end
		if (s304tl_20(k) .gt. 0)
			begin
			totlen = s304tl_20(k)
			ppi = s304prc_20(k)
			call sum_304
			end
		if (s316tl_20(k) .gt. 0)
			begin
			totlen = s316tl_20(k)
			ppi = s316prc_20(k)
			call sum_316
			end
		if (alumtl_22(k) .gt. 0)
			begin
			totlen = alumtl_22(k)
			ppi = alumprc_22(k)
			call sum_alu
			end
		if (s304tl_22(k) .gt. 0)
			begin
			totlen = s304tl_22(k)
			ppi = s304prc_22(k)
			call sum_304
			end
		if (s316tl_22(k) .gt. 0)
			begin
			totlen = s316tl_22(k)
			ppi = s316prc_22(k)
			call sum_316
			end
		if (alumtl_24(k) .gt. 0)
			begin
			totlen = alumtl_24(k)
			ppi = alumprc_24(k)
			call sum_alu
			end
		if (s304tl_24(k) .gt. 0)
			begin
			totlen = s304tl_24(k)
			ppi = s304prc_24(k)
			call sum_304
			end
		if (s316tl_24(k) .gt. 0)
			begin
			totlen = s316tl_24(k)
			ppi = s316prc_24(k)
			call sum_316
			end
		end

; create SSSP/ASP line items if required...
	if (s304_amt .gt. 0) 
		begin
		litmno = 'SSSP'
		call getitm
		savdsc = descr
		ordqty = 1
		fd_price = s304_amt
		clear clvl1, clvl2, clvl3
		clvl1 = s304_lvl
		call sg_item		;add ordlin
		end
	if (s316_amt .gt. 0) 
		begin
		litmno = 'SSSP'
		call getitm
		savdsc = descr
		ordqty = 1
		fd_price = s316_amt
		clear clvl1, clvl2, clvl3
		clvl1 = s316_lvl
		call sg_item		;add ordlin
		end
	if (alu_amt .gt. 0)
		begin
		litmno = 'ASP'
		call getitm
		savdsc = descr
		ordqty = 1
		fd_price = alu_amt
		clear clvl1, clvl2, clvl3
		clvl1 = alu_lvl
		call sg_item		;add ordlin

		end

	return
;------------------------------------------------------
sum_alu,
	sum_mat = 66	;for alum
	goto sum_mat

sum_304,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	sum_mat = 64	;for s304
	goto sum_mat
sum_316,
	sum_mat = 65	;for s316
sum_mat,
; need total lineal ft for each material to compute discount..

	using sum_mat select
	(64),	s_adj_len = tot_304
	(65),	s_adj_len = tot_316
	(66),	s_adj_len = tot_alu
	endusing

	using s_adj_len/12 select
	(20 thru 49),	s_adj = 90
	(50 thru 99),	s_adj = 85
	(100 thru 149),	s_adj = 80
	(.gt. 149),	s_adj = 70
	(),		s_adj = 100
	endusing

	totlen = totlen + 72			;add 6' (72")
	wrkamt = (totlen * ppi * s_adj)#4	;total amount
	
	using sum_mat select
	(64),	s304_amt = s304_amt + wrkamt
	(65),	s316_amt = s316_amt + wrkamt
	(66),	alu_amt = alu_amt + wrkamt
	endusing
	
	return
;------------------------------------------------------

MAKE_MEMOS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; add any memo records
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FOR I FROM 1 THRU F3_NUM
		BEGIN
		CLEAR ORDLIN
		LORDNO = OORDNO
		LINSEQ = 0
		LITMNO = 'MM_F3'
		LMSQ3 = I
		M_LDESCR = F3_MEMOL(I)
		M_SHORTD = F3_MEMOS(I)
		M_KEY = F3_KEY(I)
		LTYPE = 'M'
		STORE (5, ORDLIN, ORDKEY)
		END

	FOR I FROM 1 THRU F2_NUM
		BEGIN
		CLEAR ORDLIN
		LORDNO = OORDNO
		LINSEQ = 0
		LITMNO = 'MM_F2'
		LMSQ2 = I
		M_LDESCR = F2_MEMOL(I)
		M_SHORTD = F2_MEMOS(I)
		M_KEY = F2_KEY(I)
		LTYPE = 'M'
		STORE (5, ORDLIN, ORDKEY)
		END

	FOR I FROM 1 THRU F1_NUM
		BEGIN
		CLEAR ORDLIN
		LORDNO = OORDNO
		LINSEQ = 0
		LITMNO = 'MM_F1'
		LMSQ1 = I
		M_LDESCR = F1_MEMOL(I)
		M_SHORTD = F1_MEMOS(I)
		M_KEY = F1_KEY(I)
		LTYPE = 'M'
		STORE (5, ORDLIN, ORDKEY)
		END
	RETURN
;------------------------------------------------------

;==========================================================

SLIP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR RCTITM

	USING SZ1 SELECT
	(0 THRU 19),	BEGIN
			RCTITM(1,3) = '724'
			RCTITM(4,6) = SZ1,	'ZXX' [LEFT]
			END

	(20 THRU 30),	BEGIN
			RCTITM(1,3) = '824'
			RCTITM(4,6) = SZ1,	'ZXX' [LEFT]
			END

	(31 THRU 42),	BEGIN
			RCTITM(1,3) = '822'
			RCTITM(4,6) = SZ1,	'ZXX' [LEFT]
			END

	(.GT. 42),	BEGIN
			RCTITM(1,4) = '1022'
			RCTITM(5,7) = SZ1,	'ZXX' [LEFT]
			END
	(),		CLEAR RCTITM
	ENDUSING


	SLPITM = RCTITM
	ORDQTY = FD_QTY * 2
	SLPPRC =
	SLPDES =

	RETURN
;-----------------------------------------------------------------

DRIVE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR RCTITM
	USING SZ2 SELECT
	(3 THRU 19),	BEGIN
			RCTITM(1,1) = '6'
			RCTITM(2,4) = SZ2,	'ZXX' [LEFT]
			LN = %TRIM(RCTITM)
			RCTITM(LN+1,LN+3) = '241'	;single piece
			END

	(20 THRU 30),	BEGIN
			RCTITM(1,3) = '124'
			RCTITM(4,6) = SZ2,	'ZXX' [LEFT]
			LN = %TRIM(RCTITM)
			RCTITM(LN+1,LN+1) = '1'	;single piece
			END

	(.GT. 30),	BEGIN
			RCTITM(1,3) = '122'
			RCTITM(4,6) = SZ2,	'ZXX' [LEFT]
			LN = %TRIM(RCTITM)
			RCTITM(LN+1,LN+1) = '1'	;single piece
			END
	(),		CLEAR RCTITM
	ENDUSING

	ORDQTY = FD_QTY * 2
	DRVITM = RCTITM
	DRVPRC =
	DRVDES =

	RETURN
;--------------------------------------------------------------


CATALOG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR IS_CFG		;5-25-06
	CLEAR CFG_ON		;5-13-13
	ITEM = FD_ITEM
	
	RETURN
;-----------------------------------------------------------------
;-----------------------------------------------------------------


WRT_FABOUT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	using fd_pc select
	(30),	begin
		open (54, A, 'spl:fabduc.dat')
		clear pline
		pline (1,7) = fd_sqft #1, 'ZZZX.XX'
		pline (9,15) = fd_wght #1,'ZZZX.XX'
		pline (17,34) = fd_vdes
		pline (36,42) = fd_vnch #1,	'ZZZX.XX'
		pline (44,50) = fd_rail #1,	'ZZZX.XX'
		pline (52,69) = fd_linr
		pline (71,77) = fd_lnsf #1,	'ZZZX.XX'
		pline (79,79) = fd_fttg,	'X'
		writes (54, pline)
		close 54
		end
	(),	begin
		open (chnout, A, 'spl:fabout.dat')
		clear pline
		PLINE (1,3) = FD_NUM,	'ZZX'
		PLINE (5,6)  = FD_PC,	'ZX'
		PLINE (9,11)  = FD_CODE,	'ZZX'
		PLINE(14,15) = FD_GA,	'ZX'
		PLINE(17,18) = FD_DIMS(1),	'ZX'
		PLINE(20,24) = FD_QTY,		'ZZZZZX'
		PLINE(29,43) = ITEM
		PLINE(45,74) = FD_DESC
		writes (chnout, pline)
		call check_mat_err
		close chnout
		end
	endusing
	

;  PC CODE  GA D1 D2 ET  ITEM#           DESCRIPTON
;  XX  XXX  XX ZX ZX AA  aaaaaaaaaaaaaaa AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
;12345678901234567890123456789012345678901234567890123456789012345678901
;         1         2         3         4         5         6         7         8


	RETURN
;-----------------------------------------------------------------------
check_mat_err,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	ER5	,A30,	'316-SS Spiral: Wrong Acc File '
;	ER6	,A30,	'316-SS Spiral: Wrong Button   '
;	ER7	,A30,	'316-SS Sprial: Wrong PressureF'
;                        12345678901234567890123456789012345
;                                 1         2         3
	using fd_f1 select	; various material keys same as smc
	(61),	begin		; Paint Grip
		end
	(64),	begin		; 304 SS
		end
	(65),	begin		; 316 SS
		end
	(66),	begin		; Alum
		end
	(68),	begin		; G-90
		end

	(),	return		; Galv
	endusing

	return
;---------------------------------------------------


DAMP_ERR,
	ERN = 3		;DAMPER DIGIT
	GOTO ERROR
BADITM,
	ERN = 2		;ITEM NOT FOUND

ERROR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INCR ERCNT
	FD_DESC = ER_MSG(ERN)
	CALL WRT_FABOUT
;;;	XCALL OLMSG (W_ID, 24, ER_MSG(ERN), 1)
	RETURN
;---------------------------------------------------

;======================COPY ROUTINES====================
;=======================================================

OPEN_CPY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN041

	SWITCH = 5
	XCALL FILES (1, 'SU', 041, SWITCH)
	IF (SWITCH .EQ. 9) RETURN

	CLOSE CHN060

	SWITCH = 5
	XCALL FILES (3,'U', 060, SWITCH)
	IF (SWITCH .EQ. 9) RETURN

	SWITCH = 5
	XCALL FILES (44, 'SU', 044, SWITCH)
	CHN044 = 44

	SWITCH = 5
	XCALL FILES (45, 'SU', 045, SWITCH)
	CHN045 = 45


	SWITCH = 5
	XCALL FILES (75, 'SU', 175, SWITCH)
	CHN175 = 75

	RETURN
;-------------------------------------------------------

CLOSE_CPY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN044
	CLOSE CHN045
	CLOSE CHN175

	RETURN
;-------------------------------------------------------

	
COPY_HEADER,
	LOKCTL = 1
	XCALL IO (CHN060, COPCTL, 1, READ, LOKCTL)
	IF(LSTORD .EQ. 999999) LSTORD = 100000
	INCR LSTORD
	LOKCTL = 1
	XCALL IO (CHN060, COPCTL, 1, WRITE, LOKCTL)

	OORDNO = LSTORD

	XCALL ISIO (CHN044, ORDHDR, OORDNO, STORE, LOKCTL)
	IF (LOKCTL .NE. 0)
	THEN	BEGIN
		ERR_ORD = OORDNO
		XCALL OLMSG (W_ID, 24, ERRMSG, 1)
		ABORT = 1
		END

	ELSE	CALL CHECK_ON_HOLD		;8-20-12

; 7-15-15 update comment
	clear ordcm2
	c2_ord = xord
	c2_seq = 0
	lokctl = 1
	xcall isio (chn135, ordcm2, c2_key, read, lokctl)
	if (lokctl .eq. 0) 
		begin
		xcall isio (chn135, ordcm2, c2_key, delete, lokctl)
		c2_ord = oordno
		xcall isio (chn135, ordcm2, c2_key, store, lokctl)
		end

	RETURN


CHECK_ON_HOLD,

	XCALL ISIO (CHN001, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0) CLEAR CUSMAS
	
	
	CUSKEY = OCUSNO, 'XXXXXX'
;;;	XCALL SERCH (7,CUSIDX,CUSKEY,1,6,BSEND,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	IF (SRCCTL.NE.0)
;;;	THEN	CLEAR CUSMAS
;;;	ELSE	BEGIN
;;;		LOKCTL = 1
;;;		XCALL IO (6,CUSMAS,IRC001,READ,LOKCTL)
;;;		END

	A6 = CRDLMT,	'XXXXXX'
	IF (%INSTR (1, A6, '5')) CALL ON_HOLD

	RETURN
;-----------------------------------------------------------------

on_hold,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	a6 = crdlmt, 'XXXXXX'		;6-12-07 ssq
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
	chn195 = 19
	XCALL FILES (CHN195, 'SU', 195, SWITCH)

	IF (SWITCH .EQ. 9)
	THEN	XCALL OLMSG(WND_1,24,"CAN'T OPEN CRHOLD",2)	;;;

	ELSE	BEGIN
		CLEAR CRHOLD
		CH_ORD = OORDNO
		CH_FLAG = cr_flag		;ORDER ENTRY credit hold
		XCALL ISIO (CHN195, CRHOLD, CH_ORD, STORE, LOKCTL)
		CLOSE CHN195
		END

	return
;------------------------------------------------------------------


COPY_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FIND (5, ORDLIN, XORD) [ERR=CL_LOOP]
CL_LOOP,
	XCALL IOS (5, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO CL_EOF
;;;	reads (5, ordlin, cl_eof)

	IF (LORDNO .NE. XORD) GOTO CL_EOF

	IF(LTYPE .EQ. 'M') GOTO WRTLIN
	
	CLEAR LQTYSH, LQTYBO, LCOMPS, LFLAG

	CALL COMMIT

WRTLIN,
	LORDNO = OORDNO
	XCALL ISIO (CHN045, ORDLIN, ORDKEY, STORE, LOKCTL)
	GOTO CL_LOOP
CL_EOF,
	RETURN
;--------------------------------------------------

;cmt.x	11-16-09 copied from scrnx...
COMMIT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; COMMIT INVENTORY
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 12-18-07 use cmrfa from GETITM
	USING LITMNO SELECT
	  ('CM'),		s_item = litmno(1,3)
	  ('CR0' thru 'CR9'),	s_item = litmno(1,4)

	  ('JEFI'),	BEGIN
			S_ITEM = LITMNO(2,3) + LITMNO(5,15)
			END
	  ('EFI'),	BEGIN
			S_ITEM = LITMNO(1,2) + LITMNO(4,15)
			xcall gtitm (s_item,lf1,lf2,lf3,girfa,ivflag,vanflg,vanrfa)
			if(ivflag .eq. 0) clear girfa	;cant allocate
			END
	  (),		s_item = litmno
	ENDUSING

	xcall gtitm (s_item,lf1,lf2,lf3,girfa,ivflag,vanflg,vanrfa)
	if(ivflag .eq. 0) goto i_exit	;cant allocate

	read (chn041, invmas, rfa:girfa) [err=no_grfa]	;re-read gi item
	IF (STOCK.NE.'S') GOTO I_EXIT

	QTYCOM = QTYCOM + LQTYOR

	LOKCTL = 1
	WRITE (chn041, INVMAS, ITMKEY) [ERR=I_EXIT]
	goto i_exit

no_grfa,
	XCALL OLMSG (WND_1, 23, 'BAD ITEM, DID NOT ALLOCATE',2)
I_EXIT,
	RETURN

I_NF,
;;;	XCALL OLMSG (WND_1, 23, 'BAD ITEM, DID NOT ALLOCATE',2)
	GOTO I_EXIT

;-------------------------------------------------------

COPY_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FIND (36, ORDLIN, XORD) [ERR=CL_LOOP]
CD_LOOP,
	XCALL IOS (36, DUCACC, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO CD_EOF

	IF (DUCTOR .NE. XORD) GOTO CD_EOF

	DUCTOR = OORDNO
	XCALL ISIO (CHN175, DUCACC, DUCTOR, STORE, LOKCTL)
	GOTO CD_LOOP
CD_EOF,
	RETURN
;--------------------------------------------------

END_ORDER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	S_ORD = OORDNO
	ORD_ORD = OORDNO
	XCALL OLMSG (W_ID, 24, ORDMSG, 1)
	RETURN
;-----------------------------------------------------

;=======================================================
;=======================================================
; *** INTERNAL CFG ROUTINES ***
R_TAKEOFF,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	USING LITMNO(3,3) SELECT
	('V'),		BEGIN			;VINYL
			RTVAL = LITMNO(4,9)
			CLEAR LITMNO(4,15)
			VYNL = 1
			END
	('.'),		RETURN			;"RT." is a valid part #
	(),		BEGIN			;NOT VINYL
			RTVAL = LITMNO(3,8)
			CLEAR LITMNO(3,15)
			VYNL = 0
			END
	ENDUSING

	RTLEN = %TRIM(RTVAL)
	IF(RTLEN .LT. 6) GOTO R_TAKEOFF_ERROR

	ONERROR R_TAKEOFF_ERROR
	RT_SZ1 = RTVAL(1,2)
	RT_SZ2 = RTVAL(3,4)
	RT_DIA = RTVAL(5,6)
	OFFERROR

	IF(RT_SZ2 .GT. RT_DIA) GOTO R_TAKEOFF_ERROR

; find the greater of SZ1 & SZ2 to determin the gauge...
	RTLEN = RT_SZ1
	IF(RT_SZ2 .GT. RTLEN) RTLEN = RT_SZ2

	IF (VYNL .EQ. 1)
	THEN	BEGIN
		USING RTLEN SELECT
		(.GE. 25),	RT_GA = 20
		(17 THRU 24),	RT_GA = 22
		(1 THRU 16),	RT_GA = 24
		(),		GOTO R_TAKEOFF_ERROR
		ENDUSING
		IF (MAT_SEL.NE.GALV .AND. RT_GA.GT.24) RT_GA = 24
		LITMNO(4,5) = RT_GA,	'XX'
		RT_REST = 'PVS SADDLE'
		END
	ELSE	BEGIN
		USING RTLEN SELECT
		(.GE. 31),	RT_GA = 22
		(13 THRU 30),	RT_GA = 24
		(1 THRU 12),	RT_GA = 26
		(),		GOTO R_TAKEOFF_ERROR
		ENDUSING
		IF(MAT_SEL.NE.GALV .AND. RT_GA.GT.24) RT_GA = 24
		LITMNO(3,4) = RT_GA,	'XX'
		RT_REST = 'REG. T.O.'
		END

	TDESCR = TOFF

	XCALL ISIO (1, INVMAS, LITMNO, READ, LOKCTL)	;need this for price
	IF (LOKCTL .NE. 0) GOTO R_TAKEOFF_ERROR		;bad item
	CONFIG_PRICE = PRICE				;ssq 5-22-08

	if (.not. exact_match) return		;ssq 5-22-08
	read (1, invmas, rfa:girfa) [err=r_takeoff_error]	;re-read gi item
	tdescr = descr				;ssq 5-22-08
	RETURN

R_TAKEOFF_ERROR,
	OFFERROR
	XCALL OLMSG(WND_1, 23,'INVALID REGISTER TAKEOFF PART NUMBER',1)
	CONFIG_ERROR = 1
	RETURN
;----------------------------------------------------------

R_ANGLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRANGL = LITMNO(4,15)
	XCALL INSTR (1,WRANGL,'-',DASH)
	XCALL INSTR (1,WRANGL,'+',INCH)	;SSQ 6-1-98

	IF (DASH.EQ.0.AND.INCH.EQ.0) GOTO R_ANGLE_ERROR		;NEITHER
	IF (DASH.NE.0.AND.INCH.NE.0) GOTO R_ANGLE_ERROR		;BOTH
	IF (WRANGL(8,12).NE.BLANKS)  GOTO R_ANGLE_ERROR
	LITMNO(4,15) = 

	TDESCR = WRANGL(1,7)	
	IF (DASH) 
		BEGIN
		 ONERROR R_ANGLE_ERROR
		TDESCR(DASH,DASH) = '/'
		DSIZ = WRANGL(1,DASH-1)
		DECMAL = WRANGL(DASH+1,DASH+3)
		 OFFERROR
		RA_SIZE = DSIZ + DECMAL
		IF (DECMAL .LE. 0) GOTO R_ANGLE_ERROR	;3-26-99 SSQ
		END
	IF (INCH) 
		BEGIN
		TDESCR(INCH,INCH) = '"'
		 ONERROR R_ANGLE_ERROR
		RA_SIZE = WRANGL(1,INCH-1)
		 OFFERROR
		IF (WRANGL(INCH+1,12) .NE. BLANKS) GOTO R_ANGLE_ERROR	;2-17-99
		END

	xcall isio (1, invmas, litmno, read, lokctl)	;need this for price
	if (lokctl .ne. 0) goto r_angle_error

	RETURN

R_ANGLE_ERROR,
	OFFERROR
	XCALL OLMSG(WND_1, 23,'INVALID R-ANGLE PART NUMBER',1)
	CONFIG_ERROR = 1
	RETURN
;----------------------------------------------------------


SPIRAL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (KITMNO.EQ.'SX' .AND. MAT_SEL.NE.GALV)
		BEGIN
		XCALL OLMSG(WND_1, 23,"DON'T SELECT MATERIAL",1)
		CONFIG_ERROR = 1
		RETURN
		END
 
	IF (KITMNO(7,7).EQ.'+')		;PROCESS INCHES
	THEN	BEGIN
		 ONERROR SPIRAL_ERROR
		INCH = KITMNO(8,10)
		 OFFERROR
		TDESCR = '__"-__GA X ___"LONG SPIRL ____'
		TDESCR(12,14) = INCH,'ZZX'
		DPND = (INCH*100) / 12
		IF (DPND(5,6).NE.0) 
		THEN DSIZ = DPND(1,4) + 1 
		ELSE DSIZ = DPND(1,4)
		END
	ELSE	BEGIN
		INCH = 
		 ONERROR SPIRAL_ERROR
		DSIZ = KITMNO(7,8)
		 OFFERROR
		TDESCR = '__"-__GA X __''SPIRAL PIPE ____'
		TDESCR(12,13) = DSIZ,'ZX'
		DSIZ = KITMNO(7,8)
		END

	IF (DSIZ.GT.30) GOTO SPIRAL_ERROR

	TDESCR(1,2) = KITMNO(3,4)	;DIA
	TDESCR(5,6) = KITMNO(5,6)	;GA

	IF (MAT_SEL .EQ. ALUM)			;SSQ 5-12-03
		BEGIN
		DGA = KITMNO(5,6)	;GA
		USING DGA SELECT
		(18),	TDESCR(5,8) = '.063'
		(20),	TDESCR(5,8) = '.050'
		(22),	TDESCR(5,8) = '.040'
		(24),	TDESCR(5,8) = '.032'
		ENDUSING
		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SSQ 8-7-02
	LITMNO(7,15) = 
	SP_SIZE = DSIZ

; 5-12-03: force price to zero if alum or sst...
	IF (KITMNO.EQ.'SG') 
	THEN	USING MAT_SEL SELECT
		(GALV),	TDESCR(27,30) = 'GALV'
		(ALUM),	BEGIN
			TDESCR(27,30) = 'ALUM'
			SP_SIZE = 0		;FORCE PRICE TO ZERO
			END
		(SST),	BEGIN
			TDESCR(27,30) = 'SST'
			SP_SIZE = 0		;FORCE PRICE TO ZERO
			END
		(PGRP),	TDESCR(27,30) = 'PGRP'
		ENDUSING
	ELSE	TDESCR(27,30) = 'PVS '

	IS_CFG = 1		;ssq 2-7-06
	C_DESC = TDESCR		;ssq 2-7-06

	xcall isio (1, invmas, litmno, read, lokctl)	;need this for price
	if (lokctl .ne. 0) goto spiral_error
	RETURN

SPIRAL_ERROR,
	OFFERROR
	CONFIG_ERROR = 1
	XCALL W_DISP(WND_1, WD_POS,23,1,WD_CLR, WDC_LIN)
	DISPLAY (15,'ITEM CODE ENTERED: ',KITMNO,     '/ ITEM CODE CALCULATED: ',LITMNO      )
	XCALL OLMSG(WND_1,23,'INVALID ITEM NUMBER FOR SPIRAL',2)
	RETURN
;----------------------------------------------------------------
;sg.y

; add pipe lengths by dia and material...
sg_pipe,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	using fd_f1 select
	(64,65,66),	nop
	(),		return	;not alum, s04, s16
	endusing

	xcall alpdc (item(3,4), dia, switch)
	if (switch) return

	xqty = fd_spl*fd_qty

	using fd_f1 select
	(64),	tot_304 = tot_304 + xqty + 72	;6' scrap for each line
	(65),	tot_316 = tot_316 + xqty + 72
	(66),	tot_alu = tot_alu + xqty + 72
	endusing

	using fd_ga select
	(18),	call sg_18
	(20),	call sg_20
	(22),	call sg_22
	(24),	call sg_24
	endusing

	return
;------------------------------------------------
sg_18,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	for k from 1 thru 40
		begin
		using fd_f1 select
		(64),	begin
			if (dia.eq.s304dia_18(k))
				begin
				s304tl_18(k) = s304tl_18(k) + xqty
				exitloop
				end
			if (s304dia_18(k) .eq. 0)
				begin
				s304dia_18(k) = dia
				s304tl_18(k) = xqty
				s304prc_18(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		(65),	begin
			if (dia.eq.s316dia_18(k))
				begin
				s316tl_18(k) = s316tl_18(k) + xqty
				exitloop
				end
			if (s316dia_18(k) .eq. 0)
				begin
				s316dia_18(k) = dia
				s316tl_18(k) = xqty
				s316prc_18(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		(66),	begin
			if (dia.eq.alumdia_18(k))
				begin
				alumtl_18(k) = alumtl_18(k) + xqty
				exitloop
				end
			if (alumdia_18(k) .eq. 0)
				begin
				alumdia_18(k) = dia
				alumtl_18(k) = xqty
				alumprc_18(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		endusing		
		end

	return
;------------------------------------------------


sg_20,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	for k from 1 thru 40
		begin
		using fd_f1 select
		(64),	begin
			if (dia.eq.s304dia_20(k))
				begin
				s304tl_20(k) = s304tl_20(k) + xqty
				exitloop
				end
			if (s304dia_20(k) .eq. 0)
				begin
				s304dia_20(k) = dia
				s304tl_20(k) = xqty
				s304prc_20(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		(65),	begin
			if (dia.eq.s316dia_20(k))
				begin
				s316tl_20(k) = s316tl_20(k) + xqty
				exitloop
				end
			if (s316dia_20(k) .eq. 0)
				begin
				s316dia_20(k) = dia
				s316tl_20(k) = xqty
				s316prc_20(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		(66),	begin
			if (dia.eq.alumdia_20(k))
				begin
				alumtl_20(k) = alumtl_20(k) + xqty
				exitloop
				end
			if (alumdia_20(k) .eq. 0)
				begin
				alumdia_20(k) = dia
				alumtl_20(k) = xqty
				alumprc_20(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		endusing		
		end

	return
;------------------------------------------------


sg_22,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	for k from 1 thru 40
		begin
		using fd_f1 select
		(64),	begin
			if (dia.eq.s304dia_22(k))
				begin
				s304tl_22(k) = s304tl_22(k) + xqty
				exitloop
				end
			if (s304dia_22(k) .eq. 0)
				begin
				s304dia_22(k) = dia
				s304tl_22(k) = xqty
				s304prc_22(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		(65),	begin
			if (dia.eq.s316dia_22(k))
				begin
				s316tl_22(k) = s316tl_22(k) + xqty
				exitloop
				end
			if (s316dia_22(k) .eq. 0)
				begin
				s316dia_22(k) = dia
				s316tl_22(k) = xqty
				s316prc_22(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		(66),	begin
			if (dia.eq.alumdia_22(k))
				begin
				alumtl_22(k) = alumtl_22(k) + xqty
				exitloop
				end
			if (alumdia_22(k) .eq. 0)
				begin
				alumdia_22(k) = dia
				alumtl_22(k) = xqty
				alumprc_22(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		endusing		
		end

	return
;------------------------------------------------


sg_24,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	for k from 1 thru 40
		begin
		using fd_f1 select
		(64),	begin
			if (dia.eq.s304dia_24(k))
				begin
				s304tl_24(k) = s304tl_24(k) + xqty
				exitloop
				end
			if (s304dia_24(k) .eq. 0)
				begin
				s304dia_24(k) = dia
				s304tl_24(k) = xqty
				s304prc_24(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		(65),	begin
			if (dia.eq.s316dia_24(k))
				begin
				s316tl_24(k) = s316tl_24(k) + xqty
				exitloop
				end
			if (s316dia_24(k) .eq. 0)
				begin
				s316dia_24(k) = dia
				s316tl_24(k) = xqty
				s316prc_24(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		(66),	begin
			if (dia.eq.alumdia_24(k))
				begin
				alumtl_24(k) = alumtl_24(k) + xqty
				exitloop
				end
			if (alumdia_24(k) .eq. 0)
				begin
				alumdia_24(k) = dia
				alumtl_24(k) = xqty
				alumprc_24(k) = (fd_price*100/fd_spl)	;per inch
				clear fd_price
				exitloop
				end
			end
		endusing		
		end

	return
;------------------------------------------------

;=============================================================
OPEN_X,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Export files, fabdat...

	CLEAR OPNOK

	ONERROR OX_ERROR
;;;	IN_JOB = J_JOB	;;;>,		'XXXXXXXXXX'
;;;	OUT_FILE = IN_FILE
;;;	OUT_EXT = 'DAT'

	IN_FILE = J_JOB
	OUT_FILE = J_JOB
	LN = %TRIM(IN_FILE) + 1
	IN_FILE(LN,21) = 'TKO.TXT'
	OUT_FILE(LN,21) = 'TKO.DAT'

	FP_FILE = IN_FILE

;;;	F_PATH = EX_PATH(1,%TRIM(EX_PATH)) + FP_FILE
;;;	OPEN (CHN_IN, I, F_PATH(1,%TRIM(F_PATH)))

	OPEN (CHN_IN, I, F_PATH)

	FP_FILE = OUT_FILE

;;;	F_PATH = EX_PATH(1,%TRIM(EX_PATH)) + FP_FILE
;;;	OPEN (CHN_OUT, O, F_PATH(1,%TRIM(F_PATH)))

	OPEN (CHN_OUT, O, F_PATH)
	OPEN (CHN_FD, O, 'SPL:FABDAT.DDF')
	OFFERROR
	
	OPNOK = 1
OX_ERROR,
	RETURN
;---------------------------------------------

CLOSE_X,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN_IN) CLOSE CHN_IN
	IF (CHN_OUT) CLOSE CHN_OUT
	IF (CHN_FD) CLOSE CHN_FD

	RETURN
;--------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1, 'SI', 041, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 1

	SWITCH = 5
	XCALL FILES (3,'I', 060, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN060 = 3

	SWITCH = 5
	XCALL FILES (6, 'SI', 001, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN001 = 6

;;;	SWITCH = 5
;;;	XCALL FILES (7, 'I', 002, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN002 = 7

	SWITCH = 5
	XCALL FILES (16, 'SI', 166, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN166 = 16

	SWITCH = 5
	XCALL FILES (17, 'SI', 182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 17

	SWITCH = 5
	XCALL FILES (18, 'SU', 183, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN183 = 18

	SWITCH = 5
	XCALL FILES (29, 'SU', 137, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN137 = 29

	SWITCH = 5
	XCALL FILES (50, 'SU', 138, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN138 = 50

	SWITCH = 5
	XCALL FILEC ('SMC',84,'SU',084, SWITCH)
	CHN084 = 84


	chnf1 = 51	;for "spl:fabdat.ddf"
	CHNF2 = 52	;SSQ 10-10-06


;--------------------------------
; these channels opened in xpars.cp...

	chn_in = 21
	chn_out = 22
	chn_fd = 23
;--------------------------------
;;;	XCALL IO (CHN001, cusmas, 1, READ, LOKCTL)
;;;	BSEND = ORG001
;;;	ORGINV = ORG001

	XCALL FFILE (41,FIL041,LOKCTL)
	fil041(14,14) = 'M'

	XCALL FFILE (1,FILPRC,SWITCH)	;for price files

	SWITCH = 5
	XCALL FILES (135, 'SU', 135, SWITCH)	;FILE #135 ORDCM2
	CHN135 = 135


	OPNOK = 1
	RETURN
;--------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	close chnout
	close 54
	close 4
	close 5
	IF (CHN084) CLOSE CHN084
	IF (CHN135) CLOSE CHN135

	RETURN
;---------------------------------------------------
INPUT,
	XCALL WINPT(W_ID,ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,F_KEY)
	RETURN
;---------------------------------------------------



INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL U_START("LIB:SMC_WND",,,,,,,200)
	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,24,78)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'FABSHOP ORDER UPLOAD',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL W_UPDT
	W_ID = WND_1
	xcall u_logwnd(wnd_1)
	RETURN

get_window_d,
	XCALL W_PROC(WP_CREATE, WND_D, 'DAMPER',7, 42)
	XCALL W_DISP(WND_D, WD_CLEAR)
	RETURN

.END


.function	f_item		;spin-in collar
	rtn	,a
	ga	,d
	et	,a
	dimn	,a		;v9 
;;;	dimn	,d
	mat	,d		;material see "cfgmat.def"
	damper	,d		;0=no damper, 1=damper
	conical	,d
	scoop	,d
	s_off	,d
	ang	,d		;angle
	ara	,a		;order array
	spl	,d		;spiral pipe lenght in inches

;note: NO separate items by material - material designated
;		by material note...

record
	item	,a15

record	
	w_ara	,a12
record,x
	rt_ara	,6a2

record	l_dims
	dims	,10d2

record	vars
	wnw	,d1	;6-17-20 whole lbs for lpwgt?
	adim1	,a2	;dim1
	adim2	,a2	;dim2
	aga	,a2	;gauge
	a2	,a2	;
	a4	,a4	; 4-13-11, for SG-Spiral Pipe
;
	nn	,6a2	;dim2, ang, ga, etc.
	i	,d6
	j	,d6
	use_et	,d1	;1= use end treatment
;
.proc

	clear adim1, adim2, aga, use_et

	w_ara = ara		;work array - order of components...
	l_dims = dimn

; special cases for each routine...
	using rtn select
	('E90'),	begin				;90 adj elbow
			rtn = 'E'
			ang = 90
			end

	('E '),		if (ga.ne.24) clear ga		;e-z tap collar	

;;; 7-18-11>>>	('A'),		ang = '45'

			
	('GE','GA'),	if (ga .ge. 26) ga = 24
	('DD'),		ga = 24				;per ryan 6-22-06

	('C4','C9'),	NOP				;to prevent next case
	('C'),		using dims(1) select
			( 3 thru 12), ga = 24
			(13 thru 24), ga = 22
			(25 thru 99), ga = 20
			endusing


	('HE'),		nop				;super heto
	('H'),		begin
			clear et
			if (ga .ne. 24) clear ga	;per ryan 6-22-06
		;;;	if (dims(1) .le. 12) clear ga
			end
	endusing
;--------------------------------------


	item = rtn
	if (item .eq. 'BSE') item = 'BSEC'
	if (conical) item(%trim(item)+1, %trim(item)+1) = 'C'
	if (damper) item(%trim(item)+1, %trim(item)+1) = 'D'
	if (scoop) item(%trim(item)+1, %trim(item)+1) = 'S'
	if (s_off) item(%trim(item)+1, %trim(item)+1) = 'O'

	clear j
	for i from 1 thru 6
		begin
		clear nn(i)
		using rt_ara(i) select
		('ET'),	use_et = 1
		('GA'),	nn(i) = ga,	'ZZ'
		('AN'),	nn(i) = ang,	'XX'
		('F'),	begin
			incr j
			nn(i) = dims(j),'XX'
			end
		(),	nn(i) = rt_ara(i)	;use literal value (*,L,S)
		endusing

		a2 = nn(i)

		item(%trim(item)+1, %trim(item)+2) = a2

		end

	if (rtn.eq.'SG' .or. rtn.eq.'SP')
		begin
		if (spl/12*12 .eq. spl )	;even feet?
		then	a4(1,3) = (spl/12), 'ZZX' [left]
		else	begin
			a4(1,1) = '+'	;inches
			a4(2,4) = spl, 'ZZX' [left]
			end

		item(%trim(item)+1, %trim(item)+4) = a4
		end

	if (use_et) item(%trim(item)+1, %trim(item)+1) = et

	
	freturn item
.end

.function f_roundU
	in	,d	;number to be rounded up
	rd	,d	;number of dec places

record	vars
	temp	,d18
	decml	,d18
.proc

	temp = in##rd		;123.456 = 123.000
	if (in .gt. temp) 
	then	decml = in#rd + 1
	else	decml = in#rd

	freturn decml
.end

.function pde
	in	,d	;Price Duct Round Joints Code

record	vars
	decml,	d18

.proc
	using in select
	(1,9),	decml = 1	;w/ spiral ends
	(2),	decml = 3	;ez flange installed
	(3),	decml = 5	;crs companion rings swedged
	(4),	decml = 6	;crt companion rings tacked
	(5),	decml = 7	;crw companion rings full welded
	(8),	decml = 2	;le large end
	(),	decml = 0	;unknown
	endusing

	freturn decml
.end


