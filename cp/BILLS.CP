;  BILLS / COP isam
;DDEPT 5-29-14
;
;
;	6-25-99 SSQ: Build and sort unique work files for
;			biledt and invoic instead of using
;			"tmpidx"

;	11-2-11 ssq:	add 5% discount for customer 49250
;
;	01-16-14: make aracct isam
;	6-27-18: make cusmas isam
;	1-18-21: remove ordhdr.odisc

	.include 'wnd:tools.def'

external function
	gu_name	,a

EXTERNAL FUNCTION
	TRN3	,D


GLOBAL ORDH,	INIT
RECORD ORDHDR   	
		.INCLUDE 'DEF:RD044A.DEF'
ENDGLOBAL

GLOBAL DUCK	,INIT
		.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL

;;;GLOBAL PAR	,INIT
;;;RECORD PARAM
;;;	.INCLUDE 'DEF:PARAM.DEF'
;;;ENDGLOBAL

GLOBAL CPCHAN	,INIT
RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN007	,D2
	CHN041	,D2
	CHN042	,D2
	CHN044	,D2
	CHN045	,D2
	CHN054	,D2
	CHN060	,D2
	CHN169	,D2
	CHN170	,D2
	CHN171	,D2
	CHN172	,D2
	CHN174	,D2
	CHN175	,D2
	CHN178	,D2
	CHN182	,D2
	CHN066	,D2		;SSQ 5-24-05
	CHN101	,D3		;MISCUS 12-9-19
	CHN102	,D3		;MISSHP 12-9-19
ENDGLOBAL
;;; ----- END GLOBAL SECTION -----


record	invmas
	.include 'def:rd041a.def'

record	ordlin
	.include 'def:rd045a.def'
record,x
	.include 'def:rd045d.def'

record	usrsel
	.include 'def:rd065a.def'


record	shipto
	.include 'def:rd171a.def'

RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.DEF'

RECORD	DUCFIL
		,A4,	'SPL:'
		,A1,	'D'
	WRORD	,A6	
	,	A4,	'.ISM'

RECORD	FFILE
	FILNUM	,D2
	REDFIL	,A14
	CLCTL	,D1

RECORD ARACCT		
		.INCLUDE 'DEF:RD007A.DEF'
;RECORD	,X		
;		.INCLUDE 'DEF:RD007B.DEF'
RECORD COPCTL
		.INCLUDE 'DEF:RD060A.DEF'
RECORD ARTCDE
		.INCLUDE 'DEF:RD169A.DEF'
RECORD ARTCTL,X
		.INCLUDE 'DEF:RD169B.DEF'

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

RECORD	WRK_DATE
	MM	,D2
	DD	,D2
	YY	,D2

RECORD
	MDAYS,	12D2,	31,28,31,30,31,30,31,31,30,31,30,31


RECORD	VARS
	odisc	,d2	;1-18-21, removed from ordhdr so need it here
	has_lpcd	,d1
	sav_com	,a30	;6-30-11 for combl
	CHN065	,D2
	CMPCOD	,A3		;COMPANY CODE
	NXTSEQ		,D3
	MULTLINE	,D5
	CHNDUC	,D2
	SAVJOINT	,D5
	SAVFEET		,D6
	SAVFLIN		,D6
	SAVLBS		,D6
	SZ1		,D3
	SZ2		,D3
	OPNOK	,D1
	B_OPT	,D1
	cngtax	,d1
	TAXFRT	,D8
	BSEND	,D5
	BSMID	,D5
	SRCCTL	,D1
	KEY	,A7
	SRCHFL	,D1
	DEFACT	,D7
	SYSTEM	,D1
	DCHAR	,D3
	TCHAR	,D3
	SWITCH	,D1
	V	,D1
	D	,D1
	ORDNOA	,A6
	CUSNOA	,A6
	SAVCOM	,A70
	ENTRY	,A10
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	CTR	,D2
	SLSMAX	,D2, 08		;SET FOR MAX # OF COMMISSIONS
				;IN SALESMAN RECORDS
	SELECT	,D1
	COMPCT	,D4
	DECMAL	,D18
	OPTION	,D1
	MASK	,A10,'ZZZ,ZZZ.XX'
	MASK1	,A7,'XXXXXXX'
	MASK2	,A8,'XXXX-XXX'
	MASK3	,A6,'ZZZZZX'
	RUNTOT	,D8
	TAX	,D6
	TAXTOT	,D8
	MSCTAX	,D6
	TOTWGT	,D6
	WHLORD	,D1
	SKPORD	,D1
	NOITMS	,D1
	ENDLIN	,D1
	ALPHA	,A10
	BLANKS	,A35
	CNGCOM	,D1
	COL2	,D2
	READ	,D1	,0
	WRITE	,D1	,1
	STORE	,D1	,2
	LOKCTL	,D1
	PRGNAM	,A10
	OPENED	,D1
	MANTAX	,A1
	ORGART	,D5
	TODAY	,D6
	D_TRY	,D1
	D_DAYS	,D2
	D_DATE	,D6
	D_AMT	,D6
	D_LIN1	,A50
	D_LIN2	,A50
	D_LIN3	,A50
	DI	,D2
;;;	TXFLAG	,A3
PROC
	XCALL TERID (V)
	XCALL ENVRN (SYSTEM)
	XCALL RDATE(TODAY)
	XCALL WHO(CMPCOD)	;SSQ 6-15-04

	XCALL OUTPT (1,1,1,'BILLING',V)
	XCALL OUTPT (2,1,1,'\',1)
	CALL OPENS
	IF(.NOT. OPNOK) GOTO ENDOFF

	odisc = 0		;ssq 1-18-21
DISPLA,
	XCALL BLMNU (V)
	CTL = '08,47,01,01,#E'
	CALL INPUT
	GO TO (ENDOFF), INXCTL - 1
	SELECT = ENTRY(1,1)
	GO TO (SELCT,UNSEL,LIST,INVOIC,LIST2), SELECT
	GO TO DISPLA

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:CPMENU',1)

SELCT,
	XCALL OUTPT (1,11,1,'\',V)
	XCALL ORDBL (SAVCOM,WHLORD,SKPORD,DSTFLG,MLARFG,INXCTL,
&		RUNTOT, TAXTOT, TOTWGT, OCOST)

	IF (INXCTL.EQ.2) GO TO DISPLA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; per dianna 7-23-2012: don't update ship-to tax code if
; user changes tax code in select.

;;;	IF (OSHPTO.GT.0 .AND. OSHPTO.LT.9998) CALL CHECK_ST_TAX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	IF (SKPORD) GO TO SELCT

; consolidate ductwork and write back to DUCACC.ISM file...
;Create output file...
	WRORD = OORDNO,'XXXXXX'
	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (41, SU, DUCFIL)
	CHNDUC = 41

	CALL LOAD_DUCT
	CALL CONS_DUCT
	CALL DEL_DUCT		;DELETE DUCTWORK RECORDS FOR THIS ORDER
	CALL STOR_DUCT		;STORE THE CONSOLIDATED DUCTWORK

	CLOSE CHNDUC
	XCALL DELET(DUCFIL)

	ORDNOA = OORDNO, MASK3
	CUSNOA = OCUSNO, MASK3
	NOITMS = 1

	GOTO (MSC), WHLORD

B_OPTION,
	CALL B_MENU
	USING B_OPT SELECT
	(1),	GOTO MSC		;END
	(2),	BEGIN			;MEMO
		xcall u_start('lib:smc_wnd',,,,,,,100)
		XCALL MEMBL
		xcall u_finish
		END
	(3),	XCALL LINBL(RUNTOT,TAXTOT,TOTWGT,OCOST)
	(4),	XCALL DUCBL(OORDNO,ODISC,RUNTOT,TAXTOT,WHLORD,OTAXFL)
	(5),	BEGIN			;NOTES
		xcall u_start('lib:smc_wnd',,,,,,,100)
		XCALL COMBL (sav_com)
		xcall u_finish
		END
	(),	GOTO B_OPTION
	ENDUSING
	GOTO B_OPTION

B_MENU,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL OUTPT(2,1,2,'SELECT OPTIONS',1)
	XCALL OUTPT (8,20,2,'                  OPTION:',1)
	XCALL OUTPT (8,20,2,'SELECT AN OPTION BELOW',1)
	XCALL OUTPT (10,25,0,'1. BILL ENTIRE ORDER',1)
	XCALL OUTPT (12,25,0,'2. MEMO',1)
	XCALL OUTPT (14,25,0,'3. LINE',1)
	XCALL OUTPT (16,25,0,'4. DUCT',1)
	XCALL OUTPT (18,25,0,'5. NOTES',1)
	XCALL INPUT(8,48,01,01,'# ',ENTRY,INXCTL,1)
	B_OPT = ENTRY(1,1)
	RETURN
;----------------------------------------------------------
;-----------------------------------------------
LOAD_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; LOAD DUCTWORK INTO WORKFILE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Note: only the ductwork data is moved into the work file,
; it is assumed that all the acc info has already been moved
; to the ordlin file...

	FIND(CHN175,DUCACC,OORDNO) [ERR=LD_LOOP]
LD_LOOP,
	LOKCTL = 1
	XCALL IOS (CHN175,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.OORDNO) GOTO LD_EOF
	IF(ACC.NE.2)
		BEGIN
		XCALL MESAG('Accessories still exist!',2)
		GOTO ENDOFF
		END

	XCALL DDEPT (GAUGE,SIZE3,DUTYPE,WD_DEPT,STY)
;;;	USING STY SELECT
;;;	(1,4,5),	WD_DEPT='P'		;TDC
;;;	(2,3,6),	WD_DEPT='O'		;S&D
;;;	ENDUSING

	WDUTYPE = DUTYPE	; Type of duct 
	WSTY = 10-STY		; Style		descending
	WLINER = LINER		; Liner 
	WSEAM = SEAM		; Seam  
	WSEAL = SEAL		; Seal
	WGAUGE = GAUGE		; Gauge ( 26,24,22,20,18 )
	WSIZE3 = 100-SIZE3	; LENGTH	descending
	WSIZE1 = 1000000-SIZE1	; SLIP		descending
	WTHICK = THICK		; Dec. Thickness .024-24g .032-22g .040-20g .050-18g
	WCAT = CAT		; Catagory 
	WJOINT = JOINT		; Number of joints ( quantity)
	WSIZE2 = 1000000-SIZE2	; DRIVE		descending	

	WLOCAT = LOCAT
	WSQFEET = SQFEET
	WPOUNDS = POUNDS
	WSQFLIN = SQFLIN
	WGPRICE = GPRICE
	WLINPRC = LINPRC
	WDPUNCH = DPUNCH
	WDSEQ = DSEQ	
;;;	WRST175=RST175	;8-25-09: used for wd_cngd...
	WFLG175=FLG175

	STORE(CHNDUC,DUCWRK,WD_KEY)

	GOTO LD_LOOP
LD_EOF,
	RETURN
;-----------------------------------------------
CONS_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; CONSOLIDATE DUCTWORK
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR MULTLINE, SAVJOINT, SAVFEET, SAVFLIN, SAVLBS
	FIND(CHNDUC,DUCWRK,^FIRST) [ERR=EOF_CD]
	SAV_WD_KEY = '***'

CD_LOOP,
	READS(CHNDUC,DUCWRK,EOF_CD)
	IF(WD_KEY .NE. SAV_WD_KEY)
	THEN	CALL WD_NEWKEY
	ELSE	BEGIN
		INCR MULTLINE
		SAVJOINT = SAVJOINT + WJOINT
		SAVFEET = SAVFEET + WSQFEET
		SAVFLIN = SAVFLIN + WSQFLIN
		SAVLBS = SAVLBS + WPOUNDS
		DELETE(CHNDUC)
		END
	GOTO CD_LOOP		
EOF_CD,
	CALL WD_NEWKEY
	RETURN

WD_NEWKEY,
	IF(SAV_WD_KEY .EQ. '***') GOTO OUT_WDKEY
	IF(MULTLINE .EQ. 0) GOTO OUT_WDKEY
	READ(CHNDUC,DUCWRK,SAV_WD_KEY)
	WJOINT = SAVJOINT
	WSQFEET = SAVFEET
	WPOUNDS = SAVLBS
	WSQFLIN = SAVFLIN
	WRITE(CHNDUC,DUCWRK,SAV_WD_KEY)

OUT_WDKEY,
	SAV_WD_KEY = WD_KEY
	SAVJOINT = WJOINT
	SAVFEET = WSQFEET
	SAVFLIN = WSQFLIN
	SAVLBS = WPOUNDS

	CLEAR MULTLINE
	RETURN
;-----------------------------------------------

DEL_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; DELETE ALL DUCTWORK RECORDS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FIND(CHN175,DUCACC,OORDNO) [ERR=DELD_LOOP]
DELD_LOOP,
	LOKCTL = 1
	XCALL IOS (CHN175,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.OORDNO) GOTO DELD_EOF
	DELETE(CHN175)
	GOTO DELD_LOOP
DELD_EOF,
	RETURN
;-----------------------------------------------
STOR_DUCT,
	CLEAR NXTSEQ
	FIND(CHNDUC,DUCWRK,^FIRST) [ERR=EOF_STORD]
STORD_LOOP,
	READS(CHNDUC,DUCWRK,EOF_STORD)
	CLEAR DUCACC
	DUCTOR = OORDNO		;CURRENT ORDER #

	DUTYPE= WDUTYPE 	; Type of duct 
	STY= 10-WSTY 		; Style		descending
	LINER= WLINER 		; Liner 
	SEAM= WSEAM 		; Seam  
	SEAL= WSEAL 		; Seal
	GAUGE= WGAUGE 		; Gauge ( 26,24,22,20,18 )
	SIZE3= 100-WSIZE3 	; LENGTH	descending
	SIZE1= 1000000-WSIZE1 	; SLIP		descending
	THICK= WTHICK 		; Dec. Thickness .024-24g .032-22g .040-20g .050-18g
	CAT= WCAT 		; Catagory 
	JOINT= WJOINT 		; Number of joints ( quantity)
	SIZE2= 1000000-WSIZE2 	; DRIVE		descending	

	LOCAT= WLOCAT 
	SQFEET= WSQFEET 
	POUNDS= WPOUNDS 
	SQFLIN= WSQFLIN 
	GPRICE= WGPRICE 
	LINPRC= WLINPRC 
	DPUNCH= WDPUNCH 

	ACC=2			;NO ACC
	INCR NXTSEQ
	DSEQ= NXTSEQ

	XCALL ISIO (CHN175,DUCACC,KEY175,STORE,LOKCTL)

	GOTO STORD_LOOP
	
EOF_STORD,
	RETURN
;-----------------------------------------------

;----------------------------------------------------------
;----------------------------------------------------------
MSC,
;;;	if (ocusno .eq. 63340) xcall rocbl(runtot,taxtot)

	IF (DSTFLG.NE.'Y') GO TO MSC1
;	LOKCTL = 1
;	XCALL IO (7,ARACCT,1,READ,LOKCTL)
;	BSEND = ORG007
MSC1,
	XCALL FFILE (41, REDFIL, CLCTL)	
	redfil(14,14) = 'M'
	close chn041
	open(chn041,si,redfil)
;----------------------------------------------------------
; 7-24-08 select is no longer counting barrel clamps...
;
;;;	xcall oejr(oordno,chn041,chn042,chn045,runtot,taxtot,oloc)
;----------------------------------------------------------

;;;	if (ocusno .eq. 63340) xcall rocbl(runtot,taxtot)	;moved here 7-29-04

; 1-21-19 - changed to include all 3 companies...
	using ocusno select
	(63340), xcall rocbl(runtot,taxtot)	;roc
	(65545), xcall rocbl(runtot,taxtot)	;smc
	(68780), xcall rocbl(runtot,taxtot)	;smp
	(68781), xcall rocbl(runtot,taxtot)	;php
;;;	(65545), if (cmpcod.ne. 'CAT') xcall rocbl(runtot,taxtot)	;added 4-7-14
	endusing

	close chn041
	open(chn041,su,redfil)

	XCALL BLTOT(RUNTOT, TAXTOT, TOTWGT, OCOST)

;2-29-12 - this was never approved and got installed w/ the ship-to/tax code
;		mods...

;;;	clear ordlin, has_lpcd
;;;	lordno = oordno
;;;	find (chn045, ordlin, ordkey) [err=no_lpcd]
;;;no_lpcd,
;;;	xcall ios (chn045, ordlin, read, lokctl)
;;;	if (lokctl .ne. 0) goto skip_lpcd
;;;	if (lordno .ne. oordno) goto skip_lpcd
;;;	if (litmno .eq. 'LPCD') has_lpcd = 1
;;;	goto no_lpcd
;;;
;;;skip_lpcd,
;;;	if (ocusno.eq.49250 .and. .not. has_lpcd)	;MMC
;;;	  begin
;;;	  clear ordlin
;;;	  lordno = oordno
;;;	  litmno = 'LPCD'
;;;	  lokctl = 1
;;;	  xcall isio (chn041, invmas, litmno, read, lokctl)
;;;	  if (lokctl .eq. 0)
;;;		begin
;;;		incr ordseq
;;;		linseq = ordseq
;;;		decmal = (runtot*10*5)#2	;5%
;;;		lflag = 1		;selected
;;;		ltype = 'L'
;;;		litmno = itemno
;;;		ldescr = descr
;;;		lprdcd = prdcat
;;;		ldept = usrdef
;;;		lsrtsq = priccd
;;;		lloc = oloc
;;;		lcost = avgcst
;;;		lstokt = stock
;;;		litmwt = weight
;;;		if (otaxfl.ne.'   '.and.txflag.eq.'y') ltxflg = 1
;;;		luofm = suofm
;;;		
;;;		lqtyor = -1
;;;		lqtysh = -1
;;;		lprice = decmal
;;;		decmal = (decmal*lqtysh)#1		
;;;		lokctl = 1
;;;		xcall isio (chn045, ordlin, ordkey, store, lokctl)
;;;		
;;;		runtot = runtot + decmal
;;;		taxtot = taxtot + decmal
;;;		end
;;;	  end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CALL ORDMSC
	GO TO SELCT
UNSEL,
	CALL CLOSE
	XCALL PGCHN ('CP:UNBILL',1)
LIST,
	CALL CLOSE

;-----------------------------------------------------
	CALL BIL_IDX
	XCALL PGCHN ('CP:BILEDT',1)
;-----------------------------------------------------

LIST2,
	CALL CLOSE
	XCALL PGCHN ('CP:BILED2',1)

INVOIC,
	CALL CLOSE

	CALL INV_IDX
;;;	XCALL PGCHN ('CP:ALNINV',1)
	XCALL PGCHN ('CP:INVOIC',1)
;
OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK
	OPENED = 1

	SWITCH = 1

;;;	XCALL FILES (6,'U',041,SWITCH)		;FILE # 41 - ITMMAS FILE
	XCALL FILES (6,'SI',041,SWITCH)		;FILE # 41 - ITMMAS FILE
	IF (SWITCH.EQ.9) RETURN
	CHN041 = 6				;ssq 12-02-03

;;;	XCALL FILES (10,'I',042,SWITCH)
;;;	IF (SWITCH.EQ.9) RETURN
;;;	CHN042 = 10

	XCALL FILES (4,'SU',44,SWITCH)		;FILE # 44 - ORDHDR FILE
	IF (SWITCH.EQ.9) RETURN
	CHN044 = 4

	XCALL FILES (5,'SU',45,SWITCH)		;FILE # 45 - ORDLIN FILE
	IF (SWITCH.EQ.9) RETURN
	CHN045 = 5

	SWITCH = 5
	XCALL FILES (3,'I',60,SWITCH)		;FILE # 60 - COPCTL FILE
	IF (SWITCH.EQ.9) RETURN
	CHN060 = 3

	LOKCTL = 1
	XCALL IOS (3,COPCTL,READ,LOKCTL)
	CLOSE 3
	SAVCOM (1,35) = ORDCOM (1)
	SAVCOM (36,70) = ORDCOM (2)

	SWITCH = 1
	XCALL FILES (7,'SI',07,SWITCH)		;FILE # 07 - ARACCT FILE
	IF (SWITCH.EQ.9) RETURN
	CHN007 = 7

	XCALL FILES (69,'I',169,SWITCH)		;FILE # 169 -- ARTCDE
	IF (SWITCH.EQ.9) RETURN
	CHN169 = 69

	XCALL FILES (70,'I',170,SWITCH)		;FILE # 170 -- ARTERM
	IF (SWITCH.EQ.9) RETURN
	CHN170 = 70

	XCALL FILES (71,'SI',171,SWITCH)		;FILE # 171 -- SHIPTO
	IF (SWITCH.EQ.9) RETURN
	CHN171 = 71

	XCALL FILES (72,'I',172,SWITCH)		;FILE # 172 -- SHPVIA
	IF (SWITCH.EQ.9) RETURN
	CHN172 = 72

;;;	XCALL FILES (2,'I',002,SWITCH)		;FILE # 002 -- CUSIDX
;;;	IF (SWITCH.EQ.9) RETURN
;;;	CHN002 = 2

	XCALL FILES (1,'SI',001,SWITCH)		;FILE # 001 -- CUSMAS
	IF (SWITCH.EQ.9) RETURN
	CHN001 = 1

	XCALL FILES (74,'I',174,SWITCH)		;FILE # 174 -- DUCFIL
	IF (SWITCH.EQ.9) RETURN
	CHN174 = 74

	XCALL FILES (75,'SU',175,SWITCH)	;FILE # 175 -- DUCACC
	IF (SWITCH.EQ.9) RETURN
	CHN175 = 75

	XCALL FILES (78,'SI',178,SWITCH)	;FILE #178 -- BLHEAD
	IF (SWITCH.EQ.9) RETURN
	CHN178 = 78

	SWITCH = 5	;2-28-01
	XCALL FILES (17,'SI',182,SWITCH)	;FILE # 182 -- COPTBL
	IF (SWITCH.EQ.9) RETURN
	CHN182 = 17				;ssq 12-02-03

	SWITCH = 5				;
	XCALL FILES (66, 'SI', 066, SWITCH)	;FILE # 66 -- BLHTAG
	IF (SWITCH .EQ. 9) RETURN
	CHN066 = 66				;ssq 5-24-05

;;;	XCALL FILES (82,'SI',182,SWITCH)	;FILE # 182 -- COPTBL
;;;	IF (SWITCH.EQ.9) RETURN
;;;	CHN182 = 82

	SWITCH = 5
	XCALL FILES (65,'SU',065,SWITCH)	;FILE # 65 -- USRSEL
	IF (SWITCH.EQ.9) RETURN
	CHN065 = 65

	SWITCH = 5
	XCALL FILES (101, 'SI', 101, SWITCH)	;FILE # 101 -- MISCUS
	IF (SWITCH .EQ. 9) RETURN
	CHN101 = 101

	SWITCH = 5
	XCALL FILES (102, 'SI', 102, SWITCH)	;FILE # 102 -- MISSHP
	IF (SWITCH .EQ. 9) RETURN
	CHN102 = 102

	OPNOK = 1
	RETURN
;--------------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SWITCH = 4
;;;	IF(CHN041) XCALL FILES (9,'U',041,SWITCH)
	IF(CHN041) XCALL FILES (6,'SU',041,SWITCH)	;ssq 12-02-03
;;;	IF(CHN042) XCALL FILES (10,'I',042,SWITCH)
	IF(CHN044) XCALL FILES (4,'SU',44,SWITCH)
	IF(CHN045) XCALL FILES (5,'SU',45,SWITCH)

	IF(CHN007) XCALL FILES (7,'I',07,SWITCH)
	IF(CHN169) XCALL FILES (69,'I',169,SWITCH)
	IF(CHN170) XCALL FILES (70,'I',170,SWITCH)
	IF(CHN171) XCALL FILES (71,'SI',171,SWITCH)
	IF(CHN172) XCALL FILES (72,'I',172,SWITCH)
;;;	IF(CHN002) XCALL FILES (2,'I',002,SWITCH)
	IF(CHN001) XCALL FILES (1,'SI',001,SWITCH)
	IF(CHN174) XCALL FILES (74,'I',174,SWITCH)
	IF(CHN175) XCALL FILES (75,'SU',175,SWITCH)	

	IF(CHN066) XCALL FILES (66,'SI',066,SWITCH)	;ssq 05-24-05
	RETURN
;--------------------------------------------------------

INPUT,
	XCALL INPUT(ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	RETURN
INPUT2,
	XCALL INPUT(ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,1)
	RETURN
;
ORDMSC,
	OFLAG = 1
	XCALL OE3B (ORDNOA,CUSNOA,OCUSNM,MLMSFG,MLSTFG,MLFRFG,V)

;------------------------------------------------
; add this code to oe3b later...
	XCALL OUTPT (21,7,0,'6. DISC TEXT',1)
	XCALL OUTPT (22,7,0,'7. DISC AMT',1)
;------------------------------------------------

	XCALL OUTPT (18,40,0,OTAXFL,1)
;;;	XCALL OUTPT (19,40,0,TXFLAG,1)
	CTL = '04,25,08'
	DECMAL = RUNTOT
	CALL DSPDLR
	ALPHA = TOTWGT,'Z,ZZZ.XX'
	XCALL OUTPT (2,57,0,ALPHA,V)
	ROW = 6
	DECMAL = TAXTOT
	CALL DSPDLR
DISPL3,
	WHATNO =
	CNGCTL =
;;;ssq 7-28-99	OFRGHT =
	OMISC =
	OTAX =
	MANTAX = 
	OMSACT = DEFMSA
	OTXACT(1) = DEFSTA
	OFRACT = DEFRET

;;; SSQ 01-03-00 moved here from below...
;;;	IF (OSCAC .EQ. '2')		;SSQ 9-30-98
;;;	THEN	OFRACT = 6043000
;;;	ELSE	OFRACT = DEFRET

	OFRACT = DEFRET			;SSQ 6-15-04
	USING CMPCOD SELECT
	('SMC'),	IF(OSCAC.EQ.'2') OFRACT = 6043100
	('ROC'),	IF(OSCAC.EQ.'2') OFRACT = 6043200
	ENDUSING

;;; SSQ 01-03-00 moved here from below...

	CTL = '08,22,07,00,$-'
	IF (OFRGHT .NE. 0)
	   BEGIN
	   ENTRY(1,10) = OFRGHT,	'ZZ,ZZZ.XX-' [LEFT]
	   XCALL OUTPT (ROW, COL, 1, ENTRY(1,10), 1)
	   CALL ACCEPT
	   IF (TCHAR.NE.13) 
		BEGIN
		CALL INPUT
		GO TO (ORDMSC), INXCTL
		OFRGHT = ENTRY
	;;;	IF (OSCAC .EQ. '2')		;SSQ 9-30-98
	;;;	THEN	OFRACT = 6043000
	;;;	ELSE	OFRACT = DEFRET
		END
	   GOTO FACCT
	   END
FRGHT,
	CTL = '08,22,07,00,$-'
	CALL INPUT
	GO TO (ORDMSC), INXCTL
	OFRGHT = ENTRY
FACCT,
	DEFACT = OFRACT
;;;	IF (MLFRFG.NE.'Y'.OR.OFRGHT.EQ.0) GO TO FRGHT1
	IF (OFRGHT.EQ.0) GO TO FRGHT1
	CALL GETACT
	GO TO (ORDMSC), INXCTL
FRGHT1,
	OFRACT = DEFACT
	GO TO (ORDTOT), CNGCTL
MISC,
	CTL = '10,22,06,00,$ '
	CALL INPUT
	GOTO (ORDMSC), INXCTL
	OMISC = ENTRY
	IF (OMISC.NE.0)
	BEGIN
	  XCALL MESAG ('SORRY CHARLIE, NO MISC CHARGES ALLOWED HERE',1)
	  GOTO MISC
	END
	DEFACT = OMSACT
	IF (MLMSFG.NE.'Y'.OR.OMISC.EQ.0) GO TO MISC1
	CALL GETACT
	GO TO (ORDMSC), INXCTL
MISC1,
	OMSACT = DEFACT
	MSCTAX =
;;;	IF (RUNTOT.EQ.0) GOTO NOMTAX
;;;	MSCTAX = ((TAXTOT*OMISC*10)/RUNTOT) #1
;;;NOMTAX,
	CTL = '12,26,06'
	DECMAL = MSCTAX
	CALL DSPDLR
	CTL = '14,22,07,00,$ '
	IF (CNGCTL.EQ.1) GOTO ORDTOT
TAX,
	SWITCH = 5
;;;	CLOSE 1			;SSQ 9-12-97
;;;	XCALL FILES (1,'I',169,SWITCH)
	OTXACT(1) = DEFSTA
	MANTAX = 'N'
	ARTCDE =
FNDTCD,
	LOKCTL = 1 
	XCALL IO (CHN169,ARTCTL,1,READ,LOKCTL)
	ORGART = ORG169
	CTL = '14,22,07,00,$-'
	IF (OTAXFL.EQ.'   '.OR.SWITCH.EQ.9) GOTO SKP1 ;CUSTOMER HAS NO TAX RATE
	KEY(1,3) = OTAXFL
	SRCCTL = 2
	XCALL SERCH (CHN169,ARTCDE,KEY(1,3),1,3,ORGART,BSMID,SRCCTL,4,4,9,0,0,0,0)
	IF (SRCCTL.EQ.0) 
		BEGIN
		XCALL OUTPT (18,45,0,ARTDSC,1)	;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;SSQ 2-20-98 WISC FREIGHT IS TAXABLE...
;;;ssq 1-09-02 MN freight is now taxable...
;;;ssq 1-09-07 SD freight is now taxable...
;;;ssq 9-9-14 all "M" freight is taxable
		TAXFRT =
		IF (OTAXFL(1,1) .EQ. 'M') TAXFRT = OFRGHT
	;;;	IF (OTAXFL(1,2) .EQ. 'MN') TAXFRT = OFRGHT
	;;;	IF (OTAXFL(1,2) .EQ. 'MX') TAXFRT = OFRGHT
	;;;	IF (OTAXFL(1,2) .EQ. 'MR') TAXFRT = OFRGHT	;6-11-14
	;;;	IF (OTAXFL(1,2) .EQ. 'MH') TAXFRT = OFRGHT	;6-11-14
		IF (OTAXFL(1,1) .EQ. 'W')  TAXFRT = OFRGHT
		IF (OTAXFL(1,1) .EQ. 'S')  TAXFRT = OFRGHT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		END
SKP1,
	IF (SRCCTL.OR.OTAXFL.EQ.'   ')
	BEGIN
	  XCALL OUTPT (24,1,1,'INVALID TAX CODE OR TAX CODE NOT FOUND - REENTER <N> ? ',1)
	  XCALL INPUT (24,56,01,00,'YN',ENTRY,INXCTL,1)
	  IF (INXCTL.EQ.1) 
	  BEGIN
	    XCALL INPUT (18,40,03,00,'AT',ENTRY,INXCTL,1)
	    OTAXFL = ENTRY
	    GOTO FNDTCD
	  END
	  MANTAX = 'Y'
	  ARTCDE =
	END
;;;
ASK,
;;;	CLOSE 1
	CTL = '14,22'
	OTAX(1) = ((MSCTAX + TAXTOT + TAXFRT) * ARTPRT(1) / 100) # 3
	XCALL DSPLY (MAX,14,COL,OTAX(1),3,1)	;
	OTXACT(1) = ARTGLN(1)
	DEFACT = OTXACT(1)
	SRCHFL = 1
	CALL SERCH
	CALL DSPACT

	CTL = '15,22'
	OTAX(2) = ((MSCTAX + TAXTOT + TAXFRT) * ARTPRT(2) / 100) # 3
	XCALL DSPLY (MAX,15,COL,OTAX(2),3,1)	;
	OTXACT(2) = ARTGLN(2)
	DEFACT = OTXACT(2)
	SRCHFL = 1
	CALL SERCH
	CALL DSPACT

	CTL = '16,22'
	OTAX(3) = ((MSCTAX + TAXTOT + TAXFRT) * ARTPRT(3) / 100) # 3
	XCALL DSPLY (MAX,ROW,COL,OTAX(3),3,1)	;
	OTXACT(3) = ARTGLN(3)
	DEFACT = OTXACT(3)
	SRCHFL = 1
	CALL SERCH
	CALL DSPACT

	TAX = OTAX(1) + OTAX(2) + OTAX(3)
	XCALL DSPLY (MAX,18,COL,TAX,3,1)	;Display default tax amount
;;;	GOTO ORDTOT

	cngtax = 0
OTAX1,
	CTL = '14,22,07,00,$-'
	CALL ACCEP2
	IF (TCHAR.EQ.13.AND.CNGCTL.EQ.0) GO TO OTAX2
	IF (TCHAR.EQ.13) GO TO OTXAC1
	IF (TCHAR.NE.9) GOTO OTAX1
	CALL INPUT2
	GOTO (DISPLA), INXCTL
	OTAX(1) = ENTRY
	TAX = OTAX(1) + OTAX(2) + OTAX(3)
	XCALL DSPLY (MAX,18,COL,TAX,3,1)	;Display default tax amount
	cngtax = 1	;ssq 6-5-98
OTXAC1,
	COL = 40
	CALL ACCEP2
	IF (TCHAR.EQ.13) GOTO (OTAX2,ORDTOT), CNGCTL+1
	IF (TCHAR.NE.9) GOTO OTXAC1
	CALL GETAC1
	GOTO (OTAX1), INXCTL
	OTXACT(1) = DEFACT
	GOTO (ANYCN3), CNGCTL
OTAX2,
	CTL = '15,22,07,00,$-'
	CALL ACCEP2
	IF (TCHAR.EQ.13.AND.CNGCTL.EQ.0) GO TO OTAX3
	IF (TCHAR.EQ.13) GO TO OTXAC2
	IF (TCHAR.NE.9) GOTO OTAX2
	CALL INPUT2
	GOTO (DISPLA), INXCTL
	OTAX(2) = ENTRY
	TAX = OTAX(1) + OTAX(2) + OTAX(3)
	XCALL DSPLY (MAX,18,COL,TAX,3,1)	;Display default tax amount
	cngtax = 1	;ssq 6-5-98
OTXAC2,
	COL = 40
	CALL ACCEP2
	IF (TCHAR.EQ.13) GO TO (OTAX3,ORDTOT), CNGCTL+1
	IF (TCHAR.NE.9) GOTO OTXAC2
	CALL GETAC1
	GOTO (OTAX2), INXCTL
	OTXACT(2) = DEFACT
	GOTO (ANYCN3), CNGCTL
OTAX3,
	CTL = '16,22,07,00,$-'
	CALL ACCEP2
	IF (TCHAR.EQ.13.AND.CNGCTL.EQ.0) GO TO ORDTOT
	IF (TCHAR.EQ.13) GO TO OTXAC3
	IF (TCHAR.NE.9) GOTO OTAX3
	CALL INPUT2
	GOTO (DISPLA), INXCTL
	OTAX(3) = ENTRY
	TAX = OTAX(1) + OTAX(2) + OTAX(3)
	XCALL DSPLY (MAX,18,COL,TAX,3,1)	;Display default tax amount
	cngtax = 1	;ssq 6-5-98
OTXAC3,
	COL = 40
	CALL ACCEP2
	IF (TCHAR.EQ.13) GO TO (ORDTOT,ORDTOT), CNGCTL+1
	IF (TCHAR.NE.9) GOTO OTXAC3
	CALL GETAC1
	GOTO (OTAX3), INXCTL
	OTXACT(3) = DEFACT
	GOTO (ANYCN3), CNGCTL
	GOTO ORDTOT

;;;OK,
;;;TAX0,
;;;	IF (OTAX(1).EQ.0) GOTO TAX1
;;;	DEFACT = OTXACT(1)
;;;	IF (MLSTFG.NE.'Y'.OR.TAX.EQ.0) GO TO TAX1
;;;
;;;	CALL GETACT
;;;	MANTAX = 
;;;	GO TO (ORDMSC), INXCTL
;;;TAX1,
;;;	OTXACT(1) = DEFACT

ORDTOT,
;;;	IF (.NOT.CNGCTL .and. .not. cngtax) CALL COMP_TAX
	IF (.not. cngtax) CALL COMP_TAX	;SSQ 4-28-99

	CTL = '20,22,10'
	DECMAL = RUNTOT + OFRGHT + OMISC + TAX
	CALL DSPDLR
	OSALE = RUNTOT
	GO TO (ANYCN3), CNGCTL

;;;	DSCODE = 1
	XCALL WHO(CMPCOD)	;SSQ 1-15-04
	USING CMPCOD SELECT
	('SMC'),	DSCODE = 1
	('ROC'),	DSCODE = 1
	('TST'),	DSCODE = 1
	(),	BEGIN
			DSCODE = 0
			OCOMNT(1) = ORDCOM(1)
			OCOMNT(2) = ORDCOM(2)
			END
	ENDUSING

	using ocusno select
	(63340),	begin
			ocomnt(1) = 'Rockford'
			call inter_comp
			end
	(68780),	begin
			ocomnt(1) = 'Std Metal Products'
			call inter_comp
			end
	(65545),	begin
			ocomnt(1) = 'SMC Mpls'
			call inter_comp
			end
	endusing

;;;	if (ocusno .eq. 63340)		;ssq 4-22-04
;;;		begin
;;;		DSCODE = 0
;;;		OCOMNT(1) = 'Rockford'
;;;		OCOMNT(2) = 
;;;		end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	DSAMT = 0
	CALL DISPLA_DISC
	GOTO ANYCN3

inter_comp,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		DSCODE = 0
		OCOMNT(2) = 
	return
;-----------------------------------------------------

DSCODE,
	XCALL INPUT (21,22,01,00,'# ',ENTRY,INXCTL,1)
	GOTO (ORDMSC),INXCTL
	IF (ENTRY(1,1) .EQ. ' ') ENTRY(1,1) = '1'
	DSCODE = ENTRY(1,1)
	IF (DSCODE .GT. 5) GOTO DSCODE
	CALL DISPLA_DISC
	GO TO (ANYCN3), CNGCTL
DSAMT,
	XCALL INPUT (22,22,06,00,'$ ',ENTRY,INXCTL,1)
	DSAMT = ENTRY(1,6)
	CALL DISPLA_DISC
	GO TO ANYCN3

DISPLA_DISC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR D_LIN1, D_LIN2, D_LIN3

	USING DSCODE SELECT
	(0),	BEGIN
		D_LIN1 = OCOMNT(1)
		D_LIN2 = OCOMNT(2)
		END
	(1),	BEGIN
		D_DAYS = 15
		CALL MAKE_D_DATE
		IF (DSAMT.EQ.0)
	;;;	THEN	D_AMT = (2*OSALE)#2
		THEN	D_AMT = (1*OSALE)#2
		ELSE	D_AMT = DSAMT

		D_LIN1 = "A discount of $"
		D_LIN1(16,23) = D_AMT, 'ZZZX.XX' [LEFT]
		DI = %TRIM(D_LIN1)
		D_LIN1(DI+1, 50) = " may be taken only if"
		
		D_LIN2 = "payment is received and in our office by"
		D_LIN2(42,49) = D_DATE, 'ZX/XX/XX' [LEFT]
		END
	(2),	BEGIN
		D_DAYS = 30
		CALL MAKE_D_DATE
		D_LIN1 = "Payment must be received in our office by"
		D_LIN1(43,50) = D_DATE, 'ZX/XX/XX' [LEFT]

		D_LIN2 = "for discounted delivered prices to be valid."
		END
	(3),	BEGIN
		D_DAYS = 30
		CALL MAKE_D_DATE
		D_LIN1 = "Payment must be received in our office by"
		D_LIN1(43,50) = D_DATE, 'ZX/XX/XX' [LEFT]

		D_LIN2 = "for discount to apply."
		END
	(4),	BEGIN
		D_DAYS = 30
		CALL MAKE_D_DATE
		D_LIN1 = "A discount of $"
		D_LIN1(16,23) = DSAMT, 'ZZZX.XX' [LEFT]
		DI = %TRIM(D_LIN1)
		D_LIN1(DI+1, 50) = " and the freight charges"

		D_LIN2 = "may be deducted only if this invoice"
		
		D_LIN3 = "is paid and in our office by"
		D_LIN3(30,37) = D_DATE, 'ZX/XX/XX' [LEFT]
		END
		
	(5),	BEGIN
		D_DAYS = 30
		CALL MAKE_D_DATE
		IF (DSAMT.EQ.0)
	;;;	THEN	D_AMT = (2*OSALE)#2
		THEN	D_AMT = (1*OSALE)#2
		ELSE	D_AMT = DSAMT

		D_LIN1 = "A discount of $"
		D_LIN1(16,23) = D_AMT, 'ZZZX.XX' [LEFT]
		DI = %TRIM(D_LIN1)
		D_LIN1(DI+1, 50) = " may be taken only if"
		
		D_LIN2 = "payment is received and in our office by"
		D_LIN2(42,49) = D_DATE, 'ZX/XX/XX' [LEFT]
		END
	ENDUSING

		XCALL OUTPT (21,28,1,D_LIN1(1,%TRIM(D_LIN1)),1)
		XCALL OUTPT (22,28,1,D_LIN2(1,%TRIM(D_LIN2)),1)
		XCALL OUTPT (23,28,1,D_LIN3(1,%TRIM(D_LIN3)),1)
	RETURN
;-------------------------------------------------------
MAKE_D_DATE,
	D_TRY=0
	WRK_DATE = TODAY
	DD = DD + D_DAYS
D_FEB,
	IF (DD .GT. MDAYS(MM))
	   BEGIN
	   DD = DD - MDAYS(MM)
	   MM = MM + 1
	   IF (MM .GT. 12)
	      BEGIN
	      YY = YY + 1
	      MM = 1
	      END
	   END
	INCR D_TRY
	IF(D_TRY.LE.1)GOTO D_FEB
	D_DATE = WRK_DATE
	RETURN
;-------------------------------------------------------

GETACT,
	COL = 40
	SRCHFL = 1
	CALL SERCH
;;;	IF (MANTAX.EQ.'N') RETURN  ;NO OVERRIDE OF ACCT # IF TAX FROM TAX FILE
	SRCHFL =
	COL = 39
	CALL ACCEPT
	IF (TCHAR.EQ.13) RETURN
GETAC1,
	COL = 40
	XCALL OUTPT (ROW,40,1,'\',1)
	CALL ENTACT
	RETURN
ENTACT,
	XCALL OUTPT (ROW,COL,1,'\',1)
	CTL (7,14) = '04,03,# '
	CALL INPUT
	IF (INXCTL.EQ.1) RETURN
	DEFACT (1,4) = ENTRY (1,4)
	COL2 = COL+4
	XCALL OUTPT (ROW,COL2,0,'-',1)
	CTL (7,11) = '03,00'
	COL = COL + 5
	CALL INPUT
	COL = COL - 5
	IF (INXCTL.EQ.1) RETURN
	DEFACT (5,7) = ENTRY (1,3)
	CALL SERCH
	IF (SRCCTL.EQ.0) RETURN
	GO TO ENTACT
ACCEPT,
	XCALL FLAGS (00010000,1)		;DISABLE CHARACTER ECHOING
	XCALL OUTPT (ROW,COL,0,'\',1)
	ACCEPT (15,TCHAR)
	IF (SYSTEM.NE.1.AND.TCHAR.EQ.13) ACCEPT (15,DCHAR)
	XCALL FLAGS (00010000,0)		;RE-ENABLE CHARACTER ECHOING
	RETURN
ACCEP2,
	XCALL FLAGS (00010000,1)		;DISABLE CHARACTER ECHOING
	XCALL OUTPT (ROW,COL,0,'\',1)
	ACCEPT (15,TCHAR)
	IF (SYSTEM.NE.1.AND.TCHAR.EQ.13) ACCEPT (15,DCHAR)
	XCALL FLAGS (00010000,0)		;RE-ENABLE CHARACTER ECHOING
	RETURN
CLR45,
	XCALL OUTPT (9,22,1,'\',V)
	XCALL OUTPT (10,22,1,'\',V)
	RETURN
CNGBR3,
	GO TO (FRGHT,MISC,OTAX1,OTAX2,OTAX3,DSCODE,DSAMT), WHATNO
	XCALL MESAG (' ',6)
	GOTO ANYCN3
PRCES3,
	CLEAR USRSEL
	SB_USR = %GU_NAME
;;;	SB_SEQ = %TIME
	SB_SEQ = ocusno		;4-12-10
	SB_ORD = OORDNO
	LOKCTL = 1
	XCALL ISIO (CHN065, USRSEL, SB_KEY, STORE, LOKCTL)

	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,WRITE,LOKCTL)
	RETURN
SERCH,
	ALPHA (1,8) = DEFACT, MASK2
	LOKCTL = 1
	XCALL ISIO (7, ARACCT, DEFACT, READ, LOKCTL)
	IF (LOKCTL .EQ. 0)
	  THEN SRCCTL = 0
	  ELSE SRCCTL = 1
	    
;	KEY = DEFACT, MASK1
;	ALPHA (1,8) = DEFACT, MASK2
;	XCALL SERCH (7,ARACCT,KEY,1,7,BSEND,BSMID,SRCCTL,4,32,37,0,0,0,0)
	IF (SRCCTL.EQ.1) CALL BADACT
	IF (SRCCTL.EQ.0) CALL DSPACT
	RETURN
ANYCN3,
	XCALL ANYCN (CNGCTL,WHATNO)
	GOTO (PRCES3,CNGBR3,ORDMSC), CNGCTL+1
BADACT,
	IF (SRCHFL.EQ.1) ARACDS = 'ACCOUNT NOT ON FILE'
	IF (SRCHFL.EQ.0) XCALL MESAG('ACCOUNT NOT ON FILE',1)
	IF (SRCHFL.EQ.1) SRCCTL = 0
	RETURN
DSPACT,
	XCALL OUTPT (ROW,40,0,ALPHA(1,8),1)
	XCALL OUTPT (ROW,50,0,ARACDS,1)
	RETURN
DSPNUM,
	OPTION = 1
	GOTO CALDSP
DSPDTE,
	OPTION = 2
	GOTO CALDSP
DSPDLR,
	OPTION = 3
CALDSP,
	XCALL DSPLY(MAX,ROW,COL,DECMAL,OPTION,1)
	RETURN

CHECK_ST_TAX,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; CHANGE THE SHIP-TO TAX CODE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	SHCSNO = OCUSNO
	SHTONO = OSHPTO
	XCALL ISIO (CHN171, SHIPTO, SHTKEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) RETURN
;
	IF (OTAXFL .EQ. SHTOTC) RETURN

	SWITCH = 5
	XCALL FILES (73, 'SU', 171, SWITCH)
	IF (SWITCH .EQ. 9) RETURN

	XCALL ISIO (73, SHIPTO, SHTKEY, READ, LOKCTL)
	IF (LOKCTL .EQ. 0)
		BEGIN
		SHTOTC = OTAXFL
		XCALL ISIO (73, SHIPTO, SHTKEY, WRITE, LOKCTL)
		END

	CLOSE 73
	RETURN
;---------------------------------------------------------

COMP_TAX,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;SSQ 2-20-98 WISC FREIGHT IS TAXABLE...
;;; SSQ 4-28-99 NE FREIGHT IS TAXABLE...
;;; ssq 1-09-07 SD freight is taxable...

	USING OTAXFL(1,1) SELECT
	('W'),	TAXFRT = OFRGHT			;WISC
	('M'),	TAXFRT = OFRGHT			;MINN
	('N'),	TAXFRT = OFRGHT			;NEB
	('S'),	TAXFRT = OFRGHT			;SD
	(),	TAXFRT =
	ENDUSING

	IF (OTAXFL(1,2) .EQ. 'MN') TAXFRT = OFRGHT	;SSQ 1-9-02
	IF (OTAXFL(1,2) .EQ. 'MX') TAXFRT = OFRGHT	;SSQ 1-9-02

;;;		IF (OTAXFL(1,1) .EQ. 'W')
;;;		THEN	TAXFRT = OFRGHT
;;;		ELSE	TAXFRT =
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CTL = '14,22'
	OTAX(1) = ((TAXFRT + MSCTAX + TAXTOT) * ARTPRT(1) / 100) # 3
	XCALL DSPLY (MAX,14,COL,OTAX(1),3,1)	;

	CTL = '15,22'
	OTAX(2) = ((TAXFRT + MSCTAX + TAXTOT) * ARTPRT(2) / 100) # 3
	XCALL DSPLY (MAX,15,COL,OTAX(2),3,1)	;

	CTL = '16,22'
	OTAX(3) = ((TAXFRT + MSCTAX + TAXTOT) * ARTPRT(3) / 100) # 3
	XCALL DSPLY (MAX,16,COL,OTAX(3),3,1)	;

	TAX = OTAX(1) + OTAX(2) + OTAX(3)
	XCALL DSPLY (MAX,18,COL,TAX,3,1)	;Display default tax amount
	RETURN
;-----------------------------------------------------
BIL_IDX,
	XCALL FFILE (44, REDFIL, CLCTL)		;OPEN ON SAME DEVICE AS ORDHDR
	IF (CLCTL) RETURN
	REDFIL(5,14) = 'BILEDT.WRK'
	GOTO MAKIDX

INV_IDX,
	XCALL FFILE (44, REDFIL, CLCTL)		;OPEN ON SAME DEVICE AS ORDHDR
	IF (CLCTL) RETURN
	REDFIL(5,14) = 'INVOIC.WRK'
	GOTO MAKIDX
MAKIDX,
	XCALL OUTPT (2,1,2,'BUILD NEW INDEX...',1)
	OPEN (8, O, REDFIL)

	SWITCH = 5
	XCALL FILES (4,'SI',44,SWITCH)

	ORDHDR =
	LOKCTL = 1
	XCALL IOS (8,ORDHDR,WRITE,LOKCTL)
RDLOOP,
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.NE.0) GOTO DONE_IDX

	IF (OCUSNM(1,9).EQ.']]]CANCEL')GOTO RDLOOP
;;;	IF (OFLAG.NE.1) GOTO RDLOOP
	IF (OFLAG.EQ.0) GOTO RDLOOP
	LOKCTL = 1
	XCALL IOS (8,ORDHDR,WRITE,LOKCTL)
	GOTO RDLOOP
DONE_IDX,
	ORDHDR = ']]]]]]]]]]]]]]]]]]]]]]]]]'
	LOKCTL = 1
	XCALL IOS (8,ORDHDR,WRITE,LOKCTL)
	CLOSE 8
	XCALL FILES (4,'I',44,4)

	SORT (IN=REDFIL,
&		RECORD=ORDHDR,
&		KEY = (OCUSNO,OORDNO)
&		)
	RETURN
;-----------------------------------------------

END
