; POSTAR / COP 
;
;
;		CONVERTS ORDHDR FILE TO SALES FILE (USED IN A/R SYSTEM)
;
;	01-05-00 SSQ:	MADE "CLROF2" INTERNAL ROUTINE.
;	05-18-01 ssq:	PRDACT IS NOW ISAM.
;	07-29-02 SSQ:	UPDATE FRTCLM.ISM FILE.
;	03-20-08 ssq:	39 dist
;	10-31-11 ssq: 	LPCD discount
; 	05-30-18 ssq: make cusmas isam
;	11-11-19 ssq: remove ocomdu (changed to omicus)
;	04-20-20 ssq: spiral body tees  (late addition)

EXTERNAL FUNCTION
	FUNCV	,A		;FUNCTION RETURNS VIRTUAL ITEM NUMBERS FOR SPIRAL BODY TEES

record	fv_data
	f_item	,a15	;virtual item to return
	f_f3	,5d1	;
	f_cfg	,d1	;same as cfg_item in scrnx
	f_ga	,d2	;

RECORD SALES		
		.INCLUDE 'DEF:RD004A.DEF'
RECORD ,X		
		.INCLUDE 'DEF:RD004B.DEF'

RECORD	FRTCLM
		.INCLUDE 'DEF:RD023A.DEF'
RECORD	ITMMAS
		.INCLUDE 'DEF:RD041A.DEF'
		
RECORD PRDACT		
		.INCLUDE 'DEF:RD069A.DEF'
RECORD ORDHDR   	
		.INCLUDE 'DEF:RD044A.DEF'
RECORD ORDLIN  		
		.INCLUDE 'DEF:RD045A.DEF'
RECORD	,X		
		.INCLUDE 'DEF:RD045D.DEF'
RECORD DUCACC
		.INCLUDE 'DEF:RD175A.DEF'
RECORD CUSMAS		
		.INCLUDE 'DEF:RD001A.DEF'
RECORD	,X		
		.INCLUDE 'DEF:RD001B.DEF'
;;;RECORD CUSIDX		
;;;		.INCLUDE 'DEF:RD002A.DEF'
RECORD ARTERM
		.INCLUDE 'DEF:RD170A.DEF'
RECORD ARTCTL
		.INCLUDE 'DEF:RD170B.DEF'

RECORD RCVMSG
	FILNUM	,D3	; NUMBER OF FILE TO BE CLEARED OFF, ON DEVICE FILE
		,A1
	PGM1	,A10	; PROGRAM TO RETURN CONTROL TO
RECORD REDFIL
		,A14


RECORD	NEWVARS
	CHN041	,D2
	CHN182	,D2
	sav_lflag	,d1
	sav_ltxflg	,d1
	fl	,d6
	ln	,d6
	j	,d6
;;;	f2_idx	,d6
	blanks	,a30
	partno	,a15
	sbt		,d1	;1=item is spiral body tee
	sbt_price	,d6
	sbt_qty		,d6
	sbt_mat		,d1
	sbt_item	,a15
	sbt_idx		,d6
	sbt_code	,6a3,	'R  ','MN ','C1 ','C2 ','BR1','BR2'
	sbt_lmsq1	,d6

RECORD	F_MEMOS
	.INCLUDE 'DEF:CPMEMO.DEF'

RECORD
	SEGS	,A12			;first 2 dig are gauge
RECORD,X
	GA	,D2
	SARA	,5D2
RECORD,X
		,D2	;GA
	MA	,D2
	MB	,D2
	BC	,D2
	BD	,D2

RECORD	VARS2
	ENTRY	,A6
	INXCTL	,D1

RECORD	VARS
	DISC	,D12	;amound of LPCD line item
	TOT	,D12	;osale + disc (order total before discount)
	PCT	,D12	;disc/tot
	DIST	,D12	;total of all line item discounts
	XDISC	,D12	;

	CMPCOD	,A3
	ALPHA	,A5
	CLRAR4	,A13,'004,CP:POSTAR'
	CLRCP2	,A13,'042,CP:POSTAR'
	CLRCP9	,A13,'069,CP:POSTAR'
	CLRCP0	,A13,'170,CP:POSTAR'
	KEY	,A6
	KEY2	,A2
	MASK	,A6,'XXXXXX'
	BSEND	,D5
	BSMID	,D5
	BSMD2	,D5
	SRCCTL	,D1
	FACTOR	,D2
	SLSMAX	,D5
	SLSCNT	,D5
	CUSORG	,D5
	LINORG	,D5
	PDAORG	,D5
	SWITCH	,D1
	V	,D1
	PSTCNT	,D5	,00000
	DECMAL	,D10
	RUNTOT	,D10
	NEXT	,D2
	NINES	,D7,9999999
	ACTARY	,39D7
	AMTARY	,39D8
	N	,D2
	READ	,D1	,0
	WRITE	,D1	,1
	LOKCTL	,D1
	DUCTOT	,D8
	DUCT	,D1
	I	,D2
	EXEPRC	,D8
PROC
	XCALL TERID (V)
	XCALL MOUNT ('REGULAR PAPER','PRINTER')
	XCALL WHO(CMPCOD)
RE_START,
	XCALL OUTPT (1,1,2,'POST ORDERS TO ACCOUNTS RECEIVABLE',V)
	XCALL WATE (4,V)
	SWITCH = 2
	XCALL FILES (13,'U',04,SWITCH)		;FILE # 04 -- SALES FILE
	IF (SWITCH.EQ.9) GO TO INUSE1
	XCALL FILES (4,'SI',44,5)		;FILE # 44 -- ORDHDR
	XCALL FILES (41,'SI',41,5)		;FILE # 41 -- ITMMAS
	CHN041 = 41
	
	XCALL FILES (17,'SI',182,5)
	CHN182 = 17
CHKREC,
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GO TO EOFCHK
	IF (OFLAG.NE.2) GOTO CHKREC
	INCR PSTCNT
	GOTO CHKREC
EOFCHK,
	IF (PSTCNT.EQ.0) GO TO NOPOST
	LOKCTL = 1
	XCALL IO (13,SALES,1,READ,LOKCTL)
	SLSCNT = REC004
	SLSMAX = MAX004
	IF (SLSCNT+PSTCNT.GE.SLSMAX) GOTO SLSFUL
	SWITCH = 1
	XCALL FILES (7,'SI',69,SWITCH)		;FILE # 69 - PRDACT FILE
	IF (SWITCH.EQ.9) GO TO INUSE2
	SWITCH = 1

;;;	XCALL FILES (1,'I',42,SWITCH)		;FILE # 42 - ITMIDX FILE
;;;	IF (SWITCH.EQ.9) GO TO INUSE3
	SWITCH = 1
	XCALL FILES (8,'I',170,SWITCH)
	IF (SWITCH.EQ.9) GO TO INUSE4
	CLOSE 1
;;;	XCALL FILES (2,'I',02,5)
	XCALL FILES (1,'SI',01,5)
	XCALL FILES (5,'SI',45,5)
	XCALL FILES (6,'SI',175,5)

	XCALL FFILE (23,REDFIL,SWITCH)
	OPEN(19,O,REDFIL)		;WORK FILE - FRTCLM.SMC

	LOKCTL = 1
	XCALL IO (8,ARTCTL,1,READ,LOKCTL)
	
;;;	LOKCTL = 1
;;;	XCALL IO (1,CUSMAS,1,READ,LOKCTL)
;;;	CUSORG = ORG001

;;;	LOKCTL = 1
;;;	XCALL IO (7,PRDACT,1,READ,LOKCTL)
;;;	PDAORG = ORG069

	OORDNO = 0
	LOKCTL = 1
	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)
	GO TO RDHDR1
RDHDR,
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GO TO EOFHDR
RDHDR1,
	IF (OFLAG.NE.2) GO TO RDHDR
	SALES =
	SDOCNO = OINVNO
	SDOCTP = 1
	IF (OCCRD.EQ.'C') SDOCTP = 6	;SSQ 8/19/02 CREDIT CARD
	SDOCDT = OINVDT
	SCUSNO = OCUSNO
	SMICUS = OMICUS		;CASH CUST# 12-16-19
	SNAME = OCUSNM
	SSLMAN = OSLMAN
	SSLAMT = OSALE
	SARACT = OARACT
	SMISC = OMISC
	SMSACT = OMSACT
	STAX(1) = OTAX(1)
	STAX(2) = OTAX(2)
	STAX(3) = OTAX(3)
	STXACT(1) = OTXACT(1)
	STXACT(2) = OTXACT(2)
	STXACT(3) = OTXACT(3)
	SFRGHT = OFRGHT
	SFRACT = OFRACT

;;;	SDISAL = ODSCAL
	SDISAL = 		;3-21-2011

	SCOST = OCOST
	STAXFL = OTAXFL
	SAPLNO = OINVNO

;-------------------------------------------------------------------------
;; Removed 3-5-99 for y2k conversion,  SDOCDU not used, may
;;  re-install later...
	SRCCTL = 2
	BSEND = ORG170
	XCALL SERCH (8,ARTERM,OTERMS,1,1,BSEND,BSMID,SRCCTL,1,2,7,0,0,0,0)
	GO TO (BADTRM), SRCCTL

;;;	SDOCDU = SDOCDT		;ssq 1-6-00
;;;	XCALL ADDTE(SDOCDT,ARTRDD,SDOCDU)
;;;	XCALL ADDT8(SDOCDT,ARTRDD,SDOCDU)
	GO TO CONTIN
;-------------------------------------------------------------------------

BADTRM,
;;;	SDOCDU = SDOCDT
CONTIN,

	XCALL ISIO (1, CUSMAS, OCUSNO, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO NOAPL
	
;;;	BSEND = CUSORG
;;;	KEY = OCUSNO,'XXXXXX'
;;;	XCALL SERCH (2,CUSIDX,KEY,1,6,BSEND,BSMID,SRCCTL,4,7,11,0,0,0,0)
;;;	GO TO (NOAPL), SRCCTL
;;;	LOKCTL = 1
;;;	XCALL IO (1,CUSMAS,IRC001,READ,LOKCTL)

	IF (BALMTH.EQ.'B') SAPLNO = -1
NOAPL,
	scomm =			;11-11-19

;;;	SCOMM = ((-1)*OCOMDU*OSALE)#4
;;;	IF (OCOMDU.GT.0) SCOMM = OCOMDU
	RUNTOT =
	for i from 1 thru 39
		begin
		actary(i) =
		amtary(i) =
		end

;;;	ACTARY(1,70) =
;;;	AMTARY(1,80) =

	CALL GTLINS
	INCR SLSCNT
	IF (SLSCNT.GT.SLSMAX) GO TO SLSFUL
;;;
;	SPECIAL LOGIC TO REVERSE NEGATIVE INVOICES TO BECOME CREDITS
;;;
	IF ( ( OSALE + OMISC + OFRGHT + OTAX(1) + OTAX(2) + OTAX(3) ) .LT. 0 )
	BEGIN
	  IF (OSALE.NE.0) SSLAMT = OSALE * (-1)
	  IF (OMISC.NE.0) SMISC = OMISC * (-1)
	  IF (OFRGHT.NE.0) SFRGHT = OFRGHT * (-1)
	  IF (OTAX(1).NE.0) STAX(1) = OTAX(1) * (-1)	;;;
	  IF (OTAX(2).NE.0) STAX(2) = OTAX(2) * (-1)	;;;
	  IF (OTAX(3).NE.0) STAX(3) = OTAX(3) * (-1)	;;;
	  FOR I FROM 1 THRU 38 IF (SDAMTS(I).NE.0) SDAMTS(I) = SDAMTS(I) * (-1)
	  SDOCTP = 3
;;; 8-29-11 per diana>>>	  SAPLNO =
	END
;;;
	USING CMPCOD SELECT
	('ROC'),	SCMPNY = 'R'
	('CAT'),	SCMPNY = 'C'	;SSQ 8-24-06
	(),		CLEAR SCMPNY
	ENDUSING

	LOKCTL = 1
	XCALL IO (13,SALES,SLSCNT,WRITE,LOKCTL)

	IF (OCUSNO .EQ. 999000) CALL FRTCLM	; update the freight claim record...

	GO TO RDHDR
SLSFUL,
	XCALL OUTPT (2,1,2,'\',1)
	XCALL OUTPT (5,20,0,'THE SALES TRANSACTION FILE IN THE',V)
	XCALL OUTPT (6,20,0,'ACCOUNTS RECEIVABLE SYSTEM DOES',V)
	XCALL OUTPT (7,20,0,'NOT HAVE ENOUGH ROOM TO HOLD NEWLY',V)
	XCALL OUTPT (8,20,0,'INVOICED ORDERS - SEE INSTRUCTIONS',V)
	XCALL OUTPT (10,20,0,'AVAILABLE:          NEEDED:',V)
	ALPHA = SLSMAX - SLSCNT, 'ZZZZX'
	XCALL OUTPT (10,31,0,ALPHA,V)
	ALPHA = SLSCNT + PSTCNT, 'ZZZZX'
	XCALL OUTPT (10,48,0,ALPHA,V)
	XCALL MESAG(' ',2)
	STOP
GTLINS,
	LORDNO = OORDNO
	LINSEQ =
	LOKCTL = 1
	XCALL ISIO (5,ORDLIN,ORDKEY,READ,LOKCTL)
	IF (LOKCTL.EQ.3) GOTO ENDLIN
	IF (LORDNO.NE.OORDNO) GOTO ENDLIN
	GO TO RDLIN1
RDLINS,
	LOKCTL = 1
	XCALL IOS (5,ORDLIN,READ,LOKCTL)
	IF (LOKCTL.EQ.2) GO TO ENDLIN
	IF (LORDNO.NE.OORDNO) GO TO ENDLIN
RDLIN1,
	IF (LTYPE.EQ.'M') GOTO RDLINS	;MEMO LINE
	IF (LFLAG.NE.2) GOTO RDLINS
	
; sbt ---------------------------------------------------------------------------
	call check_sb_tees		;10-15-17
	if (.not. sbt) goto no_sbt
	f2_idx = 0
	for sbt_idx from 1 thru 6
		begin
		fv_data = funcv(sbt_item, sbt_code(sbt_idx), lf3)
		if (f_item .ne. blanks)
			begin
		;;;	call insert_f3
			xcall g_item (f_item, itmmas, ordlin, f_memos, sbt_mat, sbt_lmsq1, f2_idx, f3_idx, f_ga, sbt_qty,chn041,chn182)
			sbt_price = sbt_price + lprice
			if (sbt_code(sbt_idx).eq.'C1 ') lqtyor = lqtyor*2
			lqtysh = lqtyor
			call load_line
			end
		end
	goto rdlins

no_sbt,
	call load_line
	goto rdlins
;-------------------------------------------------------------------
load_line,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	DECMAL = ((LPRICE*LQTYSH) - ((LDISC*LPRICE*LQTYSH)#2))#1
	EXEPRC = DECMAL - ((DECMAL*LODISC)#2)
	RUNTOT = RUNTOT + EXEPRC

	IF (LITMNO .EQ. 'LPCD')		;10-31-11 SSQ
		BEGIN
		DISC = EXEPRC
		return
	;;;	GOTO RDLINS
		END

	KEY2 = LPRDCD
DUCEN1,
	XCALL ISIO (7, PRDACT, KEY2, READ, LOKCTL)
	IF(LOKCTL) PACTNO = NINES

	IF (PACTNO.EQ.0) PACTNO = NINES
	IF (PACTNO.NE.NINES) GO TO SETLIN
	ACTARY (39) = NINES
	AMTARY (39) = AMTARY (39) + DECMAL
	IF (DUCT) RETURN
	return

;;;	GO TO RDLINS
GTDUCT,
	DUCTOT = 
	DUCTOR = OORDNO
	LOKCTL = 1
	XCALL ISIO (6,DUCACC,DUCTOR,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.OORDNO) GOTO DUCEND
	GOTO ADDDUC
NXTDUC,
	LOKCTL = 1
	XCALL IOS (6,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.OORDNO) GOTO DUCEND
ADDDUC,
	IF (FLG175.NE.2) GOTO NXTDUC
	IF (ACC.EQ.1)
&	DUCTOT = DUCTOT + 
&		 ( ( SLIPS  * SLPPRC ) # 1 ) +
&		 ( ( DRIVES * DRVPRC ) # 1 ) +
&		 ( ( TCORN  * CORPRC ) # 1 ) +
&		 ( ( TNBQTY * TNBPRC ) # 1 ) +
&		 ( ( TGAQTY * TGAPRC ) # 1 ) +
&		 ( ( TCLQTY * TCLPRC ) # 1 ) +
&		 ( ( TBAQTY * TBAPRC ) # 1 ) +
&		 ( ( TBNQTY * TBNPRC ) # 1 )

	DUCTOT = DUCTOT + ( ( POUNDS * GPRICE ) # 1 )
	DUCTOT = DUCTOT + ( ( LINPRC * SQFLIN ) # 1 )
	GOTO NXTDUC
DUCEND,
	IF (DUCTOT.EQ.0) RETURN
	DECMAL = DUCTOT
	DUCTOT =
	RUNTOT = RUNTOT + DECMAL
;;;	KEY2 = '11'			;hardcoded category 11 for duct
	KEY2 = 'O'			;hardcoded category 11 for duct
	DUCT = 1
	CALL DUCEN1
	DUCT = 0
	RETURN
SETLIN,
	N =
MATCH,
	INCR N
	IF (N.GT.38) GO TO ENDARY
	IF (ACTARY(N).EQ.0) GO TO ENDARY
	IF (PACTNO.EQ.ACTARY(N)) GO TO ENDARY
	GO TO MATCH
ENDARY,
	IF (N.LT.39) ACTARY (N) = PACTNO
	AMTARY (N) = AMTARY (N) + DECMAL
	IF (DUCT) RETURN
	return				;4-27-20
;;;	GO TO RDLINS
ENDLIN,
	CALL GTDUCT

	CLEAR DIST
	TOT = RUNTOT - DISC		;remember disc is neg
	if (tot .ne. 0)
	then	PCT = (DISC*100000/TOT)#1	;(X.XX-)
	else	pct = 0

	NEXT = 1
	N =
STORE,
	INCR N
	IF (N.GT.38) GOTO CHKTOT
	IF (AMTARY(N).EQ.0) GO TO STORE

	XDISC = (AMTARY(N)*PCT)#4
	DIST = DIST + XDISC

	SDACTS (NEXT) = ACTARY (N)
	SDAMTS (NEXT) = AMTARY (N) + XDISC
;;;	SDAMTS (NEXT) = AMTARY (N)

	INCR NEXT
	GO TO STORE
CHKTOT,
	IF (AMTARY(39).EQ.0) GO TO CHK3
	IF (NEXT.EQ.39) GO TO CHK2
	SDACTS (NEXT) = NINES
	SDAMTS (NEXT) = AMTARY (39)
	GO TO CHK3
CHK2,
	SDACTS (38) = NINES
	SDAMTS (38) = SDAMTS (38) + AMTARY (39)
CHK3,
	clear decmal
	for i from 1 thru 39 decmal = decmal + sdamts(i)

;;;	DECMAL = SDAMTS(1) + SDAMTS(2) + SDAMTS(3) + SDAMTS(4) + SDAMTS(5)
;;;	DECMAL = DECMAL + SDAMTS(6) + SDAMTS(7) + SDAMTS(8) + SDAMTS(9)

	IF (DECMAL.EQ.RUNTOT) RETURN
	IF (NEXT.GT.1) SDAMTS (NEXT-1) = SDAMTS (NEXT-1) - (DECMAL - RUNTOT)
	RETURN
EOFHDR,
	LOKCTL = 1
	XCALL IO (13,SALES,1,READ,LOKCTL)
	REC004 = SLSCNT
	LOKCTL = 1
	XCALL IO (13,SALES,1,WRITE,LOKCTL)
	XCALL FILES (13,'I',04,4)
	XCALL FILES (7,'SI',69,4)
	XCALL FILES (8,'I',170,4)
	CLOSE 19				;SSQ 7/29/02
	CLOSE CHN041
	CLOSE CHN182
	XCALL PGCHN ('CP:FRTCLM',1)	;SSQ 7/29/02
;;;	XCALL PGCHN ('CP:UPDMIS',1)
INUSE4,
	CLOSE 4
	XCALL FILES (1,'SI',42,4)
	XCALL FILES (7,'SI',69,4)
	XCALL FILES (13,'I',04,4)

	RCVMSG = CLRCP0
;;;	SWITCH = 2
;;;	XCALL SNMSG (CLRCP0,SWITCH)
	GO TO INUSE
INUSE3,
	CLOSE 4
	XCALL FILES (7,'SI',69,4)
	XCALL FILES (13,'I',04,4)

	RCVMSG = CLRCP2
;;;	SWITCH = 2
;;;	XCALL SNMSG (CLRCP2,SWITCH)
	GO TO INUSE
INUSE2,
	CLOSE 4
	XCALL FILES (13,'U',04,4)

	RCVMSG = CLRCP9
;;;	SWITCH = 2
;;;	XCALL SNMSG (CLRCP9,SWITCH)
	GO TO INUSE
INUSE1,
	RCVMSG = CLRAR4
;;;	SWITCH = 2
;;;	XCALL SNMSG (CLRAR4,SWITCH)
INUSE,
	CALL CLROF2
	GOTO RE_START
;;;	XCALL PGCHN ('UT:CLROF2',2)
NOPOST,
	XCALL FILES (10,'I',41,4)
	XCALL FILES (13,'U',04,4)
;;;	XCALL FILES (2,'I',02,4)
	XCALL FILES (4,'U',44,4)
	XCALL FILES (5,'U',45,4)
	XCALL FILES (6,'SI',175,4)
	XCALL FILES (3,'I',60,4)
	CLOSE CHN041
	CLOSE CHN182
	XCALL MESAG ('NO INVOICES FOUND TO POST',2)
	XCALL PGCHN ('CP:BILLS',1)

;-----------------------------------------------------
;	CLROF2 INTERNAL ROUTINE
CLROF2,
	
	XCALL FFILE (FILNUM,REDFIL,SWITCH)
	XCALL OUTPT (2,1,2,PGM1,1)
	XCALL OUTPT (6,9,0,'The next program, ',1)
	XCALL OUTPT (0,0,0,PGM1,1)
	XCALL OUTPT (0,0,0,', requires exclusive use of the',1)
	XCALL OUTPT (8,9,0,REDFIL,1)
	XCALL OUTPT (0,0,0,
&		', which is currently in use by other terminals.',1)
	XCALL OUTPT (10,9,0,
&	  'Please have these other users end off as soon as possible,',1)
	XCALL OUTPT (12,9,0,
&		'or run the CLEAR FILE STATUS FLAGS option on the SYSTEM',1)
	XCALL OUTPT (14,9,0,
&	  'FUNCTIONS menu.  When the status is "cleared", type READY below.',1)
	GO TO ACCEPT
REPEAT,			; "READY" TYPED BUT SOMEONE IS STILL USING FILE
	XCALL OUTPT (6,9,2,'The ',1)
	XCALL OUTPT (0,0,0,REDFIL,1)
	XCALL OUTPT (0,0,0,' file is still in use by one or more other',1)
	XCALL OUTPT (8,9,0,
&		'terminals.  Have these other terminals end off as soon as',1)
	XCALL OUTPT (10,9,0,'possible, then type "READY" below.',1)
	XCALL OUTPT (12,9,0,
&		'(Or, optionally run CLEAR FILE STATUS FLAGS from the ',1)
	XCALL OUTPT (14,9,0,' SYSTEM FUNCTIONS menu).',1)
ACCEPT,
	XCALL INPUT (18,9,5,5,'AR',ENTRY,INXCTL,1)	; ACCEPT "READY"
	GO TO (ACCEPT,GETOUT), INXCTL
	IF (ENTRY(1,5).NE.'READY') GO TO ACCEPT
	SWITCH = 2
	XCALL FILES (1,'I',FILNUM,SWITCH)
	IF (SWITCH.EQ.9) GO TO REPEAT
	SWITCH = 4
	XCALL FILES (1,'I',FILNUM,SWITCH)
	XCALL OUTPT (2,1,1,'\',1)	; CLEAR LINE DISPLAYING PROGRAM NAME

	RETURN
GETOUT,
	XCALL MESAG ('PROGRAM ABORTING...',1)
	XCALL MESAG ('Restart "POSTAR" when problem resolved',1)
	STOP
;-----------------------------------------------------

FRTCLM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; WRITE FREIGHT CLAIM RECORD
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR FRTCLM

	FC_OORDNO = OORDNO
	FC_OINVDT = OINVDT
	FC_OSALE = OSALE
	FC_ORIG = OBIN		;ORIGINAL ORDER #

	XCALL IOS (19,FRTCLM,WRITE,LOKCTL)
	RETURN
;---------------------------------------------

check_sb_tees,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; is this item a spiral body tee?
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	sbt = 0
	using litmno select
	('BN'),				NOP		;BULLNOSE TEE
	('C9','C4','CV9','CV4'),	NOP	;CROSS
	('CT9','CC9','CTV','CCV'),	NOP	;CONICAL TEE/CROSS
	('T4','T9','TV4','TV9'),	NOP	;TEE
	('STC','STT'),			NOP
	(),				RETURN	; anything else
	endusing

	if (lf2 .eq. 31) return		;custom f2 notes 11-26-18

	partno = lcfgim
	if (ldampr) call damper

	xcall cfg2(partno,segs)
	if (ma .gt. 60) return		;1-3-19

	if (ma.gt.36 .and. (bc.ge.20 .or. bd.ge.20) )goto sb_tee
	return
sb_tee,
	sbt_item = lcfgim
	sbt_qty = lqtyor
	sbt_mat = lmat
	sbt_lmsq1 = lmsq1

	sbt = 1
	return
;----------------------------------------------------
damper,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ln = %trim (partno)
	for j from 1 thru 3
		begin
		xcall instr(1, partno, '*', fl)
		if (.not. fl)exitloop
		partno(fl, ln) = partno(fl+1, ln)
		end
	return
;----------------------------------------------------
END
