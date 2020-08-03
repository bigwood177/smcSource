;ORDHL1.CP - MENU VERSION OF ORDHLD.CP
;ORDHLD.CP
;6-20-17 itmmas needs to be open on ch 1
;7-30-18 ssq: added ch_ans to indicate user selected "blues only" when printing original order.


;	MAINTAIN CREDIT HOLD LIST
;

	.include 'def:HPSUB.DEF'

GLOBAL DUCK,	INIT
	.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL


RECORD	CRHOLD
	.INCLUDE 'DEF:RD195A.DEF'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	SNMSG
		,A2,	'CO'	;ID'S THIS PGM
	S_ORD	,D6

RECORD	CHANNEL
	CHN041	,D2
	CHN044	,D2
	CHN195	,D2
	CHN182	,D2
	chn060	,d2
	chn135	,d3		;9-23-15

RECORD	VARS
	cmpcod	,a3
	OPNOK	,D1
	local	,d1
	HAS_I	,D1	;1 = l/i w/ dept "I" is on order
	BATFIL	,A10
	numlbl	,d4
	XORD	,D6
	SNAM	,A12
	LNAM	,A25
	SINT	,A3
	XDATE	,D8
	ENTRY	,A30
	INXCTL	,D1
	WHATNO	,D2
	CNGCTL	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	SWITCH	,D1
	V	,D1
;
PROC
	XCALL TERID(V)
	XCALL OUTPT (1,1,2,'CREDIT HOLD',1)
	CALL OPENS
	IF(.NOT. OPNOK) GOTO ENDOFF
	xcall who(cmpcod)

DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'CREDIT HOLD',1)
	XCALL OUTPT (4,4,0,'ORDER #',1)
	XCALL OUTPT (4,36,0,'DATE:',1)
	XCALL OUTPT (6,5,0,'SREP:',1)
ORDNO,
	XCALL INPUT (4,13,06,00,'#E',ENTRY,INXCTL,1)
	GOTO(DISPLA,ENDOFF),INXCTL
	XORD = ENTRY(1,6)
	IF (XORD .LE. 0) GOTO ORDNO
	XCALL ISIO(CHN044,ORDHDR,XORD,READ,LOKCTL)
	IF(LOKCTL .NE. 0)
		BEGIN
		XCALL MESAG ('ORDER NOT ON FILE',1)
		GOTO ORDNO
		END

	XDATE(1,4) = OORDDT(5,8)
	XDATE(5,8) = OORDDT(1,4)
	ENTRY(1,10) = XDATE,	'XX/XX/XXXX'
	XCALL OUTPT (4,42,0,ENTRY(1,10),1)

	ENTRY(1,2) = OSLMAN,	'ZX' [LEFT]
	XCALL OUTPT(6,12,0,ENTRY(1,2),1)
	XCALL SREP(OSLMAN,LNAM,SNAM,SINT)
	XCALL OUTPT (6,15,0,SNAM,1)

	ENTRY(1,6) = OCUSNO,'ZZZZZX' [LEFT]
	XCALL OUTPT (8,4,0,ENTRY(1,6),1)
	XCALL OUTPT (8,12,0,OCUSNM,1)
	XCALL OUTPT (9,12,0,OSHPNM,1)
	XCALL OUTPT (10,12,0,OSHAD1,1)
	XCALL OUTPT (11,12,0,OSHAD2,1)
	XCALL OUTPT (12,12,0,OSHAD3,1)

	FIND (CHN195,CRHOLD,XORD) [ERR=NEW_OH]

	READS(CHN195,CRHOLD,ENDOFF)
	IF (CH_ORD .NE. XORD)
		BEGIN
		XCALL MESAG ('ORDER # MISSMATCH',1)
		GOTO DISPLA
		END
	XCALL OUTPT (24,1,1,'REMOVE THIS ORDER FROM CREDIT HOLD LIST?',1)
	XCALL INPUT (24,48,01,01,'YN',ENTRY,INXCTL,1)
	GOTO (REMOVE,DISPLA),INXCTL
REMOVE,
	XCALL ISIO (CHN195,CRHOLD,CH_ORD,DELETE,LOKCTL)
	IF (LOKCTL .NE. 0) XCALL MESAG ('ERROR DELETING RECORD',1)


;---------------------------------------------
	using ch_flag select
	('1'),	nop			;everthing already printed
	('2'),	xcall pdord (ch_ord)	;just print the blue's...
	('3','4'),	begin		;4-1-15 added '4'

			XCALL PDORD (ch_ord)
			if (ch_ans.ne.'B')		;B = blues only (7-30-18)
				begin
				XCALL FIND_I (ch_ord, HAS_I)
				if (has_i)
		  	  	  begin
			   	  USING OSCAC SELECT
			   	  ('0','4'),	NUMLBL = 0	;SPEE-DEE, UPS
			   	  (),		NUMLBL = 4	;
			   	  ENDUSING

			   	  using cmpcod select			;ssq 5-17-17 roc prints to different que...
			   	  ('ROC'),	BATFIL = 'BROC.BAT'
		  	   	  (),		BATFIL = 'UPSFIL.BAT'	;10-03-06: UPS
			   	  endusing

		 	   	  XCALL LBLSH (ch_ord, CHN182, NUMLBL, BATFIL)	;ssq 7/11/06
		  	   	  end
				XCALL PRDUC (ch_ord, OCUSNM)	;;here 8-31-09
				XCALL PRWOD (ch_ord ,local)	;4-18-18
				XCALL PRWO3 (ch_ord)		;ROCKFORD .XX LINES
				XCALL PRWO2 (ch_ord ,local)
				end
			xcall mesag ('Order Released',1)
			end
	endusing

;---------------------------------------------

;;;	CALL CLOSE

	GOTO DISPLA
;---------------------------------------------

NEW_OH,
	XCALL OUTPT (24,1,1,'PLACE THIS ORDER ON CREDIT HOLD?',1)
	XCALL INPUT (24,38,01,01,'YN',ENTRY,INXCTL,1)
	GOTO (ADD,DISPLA),INXCTL
ADD,
	CLEAR CRHOLD
	CH_ORD = XORD
	CH_FLAG = 'H'	;SSQ 12-15-05

	XCALL ISIO (CHN195,CRHOLD,CH_ORD,STORE,LOKCTL)
	IF (LOKCTL.NE.0) XCALL MESAG ('ERROR STORING RECORD',1)
	GOTO DISPLA
;---------------------------------------------

ENDOFF,
	CALL CLOSE
	XCALL PGCHN('AR:CUSMNU',1)
	STOP


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1,'SI',041,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 1

	SWITCH = 5
	XCALL FILES (4,'SU',044,SWITCH)
	IF(SWITCH.EQ.9) RETURN
	CHN044 = 4

	XCALL FILES (5,'SU',195,SWITCH)
	IF(SWITCH.EQ.9) RETURN
	CHN195 = 5

	SWITCH = 5
	XCALL FILES (17, 'SI', 182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 17

	switch = 5
	xcall files (3, 'I', 060, switch)
	if (switch .eq. 9) return
	chn060 = 3


	switch = 5
	xcall files (135, 'SI', 135, switch)
	if (switch .eq. 9) return
	chn135 = 135

	OPNOK = 1
	RETURN
;-----------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF(CHN041) CLOSE CHN041
	IF(CHN044) CLOSE CHN044
	IF(CHN195) CLOSE CHN195
	IF(CHN182) CLOSE CHN182
	if(chn060) close chn060
	if (chn135) close chn135

	RETURN
;-----------------------------------


