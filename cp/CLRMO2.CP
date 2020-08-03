;  CLRMO2 / IM 	isam
;
;

RECORD ITMMAS		;
		.INCLUDE 'DEF:RD041A.def'

record	dis
	ii	,d6

RECORD	VARS
	mon	,d2	;month
	date	,d6
	LY	,D12
	DECMAL	,D5
	MTDQTY	,D6
	DEFALT	,A2
	CTR	,D2
	SAVTYP	,D1
	LOCTNS	,D2
	RECCTR	,D5,00002
	ENTRY	,A2
	INXCTL	,D1
	SWITCH	,D1,1
	V	,D1
	READ	,D1	,0
	WRITE	,D1	,1
	LOKCTL	,D1
;
PROC 
	XCALL TERID (V)

	SWITCH = 2
	SWITCH = 5
	XCALL FILES (1,'SU',41,SWITCH)		;FILE # 41 -- ITMMAS FILE
	IF (SWITCH.EQ.9) GO TO END


	XCALL OUTPT (1,1,2,'CLEAR ITEM MONTH-TO-DATE FIELDS ONLY',V)
	XCALL OUTPT (4,8,0,
&	'This program clears the monthly figures accumulated in inventory',V)
	XCALL OUTPT (5,17,0,
&	 'and updates the 12 month rolling average.',V)
;;;&	 'No updating of Item Master fields will be done.',V)
	XCALL OUTPT (7,15,0,
&	'ARE YOU SURE YOU WANT TO CLEAR AND UPDATE ?',V)
;;;&	'ARE YOU SURE YOU WANT TO CLEAR WITHOUT UPDATING ?',V)
	XCALL INPUT (7,66,1,1,'YN',ENTRY,INXCTL,V)
	GO TO (EOF), INXCTL-1
	XCALL WATE (4,V)
;
	xcall rdate (date)
	mon = date(1,2)
	mon = mon -1



	read (1, itmmas, ^first) [err=read1]
READ1,
	incr ii
	if (ii/100*100 .eq. ii) xcall outpt (1,70,1,dis,1)
re_read,
	reads (1, itmmas, eof) [lock=locked]
	goto clear
locked,
	sleep 1
	goto re_read

CLEAR,
;;; 8-19-13: install new formula during first month of 2014...
;;;	REOLVL = (QTYYTD*10/MON)#1		;ROLLING 12 MONTH AVG

; at the end of 2013 set lyravg = qtyytd/12
;					; in march for example, this is
	ly = lyravg * (12-mon)		; 9 months of last years avg
	reolvl = (qtyytd + ly)/12	; plus ytd which is 3 month into this year
;------------------------------------------------------------

;;;	REOLVL = (REOLVL*11 + QTYMTD)/12	;ROLLING 12 MONTH AVG
;;;	USEMTD =
	QTYMTD =
	SLSMTD =
	CSTMTD =
	LOKCTL = 1
	XCALL ISIO (1,ITMMAS,ITEMNO,WRITE,LOKCTL)
	INCR RECCTR
	GO TO READ1

EOF,
END1,
	XCALL FILES (1,'SU',41,4)
END,
	XCALL PGCHN ('CP:SPCFUN',1)

END
