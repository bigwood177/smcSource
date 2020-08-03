;  CLRYOE / IM 
;
;

RECORD ITMMAS		;
		.INCLUDE 'DEF:RD041A.def'

record	dis
	ii	,d6

RECORD
	RECCNT	,D5,00002
	ENTRY	,A2
	INXCTL	,D1
	SWITCH	,D1
	V	,D1
	READ	,D1	,0
	WRITE	,D1	,1
	LOKCTL	,D1
;
PROC 
	XCALL TERID (V)
	SWITCH = 2
	switch = 5
	XCALL FILES (1,'SU',41,SWITCH)
	IF (SWITCH.EQ.9) GO TO END

	XCALL OUTPT (1,1,2,'CLEAR ITEM YEAR-TO-DATE FIELDS ONLY',V)
	XCALL OUTPT (6,17,0,'ARE YOU SURE YOU WANT TO CLEAR THESE FIELDS ?',V)
	XCALL OUTPT (10,1,0,
&	'PLEASE NOTE: The MONTH-TO-DATE fields are NOT cleared here.',V)
	XCALL OUTPT (11,14,0,
&	'MONTH-TO-DATE fields MUST also be cleared at this time.',V)
	XCALL INPUT (6,64,1,1,'YN',ENTRY,INXCTL,V)
	GO TO (EOF), INXCTL-1
	XCALL WATE (4,V)

	clear ii
	READ (1, ITMMAS, ^FIRST) [ERR=READ1]
READ1,
	incr ii
	if (ii/500*500 .eq. ii) display(15,$scr_pos(1,70),dis)

re_read,
;;;	LOKCTL = 1
;;;	XCALL IOS (1,ITMMAS,READ,LOKCTL)
;;;	IF (LOKCTL .NE. 0) ;;>GOTO EOF
	reads (1, itmmas, eof) [lock=locked]
	goto clear
locked,
	sleep 1
	goto re_read

clear,
	lyravg = qtyytd/12		;12 month average added here 1/9/17

	QTYYTD =
	USEYTD =
	SLSYTD =
	CSTYTD =
	LOKCTL = 1
	XCALL ISIO (1,ITMMAS,ITEMNO,WRITE,LOKCTL)
	GO TO READ1
EOF,
END1,
	XCALL FILES (1,'SU',41,4)
END,
	XCALL PGCHN ('CP:SPCFUN',1)

END
