;  PDAMNT / COP 
;	MODIFIED: SSQ 5-18-01:  changed to isam file.
;
;		PRODUCT CATEGORY SALES ACCOUNT FILE MAINTENANCE
;
;	01-16-14: make aracct isam
RECORD PRDACT		
		.INCLUDE 'DEF:RD069A.DEF'
RECORD ARACCT		
		.INCLUDE 'DEF:RD007A.DEF'
;RECORD ARACTL		
;		.INCLUDE 'DEF:RD007B.DEF'
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

RECORD	VARS
	XCAT	,A2
	ALPHA	,A8
	ORGARA	,D5
	ENTRY	,A5
	INXCTL  ,D1
	CNGCTL	,D1
	WHATNO	,D1
	SELECT	,D1
	KEY   	,A2
	KEY2	,A7
	BSEND	,D5
	BSMID	,D5
	SBSMID	,D5
	MAXCNT	,D5
	SRCCTL	,D1
	SWITCH	,D1
	V	,D1
	READ	,D1	,0
	WRITE	,D1	,1
	STORE	,D1	,2
	DELETE	,D1	,3
	LOKCTL	,D1
PROC
	XCALL TERID (V)
	SWITCH = 1
	XCALL FILES (7,'SU',69,SWITCH)		;FILE # 69 -- PRDACT FILE
	IF (SWITCH.EQ.9) GO TO EXIT
	XCALL FILES(2,'SI',07,SWITCH)		;FILE # 07 -- ARACCT FILE
	IF (SWITCH.EQ.9) GO TO ENDOFF
UPDFLG,
;	LOKCTL = 1
;	XCALL IO (2,ARACTL,1,READ,LOKCTL)
;	ORGARA = ORG007
BEGIN,
	XCALL MMENU ('PRODUCT CATEGORY ACCOUNT MAINTENANCE','CATEGORIE',
&			SELECT,V)
	GO TO (DISPLA,DISPLA,DISPLA,LIST,LIST), SELECT
ENDOFF,
	XCALL WATE(3,V)
	CALL CLOSES
EXIT,
	XCALL PGCHN ('CP:CPMENU',1)
LIST,
	CALL CLOSES
	XCALL PGCHN ('CP:PDALST',1)
;;;	IF (SELECT.EQ.4) XCALL PGCHN ('CP:PDALST',1)

CLOSES,
	XCALL FILES (7,'U',69,4)
	XCALL FILES (2,'I',07,4)
	RETURN
DISPLA,
	UNLOCK 7
	CNGCTL =
	PRDACT =
	XCALL OUTPT (3,1,2,'\',1)
	XCALL OUTPT (5,20,0,'1. PRODUCT CATEGORY',V)
	XCALL OUTPT (6,20,0,'2. SALES ACCOUNT #',V)
	IF (SELECT.NE.1) XCALL OUTPT (5,19,0,'*',V)
CATCOD,
	IF (SELECT.NE.1.AND.CNGCTL) GO TO BADCNG
	CTL = '05,41,02,01,AE'
	CALL INPUT
	GO TO (DISPLA,BEGIN), INXCTL
	PRDCAT = ENTRY (1,2)
	IF (PRDCAT.EQ.'  ') GO TO CATCOD
	XCAT = PRDCAT
	XCALL ISIO (7, PRDACT, PRDCAT, READ, LOKCTL)

	GO TO (FNDREC,FNDREC), SELECT-1
	IF(LOKCTL.EQ.0) GOTO BADPRD
	PRDACT = XCAT
	GO TO (ANYCNG), CNGCTL
	GO TO ACCTNO
BADPRD,
	UNLOCK 7
	XCALL MESAG('PRODUCT CATEGORY ALREADY ASSIGNED',1)
	GO TO DISPLA
CLRSUB,
	XCALL OUTPT (6,45,1,'\',V)
	RETURN
ACCTNO,
	CALL CLRSUB
	CTL = '06,41,04,03,#E'
	CALL INPUT
	GO TO (DISPLA,BEGIN), INXCTL
	PACTNO (1,4) = ENTRY (1,4)
	XCALL OUTPT (6,45,0,'-',V)
	CTL = '06,46,03,00,# '
	CALL INPUT
	GO TO (DISPLA), INXCTL
	IF (ENTRY(1,3).EQ.'   ') CALL CLRSUB
	PACTNO (5,7) = ENTRY (1,3)
;	BSEND = ORGARA
;	KEY2 = PACTNO, 'XXXXXXX'
;	XCALL SERCH (2,ARACCT,KEY2,1,7,BSEND,BSMID,SRCCTL,2,32,37,0,0,0,0)
      
	LOKCTL = 1
	XCALL ISIO (2, ARACCT, PACTNO, READ, LOKCTL)
	IF (LOKCTL .EQ. 0)
	  THEN	SRCCTL = 0
	  ELSE	SRCCTL = 1
	    
	GO TO (BADACT), SRCCTL
	PACTDS = ARACDS
	XCALL OUTPT (6,51,0,ARACDS,V)
	GO TO ANYCNG
BADACT,
	XCALL MESAG('ACCOUNT NOT ON FILE',1)
	GO TO ACCTNO
CNGBR,
	GO TO (CATCOD,ACCTNO), WHATNO
BADCNG,
	CNGCTL = 3
	GO TO ANYCNG
PROCES,
	GO TO (ADD,CHANGE), SELECT
FNDREC,
	IF(LOKCTL) GOTO NOFIND

	ALPHA = PACTNO, 'XXXX-XXX'
	XCALL OUTPT (6,41,0,ALPHA,V)
	XCALL OUTPT (6,51,0,PACTDS,V)
	IF (SELECT.NE.3) GO TO ANYCNG
	XCALL OUTPT (12,1,1,'OK TO DELETE ?',V)
	CTL = '12,17,01,00,YN'
	CALL INPUT
	GO TO (DELETE,DISPLA), INXCTL
ADD,
	XCALL ISIO (7, PRDACT, PRDCAT, STORE, LOKCTL)
	IF (LOKCTL)
		BEGIN
		XCALL MESAG('ERROR STORING RECORD',1)
		END

	GO TO DISPLA
CHANGE,
	XCALL ISIO (7, PRDACT, PRDCAT, WRITE, LOKCTL)
	GOTO DISPLA
DELETE,
	XCALL ISIO (7, PRDACT, PRDCAT, DELETE, LOKCTL)
	IF (LOKCTL)
	THEN	XCALL MESAG ('ERROR DELETING RECORD',1)
	ELSE	XCALL MESAG ('RECORD DELETED',2)
	GOTO DISPLA

NOFIND,
	XCALL MESAG ('RECORD NOT FOUND',1)
	GO TO DISPLA
INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,V)
	RETURN
ANYCNG,
	XCALL ANYCN (CNGCTL,WHATNO)
	GO TO (PROCES,CNGBR,DISPLA), CNGCTL+1
END
