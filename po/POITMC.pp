;POITMC / POR - D11
;
;		AUTHOR: 1-DEC-81  WRF
;		MODIFIED: 05-APR-83 PSG POR23-D11D-1
;
;			::CRELPOR.DEF::
;**********************************************************************
;		PURCHASE ORDER & RECEIVING -- Release 1.1
;		RELEASED: 1-MAY-82
;**********************************************************************
;:
;			::PCPYEMS.DEF::
;
;		PROPRIETARY RIGHTS NOTICE:  All rights reserved.  This
;		material contains the valuable properties and trade secrets
;		of Mini-Computer Business Applications, Inc. (MCBA) of
;		Glendale, California, USA and Effective Management Systems, Inc.
;		of Menomonee Falls, Wisconsin, USA embodying substantial
;		creative effort and confidential information, ideas and
;		expressions, no part of which may be reproduced or transmitted
;		in any form or by any means, electronic, mechanical or
;		otherwise, including photocopying and recording or in
;		connection with any information storage or retrieval system
;		without the permission in writing from MCBA.
;
;		COPYRIGHT NOTICES:  Copyright <C> 1980, 1981, 1982
;		by Mini-Computer Business Applications, Inc.
;		AN UNPUBLISHED WORK.  Copyright <C> 1980, 1981, 1982
;		by Effective Management Systems, Inc.  AN UNPUBLISHED WORK.
;
;
;		COMPILE & LINK PER DOCUMENTATION INSTALLATION NOTES.
;
;:
;		THIS PROGRAM MAINTAINS THE VENDOR INFORMATION IN THE
;			ITEM MASTER FILE.
RECORD VENDR	
	.INCLUDE 'DEF:RD011A.DEF'

RECORD VENCTL	
	.INCLUDE 'DEF:RD011B.DEF'

RECORD VENIDX	
	.INCLUDE 'DEF:RD012A.DEF'

RECORD ITMMAS	
	.INCLUDE 'DEF:RD041A.DEF'

RECORD ITMCTL	
	.INCLUDE 'DEF:RD041B.DEF'

RECORD ITMIDX	
	.INCLUDE 'DEF:RD042A.DEF'

RECORD DSPLIN
		,A11
	LINNUM	,A2
		,A1,	'.'
		,A1
	VNNUM	,A4
		,A2
	VNNAM	,A30
		,A4
	ACTIV	,A1
		,A7
	MNORD	,A5
		,A12
RECORD HDR1
		,A40,	'              VENDOR             VENDOR '
		,A40,	'            ACTIVE ?  MINIMUM           '
RECORD HDR2
		,A40,	'              NUMBER              NAME  '
		,A40,	'                       ORDER            '
RECORD ARRAYS
	VNUMAR	,10A4
	VNNMAR	,10A35			;(01)
	ACTVAR	,10A1
	MNORAR	,10D5
	VEN	,D2
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
RECORD
	V	,D1
	BSMID	,D5
	SWITCH	,D1
	READ	,D1,	0
	WRITE	,D1,	1
	CNGCTL	,D1
	INXCTL	,D1
	SRCOPT	,D1
	SRCCTL	,D1
	ENTRY	,A25
	LIN	,D2
	WHATNO	,D2
	WCHVEN	,A4
	LOKCTL	,D5
	RESTRT	,D1
	VENROW	,D2
	WCHITM	,D5
	RJUST	,D1
	AITMNO	,A15
PROC(2)
	XCALL TERID (V)
	V = 1
	XCALL WAIT (1,V)

	SWITCH = 1
	XCALL FILES (1,'I',012,SWITCH)
	IF (SWITCH.EQ.9) GO TO INUSE1

	SWITCH = 1
	XCALL FILES (2,'I',011,SWITCH)
	IF (SWITCH.EQ.9) GO TO INUSE2

	SWITCH = 1
	XCALL FILES (3,'I',042,SWITCH)
	IF (SWITCH.EQ.9) GO TO INUSE3

	SWITCH = 1
	XCALL FILES (4,'U',041,SWITCH)
	IF (SWITCH.EQ.9) GO TO INUSE4
	LOKCTL = 0
	XCALL IO (4,ITMCTL,1,READ,LOKCTL)
	IF (LOKCTL.EQ.1) GO TO END
	RJUST = JSTIFY
	UNLOCK 4
	LOKCTL = 1
	XCALL IO (2,VENCTL,1,READ,LOKCTL)

;**********************************************************************
;ASKS FOR ITEM NUMBER AND IF OK DISPLAYS VENDOR INFO AND PERMITS CHANGES
;**********************************************************************

BEGIN,
	ITMMAS =
	VENDR =
	CNGCTL =
	ARRAYS =
	XCALL OUTPT (1,1,3,'ITEM VENDOR MAINTENANCE',V)
	XCALL OUTPT (2,1,0,'ITEM VENDOR CONTROL',V)
	XCALL OUTPT (5,10,0,'ITEM NO',V)

	CTL = '05,19,15,01,AE'
	CALL INPUT
	GO TO (BEGIN,END), INXCTL
	IF (RJUST) XCALL FRMAT (ENTRY(1,15),15)

	SRCOPT = 4
	BSMID = 1
	SRCCTL = 1
	XCALL SERCH (3,ITMIDX,ENTRY(1,15),1,15,ORG041,BSMID,SRCCTL,
&			SRCOPT,16,20,0,0,0,0)
	IF (SRCCTL.EQ.0) GO TO CONTIN
	XCALL MESAG ('ITEM NUMBER NOT FOUND',V)
	GO TO BEGIN

CONTIN,
	LOKCTL = 1
	XCALL IO (4,ITMMAS,IRC041,READ,LOKCTL)
	AITMNO = ITEMNO
	IF (RJUST) XCALL LEFTJ (AITMNO(1,15),15)
	XCALL OUTPT (5,19,0,AITMNO,V)
	XCALL OUTPT (5,36,0,DESCR,V)
	CALL FNDVEN		;PUTS THE VENDOR INFO IN THE ARRAYS
	XCALL OUTPT (08,1,0,HDR1,V)
	XCALL OUTPT (09,1,0,HDR2,V)
FIRST5,
	LIN = 9
	VEN =
	CALL DISPLA
ASK,
	XCALL ANYCN (CNGCTL,WHATNO)
	IF (CNGCTL.EQ.0) GO TO TEST
	IF (WHATNO.GT.5 .OR. WHATNO.LT.1) GO TO BADCNG
	IF (WHATNO.GT.NUMVEN) GO TO BADCNG
	VEN = WHATNO
	VENROW = (VEN*2) + 9
	XCALL OUTPT (VENROW,16,1,'\',V)
ASKAGN,
	CTL = '00,16,04,00,A '
	ROW = (VEN*2) + 9
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO FIRST5
	IF (ENTRY.EQ.'    ') GO TO BLKOUT
	XCALL FRMAT (ENTRY(1,4),4)
	WCHVEN = ENTRY(1,4)
	XCALL OUTPT (ROW,COL,0,WCHVEN,V)
	CALL GETVEN
	IF (SRCCTL.EQ.0) GO TO VENOK
	XCALL MESAG ('VENDOR NOT ON FILE',1)
	GO TO ASKAGN

;**********************************************************************

SECND5,
	LIN = 9
	VEN = 5
	CALL DISPLA
ASK610,
	XCALL ANYCN (CNGCTL,WHATNO)
	IF (CNGCTL.EQ.0) GO TO TEST
	IF (WHATNO.GT.10.OR.WHATNO.LT.6) GO TO BADCNG
	IF (WHATNO.GT.NUMVEN) GO TO BADCNG
	VEN = WHATNO
	VENROW = ((VEN - 5)*2) + 9
	XCALL OUTPT (VENROW,16,1,'\',V)
AGAIN,
	CTL = '00,16,04,00,# '
	ROW = ((VEN-5)*2)+9
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO SECND5
	IF (ENTRY.EQ.'    ') GO TO BLKOUT
	XCALL FRMAT (ENTRY(1,4),4)
	WCHVEN = ENTRY(1,4)
	XCALL OUTPT (ROW,COL,0,WCHVEN,V)
	CALL GETVEN
	IF (SRCCTL.EQ.0) GO TO VENOK
	XCALL MESAG ('VENDOR NOT ON FILE',1)
	GO TO AGAIN

VENOK,
	LOKCTL = 1
	XCALL IO (2,VENDR,IRC011,READ,LOKCTL)
	LINNUM = VEN
	VENDOR(VEN) = WCHVEN
	VNUMAR(VEN) = WCHVEN
	ACTIV = ACTVAR(VEN)
	IF (ACTIVE.EQ.'1') ACTVAR(VEN) = 'Y'
	IF (ACTIVE.EQ.'2') ACTVAR(VEN) = 'N'
	ACTIV = ACTVAR(VEN)
	VNNUM = WCHVEN
	VNNAM = NAME
	XCALL OUTPT (ROW,1,0,DSPLIN,V)

	COL = 64
	MAX = 05
	CALL INPUT
	IF (INXCTL.EQ.1.AND.VEN.LT.5) GO TO FIRST5
	IF (INXCTL.EQ.1) GO TO SECND5

	XCALL FRMAT (ENTRY(1,5),5)
	MNORAR(VEN) = ENTRY(1,5)
	MNORD = ENTRY(1,5)
	MINORD(VEN) = ENTRY(1,5)
	XCALL OUTPT (ROW,1,0,DSPLIN,V)
	IF (VEN.LT.6) GO TO ASK
	GO TO ASK610

TEST,
	IF (VEN.LE.5.AND.NUMVEN.GT.5) GO TO SECND5
	LOKCTL = 1
	XCALL IO (4,ITMMAS,IRC041,WRITE,LOKCTL)
	LOKCTL = 1
	XCALL IO (4,ITMCTL,MAX041,READ,LOKCTL)
	LOKCTL = 1
	XCALL IO (4,ITMCTL,1,READ,LOKCTL)
	UNLOCK 4
	GO TO BEGIN

BADCNG,
	CNGCTL = 3
	IF (VEN.GT.5) GO TO ASK610
	GO TO ASK
BLKOUT,
	IF (VEN.LT.6) VENROW = (VEN*2) + 9
	IF (VEN.GT.5) VENROW = ((VEN - 5)*2) + 9
	XCALL OUTPT (VENROW,16,1,'\',V)
	MINORD(VEN) =
	MNORAR(VEN) =
	VNUMAR(VEN) =
	VNNMAR(VEN) =
	ACTVAR(VEN) =
	VENDOR(VEN) =
	IF (VEN.LT.6) GO TO ASK
	GO TO ASK610

;**********************************************************************
;	SEARCHES FOR VENDOR IN THE VENIDX FILE
;**********************************************************************

GETVEN,
	BSMID = 1
	SRCCTL =
	SRCOPT = 4
	XCALL SERCH (1,VENIDX,WCHVEN,1,4,ORG011,BSMID,SRCCTL,
&			SRCOPT,6,10,0,0,0,0)
	RETURN

;**********************************************************************
;	DISPLAYS A SCREEN OF VENDORS
;**********************************************************************

DISPLA,
	INCR VEN
	LIN = LIN + 2
	LINNUM = VEN,'ZX'
	IF (VEN.GT.NUMVEN) LINNUM =
	VNNUM = VENDOR(VEN)
	VNNAM = VNNMAR(VEN)
	ACTIV = ACTVAR(VEN)
	MNORD = MNORAR(VEN)
	IF (VEN.GT.NUMVEN) DSPLIN =
	XCALL OUTPT (LIN,1,0,DSPLIN,V)
	IF (VEN.EQ.5.OR.VEN.EQ.10) RETURN
	GO TO DISPLA

;**********************************************************************

FNDVEN,
	VEN =
STRSCH,
	INCR VEN
	IF (VEN.GT.NUMVEN) RETURN
	WCHVEN = VENDOR(VEN)
	IF (WCHVEN.EQ.'    ') GO TO STRSCH
	BSMID = 1
	SRCCTL =
	SRCOPT = 4
	XCALL SERCH (1,VENIDX,WCHVEN,1,4,ORG011,BSMID,SRCCTL,
&			SRCOPT,6,10,0,0,0,0)

	IF (SRCCTL.NE.1) GO TO ACCUM
	VENDR =
	VNNMAR(VEN) = 'VENDOR NOT ON FILE'
	GO TO STRSCH

ACCUM,
	LOKCTL = 1
	XCALL IO (2,VENDR,IRC011,READ,LOKCTL)
	VNNMAR(VEN) = NAME
	IF (ACTIVE.EQ.'1') ACTVAR(VEN) = 'Y'
	IF (ACTIVE.EQ.'2') ACTVAR(VEN) = 'N'
	MNORAR(VEN) = MINORD(VEN)
	GO TO STRSCH

INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,V)
	RETURN

END,
	XCALL WAIT (1,V)
	SWITCH = 4
	XCALL FILES (4,'U',041,SWITCH)
INUSE4,
	SWITCH = 4
	XCALL FILES (3,'I',042,SWITCH)
INUSE3,
	SWITCH = 4
	XCALL FILES (2,'I',011,SWITCH)
INUSE2,
	SWITCH = 4
	XCALL FILES (1,'I',012,SWITCH)
INUSE1,
	RESTRT = 1
	XCALL PGCHN ('PO:POIMNU',RESTRT)

;**********************************************************************

END

