;POITMR / POR  D11
;
;		AUTHOR: 1-DEC-81  WRF
;		MODIFIED:  DVD  19-AUG-82  POR10-D11D-1
;
;		::CRELPOR.DEF::
;**********************************************************************
;		PURCHASE ORDER & RECEIVING -- Release 1.1
;		RELEASED: 1-MAY-82
;**********************************************************************
;:
;
;		::PCPYEMS.DEF::
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
RECORD ITMMAS		
	.INCLUDE 'DEF:RD041A.DEF'

RECORD ITMCTL	
	.INCLUDE 'DEF:RD041B.DEF'

RECORD POSIDX	
	.INCLUDE 'DEF:RD155C.DEF'

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

RECORD SNDMSG
	PRGNAM	,A9
	RCNT	,D5
	OCNT	,D5
	MFIL	,D3
		,A3
RECORD
	ALL	,D1
	DECMAL	,D4
	STRVEN	,A4
	ENDVEN	,A4
	CNGCTL	,D1
	INXCTL	,D1
	ENTRY	,A4
	SWITCH	,D1
	LOKCTL	,D1
	RECNUM	,D5
	VEN	,D2
	BLANKS	,A4
	BRACKS	,A24,	']]]]]]]]]]]]]]]]]]]]]]]]'
	WHATNO	,D2
	MSGCTL	,D1
	RESTRT	,D1
	SIZE	,D5
	COUNTR	,D5
	ALLSW	,D1
	V	,D1
	READ	,D1,	0
	WRITE	,D1,	1
	WVEN	,A4
PROC
	XCALL TERID (V)
	V = 1
	XCALL WAIT (1,V)

	SWITCH = 1
	XCALL FILES (1,'I',041,SWITCH)
	IF (SWITCH.EQ.9) GO TO INUSE1

	LOKCTL = 1
	XCALL IO (1,ITMCTL,1,READ,LOKCTL)

	SWITCH = 3
	XCALL FILES (2,'O',155,SWITCH)
	IF (SWITCH.EQ.9) GO TO INUSE2

BEGIN,
	ALL = 1
	COUNTR = 0
	XCALL OUTPT (1,1,3,'ITEM VENDOR MAINTENANCE',V)
	XCALL OUTPT (2,1,0,'VENDOR ITEM REPORT',V)
	XCALL OUTPT (08,20,0,'PLEASE ENTER:',V)
	XCALL OUTPT (10,25,0,'1. STARTING VENDOR NUMBER',V)
	XCALL OUTPT (12,25,0,'2. ENDING VENDOR NUMBER',V)
STRNO,
	CTL = '10,52,04,00,AE'
	CALL INPUT
	GO TO (BEGIN,END), INXCTL
	IF (ENTRY(1,4).EQ.BLANKS) GO TO ALLVEN
	ALL =
	XCALL FRMAT (ENTRY,4)
	STRVEN = ENTRY
	XCALL OUTPT (ROW,COL,0,STRVEN,V)
	IF (ENDVEN.EQ.BRACKS) GO TO ENDIN2
	IF (CNGCTL.NE.1) GO TO ENDIN2
	IF (ENDVEN.LT.STRVEN) GO TO MSG1
	GO TO ANYCNG
ENDING,
	IF (ENDVEN.EQ.BRACKS) GO TO STRNO
ENDIN2,
	CTL = '12,52,04,00,A '
	CALL INPUT
	IF (INXCTL.EQ.1) GO TO BEGIN
	XCALL FRMAT (ENTRY,4)
	ENDVEN = ENTRY
	IF (ENTRY.EQ.BLANKS) ENDVEN = STRVEN
	XCALL OUTPT (12,52,0,ENDVEN,V)
	IF (ENDVEN.LT.STRVEN) GO TO MSG1
	GO TO ANYCNG
;**********************************************************************
ALLVEN,
	XCALL OUTPT (10,52,0,'ALL ',V)
	XCALL OUTPT (12,52,0,BLANKS,V)
	STRVEN =
	ENDVEN = BRACKS
	CNGCTL =
ANYCNG,
	XCALL ANYCN (CNGCTL,WHATNO)
	GO TO (PROCES), CNGCTL + 1
	GO TO (STRNO,ENDING), WHATNO
BADCNG,
	CNGCTL = 3
	GO TO ANYCNG

;**********************************************************************
MSG1,
	XCALL MESAG ('STARTING/ENDING VENDOR NUMBERS ARE OUT OF ORDER',1)
	ENDVEN = BRACKS
	GO TO STRNO
;**********************************************************************

PROCES,
	SIZE = (((REC041*NUMVEN+1)*26)/512) + 1
	XCALL OFILE (2,155,SIZE,SWITCH)
	IF (SWITCH.EQ.1) GO TO NOROOM
	IF (SWITCH.EQ.9) GO TO END
	POSIDX =
	LOKCTL = 1
	XCALL IOS (2,POSIDX,WRITE,LOKCTL)
	RECNUM = 1
LOOP,
	VEN =
	INCR RECNUM
	XCALL IO (1,ITMMAS,RECNUM,READ,LOKCTL)
	UNLOCK 1
	IF (DESCR(1,6).EQ.']]]DEL') GO TO LOOP
	IF (DESCR(1,6).EQ.BRACKS) GO TO CHAIN
	CALL VNLOOP
	GO TO LOOP
VNLOOP,
	INCR VEN
	IF (VEN.GT.NUMVEN) RETURN
	IF (VENDOR(VEN).EQ.'    ') GO TO VNLOOP
	WVEN = VENDOR(VEN)
	IF (WVEN.GT.ENDVEN.OR.WVEN.LT.STRVEN) GO TO VNLOOP
	VENNUM = VENDOR(VEN)
	ITMNUM = ITEMNO
	ITRCNO = RECNUM
	LOKCTL = 1
	XCALL IOS (2,POSIDX,WRITE,LOKCTL)
	INCR COUNTR
	GO TO VNLOOP

;**********************************************************************

CHAIN,
	IF (COUNTR.EQ.0) GO TO NONE
	XCALL WAIT (1,V)
	LOKCTL = 1
	XCALL IOS (2,BRACKS,WRITE,LOKCTL)
	CLOSE 2
	XCALL FILES (1,'I',041,4)

	MFIL = 155
	PRGNAM = 'PO:VIRPRT'
	RCNT = COUNTR + 1
	OCNT = 1
	SWITCH = 2
	XCALL SNMSG (SNDMSG,SWITCH)

	SWITCH =
	XCALL PGCHN ('PO:SRTPOS',SWITCH)
NONE,
	XCALL MESAG ('NO VENDORS WITHIN RANGE',2)
	GO TO BEGIN

;**********************************************************************
NOROOM,
	XCALL MESAG ('NO ROOM ON DISK TO CREATE POSIDX',2)
	GO TO END
;**********************************************************************

END,
	XCALL FILES (2,'O',155,4)
INUSE2,
	XCALL FILES (1,'I',041,4)
INUSE1,
	XCALL PGCHN ('PO:POIMNU',1)

;**********************************************************************

INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,V)
	RETURN

;**********************************************************************

END

