;  CLVNAV / POR - D11
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
;		COPYRIGHT NOTICES:  Copyright <C> 1977,1978,1979,1980,1981,1982
;		by Mini-Computer Business Applications, Inc.
;		AN UNPUBLISHED WORK.  Copyright <C> 1980, 1981, 1982
;		by Effective Management Systems, Inc.  AN UNPUBLISHED WORK.
;
;
;		COMPILE & LINK PER DOCUMENTATION INSTALLATION NOTES.
;
;:
;
;		PROGRAM TO ZERO OUT VENDOR PERFORMANCE AVERAGES AND
;		EVENT COUNTER
; (01) 01/04/96 DKS - ALSO CLEAR OUT YTD FIGURES THAT ARE ON THE VENDOR 
;		      PERFORMANCE ANALYSIS REPORT.
; (02) 12/03/96 DKS - CLEAR OUT OTHER FIELDS
;
RECORD VENMAS
	.INCLUDE 'DEF:RD011A.DEF'

RECORD VENCTL	
	.INCLUDE 'DEF:RD011B.DEF'

RECORD
	BLANKS	,A4
	CNGCTL	,D1
	CNT	,D5
	DECMAL	,D4
	STVEN	,A4
	ENDVEN	,A4
	V	,D1
	ENTRY	,A4
	INXCTL	,D1
	LOKCTL	,D1
	READ	,D1,	0
	SWITCH	,D1
	WHATNO	,D1
	WRITE	,D1,	1
PROC
	XCALL TERID (V)
	V = 1
	SWITCH = 1
	XCALL FILES (1,'U',11,SWITCH)
	IF (SWITCH.EQ.9) GO TO INUSE
	LOKCTL = 0
	XCALL IO (1,VENCTL,1,READ,LOKCTL)
	IF (LOKCTL) INXCTL = 2
	IF (LOKCTL) GO TO END
BEGIN,
	XCALL OUTPT (1,1,2,'CLEAR VENDOR PERFORMANCE AVERAGES',1)
	XCALL OUTPT (12,20,0,'STARTING VENDOR NUMBER',1)
	XCALL OUTPT (14,20,0,'ENDING VENDOR NUMBER',1)
STVEN,
	XCALL INPUT (12,44,4,0,'AE',ENTRY,INXCTL,1)
	GO TO (BEGIN,END), INXCTL
	IF (ENTRY.EQ.BLANKS) GO TO ALL
	XCALL FRMAT (ENTRY,4)
	STVEN = ENTRY
	XCALL OUTPT (12,44,1,STVEN,1)
ENDVEN,
	XCALL INPUT (14,44,4,0,'A ',ENTRY,INXCTL,1)
	GO TO (BEGIN), INXCTL
	IF (ENTRY.EQ.BLANKS) GO TO DEFALT
	XCALL FRMAT (ENTRY,4)
	ENDVEN = ENTRY
	XCALL OUTPT (14,44,1,ENDVEN,1)
	GO TO ANYCNG
ALL,
	STVEN =
	ENDVEN = ']]]]'
	XCALL OUTPT (12,44,1,'ALL',1)
	XCALL OUTPT (14,44,1,'\',1)
	GO TO ANYCNG
DEFALT,
	ENDVEN = STVEN
	XCALL OUTPT (14,44,1,STVEN,1)
ANYCNG,
	CNGCTL = 2
	XCALL ANYCN (CNGCTL,WHATNO)
	GO TO (PROCES,STVEN), CNGCTL + 1
PROCES,
	IF (STVEN.GT.ENDVEN) XCALL MESAG
&	('STARTING VENDOR NUMBER GREATER THAN ENDING VENDOR NUMBER',1)
	IF (STVEN.GT.ENDVEN) GO TO BEGIN
	CNT = 1
LOOP,
	INCR CNT
	IF (CNT.GT.REC011) GO TO DONE
	LOKCTL = 1
	XCALL IO (1,VENMAS,CNT,READ,LOKCTL)
	IF (VENNO.EQ.']]]]') GO TO DONE
	IF (STVEN.GT.VENNO .OR. ENDVEN.LT.VENNO) GO TO LOOP
	AVCNTR =
	AVPVAR =
	AVPREJ =
	AVLDTM =
	AVDSLA =
	LNSYTD = 			;(02)
	LSLYTD =			;(02)
	POSYTD = 			;(02)
	LSLPYR =			;(02)
	IF (LNSYTD.NE.0) LSLPYR = ( (10000 * LSLYTD) / LNSYTD) #2	;(02)
	LOKCTL = 1
	XCALL IO (1,VENMAS,CNT,WRITE,LOKCTL)
	GO TO LOOP
DONE,
	XCALL MESAG ('VENDOR PERFORMANCE AVERAGES CLEARED',2)
END,
	IF (INXCTL.NE.2.AND.(STVEN.NE.'    ' .OR. ENDVEN.NE.']]]]')) GO TO BEGIN
	XCALL FILES (1,'U',11,4)
INUSE,
	XCALL PGCHN ('PO:POSFMN',1)

END

