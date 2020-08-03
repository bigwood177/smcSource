; SSSFMN / COP 
;
;
;		::PCPYCOP.DEF::
;*********************************************************************
;		CUSTOMER ORDER PROCESSING 
;		
;		RELEASED: AUGUST 1, 1984 (d70s10)
;***********************************************************************
;
;		PROPRIETARY RIGHTS NOTICE:  All rights reserved.  This
;		material contains the valuable properties and trade secrets
;		of MCBA, Glendale, California, USA embodying substantial
;		creative effort and confidential information and ideas, no
;		part of which may be used and/or disclosed without MCBA's
;		duly authorized license agreement and/or written permission.
;
;		COPYRIGHT NOTICE:  Copyright <C> 1978, 1981, 1982, 1983, 1984
;		MCBA, AN UNPUBLISHED WORK.  ALL RIGHTS RESERVED.
;
;
;		COMPILE & LINK PER INSTALLATION OR TECHNICAL NOTES.
;
;:
;
;		SPECIAL FUNCITIONS MENU
;
RECORD
	,A64,'THIS MATERIAL CONTAINS THE VALUABLE PROPERTIES AND TRADE SECRETS'
	,A62,'OF MCBA, EMBODYING CONFIDENTIAL INFORMATION AND IDEAS, NO PART'
	,A56,'OF WHICH MAY BE USED AND/OR DISCLOSED WITHOUT MCBAs DULY'
	,A55,'AUTHORIZED LICENSE AGREEMENT AND/OR WRITTEN PERMISSION.'
	,A61,'COPYRIGHT (C) MCBA, AN UNPUBLISHED WORK. ALL RIGHTS RESERVED.'

RECORD
	ENTRY	,A3
	INXCTL	,D1
	PGM	,D1
	PROGNM	,2A9	,'CP:BLDSLH','CP:SSSET '
	V	,D1
PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'SALES HISTORY - SPECIAL FUNCTIONS',V)
	XCALL OUTPT (5,15,0,'PLEASE SELECT APPLICATION',V)
	XCALL OUTPT (6,20,0,'1. MANUAL ENTRY OF SALES HISTORY',V)
	XCALL OUTPT (7,20,0,'2. SET/RESET SALES SUMMARY PERIOD DESCRIPTIONS',V)
INPUT,
	XCALL INPUT (5,42,1,1,'#E',ENTRY,INXCTL,V)
	GO TO (INPUT,ENDOFF), INXCTL
	PGM = ENTRY (1,1)
	IF (PGM.LT.1.OR.PGM.GT.2) GO TO INPUT
	XCALL WATE(3,V)
	XCALL PGCHN (PROGNM(PGM),1)
ENDOFF,
	XCALL WATE(3,V)
	XCALL PGCHN ('CP:SSMENU',1)
	END
