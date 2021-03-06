;  CPFILS / COP 
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
;		PROGRAM TO DISPLAY CONTROL INFORMATION
;		ON ALL CUSTOMER ORDER PROCESSING FILES
;
;
RECORD
	,A64,'THIS MATERIAL CONTAINS THE VALUABLE PROPERTIES AND TRADE SECRETS'
	,A62,'OF MCBA, EMBODYING CONFIDENTIAL INFORMATION AND IDEAS, NO PART'
	,A56,'OF WHICH MAY BE USED AND/OR DISCLOSED WITHOUT MCBAs DULY'
	,A55,'AUTHORIZED LICENSE AGREEMENT AND/OR WRITTEN PERMISSION.'
	,A61,'COPYRIGHT (C) MCBA, AN UNPUBLISHED WORK. ALL RIGHTS RESERVED.'

RECORD CRMHDR		
		.INCLUDE 'DEF:RD046B.DEF'
; ** CONTROL RECORD OF CRMLIN FILE **
;
RECORD CRMLIN		
		.INCLUDE 'DEF:RD047B.DEF'
; ** CONTROL RECORD OF SLSHST FILE **
;
RECORD SLSHST		
		.INCLUDE 'DEF:RD055B.DEF'
; ** CONTROL RECORD OF SLSSUM FILE **
;
RECORD SLSSUM		
		.INCLUDE 'DEF:RD058B.DEF'
RECORD PDACTL		
		.INCLUDE 'DEF:RD069B.DEF'
RECORD SHPCTL
		.INCLUDE 'DEF:RD171B.DEF'
RECORD SHVCTL
		.INCLUDE 'DEF:RD172B.DEF'
RECORD
		.INCLUDE 'DEF:RD046S.DEF'
		.INCLUDE 'DEF:RD047S.DEF'
		.INCLUDE 'DEF:RD055S.DEF'
		.INCLUDE 'DEF:RD058S.DEF'
		.INCLUDE 'DEF:RD069S.DEF'
		.INCLUDE 'DEF:RD171S.DEF'
		.INCLUDE 'DEF:RD172S.DEF'
RECORD
	ROW	,D2
	COL	,D2
	MAX	,D2
	SWITCH  ,D1
	DECMAL	,D18
	V	,D1
	READ	,D1	,0
	WRITE	,D1	,1
	LOKCTL	,D1
PROC
	XCALL TERID(V)
	XCALL OUTPT (1,1,2,'DISPLAY COP FILE CONTROL DATA',V)
	XCALL OUTPT (2,27,0,'FILENAME  LENGTH  SORTED    USED',V)
	XCALL OUTPT (2,63,0,'MAX   DELETIONS',V)
	XCALL OUTPT (3,2,0,'CR MEMO HEADER FILE       CRMHDR',V)
	XCALL OUTPT (4,2,0,'CR MEMO LINE ITEM FILE    CRMLIN',V)
	XCALL OUTPT (5,2,0,'SALES HISTORY FILE        SLSHST',V)
	XCALL OUTPT (6,2,0,'SALES SUMMARY FILE        SLSSUM',V)
	XCALL OUTPT (7,2,0,'PROD CAT ACCOUNT FILE     PRDACT',V)
	XCALL OUTPT (8,2,0,'SHIP-TO ADDRESS FILE      SHIPTO',V)
	XCALL OUTPT (9,2,0,'SHIP-VIA CODE FILE        SHPVIA',V)
	XCALL WATE(3,V)
	SWITCH = 5
	XCALL FILES(4,'I',46,SWITCH)
	IF (SWITCH.EQ.9) GO TO EXIT
	LOKCTL = 1
	XCALL IO (4,CRMHDR,1,READ,LOKCTL)
	CLOSE 4
	SWITCH = 5
	XCALL FILES(5,'I',47,SWITCH)
	IF (SWITCH.EQ.9) GO TO EXIT
	LOKCTL = 1
	XCALL IO (5,CRMLIN,1,READ,LOKCTL)
	CLOSE 5

;;;	SWITCH = 5
;;;	XCALL FILES(6,'I',55,SWITCH)
;;;	IF (SWITCH.EQ.9) GO TO EXIT
;;;	LOKCTL = 1
;;;	XCALL IO (6,SLSHST,1,READ,LOKCTL)
;;;	CLOSE 6

	SWITCH = 5
	XCALL FILES(7,'I',69,SWITCH)
	IF (SWITCH.EQ.9) GO TO EXIT
	LOKCTL = 1
	XCALL IO (7,PDACTL,1,READ,LOKCTL)
	CLOSE 7
;;;	SWITCH = 5
;;;	XCALL FILES(8,'I',58,SWITCH)
;;;	IF (SWITCH.EQ.9) GO TO EXIT
;;;	LOKCTL = 1
;;;	XCALL IO (8,SLSSUM,1,READ,LOKCTL)
;;;	CLOSE 8
	SWITCH = 5
	XCALL FILES(8,'I',171,SWITCH)
	IF (SWITCH.EQ.9) GO TO EXIT
	LOKCTL = 1
	XCALL IO (8,SHPCTL,1,READ,LOKCTL)
	CLOSE 8
	SWITCH = 5
	XCALL FILES(8,'I',172,SWITCH)
	IF (SWITCH.EQ.9) GO TO EXIT
	LOKCTL = 1
	XCALL IO (8,SHVCTL,1,READ,LOKCTL)
	CLOSE 8
	COL = 39
	MAX = 3
	ROW = 3
	DECMAL = SIZ046
	CALL DSPNUM
	ROW = 4
	DECMAL = SIZ047
	CALL DSPNUM
	ROW = 5
;;;	DECMAL = SIZ055
;;;	CALL DSPNUM
	XCALL OUTPT (ROW,COL,0,'*NOT INSTALLED*',V)
	ROW = 6
;;;	DECMAL = SIZ058
;;;	CALL DSPNUM
	XCALL OUTPT (ROW,COL,0,'*NOT INSTALLED*',V)
	ROW = 7
	DECMAL = SIZ069
	CALL DSPNUM
	ROW = 8
	DECMAL = SIZ171
	CALL DSPNUM
	ROW = 9
	DECMAL = SIZ172
	CALL DSPNUM
	COL = 46
	MAX = 5
	ROW = 3
	DECMAL = ORG046 - 1
	CALL DSPNUM
	ROW = 4
	DECMAL = ORG047 - 1
	CALL DSPNUM
;;;	ROW = 5
;;;	DECMAL = ORG055 - 1
;;;	CALL DSPNUM
;;;	ROW = 6
;;;	DECMAL = ORG058 - 1
;;;	CALL DSPNUM
	ROW = 7
	DECMAL = ORG069 - 1
	CALL DSPNUM
	ROW = 8
	DECMAL = ORG171 - 1
	CALL DSPNUM
	ROW = 9
	DECMAL = ORG172 - 1
	CALL DSPNUM
	COL = 54
	ROW = 3
	DECMAL = REC046 - 1
	CALL DSPNUM
	ROW = 4
	DECMAL = REC047 - 1
	CALL DSPNUM
;;;	ROW = 5
;;;	DECMAL = REC055 - 1
;;;	CALL DSPNUM
;;;	ROW = 6
;;;	DECMAL = REC058 - 1
;;;	CALL DSPNUM
	ROW = 7
	DECMAL = REC069 - 1
	CALL DSPNUM
	ROW = 8
	DECMAL = REC171 - 1
	CALL DSPNUM
	ROW = 9
	DECMAL = REC172 - 1
	CALL DSPNUM
	COL = 62
	ROW = 3
	DECMAL = MAX046 - 1
	CALL DSPNUM
	ROW = 4
	DECMAL = MAX047 - 1
	CALL DSPNUM
;;;	ROW = 5
;;;	DECMAL = MAX055 - 1
;;;	CALL DSPNUM
;;;	ROW = 6
;;;	DECMAL = MAX058 - 1
;;;	CALL DSPNUM
	ROW = 7
	DECMAL = MAX069 - 1
	CALL DSPNUM
	ROW = 8
	DECMAL = MAX171 - 1
	CALL DSPNUM
	ROW = 9
	DECMAL = MAX172 - 1
	CALL DSPNUM
	COL = 72
	MAX = 3
	ROW = 3
	DECMAL = DEL046
	CALL DSPNUM
	ROW = 4
	DECMAL = DEL047
	CALL DSPNUM
;;;	ROW = 5
;;;	DECMAL = DEL055
;;;	CALL DSPNUM
;;;	ROW = 6
;;;	DECMAL = DEL058
;;;	CALL DSPNUM
	ROW = 7
	DECMAL = DEL069
	CALL DSPNUM
	ROW = 8
	DECMAL = DEL171
	CALL DSPNUM
	ROW = 9
	DECMAL = DEL172
	CALL DSPNUM
	XCALL MESAG(' ',2)
EXIT,
	XCALL PGCHN ('CP:SPCFUN',1)
DSPNUM,
	XCALL DSPLY(MAX,ROW,COL,DECMAL,1,V)
	RETURN
END
