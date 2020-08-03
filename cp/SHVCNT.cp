;  SHVCNT / COP 
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
;		UPDATES COUNTERS AFTER SORT OF TERMS CODE FILE
;
RECORD
	,A64,'THIS MATERIAL CONTAINS THE VALUABLE PROPERTIES AND TRADE SECRETS'
	,A62,'OF MCBA, EMBODYING CONFIDENTIAL INFORMATION AND IDEAS, NO PART'
	,A56,'OF WHICH MAY BE USED AND/OR DISCLOSED WITHOUT MCBAs DULY'
	,A55,'AUTHORIZED LICENSE AGREEMENT AND/OR WRITTEN PERMISSION.'
	,A61,'COPYRIGHT (C) MCBA, AN UNPUBLISHED WORK. ALL RIGHTS RESERVED.'

RECORD SHVCTL
		.INCLUDE 'DEF:RD172B.DEF'

RECORD
	LOKCTL	,D1
	MSG	,A9
	NXTPGM	,A9
	READ	,D1	,0
	SWITCH	,D1
	V	,D1
	WRITE	,D1	,1
PROC
	XCALL TERID (V)
	XCALL OUTPT (2,1,0,'UPDATE COUNTERS',1)
	XCALL WATE(4,V)
	SWITCH = 5
	XCALL FILES(1,'U',172,SWITCH)		;FILE # 172 -- SHPVIA
	LOKCTL = 1
	XCALL IO (1,SHVCTL,1,READ,LOKCTL)
	ORG172 = REC172
	LOKCTL = 1
	XCALL IO (1,SHVCTL,1,WRITE,LOKCTL)
	SWITCH = 1
	XCALL SNMSG (MSG,SWITCH)
	IF (SWITCH.EQ.9) GO TO NOMESG
	NXTPGM = MSG
	SWITCH = 3
	XCALL SNMSG (' ',SWITCH)
	IF (NXTPGM.EQ.'CP:SHVPRT') CALL CLOSE
	SWITCH = 1
	IF (NXTPGM.EQ.'CP:ORGSHV') SWITCH = 0
	XCALL PGCHN (NXTPGM,SWITCH)
NOMESG,
	CALL CLOSE
	XCALL PGCHN ('CP:SPCFUN',1)
CLOSE,
	XCALL FILES (1,'U',172,4)
	RETURN
END
