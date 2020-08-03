; UNPRBO / COP 
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
;		ROUTINE TO TURN OFF FILE PROTECTION IF USER
;		DOESN'T WANT TO WAIT FOR PRINTER WHILE IN O.E.
;		CUSTOMER ORDER STATUS REPORT JOB STREAM
;
RECORD
	,A64,'THIS MATERIAL CONTAINS THE VALUABLE PROPERTIES AND TRADE SECRETS'
	,A62,'OF MCBA, EMBODYING CONFIDENTIAL INFORMATION AND IDEAS, NO PART'
	,A56,'OF WHICH MAY BE USED AND/OR DISCLOSED WITHOUT MCBAs DULY'
	,A55,'AUTHORIZED LICENSE AGREEMENT AND/OR WRITTEN PERMISSION.'
	,A61,'COPYRIGHT (C) MCBA, AN UNPUBLISHED WORK. ALL RIGHTS RESERVED.'

RECORD
	V	,D1
PROC
	XCALL TERID (V)
	XCALL FILES (6,'I',51,7)		;FILE # 51 -- BAKORD FILE
	XCALL FILES (7,'I',52,7)		;FILE # 52 -- BOINDX FILE
	XCALL FILES (10,'I',41,4)		;FILE # 41 -- ITMMAS FILE
	XCALL PGCHN ('CP:BAKORD',1)
	END
