; SRTSHV / COP - D11
;
;		::PCPYCOP.DEF::
;*********************************************************************
;		CUSTOMER ORDER PROCESSING  -  RELEASE 1.1
;		RELEASED: AUGUST 1, 1984 (d70s10)
;***********************************************************************
;:
;
;		PROPRIETARY RIGHTS NOTICE:  All rights reserved.  This
;		material contains the valuable properties and trade secrets
;		of MCBA, of Glendale, California, USA embodying substantial
;		creative effort and confidential information and ideas, no
;		part of which may be used and/or disclosed without MCBA's
;		duly authorized license agreement and/or written permission.
;
;		COPYRIGHT NOTICES:  Copyright <C> 1978, 1981, 1982, 1983, 1984
;		MCBA, AN UNPUBLISHED WORK.  ALL RIGHTS RESERVED.
;
;		COMPILE & LINK PER DOCUMENTATION INSTALLATION NOTES.
;
;:
;
INPUT:SHPVIA
WORK:3
OUTPUT:SHPVIA
RECORD:INCLUDE DEF:RD172A.DEF
KEYS:SHPVCD
SYSTEM:
EXECUTE:CP:SHVPRT
PAD:]
END:
