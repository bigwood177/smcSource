SUBROUTINE OE7
;
; OE7 / COP 
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
;		DISPLAY MENU FOR BILLS; CALLED FROM AN OVERLAY REGION
;
	ORDNOA	,A
	V	,D
PROC
	XCALL OUTPT (1,1,1,'BILLING',1)
	XCALL OUTPT (2,1,1,'\',1)
	XCALL OUTPT (2,67,0,'ORDER # ',1)
	XCALL OUTPT (2,0,0,ORDNOA,1)
	XCALL OUTPT (3,1,1,'\',1)
	XCALL OUTPT (3,25,0,'PROMISE',1)
	XCALL OUTPT (3,34,0,'REQUEST',1)
	XCALL OUTPT (4,1,1,'ITEM NUMBER/DESCRIPTION',1)
	XCALL OUTPT (4,26,0,'DATE',1)
	XCALL OUTPT (4,36,0,'DATE',1)
	XCALL OUTPT (4,44,0,'ORD SHIP  B/O   PRICE  DISC EXT-PRICE',1)
	XCALL OUTPT (5,1,1,'\',1)
	RETURN
	END
