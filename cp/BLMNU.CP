SUBROUTINE BLMNU
;
;  BLMNU / COP 
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
;		MAIN MENU FOR BILLING
;
	V	,D
PROC
	XCALL OUTPT (2,1,2,'\',1)
	XCALL OUTPT (4,20,2,'PLEASE SELECT APPLICATION',V)
	XCALL OUTPT (5,25,0,'1. SELECT ORDERS FOR BILLING',V)
	XCALL OUTPT (6,25,0,'2. UNSELECT SELECTED ORDERS',V)
	XCALL OUTPT (7,25,0,'3. PRINT BILLING EDIT LIST',V)
	XCALL OUTPT (8,25,0,'4. PRINT INVOICES',V)
	XCALL OUTPT (9,25,0,'5. BILLING EDIT LIST BY USER',V)
	RETURN
END
