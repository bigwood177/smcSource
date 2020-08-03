SUBROUTINE OE4
;
; OE4 / COP 
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
;		DISPLAY MENU FOR OE; CALLED FROM AN OVERLAY REGION
;
	OPTION	,D
	V	,D
RECORD
	DOTS	,A25,'.........................'
	DOTS2	,A35,'...................................'
	BLANKS	,A10
PROC
	GOTO (SHIPTO,BAKOR1,BAKOR2,CRMEMO,COMENT,BILCNG), OPTION
SHIPTO,
	XCALL OUTPT (7,16,1,DOTS,V)
	XCALL OUTPT (8,16,1,DOTS(1,21),V)
	XCALL OUTPT (9,16,1,DOTS2(1,29),V)
	RETURN
BAKOR1,
	XCALL OUTPT (12,31,0,'1-CANCEL, 2-B/O BALANCE, 3-B/O ALL, 4-OVERRIDE ?',V)
	RETURN
BAKOR2,
	XCALL OUTPT (12,23,0,'(NON B/O ITEM): 1-CANCEL, 2-ORDER IN STOCK, 3-OVERRIDE ?',V)
	RETURN
CRMEMO,
	XCALL OUTPT (1,27,0,'***** CREDIT MEMO *****',V)
	RETURN
COMENT,
	XCALL OUTPT (11,16,0,DOTS2,V)
	RETURN
BILCNG,
	XCALL OUTPT (24,21,1,'SELECT:   1-SHIP QTY   2-PRICE   3-DISCOUNT',V)
	RETURN

	END
