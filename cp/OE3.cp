SUBROUTINE OE3
;
; OE3 / COP 
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
	CUSNOA	,A
	CUSNAM	,A
	OMLMSC	,A
	OMLTAX	,A
	OMLFRT	,A
	V	,D
PROC
	XCALL OUTPT (1,20,2,'CUSTOMER: ',V)
	XCALL OUTPT (0,0,0,CUSNOA,V)
	XCALL OUTPT (0,0,0,' ',V)
	XCALL OUTPT (0,0,0,CUSNAM,V)
	XCALL OUTPT (1,66,0,'ORDER #: ',V)
	XCALL OUTPT (0,0,0,ORDNOA,V)
	XCALL OUTPT (2,13,0,'ORDER NET =',V)
	XCALL OUTPT (2,42,0,'WEIGHT (LBS) =',V)
	XCALL OUTPT (3,10,0,'(TAXABLE AMT =             )',V)
	XCALL OUTPT (4,7,0,'1. FREIGHT',V)
	IF (OMLMSC.EQ.'Y') XCALL OUTPT (4,32,0,'ACCT-#:',V)
	XCALL OUTPT (5,7,0,'2. MISC CHGS',V)
	IF (OMLFRT.EQ.'Y') XCALL OUTPT (5,32,0,'ACCT-#:',V)
	XCALL OUTPT (6,10,0,'(TAXABLE MISC =           )',V)
	XCALL OUTPT (7,7,0,'3. SALES TAX',V)
	IF (OMLTAX.EQ.'Y') XCALL OUTPT (7,32,0,'ACCT-#:',V)
	XCALL OUTPT (8,10,0,'TOTAL =',V)
	XCALL OUTPT (9,7,0,'4. COMMIS AMT',V)
	XCALL OUTPT (10,7,0,'5. COMMIS PCT',V)
	RETURN
	END
