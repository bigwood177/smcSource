;  POIMNU / POR - D11
;
;		AUTHOR: 1-DEC-81  WRF
;
;				::CRELPOR.DEF::
;**********************************************************************
;		PURCHASE ORDER & RECEIVING -- Release 1.1
;		RELEASED: 1-MAY-82
;**********************************************************************
;:
;				::PCPYEMS.DEF::
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
;		COPYRIGHT NOTICES:  Copyright <C> 1980, 1981, 1982
;		by Mini-Computer Business Applications, Inc.
;		AN UNPUBLISHED WORK.  Copyright <C> 1980, 1981, 1982
;		by Effective Management Systems, Inc.  AN UNPUBLISHED WORK.
;
;
;		COMPILE & LINK PER DOCUMENTATION INSTALLATION NOTES.
;
;:
;		ITEM VENDOR MAINTENANCE MENU
RECORD
	ENTRY	,A3
	INXCTL	,D1
	PGM	,D2
	PROGNM	,4A9,	'PO:POITMC','PO:POINQI','PO:POITMR','PO:POITMA'
	RESTRT	,D1
	V	,D1
	NXTPGM	,A9
	SWITCH	,D1
	LOKCTL	,D1
	READ	,D1,	0
PROC
	XCALL TERID (V)
BEGIN,
	XCALL OUTPT (1,1,2,'ITEM VENDOR MAINTENANCE',1)
	XCALL OUTPT (5,20,0,'PLEASE SELECT FUNCTION',V)
	XCALL OUTPT (6,25,0,'1. ITEM VENDOR CONTROL',V)
	XCALL OUTPT (7,25,0,'2. ITEM VENDOR INQUIRY',V)
	XCALL OUTPT (8,25,0,'3. VENDOR ITEM REPORT',V)
	XCALL OUTPT (9,25,0,'4. APPROVED VENDOR LIST',V)
INPUT,
	XCALL INPUT (5,44,1,0,'#E',ENTRY,INXCTL,V)
	GO TO (INPUT,EXIT), INXCTL
	PGM = ENTRY (1,2)
	IF (PGM.LT.1.OR.PGM.GT.4) GO TO INPUT
	RESTRT = 1
	XCALL PGCHN (PROGNM(PGM),RESTRT)
;**********************************************************************
EXIT,
	RESTRT = 1
	XCALL PGCHN ('PO:POMENU',RESTRT)
;**********************************************************************

END

