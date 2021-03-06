; SRTVCA / AP - D11
;
;			::CRELAP.DEF::
;**********************************************************************
;		ACCOUNTS PAYABLE - RELEASE 6.1
;		RELEASED: 1-MAY-82
;**********************************************************************
;:
;			::PCPYEMS.DEF::
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
SYSTEM:
INPUT:NEWAP0
WORK:3
OUTPUT:NEWAP0
RECORD:INCLUDE DEF:RD014T.DEF
KEYS:WVCHNO,WNDISC
PAD:]
EXECUTE:AP:VCHRG2
END:
