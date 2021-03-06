; POHCNT / POR - D11
;
;
;
;
;
;
;
;
;		PROGRAM TO UPDATE SORTED RECORD COUNTER AFTER SORTING
;
;
RECORD DUMHDR	
	.INCLUDE 'DEF:RD151B.DEF'

RECORD DUMLIN	
	.INCLUDE 'DEF:RD152B.DEF'

RECORD SNDMSG
	PRGNAM	,A9
	RCNT	,D5
	OCNT	,D5
	FILN	,D3,152
RECORD
	LOKCTL	,D1
	MSGCTL	,D1
	NOABRT	,D1,	1
	NXTPGM	,A9
	READ	,D1,	0
	SWITCH	,D1
	V	,D1
	WRITE	,D1,	1
PROC
	XCALL TERID (V)
	XCALL OUTPT (2,1,1,'UPDATE ORGANIZED COUNT',1)

	SWITCH = 5
	XCALL FILES (1,'U',151,SWITCH)		;FILE # 151 -- POR HEADER FILE

	LOKCTL = NOABRT
	XCALL IO (1,DUMHDR,1,READ,LOKCTL)
	ORG151 = REC151
	LOKCTL = NOABRT
	XCALL IO (1,DUMHDR,1,WRITE,LOKCTL)
	CLOSE 1

	IF (DEL151.GT.10) XCALL PGCHN ('PO:ORGPOH',1)

	SWITCH = 5
	XCALL FILES (2,'I',152,SWITCH)		;FILE 152 -- POR LINE FILE
	LOKCTL = NOABRT
	XCALL IO (2,DUMLIN,1,READ,LOKCTL)
	CLOSE 2

	IF (ORG152.EQ.REC152.AND.DEL152.GT.50) XCALL PGCHN ('PO:ORGPOL',1)

;;	IF (ORG152.EQ.REC152) XCALL PGCHN ('PO:UNPROT',1)

	NXTPGM = 'PO:UNPROT'
	MSGCTL = 1
	XCALL SNMSG (NXTPGM,MSGCTL)
	IF (MSGCTL.EQ.9) GO TO BADMSG

	PRGNAM = 'PO:POLCNT'
	RCNT = REC152
	OCNT = ORG152

	MSGCTL = 5
	XCALL SNMSG (SNDMSG,MSGCTL)
	MSGCTL = 2
	XCALL SNMSG (NXTPGM,MSGCTL)

	XCALL PGCHN ('PO:SRTPOL',0)
;**********************************************************************
BADMSG,
	XCALL MESAG ('?MESSAGE ERROR IN POHCNT.',1)
	XCALL FILES (1,'U',151,4)
	XCALL FILES (2,'U',152,4)
	XCALL PGCHN ('PO:POMENU',1)
;**********************************************************************


END

