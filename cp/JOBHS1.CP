;jobhs1.cp
;
RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.DEF'
RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	JOBHDR
	.INCLUDE 'DEF:RD071A.DEF'

RECORD	JOBLIN
	.INCLUDE 'DEF:RD072A.DEF'

record	jlkey2
	j2itm	,a15
	j2if1	,a3
	j2if2	,a3
	j2if3	,a5

RECORD	PRINT
	TITLE	,A21,	'OPEN FAB TICKETS'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PLINE	,A132
	PRNTON	,D1
	LINCNT	,D2,60
	PGCNT	,D6
	LPSW	,D2
	SPLFIL	,A14
	RPTNUM	,D3
	PRTTYP	,A1
	PRTCTL	,D3,080
	LPARG	,D1
	PRNTSW	,D1
	PRTCTR	,D1

RECORD	LHED
		,A*,	'TICKET ISSUED ON      QTY   '
		,A*,	"REC'D ON     QTY"

RECORD	IHED
		,A*,	' OH-HAND    ALLOCATED    ON-ORDER    ORD-UPTO    '
		,A*,	'REO-LVL   SOLD-MTD   SOLD-YTD'
RECORD	CHANNEL
	CHN041	,D2
	CHN071	,D2
	CHN072	,D2

RECORD	VARS
	OPNOK	,D1
	savrfa	,A6
	A15	,A15
	xdate	,d8
	XDATE6	,D6
	i_date	,d8
	ST_ITEM	,A15
	EN_ITEM	,A15
	ST_DATE	,D8
	EN_DATE	,D8
	st_dat6	,d6
	en_dat6	,d6
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	LOKCTL	,D1
	READ	,D1,0
	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID(V)
	XCALL OUTPT (1,1,2,'PRINT FAB TICKET HISTORY',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

DISPLA,
	XCALL OUTPT (6,4,0,'1. ITEM: AAAAAAAAAAAAAAA   THRU:',1)
	XCALL OUTPT (8,4,0,'2. DATE: XX/XX/XXXX   THRU:',1)
ST_ITEM,
	XCALL OUTPT (6,4,0,'1. ITEM: AAAAAAAAAAAAAAA   THRU:',1)
	XCALL INPUT (6,12,15,00,'AE',ENTRY,INXCTL,1)
	GOTO (DISPLA,ENDOFF),INXCTL
	ST_ITEM = ENTRY(1,15)
	IF (ST_ITEM .EQ. A15)
		BEGIN
		EN_ITEM = 'ZZZZZZZZZZZZZZZ'
		XCALL OUTPT (6,10,1,'ALL',1)
		GOTO (ANYCN),CNGCTL
		GOTO ST_DATE
		END
EN_ITEM,
	XCALL INPUT (6,39,15,00,'AE',ENTRY,INXCTL,1)
	GOTO (DISPLA,ENDOFF),INXCTL
	EN_ITEM = ENTRY(1,15)
	IF (EN_ITEM .EQ. A15)
		BEGIN
		EN_ITEM = ST_ITEM
		XCALL OUTPT (6,39,1,EN_ITEM,1)
		END
	GOTO (ANYCN),CNGCTL
ST_DATE,
	XCALL OUTPT (8,4,0,'2. DATE: XX/XX/XXXX   THRU:',1)
	XCALL INPUT (8,13,08,00,'D ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	ST_DATE = ENTRY(1,8)
	IF (ST_DATE .EQ. 0)
		BEGIN
		XCALL OUTPT (8,15,1,'ALL',1)
		EN_DATE = 99999999
		GOTO ANYCN
		END
EN_DATE,
	XCALL INPUT (8,34,08,00,'D ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	EN_DATE = ENTRY(1,8)
	IF (EN_DATE .EQ. 0)
		BEGIN
		EN_DATE = ST_DATE
		XDATE(1,4) = EN_DATE(5,8)
		XDATE(5,8) = EN_DATE(1,4)
		ENTRY(1,10) = XDATE, 'ZX/XX/XXXX'
		XCALL OUTPT (8,34,1,ENTRY(1,10),1)

		END
ANYCN,
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (PROCES,CNGBR),CNGCTL+1
CNGBR,
	GOTO (ST_ITEM, ST_DATE), WHATNO
	GOTO ANYCN
PROCES,
	FIND (CHN041, ITMMAS, ST_ITEM) [ERR=LOOP]
LOOP,
	XCALL IOS (CHN041, ITMMAS, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (ITEMNO .GT. EN_ITEM) GOTO EOF

	PLINE (1,15) = ITEMNO
	PLINE (17,19) = IF1
	PLINE (21,23) = IF2
	PLINE (25,29) = IF3
	PLINE (31,60) = DESCR
	CALL PRINT

	PLINE = IHED
	CALL PRINT

	PLINE (1,8) = QTYONH,	'ZZZZ,ZZX'
	PLINE (14,21) = QTYCOM,	'ZZZZ,ZZX'
	PLINE (27,33) = QTYONO,	'ZZZZ,ZZX'
	PLINE (38,45) = ORDUPT,	'ZZZZ,ZZX'
	PLINE (49,56) = REOLVL,	'ZZZZ,ZZX'
	PLINE (60,67) = QTYMTD,	'ZZZZ,ZZX'
	PLINE (71,78) = QTYYTD,	'ZZZZ,ZZX'
	CALL PRINT
;-
	CALL PRINT
	PLINE = LHED
	CALL PRINT
	call print

	clear jlkey2
	j2itm = itemno
	j2if1 = '999'

	find (chn072, joblin, jlkey2, keynum:1) [err=gd_loop]
gd_loop,
	reads (chn072, joblin, getrfa:savrfa) [err=gd_eof]
	if (jlitm .ne. itemno) goto gd_eof
	if (jlsts .ne. 'R') goto gd_loop
	if (jlif1 .ne. if1) goto gd_loop
	if (jlif2 .ne. if2) goto gd_loop
	if (jlif3 .ne. if3) goto gd_loop
	
	if (jlnum .ne. jhnum) 
		begin
		xcall isio (chn071, jobhdr, jlnum, read, lokctl)
		if (lokctl .ne. 0) clear jobhdr
		end

	clear pline
	pline(1,6) = jhnum,	'ZZZZZX'

	;did not start recording date recv'd until 2-25-11
	i_date = jhrldt
	if (i_date .le. 0) i_date = jhshdt
	xdate6(1,4) = i_date(5,8)
	xdate6(5,6) = i_date(3,4)

	if (i_date .lt. st_date) goto gd_loop
	if (i_date .gt. en_date) goto gd_loop

	pline (9,16) = xdate6,	'ZX/XX/XX'
	pline (19,25) = jlqty,	'ZZZ,ZZX'

	xdate6(1,4) = jlrcdt(5,8)
	xdate6(5,6) = jlrcdt(3,4)

	pline (29,36) = xdate6,	'ZX/XX/XX'
	pline (38,44) = jlrcv,	'ZZZ,ZZX'
	call print

	goto gd_loop
gd_eof,

;-

	GOTO LOOP
EOF,
	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:JOBMNU',1)
PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (PRNTON .EQ. 0) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD,HD,HD
&		,LG,LG,LG,0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;-------------------------------------------------------------
PRNTON,
	SPLFIL (5,6) = 'EF'
	LPSW = 1		;PRINT,SPOOL, OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	PRNTON = 1
	RETURN
;------------------------------------------------


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (4,'SI', 041, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 4

	SWITCH = 5
	XCALL FILES (1,'SI', 071, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN071 = 1

	SWITCH = 5
	XCALL FILES (2,'SI', 072, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN072 = 2

	OPNOK = 1

	RETURN
;--------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN041) CLOSE CHN041
	IF (CHN071) CLOSE CHN071
	IF (CHN072) CLOSE CHN072

	RETURN
;--------------------------------------------------



	
;TICKET ISSUED ON      QTY   REC'D ON     QTY
;ZZZZZX  ZX/XX/XX  ZZZ,ZZX   ZX/XX/XX ZZZ,ZZX
;1234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7


;AAAAAAAAAAAAAAA XXX XXX XXXXX AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
; OH-HAND    ALLOCATED    ON-ORDER    ORD-UPTO    REO-LVL   SOLD-MTD   SOLD-YTD
;ZZZZ,ZZX     ZZZZ,ZZX    ZZZZ,ZZX    ZZZZ,ZZX   ZZZZ,ZZX   ZZZZ,ZZX   ZZZZ,ZZX
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7         8
