;advic3.cp	isam
;
;	CUSTOM FOR SHEET METAL I/M SYSTEM
;
RECORD ITMMAS
	.INCLUDE 'DEF:RD041A.def'

RECORD HDR1
		,A50,'---------------- ITEM -------------------------   '
		,A50,'    QTY      -QTY      +QTY   -REORDR      =NET   '
		,A32,'           NOTES                '
RECORD HDR2
		,A50,'NUMBER           DESCRIPTION                      '
		,A50,' ONHAND    COMMIT    ONORDR     POINT     AVAIL   '
		,A11,'           '
	LINE	,A21,'_____________________'


RECORD	VARS
	ZRO	,A5,	'00000'
	LPARG	,D1
	V	,D1
	READ	,D1,0
	LOKCTL	,D1
	SWITCH	,D1
	TITLE	,A*,	'STOCKED ITEMS REPORT'
	PLINE	,A132
	LINCNT	,D2,	60
	PGCNT	,D3
	RPTNUM	,D3
	PRTTYP	,A1
	LPSW	,D1
	SPLFIL	,A14
	RECNO	,D5
	MASK	,A8,	'ZZZ,ZZZ-'
	DECMAL	,D18
;
PROC 
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'OUT OF STOCK REPORT',1)
OPENS,
	XCALL FILES (1,'SI',041,SWITCH)		;FILE # 41 - ITMMAS
	IF (SWITCH.EQ.9) GOTO CLOSE1
	SPLFIL (5,6) = 'EA'
	LPSW = 1				;PRINTER: PRINT SPOOL OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GO TO CLOSE2
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
RDHDR,
	READ (1, ITMMAS, ^FIRST) [ERR=EOF]	;HEADER
RDLOOP,
	LOKCTL = 1
	XCALL IOS (1, ITMMAS, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF

	IF (USRDEF .NE. 'I') GOTO RDLOOP

	PLINE (1,15) = ITEMNO
	PLINE (19,47) = DESCR
	PLINE (51,58) = QTYONH, MASK
	PLINE (61,68) = QTYCOM, MASK
	PLINE (71,78) = QTYONO, MASK
	PLINE (81,88) = REOLVL, MASK
	PLINE (91,98) = DECMAL, MASK
	PLINE (101,108) = ORDUPT, MASK
	CALL PRINT
	GOTO (RDLOOP),INN

	IF (IF1.NE.ZRO .OR. IF2.NE.ZRO .OR. IF3.NE.ZRO)
		BEGIN
		PLINE(1,3) = IF1
		PLINE(5,7) = IF2
		PLINE(9,13) = IF3
		CALL PRINT
		CALL PRINT
		END

	GOTO RDLOOP
PRINT,
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HDR1,HDR2,' ','NO LEGEND',
&		' ',' ',1,0,132,0,LPSW,RPTNUM,PRTTYP)
;;;&		' ',' ',2,0,132,0,LPSW,RPTNUM,PRTTYP)
	RETURN
EOF,
	XCALL LPOFF (LPSW,SPLFIL,PGCNT)
CLOSE2,
	XCALL FILES (1,'I',41,4)
CLOSE1,
	XCALL FILES (2,'I',42,4)
CLOSE0,
	XCALL PGCHN ('CP:IMMENU',1)
END
;---------------- ITEM -------------------------       QTY       QTY       QTY       NET    REORDR     ORDER    NOTES
;NUMBER           DESCRIPTION                       ONHAND    COMMIT    ONORDR     AVAIL     POINT     UP TO    ---------------------
;XXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   XXX,XXX-  XXX,XXX-  XXX,XXX-  XXX,XXX-  XXX,XXX-  XXX,XXX-   _____________________
;123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6


