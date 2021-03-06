; TBLMNU / COP 
;
;
;		COP TABLES MENU
; 28-mar-2016 ssq: complete seal table (S1) not used.  Replace ss1mnt w/ ss2mnt in menu.

EXTERNAL FUNCTION
	GU_NAME	,A

record	copsec
	sc_user	,a10
		,a1
	sc_pgm	,a10
		,a1
	sc_dt	,a30
;
record	xvars
	dt	,d20


record	funkey
	.include 'def:funkey.def'

RECORD
	V	,D1
	f_key	,d3
	ENTRY	,A3
	INXCTL	,D1
	PGM	,D2
	PROGNM	,52A9,	'CP:DUCMNU','CP:LINPRC','CP:DUCPRT','CP:LINPRT',
&			'CP:PRCMNU','CP:TSCMNT','CP:TRSMNT','CP:MM2MNT',
&			'CP:MMDEPT','CP:SITMNT','CP:PCHPCH','CP:FOMENU',
&			'CP:SMPMNU','CP:PARMNT','CP:CHILD ','CP:CFGMNT',
&			'CP:COOMNT','CP:DSZMNT','CP:EXLMNT','CP:TBCMNT',
&			'CP:TRTMNT','CP:TFCMNT','CP:PDTMNT','CP:TEIMNT',
&			'CP:DIKMNT','CP:PPXMNT','CP:PCTMNT','CP:VULMNU',
&			'CP:RCTMNT','CP:RCTMN2','CP:strmnt','CP:MLBMNT',
&			'CP:EmMMNu','CP:TR3MNT','CP:TR2MNT','CP:LNTMNT',
&			'CP:CLRMNT','CP:FAKMNT','CP:SS2MNT','CP:SSDMNT',
&			'cp:PRQMNT','cp:blnmnt','cp:aqnmnt','cp:srnmnt',
&			'cp:adpmnt','CP:RM1MNT','cp:zbrmnt','cp:zedmnt',
&			'CP:EXPMNT','CP:SLPMNT','CP:QE2MNU','CP:PREMNT'


PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'CUSTOMER ORDER PROCESSING - TABLES',V)
PAGE_1,
	XCALL OUTPT (3,9,0,'PLEASE SELECT APPLICATION <Page Dn> for more',1)
	XCALL OUTPT ( 4,02,0,' 1. DUCT PRICE TABLE',1)
	XCALL OUTPT ( 5,02,0,' 2. LINER PRICE TABLE',1)
	XCALL OUTPT ( 6,02,0,' 3. PRINT DUCT PRICE TABLE',1)
	XCALL OUTPT ( 7,02,0,' 4. PRINT LINER PRICE TABLE',1)
	XCALL OUTPT ( 8,02,0,' 5. INVENTORY PRICING TABLES',1)
	XCALL OUTPT ( 9,02,0,' 6. CARRIER TABLE',1)
	XCALL OUTPT (10,02,0,' 7. TERRITORIES FOR A SALESMAN',1)
	XCALL OUTPT (11,02,0,' 8. MEMOS',1)
	XCALL OUTPT (12,02,0,' 9. DEPT/OPERATIONS ',1)
	XCALL OUTPT (13,02,0,'10. STOCKED PARTS',1)
	XCALL OUTPT (14,02,0,'11. PRESSURE CHART TABLE',1)
	XCALL OUTPT (15,02,0,'12. FLAT OVAL TABLES',1)
;;;	XCALL OUTPT (15,02,0,'12. SALESMAN PRINTER MAILBOXES',1)

	XCALL OUTPT (16,02,0,'13. SMP PRICING TABLES',1)
;;;	XCALL OUTPT (16,02,0,'13. SINGLE BLADE DAMPER PRICE TABLE',1)

	XCALL OUTPT (17,02,0,'14. PARENT ITEMS',1)
	XCALL OUTPT (18,02,0,'15. COMPONENT ITEMS',1)
;;;	XCALL OUTPT (17,02,0,'14. BACKDRAFT DAMPER PRICE TABLE',1)
;;;	XCALL OUTPT (18,02,0,'15. LOUVER PRICE TABLE',1)

	XCALL OUTPT (19,02,0,'16. GENERAL CONFIG PARAMETERS',1)
	XCALL OUTPT (20,02,0,'17. COMPANY INFO',1)
	XCALL OUTPT (21,02,0,'18. DAMPER SIZES',1)
	XCALL OUTPT (22,02,0,'19. EXCLUDED PARTS',1)
	XCALL OUTPT (23,02,0,'20. BARCODE PATH',1)

	XCALL OUTPT ( 4,42,0,'21. QUOTE EXPRESS ROUTINES TABLE',1)
	XCALL OUTPT ( 5,42,0,'22. QUOTE EXPRESS PRESSURE CHART',1)
	XCALL OUTPT ( 6,42,0,'23. PRICE DUCT INFO',1)
	XCALL OUTPT ( 7,42,0,'24. EMPLOYEE INITIALS',1)
	XCALL OUTPT ( 8,42,0,'25. DEFAULT ITEM KEY',1)
	XCALL OUTPT ( 9,42,0,'26. PRESSED PARTS X-REF',1)
	XCALL OUTPT (10,42,0,'27. PHY CAT %',1)
	XCALL OUTPT (11,42,0,'28. VULCAN TABLES',1)
	XCALL OUTPT (12,42,0,'29. RECT KEYS',1)
	XCALL OUTPT (13,42,0,'30. RECT VALUES',1)
	XCALL OUTPT (14,42,0,'31. SQUARE TO ROUND',1)
	XCALL OUTPT (15,42,0,'32. COST/LB TABLE',1)
	XCALL OUTPT (16,42,0,'33. EMAIL ADDRESSES',1)
	XCALL OUTPT (17,42,0,'34. PRICE CODES',1)
	XCALL OUTPT (18,42,0,'35. PRICE CODE MULTIPLIER TABLE',1)
	XCALL OUTPT (19,42,0,'36. FNOTES TO PRINT PRINT LOCAL',1)
	XCALL OUTPT (20,42,0,'37. SMC STANDARD COLORS',1)
	XCALL OUTPT (21,42,0,'38. FAKE ITEM X-REF',1)
	XCALL OUTPT (22,42,0,'39. COMPLETE SEAL PARAMETERS',1)
	XCALL OUTPT (23,42,0,'40. COMPLETE SEAL KEYS',1)
	xcall outpt (24,42,0,'41. PRINTER INFO',1)
INPUT,
	XCALL INPTK (3,55,2,0,'#E',ENTRY,INXCTL,1,F_KEY)
	GOTO (INPUT,ENDOFF), INXCTL

	USING F_KEY SELECT
	(PAGE_DN),	GOTO PAGE_2
	(PAGE_UP),	GOTO PAGE_2
	ENDUSING

	PGM = ENTRY(1,2)
	USING PGM SELECT
	(.LT.1), GOTO INPUT
	(.GT. 41), GOTO INPUT
	ENDUSING

;;;	IF (PGM.LT.1.OR.PGM.GT.41) GOTO INPUT
	XCALL WATE (3,V)
;-

	onerror no_file
	open (1,a,'smc:copsec.dat')
	offerror
	goto file_exists
no_file,
	goto old_menu
file_exists,
	dt = %datetime
	sc_dt = dt,	'XXXX-XX-XX ZX:XX:XX XXXXXX'
	sc_user = %GU_NAME
	sc_pgm = prognm(pgm)
	writes (1, copsec)
	close 1
;-
old_menu,
	XCALL PGCHN (PROGNM (PGM),1)

page_2,
	XCALL OUTPT (1,1,2,'CUSTOMER ORDER PROCESSING - TABLES',V)
	XCALL OUTPT (3,9,0,'PLEASE SELECT APPLICATION <Page Dn> = Back',1)
	XCALL OUTPT ( 4,02,0,' 1. BILLING NOTES TABLE',1)
	XCALL OUTPT ( 5,02,0,' 2. QUOTE ARCHIVE REASON TABLE',1)
	XCALL OUTPT ( 6,02,0,' 3. SERVER NAME & IP ADDRESS',1)
	XCALL OUTPT ( 7,02,0,' 4. ADOBE READER PATH',1)
	XCALL OUTPT ( 8,02,0,' 5. RAW MATERIAL TYPES',1)
	XCALL OUTPT ( 9,02,0,' 6. RM TAG ZEBRA PRINTERS',1)
	XCALL OUTPT (10,02,0,' 7. RM EDIT LIST PRINTER',1)
	XCALL OUTPT (11,02,0,' 8. EXPRESS PARTS TABLE',1)
	XCALL OUTPT (12,02,0,' 9. AUTO PRINT SHIPPING LABELS',1)
	XCALL OUTPT (13,02,0,'10. PRICE DUCT TABLES',1)
	XCALL OUTPT (14,02,0,'11. PRE FAB PARTS TABLE',1)

INPUT2,
	XCALL INPTK (3,55,2,0,'#E',ENTRY,INXCTL,1,F_KEY)
	GOTO (INPUT2,ENDOFF), INXCTL

	USING F_KEY SELECT
	(PAGE_DN),	GOTO PAGE_1
	(PAGE_UP),	GOTO PAGE_1
	ENDUSING

	PGM = ENTRY(1,2)
	PGM = PGM + 41
	IF (PGM.LT.1.OR.PGM.GT.52) GOTO INPUT
	XCALL WATE (3,V)
;-

	onerror no_file2
	open (1,a,'smc:copsec.dat')
	offerror
	goto file_exists2
no_file2,
	goto old_menu2
file_exists2,
	dt = %datetime
	sc_dt = dt,	'XXXX-XX-XX ZX:XX:XX XXXXXX'
	sc_user = %GU_NAME
	sc_pgm = prognm(pgm)
	writes (1, copsec)
	close 1
;-
old_menu2,
	XCALL PGCHN (PROGNM (PGM),1)
	
ENDOFF,
	XCALL WATE(3,V)
	XCALL PGCHN ('CP:SPCFUN',1)
END
