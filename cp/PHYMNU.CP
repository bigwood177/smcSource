;PHYMNU.CP
;
;
RECORD
	V	,D1
	ENTRY	,A3
	INXCTL	,D1
	PGM	,D2
	PROGNM	,18A9,	'CP:PSTGUN','CP:PHYENT','CP:PHYDEP','CP:PHYMNT',
&			'CP:PRTBAR','CP:PH1RPT','CP:PH2RPT','CP:PHYCLR',
&			'cp:prtbr2','CP:PRTBRZ','CP:PSTPHY','CP:PSTPH2',
&			'IM:RMMVAL','IM:RMMXCP','IM:RMMPST','IM:RMMMNT',
&			'IM:RMMADD','CP:PH2LBS'

PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'PHYSICAL INVENTORY',V)
	XCALL OUTPT (3,9,0,'PLEASE SELECT APPLICATION',1)
	XCALL OUTPT ( 4,02,0,' 1. POST SCANNED DATA',1)
	XCALL OUTPT ( 5,02,0,' 2. MANUAL INVENTORY INPUT',1)
	XCALL OUTPT ( 6,02,0,' 3. WIP EXTRACTION',1)
	XCALL OUTPT ( 7,02,0,' 4. CORRECT SCANNED DATA',1)
	XCALL OUTPT ( 8,02,0,' 5. PRINT INVENTORY BARCODE SHEETS',1)
	XCALL OUTPT ( 9,02,0,' 6. INVENTORY VALUATION',1)
	XCALL OUTPT (10,02,0,' 7. INVENTORY VALUATION BY PRD CAT',1)
	XCALL OUTPT (11,02,0,' 8. CLEAR PHYSICAL INVENTORY FILE',1)
	xcall outpt (12,02,0,' 9. SPECIAL BARCODE SHEETS',1)
	xcall outpt ( 4,42,0,'10. NO NOTE BARCODE SHEETS',1)
	XCALL OUTPT ( 5,42,0,'11. POST PHYSICAL INVENTORY',1)
	XCALL OUTPT ( 6,42,0,'12. POST CYCLE COUNT TO ITMMAS',1)

	XCALL OUTPT ( 7,42,0,'13. PRINT RM VALUATION',1)
	XCALL OUTPT ( 8,42,0,'14. PRINT RM EXCEPTIONS',1)
	XCALL OUTPT ( 9,42,0,'15. POST RM PHYSICAL INVENTORY',1)
	XCALL OUTPT (10,42,0,'16. CORRECT RM SCANNED DATA',1)
	XCALL OUTPT (11,42,0,'17. ADD RM TAGS',1)
	XCALL OUTPT (12,42,0,'18. INVENTORY POUNDS BY PRD CAT',1)
INPUT,
	XCALL INPUT (3,36,2,1,'#E',ENTRY,INXCTL,1)
	GOTO (INPUT,ENDOFF), INXCTL
	PGM = ENTRY(1,2)
	IF (PGM.LT.1.OR.PGM.GT.18) GOTO INPUT
	XCALL WATE (3,V)
	XCALL PGCHN (PROGNM (PGM),1)
ENDOFF,
	XCALL WATE(3,V)
	XCALL PGCHN ('CP:IMMENU',1)
END

