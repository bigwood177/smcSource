;OE10.NEW
SUBROUTINE OE10
;
;	OE10.COP - DISPLAYS DUCTWORK SCREEN
;
PROC
	XCALL OUTPT (01,01,2,'   0. Default         (1=On, 2=Off)',1)
	XCALL OUTPT (02,01,0,'   1. Prsr Class   ',1)
	XCALL OUTPT (03,01,0,'   2. Type         ',1)
	XCALL OUTPT (04,01,0,'   3. Cat          ',1)
	XCALL OUTPT (05,01,0,'   4. Style        ',1)
	XCALL OUTPT (06,01,0,'   5. Seam         ',1)
	XCALL OUTPT (07,01,0,'   6. Sealant      ',1)
	XCALL OUTPT (08,01,0,'   7. Qty/Joints  ______ ',1)
	XCALL OUTPT (09,01,0,'   8. Size        ',1)
	XCALL OUTPT (10,01,0,'   9. Gauge       __    ',1)
	XCALL OUTPT (11,01,0,'  10. Liner       _ ',1)
;;;	XCALL OUTPT (12,01,0,'  11. Reinforce      _',1)
;;;	XCALL OUTPT (13,01,0,'  12. Location    _______________',1)
;;;	XCALL OUTPT (13,40,0,'13. Punch ',1)
;;;	XCALL OUTPT (14,01,0,'  14. Access         _   ',1)

;;;	XCALL OUTPT (15,01,0,'  15. Slips       ',1)
;;;	XCALL OUTPT (16,01,0,'  16. Drives      ',1)
;;;	XCALL OUTPT (15,01,0,'  17. TDC Corners ',1)
;;;	XCALL OUTPT (16,01,0,'  18. TDC N/B     ',1)
;;;	XCALL OUTPT (17,01,0,'  19. TDC Gasket  ',1)
;;;	XCALL OUTPT (18,01,0,'  20. TDC Cleats  ',1)
;;;	XCALL OUTPT (19,01,0,'  21. TDC Bar     ',1)
;;;	XCALL OUTPT (20,01,0,'  22. TDC Bar N/B ',1)
;;;	XCALL OUTPT (19,01,0,'  23. Tierods(1)  ',1)
;;;	XCALL OUTPT (20,01,0,'  24. Tierods(2)  ',1)
;;;	XCALL OUTPT (21,01,0,'  25. Holes       ',1)
	RETURN
END
