;dd_old: prior to 04/20/2020

;DDEPT - "compute" ducacc dept from gauge & length
; 4'/5' 20,22,24,26ga galv = "P", all others "O"
;
; 3-30-2020 ssq: no more dept "P", everything is "O"
SUBROUTINE DD_old
	GAUGE	,D	;gauge
	SIZE3	,D	;length
	DUTYPE	,D	;material, 1=galv
	DEPT	,A	;"O" or "P", depending
	STY	,D	;STYLE
;
PROC

;;;	DEPT = 'O'		;3-30-20
;;;	XRETURN			;3-30-20
;------------------------------------------------


	CLEAR DEPT

; 10-27-14: back to old definition:
;
	USING STY SELECT
	(1,4,5),	DEPT = 'P'
	(2,3,6),	DEPT = 'O'
	ENDUSING

	XRETURN
;---------------------------------------------


	DEPT = 'O'
	USING GAUGE SELECT
	(20,22,24,26),	BEGIN
			USING SIZE3 SELECT
			(4,5),	IF(DUTYPE.EQ.1) DEPT = 'P'
			ENDUSING
			END

	ENDUSING

	RETURN
END
