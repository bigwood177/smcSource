;LPOFF.CP
SUBROUTINE CP_LPOFF ;(LPSW,SLPFIL,PGCNT,QUE_NAME)	;smc version
;
; 11-4-99 THIS VERSION FOR PRINTING INVENTORY ONLY
; 02-08-18: CHANGED TO CP_LPOFF FOR VS
;
; Print selections are hard-coded into LPON.MAN and this routine.
; Current selections are:
;	1. Local Printer
;	2. Local Printer (Compressed)
;	3. Local Printer (Landscape)
;	4. Edit
;	5. HP 8100
;	6. HP 8100 Compressed
;	7. HP 8100 Landscape

;	8. = print to local printer

	LPSW	,D
	SPLFIL	,A
	PGCNT	,D
	QUE_NAME,A

record  s_filnam
		,a4,    'spl:'
		,a1,    's'
	fter    ,d3             ;terminal #
	ftime   ,a4             ;hhmm of current time
		,a4,    '.spl'
record,x
		,a4
	s_name          ,a12


RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT
RECORD	RESET
	R_CHAR	,A1
		,A1,	'E'

RECORD	PO			;page orientation
	O_CHAR	,A1		;ESC
		,A4,	'&l0O'	;portrait
RECORD	CPI
	C_CHAR	,A1
		,A4,	'&k0S'	;6 CPI

RECORD	NOBOLD
	B_CHAR	,A1
		,A4,	'(s0B'	;normal

RECORD	VARS
	que_passed	,d1
	QUEN	,A40
	LN	,D2
	ENTRY	,A1
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	ROW	,D2
	I	,D3
	OPT	,D1

PROC
	IF (LPSW.EQ.0) RETURN

	XCALL ASCII(27,O_CHAR)
	C_CHAR = O_CHAR
	B_CHAR = O_CHAR
	E_CHAR = O_CHAR
	R_CHAR = E_CHAR
;;;	CLOSE 14

	IF (^PASSED(QUE_NAME))
	THEN	BEGIN
		QUEN = QUE_NAME
		LN = %TRIM(QUEN)
		QUE_PASSED = 1
		END
	ELSE	QUE_PASSED = 0

	s_name = splfil         ;restore printer output file name
	OPT = s_name(1,1)	;printer number
	s_name(1,1) = 's'       ;always first char of printer output file name


	USING OPT SELECT
;;;	(1,2,3),	begin	; Local laser 
	(1,2,3,8),	begin	; Local laser 
			DISPLAY(14,EJECT)
			DISPLAY(14,RESET)
		;;;	DISPLAY(14,PO)
		;;;	DISPLAY(14,CPI)
		;;;	DISPLAY(14,NOBOLD)
			CLOSE 14
			if (que_passed) 
			then	lpque(s_filnam,copies:1,lpnum:quen(1,ln),delete)
			else	lpque (s_filnam,copies:1,delete)
			end
	(4),	BEGIN			;EDIT
		close 14
		xcall spedit (s_filnam)
		xcall delet (s_filnam)
		END

	(5,6,7),	begin	; HP 8100
		DISPLAY(14,EJECT)
		DISPLAY(14,RESET)
	;;;	DISPLAY(14,PO)
	;;;	DISPLAY(14,CPI)
	;;;	DISPLAY(14,NOBOLD)
		CLOSE 14
		lpque (s_filnam,lpnum:"HP LaserJet 8100 N PCL 5e",copies:1,delete)
		end

	ENDUSING

	LPSW =
	RETURN


END
