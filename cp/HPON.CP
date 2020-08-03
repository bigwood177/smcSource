SUBROUTINE HPON; (LPSW,SPLFIL)  ;SMC VERSION
;
			;
; 3-1-99 SSQ: opens the HP LaserJet 8100 N printer connected to
;		the server.


	LPSW	,D
	SPLFIL	,A

EXTERNAL FUNCTION		;ssq 7-10-03 ssq
	GU_NAME	,A

record  s_filnam
		,a4,    'spl:'
	f_letr	,a1,    's'
	fter    ,d3             ;terminal #
	ftime   ,a4             ;hhmm of current time
		,a4,    '.spl'
record,x
		,a4
	s_name          ,a12

record  time
	hh      ,a2
	mm      ,a2
	ss      ,a2

RECORD	TRAY_3
	C_CHAR	,A1		;<ESC>
		,A2,	"&l"
	PAR	,A2,	"5H"	;select tray 3


RECORD	VARS
	cmpcod	,a3
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	U_NAME	,A25
	OPT	,D1
	I	,D5

PROC
	U_NAME = %GU_NAME	;ssq 7-10-03
	UPCASE U_NAME		;ssq 7-10-03
	xcall who(cmpcod)	;ssq 2-15-10
PROCES,
	;;; Create a unique file name and open for output
	xcall tnmbr (fter)
	xcall time (time)
	ftime(1,2) = hh
	ftime(3,4) = mm

	f_letr = "s"
	onerror spl_err
	open (14,o,s_filnam)            ;print output file
	offerror

	XCALL ASCII(27,C_CHAR)
	PAR = "5H"			;VERTICAL SPACING
	IF (U_NAME .EQ. 'SMP') par = "4H"	;tray 1

	using cmpcod select
	('SMC','TST'),	par = "4H"		;tray 1
	endusing

; 11-15-10: dianna's printer trying to print from aux tray - try this...
;;;	WRITES (14, TRAY_3)		;1ST LINE OF SPOOL FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	PAR = "8C"			;VERTICAL SPACING
	WRITES (14, TRAY_3)		;1ST LINE OF SPOOL FILE

	SPLFIL = S_NAME
	LPSW = 1
	RETURN

spl_err,
	offerror
	lpsw = 0		;error or <end>
	RETURN
END
