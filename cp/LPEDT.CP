;LPedt.CP
;	11-4-99 currently just used for inventory printout
SUBROUTINE LPedt; (LPSW,SPLFIL)  ;SMC VERSION
;
;	print to notpad...

	LPSW	,D
	SPLFIL	,A



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

RECORD	VARS
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	OPT	,D1
	I	,D5

PROC
	OPT = 4

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

	s_name(1,1) = OPT,'X'           ;save the printer # selected
	splfil = s_name                 

	
	LPSW = OPT		;5-20-98
	RETURN

spl_err,
	offerror
	lpsw = 0		;error or <end>
	RETURN
END

