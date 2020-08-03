;aumore.ar
;
;	more automatic month end reports
;
;	a/r detail aging (a_artbal)
;	a/r summary aging (a_artbl2)
;	a/r distribution (a_disrpt: detail, summary)
;		SMC, ROC, CAT
;
;
;aumail.ar
;
;	auto mail month end reports
;
record	au_header
		,a*,	'MONTH END REPORTS'
		,a3
	adate	,a10

RECORD	RPTDIR
		,a28,	'\\serversbs\smc\Reports\smc\'
	RPTMON	,A3
		,A1,	'\'
	RPTRPT	,A69

RECORD	RPTDI2
		,a28,	'\\serversbs\smc\Reports\smc\'
	RPTMO2	,A3
		,A1,	'\'
	RPTRP2	,A69


RECORD	EJECT
	E_CHAR	,A1
		,A4,	'&l0H'

RECORD	RESET
	R_CHAR	,A1
		,A1,	'E'


RECORD	COMP14
	O_CHAR	,A1		;<ESC>
		,a4,	"&l1O"	;landscape mode
	ESC_12	,A1		;<ESC>
		,A4,	"&l8D"	;vertical spacing, 8 lines/inch
	ESC_13	,A1		;<ESC>
		,A8,	"(s13.00H"	;pitch 14 CPI




RECORD
	MON,	12A3,	'JAN','FEB','MAR','APR','MAY','JUN',
&			'JUL','AUG','SEP','OCT','NOV','DEC'
		

RECORD
	WDATE	,D8
RECORD,X
	WCC	,D2
	WYY	,D2
	WMM	,D2
	WDD	,D2

record	vars
	xdate	,d8
	i	,d6
	sal_man	,4d2,	01,04,16,25	;Leon, Don G, Ryan B, James O
	maxman	,d2,	04

	SPLFIL	,A*,	'c:\smc\spool\aumail.spl'
	SPLFI2	,A*,	'c:\smc\spool\aumai2.spl'
	c_mail	,a100
	c_subj	,a100
	attr	,4a100	;up to 4 attachments
	entry	,a30
	inxctl	,d1
	cngctl	,d1
	whatno	,d2
	row	,d2
	a2	,a2
	tl	,d3
	tl2	,d3
	v	,d1
;
proc
	xcall terid (v)

displa,
	xcall outpt (1,1,2,'MONTH END REPORTS',1)
	XCALL OUTPT (12,5,1,'MONTH END DATE: ',1)
	XCALL INPUT (12,23,08,00,'DE',ENTRY,INXCTL,1)
	GOTO (DISPLA,ENDOFF),INXCTL

	wdate = entry(1,8)

	cngctl = 2
	xcall anycn (cngctl, whatno)
	goto (displa),cngctl

	xdate (1,4) = wdate(5,8)
	xdate(5,8) = wdate(1,4)
	adate = xdate,	'ZX/XX/XXXX'
	xcall outpt (1,1,2,au_header,1)

	row = 1
	RPTMON = MON(WMM)	;for reports directory
	RPTMO2 = RPTMON

;=============== Create the Report PDF files ==================

;A/R Aging summary...
	xcall a_artbal (row, wdate, splfil)

	RPTRPT = 'artbal.pdf'
	TL = %TRIM(RPTDIR)
	xcall mkpdf (splfil,rptdir(1,tl))

	dtlrpt = 'Y'
	xcall a_disrpt (row, wdate, splfil, dtlrpt)
	RPTRPT = 'disrpt.pdf'
	TL = %TRIM(RPTDIR)
	xcall mkpdf (splfil,rptdir(1,tl))

	dtlrpt = 'N'
	xcall a_disrpt (row, wdate, splfil)
	RPTRPT = 'DISRPT.pdf'
	TL = %TRIM(RPTDIR)
	xcall mkpdf (splfil,rptdir(1,tl))

endoff,
	XCALL PGCHN ('AR:ARSFMN',1)
	stop
	end


