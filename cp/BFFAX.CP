SUBROUTINE BFFAX
	F_INFO	,A	;FAXINFO
	F_SPOOL	,A	;FXSPOOL
	F_CHAN	,D	;CHANNEL TO OPEN FXSPOOL ON
	C_CODE	,A	;COMPANY CODE -SSQ 2-17-04

;RTF  STUFF...
	.INCLUDE 'DEF:RTF.DEF'

RECORD	FAXINFO
	.INCLUDE 'DEF:FAXINFO.DEF'
;
RECORD	
	FFONE	,D10
RECORD,X
	FF_AREA	,D3
	FF_EXCH	,D3
	FF_NUM	,D4

RECORD	PBUF
	PLINE	,A132

RECORD	VARS
	FAX_STRING	,A200
	F_WORK		,A10
	F_NUMBER	,A11
	LINCNT	,D3
	LFEED	,D2
	A2	,A2

PROC
	OPEN (F_CHAN, O, F_SPOOL)
	FAXINFO = F_INFO

;;;	fc_fax = 7635721100		;for testing

	FFONE = FC_FAX
	
	USING FF_AREA SELECT
	(0, 612, 651, 763, 952),	F_NUMBER = FC_FAX		;LOCAL
	(),		BEGIN
			F_WORK = FC_FAX			;DEC TO ALPHA
			F_NUMBER = "1" + F_WORK		;LONG DISTANCE
			END
	ENDUSING

	CLEAR FAX_STRING
	FAX_STRING = "::" + FC_CUSNAME(1,%TRIM(FC_CUSNAME))
&		+ "," + FC_CUSNAME(1,%TRIM(FC_CUSNAME)) + ","
&		+ F_NUMBER(1,%TRIM(F_NUMBER))

	
	WRITES (F_CHAN,FAX_STRING(1,%TRIM(FAX_STRING)) )
;;;	writes (F_CHAN,'::cOMPUTER sERVICES,sHERWOOD qUIRING,5721100')

	USING C_CODE SELECT
	('CAT'),	writes (F_CHAN,'::h, c=catORDER.rtf')
	('SMC'),	writes (F_CHAN,'::h, c=smcORDER.rtf')
	('TST'),	writes (F_CHAN,'::h, c=smcORDER.rtf')
	('ROC'),	writes (F_CHAN,'::h, c=rocORDER.rtf')
	ENDUSING

	XCALL RTF(F_CHAN, START_RTF)

	LFEED = 9
	A2 = LFEED, 'ZX'
	XCALL RTF(F_CHAN, LN_FEED, A2)

	CALL PRINT
	CALL PRINT

	LFEED = 4
	A2 = LFEED, 'ZX'
	XCALL RTF(F_CHAN, LN_FEED, A2)

	PLINE = 'To be removed from Auto Confirmation, check "Remove From List",'
	CALL PRINT
	PLINE = 'and fax this cover page to above number, or phone our office'
	CALL PRINT

	LFEED = 2
	A2 = LFEED, 'ZX'
	XCALL RTF(F_CHAN, LN_FEED, A2)
	PLINE = '____ Remove from list'
	CALL PRINT

;;;	LFEED = 14
	LFEED = 12	;ssq 2-1-5
	A2 = LFEED, 'ZX'
	XCALL RTF(F_CHAN, LN_FEED, A2)

	RETURN	
;--------------------------------------------


print,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; set up the print parameters
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL RTF(F_CHAN, LPRINT, PLINE)
	INCR LINCNT
	RETURN
;--------------------------------------------
	END

