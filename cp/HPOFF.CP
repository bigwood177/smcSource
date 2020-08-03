SUBROUTINE HPOFF ;(LPSW,SLPFIL,PGCNT)	
; This version of LPOFF will print the invoice spool file to
; the default printer tray, then switch trays and send the 
; same spool file to the printer a second time.
;
;	Note: the que_name is saved in a disk file.
;
;	7-10-03 ssq: allow for SMP printer
;	10-04-05 ssq: print rockford invoices at smc...

	LPSW	,D
	SPLFIL	,A
	PGCNT	,D

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EXTERNAL FUNCTION		;ssq 7-10-03 ssq
;;;	GU_NAME	,A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

record  s_filnam
		,a4,    'spl:'
		,a1,    's'
	fter    ,d3             ;terminal #
	ftime   ,a4             ;hhmm of current time
		,a4,    '.spl'
record,x
		,a4
	s_name          ,a12


RECORD	INVQUE
	QUE_NAME	,A40


RECORD	TRAY_2
		,A3		;<ESC>&l
	PAR	,A1		;"1"	;select tray 2
		,A1		;"H"	;select tray 2


RECORD	VARS
	CMPCOD	,A3
	QUEN	,A40
	U_NAME	,A25
	LN	,D2
	ENTRY	,A1
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	ROW	,D2
	I	,D3
	OPT	,D1

PROC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	U_NAME = %GU_NAME	;ssq 7-10-03
;;;	UPCASE U_NAME		;ssq 7-10-03
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (LPSW.EQ.0) RETURN

	s_name = splfil         ;restore printer output file name
	s_name(1,1) = 's'       ;always first char of printer output file name

; get the que_file name for the hp invoice printer...
	close 14

; 10-5-16 ssq - print to local printer...
;;;	OPEN (14,I,'UT:INVQUE.HP')
;;;	READS (14, INVQUE)
;;;	CLOSE 14
;;;	QUEN = QUE_NAME

	lpque(s_filnam,copies:1,delete)	;invoic batch

; 2-15-10: NEW PRINTER FOR SMC
; 4-19-07: skip this, all companies printing to the same printer...
;;;	quen = 'OKI_AR'		;SQ 2-18-10
;;;	quen = 'OKI_SCAN'		;SQ 8-1514
	
;;;	xcall who(cmpcod)
;;;	USING CMPCOD SELECT
;;;	('SMC'), QUEN = 'OKI_AR'	;ssq 2-15-10
;;;	('ROC'), QUEN = 'BLUE'		;SSQ 1-06-04
;;;	ENDUSING
; 4-19-07: skip this, all companies printing to the same printer...

;;;	LN = %TRIM(QUEN)

;;;	lpque(s_filnam,copies:1,lpnum:quen(1,ln),delete)	;invoic batch
;;;	lpque(s_filnam,copies:1,lpnum:quen(1,ln))	;invoic batch

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1-11-05 ssq: remove, no longer printing pink copy
;;;	OPEN (14, U, S_FILNAM)			;switch to tray 2
;;;	READ (14, TRAY_2, 1)
;;;	PAR = "1"				;TRAY 2
;;;	WRITE (14, TRAY_2, 1)
;;;	CLOSE 14
;;;
;;;	lpque(s_filnam,copies:1,lpnum:quen(1,ln),delete) ;invoice batch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	LPSW =
	RETURN


END
