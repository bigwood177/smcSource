this program never used
; create dptsts records when fab tickets printed
; will not overwrite existing records


SUBROUTINE MKJDS
	jobno	,D	;OORNO


RECORD	JOBHDR
	.INCLUDE 'DEF:RD071A.DEF'


RECORD	DPTSTS
	.INCLUDE 'DEF:RD193A.DEF'


;
RECORD	VARS
	CHN193	,D3
	WD_DEPT	,A2
	IS_VULCAN	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	SWITCH	,D1
PROC

	CHN193 = %SYN_FREECHN(50,99)	;GET NEXT FREE CHAN
	
	SWITCH = 5
	XCALL FILES (CHN193, 'SU', 193, SWITCH)
	IF (SWITCH .eq. 9) XRETURN

;6-20-17 add record for header

	CLEAR DPTSTS
	S_ORDN = jobno
	S_DEPT = '**'		;header
	XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)


; create a DPTSTS record for the fab ticket.  If there are
; already records in DPTSTS file, they will not be over-written.
	CLEAR DPTSTS
	S_ORDN = jobno


	S_DEPT(1,1) = jhdept

	XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)

ENDOFF,

	IF (CHN193) CLOSE CHN193

	XRETURN

