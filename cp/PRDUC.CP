SUBROUTINE PRDUC
	OORDNO	,D		;ORDER NUMBER
	OCUSNM	,A		;CUSTOMER NAME


	.INCLUDE 'DEF:RD175D.DEF'

record	coptbl		;2-22-17
	.include 'def:rd182a.def'

RECORD	EJECT
	E_CHAR	,A1		;<ESC>
		,A4,	"&l0H"	;PAGE EJECT
RECORD	RESET
	R_CHAR	,A1
		,A1,	'E'

RECORD	PORT
	P_CHAR	,A1
		,a4,	"&l0O"	;Portrait mode

RECORD	COMP14
	O_CHAR	,A1		;<ESC>
		,a4,	"&l1O"	;landscape mode
;;;	ESC_12	,A1		;<ESC>
;;;		,A4,	"&l8D"	;vertical spacing, 8 lines/inch
;;;	ESC_13	,A1		;<ESC>
;;;		,A8,	"(s13.00H"	;pitch 14 CPI

RECORD	SPLFIL
		,A4,	'SPL:'
		,A1,	'S'
	SPORD	,A6
		,A4,	'.SPL'

RECORD	D_ORD
		,A7,	'ORDER: '
	DORD	,A6
		,A2
	DNAME	,A25

RECORD
		,A3,	'GAL'
		,A3,	'ALU'
		,A3,	'SS '
		,A3,	'PG '
		,A3,	'G90'
		,A3,	'PVS'
		,A3,	'OTH'
RECORD,X
	QTYPE	,7A3
RECORD
		,A3,	'L-S'
		,A3,	'RAP'
		,A3,	'4PC'
		,A3,	'OTH'
		,A3,	'OVR'
RECORD,X
	QCAT	,5A3
RECORD
		,A3,	'S/D'
		,A3,	'TDC'
		,A3,	'RW4'
		,A3,	'RWS'
		,A3,	'S/O'
		,A3,	'T/O'
		,A3,	'J35'
RECORD,X
	QSTY	,7A3
RECORD
		,A4,	'SNAP'
		,A4,	'PITT'
RECORD,X
	QSEAM	,2A4
RECORD
		,A3,	'YES'
		,A3,	'NO '
RECORD,X
	QSEAL	,2A3

RECORD
		,A9,	'1" - 1.5#'
		,A9,	'.5" - 2# '
		,A9,	'1" - 3#  '
		,A9,	'None     '
		,A9,	'.5" - 3# '
		,A9,	'2" - 1.5#'
		,A9,	'1" - 2#  '
		,A9,	'2" - 3#  '
		,A9,	'2" - 3#  '
RECORD,X
	QLINER	,9A9


RECORD	PBUF
	PLINE	,A132

RECORD	VARS
	chn182	,d2,	17		;2-22-17
	i	,d6
	read	,d1,0
	lokctl	,d1
	FILNAM	,A14
	FFCTL	,D1
	LINCNT	,D6	
PROC

	call read_liner			;2-22-17

	CLEAR LINCNT
	XCALL FFILE(175, FILNAM, FFCTL)
	IF(FFCTL)RETURN
	FILNAM(14,14)='M'
	OPEN(55,SI,FILNAM)

	SPORD=OORDNO,'XXXXXX'
	OPEN(14,O,SPLFIL)

;local laser landscape
	XCALL ASCII(27,O_CHAR)
	E_CHAR = O_CHAR
	P_CHAR = O_CHAR
	R_CHAR = O_CHAR
;;;	ESC_12 = O_CHAR
;;;	ESC_13 = O_CHAR
	WRITES(14,COMP14)

	PLINE = 'ORDER:'
	PLINE(8,13)=OORDNO,'XXXXXX'
	CALL P_DD
	PLINE = 'CUSTOMER:'
	PLINE(11,41)=OCUSNM
	CALL P_DD
	CALL P_DD
;;;	PLINE='STY   QTY   SIZE1   SIZE2 LEN    LINER     GAGE TYP CAT SEAM SEAL'
;;;	PLINE='STY  QTY    SIZE1   SIZE2 LEN    LINER     GAGE TYP CAT SEAM SEAL'
	PLINE='STY  QTY    SIZE1   SIZE2 LEN    LINER     GAGE TYP CAT SEAM SEAL  P/C'
	CALL P_DD
	CALL P_DD

	FIND(55,DUCACC,OORDNO,KEYNUM:1) [ERR=PP_LOOP]	;SEQ ORDER
PP_LOOP,
	READS(55,DUCACC,PP_EOF) [ERR=PP_EOF]
	IF(DUCTOR.NE.OORDNO)GOTO PP_EOF

	CLEAR PLINE

	IF(STY.GE.1 .AND. STY.LE.7)
	THEN	PLINE(1,3)=QSTY(STY)
	ELSE	PLINE(1,3)='***'

	PLINE(5,9)=JOINT,'ZZZX-'

	PLINE(11,17)=SIZE1,	'ZZX.XXX'
	PLINE(19,25)=SIZE2,	'ZZX.XXX'
	PLINE(28,29)=SIZE3,	'ZX'

	IF(LINER.GE.1 .AND. LINER.LE.9)
	THEN	PLINE(34,42)=QLINER(LINER)
	ELSE	PLINE(34,42)='NO'

	PLINE(44,47)=GAUGE,	'ZX'

	IF(DUTYPE.GE.1 .AND. DUTYPE.LE.7)
	THEN	PLINE(49,51)=QTYPE(DUTYPE)
	ELSE	PLINE(49,51)='***'

	IF(CAT.GE.1 .AND. CAT.LE.5)
	THEN	PLINE(53,55)=QCAT(CAT)
	ELSE	PLINE(53,55)='***'

	IF(SEAM.GE.1 .AND. SEAM.LE.2)
	THEN	PLINE(57,60)=QSEAM(SEAM)
	ELSE	PLINE(57,60)='****'

	IF(SEAL.GE.1 .AND. SEAL.LE.2)
	THEN	PLINE(63,65)=QSEAL(SEAL)
	ELSE	PLINE(63,65)='***'

	PLINE(67,70) = DPC,	'ZX.X'	;SSQ 11-18-04
	PLINE(72,86)=LOCAT
	if (dpunch .eq. '0') clear dpunch
	PLINE(88,103)=DPUNCH

	CALL P_DD
	INCR LINCNT
	GOTO PP_LOOP
PP_EOF,
	CLOSE 55
	DISPLAY(14,PORT)		;SSQ 12-23-02
	DISPLAY(14,EJECT)		;SSQ 12-23-02
	DISPLAY(14,RESET)		;SSQ 01-02-03
	CLOSE 14
	IF(LINCNT.GT.0)LPQUE (SPLFIL)
	XCALL DELET(SPLFIL)
	RETURN
P_DD,
	WRITES(14,PLINE)
	CLEAR PLINE
	RETURN
;--------------------------------------

;------------------------------------------------

READ_LINER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; GET LINER DESCRIPTIONS FROM LINER PRICE TABLE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR COPTBL
	TBLCOD = 'LP'
	FOR I FROM 1 THRU 9
		BEGIN
		LP_KEY = I
		XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
		IF (TBLCOD .NE. 'LP') EXITLOOP
		QLINER(I) = lp_liner				;note this pgm is "QLINER"
		END

	RETURN
;--------------------------------------------------
