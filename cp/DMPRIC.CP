;DMPRIC.CP
;
;	BALANCING DAMPER PRICE MATRIX
;
RECORD	DMPRIC
	.INCLUDE 'DEF:DMPRIC.DEF'

RECORD	FILPRC
	F_DEV	,A3
		,A1,	':'
	F_NAME	,A6
		,A1,	'.'
	F_EXT	,A3

RECORD	SCREEN_PROMPTS
	SP_1	,A*,	
&	'           1. SEALED/NOT SEALED (S/N)         2. PROFIT MULT'
	SP_15	,A*,	
&	'           3. JACKSHAFTING PRICE              4. QUAD PRICE'
	SP_2	,A*,
&	'           -------------------- BLADE LENGTH - IN INCHES --------------------'
	SP_3	,A*,
&	'HEIGHT       1-18       19-24       25-30       31-36       37-42       43-48'


RECORD	SP_LINE
	SPL_HT	,A5		;XX-XX
		,A2
	SPL_D1	,A3
		,A9
	SPL_D2	,A3
		,A9
	SPL_D3	,A3
		,A9
	SPL_D4	,A3
		,A9
	SPL_D5	,A3
		,A9
	SPL_D6	,A3
	
RECORD	VARS
	OPNOK	,D1
	RECNO	,D6
	ROW	,D2
	COL	,D2	
	CHAN	,D2
;;;	HGTD	,6A5,	' 1-12','13-20','21-30','31-40','41-48'
	HGTD	,6A5,	' 6-11','12-18','19-24','25-32','33-40','41-48'
	I	,D6
	J	,D6
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	WHATNO	,D2
	SWITCH	,D1
	V	,D1

;
PROC
	XCALL TERID(V)
	XCALL OUTPT (1,1,2,'BALANCING DAMPER PRICE MATRIX',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'BALANCING DAMPER PRICE MATRIX',1)
	XCALL OUTPT (4,1,0,SP_1,1)
	XCALL OUTPT (6,1,0,SP_15,1)
	XCALL OUTPT (8,1,0,SP_2,1)
	XCALL OUTPT (9,1,0,SP_3,1)
	CALL DISP_ARA

DM_KEY,
	XCALL INPUT (4,39,01,01,'AE',ENTRY,INXCTL,1)
	GOTO (DISPLA,ENDOFF),INXCTL
	DM_KEY = ENTRY(1,1)

	USING DM_KEY SELECT
	('N'),	RECNO = 1
	('S'),	RECNO = 2
	(),	GOTO DM_KEY
	ENDUSING
	XCALL IO (CHAN,DMPRIC,RECNO,READ,LOKCTL)

	CALL DISP_DATA
	GOTO ANYCNG
DM_MULT,
	XCALL INPUT (4,62,03,00,'$E',ENTRY,INXCTL,1)
	GOTO (DISPLA,ENDOFF),INXCTL
	DM_MULT = ENTRY(1,3)
	GOTO ANYCNG

DM_JACK,
	XCALL INPUT (6,37,04,00,'$ ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	DM_JACK = ENTRY(1,4)
	GOTO ANYCNG
DM_QUAD,
	XCALL INPUT (6,62,03,00,'$ ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	DM_QUAD = ENTRY(1,4)
	GOTO ANYCNG

DM_PRICE,
	I = WHATNO - 4
;;;	I = WHATNO - 2
	CALL GET_PRICE
	GOTO (DISPLA),INXCTL
	GOTO ANYCNG

ADD_PRICE,
	FOR I FROM 1 THRU 30
		BEGIN
		CALL GET_PRICE
		GOTO (DISPLA,ANYCNG),INXCTL
		END

ANYCNG,
	XCALL OUTPT (24,1,1,'FIELD # TO CHANGE         <TAB> = ADD PRICES',1)
	XCALL INPUT (24,20,02,00,'#X',ENTRY,INXCTL,1)
	GOTO (ADD_PRICE),INXCTL-2			;TAB
	WHATNO = ENTRY(1,2)
	USING WHATNO SELECT
	(0),	GOTO PROCES
	(1),	GOTO DM_KEY
	(2),	GOTO DM_MULT
	(3),	GOTO DM_JACK
	(4),	GOTO DM_QUAD
	(5 thru 40),	GOTO DM_PRICE
;;;	(5 thru 34),	GOTO DM_PRICE
	(),	GOTO ANYCNG
	ENDUSING

PROCES,
	LOKCTL = 1
	XCALL IO(CHAN,DMPRIC,RECNO,WRITE,LOKCTL)
	GOTO DISPLA

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:SPCFUN',1)
	STOP

DISP_ARA,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; DISPLAY ARRAY OF PRICES
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	J = -3
	J = -1
;;;	FOR I FROM 1 THRU 5
	FOR I FROM 1 THRU 6
		BEGIN
		J = J + 6
		SPL_HT = HGTD(I)
		SPL_D1 = J, 	'ZX.'
		SPL_D2 = J+1,	'ZX.'
		SPL_D3 = J+2,	'ZX.'
		SPL_D4 = J+3,	'ZX.'
		SPL_D5 = J+4,	'ZX.'
		SPL_D6 = J+5,	'ZX.'
		XCALL OUTPT (I+10,1,0,SP_LINE,1)
;;;		XCALL OUTPT (I+8,1,0,SP_LINE,1)
		END
	RETURN
;------------------------------------------------------------

DISP_DATA,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; DISPLA PRICES
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ENTRY(1,4) = DM_MULT,	'Z.XX'
	XCALL OUTPT (4,62,0,ENTRY(1,4),1)

	ENTRY(1,4) = DM_QUAD,	'Z.XX'
	XCALL OUTPT (6,62,0,ENTRY(1,4),1)

	ENTRY(1,5) = DM_JACK,	'ZX.XX'
	XCALL OUTPT (6,37,0,ENTRY(1,5),1)

;;;	FOR I FROM 1 THRU 30
	FOR I FROM 1 THRU 36
		BEGIN
		CALL GET_ROW
		ENTRY(1,6) = DM_PRICE(I),	'ZZX.XX'
		XCALL OUTPT (ROW,COL,0,ENTRY(1,6),1)
		END

	RETURN
;------------------------------------------------------------

GET_PRICE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; GET PRICE BY ARRAY ELEMENT
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		CALL GET_ROW
		XCALL INPUT(ROW,COL,05,00,'$E',ENTRY,INXCTL,1)
		DM_PRICE(I) = ENTRY(1,5)
	RETURN
;------------------------------------------------------------

GET_ROW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		ROW = (I-1)/6 + 11
;;;		ROW = (I-1)/6 + 9
		USING I SELECT
		(1,7,13,19,25,31),	COL = 12
		(2,8,14,20,26,32),	COL = 24
		(3,9,15,21,27,33),	COL = 36
		(4,10,16,22,28,34),COL = 48
		(5,11,17,23,29,35),COL = 60
		(6,12,18,24,30,36),COL = 72
		ENDUSING
	RETURN
;------------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL FFILE(1,FILPRC,SWITCH)	;SSQ 9-9-03
	F_NAME = 'DMPRIC'		;SSQ 9-9-03
	OPEN(2,U,FILPRC)		;SSQ 9-9-03
;;;	OPEN(2,U,'tst:DMPRIC.tst') ;testing
	CHAN = 2
	OPNOK = 1

	RETURN
;------------------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLOSE 2
	RETURN
;------------------------------------------------------------
