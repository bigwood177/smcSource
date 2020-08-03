;fom008.cp
;	XCALL OUTPT (1,1,2,'FLAT OVAL TABLES - FLANGES & CLAMPS',1)

;fom005.cp

RECORD	FILLOC
		,A*,	'\\server-dc01\home\Material Data\Pricing\Flat Oval Mapping\Flanges.txt'

RECORD	LAST_UPDATE
		,A*,	'3. UPLOAD DATA FROM EXCEL - LAST UPDATE:'
		,A1
	LU_DESC	,A30

RECORD	PRINT
	TITLE	,A*,	'FLAT OVAL - FLANGES & CLAMPS'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PLINE	,A132
	PRNTON	,D1
	LINCNT	,D2,60
	PGCNT	,D6
	LPSW	,D2
	SPLFIL	,A14
	RPTNUM	,D3
	PRTTYP	,A1
;;;	PRTCTL	,D3,080
	PRTCTL	,D3,132
	LPARG	,D1
	PRNTSW	,D1
	PRTCTR	,D1


RECORD	HD1
		,A*,	'XX      YY GA DIA  MULT  PART            REF             DESCR'


record	
	dt	,a20
;
record ,x
	yyyymmdd	,d8
	hhMM		,d4
	sec	,d2


record	coptbl
	.include 'def:rd182a.def'

record	data
		,a2048	
		
RECORD	VARS
	KEYCOD	,A5
	BLANKS	,A30
	SAVR	,A15
	SAVE	,A15
	CHN182	,D2
	OPNOK	,D1
	XDATE	,D8
	ERB	,A1
	LN	,D6
	TL	,D6
	T1	,D6
	T2	,D6
	siz	,d8
	a6	,a6
	A2	,A2
	anum	,a6
	i	,d6
	j	,d6
	k	,d6
	tab	,a1
	DXX	,D2
	DYY	,D2
	DZZ	,D2
	AXX	,A4		;XX.X
	DGA	,D2
	DIA	,D2
	DMULT	,D3		;XX.X
	APART	,A10
	ADESCR	,A30
	REFO_CODE,A15
	FLAG	,D1
	NUM	,D6
	PGM	,D2
	ROW	,D2
	CNGCTL	,D1
	WHATNO	,D1
	ENTRY	,A30
	INXCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	LOKCTL	,D1
	SELECT	,D1
	SWITCH	,D1
	V	,D1

PROC
	XCALL TERID (V)
	XCALL OUTPT (1,1,2,'FLAT OVAL TABLES - FLANGES & CLAMPS',1)
	
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	KEYCOD = 

MENU,
	CALL CHECK_HEADER
	XCALL OUTPT (1,1,2,'FLAT OVAL TABLES - FLANGES & CLAMPS',1)
	XCALL OUTPT (3,9,0,'PLEASE SELECT APPLICATION',1)
	XCALL OUTPT (5,15,0,'1. TABLE MAINTENANCE',1)
	XCALL OUTPT (6,15,0,'2. PRINT TABLE',1)
	XCALL OUTPT (7,15,0,LAST_UPDATE,1)

MINPUT,
	XCALL INPUT (3,36,1,1,'#E',ENTRY,INXCTL,1)
	GOTO (MINPUT,ENDOFF), INXCTL
	PGM = ENTRY(1,1)
	GOTO (DISPLA,PRINT_TABLE, UPLOAD),PGM
	GOTO MINPUT

DISPLA,
	CLEAR CNGCTL
	XCALL OUTPT (1,1,2,'FLAT OVAL TABLES - FLANGES & CLAMPS',1)
	XCALL OUTPT (3,4,0,'1. KEY                     (EFI/EB/EFS/EF)',1)
	XCALL OUTPT (4,4,0,'1. SIZE (XX)',1)
	XCALL OUTPT (5,4,0,'2. SIZE (YY)',1)
	XCALL OUTPT (6,4,0,'3. GAUGE',1)
	XCALL OUTPT (7,4,0,'4. FAB DIA.',1)
	XCALL OUTPT (8,4,0,'5. MULT',1)
	XCALL OUTPT (9,4,0,'6. PART',1)
	XCALL OUTPT(10,4,0,'7. REF CODE',1)
	XCALL OUTPT(11,4,0,'8. DESC',1) 
KEY,
	XCALL INPUT (3,20,03,01,'AE',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	USING ENTRY(1,3) SELECT
	('EFI'),	NOP
	('EB'),		NOP
	('EFS'),	NOP
	('EF'),		NOP
	(),	GOTO KEY
	ENDUSING
	KEYCOD = ENTRY(1,3)
DXX,
	XCALL INPUT (4,20,02,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	DXX = ENTRY(1,2)
	GOTO (ANYCNG),CNGCTL

DYY,
	XCALL INPUT (5,20,02,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	DYY = ENTRY(1,2)
ANYCNG,
	XCALL ANYCN (CNGCTL, WHATNO)
	GOTO (PROCES, CNGBR),CNGCTL+1
CNGBR,
	GOTO (DXX, DYY),WHATNO
	GOTO ANYCNG
PROCES,
	CLEAR COPTBL
	TBLCOD = 'FO'
	FO_KEY = 'FO'+KEYCOD
	FO_XX = DXX
	FO_YY = DYY
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) 
		BEGIN
		XCALL MESAG ('NO TABLE ENTRY FOR THOSE SIZES',1)
		GOTO DISPLA
		END

	XCALL OUTPT (4,26,1,FO_AXX,1)

	A2 = FO_GA,'XX'
	XCALL OUTPT (6,20,1,A2,1)

	A2 = FO_DIA,'XX'
	XCALL OUTPT (7,20,1,A2,1)

	A6 = FO_MULT,	'Z.XX'
	XCALL OUTPT (8,20,1,A6,1)

	XCALL OUTPT (9,20,1,FO_PART,1)
	XCALL OUTPT(10,20,1,FO_REF,1)
	XCALL OUTPT(11,20,1,FO_DESC,1)
	XCALL MESAG ('FLANGE/CLAMP DATA',1)
	GOTO DISPLA	

PRINT_TABLE,
	LINCNT = 66
	PGCNT  = 0
PDISP,
	CNGCTL = 
	XCALL OUTPT (1,1,2,'PRINT FLAT OVAL FLANGES & CLAMPS TABLE',1)
	XCALL OUTPT (4,4,0,'KEY/ALL',1)
ST_DPT,
	XCALL INPUT (4,27,03,00,'AE',ENTRY,INXCTL,1)
	GOTO (PDISP,PDONE),INXCTL
	KEYCOD = ENTRY(1,3)
	IF (KEYCOD .EQ. BLANKS)
		BEGIN
		KEYCOD = 'ALL'
		XCALL OUTPT (4,27,1,KEYCOD,1)
		END

	USING KEYCOD SELECT
	('EFI'),	NOP
	('EB'),		NOP
	('EFS'),	NOP
	('EF'),		NOP
	('A'),		NOP
	(),	GOTO ST_DPT
	ENDUSING
	
P_ANY,
	CNGCTL = 1
	XCALL ANYCN(CNGCTL,WHATNO)
	GOTO (P_PROC,ST_DPT),CNGCTL + 1

P_PROC,	
	CLEAR TBL_KEY
	TBLCOD = 'FO'
	FO_KEY = 'FOEB'		;FIRST
	FIND (CHN182,COPTBL,TBL_KEY)[ERR=PLOOP]

PLOOP,
	XCALL IOS (CHN182,COPTBL,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF
	IF (TBLCOD .NE. 'FO') GOTO EOF
	USING FO_KEY SELECT
	('FOEFI'),	IF (KEYCOD.EQ.FO_KEY .OR. KEYCOD.EQ.'A')
			THEN 	NOP
			ELSE	GOTO PLOOP
	('FOEB'),		IF (KEYCOD.EQ.FO_KEY .OR. KEYCOD.EQ.'A')
			THEN 	NOP
			ELSE	GOTO PLOOP
	('FOEFS'),	IF (KEYCOD.EQ.FO_KEY .OR. KEYCOD.EQ.'A')
			THEN 	NOP
			ELSE	GOTO PLOOP
	('FOEF'),		IF (KEYCOD.EQ.FO_KEY .OR. KEYCOD.EQ.'A')
			THEN 	NOP
			ELSE	GOTO PLOOP
	(),		GOTO EOF
	ENDUSING

;;;	IF (FO_KEY .NE. KEYCOD) GOTO EOF

	PLINE (1,2) = FO_XX,	'ZX'
	PLINE (4,7) = FO_AXX
	PLINE (9,10) = FO_YY,	'ZX'
	PLINE (12,13) = FO_GA,	'XX'
	PLINE (16,17) = FO_DIA,	'ZX'
	PLINE (20,23) = FO_MULT,	'Z.XX'
	PLINE (26,40) = FO_PART
	PLINE (42,56) = FO_REF
	PLINE (58,87) = FO_DESC
	CALL PRINT

	GOTO PLOOP

EOF,
	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
	PRNTON  = 0

PDONE,
	GOTO MENU

PRINT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (PRNTON .EQ. 0) CALL PRNTON
	XCALL LPOUT(LINCNT,PGCNT,PLINE,TITLE,HD1,HD,HD
&		,LG,LG,LG,0,080,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
;-------------------------------------------------------------
PRNTON,
	SPLFIL (5,6) = 'EF'
	LPSW = 1		;PRINT,SPOOL, OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPARG = 2
	IF (LPSW.EQ.2) LPARG = 4
	XCALL WATE (LPARG,V)
	PRNTON = 1
	RETURN
;-------------------------------------------------------------
;===================================================================

;XX      YY GA DIA  MULT  PART            REF             DESCR
;ZX AAAA ZX ZX  ZX  ZX.X  AAAAAAAAAAAAAAA AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA 
;1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
	GOTO MINPUT

UPLOAD,
; clear old data from coptbl

; import new data from spread sheet
	ONERROR NO_FILE
	OPEN (5, I, FILLOC)
	OFFERROR

	CLEAR COPTBL
	TBLCOD = 'FO'
	FO_KEY = 'EB'		;1ST KEY
	FIND (CHN182, COPTBL, TBL_KEY) [ERR=DEL_TBL]
DEL_TBL,
	XCALL IOS (CHN182, COPTBL, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO DEL_EOF
	IF (TBLCOD .NE. 'FO') GOTO DEL_EOF
	USING FO_KEY SELECT
	('EFI'),	NOP		;OK
	('EB'), 	NOP		;OK
	('EFS'), 	NOP		;OK
	('EF'), 	NOP		;OK
	(),		GOTO DEL_EOF
	ENDUSING

	XCALL ISIO (CHN182, COPTBL, TBL_KEY, DELETE, LOKCTL)
	GOTO DEL_TBL
DEL_EOF,
	xcall ascii(9,tab)
; Each row will make 2 table records, RW & ERW

	XCALL IOS (5, DATA, READ, LOKCTL)		;READ THE HEADER
U_LOOP,
	XCALL IOS (5, DATA, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO U_EOF
	LN = %TRIM(DATA)
	IF (LN .LT. 40) GOTO U_LOOP			;IN CASE JUNK AT END OF FILE
	T1 = 1

	FOR I FROM 1 THRU 22				;DATA IN FIRST 13 COLUMNS
		BEGIN
		T2 = %INSTR(T1,DATA,TAB)		;FIND THE NEXT TAB
		USING I SELECT		
		(1),	BEGIN
			CALL CHK_DEC
			DXX = NUM
			AXX = A6(1,4)
			END
		(2),	NOP				;NO DATA THIS COL
		(3),	XCALL ALPDC (DATA(T1,T2-1), DYY, FLAG)
		(4),	XCALL ALPDC (DATA(T1,T2-1), DGA, FLAG)
		(5),	NOP
		(6),	BEGIN
			CALL CHK_QOT
			DIA = ANUM
			END
		(7),	APART = DATA(T1,T2-1)
		(8),	BEGIN
			IF (T2.GT.T1) 
			THEN	REFO_CODE = DATA(T1,T2-1)
			ELSE	REFO_CODE = SAVR
			SAVR = REFO_CODE
			END
		(9),	BEGIN
			CALL CHK_QOT
			DMULT = ANUM
			END
		(10),	BEGIN
			ADESCR = DATA(T1,T2-1)
			KEYCOD = 'FOEFI'
			CALL UPD_TABLE			;STORE THE REDUCER RECORD
			END
		(11),	APART = DATA(T1,T2-1)
		(12),	BEGIN
			IF (T2.GT.T1) 
			THEN	REFO_CODE = DATA(T1,T2-1)
			ELSE	REFO_CODE = SAVE
			SAVE = REFO_CODE
			END
		(13),	BEGIN
			CALL CHK_QOT
			DMULT = ANUM
			END
		(14),	BEGIN
			IF (T2.GT.T1)
			THEN	ADESCR = DATA(T1,T2-1)
			ELSE	BEGIN
				TL = %TRIM(DATA)
				ADESCR = DATA(T1,TL)
				END
			KEYCOD = 'FOEB'
			CALL UPD_TABLE
			END
		(15),	APART = DATA(T1,T2-1)
		(16),	BEGIN
			IF (T2.GT.T1) 
			THEN	REFO_CODE = DATA(T1,T2-1)
			ELSE	REFO_CODE = SAVE
			SAVE = REFO_CODE
			END
		(17),	BEGIN
			CALL CHK_QOT
			DMULT = ANUM
			END
		(18),	BEGIN
			IF (T2.GT.T1)
			THEN	ADESCR = DATA(T1,T2-1)
			ELSE	BEGIN
				TL = %TRIM(DATA)
				ADESCR = DATA(T1,TL)
				END
			KEYCOD = 'FOEFS'
			CALL UPD_TABLE
			END
		(19),	APART = DATA(T1,T2-1)
		(20),	BEGIN
			IF (T2.GT.T1) 
			THEN	REFO_CODE = DATA(T1,T2-1)
			ELSE	REFO_CODE = SAVE
			SAVE = REFO_CODE
			END
		(21),	BEGIN
			CALL CHK_QOT
			DMULT = ANUM
			END
		(22),	BEGIN
			IF (T2.GT.T1)
			THEN	ADESCR = DATA(T1,T2-1)
			ELSE	BEGIN
				TL = %TRIM(DATA)
				ADESCR = DATA(T1,TL)
				END
			KEYCOD = 'FOEF'
			CALL UPD_TABLE
			END
		ENDUSING

		T1 = T2+1				;SKIP PAST THE TAB
		END

	GOTO U_LOOP
U_EOF,
	CLOSE 5
	XCALL MESAG ('TABLE LOADED',1)

	CLEAR COPTBL
	TBLCOD = 'FO'
	FO_KEY = 'EB'
	FO_XX = 0
	FO_YY = 0
	FIND (CHN182, COPTBL, TBL_KEY) [ERR=ST_TBL]
	XCALL IOS (CHN182, COPTBL, READ, LOKCTL)
	CALL DATIM
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, WRITE, LOKCTL)
	GOTO MENU

ST_TBL,
	CLEAR COPTBL
	TBLCOD = 'FO'
	FO_KEY = 'EB'	;1st key
	FO_XX = 0
	FO_YY = 0
	CALL DATIM

	XCALL ISIO (CHN182, COPTBL, TBL_KEY, STORE, LOKCTL)
	GOTO MENU
NO_FILE,
	XCALL MESAG ('NO SPREADSHEET DATA FOR FILE!',1)
	GOTO MINPUT
	
	
DATIM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	dt = %datetime
	xdate(1,4) = yyyymmdd(5,8)
	xdate(5,8) = yyyymmdd(1,4)
	FO_desc(1,10) = xdate, 'ZX/XX/XXXX'
	FO_desc(13,17) = hhmm,	'XX:XX'
	
	RETURN
;--------------------------------------------------------

CHECK_HEADER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR COPTBL
	TBLCOD = 'FO'
	FO_KEY = 'EB'
	FO_XX = 0
	FO_YY = 0
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	UNLOCK CHN182
	IF (LOKCTL .NE. 0) CLEAR COPTBL
	LU_DESC = FO_DESC
	RETURN
;---------------------------------------------
UPD_TABLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR COPTBL
	TBLCOD = 'FO'

	FO_KEY = KEYCOD
	FO_XX = DXX
	FO_AXX = AXX
	FO_YY = DYY
	FO_ZZ = DZZ
	FO_GA = DGA
	FO_DIA = DIA
	FO_MULT = DMULT
	FO_PART = APART
	FO_REF = REFO_CODE
	FO_DESC = ADESCR
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, STORE, LOKCTL)

	RETURN
;------------------------------------------------
CHK_DEC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR A6
	TL = %INSTR (1, DATA(T1,T2-1), '.')	;IS THERE A PERIOD?
	IF (TL)
	THEN	BEGIN
		A6 = DATA(T1,T2)
		XCALL ALPDC(DATA(T1,T2-2), NUM, FLAG)
		END
	ELSE	BEGIN
		XCALL ALPDC(DATA(T1,T2-1), NUM, FLAG)
		A6 = NUM, 'ZX' [LEFT]
		END
	RETURN
;------------------------------------------------

CHK_QOT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	A6 = DATA(T1,T2)
	K =
	ANUM =
	FOR J FROM 1 THRU 6
		BEGIN
		IF (A6(J,J).GE.'0' .AND. A6(J,J).LE.'9')
			BEGIN
			INCR K
			ANUM(K,K) = A6(J,J)
			END
		END

	RETURN
;------------------------------------------------
OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	OPNOK =
	SWITCH = 5
	XCALL FILES (17,'SU',182,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 17

	OPNOK = 1
	RETURN
;--------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN182) CLOSE CHN182

	RETURN
;-------------------------------------------------	
ENDOFF,
	
	XCALL PGCHN ('CP:FOMENU',1)
	END


