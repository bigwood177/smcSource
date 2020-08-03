;changed to sub 9-27-05

subroutine	xpars
	chn_fd	,d		;chan for 'spl:fabdat.dat'
	chn_in	,d		;chan for fabshop export file
	chn_out	,d		;chan for debug file
	p_error	,d		;track any errors


RECORD	FABDAT
	.INCLUDE 'DEF:FABDAT.DEF'


RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'


RECORD	PARSE
	LN	,D4
	START	,D4,	1
	DIM	,D3,	600
	POS	,600D4
	LEN	,600D4
	TYPE	,600D2
	N_ITEMS	,D3
	END	,D4
	IX	,D4
	I1	,D4
	I2	,D4
	NUM	,D4
	PRESSUR_FILE	,A60	;data element (255) Need at least 53 char...

RECORD	BUFFER
		,A4096

RECORD	ARRAYS
	MAXARA	,D3,	256
	NAMES	,300A20
	TYPES	,300A3
	DATA	,300A20

RECORD	OUTF
		,A5,	'XPARS'
	OFC	,D2	;COUNTER
		,A4,	'.DAT'

RECORD	OUT
	O_NUM	,D3
		,A1
	O_NAME	,A20
		,A1
	O_TYPE	,A3
		,A1
	O_DATA	,A20

RECORD	VARS
	CHN182	,D2,	17
	acc_ez	,D1	;1 = ez flange
	XMULT	,D6	;10-21-13 First time I've seen this...
	XREC	,A5
	XRND	,A5
	XACC	,A4
	btn	,a3
	btn_err	,d1
	pf_err	,d1
	acc_err	,d1

	blen	,d4,	4096	;size of buffer

	is_pipe		,d1	;1= pipe
	is_spiral_pipe	,d1	;1 = Spiral Pipe
	RC1	,D6
	RC2	,D6
	BF	,D1
	OPNOK	,D1
	A30	,A30
	DP	,D6
	X_PC	,A30
	CODE	,A5
	KEY	,D1
	I	,D6
	N	,D6
	D1	,D1
	D2	,D2
	d3	,d3
	tchar	,a1
	size	,d6
	D18	,D18
;
PROC

	READS (CHN_IN, BUFFER, EOF)	;header record - field names
	KEY = 1
	NUM = 0		;START W/ *
	CALL PARSE

	READS (CHN_IN, BUFFER, EOF)	;field types
	KEY = 2
	NUM = 0		;START W/ *
	CALL PARSE

	OFC = 0

	RC1 = 46		;PROCESS ONLY THIS RECORD...
	RC2 = 0		;COUNTER
LOOP,					;data records
	INCR RC2
; 6-9-10: problem with embedded <lf> (dec=10)...
	READS (CHN_IN,BUFFER,EOF) [err=buff_err]

;;;	IF (RC2 .NE. RC1) GOTO LOOP	;DBG
buff_err,
	KEY = 3
	NUM = 0			;6-15-06
	CALL PARSE
	CALL WRITE_OUT		; debug record
	WRITES (CHN_FD, FABDAT)	; data record for mainline
;;;	xreturn		;dbg 10/19/06 - this must be removed for production!!!
	GOTO LOOP

ENDOFF,
EOF,

;--------------------------------------------
; EOF char seems to be part of last record, 
;	so last record doesn't get parsed 
;	above...

	KEY = 3
;;;	NUM = 1
	NUM = 0
	CALL PARSE
	CALL WRITE_OUT
	WRITES (CHN_FD, FABDAT)
;--------------------------------------------

	XRETURN
;============================================================
;============================================================


WRITE_OUT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; debug file contains each data element
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FOR I FROM 1 THRU MAXARA
		BEGIN
		O_NUM = I
		O_NAME = NAMES(I)
		O_TYPE = TYPES(I)
		O_DATA = DATA(I)
		WRITES (CHN_OUT,OUT)
		END
	RETURN
;-------------------------------------------------------------

PARSE,	;--------------------------------

	CLEAR P_ERROR, FABDAT
	ln = %trim(buffer)
	if (ln .le. 0) 
		begin
		p_error = 1
		return
		end

	xcall par2 (buffer(1,ln), pos, len, type, n_items, dim)

	FOR IX FROM 1 THRU N_ITEMS	;EXAMINE EACH DATA ELEMENT
		BEGIN
		I1 = POS(IX)
		I2 = POS(IX) + LEN(IX) - 1

		incr num

		IF (LEN(IX).LE.0) 		;ssq 7-25-06
			begin
			USING KEY SELECT
			(1),	clear NAMES(NUM) 
			(2),	clear TYPES(NUM) 
			(3),	clear DATA(NUM) 
			ENDUSING
			NEXTLOOP	;NO DATA
			end
		USING KEY SELECT
		(1),	NAMES(NUM) = BUFFER(I1,I2)
		(2),	TYPES(NUM) = BUFFER(I1,I2)
		(3),	DATA(NUM) = BUFFER(I1,I2)
		ENDUSING

		END


;per Kerry:
; 201 jf = main dia
; 200 hf = input size
; 197 ef = input size or inches on pipe
; 195 cf = length
; 188 spiral ?
; 242 length of spiral pipe in inches

	CLEAR FABDAT

; fields common to all record types:

	XCALL ALPDC (DATA(1), FD_NUM, BF)
	CODE    = DATA(2)
	FD_CODE = CODE(2,3)
	FD_GA   = DATA(28)
	XMULT   = DATA(23)	;10-21-13
	FD_QTY  = DATA(24)
	IF (XMULT) FD_QTY = FD_QTY * XMULT	;10-21-13

	A30 = DATA(5)
	CALL DEC3
	FD_PRICE = D18

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; load all dimension data...

	XCALL ALPDC (DATA(193), FD_DIMS(1), BF)
	XCALL ALPDC (DATA(194), FD_DIMS(2), BF)
	XCALL ALPDC (DATA(195), FD_DIMS(3), BF)
	XCALL ALPDC (DATA(196), FD_DIMS(4), BF)
	XCALL ALPDC (DATA(197), FD_DIMS(5), BF)
;;;	XCALL ALPDC (DATA(197), FD_DIMS(6), BF)
	XCALL ALPDC (DATA(199), FD_DIMS(7), BF)
	XCALL ALPDC (DATA(200), FD_DIMS(8), BF)
	XCALL aLPDC (DATA(201), FD_DIMS(9), BF)
	XCALL ALPDC (DATA(202), FD_DIMS(10), BF)

; f3 end treatment codes - 9/3/08...
	is_pipe = %instr(1,data(3), 'Pipe')
	is_spiral_pipe = %instr(1, data(3), 'Spiral')

	if (data(35) .eq. '22') clear data(35)
	if (data(52) .eq. '22') clear data(52)
	if (data(62) .eq. '22') clear data(62)
	if (data(72) .eq. '22') clear data(72)

	if (is_pipe) fd_dims(3) = 05	;1-13-2010 per ryan b
	if (is_spiral_pipe) XCALL ALPDC (DATA(242), FD_SPL, BF)

	IF (DATA(30) .EQ. 'None')
	THEN	if (is_pipe)
		then	FD_F3(1) = -1
		else	fd_f3(1) = 1	;not pipe default to spiral
	ELSE	FD_F3(1) = DATA(35)	;joint A code

	IF (DATA(47) .EQ. 'None')
	THEN	if (is_pipe)
		then	FD_F3(2) = -1
		else	fd_f3(2) = 1
	ELSE	FD_F3(2) = DATA(52)	;joint B code

	IF (DATA(57) .EQ. 'None')
	THEN	if (is_pipe)
		then	FD_F3(3) = -1
		else	fd_f3(3) = 1
	ELSE	FD_F3(3) = DATA(62)	;joint C code

	IF (DATA(67) .EQ. 'None')
	THEN	if (is_pipe)
		then	FD_F3(4) = -1
		else	fd_f3(4) = 1
	ELSE	FD_F3(4) = DATA(72)	;joint D code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5-26-20, now 9 is safety seal
;;;	for i from 1 thru 4
;;;		begin
;;;		if (fd_f3(i).eq.9) fd_f3(i) = 1	;se = spiral
;;;		end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	for i from 1 thru 4
	  begin
		if (fd_f3(i).eq.2) ;for ez,this price doesn't include bc & gasket
		begin
		A30 = DATA(19)
		CALL DEC3
		if (d18.gt.0) 
		then	FD_PRICE = D18
		else	begin
			a30 = data(46)		;barrel clamp
			call dec3
			fd_price = fd_price - d18
			end
		end
	  end


	USING CODE(1,1) SELECT		;SSQ 1-17-06
	('A'),	BEGIN
		FD_PC = 10	;ACC
		FD_TYPE = 'I'
		CALL ACC
		CALL ACC_MATL	
		END

	('R'),	BEGIN
		FD_PC = 20	;ROUND
		FD_TYPE = 'I'
		CALL ACC
		CALL RND_MATL
		END

	('S'),	BEGIN
		FD_PC = 30	;RECT
		CALL RECT		
		CALL RECT_MATL
		END

	('C'),	BEGIN
		FD_PC = 40	;CATALOG
		FD_ITEM = DATA(248)	;item # for catalog items
		END
	ENDUSING

;--------------------------------------------------

	FD_BTN = BTN_ERR
	FD_PF = PF_ERR
	FD_ACC = ACC_ERR

	RETURN
;---------------------------------------------------
;---------------------------------------------------

RECT_MATL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; add code for this later
	A30= DATA(216)
	XREC = A30(1,5)

	RETURN
;-----------------------------------------------------

RND_MATL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear acc_ez

	A30 = DATA(217)
	XRND = A30(1,5)


	FD_PCM = XRND(5,5)		;PC MAT (see fabmat.def)
	using xrnd(5,5) select
		('1'),	return		;galv
		('2'),	return		;black iron
		('3'),	call alum
		('4'),	call s_304	;stainless
		('5'),	call pvs	
		('6'),	call pvs_4x4
		('7'),	call s_316	;stainless
		('8'),	call Paint_Grip	;
		('9'),	call G_90
	endusing

	RETURN
;-----------------------------------------------------

ACC_MATL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear acc_ez, btn_err

	A30 = DATA(218)
	XACC = A30(1,4)
	
	A30 = data(213)			;button
	IF (A30 .eq. 'Button')
	then btn = A30(8,10)
	else btn = 

	FD_PCM = XACC(4,4)		;PC MAT (see fabmat.def)

	using xacc(4,4) select
	('1'),	return			;galv
	('2'),	using btn select
		('A01' thru 'A23'),	call pvs
		('A24' thru 'A46'),	call pvs_4x4
		(),btn_err = 1		;button error
		endusing
	('3'),				call alum
	('4'),	using btn select
		('A01' thru 'A23'),	call s_304
		('A24' thru 'A46'),	call s_316
		(),btn_err = 1		;button error
		endusing		
	('5'),				call paint_grip
	('6'),				call g_90
	('7'),	begin
		fd_pcm = 1		;galv
		return			;galv ez-flange
		end
	('8'),	begin
		acc_ez = 1
		call g_90
		end
	('9'),	begin
		acc_ez = 1
		call paint_grip
		end
	endusing


;;;	pf_err = 1			;pressure file error


	RETURN
;-------------------------------------------------------

PVS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	fd_f1 = 59		;PVS
	fd_f3 = 1		;spiral in fabcnv, will get converted to 4
	if (acc_ez) fd_f3 = 2	;ez flange
	
	RETURN
;-------------------------------------------------------

PVS_4x4,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	fd_f1 = 62		;PVS
	fd_f3 = 1		;spiral in fabcnv, will get converted to 4
	if (acc_ez) fd_f3 = 2	;ez flange
	
	RETURN
;-------------------------------------------------------


S_304,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FD_PCM = 4		;PC MAT (see fabmat.def)
	fd_f1 = 64		;304 Stls Stl
	fd_f3 = 1		;spiral in fabcnv, will get converted to 4
	if (fd_ga .le. 0) fd_ga = 24

	using fd_code select
	(1,2,6,8,9),	fd_price = (fd_price * 21)#1
	(16 thru 17),	fd_price = (fd_price * 21)#1
	endusing

	return
;---------------------------------------------------------

S_316,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FD_PCM = 7		;PC MAT (see fabmat.def)

	fd_f1 = 65		;304 Stls Stl
	fd_f3 = 1		;spiral in fabcnv, will get converted to 4
	if (fd_ga .le. 0) fd_ga = 24

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8-01-11
;;; this doesn't work because it's the smc price that
;;; needs to be multiplied, not the price-duct price.

;;;	using fd_code select
;;;	(24 thru 29),	fd_price = (fd_price * 16)#1
;;;	(39,40),	fd_price = (fd_price * 16)#1
;;;	(30,35,36,38,41),	fd_price = (fd_price * 30)#1
;;;	endusing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	return
;---------------------------------------------------------

Paint_Grip,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	fd_pcm = 8	;same as galv for now... (8/3/11)
	fd_f1 = 61		;Paint Grip
	fd_f3 = 1		;spiral in fabcnv, will get converted to 4
	if (acc_ez) fd_f3 = 2	;ez flange
	return
;---------------------------------------------------------

G_90,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	fd_f1 = 68		;G-90
	fd_f3 = 1		;spiral in fabcnv, will get converted to 4
	if (acc_ez) fd_f3 = 2	;ez flange
	return
;---------------------------------------------------------


ALUM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	fd_f1 = 66		;Alum
	fd_f3 = 1		;spiral in fabcnv, will get converted to 4

	
	RETURN
;-----------------------------------------------------

	
ACC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PC 10, ACCESSORIES
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	A30 = DATA(188)
	UPCASE A30
	IF (%INSTR(1,A30,'SPIRAL') )
	THEN	FD_ET = 'S'
	ELSE	FD_ET =



	RETURN
;---------------------------------------------------

RECT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; PC 30 RECTANGULAR
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	USING DATA(30) SELECT	;JointA
	('Slip & Drive'),	FD_SDC = 'S'		;SLIP, DRIVE (NORMAL)
	('Drive / Slip'), FD_SDC = 'D'		;DRIVE, SLIP (REVERSED)
	(),		FD_SDC = 'S'		;DEFAULT TO SLIP & DRIVE
	ENDUSING


	A30 = DATA(25)
	CALL DEC3
	FD_SQFT = D18

	A30 = DATA(26)
	CALL DEC3
	FD_WGHT = D18

	A30 = DATA(167)
	CALL DEC3
	FD_VNCH = D18

	A30 = DATA(169)
	CALL DEC3
	FD_RAIL = D18

	A30 = DATA(181)
	CALL DEC3
	FD_LNSF = D18
	
; assume for straight duct that these are size1 & size2
	A30 = DATA(193)
	CALL DEC3
	FD_SIZ1 = D18

	A30 = DATA(194)
	CALL DEC3
	FD_SIZ2 = D18
;--------------------------------------------------------

	FD_VDES = DATA(163)
	FD_LINR = DATA(177)


;;;	IF (DATA(188) .EQ. 'SLIP/DRIVE FITT ASSE')
;;;	THEN FD_FTTG = 1
;;;	ELSE FD_FTTG = 0
	USING CODE SELECT
	('S27'),	FD_FTTG = 0	;(CC) NOT PLASMA FITTING
	(),		FD_FTTG = 1	;EVERY THING ELSE
	ENDUSING

; Style should come from joint B, use joint A if B is blank...
	FD_STYL = DATA(52)		;JOINT B CODE
	IF (FD_STYL .LE. 0) FD_STYL = DATA(35)		;JOINT A CODE
	


	D1 = DATA(95)		;SEAM CODE 1=PITT, 2=S/L
	USING D1 SELECT
	(1),	FD_SEAM = 2	;PITTSBURG
	(2),	FD_SEAM = 1	;SNAP LOCK
	ENDUSING

	RETURN
;---------------------------------------------------


DEC3,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	dp=%instr(1,a30,'.')
	if (dp.gt.0 .and. dp.lt.25) clear a30(dp+4,30)	;5-16-13 for no dec pt
;;;	if (dp .lt. 25) clear a30(dp+4,30)	
	xcall dec3(A30, d18)
	return
;-------------------------------------------------

	END

