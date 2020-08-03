;PHYENT.CP
;
RECORD	PHYMAN
	.INCLUDE 'DEF:RD197A.DEF'
;;;	.INCLUDE 'DEF:PHYMAN.DEF'
;
RECORD	ITMMAS
	.INCLUDE 'DEF:RD041A.def'

RECORD	ITMKEY
	.INCLUDE 'DEF:RD041K.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	PHYINV
	.INCLUDE 'DEF:RD196A.DEF'
;;;	.INCLUDE 'DEF:PHYINV.DEF'

RECORD	HD1
		,A*,	' SEQ # ITEM #          DESCRIPTION'
		,A*,	'                          COUNT'


RECORD	PRINT
	TITLE	,A26,	'MANUAL INVENTORY EDIT LIST'
	HD	,A6,	'NO HDR'
	LG	,A9,	'NO LEGEND'
	PLINE	,A80
	PRNTON	,D1
	LINCNT	,D2,60
	PGCNT	,D6
	LPSW	,D2
	SPLFIL	,A14
	RPTNUM	,D3
	PRTTYP	,A1
	PRTCTL	,D3,080
	LPARG	,D1
	PRNTSW	,D1
	PRTCTR	,D1

record	data
;;;	item	,a15
;;;	f1	,d3
;;;	f2	,d3
;;;	f3	,d5
;;;	cnt	,d6
;;;	box	,d6
	errdes	,a30

RECORD	CHANNEL
	CHN197	,D2
	CHN196	,D2
	CHN041	,D2
	CHN182	,D2

RECORD	VARS
	a3	,a3
	bf	,d1
	d_price	,d9
	x_price	,d9

	dec	,d18
	case	,d4
	itm_err	,d1
	new_phy	,d1
	POST	,D1
	IS_MEMO	,D1
	BLANKS	,A30
	ZEROS	,A5,	'00000'
	SELECT	,D1
	J	,D6
	NEXT	,D6
	ITM_FOUND	,D1
	OPNOK	,D1
	PGM	,D3
	A6	,A6
	A9	,A9
	SEQ	,D6
	ITEM	,A15
	F1	,D3
	F2	,D3
	F3	,D5
	COUNT	,D9
	ENTRY	,A30
	INXCTL	,D2
	CNGCTL	,D1
	WHATNO	,D2
	READ	,D1,0
	WRITE	,D1,1
	LOKCTL	,D1
	SWITCH	,D1
	V	,D1
;
PROC
	XCALL TERID (V)
	V = 1
	XCALL OUTPT (1,1,2,'MANUAL INVENTORY ENTRY',1)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
MENU,
	CLEAR PRNTON
	XCALL OUTPT (1,1,2,'MANUAL INVENTORY ENTRY',1)
	XCALL OUTPT (3,9,0,'PLEASE SELECT APPLICATION',1)
	XCALL OUTPT (5,15,0,'1. INVENTORY ENTRY',1)
	XCALL OUTPT (6,15,0,'2. PRINT EDIT LIST',1)
	XCALL OUTPT (7,15,0,'3. POST INVENTORY',1)
MINPUT,
	XCALL INPUT (3,36,1,1,'#E',ENTRY,INXCTL,1)
	GOTO (MINPUT,ENDOFF), INXCTL
	PGM = ENTRY(1,1)
	GOTO (START, EDIT, POST), PGM
	GOTO MINPUT
START,
	CLEAR M_SEQ
	READ (CHN197, PHYMAN, M_SEQ) [ERR=NO_HDR]
	NEXT = SEED

DISPLA,
	CLEAR CNGCTL, PHYMAN, SELECT
	XCALL OUTPT (1,1,2,'MANUAL INVENTORY ENTRY',1)
	XCALL OUTPT ( 2,4,0,'1. SEQ #',1)
	XCALL OUTPT ( 4,4,0,'2. ITEM #',1)
	XCALL OUTPT ( 6,4,0,'3. F1-KEY',1)
	XCALL OUTPT ( 8,4,0,'4. F2-KEY',1)
	XCALL OUTPT (10,4,0,'5. F3-KEY',1)
	XCALL OUTPT (12,4,0,'6. COUNT',1)

SEQ,
	XCALL INPUT (2,16,06,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,MENU),INXCTL
	SEQ = ENTRY(1,6)
	IF (SEQ .NE. 0)
		BEGIN
		READ (CHN197, PHYMAN, SEQ) [ERR=NO_SEQ]
		SELECT = 2
		CALL DSP_MAN
		GOTO ANYCNG
		END
	SELECT = 1
	INCR NEXT
	A6 = NEXT, 'ZZZZZX' [LEFT]
	XCALL OUTPT (2,16,1,A6,1)
	SEQ = NEXT
ITEM,
	XCALL INPUT (4,16,15,00,'AE',ENTRY,INXCTL,1)
	GOTO (DISPLA,ENDOFF),INXCTL
	ITEM = ENTRY(1,15)
	IF (ITEM .EQ. BLANKS) GOTO ITEM
	GOTO (ANYCNG),CNGCTL
F1,
	XCALL INPUT (6,16,03,00,'# ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	F1 = ENTRY(1,3)
	GOTO (ANYCNG),CNGCTL
F2,
	XCALL INPUT (8,16,03,00,'# ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	F2 = ENTRY(1,3)
	GOTO (ANYCNG),CNGCTL
F3,
	XCALL INPUT (10,16,03,00,'# ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	F3 = ENTRY(1,5)
	GOTO (ANYCNG),CNGCTL
	CALL CHK_KEY

COUNT,
	XCALL INPUT (12,16,09,00,'# ',ENTRY,INXCTL,1)
	GOTO (DISPLA),INXCTL
	COUNT = ENTRY(1,9)
	ENTRY(1,10) = COUNT, 'ZZZ,ZZX.XXX' [LEFT]
	XCALL OUTPT (12,16,1, ENTRY(1,10), 1)
	GOTO ANYCNG

ANYCNG,
	CALL CHK_KEY
	XCALL ANYCN(CNGCTL,WHATNO)

	GOTO (PROCES,CNGBR),CNGCTL+1
CNGBR,
	GOTO (ITEM, F1, F2, F3, COUNT),WHATNO-1
	GOTO ANYCNG

PROCES,
	IF (.NOT. ITM_FOUND) GOTO ANYCNG

	CLEAR PHYMAN
	M_SEQ = SEQ
	M_ITEM = ITEM
	M_F1 = F1,	'XXX'
	M_F2 = F2,	'XXX'
	M_F3 = F3,	'XXXXX'
	M_COUNT = COUNT

	USING SELECT SELECT
	(1),	STORE (CHN197, PHYMAN, MAN_KY1) [ERR=BAD_REC]
	(2),	WRITE (CHN197, PHYMAN, MAN_KY1) [ERR=BAD_REC]
	ENDUSING

	CLEAR M_SEQ
	READ (CHN197, PHYMAN, M_SEQ) [ERR=NO_HDR]
	SEED = NEXT 
	WRITE (CHN197, PHYMAN, M_SEQ)

	GOTO DISPLA

ENDOFF,
	CALL CLOSE
	XCALL PGCHN ('CP:PHYMNU',1)


NO_HDR,
	XCALL MESAG ('NO HEADER RECORD',1)
	GOTO MENU
NO_SEQ,
	XCALL MESAG ('RECORD NOT FOUND',1)
	GOTO SEQ

BAD_REC,
	XCALL MESAG ('ERROR STORING RECORD',1)
	GOTO DISPLA

EDIT, 
	LINCNT = 66
	READ (CHN197, PHYMAN, ^FIRST) [ERR=E_EOF]	;SKIP HEADER
PLOOP,
	LOKCTL = 0
	XCALL IOS (CHN197, PHYMAN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO E_EOF

	CLEAR ITMKEY

	K_ITEM = M_ITEM
	K_F1 = M_F1
	K_F2 = M_F2
	K_F3 = M_F3

	READ (CHN041, ITMMAS, ITMKEY, KRF=1) [ERR=E_NO_ITEM]
	GOTO ITM_OK
E_NO_ITEM,
	DESCR = '*** NOT FOUND ***'
ITM_OK,
	PLINE (1,6) = M_SEQ,	'ZZZZZX'
	PLINE (8,22) = M_ITEM
	PLINE (24,53) = DESCR
	PLINE (55,65) = M_COUNT,	'ZZZ,ZZX.XXX'
	CALL PRINT


	IF (M_F1 .NE. ZEROS) 
		BEGIN
		TBLCOD = 'M1'
		M1_KEY = M_F1
		CALL GET_MEMO
		PLINE (15,26) = MM_SHORT
		END

	IF (M_F2 .NE. ZEROS) 
		BEGIN
		TBLCOD = 'M2'
		M2_KEY = M_F2
		CALL GET_MEMO
		PLINE (28,39) = MM_SHORT
		END

	IF (M_F3 .NE. ZEROS) 
		BEGIN
		TBLCOD = 'M3'
		M3_KEY = M_F3
		CALL GET_MEMO
		PLINE (41,52) = MM_SHORT
		END

	PLINE (1,3) = M_F1
	PLINE (5,7) = M_F2
	PLINE (9,13) = M_F3
	CALL PRINT

;;;	IF (IS_MEMO) CALL PRINT

	CALL PRINT

	GOTO PLOOP
E_EOF,
	IF (PRNTON.EQ.1)  XCALL LPOFF(LPSW,SPLFIL,PGCNT)
	PRNTON = 0
	IF (POST) RETURN
	GOTO MENU

; SEQ # ITEM #          DESCRIPTION                      COUNT
;ZZZZZX AAAAAAAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA ZZZ,ZZX
;AAA AAA AAAAA AAAAAAAAAAAA AAAAAAAAAAAA AAAAAAAAAAAA
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7         8

POST,
	POST = 1
	XCALL OUTPT (2,1,1,'POST MANUAL INVENTORY TO PHYSICAL',1)
	TITLE = 'MANUAL INVENTORY REGISTER'

	CALL EDIT

;;;	OPEN (4, SU, 'SMC:PHYINV.SMM')
	SWITCH = 5
	XCALL FILES (4, 'SU', 196, SWITCH)
	CHN196 = 4

	READ (CHN197, PHYMAN, ^FIRST) [ERR=I_EOF]	;SKIP HEADER
ILOOP,
	LOKCTL = 0
	XCALL IOS (CHN197, PHYMAN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO I_EOF

	call get_item
	goto (iloop),itm_err		;could not find itmmas

; at this point we have a valid itmmas record...

; removed 10-05-11 per jess
;;;	m_count = m_count / case		;per case
	call get_phys			;existing phyinv record?
	if (.not. new_phy)
	then	begin
		p_count = p_count + m_count
		write (CHN196, phyinv, phy_key)
		end

	else	begin
		p_item = itemno
		p_f1 = if1
		p_f2 = if2
		p_f3 = if3
		p_cat = prdcat
		p_desc = descr
		p_onh = qtyonh
		p_count = m_count
		p_price = d_price
		store (CHN196, phyinv, phy_key)
		end

	goto iloop
I_EOF,
	read (CHN197, phyman, ^first) [err=ie_loop]	;skip header
ie_loop,
	reads (CHN197, phyman, ie_eof)
	delete (CHN197)
	goto ie_loop
ie_eof,
	xcall mesag ('Inventory posted!',1)

	GOTO MENU


;==================================================

get_phys,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	new_phy = 0					;assum not found
	read (CHN196, phyinv, itmkey) [err=no_phy]	;use itmmas key

	return
no_phy,
	new_phy = 1					;add new record
	clear phyinv
	return

;----------------------------------------------

get_item,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear itm_err
	if (f1.ne.0 .or. f2.ne.0) goto no_pp	;only galv is pressed

;look up in pressed parts table!
	clear tbl_key
	tblcod = 'PP'
	pp_item = m_item

	read (chn182,coptbl,tbl_key)[err=no_pp]
	item = pp_part				;pressed part
no_pp,
;look up in pressed parts table!
	clear itmkey
	k_f1 = '000'
	k_f2 = '000'
	k_f3 = '00000'

	k_item = m_item

	if (m_f1.eq.blanks.and.m_f2.eq.blanks.and.m_f3.eq.blanks) call get_default
;;;	dec = f1 + f2 + f3
;;;	if (dec .eq. 0) call get_default

	k_f1 = m_f1
	k_f2 = m_f2
	k_f3 = m_f3


	read (chn041, itmmas, itmkey, keynum:1) [err=p_bad_key]	;krf = 2
;-
	read (chn041, itmmas, itmkey, keynum:1) [err=p_bad_key]	;krf = 2

	D_PRICE = PRICE

	using icfg select
	(1),	begin		;configured item
		xcall cfg_p (itemno, if1, if2, if3, x_price, bf)
		if (bf .eq. 0) d_price = x_price
		end
	(),	begin		;regular item
		if (if1.ne.'000' .or. if2.ne.'000' .or. if3.ne.'00000')
			begin
			xcall cfg_p (itemno, if1, if2, if3, x_price, bf)
			if (bf .eq. 0) d_price = x_price
			end
		end
	endusing
;-

	using prdcat select
	('J'),	begin
		case = 1
	;;;	case = ftcase
		d_price = avgcst
		end
	(),	case = 1
	endusing
	if (case .lt. 1) case = 1

;-
; check price/qty table...

	clear coptbl
	tblcod = 'I1'
	i1_key = itemno
	read (chn182, coptbl, tbl_key) [err=no_p_tab]
	d_price = i1_price(1)	;use first price

no_p_tab,

	if (imat2 .eq. a3) return	;no mat'l code

; change "price" to be price per unit counted.  itmmas.weight is lbs/unit,
; so qty counted * weight is the weight of the qty counted.  the price in
; the "I2" table is price/lb, so qty*weight*i2_price is the total amount
; for the qty counted.  Therefore, (weight*i2_price) is the price for the
; qty counted. 

; look up price/lb for this material in table...
	clear tbl_key
	tblcod = 'I2'
	i2_mat = imat2
	i2_ga = iga
	read (chn182, coptbl, tbl_key) [err=no_matl]

	d_price = (weight * i2_price)#2	; itmmas.weight is lbs/unit counted,
				; so (weight*i2_price) is price/unit counted
no_matl,

;-
	return
;------------------------------------------------

p_bad_key,
	itm_err = 1			;no itmmas
;;;	errdes = 'no itmmas record'
;;;	writes (chnerr, data)		;bad data
	return

get_default,
	clear tbl_key
	tblcod = 'IK'
	ik_item = item

	read (chn182,coptbl,tbl_key)[err=no_default]
	f1 = ik_f1
	f2 = ik_f2
	f3 = ik_f3
no_default,
	return

;----------------------------------------------

CHK_KEY,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR ITMKEY, ITM_FOUND

	K_ITEM = ITEM
	K_F1 = F1,	'XXX'
	K_F2 = F2,	'XXX'
	K_F3 = F3,	'XXXXX'

	READ (CHN041, ITMMAS, ITMKEY, KRF=1) [ERR=NO_ITEM]

	XCALL OUTPT (4,34,0,DESCR,1)

	FOR J FROM 1 THRU 3
		BEGIN
		TBLCOD = J,	'MX'
		USING J SELECT
		(1),	M1_KEY = F1
		(2),	M2_KEY = F2
		(3),	M3_KEY = F3
		ENDUSING

		CLEAR LOKCTL
		IF (MM_KEY) 
			BEGIN
			XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
			IF (LOKCTL .NE. 0) GOTO NO_MEMO
			XCALL OUTPT ( J*2+4, 22, 0, MM_LONG, 1)
			END
		END

	ITM_FOUND = 1
	RETURN

NO_ITEM,
	XCALL MESAG ('ITEM NOT FOUND',1)
	RETURN
NO_MEMO,
	XCALL MESAG ('MEMO NOT FOUND',1)
	RETURN
;--------------------------------------------

GET_MEMO,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR LOKCTL
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL .NE. 0) 
	THEN	MM_LONG = '*** NO MEMO FOUND ***'
	ELSE	IS_MEMO = 1			;AT LEAST 1 MEMO

	RETURN
;--------------------------------------------

DSP_MAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	A9 = M_COUNT,	'ZZZZZX.XXX' [LEFT]
	XCALL OUTPT ( 4,16,1,M_ITEM,1)
	XCALL OUTPT ( 6,16,1,M_F1,1)
	XCALL OUTPT ( 8,16,1,M_F2,1)
	XCALL OUTPT (10,16,1,M_F3,1)
	XCALL OUTPT (12,16,1,A9,1)

	ITEM = M_ITEM
	F1 = M_F1
	F2 = M_F2
	F3 = M_F3
	COUNT = M_COUNT
	SEQ = M_SEQ

	RETURN
;--------------------------------------------
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

	RETURN
;--------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

;;;	OPEN (1,SU,'SMC:PHYMAN.SMM')
	SWITCH = 5
	XCALL FILES (1, 'SU', 197, SWITCH)	;197 - PHYMAN
	IF (SWITCH .EQ. 9) RETURN
	CHN197 = 1

	SWITCH = 5
	XCALL FILES (2, 'SI', 41, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN041 = 2

	SWITCH = 5
	XCALL FILES (17, 'SI', 182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 17

;;;	XCALL FILES (17, 'SI', 182, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN182 = 3

	OPNOK = 1
	RETURN
;--------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN197) CLOSE CHN197
	IF (CHN041) CLOSE CHN041
	IF (CHN182) CLOSE CHN182

	RETURN
;--------------------------------------------

