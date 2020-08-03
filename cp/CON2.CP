subroutine con2			;customers who get email by ship-to
	cust	,d
	ship	,d
	nbr	,d
	cash_cust,d		;1 = cash cust
;TESTE2.DBL
;

.INCLUDE 'WND:TOOLS.DEF'
.INCLUDE 'WND:WINDOWS.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	CPOP		
	.INCLUDE 'DEF:POP1.DEF'

RECORD	SPOP		
	.INCLUDE 'DEF:POP1A.DEF'

RECORD	WN_NAME
		,A5,	'EMAIL'
	WN_TNMBR,D4

RECORD	WARS
	WND_1	,D4
	W_ID	,D4

RECORD	VARS
	ct_key2	,a2
	ct_key3	,a2
	func	,d1
	last_seq	,d3
	first	,d1
	read	,d1,0
	write	,d1,1
	lokctl	,d1
	switch	,d1
	chn182	,d2
	chn084	,d2
	opnok	,d1
	savrfa	,a6
	a1	,a1
;;;	CUST	,D6	- passed as parameter
;;;	SHIP	,D4	- passed as parameter
	XNBR	,D3
	XNAM	,A10
	XCUST	,D6
	I	,D6
;
PROC
;;;	open (15,i,'tt:')
	CALL INIT_WINDOW

	CALL OPENS

	if (cash_cust)
	then	begin
		ct_key2 = '92'
		ct_key3 = '93'
		end
	else	begin
		ct_key2 = 'E2'
		ct_key3 = 'E3'
		end

;;;	CALL GET_SHIP

	if (ship .eq. 0 .or. aNUMARA.EQ.0)
		begin
		clear xnbr
		func = 1	;get
		xcall emcmn (cust, xnbr, chn084, func)
	;;;	xcall cont (cust, xnbr)
		nbr = xnbr
		close chn182
		close chn084
		xreturn
		end

	first = 1
START,
	CALL GET_CUST

	DLINE = 'CUSTOMER EMAIL'
	if (first) goto fini

LOOP,
	CALL GET_SHIP
	aDLINE = 'SHIP-TO'

	XCALL CSPOP (CPOP, SPOP)

	USING P_ACTION SELECT
	(1),	BEGIN
		CALL SET
		CALL SELECTED
		END

	(2),	BEGIN		;INSERT
		CALL SET
		CALL INSERT_new
		END

	(3),	BEGIN		;DELETE
		CALL SET
		CALL DELETE
		END
	(4),	BEGIN
		CALL GET_MORE
		IF (NUMARA.EQ.0) CALL GET_CUST
		END
	(),	GOTO FINI
	ENDUSING

	GOTO LOOP

FINI,
	first = 0

	CALL GET_SHIP
	aDLINE = 'SHIP-TO'
F_LOOP,
	XCALL POP1 (SPOP)	;SELECT SHIPTO

	USING aP_ACTION SELECT
	(1),	BEGIN
		CALL SET_a
		GOTO FINI2
		END

	(2),	BEGIN		;INSERT
		GOTO START
		END

	(3),	BEGIN		;DELETE
		NOP
		END

	(4),	BEGIN
		NOP
		END

	(),	BEGIN
		CLEAR XNBR
		GOTO FINI
		END
	ENDUSING

	GOTO F_LOOP

FINI2,
	nbr = xnbr
	if (chn182) close chn182
	if (chn084) close chn084
	xreturn

;;;	STOP

SET,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	DLINE = PARRY(PI)
	XNBR =  DLINE(1,3)
	XNAM =  DLINE(12,21)

	RETURN
;--------------------------------------------
SET_a,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	DLINE = aPARRY(aPI)
	XNBR =  DLINE(1,3)

	RETURN
;--------------------------------------------

insert_new,
	xnbr = -1		;insert
	xcall cont (cust, xnbr, cash_cust)
	reads (15,a1)
	call get_cust
	return

SELECTED,;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (PFLAGS(PI) .EQ. 1) CALL INSERT
	IF (PFLAGS(PI) .EQ. 0) CALL DELETE
	RETURN
;--------------------------------------------

INSERT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR TBL_KEY
	TBLCOD = ct_key3
	E3_CUST = CUST
	E3_SHIP = SHIP
	E3_NBR = XNBR
	FIND (chn182, COPTBL, TBL_KEY) [ERR=NO_CUST]

	RETURN			;ALREADY IN TABLE
NO_CUST,
	CLEAR COPTBL
	TBLCOD = ct_key3
	E3_CUST = CUST
	E3_SHIP = SHIP
	e3_seq = last_seq + 1

	E3_NBR = XNBR
	E3_FNAM = XNAM

	STORE (chn182, COPTBL, TBL_KEY)
	
	RETURN
;--------------------------------------------
DELETE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = ct_key3
	E3_CUST = CUST
	E3_SHIP = SHIP
;;;	E3_NBR = XNBR
	find (chn182, COPTBL, TBL_KEY) [ERR=de_loop]
de_loop,
	xcall ios (chn182, coptbl, read, lokctl)
	if (lokctl .ne. 0) goto not_del
	if (tblcod .ne. ct_key3) goto not_del
	if (e3_cust .ne. cust) goto not_del
	if (e3_ship .ne. ship) goto not_del
	if (e3_nbr .ne. xnbr) goto de_loop


	DELETE(chn182)

NOT_DEL,			;NOT IN TABLE
	RETURN
;--------------------------------------------
GET_MORE,
	read (chn182, COPTBL, RFA:PARRFA(numara) )

	CALL CLEAR_CUST_ARRAY
	GOTO GC_LOOP


GET_CUST,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CALL CLEAR_CUST_ARRAY

	CLEAR TBL_KEY
	TBLCOD = ct_key2
	e2_CUST = CUST
	FIND (chn182, COPTBL, TBL_KEY) [ERR=GC_LOOP]
	
GC_LOOP,
	READS (chn182, COPTBL, GC_EOF, GETRFA:SAVRFA)
	IF (TBLCOD .NE. ct_key2) GOTO GC_EOF
	if (e2_cust .ne. cust) goto gc_eof
	clear dline
	DLINE (1,3) = e2_NBR,	'ZZX'
	DLINE (5,10) = e2_CUST,	'ZZZZZX'
	DLINE (12,21) = e2_FNAM
	DLINE (23,45) = e2_MAIL
	INCR I
	PARRY (I) = DLINE
	PARRFA (I) = SAVRFA
	if (i .lt. maxara) GOTO GC_LOOP	
GC_EOF,
	NUMARA = I
	unlock chn182
	RETURN
;--------------------------------------------

CLEAR_CUST_ARRAY,

	CLEAR NUMARA
	FOR I FROM 1 THRU 20
		BEGIN
		CLEAR PARRY(I)
		CLEAR PARRFA(I)
		CLEAR PFLAGS(I)
		END
	CLEAR I
	RETURN
;--------------------------------------------


GET_SHIP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR aNUMARA
	FOR I FROM 1 THRU 20
		BEGIN
		CLEAR aPARRY(I)
		CLEAR aPARRFA(I)
		CLEAR aPFLAGS(I)
		END

	CLEAR I
	CLEAR TBL_KEY
	TBLCOD = ct_key3
	E3_CUST = CUST
	E3_SHIP = SHIP

	FIND (chn182, COPTBL, TBL_KEY) [ERR=GS_LOOP]
	
GS_LOOP,
	READS (chn182, COPTBL, GS_EOF)
	IF (TBLCOD .NE. ct_key3) GOTO GS_EOF
	if (e3_cust .ne. cust) goto gs_eof
	if (e3_ship .ne. ship) goto gs_eof
	clear dline
	DLINE (1,3) = E3_NBR,	'ZZX'
	DLINE (5,14) = E3_FNAM
	INCR I
	aPARRY (I) = DLINE

	last_seq = e3_seq
	GOTO GS_LOOP	
GS_EOF,
	aNUMARA = I
	unlock chn182
	RETURN
;--------------------------------------------

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

	chn182 = 16
	do	begin
		if (.not. %chopen(chn182) ) exitloop
		incr chn182
		end
	until (chn182 .ge.98) 
	xcall files (chn182, 'SU', 182, switch)

	chn084 = chn182 + 1
	do	begin
		if (.not. %chopen(chn084) ) exitloop
		incr chn084
		end
	until (chn084 .ge.98) 
	switch = 5

	if (cash_cust)
	then	xcall files (chn084, 'SU', 103, switch)	;misemc
	else	xcall filec ('SMC',chn084, 'SU', 084, switch)	;emailc

	opnok = 1
	return
;-----------------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	xcall u_start("lib:smc_wnd",,,,,,,200)
;	xcall e_sect("Bills of Lading",D_CAPTION)
;	XCALL TNMBR (WN_TNMBR)
;	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
;	IF (.NOT. WND_1)
;		BEGIN
;		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)
;		END
;	XCALL W_BRDR(WND_1,WB_TITLE,'Test Cust / Ship-To',
;&			WB_TPOS,WBT_TOP,WBT_CENTER)
;	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
;	XCALL W_DISP(WND_1,WD_CLEAR)
;	XCALL W_UPDT
;	W_ID = WND_1
;	xcall u_logwnd(wnd_1)
;
;; POP info...

	MAXARA = 20
	PLEN = 46
	NUMROW = 15
	WX = 10
	WY = 5
	POP_WID(1,5) = "CSPOP"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "Customer Email"

;; POP info...

	aMAXARA = 20
	aPLEN = 14
	aNUMROW = 15
	aWX = 10
	aWY = 60
	aPOP_WID(1,5) = "SHPOP"
	aPOP_WID(6,8) = WN_TNMBR,	'XXX'
	aPOP_TITLE = "Ship-To Email"

	RETURN
;--------------------------------------------
END


