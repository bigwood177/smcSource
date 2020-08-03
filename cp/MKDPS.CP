; create dptsts records when order printed
; will not overwrite existing records

;6-20-17 ssq: add record for header.
;11-26-18 ssq: don't create dptsts records for excluded items.
; 01-28-20 ssq: IC sb based on consolidated qty

SUBROUTINE MKDPS
	OORDNO	,D	;OORNO
	OCUSNO	,D	;CUST #
	CHN045	,D	;ORDLIN
	CHN175	,D	;DUCACC
	CHN182	,D	;COPTBL

EXTERNAL FUNCTION
	IS_IC	,D

EXTERNAL FUNCTION		;1-28-20
	IS_IC2	,D

record	key3
	k3_ord	,d6
	k3_dept	,a2
	k3_item	,a15
	k3_desc	,a30

record	icitem			;1-28-20
	ic_item	,a15
	ic_qty	,d6
	
RECORD	ICFIL
		,A4,	'SPL:'
		,A1,	'C'
		OCORD	,A6
		,A4,	'.ISM'


RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.DEF'

RECORD	DUCACC
	.INCLUDE 'DEF:RD175A.DEF'

RECORD	DPTSTS
	.INCLUDE 'DEF:RD193A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'		;11-26-18

;
RECORD	VARS
	is_ic_item	,d1		;1-28-20
	totqty		,d6		;1-28-20
	savitm		,a15	;1-28-20
	chnic		,d2		;1-28-20
	CHN193	,D3
	WD_DEPT	,A2
	GOTO	,D1	;GOT DEPT "O"
	GOTP	,D1	;GOT DEPT "P"
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
	
	chnic = %SYN_FREECHN(50,99)		;
	OCORD = OORDNO, 'XXXXXX'
	XCALL ISAMC (ICFIL, 21, 1, 'START=1, LENGTH=15, NODUPS, ASCEND')
	OPEN (CHNIC, SU, ICFIL)

	CALL LOAD_IC
	
;6-20-17 add record for header

	CLEAR DPTSTS
	S_ORDN = OORDNO
	S_DEPT = '**'		;header
	XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)


	CLEAR GOTP, GOTO
	READ (CHN175, DUCACC, OORDNO) [ERR=FNDDUC]
FNDDUC,
	WHILE (DUCTOR .EQ. OORDNO)
		BEGIN
		XCALL DDEPT (GAUGE,SIZE3,DUTYPE,WD_DEPT,STY)
		USING WD_DEPT SELECT
		('O'),	GOTO = 1
		('P'),	GOTP = 1
		ENDUSING
		READS(CHN175, DUCACC, EOF_FDD)
		END

EOF_FDD,	
	CLEAR DPTSTS
	S_ORDN = OORDNO
	IF(GOTP)
		BEGIN
		S_DEPT(1,1) = 'P'
		XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)
		END

	IF(GOTO)
		BEGIN
		S_DEPT(1,1) = 'O'
		XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)
		END

FNDLIN,
	FIND (CHN045, ORDLIN, OORDNO) [ERR=NXTLIN]
NXTLIN,
	XCALL IOS (CHN045, ORDLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF_LIN
	IF (LORDNO .NE. OORDNO) GOTO EOF_LIN
	IF (LTYPE .EQ. 'M') GOTO NXTLIN			;SKIP MEMOS
	IF (%INSTR(1,LDESCR,'DISCOUNT') ) GOTO NXTLIN	;ssq 1-17-08 SKIP DISCOUNTS

; check for excluded parts ... 11/26/18
	CLEAR TBL_KEY
	TBLCOD = 'EX'
	TBLKEY = EX_ITEM = LITMNO
	FIND (CHN182,COPTBL,TBL_KEY) [ERR=LCONT]	;continue if NOT in table
	GOTO nxtlin
lcont,

; create a DPTSTS record for every dept in order.  If there are
; already records in DPTSTS file, they will not be over-written.
	CLEAR DPTSTS
	S_ORDN = LORDNO

; 8-5-19 no more "V" for vulcan

	xcall cvcan (ordlin, ocusno, chn182, is_vulcan)
;;;	if (is_vulcan) ldept(2,2) = 'V'

	S_DEPT(1,1) = LDEPT(1,1)
;;;	if (ldept(2,2) .eq. 'V') s_dept(2,2) = 'V'


	if (ldept .eq. 'C')
		begin
		is_ic_item = 0	;assume not an IC item				;1-28-20
		if (%is_ic(ordlin)) 
			begin
			xcall isio (chnic, icitem, lcfgim, read, lokctl) ;1-28-20
			if (lokctl.eq.0) is_ic_item = 1
			end
		if (is_ic_item)	s_dept = 'IC'
		end
;;;	if (ldept.eq.'C' .and. %is_ic(ordlin) ) s_dept = 'IC'	;6-24-19

	XCALL ISIO (CHN193, DPTSTS, S_KEY, STORE, LOKCTL)


	GOTO NXTLIN

LOAD_IC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; dept IC SB based on 
		;;; consolidated qty
		;;;;;;;;;;;;;;;;;;;;;;;;;;;

	k3_ord = oordno
	k3_dept = 'C '
	k3_item =
	k3_desc =

	savitm = '-1'

	find (chn045, ordlin, key3, krf=3) [err=ic_loop]
ic_loop,
	reads (chn045, ordlin, eof_ic)
	if (lordno .ne. oordno) goto eof_ic
	if (ldept .ne. 'C') goto ic_loop
	if (lcfgim .ne. savitm) call newitm
	if (.not. %is_ic2(ordlin) ) goto ic_loop
	totqty = totqty + lqtyor
	goto ic_loop

eof_ic,
	call newitm
	return

newitm,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (savitm .eq. '-1') goto outitm
	using totqty select
	(0),		goto outitm	;no ic items
	(.gt.9),	goto outitm	;qty more than 9
	(),		begin
			ic_item = savitm
			ic_qty = totqty
			xcall isio  (chnic, icitem, ic_item, store, lokctl)
			end
	endusing


outitm,
	savitm = lcfgim
	totqty =
	return
;--------------------------------------------
	
EOF_LIN,

ENDOFF,

	IF (CHN193) CLOSE CHN193
	IF (CHNIC)
		BEGIN
		CLOSE CHNIC	
		XCALL DELET (ICFIL)
		END
		
	XRETURN

