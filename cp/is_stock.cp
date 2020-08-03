function is_stock	;returns "Y" if order is a stock order, else "N"
	order	,d	;order number
	chn182	,d	;channel coptbl open on
;

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

record	coptbl
	.include 'def:rd182a.def'

record	ordlin
	.include 'def:rd045a.def'
record,x
	.include 'def:rd045d.def'

record	ducacc
	.include 'def:rd175a.def'


RECORD	ICFIL
		,A4,	'SPL:'
		,A1,	'C'
	OCORD	,A6
		,A4,	'.ISM'

record	vars
	chnic	,d4
	is_ic_item	,d1
	hasI	,d1	;0= no dept I on order (or IC)
	savitm	,a15
	boxqty	,d6
	sr_qty	,d6	;square to round 
	totwgt	,d10
	totqty	,d10
	maxwgt	,d6
	value	,a1
	chn045	,d4
	chn175	,d4
	redfil	,a14
	read	,d1,0
	write	,d1,1
	store	,d1,2
	lokctl	,d1
	switch	,d1

proc
	
	value = 'N'		;assum false

	xcall ffile (175, redfil, switch)
	redfil(14,14) = 'M'
	open (chn175=0, si, redfil)

	find (chn175, ducacc, order) [err=cont]
	goto endoff				;7-22-20 can't have any duct

cont,
	xcall ffile (45, redfil, switch)
	redfil(14,14) = 'M'
	open (chn045=0, si, redfil)

; need this for IC items
	OCORD = ORDER, 'XXXXXX'
	XCALL ISAMC (ICFIL, 21, 1, 'START=1, LENGTH=15, NODUPS, ASCEND')
	OPEN (chnic=0, SU, ICFIL)

	clear coptbl
	tblcod = 'DD'
	dd_dept = 'I'
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .ne. 0) goto endoff
	maxwgt = dd_exlb			;max weight for express orders

	totwgt = 0
	boxqty = 0
	sr_qty = 0
	hasI = 0				;assume no dept I items
	
	call load_ic

	clear ordlin
	lordno = order
	find (chn045, ordlin, ordkey) [err=loop]
loop,
	xcall ios (chn045, ordlin, read, lokctl)
	if (lordno .ne. order) goto eof
	if (ltype .eq. 'M') goto loop

	using ldept select
	('C'),	begin
		is_ic_item = 0	;assume not an IC item	;1-28-20
		if (%is_ic(ordlin)) 
			begin
			xcall isio (chnic, icitem, lcfgim, read, lokctl) ;1-28-20
			if (lokctl.eq.0) is_ic_item = 1
			end
		if (is_ic_item)
		then	nop
		else	begin
			hasI =
			goto eof	;all it takes is one bad item
			end
		end

	('I '),	nop
	(),	begin
		hasI =			;can only have "I"
		goto eof		;done
		end
	endusing

	hasI = 1					;at least 1 dept I item (or "IC")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; don't need this for dept I
;	using litmno select
;	('B.'),		begin
;			boxqty = boxqty + lqtyor
;			if (boxqty .gt. 5) goto endoff
;			end
;	('CB.'),	begin
;			boxqty = boxqty + lqtyor
;			if (boxqty .gt. 5) goto endoff
;			end	
;	('SR'),		begin
;			sr_qty = sr_qty + lqtyor
;			if (sr_qty .gt. 5) goto endoff		;5-15-19
;			end
;	endusing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
;	clear coptbl
;	tblcod = 'EW'		;express weight
;	ew_item = lcfgim
;	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
;	if (lokctl .ne. 0) goto loop


	if (ldept .ne. 'IC') totwgt = totwgt + lqtyor
	goto loop

;================================================================
LOAD_IC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; dept IC SB based on 
		;;; consolidated qty
		;;;;;;;;;;;;;;;;;;;;;;;;;;;

	k3_ord = order
	k3_dept = 'C '
	k3_item =
	k3_desc =

	savitm = '-1'

	find (chn045, ordlin, key3, krf=3) [err=ic_loop]
ic_loop,
	reads (chn045, ordlin, eof_ic)
	if (lordno .ne. order) goto eof_ic
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
	
eof,
	if (hasI .and. totwgt.lt.maxwgt) value = 'Y'
	if (totwgt .lt. 1) value = 'N'			;

endoff,
	if (chnic) close chnic
	if (chn175) close chn175
	if (chn045) close chn045
	freturn value
end

