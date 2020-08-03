function is_exp
	order	,d	;order number
	chn182	,d	;channel coptbl open on
;
record	coptbl
	.include 'def:rd182a.def'

record	ordlin
	.include 'def:rd045a.def'
record,x
	.include 'def:rd045d.def'

record	vars
	hasL	,d1	;0= no dept L on order
	boxqty	,d6
	sr_qty	,d6	;square to round 
	totwgt	,d10
	maxwgt	,d6
	value	,a1
	chn045	,d4
	redfil	,a14
	read	,d1,0
	lokctl	,d1
	switch	,d1

proc
	
	value = 'N'		;assum false
	xcall ffile (45, redfil, switch)
	redfil(14,14) = 'M'
	open (chn045=0, si, redfil)

	clear coptbl
	tblcod = 'DD'
	dd_dept = 'L'
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .ne. 0) goto endoff
	maxwgt = dd_exlb			;max weight for express orders

	totwgt = 0
	boxqty = 0
	sr_qty = 0
	hasL = 0				;assume no dept L items

	clear ordlin
	lordno = order
	find (chn045, ordlin, ordkey) [err=loop]
loop,
	xcall ios (chn045, ordlin, read, lokctl)
	if (lordno .ne. order) goto eof
	if (ltype .eq. 'M') goto loop

	using ldept(1,1) select
	('L'),	nop
	('M'),	nop
	(),	goto loop
	endusing

;;;	if (ldept(1,1) .ne. 'L') goto loop

	if (lxprs .ne. 'Y') goto endoff			;all or none
	hasL = 1					;at least 1 dept L item (or "M")

	using litmno select
	('B.'),		begin
			boxqty = boxqty + lqtyor
			if (boxqty .gt. 5) goto endoff
			end
	('CB.'),	begin
			boxqty = boxqty + lqtyor
			if (boxqty .gt. 5) goto endoff
			end	
	('SR'),		begin
			sr_qty = sr_qty + lqtyor
			if (sr_qty .gt. 5) goto endoff		;5-15-19
			end
	endusing

	
	clear coptbl
	tblcod = 'EW'		;express weight
	ew_item = lcfgim
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .ne. 0) goto loop
	totwgt = totwgt + lqtyor
	goto loop
	
eof,
	if (hasL .and. totwgt.lt.maxwgt) value = 'Y'
	if (totwgt .lt. 1) value = 'N'			;

endoff,
	freturn value
end

