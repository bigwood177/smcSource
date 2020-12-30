;is_prf.cp
function is_prf
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
	
	value = 'Y'		;assum true
	xcall ffile (45, redfil, switch)
	redfil(14,14) = 'M'
	open (chn045=0, si, redfil)

	clear ordlin
	lordno = order
	find (chn045, ordlin, ordkey) [err=loop]
loop,
	xcall ios (chn045, ordlin, read, lokctl)
	if (lordno .ne. order) goto eof
	if (ltype .eq. 'M') goto loop
	
	clear coptbl
	tblcod = 'PE'		;express weight
	pe_item = lcfgim
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .eq. 0) goto loop	;good

	value = 'N'			;not in table
	
eof,
	freturn value
end

