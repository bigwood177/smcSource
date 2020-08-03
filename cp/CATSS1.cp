subroutine catss1		;now a subroutine called by cstmrd.cp

;
;	sales by category for non-stocked items
;
record	ordhdr
	.include 'def:rd044a.def'

record	ordlin
	.include 'def:rd045a.def'

record	out
	o_cat	,a2
	o_mon	,d2
	o_sales		,d12
	o_tot		,d12
;
record	buffer
	buf	,a100

record	dis
	ii	,d6

record	vars
	wrkfil	,a14,	'spl:wrkfil.ism'
	mon	,d2
	i	,d6
	sales	,d12
	tl	,d6
	rl	,d6
	t2	,d6
	tab	,a1

;
proc
	xcall outpt (3,35,1,'NON-STOCK & TOTAL SALES BY CAT (CUSTOM.XLS)',1)

	XCALL ISAMC (wrkfil, 28, 1, 'start=1, length=2, nodups, ascend')
	open (1, su, wrkfil)
	open (10,o,'catsls.dat')

;;;	open (5,si,'smc:slhhdr.smm')
;;;	open (4,si,'smc:slhlin.smm')

; for after year end...
	open (5,si,'smc:slhh18.smm')
	open (4,si,'smc:slhl18.smm')

	clear ii
loop,
	incr ii
	if (ii/1000*1000 .eq. ii) display (15,$scr_pos(2,1),dis)
	reads (4, ordlin, eof)
	if (ltype .eq. 'M') goto loop	

	if (oordno .ne. lordno) read (5,ordhdr,lordno)

	
	mon = oinvdt(5,6)
	sales = (lqtysh * lprice)#1


	read (1, out, lprdcd) [err=new_cat]
	using lstokt select
	('S'),	begin
		o_sales = o_sales + sales
		if (lprdcd .eq. 'B') call isb
		end
	endusing
	o_tot = o_tot + sales

	write (1, out, o_cat)
	goto loop
isb,
	i = 6
	return	
new_cat,
	clear out

	o_cat = lprdcd
	using lstokt select
	('S'),	o_sales = o_sales + sales
	endusing
	o_tot = o_tot + sales

	store (1, out, o_cat)
	goto loop

eof,

	close 4
	close 5
	close 1
	close 10

;part 2 (catss2)
;catss2
;

	xcall ascii (9,tab)

	open (1, si, wrkfil)
	open (2,o,'spl:custom.xls')

	buf = 'cat' + tab + 'non custom' + tab + 'total'
	rl = %trim(buf)
	writes (2, buffer(1,rl) )

loop2,

	reads (1, out, eof2)
	rl = %trim(out)
	if (rl .lt.5) goto loop2

	buf = o_cat
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

;;;	buf(tl,100) = o_dpt
;;;	tl = %trim(buf)
;;;	incr tl
;;;	buf(tl,tl) = tab
;;;	incr tl

	buf(tl, tl+12) = o_sales, 'ZZZZZZZZZZ.XX' [LEFT]	
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl, tl+12) = o_tot, 'ZZZZZZZZZZ.XX' [LEFT]	

	rl = %trim(buffer)
	writes (2,buffer(1,rl) )

	goto loop2
	
eof2,
	close 1
	close 2
	xreturn

	end

;;-------------




