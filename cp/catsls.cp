subroutine catsls	;stocked sales by category
			;from catsls.dbl
			;create "stocked_sales.xls"
;
;catsls.dbl
;
;	sales for 2014
;	sales by category for non-stocked items;2010 & 2011
;
record	ordhdr
	.include 'def:rd044a.def'

record	ordlin
	.include 'def:rd045a.def'

record	out
	o_cat	,a2
	o_dpt	,a2
	o_sales_10	,d12

record	buffer
	buf	,a100


record	dis
	ii	,d6

record	vars
	wrkfil	,a14,	'spl:wrkfil.ism'
	i	,d6
	sales	,d12

	tl	,d6
	rl	,d6
	t2	,d6
	tab	,a1

;
proc
	xcall outpt (1,35,1,'STOCKED SALES BY CAT (STOCKED_SALES.XLS)',1)
	
	XCALL ISAMC (wrkfil, 16, 1, 'start=1, length=2, nodups, ascend')
	open (1, su, wrkfil)
	open (10,o,'catsls.dat')
new_year,
	call opens
	clear ii
loop,
	incr ii
	if (ii/1000*1000 .eq. ii) display (15,$scr_pos(2,1),dis)
	reads (4, ordlin, eof)
	if (ltype .eq. 'M') goto loop	
	if (lstokt .ne. 'S') goto loop

	if (lordno .ne. oordno) read (5, ordhdr, lordno)
	if (ocusno .eq. 63340) goto loop	;rockford

	using lprdcd select
	('B','T'),	call wtf
	endusing

	sales = (lqtysh * lprice)#1

	read (1, out, lprdcd) [err=new_cat]

	o_sales_10 = o_sales_10 + sales

	write (1, out, o_cat)
	goto loop
	
new_cat,
	clear out

	o_cat = lprdcd
	o_dpt = ldept
	o_sales_10 = sales


	store (1, out, o_cat)
	goto loop

wtf,
	writes (10, litmno)
	return
eof,
	close 4
	close 5
	close 1
	close 10

;; the rest (catsl2)


	xcall ascii (9,tab)

	open (1, si, wrkfil)
	open (2,o,'spl:stocked_sales.xls')

	buf = 'cat' + tab + 'dpt' + tab + 'Sales' ;>>+ tab + '2015'
;;;	buf = 'cat' + tab + 'dpt' + tab + '2016' ;>>+ tab + '2015'

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

	buf(tl,100) = o_dpt
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl, tl+12) = o_sales_10, 'ZZZZZZZZZZ.XX' [LEFT]	
	tl = %trim(buf)


	rl = %trim(buffer)
	writes (2,buffer(1,rl) )

	goto loop2
	
eof2,
	close 1
	close 2
	xreturn



;;-------------------
	stop

opens,
	open (4,si,'smc:slhl18.smm')
	open (5,si,'smc:slhh18.smm')

;;;	open (4,si,'smc:slhlin.smm')
;;;	open (5,si,'smc:slhhdr.smm')


	return

