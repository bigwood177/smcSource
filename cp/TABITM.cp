subroutine tabitm	;now a subroutine called by cstmrd

;
record	itmmas
	.include 'def:rd041a.def'

record	dis
	ii	,d6

record	buf
	,a80

record	vars
	tab	,a1
;
proc
;;;	open (15,i,'tt:')
;;;	display (15, $scr_clr(screen), 'tabitm')
	xcall outpt (4,35,1,'STOCKED ITEMS (STOCKED_ITEMS.XLS)',1)

	open (1,si,'smc:itmmas.smm')
	open (2, o, 'SPL:STOCKED_ITEMS.XLS')

	xcall ascii(9,tab)
	clear ii

	find (1, itmmas, ^first)
loop,
	incr ii
	if (ii/500*500 .eq. ii) display (15,$scr_pos(2,1),dis)

	reads (1, itmmas, eof)
	if (stock .ne. 'S') goto loop

	clear buf
	xcall tabz(buf, 80, itemno,tab)	;item
	xcall tabz(buf, 80, descr,tab)	;descr
	writes (2, buf(1, %trim(buf)) )
	goto loop
eof,
	close 1
	close 2
	XRETURN

	end

