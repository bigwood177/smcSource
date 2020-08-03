;reolvl.cp
;
;reset the rolling 12 month average (reolvl).

record	itmmas
	.include 'def:rd041a.def'
;
record	dis
	ii	,d6

record	vars
	mon	,d2	;month
	date	,d6
;
proc
	open (15,i,'tt:')
	display (15,$scr_clr(screen),'Reolvl - reset 12 month rolling avg')
	xcall rdate (date)
	mon = date(1,2)
	mon = mon -1
;
;;;	open (1,su,'tst:itmmas.tsm')
	open (1,su,'smc:itmmas.smm')
;
	clear ii
loop,
	incr ii
	if (ii/500*500 .eq. ii) display (15,$scr_pos(1,70),dis)
ragain,
	reads (1, itmmas, eof) [lock=lock]

	reolvl = (qtyytd-qtymtd)/mon
	write (1, itmmas, itemno)
	goto loop
lock,
	sleep 1
	goto ragain
eof,
	close 1
	stop
	end

