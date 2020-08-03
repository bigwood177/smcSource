;r_init.cp

record	joblog
	.include 'def:rd076b.def'
;
proc
	open (1,su,'tst:joblog.tsm')
	clear joblog
	rh_seq = 1
	store (1,joblog,rh_key)
	close 1
	stop
	end



