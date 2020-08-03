;maildp.cp
;
;	every 30 minutes
;	calls maldpt (cmpcod) for 'tst'
;

.include 'def:hpsub.def'

record	vars
	cmpcod	,a3
;
proc
	cmpcod = 'SMC'

	xcall maldpt (cmpcod)

	xcall flags (7000000)
	stop
end
