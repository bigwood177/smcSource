;tprtst.cp
;
;	every 30 minutes
;	calls tprord (cmpcod) for 'tst'
;

.include 'def:hpsub.def'

record	vars
	cmpcod	,a3
;
proc
	cmpcod = 'TST'

	xcall tprord (cmpcod)
	xcall smalor (cmpcod)

	xcall flags (7000000)
	stop
end
