;tprCAT.cp
;
;	every 30 minutes
;	calls tprord (cmpcod) for 'cat'	
;

.include 'def:hpsub.def'

record	vars
	cmpcod	,a3
;
proc
	cmpcod = 'CAT'

	xcall tprord (cmpcod)
	xcall smalor (cmpcod)

	xcall flags (7000000)
	stop
end
