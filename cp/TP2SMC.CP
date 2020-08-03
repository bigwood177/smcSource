;tp2smc.cp
;
;	nite batch
;	calls tpror2 (cmpcod) for 'smc'
;

.include 'def:hpsub.def'

record	vars
	cmpcod	,a3
;
proc
	cmpcod = 'SMC'

	xcall tpror2 (cmpcod)
	xcall smalor (cmpcod)

	xcall flags (7000000)
	stop
end
