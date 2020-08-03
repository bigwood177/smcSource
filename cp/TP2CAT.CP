;tp2CAT.cp
;
;	nite batch
;	calls tpror2 (cmpcod) for 'cat'
;

.include 'def:hpsub.def'

record	vars
	cmpcod	,a3
;
proc
	cmpcod = 'CAT'

	xcall tpror2 (cmpcod)
	xcall smalor (cmpcod)

	xcall flags (7000000)
	stop
end
