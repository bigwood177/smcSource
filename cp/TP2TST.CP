;tp2tst.cp
;
;	nite batch
;	calls tpror2 (cmpcod) for 'tst'
;

.include 'def:hpsub.def'

record	vars
	cmpcod	,a3
;
proc
	cmpcod = 'TST'

	xcall tpror2 (cmpcod)
	xcall smalor (cmpcod)

	xcall flags (7000000)
	stop
end
