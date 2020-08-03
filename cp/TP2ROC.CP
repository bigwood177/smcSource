;tp2roc.cp
;
;	nite batch
;	calls tpror2 (cmpcod) for 'roc'
;

.include 'def:hpsub.def'

record	vars
	cmpcod	,a3
;
proc
	cmpcod = 'ROC'

	xcall tpror2 (cmpcod)
	xcall smalor (cmpcod)

	xcall flags (7000000)
	stop
end
