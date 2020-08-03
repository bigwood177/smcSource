;tprROC.cp
;
;	every 30 minutes
;	calls tprord (cmpcod) for 'roc'
;

.include 'def:hpsub.def'

record	vars
	cmpcod	,a3
;
proc
	cmpcod = 'ROC'

	xcall tprord (cmpcod)
	xcall smalor (cmpcod)

	xcall flags (7000000)
	stop
end
