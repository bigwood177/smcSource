;concat.cp
;
;

.include 'def:hpsub.def'

record	vars
	cmpcod	,a3
;
proc
	cmpcod = 'CAT'

	xcall confr1 (cmpcod)

	xcall flags (7000000)
	stop
end

