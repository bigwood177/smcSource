;n_opnord.cp
;
;	6-16-14: opnord.cp is now a subroutine.
;
record	vars
	nite	,d1
;
proc
	nite = 1
	xcall opnord(nite)
	xcall flags (7000000)
	stop
	end

