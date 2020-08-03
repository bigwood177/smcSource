subroutine of128		;turn off barcode
	
record	off
	o_esc	,a1
		,a3,	'(3@'

record	vars
	n	,d6
;
proc
	xcall ascii (27,o_esc)

	display(14,off)

	xreturn
	end
	

