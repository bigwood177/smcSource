subroutine cp_makp		;make path from order #
	order	,d	;order #
	path	,a	;path to pdf files
;
record	vars
	temp	,a100
	server	,a*,	'\\192.168.1.10\scan\'		;9-19-16

	num	,a2
	aord	,a6
;
proc
	num = order(1,2),	'XX'
	aord = order,	'XXXXXX'
	temp = server + num + '\' + aord + '.pdf'
	path = temp
	xreturn
end

