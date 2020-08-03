.function sskey		;subst le for ss
	in	,d

record	vars
	dec	,d5
	adig	,a1
	a5	,a5
	i	,d5
.proc

	a5 = in, 'XXXXX'

	for i from 1 thru 5 if(a5(i,i).eq.'1') a5(i,i)='8'	;le

	dec = a5

	freturn dec
.end
