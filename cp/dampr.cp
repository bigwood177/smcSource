function dampr
	item	,a
;
record	vars
	ln	,d6
	fl	,d6
	i	,d6
	partno	,a15

proc
	partno = item
	
	ln = %trim (partno)

	for i from 1 thru 3
		begin
		xcall instr (1, partno, '*', fl)
		if (.not. fl)exitloop
		partno(1,fl) = partno(fl+1, ln)
		end


	freturn partno

end
