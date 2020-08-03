.function IsLastDept
			
	order	,d
	chn193	,d	;dptsts channel opened in input mode

record	dptsts
	.include 'def:rd193a.def'


record	vars
	value	,d1
			
.proc
	value = 0
	if (order .le. 0) goto eoff

	value = 1		;assume all dept's are closed
	
	clear dptsts
	S_ORDN= order
	find (chn193, dptsts, S_KEY) [err=loop]
loop,
	reads (chn193, dptsts, eoff)
	if (S_ORDN .ne. order) goto eoff
	if (S_DEPT .eq. '**') goto loop	;skip header
	if (S_STAT .ne. 3) value = 0	;at least one dept is open
	goto loop
eoff,
	freturn value				
.end
	
