.subroutine tabd
	buf	,a	;working buffer
	len	,d	;lenght of buffer
	field	,a	;field to add
	tab	,a	;delimiter (tab)

record	vars
	ln	,d8

;
.proc
	ln = %trimz(buf)	;get end of string
	incr ln
	buf(ln,len) = field	;append field to end of buffer
	ln = %trimz(buf)	;get end of string
	if (ln .le. 0) xreturn	;empty string
	incr ln
	buf(ln,ln) = tab	;append tab to end of string
	xreturn
.end

