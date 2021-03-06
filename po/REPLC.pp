.subroutine	replc
	buf	,a	;buffer to be searched
	from	,a	;target string
	to	,a	;replacement string
	sts	,d	;success flag

record	out
	obuf	,a500

record	vars
	str1	,a100
	str2	,a100
	fl	,d4	;lenght of from
	tl	,d4	;lenght of to
	bl	,d4
	xl	,d4
	sl1	,d4
	sl2	,d4

.proc
	clear sts			;assume failure
	clear out

	bl = %trim(buf)
	if (bl .le. 1) return		;zero length buffer

	fl = %trim(from)
	if (fl .le. 1) return		;zero length target

	tl = %trim(to)			;zero length replacement
	if (tl .le. 1) return
	tl = tl + 1			;leave a space after rtf formatting
					; string

	xl = %instr(1, buf, from(1,fl) )	;find the target string
	if (xl .lt. 1) 	return			;target not found

	sl1 = 1					;start pos
loop,
	xl = %instr(sl1, buf, from(1,fl) )	;find the target string
	if (xl .lt. 1) goto done		;not found

	if (xl.gt.1) obuf(1,xl-1) = buf(1,xl-1)	;move in buffer to out buffer
	obuf(xl,xl+tl-1) = to(1,tl)		;move the "to" to out buffer
	if (xl+fl .gt. bl) 
	then	obuf(xl+tl,500) =
	else	obuf(xl+tl,500) = buf(xl+fl,bl);move remainder of in buffer to out buffer
	sl1 = xl+fl
	goto loop
done,
	sts = 1		;success
	buf = out
	return
.end
