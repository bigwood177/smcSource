subroutine par2		;s_parse substitute
	in	,a	;in string
	pos	,d	;array of start pos
	len	,d	;array of lenght
	type	,d	;array of types (8 = comma)
	n	,d	;actual number of elements
	nl	,d	;number of array elements
;
record	vars
	i	,d6
	j	,d6
	ix	,d6
	ln	,d6
	p1	,d6
	p2	,d6
	string	,a4096	;long string
;
proc

	string = in

	clear in, j
	ln = %trim(string)
	for i from 1 thru ln
		begin
		if (string(i,i) .ne. '"')
			begin
			incr j
			in(j,j) = string(i,i)
			end
		end

	p1 = 1
	p2 = 0
	for ix from 1 thru nl
		begin
		clear pos(ix)
		clear len(ix)
		clear type(ix)
		end

	clear ix, n
	do forever
		begin
		incr ix
		if (ix .gt. nl) return
		pos(ix) = p1
		p2 = %instr(p1, in, ',')	;find the target
		if (p2 .le. 0) goto done
		len(ix) = p2 - p1
		type(ix) = 8			;comma
		p1 = p2 + 1
		incr n				;number of elements
		end
done,
	if (p1 .lt. ln)				;1 last field...
		begin
		pos(ix) = p1
		p2 = ln
		len(ix) = p2 - p1
		type (ix) = 8
		incr n
		end

	return

