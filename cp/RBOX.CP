.subroutine rbox
	x,	d
	y,	d
	idx,	d


record	code
	esc	,a1	;esc
	cd	,a20

record	idx_sel
	e5	,a1	;esc
		,a*,	'*v'
	isidx	,d2
		,a*,	'S'

record	vars
	ln	,d6
	row	,d6
	col	,d6
	ax	,a6
	ay	,a6
.proc
	ax = (x * 300), 'ZZZZZX' [LEFT]	;300 = 1 inch
	ay = (y * 300), 'ZZZZZX' [LEFT]

	xcall ascii (27, esc)
	e5 = esc

	isidx = idx
	display (14, idx_sel)			;select the color

	cd = '*p' + ax

	cd(%trim(cd)+1, 20) = 'x'
	cd(%trim(cd)+1, 20) = ay
	cd(%trim(cd)+1, 20) = 'Y'
	call put

	call put

	cd = '*c300A'	;width, 1 inch
	call put

;;;	cd = '*c75B'	;height
	cd = '*c50B'	;height
	call put

	cd = '*c90G'	;70%
	call put

	cd = '*c2P'
	call put
	
	isidx = 1	;back to black
	display (14, idx_sel)			;select the color
	xreturn
;---------------------------------------------

put,
	ln = %trim (code)
	display (14,code(1,ln) )
	return
;-------------------------------------

.end
