.subroutine exprs
	x,	d
	y,	d

	.include 'def:hpsub.def'

record	code
	esc	,a1	;esc
	cd	,a20

record	idx_sel
	e5	,a1	;esc
		,a*,	'*v'
	isidx	,d2
		,a*,	'S'

record	vars
	idx	,d2
	ln	,d6
	row	,d6
	col	,d6
	ax	,a6
	ay	,a6
.proc
	idx = 4				;red
	ax = (x * 300), 'ZZZZZX' [LEFT]	;300 = 1 inch
	ay = (y * 120), 'ZZZZZX' [LEFT]
;;;	ay = (y * 300), 'ZZZZZX' [LEFT]

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

;;;	XCALL HP (14, hpFONT, hpBOLD+hp6CPI)
	XCALL HP (14, hpFONT, hpBOLD+hp5CPI)
	display (14, 'EXPRESS')
	xcall HP (14, hpFONT,hpMEDIUM+hp10CPI)

	
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
