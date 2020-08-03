;this version is calibrated to the line :
; 300 = 1 inch, 50 = 1 line at 6 lines/inch

.subroutine rbox2	
	x,	d
	y,	d
	idx,	d
	lpi,	d	;lines/inch
	cpi,	d	;width of box in char (10 cpi)

record	code
	esc	,a1	;esc
	cd	,a20

record	idx_sel
	e5	,a1	;esc
		,a*,	'*v'
	isidx	,d2
		,a*,	'S'
record	c_wid
		,a*,	'*c'
	cwid	,a3
		,a1,	'A'

record	vars
	inch	,d6
	ln	,d6
	row	,d6
	col	,d6
	ax	,a6
	ay	,a6
.proc
	inch = 300/lpi			;300 = 1 inch
	
;;;	ax = (x * inch), 'ZZZZZX' [LEFT]	
	ax = (x * inch/2), 'ZZZZZX' [LEFT]	
	ay = (y * inch), 'ZZZZZX' [LEFT]

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

;;;	cd = '*c300A'	;width, 1 inch
;;;	cd = '*c150A'	;width, 1/2 inch
	cwid = cpi*30
	cd = c_wid
	call put

;;;	cd = '*c75B'	;height
	cd = '*c50B'	;height

	call put

	cd = '*c90G'	;70%
;;;	cd = '*c50G'	;50%
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

