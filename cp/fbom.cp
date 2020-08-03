function fbom	;convert item to code
	item	,a
	chan	,d

; build a "code" item to return:
; eg: 	ED04 => EDXX
;	ED1324 => EDXXGA
;	ED0390 => EDXX90
;	ED0345 => EDXX45
;	
record	parent
	.include 'def:rd162a.def'


record	retval
	r_par	,a15		;item comverted to parent code
	r_seg	,10a2		;segments
	r_val	,10d2		;corresponing values

record	vars
	i	,d6
	j	,d6
	savcod	,a3
	xitem	,a15
	fitem	,a15	;return code
	lni	,d3	;length of item
	lnf	,d3	;length of parent 
	lcd	,d3	;length of code
	cseg	,a2	;current segment of item
	td	,d3	;current position
	num	,d3

proc
	clear fitem, savcod

	find (chan, parent, ^first) [err=loop]
loop,
	reads (chan, parent, eof)
	lcd = %trim(p_code)		;length of parent code
	lnf = %trim(savcod)		;length of current code
	if (lcd .lt. lnf) goto loop	;this is shorter than current code

	if (item .eq. p_code(1,lcd)) call check
	goto loop

check,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (%instr(1,item,'*') .and. .not. %instr(1,p_par,'*') ) return		;both or ...
	if (.not. %instr(1,item,'*') .and. %instr(1,p_par,'*') ) return		;neither

	savcod = p_code
	clear j
	clear xitem
	lni = %trim(item)
	if (lni .lt. 3) return

	lnf = %trim(p_par)
;;;	if (lnf .lt. lni) return	;parent is too short
	if (lnf .ne. lni) return	;wrong size

;start to build "code" item
	xitem(1,lcd) = item(1,lcd)

	for i from 1 thru 10
	   begin
		clear r_val(i), r_seg(i)

		td = %trim(xitem) + 1	;next char
		using item(td,td) select
		('*'),	xitem(td,td) = '*'
		('S'),	xitem(td,td) = 'S'
		('45'),	xitem(td,td+1) = '45'
		('90'),	xitem(td,td+1) = '90'
		(),	xitem(td,td+1) = item(td,td+1)
		endusing


		using p_seg(i) select
		('  '),	exitloop				;no more segs
		('45'),	if (xitem(td,td+1).eq.'45') nop		;we're good
		('90'),	if (xitem(td,td+1).eq.'90') nop		;we're good
		('*'),	if (xitem(td,td) .eq. '*') nop
		('S'),	if (xitem(td,td) .eq. 'S') nop
		('GA'),	using xitem(td,td+1) select
			('16','18','20','22','24','26'), xitem(td,td+1) = 'GA'	;gauge
			endusing
		('XX'),	
			begin
			onerror not_num
			num = xitem(td,td+1)
			offerror
			xitem(td,td+1) = 'XX'
			incr j
			r_seg(j) = 'XX'
			r_val(j) = num
			end
		('YY'),	
			begin
			onerror not_num
			num = xitem(td,td+1)
			offerror
			xitem(td,td+1) = 'YY'
			incr j
			r_seg(j) = 'YY'
			r_val(j) = num
			end
		('ZZ'),	
			begin
			onerror not_num
			num = xitem(td,td+1)
			offerror
			xitem(td,td+1) = 'ZZ'
			incr j
			r_seg(j) = 'ZZ'
			r_val(j) = num
			end
		endusing
	   end	

;special case for EDXX45, EDXX90, EDXXGA
;now ED1824 is comming back EDXX24 instead of EDXXGA...

	IF (XITEM .EQ. 'EDXX')
		BEGIN
		td = td -2
		using xitem(td,td+1) select
		('45'), nop
		('90'),	nop
		('GA'), nop
		(),	if (%trim(xitem).gt.4) xitem(td,td+1) =		
		endusing
		END

	if (%trim(xitem) .gt. %trim(fitem) ) fitem = xitem
not_num,
	offerror	
	return
;-------------------------------------------

eof,
	r_par = fitem
	freturn retval
