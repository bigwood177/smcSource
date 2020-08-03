;mvbdmn.cp
;MVBD table
	
record
		,a7,	'  12-17'
		,a7,	'  18-21'
		,a7,	'  22-28'
		,a7,	'  29-34'
		,a7,	'  35-38'
		,a7,	'  39-46'
		,a7,	'     48'
		,a7,	'  32-35'
		,a7,	'  36-38'
record	,X
	row_head,9a7	;array of row headers
	
record	col_head
		,a7,	'   6-18'
		,a5
		,a7,	'  19-24'
		,a5
		,a7,	'  25-30'
		,a5
		,a7,	'  31-36'


record	smpric
	.include 'def:smpric.def'
	
RECORD	FILPRC
	FL_DEV	,A3
		,A1,	':'
	FL_NAME	,A6
		,A1,	'.'
	FL_EXT	,A3

record	vars
	chnprc	,d2
	opnok	,d1
	cmpcod	,a3
	entry	,a30
	inxctl	,d1
	n_row	,d3	;number of rows
	n_col	,d3	;number of columns
	n_elm	,d6	;number of elements
	idx	,d3	;index to array
	c_row	,d3	;current row
	c_col	,d3	;current column
	c_val	,d6	;current value (array(idx))
	a_val	,a7	;current value ZZZX.XX
	s_row	,d3	;starting row
	s_col	,d3	;starting col
	col_d	,d3	;distance between col
	i	,d6
	j	,d6
	a2	,a2
	a3	,a3
	a5	,a5
	a7	,a7
	val	,d6
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	cngctl	,d1
	whatno	,d2
	switch	,d1
	v	,d1

proc
	xcall terid (v)
	xcall outpt (1,1,2,'MED VEL BACK DRAFT DAMPER PRICES',1)


	n_row = 7
	n_col = 4
	n_elm = n_row * n_col
	
	call opens
	if (.not. opnok) goto endoff
	
	col_d = 12		;33.        34.
	s_row = 6		;headers start on row 6
	s_col = 10		;columns start on col 9


	c_row = 0
	c_col = 0

	call dis_scr
displa1,
	call dis_data
displa,
	xcall anycn (cngctl, whatno)
	goto (proces, cngbr), cngctl + 1

cngbr,
	call get_data
	goto displa1

proces,
	xcall isio (chnprc, smpric, sm_key, write, lokctl)
	goto endoff


dis_scr,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; display grid of prompts
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		xcall outpt (1,1,2,'MED VEL BACK DRAFT DAMPER PRICING',1)
		xcall outpt (s_row, s_col+3, 0, col_head, 1)
		for i from 1 thru n_row
		  begin
		  c_row = s_row + i
		  xcall outpt (c_row, 1, 1, row_head(i), 1)
		  c_col = s_col

		  for j from 1 thru n_col
			begin
			call mak_idx
			a3 = idx, 'ZX.'
			xcall outpt (c_row, c_col, 0, a3, 1)
			c_col = c_col + col_d		
			end
		  end

	return
;----------------------------------------------------------
dis_data,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; display grid of data
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	for i from 1 thru n_row
		begin
		c_row = s_row + i
		c_col = s_col + 3
		for j from 1 thru n_col
			begin
			call dis_el			;display a single element
			c_col = c_col + col_d
			end
		end

	return
;----------------------------------------------------------

dis_el,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; display a single array element
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	call mak_idx
	a7 = sm_price(idx), 'ZZZX.XX'
	xcall outpt (c_row, c_col, 0, a7, 1)
	return
;----------------------------------------------------------


get_data,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; get data element
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	i = whatno/n_col
	if (whatno/n_col*n_col .ne. whatno) i = i + 1	;row
	j = whatno - (i-1)*n_col

	c_row = s_row + i
	c_col = s_col+3 + (j-1)*col_d
	xcall input (c_row,c_col,6,0,'# ',entry,inxctl,1)
	val = entry(1,6)
	sm_price(whatno) = val

	return
;----------------------------------------------------------

mak_idx,	;;;;;;;;;;;;;;;;;;;;;;;
	idx = ((i-1) * n_col) + j
	return
;--------------------------------------

endoff,
	call close
	xcall pgchn ('cp:smpmnu',1)

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	clear opnok

	xcall who(cmpcod)

	fl_dev = cmpcod
	fl_ext = cmpcod
	fl_ext(3,3) = 'M'

	fl_name = 'SMPRIC'

	open (1,su,filprc)
	chnprc = 1

;;;	call i_load			;remove after initial load
	sm_key = 'MVBDMN'
	xcall isio (chnprc, smpric, sm_key, read, lokctl)
	if (lokctl .eq. 0) goto opnok
	xcall mesag ('no pricing record',1)
	return

opnok,
	opnok = 1

	return
i_load,
	clear smpric
	sm_key = 'MVBDMN'
	for i from 1 thru n_elm
		begin
		display (15, $scr_pos(5,5), 'val: ')
		reads (15, a5)
		val = a5
		if (val .eq. 99) exitloop
		sm_price(i) = val
		
		end
	store (chnprc, smpric, sm_key)
	return
;----------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (chnprc) close chnprc
	return
;----------------------------------------------

end
