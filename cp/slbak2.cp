;SLBAK2.cp
;SLB 4" or SLF 4"
; Aluminum Kynar color clad

	.include 'wnd:windows.def'
	.INCLUDE 'WND:TOOLS.DEF'
	
record
		,a7,	'   6-14'
		,a7,	'  15-16'
		,a7,	'  17-20'
		,a7,	'  21-24'
		,a7,	'  25-30'
		,a7,	'  31-36'
		,a7,	'  37-40'
		,a7,	'  41-44'
		,a7,	'  45-50'
		,a7,	'  51-54'
		,a7,	'  55-60'
record	,X
	row_head,11a7	;array of row headers
	
record	col_head
		,a7,	'   6-18'
		,a3
		,a7,	'  19-24'
		,a3
		,a7,	'  25-30'
		,a3
		,a7,	'  31-36'
		,a3
		,a7,	'  37-42'
		,a3
		,a7,	'  43-47'
		,a3
		,a7,	'  48-54'
		,a3
		,a7,	'  55-59'
		,a3
		,a7,	'     60'


record	smpric
	.include 'def:smpric.def'

RECORD	WN_NAME
		,A5,	'dpsts'
	WN_TNMBR,D4

RECORD	WARS
	W_ID	,D4
	WND_1	,D4
	WND_2	,D4
	WLEN	,D4
	
RECORD	FILPRC
	FL_DEV	,A3
		,A1,	':'
	FL_NAME	,A6
		,A1,	'.'
	FL_EXT	,A3

record	vars
	TCHAR	,D3
	LL	,d3
	opnok	,d1	
	cmpcod	,a3
	chnprc	,d2
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
	a4	,a4
	a7	,a7
	val	,d6
	cngctl	,d1
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,1
	whatno	,d3
	switch	,d1
	v	,d1

.proc
	open (15,i,'tt:')
	call init_window

	n_row = 11
	n_col = 9
	n_elm = n_row*n_col

	call opens
	if (.not. opnok) goto endoff
	
	col_d = 10		;33.        34.
	s_row = 6		;headers start on row 6
	s_col = 10		;columns start on col 9


	c_row = 0
	c_col = 0
	
	call dis_scr
displa1,
	call dis_data
;;;	xcall wancn (w_id, ll, cngctl, whatno)
	call wancn
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
		XCALL W_DISP(WND_1,WD_CLEAR)
		xcall xyb (1,1,'2" AK COLOR CLAD SLB/SLF PRICING',w_id, 0)

		xcall xyb (s_row, s_col+3, col_head, w_id, 0)
		for i from 1 thru n_row
		  begin
		  c_row = s_row + i
		  xcall xyb (c_row, 1, row_head(i), w_id, 0)
		  c_col = s_col

		  for j from 1 thru n_col
			begin
			call mak_idx
			a4 = idx, 'ZZX.'
			xcall xyb (c_row, c_col, a4, w_id, 0)
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
		c_col = s_col + 5
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
	a3 = sm_price(idx), 'ZZX'
	xcall xyb (c_row, c_col, a3, w_id, 0)
	return
;----------------------------------------------------------


get_data,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; get data element
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	i = whatno/n_col
	if (whatno/n_col*n_col .ne. whatno) i = i + 1	;row
	j = whatno - (i-1)*n_col

;;;	j = whatno - (whatno/n_col*n_col)	;col = remainder
;;;	if (whatno/n_col*n_col .eq. whatno) j = j + 1	;row

	c_row = s_row + i
	c_col = s_col+5 + (j-1)*col_d
	xcall winpt (w_id, c_row, c_col, 3,0, '# ', entry, inxctl)
	val = entry(1,3)
	sm_price(whatno) = val

	return
;----------------------------------------------------------

mak_idx,	;;;;;;;;;;;;;;;;;;;;;;;
	idx = ((i-1) * n_col) + j
	return	
;--------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall u_start(,,,,24,110,,99)		;24 row by 110 col
	
		XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (WND_1.EQ.0)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)	;full size
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'SLB 4" / SLF 4"',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,2,2)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	xcall u_resize (24,110)
	W_ID = WND_1
	LL = 24

	RETURN
;-----------------------------------------------------------------
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
	sm_key = 'SLBAK2'
	xcall isio (chnprc, smpric, sm_key, read, lokctl)
	if (lokctl .eq. 0) goto opnok
	xcall mesag ('no pricing record',1)
	return

opnok,
	opnok = 1

	return
i_load,
	clear smpric
	sm_key = 'SLBAK2'
	for i from 1 thru n_elm
		begin
		xcall w_disp (wnd_1, wd_pos, 5, 5, 'val: ')
		xcall winpt (w_id, 5, 11, 3,0, '# ', entry, inxctl)
		val = entry(1,3)
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

wancn,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; internal wancn for 3-digit whatno
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WHTNUM,
	CNGCTL = 1			; SET FOR CHANGE MODE
	XCALL W_DISP (W_ID,WD_POS,LL,1,wd_clr,wdc_lin,'FIELD # TO CHANGE')
	XCALL WINPT (W_ID,LL,20,2,0,'# ',a3,INXCTL)    ; GET # OF FIELD
	GO TO (WHTNUM), INXCTL
	IF (a3.EQ.'  '.OR.INXCTL.EQ.2) GO TO NOCNG
	ON ERROR BADCNG
	whatno = a3
	OFF ERROR
	return
nocng,
		cngctl = 0
	return
	
BADCNG,
	OFF ERROR
	XCALL W_DISP (W_ID,WD_POS,LL,1,'INVALID SELECTION')
	XCALL W_DISP (W_ID,WD_POS,LL,67,'CR TO RECOVER')
	xcall beep		;ssq 2-28-01
	CALL ACPTCR
	CNGCTL =
	GO TO whtnum
ACPTCR,
	XCALL FLAGS (10001000)
	XCALL W_DISP(W_ID,WD_ACCEPT,TCHAR)	
	IF (TCHAR.NE.13) GO TO ACPTCR
	XCALL W_DISP(W_ID,WD_ACCEPT,TCHAR)
	XCALL FLAGS (10000000)
	RETURN


.END


.SUBROUTINE XYB
	X	,D
	Y	,D
	TEXT	,A
	w_id	,d
	bold	,d	;1=bold, 0=don't bold

.include 'wnd:windows.def'

.PROC
	if (bold) xcall w_disp (w_id, wd_attr, attr_set+attr_rvrs)
	xcall w_disp (w_id, wd_pos, x, y, text)
	if (bold) xcall w_disp (w_id, wd_attr, attr_clr+attr_rvrs)

	RETURN
