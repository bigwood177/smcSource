subroutine xcom
	item,	a	;parent item
	f1,	d	;f1-note
	f2,	d	;f2-note
	f3,	d	;f3-note
	qty,	d	;qty of parent
	qtyr,	d	;qty recieved
	flag,	d	;1=allocate, 2=relieve inventory
	chn162,	d	;channel for parent.ism
	chn163,	d	;channel for childx.ism
	chn041,	d	;channel for itmmas.ism
;
;	allocate/relieve inventory for fab tickets
;

EXTERNAL FUNCTION
	FBOM	,A

record	child
	.include 'def:rd163a.def'
;
record	itmmas
	.include 'def:rd041a.def'
record	itmkey
	.include 'def:rd041k.def'


record	retval
	r_par	,a15
	r_seg	,10a2
	r_val	,10d2

record	debug1
	d1_item	,a15
		,a2
	d1_qty	,a2
		,a1
		,a3, 'f3='
	d1_f3	,d5


record	debug
		,a2
	dqty	,a2
		,a2
	xitem	,a15
;;;		,a3,	'f3='
;;;	d_f3	,d5

record	vars
	good_item	,d1	;1=is good item
;;;	xitem	,a15
	a2	,a2
	val	,d2
	num	,d2
	tl	,d6
	ln	,d6
	i	,d6
	j	,d6
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	

proc
;;;	open (10,a,'xcom.dat')		;debug file
;;;	d1_item = item
;;;	d1_qty = qty
;;;	d1_f3 = f3
;;;	writes (10, debug1)

	retval = %fbom(item, chn162)

	find (chn163, child, r_par) [err=loop]
loop,
	reads (chn163, child, eof)
	if (c_par .ne. r_par) goto eof
	if (c_f1.ne.0 .and. c_f1.ne.f1) goto loop
	if (c_f2.ne.0 .and. c_f2.ne.f2) goto loop
	if (c_f3.ne.0 .and. c_f3.ne.f3) goto loop

	call make_item
	if (good_item) call upd_inventory
	goto loop
eof,
	close 10
	xreturn
;======================================================


make_item,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; build item number from child data
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	good_item = 1
	xitem = c_code
	for i from 1 thru 4
		begin
		using c_seg(i) select
		(a2),	exitloop
		('4'),	call add_alpha
		('9'),	call add_alpha
		(),	begin
			call get_val
			if(val.ne.0)
			then	call add_seg
			else	call add_alpha
			end
		endusing

	;;;	if (c_seg(i) .eq. a2) exitloop
	;;;	call get_val
	;;;	if (val.ne.0)
	;;;	then call add_seg
	;;;	else call add_alpha

		end

	return
;-----------------------------------------------------
add_seg,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	call check_up
	tl = %trim(xitem)
	xitem(tl+1,tl+2) = val,'XX'

	return
;-----------------------------------------------------
check_up,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	using c_par select
	('ECXX','ECXXGA'),	nop
	('ECDXX','ECDXXGA'),	nop
	('ECEXX'),		nop
	('BOXX'),		nop
	(),	return
	endusing

	if (c_code .eq. 'G ') val = val + 2

	return
;-----------------------------------------------------

add_alpha,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	tl = %trim(xitem)
	xitem(tl+1,tl+2) = c_seg(i)

	return
;-----------------------------------------------------


get_val,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; get the value of the current seg
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	val = 0

	for j from 1 thru 10
		begin
		if (r_seg(j) .eq. a2) exitloop
		if (r_seg(j) .eq. c_seg(i))
			begin
			onerror not_num
			val = r_val(j)
			offerror
			exitloop
			end
		end

	if (c_beg.ne.0 .and. (val.lt.c_beg .or. val.gt.c_end) ) 
	then good_item = 0
	else good_item = 1

not_num,
	offerror
	return	
;------------------------------------------------------


upd_inventory,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; allocate or relieve inventory
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	dqty = (qty*c_qty), 'ZX'	;debug
;;;	writes (10, debug)		;debug

	clear itmkey
	k_item = xitem
	xcall isio (chn041, itmmas, itmkey, read, lokctl)

	using flag select
	(1),	qtycom = qtycom + (c_qty*qty)
	(2),	begin
		qtycom = qtycom - (c_qty*qty)
		if (qtycom.le.0) qtycom = 0

		qtyonh = qtyonh - (c_qty*qtyr)
		if (qtyonh.lt.0) qtyonh = 0

		qtymtd = qtymtd + (c_qty*qtyr)
		qtyytd = qtyytd + (c_qty*qtyr)
		end
	endusing

	xcall isio (chn041, itmmas, itmkey, write, lokctl)

	return
;------------------------------------------------------

