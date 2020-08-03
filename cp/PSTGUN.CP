;PSTGUN.CP
;ph1.dbl
;		process raw physical inventory data
;
;	data format: item#, qty, box, date-time, terminal#
;		date-time: 3/23/2007 3:08:03 AM

; 1-04-08 ssq: onerror for f3 notes...
; 9-23-08 ssq: p_count is 3 dec places XXXXXX.XXX
; 12-13-17 ssq: added raw material inventory: tags start w/ "T"

record	itmmas
	.include 'def:rd041a.def'

record	rinvms
	.include 'def:rd131a.def'


record	itmkey
	.include 'def:rd041k.def'
record	vankey
	.include 'def:rd041kv.def'

;
record	coptbl
	.include 'def:rd182a.def'

record	phyinv
	.include 'def:rd196a.def'

record	physrm
	.include 'def:rd062a.def'
;
record	work
		,a512

record	data
	item	,a15
	f1	,d3
	f2	,d3
	f3	,d5
	cnt	,d9
	box	,d6
	errdes	,a30

record	channel
	chn041	,d2
	chn132	,d2	;rinvms
	chn182	,d2
	chn196	,d2
	chn062	,d2	;physrm
	chnwrk	,d2
	chnerr	,d2
	chnroc	,d2
	chnvik	,d2
	chnsmc	,d2
;
record	dis
	ii	,d6

record	vars
	rm	,a1	;R = raw material item
	cmp	,a1	;company code S,R,V
	tag	,d6
	bf	,d1
	x_price	,d9
	d_price	,d9

	entry	,a30
	inxctl	,d1
	cngctl	,d1
	whatno	,d1
	a1	,a1
	a3	,a3
	a7	,a7
	case	,d4		;per case
	tab	,a1,	','	;tab or comma
	field	,a30
	rawdata	,a60
	errfil	,a60
	opnok	,d1
	ln	,d6
	pn	,d6
	dec	,d18
	raw_err	,d1
	itm_err	,d1
	new_phy	,d1
	qty	,d9
	switch	,d1
	v	,d1
;
proc
	xcall terid(v)
	xcall outpt (1,1,2,'POST SCANNED DATA',1)

displa,
	xcall outpt (4,4,0,'1. UPLOAD FILE:',1)
	xcall input (4,20,30,00,'AE',entry,inxctl,1)
	goto (displa,endoff),inxctl
	rawdata = 'gun:' + entry
	xcall anycn (cngctl,whatno)
	goto (proces, displa), cngctl

proces,

	call opens
	if (.not. opnok) goto endoff

	clear ii
loop,
	incr ii
	if (ii/250*250 .eq. ii) display(15,$scr_pos(1,70),dis)
	reads (chnwrk, work, eof)

	call parse
	goto (loop),raw_err		;could not parse data

	if (rm .eq. 'R')
	then	call raw_material
	else	call regular_inventory

	goto loop



raw_material,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; raw material tags
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	call get_rm
	if (itm_err) return

	call get_rm_phys
	if (.not. new_phy)
	then	begin
		itm_err = 1			;no itmmas
		errdes = 'duplicate tag'
		writes (chnerr, data)		;bad data

	;;;	ph_qty = ph_qty + qty
	;;;	write (chn062, physrm, ph_tag)
		end
	else	begin
		clear physrm
		ph_tag = tag		;set in get_rm
		ph_qty = qty
		PH_CMP = cmp
		store (chn062, physrm, ph_tag)
		end
	return
;------------------------------------------------------------


regular_inventory,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; regular inventory items
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	call get_item
	if (itm_err) return

; removed 10-05-11 per jess
;;;	qty = qty / case		;per case
	call get_phys			;existing phyinv record?
	if (.not. new_phy)
	then	begin
		p_count = p_count + qty
		write (chn196, phyinv, phy_key)
		end

	else	begin
		p_mat = 
		p_item = item
		p_f1 = f1,	'XXX'
		p_f2 = f2,	'XXX'
		p_f3 = f3,	'XXXXX'
		p_cat = prdcat
		p_desc = descr
		p_onh = qtyonh
		p_count = qty 
		p_price = d_price
		store (chn196, phyinv, phy_key)
		end
	return
;-------------------------------------------------------

eof,
	ln = %trim(rawdata)
	rawdata(ln+1,40) = ' Processed'
	xcall mesag(rawdata,1)



endoff,
	call close
	xcall pgchn ('cp:phymnu',1)
	stop

get_rm_phys,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	new_phy = 0
	read (chn062, physrm, tag) [err=no_phy]
	return
;----------------------------------------------

get_phys,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	new_phy = 0					;assum not found
	read (chn196, phyinv, itmkey) [err=no_phy]	;use itmmas key

	return
no_phy,
	new_phy = 1					;add new record
	clear phyinv
	return

;----------------------------------------------
get_rm,		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear itm_err
	clear itmmas
	item(7,15) =
	xcall alpdc (item, tag, bf)
	goto (r_bad_key), bf

	CMP = 'S'		;ASSUME SMC

	read (chnsmc, rinvms, tag) [err=r_bad_smc]
	goto get_rm_ok
r_bad_smc,
	read (chnroc, rinvms, tag) [err=r_bad_roc]
	CMP = 'R'
	goto get_rm_ok
r_bad_roc,
	read (chnvik, rinvms, tag) [err=r_bad_key]
	CMP = 'V'
get_rm_ok,

	d_price = rmcost

	a7 = rmwid, 'ZZZ.XXXX' [LEFT]
	descr = rmmat + '-' + rmga + '-' + a7
	qtyonh = rmwgt


	return

r_bad_key,
	itm_err = 1			;no itmmas
	errdes = 'no rinvms record'
	writes (chnerr, data)		;bad data
	return
;----------------------------------------------

get_item,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear itm_err

	if (f1.ne.0 .or. f2.ne.0) goto no_pp	;only galv is pressed

;look up in pressed parts table!
	clear tbl_key
	tblcod = 'PP'
	pp_item = item

	read (chn182,coptbl,tbl_key)[err=no_pp]
	item = pp_part				;pressed part
no_pp,
;look up in pressed parts table!
	clear itmkey
	k_f1 = '000'
	k_f2 = '000'
	k_f3 = '00000'

	k_item = item

	dec = f1 + f2 + f3
	if (dec .eq. 0) call get_default

	k_f1 = f1,	'XXX'
	k_f2 = f2,	'XXX'
	k_f3 = f3,	'XXXXX'

	if (item .eq. 'TS51')
		begin
		clear field		
		end

	read (chn041, itmmas, itmkey, keynum:1) [err=p_bad_key]	;krf = 2
;-
	D_PRICE = PRICE
	using icfg select
	(1),	begin		;configured item
		xcall cfg_p (itemno, if1, if2, if3, x_price, bf)
		if (bf .eq. 0) d_price = x_price
		end
	(),	begin		;regular item
		if (if1.ne.'000' .or. if2.ne.'000' .or. if3.ne.'00000')
			begin
			xcall cfg_p (itemno, if1, if2, if3, x_price, bf)
			if (bf .eq. 0) d_price = x_price
			end
		end
	endusing
;-

;;;	read (chn041, itmmas, itmkey, keynum:1) [err=p_bad_key]	;krf = 2

	using prdcat select
	('J'),	begin
		case = 1	;8-12-08 per jes
	;;;	case = ftcase
		d_price = avgcst
		end
	(),	case = 1
	endusing
	if (case .lt. 1) case = 1

; check price/qty table...

	clear coptbl
	tblcod = 'I1'
	i1_key = itemno
	read (chn182, coptbl, tbl_key) [err=no_p_tab]
	d_price = i1_price(1)	;use first price

no_p_tab,

	if (imat2 .eq. a3) return	;no mat'l code

; change "price" to be price per unit counted.  itmmas.weight is lbs/unit,
; so qty counted * weight is the weight of the qty counted.  the price in
; the "I2" table is price/lb, so qty*weight*i2_price is the total amount
; for the qty counted.  Therefore, (weight*i2_price) is the price for the
; qty counted. 

; look up price/lb for this material in table...
	clear tbl_key
	tblcod = 'I2'
	i2_mat = imat2
	i2_ga = iga
	read (chn182, coptbl, tbl_key) [err=no_matl]

	d_price = (weight * i2_price)#2	; itmmas.weight is lbs/unit counted,
				; so (weight*i2_price) is price/unit counted
no_matl,
	return
;------------------------------------------------

p_bad_key,
	itm_err = 1			;no itmmas
	errdes = 'no itmmas record'
	writes (chnerr, data)		;bad data
	return

get_default,
	clear tbl_key
	tblcod = 'IK'
	ik_item = item

	read (chn182,coptbl,tbl_key)[err=no_default]
	f1 = ik_f1
	f2 = ik_f2
	f3 = ik_f3
no_default,
	return

;----------------------------------------------

parse,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear raw_err
	ln = 1
	call get_field
	if (raw_err) return
	
	if (field(1,1) .eq. 'T')	;12-13-17 raw material tag
	then	rm = 'R'
	else	rm =

	item = field(2,30)

	onerror bad_data		;1-04-08
	f1 = field(17,19)
	f2 = field(20,22)
	f3 = field(23,27)
	offerror			;1-04-08

;;;	if (f3 .eq. 3) f3 = 1		;replace small end w/ spiral end
	call get_field
	if (raw_err) return

	xcall alpdc (item, tag, bf)
;;;	cnt = field
	if (rm .eq. 'R')
	then	xcall alpdc (field, cnt, bf)
	else	xcall dec3 (field, cnt)		;normalize cnt to .xxx

	if (item .eq. '2PB')
		begin
		a1 = 
		end

	call get_field
	if (raw_err) return
	box = field

	if (box .le. 0) box = 1
	qty = cnt * box

;skip date, time and terminal...	
	return
;----------------------------------------------
get_field,
	onerror bad_data	
	pn = %instr (ln,work,tab)
	field = work (ln,pn-1)
	ln = pn+1
	offerror
	return
bad_data,
	offerror
	raw_err = 1
	writes (chnerr, work)
	return
;----------------------------------------------

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

;;;	open (1,si,'smc:itmmas.smm')
	switch = 5
	xcall files (1, 'SI', 41, switch)
	if (switch .eq. 9) return
	chn041 = 1
	
	switch = 5
	xcall files (17, 'SI', 182, switch)
	if (switch .eq. 9) return
	chn182 = 17

;;;	switch = 5
;;;	xcall files (22, 'SI', 132, switch)	;132 - rinvms.ism
;;;	if (switch .eq. 9) return
	chn132 = 22


	open (22,si,'smc:rinvms.smm')
	chnsmc = 22
	open (23,si,'roc:rinvms.rom')
	CHNROC = 23
	open (24,si,'vik:rinvms.vim')
	CHNVIK = 24

	switch = 5
	xcall files (3,'SU',196,switch)		;196 - phyinv.ism
	if (switch .eq. 9) return
	chn196 = 3

	switch = 5
	xcall files (33,'SU', 062, switch)	;062 - physrm.ism
	if (switch .eq. 9) return
	chn062 = 33

;;;	rawdata = 'gun:t3.dat'		;for ...253

	onerror no_raw
	ln = %trim(rawdata)
	open (4,i,rawdata(1,ln) )
	chnwrk = 4
	goto raw_ok
no_raw,
	offerror
	xcall mesag ('Could not open raw data file...',1)
	return
raw_ok,
	offerror

;;;	open (10,o,'phyerr.dat')

	errfil = rawdata
	ln = %trim(errfil)
	if (ln.gt.5)
	then	errfil(ln-2, ln) = 'err'
	else	errfil = 't.err'

	open (10, O, errfil)

	chnerr = 10

	opnok = 1
	return
;-----------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (chn041) close chn041
	if (chn132) close chn132
	if (chn182) close chn182
	if (chn196) close chn196
	if (chnwrk) close chnwrk
	if (chnerr) close chnerr
	if (chn062) close chn062
	if (chnsmc) close chnsmc
	if (chnroc) close chnroc
	if (chnvik) close chnvik

	return
;------------------------------------------------

