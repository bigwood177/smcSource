.subroutine	roldx
	cus_rec	,a		;cusmas record
	allow_update	,d	;0 = no update allowed, 1 = updates
	x	,d		;row
	y	,d		;col
	r_type	,d		;0=both, 1=sales, 2=shipping
	chan	,d		;channel (optional)
;

	.include 'wnd:tools.def'

record	coptbl				;ssq 4-20-06
	.include 'def:rd182a.def'

record	cusmas
	.include 'def:rd001a.def'

record	rolo		;(534 bytes)
	.include 'def:rolo.def'

record	PriceDuct		;ssq 4-20-06
	is_PD		,a10
	is_Pdate	,a10

record	vars
	chn182	,d2		;ssq 4-20-06
	xdate	,d8		;ssq 4-20-06
	redfil	,a14
	rl	,d4
	r_chan	,d2
	i_chan	,d2
	inp_id	,i4
	col_id	,i4
	select	,d1
	status	,d1	;0 = ok, 1 = abort
	srcflg	,d1,	1
	sts	,d4
	done	,d1

.proc
	cusmas = cus_rec

; look in coptbl for price duct info for this cust...

	xcall ffile(182,redfil,sts)	;ssq 4-20-06 ssq
	rl = %trim(redfil)
	redfil(rl,rl) = 'M'
	xcall u_open(chn182,"u:i",redfil(1,rl),,srcflg,sts)

	clear PriceDuct

	clear tbl_key
	tblcod = 'PD'
	pd_cust = cusno
	read (chn182, coptbl, tbl_key) [err=no_coptbl]

	is_PD = 'Price Duct'
	xdate(1,4) = pd_date(5,8)
	xdate(5,8) = pd_date(1,4)
	is_Pdate = xdate,	'ZX/XX/XXXX'


no_coptbl,	
	xcall ffile(183,redfil,sts)
	rl = %trim(redfil)
	redfil(rl,rl) = 'M'

	xcall i_ldinp(inp_id, g_utlib, "roldx_input",D_NOPLC)

	xcall i_display(inp_id,"cus_info",cusmas,"cusno,name,add1,add2,city,
&		state,zip,phone,efax")

	xcall i_display(inp_id,"pd_info",PriceDuct,"is_PD, is_Pdate")

	if (allow_update)
	then	xcall b_enable(inp_id,"Save")
	else	xcall b_disable(inp_id, "Save")

	if (%passed(chan))
	then	r_chan = chan
	else	begin
		xcall u_open(r_chan,"u:i",redfil(1,rl),,srcflg,sts)
	;;;	xcall u_open(r_chan,"u:i","SMC:ROLO.SMM",,srcflg,sts)
		if (sts)
			begin
			xcall u_message("Error opening Rolo")
			xreturn
			end
		end
r_read,
	select = 2			;change
	read (r_chan,rolo,cusno) [lock=r_locked, err=eof]

	using r_type select
	(0),	xcall i_display(inp_id,"rolo_info",rolo,"r_cont1,r_cont2,
&		r_cont3,r_cont4,r_sales,r_ship")
	(1),	xcall i_display(inp_id,"rolo_sales",rolo,"r_cont1,r_cont2,
&		r_cont3,r_cont4,r_sales")
	(2),	xcall i_display(inp_id,"rolo_shipping",rolo,"r_ship")
;;;	(2),	xcall i_display(inp_id,"rolo_shipping",rolo,"r_cont1,r_cont2,
;;;&		r_cont3,r_cont4,r_ship")
	endusing
	goto eof

r_locked,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; record locked, open in input mode...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	xcall u_open(i_chan,"i:i",redfil(1,rl),,srcflg,sts)	;ssq 7-20-04
;;;	xcall u_open(i_chan,"i:i","SMC:ROLO.SMM",,,sts)
	if (sts)
		begin
		xcall u_message("Error opening Rolo")
		xreturn
		end
	r_chan = i_chan
	xcall b_disable(inp_id, "Save")
	goto r_read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

eof,
	xcall U_WINDOW (D_PLACE, inp_id, x, y)

	if (r_cust .eq. cusno)
	then	select = 2
	else	select = 1	

	using r_type select
	(0),	xcall i_input(inp_id,"rolo_info", rolo,,,,,,,status)
	(1),	xcall i_input(inp_id,"rolo_sales", rolo,,,,,,,status)
	(2),	xcall i_input(inp_id,"rolo_shipping", rolo,,,,,,,status)
	endusing
	if (g_select)
		begin
		using G_entnam select
		("SAVE"),using select select
			(1),	begin
				r_cust = cusno
				store (r_chan,rolo,r_cust)
				xcall u_message ("Rolodex updated")
				end
			(2),	begin
				write (r_chan,rolo,r_cust)
				xcall u_message ("Rolodex updated")
				end
			endusing
		("CANCEL"),	 xcall u_beep
		endusing
		done = TRUE
		end


	;;;close r_chan
fini,	
	if (.not. %passed(chan) ) xcall u_close (r_chan)
	XCALL U_WINDOW(D_REMOVE, inp_id)
	XCALL U_WINDOW(D_DELETE, inp_id)
	xreturn
	.end

.subroutine	cmd_save
	id	,i
	name	,a
	status	,d
.proc
	status = 0	;ok
	xcall m_signal("Save")
	return
.end

.subroutine	cmd_cancel
	id	,i
	name	,a
	status	,d
.proc
	status = 1	;abort
	xcall m_signal("Cancel")
	return
.end



