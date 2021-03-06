.subroutine uifax
	o_date		,d	;quote date (oordte)
	f_send		,d	;1 = send, 0 = abort
	cus_rec		,a	;cusmas
	rolo_rec	,a	;rolo
	sales_rec	,a	;salesman rec
	f_info		,a	;faxinfo
	order		,d	;oordno

	.include 'wnd:tools.def'

.define UPDATE		,1
.define SALES_INFO	,0

record	cusmas
	.include 'def:rd001a.def'

record	salesmn
	.include 'def:rd054a.def'

record	rolo
	.include 'def:rolo.def'

record	faxselect
	q_date	,d6
	q_type	,d1

record	faxinfo
	.include 'def:faxinfo.def'

record	channel
	chnrol	,d2
	chn001	,d2
	chn002	,d2


record	def_note
		,a*,	'Thank you for the opportunity to quote'
		,a*,	' you on the following material.  Please'
		,a*,	' review all quantities,items, and gauges.'
record	vars
	first_note	,a350
	rx	,d2
	ry	,d2
	opnok	,d1
	inp_1	,i4
	inp_2	,i4
	col_1	,i4
	col_2	,i4
	sts	,d4
	done	,d1
	today	,d6
	srcflg	,d1,1
.proc
	xcall e_enter
	
	call opens

;;;	xcall i_ldinp(inp_1, u_utlib, "fc_inp1")
	xcall i_ldinp(inp_2, g_utlib, "fc_inp2")
	xcall m_ldcol(col_1, g_utlib, "file")
	xcall m_ldcol(col_2, g_utlib, "edit")

	if (f_send .eq. 9)		;retrieved old cover page info
	then		xcall i_display(inp_2,, FAXINFO, 
&			"fc_date, fc_cusname, fc_cont, fc_fax, 
&			 fc_notes, fc_salesman,")
	else	begin
		first_note = def_note
		xcall i_force(first_note)
		xcall i_display(inp_2,, FAXINFO, 
&		"fc_date, fc_cusname, fc_cont, fc_fax, fc_salesman")
		end

	done = 0
	do begin
;;;	   xcall i_input(inp_2,, FAXINFO)
	   xcall i_input(inp_2,, FAXINFO,,,,,,,first_note)
	   if (g_select)
		begin
		using g_entnam select
		("UIFAX_SEND"),	begin
				f_send = 1
				f_info = faxinfo
				done = 1
				end
		("UIFAX_CANCEL"),	begin
				f_send = 0
				xcall u_message("fax cancelled")
				done = 1
				end
		("ROLODEX"),	begin
				rx = 2
				ry = 5
				xcall roldx(cusmas, UPDATE, rx, ry, SALES_INFO)
				end
		endusing
		end
	   end
	   until (done)

	xcall u_window(D_DELETE,inp_2)
	xcall e_exit
	xreturn


;===========================================================
;----------------- internal routines -----------------------

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cusmas = cus_rec
	rolo   = rolo_rec
	salesmn = sales_rec
	faxinfo = f_info

; ssq 5-12-99 set as opono in faxque...
;;;	fc_cont = r_cont1(1,20)
	if (f_send .ne. 9)
		begin
		fc_cusname = name
		fc_fax = efax
		fc_salesman = slsnm
		fc_date = o_date
		end
	opnok = 1
	return

open_error,
	clear opnok
	return
;-----------------------------------------

.end

.subroutine	uifax_send
	id	,i

.proc
	xcall m_signal("UIFAX_SEND")
	xreturn
.end
;---------------------------------

.subroutine	uifax_cancel
	id	,i

.proc
	xcall m_signal("UIFAX_CANCEL")
	xreturn
.end
;---------------------------------

.subroutine	uifax_rolo
	id	,i

.proc
	xcall m_signal("ROLODEX")
	xreturn
.end
;---------------------------------

.subroutine	default_notes
	.include 'wnd:inpinf.def'	;input group
	afaxinfo	,a
	anotes	,a

record	faxinfo
	fc_date		,d6
	fc_cusname	,a25
	fc_cont		,a20
	fc_fax		,d10
	fc_notes	,a350
	fc_salesman	,a20

record	vars
	blanks	,a350

.proc
	clear blanks
	faxinfo = afaxinfo	

	if (fc_notes .eq. blanks)
		begin
		fc_notes = anotes
		xcall i_display(inp_wndid,, FAXINFO, "fc_notes")
		end
	xreturn
.end

