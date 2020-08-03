;cont.cp
;	contant info for ordmnt.
;
; 11-27-19 ssq: cash cust mod.

subroutine cont
	cust	,d	;cust
	con_nbr	,d	;contact number (-1 = insert new)
	cash_cust,d	;1 = is cash cust (cust 10)

	.include 'wnd:windows.def'

record	coptbl
	.include 'def:rd182a.def'
;

record	funkey
	.include 'def:funkey.def'

record	vars
	ct_key	,a2		;if cash_cust use '92', else use 'E2'
	savrfa	,a6
	a30		,a30
	dfl_nbr		,d3	;e2_nbr of default email
	dflt		,d1	;1=default
	delete_ok	,d1
	save_key	,d3
	pd		,d1	;page down?
	insert		,d1	;1-insert new mail
	del_mail	,d1	;delete or change default
	strt		,d2
	f_key		,d3
	w_id		,d4
	w_name		,a4
	i		,d6
	j		,d6
	nbr_mail	,d4
	max_mail	,d2,	10
	row		,d2
	fnam		,a10
	lnam		,a15
	cell		,d10
	mail		,a50
	anbr		,10d4
	afnam		,10a10
	alnam		,10a15
	acell		,10d10	;xxx-xxx-xxxx
	amail		,10a50
	entry		,a50
	inxctl		,d1
	whatno		,d2
	cngctl		,d1
	lokctl		,d1
	read		,d1,0
	write		,d1,1
	store		,d1,2
	delete		,d1,3
	xnum		,d2	;index to email selected
	win		,a1	;i=input, d=display
	sz1		,d2	;row
	sz2		,d2	;col
	chn182		,d2
	switch		,d1
	opnok		,d1
;

proc

	clear w_id, xnum
	call opens
	if (.not. opnok) xreturn

	if (cash_cust)
	then	ct_key = '92'
	else	ct_key = 'E2'

restart,
	clear del_mail
	call get_cust_mail
;;;	if (nbr_mail .le. 0) 
	if (nbr_mail.le.0 .or. con_nbr.eq.-1) 
		begin
		call insert_mail
		goto endoff
		end

	call display_mail
	if (insert) call insert_mail
	if (del_mail) goto restart
endoff,
	if (xnum.gt.0)
	then	con_nbr = anbr(xnum)		;contact # selected
	else	con_nbr = xnum	;might be neg 3-28-13

;;;	if (xnum.le.0)
;;;	then con_nbr = 0
;;;	else 	con_nbr = anbr(xnum)		;contact # selected

;;;	con_nbr = xnum		;contact # selected
	close chn182
	if (w_id) 
		begin
		xcall w_proc (wp_remove, w_id)
		xcall w_updt
		end
	xreturn

;-----------------------------------------------
insert_mail,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	win = 'i'
	call new_window
	clear cngctl

displa,
	xcall w_disp(W_ID, wd_clear)
	xcall w_disp(w_id, wd_pos, 1,5, '1. FIRST NAME')
	xcall w_disp(w_id, wd_pos, 2,5, '2. LAST NAME')
	xcall w_disp(w_id, wd_pos, 3,5, '3. EMAIL')
	xcall w_disp(w_id, wd_pos, 4,5, '4. CELL/DIRECT')
	xcall w_disp(w_id, wd_pos, 5,5, '5. DEFAULT?')
	if (xnum .gt. 0) 		;changing existing record
		begin
		call dsprec
		goto anycn
		end

fnam,
	xcall winpt (w_id, 01, 20, 10, 00, 'AE', entry, inxctl)
	goto (displa, endoff),inxctl
	fnam = entry(1,15)
	if (fnam .eq. a30) goto fnam
	goto (anycn),cngctl

lnam,
	xcall winpt (w_id, 02, 20, 15, 00, 'A ',entry,inxctl)
	goto (displa),inxctl
	lnam = entry(1,15)
;;;	if (lnam .eq. a30) goto lnam	;3-28-13 can leave blank
	goto (anycn),cngctl

mail,
	xcall winpt (w_id, 03, 20, 50, 00, 'A ',entry, inxctl)
	goto (displa),inxctl
	mail = entry(1,50)
	if (mail .eq. a30) goto mail
	goto (anycn),cngctl
cell,
	xcall winpt (w_id, 04, 20, 10, 00, '# ',entry, inxctl)
	goto (displa),inxctl
	cell = entry(1,10)
	entry(1,12) = cell, 'XXX-XXX-XXXX'
	xcall w_disp (w_id, wd_pos, 4,20, entry(1,12) )
	goto (anycn),cngctl

dflt,
;;;	if (xnum.gt.0) goto anycn
	xcall winpt (w_id, 05, 20, 01, 00, 'YN', entry, inxctl)
	if (inxctl .eq. 1)
	then	dflt = 1
	else	dflt = 0
	if (xnum.gt.0 .and. dflt.eq.1)
		begin
		save_key = anbr(xnum)
		e2_nbr = save_key

		call remove_default
		; re-read the original record...
		clear tbl_key
		tblcod = CT_KEY
		e2_cust = cust
		e2_nbr = xnum
		xcall isio (chn182, coptbl, tbl_key, read, lokctl)

		end

	goto (anycn),cngctl

anycn,
;;;	xcall wancn (w_id, 7, cngctl, whatno)
	xcall w_disp (w_id, wd_pos, 7,1, 'FIELD # TO CHANGE       <DEL>=DELETE')
	xcall winpt (w_id, 07, 20, 01,00, '# ',entry,inxctl, f_key)

	using f_key select
	(del_key),	begin
			clear whatno
			call delete_mail
		
			if (delete_ok) 
			then	begin
				del_mail = 1
				return
				end
			else	goto anycn
			end

	(),	whatno = entry(1,1)
	endusing

	if (whatno .eq. 0)
	then	clear cngctl
	else	cngctl = 1
	entry =
	goto (proces, cngbr),cngctl+1
cngbr,
	goto (fnam, lnam, mail, cell, dflt), whatno
	goto anycn
proces,
	clear coptbl
	tblcod = CT_KEY
	e2_cust = cust
	e2_fnam = fnam
	e2_lnam = lnam
	e2_cell = cell
	e2_mail = mail
	e2_dflt = dflt
	if (xnum .gt. 0)		;changing existing email...
	then	begin
		e2_nbr = xnum
		xcall isio (chn182, coptbl, tbl_key, write, lokctl)

		end
	else	begin
		if (dflt) call remove_default

		clear coptbl
		tblcod = CT_KEY
		e2_cust = cust
		e2_fnam = fnam
		e2_lnam = lnam
		e2_cell = cell
		e2_mail = mail
		e2_dflt = dflt

		xnum = 1		;use first available slot
		for i from 1 thru 999
			begin
			e2_nbr = xnum
			e2_dflt = dflt
			xcall isio (chn182, coptbl, tbl_key, store, lokctl)

			using lokctl select
			(0),	begin
				xnum = 1	;get new key into array
				exitloop
				end
			(4),	incr xnum		;bump xnum and try again
			(),	begin
				xcall olmsg (w_id, 6, "Can't add Contact!!",1)
				xnum = 0
				exitloop
				end
			endusing
	
			end

		xcall isio (chn182, coptbl, tbl_key, read, lokctl)
		if (lokctl .ne. 0)
		then	clear anbr(xnum)
		else	anbr(xnum) = e2_nbr
	;;;	else	anbr(xnum) = e2_key
		end
	
	con_nbr = xnum
	clear pd
	call get_cust_mail		;9-13-12: restore array
	xnum = con_nbr			;newly added is selected
	return
;-----------------------------------------------
delete_mail,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear delete_ok

	xcall w_disp (w_id, wd_pos,7,1, wd_clr, wdc_lin, 'Delete, are you sure?')
	xcall winpt (w_id, 7,26, 01,01, 'YN', entry, inxctl)

	using inxctl select
	(1),	begin
		xcall isio (chn182, coptbl, tbl_key, delete, lokctl)
		if (lokctl .eq. 0)
		then	delete_ok = 1
		else	delete_ok = 0
		end

	(2),	return
	endusing
	
	return
;-----------------------------------------------

dsprec,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; display current contact record
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall w_disp (w_id, wd_pos, 1,20, e2_fnam)
	xcall w_disp (w_id, wd_pos, 2,20, e2_lnam)
	xcall w_disp (w_id, wd_pos, 3,20, e2_mail)
	entry(1,12) = e2_cell, 'XXX-XXX-XXXX'
	xcall w_disp (w_id, wd_pos, 4,20, entry(1,12))
	using e2_dflt select
	(0),	xcall w_disp (w_id, wd_pos, 5,20, 'N' )
	(1),	xcall w_disp (w_id, wd_pos, 5,20, 'Y' )
	endusing

;set input fields from current record...
	fnam = e2_fnam
	lnam = e2_lnam
	cell = e2_cell
	mail = e2_mail
	dflt = e2_dflt

	return
;-----------------------------------------------

display_mail,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	win = 'd'
	call new_window
	strt = 1

re_displa,
	xcall w_disp(W_ID, wd_clear)

	for i from strt thru 15
		begin
		if (i .gt. nbr_mail) exitloop	;less than 15 records

		row = i
		if (i .eq. 1) xcall w_disp (w_id, wd_pos, row, 1, '*')

		entry(1,2) = i,	'ZX'
		xcall w_disp (w_id, wd_pos, row,2, entry(1,2) )

	;;;	entry(1,2) = anbr(i),	'ZX'
	;;;	xcall w_disp (w_id, wd_pos, row,4, entry(1,2) )

		xcall w_disp (w_id, wd_pos, row,7, afnam(i) )
		xcall w_disp (w_id, wd_pos, row,18, alnam(i) )
		xcall w_disp (w_id, wd_pos, row, 34, amail(i) )
		end

	xcall w_updt
	row = row + 2
dany,
	clear pd
	xcall w_disp (w_id, wd_pos, row, 1, wd_clr, wdc_lin)

	xcall w_disp (w_id, wd_pos, row,1, 
&	'select #/ <PD>=more/ C#=change/ D#=make default/ <INS>=New')

	xcall winpt (w_id, row, 66, 03, 00, 'A ', entry, inxctl, f_key)

	clear insert, del_mail
	using f_key select
	(ins_key),	begin
			insert = 1
			return
			end

	(page_dn),	begin
			pd = 1
		;;;	if (nbr_mail.ge.max_mail) ;else no reason to page_dn
			  begin
			  for j from 1 thru nbr_mail
				begin
				clear anbr(j)
				clear afnam(j)
				clear alnam(j)
				clear amail(j)
				end
		 	  clear nbr_mail
			  call find_default	;make sure default is 1st
			  find (chn182, coptbl, rfa=savrfa)
			  call loop		;keep reading
			  end
			goto display_mail
			end
	endusing


	upcase entry
	onerror not_num

	using entry(1,1) select
	('C'),	xnum = entry(2,3)	;change
	('D'),	xnum = entry(2,3)	;make default
	('*'),	xnum = -9		;jess for returns 3-28-13
	(),	xnum = entry(1,2)
	endusing
	offerror 

	if (entry.eq.'0') return	;2-25-13: don't send email this time
	if (xnum .eq. -9) return

	if (xnum .eq. 0) xnum = 1		;default
	if (xnum .lt. 1) goto not_num	
	if (xnum .gt. i-1) goto not_num
	if (entry(1,1) .eq. 'C')
		begin
		clear tbl_key
		tblcod = CT_KEY
		e2_cust = cust
		e2_nbr = anbr(xnum)
		xcall isio (chn182, coptbl, tbl_key, read, lokctl)
		if (lokctl .eq. 0)
		then	begin
			xnum = e2_nbr
			call insert_mail
			if (del_mail) goto restart
			goto display_mail
		;;;	goto insert_mail
			end

		else	xnum = 0
		end
	if (entry(1,1) .eq. 'D')
		begin
		save_key = anbr(xnum)
		e2_nbr = save_key

		call remove_default

		clear tbl_key
		tblcod = CT_KEY
		e2_cust = cust
		e2_nbr = save_key

		read (chn182, coptbl, tbl_key)
		e2_dflt = 1
		write (chn182, coptbl, tbl_key)

		return
		end

	return
not_num,
	xcall olmsg (w_id, row, "invalid entry", 1)
	goto dany
	
	return
;-----------------------------------------------


get_cust_mail,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear nbr_mail, xnum
pd,				;re-start at beginning of list

	if (.not. pd) call find_default

	clear tbl_key
	tblcod = CT_KEY
	e2_cust = cust
	find (chn182, coptbl, tbl_key) [err=loop]
loop,
	reads (chn182, coptbl, eof, getrfa:savrfa, lock:Q_NO_LOCK)
;;;	reads (chn182, coptbl, eof)
	if (tblcod .ne. CT_KEY) goto eof

	using pd select
	(1),	if (e2_cust .ne. cust) goto pd
	(),	if (e2_cust .ne. cust) goto eof
	endusing

	if (e2_dflt.eq.1) goto loop	;already have default
;;;	if (.not. pd .and. e2_dflt.eq.1) goto loop	;already have default

	incr nbr_mail
	anbr(nbr_mail) = e2_nbr
	afnam(nbr_mail) = e2_fnam
	alnam(nbr_mail) = e2_lnam
	acell(nbr_mail) = e2_cell
	amail(nbr_mail) = e2_mail
	if (nbr_mail .lt. max_mail) goto loop
eof,
	row = nbr_mail+2

	return
;-----------------------------------------------

find_default,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; first array element is always the default
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear tbl_key
	tblcod = CT_KEY
	e2_cust = cust
	find (chn182, coptbl, tbl_key) [err=fd_loop]
fd_loop,
	reads (chn182, coptbl, fd_eof, lock:Q_NO_LOCK)
;;;	reads (chn182, coptbl, fd_eof)
	if (tblcod .ne. CT_KEY) goto fd_eof

	if (e2_cust .ne. cust) goto fd_eof
	if (e2_dflt .ne. 1) goto fd_loop

	nbr_mail = 1
	anbr(nbr_mail) = e2_nbr
	afnam(nbr_mail) = e2_fnam
	alnam(nbr_mail) = e2_lnam
	acell(nbr_mail) = e2_cell
	amail(nbr_mail) = e2_mail
fd_eof,

	return
;-----------------------------------------------

remove_default,
	clear tbl_key
	tblcod = CT_KEY
	e2_cust = cust
	find (chn182, coptbl, tbl_key) [err=rd_loop]
rd_loop,
	xcall ios (chn182, coptbl, read, lokctl)
	if (lokctl .ne. 0) goto rd_eof
	if (tblcod .ne. CT_KEY) goto rd_eof
	if (e2_cust .ne. cust) goto rd_eof
	e2_dflt = 0

	xcall io (chn182, coptbl, tbl_key, write, lokctl)

	goto rd_loop

rd_eof,
	clear coptbl
	unlock chn182
	return
;-----------------------------------------------

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

	chn182 = 16
	do	begin
		if (.not. %chopen(chn182) ) exitloop
		incr chn182
		end
	until (chn182 .ge.98) 

	switch = 5
	xcall files (chn182, 'SU', 182, switch)

	opnok = 1
	return
;-----------------------------------------------

new_window,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (w_id)
		begin
		xcall w_proc(wp_remove,w_id)
		xcall w_proc(wp_delete,w_id)
		end
	
	using win select
	('i'),	begin
		sz1 = 7
		sz2 = 70
		end
	('d'),	begin
		sz1 = nbr_mail + 2
		if (nbr_mail.gt.15) sz1 = 17
		sz2 = 70
		end
	endusing

	W_NAME = 'mail'
	XCALL W_PROC(WP_FIND, w_id, W_NAME)
	IF (.NOT. w_id) XCALL W_PROC(WP_CREATE,w_id,W_NAME,sz1,sz2)
	XCALL W_BRDR(w_id,WB_TITLE,'Customer Contacts',
&		WB_TPOS,WBT_TOP,WBT_CENTER)

	XCALL W_PROC(WP_PLACE, w_id, 4, 4)
	XCALL W_DISP(w_id, WD_CLEAR)

	return
;-----------------------------------------------
END
