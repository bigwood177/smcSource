subroutine emcmn
	cust	,d
	nbr	,d
	chan	,d	;emailc.smm
	func	,d	;1=get, 2=maint
	m_wid	,d	;main w_id

	.include 'wnd:windows.def'

;5-7-14: pop40 to pop80

external function
	mail_cnt	,d	;number of existing emails for this cust

record	emailc
	.include 'def:rd084a.def'
;
;
record	emailv
	V_CUST	,D6
	V_NBR	,D3
	V_DFLT	,D1	;0=default, else 9
	V_FNAM	,A10
	V_LNAM	,A10
	V_CELL	,D10
	V_MAIL	,A50
		,A10	;FILLER
RECORD,X
	V_KEY	,D9

record	pop
	.include 'def:pop80.def'

	
record	e_key2
	k2_cust	,d6
	k2_dflt	,d1
	k2_fnam	,a10

	
.define GET	,1
.define MAINT	,2

record	vars
	dont_lock	,d1	;10-31-18
	chnlok	,d2,	97
	select	,d1
	achar	,a1
	stat	,d1
	rfa	,a6
	wnd_1	,d4
	w_id	,d4
	wn_name	,a5,	'EMCMN'
	i	,d6
	key	,a6
	lokctl	,d1
	read	,d1,0
	write	,d1,1
	store	,d1,2
	delete	,d1,3
	entry	,a50
	inxctl	,d1
	cngctl	,d1
	whatno	,d1
	switch	,d1	
;
proc
	call init_window
	xcall ttsts(stat)
	if (stat) xcall w_disp(wnd_1,wd_accept,achar)

	if (nbr .gt. 0)
	then	call change
	else	call add

	xcall w_proc (wp_remove, wnd_1)
	xcall w_updt
	xreturn

;------------------------------------------
add,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	call get_pop

	if (numara .le. 0)		;9-30-13 if no email exists
		begin
		select = 1	;add
		call add_new
		nbr = v_nbr
		return
		end

	call dsp_wnd

;	dline = '  Nbr First-Name Last-Name  Email'
;	dline = '  ZZX AAAAAAAAAA AAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAA'
	dline = '<Insert>=Add New, <Home>=Change Selected'
	xcall pop80(pop)
	
	using p_action select
;;;	(1),	begin
	(1,4,5),	begin
		select = 2	;selected
		dline = parry(pi)
		nbr = dline(1,3)
		if(func.eq.MAINT.or.p_action.eq.5) call change
		end

	(2),	begin
		select = 1	;add
		call add_new
		nbr = v_nbr
		end
	(3),	begin		;delete
		dline = parry(pi)
		nbr = dline(1,3)
		dont_lock = 1
		call get_mail
		lokctl = 1
	;;;	delete (chan, e_key)
		xcall cp_isio (chan, emailc, e_key, delete, lokctl)
		clear nbr, v_nbr
		goto add
		end
	endusing

	return
;------------------------------------------
add_new,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear emailc
	v_cust = cust
	v_nbr = mail_cnt(cust, chan)	;get next nbr
	v_nbr = v_nbr + 1		;next number
	call dsp_wnd
	goto displa


change,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	call get_mail
	call dsp_wnd
	call dsp_mail
	goto anycn
displa,
	clear cngctl
dflt,
	xcall winpt (wnd_1, 04, 20, 01, 00, 'YN', entry, inxctl)
	using inxctl select
;;;	(1),	xcall mail_dft(v_cust, v_nbr, chan)
	(1),	begin
		xcall mail_dft(v_cust, v_nbr, chan)
		call get_mail		;need new record - key just changed
		end
	(2),	v_dflt=9
	endusing

	goto (anycn), cngctl
fnam,
	xcall winpt (wnd_1, 05, 20, 10, 0, 'AE', entry, inxctl)
	goto (displa,endoff),inxctl
	v_fnam = entry(1,10)
	goto (anycn),cngctl

lnam,
	xcall winpt (wnd_1, 06, 20, 10, 0, 'A ',entry, inxctl)
	goto (displa),inxctl
	v_lnam = entry(1,10)
	goto (anycn), cngctl

cell,
	xcall winpt (wnd_1, 07, 20, 10, 0, '# ',entry, inxctl)
	goto (displa),inxctl
	v_cell = entry(1,10)
	entry(1,12) = v_cell, 'ZZZ ZZZ ZZZZ'
	xcall w_disp (wnd_1, wd_pos, 7,20, entry(1,12) )
	goto (anycn),cngctl
mail,
	xcall winpt (wnd_1, 08, 20, 50, 0, 'A ', entry, inxctl)
	goto (displa),inxctl
	v_mail = entry(1,50)
	goto (anycn), cngctl	
anycn,
	xcall wancn (wnd_1, 10, cngctl, whatno)
	goto (proces, cngbr),cngctl+1
cngbr,
	goto (cngbr, cngbr, dflt, fnam, lnam, cell, mail),whatno
	goto anycn
proces,
	emailc = emailv

	using select select
	(1),	begin	;store
		lokctl = 1
		xcall isio (chan, emailc, e_key, store, lokctl)
		end
	(2),	begin	;change
		lokctl = 1
		xcall isio (chan, emailv, e_key, read, lokctl) ;need to re-lock (note: emailV)
		xcall isio (chan, emailc, e_key, write, lokctl)
	;;;	write (chan, emailc, e_key)
		end
	(3),	begin	;delete
		end
	endusing
	return
endoff,
	clear emailv
	return
;------------------------------------------

dsp_wnd,	;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall w_disp (wnd_1, wd_clear)
	xcall w_disp (wnd_1, wd_pos, 2,4, '1. CUSTOMER #')
	xcall w_disp (wnd_1, wd_pos, 3,4, '2. NBR')
	xcall w_disp (wnd_1, wd_pos, 4,4, '3. DEFAULT <N>')
;                                          45678901234567890
	xcall w_disp (wnd_1, wd_pos, 5,4, '4. FIRST NAME                <end>')
	xcall w_disp (wnd_1, wd_pos, 6,4, '5. LAST NAME')
	xcall w_disp (wnd_1, wd_pos, 7,4, '6. CELL')
	xcall w_disp (wnd_1, wd_pos, 8,4, '7. EMAIL')

	xcall w_updt
	return
;------------------------------------------
dsp_mail,	;;;;;;;;;;;;;;;;;;;;;;;;;;;

	entry(1,6) = e_cust, 'ZZZZZX' [left]
	xcall w_disp (wnd_1, wd_pos, 2,20, entry(1,6))

	entry(1,3) = e_nbr, 'ZZX' [left]
	xcall w_disp (wnd_1, wd_pos, 3,20, entry(1,3))
	
	using e_dflt select
	(0),	xcall w_disp (wnd_1, wd_pos, 4,20, 'Y')
	(9),	xcall w_disp (wnd_1, wd_pos, 4,20, 'N')
	endusing

	xcall w_disp (wnd_1, wd_pos, 5,20, e_fnam)
	xcall w_disp (wnd_1, wd_pos, 6,20, e_lnam)
	entry(1,12) = v_cell, 'ZZZ ZZZ ZZZZ'
	xcall w_disp (wnd_1, wd_pos, 7,20, entry(1,12))
	xcall w_disp (wnd_1, wd_pos, 8,20, e_mail)

	xcall w_updt
	return
;------------------------------------------

get_mail,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	e_cust = cust
	e_nbr = nbr
	lokctl = 1
	xcall CP_isio (chan, emailc, e_key, read, lokctl)
;;;	xcall CP_isio (chan, emailc, e_key, read, lokctl, ,chnlok)	;2-12-18 DON'T KNOW???
	emailv = emailc

	if (.not. dont_lock) unlock chan		;ssq 10-11-16 - 
	dont_lock = 0
	return
;------------------------------------------
get_pop,	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear pi, numara

	clear e_key2
	k2_cust = cust

	find (chan, emailc, e_key2, keynum:1) [err=gp_loop]
gp_loop,
	reads (chan , emailc, getrfa:rfa) [lock=gp_locked,eof=gp_eof]
;;;	reads (chan , emailc, getrfa:rfa) [err=gp_eof]
	if (e_cust .gt. cust) goto gp_eof
	dline(1,3) = e_nbr, 'ZZX'
	dline(5,14) = e_fnam
	dline(16,25) = e_lnam
	dline(27,50) = e_mail

	incr pi
	parry(pi) = dline
	parrfa(pi) = rfa
	if (pi.lt.maxara) goto gp_loop

gp_eof,
	numara = pi
	unlock chan	;ssq 10-11-16

	return

gp_locked,
	if (%passed(m_wid)) 
		begin
		xcall wnmsg (m_wid, 24, 'record locked',1)
		goto gp_eof
		end
	sleep 1
	goto gp_loop

;------------------------------------------

init_window,	;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall w_proc(wp_find, wnd_1, wn_name)
	if (.not. wnd_1) xcall w_proc (wp_create, wnd_1, wn_name, 11,70)
	xcall w_brdr (wnd_1, wb_title, 'Customer Email', wb_tpos, wbt_top, wbt_center)
	xcall w_proc(wp_place, wnd_1, 1,1)
	xcall w_disp(wnd_1, wd_clear)


init_pop,	;;;;;;;;;;;;;;;;;;;;;;;;;;;

	MAXARA = 80
	PLEN = 50
	NUMROW = 8
	WX = 6
	WY = 4
	POP_WID = 'EMCPP'
	POP_TITLE = "Customer Email"
	
	return
;------------------------------------------

;ZZX AAAAAAAAAA AAAAAAAAAA AAAAAAAAAAAAAAAAAAAAAAAA
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
