;FAKCOM.CP
;	Stand alone (for now) program to allocate real items and
;	deallocate fake items.

record	itmmas
	.include 'def:rd041a.def'

record	itmkey
	.include 'def:rd041k.def'

record	coptbl
	.include 'def:rd182a.def'
;
record	out
	o_fitem	,a15
	o_fi1	,a3
	o_fi2	,a3
	o_fi3	,a5
		,a1
	o_fcom	,d6
		,a1
	o_ritem	,a15
	o_ri1	,a3
	o_ri2	,a3
	o_ri3	,a5
		,a1
	o_rcom	,d6
		,a1
	o_new	,d6

record	channel
	chn182	,d2
	chnfak	,d2
	chnrel	,d2
	chnout	,d2,	10
;
record	vars
	real_found	,d1
	opnok	,d1
	com	,d6
	switch	,d1
	i	,d6
;
proc
	open (15,i,'tt:')
	display (15,$scr_clr(screen),'real/fake allocations')

	call opens
	if (.not. opnok) goto eof

	clear tbl_key
	tblcod = 'FP'
	find (chn182, coptbl, tbl_key) [err=loop]
loop,
	reads (chn182, coptbl, eof)
	if (tblcod .ne. 'FP') goto eof

; get the fake item...
	find (chnfak, itmmas, fp_item) [err=fake_loop]	;first fake item...
fake_loop,
	reads (chnfak, itmmas, eof_fake)
	if (itemno .ne. fp_item) goto eof_fake

	if (qtycom .le. 0) goto fake_loop

	com = qtycom

	clear qtycom

	write (chnfak, itmmas, itemno)

	call get_real

	goto fake_loop

eof_fake,
	goto loop


eof,
	call close
	stop



get_real,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; update the real item...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear real_found	;assume not found

; data from the fake item...

	o_fitem = itemno
	o_fi1 = if1
	o_fi2 = if2
	o_fi3 = if3
	o_fcom = com

	clear itmkey

	K_ITEM = fp_part	;real item #
	K_F1 = if1
	K_F2 = if2
	K_F3 = if3
	read (chnrel, itmmas, itmkey, keynum:1) [err=no_real]

	o_ritem = itemno
	o_ri1 = if1
	o_ri2 = if2
	o_ri3 = if3
	o_rcom = qtycom

	qtycom = qtycom + com
	write (chnrel, itmmas, itemno)

; data from the real item...

	o_new = qtycom
	
	writes (chnout, out)
	real_found = 1		;found & updated

no_real,
	return
;----------------------------------------------------------------	

opens,
	clear opnok

	switch = 5
	xcall files (1, 'SU', 041, switch)
	if (switch .eq. 9) return
	chnfak = 1

	switch = 5
	xcall files (2, 'SU', 041, switch)
	if (switch .eq. 9) return
	chnrel = 2

	switch = 5
	xcall files (17, 'SI', 182, switch)
	if (switch .eq. 9) return
	chn182 = 17

	open (chnout, o, 'tst:fakcom.dat')

	opnok = 1
	return
;------------------------------------------

close,
	if (chnfak) close chnfak
	if (chn182) close chn182
	if (chnrel) close chnrel
	close chnout

	return

