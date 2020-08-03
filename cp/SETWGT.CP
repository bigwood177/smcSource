;setwgt.cp
;set_wgt.dbl
;
record	itmmas
	.include'def:rd041a.def'
;
record	key2
	k2_cat	,a2
	k2_item	,a15


record	dis
	ii	,d6
;
record	vars
	a2	,a2
	opnok	,d1
	stcat	,a2
	encat	,a2
	chn041	,d2
	switch	,d1
	read	,d1,0
	write	,d1,1
	lokctl	,d1
	entry	,a30
	inxctl	,d1
	cngctl	,d1
	whatno	,d2
	v	,d1

proc
	xcall terid (v)
	xcall outpt (1,1,2,'COPY ACTUAL WEIGHT TO PRODUCTION WEIGHT',1)

	call opens
	if (.not. opnok) goto endoff

;
displa,
	xcall outpt (1,1,2,'COPY ACTUAL WEIGHT TO PRODUCTION WEIGHT',1)
	clear cngctl
	xcall outpt (4,4,0,'1. STARTING PRDCAT: ',1)
	XCALL OUTPT (6,4,0,'2. ENDING   PRDCAT: ',1)

stcat,
	xcall input(4,24,02,00,'AE',entry,inxctl,1)
	goto (displa,endoff),inxctl
	stcat = entry(1,2)
	if (stcat .eq. a2)	
		begin
		encat = 'ZZ'
		xcall outpt (4,24,1,'ALL',1)
		xcall outpt (6,24,1,' ',1)
		goto anycng
		end
	goto (anycng),cngctl
encat,
	xcall input (6,24,02,00,'AE',entry,inxctl,1)
	goto (displa,endoff),inxctl
	encat = entry(1,2)
	if (encat .eq. a2)
		begin
		encat = stcat
		xcall outpt (6,24,1,encat,1)
		end
anycng,
	xcall anycn (cngctl,whatno)
	goto (proces, cngbr), cngctl + 1
cngbr,
	goto (stcat, encat),whatno
	goto anycng
proces,

	clear ii
	clear key2
	k2_cat = stcat
	find (chn041, itmmas, key2, krf=2) [err=loop]	;krf=2, prdcat-itemno
loop,
	incr ii
	if (ii/1000*1000 .eq. ii) display (15,$scr_pos(1,70),dis)
	xcall ios (chn041, itmmas, read, lokctl) 
	if (lokctl .ne. 0) goto eof
	if (prdcat .gt. encat) goto eof
	if (weight .le. 0) goto loop
	if (ip_wgt .ne. 0) goto loop	;already st

	ip_wgt = weight
	ip_um =
	xcall isio (chn041, itmmas, itemno, write, lokctl)
	goto loop

endoff,
eof,
	call close 
	xcall pgchn ('cp:imsfmn',1)

	stop

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok, chn041

	switch = 5
	xcall files (1,'SU', 041, switch)
	if (switch .eq. 9) return

	chn041 = 1
	opnok = 1

	return
;----------------------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (chn041) close chn041

	return
;----------------------------------------------------

	end


