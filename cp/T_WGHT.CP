;t_wght.cp
; driver to test cfg_2
;
record	vars
	opnok	,d1
	chn182	,d2
	entry	,a30
	inxctl	,d1
	cngctl	,d1
	whatno	,d2
	litmno	,a15
	err	,d1
	mat_sel	,d1
	st_guage	,d2
	weight	,d6
	v	,d1
;
proc
	xcall terid(v)
	xcall outpt (1,1,2,'test configured weights',1)

	call opens
	if (.not. opnok) goto endoff


;
displa,
	clear cngctl
	xcall outpt (1,1,2,'test configured weights',1)
	xcall outpt (4,1,0,'1. item:',1)
	xcall outpt (5,1,0,'2. material:         G/A/P/S/V',1)
	xcall outpt (6,1,0,'3. gauge:',1)
	xcall outpt (7,1,0,'   weight:',1)

litmno,
	xcall input (4,16,15,01,'AE',entry,inxctl,1)
	goto (displa,endoff),inxctl
	litmno = entry(1,15)
	goto (anycng),cngctl
mat,
	xcall input (5,16,01,00,'A ',entry,inxctl,1)
	goto (displa),inxctl
	if (entry(1,1) .eq. ' ') 
		begin
		entry(1,1) = 'g'
		xcall outpt (5,16,0,'G',1)
		end

	using entry(1,1) select
	('g'),	mat_sel = 1
	('a'),	mat_sel = 2
	('p'),	mat_sel = 3
	('s'),	mat_sel = 4
	('v'),	mat_sel = 5
	(),	goto mat
	endusing

	goto (anycng),cngctl
ga,
	xcall input (6,16,02,00,'# ',entry,inxctl,1)
	goto (displa),inxctl
	st_guage = entry(1,2)
anycng,
	xcall anycn (cngctl, whatno)
	goto (process,cngbr), cngctl+1
	goto anycng
cngbr,
	goto (litmno, mat, ga), whatno
	goto anycng
;
process,	
;
	xcall cfg_w (litmno, weight, err, mat_sel, st_guage)

	entry(1,8) = weight, 'Z,ZZZ.XX'
	xcall outpt (7,16,0,entry(1,8),1)

	xcall mesag ('computed configured weight',1)
	
	goto displa
endoff,
	call close
	xcall pgchn ('cp:w_menu',1)
	

opens,
	clear opnok
	open (17, si, 'smc:coptbl.smm')
	chn182 = 17
	opnok = 1
	return
close,
	if (chn182) close chn182
	return

end


