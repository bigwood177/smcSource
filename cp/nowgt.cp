;nowgt.cp
;
;	non-cfg items w/ zero ip_wgt and no ip_um
;

record	itmmas
	.include 'def:rd041a.def'
;
record	out
	o_item	,a15
		,a1
	o_descr	,a30
		,a1
	o_usrdef,a2
		,a1
	o_ip_wgt,a7	;ZZZ,ZZX
		,a1
	o_ip_um	,a2
		,a1
	o_weight,a7	;ZZZ,ZZX

record	dis
	ii	,d6
;
record	vars
	v	,d1


proc
	open (15,i,'tt:')
	display (15, $scr_clr(screen),'Non-cfg items w/ zero prod weight')

	open (1,si,'smc:itmmas.smm')
	open (10,o,'zero_wght.txt')

	o_item = "item"
	o_descr = 'Description'
	o_ip_wgt = 'prd wgt'
	o_weight = 'act wgt'
	writes (10, out)


	ii = 0
loop,
	incr ii
	if (ii/500*500 .eq. ii) display (15,$scr_pos(1,70),dis)

	reads (1, itmmas, eof)
	goto (loop),icfg	;skip cfg items

	if (ip_wgt .ne. 0) goto loop
	if (ip_um .ne. '  ') goto loop

	if (if1 .ne. '000') goto loop
	if (if2 .ne. '000') goto loop
	if (if3 .ne. '00000') goto loop

	using usrdef select
	('A' thru 'T'),	nop
	(),		goto loop
	endusing


	o_item = itemno
	o_descr = descr
	o_usrdef = usrdef
	o_ip_wgt = ip_wgt,	'ZZZ,ZZX'
	o_ip_um = ip_um
	o_weight = weight, 	'ZZZ,ZZX'

	writes (10, out)
	goto loop

eof,
	close 1
	close 10
	stop
	end
