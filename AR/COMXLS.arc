;comxls.dbl
;
;		make comma delimited file for moving commissions 
;		to excell spread sheet
;
record	comdue
	.include 'def:rd050a.def'

record	comxls
	e_slsman	,d2
	e_year		,d2
	e_inv		,d5
	e_sales		,d10
	e_frt		,d10

record	dis
	i	,d6

record	comkey
	com_slsman	,d2
	com_yr		,d2
;
record	vars
	today	,d6
	cur_year	,d2
proc
	open (15,i,'tt:')
	xcall rdate(today)
	cur_year = today(5,6)

	display (15,$scr_clr(screen),'comxls')
	open (1,i,'smc:comdue.arc')
	open (2,su,'comxls')
loop,
	incr i
	if (i/1000*1000.eq.i) display (15,$scr_pos(1,70), dis)
	reads(1,comdue,eof)
	if (comdue .eq. ']]]]]]') goto eof
	com_slsman = kslman
	com_yr = kinvdt(5,6)
	if (com_yr .eq. cur_year) goto loop	;don't get current year
						;from archive file
	read (2, comxls, comkey) [err=no_record]
	incr e_inv
	e_sales = e_sales + kinvam
	e_frt = e_frt + kfrght
	write (2, comxls, comkey)
	goto loop
no_record,
	clear comxls
	e_slsman = kslman
	e_year = kinvdt(5,6)
	e_inv = 1
	e_sales = kinvam
	e_frt = kfrght
	store (2, comxls, comxls(1,4))
	goto loop
eof,
	close 1
	close 2
	stop
	end


