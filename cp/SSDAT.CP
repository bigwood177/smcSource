;ssdat.dbl
;
;	set date of last order in ship-to file
;
record	shipto
	.include 'def:rd171a.def'
;
record	ordhdr
	.include 'def:rd044a.def'
;
record	dis
	d_file	,a14
		,a2
	ii	,d6

record	vars
	i	,d6
	filen	,5a14,	
&	'smc:ordhdr.smm','smc:slhhdr.smm','smc:slhh13.smm','smc:slhh12.smm',
&	'smc:slhh11.smm'

proc
	open (15,i,'tt:')
	display (15,$scr_clr(screen),'ssdat')

	open (5,su,'smc:shipto.smm')

	for i from 1 thru 5
		begin
		d_file = filen(i)
		open (1,si,d_file)
		call set_date
		end
	close 1
	close 5
	stop

set_date,
	clear ii
	find (1, ordhdr, ^first) [err=loop]
loop,
	incr ii
	if (ii/1000*1000 .eq. ii) display (15,$scr_pos(1,50), dis)
	reads (1, ordhdr, eof)
	if (oloc .ne. 'O') goto loop
	if (oshpto.le.0 .or. oshpto.ge.9998) goto loop

	shcsno = ocusno
	shtono = oshpto
	read (5, shipto, shtkey) [err=no_shipto]
	if (shodat .ge. oorddt) goto loop

	shodat = oorddt
	write (5, shipto, shtkey)

no_shipto,
	goto loop	
eof,
	close 1
	return

end

