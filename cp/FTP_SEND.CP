;ftp_send.cp
;
;	resend orders
;
record	vars
	order	,d6
	a6	,a6
;
proc
	open (15,i,'tt:')
	display (15,$scr_clr(screen),'FTP machine file to Shop')
;
	display (15, $scr_pos(4,4),'Order: ')
	reads (15, a6)
	order = a6
	xcall ftp (order)
	stop
	end

