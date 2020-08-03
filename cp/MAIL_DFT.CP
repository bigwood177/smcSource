subroutine mail_dft
	cust	,d
	nbr	,d
	chan	,d	;emailc.ism
;
record	emailc
	.include 'def:rd084a.def'
;
record	vars
	i	,d6
	lokctl	,d1
	read	,d1,0
	write	,d1,1
;
proc
	clear emailc
	e_cust = cust
	find (chan, emailc, e_key) [err=loop]
loop,
	xcall ios (chan, emailc, read, lokctl)
	if (lokctl .ne. 0) goto eof
	if (e_cust .ne. cust) goto eof

	if (e_nbr .eq. nbr)
	then	begin
		e_dflt = 0	;default
		xcall isio (chan, emailc, e_key, write, lokctl)
		end
	else	if (e_dflt.eq.0)
			begin
			e_dflt = 9	;not default
			xcall isio (chan, emailc, e_key, write, lokctl)
			end
	goto loop

eof,
	return

	end
