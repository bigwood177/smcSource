.subroutine ftp
	order	,d	;order number
;
record	dec_file
	df_order	,d6
			,a4,	'.DEC'
record	dec_script
	dn_order	,d6
			,a4,	'.ftp'

record	bat_file
	bf_order	,d6
			,a4,	'.bat'

record	vars
	d_path	,a512
	sts	,d6
	ln	,d6
	chan	,d2
	stat	,d1
	buf	,a100
	tl	,d6

.proc
;-----------------------------------------------------
; this code will cause the work files to be opened in the users
; local "spl:" directory...

	xcall getlog ('SPL', d_path, ln)	;logical for spl...
	sts = %syn_setdir (d_path)		;set current directory to spl...
;-----------------------------------------------------

	set df_order, dn_order, bf_order = order

; create a script file
	for chan from 10 thru 90
		begin
		xcall chnopn (chan, stat)
		if (stat.eq.0)
			begin
			open (chan, o, dec_script)
			exitloop
			end
		end
;;;	open (19, o, dec_script)
	writes (chan, 'open 192.168.1.22')
	writes (chan, 'user USER')
	writes (chan, 'pass PASS')
	writes (chan, 'cd Downloads')
	buf = 'send ' + dec_file
	writes (chan, buf(1, %trim(buf)) )
	writes (chan, 'bye')
	close chan

; create the batch file
	open (chan,o, bat_file)

	buf = 'ftp -n -s:' + dec_script
	writes (chan, buf(1,%trim(buf)) )
	close chan

; execute the batch file

	xcall shell (0, bat_file)

	xcall delet (bat_file)
	xcall delet (dec_file)
	xcall delet (dec_script)

	xreturn
.end


