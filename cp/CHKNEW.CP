;chknew.cp
;
; rebuild smc:ordsch.smm if there are any records in smc:neword.smm
;
; scheduled task runs every 15 minutes.

; 21-apr-2018 ssq: smc & roc
;  5-aug-2018 ssq: call even if neword file is empty, else sched not updated as from dptsts...
; 29-jul-2020 ssq: "lock" file for both smc & roc

;
record	o_buffer
		,a250

record	filen
	dev	,a3
		,a1,':'
	filnam	,a6,	'neword'
		,a1,'.'
	ext	,a3

record	lokfil
	ldev	,a3
		,a1,':'
	lfil	,a5,	'ordsc'
		,a1,'.'
	lext	,a3

record	vars
	i	,d6
	chan	,d6
	lchan	,d6
	cmpcod	,2a3,	'SMC','ROC'
;
proc

;;;	open (66, u, 'smc:ordsc.smc')	;lock file
;;;	reads (66, o_buffer, o_eof) [err=o_eof]

	for i from 1 thru 2
		begin
		ldev = cmpcod(i)
		lext = cmpcod(i)
		clear lchan
		open (lchan,u,lokfil)		;incase ordsch is locked
		reads (lchan, o_buffer, o_eof)[err=o_eof]	;is file locked?
		clear chan
		dev = cmpcod(i)
		ext = cmpcod(i)
		ext(3,3) = 'M'
		open (chan, si, filen)
		xcall ordsc(cmpcod(i))					;8-5-19
	;;;	if (%isinfo (chan, 'NUMRECS')) xcall ordsc(cmpcod(i))	;8-5-19
		close chan	
		close lchan	
		end


o_eof,
;;;	close lchan
	xcall flags (7000000)
	stop
	end

