subroutine o_note
	order	,d
	o_flag	,d	;1=return if no file
;
record	coptbl
	.include 'def:rd182a.def'

record	ord_string
		,a*,	'order: '
	o_ord	,d6

record	word_string
		,a*,	'c:\program files\windows NT\Accessories\wordpad'
		,a1
	w_path	,a100

record	edit_string
		,a*,	'notepad'
		,a1
	path	,a100

record	n_path
;;;		,a*,	'\\serversbs\smc\Notes\'
		,a*,	'\\'
	sv_ser	,a11	
		,a*,	'\smc\Notes\'
	n_order	,a6
		,a4,	'.txt'
record	vars
	flag	,d1
	p	,d6
	ln	,d6
	chn	,d3
	lokctl	,d1
	read	,d1,0
	chn182	,d3
	cmpcod	,a3
	stat	,d1
;
.proc
	clear path
	if (%passed(o_flag))
	then flag = o_flag
	else clear flag

	chn182 = %syn_freechn(50,99)
	open (chn182, si, 'smc:coptbl.smm')
	xcall who (cmpcod)
	clear coptbl
	tblcod = 'SR'
	sr_cmp = cmpcod
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .ne. 0)
		begin
		xcall mesag('no coptbl server record',1)
	;;;	goto endoff
		end

	sv_ser = sr_server
	close chn182


; find the pdf file for the order in then "scan" directory...
	xcall cp_makp(order, path)	;path ENDS IN ".PDF"

	upcase path
	p = %rvstr(1,path,'PDF')
	if (p .le. 1) xreturn	;NOTHING!

	n_order = order, 'XXXXXX'
	path = n_path
;;;	path(p,p+2) = 'txt'

; find an open channel...
	for chn from  1 thru 999
		begin
		xcall chnopn (chn,stat)
		if (stat .eq. 0) exitloop
		end
	if (chn .gt. 900) xreturn

	ln = %trim(path)

; if the file does not already exist, create it
	open (chn, i, path(1,ln)) [err=no_file]
	goto f_edit

no_file,
	if (flag) xreturn		;file does not exist - return.

	open (chn, o ,path(1,ln) )
	writes(chn,'.LOG')
	o_ord = order
	writes (chn, ord_string)


f_edit,
	close chn
	w_path = path
	if (flag .eq. 1)
	then	xcall spawn (word_string(1, %trim(word_string)))
	else	xcall spawn (edit_string(1, %trim(edit_string)))
	xreturn
		
.end

