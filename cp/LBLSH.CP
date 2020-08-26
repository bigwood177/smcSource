subroutine lblsh
	order	,d		;current order number	
	chn182	,d		;coptbl channel
	numlbl	,d		;number of labels to print
	batfil	,a		;batch file 'bfile.bat'

; Print shipping labels for current order.  
; Calling pgm opens ordhdr and coptbl files.
; This routine creates "spl:shippping.txt" with the following
; format:
;	LABEL		;first record
;	data		;data to print on label
;	data		;repeat for each label to print
;	END		;last record
;
; This routine executes a batch file (bfile.bat)
; which contains the string:
;	"rpxprint.exe shipping.rpx,spl:shipping.txt"
;
; There are 4 files required to print the lables:
;    file name		location	purpose
; 1. rpxprint.exe	/exe/cp		executable
; 2. shipping.rpx	/exe/cp		format file
; 3. shipping.txt	spl:		parameter file created by this routine
; 4. bfile.bat		/exe/cp		batch file to print labels

; Note that the mainline calling this routine must be started in a directory
; containing bfile.bat, rpxprint.exe & shipping.rpx.

record	crhold
	.include 'def:rd195a.def'

record	ordhdr
	.include 'def:rd044a.def'

record	coinfo
	.include 'def:coinfo.def'

record	coptbl
	.include 'def:rd182a.def'

record	data
		,a2048	
record	bfile
		,a*,	'rpxprint.exe shipping.rpx,shipping.txt'
record	channel
	chnwrk	,d3	;channel for batch file
	chn195	,d3

record	vars
	routine	,a50		;8-24-20
	d_path	,a512
	ln	,d6
	sts	,d6
	len	,d8,	00002048
	siz	,d8
	i	,d6
	opnok	,d1
	cmpcod	,a3
	date	,d8
	lotno	,d6
	path	,a512
	string	,a512
	tab	,a1
	field	,a512
	lot	,a10
	entry	,a30
	read	,d1,0
	lokctl	,d1
	whatno	,d2
	switch	,d1
;
proc

	if (numlbl .le. 0) return

	xcall modname (1, routine)
	upcase routine
	using routine select
	('S_ORDADD'),	nop
	('S_CHANGE'),	nop
	(),	goto dontCheck
	endusing

;check print flag in table
	clear coptbl
	tblcod = 'SL'
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	unlock chn182
	if (lokctl .ne. 0) goto dontCheck
	if (sl_prnt .eq. 'N') xreturn

dontCheck,

;-----------------------------------------------------------
; don't print labels for orders on hold.

	for chn195 from 20 thru 99
		begin
		switch = %chopen(chn195)
		if (.not. switch) exitloop
		end

	switch = 5
	XCALL FILES (CHN195, 'SI', 195, SWITCH)
	read (chn195, crhold, order) [err=not_on_hold]
	goto is_on_hold
not_on_hold,
	clear crhold
is_on_hold,
	CLOSE chn195

	using ch_flag select
	('3'),	xreturn		;nOTHING PRINTS FOR 555 CRDLMT...	
	('4'),	xreturn		;COD 5555 CRDLMT...	
	endusing
;-----------------------------------------------------------

	clear data			;ssq 8-15-06

	call opens
	if (.not. opnok) goto endoff

	xcall ascii(9,tab)
;;;	xcall rdat8(date)
;;;	lotno(1,2) = date(5,6)	;month
;;;	lotno(3,4) = date(3,4)	;year

;;;	lotno(1,2) = oorddt(5,6)	;month
;;;	lotno(3,4) = oorddt(3,4)	;year

;;;	lotno(1,2) = opromd(5,6)	;month
;;;	lotno(3,4) = opromd(7,8)	;day

	lotno(1,4) = opromd(5,8)	;mmdd
	lotno(5,6) = opromd(3,4)	;yr
;

;;;	ordhdr = header			;current ordhdr record
;;;	xcall oeco (chn182, coinfo, cmpcod)	;get company info
	xcall oeco (chn182, coinfo)		;get company info
	
	field = 'LABEL'
	call tab_d

	field = c_name
	call tab_d

	field = c_add1	;restored 9-7-06
;;;	field =		;don't print
	call tab_d

	field = c_add2
	call tab_d

	field = c_ld,	'XXX XXX-XXXX'
	call tab_d

	field = oordno ,'ZZZZZZ' [left]
	call tab_d

	field = ocusnm
	call tab_d

	field = oshpnm
	call tab_d

	field = oshad1
	call tab_d

	field = oshad2
	call tab_d

	field = oshad3
	call tab_d

	field = opono
	call tab_d

	field = ojobno
	call tab_d

	field = lotno,	'ZX-XX-XX' [left]
	call tab_d
	
;--------------------------------------------------------------------
; opening in output mode should destroy any existing file, but during
; testing I was getting data from 2 orders in the same file...
; solution: record data needed to be cleared.

;;;	xcall delet ('c:\smc\spool\shipping.txt')	;just in case
	open (chnwrk, o, 'c:\smc\spool\shipping.txt')
;--------------------------------------------------------------------

	if (numlbl .le. 0) numlbl = 1
	for i from 1 thru numlbl	writes (chnwrk,data)
	writes (chnwrk, 'END')
	close chnwrk

;-----------------------------------------------------
; this code will cause the work files to be opened in the users
; local "spl:" directory...

; should not need this if change made in mkdec.cp
;;;	xcall getlog ('CP', d_path, ln)	;logical for spl...
;;;	sts = %syn_setdir (d_path)		;set current directory to spl...
;-----------------------------------------------------

	xcall shell (0, batfil)
;;;	xcall spawn (batfil)
;;;	xcall spawn ('bfile.bat')
endoff,
	xreturn

;================================================
tab_d,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	xcall tabd (data, len, field, tab)
	return
;------------------------------------------------


opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

	chnwrk = 1
	do forever
		begin
		incr chnwrk
		if (.not. %chopen(chnwrk))	exitloop
		end

	switch = 5
	xcall files (chnwrk, 'SI', 044, switch)
	read (chnwrk, ordhdr, order) [err=no_hdr]
no_hdr,
	close chnwrk
	
	opnok = 1
	return
;------------------------------------------------

	end


