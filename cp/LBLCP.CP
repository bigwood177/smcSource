subroutine lblcp
	order	,a		;current order number	
	chn182	,d		;coptbl channel
	numlbl	,d		;number of labels to print

;This version automaticall prints labels for current order to a specific
;printer as follows:
; ship via = 10 (will call printer) or user = "WCALL" =>
;	print from tray 4 in using will call printer (new blue)
; Else use "SortLabels" connected to PC in Sort Room.


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


record	ordhdr
	.include 'def:rd044a.def'

record	coinfo
	.include 'def:coinfo.def'

record	data
		,a1024	

record	batch_stuff
	bat_1	,a*,	'rpxprint.exe shipping.rpx,shipping.txt'
	loc_bat	,a*,	'c:\smc\spool\ssqbat.bat'

record	channel
	chnwrk	,d3	;channel for batch file

record	vars
	ln	,d6
	len	,d8,	00001024
	siz	,d8
	i	,d6
	opnok	,d1
	cmpcod	,a3
	date	,d8
	lotno	,d4
	path	,a512
	string	,a512
	tab	,a1
	field	,a512
	lot	,a10
	entry	,a30
	whatno	,d2
	switch	,d1
;
proc

	call opens
	if (.not. opnok) goto endoff

	xcall ascii(9,tab)
	xcall rdat8(date)
	lotno(1,2) = date(5,6)	;month
	lotno(3,4) = date(3,4)	;year
;

;;;	ordhdr = header			;current ordhdr record
;;;	xcall oeco (chn182, coinfo, cmpcod)	;get company info
	xcall oeco (chn182, coinfo)		;get company info
	
	field = 'LABEL'
	call tab_d

	field = c_name
	call tab_d

;;;	field = c_add1
	field =		;don't print
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

	field = lotno,	'XXXX' [left]
	call tab_d
	
	open (chnwrk, o, 'c:\smc\spool\shipping.txt')

	if (numlbl .le. 0) numlbl = 1
	for i from 1 thru numlbl	writes (chnwrk,data)
	writes (chnwrk, 'END')
	close chnwrk
	if (oscac .eq. '10')
	then	xcall spawn ('wclbl.bat')	;will call
	else	xcall spawn ('srlbl.bat')	;sort room
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

	xcall files (chnwrk, 'SI', 044, switch)
	read (chnwrk, ordhdr, order) [err=no_hdr]
no_hdr,
	close chnwrk
	
	opnok = 1
	return
;------------------------------------------------

	end


