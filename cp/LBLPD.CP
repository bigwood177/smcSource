;lblpd
subroutine lblpd
	invmas	,a		;current item record	
	chn182	,d		;coptbl channel
	itmqty	,d		;qty counted
	numlbl	,d		;number of labels to print
	desc1	,a		;user input descr
	boxsiz	,a		;box size
	pic	,a		;pic number
	batfil	,a		;batfil

; print shipping labels for current order.  calling pgm opens ordhdr and 
; coptbl files, this routine opens and executes batch file.
;
; 5-dec-16 ssq: add bin size, bin qty, box loc, bin loc

EXTERNAL FUNCTION
	GU_NAME	,A


record	itmmas
	.include 'def:rd041a.def'

record	coinfo
	.include 'def:coinfo.def'

record	ppath		;for rockford...
		,a29,	'rpxprint.exe p2.rpx,c:\users\'
	p_spl	,a1024		;spl:pfile.bat	

record	data
		,a1024	

record	channel
	chnwrk	,d3	;channel for batch file

record	vars
	UNAME	,A25
	tl	,d6
	smc_star	,a1
	roc_star	,a1
	ln	,d4
	cl	,a255
	len	,d8,	00001024
	siz	,d8
	i	,d6
	opnok	,d1
	cmpcod	,a3
	date	,d8
	lotno	,d6
	jpeg_name	,a8
	tab	,a1
	field	,a512
	order	,d6
	lot	,a10
	entry	,a30
	whatno	,d2

;
proc

	call opens
	if (.not. opnok) goto endoff

	xcall cmdln(cl)
	display(15,$scr_pos(2,1),cl)
	reads (15,entry)
	clear data
	xcall ascii(9,tab)
	xcall rdat8(date)
	lotno(1,2) = date(5,6)	;month
	lotno(3,4) = date(7,8)	;day
	lotno(5,6) = date(3,4)	;year
;

	itmmas = invmas			;current item record

	jpeg_name = pic
	ln = %trim(jpeg_name)
	incr ln
	jpeg_name(ln,ln+3) = '.jpg'

;;;	jpeg_name(1,2) = pic,	'XX'
;;;	jpeg_name(3,6) = '.jpg'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ssq 9-21-06 company info now hard coded into label
;;;	xcall oeco (chn182, coinfo, cmpcod)	;get company info
;;;	
;;;	field = 'LABEL'
;;;	call tab_d
;;;
;;;	field = c_name
;;;	call tab_d
;;;
;;;	field = ;;;> c_add1
;;;	call tab_d
;;;
;;;	field = c_add2
;;;	call tab_d
;;;
;;;	field = c_ld,	'XXX XXX-XXXX'
;;;	call tab_d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	field = 'LABEL'
	call tab_d

;;;	field = itemno
;;;	field = 'I~' + itemno		;1-9-07 all items start with "I~"
	field = 'I~' + itemno + if1 + if2 + if3
	call tab_d

	field = itmqty	,'ZZZZZX' [left]
	call tab_d

	field = jpeg_name
	call tab_d

	field = descr
	call tab_d

	field = lotno,	'XXXXXX' [left]
	call tab_d
	
	field = desc1
	call tab_d

	field = boxsiz
	call tab_d

;;;	field = itemno	; 1-9-07 this is for the barcode caption
	field = itemno + if1 + if2 + if3
	call tab_d

	clear smc_star, roc_star
	xcall who(cmpcod)
	if (cmpcod .eq. 'SMC')
	then	smc_star = '*'
	else	roc_star = '*'

	field = smc_star
	call tab_d

	field = roc_star
	call tab_d

;12/5/16:
	field = ibnsiz
	call tab_d

	field = ibnqty,	'ZZZZ' [left]
	call tab_d

	field = ibxloc
	call tab_d

	field = ibnloc
	call tab_d

;;;	xcall delet ('c:\smc\spool\product.txt')
;;;	open (chnwrk, o, 'c:\smc\spool\product.txt')

	xcall delet ('spl:product.txt')
	open (chnwrk, o, 'spl:product.txt')

	if (numlbl .le. 0) numlbl = 1
	for i from 1 thru numlbl	writes (chnwrk,data)
	writes (chnwrk, 'END')
	close chnwrk

	USING CMPCOD SELECT
	('ROC'), begin
		UNAME = %GU_NAME
		p_spl = uname
		tl = %trim(p_spl)
		p_spl(tl+1,tl+34) = '\AppData\spool\product.txt,ZEBRA'
		open (chnwrk,o,'r_pfile.bat')
		writes (chnwrk, ppath(1,%trim(ppath)))
		close chnwrk

		xcall shell (0, 'r_pfile.bat')
		end
	(),	xcall shell (0, batfil)
;;;	(),	xcall shell (0, 'pfile.bat')

	ENDUSING

;;;	xcall shell (0, 'pfile.bat')
;;;	xcall spawn ('pfile.bat')
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

	opnok = 1
	return
;------------------------------------------------

	end



