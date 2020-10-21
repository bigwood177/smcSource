; must run mset.bat for following import statements to work...
; can't have records named "new"


; 11-7-13 move all "process" records to history so the same orders
; don't show up on error report every run...

; 12-23-19 cash-cust
; 02-05-20 don't attach the same order multiple times (eg: 1 order has 3 ups tracking numbers)
;

import SynPSG.System
import SynPSG.System.IO
import SynPSG.System.Net
import SynPSG.System.Net.Mail
import SynPSG.System.Net.Mime
import SynPSG.System.Net.Sockets
import SynPSG.System.Security


subroutine confr1
	cmpcod	,a
	p_order	,d	;optional order number

;confr1.cp
;
;
;mailor.cp
; 11-1-12:
;	changed this program to use the smtpMail program from Synergex.
;
;mail orders
;	note that this pgm uses the HTML verion of omail.cp, namely
;	mhtml.cp which calls m7_html.dll
;	mhtml.bat does the gennet40, etc and expects that "m7_html.dll"
;	exists (m7_html.dll is created from the vb project of the same
;		name).

;note on embedding images:
;in the code: '<BR><IMG SRC=cid:companylogo height=133 width=200><br>'
;"cid:companylogo" is linked to the code in m7_html.dll (class VB) in
;the VB project - it must have this specific name.
;
;	the first jpg must be "smc_1.jpg", the 2nd must be "smc_2.jpg".
;	companylogo=smc1.jpg, artwork=smc_2.jpg

EXTERNAL FUNCTION
	sig	,a


record
        mailClient     ,@SMTPClient
        mailMsg        ,@MailMessage
        mailAtt        ,@Attachment
	myContentType	,string
        attachment  ,a80        ;Attachment name
	filename    ,a256       ;attachment filename
	fName       ,a256       ;name part of filename
        fExt        ,a256       ;extension part of filename
        chan        ,i4         ;channel
        port        ,i4         ;port
	mycredents, @SynPSG.System.NetworkCredential

;----------- end of 12-10-12 stuff -----------------------

;
	.include 'def:hpsub.def'

.define SMERR_SUCCESS       1
.define SMERR_BADSERVER     2   ;Invalid server name or address
.define SMERR_NORELAY       3   ;Mail server can't relay
.define SMERR_NETPROB       4   ;Failed to connect to SMTP server
.define SMERR_TOLIST        5   ;No recipient address provided
.define SMERR_CCLIST        6
.define SMERR_BODYFILE      7   ;Failed to open body file
.define SMERR_SENDADDR      8
.define SMERR_SERVERFAIL    9   ;Invalid response from SMTP server
.define SMERR_BADLIST       10
.define SMERR_ATTACHFILE    11  ;Failed to open attachment file
.define SMERR_BCCLIST       12


    external function
        smtpM    ,^val

record	smtp_defines
	server	,a*,	'192.168.1.4'
;;;	server	,a*,	'192.168.1.203'
	domain	,a*,	'smcduct.com'
	sender	,a*,	'Orders@SMCduct.com'
	senderName,	a*,	'Sheet Metal Connectors'


record	varatt
	attch_array	,10a80


record	err_rec
	err_name	,a25
			,a1
	err_ord		,d6
			,a1
	err_email	,a25
			,a1
	err_msg		,a35


record	emailc
	.include 'def:rd084a.def'

record	confrm
	.include 'def:rd158a.def'

;;;record	shipto
;;;	.include 'def:rd171a.def'

record	blhead
	.include 'def:rd178a.def'

record	ordhdr
	.include 'def:rd044a.def'

record	coptbl
	.include 'def:rd182a.def'

record	badmsg
	bad_cust	,d6
			,a1,	'-'
	bad_nbr		,d3
			,a1
	bad_ord		,d6
			,a*,	' bad email!! '
	bad_mail	,a30
	bad_slman	,a3
	bad_date	,a10

record	cuserr
	cer_cus	,d6
	cer_nam	,a15

record	channel		;this record passed as a parameter
	chn001	,d2
	chn002	,d2
	chn044	,d2	;header file
	chn045	,d2	;l/i file
	chn175	,d2	;duct file
	chn054	,d2
	chn068	,d2
	chn075	,d2
	chn139	,d2
	chn170	,d2
	chn182	,d2
	chnerr	,d2
	chn138	,d2

record	chn2
	chn158	,d2	;confrm
;;;	chn171	,d2	;shipto
	chn159	,d2
	chn184	,d3
	chn084	,d2
	chn178	,d3
	chn188	,d3

record	chn_cash_cust
	chn103	,d3	;misemc
	chnemc	,d3	;either chn084 or chn103 (misemc/emailc)

record	splfil	
		,a4,	'spl:'
	spl_ord	,d6
		,a4,	'.spl'

record	errfil
		,a5,	'spl:E'
	errtim	,a5
		,a4,	'.spl'
;;;		,a14,	'spl:mailor.err'

;;;record	new	;turns out you can't have a record named "new"
record	xnew
	n_from	,a80	;from
	n_toN	,a40
	n_toA	,a500
	n_subj	,a100	;subject
	n_body	,a2000	;body
	n_att1	,a100	;attachment 1
	n_att2	,a9	;attachment 2
	n_att3	,a9	;attachment 3
	n_att4	,a9	;attachment 4

record	ordara
	oa	,10d6	;order #'s for current cust.
	op	,10a10	;po #'s
	oj	,10a10	;job #'s
	oidx	,d6

		
;dt.dbl
;
record	varfs
	dt	,a20
;
record,x
	yyyy	,d4
	mm	,d2
	dd	,d2
	hh	,d2
	min	,d2
record,x
		,a8
	dtime	,d4


record	salman
	thnku	,a20
	name 	,a20
	title 	,a20
	d_ph	,a15
	d_fax 	,a15
	d_em 	,a30

record	ups_track
	ut_string	,a*,	'https://wwwapps.ups.com/WebTracking/track?track=yes&trackNums=1Z'
	ut_track	,a20


record	vars
	savord	,d6		;2-5-20
	err_cnt	,d6
	scac	,a4		;6-9-14
	first_order	,d1
	LNAM	,A25
	SNAM	,A12
	SINT	,A3
	order	,d6		;for an immediate confirmation
	sav_fnam	,a10
	sav_lnam	,a15
	ship_line	,a100
	car_name	,a30
	car_tel2	,d10
	dt_line	,a20
	xdate	,d8
	xtime	,d4
	ampm	,a2
	splerr	,d2
	maxidx	,d3,	10
	errors	,d1
	status	,i4
	encodetype	,d1,1
	errFlag	,@int	;1 = mail not sent
	a2	,a2
	a10	,a10
	a12	,a12
	q	,a1	;quote
	clctl	,d1
	redfil	,a14
	cr	,a1
	lf	,a1
	filen	,a14
	reccnt	,d6
	sentOK	,d18
;;;	cmpcod	,a3
	a1	,a1
	opnok	,d1
	chmail	,d2
	t1	,d6
	t2	,d6
	tlen	,d3,	500
	savcus	,d6
	savnbr	,d3
	pline	,a132
	trans	,a100
	pdffil	,a100
	dprim	,d1	;1= primary
	tl	,d6
	nbl	,d3, 600		;length of n_body	
	spllen	,d6
	len	,d6
	lens	,d6
	lenp	,d6
	read	,d1,0
	write	,d1,1
	lokctl	,d1
	switch	,d1
	v	,d1
.proc
	xcall terid (v)
	xcall ascii(34,q)	;quote (")
	xcall getlog ('spl', trans, spllen)
	clear errors

	if (%passed(p_order) )
	then	order = p_order
	else	order = -1

    ;; new stuff for credentials...
    mycredents = new SynPSG.system.NetworkCredential()
;;;    mycredents.Username="smctest"
;;;    mycredents.Password="smcduct"
    mycredents.Username="orders"    
    mycredents.Password="$MCod5900"


    mycredents.Domain = "smcduct.com"
    
    ;; put default values here
    port = 25
    attachment = ''

;;;    sender = "wood@smcduct.com"

;;;	xcall who (cmpcod)
	using cmpcod select
	('SMC','TST'),	nop
	('CAT'),	nop
	('ROC'),	nop
	(),	goto fini	;skip unless smc
	endusing

	n_from = 'Orders@SMCduct.com'
	call opens
	if (.not. opnok) goto endoff


	mailClient = new SMTPClient(server, port)

	savcus = -1
	savnbr = -1
	savord = -1		;2-5-20

	reccnt = 0

	find (chn158, confrm, ^first) [err=eof]
loop,
	reads (chn158, confrm, eof)
	if (sp_cust .le. 0) goto loop
	if (sp_mnbr .le. 0) goto c_hist	;no email, move directly to hist file

	if (order.gt.0 .and. sp_ord.ne.order) goto loop	;immediate confirmation

	if (sp_cust .eq. 10)			;12-23-19 cash-cust
	then	if (savcus.eq.10 .or. sp_cust.ne.savcus) call newcus
	else	begin
		if (sp_cust .ne. savcus) call newcus
		if (sp_mnbr .ne. savnbr) call newcus
		end
	
	if (bad_cust) goto c_hist	;11-7-13: once is enough...
;;;	if (bad_cust) goto loop		;11-7-13


	read (chn044, ordhdr, sp_ord) [err=no_order]
	scac = oscac
	xcall isio (chn178, blhead, oordno, read, lokctl)
	if (lokctl .eq. 0) scac = bhscac

;;;	read (chn044, ordhdr, sp_ord) [err=c_hist]
;;;	read (chn044, ordhdr, sp_ord) [err=loop]
	goto order_ok
no_order,
	read (chn184, ordhdr, sp_ord) [err=c_hist]
	scac = oscac
	xcall isio (chn188, blhead, oordno, read, lokctl)
	if (lokctl .eq. 0) scac = bhscac
order_ok,
	if (first_order) call get_salesman

	call open_spoolfile
	xcall mkors (sp_ord, channel, 1, cmpcod)	;no prices
;;;	xcall mkors (sp_ord, channel, 1)	;no prices

	close 14

	if (sp_ord .eq. savord) goto dupord		;2-5-20
	savord = sp_ord					;2-5-20

	incr oidx

	len = spllen
	trans (len+1, len+11) = '\' + splfil(5,14)
	pdffil = trans
	len = %trim (pdffil)
	len = len - 2
	pdffil(len, len+2) = 'pdf'

	lens = %trim(trans)
	lenp = %trim(pdffil)
	XCALL MKPDF (trans(1,lens), PDFFIL(1,lenp) )

	attch_array(oidx) = pdffil

dupord,							;2-5-20
;
;  add confirmation info to the body of the order...

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br>'
	tl = %trim(n_body)
	tl = tl + 2
	n_body(tl,tl+20)= '<b>SMC ORDER # </b>'

	tl = %trim(n_body)
	tl = tl + 2
	n_body(tl,tl+6)= oordno, 'ZZZZZX'
	tl = %trim(n_body)
	tl = tl + 2
	n_body(tl,tl+8) =''

	if (opono .ne. a10)
	  begin
	  tl = %trim(n_body)
	  tl = tl + 2
	  n_body(tl,tl+30)= '&nbsp &nbsp <b>PO# </b>'
	  tl = %trim(n_body)
	  tl = tl + 2
	  n_body(tl,tl+10)= opono
	  end

	if (ojobno .ne. a10)
	  begin
	  tl = %trim(n_body)
	  tl = tl + 2
	  n_body(tl,tl+28)= '&nbsp &nbsp <b>  JOB# </b>'

	  tl = %trim(n_body)
	  tl = tl + 2
	  n_body(tl,tl+10)= ojobno
	  end


;line 2
	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br>'

	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + '<b> Ordered by: </b>'

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + SAV_FNAM
	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + SAV_LNAM

	tl = %trim (n_body)
	tl = tl + 2
	n_body(tl,tl+36) = '&nbsp &nbsp <b> Shipped Via: </b>'

	tl = %trim (n_body)

	using sp_src select
	('W'),	begin
		n_body = n_body(1,tl+1) + 'Will Call' + '<br>'

		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + '<b> Signed by: </b>'
		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + sp_conf		
		tl = %trim (n_body)
		
		call d_time

		n_body = n_body(1,tl+1) + dt_line		

		end

	('T'),	begin
		n_body = n_body(1,tl+1) + 'Our Truck' + '<br>'

		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + '<b> Expected Del: </b>'
		tl = %trim (n_body)
		xdate(1,4) = sp_exd(5,8)
		xdate(5,8) = sp_exd(1,4)
		a10 = xdate, 'ZX/XX/XXXX'
		n_body = n_body(1,tl+1) + a10 
		tl = %trim (n_body)

		if (sp_ap .eq. 'A')
		then n_body = n_body(1,tl+1) + 'AM' + '<br>'
		else n_body = n_body(1,tl+1) + 'PM' + '<br>'

		end

	('U'),	begin
		n_body = n_body(1,tl+1) + 'UPS'
		tl = %trim (n_body)
		tl = tl + 2
		n_body(tl,tl+40) = '&nbsp &nbsp <b> Packages: </b>'
		tl = %trim (n_body)
		a2 = sp_pkg, 'ZX' [left]
		n_body = n_body(1,tl+1) + a2		
		tl = %trim (n_body)

		n_body = n_body(1,tl+1) + '<br>'
		tl = %trim (n_body)
		tl = tl + 2
		n_body(tl,tl+40) = '&nbsp &nbsp <b> Tracking # </b>'
		tl = %trim (n_body)
	;---------------------------------------------------------------
	; link to ups tracking ...
;;; https://wwwapps.ups.com/WebTracking/track?track=yes&trackNums=1Z8E37090303304426

		n_body = n_body(1,tl+1) + '<a href = ""'

		ut_track = sp_ref

		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + ups_track


		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + '"">' + sp_ref

		tl = %trim (n_body)
		n_body = n_body(1,tl+1)  + '</a>'+ '<br>'

	;;;	n_body = n_body(1,tl+1) + sp_ref + '<br>'

	;---------------------------------------------------------------


		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + '<b> Shipped To: </b>'

		call ship_line 

		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + ship_line + '<br>'

		end

	('S'),	begin
		n_body = n_body(1,tl+1) + 'Spee-Dee'
		tl = %trim (n_body)
		tl = tl + 2
		n_body(tl,tl+40) = '&nbsp &nbsp <b> Packages: </b>'
		tl = %trim (n_body)
		a2 = sp_pkg, 'ZX' [left]
		n_body = n_body(1,tl+1) + a2		
		tl = %trim (n_body)

		n_body = n_body(1,tl+1) + '<br>'
		tl = %trim (n_body)
		tl = tl + 2
		n_body(tl,tl+40) = '&nbsp &nbsp <b> Tracking # </b>'
		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + sp_ref + '<br>'

		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + '<b> Shipped To: </b>'

		call ship_line 

		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + ship_line + '<br>'

		end

	('P'),	begin
		call get_scac
		n_body = n_body(1,tl+1) + car_name + '<br>'
		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + '<b> Carrier Phone # </b>'
		tl = %trim (n_body)

		a12 = car_tel2, 'ZZZ ZZZ ZZZZ' [left]
		n_body = n_body(1,tl+1) + a12
		tl = %trim (n_body)
		tl = tl + 2
		n_body(tl,tl+40) = '&nbsp &nbsp <b> Website: </b>'
		tl = %trim (n_body)
;;;		n_body = n_body(1,tl+1) + '<a href = "'
		n_body = n_body(1,tl+1) + '<a href = ""'

		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + sw_web

		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + '"">' + sw_web

		tl = %trim (n_body)
		n_body = n_body(1,tl+1)  + '</a>'+ '<br>'

		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + '<b> PRO # </b>'
		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + sp_ref + '<br>'

		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + '<b> Shipped To: </b>'

		call ship_line 

		tl = %trim (n_body)

		n_body = n_body(1,tl+1) + ship_line 
		tl = %trim (n_body)
		n_body = n_body(1,tl+1) + '<br>'
		
		end

	endusing

;----------------------------------------------------

c_hist,
	store (chn159, confrm)	;159 = confhs - history file
	delete (chn158)		;delete from process file

	goto loop

newcus,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (savcus .eq. -1) goto outcus
	close 14

	if (oidx .le. 0) goto outcus	;nothing attached	

;line 4
	call get_body
	

;;;	n_toa = 'wood@smcduct.com'
	tl = %trim(n_toA)


        mailAtt = ^NULL

	clear status


	Try
	begin
		mailMsg = new MailMessage (%atrim(sender), %atrim(n_toA),%atrim(n_subj), %atrim(n_body))
	end
	catch (ex, @Exception)
		begin
		status = 1
		end

	endtry

	if (status) 
		begin
		call mail_error
		goto bad_mail
		end


	mailMsg.IsBodyHtml = 1

	myContentType = 'application/octet-stream'

	oidx = 0
att_loop,
	incr oidx
	attachment = attch_array(oidx)
	if (!attachment) goto att_done

	parse(attachment,,,,, fName, fExt,, filename)
	mailAtt = new Attachment(%atrim(filename), new ContentType(myContentType))
	mailAtt.Name = %atrim(fName) + %atrim(fExt)
	mailMsg.Attachments.Add(mailAtt)
	goto att_loop

att_done,

	clear err_cnt
re_send,
	clear status
	try
	begin
		mailClient.Send (mailMsg, myCredents)
	end

	catch (ex, @Exception)
		begin
		if (%instr(1,ex.Message,"SMTP response 535") )
			begin
			incr err_cnt
			if (err_cnt .le. 3) goto re_send
			end
		if (%instr(1,ex.Message,"SMTP response 999") )
			begin
			incr err_cnt
			if (err_cnt .le. 3) goto re_send
			end

	;;;	writes (chnerr, "mailit error:" + ex.Message)
		writes (splerr, "CONFR1 error:" + ex.Message)
		status = 1
		end

	endtry

	if (status) call mail_error

bad_mail,
	clear mailMsg, mailAtt


;	onerror inuse
	if (lens.gt.0) xcall delet (trans(1,lens) )
;;;	xcall delet (pdffil(1,lenp) )
	xcall delet (splfil)
inuse,
;	offerror

outcus,	
	savcus = sp_cust
	savnbr = sp_mnbr
	first_order = 1			;some processing only for first order.
	for oidx from 1 thru maxidx
		begin
		clear oa(oidx), op(oidx), oj(oidx)
		end	

	for oidx from 1 thru 10	clear attch_array(oidx)
	clear oidx

;12-23-19: for cash customers need to use file misemc instead of emailc.

	read (chn044, ordhdr, sp_ord) [err=no_order2]
	goto order_ok2
no_order2,
	read (chn184, ordhdr, sp_ord) [err=no_order3]
order_ok2,
	if (omicus .gt. 0)
	then	chnemc = chn103		;use misemc
	else	chnemc = chn084		;use emailc
;-

	call set_mail_top

no_order3,
	return
;--------------------------------------------------

get_body,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	tl = %trim (n_body)

	n_body = n_body(1,tl+1) + '<br><br>'
	tl = %trim (n_body)

	thnku = 'Thank you'
	name = 	;;;>'Dianna Hanson'
	title = ;;;>'Credit Manager'
	d_ph = ;;;>'763.123.1234'
	d_fax = '763.572.1100'
	d_em = ;;;>'diannah@smcduct.com'

	n_body = n_body(1,tl+1) + %sig(thnku, name, title, d_ph, d_fax, d_em)

	return
;------------------------------------------------------
; 5-11-15 this code no longer required
;;;
;;;	tl = %trim (n_body)
;;;	n_body = n_body(1,tl+1) + '<br><br><br><br>Thank you,'
;;;
;;;	tl = %trim (n_body)
;;;	n_body = n_body(1,tl+1) + '<br><br>Sheet Metal Connectors, Inc.'
;;;
;;;
;;;	tl = %trim (n_body)
;;;	n_body = n_body(1,tl+1) + '<br><br><a href="www.smcduct.com"> www.smcduct.com </a>'
;;;
;;;
;;;	tl = %trim (n_body)
;;;
;;;	tl = %trim (n_body)
;;;	n_body = n_body(1,tl+1) + '<br><br><br><br>*Please do not reply to this email as it has been automatically generated <br>'
;;;
;;;	tl = %trim (n_body)
;;;	n_body = n_body(1,tl+1) + '</html>'
;--------------------------------------------------------------------------

set_mail_top,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; headers and common part of body
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear n_toA, bad_cust

	t1 = 1

if (savnbr .le. 0) goto mail_to_wood	;dbg
;;;	clear tbl_key
;;;	tblcod= 'E2'
;;;	E2_cust = savcus
;;;	e2_nbr = savnbr
;;;	read (chn182, coptbl, tbl_key) [err=email_nf]

	clear emailc
	if (omicus .gt. 0)
	then	e_cust = omicus
	else	e_cust = savcus

;;;	e_cust = savcus
	e_nbr = savnbr
	read (chnemc, emailc, e_key) [err=email_nf]	;12-23-19
;;;	read (chn084, emailc, e_key) [err=email_nf]

	if (.not. %instr(1,e_mail,'@') )	;5-20-13
	then	call bad_email
	else	n_toA = e_mail
	goto not_wood

email_nf,
	clear coptbl
	e_mail = 'No Email Coptbl Record'
	goto bad_email

mail_to_wood,
	n_toa = 'orders@smcduct.com'	;debug
;;;	n_toa = 'wood@smcduct.com'	;debug
not_wood,
	sav_fnam = e_fnam
	sav_lnam = e_lnam

	n_subj = 'Sheet Metal Connectors, Inc - SHIPPING NOTIFICATION'

	n_body = '<html>'	;start html...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 5-11-15 ssq: standardize signature line

;;;	tl = %trim (n_body)
;;;
;;;	n_body = n_body(1,tl+1) + 
;;;&		'<BR><IMG SRC="""\\serversbs\smc\wrk\smc_1.jpg"><br>'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + 'Thank you for your recent order with Sheet Metal Connectors, Inc.  Below you will' + '<br>'

	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + 'find a list of orders that shipped.  You can track your'

	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + ' orders by clicking on the links in this email' + '<br>'

	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + 'If you have any other questions '; + '<br>'

	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + 'please call SMC at 1-800-328-1966.'+ '<br><br><br><br>'


	return
;-----------------------------------------------------

d_time,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xdate(1,4) = sp_dt(5,8)
	xdate(5,8) = sp_dt(1,4)
	xtime = sp_dt(9,12)
	ampm = 'AM'

	if (xtime.ge.1300) 
		begin
		xtime = xtime - 1200
		ampm = 'PM'
		end

	dt_line(1,10) = xdate,	'ZX/XX/XXXX'
	dt_line (12,16)= xtime, 'ZX:XX'
	dt_line (18,19) = ampm

	return
;-----------------------------------------------------

ship_line,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ship_line = oshad1
	tl = %trim(ship_line)
	ship_line = ship_line(1,tl+1) +  oshad2

	tl = %trim(ship_line)
	ship_line = ship_line(1,tl+1) +  oshad3

	return
;-----------------------------------------------------

get_scac,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear tbl_key
	tblcod= 'SC'
	sc_scac = scac
;;;	sc_scac = oscac
	read (chn182, coptbl, tbl_key) [err=no_scac]
	goto scac_ok

no_scac,
	clear coptbl
scac_ok,
	car_name = sc_name
	car_tel2 = sc_tel2		;tracing phone number

	clear tbl_key
	tblcod= 'SC'
	sw_scac = oscac
	sw_w = 'W'
	read (chn182, coptbl, tbl_key) [err=no_web]
	goto web_ok
no_web,
	clear coptbl
web_ok,

	return
;-----------------------------------------------------

bad_email,

	read (chn044, ordhdr, sp_ord) [err=bm_no_order]
	goto bm_order_ok
bm_no_order,
	read (chn184, ordhdr, sp_ord) [err=bm_c_hist]
	goto bm_order_ok
bm_c_hist,
	clear ordhdr
bm_order_ok,
	if (first_order) call get_salesman

	bad_cust = savcus		;5-20-13
	bad_mail = e_mail		;5-20-13
	bad_ord = sp_ord
	bad_nbr = sp_mnbr
	bad_slman = sint

	xdate(1,4) = sp_dt(5,8)
	xdate(5,8) = sp_dt(1,4)
	bad_date = xdate, 'ZX/XX/XXXX'
	writes (splerr, badmsg)
	errors = 1
	return
;--------------------------------------------------

open_spoolfile,
	spl_ord = sp_ord
	open (14,o,splfil)
	return
;--------------------------------------------------
;not_sent,
;	ns_order = oa(1)
;	ns_mail = n_toa
;	writes (chnerr, not_send)
;	return
;--------------------------------------------------
mail_error,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear err_rec

	err_name = ocusnm
	err_ord = oordno
	err_email = n_toa
;;;	err_msg = ex.Message

;;;	using status select
;;;	(1),	err_msg = 'success'
;;;	(2),	err_msg = 'invalid server IP'
;;;	(3),	err_msg = "mail server can't relay"
;;;	(4),	err_msg = 'Failed to connect to SMTP server'
;;;	(5),	err_msg = 'No recipient address'
;;;	(6),	err_msg = ' '
;;;	(7),	err_msg = 'failed to oopen body file'
;;;	(8),	err_msg = ' '
;;;	(9),	err_msg = 'invalid response from SMTP server'
;;;	(10),	err_msg = ' '
;;;	(11),	err_msg = 'failed to open attachement'
;;;	endusing

;;;	writes (chnerr, err_rec)
	writes (splerr, err_rec)
	errors = 1
	return
;---------------------------------------------------------
GET_SALESMAN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL SREP(OSLMAN,LNAM,SNAM,SINT)
	clear first_order
	RETURN
;-----------------------------------------------------


eof,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-18-11 - empty files not marked as processed!

	if (savcus .ne. -1) call newcus



endoff,

	call close
	close 44

fini,

	xreturn

;;;	xcall flags (7000000)
;;;	stop



OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (1,'SI',001,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN001 = 1

;;;	XCALL FILES (2,'I', 002, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN002 = 2


	XCALL FILEC (CMPCOD, 4,'SI',44,SWITCH)		;FILE # 44 -- ORDHDR FILE
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 4

	XCALL FILEC (CMPCOD, 184, 'SI', 184, SWITCH)	;184= SLHHDR
	IF (SWITCH .EQ. 9) RETURN
	CHN184 = 184

	XCALL FILEC (CMPCOD, 5,'SI',45,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 5

	XCALL FILEC (CMPCOD, 6,'SI',175,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN175 = 6


;;;	XCALL FILEC (CMPCOD, 7,'SU',171,SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN171 = 7

	XCALL FILEC (CMPCOD, 8,'SU',158,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN158 = 8

	XCALL FILEC (CMPCOD, 9,'SU',159,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN159 = 9

	XCALL FILEC (CMPCOD, 76,'SI',075,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN075 = 76


	XCALL FILEC (CMPCOD, 17,'I',170, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN170 = 17

	XCALL FILEC (CMPCOD, 38, 'SI', 138, SWITCH)	;CCTRAN
	IF (SWITCH .EQ. 9) RETURN
	CHN138 = 38

;-------------------------------------------------------
; open the smc table for email address's
;
	XCALL FILEC (CMPCOD, 18,'SI',182,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 18

;;;	XCALL FFILC ('SMC',182, REDFIL, CLCTL)
;;;	REDFIL(14,14) = 'M'
;;;	OPEN (18,SI,REDFIL)
;;;	CHN182 = 18
;-------------------------------------------------------

	XCALL FILEC (CMPCOD, 12, 'I', 54, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN054 = 12

	XCALL FILEC (CMPCOD, 13, 'SI', 139, SWITCH)	;TMPCUS
	IF (SWITCH .EQ. 9) RETURN
	CHN139 = 13

	switch = 5
	XCALL FILEC ('SMC', 84,'SI',84,SWITCH)		;FILE # 84 -- emailc
	IF (SWITCH .EQ. 9) RETURN
	CHN084 = 84

	SWITCH = 5
	XCALL FILEC (CMPCOD, 103, 'SI', 103, SWITCH)	;FILE # 103 -- misemc
	IF (SWITCH .EQ. 9) RETURN
	CHN103 = 103

	SWITCH = 5
	XCALL FILEC(CMPCOD, 178, 'SI', 178, SWITCH)	;BLHEAD
	IF (SWITCH .EQ. 9) RETURN
	CHN178 = 178

	SWITCH = 5
	XCALL FILEC(CMPCOD, 188, 'SI', 188, SWITCH)	;BLHEAD
	IF (SWITCH .EQ. 9) RETURN
	CHN188 = 188

;;;	open (20, A, 'smc:conf-err.dat')
;;;	chnerr = 20

	dt = %datetime
	errtim = dtime,	'XXRXX'

	open (21, O, errfil)
	splerr = 21

	OPNOK = 1
	RETURN
;-----------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN001) CLOSE CHN001
;;;	IF (CHN002) CLOSE CHN002

	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045
	IF (CHN175) CLOSE CHN175
	IF (CHN184) CLOSE CHN184

	IF (CHN068) CLOSE CHN068
	IF (CHN075) CLOSE CHN075
	IF (CHN054) CLOSE CHN054
	IF (CHN170) CLOSE CHN170
	IF (CHN182) CLOSE CHN182
	IF (CHN138) CLOSE CHN138
	IF (CHN139) CLOSE CHN139

	IF (CHN158) CLOSE CHN158
;;;	IF (CHN171) CLOSE CHN171
	IF (CHN159) CLOSE CHN159

	IF (CHN084) CLOSE CHN084
	IF (CHN103) CLOSE CHN103

	IF (CHN178) CLOSE CHN178
	IF (CHN188) CLOSE CHN188

;;;	if (chnerr) close chnerr
;;;	if (chmail) close chmail
	if (splerr) close splerr	;10-8-13
	if (errors) 
		begin
	;;;> 10-8-13	close splerr
		lpque (errfil)
		end

	RETURN
;-----------------------------------------------


