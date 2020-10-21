import SynPSG.System
import SynPSG.System.IO
import SynPSG.System.Net
import SynPSG.System.Net.Mail
import SynPSG.System.Net.Mime
import SynPSG.System.Net.Sockets
import SynPSG.System.Security




subroutine smalor
	cmpcod	,a


;------------------------------------------------------------
; 2-24-14: important information:
;
; the above import statements are expecting that the following
; logicals have been set (use mset.bat):
;set obj=\\serversbs\smc\source\mail\SynPsg.System\obj
;set synexpdir=\\serversbs\smc\source\mail\SynPsg.System\hdr
;set synimpdir=\\serversbs\smc\source\mail\SynPsg.System\hdr

; the \\serversbs\smc\source\mail\SynPsg.System\hdr should containt
; "prototype" files created by dblproto.  The files currently in that
; directory were created by opening SynPsg.System.vpj in workbench and
; then "building".  
; if for any reason the \hdr folder is empty you may need to repeat
; this process.


;this is a subroutine version of mailor.cp
;
; 12-10-12 ssq: new stuff
; must run mset.bat for following import statements to work...
;mailor.cp
; 11-1-12:
;	changed this program to use the smtpMail program from Synergex.
;
; 6-30-16: try 3 times if authentication error.
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

; 12-10-12 ssq: new stuff
; must run mset.bat for following import statements to work...

; 6-6-18 SSQ: MAKE CUSMAS ISAM


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
	sig,	a


record	smtp_defines
	server	,a*,	'192.168.1.4'
;;;	server	,a*,	'192.168.1.203'
	domain	,a*,	'smcduct.com'
	sender	,a*,	'Orders@SMCduct.com'
	senderName,	a*,	'Sheet Metal Connectors'


record	varatt
	attch_array	,20a80


record	err_rec
	err_status	,a7
			,a1
	err_name	,a25
			,a1
	err_ord		,d6
			,a1
	err_email	,a25
			,a1
	err_msg		,a35


record	not_send
	ns_order	,d6
			,a*,	' confirmation not sent'
;	ns_mail		,a30
;

RECORD	EMFILE			;will be ORFILE.DAT for this pgm
	EF_FILE	,A17
	EF_PROC	,D1		;1 = PROCESSED

record	del_ism
	is_file	,a17		;same as ef_file
		,a1,	'.'
	is_ext	,a3		;is1 & ism


record	ordhdr
	.include 'def:rd044a.def'

record	ordlin
	.include 'def:rd045a.def'

record	emailc
	.include 'def:rd084a.def'

record	email
	.include 'def:c2mail.def'

record	coptbl
	.include 'def:rd182a.def'

record	slsman
	.include 'def:rd054a.def'

;
record	badmsg
	bad_cust	,d6
			,a1,'-'
	bad_nbr		,d3
			,a1
	bad_mail	,a50
			,a*,	' is bad!!'

record	cuserr
	cer_cus	,d6
	cer_nam	,a15

record	channel
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

record	chn_cash_cust
	chn103	,d3	;misemc
	chnemc	,d3	;either chn084 or chn103 (misemc/emailc)

record	splfil	
		,a4,	'spl:'
	sp_ord	,d6
;;;	sp_cust	,d6
		,a4,	'.spl'

record	errfl2
	ef2	,a3	;company code
		,a11,	':mailor.err'

record	errfil
		,a5,	'spl:T'
	errtim	,a5
		,a4,	'.spl'
;;;		,a14,	'spl:mailor.err'

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
	oa	,20d6	;invoice #'s for current cust.
	op	,20a10	;po #'s
	oj	,20a10	;job #'s
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

record	vars
	savord	,d6	;sav omicus	12-30-19
	has_L	,d1	;1=has dept "L" on order
	SHOP_STD	,A*,	'SMC_Shop_Standards_2019.pdf'
	STDPDF	,A200

	err_cnt	,d6
	chn084	,d2
	splerr	,d2
	maxidx	,d3,	20
	errors	,d1
	status	,i4
	encodetype	,d1,1
	errFlag	,@int	;1 = mail not sent
	a10	,a10
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
;;;	xcall terid (v)


	xcall ascii(34,q)	;quote (")
	xcall getlog ('spl', trans, spllen)
	clear errors

    ;; new stuff for credentials...
    mycredents = new SynPSG.system.NetworkCredential()
    mycredents.Username="orders" 
    mycredents.Password="$MCod5900"   ;5-16-16

;;;    mycredents.Username="smctest"
;;;    mycredents.Password="smcduct"
    mycredents.Domain = "smcduct.com"
    
    ;; put default values here
    port = 25
    attachment = ''

; 8-7-19 ssq: attach smc_shop_standards_2019.pdf
	xcall getlog ('UT', STDPDF, len)
	STDPDF = STDPDF(1,LEN) + '\' + SHOP_STD 

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

; mailor needs name of email file...
	filen(1,3) = cmpcod
	filen(4,4) = ':'
	filen(5,14) = 'orfile.dat'
	OPEN (44,u,filen)

	clear chmail
	reccnt = 0
eml,
	incr reccnt
	read (44, EMFILE, reccnt) [err=eof]	;any email files not processed?
	if (ef_proc .ne. 0) goto eml

	CLOSE 44
	open (44,su,ef_file)
	chmail = 44


loop,
	reads (chmail, email, eof)
	if (c2_cust .le. 0) goto loop
;;;	if (c2_cust .ne. savcus) call newcus
;;;	if (c2_nbr .ne. savnbr) call newcus

	if (c2_cust .eq. 10)			;12-23-19 cash-cust
	then	if (savcus.eq.10 .or. c2_cust.ne.savcus) call newcus
	else	begin
		if (c2_cust .ne. savcus) call newcus
		if (c2_nbr .ne. savnbr) call newcus
		end


	if (bad_cust) goto loop

	read (chn044, ordhdr, c2_order)[err=loop]
	if (oslman.gt.0) 						;10-24-18
	then	xcall io (chn054, slsman, oslman, read, lokctl)
	else 	clear slsman
	
	call get_L			;8-14-19 does order have dept "L"?

	call open_spoolfile
	xcall mkors (c2_order, channel, 0, cmpcod)	;1-3-17
;;;	xcall mkors (c2_order, channel)

	close 14

	incr oidx
	if (oidx .gt. maxidx) oidx = maxidx	;at least won't error
	oa(oidx) = c2_order
	op(oidx) = opono
	oj(oidx) = ojobno


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

	goto loop

newcus,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear bad_cust

	if (savcus .eq. -1) goto outcus
	close 14
	
;;;	n_subj = 'Sheet Metal Connectors, Inc - ORDER CONFIRMATION ' + %string(oa(1))

	if (oidx .gt. 1)
	then	n_subj = 'Sheet Metal Connectors, Inc - ORDER CONFIRMATIONS ' 
	else	n_subj = 'Sheet Metal Connectors, Inc - ORDER CONFIRMATION ' + %string(oa(1))

	clear n_toA
	t1 = 1

	if (savnbr .le. 0) goto mail_to_wood	;dbg
;--
;12-23-19: for cash customers need to use file misemc instead of emailc.

	read (chn044, ordhdr, savord) [err=no_order]
	goto order_ok
no_order,
	clear ordhdr
order_ok,
	clear emailc

	if (omicus .gt. 0)
	then	begin
		chnemc = chn103		;use misemc
		e_cust = omicus
		end
	else	begin
		chnemc = chn084		;use emailc
		e_cust = savcus
		end
;--

;;;	clear emailc
;;;	e_cust = savcus
	e_nbr = savnbr
	read (chnemc, emailc, e_key) [err=email_nf]
;;;	read (chn084, emailc, e_key) [err=email_nf]

	if (.not. %instr(1,e_mail,'@') )	;4-17-18
	then	begin
		call bad_email
		goto bad_mail			;not sent
		end
	else	n_toA = e_mail

;;;	if (.not. %instr(1,e_mail,'@') )	;5-20-13
;;;	then	call bad_email
;;;	else	n_toA = e_mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	goto not_wood

email_nf,
	clear coptbl
	e_mail = 'No Email Coptbl Record'
	goto bad_email

mail_to_wood,
	n_toa = 'orders@smcduct.com'	;debug
not_wood,

	n_body = '<html>'	;start html...
;-- part 1

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<b>*Please do not reply to this email as it has been automatically generated. </b><br><br>'

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + 'Contact your sales rep if you have any questions.<br><br>'

	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + '<br><br>Thank you for your recent order with Sheet Metal Connectors, Inc.  Below you will' + '<br>'

	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + 'find a list of orders placed today that are in process.  If you have any other questions ' + '<br>'

	tl = %trim(n_body)

	using cmpcod select
	('ROC'),	n_body = n_body(1,tl+1) + 'please call SMC-Rockford at 1-866-504-4600.'+ '<br><br><br><br>'
	(),	n_body = n_body(1,tl+1) + 'please call SMC at 1-800-328-1966.'+ '<br><br><br><br>'
	endusing
;-----------------

;-- part 2

	for oidx from 1 thru maxidx
		begin
		if (oa(oidx).eq. 0)
		then	exitloop
		else	begin
			tl = %trim (n_body)
			n_body = n_body(1,tl+1) + '<br><br>'
			tl = %trim(n_body)
			tl = tl + 2
			n_body(tl,tl+20)= '<b>SMC ORDER # </b>'

			tl = %trim(n_body)
			tl = tl + 2
			n_body(tl,tl+6)= oa(oidx), 'ZZZZZX'
			tl = %trim(n_body)
			tl = tl + 2
			n_body(tl,tl+8) =''

			if (op(oidx).ne.a10)
			  begin

			  tl = %trim(n_body)
			  tl = tl + 2
			  n_body(tl,tl+26)= '&nbsp &nbsp <b>  PO# </b>'

			  tl = %trim(n_body)
			  tl = tl + 2
			  n_body(tl,tl+10)= op(oidx)
			  end

			if (oj(oidx).ne.a10)
			  begin
			  tl = %trim(n_body)
			  tl = tl + 2
			  n_body(tl,tl+28)= '&nbsp &nbsp <b>  JOB# </b>'

			  tl = %trim(n_body)
			  tl = tl + 2
			  n_body(tl,tl+10)= oj(oidx)
			  end

			end
		end

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br><br>'

	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + 'Ordered by: '

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + E_FNAM		;from emailc 12-30-19
;;;	n_body = n_body(1,tl+1) + E2_FNAM

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + E_LNAM		;from emailc 12-30-19
;;;	n_body = n_body(1,tl+1) + E2_LNAM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 5-18-15: standardize signature...

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br><br><br><br>'

	tl = %trim (n_body)

	thnku = 'Thank you'
	name = 	slsnm			;10-24-18
	d_em = slseml			;11-21-18

	title = 
	d_ph = 	;;;> '763.572.0000'
	d_fax = '763.572.1100'
;;;	d_em = "sales@smcduct.com"		;11/23/16 ssq

	n_body = n_body(1,tl+1) + %sig(thnku, name, title, d_ph, d_fax, d_em)


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
;;;	n_body = n_body(1,tl+1) + 
;;;&		'<BR><IMG SRC="""\\serversbs\smc\wrk\smc_1.jpg"><br>'
;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '</html>'
	

;;;	n_toa = 'wood@smcduct.com'	;debug
	tl = %trim(n_toA)


        mailAtt = ^NULL
	clear status

	try
	begin
	        mailMsg = new MailMessage (%atrim(sender), %atrim(n_toA),%atrim(n_subj), %atrim(n_body))
	end

	catch (ex, @Exception)
		begin
		writes (splerr, "SMALOR new error:" + ex.Message)
		status = 1
		end

	endtry


	if (status) 
		begin
		call mail_error
		goto bad_mail
		end


	mailMsg = new MailMessage (%atrim(sender), %atrim(n_toA),%atrim(n_subj), %atrim(n_body))

	mailMsg.IsBodyHtml = 1

	myContentType = 'application/octet-stream'

	oidx = 0
att_loop,
	incr oidx
	if (oidx .gt. maxidx) goto att_done	;5-8-13

	attachment = attch_array(oidx)
	if (!attachment) goto att_done
	if (!%instr(1, attachment, 'pdf') ) goto att_done

	parse(attachment,,,,, fName, fExt,, filename)
	mailAtt = new Attachment(%atrim(filename), new ContentType(myContentType))
	mailAtt.Name = %atrim(fName) + %atrim(fExt)
	mailMsg.Attachments.Add(mailAtt)
	goto att_loop

att_done,
; 8-7-19 ssq: attach smc_shop_standards_2019.pdf
	
	if (has_L)
		begin
		attachment = stdpdf					;created at beg of pgm
		parse(attachment,,,,, fName, fExt,, filename)
		mailAtt = new Attachment(%atrim(filename), new ContentType(myContentType))
		mailAtt.Name = %atrim(fName) + %atrim(fExt)
		mailMsg.Attachments.Add(mailAtt)
		end
;
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
			if (err_cnt .le. 6) 		;changed to 6 on 12-28-16
				begin
				sleep 5			;changed to 5 on 12-29-16
				goto re_send
				end
			end
		if (%instr(1,ex.Message,"SMTP response 999") )
			begin
			incr err_cnt
			if (err_cnt .le. 6) 		;changed to 6 on 12-28-16
				begin
				sleep 1
				goto re_send
				end
			end


		writes (splerr, "SMALOR send error:" + ex.Message)
		dt = %datetime
		writes (splerr,dt)
		status = 1
		end

	endtry

;1-7-19 don't need this anymore...
;;;	clear err_rec
;;;	err_status = 'success'
;;;	err_name = ocusnm
;;;	err_ord = oordno
;;;	err_email = n_toa
;;;
;;;	clear err_msg
;;;	dt = %datetime
;;;	err_msg(1,4) = yyyy, 'XXXX'
;;;	err_msg(5,5) = '-'
;;;	err_msg(6,7) = mm, 'ZX'
;;;	err_msg(8,8) = '-'
;;;	err_msg(9,10) = dd, 'ZX'
;;;	err_msg(12,13) = hh, 'ZX'
;;;	err_msg(14,14) = ':'
;;;	err_msg(15,16) = min,	'ZX'
;;;
;;;	if (status) call mail_error
;;;	writes (chnerr, err_rec)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bad_mail,
	clear mailMsg, mailAtt


;	onerror inuse
	if (lens.gt.1) xcall delet (trans(1,lens) )
;;;	xcall delet (pdffil(1,lenp) )
	xcall delet (splfil)
inuse,
;	offerror

outcus,	
	savcus = c2_cust
	savnbr = c2_nbr
	savord = c2_order			;12-30-19
;;;	call open_spoolfile
	for oidx from 1 thru maxidx
		begin
		clear oa(oidx), op(oidx), oj(oidx)
		end	

;;;	for oidx from 1 thru 10	clear attch_array(oidx)
	for oidx from 1 thru maxidx	clear attch_array(oidx)
	clear oidx

	clear has_L
	return
;--------------------------------------------------


bad_email,
;;;	bad_cust = ec_cust		;4-17-18
	bad_cust = savcus		;4-17-18
	bad_nbr = savnbr		;4-17-18
	bad_mail = e_mail
	writes (splerr, badmsg)

	ns_order = oa(1)		;4-17-18
	writes (chnerr, not_send)	;4-17-18

	n_subj = 'No Email Address'

	return
;--------------------------------------------------
cust_mail_error,
	bad_cust =
	goto outcus
;--------------------------------------------------

open_spoolfile,
;;;	sp_cust = c2_cust
	sp_ord = c2_order
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

	err_status = 'failed'

	writes (splerr, err_rec)
	errors = 1
	return
;---------------------------------------------------------


eof,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-18-11 - empty files not marked as processed!

	if (savcus .ne. -1) call newcus

	if (chmail .gt. 0)
		begin
		close chmail
		OPEN (chmail,u,filen)
		ef_proc = 1
		write (chmail, emfile, reccnt)	;mark as processed
		end

;;;	if (savcus .ne. -1)
;;;		begin
;;;		call newcus
;;;		close chmail
;;;		OPEN (chmail,u,filen)
;;;		ef_proc = 1
;;;		write (chmail, emfile, reccnt)	;mark as processed
;;;		end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

endoff,

	call close
	close 44

	is_file = ef_file
	is_ext = 'ism'
	xcall delet (del_ism)

	is_ext = 'is1'
	xcall delet (del_ism)
fini,
	xreturn
;;;	xcall flags (7000000)
;;;	stop



OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILEC (CMPCOD, 1,'SI',001,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN001 = 1

;;;	XCALL FILEC (CMPCOD, 2,'I', 002, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN002 = 2

	XCALL FILEC (CMPCOD, 4,'SI',44,SWITCH)		;FILE # 44 -- ORDHDR FILE
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 4

	XCALL FILEC (CMPCOD, 5,'SI',45,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 5

	XCALL FILEC (CMPCOD, 6,'SI',175,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN175 = 6

	XCALL FILEC (CMPCOD, 76,'SI',075,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN075 = 76


	XCALL FILEC (CMPCOD, 8,'I',170, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN170 = 8

	XCALL FILEC (CMPCOD, 38, 'SI', 138, SWITCH)	;CCTRAN
	IF (SWITCH .EQ. 9) RETURN
	CHN138 = 38

;-------------------------------------------------------
; open the smc table for email address's
;
	XCALL FILEC (CMPCOD, 18,'SI',182,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 18

;;;	XCALL FILEC (CMPCOD, 84,'SI',084,SWITCH)
	XCALL FILEC ('SMC', 84,'SI',084,SWITCH)		;10-10-13
	chn084 = 84

	XCALL FILEC (CMPCOD, 103, 'SI', 103, SWITCH)	;103 - MISEMC
	CHN103 = 103
;-------------------------------------------------------

	XCALL FILEC (CMPCOD, 12, 'I', 54, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN054 = 12

	XCALL FILEC (CMPCOD, 13, 'SI', 139, SWITCH)	;TMPCUS
	IF (SWITCH .EQ. 9) RETURN
	CHN139 = 13

	ef2 = cmpcod


	open (20, A, errfl2)
	chnerr = 20

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

	IF (CHN068) CLOSE CHN068
	IF (CHN075) CLOSE CHN075
	IF (CHN054) CLOSE CHN054
	IF (CHN170) CLOSE CHN170
	IF (CHN182) CLOSE CHN182
	IF (CHN138) CLOSE CHN138
	IF (CHN139) CLOSE CHN139

	if (chn084) close chn084
	IF (CHN103) CLOSE CHN103

	if (chnerr) close chnerr
	if (chmail) close chmail
	if (splerr) close splerr	;10-08-13
	if (errors) 
		begin
	;;;	close splerr
		lpque (errfil)
		end

	RETURN
;-----------------------------------------------
catch_all,
;;;	offeror
	xreturn

get_L,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (has_L) return	;if any  of the orders has dept "L"...

	find (chn045, ordlin, oordno) [err=gl_loop]
gl_loop,
	xcall ios (chn045, ordlin, read, lokctl)
	if (lokctl .ne. 0) goto gl_eof
	if (lordno .ne. oordno) goto gl_eof
	if (ltype .eq. 'M') goto gl_loop
	if (ldept .ne. 'L') goto gl_loop

	has_L = 1
gl_eof,

	return
;-----------------------------------------------

end
;-----------------------------------------------

