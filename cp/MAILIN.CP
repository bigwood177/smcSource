;mailin.cp
; 12-20-12: using smtp functions from SynPsgSystem.elb
;  mset.bat sets up required logicals.
;
; 6-6-18 SSQ: MAKE CUSMAS ISAM
;12-16-19 ssq: cash_cust


; 12-10-12 ssq: new stuff
;
; must run mset.bat for following import statements to work...
import SynPSG.System
import SynPSG.System.IO
import SynPSG.System.Net
import SynPSG.System.Net.Mail
import SynPSG.System.Net.Mime
import SynPSG.System.Net.Sockets
import SynPSG.System.Security

external function
	sig	,a

record	smtp_defines
;;;	server	,a*,	'192.168.1.13'
	server	,a*,	'192.168.1.4'
	domain	,a*,	'smcduct.com'
	sender	,a*,	'AccountsReceivable@SMCduct.com'
	senderName,	a*,	'Sheet Metal Connectors'

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

;mail invoices
;
	.include 'def:hpsub.def'

RECORD	EMFILE
	EF_FILE	,A17
	EF_PROC	,D1		;1 = PROCESSED

	
record	ordhdr
	.include 'def:rd044a.def'
record	email
	.include 'def:cemail.def'

record	emailc
	.include 'def:rd084a.def'


record	coptbl
	.include 'def:rd182a.def'
;
record	badmsg
	bad_cust	,d6
			,a*,	' has a bad email address!!'

record	cuserr
	cer_cus	,d6
	cer_nam	,a15

record	channel
	chn001	,d2
	chn002	,d2
	chn041	,d2	;added 10-17-18
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
	chn084	,d2	;miscus

record	splfil	
		,a4,	'spl:'
	sp_cust	,d6
		,a4,	'.spl'

record	new
	n_from	,a80	;from
	n_toN	,a40
	n_toA	,a500
	n_cc	,10a100
	n_subj	,a30	;subject
	n_body	,a1000	;body
;;;	n_body	,a600	;body
	n_img	,a100	;path to image
	n_sig	,a600	;Dianna's signature
	n_att1	,a100	;attachment 1
	n_att2	,a9	;attachment 2
	n_att3	,a9	;attachment 3
	n_att4	,a9	;attachment 4

record	ordara
	oa	,200d6	;invoice #'s for current cust.
	oidx	,d6
	maxidx	,d3,	200

record	salman
	thnku	,a20
	name 	,a20
	title 	,a20
	d_ph	,a15
	d_fax 	,a15
	d_em 	,a30

record	err_mesag
	ems_cus	,d6
		,a1
	ems_desc	,a30

record	
	sav_cc	,d1
	savcus	,d6
record,x
	savkey	,d7

record	vars
	savord	,d6
	is_err	,d1
	bad_mail	,a30
	cc_cnt	,d3
	i	,d6
	status	,d1
;;	errFlag	,@int	;1 = mail not sent
	system	,d2
	machine	,d3
	windows7,	d2, 22
	q	,a1	;quote
	clctl	,d1
	redfil	,a14
	cr	,a1
	lf	,a1
	filen	,a14
	reccnt	,d6
	sentOK	,d18
	cmpcod	,a3
	a1	,a1
	opnok	,d1
	chmail	,d2
	t1	,d6
	t2	,d6
	tlen	,d3,	500
	pline	,a132
	trans	,a100
	pdffil	,a100
	dprim	,d1	;1= primary
	tl	,d6
	nbl	,d3, 600		;length of n_body	
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
;;; 2-26-18 ssq>	xcall envrn (system, machine)

	xcall who (cmpcod)
	using cmpcod select
	('SMC'),	bad_mail = 'accountsReceivable@smcduct.com'
	('ROC'),	bad_mail = 'jessier@smcduct.com'
	('TST'),	bad_mail = 'wood@smcduct.com'
	('CAT'),	bad_mail = 'accountsReceivable@smcduct.com'
;;;	('CAT'),	nop
	(),	goto fini	;skip unless smc
	endusing

	call opens
	if (.not. opnok) goto endoff


    ;; new stuff for credentials...
    mycredents = new SynPSG.system.NetworkCredential()


    mycredents.Username="accountsreceivable"
    mycredents.Password="$MCar5900"

    mycredents.Domain = "smcduct.com"
    
    ;; put default values here
    port = 25
    attachment = ''

	n_from = 'AccountsReceivable@SMCduct.com'

	mailClient = new SMTPClient(server, port)

	savcus = -1
	clear is_err				;3-13-17

; mailin needs name of email file...
	filen(1,3) = cmpcod
	filen(4,4) = ':'
	filen(5,14) = 'emfile.dat'
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
	if (ce_key(1,7) .ne. savkey) call newcus		;12-16-19
;;;	if (ce_cust .ne. savcus) call newcus
	xcall mkins (ce_order, channel, ce_date)
	incr oidx
	oa(oidx) = ce_order

	goto loop

newcus,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	if (savkey .eq. -1) goto outcus	
	if (savcus .eq. -1) goto outcus
	close 14

	clear n_toA
	for i from 1 thru 10 clear n_cc(i)	;2-25-15
	cc_cnt = 0

	n_subj = 'Invoices from SMC attached...'	;moved here so can be overriden by bad email
	t1 = 1
; 12-16-19 use contact email for cash customers
	if (sav_cc)
	then	call cash_cust
	else	call not_cash_cust

;;;> 6-17-19	n_subj = 'Invoices from SMC attached...'

	if (.not. %instr(1,n_toa,'@') ) call bad_email
	t1 = %trim(n_toA)
	if (t1.lt.5) call bad_email

	xcall getlog ('spl', trans, len)

	trans (len+1, len+11) = '\' + splfil(5,14)
	pdffil = trans
	len = %trim (pdffil)
	len = len - 2
	pdffil(len, len+2) = 'pdf'

	lens = %trim(trans)
	lenp = %trim(pdffil)
	XCALL MKPDF (trans(1,lens), PDFFIL(1,lenp) )

	call make_html			;4-28-15
	
	tl = %trim(n_toA)

        mailAtt = ^NULL

	parse(pdffil,,,,, fName, fExt,, filename)
	myContentType = 'application/octet-stream'
	mailAtt = new Attachment(%atrim(filename), new ContentType(myContentType))
	mailAtt.Name = %atrim(fName) + %atrim(fExt)

	mailMsg = new MailMessage(%atrim(sender), %atrim(n_toA),%atrim(n_subj), %atrim(n_body))

bad_msg,
;3-13-17 ssq: add all these "mail" statements inside the "try" block so any errors can be trapped...
	clear status
	try	
		begin				;3-13-17

		for i from 1 thru cc_cnt
			begin
			mailMsg.CC.Add(%atrim(n_cc(i)))
			end

		mailMsg.IsBodyHtml = 1


;;;		parse(pdffil,,,,, fName, fExt,, filename)
;;;		mailAtt = new Attachment(%atrim(filename), new ContentType(myContentType))
;;;		mailAtt.Name = %atrim(fName) + %atrim(fExt)
		mailMsg.Attachments.Add(mailAtt)

		mailClient.Send (mailMsg, myCredents)
		end

	catch (ex, @Exception)
		begin
		writes (chnerr, "mailit error:" + ex.Message)
		status = 1
		end

	endtry


	if (status) call not_sent

	clear mailMsg, mailAtt


	xcall delet (trans(1,lens) )
	xcall delet (pdffil(1,lenp) )
	xcall delet (splfil)


outcus,	
	savcus = ce_cust
	sav_CC = ce_cc
	savord = ce_order

	call open_spoolfile
	for oidx from 1 thru maxidx	clear oa(oidx)
	
	clear oidx
	return
;--------------------------------------------------
cash_cust,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; use contact email for cash customes
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	XCALL ISIO (4,ORDHDR,savord,READ,LOKCTL)
	unlock 4

	clear emailc
	e_cust = savcus
	e_nbr = oc_nbr

	xcall isio (chn084, emailc, e_key, read, lokctl)
	if (lokctl .ne. 0) clear emailc

	if (.not. %instr(1,e_mail,'@') )
	then	call bad_email
	else	n_toA(t1,tlen) = e_mail
	
	return
;------------------------------------------------------------------------


not_cash_cust,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; look up email in coptbl for regular customes
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear tbl_key
	tblcod= 'EC'
	EC_cust = savcus
	EC_PRIM = 1					;primary email
	find (chn182, coptbl, tbl_key) [err=e_loop]
e_loop,
	reads (chn182, coptbl, e_eof)
	if (tblcod .ne. 'EC') goto e_eof		;3-13-17
	if (ec_cust .ne. savcus) goto e_eof
	if (.not. %instr(1,ec_email,'@') )
	then	call bad_email
	else	n_toA(t1,tlen) = ec_email


e_loop2,
	reads (chn182, coptbl, e_eof)
	if (tblcod .ne. 'EC') goto e_eof		;3-13-17
	if (ec_cust .ne. savcus) goto e_eof
	if (.not. %instr(1,ec_email,'@') )
	then	call bad_email
	else	begin
		if (cc_cnt.lt.10)
		  begin
		  incr cc_cnt
		  n_cc(cc_cnt) = ec_email
		  end
		end

	goto e_loop2

e_eof,
	return
;--------------------------------------------------------------


make_html,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; html code for body of invoice
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	n_body = '<html>'
	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + 'We appreciate your business! ' + '<br><br>'

	tl = %trim(n_body)
	n_body = n_body(1,tl+1) + 'Attached are invoice(s): ' + '<br>'
	for oidx from 1 thru maxidx
		begin
		if (oa(oidx).eq. 0)
		then	exitloop
		else	begin
			tl = %trim(n_body)
			tl = tl + 2
			n_body(tl,tl+6)= oa(oidx), 'ZZZZZX'
			end
		end
	tl = %trim (n_body)


	n_body = n_body(1,tl+1) + '<p class=MsoNormal><b><span style='
	tl = %trim (n_body)


	thnku = 'Thank you'
	name = 'Chris Vang'
	title = 'Accounts Receivable'
;;;	d_ph = '763.123.1234'
;;;	d_fax = '763.456.4567'
	d_em = 'accountsreceivable@smcduct.com'

	n_body = n_body(1,tl+1) + %sig(thnku, name, title, d_ph, d_fax, d_em)

	return
;------------------------------------------------------------

; this is old code ...
; the "return above prevents it from executing.

	n_body = n_body(1,tl+1) + "''" + 'font-family:'

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + ' "Arial" , "sans-serif";color:#548DD4'

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + "'>"

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br><br><br><br>Chris Vang'
;;;	n_body = n_body(1,tl+1) + '<br><br><br><br>Dianna Hanson'


	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br>Accounts Receivable'
;;;	n_body = n_body(1,tl+1) + '<br>Credit Manager'


	tl = %trim (n_body)

	n_body = n_body(1,tl+1) + 
&		'<BR><IMG SRC="""\\serversbs\smc\wrk\smcL.bmp"><br>'
	
	
	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br>5850 Main Street N.E.'

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br>Minneapolis, MN 55432-5439'

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br>Direct Phone: (763) 586-7171'

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br>Phone: (763) 572-0000'
	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br>Fax: (763) 572-1100'
	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<br>www.smcduct.com'

	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '<o:p></o:p></span></b></p> '


	tl = %trim (n_body)
	n_body = n_body(1,tl+1) + '</html>'
	return
;--------------------------------------------------

bad_email,
;;;	bad_cust = ec_cust
	bad_cust = savcus
	xcall mesag (badmsg,1)
	n_toa = bad_mail		;jess or dianna
	n_subj = 'Invoice not sent - bad email address!!'
	return
;--------------------------------------------------

open_spoolfile,
	sp_cust = ce_cust
	open (14,o,splfil)
	return
;--------------------------------------------------
not_sent,
	ems_cus = savcus
	ems_desc = "invoice not sent"

	writes (chnerr, err_mesag)
	is_err = 1
	return
;--------------------------------------------------

eof,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2-18-11 - empty files not marked as processed!

	if (savcus .ne. -1) call newcus
;;;	if (savkey .ne. -1) call newcus

	if (chmail .gt. 0)
		begin
		close chmail
		OPEN (chmail,u,filen)
		ef_proc = 1
		write (chmail, emfile, reccnt)	;mark as processed
		end


endoff,
RBLIDX,
	XCALL OUTPT (2,1,1,'REBUILD ALPHA BILLING INDEX',1)

	
	XCALL FFILE (44, REDFIL, CLCTL)
	REDFIL(5,14) = 'INVOIC.WRK'
	OPEN (7, O, REDFIL)		;SSQ 6-25-99


	close chn044				;9-28-11
	switch = 5				;9-28-11
	xcall files (4, 'SI', 044, switch)	;9-28-11

	ORDHDR =
	LOKCTL = 1
	XCALL IOS (7,ORDHDR,WRITE,LOKCTL)


	read (chn044, ordhdr, ^first) [err=rblp2]	;9-28-11

;;;	LOKCTL = 1
;;;	XCALL ISIO (4,ORDHDR,OORDNO,READ,LOKCTL)
RBLP2,
	IF (OFLAG.GE.2)
	BEGIN
	  LOKCTL = 1
	  XCALL IOS (7,ORDHDR,WRITE,LOKCTL)
	END
	LOKCTL = 1
	XCALL IOS (4,ORDHDR,READ,LOKCTL)
	IF (LOKCTL.EQ.0) GOTO RBLP2

	ORDHDR = ']]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]'
	LOKCTL = 1
	XCALL IOS (7,ORDHDR,WRITE,LOKCTL)
	CLOSE 7
	XCALL OUTPT (2,1,0,'SORT    ALPHA BILLING INDEX',1)
	SORT (INPUT=REDFIL,RECORD=ORDHDR,KEY=(OCUSNO,OORDNO))
	XCALL OUTPT (0,0,0,' ... COMPLETED',1)

	call close
fini,
	if (is_err) xcall spedit ('smc:mailerr.dat')
	xcall pgchn ('cp:pstrin',1)
	stop



OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	
	XCALL FILES (1, 'SI', 041, SWITCH)		;10-17-18 FOR SBT
	CHN041 = 1
	XCALL FILES (2,'SI',001,SWITCH)			;10-17-18 changed from 1 to 2
	IF (SWITCH .EQ. 9) RETURN
	CHN001 = 2

;;;	XCALL FILES (2,'I', 002, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN002 = 2

	XCALL FILES (4,'SU',44,SWITCH)		;FILE # 44 -- ORDHDR FILE
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 4

	XCALL FILES (5,'SU',45,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 5

	XCALL FILES (6,'SU',175,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN175 = 6

	XCALL FILES (76,'SI',075,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN075 = 76


	XCALL FILES (8,'I',170, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN170 = 8

;-------------------------------------------------------
; open the smc table for email address's
;
;;;	XCALL FILES (18,'SI',182,SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN
;;;	CHN182 = 18

	XCALL FFILC ('SMC',182, REDFIL, CLCTL)
	REDFIL(14,14) = 'M'
	OPEN (17,SI,REDFIL)			;changed from 18 to 17 10-17-18
	CHN182 = 17
;-------------------------------------------------------

	XCALL FILES (12, 'I', 54, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN054 = 12

	XCALL FILES (13, 'SI', 139, SWITCH)	;TMPCUS
	IF (SWITCH .EQ. 9) RETURN
	CHN139 = 13

	switch = 5
;;;	xcall filec ('SMC', 84, 'SU', 103, switch)	;103 - misemc 9-16-19
	xcall files (84, 'SU', 103, switch)		;103 - misemc each company has it's own file!
	if (switch .eq. 9) return
	chn084 = 84

;;;	open (20, O, 'smc:mailerr.dat')
	open (20, O, 'spl:mailerr.dat')		;3-13-17 ssq
	chnerr = 20

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
	IF (CHN139) CLOSE CHN139
	IF (CHN084) CLOSE CHN084	;12-16-19
	if (chnerr) close chnerr
	if (chmail) close chmail
	RETURN
;-----------------------------------------------
