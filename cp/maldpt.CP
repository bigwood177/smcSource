import SynPSG.System
import SynPSG.System.IO
import SynPSG.System.Net
import SynPSG.System.Net.Mail
import SynPSG.System.Net.Mime
import SynPSG.System.Net.Sockets
import SynPSG.System.Security




subroutine maldpt
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


; must run mset.bat for following import statements to work...



record
        mailClient     ,@SMTPClient
        mailMsg        ,@MailMessage
        mailAtt        ,@Attachment
	myContentType	,string
        attachment  ,a80        ;Attachment name
	filename    ,a17       ;attachment filename
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
	server	,a*,	'192.168.1.203'
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
;

record	lasdpt
	.include 'def:rd161a.def'

record	ordhdr
	.include 'def:rd044a.def'

record	blhead
	.include 'def:rd178a.def'

record	coptbl
	.include 'def:rd182a.def'


record	willcall
		,a*,	'Your PO# '
	wc_po	,a10
		,a*, ', Job# '
	wc_job	,a10
		,a*,	' SMC# '
	wc_ord	,a6
		,a*,	' , with SMC is complete.  You may pick up your order between the hours of 7:00am - 4:30pm, Monday - Friday.'

record	OurTruck
		,a*,	'Your PO# '
	ot_po	,a10
		,a*, ', Job# '
	ot_job	,a10
		,a*,	' SMC# '
	ot_ord	,a6
		,a*,	' , with SMC is complete.  We will contact you to schedule delivery.'

record	CommonCarrier
		,a*,	'Your PO# '
	cc_po	,a10
		,a*, ', Job# '
	cc_job	,a10
		,a*,	' SMC# '
	cc_ord	,a6
		,a*,	' , with SMC is complete and being packaged for shipment.  We will contact you with shipping details via email or phone call.'

record	SpeedeeUps
		,a*,	'Your PO# '
	su_po	,a10
		,a*, ', Job# '
	su_job	,a10
		,a*,	' SMC# '
	su_ord	,a6
		,a*,	' , with SMC is complete and will ship within the next business day.'


record	varfs
	dt	,a20
;
record,x
	cc	,d2
	yy	,d2
	mm	,d2
	dd	,d2
	hh	,d2
	min	,d2

record
	xdate	,d8
record,x
	xcc	,d2
	xyy	,d2
	xmm	,d2
	xdd	,d2

record
	xtime	,d4
record,x
	xhh	,d2
	xmin	,d2
record
	dt_min	,d4
	x_min	,d4


record	channel
	chn044	,d2
	chn161	,d2
	chn182	,d2
	chn178	,d2

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

		
	
record	varsxy
	i	,d6
	x	,d18
	y	,d18
	adate	,a8
	days	,d6
	mon	,12d3,	31,28,31,30,31,30,31,31,30,31,30,31

record	vars
	opnok	,d1
	cnt	,d6	;record count
	errfil	,a14,	'spl:maldpt.err'
	trans	,a100
	pline	,a88
	lenp	,d6
	lens	,d6
	spllen	,d6
	tl	,d6
	errors	,d1
	status	,i4
	err_cnt	,d6
	splerr	,d2
	read	,d1,0
	write	,d1,1
	lokctl	,d1
	switch	,d1
	v	,d1
.proc
	xcall getlog ('spl', trans, spllen)
	clear errors


	filename = 'spl:completed.txt'
	open (14,o,filename)

    ;; new stuff for credentials...
    mycredents = new SynPSG.system.NetworkCredential()
    mycredents.Username="orders" 
    mycredents.Password="$MCod5900"   ;5-16-16
    mycredents.Domain = "smcduct.com"
    
    ;; put default values here
    port = 25
    attachment = ''

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

	pline (1,6) = 'Order#'
	pline (9,34) = 'Customer'
	pline (36,51) = 'Ship Via'
	pline (53,88) = 'Contact Info'
	writes (14, pline)
	clear pline
	cnt = 0
loop,
	reads (chn161, lasdpt, eof)
	if (ld_flag .eq. 9) goto loop	;already printed

;record must in file for at least 5 minutes...
	xtime = ld_time
	x_min = xhh*60 + xmin

	dt = %datetime
	dt_min = hh*60 + min
	if (dt_min-x_min .lt. 5) goto loop

	xcall isio (chn178, blhead, ld_ord, read, lokctl)
	if (lokctl.eq.0 .and. bhshpd.gt.0) goto loop				;already shipped


	xcall isio (chn044, ordhdr, ld_ord, read, lokctl)
	if (lokctl .ne. 0) goto mark9				;11-20-19
;;;	if (lokctl .ne. 0) clear ordhdr
	clear coptbl
	tblcod = 'SC'
	sc_scac = oscac
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .ne. 0) clear coptbl

	pline(1,6) = ld_ord, 'ZZZZZX'
	pline(9,34) = ld_cus
	pline(36,51) = sc_name
	pline(53,88) = ld_cont
	writes (14, pline)
	
	clear pline
	writes (14, pline)

	using oscac select
	('0','4','7','8','9'),	call speedeeUPS
	('1','10','WC'),	call willcall
	('2','5'),		call OurTruck
	(),			call CommonCarrier
	endusing


	incr cnt
mark9,
	ld_flag = 9
	write (chn161, lasdpt, ld_ord)
	goto loop


speedeeUPS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	su_po = opono
	su_job = ojobno
	su_ord = oordno, 'ZZZZZX'
	writes (14, speedeeUPS)
	writes (14, '_____________________________')
	writes (14, pline)

	return
;--------------------------------------------------
willcall,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	wc_po = opono
	wc_job = ojobno
	wc_ord = oordno, 'ZZZZZX'
	writes (14, willcall)
	writes (14, '_____________________________')
	writes (14, pline)

	return
;--------------------------------------------------

OurTruck,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ot_po = opono
	ot_job = ojobno
	ot_ord = oordno, 'ZZZZZX'
	writes (14, OurTruck)
	writes (14, '_____________________________')
	writes (14, pline)

	return
;--------------------------------------------------

CommonCarrier,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	cc_po = opono
	cc_job = ojobno
	cc_ord = oordno, 'ZZZZZX'
	writes (14, CommonCarrier)
	writes (14, '_____________________________')
	writes (14, pline)

	return
;--------------------------------------------------

eof,
	close 14
	if (cnt .le. 0) goto endoff


	n_subj = 'Last Dept Completed Orders'

	clear n_toA

;;;	n_toa = 'wood@smcduct.com'	;debug
	n_toa = 'Charletted@smcduct.com'	;debug

	n_body = 'Last Department Completed Batch Report - See attached'

	tl = %trim(n_toA)


        mailAtt = ^NULL
	clear status

	try
	begin
	        mailMsg = new MailMessage (%atrim(sender), %atrim(n_toA),%atrim(n_subj), %atrim(n_body))
	end

	catch (ex, @Exception)
		begin
		writes (splerr, "MALDPT new error:" + ex.Message)
		status = 1
		end

	endtry


	if (status) 
		begin
		call mail_error
		goto bad_mail
		end


	mailMsg = new MailMessage (%atrim(sender), %atrim(n_toA),%atrim(n_subj), %atrim(n_body))

	mailAtt = new Attachment(%atrim(filename))
	mailAtt.Name = filename
	mailMsg.Attachments.Add(mailAtt)


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


		writes (splerr, "MALDPT send error:" + ex.Message)
		status = 1
		end

	endtry

	goto endoff
;
bad_mail,
	clear mailMsg, mailAtt

	lens = %trim(trans)
	if (lens.gt.1) xcall delet (trans(1,lens) )
	


mail_error,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	err_status = 'failed'

	writes (splerr, err_rec)
	errors = 1
	return
;---------------------------------------------------------

endoff,

;;;	xcall delet (filename)

	find (chn161, lasdpt, ^first) [err=dloop]
dloop,
	reads (chn161, lasdpt, d_eof)
	if (ld_flag .eq. 9) call days		;how old is record?
;;;	if (ld_flag .eq. 9) delete (chn161)
	goto dloop
d_eof,

fini,
	xreturn

days,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; leave records in file for 7 days so printing 
		; cover sheet or just going in and out of orders
		; don't trigger store to lasdpt.ism
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	xdate = ld_date					;record date
	dt = %datetime					;today's date

	x = dd							;days
	if (mm .gt. 1)
		for i from 1 thru mm-1 x = x + mon(i)		;days in prev months
	x = x + yy*365						;days in prev years

	y = xdd							;days
	if (xmm .gt. 1)
		for i from 1 thru xmm-1 y = y + mon(i)
	y = y + xyy*365

	days = x-y							;how old is record?
	if (days .gt. 7) delete (chn161)	;delete after 7 days
	return
;--------------------------------------------------------


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILEC (CMPCOD, 1,'SU',161,SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN161 = 1

	SWITCH = 5
	XCALL FILEC (CMPCOD, 44, 'SI', 044, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 44

	SWITCH = 5
	XCALL FILEC (CMPCOD, 17, 'SI', 182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 17
	
	SWITCH = 5
	XCALL FILEC (CMPCOD, 28, 'SI', 178, SWITCH)	;178 - BLHEAD
	IF (SWITCH .EQ. 9) RETURN
	CHN178 = 28

	open (21, O, errfil)
	splerr = 21

	OPNOK = 1
	RETURN
;-----------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (CHN161) CLOSE CHN161
	IF (CHN044) CLOSE CHN044
	IF (CHN182) CLOSE CHN182
	IF (CHN178) CLOSE CHN178

	if (splerr) close splerr	;10-08-13
	if (errors) 
		begin
		lpque (errfil)
		end

	RETURN
;-----------------------------------------------
catch_all,
;;;	offeror
	xreturn
end
;-----------------------------------------------

