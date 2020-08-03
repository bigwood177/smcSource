subroutine inmal

	from	,a	;from
	toN	,a	;to: name
	toA	,a	;to: email address
	subj	,a	;subject
	body	,a	;body
	img	,a	;path to smc logo
	sig	,a	;Dianna's signature
	att1	,a	;attachment 3
	sendOK	,d	;1= email got sent
;
; 10-11-12: this now works for either XP or Win7
;
; inmail.xmail
;
; VB source for active-X control is in:
;	\\Server01\\SMC\source\vb\cpmail
;
;	axutl.exe will load and test control

;	inmail.ocx is the control created, reference it by project.UserControl;
;		ie: inmail.xmail
;


	.INCLUDE 'DBLDIR:ACTIVEX.DEF'
	.INCLUDE "WND:TOOLS.DEF"

.define		AX_HEIGHT,	450
.define		AX_WIDTH,	430


record	vars
	a1,	a1
;;;	sendok	,d18
	tox	,a400
	blanks	,a3
	C_CODE	,A3		;COMPANY CODE
	B_ERR	,D1
	CP_INFO	,D1
	A_NODATE	,I4
	A_RESET		,I4
	J	,i4
	DECMAL	,D18
	name	,a25
	primary	,d1
	email	,a80
	send	,d1	;1=send, 2=display
	a2	,a2
	i	,d6
	attch	,10a100
	axwid	,i4
	axid	,i4
	Windows7,	d2,	22
	system	,d2
	machine	,d3

.proc

	xcall envrn (system, machine)

	send = 1	;send mail

; initialize all attachemnts..
;;;	for i from 1 thru 3 attch(i) = '~'

;;;	if (att1 .ne. blanks) attch(1) = att1
;;;	if (att2 .ne. blanks) attch(2) = att2
;;;	if (att3 .ne. blanks) attch(3) = att3
	

	if (system .eq. Windows7)
		begin
		xcall in_mail7 (
		xreturn
		end

; just 1 attachment actually will be used...

; Create container and load control...
	axwid = %ax_create(50, 50, AX_WIDTH, AX_HEIGHT)
	axid = %ax_load(axwid, "ck_mail.mail", 0, 0, AX_WIDTH, AX_HEIGHT)

; Process control...

	xcall ax_call(axid, "mail", from, toN,toA, subj, body, img, sig,
&		att1, sendok )

;;;	if (.not. sendok)
;;;	then	display (15,$scr_pos(10,1),'did not go')
;;;	else	display (15,$scr_pos(11,1),'mail got sent')
;;;	reads (15,a1)

	xcall win_stop(1)

	xreturn
.end



