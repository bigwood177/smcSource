subroutine ntmal

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
; inmail.xmail
;
; VB source for active-X control is in:
;	\\Server01\\SMC\source\vb\cpmail
;
;	axutl.exe will load and test control



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

.proc
	send = 1	;send mail

	
; just 1 attachment actually will be used...

; Create container and load control...
	axwid = %ax_create(50, 50, AX_WIDTH, AX_HEIGHT)
	axid = %ax_load(axwid, "ck_night.mail", 0, 0, AX_WIDTH, AX_HEIGHT)

; Process control...

	xcall ax_call(axid, "mail", from, toN,toA, subj, body, img, sig,
&		att1, sendok )


	xcall win_stop(1)

	xreturn
.end




