;cpmal.cp
;cop version of xmail.man
;xmail.man

subroutine	cpmal
	recpt	,a	;recipient
	subj	,a	;subject
	body	,a	;body
	att1	,a	;attachment 1
	att2	,a	;attachment 2
	att3	,a	;attachment 3
	att4	,a	;attachment 4
	att5	,a	;attachment 5
	att6	,a	;attachment 6
	sndflg	,d	;optional, 1=send, 2=display

; mail.xmail
;
; VB source for active-X control is in:
;	\\Server01\\SMC\source\vb\cpmail
;
;	axutl.exe will load and test control

;	cpmail.ocx is the control created, reference it by project.UserControl;
;		ie: cpmail.xmail
;

.INCLUDE 'DBLDIR:ACTIVEX.DEF'

.define		AX_HEIGHT,	450
.define		AX_WIDTH,	430


record	vars
	send	,d1	;1=send, 2=display
	a2	,a2
	i	,d6
	attch	,10a100
	axwid	,i4
	axid	,i4

.proc
	send = 1	;send mail
	if (%passed(sndflg)) send = sndflg


; initialize all attachemnts..
;;;	for i from 1 thru 10 if (attch(i) .eq. a2) attch(i) = '~'
	for i from 1 thru 10 attch(i) = '~'

	if (%passed(att1)) attch(1) = att1
	if (%passed(att2)) attch(2) = att2
	if (%passed(att3)) attch(3) = att3
	if (%passed(att4)) attch(4) = att4
	if (%passed(att5)) attch(5) = att5
	if (%passed(att6)) attch(6) = att6

; Create container and load control...
	axwid = %ax_create(50, 50, AX_WIDTH, AX_HEIGHT)
;;;	axid = %ax_load(axwid, "xeml.xmail", 0, 0, AX_WIDTH, AX_HEIGHT)
	axid = %ax_load(axwid, "cpmail.xmail", 0, 0, AX_WIDTH, AX_HEIGHT)

; Process control...

	using send select
	(1),	xcall ax_call(axid, "mail", recpt, subj, body,
&		attch(1),attch(2),attch(3),attch(4),attch(5),attch(6) )

	(2),	xcall ax_call(axid, "maild", recpt, subj, body,
&		attch(1),attch(2),attch(3),attch(4),attch(5),attch(6) )
	endusing

	xcall win_stop(1)
	xreturn
.end




