;mhtml.dbl
; see mhtml.bat - it creates "m7_mail7.inc"
; note also that you must have an environmental variable "dllp"
; pointing to "c:\smc\dll" which must in turn contain "m7_mail7.dll".
;

;;;.include 'ck_mail7.inc'
.include 'm7_html.inc'

subroutine mhtml
	errFlag	,@int
	from	,a	
	recpt	,a
	subj	,a
	body	,a
	pat1	,a
	pat2	,a
	pat3	,a
	pat4	,a
	pat5	,a
	pat6	,a
	pat7	,a

record
	obj,	@m7_html.mail
;;;	obj,	@ck_mail7.mail

record	attahcemnts
	at1	,a100
	at2	,a100
	at3	,a100
	at4	,a100
	at5	,a100
	at6	,a100
	at7	,a100

record	vars
	a2	,a2
	tmpFlag	,@int
;
proc

	set at1,at2,at3,at4,at5,at6,at7 = '~'

	if (%passed(pat1) ) at1 = pat1
	if (%passed(pat2) ) at2 = pat2
	if (%passed(pat3) ) at3 = pat3
	if (%passed(pat4) ) at4 = pat4
	if (%passed(pat5) ) at5 = pat5
	if (%passed(pat6) ) at6 = pat6
	if (%passed(pat7) ) at7 = pat7

	obj = new Mail()
	obj.gAttach (at1(1,%trim(at1)),
&			at2(1,%trim(at2)),
&			at3(1,%trim(at3)),
&			at4(1,%trim(at4)),
&			at5(1,%trim(at5)),
&			at6(1,%trim(at6)),
&			at7(1,%trim(at7)) )


;;;	tmpFlag = 0

	obj.test(tmpflag)

	errFlag = tmpFlag

	obj.sndMail (from, recpt, subj, body, tmpFlag)

	errFlag = tmpFlag

	xreturn
end
	

