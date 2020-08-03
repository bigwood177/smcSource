subroutine	sndem
	slsman	,d		;salesman
	splfil	,a		;spool file (optional)

;SndEmail.dbl
;
; VB source for active-X control is in:
;	\\Server01\Wood\SMC\vb_source\email
;
;	xemail.ocx is the control created.  It must be either
;	copied (win95) or registered(win98) in the users
;	windows\system directory.
;

.INCLUDE 'DBLDIR:ACTIVEX.DEF'

.define		AX_HEIGHT,	450
.define		AX_WIDTH,	430

record	vars
	axwid	,i4
	axid	,i4
	mladdr	,4a30		;e-mail address's
	rtffil	,a50		;path of attachment

.proc

	using slsman select
	(01),	begin
		rtffil = 'c:\smc\spool\leon.rtf'
		mladdr(1) = "LeonS@smcduct.com"
		end
	(03),	begin
		rtffil = 'c:\smc\spool\lou.rtf'
		mladdr(1) = "Manykey3514@aol.com"
		end
	(04),	begin
		rtffil = 'c:\smc\spool\don.rtf'
	;;;	mladdr(1) = "BadgerDon@aol.com"
		mladdr(1) = "BadgersDen@charter.net"
		end
	endusing

	if (%passed(splfil)) rtffil = splfil

;;;	rtffil = 'c:\smc\spool\catrpt.doc'
; Test values...
;debug:
;;;	mladdr(1) = "wood@smcduct.com"
;;;	mladdr(2) = "diannah@smcduct.com"

; Create container and load control...
	axwid = %ax_create(50, 50, AX_WIDTH, AX_HEIGHT)
	axid = %ax_load(axwid, "xemail.xmail", 0, 0, AX_WIDTH, AX_HEIGHT)

; Bind events...
	xcall ax_bind(axid, "cmdMail", "cmdMail")

; Process control...
	xcall process_control 
&	(axwid, axid, rtffil, mladdr(1), mladdr(2), mladdr(3), mladdr(4))

	xcall win_stop(1)
	xreturn
.end

.subroutine process_control
	a_axwid		,n	;id of container
	a_axid		,n	;id of control
	rtffil		,a	;path of attachment
	ema1		,a	;e-mail address 1
	ema2		,a	;e-mail address 2
	ema3		,a	;e-mail address 3
	ema4		,a	;e-mail address 4
.proc
	xcall ax_show(a_axwid)
	xcall ax_call(a_axid, "GetPassedData", rtffil, ema1, ema2, ema3, ema4)
;;;	xcall ax_call(a_axid, "PushCB1")

;;;	xcall win_proc(1)
;;;	xcall win_proc(0)

	xcall ax_hide(a_axwid)

	xreturn
.end

.subroutine cmdMail
	a_axid	,n

record
	alpha	,a20

.proc
	xcall win_stop(1)
	xreturn
.end

