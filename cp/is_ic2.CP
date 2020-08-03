.function is_ic2
	oline	,a

record	ordlin
	.include 'def:rd045a.def'


record	vars
	tf	,d1
	sz	,d2
	ga	,d2
	len	,d2
	cmpcod	,a3

.proc

	tf = 0		;false

	xcall who(cmpcod)
	using cmpcod select
	('SMC','TST'),	NOP
	(),	GOTO FINI
	endusing

	ordlin = oline

	if (lf1.ne.0 .or. lf2.ne.0 .or. lf3.ne.0) goto fini	;no f-notes
;;;	if (lqtyor .gt. 9) goto fini
	if (litmno .ne. 'SG') goto fini
	if (lcfgim(7,7) .eq. '+') goto fini
	
	onerror fini
	sz = lcfgim(3,4)
	ga = lcfgim(5,6)
	len = lcfgim(7,8)
	offerror

	using sz select
	(4,6,8,10,12,14,16,18),	nop
	(20,22,24),	nop
	(),		goto fini	;invalid size 
	endusing

	using ga select
	(24, 26),	nop
	(),		goto fini	;invalid ga
	endusing

	if (len .ne. 10) goto fini	;must be 10'

	tf = 1				;we've got a winner!


fini,
	offerror
	freturn tf
.end
