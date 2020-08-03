subroutine getd
	item	,a	;item number 
	br1	,d	;1=damper on branch 1
	br2	,d	;1=damper on branch 2


;

record	vars
	st	,d6
	i	,d6
	j	,d6
	tl	,d6
	wrkitm	,a15
	an	,d1
	code	,a3
	body	,a13
	
;
proc

	clear br1, br2
	if (.not. %instr(1,item,'*') ) xreturn		;no dampers

	i = 0
	for j from 1 thru 4
		begin
		using item(j,j) select
		('V'),	nop
		('4','9'),	if (an .eq. 0)
				then	an = item(j,j)
				else	call shift

		(),	call shift
		endusing
		end
	incr i
	wrkitm(i,15) = item(j,15)

	USING WRKITM SELECT
	('ST'),		BEGIN
			CODE = WRKITM(1,3)	;STC,STT
			BODY = WRKITM(4,15)
			END

	('CC','CT'),	BEGIN
			CODE = WRKITM(1,2)	;CC,CT
			BODY = WRKITM(3,15)
			END

	('C','T'),	BEGIN
			CODE = WRKITM(1,1)	;C,T
			BODY = WRKITM(2,15)
			END
	ENDUSING


	st = 1
	for j from 1 thru 3
		begin
		tl = %instr(st, body, '*')
		if (tl.gt.0)st = tl+1
		using tl select
		(7,8), 		br1 = 1	;damper on branch 1
		(9,10,11),	br2 = 1	;damper on branch 2
		endusing
		end


	xreturn

shift,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	incr i
	wrkitm(i,i) = item(j,j)
	return
;---------------------------------------

	end
