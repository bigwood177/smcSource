subroutine b128		;print bacode
	strng	,a		;string to print
	b_strng	,a		;return string
	bcf	,d		;bar code font to use

record	b128
	e_esc	,a1
		,a1,	'('
	b_font	,a2
		,a1,	'X'
record	of128
	o_esc	,a1
		,a3,	'(3@'

record	tbl128
	bcc	,a1	;char
	bcn	,d3	;value

record	b_chars
	bs	,a1,'Ì'		; <alt>0204
	be	,a1,'Î'		; <alt>0206
	chk	,a1		;check sum char

record	vars
	space	,a1	;use ascii 194
	temp	,a100	;
	chn128	,d4
	string	,a200
	ln	,d4
	char	,a1
	n	,d6
	i	,d6
	chksum	,d18
;
proc
	xcall ascii (194, space)
	temp = strng

	for i from 1 thru 100
		begin
		if (.not. %chopen(i))
			begin
			chn128 = i
			open (chn128,si,'ut:tbl128.ism')
			exitloop
			end
		end

	xcall ascii (27,e_esc)
	o_esc = e_esc

	b_font = bcf,	'XX'
	ln = %trim(strng)
	call chk_sum

;;;	string = b128 + '*' + strng(1,ln) + '*'
;;;	string = b128 + bs + strng(1,ln) + bcc + be
	string = b128 + bs + strng(1,ln) + bcc + be + of128
	ln = %trim(string)
	b_strng = string
	strng = temp		;restore
	close chn128
	xreturn

chk_sum,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	chksum = 104	;start value

	for i from 1 thru ln
		begin
		if (strng(i,i) .eq. ' ') strng(i,i) = space
		char = strng(i,i)
		read (chn128, tbl128, char)
		n = i * bcn
		chksum = chksum + n
		end
	bcn = chksum - (chksum/103*103)
	read (chn128, tbl128, bcn, krf=1)
	return
;--------------------------------------------------
	end
	
