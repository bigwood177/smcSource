subroutine tdate
;
;---------subroutine for converting date format--------------
;
;
	tdate	,d

record
	date	,d8

record, x
	YYYY	,d4
	MM	,d2
	DD	,d2

record vars
	today	,a11
	
proc
	today = %DATE
	YYYY = today(8,11)
	DD = today(1,2)
	
	using today(4,6) select
	('JAN'), MM = 01
	('FEB'), MM = 02
	('MAR'), MM = 03
	('APR'), MM = 04
	('MAY'), MM = 05
	('JUN'), MM = 06
	('JUL'), MM = 07
	('AUG'), MM = 08
	('SEP'), MM = 09
	('OCT'), MM = 10
	('NOV'), MM = 11
	('DEC'), MM = 12
	endusing

	tdate = date
	xreturn

	
	
	
