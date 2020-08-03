subroutine catg
	litmno	,a
	rsegs	,a

; for prdcat G?, first 2 dig in item # is dia, only
; counting barrel clams, count only once.

RECORD
	SEGS	,A12			;first 2 dig are gauge
RECORD,X
	GA	,D2
	SARA	,5D2

record	vars
	char	,a1
	i	,d3
proc
	clear segs
	
	for i from 1 thru 15
		begin
		char = litmno(i,i)
		if(char.ge.'0' .and. char.le.'9')
			begin
			sara(2) = litmno(i,i+1)		;dim 1 skipped if jr
			exitloop
			end
		end
	rsegs = segs
	xreturn

