.function trn3
	in	,d

;always round up to next interger value
; ie 5.001 returned as 6.000

record	vars
	dec	,d18

.proc
	dec=in/1000
	if(in.gt.dec*1000)dec=dec+1	
	freturn dec
.end
