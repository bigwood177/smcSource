;nit_arq.cp
;
;	nite batch version of quote reports.
;	run every wed nite, print date for the last 7 days

record
	xdat1	,d8
record,x
	yy	,d4
	mm	,d2
	dd	,d2

record	vars
	xdat2	,d8
	nite	,d1
	mon	,12d2	,31,28,31,30,31,30,31,31,30,31,30,31
;
proc
	xcall rdat8(xdat2)
	xdat1= 20140314		;hard-coded per jes 4/2/14

; go back 7 days
;;;	xdat1 = xdat2
;;;
;;;	dd = dd - 6
;;;	if (dd .le. 0)
;;;		begin
;;;		mm = mm - 1
;;;		if (mm .le. 0)
;;;			begin
;;;			yy = yy - 1
;;;			mm = 1
;;;			end
;;;		dd = mon(mm) + dd
;;;		end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	nite = 1
;;;	xcall arqrp (xdat1, xdat2, nite)
;;;	xcall qutrp (xdat1, xdat2, nite)
	xcall slmes (xdat1, xdat2, nite)

	xcall flags (7000000)
	stop

