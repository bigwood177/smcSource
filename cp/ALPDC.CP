.subroutine alpdc
	alpha	,a
	dec	,d
	bf	,d
.proc
	onerror not_num
	dec = alpha
	offerror
	bf = 0
	return
not_num,
	offerror
	dec =
	bf = 1
	return
.end
