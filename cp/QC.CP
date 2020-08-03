.subroutine	qc	;inv committed in cop
	chnaud	,d
	order	,d
	prtf	,d
	itm_key	,a
	qty	,d	;qty committed
	ref	,a	;routine

external function
	gu_name	,a

record	audit
	.include 'def:audit.def'

.proc

	clear audit

	A_ORDNO = order
	A_OPRTF = prtf
	A_ITEM = itm_key
	A_DATE = %datetime
	A_USER = ref + %gu_name
	A_OLD = qty
	A_NEW = 
	store (chnaud, audit, a_ordno)

	xreturn
.end


