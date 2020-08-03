;UROCWT.CP
;
; Copy SMC weights from itmmas to ROC itmmas
;
record	itmkey
	.include 'def:rd041k.def'

record	itmmas
	.include 'def:rd041a.def'

record	rocitm
	.include 'def:rd041m.def'
;

record	channel
	chn041	,d2
	chnroc	,d2

record	dis
	ii	,d6

record	vars
	opnok	,d1
	v	,d1
	inxctl	,d1
	entry	,a30
	lokctl	,d1
	read	,d1,0


proc
	xcall terid(v)
	v = 1
	xcall outpt (1,1,2,'Transfer inventory weights to Rockford',1)

displa,
	xcall outpt (5,4,0,'ARE YOU SURE?',1)
	XCALL INPUT (5,20,01,01,'YN',ENTRY,INXCTL,v)
	GOTO (PROCES, ENDOFF),INXCTL

PROCES,

	call opens
	if (.not. opnok) goto endoff

	clear ii
loop,
	incr ii
	if (ii/1000*1000 .eq. ii) xcall outpt (1,70,0,dis,1)

	xcall ios (chnroc, rocitm, read, lokctl)
	if (lokctl .eq. 2) goto eof
	if (lokctl .ne. 0) goto loop

	clear itmkey
	k_item = m_itemno
	k_f1 = m_if1
	k_f2 = m_if2
	k_f3 = m_if3

	xcall isio (chn041, itmmas, itmkey, read, lokctl)
	if (lokctl .ne. 0) goto loop

	m_weight= weight
	m_ip_wgt = ip_wgt
	m_ip_um = ip_um		;7-24-17

	write (chnroc, rocitm, itmkey)
	goto loop

opens,
	clear chn041, chnroc
	open (1,si, 'smc:itmmas.smm')
	chn041 = 1

	open (2,su,'roc:itmmas.rom')
;;;	open (2,su,'tst:itmmas.tsm')

	chnroc = 2

	opnok = 1
return


eof,
endoff,
	if (chn041) close chn041
	if (chnroc) close chnroc
	XCALL PGCHN ('CP:IMSFMN',1)

	stop
	end
	

	