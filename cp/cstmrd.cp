;cstmrd.cp
;		custom product R & D reports/spread sheets for Jim at year end
;
;	if this is run prior to year end use slhhdr, slhlin, slshst - if after year end
;	use slhh16, slhl16, slshst.y16, etc
;

record	mail
	c_body	,a20
	at1	,a100
	at2	,a100
	at3	,a100
	at4	,a100
	at5	,a100
	at6	,a100
	at7	,a100
	at8	,a100
	at9	,a100
	at10	,a100

	c_mail	,a40
	c_subj	,a80
	send	,d1,	1	;send
	
record	vars
	i	,d6
	lokctl	,d1
	inxctl	,d1
	cngctl	,d1
	whatno	,d2
	entry	,a30
	read	,d1,0
	write	,d1,1
	store	,d1,2
	v	,d1

;
proc
	xcall terid (v)
	xcall outpt (1,1,2,'Custom, R&D Reports',1)
;;;	goto endoff
	xcall outpt (4,4,1,'DO YOU WANT TO CREATE THE TAB DELIMITED FILES?',1)
	xcall input (4,55,01,00,'YN',entry,inxctl,1)
	goto (proces,endoff),inxctl
;

proces,
	xcall outpt (1,1,2,'Custom, R&D Reports',1)

	xcall catsls			;"stocked_sales.xls" - sales by category
	xcall slhpt3			;"non_stocked.xls" - non-stocked sales history by category
	xcall catss1			;"custom.xls" - non-stocked sales and total sales by category
	xcall tabitm			;"stocked_items.xls"

endoff,

	c_mail = "jessier@smcduct.com"
	c_subj = "Custom R&D tab delimited files"
	c_body = "Data attached."

	at1 = "c:\smc\spool\stocked_sales.xls"
	at2 = "c:\smc\spool\non_stocked.xls"
	at3 = "c:\smc\spool\custom.xls"
	at4 = "c:\smc\spool\stocked_items.xls"

	xcall mail (c_mail, c_subj, c_body, at1, at2, at3, at4,,,,send)

	xcall pgchn ('cp:coprpt',1)
	end

