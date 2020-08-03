;dux.cp
;
;	transfer prices from dux to duc


record	dux
;TABLE.DEF	(147 BYTES)
; file # 182 - COPTBL.ISM
; Generic table ...
; add overlays as necessary.

	xBLCOD	,A2	;DP-DUCT PRICING, LP-LINER PRICING
	xBLKEY	,A15	;KEY FOR CODE
	xBLDES	,A30	;TABLE DESCRIPTION
	xBLDAT	,A100	;DATA SPECIFIC TO EACH CODE

; table layout for duct and liner pricing:
RECORD,X
			,A2	;'DP'
	xP_TYPE		,D1	;TYPE
	xP_STYLE	,D1	;STYLE
	xP_CAT		,D1	;CATAGORY
	xP_GAUGE	,D3	;GAUGE
			,A9	;KEY FILLER
	xP_DESC		,A30
	xP_PRICE	,D8	;(XXXXX.XXX)
			,A92
RECORD,X
	xBL_KEY		,A17


;
record	coptbl
	.include 'def:rd182a.def'
;
record	data
	k_type	,d1
	k_style	,d1
	k_cat	,d1
	k_gauge	,d3
	k_price	,d8
;
record	vars	
	chnwrk	,d2
	chn182	,d2
	i	,d6
;
proc
	open (15,i,'tt:')
	display (15,$scr_clr(screen), 'dux')

	chnwrk = 18
	chn182 = 17	

	open (chn182, 'su', 'smc:coptbl.smm')
	open (chnwrk, 'si', 'smc:coptbl.smm')

	clear tbl_key
	tblcod = 'DP'
	find (chn182, coptbl, tbl_key) [err=loop]
loop,
	reads (chn182, coptbl, eof)
	if (tblcod .ne. 'DP') goto eof
	
	clear xbl_key
	xblcod = 'DX'
	xp_type = dp_type
	xp_cat = dp_cat
	xp_style = 
	xp_gauge = dp_gauge
	read (chnwrk, dux, xbl_key) [err=loop]

	dp_price = xp_price
	write (chn182, coptbl, tbl_key)
	goto loop

eof,
	close chn182
	close chnwrk

	xcall pgchn ('cp:ducmnu',1)
	stop

	end

