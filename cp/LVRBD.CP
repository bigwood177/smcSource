subroutine lvrbd		;new louver/back draft damper pricing routine
	ditem	,a
	p_dht	,d
	p_dlen	,d
	dmat	,a
	tdescr	,a
	config_price	,d
	cfg_err	,d
	wnd	,d	;window open in mainline

;10-25-19 ssq: changed slbak4 cols: from 9 to 8 so that slbu6040 would require
;	two sections 30X40 per Cathy M


; BDD LN HT

	.include 'wnd:windows.def'

EXTERNAL FUNCTION
	GNS	,D		;FUNCTION TO GET NUMBER OF DAMPER SECTIONS

record	smpric
	.include 'def:smpric.def'

record	slb2mn
		,d3,	014	;rows
		,d3,	010	;cols
		,14d3,	014,016,020,024,030,036,040,044,050,054,060,064,071,072	; height
		,14d3,	018,024,030,036,042,047,054,059,066,072,999,999,999,999	; length
	
record	slb4mn
		,d3,	013	;rows
		,d3,	010	;cols
		,14d3,	016,020,024,030,036,040,044,050,054,060,064,071,072,999
		,14d3,	018,024,030,036,042,047,054,059,066,072,999,999,999,999
record	slbak2
		,d3,	011	;rows
		,d3,	009	;cols
		,14d3,	014,016,020,024,030,036,040,044,050,054,060,999,999,999
		,14d3,	018,024,030,036,042,047,054,059,066,072,999,999,999,999
record	slbak4
		,d3,	011	;rows
	;;;	,d3,	008	;cols
		,d3,	009	;cols
		,14d3,	014,016,020,024,030,036,040,044,050,054,060,999,999,999
		,14d3,	018,024,030,036,042,047,054,059,066,072,999,999,999,999
record	lvbdmn
		,d3,	009	;rows
		,d3,	004	;cols
		,14d3,	010,014,017,021,025,027,031,035,038,999,999,999,999,999
		,14d3,	012,020,028,036,999,999,999,999,999,999,999,999,999,999
record	mvbdmn
		,d3,	007	;rows
		,d3,	004	;cols
		,14d3,	017,021,028,034,038,046,048,999,999,999,999,999,999,999
		,14d3,	018,024,030,036,999,999,999,999,999,999,999,999,999,999
record lvr_data
	n_row	,d3	;number of rows
	n_col	,d3	;number of cols
	lvr_row	,14d3
	lvr_col	,14d3

record	vars
	opnok	,d1
	dht	,d3
	dlen	,d3
	dqty	,d3	;qty of damper sections
	tbl_name	,a6
	d_opt	,d1
	d_opt2	,d1
	str	,a30
	tl	,d3
	flang	,d1	;1=flanged, add 10% to price
	i	,d6
	j	,d6
	temp	,d10
	temp2	,d10
	qlen	,d10
	qht	,d10
	lmax	,d3
	hmax	,d3
	ddim	,d3
	n_elm	,d3	;number of elements 
	idx	,d6
	chnprc	,d4
	entry	,a30
	inxctl	,d1
	cngctl	,d1
	whatno	,d2
	lokctl	,d1
	read	,d1,0

proc

	dht = p_dht
	dlen = p_dlen

	cfg_err = 1			;err flag gets cleared if all ok...
	clear str, flang

	call opens
	if (.not. opnok) goto endoff

	using ditem select
	('BD','MVB'),	CALL BDD	;BACKDRAFT DAMPER
	('SLF','SLB'),	CALL LVR	;LOUVER
	endusing

	if (config_price .le. 0) cfg_err = 1
endoff,
	call close
	xreturn

;=========================================================

BDD,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Back Draft Damper 
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

d2_disp,
	clear cngctl
	
	xcall w_disp (wnd, wd_clear)
	xcall w_proc (wp_place, wnd, 8,15)
	xcall w_disp (wnd, wd_pos, 1,1, 'CHOOSE ONE OF THE FOLLOWING:')
	xcall w_disp (wnd, wd_pos, 2,4, '1. REGULAR')
	xcall w_disp (wnd, wd_pos, 3,4, '2. FLANGED')
	xcall winpt (wnd, 1, 32, 01, 00, '# ', entry, inxctl)
	d_opt2 = entry(1,1)

	if (d_opt2 .eq. 0) d_opt2 = 1
	if (d_opt2.lt.1 .or. d_opt2.gt.2) goto d2_disp
	entry(1,1) = d_opt2, 'X'
	xcall w_disp (wnd, wd_pos, 1,32, entry(1,1) )
	xcall w_updt
	cngctl = 2
	xcall wancn (wnd, 7, cngctl, whatno)
	goto (d2_disp),cngctl

	xcall w_disp (wnd, wd_clear)
	xcall w_proc (wp_remove, wnd)
	xcall w_updt


	TDESCR(7,13) = 'B-DRAFT'
	using ditem select
	('BDD'),	begin
			call bdd_opt
			tbl_name = 'LVBDMN'
			tdescr(7,13) = 'B-DRAFT'
			end

	('MVB'),	begin
			tbl_name = 'MVBDMN'
			tdescr(7,21) = 'B-DRAFT MED-VEL'
			end
			
	endusing

	using d_opt select
	(2),	str = 'SPNG/'
	(3),	str = 'CB/'
	endusing

	tl = %trim(str)
	using d_opt2 select
	(2),	begin
		str = str(1,tl) + 'FLG'
		flang = 1
		end
	(),	flang = 0
	endusing
	
	tl = %trim(str)
	if (str(tl,tl) .eq. '/') str(tl,tl) = 

	tl = %trim (tdescr)
	tdescr(tl+2,30) = str
;;;	tdescr(15,30) = str

	call get_idx
	config_price = config_price * 10	;tbl .xx to .xxx

	return

bdd_opt,	;

	clear cngctl
	xcall w_disp (wnd, wd_clear)
	xcall w_proc (wp_place, wnd, 8,15)
	xcall w_disp (wnd, wd_pos, 1,1, 'CHOOSE ONE OF THE FOLLOWING:')
	xcall w_disp (wnd, wd_pos, 2,4, '1. REGULAR')
	xcall w_disp (wnd, wd_pos, 3,4, '2. SPRING LOADED')
	xcall w_disp (wnd, wd_pos, 4,4, '3. COUNTER BALANCED')
	xcall winpt (wnd, 1, 32, 01, 00, '# ',entry, inxctl)
	d_opt = entry(1,1)
	if (d_opt .eq. 0) d_opt = 1
	if (d_opt .lt.1 .or. d_opt.gt.3) goto bdd_opt
	entry(1,1) = d_opt, 'X'
	xcall w_disp (wnd, wd_pos, 1,32, entry(1,1) )
	xcall w_updt

	xcall wancn (wnd, 7, cngctl, whatno)
	goto (bdd_opt), cngctl


	xcall w_disp (wnd, wd_clear)
	xcall w_proc (wp_remove, wnd)
	xcall w_updt
	return
	

;--------------------------------------------------------

LVR,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Louvers
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	using ditem select
	('SLF'),	TDESCR(8,30) = 'STD LOUVER FLANGE'
	('SLB'),	TDESCR(8,30) = 'STD LOUVER BOX'
	endusing

	if (dht .le. 12) tdescr(8,10) = '2"'
	using dmat select
	('P'),		if (dht.le.12)			;paint grip
			then tbl_name = 'SLB2MN'
			else tbl_name = 'SLB4MN'

	('A','U'),	if (dht.le.12)			;alum or clad
			then tbl_name = 'SLBAK2'
			else tbl_name = 'SLBAK4'

	(),		if (dht.le.12)			;galv
			then tbl_name = 'SLB2MN'
			else tbl_name = 'SLB4MN'

	endusing

	call get_idx
	config_price = config_price * 1000	;tbl XXX to .XXX

	return
;--------------------------------------------------------



get_idx,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; look up the price in smpric.cat
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	using tbl_name select
	('SLB2MN'),	lvr_data = slb2mn
	('SLB4MN'),	lvr_data = slb4mn
	('SLBAK2'),	lvr_data = slbak2
	('SLBAK4'),	lvr_data = slbak4
	('LVBDMN'),	lvr_data = lvbdmn
	('MVBDMN'),	lvr_data = mvbdmn
	endusing
	
	n_elm = n_row * n_col

	lmax = lvr_col(n_col)		;max length (across)
	hmax = lvr_row(n_row)		;max height (down)

	call get_damper_sections

	for i from 1 thru n_row		;find the row
		begin
		if (dht.le.lvr_row(i)) exitloop
		end

	for j from 1 thru n_col
		begin
		if (dlen.le.lvr_col(j)) exitloop
		end

	idx = ( (i-1) * n_col) + j

	clear smpric
	sm_key = tbl_name
	xcall isio (chnprc, smpric, sm_key, read, lokctl)
	if (lokctl .ne. 0) return

	config_price = sm_price(idx) * dqty
	if (flang) config_price = config_price + (config_price/10)	;add 10% for flanged
	cfg_err = 0				;success !
	return
;-------------------------------------------------------------

get_damper_sections,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; split into sections
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;; determine if overall size requires multiple damper sections...
	QLEN = %GNS(DLEN,LMAX)		;NUMBER OF COLUMNS
	QHT = %GNS(DHT,HMAX)		;NUMBER OF ROWS
	DQTY = QLEN * QHT		;NUMBER OF DAMPER SECTIONS

	TEMP = DLEN/QLEN 				;LEN OF EACH SEG
	IF (TEMP*QLEN .LT. DLEN) TEMP = TEMP + 1	;NEED TO ADD 1 FOR ODD LENGHTS
	DLEN = TEMP

	TEMP = DHT/QHT 					;HT OF EACH SEG
	IF (TEMP*QHT .LT. DHT) TEMP = TEMP + 1
	DHT = TEMP

	RETURN
;--------------------------------------------------------


opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	opnok = 
;;;	open (chnprc=0, si, 'tst:smpric.tsm')
	open (chnprc=0, si, 'cat:smpric.cam')
	opnok = 1
	return
;-------------------------------------------------------------

close,
	if (chnprc) close chnprc
	return
;-------------------------------------------------------------

		


