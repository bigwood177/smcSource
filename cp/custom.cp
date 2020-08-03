;custom.dbl
;
;custom report for jimm
;
;catsls.dbl
;
;	sales by category for non-stocked items;2010 & 2011
;
record	ordhdr
	.include 'def:rd044a.def'

record	ordlin
	.include 'def:rd045a.def'

record	chan
	chn044	,d2
	chn045	,d2

RECORD TITLE
		,A39,	'CUSTOM REPORT: YTD NON-STOCKED ITEMS'
RECORD HDR1
		,A*,	'CAT DPT ITEM #          F1 F2 F3    DESCRIPTION                         QTY'
		,A*,	'               SALES'

RECORD	PRINT
	HDR2	,A*,	'NO HDR'
	LINCNT	,D2,	60
	SPLFIL	,A14
	LPONSW	,D1
	LPARG	,D1
	RPTNUM	,D3
	PRTTYP	,A1
	PGCNT	,D6,	000000
	PLINE	,A132
	PRTCTL	,D3
	LPSW	,D2



record	out
	o_cat	,a2
	o_item	,a15
	o_f1	,a3
	o_f2	,a3
	o_f3	,a5
	o_dept	,a2
	o_descr	,a30
	o_date	,d8
	o_qty	,d8
	o_sales	,d12

;
record	total
	tcat	,d12
	ttot	,d12


record	dis
	ii	,d6

record	vars
	opnok	,d1
	wrkfil	,a14,	'spl:wrkfil.ism'
	string	,a100
	path	,a100
	len	,d6
	ENTRY	,A30
	INXCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	LOKCTL	,D1
	savcat	,a2
	mon	,d2
	i	,d6
	sales	,d12
	year	,d2	;10 or 11
	key	,a28
	tab	,a1
	buf	,a200
	tl	,d6
	rl	,d6
	switch	,d1
	v	,d1

;
proc
	xcall terid (V)

	xcall outpt (1,1,2,'CUSTOM ITEMS (YTD NON-STOCK SALES)',V)
	xcall outpt (2,1,1,'SPL:CUSTOM.XLS',1)

	CALL OPENS 
	IF (.NOT. OPNOK) GOTO ENDOFF

	XCALL ISAMC (wrkfil, 88, 1, 'start=1, length=28, dups, ascend')
	open (1, su, wrkfil)
	open (10,o,'catsls.dat')


	clear ii
loop,
	incr ii
	if (ii/1000*1000 .eq. ii) display (15,$scr_pos(1,70),dis)
	XCALL IOS (CHN045, ordlin, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO EOF

	if (ltype .eq. 'M') goto loop	
	if (lstokt .eq. 'S') goto loop
	IF (LDEPT .EQ. 'I') GOTO LOOP

	if (oordno .ne. lordno) XCALL ISIO (CHN044,ordhdr,lordno, READ, LOKCTL)

	
	sales = (lqtysh * lprice)#1

	key(1,2) = lprdcd
	key(3,17) = litmno
	key(18,20) = lf1,	'XXX'
	key(21,23) = lf2,	'XXX'
	key(24,28) = lf3,	'XXXXX'
	read (1, out, key) [err=new_cat]

	o_sales = o_sales + sales
	o_qty = o_qty + lqtysh


	write (1, out, key)
	goto loop


new_cat,
	clear out

	o_cat = lprdcd
	o_dept = ldept
	o_f1 = lf1,	'XXX'
	o_f2 = lf2,	'XXX'
	o_f3 = lf3,	'XXXXX'

	o_date = oinvdt
	o_item = litmno
	o_descr = ldescr
	o_date = oinvdt
	o_qty = lqtysh
	o_sales = sales


	store (1, out, o_cat)
	goto loop

eof,
	CLOSE CHN044
	CLOSE CHN045

	close 1
	close 10

	LPONSW = 0
	LINCNT = 60
	PGCNT =

	xcall ascii (9,tab)

	open (1,si, wrkfil)
	open (2,o,'spl:custom.xls')
;;;	open (2,o,'spl:custom.tab')
;;;	open (2,o,'c:\for_jimm\custom.tab')

	
	buf = 'Cat'
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl,100) = 'Dept'
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl
	
	buf(tl,100) = 'Item'
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl,100) = 'Description'
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl,100) = 'Qty'
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl,100) = 'Sales'
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl
	rl = %trim(buf)
	writes (2,buf(1,rl) )


	savcat = '-1'
loop2,

	reads (1, out, eof2)
	rl = %trim(out)
	if (rl .lt.5) goto loop
	if (o_cat .ne. savcat) call newcat


	CALL PRINT_LINE
	CALL TAB_LINE


	tcat = tcat + o_sales

	goto loop2

TAB_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	buf = o_cat
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl,100) = o_dept
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl,100) = o_item
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl,100) = o_descr
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl,100) = o_qty,	'ZZZZZZZX-' [RIGHT]
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl


	buf(tl, tl+13) = o_sales, 'ZZZZZZZZZZ.XX-' [RIGHT]	

	rl = %trim(buf)
	writes (2,buf(1,rl) )

	RETURN
;--------------------------------------------------------

PRINT_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	PLINE(1,2) = O_CAT
	PLINE(5,6) = O_DEPT
	PLINE(9,23) = O_ITEM
	PLINE(25,26) = O_F1
	PLINE(28,29) = O_F2
	PLINE(31,35) = O_F3
	PLINE(37,66) = O_DESCR
	PLINE(68,75) = O_QTY,	'ZZZZZZX-' 
	PLINE(85,98) = O_SALES,	'ZZ,ZZZ,ZZX.XX-'
	CALL PRINT
	RETURN
;--------------------------------------------------------


newcat,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (savcat .eq. '-1') goto outcat
	buf = savcat
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl,100) = 'Cat Totals'
	tl = %trim(buf)
	incr tl

	buf(tl,tl) = tab
	incr tl

	buf(tl,tl) = tab
	incr tl

	buf(tl,tl) = tab
	incr tl

	buf(tl,tl) = tab
	incr tl

	buf(tl, tl+14) = tcat, 'ZZZZ,ZZZ,ZZZ.XX' [RIGHT]	

	rl = %trim(buf)
	writes (2,buf(1,rl) )
	clear buf
	writes (2, buf)

	PLINE = 'CAT TOTALS:'

	PLINE(85,98) = TCAT,	'ZZ,ZZZ,ZZX.XX-'
	CALL PRINT
	CALL PRINT

outcat,
	savcat = o_cat	
	ttot = ttot + tcat
	tcat = 

	return
;--------------------------------------------------------

eof2,
	call newcat

	buf = 'Grand Totals'
	tl = %trim(buf)
	incr tl
	buf(tl,tl) = tab
	incr tl

	buf(tl,tl) = tab
	incr tl

	buf(tl,tl) = tab
	incr tl

	buf(tl,tl) = tab
	incr tl

	buf(tl,tl) = tab
	incr tl

	buf(tl, tl+14) = ttot, 'ZZZZ,ZZZ,ZZZ.XX' [RIGHT]	

	rl = %trim(buf)
	writes (2,buf(1,rl) )
	close 1
	close 2

	PLINE = 'GRAND TOTALS:'

	PLINE(85,98) = TTOT,	'ZZ,ZZZ,ZZX.XX-'
	CALL PRINT
	CALL PRINT

;;;	stop

ENDOFF,
	CALL CLOSE
	IF (LPONSW.EQ.1) XCALL LPOFF (LPSW,SPLFIL,PGCNT)

	xcall outpt (12,4,2,'OPEN SPREAD SHEET?',1)
	xcall input (12, 25,01,01,'YN',ENTRY,INXCTL,1)
	GOTO (NOPE),INXCTL -1

	XCALL GETLOG ("SPL",string,LEN)
	path = string(1,len) + "\" + "custom.xls"
	xcall shell (0, path(1, %trim(path) ) )
;;;	xcall spawn (path(1, %trim(path) ) )
NOPE,
	XCALL PGCHN ('CP:COPRPT',1)

PRINT,
	IF (LPONSW.EQ.0) CALL PRNTON
	XCALL LPOUT (LINCNT,PGCNT,PLINE,TITLE,HDR1,HDR2,'NO HDR3',
&			'NO LEGEND',' ',' ',1,132,PRTCTL,0,LPSW,RPTNUM,PRTTYP)
	RETURN
PRNTON,
	SPLFIL (5,6) = 'EL'
	LPSW = 1	; PRINT, SPOOL OR DISPLAY
	XCALL LPON (LPSW,SPLFIL)
	IF (LPSW.EQ.0) GOTO ENDOFF
	LPONSW = 1
	RETURN
;************************************************************************



OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR OPNOK
	SWITCH = 5

	XCALL FILES (4,'SI',184, SWITCH)		;SLHHDR
	IF (SWITCH .EQ. 9) RETURN
	CHN044 = 4

	SWITCH = 5
	XCALL FILES (5,'SI',185, SWITCH)		;SLHLIN
	IF (SWITCH .EQ. 9) RETURN
	CHN045 = 5

	OPNOK = 1
	RETURN
;---------------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN044) CLOSE CHN044
	IF (CHN045) CLOSE CHN045

	RETURN
;---------------------------------------------------

