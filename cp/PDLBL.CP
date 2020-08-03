subroutine pdlbl
	ordno	,d
	que	,a	;from prwod
	dept	,a	;current dept
	ducfil	,a	;duct work file created in prwod.cp

;print coil line labels

; 2-08-10 note that this routine is reading the duct work file created
; in prwod.cp not the ducacc.ism file...

;
EXTERNAL FUNCTION
	TRN3	,D

EXTERNAL FUNCTION
	GU_NAME	,A

	.INCLUDE 'DEF:HPSUB.DEF'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PCL code for IDX C8800 printer

record	reset
	re	,a1	;esc
		,a1,	'E'

record	l1
	e1	,a1	;esc
		,a*,	'*v6W'
	lx	,8a1
;;;		,a*,	' 010108080808'
record	idx_pal
	e2	,a1	;esc
		,a*,	'*v'
	ipcR	,d3	;color 1 
		,a*,	'a'
	ipcG	,d3	;color 2
		,a*,	'b'
	ipcB	,d3	;color 3
		,a*,	'c'
	ipidx	,d2	
		,a*,	'I'


record	idx_sel
	e5	,a1	;esc
		,a*,	'*v'
	isidx	,d2
		,a*,	'S'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


RECORD ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	DUCACC
	.include 'def:pdlbl.def'
;;;	.INCLUDE 'DEF:RD175A.DEF'

RECORD	PARAM2
	.INCLUDE 'DEF:PARAM2.DEF'


RECORD	P_EJECT
	PJ_ESC	,A1
		,A1,	'&'
		,A1,	'l'
		,A1,	'0'
		,A1,	'H'
		
	
record	tray
	te	,a1	;esc
		,a*,	'&l'
	tra	,d1
		,a*,	'H'


RECORD	LP_BOLD
	B_ESC	,A1
		,A1,	'%'
	L_BOLD	,A1

RECORD	LP_MEDIUM
	M_ESC	,A1
		,A1,	'%'
	L_MED	,A1

RECORD	SPLFIL
		,A4,	'SPL:'
		,A1,	'O'
	SPORD	,A6
		,A4,	'.SPL'
	

RECORD	CHANNEL
	CHN182	,D2
	CHN044	,D2
	CHN175	,D2


RECORD	VARS
	xdept	,a2
	a30	,a30
	tl	,d3
	cmpcod	,a3
	hex0	,a1
	hex1	,a1
	hex2	,a1
	hex8	,a1

	color	,d2
	numlbl	,d4
	E_ESC	,A1
	LABLIN	,A30	;DATA FOR 1 LABEL LINE
	RI	,D6	;LABEL PRINT ROW NUMBER
	CI	,D6	;LABEL PRINT COL INDEX
	LBLROW	,D6	;CURRENT ROW FOR 1ST LINE OF DATA
	LBLCOL	,4D4,	003,039,075,111		;STARTING COLUMNS

;;;	LBLCOL	,4D4,	002,036,071,104		;STARTING COLUMNS
	PROW	,D6	;PRINT ROW
	PCOL	,D6	;PRINT COL
	I	,D6
	YELLOW	,D1	;Tray number for yellow paper
	BLUE	,D1	;Tray number for blue paper
	U_NAME	,A25
	TRA_B	,D1
	TRA_Y	,D1
	TRA_W	,D1
	cd_rfa	,a6
	POLX1	,A12
	POLX2	,A12
	LL	,D2
	S_LL	,D2
	PRINTER_NAME	,A8
	LP_ON		,D1	;first pass just to count pages
	NPAG		,D4
	HPR		,D4	;ROW FOR CALL HP

	P_OLINE	,D1	;1=PRINTING ORDER LINE
	SPLFL2	,A14
	t_cnt	,d4
	VL	,D3,	082
	VL2	,D3
	SZ1	,D3
	SZ2	,D3

	SKPLIN	,D2
	NO_DUCT	,D1
	A2	,A2
	SAVQTY	,D6
	STAT	,D1
	LOKCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	DECMAL	,D18
	PLINE	,A95
	S_PLINE	,A95
	LONG_LINE	,A110
	S_LINCNT	,D3
	S_PAGE		,D3
	LINCNT	,D3
	TOTPRC		,D8
	SAV_TOTPRC	,D8
	BLANKS	,A30
	SAVJOINT	,D5
	SAVFEET		,D6
	SAVFLIN		,D6
	SAVLBS		,D6
	CONFIG	,D7
	PAGE	,D2
	KEY	,A6
	SWITCH	,D1
	V	,D1
	J	,D5
	SEQNO	,D2
	SKIP_LINE	,D1		;SKIP THIS LINE IF BLOCK MEMO
	BLANK90		,A90
	REM		,D4
	DASHES		,A30,	'------------------------------'
	LFEED	,D2
	DTMASK	,A8,	'XX/XX/XX'
	NUMASK	,A8,	'ZZZZZZX-'
	MAXLIN	,D2

PROC
	xcall who (cmpcod)
	using cmpcod select
	('SMC','TST'),	NOP
	('ROC'),	NOP	;7-21-10 rockford online...
	(),	XRETURN
	endusing

	XCALL ASCII(27,E_ESC)
	SET PJ_ESC, te, B_ESC, M_ESC = E_ESC
	CALL OPENS

	call read_liner			;2-22-17

	LOKCTL = 1
	XCALL ISIO (CHN044,ORDHDR,ORDNO,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.OORDNO.NE.ORDNO) GOTO ENDOFF

; print 4 labels across the page...

;;;>	FIND(CHN175,DUCACC,ORDNO) [ERR=LD_SETUP]
LD_SETUP,
	CALL OPEN_SPLFIL

	color = 0
	LBLROW = 0	;FIRST ROW
	CI = 1		;FIRST COL
LD_LOOP,
	LOKCTL = 1
	XCALL IOS (CHN175,DUCACC,READ,LOKCTL)
	if (lokctl .ne. 0) goto ld_eof
;;;	IF (LOKCTL.NE.0.OR.DUCTOR.NE.ORDNO) GOTO LD_EOF

;;;	using xdept select
;;;	('O'),	if (dept .ne. 'Y') goto ld_loop	;O
;;;	('P'),	if (dept .ne. 'Z') goto ld_loop	;O
;;;	endusing

	sty = 10 - sty				;restore original value

	xcall ddept (gauge, (100-size3), dutype, xdept, sty)
	using xdept select
	('O'),	if (dept .ne. 'Y') goto ld_loop	;O
	('P'),	if (dept .ne. 'Z') goto ld_loop	;O
	endusing

;;;	using sty select
;;;	(1,4,5),	if (dept .ne. 'Z') goto ld_loop		;P
;;;	(2,3,6),	if (dept .ne. 'Y') goto ld_loop		;O
;;;	endusing

;-
	SIZE3 = 100-SIZE3	; LENGTH 	restore original value
	SIZE1 = 1000000-SIZE1	; SLIP		restore original value
	SIZE2 = 1000000-SIZE2	; DRIVE		restore original value	
	STY = 10-STY		; Style		restore original value
;-

	PCOL = LBLCOL(CI)		;CURRENT PRINT COL
	RI = -1				;CURRENT PRINT ROW OFFSET
	using cat select
	(1),	numlbl = joint * 2	;L - Shaped
	(2),	numlbl = joint		;Wraparound
	(3),	numlbl = joint * 4	;4 piece
	(4),	numlbl = joint		;Other
	(5),	numlbl = joint * 4	;Oversize
	(),	numlbl = joint		;anything else
	endusing

;;;	for i from 1 thru numlbl call print_labl
	for i from 1 thru numlbl 
		begin
		call print_labl
		if (lblrow .gt. 56) 	;15 rows
			begin
			lblrow = 0
			writes (14, p_eject)
			end
		end

	GOTO LD_LOOP

;=============================================================
print_labl,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; print a label for the current duct record
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	color = 1
;1
	;pick color based on last dig of ordno...
	isidx = ordno(6,6) 
	display (14, idx_sel)	;select an index

; special case for first line...
	incr ri
	if (ri .gt. 3) ri = 0
	prow = lblrow + ri	;current row + offset
	pcol = lblcol(ci)	;current column

	lablin (1,20) = ocusnm
	xcall hp (14, hpPOS, prow, pcol, 0, lablin(1,20) )

	isidx = 1
	display (14, idx_sel)	;back to black

	clear lablin
	lablin (1,6) = ordno,	'ZZZZZX'
	xcall hp (14, hpPOS, prow, pcol+21, 0, lablin(1,6) )
	clear lablin

;;;	isidx = 1
;;;	display (14, idx_sel)	;back to black
;2
	sz1 = %trn3 (size1)
	sz2 = %trn3 (size2)
	lablin (1,3) = sz1,	'ZZX' [left]
	lablin (5,5) = 'X'
	lablin (7,9) = sz2,	'ZZX' [left]
	lablin (10,10) = '-'
	lablin (11,11) = size3,	'X'
	lablin (13,17) = gauge,	'ZX GA'

;;;	using sty select
	using (10-sty) select
	(1 thru 6),	lablin (20,22) = dsty( (10-sty) )
	(),		lablin (20,22) =
	endusing

	using seam select
	(1,2),		lablin (24,26) = dseam(seam)
	(),		lablin (24,26) = 
	endusing
	call p_lab

;3
	a30 = dpunch
	tl = %trim(a30)
	tl = tl+2
	if (tl .gt. 30) tl = 30
	a30(tl,30) = locat
	lablin = a30

;;;	lablin (1,15) = dpunch
;;;	lablin (16,30) = locat
	call p_lab

;4
	using liner select
	(1 thru 9),	lablin (1,7) = dliner(liner)
;;;	(1 thru 8),	lablin (1,7) = dliner(liner)
	endusing
	

	call p_lab
		
	incr ci				;next print column
	if (ci .gt. 4) 
		begin
		ci = 1				;reset starting print column
		lblrow = lblrow + 4		;starting row for next 4 labels
		end
	
	return
;------------------------------------------------------

p_lab,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Print a Label Line...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	incr ri
	if (ri .gt. 3) ri = 0
	prow = lblrow + ri	;current row + offset
	pcol = lblcol(ci)	;current column

	xcall hp (14, hpPOS, prow, pcol, 0, lablin)
	clear lablin

	return
;-------------------------------------------------------
endoff,
ld_eof,
	close 14
;;;	lpque (splfil)	;dbg
	if (color .ne. 0) lpque (splfil, LPNUM:que)	;labels were printed.

	CLOSE CHN175
;;;	CLOSE CHN044

	XRETURN

;================================================


;----------------------------------------------

CONTIN,
	WRITES(14,P_EJECT)

	return
;----------------------------------------------


OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	L_BOLD = 'G'		;START ITALIC
	M_ESC = B_ESC
	L_MED = 'H'		;END ITALIC
	SWITCH = 5

;---------------------------------------------------
; ORDHDR already opened on ch 4 in calling routines...
	CHN044 = 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CHN175 = 46
	open (chn175, si, ducfil)	;use this instead of ducacc.ism
;;;	XCALL CHNOPN (26,STAT)
;;;	IF (STAT .EQ. 0)
;;;		BEGIN
;;;		XCALL FILES (26,'SI',175,SWITCH)
;;;		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CHN182 = 17	;COP TABLES


	RETURN
;----------------------------------------------------------

OPEN_SPLFIL,	;;;;;;;;;;;;;;;;;;;;;;;
	SPORD = ORDNO,'XXXXXX'
	OPEN (14, O, SPLFIL)

	set re, e1,e2,e5 = e_esc
	
	display (14, reset)

;tray 3 is 2nd tray on OKI C8800 series
;;;	tra = 5		
;;;	tra = 4		;8-26-13 for C610
	tra = 8		;4-2-18 multi-purpose tray for Lexmark


	display (14, tray)	;labels come from 2nd tray 

; set the color palettes...
	xcall ascii (0, hex0)
	xcall ascii (1, hex1)
	xcall ascii (8, hex8)
	xcall ascii (2, hex2)

	set lx(1), lx(2) = hex1
	lx(1) = hex2
	set lx(3) = hex8
	set lx(4), lx(5), lx(6) = hex0
	
	writes (14,l1)

;-

	clear tbl_key
	tblcod = 'CL'
	find (chn182, coptbl, tbl_key) [err=color_loop]
color_loop,
	xcall ios (chn182, coptbl, read, lokctl)
	if (lokctl .ne. 0) goto eof_color
	if (tblcod .ne. 'CL') goto eof_color	

	ipidx = cl_num
	ipcR = cl_red
	ipcG = cl_grn
	ipcB = cl_blu
	display (14,idx_pal)
	
	goto color_loop
eof_color,


;--------------------------------------

	XCALL HP (14,hpDOTS, 0)
	XCALL HP (14,hpTHICK,3)
;;;	XCALL HP (14,hpLPI, 12)			;12 Lines per inch
;;;	XCALL HP (14,hpLPI, 6)			;12 Lines per inch
	XCALL HP (14,hpLPI, 7)			;12 Lines per inch


;;;	XCALL HP (14,hpFONT,hp167CPI+hp12PT+hpBold+hpGOTHIC)	
	XCALL HP (14,hpFONT,hp18CPI+hp12PT+hpBold+hpGOTHIC)	

	RETURN
;--------------------------------------


;------------------------------------------------

READ_LINER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; GET LINER DESCRIPTIONS FROM LINER PRICE TABLE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR COPTBL
	TBLCOD = 'LP'
	FOR I FROM 1 THRU 9
		BEGIN
		LP_KEY = I
		XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
		IF (TBLCOD .NE. 'LP') EXITLOOP
		DLINER(I) = lp_liner
		END

	RETURN
;--------------------------------------------------
END


