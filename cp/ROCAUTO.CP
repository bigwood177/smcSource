;ROCauto.cp
;smcauto.dbl
; version of vtest.dbl that creates Vulcan job via SMCAutoImport.exe
;
; the following environmental variables must be set:
; vul=c:\program files\mechanical data\vulcan 6 xp
; vulsig=\\server01\apps\quote-vulcan exports\vulcan round
; vulsig=\\server01\apps\quote-vulcan exports\Rockford	;for rockford
;
;	10-07-08: Use SMCAUTO.ISM to keep track of orders sent to Vulcan.
;	5-11-15: get server from coptbl
;	6-10-15: not processing -2 & -3 if both created.

;
;vtest.dbl
;
;	mainline to test vt01s.cp
;
common	vulcan
	dampr	,d1
	is_2	,d1	;1 = w2file has data
	is_3	,d1	;1 = w3file has data
	chnw2	,d2	;-2
	chnw3	,d2	;-3
	vdkey	,d4	; vulcan table (VD)
	m1_note	,a12
	m2_note	,a12
	m3_note	,a12
	d_br1	,d1	;1 = damper on branch 1	(3-20-17)
	d_br2	,d1	;1 = damper on branch 2	(3-20-17)
	org_item	,a15		;original value of ordlin.lcfgim

common	pressed_reducer
	p_file_data	,d1	;1=vt08s has written data to 'spl:"

;===========================================
	.include 'dbldir:dbl.def'
;===========================================
record	smcord
	vul_ord	,d6

record	p_list
	p_reducer	,a15
	p_qty		,a6
			,a2
	p_f3		,a30

record	vulcli
	v_lordno	,d6
	v_linseq	,d3
record,x
	vul_key		,a9

record	ordhdr
	.include 'def:rd044a.def'

record	ordlin
	.include 'def:rd045a.def'
record,x
	.include 'def:rd045d.def'

record	coptbl
	.include 'def:rd182a.def'

record	cpmemo
	.include 'def:cpmemo.def'

record	sp_file
;;;		,a*,	'c:\fab\VulcanFileSign.exe '
		,a*,	'c:\QuoteSoft\fab\VulcanFileSign.exe '
	sp_work	,a8
		,a7,	'.import'


	
RECORD	W2FILE
		,a27,	'\\tsclient\c\QuoteSoft\fab\'
	W2ORD	,D6
		,A2,	'-2'
		,A7,	'.import'
record,x
		,a27
	w2imp	,a8		;11-22-10: was a18

RECORD	W3FILE
		,a27,	'\\tsclient\c\QuoteSoft\fab\'
	W3ORD	,D6
		,A2,	'-3'
		,A7,	'.import'

record,x
		,a27
	w3imp	,a8

record	wsig
	wfile	,a32
		,a*,	'.signature'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
record	svrsig
		,a44,	'\\server-dc01\Trimble\Quote-Vulcan\Autoroll\'
		,a15
		,a10,	'.signature'
record	svrimp,x
		,a44
	svrfil	,a15

;;;record	svrsig
;;;;;;		,a47,	'\\serversbs\apps\quote-vulcan exports\Rockford\'
;;;		,a2,	'\\'
;;;	sv_ser	,a11
;;;		,a36,	'\apps\quote-vulcan exports\Rockford\'
;;;		,a15
;;;		,a10,	'.signature'
;;;record	svrimp,x
;;;		,a49
;;;	svrfil	,a15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

record	vfile
		,a4,	'spl:'	
		,a1,	'V'
	vord	,d6
		,A4,	'.ism'

record	SMCauto
		,a17,	'SMCAutoImport.exe'
		,a1
		,a1,	'"'
	smcName	,a10
		,a1,	'"'
		,a1
		,a1,	'"'
	smcOrder,d6
		,a1,	'"'
		,a1
		,a1,	'"'
		,a6,	'Master'
		,a1,	'"'
		,a1
		,a1,	'"'
	smcRest	,a80
	

record	p_line
	vline	,a80

record	memara
	ma2	,a90
record,x
	ma	,3a30


record	channel
	chn044	,d2
	chn045	,d2
	chn182	,d2
	chnv	,d2

record	vars
	cmpcod	,a3
	lokctl	,d1
	read	,d1,0
	spldir	,a200
	xsp_file	,a300
	ln	,d6
	resend	,a1
	opnok	,d1
	logfil	,a14		;smc:smcauto.smm
	vulimp	,a8		;w2imp or w3imp
	len	,d6
	fl	,d6
	err	,d1
	pnum	,d6
	a1	,a1
	a5	,a5
	a6	,a6
	a30	,a30
	d6	,d6
	i	,d6
	switch	,d1
	v	,d1
;
proc
	open (15,i,'tt:')
	display (15,$scr_clr(screen),'SMCauto')
;
	call opens
	if (.not. opnok) goto endoff

	xcall who (cmpcod)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	clear coptbl
;;;	tblcod = 'SR'
;;;	sr_cmp = cmpcod
;;;	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
;;;	if (lokctl .ne. 0)
;;;		begin
;;;		xcall mesag('no coptbl server record',1)
;;;		goto endoff
;;;		end
;;;
;;;	sv_ser = sr_server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

displa,
	display (15, $scr_pos(1,1), $scr_clr(screen), 'ROCauto')
	display (15, $scr_pos(5,5), 'order # ')
	reads (15, a6)
	d6 = a6
	if (d6 .eq. 0) goto endoff

	read (chn044, ordhdr, d6) [err=displa]

	open (9, si, logfil)		;smcauto.ism
	find (9, smcord, d6) [err=log_ok]
	close 9
	display (15,$scr_pos(12,5), 'Order already sent to Vulcan!')
	display (15,$scr_pos(14,5), 'Do you want to re-send? ')
	reads (15, resend)
	if (resend .eq. 'Y') goto log_ok
	goto displa
log_ok,
	close 9
;;;	xcall mesag ('logfile opened',1)
	
	clear pnum

	xcall cpm (d6, cpmemo)

	CALL OPEN_WORK
	clear is_2, is_3
	
	clear ordlin
	lordno = d6
	find (chn045, ordlin, ordkey) [err=loop]
loop,
	reads (chn045, ordlin, eof)
	if (lordno .ne. d6) goto eof
	if (ltype .eq. 'M') goto loop		;skip memos
	if (lstokt .eq. 'S') goto loop		;skip stocked items
	if (lqtyor .eq. 0) goto loop		;6-22-09
	if (ocusno .eq. 5) goto skip_test	;smc, bill is using this for stock orders

	using ldept select
	('H','F','K'),	nop		;these are OK
;;;	('H','F'),	nop		;these are OK ;changed 3-19-09
	(),		goto loop	;skip
	endusing

;---------------------------------------------------
; 6-20-18 ssq:
	a5 = lf3, 'XXXXX'
	if (%instr(1, a5, '5') ) goto loop
	if (%instr(1, a5, '6') ) goto loop
	if (%instr(1, a5, '7') ) goto loop
;---------------------------------------------------

skip_test,
;;;; if salesman types info into f2-note, reject the line...
;;;	clear ma2
;;;	if (lmsq2 .gt. 0)ma2 = f2_memol(lmsq2)
;;;	if (ma(2) .ne. a30) goto loop

;----------------------------------------------
;9-21-09: also check for user typing in f1 & f3 notes...
;;;	if (lmsq1 .gt. 0)
;;;	then	ma2 = f1_memol(lmsq1)
;;;	else	clear ma2
;;;	if (ma(2) .ne. a30) goto loop

;;;	if (lmsq3 .gt. 0)
;;;	then	ma2 = f3_memol(lmsq3)
;;;	else	clear ma2
;;;	if (ma(2) .ne. a30) goto loop
;----------------------------------------------

	clear m1_note, m2_note, m3_note
; if notes are "send to vulcan = n" skip item...
        clear tblkey
        tblcod = 'M1'
        mm_key = lf1
        read (chn182, coptbl, tbl_key) [err=nk1]
        if (mm_vulc .eq. 2) goto loop
	m1_note = m1_short
nk1,
        clear tblkey
        tblcod = 'M2'
        mm_key = lf2
        read (chn182, coptbl, tbl_key) [err=nk2]
        if (mm_vulc .eq. 2) goto loop
	m2_note = m2_short
nk2,
        clear tblkey
        tblcod = 'M3'
        mm_key = lf3
        read (chn182, coptbl, tbl_key) [err=nk3]
        if (mm_vulc .eq. 2) goto loop
	m3_note = m3_short
nk3,
	org_item = lcfgim				;3-27-17 save for vt08s
	clear d_br1, d_br2				;3-27-17

;-

	if (%instr(1, lcfgim, '*'))	;is there a damper?
	then	call dampr
	else	clear dampr

	CALL GET_VULCAN_KEY

	err = 1			;will get cleared in vt routines...

	USING LITMNO SELECT
	('BN'),				CALL BN		;BULLNOSE TEE
	('C4020','C4420','C4620'),	NOP
	('C4820','C4220'),		NOP
	('C9','C4','CV9','CV4'),	CALL SHOE_TAP		;CROSS
	('CT','CC','CTV','CCV'),	CALL SHOE_TAP		;TEE	;CONICAL TEE/CROSS
	('T4','T9','TV4','TV9'),	CALL SHOE_TAP		;TEE
;;;	('ER','ERV'),			NOP;	CALL REDU	;ECCENTRIC REDUCER
	('PD'),				NOP;	CALL ELBOW	
	('EC'),				CALL END_CAP	;enabled 11-20-08
	('EG'),				CALL ELBOW	
	('EV'),				CALL ELBOW
	('AV'),				CALL ELBOW
	('GA','GE'),			CALL ROUND_ELBOW_ANGLE
	('ER16' THRU 'ER26'),		CALL ECC_REDUCER
	('ERW16' THRU 'ERW26'),		CALL ECC_REDUCER
	('R16' THRU 'R26'),		CALL ROUND_CONCENTRIC_REC
	('RW16' THRU 'RW26'),		CALL ROUND_CONCENTRIC_REC
	('RV16' THRU 'RV26'),		NOP;	CALL REDU
;;;	('RT16' THRU 'RT26'),		NOP;	CALL REG_TAKE_OFF
	('RT','RTV'),			NOP;	CALL REG_TAKE_OFF
	('SR','SRK'),			NOP;	s-t-r 9-23-13
	('STC','STT'),			CALL SHOE_TAP		;SHOE TAP CROSS
	('SC0' THRU 'SC8'),		NOP;	STORM COLLARS
	('S'),				CALL SADDLE
	('P4','P9'),			NOP;	CALL PP		;PAIR/PANTS
	('PV4'),			NOP;	CALL PP		
	ENDUSING

	if (.not. err)
		begin
		v_lordno = lordno
		v_linseq = linseq
		store (chnv, vulcli, vul_key) [err=loop]
		end
	goto loop

ELBOW,			;uses the same routine...
ROUND_ELBOW_ANGLE,
;;;	xcall mesag ('vt01',1)
	XCALL VT01S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN

ECC_REDUCER,
ROUND_CONCENTRIC_REC,
;;;	xcall mesag ('vt02',1)
	XCALL VT02S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN

SADDLE,
;;;	xcall mesag ('vt03',1)
	XCALL VT03S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN
END_CAP,
;;;	xcall mesag ('vt04',1)
	XCALL VT07S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN

SHOE_TAP,
;;;	xcall mesag ('vt08',1)
	XCALL VT08S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN

BN,
;;;	xcall mesag ('vt09',1)
	XCALL VT09S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN

REG_TAKE_OFF,
;;;	xcall mesag ('vtrts',1)
	XCALL VTRTS (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN
dampr,
	xcall getd(lcfgim, d_br1, d_br2)		;3-20-17 check for damper at taps
	dampr = 1
	len = %trim (lcfgim)
	for i from 1 thru 3
		begin
		xcall instr(1, lcfgim, '*', fl)
		if (.not. fl) exitloop
		lcfgim(fl,len) = lcfgim(fl+1, len)
		end
	return
;------------------------------------------------------

GET_VULCAN_KEY,
	USING LITMNO SELECT
	('STC'),			VDKEY = 12		;SHOE TAP CROSS
	('STT'),			VDKEY = 10		;SHOE TAP TEE
	('ST9'),			VDKEY = 8		;SHOE TAP
	('S'),				VDKEY = 5		;SADDLE
	('C9','C4','CV9','CV4'),	VDKEY = 11		;CROSS
	('CT','CTV'),			VDKEY = 10		;CONICAL TEE
	('CC','CCV'),			VDKEY = 12		;CONICAL CROSS
	('T4','T9','TV4','TV9'),	VDKEY = 9		;TEE
	('BN9'),			VDKEY = 9		;BULL NOSE TEE
	('EC'),				VDKEY = 7		;END CAP
	('EG'),				VDKEY = 13		;GORE ELBOW	
	('EV'),				VDKEY = 1		;GORE ELBOW
	('AV'),				VDKEY = 1		;GORE ANGLE
	('GA','GE'),			VDKEY = 1		;ROUND ELBOW & ANGLE
	('ER16' THRU 'ER26'),		VDKEY = 2		;GORE REDUCER
;;;	('R16' THRU 'R22'),		VDKEY = 2		;GORE REDUCER
	('R16' THRU 'R24'),		VDKEY = 2		;GORE REDUCER
;;;	('R24' THRU 'R26'),		VDKEY = 4		;GORE REDUCER
	('R26'),			VDKEY = 4		;h-dept REDUCER
	('RW16' THRU 'RW26'),		VDKEY = 3		;WELDED REDUCER
	('RV16' THRU 'RV26'),		VDKEY = 2		;GORE REDUCER
	('RT16' THRU 'RT26'),		VDKEY = 14		;REGISTER TO
	ENDUSING

	RETURN
;------------------------------------------------------
return
;------------------------------------------------------

eof,
	if (is_2 .or. is_3)	;6-2-15
		begin
		open (23,o,'\\tsclient\c\QuoteSoft\fab\vt2.bat')
		open (22,o,'\\tsclient\c\QuoteSoft\fab\vt1.bat')
		end

	if (is_2) 
		begin
		close chnw2
		vulimp = w2imp
		sp_work = W2FILE(28,35)

		writes (22, sp_file)
		call shell_batch_job		;create the vulcan job
		end

	if (is_3) 
		begin
		close chnw3
		vulimp = w3imp

		sp_work = W3FILE(28,35)

		writes (22, sp_file)

		call shell_batch_job		;create the vulcan job
		end

	if (is_2 .or. is_3)
	then	begin
		close 22		;6-10-16
		close 23		;6-10-15
		open (9, su, logfil)
		vul_ord = oordno
		if (resend.ne.'Y')store (9, smcord, vul_ord) [err = no_log]
		close 9
		end
	else	xcall mesag ('No Export was Created!!',1)
	goto endoff
no_log,
	close 9
	display (15, $scr_pos(12,5), 'Error storing smcord')
	reads (15,a6)

endoff,
	call close

	xcall flags (7000000)
	stop

shell_batch_job,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; create & ex SMCauto batch job
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	open (22,o,'\\tsclient\c\fab\vt2.bat')

;;;	writes (23, 'path=%path%;c:\Program Files\QuickPen\Vulcan')
	writes (23, 'path=%path%;c:\Program Files\Trimble\TrimbleFabShop')
;;;c:\Program Files\Trimble\Trimble®FabShop
	smcOrder = oordno
	smcName = ocusnm

	SMCrest = 'c:\QuoteSoft\fab'
	len = %trim(SMCrest)
	incr len
	smcRest(len,len) = '\'
	incr len
	smcRest(len, len+7) = vulimp
	len = len + 8
	smcRest(len,len) = '"'
	writes (23, SMCauto)

;;;	close 22

	return
;-----------------------------------------------------	

OPEN_WORK,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; OPEN WORK FILES FOR EACH PLASMA TABLE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3-12-08 ssq: pressed reducer spool file...
;2-12-09 ssq: don't want this report anymore...
;;;	open (33,o,'spl:h_red.spl')
;;;
;;;	writes (33, 'AP&T REDUCERS for "H" DEPARTMENT')
;;;	FORMS (33,2)
;;;
;;;	VLINE = 'SMC JOB#'
;;;	VLINE (10,15) = OORDNO,	'XXXXXX'
;;;	WRITES (33, VLINE)
;;;	CLEAR VLINE
;;;	forms (33,2)
;;;
;;;	p_reducer = 'Reducer'
;;;	p_qty = '   Qty'
;;;	p_f3 = 'End Treatment'
;;;	writes (33, p_list)
;;;	clear p_file_data
;;;	forms (33,1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;w3file = 'spl:w3.dat'
;;;w2file = 'spl:w2.dat'

	VLINE = '//vulcan'
	VLINE (10,15) = OORDNO,	'XXXXXX'
	
	W3ORD = d6

	OPEN (10,O,W3FILE)
	VLINE (17,26) = '26 - 22 GA'
	WRITES (10, VLINE)

	WRITES (10, '// Export from orders')
	WRITES (10,'VENDOR= "Quote Software";')
	CHNW3 = 10

	W2ORD = D6
	OPEN (20,O,W2FILE)
	VLINE (17,26) = '16 - 20 GA'
	WRITES (20, VLINE)

	WRITES (20, '// Export from orders')
	WRITES (20,'VENDOR= "Quote Software";')
	CHNW2 = 20

	vord = d6
	XCALL ISAMC (VFILE, 9, 1, 'START=1, LENGTH=9, NODUPS, ASCEND')
	OPEN (30, SU, VFILE)
	chnv = 30

	RETURN
;-----------------------------------------------------

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

	xcall files (4, 'SI', 044, SWITCH)
	if (switch .eq. 9) return

	chn044 = 4

	xcall files (5, 'SI', 045, SWITCH)
	if (switch .eq. 9) return

	chn045 = 5

	open (8, si, 'smc:coptbl.smm')
	chn182 = 8
	
	xcall ffile (44, logfil, switch)
;;;	logfil(5,10) = 'SMCAUT'
	logfil(5,10) = 'rocAUT'
	logfil(14,14) = 'M'	

	opnok = 1
	return
;---------------------------------------

close,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (chn044) close chn044
	if (chn045) close chn045
	if (chn182) close chn182
	IF (CHNV) CLOSE CHNV

	return
;---------------------------------------


