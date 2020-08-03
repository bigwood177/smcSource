;jauto.cp
;
;jtest.dbl
; vtest for fab tickets
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

record	jobhdr
	.include 'def:rd071a.def'

record	joblin
	.include 'def:rd072a.def'

record	ordlin
	.include 'def:rd045a.def'
record,x
	.include 'def:rd045d.def'

record	coptbl
	.include 'def:rd182a.def'

record	cpmemo
	.include 'def:cpmemo.def'

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

record	sp_file
;;;		,a*,	'c:\fab\VulcanFileSign.exe c:\smc\spool\'
		,a*,	'c:\QuoteSoft\fab\VulcanFileSign.exe c:\smc\spool\'
	sp_work	,a8
		,a7,	'.import'


RECORD	W2FILE
		,A4,	'SPL:'	
	W2ORD	,D6
		,A2,	'-2'
		,A7,	'.import'
record,x
		,a4
	w2imp	,a8

RECORD	W3FILE
		,A4,	'SPL:'	
	W3ORD	,D6
		,A2,	'-3'
		,A7,	'.import'

record,x
		,a4
	w3imp	,a8


record	wsig
	wfile	,a19
		,a*,	'.signature'
;-----------------------------------------------------------------
record	svrsig
		,a44,	'\\server-dc01\Trimble\Quote-Vulcan\Autoroll\'
		,a15
		,a10,	'.signature'
record	svrimp,x
		,a44
	svrfil	,a15

;;;record	svrsig
;;;;;;		,a51,	'\\serversbs\apps\quote-vulcan exports\vulcan round\'
;;;		,a2,	'\\'
;;;	sv_ser	,a11	
;;;		,a40,	'\apps\quote-vulcan exports\vulcan round\'
;;;		,a15
;;;		,a10,	'.signature'
;;;record	svrimp,x
;;;		,a53
;;;	svrfil	,a15
;-----------------------------------------------------------------

record	vfile
		,a4,	'spl:'	
		,a1,	'V'
	vord	,d6
		,A4,	'.ism'

record	p_line
	vline	,a80

record	memara
	ma2	,a90
record,x
	ma	,3a30

record	smcord
	vul_ord	,d6

record	channel
	chn044	,d2
	chn045	,d2
	chn182	,d2
	chnv	,d2

record	vars
	cmpcod	,a3
	logfil	,a14	;smc:jobaut.smm
	resend	,a1
	vulimp	,a8
	opnok	,d1
	lokctl	,d1
	read	,d1,0
	len	,d6
	fl	,d6
	err	,d1
	pnum	,d6
	a5	,a5
	a6	,a6
	a30	,a30
	d6	,d6
	i	,d6
	switch	,d1
	v	,d1
;
proc
;;;	open (15,i,'tt:')
	xcall terid (v)
	display (15,$scr_clr(screen),'Jauto')
;
	call opens
	if (.not. opnok) goto endoff

	xcall who (cmpcod)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

displa,
	display (15, $scr_pos(1,1), $scr_clr(screen), 'Jauto')
	display (15, $scr_pos(5,5), 'Job # ')
	reads (15,a6)
	d6 = a6
	if (d6 .eq. 0) goto endoff

	read (chn044, jobhdr, d6) [err=displa]

	open (9, si, logfil)		;jobauto.ism
	find (9, smcord, d6) [err=log_ok]
	close 9
	display (15,$scr_pos(12,5), 'Job already sent to Vulcan!')
	display (15,$scr_pos(14,5), 'Do you want to re-send? ')
	reads (15, resend)
	if (resend .eq. 'Y') goto log_ok
	goto displa
log_ok,
	close 9

	clear pnum

;;;	xcall cpm (d6, cpmemo)

	CALL OPEN_WORK
	clear is_2, is_3
	
	clear joblin
	jlnum = d6
	find (chn045, joblin, jlnum) [err=loop]
loop,
	reads (chn045, joblin, eof)
	if (jlnum .ne. d6) goto eof
	if (jlqty .eq. 0) goto loop
;;;	if (ltype .eq. 'M') goto loop		;skip memos
;;;	if (ocusno .eq. 5) goto skip_test	;smc, bill is using this for stock orders
;;;	if (lstokt .eq. 'S') goto loop		;skip stocked items


	clear ordlin
	ldept = jhdept
	litmno = jlitm
	lcfgim = jlitm
	ldescr = jldes
	lqtyor = jlqty

	lf1 = jlif1
	lf2 = jlif2
	lf3 = jlif3

	;---------------------------------------------------
; 6-20-18 ssq:
	a5 = lf3, 'XXXXX'
	if (%instr(1, a5, '5') ) goto loop
	if (%instr(1, a5, '6') ) goto loop
	if (%instr(1, a5, '7') ) goto loop
;---------------------------------------------------

;;;	using ldept select
;;;	('H','F'),	nop		;these are OK
;;;	(),		goto loop	;skip
;;;	endusing

skip_test,
; if salesman types info into f2-note, reject the line...
;;;	if (lmsq2 .gt. 0)
;;;	then	ma2 = f2_memol(lmsq2)
;;;	else	clear ma2
;;;
;;;	if (ma(2) .ne. a30) goto loop

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
	('EC'),				CALL END_CAP
	('EG'),				CALL ELBOW	
	('EV'),				CALL ELBOW
	('AV'),				CALL ELBOW
	('GA','GE'),			CALL ROUND_ELBOW_ANGLE
	('ER16' THRU 'ER26'),		CALL ECC_REDUCER
	('ERW16' THRU 'ERW26'),		CALL ECC_REDUCER
	('R16' THRU 'R26'),		CALL ROUND_CONCENTRIC_REC
	('RW16' THRU 'RW26'),		CALL ROUND_CONCENTRIC_REC
	('RV16' THRU 'RV26'),		CALL ROUND_CONCENTRIC_REC
;;;	('RT16' THRU 'RT26'),		NOP;	CALL REG_TAKE_OFF
	('RT','RTV'),			NOP;	CALL REG_TAKE_OFF
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
	XCALL VT01S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN

ECC_REDUCER,
ROUND_CONCENTRIC_REC,
	XCALL VT02S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN

SADDLE,
	XCALL VT03S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN
END_CAP,
	XCALL VT07S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN

SHOE_TAP,
	XCALL VT08S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN

BN,
	XCALL VT09S (PNUM, ORDLIN, CHN182, CPMEMO, ERR)
	RETURN

REG_TAKE_OFF,
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
	if (p_file_data .gt. 0)
		begin
		close 33
		lpque('spl:h_red.spl')
		end

	if (is_2) 
		begin
		close chnw2
		vulimp = w2imp
		sp_work = W2FILE(5,12)
		xcall spawn (sp_file)

		svrfil = w2file(5,19)
		xcall copy (w2file, svrimp)
		wfile = w2file
		xcall copy (wsig, svrsig)
		call shell_batch_job
		end

	if (is_3) 
		begin
		vulimp = w3imp
		close chnw3
		sp_work = W3FILE(5,12)
		xcall spawn (sp_file)

		svrfil = w3file(5,19)
		xcall copy (w3file, svrimp)
		wfile = w3file
		xcall copy (wsig, svrsig)
		call shell_batch_job
		end
;-
	if (is_2 .or. is_3)
		begin
		open (9, su, logfil)
		vul_ord = jhnum
		if (resend.ne.'Y')store (9, smcord, vul_ord) [err = no_log]
		close 9
		end
	goto endoff
no_log,
	close 9
	display (15, $scr_pos(12,5), 'Error storing smcord')
	reads (15,a6)

;-


endoff,
	call close
;;;	sp_work = wrkord

	xcall flags (7000000)
	stop

OPEN_WORK,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; OPEN WORK FILES FOR EACH PLASMA TABLE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3-12-08 ssq: pressed reducer spool file...

	open (33,o,'spl:h_red.spl')

	writes (33, 'AP&T REDUCERS for "H" DEPARTMENT')
	FORMS (33,2)

	VLINE = 'SMC JOB#'
	VLINE (10,15) = jhnum,	'XXXXXX'
	WRITES (33, VLINE)
	CLEAR VLINE
	forms (33,2)

	p_reducer = 'Reducer'
	p_qty = '   Qty'
	p_f3 = 'End Treatment'
	writes (33, p_list)
	clear p_file_data
	forms (33,1)



	VLINE = '//vulcan'
	VLINE (10,15) = jhnum,	'XXXXXX'
	
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
shell_batch_job,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; create & ex SMCauto batch job
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	open (22,o,'vt2.bat')
	writes (22, 'path=%path%;%vul%')

	smcOrder = jhnum
	smcName = 'Fab Ticket'
	xcall getlog ('vulsig',SMCrest, len)
	incr len
	smcRest(len,len) = '\'
	incr len
	smcRest(len, len+7) = vulimp
	len = len + 8
	smcRest(len,len) = '"'
	writes (22, SMCauto)

	close 22
	xcall shell (0,'vt2.bat')

	return
;-----------------------------------------------------	

opens,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear opnok

	switch = 5
	xcall files (4, 'SI', 071, SWITCH)
	if (switch .eq. 9) return

	chn044 = 4

	switch = 5
	xcall files (5, 'SI', 072, SWITCH)
	if (switch .eq. 9) return

	chn045 = 5

	open (8, si, 'smc:coptbl.smm')
	chn182 = 8
	xcall ffile (44, logfil, switch)
	logfil(5,10) = 'JOBAUT'
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


