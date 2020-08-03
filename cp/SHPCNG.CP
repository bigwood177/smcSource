;SHPCNG.CP
;
;	LIST OF SHIP-TO'S ADDED OR CHANGED.
;	THIS REPORT WILL RE-SET SHCNGD.
;
;
RECORD	SHIPTO
	.INCLUDE 'DEF:RD171A.DEF'
;

RECORD	HDR
		,A*,	' CUST# SHIP SHIP TO ADDRESS                 TAX'


RECORD	PBUF
	PLINE	,A60

RECORD	CHANNEL
	CHN171	,D2
	CHNSPL	,D2


record	new
	n_from	,a80	;from
	n_toN	,a40
	n_toA	,a500
	n_subj	,a30	;subject
	n_body	,a600	;body
	n_img	,a100	;path to image
	n_sig	,a600	;Dianna's signature
	n_att1	,a100	;attachment 1
	n_att2	,a9	;attachment 2
	n_att3	,a9	;attachment 3
	n_att4	,a9	;attachment 4


record	dis
	ii	,d6

record	vars2
	ANS	,A1
	clctl	,d1
	redfil	,a14
	cr	,a1
	lf	,a1
	filen	,a14
	sentOK	,d18
	chmail	,d2
	t1	,d6
	t2	,d6
	tlen	,d3,	500
	trans	,a100
	pdffil	,a100
	tl	,d6
	nbl	,d3, 600		;length of n_body	
	len	,d6
	lens	,d6
	lenp	,d6

RECORD	VARS
	send	,d1,	1	;send 
	atc	,a2
	cmpny	,a3
	splfil	,a14
	OPNOK	,D1
	I	,D6
	A30	,A30
	SWITCH	,D1

;
PROC
	cmpny = 'smc'
;;;	cmpny = 'tst'	;dbg
	open (15,i,'tt:')
	display (15,$scr_clr(screen), 'SHIP TO CHANGE REPORT')

chk_cmp,
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF


	writes (14, hdr)
	clear ii
	FIND (CHN171, SHIPTO, ^FIRST) [ERR=LOOP]
LOOP,
	incr ii
	if (ii/100*100 .eq. ii) display (15,$scr_pos(1,70),dis)

	READS (CHN171, SHIPTO, EOF)
;;;	IF (SHCNGD .NE. 1) GOTO LOOP
	IF (SHCNGD .LE. 0) GOTO LOOP	;12-23-13

	PLINE ( 1,6) = SHCSNO,	'ZZZZZX'
	PLINE (8,11) = SHTONO,	'ZZZX'
	PLINE (13,42) = SHTONA
	PLINE (45,47) = SHTOTC

	using shpgm select
	('A'),	pline (50,60) = 'Added'
	('B'),	pline (50,60) = 'BOL'
	('C'),	pline (50,60) = 'Changed'
	endusing

	CALL PRINT

	FOR I FROM 1 THRU 3
		BEGIN
		IF (SHTOAD(I) .NE. A30)
			BEGIN
			PLINE (13,42) = SHTOAD(I)
			CALL PRINT
			END
		END

	CALL PRINT

	
	GOTO LOOP

PRINT,
	WRITES (CHNSPL, PLINE)
	CLEAR PLINE
	RETURN

EOF,
	IF (CHNSPL) CLOSE CHNSPL
	CLEAR CHNSPL


; spool file was created, clear ship-to change flag
	READ (CHN171, SHIPTO, ^FIRST) [ERR=CLEAR]

CLEAR,
	READS (CHN171, SHIPTO, C_EOF)
;;;	IF (SHCNGD .EQ. 1)
	IF (SHCNGD .NE. 0)
		BEGIN
		SHCNGD = 0
		WRITE (CHN171, SHIPTO, SHTKEY)
		END
	GOTO CLEAR
C_EOF,

ENDOFF,
	CALL CLOSE
	DISPLAY (15,$SCR_POS(12,5), 'PRINT REPORT (Y/N)?')
	READS (15, ANS)
	IF (ANS .EQ. 'Y') LPQUE (SPLFIL)

	n_from = 'accountsReceivable@smcduct.com'
	n_toA =  'accountsReceivable@smcduct.com;BarbR@smcduct.com'

;;;	n_from = 'DiannaH@SMCduct.com'
;;;	n_toA =  'DiannaH@smcduct.com;BarbR@smcduct.com'


	xcall getlog ('spl', trans, len)

	trans (len+1, len+11) = '\' + splfil(5,14)
	pdffil = trans
	len = %trim (pdffil)
	len = len - 2
	pdffil(len, len+2) = 'pdf'

	lens = %trim(trans)
	lenp = %trim(pdffil)
	XCALL MKPDF (trans(1,lens), PDFFIL(1,lenp) )


	n_img = 	;;;> '\\serversbs\smc\wrk\smcL.bmp'	;smc logo

	n_subj = 'Ship-To added or changed...'


	using cmpny select
	('smc'),	n_body = 'SMC - Minn.' + '<br><br>'
	('roc'),	n_body = 'SMC - Roc' + '<br><br>'
	('tst'),	begin
			n_body = 'this is as test' + '<br><br>'
			n_subj = 'this is only a test'
			end
	endusing

	n_sig = 'Night Batch Job' 	;;;>'5850 Main Street N.E.'

	
	tl = %trim(n_toA)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	xcall mail (c_mail, pline(1,44), body, pdffil,atc,atc,atc,atc,atc,atc,send)	
	xcall mail (n_toa(1,tl), n_subj, n_body, pdffil,atc,atc,atc,atc,atc,atc,send)	

;;;	xcall ntmal (n_from, n_toN, n_toA(1,tl), n_subj,
;;;&		n_body,n_img,n_sig, pdffil,sentOK)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	if (.not. sentOK) call not_sent
	xcall delet (trans(1,lens) )
	xcall delet (pdffil(1,lenp) )
	xcall delet (splfil)


	if (cmpny .eq. 'smc')
		begin
		cmpny = 'roc'
		goto chk_cmp
		end


	XCALL FLAGS (7000000)
	STOP



OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

;;;	SWITCH = 5
;;;	XCALL FILES (1,'SU',171, SWITCH)
;;;	IF (SWITCH .EQ. 9) RETURN

	using cmpny select
	('smc'),	open (1, su, 'smc:shipto.smm')
	('roc'),	open (1, su, 'roc:shipto.rom')
	('tst'),	open (1, su, 'tst:shipto.tsm')
	endusing

	CHN171 = 1
	

	splfil = 'spl:shpcng.spl'
	open (14,o, splfil)
	CHNSPL = 14

	OPNOK = 1

	RETURN
;---------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	IF (CHN171) CLOSE CHN171

	RETURN
;---------------------------------------------

;             AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
;             AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
;             AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
;ZZZZZX-ZZZZ AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA  AAA
;12345678901234567890123456789012345678901234567890123456789012345678901234567890
;         1         2         3         4         5         6         7
