;duclbl.cp
;DDEPT 5-29-14

	.include 'def:hpsub.def'

EXTERNAL FUNCTION
	TRN3	,D


	.INCLUDE 'DEF:RD175D.DEF'

RECORD	DUCWRK
	.INCLUDE 'DEF:WRKDUC.DEF'


RECORD	DUCFIL
		,A4,	'SPL:'
		,A1,	'D'
	WRORD	,A6
		,A4,	'.ISM'

RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'

RECORD PARAM
	.INCLUDE 'DEF:PARAM2.DEF'

RECORD
	MAXDUC	,D3,100
	QTY	,D5
	ITM	,A15
	DES	,A30
	PRC	,D7
	COMPA	,A15
	COMPB	,A15

RECORD ARRAYS
	SDUCRC	,100A32	;SUMMARIZED DUCT CONFIGURATIONS   (SEE DUCREC BELOW)
	LINSQF	,9D6	;SUMMARIZE LINER SQUARE FEET

RECORD DUCREC
	DUCCFG	,D7	;CONFIGURATION CODE
	DUCSQF	,D7	;SQ FEET OF MATERIAL
	DUCPND	,D7	;POUNDS OF MATERIAL
	DUCGPR	,D7	;GAUGE PRICE

RECORD ACCREC
	.INCLUDE 'DEF:ACCREC.DEF'


RECORD	CHANNEL
	CHN044	,D2
	CHN175	,D2
	CHN182	,D2
	CHNDUC	,D2


RECORD	PVARS
	CMPCOD	,A3
	A2	,A2
	PLINE	,A80
	SAVLIN	,A80
	TMPDSC	,A56
	REM	,D6
	DECMAL	,D18


RECORD	NJOINT
	JT1	,A3
		,A1,	'/'
	JT2	,A3

RECORD	KEYSP
	KEY_SPEC	,A*,	'START=1:126, LENGTH=57:2, DUPS, ASCEND'


RECORD	VARS
	MULTLINE	,D2
	ordno	,d6
	local	,d1
	a30		,a30
	prt_dept,d1	;1 = print this dept
	fnl	,d1	;if this fnote, print to local printer.
	ln	,d6
	printq	,a20
	A1	,A1
	CDPT	,50A2
	MAXC	,D2,50
	e_esc	,a1
	a6	,a6
	got_duc	,d1
	numcop	,d2	;number of copies to print
	prow	,d3
	PL_INFO	,D1
	W2	,A2
	WX_KEY	,A59
	I	,D6
	JT	,D4
	XPOL1	,A12
	XPOL2	,A12
	XDEPT	,A2
	DT	,A20
	D	,D8
	T	,D6
	SZ1	,D3
	SZ2	,D3
	CNT_SD	,D8
	CNT_TDC	,D8
	DUCT_DPT	,D1
	SAVJOINT	,D5
	SAVFEET		,D6
	SAVFLIN		,D6
	SAVLBS		,D6
	OPNOK	,D1
	WD_RFA	,A6
	CL_COUNT	,D6
	SAV_MO	,D1
	SV_NAME	,A15
	CONFIG	,D7
	J	,D5
	LL	,D2
	TL	,D2
	SEQNO	,D2
	SAVDPT	,A2
	SAVKEY	,A57
	XSAVKEY	,A59
	SAVQTY	,D5
	STAT	,D3
	READ	,D1,0
	WRITE	,D1,1
	LOKCTL	,D1
	SWITCH	,D1
	ENTRY	,A30
	INXCTL	,D1
	whatno	,d2
	CNGCTL	,D1
	V	,D1

PROC
	XCALL TERID (V)
	XCALL WHO (CMPCOD)

	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF
DISPLA,
	XCALL OUTPT (1,1,2,'PRINT DUCT LABELS',1)
	XCALL OUTPT (5,4,0,'ORDER #',1)
	XCALL INPUT (5,14,06,00,'#E',ENTRY,INXCTL,1)
	GOTO (DISPLA,ENDOFF),INXCTL
	ORDNO = ENTRY(1,6)
	
	CNGCTL = 1
	XCALL ANYCN (CNGCTL, WHATNO)
	GOTO (DISPLA),CNGCTL

	local = 1

	CALL LOAD_DUCT
	CALL CONS_DUCT

	USING CMPCOD SELECT
	('SMC'),	BEGIN
			savdpt = 'Z'
			xcall pdlbl (ordno, "SMC_FOREMAN", savdpt, ducfil)	;print the labels
	
			savdpt = 'Y'
			xcall pdlbl (ordno, "SMC_FOREMAN", savdpt, ducfil)	;print the labels
			END

	('ROC'),	BEGIN
			savdpt = 'Z'
			xcall pdlbl (ordno, "ROC_FOREMAN", savdpt, ducfil)	;print the labels
	
			savdpt = 'Y'
			xcall pdlbl (ordno, "ROC_FOREMAN", savdpt, ducfil)	;print the labels
			END
	ENDUSING

	CLOSE CHNDUC
	XCALL DELET (DUCFIL)

ENDOFF,

	XCALL FLAGS (1000000)
	STOP

;================================================


LOAD_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; LOAD DUCTWORK INTO WORKFILE
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Note: only the ductwork data is moved into the work file,
; it is assumed that all the acc info has already been moved
; to the ordlin file...

	FIND(CHN175,DUCACC,ORDNO) [ERR=LD_LOOP]
LD_LOOP,
	LOKCTL = 1
	XCALL IOS (CHN175,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0.OR.DUCTOR.NE.ORDNO) GOTO LD_EOF

	XCALL DDEPT (GAUGE,SIZE3,DUTYPE,WD_DEPT,STY)
;;;	USING STY SELECT
;;;	(1,4,5),	WD_DEPT='P'		;TDC
;;;	(2,3,6),	WD_DEPT='O'		;S&D
;;;	ENDUSING

; ssq 6-4-01 force duct work to end
	USING WD_DEPT(1,1) SELECT
	('O'),	WD_DEPT(1,1) = 'Y'
	('P'),	WD_DEPT(1,1) = 'Z'
	ENDUSING

	WDUTYPE = DUTYPE	; Type of duct 
	WSTY = 10-STY		; Style		descending
	WLINER = LINER		; Liner 
	WSEAM = SEAM		; Seam  
	WSEAL = SEAL		; Seal
	WGAUGE = GAUGE		; Gauge ( 26,24,22,20,18 )
	WSIZE3 = 100-SIZE3	; LENGTH	descending
	WSIZE1 = 1000000-SIZE1	; SLIP		descending
	WTHICK = THICK		; Dec. Thickness .024-24g .032-22g .040-20g .050-18g
	WCAT = CAT		; Catagory 
	WJOINT = JOINT		; Number of joints ( quantity)
	WSIZE2 = 1000000-SIZE2	; DRIVE		descending	

	WLOCAT = LOCAT
	WSQFEET = SQFEET
	WPOUNDS = POUNDS
	WSQFLIN = SQFLIN
	WGPRICE = GPRICE
	WLINPRC = LINPRC
	WDPUNCH = DPUNCH
	if (wdpunch .eq. '0') clear wdpunch	;ssq 6-28-05
	WDSEQ = DSEQ	
	
	STORE(CHNDUC,DUCWRK,WD_KEY)

	GOTO LD_LOOP
LD_EOF,
	RETURN
;-----------------------------------------------
CONS_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; CONSOLIDATE DUCTWORK
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR MULTLINE, SAVJOINT, SAVFEET, SAVFLIN, SAVLBS
	FIND(CHNDUC,DUCWRK,^FIRST) [ERR=EOF_CD]
	SAV_WD_KEY = '***'

CD_LOOP,
	READS(CHNDUC,DUCWRK,EOF_CD)
	IF(WD_KEY .NE. SAV_WD_KEY)
	THEN	CALL WD_NEWKEY
	ELSE	BEGIN
		INCR MULTLINE
		SAVJOINT = SAVJOINT + WJOINT
		SAVFEET = SAVFEET + WSQFEET
		SAVFLIN = SAVFLIN + WSQFLIN
		SAVLBS = SAVLBS + WPOUNDS
		DELETE(CHNDUC)
		END
	GOTO CD_LOOP		
EOF_CD,
	CALL WD_NEWKEY
	RETURN

WD_NEWKEY,
	IF(SAV_WD_KEY .EQ. '***') GOTO OUT_WDKEY
	IF(MULTLINE .EQ. 0) GOTO OUT_WDKEY
	READ(CHNDUC,DUCWRK,SAV_WD_KEY)
	WJOINT = SAVJOINT
	WSQFEET = SAVFEET
	WPOUNDS = SAVLBS
	WSQFLIN = SAVFLIN
	WRITE(CHNDUC,DUCWRK,SAV_WD_KEY)

OUT_WDKEY,
	SAV_WD_KEY = WD_KEY
	SAVJOINT = WJOINT
	SAVFEET = WSQFEET
	SAVFLIN = WSQFLIN
	SAVLBS = WPOUNDS

	CLEAR MULTLINE
	RETURN
;-----------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK
	

	SWITCH = 5
	XCALL FILES (4,'SI',044,SWITCH)
	if (switch .eq. 9) return
	CHN044 = 4

	SWITCH = 5
	XCALL FILES (26,'SI',175,SWITCH)
	if (switch .eq. 9) return
	chn175 = 26

	SWITCH = 5
	xcall files (17, 'SI', 182, switch)
	if (switch .eq. 9) return

	CHN182 = 17	;COP TABLES

	WRORD = ORDNO,'XXXXXX'
	XCALL ISAMC (DUCFIL, 111, 1, 'START=1, LENGTH=83, DUPS, ASCEND')
	OPEN (39, SU, DUCFIL)
	CHNDUC = 39


	OPNOK = 1

	RETURN
;-------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLOSE CHN044
	CLOSE CHN175
	RETURN
	END

