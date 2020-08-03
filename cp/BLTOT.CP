SUBROUTINE BLTOT
;
;
;		RE-TOTAL ORDER
;
	RUNTOT	,D
	TAXTOT	,D
	TOTWGT	,D
	TOTCST	,D

; 10-15-18 ssq: SBT pricing

RECORD	D_STUFF
	D_IN	,D8		;DATE-IN, ANY FORMAT
	D_OUT	,D6		;RETURN VALUE MMDDYY
	D_OUTR	,D8		;RETURN VALUE CCYYMMDD
	D_FMT	,A10		;RETURN VALUE MM/DD/CCYY
	D_SW	,A2		;"99" = DATE CONVERSION ERROR

	EXTERNAL FUNCTION
	FUNCV	,A		;FUNCTION RETURNS VIRTUAL ITEM NUMBERS FOR SPIRAL BODY TEES

record	fv_data
	f_item	,a15	;virtual item to return
	f_f3	,5d1	;
	f_cfg	,d1	;same as cfg_item in scrnx
	f_ga	,d2	;

RECORD
	SEGS	,A12			;first 2 dig are gauge
RECORD,X
	GA	,D2
	SARA	,5D2
RECORD,X
		,D2	;GA
	MA	,D2
	MB	,D2
	BC	,D2
	BD	,D2

record	coptbl
	.include 'def:rd182a.def'

record	itmmas
	.include 'def:rd041a.def'
		
RECORD	F_MEMOS
	.INCLUDE 'DEF:CPMEMO.DEF'

RECORD	ORDLIN
	.INCLUDE 'DEF:RD045A.def'
RECORD,X
	.INCLUDE 'DEF:RD045D.def'
record,x
	.include 'def:rd045m.def'
	

RECORD DUCFIL	; Table of all parameters necessary
	.INCLUDE 'DEF:RD174A.DEF'


GLOBAL ORDH
RECORD 	ORDHDR	
	.INCLUDE 'DEF:RD044A.DEF'
ENDGLOBAL

GLOBAL DUCK
	.INCLUDE 'DEF:RD175D.DEF'
ENDGLOBAL


GLOBAL CPCHAN
RECORD	CHANNEL
	CHN001	,D2
	CHN002	,D2
	CHN007	,D2
	CHN041	,D2
	CHN042	,D2
	CHN044	,D2
	CHN045	,D2
	CHN054	,D2
	CHN060	,D2
	CHN169	,D2
	CHN170	,D2
	CHN171	,D2
	CHN172	,D2
	CHN174	,D2
	CHN175	,D2
	CHN178	,D2
	CHN182	,D2
	CHN066	,D2		;SSQ 5-24-05
	CHN101	,D3		;MISCUS 12-9-19
	CHN102	,D3		;MISSHP 12-9-19
ENDGLOBAL


RECORD	VARS
	j	,d6
	sav_ltxflg	,d1
	sbt		,d1	;1=item is spiral body tee
	sbt_price	,d6
	sbt_qty		,d6
	sbt_mat		,d1
	sbt_item	,a15
	sbt_idx		,d6
	sbt_code	,6a3,	'R  ','MN ','C1 ','C2 ','BR1','BR2'
	sbt_lmsq1	,d6
	fl	,d6
	ln	,d6
	mm_code	,a5
	partno	,a15
	xmcod	,a5
	SAVV	,D1
	TCHAR	,D3
	DOTS2	,A35,	'...................................'
	LSEQ	,D3
	D2	,D2
	A2	,A2
	CHNWRK	,D3
	MULTLINE	,D1
	SAVQTY	,D5
	SAVKEY	,A63
	STAT	,D1
	ENTRY	,A35
	CNGCTL	,D1
	WHATNO	,D2
	DECMAL	,D18
	OPTION	,D1
	ABORT	,D1
	ALPHA	,A10
	COUNT	,D1
	BLANKS	,A35
	SRCCTL	,D1
	SWITCH	,D1
	READ	,D1	,0
	WRITE	,D1	,1
	LOKCTL	,D1
	FILENM	,A14	;;;
	V	,D1

PROC
	CLEAR RUNTOT, TAXTOT, TOTWGT, TOTCST

	call load_memos			;10-15-18
	
	CALL SELECT_DUCT
	CALL SELECT_LINES
	XRETURN

SELECT_DUCT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SELECT ALL DUCTWORK
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FNDDUC,
	DUCTOR = OORDNO
	FIND(CHN175,DUCACC,DUCTOR)[ERR=EOF_DUC]
NXTDUC,
	LOKCTL = 1
	XCALL IOS (CHN175,DUCACC,READ,LOKCTL)
	IF (LOKCTL.NE.0) GOTO EOF_DUC
	IF (DUCTOR.NE.OORDNO) GOTO EOF_DUC

SEL_DUC,
	DECMAL = (POUNDS*GPRICE)#1
	IF (LINER.GT.0.AND.LINER.LE.9) DECMAL = DECMAL + (SQFLIN*LINPRC)#1
	IF (ACC.EQ.1)
	BEGIN
	  IF (SLIPS)  DECMAL = DECMAL + (SLIPS*SLPPRC)#1
	  IF (DRIVES) DECMAL = DECMAL + (DRIVES*DRVPRC)#1
	  IF (TCORN)  DECMAL = DECMAL + (TCORN*CORPRC)#1
	  IF (TNBQTY) DECMAL = DECMAL + (TNBQTY*TNBPRC)#1
	  IF (TGAQTY) DECMAL = DECMAL + (TGAQTY*TGAPRC)#1
	  IF (TCLQTY) DECMAL = DECMAL + (TCLQTY*TCLPRC)#1
	  IF (TBAQTY) DECMAL = DECMAL + (TBAQTY*TBAPRC)#1
	  IF (TBNQTY) DECMAL = DECMAL + (TBNQTY*TBNPRC)#1
	END
	RUNTOT = RUNTOT + DECMAL
	TAXTOT = TAXTOT + DECMAL		;;;10/14/93 force taxable
	FLG175 = 1		;SET BILLING SELECT FLAG IN DUCACC

	LOKCTL = 1
	XCALL ISIO (CHN175,DUCACC,KEY175,WRITE,LOKCTL)
	GOTO NXTDUC

EOF_DUC,
	RETURN
;-----------------------------------------------------

SELECT_LINES,

	FIND (CHN045, ORDLIN, OORDNO) [ERR=LOOP]
LOOP,
	READS (CHN045, ORDLIN, EOF)
	IF (LORDNO .NE. OORDNO) GOTO EOF
	IF (LTYPE .EQ. 'M') GOTO LOOP
	sav_ltxflg = ltxflg
;-
; sbt ---------------------------------------------------------------------------
	call check_sb_tees		;10-15-17
	if (.not. sbt) goto no_sbt
	f2_idx = 0
	for sbt_idx from 1 thru 6
		begin
		fv_data = funcv(sbt_item, sbt_code(sbt_idx), lf3)
		if (f_item .ne. blanks)
			begin
			call insert_f3
			xcall g_item (f_item, itmmas, ordlin, f_memos, sbt_mat, sbt_lmsq1, f2_idx, f3_idx, f_ga, sbt_qty, chn041, chn182)
			if (sbt_code(sbt_idx).eq.'C1 ') lqtyor = lqtyor*2
			lqtysh = lqtyor
			ltxflg = sav_ltxflg
			call load_line
			end
		end
	goto loop

no_sbt,
	call load_line			;10-11-17
	goto loop
;-
load_line,

	DECMAL = (LQTYSH*LPRICE)#1
	RUNTOT = RUNTOT + DECMAL
	IF(LTXFLG) TAXTOT = TAXTOT + DECMAL

	TOTCST = TOTCST + (LCOST*LQTYSH)#1
	TOTWGT = TOTWGT + (LQTYSH*LITMWT)
;;	GOTO LOOP
	return
	
EOF,
	RETURN
;----------------------------------------------

INSERT_F3,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF(F3_NUM .GE. F_MAX) goto not_key	;too many

; check to see if key is already in array...

	clear mm_code
	for j from 1 thru 5 if (f_f3(j).ne.0) mm_code(j,j) = f_f3(j), 'X'

	for j from 1 thru f3_num
		begin
		xmcod = f3_key(j),	'ZZZZZ' [left]
	   	if (xmcod .eq. mm_code)
		  begin
		  f3_idx = j		;index to existing key
		  return		;key already in array
		  end
		end

	CLEAR TBL_KEY
	TBLCOD = 'M3'
	MM_KEY = MM_CODE
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_KEY]

	INCR F3_NUM
	F3_IDX = F3_NUM
	F3_MEMOL(F3_IDX) = MM_LONG
	F3_MEMOS(F3_IDX) =  MM_SHORT		
	F3_KEY(F3_IDX) = MM_KEY
	RETURN

not_key,
	;something
	return
;---------------------------------------------------------

check_sb_tees,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; is this item a spiral body tee?
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	sbt = 0
	using litmno select
	('BN'),				NOP		;BULLNOSE TEE
	('C9','C4','CV9','CV4'),	NOP	;CROSS
	('CT9','CC9','CTV','CCV'),	NOP	;CONICAL TEE/CROSS
	('T4','T9','TV4','TV9'),	NOP	;TEE
	('STC','STT'),			NOP
	(),				RETURN	; anything else
	endusing

	if (lf2 .eq. 31) return		;custom f2 notes 11-26-18

	partno = lcfgim
	if (ldampr) call damper

	xcall cfg2(partno,segs)
	if (ma .gt. 60) return		;1-3-19

	if (ma.gt.36 .and. (bc.ge.20 .or. bd.ge.20) )goto sb_tee
	return
sb_tee,
	sbt_item = lcfgim
	sbt_qty = lqtyor
	sbt_mat = lmat
	sbt_lmsq1 = lmsq1

	sbt = 1
	return
;----------------------------------------------------
damper,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ln = %trim (partno)
	for j from 1 thru 3
		begin
		xcall instr(1, partno, '*', fl)
		if (.not. fl)exitloop
		partno(fl, ln) = partno(fl+1, ln)
		end
	return
;----------------------------------------------------
LOAD_MEMOS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Read thru ordlin file, and
		;;; load memo arrays
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR F1_NUM, F2_NUM, F3_NUM
	FOR J FROM 1 THRU F_MAX
		BEGIN
		CLEAR F1_MEMOS(J)
		CLEAR F2_MEMOS(J)
		CLEAR F3_MEMOS(J)

		CLEAR F1_MEMOL(J)
		CLEAR F2_MEMOL(J)
		CLEAR F3_MEMOL(J)
		END

	FIND (CHN045, ORDLIN, OORDNO) [ERR=MLOOP]
MLOOP,
	READS (CHN045, ORDLIN, EOF_M)
	IF (LINSEQ .NE. 0) GOTO EOF_M
	IF (LMSQ1 .GT. 0) 
		BEGIN
		F1_NUM = LMSQ1
		F1_MEMOS(LMSQ1) = M_SHORTD
		F1_MEMOL(LMSQ1) = M_LDESCR
		END
	IF (LMSQ2 .GT. 0) 
		BEGIN
		F2_NUM = LMSQ2
		F2_MEMOS(LMSQ2) = M_SHORTD
		F2_MEMOL(LMSQ2) = M_LDESCR
		END
	IF (LMSQ3 .GT. 0) 
		BEGIN
		F3_NUM = LMSQ3
		F3_MEMOS(LMSQ3) = M_SHORTD
		F3_MEMOL(LMSQ3) = M_LDESCR
		END
	GOTO MLOOP
EOF_M,
	RETURN
;-------------------------------------------------

END
