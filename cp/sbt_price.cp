function sbt_price
	oline	,a		;ordlin
	memos	,a		;f_memos
	
EXTERNAL FUNCTION
	FUNCV	,A		;FUNCTION RETURNS VIRTUAL ITEM NUMBERS FOR SPIRAL BODY TEES

EXTERNAL FUNCTION
	DAMPR	,A		;FUNCTION RETURNS LITMNO STRIPPED OF '*'


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

RECORD	F_MEMOS
	.INCLUDE 'def:CPMEMO.DEF'

record	coptbl
	.include 'def:rd182a.def'
	
record	ordlin
	.include 'def:rd045a.def'

record	itmmas
	.include 'def:rd041a.def'


record	vars
	j		,d6
	chn182	,d2,	17
	sbt_price	,d6
	c_desc		,a30	;desc for cross (sbt)
	sbt			,d1	;1=item is spiral body tee
	sbt_qty		,d6
	sbt_mat		,d1
	sbt_item	,a15
	sbt_f3		,d5
	sbt_f1idx	,d3
	sbt_idx		,d6
	sbt_lmsq1	,d2
	code	,9a3,	'MN ','SG ','R  ','SW1','SW2','C1 ','C2 ','BR1','BR2'
	mm_code	,a5
	xmcod	,a5
	blanks	,a15
	partno	,a15

;
proc
	f_memos = memos
	ordlin = oline

	sbt_price = lprice		;if not sbt will just return lprice

	call check_sb_tees
	if (.not.sbt) goto fini

	sbt_price = 0			;will be the sum of all sbt components

	f2_idx = 0
	for sbt_idx from 1 thru 6
		begin
		fv_data = funcv(sbt_item, code(sbt_idx), lf3)
			if (code(sbt_idx).eq.'SG' .or. code(sbt_idx).eq.'SW')call insert_f2
			if (code(sbt_idx).ne. 'SG')
			then	call insert_f3
			else	if (sbt_f3.eq.3) call insert_f3
		xcall g_item(f_item,itmmas,ordlin,f_memos,sbt_mat,lmsq1,f2_idx,f3_idx,f_ga,sbt_qty)
		sbt_price = sbt_price + lprice
		end


fini,
	freturn	sbt_price

;=========================================================================
; internal routines

check_sb_tees,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; is this item a spiral body tee?
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	sbt = 0
	using litmno select
	('BN'),						NOP
	('C9','C4','CV9','CV4'),	NOP
	('CT9','CC9','CTV','CCV'),	NOP
	('T4','T9','TV4','TV9'),	NOP
	('STC','STT'),				NOP
	(),							RETURN
	endusing

	if (lf2 .eq. 31) return		;custom f2 notes 11-26-18

	if (ldampr)
	then	partno = %dampr(lcfgim)
	else	partno = lcfgim

	xcall cfg2 (partno, segs)

	if (ma.gt.36 .and. (bc.ge.20 .or. bd.ge.20) ) goto sb_tee

	return

sb_tee,
	sbt_item = lcfgim
	sbt_qty = lqtyor
	sbt_mat = lmat
	sbt_lmsq1 = lmsq1

	sbt = 1

	return
;------------------------------------------------------
INSERT_F2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF(F2_NUM .GE. F_MAX) goto not_key	;too many

; check to see if key is already in array...

	mm_code(1,1) = 8,'X'			;f2 note for spiral body tee...


	for j from 1 thru f2_num
		begin
		xmcod = f2_key(j),	'ZZZZZ' [left]
	   	if (xmcod.eq.mm_code)
		  begin
		  f2_idx = j		;index to existing key
		  return		;key already in array
		  end
		end

	CLEAR TBL_KEY
	TBLCOD = 'M2'
	MM_KEY = MM_CODE
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_KEY]
	
	INCR F2_NUM
	F2_IDX = F2_NUM
	F2_MEMOL(F2_IDX) = mm_long
	F2_MEMOS(F2_IDX) =  MM_SHORT		
	F2_KEY(F2_IDX) = MM_KEY
	RETURN
;-------------------------------------------------------------


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

;;;;;;;;;;;;;;;;;;
end
