subroutine em2
	cust	,d
	w_id	,d

;
;		customer e-mail addresses
;
	.include 'wnd:windows.def'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

record	funkey
	.include 'def:funkey.def'

record	pop
	.include 'def:pop1.def'

RECORD	CHANNEL
	CHN182	,D2,	18


RECORD	WN_NAME
		,A5,	'ECKEY'
	WN_TNMBR,D4

RECORD	VARS
	wnd_1	,d4
	prim	,2a1,	'P','C'
	dprim	,d1
	OPNOK	,D1
	sav_mail	,a80
	achar	,a1
	tstat	,d1
	savrfa	,a6
	f_key	,d3
	BLANKS	,A15
	PGM	,D1
	ROW	,D2
	SKEY	,A6
	EKEY	,A9
	TL	,D2
	NUMITM	,D2
	ENTRY	,A30
	INXCTL	,D1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	DELETE	,D1,3
	LOKCTL	,D1
	WHATNO	,D2
	SELECT	,D1
	CNGCTL	,D1
	I	,D3
proc
	wnd_1 = w_id
	call init_window ; (pop1)

beg_displa,
	call getdpt
	if (i.gt.0) goto start_disp

displa,
	clear cngctl

prim,
	xcall winpt (w_id,06,33,1,1,'AE',entry,inxctl)
	goto (displa,endoff),inxctl
	using entry(1,1) select
	('P'),	dprim = 1
	('C'),	dprim = 2
	(),	goto prim
	endusing
	goto (anycng),cngctl

EKEY,
	XCALL wINPT (w_id, 8,17,08,00,'A ',ENTRY,INXCTL,f_key)
	goto (displa),inxctl
	using f_key select
	(f_01),	nop
	(exit_key),	goto endoff
	(end_key),	goto endoff
	(),		begin
			if (cngctl)
			  begin
			  xcall isio (chn182, coptbl, tbl_key, delete, lokctl)
			  end

			EKEY = ENTRY(1,8)
			if (cngctl)
			  begin
			  ec_key = ekey
			  xcall isio (chn182, coptbl, tbl_key, store, lokctl)
			  end
			goto reg
			end
	endusing

start_disp,

	NUMARA = I

	DO BEGIN
	  XCALL TTSTS (TSTAT)
	  IF (TSTAT) xcall w_disp(wnd_1,wd_accept,achar)
	END UNTIL (TSTAT.EQ.0)

	DLINE = 'Key       Email Addr'
	XCALL POP1 (POP)

	USING P_ACTION SELECT
	(1),	BEGIN
		SAVRFA = PARRFA(PI)
		READ (CHN182, COPTBL, RFA:SAVRFA) [ERR=DISPLA]
		UNLOCK CHN182
		ekey = ec_key
		dprim = ec_prim
		XCALL w_disp (w_id,wd_pos, 8,17,ekey)
		xcall w_updt
		END
	(2),	GOTO DISPLA
	(4),	begin
		call nxtdpt
		goto beg_displa
		end
	(),	GOTO ENDOFF		;NEXT ORDER
	ENDUSING

reg,
;-

	CLEAR TBL_KEY
	TBLCOD= 'EC'
	ec_CUST = CUST
	ec_prim = dprim
	ec_KEY = EKEY
	XCALL ISIO (CHN182,COPTBL,TBL_KEY,READ,LOKCTL)
	IF (LOKCTL .EQ. 0)
	then	BEGIN
		SELECT = 2
		sav_mail = ec_email
		CALL DSPREC
		GOTO ANYCNG
		END

	else	begin
		SELECT = 1			;ADD MODE
		CLEAR COPTBL
		if (cngctl)
			begin
			ec_email = sav_mail 
			XCALL w_disp (wnd_1,wd_pos, 11,1,ec_EMAIL)
			xcall w_updt
			end
		end

	goto (anycng),cngctl

EMAIL,
	XCALL wINPT (w_id,11,1,80,00,'A ',ec_EMAIL,INXCTL)
	goto (displa),inxctl
	sav_mail = ec_email

ANYCNG,
	XCALL w_disp (wnd_1,wd_pos, 24,1,wd_clr,wdc_eol,'FIELD # TO CHANGE            <TAB> = DELETE')
	XCALL wINPT (w_id,24,20,02,00,'#T',ENTRY,INXCTL)
	IF (INXCTL .EQ. 3)
		BEGIN
		XCALL w_disp (wnd_1,wd_pos, 24,1,wd_clr,wdc_lin,'DELETE, ARE YOU SURE ?')
		XCALL wINPT (w_id,24,24,01,01,'YN',ENTRY,INXCTL)
		GOTO (ANYCNG),INXCTL-1
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,DELETE,LOKCTL)
		GOTO DISPLA
		END

	WHATNO = ENTRY(1,2)
	IF (WHATNO .EQ. 0)
	THEN 	CLEAR CNGCTL
	ELSE 	CNGCTL = 1

	GOTO (PROCES,CNGBR),CNGCTL+1
CNGBR,
	GOTO (endoff, prim, EKEY, EMAIL),WHATNO
;;;	GOTO (CUST, prim, EKEY, EMAIL),WHATNO
	GOTO ANYCNG
	
PROCES,
	CASE SELECT OF
	BEGINCASE
	1:	BEGIN
		TBLCOD = 'EC'
		ec_CUST = CUST
		ec_prim = dprim
		ec_KEY = EKEY
		XCALL ISIO (CHN182,COPTBL,TBL_KEY,STORE,LOKCTL)
		END
	2:	begin
		if(ec_cust.eq.cust .and. ec_prim.eq.dprim.and.ec_key.eq.ekey)
		then	XCALL ISIO (CHN182,COPTBL,TBL_KEY,WRITE,LOKCTL)
		else	begin
		;;;	xcall isio (chn182,coptbl,tbl_key,delete,lokctl)
			delete(chn182)
			tblcod = 'ec'
			ec_cust = cust
			ec_prim = dprim
			ec_KEY = EKEY
			XCALL ISIO (CHN182,COPTBL,TBL_KEY,STORE,LOKCTL)
			end
		end
;;	2:	XCALL ISIO (CHN182,COPTBL,TBL_KEY,WRITE,LOKCTL)
	ENDCASE
	GOTO BEG_DISPLA

endoff,
	xreturn
;===========================================================


DSPREC,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; DISPLAY RECORD
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ENTRY(1,6) = ec_CUST,	'ZZZZZX' [LEFT]
	XCALL w_disp (wnd_1,wd_pos, 4,17,ENTRY(1,6))

	using ec_prim select
	(1),	nop
	(2),	nop
	(),	ec_prim = 2
	endusing

	XCALL w_disp (wnd_1,wd_pos, 6,17,prim(ec_prim))
	XCALL w_disp (wnd_1,wd_pos, 8,17,ec_KEY)
	XCALL w_disp (wnd_1,wd_pos, 11,1,ec_EMAIL)
	xcall w_updt
	RETURN
;-----------------------------------

nxtdpt,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	call clr_dpt
	goto gd_loop

getdpt,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	call clr_dpt

	clear i, tbl_key
	tblcod = 'EC'
	ec_cust = cust
;;;	ec_prim = dprim
	find (chn182, coptbl, tbl_key) [err=gd_loop]
gd_loop,
	reads (chn182, coptbl, getrfa:savrfa) [err=gd_eof]
	if (tblcod .ne. 'EC') goto gd_eof
	if (ec_cust .ne. cust) goto gd_eof
;;;	if (ec_prim .ne. dprim) goto gd_eof

	clear dline
	dline (1,1) = ec_prim
	dline (3,10) = ec_key
	dline (12,70) = ec_email

	incr i
	parry(i) = dline
	parrfa(i) = savrfa
	if (i .lt. maxara) goto gd_loop

gd_eof,
	return
;---------------------------------------

clr_dpt,	;;;;;;;;;;;;;;;;;;;;;;;;
	for i from 1 thru maxara
		begin
		clear parrfa(i), parry(i)
		end

	clear i
	return
;---------------------------------------


init_window,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POP info...

	XCALL TNMBR (WN_TNMBR)

	MAXARA = 20
	maxara = 15	;might as well be the same as numros
	PLEN = 70
	NUMROW = 15
	WX = 12
	WY = 4
	POP_WID(1,5) = "EMAIL"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "Email Addr"
	RETURN
;-----------------------------------------------------------------

