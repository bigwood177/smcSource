SUBROUTINE COMBL
	sav_com	,a

	.INCLUDE 'WND:WINDOWS.DEF'

RECORD	POP
	.INCLUDE 'DEF:POP1.DEF'

GLOBAL ORDH
RECORD	ORDHDR
	.INCLUDE 'DEF:RD044A.DEF'
ENDGLOBAL

record	funkey
	.include 'def:funkey.def'

record	coptbl
	.include 'def:rd182a.def'

RECORD	ORDCOM
	.INCLUDE 'DEF:RD075A.DEF'

RECORD	WN_NAME
		,A5,	'COMBL'
	WN_TNMBR,D4

RECORD	VARS
	bl_num	,d4	;key for bl_key
	f_key	,d3
;;;	sav_com	,a30

;;;	CHN075	,D2,	17
	CHN075	,D2,	18	;ssq 1-13-03
	chn182	,d2,	17	;opened in mainline
	BLANKS	,A30
	W_ID	,D4
	WND_1	,D4
	SAVRFA	,A6
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	A1	,A1
	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	LOKCTL	,D1
	SWITCH	,D1
	I	,D5
;
PROC
	CALL INIT_WINDOW

	SWITCH = 5
	XCALL FILES(CHN075, 'SU', 075, SWITCH)

	IF(SWITCH .EQ. 9) RETURN

	CALL GET_MEMOS
PLOOP,
	IF (NUMARA .EQ. 0) 
		BEGIN
		CALL INSERT
		READS (15, A1)
		IF(NUMARA.EQ.0) GOTO EXIT
		END
	DLINE = '  SEQ NOTES'
	XCALL W_DISP(WND_1, WD_POS,14,1,WD_CLR, WDC_EOW)
	XCALL POP1(POP)
	USING P_ACTION SELECT
	(0),	GOTO EXIT
	(1,4),	BEGIN
		CALL SELECT
		READS (15, A1)
		END
	(2),	BEGIN
		CALL INSERT
		READS (15, A1)
		END
	(3),	BEGIN
		CALL DELETE
		READS (15, A1)
		END
	ENDUSING
	GOTO PLOOP
EXIT,
	CLOSE CHN075
	XCALL W_PROC(WP_DELETE, WND_1)
	XRETURN
;================================================
SELECT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	READ(CHN075, ORDCOM, RFA:PARRFA(PI)) [ERR=LINE_ERR]
	CALL DSP_LINE

	XCALL WINPT(W_ID,14,5,30,00,'AE',ENTRY,INXCTL)
	GOTO(SELECT,LINE_ERR),INXCTL
	IF(ENTRY.EQ.BLANKS) ENTRY=OC_COM
	IF(ENTRY(1,1).EQ.'.')ENTRY=
	OC_COM = ENTRY
	xcall alpdc (oc_com(1,2), bl_num, lokctl)
	if (lokctl .eq. 0) call get_bl_notes

	sav_com = oc_com	;6-6-11
	XCALL W_DISP(WND_1,WD_POS,14,5,OC_COM)
	XCALL W_UPDT

	XCALL ISIO(CHN075,ORDCOM,COMKEY,WRITE,LOKCTL)
	IF(LOKCTL.NE.0)GOTO LINE_ERR
	CALL GET_MEMOS
LINE_ERR,
	RETURN
;-----------------------------------------------

INSERT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FOR I FROM 1 THRU NUMARA
		BEGIN
		DLINE = PARRY(I)
		XCALL W_DISP(W_ID,WD_POS,I+1,7,DLINE)
		END
	XCALL W_UPDT

	XCALL WINPT(W_ID,14,1,03,00,'#E',ENTRY,INXCTL, f_key)
	if (f_key.eq.f_01 .and. numara.eq.0) 
		begin
		oc_seq = 900
		oc_com = sav_com
		call dsp_line	;use last input
		goto storem
		end
	GOTO(INSERT,END_INS),INXCTL
	OC_SEQ = ENTRY(1,3)

	XCALL WINPT(W_ID,14,5,30,00,'AE',ENTRY,INXCTL)
	GOTO(INSERT,END_INS),INXCTL
	IF(ENTRY.EQ.BLANKS) ENTRY=OC_COM
	IF(ENTRY(1,1).EQ.'.')ENTRY=
	OC_COM = ENTRY

	xcall alpdc (oc_com(1,2), bl_num, lokctl)
	if (lokctl .eq. 0) call get_bl_notes

	XCALL W_DISP(WND_1,WD_POS,14,5,OC_COM)

storem,
	OC_ORD = OORDNO
	XCALL ISIO (CHN075,ORDCOM,COMKEY,STORE,LOKCTL)
	IF(LOKCTL.NE.0) GOTO END_INS

	sav_com = oc_com	;6-6-11

	FOR I FROM 1 THRU NUMARA XCALL W_DISP(W_ID,WD_POS,I+1,7,WD_CLR,WDC_LIN)
	XCALL W_UPDT

	CALL GET_MEMOS
END_INS,
	RETURN
;-----------------------------------------------

DELETE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	READ(CHN075, ORDCOM, RFA:PARRFA(PI)) [ERR=LINE_ERR]
	CALL DSP_LINE

	XCALL W_DISP(WND_1, WD_POS, 23, 1, 'DELETE THIS MEMO ?')
	XCALL WINPT(W_ID, 23, 23, 01, 01, 'YN', ENTRY, INXCTL)
	IF(ENTRY(1,1) .EQ. 'N') RETURN

	DELETE (CHN075) 	;DELETE THE MEMO RECORD
	CALL GET_MEMOS
	RETURN
;-----------------------------------------------

DSP_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL  W_DISP(WND_1,WD_POS,WD_CLR,WDC_EOW,'SEQ NOTE')
	ENTRY(1,3) = OC_SEQ,'ZZX' [LEFT]
	XCALL W_DISP(WND_1,WD_POS,14,1,ENTRY(1,3))
	XCALL W_DISP(WND_1, WD_POS,14,5,OC_COM)

	XCALL W_UPDT
	RETURN
;-----------------------------------------------
	
GET_MEMOS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FOR I FROM 1 THRU MAXARA
		BEGIN
		CLEAR PARRY(I)
		CLEAR PARRFA(I)
		CLEAR PFLAGS(I)
		END
	CLEAR I

	FIND(CHN075, ORDCOM, OORDNO) [ERR=M_LOOP]
M_LOOP,
	READS(CHN075, ORDCOM, M_EOF, GETRFA:SAVRFA)
	IF (OC_ORD .NE. OORDNO) GOTO M_EOF

	DLINE(1,3) = OC_SEQ,	'ZZX' [LEFT]
	DLINE(5,34) = OC_COM
	INCR I
	PARRY(I) = DLINE
	PARRFA(I) = SAVRFA
	GOTO M_LOOP
M_EOF,
	NUMARA = I
	RETURN
;------------------------------------------------
get_bl_notes,
	clear tbl_key
	tblcod = 'BL'
	bl_key = bl_num
	lokctl = 1
	xcall isio (chn182, coptbl, tbl_key, read, lokctl)
	if (lokctl .eq. 0) oc_com = bl_desc	

	return
;----------------------------------------------------------


INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,24,78)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'SELECT FOR BILLING',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1
	xcall u_logwnd(wnd_1)

;; POP info...

	MAXARA = 20		;9-19-97 NOT MORE THAN 1 FULL WINDOW
	PLEN = 36
	NUMROW = 10
	WX = 2
	WY = 7
	POP_WID(1,5) = "BLCOM"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "NOTES"
	RETURN
;-------------------------------------------------------------------
;SEQ NOTE
;ZZX AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
;12345678901234567890123456789012345678901234567890
;          1          2          3          4

