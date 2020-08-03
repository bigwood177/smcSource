;RINMNT.CP

	.INCLUDE 'WND:WINDOWS.DEF'
	.DEFINE POOLSIZE	,25000
	.DEFINE WNDCHNL		,15
	.DEFINE MAXWINS		,10

RECORD	ROCINV
	.INCLUDE 'DEF:RD091A.DEF'

RECORD	SAV045
	.INCLUDE 'DEF:RD091Z.DEF'

RECORD INVMAS
	.INCLUDE 'DEF:RD041A.DEF'
RECORD DUMINV,X
	.INCLUDE 'DEF:RD041B.DEF'
RECORD ITMIDX 
	.INCLUDE 'DEF:RD042A.DEF'

RECORD	MATERIALS
	.INCLUDE 'DEF:CFGMAT.DEF'

RECORD	POP6		;MATERIAL
	.INCLUDE 'DEF:POP6.DEF'

RECORD	POP		;INTERNAL USE
	.INCLUDE 'DEF:POP40.DEF'

RECORD	MPOP2
	.INCLUDE 'def:mpop2.DEF'
RECORD	MPOP3
	.INCLUDE 'def:mpop3.DEF'
RECORD	LPOP
	.INCLUDE 'DEF:LPOP.DEF'
RECORD	FKEYS
	.INCLUDE 'DEF:FUNKEY.DEF'
;------------------------------------
RECORD	HD_LINE
	,A*,'ITEM #          DESCRIPTION                  QTY '

RECORD	F_MEMOS
	.INCLUDE 'DEF:CPMEMO.DEF'

	; don't need this, was usning ''cpmeno.def' instead of 'def:cpmemno.def'
;;;RECORD
;;;	F1_SEQ	,10D2
;;;	F2_SEQ	,10D2
;;;	F3_SEQ	,10D2

RECORD	WRKLIN
	ARO	,A1
;;;		,A1
	REST	,A80

;-------------------------
RECORD	
	D_LONG	,A90
	D_SHORT	,A12
	D_KEY	,D5		;SSQ 7-31-03
RECORD,X
	DL_ARA	,3A30
;-------------------------


RECORD	NEWVARS
	LCFGIM	,A15
	ORGINV	,D5
	CONFIG_PRICE	,D8

RECORD	WVARS
	W_ID	,D4
	WND_1	,D4
	W1_NAME	,A6
	WND_2	,D4
	W2_NAME	,A6
	WND_4	,D4	;CNG_LI
	W4_NAME	,A6
	WND_6	,D4	;MATERIAL
	W6_NAME	,A6
	WND_M	,D4	
	WM_NAME	,A6
	WND_P	,D4	;P_MULT
	WP_NAME	,A6
	WND_D	,D4	;DAMPER
	WD_NAME	,A6
	WN_NAME	,A6,	'MAKMEM'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	NVP_MSG
		,A*,	'NOT VALID FOR PRDCAT '
	NVP	,A1

RECORD	TOFF
	RT_SZ1	,D2
		,A1,	'/'
	RT_SZ2	,D2
		,A4,	' ON '
	RT_DIA	,D2
		,A*,	'"RD '
	RT_GA	,D2
		,A*,	'GA '
	RT_REST	,A10	
	
RECORD TEEDAT
	TGUAG	,D2
	TMAIN	,D2
	TREDU	,D2
	TBRAN	,D2
	TDESCR	,A30

RECORD	SADDLE
	SANGL	,A2
	SGUAG	,A2
	SBRAN	,A2
	SMAIN	,A2
		,A1	;IN CASE "DAMPER" (*)
RECORD,X
		,A4	;ANGLE, GAUGE
	STAR	,A1	;FOR DAMPER
	ST_BRAN	,A2
	ST_MAIN	,A2

RECORD CTL
	ROW	,D2
		,A1
	COL	,D2
		,A1
	MAX	,D2
		,A1
	MIN	,D2
		,A1
	TYPE	,A2

RECORD	VDAMPR
	DAMP		,D2	;DAMPER DIMENSIONS
	DAMPER_PRICE	,3D8
	SAVE_DP		,3D8
	FL	,D2	;
	MAXFLEN	,D2
	BAD_DIGIT	,D1
	PARTNO	,A15
	PFF	,A15

RECORD	W2_LINE
	W2_ITEM		,A15
			,A1
	W2_DESCR	,A30
	W2_QTY		,A6
			,A1
	
RECORD	PVARS
	IVFLAG	,D1
	LI		,D2
	VAL		,D1
	SAVE_ROW	,D2
	FIRST_ROW	,D2
	LAST_ROW	,D2
	WROW		,D2
	FROW		,D2	;FOR <F5> MEMOS
	WLEN		,D2
	TITLEN		,D2
	STAT_KEY	,D2
	STAT		,D2
	DEC		,D18

RECORD	CHANNEL
	CHN182	,D2
	CHN045	,D2

RECORD	VARS
	IS_15	,D1
	RI	,D6
	A15	,A15
	F6A	,4A4,	'GALV','ALUM','SST ','PGRP'
	A4	,A4
	A2	,A2
	D2	,D2
	BAD_MAT	,D1			;INVALID PART FOR THIS MAT'L
	MAT_SEL	,D1			;MATERIAL SELECTED
	N_NUM	,D10
	N_DIG	,D1
	D_SQFT	,D6
	D_HV	,A1	;(Y/N)
	D_FM	,A1
	D_SL	,A1
	D_CNGBR	,D1
	DDIM	,D3
	TL	,D6
	TEMP	,D6
	TEMP2	,D6
	QLEN	,D3
	QHT	,D3
	DQTY	,D3
	CFG_ON	,D1
	CFG_ITEM	,D1	;1=CONFIGURABLE ITEM, 2=REGULAR ITEM
	ENTDES	,A30	;OVER-RIDE DESCRIPTION
	ABSQTY	,D6
	RTLEN	,D2
	RTVAL	,A6
	VYNL	,D1
	X_MEMO	,A90
	SAV_OP	,D8
	SAVPRC	,D8
	TODAY	,D8
	SEARCH_ON	,D1	;LINE ITEM SEARCH FLAG
	F5_POP_ON	,D1
	F7_ON		,D1
	S_ITEM	,A15		;SEARCH ITEM
	SL	,D2		;SEARCH LENGTH
	XROW	,D2
	XROW2	,D2
	CMTFLG	,D1
	COMIT	,D1,0
	UN_COMIT,D1,1
	CNG_LI	,D1
	NEWITEM	,D1		;new item# when changing line items
	P_INS		,D1,2
	P_DEL		,D1,3
	P_UPD		,D1,4
	P_PG_UP		,D1,5
	P_PG_DN		,D1,6
	LINRFA		,A6
;;;	MM_CODE		,D2
	MM_CODE		,D5		;ssq 7-31-03
	V_LEVEL		,D1
	KEY_FOUND	,D1
	CLVL1		,D2
	CLVL2		,D2
	CLVL3		,D2
	SAVL1		,D2
	SAVL2		,D2
	SAVL3		,D2
	F_KEY	,D3
	T_REC	,D5
	VINYL		,D1
	CONFIG_ERROR	,D1
	A_GUAGE		,A2
	ST_GUAGE	,A2
	IS_DAMPER	,D1
	PIDX	,D5
	I	,D5
	J	,D5
	X	,D5
	Y	,D5
	NUMITM	,D2
	JUSTIF	,D1
	WRKDAT	,D6
	ROWX	,D2
	ROW2	,D2
	OPTION	,D1
	ENTRY	,A36
	INXCTL  ,D1
	CNGCTL	,D1
	WHATNO	,D2
	DECMAL	,D18
	KEY   	,A15
	BSEND	,D5
	BSMID	,D5
	BSAVE	,D5
	SRCCTL	,D1
	ORDNOA	,A6
	ODISCA	,A2
;;;	SAVEL	,D2
	RECNO	,D5
	TCHAR	,D3
	DCHAR	,D3
	CTR	,D2
	MSG	,A17
	ALPHA	,A10
	BLANKS	,A25
	BLANK30	,A30
	BLANK1	,A80
	BADBIL	,D1
	BRACKS	,A15,	']]]]]]]]]]]]]]]'
	READ	,D1,	0	;USED FOR IO SUBROUTINE
	WRITE	,D1,	1	;USED FOR IO SUBROUTINE
	STORE	,D1,	2	;USED FOR ISIO SUBROUTINE
	LOKCTL	,D1
	SAVROW	,D2
	SAVITM	,A15	;SAVE THE LAST ITEM NUMBER *** NOTE SIZE ***
	SYSTEM	,D1
	DISDAT	,A8
	DPND	,D6
	DSIZ	,D3
	DGA	,D2
	RA_SIZE	,D3		;R_ANGLE SIZE
	SP_SIZE	,D3		;SPIRAL SIZE
	WTPIN	,D6
	PRPPN	,D6
	WRANGL	,A12
	INCH	,D3
	DASH	,D2
	BADTPR	,D1
	NUMASK	,A7,'ZZ,ZZZ-'
	KITMNO	,A15
	TONH	,D8
	TONO	,D8
	TCOM	,D8
	TREO	,D8
	TREC	,D5
	SWITCH	,D1
	V	,D1
;
.PROC
	XCALL TERID(V)
	XCALL ENVRN (SYSTEM)
	XCALL RDAT8(TODAY)

OPEN_WND,
	CALL INIT_WINDOW
	CALL OPENS		;SET CH #'S
	CALL SET_X		;DISPLAY HEADER, INIT VARS

RE_START,
	CALL FIND_LINE_ITEMS

NEW_ITEM,	;;;;;;;;;;;;;;;;;;;;;
		;;; NOW A ROUTINE
		;;;;;;;;;;;;;;;;;;;;;

BEGIN,
	UNLOCK 1
	ROW = XROW
	ROW2 = XROW2
	CLEAR NEWITEM

ITEM,
	ROCINV =
	IF (CLVL3.GT.0)			;SSQ 9-9-03
	then	BEGIN
		IF (F3_KEY(CLVL3).EQ.15)
		THEN	IS_15 = 1
		ELSE	IS_15 = 0
		END
	else	is_15 = 0
	
;;;	XCALL W_DISP(WND_P, WD_CLEAR)
;;;	XCALL W_PROC(WP_REMOVE, WND_P)
	XCALL W_UPDT

	CLEAR CFG_ON
	CLEAR BADTPR, DASH, INCH
	CLEAR ST_GUAGE, SEARCH_ON

	XCALL W_DISP(WND_1, WD_POS, ROW, 1, WD_CLR, WDC_LIN)
	XCALL W_DISP(WND_1, WD_POS, ROW+1, 1, WD_CLR, WDC_LIN)
	XCALL W_DISP(WND_1, WD_POS, 24,1,
&	'<F1>=Memo1, <F2>=Memo2, <F3>=Memo3, <F5>=Change, <F6>=MATERIAL, <TAB>=Search')
;;;xcall w_disp(wnd_1,wd_pos,25,1,'this space for rent')

	CTL (4,14) = '01,15,00,AX'
	CALL INPUT
	IF (INXCTL) CALL CLEAR1
	IF (F5_POP_ON)
	THEN	GO TO (CLRDTA,ENDITM), INXCTL
	ELSE	GO TO (CLRDTA,ENDITM,TABNXT), INXCTL

	USING F_KEY SELECT
	(F_01),		BEGIN
			CALL F1_POP
			GOTO ITEM
			END
	(F_02),		BEGIN
			CALL F2_POP
			GOTO ITEM
			END
	(F_03),		BEGIN
			CALL F3_POP
			GOTO ITEM
			END
	(F_05),		BEGIN
			IF (F5_POP_ON .EQ. 0)	CALL F5_POP
			GOTO ITEM
			END
	(F_06),		BEGIN
			CALL MATL
			GOTO ITEM
			END
	ENDUSING

	IF (ENTRY .EQ. BLANKS)
	THEN	BEGIN
		CLEAR NEWITEM
		IF (CNG_LI)
		THEN	BEGIN
			ROCINV = SAV045
			XCALL W_DISP(W_ID, WD_POS,ROW,1,R_ITEM)
			KITMNO = R_ITEM		;ssq 9/23/02
			CALL CHK_F6_MATL		;SSQ 5-14-03
			GOTO (ITEM), BAD_MAT
			GOTO CLI_DESC
			END
		ELSE	IF (CNGCTL)
			THEN	BEGIN
				CALL LSTITM
				NEWITEM = 1	;ELSE WON'T GET CAT & DEPT
				END
			ELSE	GOTO ITEM
		END
	ELSE	NEWITEM = 1
	
	IF (JUSTIF) XCALL FRMAT (ENTRY(1,15),15)
	XCALL W_DISP(W_ID, WD_POS,ROW,1,ENTRY(1,15))
	R_ITEM = ENTRY (1,15)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7-29-04 ssq moved here in case damper...
	LCFGIM = R_ITEM			;SSQ 7-29-03 SAVE ITEM#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CALL FIND_DAMPER
	GOTO (ITEM),BAD_DIGIT

	IF (R_ITEM.EQ.BLANKS) GO TO ITEM

;;;	LCFGIM = R_ITEM			;SSQ 7-29-03 SAVE ITEM#
	KITMNO = R_ITEM					;;;

	CLEAR TDESCR, CONFIG_ERROR
	SP_SIZE = 1				;in case sph

	USING R_ITEM SELECT
	('WR'),				CFG_ITEM = 1
	('R16' THRU 'R26'),		CFG_ITEM = 1
	('RV16' THRU 'RV26'),		CFG_ITEM = 1
	('T4','T9','TV4','TV9'),	CFG_ITEM = 1
	('C4020','C4420','C4620'),	CFG_ITEM = 2	;SSQ 10-14-03
	('C4820','C4220 '),		CFG_ITEM = 2	;SSQ 10-14-03
	('C9','C4','CV9','CV4'),	CFG_ITEM = 1
	('CT','CC','CTV','CCV'),	CFG_ITEM = 1
	('ER'),				CFG_ITEM = 1
	('BN'),				CFG_ITEM = 1	
	('P4','P9','PV4'),			CFG_ITEM = 1
	('S9','S4','SV','SW','SC','ST'),	CFG_ITEM = 1
	('SG','SX'),			CFG_ITEM = 1
	('RT','RTV'),			CFG_ITEM = 1
	('OB','PB','SBD','BDD'),	
					BEGIN
					CFG_ITEM = 1
					GOTO CALL_CONFIG
					END
	('SLF','SLB'),			BEGIN		;LOUVERS
					CFG_ITEM = 1
					GOTO CALL_CONFIG
					END
	(),				CFG_ITEM = 2	;item not configurable
	ENDUSING
	
	GOTO (REG_ITEM),CFG_ITEM -1

;------------------------------------------------------------
;SSQ 2-17-03
; if configurable item but material is not galv, cannot be
; stamped part, so don't look up in itmmas - go straight to config...

	IF(CFG_ITEM.EQ.1 .AND. MAT_SEL.NE.1) GOTO CALL_CONFIG
;------------------------------------------------------------

	CALL GETITM			;check if cfg item is in itmmas...
	GOTO (SKIP_CFG,CALL_CONFIG,ITEM),IVFLAG+1	;(found,not-found,abort)
CALL_CONFIG,
	CALL CONFIG			;call the configurator
	GOTO (ITEM), CONFIG_ERROR

REG_ITEM,
	CALL GETITM
	GOTO (BADITM, ITEM, ITEM),IVFLAG

SKIP_CFG,
	IF (IS_15)	USING PRDCAT SELECT	;SSQ 9-9-03
			('C' THRU 'H'),	NOP
			('K1','K3'),	NOP
			(),	BEGIN
				XCALL OLMSG(WND_1, 23,'INVALID EZ FLANGE ITEM',2)
				GOTO ITEM
				END
			ENDUSING

	COL = 1
	R_ITEM = ITEMNO
	SAVITM = ITEMNO
	XCALL W_DISP(W_ID, WD_POS,ROW,COL,ITEMNO)

	XCALL W_DISP(W_ID, WD_POS,15,1, WD_CLR, WDC_LIN)
	IF (R_ITEM.EQ.'???') GO TO ENTDES
	IF (R_ITEM.EQ.'M              ') GOTO ENTDES	;moved here 8-15-01 ssq
	ROW2 = (ROW+1)

	IF (TDESCR .EQ. BLANKS)		;was desc created in configurator ?
	THEN	R_DESC = DESCR
	ELSE	R_DESC = TDESCR

	CALL CHK_F6_MATL		;SSQ 4-30-03
	GOTO (ITEM), BAD_MAT

	IF (R_ITEM.EQ.'WR')
	BEGIN
	  R_DESC = DESCR
	  IF (DASH.OR.INCH) R_DESC(1,8) = WRANGL(1,7)		;;;
	  IF (DASH) R_DESC(DASH,DASH) = '/'		;;;
	  IF (INCH) R_DESC(INCH,INCH) = '"'		;;;
	END


CLI_DESC,
	XCALL W_DISP(W_ID, WD_POS, ROW2, 3, R_DESC)
	XCALL W_UPDT
	COL = 03
BADCHR,
	SAVROW = ROW
	ROW = ROW + 1
	CALL ACCEPT
	ROW = SAVROW
	IF (TCHAR.EQ.13) GOTO QNTITY	;RETURN TYPED -- ITEM & DESCRIPTION OK
	IF (TCHAR.EQ.09) GOTO TABNXT	;TAB TYPED -- RETREIVE NEXT ITEM
	IF (TCHAR.EQ.21) GOTO ENTDES	;CNTRL/U TYPED -- ENTER DESCRIPTION
	IF (TCHAR.EQ.23) CALL CLEAR1
	IF (TCHAR.EQ.23) ROW2 = ROW2 - 1
	IF (TCHAR.EQ.23) GOTO CLRDTA	;CNTRL/W TYPED -- CLEAR LINE
	GOTO BADCHR

TABNXT,
;---------------------------------------
	XCALL W_DISP(WND_1, WD_POS, 24, 1, WD_CLR, WDC_LIN, 'SEARCH KEY')
	XCALL WINPT (WND_1, 24, 13, 15, 00, 'AE', ENTRY, INXCTL)
	GOTO (TABNXT, ITEM), INXCTL
	S_ITEM = ENTRY(1,15)
	SL = %TRIMZ(S_ITEM)
	IF (SL .EQ. 0) GOTO TABNXT

	SEARCH_ON = 1
;;;	XCALL TTSTS(STAT)
;;;	IF (STAT)  XCALL W_DISP(WND_1,WD_ACCEPT,ENTRY)
	reads (15,entry)

	CALL F5_POP
	GOTO TABNXT
;---------------------------------------

LSTITM,
	ENTRY = SAVITM
	XCALL W_DISP(W_ID, WD_POS,ROW,1,SAVITM)
	RETURN
ACCEPT,
	XCALL FLAGS (00010000,1)
	XCALL W_DISP(W_ID, WD_ACCEPT, TCHAR)
	IF (TCHAR.EQ.10) GOTO ACCEPT
	IF (SYSTEM.NE.1 .AND. TCHAR.EQ.13) ACCEPT (15,DCHAR)
	XCALL FLAGS (00010000,0)
	RETURN
ENTDES,
	SAVROW = ROW
	ROW = ROW + 1
	CTL (4,14) = '03,30,01,A '
	CALL INPUT
	IF (INXCTL) CALL CLEAR2
	IF (INXCTL.AND.R_ITEM.NE.'???') ROW2 = ROW2 - 1
	GO TO (CLRDTA), INXCTL
	IF (ENTRY.EQ.BLANKS .AND. CNG_LI) ENTRY = Z_R_DESC

	R_DESC = ENTRY
	ENTDES = ENTRY
	XCALL W_DISP(W_ID, WD_POS,ROW,3,WD_CLR, WDC_LIN ,R_DESC)
	XCALL W_UPDT
	ROW = ROW - 1
;;;
	IF (R_ITEM.EQ.'M              ')
	BEGIN
	  ROW2 = ROW2 + 1
	  R_QTYH = 1
	  GOTO ISLNOK
	END

	IF (CFG_ON.EQ.0 .AND. CFG_ITEM.EQ.1)
		BEGIN
		CALL CONFIG	;CONFIGURE PART
		GOTO (ITEM),CONFIG_ERROR
		END

QNTITY,
	CTL (4,14) = '44,05,00,#-'		;;;ALLOW NEGATIVE QUANTITY
	CALL INPUT
	IF (INXCTL.AND.R_ITEM.NE.'???') ROW2 = ROW2 - 1
	IF (INXCTL) CALL CLEAR1
	GO TO (CLRDTA), INXCTL
	R_QTYH = ENTRY
ISLNOK,
	CNGCTL = 2
	XCALL WANCN(WND_1, 24, CNGCTL, WHATNO)
	IF (R_ITEM.EQ.'???') GO TO ANYCN2
ANYCN1,
	IF (CNGCTL) XCALL W_DISP(W_ID, WD_POS,ROW2,1,WD_CLR, WDC_LIN)
	IF (CNGCTL) ROW2 = ROW2 - 1
	ROW = ROW2
	GO TO (LINEOK), CNGCTL+1
	CALL CLEAR1
	GO TO CLRDTA
ANYCN2,
	ROW2=ROW2 + 1
	XCALL W_DISP(W_ID, WD_POS,ROW2,1,WD_CLR, WDC_LIN)
	ROW2 = ROW2 - 1
	GO TO ANYCN1

LINEOK,
;--------------------------------------------------------------------------
; if a new item # was NOT entered, no ITMMAS record was read, 
; skip any updates from ITMMAS to ROCINV, the orginal ROCINV
; values were restored to the new line item in code above (ROCINV = SAV045).

	IF (NEWITEM .EQ. 0) GOTO NOT_NEWLINE
;--------------------------------------------------------------------------

NOT_NEWLINE,			;LABEL MOVED HERE 5-10-00, price changed

	IF (CLVL1.LT.0) CLVL1 = -CLVL1
	IF (CLVL2.LT.0) CLVL2 = -CLVL2
	IF (CLVL3.LT.0) CLVL3 = -CLVL3

	R_F1KEY = CLVL1
	R_F2KEY = CLVL2
	R_F3KEY = CLVL3

	IF (CNG_LI) RETURN

	CLEAR CNG_LI
ADDLIN,
	LOKCTL = 1
	R_MAT = MAT_SEL
	XCALL ISIO (5,ROCINV,R_KEY,STORE,LOKCTL)
	IF (LOKCTL.EQ.4) GO TO DUPLIC
	IF (LOKCTL.EQ.5) GO TO FULL
;----------------------------------------------------
	XCALL W_AREA(WND_2, WA_SCROLL, WAS_UP, 1)
	CALL MAKE_W2
	XCALL W_DISP(WND_2, WD_POS, 14, 1, W2_LINE)
	XCALL W_UPDT
;----------------------------------------------------

	GOTO BEGIN

;--------------------------------------------------

GETITM,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; NOW A ROUTINE, LOOK UP ITEM IN ITMMAS...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR IVFLAG

; read itmmas based on partial key for matching items below...
	USING R_ITEM SELECT
	  ('X'),		BEGIN
				KEY = R_ITEM(1,3)
				KEY(4,4) = '0'
				CALL GI_TEMP		;READ ITMMAS
				END
	  ('CM'),		BEGIN
				KEY = R_ITEM(1,3)
				CALL GI_TEMP
				END
	  ('CR0' thru 'CR9'),	BEGIN
				KEY = R_ITEM(1,4)
				CALL GI_TEMP		;READ ITMMAS
				END
	  ('EDS0' THRU 'EDS9'),	BEGIN
				KEY = R_ITEM(1,9)
				CALL GI_TEMP		;READ ITMMAS
				END
	ENDUSING

;read the actual itmmas record to get correct description...
	KEY = R_ITEM
	XCALL SERCH (2,ITMIDX,KEY,1,15,ORGINV,BSMID,SRCCTL,4,16,20,0,0,0,0)
	GO TO (GI_BAD), SRCCTL
	BSAVE = BSMID		;SAVE BSMID IN CASE TAB IS USED FOR NEXT ITEM
READIV,
	LOKCTL = 0
	XCALL IO (1,INVMAS,IRC041,READ,LOKCTL)
	IF (LOKCTL.EQ.1) GO TO GI_ITEM		;ABORT SELECTED
	RETURN
GI_BAD,
	IVFLAG = 1
	RETURN
GI_ITEM,
	IVFLAG = 2
	RETURN
;-----------------------------------------------------------------

GI_TEMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL SERCH (2,ITMIDX,KEY,1,15,ORGINV,BSMID,SRCCTL,4,16,20,0,0,0,0)
	GO TO (GIT_BAD), SRCCTL
	LOKCTL = 1
	XCALL IO (1,INVMAS,IRC041,READ,LOKCTL)
	TREC = IRC041
	RETURN

GIT_BAD,
	IVFLAG = 1
	RETURN
;-----------------------------------------------------------------

DUPLIC,
	XCALL OLMSG
&	(WND_1, 23,'**RECORD NOT ADDED** ALREADY ADDED BY ANOTHER USER',1)
	GO TO BEGIN
FULL,
	XCALL OLMSG
&	(WND_1, 23,'**RECORD NOT ADDED**THE "ROCINV" FILE IS NOW FULL',1)
;;;	FULL = 1
	RETURN
TOOBIG,
	XCALL OLMSG(WND_1, 23,'TOTAL SALE OVER 1 MILLION DOLLARS',1)
	CALL CLEAR1
	ROW2 = ROW2 - 1
	GO TO CLRDTA
BADITM,
	XCALL OLMSG(WND_1, 23,'ITEM NOT ON FILE',1)
	GO TO ITEM
CLRDTA,
	XCALL W_DISP(WND_1, WD_POS, 15, 1, WD_CLR, WDC_LIN)
	XCALL W_DISP(WND_1, WD_POS, ROW, 1, WD_CLR, WDC_LIN)
	ROCINV =
	GO TO BEGIN

ENDITM,
	IF (CNG_LI) RETURN

;write memos to ROCINV here...

;remove all windows...
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (WND_1)	XCALL W_PROC(WP_DELETE, WND_1)

	XCALL W_PROC(WP_FIND,WND_2,W2_NAME)
	IF (WND_2)	XCALL W_PROC(WP_DELETE, WND_2)

	XCALL W_PROC(WP_FIND,WND_4,W4_NAME)
	IF (WND_4)	XCALL W_PROC(WP_DELETE, WND_4)

	XCALL W_PROC(WP_FIND,WND_M,WM_NAME)
	IF (WND_M)	XCALL W_PROC(WP_DELETE, WND_M)

;;;	XCALL W_PROC(WP_FIND,WND_P,WP_NAME)
;;;	IF (WND_P)	XCALL W_PROC(WP_DELETE, WND_P)

	XCALL W_PROC(WP_FIND,WND_6,WP_NAME)
	IF (WND_6)	XCALL W_PROC(WP_DELETE, WND_6)

	XCALL W_PROC(WP_FIND,POP_WND2,'F2WIN')
	IF (POP_WND2)	XCALL W_PROC(WP_DELETE, POP_WND2)

	XCALL W_PROC(WP_FIND,POP_WND3,'F3WIN')
	IF (POP_WND3)	XCALL W_PROC(WP_DELETE, POP_WND3)

	XCALL W_UPDT

	CALL CLOSE
	STOP
;;;	XRETURN			;RETURN TO MAINLINE
;===============================================================


;=================================================================
INPUT,
	XCALL WINPT(W_ID,ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,F_KEY)
	RETURN
DSP3DP,				
	OPTION = 4		
	GOTO CALDSP		
DSPNUM,
	OPTION = 1
	GOTO CALDSP
DSPDTE,
	OPTION = 2
	GOTO CALDSP
DSPDLR,
	OPTION = 3
CALDSP,
	XCALL WDSPL(W_ID, MAX, ROW, COL, DECMAL, OPTION)
	RETURN


CLEAR1,
	ROWX = ROW + 2
	XCALL W_DISP(W_ID, WD_POS, ROWX,1, WD_CLR, WDC_LIN)
	ROWX = ROW + 1
	XCALL W_DISP(W_ID, WD_POS,ROWX,1, WD_CLR, WDC_LIN)
	XCALL W_DISP(W_ID, WD_POS,ROW,1, WD_CLR, WDC_LIN)
	RETURN
CLEAR2,
	ROWX = ROW + 1
	XCALL W_DISP(W_ID, WD_POS,ROWX,1, WD_CLR, WDC_LIN)
	XCALL W_DISP(W_ID, WD_POS,ROW,1, WD_CLR, WDC_LIN)
	ROWX = ROW - 1
	XCALL W_DISP(W_ID, WD_POS,ROWX,1, WD_CLR, WDC_LIN)
	RETURN
CLEAR3,
	XCALL W_DISP(W_ID, WD_POS,ROW,1, WD_CLR, WDC_LIN)
	ROWX = ROW - 1
	XCALL W_DISP(W_ID, WD_POS,ROWX,1, WD_CLR, WDC_LIN)
	ROWX = ROW - 2
	XCALL W_DISP(W_ID, WD_POS,ROWX,1, WD_CLR, WDC_LIN)
	RETURN

CONFIG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; BRANCH TO PRODUCT CONFIGURATORS
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	TDESCR=R_DESC			;SAVE IN CASE USER HAS CHANGED
	IF(CFG_ON.EQ.1) RETURN		;ALREADY CONFIGRED
	CFG_ON = 1

	USING R_ITEM SELECT		;SSQ 5-27-03
	('SG','SX'),	IF(%TRIM(R_ITEM) .LT. 7) RETURN
	(),		IF(%TRIM(R_ITEM) .LT. 5) RETURN
	ENDUSING
;;;	IF(%TRIM(R_ITEM) .LT. 5) RETURN	;ASSUME CHANING CFG ITEM (USING <F5>)

	CLEAR CONFIG_ERROR

	USING R_ITEM SELECT
	('C4020','C4420','C4620'),	RETURN	;SSQ 2-19-03
	('C4820','C4220 '),		RETURN	;SSQ 2-19-03
	('WR'),				CALL R_ANGLE
	('R16' THRU 'R26'),		CALL CALL_CFG
	('RV16' THRU 'RV26'),		CALL CALL_CFG
	('T4','T9','TV4','TV9'),	CALL CALL_CFG
	('C9','C4','CV9','CV4'),	CALL CALL_CFG
	('CT','CC','CTV','CCV'),	CALL CALL_CFG
	('ER'),				CALL CALL_CFG
	('BN'),				CALL CALL_CFG	
	('P4','P9','PV4'),			CALL CALL_CFG

	('S9','S4','SV','SW','SC','ST'),CALL CALL_CFG
	('SG','SX'),			CALL SPIRAL
	('RT','RTV'),			CALL R_TAKEOFF
;;;	('OB','PB','SBD','BDD'),	CALL CFG_DAMPER	;BALANCING DAMPER
;;;	('SLF','SLB'),			CALL CFG_DAMPER	;LOUVERS
	ENDUSING
	RETURN
;----------------------------------------------------

CALL_CFG,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; internal routine so I can see parameter list...
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL CFG(R_ITEM,CONFIG_PRICE,TDESCR,CONFIG_ERROR,MAT_SEL,ST_GUAGE)
	IF (CONFIG_ERROR) XCALL OLMSG(WND_1, 23,'INVALID CONFIGURATION',2)
;;;	IF (CONFIG_ERROR .EQ. 1) XCALL OLMSG(WND_1, 23,'INVALID CONFIGURATION',2)
	RETURN
;----------------------------------------------------

;====================================================
;----------- PRODUCT CONFIGURATOR ROUTINES ----------
;====================================================


R_TAKEOFF,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	USING R_ITEM(3,3) SELECT
	('V'),		BEGIN			;VINYL
			RTVAL = R_ITEM(4,9)
			CLEAR R_ITEM(4,15)
			VYNL = $TRUE
			END
	('.'),		RETURN			;"RT." is a valid part #
	(),		BEGIN			;NOT VINYL
			RTVAL = R_ITEM(3,8)
			CLEAR R_ITEM(3,15)
			VYNL = $FALSE
			END
	ENDUSING

	RTLEN = %TRIM(RTVAL)
	IF(RTLEN .LT. 6) GOTO R_TAKEOFF_ERROR

	ONERROR R_TAKEOFF_ERROR
	RT_SZ1 = RTVAL(1,2)
	RT_SZ2 = RTVAL(3,4)
	RT_DIA = RTVAL(5,6)
	OFFERROR

	IF(RT_SZ2 .GT. RT_DIA) GOTO R_TAKEOFF_ERROR

; find the greater of SZ1 & SZ2 to determin the gauge...
	RTLEN = RT_SZ1
	IF(RT_SZ2 .GT. RTLEN) RTLEN = RT_SZ2

	IF (VYNL .EQ. $TRUE)
	THEN	BEGIN
		USING RTLEN SELECT
		(.GE. 25),	RT_GA = 20
		(17 THRU 24),	RT_GA = 22
		(1 THRU 16),	RT_GA = 24
		(),		GOTO R_TAKEOFF_ERROR
		ENDUSING
		IF (MAT_SEL.NE.GALV .AND. RT_GA.GT.24) RT_GA = 24
		R_ITEM(4,5) = RT_GA,	'XX'
		RT_REST = 'PVS SADDLE'
		END
	ELSE	BEGIN
		USING RTLEN SELECT
		(.GE. 31),	RT_GA = 22
		(13 THRU 30),	RT_GA = 24
		(1 THRU 12),	RT_GA = 26
		(),		GOTO R_TAKEOFF_ERROR
		ENDUSING
		IF(MAT_SEL.NE.GALV .AND. RT_GA.GT.24) RT_GA = 24
		R_ITEM(3,4) = RT_GA,	'XX'
		RT_REST = 'REG. T.O.'
		END

	TDESCR = TOFF
	RETURN

R_TAKEOFF_ERROR,
	OFFERROR
	XCALL OLMSG(WND_1, 23,'INVALID REGISTER TAKEOFF PART NUMBER',1)
	CONFIG_ERROR = 1
	RETURN
;----------------------------------------------------------

R_ANGLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	WRANGL = R_ITEM(4,15)
	XCALL INSTR (1,WRANGL,'-',DASH)
	XCALL INSTR (1,WRANGL,'+',INCH)	;SSQ 6-1-98

	IF (DASH.EQ.0.AND.INCH.EQ.0) GOTO R_ANGLE_ERROR		;NEITHER
	IF (DASH.NE.0.AND.INCH.NE.0) GOTO R_ANGLE_ERROR		;BOTH
	IF (WRANGL(8,12).NE.BLANKS)  GOTO R_ANGLE_ERROR
	R_ITEM(4,15) = 

	TDESCR = WRANGL(1,7)	
	IF (DASH) 
		BEGIN
		 ONERROR R_ANGLE_ERROR
		TDESCR(DASH,DASH) = '/'
		DSIZ = WRANGL(1,DASH-1)
		DECMAL = WRANGL(DASH+1,DASH+3)
		 OFFERROR
		RA_SIZE = DSIZ + DECMAL
		IF (DECMAL .LE. 0) GOTO R_ANGLE_ERROR	;3-26-99 SSQ
		END
	IF (INCH) 
		BEGIN
		TDESCR(INCH,INCH) = '"'
		 ONERROR R_ANGLE_ERROR
		RA_SIZE = WRANGL(1,INCH-1)
		 OFFERROR
		IF (WRANGL(INCH+1,12) .NE. BLANKS) GOTO R_ANGLE_ERROR	;2-17-99
		END

	RETURN

R_ANGLE_ERROR,
	OFFERROR
	XCALL OLMSG(WND_1, 23,'INVALID R-ANGLE PART NUMBER',1)
	CONFIG_ERROR = 1
	RETURN
;----------------------------------------------------------


SPIRAL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (KITMNO.EQ.'SX' .AND. MAT_SEL.NE.GALV)
		BEGIN
		XCALL OLMSG(WND_1, 23,"DON'T SELECT MATERIAL",1)
		CONFIG_ERROR = 1
		RETURN
		END
 
	IF (KITMNO(7,7).EQ.'+')		;PROCESS INCHES
	THEN	BEGIN
		 ONERROR SPIRAL_ERROR
		INCH = KITMNO(8,10)
		 OFFERROR
		TDESCR = '__"-__GA X ___"LONG SPIRL ____'
		TDESCR(12,14) = INCH,'ZZX'
		DPND = (INCH*100) / 12
		IF (DPND(5,6).NE.0) 
		THEN DSIZ = DPND(1,4) + 1 
		ELSE DSIZ = DPND(1,4)
		END
	ELSE	BEGIN
		INCH = 
		 ONERROR SPIRAL_ERROR
		DSIZ = KITMNO(7,8)
		 OFFERROR
		TDESCR = '__"-__GA X __''SPIRAL PIPE ____'
		TDESCR(12,13) = DSIZ,'ZX'
		DSIZ = KITMNO(7,8)
		END

	IF (DSIZ.GT.30) GOTO SPIRAL_ERROR

	TDESCR(1,2) = KITMNO(3,4)	;DIA
	TDESCR(5,6) = KITMNO(5,6)	;GA

	IF (MAT_SEL .EQ. ALUM)			;SSQ 5-12-03
		BEGIN
		DGA = KITMNO(5,6)	;GA
		USING DGA SELECT
		(18),	TDESCR(5,8) = '.063'
		(20),	TDESCR(5,8) = '.050'
		(22),	TDESCR(5,8) = '.040'
		(24),	TDESCR(5,8) = '.032'
		ENDUSING
		END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SSQ 8-7-02
	R_ITEM(7,15) = 
	SP_SIZE = DSIZ

; 5-12-03: force price to zero if alum or sst...
	IF (KITMNO.EQ.'SG') 
	THEN	USING MAT_SEL SELECT
		(GALV),	TDESCR(27,30) = 'GALV'
		(ALUM),	BEGIN
			TDESCR(27,30) = 'ALUM'
			SP_SIZE = 0		;FORCE PRICE TO ZERO
			END
		(SST),	BEGIN
			TDESCR(27,30) = 'SST'
			SP_SIZE = 0		;FORCE PRICE TO ZERO
			END
		(PGRP),	TDESCR(27,30) = 'PGRP'
		ENDUSING
	ELSE	TDESCR(27,30) = 'PVS '

	RETURN

SPIRAL_ERROR,
	OFFERROR
	CONFIG_ERROR = 1
	XCALL W_DISP(WND_1, WD_POS,23,1,WD_CLR, WDC_LIN)
	DISPLAY (15,'ITEM CODE ENTERED: ',KITMNO,     '/ ITEM CODE CALCULATED: ',R_ITEM      )
	XCALL OLMSG(WND_1,23,'INVALID ITEM NUMBER FOR SPIRAL',2)
	RETURN
;----------------------------------------------------------------

FIND_DAMPER,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR BAD_DIGIT

	PARTNO = R_ITEM

	FOR I FROM 1 THRU 3
		BEGIN
		CLEAR SAVE_DP(I)
		END

	MAXFLEN = %TRIM(PARTNO)
	FOR I FROM 1 THRU 3
		BEGIN
		XCALL INSTR(1, PARTNO, "*", FL)
		IF (.NOT. FL) 		EXITLOOP
		PARTNO(FL, MAXFLEN) = PARTNO(FL+1, MAXFLEN)
		CALL D_SIZE
		IF (BAD_DIGIT)
			BEGIN
			XCALL OLMSG(WND_1,23,'damper configuration',2)
			RETURN
			END
		END

	R_ITEM = PARTNO
	RETURN

D_SIZE,	;---------------------------------------
	CLEAR BAD_DIGIT

	ONERROR BADDIG
	DAMP = PARTNO(FL, FL+1)
	OFFERROR

;check damper price...

	RETURN
BADDIG,
	BAD_DIGIT = 1
	RETURN
;----------------------------------------------------------------
;;; FUNCTION KEY WINDOWS.....

MATL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	DLINE6 = "MATERIAL"
	XCALL POP1(POP6,10)	;PALETTE 10 - YELLOW
	IF (PI6 .LT. 0) PI6 = -PI6
	USING P_ACTION6 SELECT
	(1, 4),	NOP			;USE WHATEVER SELECTED
	(),	PI6 = 1			;GALVANIZED
	ENDUSING
DISP_MATL,
	DLINE6 = PARRY6(PI6)
	MAT_SEL = PI6		;MATERIAL SELECTED
	IF (MAT_SEL .EQ. GALV)
	THEN	XCALL W_DISP(WND_6, WD_CLEAR)
	ELSE	BEGIN
		XCALL W_DISP(WND_6, WD_POS, 1, 1, DLINE6(1,PLEN6))
		XCALL W_AREA(WND_6, WA_COLOR, 10)	;blue
		END
	XCALL W_UPDT

	CLEAR TBL_KEY
	TBLCOD = 'CF'
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_CF_MUL]

	RETURN
;----------------------------------------

CHK_F6_MATL,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR BAD_MAT

	USING R_ITEM SELECT
	('SG','SX'),	NOP
	('RT','RTV'),	NOP
	('WR'),		NOP
	(),		IF (CFG_ITEM .NE. 2) RETURN	;MUST BE NON-CFG PART
	ENDUSING

	IF (MAT_SEL.LE.GALV .AND. R_MAT.LE.1) RETURN	;SSQ 9-24-03
;also check prdcat = "G" and ga >24
	I = 1
CF6L,
	A4 = F6A(I)			;CHECK FOR EACH MAT'L
	TL = %INSTR(1,R_DESC,A4)
	IF (TL .LE. 1) 
		BEGIN
		INCR I
		IF(I.LE.4) GOTO CF6L
		END

	IF (TL .LE. 1) 
		BEGIN
		BAD_MAT = 1
		XCALL OLMSG(WND_1, 23,'INVALID ITEM FOR CURRENT MATERIAL',2)
		RETURN
		END

	;;;IF (PRDCAT(1,1) .EQ. 'G')
	;;;USING PRDCAT(1,1) SELECT
	USING PRDCAT SELECT
	('G','A','J','K2'),	BEGIN
			BAD_MAT = 1
		;;;	XCALL OLMSG(WND_1,23,'NOT VALID FOR PRDCAT "G"',2)
			NVP = PRDCAT(1,1)
			XCALL OLMSG(WND_1,23,NVP_MSG,2)
			RETURN
			END
	ENDUSING

	SL = %INSTR(1,R_DESC,'GA')
	IF (SL .LE. 2) GOTO C_F6_OK
	SL = SL - 2
	A2 = R_DESC(SL,SL+1)
	ONERROR C_F6_OK
	D2 = A2
	OFFERROR
	
	IF (D2 .GT. 24)
		BEGIN
		BAD_MAT = 1
		XCALL OLMSG(WND_1,23,'GAUGE MUST BE 24 OR LESS',2)
		RETURN
		END
C_F6_OK,
	USING MAT_SEL SELECT
	(GALV),	R_DESC(TL,TL+3) = 'GALV'
	(ALUM),	R_DESC(TL,TL+3) = 'ALUM'
	(PGRP),	R_DESC(TL,TL+3) = 'PGRP'
	(SST),	R_DESC(TL,TL+3) = 'SST'
	ENDUSING

NOT_CF_MUL,
	RETURN
;----------------------------------------------------

F1_POP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL WINPT(WND_M, 1, 12, 30, 00, 'AE', ENTRY, INXCTL, F_KEY)
	USING F_KEY SELECT
	(F_01),	BEGIN
		XCALL W_DISP(WND_M, WD_POS,1,1,WD_CLR, WDC_LIN)
		CLEAR CLVL1
		RETURN
		END
	ENDUSING

	GOTO (F1_POP,ABORT_F1),INXCTL

;;;	IF (ENTRY(1,1).EQ.'.');;;> .AND. F1_MEMOL(1).EQ.BLANKS)
	IF (ENTRY(1,1).EQ.'.' .OR. ENTRY(1,1) .EQ. '9')
		BEGIN
		V_LEVEL = 1
		CALL GET_MEMO_TABLE
		IF (KEY_FOUND)
		THEN	ENTRY(1,30) = MM_LONG
		ELSE	GOTO F1_POP
		END

	IF (ENTRY.EQ.BLANKS)	ENTRY(1,30) = F1_MEMOL(1)

	XCALL W_DISP(WND_M, WD_POS, 6, 12, ENTRY(1,30))
	XCALL W_UPDT
	XCALL W_AREA(WND_M, WA_COLOR, 7)		;RED

	CNGCTL = 2
	XCALL WANCN(WND_1, 24, CNGCTL, WHATNO)
	GOTO (F1_POP),CNGCTL	

	F1_NUM = 1		;ONLY 1 MAIN COMMENT ALLOWED
	F1_IDX = F1_NUM
	F1_MEMOL(F1_IDX) = ENTRY(1,30)		
	CLVL1 = F1_IDX
ABORT_F1,
	RETURN
;---------------------------------------

F2_POP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR PI2
	IF (F2_NUM .LE. 0)
		BEGIN
		CALL INSERT_F2
		IF (INXCTL .NE. 0) RETURN
		END

DISP_F2,
; reverse order...
	I = 0
	FOR J FROM F2_NUM THRU 1 BY -1
		BEGIN
		X_MEMO = F2_MEMOL(J)
		INCR I
		F2_SEQ(I) = J		;save the actual seq #
		PARRY2(I) = X_MEMO(1,30)
		END
	NUMARA2 = F2_NUM

	DLINE2 = 'LEVEL-2 MEMOS'
	PALET2 = 5				;PALETTE 5 - GREEN
	XCALL POPM (MPOP2)
	IF(PI2 .LT. 0) PI2 = -PI2
	USING P_ACTION2 SELECT
	(2),	BEGIN			;INSERT
		CALL INSERT_F2
		IF (INXCTL .NE. 0)
		THEN	RETURN
		ELSE	GOTO DISP_F2
		END
	(),	IF(PI2.GT.0)
		THEN	CLVL2 = F2_SEQ(PI2)
		ELSE	CLVL2 = 0
	ENDUSING

	RETURN
;---------------------------------------
F3_POP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR PI3
	IF (F3_NUM .LE. 0)
		BEGIN
		CALL INSERT_F3
		IF (INXCTL .NE. 0) RETURN
		END

DISP_F3,
; reverse order...
	I = 0
	FOR J FROM F3_NUM THRU 1 BY -1
		BEGIN
		X_MEMO = F3_MEMOL(J)
		INCR I
		F3_SEQ(I) = J		;save the actual seq #
		PARRY3(I) = X_MEMO(1,30)
		END

	NUMARA3 = F3_NUM

	DLINE3 = 'LEVEL-3 MEMOS'
	PALET3 = 6			;PALETTE 6 - CYAN
	XCALL POPM (MPOP3)
	IF(PI3.LT.0)PI3 = -PI3
	USING P_ACTION3 SELECT
	(2),	BEGIN			;INSERT
		CALL INSERT_F3
		IF (INXCTL .NE. 0)
		THEN	RETURN
		ELSE	GOTO DISP_F3
		END
	(),	IF(PI3 .GT. 0)
		THEN	CLVL3 = F3_SEQ(PI3)
		ELSE	CLVL3 = 0
	ENDUSING

	RETURN
;---------------------------------------

INSERT_F2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF(F2_NUM .GE. F_MAX)
		BEGIN
		XCALL OLMSG(W_ID, 23, 'Cannot exceed 10 memos',1)
		RETURN
		END
	XCALL W_DISP(W_ID, WD_POS, ROW, 1, 'LEVEL-2 MEMO')
	V_LEVEL = 2
	CALL INSERT
	IF (INXCTL .NE. 0) RETURN

	INCR F2_NUM
	F2_IDX = F2_NUM
	F2_MEMOS(F2_IDX) = D_SHORT		
	F2_MEMOL(F2_IDX) = D_LONG		
	F2_KEY(F2_IDX) = D_KEY
	RETURN
;---------------------------------------

INSERT_F3,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF(F3_NUM .GE. F_MAX)
		BEGIN
		XCALL OLMSG(W_ID, 23, 'Cannot exceed 10 memos',1)
		RETURN
		END
	XCALL W_DISP(W_ID, WD_POS, ROW, 1, 'LEVEL-3 MEMO')
	V_LEVEL = 3
	CALL INSERT
	IF (INXCTL .NE. 0) RETURN

	INCR F3_NUM
	F3_IDX = F3_NUM
	F3_MEMOL(F3_IDX) = D_LONG
	F3_MEMOS(F3_IDX) = D_SHORT		
	F3_KEY(F3_IDX) = D_KEY
	RETURN
;---------------------------------------

INSERT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR D_SHORT, D_LONG, D_KEY
	XCALL W_DISP(WND_1, WD_POS, 24,1,WD_CLR, WDC_LIN,'".XX"=Canned memos')

	XCALL WINPT(W_ID, ROW+1, 3, 30, 00, 'AE', ENTRY, INXCTL)
	GOTO (INSERT,ABORT),INXCTL
;;;	IF (ENTRY(1,1) .EQ. '.') 
	IF (ENTRY(1,1).EQ.'.' .OR. ENTRY(1,1) .EQ. '9')
		BEGIN
		CALL GET_MEMO_TABLE
		IF (KEY_FOUND)
		THEN	BEGIN
			ENTRY(1,30) = MM_LONG
			D_SHORT = MM_SHORT
			D_KEY = MM_KEY		;SSQ 7-31-03
			XCALL W_DISP(W_ID, WD_POS, ROW+1, 3, ENTRY(1,30))
			END
		ELSE	GOTO INSERT
		END

	DL_ARA(1) = ENTRY(1,30)

	XCALL W_DISP(W_ID, WD_POS, ROW+2, 3, WD_CLR, WDC_LIN)
	XCALL W_DISP(W_ID, WD_POS, ROW+3, 3, WD_CLR, WDC_LIN)
	XCALL W_PROC(WP_REMOVE, WND_2)
	XCALL W_UPDT

	XCALL WINPT(W_ID, ROW+2, 3, 30, 00, 'AE', ENTRY, INXCTL)
	GOTO (INSERT,ABORT),INXCTL
	IF (ENTRY .EQ. BLANKS) GOTO I_CHANGE
	DL_ARA(2) = ENTRY(1,30)

	XCALL WINPT(W_ID, ROW+3, 3, 30, 00, 'AE', ENTRY, INXCTL)
	GOTO (INSERT,ABORT),INXCTL
	IF (ENTRY .EQ. BLANKS) GOTO I_CHANGE
	DL_ARA(3) = ENTRY(1,30)

I_CHANGE,
	CNGCTL = 2
	XCALL WANCN(WND_1, 24, CNGCTL, WHATNO)
	GOTO (INSERT),CNGCTL	
	IF (D_SHORT .EQ. BLANKS) D_SHORT = D_LONG
ABORT,
	XCALL W_PROC(WP_MOVE, WND_2, 0, 0)
	XCALL W_UPDT
	RETURN

GET_MEMO_TABLE,	;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR KEY_FOUND

	ONERROR NOT_KEY
	USING ENTRY(1,1) SELECT
	('.'),	MM_CODE = ENTRY(2,3)
	('9'),	MM_CODE = ENTRY(1,5)
	ENDUSING
	OFFERROR
MEMO_KEY,
	CLEAR TBL_KEY
	TBLCOD = 'MM'
	MM_KEY = MM_CODE
	READ (CHN182,COPTBL,TBL_KEY)[ERR=NOT_KEY]
	KEY_FOUND = 1		;PER STEVE M 1-18-00
	RETURN
NOT_KEY,
	OFFERROR
	IF(MM_CODE.GT.91) XCALL OLMSG (WND_1,23,'BAD CONFIGURATION',2)	;SSQ 9/4/03

	CALL LOAD_MEMO_POP
	IF (NUMARA .EQ. 0) RETURN
	DLINE = "  NO  MEMO"
	READS (15,ALPHA)
	XCALL POP40(POP)
	USING P_ACTION SELECT
	(1, 4),	BEGIN
		DLINE = PARRY(PI)
	;;;	MM_CODE = DLINE (1,2)
		MM_CODE = DLINE (1,5)	;ssq 7-31-03
		GOTO MEMO_KEY
		END
	ENDUSING

	RETURN
LOAD_MEMO_POP,
	CLEAR MM_KEY
	FIND (CHN182, COPTBL, TBL_KEY) [ERR=LM_FIND]
LM_FIND,
	CLEAR I
	FOR J FROM 1 THRU MAXARA
		BEGIN
		XCALL IOS (CHN182, COPTBL, READ, LOKCTL)
		IF (LOKCTL .NE. 0) GOTO NO_MORE_TBL
		IF (TBLCOD .NE. 'MM') GOTO NO_MORE_TBL
		IF (MM_VALID.EQ.0 .OR. MM_VALID.EQ.V_LEVEL)
		   BEGIN
		   CLEAR DLINE
	;;;	   DLINE (1,2) = MM_KEY,	'ZX'
	;;;	   DLINE (4,16) = MM_SHORT
	;;;	   DLINE (18,47) = MM_LONG
		   DLINE (1,5) = MM_KEY,	'ZZZZX' [LEFT]
		   DLINE (7,37) = MM_LONG
		   INCR I
		   PARRY(I) = DLINE
		   END
		END
NO_MORE_TBL,
	NUMARA = I
	RETURN
;----------------------------------------------------------------
;===========================================================

;---------------------------------------
ADD_LINE,
	IF (PIL .EQ. 0) RETURN
	RETURN
;---------------------------------------
DEL_LINE,
	RETURN
;---------------------------------------

F5_POP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; Inquire mode, hi-lite memos
	;;; associated w/ each line...
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	F5_POP_ON = 1		;DON'T ALLOW MULTIPLE ENTRY INTO F5_POP
; save current memos...
	SAVL1 = CLVL1
	SAVL2 = CLVL2
	SAVL3 = CLVL3

	CALL GET_LINES

	SAVE_ROW = 1
	LI = 1
;wham
RE_PAGE,
	IF (NUMARAL .EQ. 0) GOTO F5_RETURN
	LAST_ROW = NUMROWL
	IF (NUMROWL .GT. NUMARAL) LAST_ROW = NUMARAL

	XCALL W_PROC(WP_REMOVE,WND_2)
	XCALL W_PROC(WP_DELETE,WND_2)
	XCALL W_PROC(WP_CREATE, WND_2, 'SCROL',LAST_ROW, 78)
	XCALL W_BRDR(WND_2,WB_TITLE,'Review Memos/Change Lines',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE, WND_2, 8, 0)
	XCALL W_DISP(WND_2, WD_CLEAR)
	WLEN = 78

	CLEAR ARO
	CLEAR WROW
	FOR I FROM 1 THRU LAST_ROW
		BEGIN
		INCR WROW
	;;;	DLINE = PARRYL(I)
		REST = PARRYL(I)
		XCALL W_DISP(WND_2, WD_POS, WROW, 1, WRKLIN(1,PLENL+2))

	;;;	REST = DLINE(1,PLENL)
	;;;	XCALL W_DISP(WND_2, WD_POS, WROW, 1, WRKLIN(1,PLENL+2))
		END
	XCALL W_UPDT

	IF (F7_ON .EQ. 1) RETURN		;SSQ 4-9-04

	CLEAR VAL
	WROW = SAVE_ROW	
	CLEAR SAVE_ROW	
	IF (WROW .LE. 0) WROW = 1	;SSQ 12-16-99

	FIRST_ROW = 1
	CALL DSPLIN

DSPLOP,
	IF (SAVE_ROW .NE. 0)	CALL CLEAR_LAST_LINE

	WROW = WROW + VAL
	IF (WROW .GT. LAST_ROW) CALL SCROLL_DN
	IF (WROW .LT. FIRST_ROW) CALL SCROLL_UP

	LI = LI + VAL
	IF (LI .LE. 0) LI = 1
	IF (LI .GT. NUMARAL) LI = NUMARAL

	CALL DSPLIN
	CALL PINPUT
	USING F_KEY SELECT
	(EXIT_KEY, CR_KEY),	BEGIN
			CLEAR PIL
			CLEAR P_ACTIONL
			END
	(UP_ARO),	BEGIN
			VAL = - 1
			GOTO DSPLOP
			END
	(DN_ARO),	BEGIN
			VAL =  1
			GOTO DSPLOP
			END
	(PAGE_UP),	BEGIN
			CALL OL_PAGE_UP
			IF (NUMARAL .GT. 0) GOTO RE_PAGE
			END
	(PAGE_DN),	BEGIN
			CALL OL_PAGE_DN
			IF (NUMARAL .GT. 0) GOTO RE_PAGE
			END
	(RGHT_ARO),	BEGIN
			PIL = LI
			CALL CNG_LI		;CHANGE THIS LI
			IF (NUMARAL .GT. 0) 
				BEGIN
				if (%ttsts)READS (15, ENTRY)
				GOTO RE_PAGE
				END
			END
	(DEL_KEY),	BEGIN
			CALL DEL_LI
			GOTO DSPLOP
			END
	ENDUSING
	
DONE_F5,
	XCALL W_UPDT
	CALL LOAD_LINE_ITEMS

; restore current memos...
	CLVL1 = SAVL1
	CLVL2 = SAVL2
	CLVL3 = SAVL3

	R_F1KEY = CLVL1
	R_F2KEY = CLVL2
	R_F3KEY = CLVL3
	CALL DISPLAY_MEMO_WINDOWS

	CLEAR ROCINV
	CLEAR PI2, PI3

F5_RETURN,
	F5_POP_ON = 0		;DON'T ALLOW MULTIPLE ENTRY INTO F5_POP
	RETURN
;;;control point here..


DSPLIN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; move arrow, highlight selected line
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR CLVL1, CLVL2, CLVL3
	ARO = '>'
	REST = PARRYL(LI)
	XCALL W_DISP (WND_2,WD_ATTR,ATTR_SET+ATTR_RVRS)
	XCALL W_DISP (WND_2,WD_POS,WROW,1,WRKLIN(1,WLEN))
	XCALL W_DISP (WND_2,WD_POS,WROW,1)
	SAVE_ROW = WROW
;display any memos for current item...
	READ (CHN045, ROCINV, RFA=PARRFAL(LI))

	CALL DISPLAY_MEMO_WINDOWS
	RETURN
;----------------------------------------------------------------------
DISPLAY_MEMO_WINDOWS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	IF (R_F1KEY.GT.0)
	THEN	BEGIN
		ENTRY(1,30) = F1_MEMOL(R_F1KEY)
		XCALL W_DISP(WND_M, WD_POS, 6, 12, ENTRY(1,30))
		XCALL W_AREA(WND_M, WA_COLOR, 7)		;RED
		CLVL1 = R_F1KEY
		END
	ELSE	XCALL W_DISP(WND_M, WD_CLEAR)
	XCALL W_UPDT

	IF (R_F2KEY .EQ. 0)
	THEN	BEGIN
		XCALL W_PROC(WP_FIND, POP_WND2, POP_WID2)
		IF (POP_WND2)
		   BEGIN
		   XCALL W_PROC(WP_REMOVE,POP_WND2)
	    	   XCALL W_PROC(WP_DELETE,POP_WND2)
		   END
		END
	ELSE	BEGIN
		PI2 = -((F2_NUM-R_F2KEY)+1)
		CALL DISP_F2
		END

	IF (R_F3KEY .EQ. 0)
	THEN	BEGIN
		XCALL W_PROC(WP_FIND, POP_WND3, POP_WID3)
		IF (POP_WND3)
		   BEGIN
		   XCALL W_PROC(WP_REMOVE,POP_WND3)
		   XCALL W_PROC(WP_DELETE,POP_WND3)
		   END
		END
	ELSE	BEGIN
		PI3 = -((F3_NUM-R_F3KEY)+1)
		CALL DISP_F3
		END

	PI6 = R_MAT
	IF (PI6.LT.1 .OR. PI6.GT.4)PI6 = 1
	CALL DISP_MATL

	RETURN
;----------------------------------------------------------------------

CLEAR_LAST_LINE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; CLEAR HI-LITE ON PREV LINE
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR ARO
	XCALL W_DISP (WND_2,WD_ATTR,ATTR_CLR+ATTR_RVRS)
	XCALL W_DISP (WND_2,WD_POS,SAVE_ROW,1,WRKLIN(1,WLEN))
	RETURN
;----------------------------------------------------------------------
PINPUT,	;;;;;;;;;;;;;;;;;;;;;;;
	;;; FUNCTION KEY INPUT
	;;;;;;;;;;;;;;;;;;;;;;;

	XCALL W_DISP(WND_2,WD_ACCEPT,STAT_KEY)
	XCALL TTSTS(STAT)
	IF (STAT) 
	THEN	XCALL W_DISP(WND_2,WD_ACCEPT,F_KEY)
	ELSE	F_KEY = STAT_KEY

	CASE F_KEY OF
	BEGINCASE
	079:	F_KEY = 027
	133:	F_KEY = 027
	008:	F_KEY = 027		;<BS>
	010:	F_KEY = 013		;<CR>
	ENDCASE

	RETURN

GETCHR,	
	XCALL W_DISP(WND_2,WD_ACCEPT,ALPHA)
	XCALL DECML (ALPHA, DEC)
	RETURN

;----------------------------------------------------------------------
;----------------------------------------------------------------------

;;; WINDOW SCROLLING ROUTINES
SCROLL_DN,
	WROW = LAST_ROW
	IF (LI .EQ. NUMARAL) RETURN
	XCALL W_AREA(WND_2,WA_SCROLL,WAS_UP,1)
	RETURN

SCROLL_UP,
	WROW = FIRST_ROW
	IF (LI .EQ. 1) RETURN
	XCALL W_AREA(WND_2,WA_SCROLL,WAS_DOWN,1)
	RETURN
;--------------------------
;===================================================================
CNG_LI,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; ALLOW CHANGES TO SELECTED L/I
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	READ (CHN045, ROCINV, RFA=PARRFAL(PIL)) ;;dbg [ERR=CNG_LI_ERR]

	SAV045 = ROCINV
	MAT_SEL = R_MAT			;SSQ 5-14-03

;create and place input window to make change in...
	CNG_LI = 1
	CALL CREATE_WND4
	W_ID = WND_4
	XROW = 1
	XROW2 = 2

;input...
	CALL NEW_ITEM

;remove the input window, and restore screen...
	XCALL W_PROC(WP_REMOVE, WND_4)
	XCALL W_PROC(WP_DELETE, WND_4)
	XCALL W_UPDT

	XROW = 5
	XROW2 = 6
	ROW = XROW
	ROW2 = XROW2
	W_ID = WND_1
	CNG_LI = 0

	IF (R_ITEM.EQ.BLANKS) ROCINV = SAV045	;in case aborted

	R_MAT = MAT_SEL

	WRITE (CHN045, ROCINV, RFA=PARRFAL(PIL)) ;;;>[ERR=CNG_LI_ERR]	

; refresh array
	FOR I FROM 1 THRU NUMARAL
		BEGIN
		READ (CHN045, ROCINV, RFA=PARRFAL(I) )
		CALL MAKE_W2
		PARRYL(I) = W2_LINE
		END
;--------------------------------------------------

	RETURN
CNG_LI_ERR,
	RETURN
;--------------------------------------------------

DEL_LI,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; ALLOW CHANGES TO SELECTED L/I
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	FIND (CHN045, ROCINV, RFA=PARRFAL(1)) [ERR=GL_INIT]
	GOTO GL_INIT
	RETURN
;--------------------------------------------------

OL_PAGE_DN,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PAGE FORWARD NUMARAL RECORDS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR SAVE_ROW
	WROW = 1
	LI = 1
	FIND (CHN045, ROCINV, RFA=PARRFAL(NUMARAL)) [ERR=GL_INIT]
	GOTO GL_INIT

OL_PAGE_UP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; PAGE BACKWARDS NUMARAL RECORDS
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR SAVE_ROW
	WROW = 1
	LI = 1
; read reverse numaral records...
	FIND (CHN045, ROCINV, RFA=PARRFAL(1)) [ERR=GL_INIT]
	FOR I FROM 1 THRU MAXARAL
		BEGIN
		READS (CHN045, ROCINV, GL_INIT, REVERSE) [ERR=GL_INIT]
		END
	GOTO GL_INIT
;------------------------------------------------------------------

GET_LINES,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; READ NUMARAL ROCINV RECORDS INTO ARRAY
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CLEAR I
	FIND (CHN045, ROCINV, ^FIRST) [ERR=GL_INIT, EOF=RL_EOF]

GL_INIT,
	FOR I FROM 1 THRU MAXARAL CLEAR PARRYL(I), PARRFAL(I), PFLAGSL(I)
	CLEAR I

READ_LINES,
	XCALL IOS (CHN045, ROCINV, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO RL_EOF
	IF (SEARCH_ON .AND. (R_ITEM(1,SL).NE.S_ITEM(1,SL)) ) GOTO READ_LINES
	INCR I
	XCALL GETRFA(CHN045, LINRFA)
	PARRFAL(I) = LINRFA
	CALL MAKE_W2
	PARRYL(I) = W2_LINE

	IF (I .LT. MAXARAL) GOTO READ_LINES	
RL_EOF,
	UNLOCK CHN045
	NUMARAL = I
	RETURN
;---------------------------------------
FIND_LINE_ITEMS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; CHECK FOR EXISTING LINE ITEMS
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	CALL LOAD_LINE_ITEMS
	RETURN
;-----------------------------------------------------------

LOAD_LINE_ITEMS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;; LOAD EXISTIN LINE ITEMS INTO WND_2
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; restore current line items...
	XCALL W_PROC(WP_REMOVE,WND_2)
	XCALL W_PROC(WP_DELETE,WND_2)
	CALL CREATE_WND2

	FIND (5, ROCINV, ^FIRST) [ERR=RES_LOOP, EOF=RES_EOF]
RES_LOOP,
	LOKCTL = 1
	XCALL IOS (5,ROCINV,READ,LOKCTL)
	IF (LOKCTL .NE. 0) GOTO RES_EOF

	XCALL W_AREA(WND_2, WA_SCROLL, WAS_UP, 1)
	CALL MAKE_W2
	XCALL W_DISP(WND_2, WD_POS, 14, 1, W2_LINE)
	XCALL W_UPDT

	GOTO RES_LOOP
RES_EOF,
	UNLOCK CHN045
	RETURN
;----------------------------------------------------
SET_X,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL W_DISP(WND_1, WD_POS,4,1, HD_LINE)
	XCALL W_UPDT

	CLEAR CLVL1, CLVL2, CLVL3
	CLEAR CNG_LI
	F5_POP_ON = 0
	F7_ON = 0
	W_ID = WND_1
	XROW = 5
	XROW2 = 6
	FROW = 5
	NUMITM = 4

	LOKCTL = 1
	XCALL IO (1,DUMINV,1,READ,LOKCTL)
	JUSTIF = JSTIFY
	ORGINV = ORG041
	MAT_SEL = GALV
	CLEAR IS_15		;9-4-03 SSQ
	RETURN
;---------------------------------------------------	


MAKE_W2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR W2_LINE
	W2_ITEM = R_ITEM
	IF (R_F1KEY) W2_ITEM(13,13) = '*'
	IF (R_F2KEY) W2_ITEM(14,14) = '*'
	IF (R_F3KEY) W2_ITEM(15,15) = '*'
	W2_DESCR = R_DESC
	W2_QTY = R_QTYH,	'ZZZZZ-'
	RETURN
;---------------------------------------------------	

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	SWITCH = 5
	XCALL FILES (17,'SI',182,SWITCH)
	SWITCH = 5
	XCALL FILES (1,'I',041,SWITCH)
	XCALL FILES (2,'I',042,SWITCH)
	XCALL FILES (5,'SU',091,SWITCH)

	CHN182 = 17		;OPENED IN ORDADD
	CHN045 = 5

	XCALL IO (1,DUMINV,1,READ,LOKCTL)

	RETURN
;---------------------------------------------------
CLOSE,
	CLOSE CHN182 		;OPENED IN ORDADD
	CLOSE CHN045
	CLOSE 1
	CLOSE 2

	RETURN
;---------------------------------------------------
;===========================================================

INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SET UP SCREEN 1 WINDOW
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	W2_NAME = 'SCROL'
	W4_NAME = 'CNGLI'
	W6_NAME = 'MATL'
	WM_NAME = 'TOPL'

;;;	XCALL W_INIT(POOLSIZE,WNDCHNL,MAXWINS)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (.NOT. WND_1)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'ORDER ADD',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,1,1)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1

	CALL CREATE_WND2

	XCALL W_PROC(WP_CREATE, WND_M, 'TOPL',1, 43)
	XCALL W_PROC(WP_PLACE, WND_M, 3, 0)
	XCALL W_DISP(WND_M, WD_CLEAR)


	XCALL W_PROC(WP_CREATE, WND_6, W6_NAME,1, 15)
	XCALL W_PROC(WP_PLACE, WND_6, 2, 48)
	XCALL W_DISP(WND_6, WD_CLEAR)

	XCALL W_PROC(WP_CREATE, WND_D, 'DAMPER',7, 42)
	XCALL W_DISP(WND_D, WD_CLEAR)

;Pallets...
	XCALL W_PROC(WP_PALET, 4, 1, 0)		;1=BLUE
	XCALL W_PROC(WP_PALET, 5, 2, 0)		;2=GREEN
	XCALL W_PROC(WP_PALET, 6, 3, 0)		;3=CYAN
	XCALL W_PROC(WP_PALET, 7, 4, 7)		;4=RED
	XCALL W_PROC(WP_PALET, 8, 7, 0)		;7=WHITE
	XCALL W_PROC(WP_PALET, 9, 0, 7)		;4=BLACK ON WHITE
	XCALL W_PROC(WP_PALET,10, 6, 0)		;6=YELLOW
;POP40:
	MAXARA = 40
	PLEN = 48
	NUMROW = 10
	WX = 19
	WY = 18
	POP_WID(1,5) = "MM_TBL"
	POP_TITLE = "CANNED MEMOS"

;POP-M2:
	MAXARA2 = 10		
	PLEN2 = 34
	NUMROW2 = 4
	WX2 = 19
	WY2 = 4
	POP_WID2(1,5) = "F2WIN"
	POP_TITLE2 = "LEVEL-2 MEMOS"

;POP-M3:
	MAXARA3 = 10	
	PLEN3 = 34
	NUMROW3 = 4
	WX3 = 19
	WY3 = 44
	POP_WID3(1,5) = "F3WIN"
	POP_TITLE3 = "LEVEL-3 MEMOS"

;POP-MATERIAL:
	MAXARA6 = 4	
	NUMARA6 = 4
	PLEN6 = 15
	NUMROW6 = 4
	WX6 = 1
	WY6 = 48
	POP_WID6(1,5) = "F6WIN"
	POP_TITLE6 = "MATERIAL"
	
	FOR I FROM 1 THRU 4	PARRY6(I) = C_DES(I)
	MAXARAL = 10
	PLENL = 76
	NUMROWL = 10
	WXL = 8
	WYL = 1
	POP_WIDL(1,5) = "F4WIN"
	POP_TITLEL = "Select/Unselect Memos"

	RETURN

;---------------------------------------
CREATE_WND2,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	XCALL W_PROC(WP_CREATE, WND_2, 'SCROL',10, 78)
	XCALL W_BRDR(WND_2,WB_TITLE,'LINE ITEMS',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE, WND_2, 8, 0)
	XCALL W_DISP(WND_2, WD_CLEAR)
	WLEN = 78
	RETURN
;--------------------------------------------------------

CREATE_WND4,
	XCALL W_PROC(WP_CREATE, WND_4, 'CNGLI',2, 78)

	XCALL W_BRDR(WND_4, WB_OFF)
	XCALL W_PROC(WP_PLACE, WND_4, 5, 0)
	XCALL W_DISP(WND_4, WD_CLEAR)
	XCALL W_AREA(WND_4, WA_COLOR, 4)		;blue
	WLEN = 78
	XCALL W_UPDT
	RETURN
;--------------------------------------------------------


.END


