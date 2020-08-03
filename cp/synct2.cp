;SYNCT2.CP
;
;	Syncronize prices from coptbl.smm to coptbl.rom
;	desk top version for Jessie...

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'
RECORD	SAV
	.INCLUDE 'DEF:RD182S.DEF'
;
RECORD	DIS
	II	,D6
;
record	error_msg
	emsg	,a20
		,a*,	'locked! '
RECORD	VARS

	SAVCOD	,A2
;
PROC
	OPEN(15,I,'TT:')
	DISPLAY(15,$SCR_CLR(SCREEN),'SYNCT2 - UPDATE ROC COP TABLES')
;
	OPEN(1,SI,'SMC:COPTBL.SMM')
;;;	OPEN(2,SU,'TST:COPTBL.TSM')

	OPEN(2,SU,'ROC:COPTBL.ROM')	;12-14-06 MUST GET EXCLUSIVE USE!
;;;	OPEN(2,SU,'ROC:COPTBL.ROM',SHARE:0)	;12-14-06 MUST GET EXCLUSIVE USE!


; delete from rockford table...
	CLEAR II


; add SMC records to Rockford table...
S_LOOP,
	INCR II
	IF (II/100*100 .EQ. II) DISPLAY(15,$SCR_POS(1,70),DIS)
	READS (1,COPTBL,S_EOF)
	SAV = COPTBL		;SAVE TO STORE

	USING TBLCOD SELECT
	('DP'),	CALL UPDATE
	('LP'),	CALL UPDATE	;LINER PRICING
	('I1'),	CALL UPDATE	;ITEM QTY PRICING
	('SB'),	CALL UPDATE	;SINGLE BLADE DAMPER
	('BD'),	CALL UPDATE	;BACK-DRAFT DAMPER
	('LV'),	CALL UPDATE	;LOUVER 
	('CF'),	CALL UPDATE	;GENERAL CONFIG PARAMETERS
	('SQ'),	CALL UPDATE	;SQUARE TO ROUND
	ENDUSING
	
	GOTO S_LOOP

UPDATE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	READ (2, COPTBL, TBL_KEY) [ERR=U_STORE]
	GOTO U_ERR				;DELETE THEN STORE
U_STORE,
	COPTBL = SAV				;SMC VERSION
	STORE(2,COPTBL,TBL_KEY)	[ERR=U_ERR]
	RETURN
U_ERR,
	DELETE (2, TBL_KEY) [ERR=U_DONE]

	COPTBL = SAV				;SMC VERSION
	STORE (2, COPTBL, TBL_KEY) [ERR=U_DONE]
	RETURN
U_DONE,
	USING TBLCOD SELECT
	('DP'),	emsg = 'DUCT PRICING'
	('LP'),	emsg = 'LINER PRICING'
	('I1'),	emsg = 'ITEM QTY PRICING'
	('SB'),	emsg = 'SINGLE BLADE DAMPER'
	('BD'),	emsg = 'BACK-DRAFT DAMPER'
	('LV'),	emsg = 'LOUVER '
	('CF'),	emsg = 'GENERAL CONFIG PARAMETERS'
	('SQ'),	emsg = 'SQUARE TO ROUND'
	ENDUSING
	
	XCALL MESAG (error_msg,1)
	RETURN

;-----------------------------------------------------------


S_EOF,

	xcall mesag ('Done!',1)
	CLOSE 2

	CLOSE 1

	XCALL FLAGS(7000000)
	STOP
	END

