FUNCTION	R1
	NUM	,D		;CC #
	OPT	,A		;E =ENCRYPT, D=DECRYPT

RECORD	VARS
	VAL	,D16	;RETURN VALUE
	I	,D6
	J	,D6
	DIG	,D1
	CRYP	,10A1,	'5','9','4','8','3','7','2','6','1','0'
	ALET	,A16
	ANUM	,A16
;
PROC

	CLEAR ALET, ANUM

	USING OPT SELECT
	('E'),	CALL ENCRYPT
	('D'),	CALL DECRYPT
	ENDUSING

	FRETURN VAL
;
;===========================================================

ENCRYPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ANUM = NUM, 'XXXXXXXXXXXXXXXX'
	FOR I FROM 1 THRU 16
		BEGIN
		DIG = ANUM(I,I)
		ALET(I,I) = CRYP(DIG+1, DIG+1)
		END

	VAL = ALET	
	RETURN
;-----------------------------------------------------------


DECRYPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ALET = NUM, 'XXXXXXXXXXXXXXXX'

	FOR I FROM 1 THRU 16
		BEGIN
		FOR J FROM 1 THRU 10 IF (ALET(I,I) .EQ. CRYP(J,J)) EXITLOOP

		ANUM(I,I) = J-1, 'X'
		END

	VAL = ANUM

	RETURN
;-----------------------------------------------------------
	

