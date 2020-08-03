SUBROUTINE	CMPRS
	IN	,A		;UNCOMPRESSED VALUE
	OUT	,A		;COMPRESSED VALUE
;
;		Remove the following char's from IN:
;		leading 0's, spaces, "-", '/'.
;
RECORD	VARS
	WORK	,A15
	TEMP	,A15
	LEN	,D4
	I	,D4
	J	,D4
;
PROC
	CLEAR OUT, WORK, TEMP, I, J

	WORK = IN
	LEN = %TRIM(WORK)

; replace leading zeros w/ spaces...
	FOR I FROM 1 THRU LEN
		IF (WORK(I,I) .NE. '0')
		THEN	EXITLOOP
		ELSE	WORK(I,I) = " "

; compress...
	FOR I FROM 1 THRU LEN
	    CASE WORK(I,I) OF
	    BEGINCASE
	    " ":	NEXTLOOP
	    "-":	NEXTLOOP
	    "/":	NEXTLOOP
	    "\":	NEXTLOOP
	    ENDCASE
	    ELSE	BEGIN
			INCR J
			TEMP(J,J) = WORK(I,I)
			END
	OUT = TEMP
	RETURN
	END
			
