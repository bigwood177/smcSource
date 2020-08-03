subroutine rnick
	price	,d	;.xxx

record	vars
	n_num	,d10
	n_dig	,d1
	n_price	,d10
proc
	

	N_PRICE = price#1	;.XXX -> .XX
	N_NUM = N_PRICE/10*10
	N_DIG = N_PRICE - N_NUM		;ENDING DIGIT .XX

	USING N_DIG SELECT
	(0),	RETURN		;ALREADY ENDS IN ZERO
	(.LE. 5),	N_PRICE = N_NUM + 5	;.05
	(.GT. 5),	N_PRICE = N_NUM + 10	;.10
	ENDUSING
	
	PRICE = N_PRICE * 10	;.XX -> .XXX
	RETURN

	end

