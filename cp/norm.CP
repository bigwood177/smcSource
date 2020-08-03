function norm
	in_fld	,a	;data to be normalized
;
record	vars
	x	,d6
	y	,d6
	a1	,a1
	ln	,d6
	ret_fld	,a30


proc

	clear ret_fld
	ln = %trim(in_fld)

	CLEAR X,Y

	FOR X FROM 1 THRU ln
		BEGIN
		A1=in_fld(X,X)
		IF ((A1.GE.'1'.AND.A1.LE.'9') .OR. (A1.GE.'A'.AND.A1.LE.'Z'))
			BEGIN
			INCR Y
			ret_fld(Y,Y)=A1
			END
		END
	incr y
	if (y.lt.ln) ret_fld(y,ln) = 
	
	freturn ret_fld
	end

