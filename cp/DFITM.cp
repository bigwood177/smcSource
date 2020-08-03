SUBROUTINE DFITM
;
;
;
	WHICH	,D
	PARM	,D
	ITEMN	,A

PROC
	IF (WHICH.EQ.1)
	BEGIN
	CASE PARM OF
	BEGINCASE

	0-12:	; If the slip size is between 0 - 12 inches, it requires
		; a flat s 26 gauge slip, cut to length of SIZE1
		BEGIN
		  ITEMN(1,3) = '726'
		  ITEMN(4,5) = PARM,'XX'
		END

	13-19:	; If the slip size is between 13 - 19 inches, it requires
		; a flat s 24 gauge slip, cut to length of SIZE1
		BEGIN
		  ITEMN(1,3) = '724'
		  ITEMN(4,5) = PARM,'XX'
		END

	20-30:	; If the slip size is between 20 - 30 inches, it requires
		; a 1" standing 24 gauge slip, cut to length of SIZE1
		BEGIN
		  ITEMN(1,3) = '824'
		  ITEMN(4,5) = PARM,'XX'
		END

	31-42:	; If the slip size is between 31 - 42 inches, it requires
		; a 1" standing 22 gauge slip, cut to length of SIZE1
		BEGIN
		  ITEMN(1,3) = '822'
		  ITEMN(4,5) = PARM,'XX'
		END

	43-49:	; If the slip size is between 43 - 49 inches, it requires
		; a 1.5" standing 22 gauge slip, cut to length of SIZE1
		BEGIN
		  ITEMN(1,4) = '1022'
		  ITEMN(5,6) = PARM,'XX'
		END

	50-120:	; If the slip size is over 50 inches, it requires
		; a standing 22 gauge slip, with reinforcing bar ( re-bar )
		BEGIN
		  ITEMN(1,5) = '1022B'
		  ITEMN(6,7) = PARM,'XX'
		END
	ENDCASE
	END

	IF (WHICH.EQ.2)
	BEGIN
	CASE PARM OF
	BEGINCASE

	0-9:	; If the drive size is between 0 - 9 inches, it requires
		; a flat 26 gauge drive, the size of the 2nd dimension
		BEGIN
		  ITEMN(1,1) = '6'
		  ITEMN(2,3) = PARM, 'XX'
		  ITEMN(4,5) = '26'		;26 GAUGE
		  ITEMN(6,6) = '1'		;USE CUT TO LENGTH
		END				;PER DW 1/20/87

	10-19:	; If the drive size is between 10 - 19 inches, it requires
		; a flat 24 gauge drive, the size of the 2nd dimension
		BEGIN
		  ITEMN(1,1) = '6'
		  ITEMN(2,3) = PARM,'XX'
		  ITEMN(4,5) = '24'		;24 GAUGE
		  ITEMN(6,6) = '1'		;USE CUT TO LENGTH 
		END				;PER DW 1/20/87

	20-29:	; If the drive  size is between 20 - 29 inches, it requires
		; a standing 24 gauge drive, the size of the 2nd dimension
		BEGIN
		  ITEMN(1,3) = '124'
		  ITEMN(4,5) = PARM,'XX'
		  ITEMN(6,6) = '1'		;USE CUT TO LENGTH 
		END				;PER DW 1/20/87

	30-99:	; If the drive size is over 30 inches, it requires
		; a 22 gauge drive, the size of the 2nd dimension
		BEGIN
		  ITEMN(1,3) = '122'
		  ITEMN(4,5) = PARM,'XX'
		  ITEMN(6,6) = '1'		;USE CUT TO LENGTH
		END				;PER DW 1/20/87

	ENDCASE
	END

	RETURN
END
