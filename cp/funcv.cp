function	funcv
	item	,a	;line item as entered
	code	,a	;create virtual line item for this code
	p_f3	,d	;f3 note


record	fv_data
	f_item	,a15	;virtual item to return
	f_f3	,5d1	;f3 note for returned part
	f_cfg	,d1	;same as cfg_item in scrnx
	f_ga	,d2

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


record	damper_locs
	a12	,a12
	da	,d1
	db	,d1
	dc	,d1
	dd	,d1

record
	a_f3,	a5
record,x
	f3	,5d1
	
RECORD	ADIS
	AAN	,A2
		,A1
	AGA	,A2
		,A1
	AMA	,A2
		,A1
	AMB	,A2
		,A1
	ABC	,A2
		,A1
	ABD	,A2


record	vars
	MCODE	,A4	;CODE FOR MAIN (T9, CC4, ETC)
	LG_MA	,D2	;LARGER OF MA/MB
	AN	,D1	;ANGLE (4/9)
	L	,D2	;COMPUTED PART OF LENGTH
	LN	,D6
	LG	,D2	;LARGER OF 2 SEGS
	LEN	,D2	;COMPUTED LENGTH
	TEE	,A1	;"T" for tee, "C" for cross, "B" for bullnose tee
	I	,d6
	J	,d6
	K	,d6
	ST	,D6

proc
	clear fv_data
	f_cfg = 1		;for everything except C MA GA

	call cfg		;get segs

	call mak_f3	
	call comput_L

	using item select
	('T','CT','STT'),	TEE = 'T'
	('C','CC','STC'),	TEE = 'C'
	('BN'),			TEE = 'B'
	endusing

	using code select
	('SG'),		call SG
	('R'),		if (MA.GT.MB .and. tee.ne.'B') call R	;there is a reducer
	('BR1'),	if (tee .eq. 'B') call BR1	;only for bullnose tee's
	('BR2'),	if (tee .eq. 'B') call BR2	;only for bullnose tee's
	('SW1'),	call SW1
	('SW2'),	if (tee.eq.'C') call SW2
	('C1'),		if (tee.ne.'B') call C1
	('C2'),		if (tee.ne.'B') call C2		;1-3-19 don't create a 2nd connector.
	('MN'),		call main			;split the spt and the reducer, this is the remenant Tee, Cross, BN Tee
	endusing

	f_ga = ga
	freturn	fv_data

;======================================
main,	; create the remenant
	if (tee .eq. 'B')
		begin
		lg_ma = bc
		mcode = 'T'
		end
	ln = %trim(mcode)
	f_item(1,ln) = mcode
	f_item(ln+1,ln+2) = GA, 'XX'
	f_item(ln+3,ln+4) = LG_MA, 'XX'
	f_item(ln+5,ln+6) = LG_MA, 'XX'
	f_item(ln+7,ln+8) = BC, 'XX'
	f_item(ln+9,ln+10) = BD, 'ZZ'

	return
;--------------------------------------


SG,	; create SG MA GA +LN

;;;	display (15, $scr_pos(3,40),'AN GA MA MB BC BD')
	AAN = AN,'X'
	AGA = GA, 'XX'
	AMA = MA, 'XX'
	AMB = MB, 'XX'
	ABC = BC, 'XX'
	ABD = BD, 'ZZ'
;;;	DISPLAY (15, $SCR_POS(4,40), ADIS)

	
	f_item = 'SG'

	f_item(3,4) = MA, 'XX'
	if (tee .eq. 'B') f_item(3,4) = BC, 'XX'

	f_item(5,6) = GA, 'XX'
	f_item(7,7) = '+'
	f_item(8,9) = LEN, 'ZX' [LEFT]
	
; f3 note:
	if (f3(1) .eq. 3)
	then	f_f3(1) = 3
	else	f_f3(1) = 0

	if (MA.EQ.MB .AND. F3(2).EQ.4) f_f3(2) = 2
	return
;------------------------------------------

R,	; create R GA MA MB
	f_item = 'RW'
	f_item(3,4) = ga, 'XX'

	j = 5
	if (da) call add_damper
	k = j+1
	f_item(j,k) = ma, 'XX'
	j = k+1

	if (db) call add_damper
	k = j+1
	f_item (j,k) = mb, 'XX'


; f3 note:
	if (f3(1) .eq. 3)
	then	f_f3(1) = 3
	else	f_f3(1) = 4

	if (f3(2) .ne. f_f3(1)) f_f3(2) = f3(2) ;don't need to duplicate note (eg: f3=33)

	return
;------------------------------------------

BR1,	; create R GA BC MB

	f_item = 'RW'
	f_item(3,4) = ga, 'XX'

	j = 5
;;;	if (da) call add_damper
	k = j+1
	f_item(j,k) = bc, 'XX'
	j = k+1

	if (da) call add_damper
	k = j+1
	f_item (j,k) = ma, 'XX'


; f3 note:
	if (f3(1) .eq. 3)
	then	f_f3(1) = 3
	else	f_f3(1) = 4

	if (f3(2) .ne. f_f3(1)) f_f3(2) = f3(2) ;don't need to duplicate note (eg: f3=33)

	return
;------------------------------------------

BR2,	; create R GA BC MB

	f_item = 'RW'
	f_item(3,4) = ga, 'XX'

	j = 5
;;;	if (da) call add_damper
	k = j+1
	f_item(j,k) = bc, 'XX'
	j = k+1

	if (db) call add_damper
	k = j+1
	f_item (j,k) = mb, 'XX'

; f3 note:
	if (f3(1) .eq. 3)
	then	f_f3(1) = 3
	else	f_f3(1) = 4

	if (f3(2) .ne. f_f3(1)) f_f3(2) = f3(2) ;don't need to duplicate note (eg: f3=33)

	return
;------------------------------------------

SW1,	; create SW AN GA BC MA

	using item select
	('STT','STC'),	f_item = 'ST'
	('CT','CC'),	f_item = 'SC'
	('T','C'),	f_item = 'SW'
	('BN'),		f_item = 'SW'
	endusing

	f_item(3,3) = AN, 'X'
	f_item(4,5) = GA, 'XX'

	j = 6
	if (dc) call add_damper
	k = j+1
	f_item(j,k) = BC, 'XX'
	j = k+1

	if (tee .eq. 'B')
	then	begin
		if (dc) call add_damper
		k = j+1
		f_item(j,k) = BC, 'XX'
		end
	else	begin
		if (da) call add_damper
		k = j+1
		f_item (j,k) = MA, 'XX'
		end

; f3 note:
	f_f3(1) = f3(3)
	return
;------------------------------------------

SW2,	; create SW AN GA BD MA

	using item select
	('STT','STC'),	f_item = 'ST'
	('CT','CC'),	f_item = 'SC'
	('T','C'),	f_item = 'SW'
	endusing

	f_item(3,3) = AN, 'X'
	f_item(4,5) = GA, 'XX'

	j = 6
	if (dd) call add_damper
	k = j+1
	f_item(j,k) = BD, 'XX'

	j = k+1
	if (da) call add_damper
	k = j+1
	f_item(j,k) = MA, 'XX'


; f3 note:
	f_f3(1) = f3(4)

	return
;------------------------------------------

C1,	; create C MA GA
	
	if (f3(1) .ne. 4) return		;only need C for spiral ends
	call make_c
	return
;-------------------------------------------

C2,	; create C MA GA

	return					;1-3-19 not doing this any more...

	if (MA .ne. MB) return			;no 2nd C if reducer
	if (f3(2) .ne. 4) return		;only need C for spiral ends
	call make_c
	return
;---------------------------------------------

make_c,
	clear f_cfg		;not cfg'd
	f_item = 'C'
	f_item(2,3) = MA, 'XX'
	f_item(4,5) = 20, 'XX'	;1-3-19 per brandon, always 20 ga
;;;	f_item(4,5) = GA, 'XX'

; f3 note:
	f_f3(1) = 4		;otherwise we wouldn't be here

	return
;------------------------------------------

mak_f3,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	a_f3 = p_f3,	'ZZZZZ' [left]

; if only the first f3 note has a value, fill in the remaining 4 elements
	if (f3(2) .eq. 0) for i from 2 thru 5 f3(i) = f3(1)	
	return
;------------------------------------------

T_or_C,

	return
;------------------------------------------

comput_L,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	AN = 9


	; get larger of bc/bd....
	lg = bc
	if (bd.gt.bc) lg = bd

	; get larger of ma/mb
	lg_ma = ma
	if (mb.gt.ma) lg_ma = mb

	;compute "L"

	using item select
	('BN9'),	begin
			st = 3
			if (f3.eq.3)
			then len = lg + 6
			else len = lg + 4
			end

	('T9','C9'),	begin
			st = 2
			if (f3.eq.3)
			then len = lg + 6
			else len = lg + 4
			end

	('T4','C4'),	begin
			st = 2
			AN = 4

			if (f3.eq.3)
			then len = (lg*1414)#3 + 9
			else len = (lg*1414)#3 + 4
			end

	('CT','CC'),	begin
			st = 2
			if (item(3,3) .eq. '9') st=3	;4-25-18 for CT9 and CC9
			if (f3.eq.3)
			then len = lg + 7
			else len = lg + 5
			end


	('STT','STC'),	begin
			st = 3
			if (f3.eq.3)
			then len = lg + 9
			else len = lg + 7
			end
	endusing

	mcode = item(1,st)

	return
;-------------------------------------------

cfg,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;; get segs
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	clear segs

	using item select
	('C9','C4','T9','T4'),	ST = 3
	('CT9','CC9'),		ST = 4
	('STT','STC'),		ST = 4
	('BN9'),		ST = 4
	endusing

;check for dampers...
	clear da, db, dc, dd

	a12 = item(st,15)
	ln = %trim (a12)
	j = 0

	for i from 1 thru ln
		begin
		using a12(i,i) select
		('0' thru '9'),	begin
				incr j
				segs(j,j) = a12(i,i)
				end
		('*'),		using j select
				(.lt.3),	da = 1		;before ma
				(.lt.5),	db = 1
				(.lt.7),	dc = 1
				(.lt.9),	dd = 1
				endusing

		endusing
		end

;--------------------------------------------
;;;	segs = item(st,15)


	return
;----------------------------------------------

add_damper,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		f_item(j,j) = '*'	;damper
		incr j
	return
;-----------------------------------------------------

	end
