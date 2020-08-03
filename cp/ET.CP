Subroutine ET
	fabd	,a	;fabdat record
	psegs	,a	;segs
	matl	,d	;1=galv
	f3	,d	;resultant f3 note

;
;	QE end treatment conversion
;
record	fabdat
	.include 'def:fabdat.def'
record
	endq	,d4
record,x
	endt	,4d1
;
record
	fwork	,d4
record,x
	fw	,4d1	;f3 work variable

record
	flags	,d6	;qe et: 1 3 4 5 8 9
record,x
	df	,6d1	;is this note represented? 1 = yes

record
	fd5	,d5
record,x
	fdig	,5d1

RECORD
	SEGS	,A12			;first 2 dig are gauge
RECORD,X
	GA	,D2
	SARA	,5D2

record	vars
	wf3	,d5	;working var for f3
	a4	,a4
	i	,d6
	num_end	,d6	;number of end treatments
	is_ez	,d1	;1=one of the ends is ez flange
	is_other,d1	;1=one of the ends is not ez
	is_1	,d4
	is_2	,d4
	is_3	,d4
	is_4	,d4
	is_5	,d4
	is_8	,d4
	is_9	,d4
	dia	,d3	;dia. for determining jr
	err	,d1	;1=error cfg
;
.proc
	fabdat = fabd
	segs = psegs

	for i from 1 thru 4 endt(i) = fd_f3(i)


	clear f3, fwork, err
	
;----------------------------------------------------
; 3 cases to consider:
;	1. ez and other ends (.9xxxx)
;	2. ez only (15 or 16)
;	3. other ends (convert & consolidate)

; all "None" converted to spiral in XPARS.CP, unless Pipe,
; if Pipe (fd_f3 = -1) then leave blank, unless EZ, then
; change to RW...

	clear num_end, is_ez, is_other

	for i from 1 thru 4
		begin
		using endt(i) select
		(-1),	is_other = 1	;part was "Pipe", end came in as "None"

		(0),	nop		;no note

		(2),	begin
			is_ez = 1
			incr num_end	;count end treatments
			end

		(),	begin
			is_other = 1
			incr num_end	;count end treatments
			end
		endusing
		end

	
	using num_end select
	(0),	nop	;no notes, nothing to do...
	(),	begin
		if (is_ez .and. is_other) call ez_plus
		if (is_ez .and. .not. is_other) call ez_only
		if (.not. is_ez) call reg_note
		end
	endusing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (err) clear f3
	xreturn
;============================================


ez_only,	;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; only et is ez flange
		;;;;;;;;;;;;;;;;;;;;;;;;;

	if (matl .eq. 1)
	then 	f3 = 3		;ez flange installed
	else	f3 = 9		;ez flange not installed
	return
;----------------------------------------

ez_plus,	;;;;;;;;;;;;;;;;;;;;;;;;;
	
	for i from 1 thru 4
		begin
		dia = sara(i)

		using endt(i) select
		(-1),	fw(i) = 8	;rw
		(0),	nop		;no note
		(1),	fw(i) = 4	;sp
		(2),	using dia select
			(1 thru 5),	fw(i) = 4	;sp			
			(6 thru 24),	fw(i) = 3	;jr
			(.gt. 24),	fw(i) = 3	;ez
			endusing
		(8),	fw(i) = 8	;rw
	;;;	(9),	fw(i) = 1	;ss
		(9),	using dia select
			(6,8,10,12,14,16,18,20,22,24), fw(i) = 1	;ss
			(),	fw(i) = 4	;sp
			endusing
		(),	err = 1		;??
		endusing
		end

	f3 = fwork

	fd5 = f3	;digits in f3 note

	clear wf3	;clear working var

	for i from 1 thru 5
		begin
		if (fdig(i) .ne. 0) wf3 = wf3*10 + fdig(i)
		end

	f3 = wf3

	return
;----------------------------------------

reg_note,	;;;;;;;;;;;;;;;;;;;;;;;;;

	a4 = endq,	'XXXX'
	is_1 = %instr (1, a4, '1')
	is_3 = %instr (1, a4, '3')
	is_4 = %instr (1, a4, '4')
	is_5 = %instr (1, a4, '5')
	is_8 = %instr (1, a4, '8')
	is_9 = %instr (1, a4, '9')
	
	flags = 000000
	if (is_1) df(1) = 1
	if (is_3) df(2) = 1
	if (is_4) df(3) = 1
	if (is_5) df(4) = 1
	if (is_8) df(5) = 1
	if (is_9) df(6) = 1

	using flags select
	(100000),	f3 = 4	;spiral end is only note
;;;	(000010),	f3 = 2	;large end is only note
	(010000),	f3 = 5	;rings swedged
	(001000),	f3 = 6	;tack welded rings
	(000100),	f3 = 7	;full welded rings
	(000001),	f3 = 1	;safety seal
	endusing
	
	if (f3 .gt. 0) return	;good enough...

;some combination of ET
	if (is_3 .or. is_4 .or. is_5) ;welded rings can't be combined w/ other ET
		begin
		clear f3
		return
		end

	using endq select
	(8000),	f3 = 2	;le
	(0800),	f3 = 2	;le
	(0080),	f3 = 2	;le
	(0008),	f3 = 2	;le
	endusing

	using endq select
	(8800),	f3 = 2	;le/le
	(8008),	f3 = 2	;le/le
	(8080),	f3 = 2	;le/le
	(0880),	f3 = 2	;le/le
	(0808),	f3 = 2	;le/le
	(0088),	f3 = 2	;le/le
	endusing
	
	using endq select
	(8880),	f3 = 2	;le/le/le
	(8088),	f3 = 2	;le/le/le
	(8808),	f3 = 2	;le/le/le
	(0888),	f3 = 2	;le/le/le
	endusing

	using endq select
	(8100),	f3 = 24	;le/se
	(8001),	f3 = 24	;le/se
	(8010),	f3 = 24	;le/se
	(0810),	f3 = 24	;le/se
	(0801),	f3 = 24	;le/se
	(0081),	f3 = 24	;le/se
	endusing
	
	using endq select
	(1800),	f3 = 42	;se/le
	(1008),	f3 = 42	;se/le
	(1080),	f3 = 42	;se/le
	(0180),	f3 = 42	;se/le
	(0108),	f3 = 42	;se/le
	(0018),	f3 = 42	;se/le

	(8810),	f3 = 224	;le/le/se
	(8801),	f3 = 224	;le/le/se
	(8081),	f3 = 224	;le/le/se
	(0881),	f3 = 224	;le/le/se

	(8110),	f3 = 244	;le/se/se
	(8101),	f3 = 244	;le/se/se
	(8011),	f3 = 244	;le/se/se
	(0811),	f3 = 244	;le/se/se

	(1180),	f3 = 442	;se/se/le
	(1108),	f3 = 442	;se/se/le
	(1018),	f3 = 442	;se/se/le
	(0118),	f3 = 442	;se/se/le

	(1180),	f3 = 422	;se/le/le
	(1108),	f3 = 422	;se/le/le
	(1018),	f3 = 422	;se/le/le
	(0118),	f3 = 422	;se/le/le

	endusing

;5-26-10 safety seal

	using endq select
	(9000),	f3 = 1	;ss
	(0900),	f3 = 1	;ss
	(0090),	f3 = 1	;ss
	(0009),	f3 = 1	;ss
	endusing

	using endq select
	(9900),	f3 = 1	;ss
	(9009),	f3 = 1	;ss
	(9090),	f3 = 1	;ss
	(0990),	f3 = 1	;ss
	(0909),	f3 = 1	;ss
	(0099),	f3 = 1	;ss
	endusing

	using endq select
	(9990),	f3 = 1	;ss
	(0999),	f3 = 1	;ss
	(9099),	f3 = 1	;ss
	(9909),	f3 = 1	;ss
	endusing

;5-26-10: combinations:
; le/ss
	using endq select
	(8900),	f3 = 21	;le/ss
	(8009),	f3 = 21	;le/ss
	(8090),	f3 = 21	;le/ss
	(0890),	f3 = 21	;le/ss
	(0809),	f3 = 21	;le/ss
	(0089),	f3 = 21	;le/ss
	endusing
	
	using endq select
	(9800),	f3 = 12	;ss/le
	(9008),	f3 = 12 ;ss/le
	(9080),	f3 = 12	;ss/le
	(0980),	f3 = 12	;ss/le
	(0908),	f3 = 12	;ss/le
	(0098),	f3 = 12	;ss/le

	(8890),	f3 = 221	;le/le/ss
	(8809),	f3 = 221	;le/le/ss
	(8089),	f3 = 221	;le/le/ss
	(0889),	f3 = 221	;le/le/ss

	(8990),	f3 = 211	;le/ss/ss
	(8909),	f3 = 211	;le/ss/ss
	(8099),	f3 = 211	;le/ss/ss
	(0899),	f3 = 211	;le/ss/ss

	(9980),	f3 = 112	;ss/ss/le
	(9908),	f3 = 112	;ss/ss/le
	(9098),	f3 = 112	;ss/ss/le
	(0998),	f3 = 112	;ss/ss/le

	(9980),	f3 = 122	;ss/le/le
	(9908),	f3 = 122	;ss/le/le
	(9098),	f3 = 122	;ss/le/le
	(0998),	f3 = 122	;ss/le/le

	endusing
; sp/ss
	using endq select
	(1900),	f3 = 41	;sp/ss
	(1009),	f3 = 41	;sp/ss
	(1090),	f3 = 41	;sp/ss
	(0190),	f3 = 41	;sp/ss
	(0109),	f3 = 41	;sp/ss
	(0019),	f3 = 41	;sp/ss
	endusing
	
	using endq select
	(9100),	f3 = 14	;ss/sp
	(9001),	f3 = 14 ;ss/sp
	(9010),	f3 = 14	;ss/sp
	(0910),	f3 = 14	;ss/sp
	(0901),	f3 = 14	;ss/sp
	(0091),	f3 = 14	;ss/sp

	(1190),	f3 = 441	;sp/sp/ss
	(1109),	f3 = 441	;sp/sp/ss
	(1019),	f3 = 441	;sp/sp/ss
	(0119),	f3 = 441	;sp/sp/ss

	(1990),	f3 = 411	;sp/ss/ss
	(1909),	f3 = 411	;sp/ss/ss
	(1099),	f3 = 411	;sp/ss/ss
	(0199),	f3 = 411	;sp/ss/ss

	(9910),	f3 = 114	;ss/ss/sp
	(9901),	f3 = 114	;ss/ss/sp
	(9091),	f3 = 114	;ss/ss/sp
	(0991),	f3 = 114	;ss/ss/sp

	(9910),	f3 = 144	;ss/sp/sp
	(9101),	f3 = 144	;ss/sp/sp
	(9011),	f3 = 144	;ss/sp/sp
	(0911),	f3 = 144	;ss/sp/sp

	endusing

;sp/le/ss
	using endq select
	(0189),	f3 = 421	;sp/le/ss
	(0198),	f3 = 412	;sp/ss/le
	(0819),	f3 = 241	;le/sp/ss
	(0891),	f3 = 214	;le/ss/sp
	(0918),	f3 = 142	;ss/sp/le
	(0981),	f3 = 124	;ss/le/sp

	(1089),	f3 = 421	;sp/le/ss
	(1098),	f3 = 412	;sp/ss/le
	(8019),	f3 = 241	;le/sp/ss
	(8091),	f3 = 214	;le/ss/sp
	(9018),	f3 = 142	;ss/sp/le
	(9081),	f3 = 124	;ss/le/sp

	(1809),	f3 = 421	;sp/le/ss
	(1908),	f3 = 412	;sp/ss/le
	(8109),	f3 = 241	;le/sp/ss
	(8901),	f3 = 214	;le/ss/sp
	(9108),	f3 = 142	;ss/sp/le
	(9801),	f3 = 124	;ss/le/sp

	(1890),	f3 = 421	;sp/le/ss
	(1980),	f3 = 412	;sp/ss/le
	(8190),	f3 = 241	;le/sp/ss
	(8910),	f3 = 214	;le/ss/sp
	(9180),	f3 = 142	;ss/sp/le
	(9810),	f3 = 124	;ss/le/sp
	endusing

	return
;----------------------------------------

