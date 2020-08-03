;sg.y

	if (item(1,2) .eq. 'SG')	call sg_pipe

sg_pipe,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	using fd_f1 select
	(64,65,66),	nop
	(),		return	;not alum, s04, s16
	endusing

	xcall alpdc (item(3,4), dia, switch)
	if (switch) return

	using fd_ga select
	(18),	call sg_18
	(20),	call sg_20
	(22),	call sg_22
	(24),	call sg_24
	endusing

	return
;------------------------------------------------
sg_18,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	for k from 1 thru 40
		begin
		using fd_f1 select
		(64),	begin
			if (dia.eq.s304dia_18(k))
				begin
				s304tl_18(k) = s304tl_18(k) + fd_spl
				exitloop
				end
			if (s304dia_18(k) .eq. 0)
				begin
				s304tl_18(k) = fd_spl
				s304prc_18(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		(65),	begin
			if (dia.eq.s316dia_18(k))
				begin
				s316tl_18(k) = s316tl_18(k) + fd_spl
				exitloop
				end
			if (s316dia_18(k) .eq. 0)
				begin
				s316tl_18(k) = fd_spl
				s316prc_18(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		(66),	begin
			if (dia.eq.alumdia_18(k))
				begin
				alumtl_18(k) = alumtl_18(k) + fd_spl
				exitloop
				end
			if (alumdia_18(k) .eq. 0)
				begin
				alumtl_18(k) = fd_spl
				alumprc_18(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		endusing		
		end

	return
;------------------------------------------------


sg_20,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	for k from 1 thru 40
		begin
		using fd_f1 select
		(64),	begin
			if (dia.eq.s304dia_20(k))
				begin
				s304tl_20(k) = s304tl_20(k) + fd_spl
				exitloop
				end
			if (s304dia_20(k) .eq. 0)
				begin
				s304tl_20(k) = fd_spl
				s304prc_20(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		(65),	begin
			if (dia.eq.s316dia_20(k))
				begin
				s316tl_20(k) = s316tl_20(k) + fd_spl
				exitloop
				end
			if (s316dia_20(k) .eq. 0)
				begin
				s316tl_20(k) = fd_spl
				s316prc_20(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		(66),	begin
			if (dia.eq.alumdia_20(k))
				begin
				alumtl_20(k) = alumtl_20(k) + fd_spl
				exitloop
				end
			if (alumdia_20(k) .eq. 0)
				begin
				alumtl_20(k) = fd_spl
				alumprc_20(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		endusing		
		end

	return
;------------------------------------------------


sg_22,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	for k from 1 thru 40
		begin
		using fd_f1 select
		(64),	begin
			if (dia.eq.s304dia_22(k))
				begin
				s304tl_22(k) = s304tl_22(k) + fd_spl
				exitloop
				end
			if (s304dia_22(k) .eq. 0)
				begin
				s304tl_22(k) = fd_spl
				s304prc_22(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		(65),	begin
			if (dia.eq.s316dia_22(k))
				begin
				s316tl_22(k) = s316tl_22(k) + fd_spl
				exitloop
				end
			if (s316dia_22(k) .eq. 0)
				begin
				s316tl_22(k) = fd_spl
				s316prc_22(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		(66),	begin
			if (dia.eq.alumdia_22(k))
				begin
				alumtl_22(k) = alumtl_22(k) + fd_spl
				exitloop
				end
			if (alumdia_22(k) .eq. 0)
				begin
				alumtl_22(k) = fd_spl
				alumprc_22(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		endusing		
		end

	return
;------------------------------------------------


sg_24,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	for k from 1 thru 40
		begin
		using fd_f1 select
		(64),	begin
			if (dia.eq.s304dia_24(k))
				begin
				s304tl_24(k) = s304tl_24(k) + fd_spl
				exitloop
				end
			if (s304dia_24(k) .eq. 0)
				begin
				s304tl_24(k) = fd_spl
				s304prc_24(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		(65),	begin
			if (dia.eq.s316dia_24(k))
				begin
				s316tl_24(k) = s316tl_24(k) + fd_spl
				exitloop
				end
			if (s316dia_24(k) .eq. 0)
				begin
				s316tl_24(k) = fd_spl
				s316prc_24(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		(66),	begin
			if (dia.eq.alumdia_24(k))
				begin
				alumtl_24(k) = alumtl_24(k) + fd_spl
				exitloop
				end
			if (alumdia_24(k) .eq. 0)
				begin
				alumtl_24(k) = fd_spl
				alumprc_24(k) = (fd_price*100/fd_spl)	;per inch
				exitloop
				end
			end
		endusing		
		end

	return
;------------------------------------------------
