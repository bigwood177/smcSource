;jobbar.cp
; created from dptbar.cp 9-24-12
;
;	JOB STATUS UPDATE
;
;
EXTERNAL FUNCTION
	GU_NAME	,A


	.include 'wnd:windows.def'

	.include 'def:hpsub.def'


.DEFINE	OPN,	0
.DEFINE	BURN,	1
.DEFINE	FORM,	2
.DEFINE	CMPLT,	3

record	funkey
	.include 'def:funkey.def'

;
RECORD	JOBHDR
	.INCLUDE 'DEF:RD071A.DEF'

RECORD	JOBLIN
	.INCLUDE 'DEF:RD072A.DEF'

RECORD	SALMAN
	.INCLUDE 'DEF:RD054A.DEF'

RECORD	COPTBL
	.INCLUDE 'DEF:RD182A.DEF'

RECORD	JOBTRX
	.INCLUDE 'DEF:RD073A.DEF'

RECORD	DPTDET
	.INCLUDE 'DEF:RD198A.DEF'

RECORD	DUCACC
	.INCLUDE 'DEF:RD175A.DEF'

RECORD	HLINE
	,A*,'DATE ENTERED  SALESMAN   EXT  PO NUMBER  JOB NUMBER  SHIP VIA'

RECORD	HLINE2
	,A*,'  DEPARTMENT           STATUS   COMMENT'

RECORD	DPT_DATA
	DPTARA	,20D3
	DPTDSC	,20A30
	DPTSTS	,20A16	;ZX/XX/XXXX HH:MM
	NUMDPT	,D2

RECORD	DT_FORMAT
	DT_DEC	,D12
RECORD,X
	YYYY	,D4
	MM	,D2
	DD	,D2
	HH	,D2
	MIN	,D2

record	pop
	.include 'def:pop1.def'

RECORD	WN_NAME
		,A5,	'dpsts'
	WN_TNMBR,D4

RECORD	WARS
	W_ID	,D4
	WND_1	,D4

RECORD	CHANNEL
	CHN071	,D2
	CHN072	,D2
	CHN182	,D2
	CHN073	,D2
	CHN198	,D2

RECORD	VARS
	JT_COMM	,A20
	JLCNT	,D6
	T_DPTSTS	,A16
	stsfil	,a14		;smc:JOBTRX.smm
	dfl_burn	,a20	;default burn comment
	dfl_form	,a20	;defautl form comment
	dfl_cmplt	,a20	;default compl comment
	fkey	,d3
	man	,d1	;1=allow manual comment
	OPS	,D1
	SAVDPT	,A2
	X_BURN	,D1
	X_FORM	,D1
	NEXT	,D1
	b_seq	,d3
	A6	,a6
	A20	,A20
	savrfa	,a6
	tstat	,d1
	LINARA	,20A2		;DEPTS IN JOBLIN 
	is_vulcan	,d1
	CMPCOD	,A3
	DT	,A20
	D	,D8
	T	,D6
	pline	,a80
	GOTP	,D1
	GOTO	,D1
	UNAME	,A25
	I	,D5
	J	,D5
	BLANKS	,A30
	XDATE	,D8
	D_LINE	,A60
	DCHAR	,D3
	TCHAR	,D3
	OPNOK	,D1
	XORD	,D6
	yord	,d6
	ROW	,D2
	COL	,D2
	ENTRY	,A30
	INXCTL	,D1
	CNGCTL	,D1
	WHATNO	,D2
	WDEPT	,A2
	LOKCTL	,D1

	READ	,D1,0
	WRITE	,D1,1
	STORE	,D1,2
	SWITCH	,D1
	V	,D1

.PROC
	XCALL TERID (V)

	UNAME = %GU_NAME
	UPCASE UNAME

	XCALL WHO(CMPCOD)
	
	CALL OPENS
	IF (.NOT. OPNOK) GOTO ENDOFF

	call init_window
DISPLA,

;	onerror f073_not_open
;	close chn073
;f073_not_open,
;	offerror

	CLEAR CNGCTL
	XCALL W_DISP(WND_1,WD_CLEAR)
	XCALL w_disp (wnd_1, wd_pos, 1,1,'Fab Ticket Completion')
	XCALL w_disp (wnd_1, wd_pos, 3,4,'TICKET #')
	xcall w_updt

	XCALL wINpT (wnd_1,3, 14, 11, 00 , 'AE', ENTRY, INXCTL)
	IF (INXCTL .EQ. 9) GOTO ENDOFF
	GOTO (DISPLA, ENDOFF), INXCTL
	IF (ENTRY(1,2) .EQ. 'S~')
	THEN	BEGIN
		A6 = ENTRY(3,8)
		IF (ENTRY(9,9) .EQ. '~')
		THEN	WDEPT = ENTRY(10,11)
		ELSE	CLEAR WDEPT
		END

	ELSE	A6 = ENTRY(1,6)

	onerror not_num
	xord = a6
	offerror
	goto is_num
not_num,
	offerror
	goto displa
is_num,
	xcall w_disp(wnd_1, wd_pos, 3, 14, wd_clr, wdc_eol, a6)
	xcall w_updt

;11-8-12:	there will be no DPTSTS file for fab tickets (jobs),
;		all data will come from the job header or the jobtrx file.
;
;		get the job and display header info., including a line for
;		each dept printed on the fab ticket.
;
;		next read thru the trx for the current job and save the date
;		of the last trx for each dept.
;
;		scanning the barcode at the bottom of the fab ticket
;			format S~000445~E 
;		will cycle thru the operations associated with the dept.
;

	CALL GETJOB
	IF (LOKCTL) GOTO DISPLA

;	open (chn073, su, stsfil)	;12-27-2011 try to keep records unlocked

;;;	CALL DSPDPT
	clear savdpt
	IF (WDEPT .NE. A6) GOTO NEXT_DEPT

ANYCN,
	offerror

	clear b_seq, wdept, ops
	clear dfl_burn, dfl_form, dfl_cmplt

	XCALL w_disp (wnd_1, wd_pos, 24,1,wd_clr,wdc_eol,'DEPT TO CHANGE:                  <F1> manu<tab> for ops <PM>print Main <VM>view Main')
;;;	XCALL winpT (wnd_1, 24, 19, 02, 00, 'AX' ,ENTRY, INXCTL,fkey)
	XCALL winTT (wnd_1, 24, 19, 11, 00, 'AX' ,ENTRY, INXCTL,fkey)


	IF (INXCTL .EQ. 9) GOTO ENDOFF
	using fkey select
	(f_01),	begin
		man = 1			;manual comment
		wdept = savdpt
		goto next_dept
		end
	(),	man = 0
	endusing

	GOTO (DISPLA, PROCES,OPS), INXCTL


	if (entry(1,2) .eq. 'S~')
	then	begin
		A6 = ENTRY(3,8)
		onerror anycn
		yord = a6
		offerror
		if (xord .ne. yord) goto anycn

		wdept = entry(10,11)
		goto next_dept
	;;;	b_seq = entry(6,8)
		end
	else	WDEPT = ENTRY(1,2)


	IF (WDEPT .EQ. '  ') GOTO DISPLA

	GOTO NEXT_DEPT
OPS,
	OPS = 1
	WDEPT = SAVDPT


NEXT_DEPT,		

	GOTO ANYCN

;---------------------------------------------------------

CNGDPT,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	JT_JOB = JHNUM

;;;	XCALL ISIO (CHN073, JOBTRX, JT_KEY, READ, LOKCTL)
;;;	IF (LOKCTL) RETURN
	ROW = I + 8


	IF (MAN .EQ. 1)
		BEGIN
		COL = 53
		XCALL winpT (wnd_1, ROW, 33, 20, 00, 'A ', ENTRY, INXCTL)
	;;;	XCALL winTT (wnd_1, ROW, 33, 20, 00, 'A ', ENTRY, INXCTL)
;;;		IF (INXCTL .EQ. 0) 	F_COMM = ENTRY(1,20)
		GOTO UPDSTS
		END



	CALL GET_OPS		;12-21-11 need this for default op
	IF (OPS .NE. 1) GOTO NO_DOP

	XCALL TTSTS(TSTAT)
	IF (TSTAT) 
		begin
		xcall w_disp(wnd_1,wd_pos,1,1)
		XCALL W_DISP(WND_1,WD_READS,ENTRY)
		end
	IF (NUMARA .LE. 0) return

	DLINE = 
	if (b_seq .gt. 0)
	then	begin
		p_action = 1
		pi = b_seq
		end
	else	xcall pop1 (pop)	


	using p_action select
	(1,4),	begin	;select, cr
		if (pi.gt.0) savrfa = parrfa(pi)
		read (chn182, coptbl, rfa:savrfa) [err=cngdpt]
	;;;	F_stat = op_seq
		JT_comm = op_desc
		xcall w_disp(wnd_1, wd_pos, row, 33, JT_comm)
		xcall w_updt		
		end
	endusing

	goto updsts	;1-16-12
;;;	return		;don't update status for operations.


no_dop,

	call get_table

	next = JT_stat
	for i from 1 thru 4
		begin
		incr next
		if (next.eq.BURN .and. x_burn.eq.1) exitloop
		if (next.eq.FORM .and. x_form.eq.1) exitloop
		if (next.eq.CMPLT) exitloop
		if (next.gt.CMPLT)
			begin
			next = 0
			exitloop
			end
		end

	JT_stat = next


	using JT_stat select
	(OPN),	begin
		D_LINE(24,28) = 'OPEN '
		end
	(BURN),	begin
		D_LINE(24,28) = 'BURN '
		if (JT_comm.eq.a20) JT_comm = dfl_burn
		end
	(FORM),	begin
		D_LINE(24,28) = 'FORM '
		if (JT_comm.eq.a20) JT_comm = dfl_form
		end
	(CMPLT),begin
		D_LINE(24,28) = 'COMPL'
		if (JT_comm.eq.a20) JT_comm = dfl_cmplt
		end
	endusing
	xcall w_disp (wnd_1, wd_pos, row, 33, JT_comm)
	xcall w_disp (wnd_1, wd_pos, row, 24,d_line(24,28))
	xcall w_updt


UPDSTS,
	SAVDPT = WDEPT

	CLEAR DPTDET
	CALL TIME_STAMP			;SSQ 10-26-06

;ADD TRX HERE...

;;;	XCALL ISIO (CHN073, JOBTRX, JT_KEY, WRITE, LOKCTL)
;;;
;;;	DE_ORD = JT_JOB
;;;	DE_DPT = F_DEPT
;;;	DE_STAT = F_STAT
;;;	XCALL ISIO (CHN198, DPTDET, DE_KEY, STORE, LOKCTL)

	RETURN
;---------------------------------------------------------
GET_OPS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; SCAC ALPHA LOOK-UP
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR NUMARA, I
	CLEAR TBL_KEY
	TBLCOD = 'CS'
	TBLKEY = JHDEPT
	FIND (CHN182,COPTBL,TBL_KEY)[ERR=NXTDOP]
NXTDOP,
	READS (CHN182,COPTBL,GETRFA:SAVRFA) [ERR=ENDDOP]
	IF (TBLCOD .NE. 'CS') GOTO ENDDOP
	IF (OP_DEPT .NE. JHDEPT) GOTO ENDDOP
	INCR I
	DLINE = OP_DESC
	PARRY(I) = DLINE
	PARRFA(I) = SAVRFA

	USING OP_DFL SELECT
	(BURN),	DFL_BURN = OP_DESC
	(FORM),	DFL_FORM = OP_DESC
	(CMPLT),DFL_CMPLT= OP_DESC
	ENDUSING

	GOTO NXTDOP

ENDDOP,
	NUMARA = I
	RETURN

	RETURN
;--------------------------------------------------


PROCES,
	GOTO DISPLA

ENDOFF,
	CALL CLOSE
	IF (UNAME(1,5) .EQ. 'PLANT')
		BEGIN
		XCALL PGCHN('CP:SHPMNU',1)
		END
	XCALL PGCHN ('CP:CPLMNU',1)
	
	CALL GET_TABLE

	D_LINE(3,4) = DD_DEPT
	D_LINE(6,20) = DD_DESC

	USING JT_STAT SELECT
	(0),	D_LINE(24,28) = 'OPEN '
	(.gt.1),	D_LINE(24,28) = 'COMPL'
	ENDUSING

	using JT_stat select
	(OPN),	D_LINE(24,28) = 'OPEN '
	(BURN),	D_LINE(24,28) = 'BURN '
	(FORM),	D_LINE(24,28) = 'FORM '
	(CMPLT),D_LINE(24,28) = 'COMPL'
	endusing

	D_LINE(33,52) = JT_COMM
	INCR ROW

	pline = d_line

	XCALL w_disp (wnd_1, wd_pos, ROW, 1, D_LINE)
	IF (JT_STAT) XCALL XYB(ROW,24,d_line(24,28), wnd_1)

;;;	GOTO NXTDPT

EOF_DPT,
	RETURN
;---------------------------------------------------------


TIME_STAMP,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; TIME & DATE STAMP			;SSQ 10-26-06
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	DT = %DATETIME
	JT_DATIME = %DATETIME
;;;	F_DATE = DT(1,8)
;;;	F_TIME = DT(9,12)
;;;
;;;	DE_DATE = F_DATE
;;;	DE_TIME = F_TIME

	RETURN
;----------------------------------------------------------


GET_TABLE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; READ DEPT FROM COPTBL
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR TBL_KEY
	TBLCOD = 'DD'
	DD_DEPT = JHDEPT	
	XCALL ISIO (CHN182, COPTBL, TBL_KEY, READ, LOKCTL)
	IF (LOKCTL) 
		BEGIN
		CLEAR COPTBL
		DD_DESC = '* NOT ON FILE *'
		END

	X_BURN = D_BURN
	X_FORM = D_FORM
	RETURN
;--------------------------------------------------------


GETJOB,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	FOR I FROM 1 THRU 20
		BEGIN
		DPTARA(I) =
		DPTDSC(I) =
		DPTSTS(I) =
		END

	CLEAR I

	XCALL ISIO (CHN071, JOBHDR, XORD, READ, LOKCTL)
	IF (LOKCTL)
		BEGIN
		XCALL wnmsg (wnd_1, 23,'JOB NOT ON FILE',1)
		RETURN
		END

	XCALL w_disp (wnd_1, wd_pos, 3,25,JHDEPT)

	XDATE(1,4) = JHSHDT(5,8)
	XDATE(5,8) = JHSHDT(1,4)
	ENTRY(1,12) = XDATE,	'ZX/XX/XXXX'
	XCALL w_disp (wnd_1, wd_pos, 5,1,ENTRY(1,12))


	CLEAR JLCNT
	FIND (CHN072, JOBLIN, JHNUM) [ERR=JL_LOOP]
	
JL_LOOP,
	XCALL IOS (CHN072, JOBLIN, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO JL_EOF
	IF (JLNUM .NE. JHNUM) GOTO JL_EOF
	INCR JLCNT			;JUST DISPLAY 3 LINES
	XCALL W_DISP (WND_1, WD_POS, JLCNT+5, 1, JLITM)
	IF (JLCNT .LT. 3) GOTO JL_LOOP
JL_EOF,

	CLEAR TBL_KEY
	TBLCOD = 'OP'
	OP_DEPT = JHDEPT
	FIND (CHN182, COPTBL, TBL_KEY) [ERR=OP_LOOP]
OP_LOOP,
	XCALL IOS (CHN182, COPTBL, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO OP_EOF
	IF (OP_DEPT .NE. JHDEPT) GOTO OP_EOF
	IF (OP_WRK .NE. 1) GOTO OP_LOOP		;WORK DPT'S ONLY
	INCR I
	DPTARA(I) = OP_SEQ
	DPTDSC(I) = OP_DESC	
	GOTO OP_LOOP
OP_EOF,
	NUMDPT = I
	CLEAR JOBTRX
	JT_JOB = JHNUM
	FIND (CHN073, JOBTRX, JT_KEY) [ERR=JT_LOOP]
JT_LOOP,
	XCALL IOS (CHN073, JOBTRX, READ, LOKCTL)
	IF (LOKCTL .NE. 0) GOTO JT_EOF
	IF (JT_JOB .NE. JHNUM) GOTO JT_EOF
	CALL SET_STATUS
	GOTO JT_LOOP
JT_EOF,
;DISPLAY STATUS HERE
	ROW = 9

	FOR I FROM 1 THRU NUMARA
		BEGIN
		XCALL W_DISP (WND_1, WD_POS, ROW+I, 5, DPTDSC(I) )
		XCALL W_DISP (WND_1, WD_POS, ROW+I, 36, DPTSTS(I) )
		END

	xcall w_updt

	reads (15,entry)
	RETURN

SET_STATUS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	DT_DEC = JT_DATIME

	CLEAR DPTSTS
	DPTSTS(1,2) = MM,	'ZX'
	DPTSTS(3,3) = '/'
	DPTSTS(4,5) = DD,	'XX'
	DPTSTS(6,6) = '/'
	DPTSTS(7,10) = YYYY,	'XXXX'
	DPTSTS(12,13) = HH,	'ZX'
	DPTSTS(14,14) = ':'
	DPTSTS(15,16) = MM,	'ZZ'

	FOR I FROM 1 THRU NUMDPT
		BEGIN
		IF (JT_OP .EQ. DPTARA(I) )
			BEGIN
			DPTSTS(I) = T_DPTSTS
			EXITLOOP
			END
		END

	RETURN
;---------------------------------------------------------

OPENS,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CLEAR OPNOK

	SWITCH = 5
	XCALL FILES (4, 'SI', 071, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN071 = 4

	XCALL FILES (5, 'SI', 072, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN072 = 5



	XCALL FILES (17, 'SI',  182, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN182 = 17

	XCALL FILES (9, 'SI', 073, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	xcall ffile (073, stsfil, switch)
	stsfil(14,14) = 'M'
	CHN073 = 9


	XCALL FILES (10, 'SU', 198, SWITCH)
	IF (SWITCH .EQ. 9) RETURN
	CHN198 = 10

	OPNOK = 1
	RETURN
;---------------------------------------------

CLOSE,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF (CHN071) CLOSE CHN071
	IF (CHN072) CLOSE CHN072
	IF (CHN073) CLOSE CHN073
	if (chn182) close chn182
	IF (CHN198) CLOSE CHN198

	RETURN
;---------------------------------------------
ACCEPT,
	XCALL FLAGS (00010000,1)
	XCALL w_disp (wnd_1, wd_pos, ROW,COL,' ')
	XCALL W_DISP(W_ID,wd_wait, 30, WD_ACCEPT,TCHAR) ; INPUT 1 DECIMAL CHARACTER AT A TIME
;;;	ACCEPT (15,TCHAR,WAIT:60) [ERR=AC_RET]
	IF (TCHAR.EQ.10) GOTO ACCEPT
AC_RET,
	if (tchar.eq.13) XCALL W_DISP(W_ID,wd_wait, 30, WD_ACCEPT,TCHAR) ; INPUT 1 DECIMAL CHARACTER AT A TIME
;;;	IF (TCHAR.EQ.13) ACCEPT (15,DCHAR)
	XCALL FLAGS (00010000,0)
	RETURN

INIT_WINDOW,	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	xcall u_start(,,,,,,,99)

	XCALL TNMBR (WN_TNMBR)
	XCALL W_PROC(WP_FIND,WND_1,WN_NAME)
	IF (WND_1.EQ.0)
		BEGIN
		XCALL W_PROC(WP_CREATE,WND_1,WN_NAME,0,0)	;full size
		END
	XCALL W_BRDR(WND_1,WB_TITLE,'Order Completion Status',
&			WB_TPOS,WBT_TOP,WBT_CENTER)
	XCALL W_PROC(WP_PLACE,WND_1,2,2)	
	XCALL W_DISP(WND_1,WD_CLEAR)

	W_ID = WND_1


;; POP info...

	MAXARA = 20
	PLEN = 50
	NUMROW = 15
	WX = 12
	WY = 4
	POP_WID(1,5) = "DPSTS"
	POP_WID(6,8) = WN_TNMBR,	'XXX'
	POP_TITLE = "Status"
	RETURN
;-----------------------------------------------------------------
.END

.SUBROUTINE XYB
	X	,D
	Y	,D
	TEXT	,A
	w_id	,d

.include 'wnd:windows.def'

.PROC
	xcall w_disp (w_id, wd_attr, attr_set+attr_rvrs)
	xcall w_disp (w_id, wd_pos, x, y, text)
	xcall w_disp (w_id, wd_attr, attr_clr+attr_rvrs)

;;;	DISPLAY (15, $SCR_ATT(REVERSE), $SCR_POS(X,Y), TEXT)
;;;	DISPLAY (15, $SCR_ATT(CLEAR))
	RETURN
.END

