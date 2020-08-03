;
;	DUCMNT.COP
;
;
;	Program to allow for maintenance for the parameters used in the
;	entry of rectangular duct work.
;
;
;
RECORD DUCFIL	; Table of all parameters necessary
	.INCLUDE 'DEF:RD174A.DEF'
;
RECORD	CTL
	ROW	,D2
		,A1
	COL	,D2
		,A1
	MAX	,D2
		,A1
	MIN	,D2
		,A1
	TYPE	,A2

RECORD DEFLTS
	POUND	,5D4,0906,1156,1406,1656,2156
		; These are the default pounds per square foot for gauges
		; from 26-18.
	SEAMAL	,5D3,264,337,410,483,629
		; These are the default seam allowance 's
	DEFNUM	,D6,00005



RECORD
	MODEID	,3A14,'ADD           ','CHANGE/INQUIRE','DISPLAY       '
	ENTRY	,A10
	RECNO	,D5
	SWITCH	,D1
	SELECT	,D1
	CNGCTL	,D1
	V	,D1
	INXCTL	,D1
	WHATNO	,D2
	DECMAL	,D18
	READ	,D1,	0
	WRITE	,D1,	1
	LOKCTL	,D1
	KGAGE	,D2
	SUB	,D2
	OPTION	,D2

PROC
	OPEN (15,I,'TT:')
	XCALL TERID (V)
	SWITCH = 1
	XCALL FILES (1,'U',174,SWITCH)
	IF (SWITCH.EQ.9) XCALL PGCHN ('CP:CPMENU',1)

BEGIN,
	SELECT = -1
	XCALL OUTPT (1,1,2,'Rectangular Ductwork Parameter Maintenance',V)
	XCALL OUTPT (10,20,0,'PLEASE SELECT APPLICATION',1)
	XCALL OUTPT (12,25,0,'1. Add new Gauge parameters ',1)
	XCALL OUTPT (14,25,0,'2. Change/inquire Gauge parameters ',1)
	XCALL OUTPT (16,25,0,'3. Display Gauge Parameters ',1)

MNUSEL,
	XCALL INPUT (10,47,1,0,'#E',ENTRY,INXCTL,1)	; INPUT SELECTED OPTION
	GO TO (MNUSEL,ENDOFF), INXCTL
	SELECT = ENTRY
	GO TO (DSPMOD,DSPMOD,DSPMOD), SELECT
	GO TO MNUSEL

DSPMOD,							; DISPLAY MODE
	XCALL OUTPT (2,1,2,MODEID(SELECT),1)
	GO TO (DISPLA,DISPLA,SEE), SELECT
	XCALL WAIT (3,V)

ENDOFF,
	XCALL FILES(1,'U',174,4)
	XCALL PGCHN ('CP:CPMENU',1)

SEE,
	; DISPLAY INFO TO SCRENE

DISPLA,
;	UNLOCK 1
	CNGCTL =
	DUCFIL  =
	XCALL OUTPT (2,3,2,'1. Gauge        ',V)	; ducgag d2
	XCALL OUTPT (3,3,0,'2. Pounds/sq ft ',V)	; duclbs d6
	XCALL OUTPT (4,3,0,'3. Waste %      ',V)	; ducwas d5
	XCALL OUTPT (5,3,0,'4. Corners      ',V)	; duccor d2
	XCALL OUTPT (6,3,0,'5. Cleats       ',V)	; duccle d4
	XCALL OUTPT (7,3,0,'6. Nuts / bolts ',V)	; ducnb  d6
	XCALL OUTPT (2,40,0,'7. Liner overage  ',V)	; dline  d5
	XCALL OUTPT (3,40,0,'8. Seam Allowance ',V)	; ducsea d5
	XCALL OUTPT (4,40,0,'9. TDC Nuts/bolts ',V)	; ductnv d6
	XCALL OUTPT (5,40,0,'10. Slips          ',V)	; ducslp d3
	XCALL OUTPT (6,40,0,'11. Drives         ',V)	; ducdrv d3
	XCALL OUTPT (7,40,0,'12. Price per pound',V)	; ducprc d8

GAUGE,
	IF (SELECT.NE.1.AND.CNGCTL) GO TO BADCNG
	CALL DSPGAG
	CTL = '02,22,02,01,#E'
	CALL INPUT
	GO TO (DISPLA,BEGIN), INXCTL
	KGAGE = ENTRY (1,2)
	IF ( KGAGE.NE.26.AND.KGAGE.NE.24.AND.KGAGE.NE.22.AND.KGAGE.NE.20.AND.KGAGE.NE.18.AND.KGAGE.NE.16) GOTO GAUGE
	IF ( KGAGE.EQ.26 ) SUB = 1
	IF ( KGAGE.EQ.24 ) SUB = 2
	IF ( KGAGE.EQ.22 ) SUB = 3
	IF ( KGAGE.EQ.20 ) SUB = 4
	IF ( KGAGE.EQ.18 ) SUB = 5
	LOKCTL = 1
	XCALL IO (1,DUCFIL,KGAGE,READ,LOKCTL)
	IF ( DUCFIL( 1,6 ).EQ.']]]]]]'.AND.SELECT.EQ.1 ) GOTO OK
	IF ( DUCFIL( 1,6 ).EQ.']]]]]]' ) GOTO BADGAG
	IF ( DUCFIL( 1,6 ).NE.']]]]]]'.AND.SELECT.EQ.1 ) GOTO GAGONF

OK,
;	DUCFIL =
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	IF ( SELECT.EQ.2 ) GOTO SHOREC
	DUCGAG = KGAGE
	GOTO (ANYCNG), CNGCTL

POUNDS,
	CALL DEFPND			; Display acutal pounds per square ft
	CTL = '03,22,06,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DUCLBS = ENTRY( 1,6 )
	IF ( DUCLBS.EQ.0 ) DUCLBS = POUND( SUB )
	DECMAL = DUCLBS
	CALL DISP4
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	GOTO (ANYCNG), CNGCTL

WASTE,
	CALL DSPWAS		; Display default waste information
	CTL = '04,22,05,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DUCWAS = ENTRY( 1,5 )
	DECMAL = DUCWAS
	CALL DISP4
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	GOTO (ANYCNG), CNGCTL

CORNR,
	CALL DSPCOR		; Display default corner information
	CTL = '05,22,02,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DUCCOR = ENTRY( 1,2 )
	IF ( DUCCOR.EQ.0 ) DUCCOR = 08
	DECMAL = DUCCOR
	CALL DSPNUM
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	GOTO (ANYCNG), CNGCTL

CLEAT,
	CALL DSPCLE		; Display default cleat information
	CTL = '06,22,04,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DUCCLE = ENTRY( 1,4 )
	IF ( DUCCLE.EQ.0 ) DUCCLE = 15
	DECMAL = DUCCLE
	CALL DISP1
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	GOTO (ANYCNG), CNGCTL

NUTS,
	CALL DSPNUT		; Display default nut/bolt information
	CTL = '07,22,06,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DUCNB = ENTRY( 1,6 )
	IF ( DUCNB.EQ.0 ) DUCNB = 5
	DECMAL = DUCNB
	CALL DISP1
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	GOTO (ANYCNG), CNGCTL

LINER,
	CALL DSPLIN		; Display default liner information
	CTL = '02,60,05,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DLINE = ENTRY( 1,5 )
	DECMAL = DLINE
	CALL DISP4
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	XCALL OUTPT (20,1,1,'\',1 )
	GOTO (ANYCNG), CNGCTL

SEAM,
	CALL DSPSEA		; Display seam allowance information
	CTL = '03,60,05,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DUCSEA = ENTRY( 1,5 )
	IF ( DUCSEA.EQ.0 ) DUCSEA = SEAMAL( SUB )
	DECMAL = DUCSEA
	CALL DISP4
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	XCALL OUTPT (20,1,1,'\',1 )
	XCALL OUTPT (21,1,1,'\',1 )
	GOTO (ANYCNG), CNGCTL

TDCNUT,
	CTL = '04,60,06,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DUCTNB = ENTRY( 1,6 )
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	XCALL OUTPT (20,1,1,'\',1 )
	GOTO (ANYCNG), CNGCTL

SLIPS,
	CALL DSPSLP		; Display slips information
	CTL = '05,60,03,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DUCSLP = ENTRY( 1,3 )
	XCALL OUTPT (17,1,1,'\',1 )
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	XCALL OUTPT (20,1,1,'\',1 )
	XCALL OUTPT (21,1,1,'\',1 )
	XCALL OUTPT (22,1,1,'\',1 )
	XCALL OUTPT (23,1,1,'\',1 )
	GOTO (ANYCNG), CNGCTL

DRIVES,
	CALL DSPDRV		; Display drives information
	CTL = '06,60,03,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DUCDRV = ENTRY( 1,3 )
	XCALL OUTPT (17,1,1,'\',1 )
	XCALL OUTPT (18,1,1,'\',1 )
	XCALL OUTPT (19,1,1,'\',1 )
	XCALL OUTPT (20,1,1,'\',1 )
	XCALL OUTPT (21,1,1,'\',1 )
	XCALL OUTPT (22,1,1,'\',1 )
	XCALL OUTPT (23,1,1,'\',1 )
	GOTO ( ANYCNG ), CNGCTL

DUCPRC,
	CALL DSPPRC		; Display default price information
	CTL = '07,60,08,00,# '
	CALL INPUT
	GO TO ( DISPLA ), INXCTL
	DUCPRC = ENTRY( 1,3 )
	DECMAL = DUCPRC
	CALL DISP4
	XCALL OUTPT (17,1,1,'\',1 )
	XCALL OUTPT (18,1,1,'\',1 )
	GOTO ( ANYCNG ), CNGCTL

CNGBR,
	GO TO (GAUGE,POUNDS,WASTE,CORNR,CLEAT,NUTS,LINER,SEAM,TDCNUT,SLIPS,
&		DRIVES,DUCPRC ), WHATNO

BADCNG,
	CNGCTL = 3
	GO TO ANYCNG

SHOREC,
	CTL = '03,22,06,00'
	DECMAL = DUCLBS
	CALL DISP4

	ROW = 04
	MAX = 05
	DECMAL = DUCWAS
	CALL DISP4

	ROW = 05
	ENTRY(1,2) = DUCCOR,'ZX'
	XCALL OUTPT( 05,22,0,ENTRY( 1,2 ),V)
	ENTRY(1,4) = DUCCLE,'ZZX.X'
	XCALL OUTPT( 06,22,0,ENTRY( 1,4 ),V)
	ROW = 07
	DECMAL = DUCNB
	CALL DISP1
	ROW = 02
	COL = 60
	DECMAL = DLINE
	CALL DISP4
	ROW = 03
	DECMAL = DUCSEA
	CALL DISP4
	ROW  = 04
	DECMAL = DUCTNB
	CALL DSPNUM
	ROW = 05
	DECMAL = DUCSLP
	CALL DSPNUM
	ROW = 06
	DECMAL = DUCDRV
	CALL DSPNUM
	ROW = 07
	MAX = 08
	DECMAL = DUCPRC
	CALL DISP4
	GOTO ANYCNG

PROCES,
	XCALL WAIT (3,1)
	GO TO ( ADD,CHANGE ), SELECT

ADD,
	LOKCTL = 1
	XCALL IO (1,DUCFIL,KGAGE,WRITE,LOKCTL)
	GO TO FREBUF

CHANGE,
	LOKCTL = 1
	XCALL IO (1,DUCFIL,KGAGE,WRITE,LOKCTL)
	IF (SELECT.EQ.2) GO TO FREBUF

FREBUF,
	LOKCTL = 1
	XCALL IO (1,DUCFIL,50,READ,LOKCTL)
	LOKCTL = 1
	XCALL IO (1,DUCFIL,1,READ,LOKCTL)
	GO TO DISPLA

DSPGAG,
	XCALL OUTPT (18,1,0,' Select 1 of the following: 26 24 22 20 18  ',1)
	RETURN

BADGAG,
	XCALL MESAG (' Gauge is not on file. ',1 )
	GOTO GAUGE

GAGONF,
	XCALL MESAG (' Gauge is all ready on file. ',1 )
	GOTO GAUGE

DEFPND,
	XCALL OUTPT (18,1,0,' The actual pounds per square foot is ',1 )
	ENTRY( 1,5 ) = '?????'
	ENTRY( 1,5 ) = POUND( SUB ),'X.XXX'
	XCALL OUTPT (0,0,0, ENTRY( 1,5 ), 1 )
	RETURN

DSPWAS,
	XCALL OUTPT (18,1,0,' Waste factor ( as a % ) 3% =  3.000',1)
	XCALL OUTPT (19,1,0,' this will be added to pounds per square foot',1)
	RETURN

DSPCLE,
;;;	XCALL OUTPT( 18,1,0,' Number of cleats is feet of gasket / 1.5',1)
	XCALL OUTPT( 18,1,0,' Number of cleats is feet of gasket / this value',1)
	RETURN

DSPCOR,
	XCALL OUTPT( 18,1,0,' Number of corners is 8 times the number of joints',1)
	RETURN

DSPNUT,
	XCALL OUTPT (18,1,0,' Number of nuts/bolts is 1/2 number of corners',1)
	XCALL OUTPT (19,1,0,' Nut/bolts = 0.5 ',1 )
	RETURN

DSPLIN,
	XCALL OUTPT( 18,1,0,' Enter the percentage of overage for liner',1)
	XCALL OUTPT( 19,1,0,' For example 100 sqft of duct, would require 110 sqft of liner.',1)
	XCALL OUTPT( 20,1,0,' So the liner would be 10.000% extra ',1)
	RETURN

DSPSEA,
	XCALL OUTPT( 18,1,0,' Enter the seam allowance ( in pounds per square foot) ',1)
	ENTRY( 1,5 ) = SEAMAL( SUB ), 'X.XXX'
	XCALL OUTPT ( 0,0,0,ENTRY( 1,5 ),1 )
	XCALL OUTPT (19,1,0,' The seam allowance is calculated per running foot',V)
	XCALL OUTPT (20,1,0,' So for a 4 foot length, it is 4 times this amount',V)
	RETURN

DSPSLP,
	XCALL OUTPT( 17,1,0,' Slips are the 700, and 800 series items, and are based on the following:',1)
	XCALL OUTPT( 18,1,0,'0  - 12 --> Flat "S" 26 gauge      ( 726 )',1)
	XCALL OUTPT( 19,1,0,'12 - 19 --> Flat "S" 24 gauge      ( 724 )',1)
	XCALL OUTPT( 20,1,0,'20 - 30 --> 1" Standing 24 gauge   ( 824 )',1)
	XCALL OUTPT( 21,1,0,'31 - 42 --> 1" standing 22 gauge   ( 822 )',1)
	XCALL OUTPT( 22,1,0,'43 - 49 --> 1.5" standing 22 gauge ( 1022 )',1)
	XCALL OUTPT( 23,1,0,'50 -120 --> uses reinforcing bar   ( 1022B )',1)
	RETURN

DSPDRV,
	XCALL OUTPT( 17,1,0,' Drives are the 700, and 800 series items, and are based on the following:',1)
	XCALL OUTPT( 18,1,0,'0  -  9 --> Flat 26 gauge      ( 60x )',1)
	XCALL OUTPT( 19,1,0,'10 - 19 --> Flat 24 gauge      ( 6x24 )',1)
	XCALL OUTPT( 20,1,0,'20 - 30 --> Standing 24 gauge  ( 6x24 )',1)
	XCALL OUTPT( 21,1,0,'31 - up --> Standing 22 gauge  ( 6x22 )',1)
	XCALL OUTPT( 22,1,0,'  the x above designates the size of duct ',1)
	RETURN

DSPPRC,
	XCALL OUTPT( 17,1,0,' Enter the price per pound for this gauge',V)
	RETURN

DISP1,
	OPTION = 5
	GOTO DSPLY

DISP4,
	OPTION = 4
	GOTO DSPLY

DSPNUM,
	OPTION = 1
	GOTO DSPLY

DSPLY,
	XCALL DSPLY( MAX,ROW,COL,DECMAL,OPTION,V)
	RETURN

INPUT,
	XCALL INPUT (ROW,COL,MAX,MIN,TYPE,ENTRY,INXCTL,V)
	RETURN

ANYCNG,
	XCALL ANYCN (CNGCTL,WHATNO)
	GO TO (PROCES,CNGBR,DISPLA), CNGCTL + 1
END
