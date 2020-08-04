!-TSX32 command file to set environment variables retrievable via XCALL GETLOG
!	for typical font descriptors such as TINYFONT,SMALLFONT,etc.  This is
!	ONE technique by which font descriptor values can be maintained outside
!	of DBL code itself.
!
!	It is functionally the same as a DOS BAT file or a UNIX SCRIPT file.
!	Edit this file as necessary to accomodate different operating systems.
!
!	Lines beginning with "!" are comments
!	Lines beginning with xxx: are labels to which you can "go to"
!	Lines of the form "ASSIGN/SYSTEM value logical" put "value" into the
!		the environment under the logical name "logical"  (The /SYSTEM
!		qualifier means the assignment applies to the whole system as
!		opposed to just an individual user)

! GOTO PDS	! use this line for Pacific 25 in 1
! GOTO HP3	! use this line for HP III
GOTO HP3TEST	! same as Pacific except uses a 6pt proport font for TINY

!
! Set up environment variables for main fonts
!

PDS:
!
! For Pacific 25 in 1 cartridge
!
!_____________________________________________________________________________
!              Suggested LPI:-->     12       8        6        6        4
!
!                                 TINYFONT SMALFONT STNDFONT LRGEFONT HUGEFONT
!
! _______________________orient   portrait portrait portrait portrait portrait
! | _____________________symbol   ASCII=0U PC8=10U  ASCII=0U ASCII=0U ASCII=0U
! | |    ________________spacing  fixed    fixed    fixed    fixed    fixed
! | |    | ______________pitch    27.2cpi  16.67cpi 12cpi    10cpi    6.5cpi
! | |    | |   __________points   3.6 pts  8.5 pts  10 pts   12pts    18pts
! | |    | |   |   ______style    upright  upright  upright  upright  upright
! | |    | |   |   | ____weight   medium   medium   medium   medium   bold
! | |    | |   |   | | __typeface gothic   courier  gothic   gothic   present
! | |    | |   |   | | |
! 1 0085 2 272 036 1 5 0006       TINYFONT
! 1 1085 2 167 085 1 5 0003                SMALFONT
! 1 0885 2 120 100 1 5 0006                         STNDFONT
! 1 0885 2 100 120 1 5 0006                                  LRGEFONT
! 1 0085 2 065 180 1 9 0011                                           HUGEFONT


ASSIGN/SYSTEM 100852272036150006 TINYfont
ASSIGN/SYSTEM 110852167085150003 SMALfont
ASSIGN/SYSTEM 100852120100150006 STNDfont
ASSIGN/SYSTEM 100852100120150006 LRGEfont
ASSIGN/SYSTEM 100852065180190011 HUGEfont
EXIT


HP3:
!
! For HP III	TINY & HUGE use proportional spacing CG Times (scalable)
!
!_____________________________________________________________________________
!              Suggested LPI:-->     12       8        6        6        4
!
!                                 TINYFONT SMALFONT STNDFONT LRGEFONT HUGEFONT
!
! _______________________orient   portrait portrait portrait portrait portrait
! | _____________________symbol   ROMAN=8U PC8=10U  ASCII=0U ASCII=0U ASCII=0U
! | |    ________________spacing  Proport  fixed    fixed    fixed    Proport
! | |    | ______________pitch    15.0cpi  16.67cpi 12cpi    10cpi    6.5cpi
! | |    | |   __________points   6.0 pts  8.5 pts  10 pts   12pts    18pts
! | |    | |   |   ______style    upright  upright  upright  upright  upright
! | |    | |   |   | ____weight   medium   medium   medium   medium   bold
! | |    | |   |   | | __typeface CG-times courier  gothic   gothic   CG-times
! | |    | |   |   | | |
! 1 0885 1 272 036 1 5 4101       TINYfont
! 1 0885 2 167 085 1 5 0003                SMALfont
! 1 0885 2 120 100 1 5 0003                         STNDfont
! 1 0885 2 100 120 1 5 0003                                  LRGEfont
! 1 0085 1 065 180 1 9 4101                                           HUGEfont

ASSIGN/SYSTEM 108851272036154101 TINYfont
ASSIGN/SYSTEM 108852167085150003 SMALfont
ASSIGN/SYSTEM 108852120100150003 STNDfont
ASSIGN/SYSTEM 108852100120150003 LRGEfont
ASSIGN/SYSTEM 100851065180194101 HUGEfont
EXIT



HP3TEST:
!
! Same as Pacific except uses 6pt proportional font for TINY --
!
!_____________________________________________________________________________
!              Suggested LPI:-->     12       8        6        6        4
!
!                                 TINYFONT SMALFONT STNDFONT LRGEFONT HUGEFONT
!
! _______________________orient   portrait portrait portrait portrait portrait
! | _____________________symbol   ROMAN=8U PC8=10U  ASCII=0U ASCII=0U ASCII=0U
! | |    ________________spacing  Proport  fixed    fixed    fixed    fixed
! | |    | ______________pitch    20.0cpi  16.67cpi 12cpi    10cpi    6.5cpi
! | |    | |   __________points   6.0 pts  8.5 pts  10 pts   12pts    18pts
! | |    | |   |   ______style    upright  upright  upright  upright  upright
! | |    | |   |   | ____weight   medium   medium   medium   medium   bold
! | |    | |   |   | | __typeface helvet   courier  gothic   gothic   present
! | |    | |   |   | | |
! 1 0885 1 200 060 1 5 0004       TINYfont
! 1 1085 2 167 085 1 5 0003                SMALfont
! 1 0085 2 120 100 1 5 0006                         STNDfont
! 1 0085 2 100 120 1 5 0006                                  LRGEfont
! 1 0085 2 065 180 1 9 0011                                           HUGEfont


ASSIGN/SYSTEM 108851200060150004 TINYfont
ASSIGN/SYSTEM 110852167085150003 SMALfont
ASSIGN/SYSTEM 100852120100150006 STNDfont
ASSIGN/SYSTEM 100852100120150006 LRGEfont
ASSIGN/SYSTEM 100852065180190011 HUGEfont
EXIT

