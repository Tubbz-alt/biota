!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
!
MODULE Data_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables for the overall data set.
!
!  History:
!
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!    Paul W. Eslinger : 11 May 2001 : Version 1.10.A
!    Paul W. Eslinger : 12 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger : 20 Jan 2006 : Add the plot file
!    Paul W. Eslinger : 13 Sep 2007 : Update comments
!
!  Code Block:
!
      REAL(KIND=8) :: SDSTOC ! Random seed the stochastic generation
      INTEGER :: NREAL       ! Number of realizations to generate
!
      CHARACTER(LEN=200) :: PTITLE ! Optional problem title line
!       Length should be less than or equal to LENQQQ
!
      LOGICAL :: REPORT  ! Flag whether the report file is open
!
      CHARACTER(LEN=7) :: SNG_TYP ! Type of value to output for the single key
      REAL :: SNG_PCT             ! Percent for the single keyword output
      LOGICAL :: BG_SNG_KEY       ! Flag whether the the single key file is written
      LOGICAL :: COLUMN           ! Flag whether generated stochastic data are
!                                   written in columns (see BG_STOC_VALU)
!
      LOGICAL :: BG_STOC_PLOT     ! Logical flag controlling writing the empirical
                                  ! CDF to a plot file
!
      INTEGER, PARAMETER :: NALPHA = 107
      REAL, DIMENSION(NALPHA) :: ALPHA  ! Alpha (probability levels) for the CDF plot
      REAL, DIMENSION(NALPHA) :: PLTCDF ! Values for the CDF plot
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Data_Mod

MODULE Files_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains file names and unit numbers
!
!  History:
!
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!    Paul W. Eslinger : 12 Mar 2003 : SAC Rev. 1
!    Paul W. Eslinger : 20 Jan 2006 : Add the plot file
!
!  Code Block:
!
     CHARACTER(LEN=256) :: FNTMP ! Temporary file name
     CHARACTER(LEN=256) :: FNRPT ! Name of the report file
     CHARACTER(LEN=256) :: FNKEY ! Name of the input keyword file
     CHARACTER(LEN=256) :: FNVLU ! Name of the input keyword file
     CHARACTER(LEN=256) :: FNSNG ! Name of the output single keyword file
     CHARACTER(LEN=256) :: FNPLT ! Name of the output empirical cdf plot file
     INTEGER :: IRPT ! Unit number for the report file
     INTEGER :: IKEY ! Unit number for the input keyword file
     INTEGER :: IVLU ! Unit number for the generated stochastic values
     INTEGER :: ISNG ! Unit number for the generated stochastic values
     INTEGER :: IPLT ! Unit number for the generated empirical CDF
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Files_Mod

MODULE Iden_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains run and user identification information
!
!  History:
!
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!
!  Code Block:
!
      CHARACTER(LEN=14) :: CRUNID ! Run identification number
!       Digit 1 is the leftmost digit
!         Digits  1- 4 = Year
!         Digits  5- 6 = Month
!         Digits  7- 8 = Day (1 to 31)
!         Digits  9-10 = Hour (0 to 23)
!         Digits 11-12 = Minute (0 to 59)
!         Digits 13-14 = Second (0 to 59)
      CHARACTER(LEN=12) :: PRGDAT ! Program modification date
      CHARACTER(LEN=10) :: PRGNAM ! Program name
      CHARACTER(LEN= 8) :: PRGVER ! Program version number
      CHARACTER(LEN=10) :: SYSDAT ! System date in the format mm-dd-yyyy
      CHARACTER(LEN=12) :: SYSTIM ! System time in the format hh:mm:ss
      CHARACTER(LEN=16) :: USRNAM ! User name
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Iden_Mod

      PROGRAM STOCHASTIC
!!***********************************************************************************************************
!!
!!                                          STOCHASTIC
!!               Test Stochastic Generation Routines and Generate Sample Values
!!           Battelle, Pacific Northwest National Laboratories, Richland, Washington
!!
!!***********************************************************************************************************
!!
!! Reference:
!!
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!  Language:
!!    This program is written in Fortran 95 (free source form)
!!
!!***********************************************************************************************************
!!
!!  Module History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C
!!      Add Log Ratio and Hyperbolic Arcsine Distributions
!!      Upgrade RDBLK and Stochastic routines
!!      Change to two-pass reading of keywords
!!    Paul W. Eslinger : 11 May 2001 : Version 1.10.A
!!      Add the "single key" output capability
!!    Paul W. Eslinger : 30 Jan 2002 : Version 1.10.B.4
!!      Add column output option
!!    Paul W. Eslinger : 12 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2004 : Version 2.00.C.1
!!      Change to using NARGS and GETARG
!!      Add the command line help option
!!    Paul W. Eslinger : 25 Mar 2005 : Allow truncation on type 10
!!    Paul W. Eslinger : 28 Mar 2005 : Change loop logic in SERR for type 10
!!    Paul W. Eslinger :  6 Jan 2006 : Import updated RDBLK routines
!!                                     Change large sample median pointer
!!    Paul W. Eslinger : 20 Jan 2006 : Add the plot file
!!    Paul W. Eslinger : 11 Aug 2006 : Add TELLTIME logic
!!    Paul W. Eslinger : 31 May 2007 : Update for TIIA suite of codes
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!***********************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Iden_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Data_Mod
      USE Stats_Mod
!
! *** Explicitly define all variables
      IMPLICIT NONE
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'STOCHASTIC' ! Name of this routine
      CHARACTER(LEN=24) :: TMPNAM  ! Same length string as VLABEL
      CHARACTER(LEN=64) :: TMPMESS ! Temporary variable of length VMESS
!
      REAL, ALLOCATABLE :: SDATA(:) ! Vector (length NREAL) of generated stochastic data
      REAL, ALLOCATABLE :: SWORK(:) ! Work vector (length NREAL)
      INTEGER :: IERR  ! Error number flag
      INTEGER :: NTEST ! Local looping control variable
      INTEGER :: VIDX  ! Index into stored data for a stochastic variable
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
!-----------------------------------------------------------------------
!  Program, run identification, and initialization
!-----------------------------------------------------------------------
!
      CALL IDENC( )
      CALL INIT( )
!
! *** Open the input keyword file
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the keyword file'
        WRITE(*,*) 'Stop in ' // CALLER
        STOP
      END IF
!
!-----------------------------------------------------------------------
!     Process the keywords except for stochastic variable definitions
!-----------------------------------------------------------------------
!
! *** Obtain keywords that are stored in variables of fixed dimension
!     Scan the keyword file for the number of stochastic variables
!     Open the report file
      CALL TELLTIME( 'Reading keywords - Pass #1', 'SCREEN', .FALSE. )
      CALL READ_KEYS_1( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Problem size checks
      IF( NREAL .LE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid number of realizations - zero or negative entry.'
        MESSAG(2) = 'Modify the REALIZATION keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        GO TO 9999
      END IF
!
! *** Write a final banner page to the report file
      CALL BANNER( 2, IRPT )
!
! *** Set up memory for stochastic variables
!     INDSTO mapped to MAXSTO, INDTBL mapped to MAXTBL, NREAL  mapped to MAXGEN
      CALL STOCH_MEMORY( INDSTO, INDTBL, NREAL, IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Initialize the stochastic labels and arrays
!     Must be preceded by a call to STOCH_MEMORY
      CALL INIT_STOCH( )
!
! *** Process the stochastic keywords
      CALL TELLTIME( 'Reading keywords - Pass #2', 'SCREEN', .FALSE. )
      CALL READ_KEYS_2( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Open the optional output file for stochastic values
      IF( BG_STOC_VALU ) THEN
        CALL OPEN_VALUES( IERR )
        IF( IERR .NE. 0 ) GO TO 9999
      END IF
!
! *** Open the optional output file for single stochastic keywords
      IF( BG_SNG_KEY ) THEN
        CALL OPEN_SNG( IERR )
        IF( IERR .NE. 0 ) GO TO 9999
      END IF
!
! *** Open the optional output file for CDF plot
      IF( BG_STOC_PLOT ) THEN
        CALL OPEN_PLT( IERR )
        IF( IERR .NE. 0 ) GO TO 9999
      END IF
!
! *** Output some information to the report file
!
      WRITE(IRPT,2000) TRIM(PTITLE)
 2000 FORMAT(/'Title: "',A,'"')
      WRITE(IRPT,2010) INDSTO, NREAL
 2010 FORMAT('Tests for ',I0,' stochastic variables'/ &
             'Each test uses ',I0,' realizations')
      WRITE(IRPT,2020) 'Keyword file: ',TRIM(FNKEY)
 2020 FORMAT(/A,A)
      IF( BG_STOC_VALU ) WRITE(IRPT,2020) 'Values file: ', TRIM(FNVLU)
      IF( BG_SNG_KEY   ) WRITE(IRPT,2020) 'STOCHASTIC KEYWORD file: ', TRIM(FNSNG)
!
!-----------------------------------------------------------------------
!  Generate values for all tests
!-----------------------------------------------------------------------
!
! *** Set up the vector for the generated values
      ALLOCATE( SDATA(NREAL) )
      ALLOCATE( SWORK(NREAL) )
!
! *** Loop over all of the tests and generate the values
!     Simply report errors and skip to the next test
!
      DO NTEST = 1, INDSTO
!
        TMPNAM  = VLABEL( NTEST )
        CALL TELLTIME( 'Processing Variable: '//TRIM(TMPNAM), 'SCREEN', .FALSE. )
!
        VIDX = 0
        CALL STONE( TMPNAM, VIDX, NREAL, SDSTOC, SDATA, IRPT, IERR )
        IF( IERR .NE. 0 ) THEN
          WRITE(IRPT,1000) 'STONE', TRIM(TMPNAM)
 1000     FORMAT('Error detected in subroutine ',A,' for variable: ',A)
          CYCLE
        END IF
!
        IF( BG_STOC_VALU ) THEN
          IF( COLUMN ) THEN
            CALL STBUG( TMPNAM, NREAL, SDATA, IVLU, 'COLUMN', IERR )
            IF( IERR .NE. 0 ) THEN
              WRITE(IRPT,1000) 'STBUG-COLUMN', TRIM(TMPNAM)
              CYCLE
            END IF
          ELSE
            CALL STBUG( TMPNAM, NREAL, SDATA, IVLU, 'ROW', IERR )
            IF( IERR .NE. 0 ) THEN
              WRITE(IRPT,1000) 'STBUG-ROW', TRIM(TMPNAM)
              CYCLE
            END IF
          END IF
        END IF
!
        IF( BG_STOC_PLOT ) THEN
          CALL PLOT_CDF( TMPNAM, SDATA, IERR )
          IF( IERR .NE. 0 ) THEN
            WRITE(IRPT,1000) 'PLOT_CDF', TRIM(TMPNAM)
            CYCLE
          END IF
        END IF
!
        IF( BG_SNG_KEY ) THEN
          TMPMESS = VMESS( VIDX )
          CALL SNG_PICK( TMPNAM, TMPMESS, SNG_TYP, SNG_PCT, NREAL, SDATA, SWORK, ISNG, IERR )
          IF( IERR .NE. 0 ) THEN
            WRITE(IRPT,1002) 'SNG_PICK', TRIM(TMPNAM)
 1002       FORMAT('Error detected in subroutine ',A,' for variable: ',A)
            CYCLE
          END IF
        END IF
!
      END DO
!
!-----------------------------------------------------------------------
! Free the allocated memory after all of the tests have been completed
!-----------------------------------------------------------------------
!
      CALL FREE_STOCH_DIM( 'TABLE', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine FREE_STOCH_DIM'
        MESSAG(2) = 'Problem in deallocating memory for stochastic variables'
        MESSAG(3) = 'Processing group "TABLE"'
        MESSAG(4) = 'Terminal error encountered'
        CALL PRTERR( IERR, CALLER, 3 )
        GO TO 9999
      END IF
!
      CALL FREE_STOCH_DIM( 'VARIABLE', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine FREE_STOCH_DIM'
        MESSAG(2) = 'Problem in deallocating memory for stochastic variables'
        MESSAG(3) = 'Processing group "VARIABLE"'
        MESSAG(4) = 'Terminal error encountered'
        CALL PRTERR( IERR, CALLER, 3 )
        GO TO 9999
      END IF
!
      CALL FREE_STOCH_DIM( 'WORK', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine FREE_STOCH_DIM'
        MESSAG(2) = 'Problem in deallocating memory for stochastic variables'
        MESSAG(3) = 'Processing group "WORK"'
        MESSAG(4) = 'Terminal error encountered'
        CALL PRTERR( IERR, CALLER, 3 )
        GO TO 9999
      END IF
!
      DEALLOCATE( SDATA )
      DEALLOCATE( SWORK )
!
!----------------------------------------------------------------------C
!     Normal termination location
!----------------------------------------------------------------------C
!
      MESSAG(1) = 'Normal Termination'
      CALL PRTERR( IERR, CALLER, 1 )
      CALL TELLTIME( 'Normal Termination', 'SCREEN', .FALSE. )
      STOP
!
!----------------------------------------------------------------------C
!     Fatal errors trapped return to this point for termination
!----------------------------------------------------------------------C
!
 9999 CONTINUE
!
      IERR = 9999
!
      IF( REPORT ) THEN
        MESSAG(1) = 'Error encountered in a lower-level routine.'
        MESSAG(2) = 'Execution halted because of the above errors.'
        CALL PRTERR( IERR, CALLER, 2 )
      ELSE
        WRITE(*,*) 'Error encountered in a lower-level routine.'
        WRITE(*,*) 'Execution halted because of the above errors.'
        WRITE(*,*) 'Program stop in ' // TRIM(CALLER)
      END IF
!
      CALL TELLTIME( 'Abnormal Run Termination Due to Errors', 'SCREEN', .FALSE. )
      STOP
!
      END PROGRAM

      SUBROUTINE PLOT_CDF( TMPNAM, SDATA, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!    This subroutine writes an empirical cdf to the plot file
!!
!!  History:
!!    Paul W. Eslinger : 20 Jan 2006 : Original code
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Iden_Mod
      USE Data_Mod
      USE Stats_Mod
      USE Errors_Mod
      USE Files_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: TMPNAM
      REAL, DIMENSION(*), INTENT(IN) :: SDATA ! Input data vector of length NREAL
      INTEGER :: IERR ! Error indicator (0=no errors)
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'PLOT_CDF'
      INTEGER :: IREL ! Realization loop
      INTEGER :: KFLAG ! Sort flag
      REAL :: FN, FJ, FJP ! Temporary real values
      INTEGER :: I, J  ! Looping indices
!
!---- First executable code ---------------------------------------------
!
! *** Copy the data to a work location
      DO IREL = 1, NREAL
        RWORK(IREL) = SDATA(IREL)
      END DO
!
! *** Sort the data into increasing order
      KFLAG = 1
      CALL SORT( RWORK, NREAL, KFLAG, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Save off the plot values
      PLTCDF(1) = RWORK(1)
      PLTCDF(NALPHA) = RWORK(NREAL)
!
! *** Loop over the ALPHA levels and compute the CDF by interpolation
!     between the sorted sample data values
!     All alpha levels smaller than 1/NSIZE are mapped to the first data value.
!
      FN = REAL(NREAL)
      DO I = 2, NALPHA-1
!
        IF( ALPHA(I) .LE. (1.0/FN) ) THEN
          PLTCDF(I) = RWORK(1)
          CYCLE
        ENDIF
!
        DO J = 1, NREAL-1
          FJ  = REAL(J)   / FN
          FJP = REAL(J+1) / FN
          IF( ALPHA(I).GE.FJ .AND. ALPHA(I).LT.FJP ) THEN
            PLTCDF(I) = FN*(ALPHA(I)-FJ) * (RWORK(J+1)-RWORK(J)) + RWORK(J)
!            PLTCDF(I) = RWORK(J)
            CYCLE
          ENDIF
        END DO
!
!        PLTCDF(I) = RWORK(NREAL)
!
      END DO
!
! *** Output the values
!
      WRITE(IPLT,1000) ' '
      WRITE(IPLT,1000) TRIM(TMPNAM)
 1000 FORMAT(A)
!
      DO I = 1, NALPHA
        WRITE(IPLT,1010) PLTCDF(I), ALPHA(I)
 1010   FORMAT(1P,E14.7,',',E14.7)
      END DO
!
      RETURN
      END SUBROUTINE

      SUBROUTINE COMMAND_LINE_HELP( )
!!**********************************************************************
!!
!!  Purpose:
!!    This subroutine writes information on the command line invocation
!!    needed to run the code.  The information is written to the
!!    standard output device.
!!
!!  History:
!!    Paul W. Eslinger :  4 Jun 2004 : Original code
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
!
      USE Iden_Mod
!
      IMPLICIT NONE
!
!---- First executable code ---------------------------------------------
!
      WRITE(*,*) ' '
      WRITE(*,*) 'A single (optional) command line argument is allowed.  Enter'
      WRITE(*,*) '  '//TRIM(PRGNAM)//'            : To be prompted for the control keyword file name'
      WRITE(*,*) '  '//TRIM(PRGNAM)//'  file_name : To define the control keyword file name'
      WRITE(*,*) '  '//TRIM(PRGNAM)//' -HELP      : To get this help message (but not execute anything)'
!
      RETURN
      END SUBROUTINE

      SUBROUTINE IDENC( )
!!**********************************************************************
!!
!!  Purpose:
!!    This routine generates identification variables for this run
!!    of the code.  Variables are:
!!      1. A unique run identifier number, utilizing the date
!!         and time which this routine was called
!!      2. Hard coded - Program name and version number
!!      3. User name  - Hard coded to blank
!!      4. Hard coded - Date that program was last modified
!!      5. System date and time
!!
!!  Operating system:
!!    This subroutine is operating system independent.
!!
!!  History:
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger : 12 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 25 Mar 2005 : Last update
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
!
      IMPLICIT NONE
!
! *** Local variables
!
      CHARACTER(LEN=10) :: SDATE ! System date in the form YYYYMMDD
      CHARACTER(LEN=10) :: STIME ! System time in the form HHMMSS.SSS
!
!---- Executable code --------------------------------------------------
!
! *** User name
      USRNAM = 'Anonymous User'
!
! *** Program name and version number
      PRGNAM = 'STOCHASTIC'
      PRGVER = '4.00.001'
!
! *** Program date (DD MMM YYYYY)
      PRGDAT = ' 9 Jul 2012'
!
! *** Get the date and time from the operating system
      CALL DATE_AND_TIME( SDATE, STIME )
!
! *** System time in the (CHARACTER) form hh:mm:ss
      SYSTIM = STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
!
! *** System date in the (CHARACTER) form mm-dd-yyyy
      SYSDAT = SDATE(5:6)//'-'//SDATE(7:8)//'-'//SDATE(1:4)
!
! *** Run identification number
      CRUNID = SDATE(1:8)//STIME(1:6)
!
      RETURN
      END SUBROUTINE

      SUBROUTINE INIT( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine initializes some global variables.  The global
!!    variables for the stochastic routines are set in another routine.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C
!!    Paul W. Eslinger : 11 May 2001 : Version 1.10.A
!!    Paul W. Eslinger : 20 Jan 2006 : Add the plot file
!!    Paul W. Eslinger : 31 May 2007 : Add NREAL
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
!
      USE Files_Mod
      USE Errors_Mod
      USE Data_Mod
!
      IMPLICIT NONE
!
!---- Executable code ---------------------------------------------------
!
! *** Keyword file
      IKEY  = 7
      FNKEY = ' '
!
! *** Statistics values file
      IVLU  = 8
      FNVLU = ' '
!
! *** Report file
      IRPT  = 10
      FNRPT = ' '
      REPORT = .FALSE.
!
! *** Error message file
      IRPT_ERR = IRPT
!
! *** Single key keyword file
      ISNG = 11
      FNSNG = ' '
      BG_SNG_KEY = .FALSE.
      SNG_TYP = 'NONE'
      SNG_PCT = -1.0
!
! *** Plot file
      IPLT = 12
      FNPLT = ' '
      BG_STOC_PLOT = .FALSE.
!
! *** Seed for random number generator
      SDSTOC = 0.0D0
!
! *** Column data output
      COLUMN = .FALSE.
!
! *** Number of realizations
      NREAL = -1
!
! *** Probability levels for the CDF plot
      ALPHA(1) = 0
      ALPHA(2) = 0.0005
      ALPHA(3) = 0.001
      ALPHA(4) = 0.005
      ALPHA(5) = 0.01
      ALPHA(6) = 0.02
      ALPHA(7) = 0.03
      ALPHA(8) = 0.04
      ALPHA(9) = 0.05
      ALPHA(10) = 0.06
      ALPHA(11) = 0.07
      ALPHA(12) = 0.08
      ALPHA(13) = 0.09
      ALPHA(14) = 0.1
      ALPHA(15) = 0.11
      ALPHA(16) = 0.12
      ALPHA(17) = 0.13
      ALPHA(18) = 0.14
      ALPHA(19) = 0.15
      ALPHA(20) = 0.16
      ALPHA(21) = 0.17
      ALPHA(22) = 0.18
      ALPHA(23) = 0.19
      ALPHA(24) = 0.2
      ALPHA(25) = 0.21
      ALPHA(26) = 0.22
      ALPHA(27) = 0.23
      ALPHA(28) = 0.24
      ALPHA(29) = 0.25
      ALPHA(30) = 0.26
      ALPHA(31) = 0.27
      ALPHA(32) = 0.28
      ALPHA(33) = 0.29
      ALPHA(34) = 0.3
      ALPHA(35) = 0.31
      ALPHA(36) = 0.32
      ALPHA(37) = 0.33
      ALPHA(38) = 0.34
      ALPHA(39) = 0.35
      ALPHA(40) = 0.36
      ALPHA(41) = 0.37
      ALPHA(42) = 0.38
      ALPHA(43) = 0.39
      ALPHA(44) = 0.4
      ALPHA(45) = 0.41
      ALPHA(46) = 0.42
      ALPHA(47) = 0.43
      ALPHA(48) = 0.44
      ALPHA(49) = 0.45
      ALPHA(50) = 0.46
      ALPHA(51) = 0.47
      ALPHA(52) = 0.48
      ALPHA(53) = 0.49
      ALPHA(54) = 0.5
      ALPHA(55) = 0.51
      ALPHA(56) = 0.52
      ALPHA(57) = 0.53
      ALPHA(58) = 0.54
      ALPHA(59) = 0.55
      ALPHA(60) = 0.56
      ALPHA(61) = 0.57
      ALPHA(62) = 0.58
      ALPHA(63) = 0.59
      ALPHA(64) = 0.6
      ALPHA(65) = 0.61
      ALPHA(66) = 0.62
      ALPHA(67) = 0.63
      ALPHA(68) = 0.64
      ALPHA(69) = 0.65
      ALPHA(70) = 0.66
      ALPHA(71) = 0.67
      ALPHA(72) = 0.68
      ALPHA(73) = 0.69
      ALPHA(74) = 0.7
      ALPHA(75) = 0.71
      ALPHA(76) = 0.72
      ALPHA(77) = 0.73
      ALPHA(78) = 0.74
      ALPHA(79) = 0.75
      ALPHA(80) = 0.76
      ALPHA(81) = 0.77
      ALPHA(82) = 0.78
      ALPHA(83) = 0.79
      ALPHA(84) = 0.8
      ALPHA(85) = 0.81
      ALPHA(86) = 0.82
      ALPHA(87) = 0.83
      ALPHA(88) = 0.84
      ALPHA(89) = 0.85
      ALPHA(90) = 0.86
      ALPHA(91) = 0.87
      ALPHA(92) = 0.88
      ALPHA(93) = 0.89
      ALPHA(94) = 0.9
      ALPHA(95) = 0.91
      ALPHA(96) = 0.92
      ALPHA(97) = 0.93
      ALPHA(98) = 0.94
      ALPHA(99) = 0.95
      ALPHA(100) = 0.96
      ALPHA(101) = 0.97
      ALPHA(102) = 0.98
      ALPHA(103) = 0.99
      ALPHA(104) = 0.995
      ALPHA(105) = 0.999
      ALPHA(106) = 0.9995
      ALPHA(107) = 1
!
      RETURN
      END SUBROUTINE

      SUBROUTINE BANNER( ISEC, IRPT )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a banner page to the report file.
!!
!!  Calling Requirements:
!!    Subroutine IDENC must have been called prior to this routine
!!    to set the code identification variables.
!!
!!    Subroutine READ_KEYS_1 must have been called prior to this
!!    routine to set the user name.
!!
!!  History:
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C
!!    Paul W. Eslinger :  4 Jun 2004 : Change formats
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE QA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER, INTENT(IN) :: ISEC ! Section number to print
      INTEGER, INTENT(IN) :: IRPT ! Unit number of outputs
!
!---- Executable code ---------------------------------------------------
!
      SELECT CASE ( ISEC )
!
        CASE( 1 )
!
          WRITE(IRPT,'(/)')
!
          WRITE(IRPT,7010) ' SSSSS  TTTTTTT  OOOOO   CCCCC  H     H   AAA    SSSSS  TTTTTTT IIIIIII  CCCCC '
          WRITE(IRPT,7010) 'S     S    T    O     O C     C H     H  A   A  S     S    T       I    C     C'
          WRITE(IRPT,7010) ' S         T    O     O C       H     H A     A  S         T       I    C      '
          WRITE(IRPT,7010) '  SSS      T    O     O C       HHHHHHH AAAAAAA   SSS      T       I    C      '
          WRITE(IRPT,7010) '     S     T    O     O C       H     H A     A      S     T       I    C      '
          WRITE(IRPT,7010) 'S     S    T    O     O C     C H     H A     A S     S    T       I    C     C'
          WRITE(IRPT,7010) ' SSSSS     T     OOOOO   CCCCC  H     H A     A  SSSSS     T    IIIIIII  CCCCC '
 7010     FORMAT(A)
!
          WRITE(IRPT,7020) PRGNAM, PRGVER, PRGDAT
 7020     FORMAT(//25X,A,'  Version ',A/25X,'Last Modified on ',A)
!
        CASE( 2 )
!
! ***     Identification information
          WRITE(IRPT,7030) CRUNID, USRNAM
 7030     FORMAT(/10X,'Current Run ID = ',A14,'   User Name = ',A16)
!
          WRITE(IRPT,7040) SYSDAT, SYSTIM
 7040     FORMAT(17X,'System Date = ',A10,'   System Time = ',A8)
!
! ***     Code status disclaimer (centered on page)
          CALL QA_CopyrightFull( Irpt, .TRUE. )
          CALL QA_Disclaimer( Irpt, .TRUE. )
          CALL QA_Reference( Irpt )
!
! ***     Review Block
          WRITE(IRPT,7050)
 7050     FORMAT(//'                             Review Signatures')
!
          WRITE(IRPT,7060)
 7060     FORMAT(/'Input Prepared By: ______________________________       Date: _______________')
!
          WRITE(IRPT,7070)
 7070     FORMAT(/'Input Reviewed By: ______________________________       Date: _______________')
!
        CASE DEFAULT ! Should't get here, but ignore it if we do
!
      END SELECT
!
      RETURN
      END SUBROUTINE

      SUBROUTINE READ_KEYS_1( IERR )
!!***********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the initial set of keywords.  All keywords
!!    except STOCHASTIC keywords are processed in full.  The set of
!!    STOCHASTIC keywords are parsed enough to get the information
!!    to set array dimensions.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C
!!    Paul W. Eslinger : 11 May 2001 : Version 1.10.A
!!
!!***********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Data_Mod
      USE Errors_Mod
      USE Stats_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: IERR ! Error flag number, 0 is returned for no errors
!
! *** Local variables
!
      INTEGER :: IPERR ! Error unit number
      INTEGER :: ICARD ! Input keyword card count
      INTEGER :: ITMP  ! Temporary integer index
      CHARACTER :: TITLE*(LENCRD) ! Title line from RDBLK
      CHARACTER(LEN=11) :: CALLER = 'READ_KEYS_1' ! Name of this subroutine
!
      LOGICAL, EXTERNAL :: CEXIST ! Function used by RDBLK routines
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
!
      ILINE = 0
      IPERR = 6
!
! *** Initialize for error checking
!       ICARD : Counter for cards to allow checking for entering the
!               report file name as the first entry
!
      ICARD = 0
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!----------------------------------------------------------------------C
!
   10 CONTINUE
!
      CALL RDBLK( IKEY, IPERR, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        IF( REPORT ) THEN
          MESSAG(1) = 'Error in lower level RDBLK routine'
          CALL PRTERR( IERR, CALLER, 1 )
        ELSE
          WRITE(*,*) 'Error in lower level RDBLK routine'
          WRITE(*,*) 'Message issued in routine ' // CALLER
        END IF
        RETURN
      END IF
!
      ICARD = ICARD + 1
!
! *** Error check that the report file is open
!
      IF( ( ICARD.GT.1 .AND. (.NOT.REPORT) ) .OR. &
          ( ICARD.EQ.1 .AND. KNAME.NE.'REPORT') ) THEN
        IERR = 1
        WRITE(*,1000)
 1000   FORMAT(' The REPORT keyword was not the first keyword')
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'END' ) ! ===> END keyword
          REWIND( IKEY )
          RETURN
!
        CASE( 'REALIZAT' ) ! REALIZAT(IONS) Keyword
          NREAL = VALUE(1)
!
        CASE( 'REPORT' ) ! REPORT Keyword
!         The report card must be the first entry in the keyword file
          IF( ICARD .NE. 1 ) THEN
            WRITE(*,1010)
 1010       FORMAT(/' Illegal keyword order in subroutine KEYWD'/ &
              ' The REPORT keyword must be the first entry')
            IERR = 6
            RETURN
          END IF
!         Get the report file name and open the file
          FNRPT = QUOTE(1)
          IF( FNRPT .NE. ' ' ) THEN
            OPEN(IRPT,FILE=FNRPT,STATUS='UNKNOWN',IOSTAT=IERR)
            REPORT = .TRUE.
          ELSE
            IERR = 7
          END IF
!         Error opening the report file
          IF( IERR .NE. 0 ) THEN
            WRITE(*,1020) FNRPT
 1020       FORMAT(/' Unable to open the report file in subroutine KEYWD'/' File: ',A)
            IERR = 7
            RETURN
          END IF
!         Print the opening banner page
          CALL BANNER( 1, IRPT )
!
        CASE( 'SEED' ) ! SEED Keyword
          SDSTOC = VALUE(1)
!
        CASE( 'STOCHAST' ) ! ===> STOCHASTIC keyword
          INDSTO = INDSTO + 1
          ITMP = VALUE(1)
          IF( ITMP .EQ. 10 ) INDTBL = INDTBL + VALUE(2)
!
        CASE( 'TITLE' ) ! TITLE Keyword
          IF( NQUOTE .GT. 0 ) PTITLE = QUOTE(1)
!
        CASE( 'USER' ) ! USER Keyword
          IF( NQUOTE .GT. 0 ) USRNAM = QUOTE(1)
!
        CASE DEFAULT ! Ignore all other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE

      SUBROUTINE READ_KEYS_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and storing all stochastic
!!    variable inputs from the keyword control file.
!!
!!  Call List Variables:
!!
!!    IERR : Output integer error flag.
!!            0 = Normal execution
!!           >0 = Terminal error encountered
!!
!!  Auxiliary Routines:
!!
!!    PRTERR, SDECOD and all RDBLK related routines
!!
!!  History:
!!
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C
!!    Paul W. Eslinger : 20 Jan 2006 : Add the plot file
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
!
      USE Errors_Mod
      USE Files_Mod
      USE Iden_Mod
      USE Data_Mod
      USE Rdblk_Mod
      USE Stats_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR ! Error number
!
! *** Local variables
!
      CHARACTER(LEN=LENCRD) :: TITLE  ! Temporary storage for a single card
      CHARACTER(LEN=LENQQQ) :: TMP_ID ! Name returned from RDBLK in a quote string
      CHARACTER(LEN=24) :: CTMP ! Same length as VLABEL
      CHARACTER(LEN=64) :: CMES ! Same length as VMESS
      INTEGER :: IDX ! Temporary integer index
!
      CHARACTER(LEN=11) :: CALLER = 'READ_KEYS_2' ! Name of this routine
!
      LOGICAL :: TRUNC   ! Logical flag for truncation test
      INTEGER :: ONE = 1 ! The number 1
      REAL :: RTMP       ! Temporary real value
!
      INTEGER :: ICARD ! Keyword card counter
      INTEGER :: IPERR ! Error file number for RDBLK
!
! *** User defined functions
!
      LOGICAL, EXTERNAL :: CEXIST ! Routine for existence of keyword modifier
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize for RDBLK
!
      ILINE = 0
      IPERR = 6
!
! *** Initialize for error checking
!       ICARD : Counter for cards to allow checking for entering the
!               report file name as the first entry
!
      ICARD = 0
!
      IERR = 0
      CTMP = 'No quotes entered'
!
!-----------------------------------------------------------------------
!                   Top of loop on reading keyword cards
!-----------------------------------------------------------------------
!
   10 CONTINUE
!
      CALL RDBLK( IKEY, IPERR, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ICARD = ICARD + 1
!
      SELECT CASE ( KNAME )
!
        CASE ( 'DEBUG' ) ! DEBUG Keyword
!
          IF( CEXIST('STOCHAST') ) THEN
            BG_STOC_VALU = .TRUE.
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              FNVLU = TMP_ID
            ELSE
              IERR = 1
              MESSAG(1) = 'File name missing for stochastic values'
              MESSAG(2) = 'STOCHASTIC modifier on DEBUG keyword'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            IF( CEXIST('COLUMN') ) COLUMN = .TRUE.
          END IF
!
          IF( CEXIST('PLOT') ) THEN
            BG_STOC_PLOT = .TRUE.
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              FNPLT = TMP_ID
            ELSE
              IERR = 1
              MESSAG(1) = 'File name missing for stochastic values'
              MESSAG(2) = 'PLOT modifier on DEBUG keyword'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('DEFINITI') ) BG_STOC_DEFN = .TRUE.
!
          IF( CEXIST('STATISTI') ) BG_STOC_STAT = .TRUE.
!
        CASE ( 'END' ) ! END Keyword
          CLOSE( IKEY )
          RETURN
!
        CASE ( 'SINGLEKE' ) ! SINGLEKEY Keyword
          BG_SNG_KEY = .TRUE.
          IF( CEXIST('FILE') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              FNSNG = TMP_ID
            ELSE
              IERR = 2
              MESSAG(1) = 'File name missing for single key values'
              MESSAG(2) = 'FILE modifier on SINGLEKEY keyword'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
          IF( CEXIST('MEAN') .AND. CEXIST('PERCENT') ) THEN
            IERR = 3
            MESSAG(1) = 'Cannot use both the MEAN and PERCENT modifiers'
            MESSAG(2) = 'Change the SINGLEKEY keyword'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( .NOT.CEXIST('MEAN') .AND. .NOT.CEXIST('PERCENT') ) THEN
            IERR = 4
            MESSAG(1) = 'Exactly one of the MEAN or PERCENT modifiers is required'
            MESSAG(2) = 'Change the SINGLEKEY keyword'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( CEXIST('MEAN')    ) SNG_TYP = 'MEAN'
          IF( CEXIST('PERCENT') ) THEN
            SNG_TYP = 'PERCENT'
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              SNG_PCT = RTMP
            ELSE
              IERR = 5
              MESSAG(1) = 'Percent value not found on the SINGLEKEY keyword'
              MESSAG(2) = 'Check the keyword definition'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
        CASE( 'STOCHAST' ) ! STOCHAST(IC) Keyword
!         Force the user to enter the variable identifier
          IF( NQUOTE .LT. 1 ) THEN
            IERR = 6
            MESSAG(1) = 'At least one quote string required for the STOCHASTIC card'
            MESSAG(2) = 'The first quote must gives the variable identifier'
            MESSAG(3) = 'The second quote optionally gives a one-line message'
            CALL PRTERR( IERR, CALLER, 3 )
            GO TO 10
          END IF
!         Store the quote strings and decode the numerical parameters
          CTMP = QUOTE(1)
          IF( NQUOTE .GT. 1 ) THEN
            CMES = QUOTE(2)
          ELSE
            CMES = ' '
          END IF
          IF( CEXIST('TRUNCATE') ) THEN
            TRUNC = .TRUE.
          ELSE
            TRUNC = .FALSE.
          END IF
          CALL SDECOD( CTMP, CMES, ONE, TRUNC, VALUE, NVALUE, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 9999
            MESSAG(1) = 'Error detected in subroutine SDECOD'
            MESSAG(2) = 'Error decoding variable ' // CTMP
            MESSAG(3) = 'Change the definition of the STOCHASTIC card'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
        CASE DEFAULT ! Ignore other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE

      SUBROUTINE OPEN_KEY( IERR )
!!********************************************************************************
!!
!!  Purpose:
!!    This subroutine performs two actions for the keyword file:
!!      1. Obtain the file name and store it in the variable FNKEY
!!         a) First check the command line to see if the file
!!            name was provided or,
!!         b) query the user for the file name if it was not
!!            entered on the command line.
!!      2. Open the file on unit number IKEY for use by subroutine KEYWD
!!
!!  Note:
!!    This subroutine does not call PRTERR when an error occurs because
!!    PRTERR writes to the report file.  The report file is not opened
!!    until the keywords are read in subroutine KEYWD.
!!
!!  History:
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Jun 2004 : Change to using NARGS and GETARG
!!                                     Add the command line help option
!!    Paul W. Eslinger :  9 Jul 2012 : Revise to a common callable routine
!!
!!  Call List Variables:
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in locating or opening the input keyword file
!!
!!********************************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Iden_Mod
!
      IMPLICIT NONE
!
! *** External functions
      INTEGER, EXTERNAL :: NARGS
!
! *** Call list variables
      INTEGER :: IERR ! Error number variable
!
! *** Local variables
!
      LOGICAL :: THERE    ! File existence flag
      INTEGER :: NUM_ARGS ! Number of command line arguments
      INTEGER :: NUM_FNAM ! Argument number for the keyword file name
      CHARACTER(LEN=5) :: HELP ! Character for checking for help
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
      CALL QA_CopyRightSingle( 6, .FALSE. )
      WRITE(*,1000) PRGNAM, PRGVER, PRGDAT
 1000 FORMAT( 1X,'-------------------------------------------------'/&
              1X,1X,A,1X,A,'  Last Modified: ',A/                   &
              1X,'-------------------------------------------------')
!
! *** Check for number of command line arguments
!     NARGS is a Lahey Fortran service routine
!     The command is argument number 1 for this call
      NUM_ARGS = NARGS( )
!
! *** Use the file name if it was entered on the command line
      IF( NUM_ARGS .GT. 1 ) THEN
!       GETARG is a Lahey Fortran service routine that returns the specified
!       command-line argument (the command is argument number 0 for this call)
        NUM_FNAM = 1
        CALL GETARG( NUM_FNAM, FNKEY )
!
! ***   Special processing for command line help (print info and stop)
!       The argument -HELP (not case sensitive) is allowed
        HELP = FNKEY(1:5)
        CALL UPCASE( HELP )
        IF( HELP .EQ. '-HELP' ) THEN
          CALL COMMAND_LINE_HELP( )
          STOP
        END IF
!
      ELSE
        WRITE(*,1010)
 1010   FORMAT(/' Enter the keyword file name > ')
        READ(*,*) FNKEY
      END IF
!
! *** Check if the requested file exists
!
      INQUIRE(FILE=FNKEY,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        WRITE(*,1020) TRIM(FNKEY)
 1020   FORMAT(' The requested keyword file was not found'/' File: ',A)
        IERR = 1
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IKEY,FILE=FNKEY,STATUS='OLD',ERR=999)
!
      RETURN
!
! *** Error opening the file
!
  999 CONTINUE
!
      WRITE(*,1030) FNKEY
 1030 FORMAT(' System error encountered opening the input keyword file'/' File: ',A)
      IERR = 2
!
      RETURN
      END SUBROUTINE

      SUBROUTINE OPEN_PLT( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain the
!!    empirical CDF plot.
!!
!!  History:
!!
!!    Paul W. Eslinger : 20 Jan 2006 : Original source
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in opening the file
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Data_Mod
      USE Errors_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: IERR
!
! *** Local variables
!
      CHARACTER(LEN=8) :: CALLER = 'OPEN_SNG' ! Name of this subroutine
      INTEGER :: IERA ! Error return code from OPEN action
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check that the file name is entered
!
      IF( FNPLT .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'Name not entered for the output plot file'
        MESSAG(2) = 'Enter it on the DEBUG keyword using the PLOT modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Open the output header file
!
      OPEN(IPLT,FILE=FNPLT,STATUS='UNKNOWN',IOSTAT=IERA)
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the output plot file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE OPEN_SNG( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain the
!!    single stochastic keywords based on generated data.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 May 2001 : Version 1.10.A
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in opening the file
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Data_Mod
      USE Errors_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR
!
! *** Local variables
!
      CHARACTER(LEN=8) :: CALLER = 'OPEN_SNG' ! Name of this subroutine
      INTEGER :: IERA ! Error return code from OPEN action
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check that the file name is entered
!
      IF( FNSNG .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'Name not entered for the output single STOCHASTIC keywords'
        MESSAG(2) = 'Enter it on the SINGLEKEY keyword using the FILE modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Open the output header file
!
      OPEN(ISNG,FILE=FNSNG,STATUS='UNKNOWN',IOSTAT=IERA)
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the output stochastic keyword file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE OPEN_VALUES( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain the
!!    stochastically generated data.
!!
!!  History:
!!
!!    Paul W. Eslinger :  8 Nov 2000 : Version 1.00.C
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in opening the file
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Data_Mod
      USE Errors_Mod
!
! *** Call list variables
      IMPLICIT NONE
!
      INTEGER :: IERR
!
! *** Local variables
!
      CHARACTER(LEN=11) :: CALLER = 'OPEN_VALUES' ! Name of this subroutine
      INTEGER :: IERA ! Error return code from OPEN action
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check that the file name is entered
!
      IF( FNVLU .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'Name not entered for the stochastic values file'
        MESSAG(2) = 'Enter it on the DEBUG keyword using the STOCHASTIC modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Open the output header file
!
      OPEN(IVLU,FILE=FNVLU,STATUS='UNKNOWN',IOSTAT=IERA)
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the stochastic value file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE SNG_PICK( TMPNAM, TMPMESS, SNG_TYP, SNG_PCT, NREAL, SDATA, SWORK, ISNG, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine uses the generated stochastic data in the vector
!!    SDATA to choose one representative value.  The representative
!!    value can be the mean or a user specified percentile.
!!
!!    The specified value is written to the file connected to the
!!    unit ISNG.  The output format makes the data into a STOCHASTIC
!!    card that can be read by the stochastic keyword decoding routines.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 May 2001 : Version 1.10.A
!!    Paul W. Eslinger : 21 Apr 2004 : Modify output format
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
!
      USE Errors_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: TMPNAM  ! Variable name (or ID)
      CHARACTER(LEN=*), INTENT(IN) :: TMPMESS ! Variable message string
      CHARACTER(LEN=*), INTENT(IN) :: SNG_TYP ! Variable message string
      REAL, INTENT(IN) :: SNG_PCT             ! Percentile for "PERCENT" analyses
      INTEGER, INTENT(IN) :: NREAL            ! Number of realizations
      REAL, DIMENSION(*), INTENT(IN) :: SDATA ! Input data vector of length NREAL
      REAL, DIMENSION(*) :: SWORK             ! Work vector of length NREAL
      INTEGER, INTENT(IN) :: ISNG             ! Output file unit number
      INTEGER :: IERR                         ! Error flag
!
! *** Local variables
      REAL(KIND=8) :: DATASUM ! Data sum
      INTEGER :: IREL         ! Realization loop index
      REAL :: SNG_VAL         ! Computed single value
      INTEGER :: KFLAG        ! Flag for the sort routine
      INTEGER :: IPCT         ! Index for the percentile
      CHARACTER(LEN=7) :: CALLER = 'SNG_PICK' ! Name of this subroutine
!
!---- First executable code ---------------------------------------------
!
      IERR = 0
!
      IF( NREAL .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'NREAL is less than 1'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Choose the analysis type
!
      SELECT CASE ( SNG_TYP )
!
        CASE( 'MEAN' )  ! Mean analysis ---------------------------------
!
!         Compute the mean
!
          DATASUM = 0.0D0
          DO IREL = 1, NREAL
            DATASUM = DATASUM + SDATA(IREL)
          END DO
!
!         Output the value
!
          SNG_VAL = DATASUM / DBLE(NREAL)
          IF( LEN_TRIM(TMPMESS) .GT. 0 ) THEN
            WRITE(ISNG,1000) TRIM(TMPNAM), SNG_VAL, TRIM(TMPMESS)
 1000       FORMAT('STOCHASTIC "',A,'" 1 ',1P,E11.4,:,' "',A,'"')
          ELSE
            WRITE(ISNG,1000) TRIM(TMPNAM), SNG_VAL
          END IF
!
        CASE( 'PERCENT' )  ! Percent analysis ---------------------------
!
!         Check the percentile and set up the percentile index
!
          IF( SNG_PCT.LT.0.0 .OR. SNG_PCT.GT.1.0 ) THEN
            IERR = 3
            MESSAG(1) = 'Percent must lie in the range 0 to 1'
            MESSAG(2) = 'Change the SINGLEKEY keyword'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IPCT = SNG_PCT * NREAL
          IPCT = MAX( IPCT, 1 )
          IPCT = MIN( IPCT, NREAL )
!          WRITE(*,*) 'IPCT=',IPCT !pwe
!
!         Sort the data to allow percentile selection
!
          DO IREL = 1, NREAL
            SWORK(IREL) = SDATA(IREL)
          END DO
          KFLAG = 1
          CALL SORT( SWORK, NREAL, KFLAG, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!         Output the chosen percentile
!
          SNG_VAL = SWORK(IPCT)
          IF( LEN_TRIM(TMPMESS) .GT. 0 ) THEN
            WRITE(ISNG,1000) TRIM(TMPNAM), SNG_VAL, TRIM(TMPMESS)
          ELSE
            WRITE(ISNG,1000) TRIM(TMPNAM), SNG_VAL
          END IF
!
        CASE DEFAULT ! Illegal analysis type ----------------------------
          IERR = 2
          MESSAG(1) = 'Type of analysis chose is not MEAN or PERCENT'
          MESSAG(2) = 'Change the SINGLEKEY keyword'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
!
      END SELECT
!
      RETURN
      END SUBROUTINE SNG_PICK

      SUBROUTINE TELLTIME( MESSAGE, PLACE, LDATE )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine prints the date and time and a message to the
!!    report file.  It can also output the same message to the screen.
!!
!!  Inputs:
!!
!!    Variable  Description
!!    --------  ---------------------------------------
!!    MESSAGE   One line character message to be output
!!    PLACE     Character string
!!              "FILE"   = only output message to the report file
!!              "SCREEN" = only output message to the screen
!!              "BOTH"   = output message to screen and report file
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Jul 2003 : Version 1.0
!!    Paul W. Eslinger :  3 Aug 2004 : Add date output option
!!    Paul W. Eslinger : 31 Mar 2005 : Change SEQI to STRCOMP
!!
!!**********************************************************************
!
      USE FILES_MOD
!
! *** Call list variables
      CHARACTER(LEN=*) :: MESSAGE ! Character string to write
      CHARACTER(LEN=*) :: PLACE   ! Place to output the mesage
      LOGICAL :: LDATE            ! Only print date if LDATE=.TRUE.
!
! *** External functions
      LOGICAL, EXTERNAL :: STRCOMP
!
! *** Local variables
      CHARACTER(LEN=10) :: EDATE ! System date in the form YYYYMMDD
      CHARACTER(LEN=10) :: ETIME ! System time in the form HHMMSS.SSS
!
! *** Start of executable code
      CALL DATE_AND_TIME( EDATE, ETIME )
!
! *** Output to the requested places
      IF( STRCOMP(PLACE,'FILE',4) .OR. STRCOMP(PLACE,'BOTH',4) ) THEN
        IF( LDATE ) THEN
          WRITE(IRPT,1000) EDATE(5:6), EDATE(7:8), EDATE(1:4), ETIME(1:2), ETIME(3:4), ETIME(5:6), TRIM(MESSAGE)
 1000     FORMAT(' Date: ',A,"-",A,"-",A,'  Time: ',A,":",A,":",A,1X,A)
        ELSE
          WRITE(IRPT,1010) ETIME(1:2), ETIME(3:4), ETIME(5:6), TRIM(MESSAGE)
 1010     FORMAT(' Time: ',A,":",A,":",A,1X,A)
        END IF
      END IF
!
      IF( STRCOMP(PLACE,'SCREEN',6) .OR. STRCOMP(PLACE,'BOTH',4) ) THEN
        IF( LDATE ) THEN
          WRITE(*,1000) EDATE(5:6), EDATE(7:8), EDATE(1:4), ETIME(1:2), ETIME(3:4), ETIME(5:6), TRIM(MESSAGE)
        ELSE
          WRITE(*,1010) ETIME(1:2), ETIME(3:4), ETIME(5:6), TRIM(MESSAGE)
        END IF
      END IF
!
      RETURN
      END SUBROUTINE
