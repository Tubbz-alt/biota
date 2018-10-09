!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
!
MODULE Control_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This include deck contains control information for the entire problem as well as
!    providing allocation of work arrays for a variety of purposes.
!
!  History:
!
!    Paul W. Eslinger : 25 Jul 2004 : SAC Rev. 1
!    Carmen Arimescu  : 22 Mar 2006 : Revised solution variable lengths
!
      CHARACTER(LEN=200) :: PTITLE  ! Title for this program run
!
      CHARACTER(LEN=7) :: ANA_USE ! Analyte to process
      CHARACTER(LEN=7) :: SOL_USE ! Solution type to process
      CHARACTER(LEN=8) :: RES_USE ! Result type to process
!
      CHARACTER(LEN=6), ALLOCATABLE :: LOC_USE(:) ! Locations to process
      INTEGER :: NUM_LOC                          ! Number of locations to process
!
      INTEGER, ALLOCATABLE :: TIM_USE(:)          ! Times (years) to process
      INTEGER :: NUM_TIM                          ! Number of times to process
!
      CHARACTER(LEN=10) :: SDATE ! Start date of processing
      CHARACTER(LEN=10) :: STIME ! Start time of processing
!
      LOGICAL :: REPORT  ! Flag whether the report file is open
      LOGICAL :: VERBOSE ! Flag for verbose outputs to the screen
!
      INTEGER :: NREAL   ! Total number of realizations in the HUMAN data file
      INTEGER :: NUM_REL ! Realization number to extract (SINGLE option)
!
      LOGICAL :: USE_MAX ! Output maximum realization
      LOGICAL :: USE_AVG ! Output mean of realizations
      LOGICAL :: USE_SNG ! Output values for single realization
!
      REAL, ALLOCATABLE :: TMP_DATA(:) ! Vector of single realization data (length NREAL)
      REAL, ALLOCATABLE :: REL_DATA(:) ! Vector of extracted realization data (length NUM_TIM)
      CHARACTER(LEN=6), ALLOCATABLE :: LOC_DATA(:) ! Vector of extracted realization data (locations)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Files_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This include deck contains file specific variable information.
!
!  History:
!
!    Paul W. Eslinger : 25 Jul 2004 : Version 1.0
!
      INTEGER, PARAMETER :: MAXFN=128 ! Length of file names
!
      INTEGER :: IHUM               ! Unit number for the HUMAN data file
      CHARACTER(LEN=MAXFN) :: FNHUM ! Name of the HUMAN data file
!
      INTEGER :: IKEY               ! Unit number for the keyword file
      CHARACTER(LEN=MAXFN) :: FNKEY ! Name of the input keyword file
!
      INTEGER :: IRES               ! Unit number for the output results file
      CHARACTER(LEN=MAXFN) :: FNRES ! Name of the output results file
!
      INTEGER :: IRPT               ! Unit number for the report file
      CHARACTER(LEN=MAXFN) :: FNRPT ! Name of the output report file
!
      INTEGER :: ISPE               ! Unit number for the input species list file
      CHARACTER(LEN=MAXFN) :: FNSPE ! Name of the input species list file
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Iden_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains run and user identification information
!
!  History:
!
!    Paul W. Eslinger : 25 Jul 2004 : Version 1.0
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
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

      PROGRAM HighImpact
!!***********************************************************************************************************
!!
!!       HighImpact - Highest Impact Extraction from the Human Exposure Model
!!                Toolkit for Integrated Impacts Assessments (TIIA)
!!      Battelle, Pacific Northwest National Laboratories, Richland, Washington
!!
!!***********************************************************************************************************
!!
!!  Purpose:
!!    HighImpact is a utility code to extract the highest impact from a detailed
!!    results file written by HUMAN.  HUMAN is the top level routine for the human
!!    impacts portion of the suite of TIIA, Version 1, codes.
!!
!! Reference:
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!  History:
!!    Paul W. Eslinger : 25 Jul 2004 : Version 1.0
!!    Carmen Arimescu  : 19 Jan 2006 : Upgrade RDBLK routines
!!    Carmen Arimescu  : 22 Mar 2006 : Change solution variable lengths
!!    Paul W. Eslinger :  5 Jun 2007 : Udate to TIIA, add copyright information
!!
!!***********************************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Declare local variables
      INTEGER :: IERR ! Integer error flag
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Program, run identification, and initialization
      CALL IDEN_SET( )
      CALL INIT( )
!
! *** Open the input file
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) GOTO 999
!
! *** Define the report file
      CALL TELLTIME( 'Extracting the report file name', 'SCREEN', .FALSE., IRPT )
      CALL READ_KEYS_REPORT( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the report file'
        WRITE(*,*) 'Stop in '//PRGNAM
        STOP
      END IF
!
! *** Output first portion of the banner page
      CALL BANNER_1( )
!
! *** Read the first pass of the keywords to get user definition and array dimensions
      CALL TELLTIME( 'Reading keywords', 'SCREEN', .FALSE., IRPT )
      CALL READ_KEY( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Output second portion of the banner page
      CALL BANNER_2( )
!
!-----------------------------------------------------------------------------
! *** Output initial and specie information to the report file and open files
!-----------------------------------------------------------------------------
!
! *** Output information and check for errors
!
      IF( VERBOSE ) CALL TELLTIME( 'Checking for input errors', 'SCREEN', .FALSE., IRPT )
      CALL CHECK_ECHO( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Open the data file from ECEM
!
      IF( VERBOSE ) CALL TELLTIME( 'Opening HUMAN details file', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_HUMAN( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Open the output data file
!
      IF( VERBOSE ) CALL TELLTIME( 'Opening output results file', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_RESULTS( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
!-----------------------------------------------------------------------------
! *** Process the requested set of table extractions
!-----------------------------------------------------------------------------
!
      CALL TELLTIME( 'Processing Data', 'SCREEN', .FALSE., IRPT )
      CALL PROCESS_HUMAN( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Normal completion of the program
!
      GO TO 1000
!
!-----------------------------------------------------------------------
!     Fatal errors trapped after report file is available return to
!     this point for termination (errors trapped before the report
!     file is available terminate at the point of the error trap).
!-----------------------------------------------------------------------
!
  999 CONTINUE
!
      MESSAG(1) = 'Error encountered in a lower-level routine.'
      MESSAG(2) = 'Execution halted because of the above errors.'
      CALL PRTERR( IERR, PRGNAM, 2 )
      CALL TELLTIME( 'Abnormal Run Termination Due to Errors', 'SCREEN', .FALSE., IRPT )
      STOP
!
 1000 CONTINUE
!
! *** Elapsed time message
!
      MESSAG(1) = 'Normal Termination'
      CALL PRTERR( IERR, PRGNAM, 1 )
      CALL TELLTIME( 'Normal Termination', 'SCREEN', .FALSE., IRPT )
!
      STOP
      END PROGRAM

      SUBROUTINE BANNER_1( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a banner page to the report file.
!!
!!  History:
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Udate to TIIA, add copyright information
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
!---- Executable code --------------------------------------------------
!
! *** Define the header
!
      WRITE(IRPT,1000)
 1000 FORMAT(/)
!
      WRITE(IRPT,1010) 'H     H IIIIIII  GGGGG  H     H IIIIIII M     M PPPPPP   AAAAA   CCCCC  TTTTTTT'
      WRITE(IRPT,1010) 'H     H    I    G     G H     H    I    MM   MM P     P A     A C     C    T   '
      WRITE(IRPT,1010) 'H     H    I    G       H     H    I    M M M M P     P A     A C          T   '
      WRITE(IRPT,1010) 'HHHHHHH    I    G   GGG HHHHHHH    I    M  M  M PPPPPP  AAAAAAA C          T   '
      WRITE(IRPT,1010) 'H     H    I    G     G H     H    I    M     M P       A     A C          T   '
      WRITE(IRPT,1010) 'H     H    I    G     G H     H    I    M     M P       A     A C     C    T   '
      WRITE(IRPT,1010) 'H     H IIIIIII  GGGGG  H     H IIIIIII M     M P       A     A  CCCCC     T   '
 1010 FORMAT(1X,A)
!
      WRITE(IRPT,1020) PRGNAM, PRGVER, PRGDAT
 1020 FORMAT(//27X,A,' Version ',A/31X,'Modified on ',A)
!
      RETURN
      END SUBROUTINE

      SUBROUTINE BANNER_2( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints banner information to the report file.
!!
!!  History:
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Qa_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
!---- First executable code --------------------------------------------
!
! *** Identification information
!
      WRITE(IRPT,1000) CRUNID, USRNAM
 1000 FORMAT(/10X,'Current Run ID = ',A14,'   User Name = ',A16)
!
      WRITE(IRPT,1010) SYSDAT, SYSTIM
 1010 FORMAT(17X,'System Date = ',A10,'   System Time = ',A8)
!
! *** Code status disclaimer (centered on page)
      CALL QA_CopyrightFull( Irpt, .TRUE. )
      CALL QA_Disclaimer( Irpt, .TRUE. )
      CALL QA_Reference( Irpt )
!
! *** Review Block
!
      WRITE(IRPT,1050)
 1050 FORMAT(//'                             Review Signatures')
!
      WRITE(IRPT,1060)
 1060 FORMAT(/'Input Prepared By: ______________________________', &
             '       Date: _______________')
!
      WRITE(IRPT,1070)
 1070 FORMAT(/'Input Reviewed By: ______________________________', &
             '       Date: _______________')
!
      RETURN
      END SUBROUTINE

      SUBROUTINE CHECK_ECHO( IERR )
!!************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles writing an echo of the problem definition
!!    to the report file.  In addition, it error checks the input data.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Jul 2004 : Version 2.0
!!    Paul W. Eslinger :  5 Jun 2007 : Enhance outputs
!!
!!************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'CHECK_ECHO' ! Name of this subroutine
      INTEGER :: ILOC ! Location looping index
      INTEGER :: ITIM ! Time looping index
      INTEGER :: ITMP ! Temporary integer
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(//24('='),' Echo of the Problem Definition ',24('='))
!
! *** Problem Identification
      WRITE(IRPT,1010) PTITLE, USRNAM
 1010 FORMAT(/'Title: ',A/'User:  ',A)
!
! *** Files names used in the analysis
      WRITE(IRPT,1020) 'File name for Input Keyword Data', TRIM(FNKEY)
      WRITE(IRPT,1020) 'File name for the Report File', TRIM(FNRPT)
      WRITE(IRPT,1020) 'File name for input HUMAN Data', TRIM(FNHUM)
      WRITE(IRPT,1020) 'File name for output Results', TRIM(FNRES)
 1020 FORMAT(/A/'File: ',A)
!
! *** Solutions requested
      IF( SOL_USE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The desired solution type is undefined'
        MESSAG(2) = 'Change the S_TYPE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      WRITE(IRPT,1050) TRIM(SOL_USE)
 1050 FORMAT(/'Solution (type) = "',A,'"')
!
! *** Analytes requested
      IF( ANA_USE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'The desired analytes is undefined'
        MESSAG(2) = 'Change the ANA_ID keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      WRITE(IRPT,1060) TRIM(ANA_USE)
 1060 FORMAT('Analyte (type) =  "',A,'"')
!
! *** Result types requested
      IF( RES_USE .EQ. ' ' ) THEN
        IERR = 3
        MESSAG(1) = 'The desired result type is undefined'
        MESSAG(2) = 'Change the R_TYPE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      WRITE(IRPT,1080) TRIM(RES_USE)
 1080 FORMAT('Result (type)   = "',A,'"')
!
! *** Locations desired
      IF( NUM_LOC .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'The number of desired locations is zero'
        MESSAG(2) = 'Change the LOC_ID keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      WRITE(IRPT,1100)
 1100 FORMAT(/'Location information'/ &
              'Index    ID   '/ &
              '----- --------')
      DO ILOC = 1, NUM_LOC
        WRITE(IRPT,1040) ILOC, LOC_USE(ILOC)
 1040   FORMAT(1X,I3,'  "',A6,'"')
      END DO
      WRITE(IRPT,1120) NUM_LOC
 1120 FORMAT('A total of ',I0,' locations have been requested.')
!
! *** Times desired
!
      IF( NUM_TIM .LT. 1 ) THEN
        IERR = 5
        MESSAG(1) = 'The number of desired times is zero'
        MESSAG(2) = 'Change the TIME keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      WRITE(IRPT,1130)
 1130 FORMAT(/'Time information'/ &
              'Index  Year  '/ &
              '----- -----')
      DO ITIM = 1, NUM_TIM
        WRITE(IRPT,1140) ITIM, TIM_USE(ITIM)
 1140   FORMAT(1X,I3,3X,I5)
      END DO
      WRITE(IRPT,1150) NUM_TIM
 1150 FORMAT('A total of ',I0,' times have been requested.')
!
      ALLOCATE( REL_DATA(NUM_TIM), STAT=IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating memory for RES_DATA(NUM_TIM)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( LOC_DATA(NUM_TIM), STAT=IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating memory for LOC_DATA(NUM_TIM)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Realization checks
      ITMP = 0
      IF( USE_MAX ) ITMP = ITMP + 1
      IF( USE_AVG ) ITMP = ITMP + 1
      IF( USE_SNG ) ITMP = ITMP + 1
      IF( ITMP .NE. 1 ) THEN
        IERR = 7
        MESSAG(1) = 'Only one solution type is allowed, options are'
        MESSAG(2) = 'MAXIMUM, MEAN, or SINGLE'
        MESSAG(3) = 'Change the REALIZAT keyword'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      IF( NREAL .LT. 1 ) THEN
        IERR = 8
        MESSAG(1) = 'At least one realization required in the HUMAN file'
        MESSAG(2) = 'Change the HUMAN modifier on the REALIZAT keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( USE_SNG ) THEN
        IF( NUM_REL.LT.1 .OR. NUM_REL.GT.NREAL )  THEN
          IERR = 9
          MESSAG(1) = 'Invalid index for the single realization'
          MESSAG(2) = 'Range is 1 to the number of HUMAN realizations'
          MESSAG(3) = 'Change the value with the SINGLE modifier on the REALIZAT keyword'
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
      END IF
!
      IF( USE_MAX ) WRITE(IRPT,1160) 'Solution type is MAXIMUM'
      IF( USE_AVG ) WRITE(IRPT,1160) 'Solution type is MEAN'
      IF( USE_SNG ) WRITE(IRPT,1160) 'Solution type is SINGLE =', NUM_REL
 1160 FORMAT(/A,:,1X,I0)
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE COMMAND_LINE_HELP( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes information on the command line invocation
!!    needed to run the code.  The information is written to the
!!    standard output device.
!!
!!  History:
!!
!!    Paul W. Eslinger : 27 Oct 2004 : Original source
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
!---- First executable code ---------------------------------------------
!
      WRITE(*,*) ' '
      WRITE(*,*) 'A single (optional) command line argument is allowed.  Enter'
      WRITE(*,*) '  '//TRIM(PRGNAM)//'            : To be prompted for the control keyword file name'
      WRITE(*,*) '  '//TRIM(PRGNAM)//' file_name  : To define the control keyword file name'
      WRITE(*,*) '  '//TRIM(PRGNAM)//' -HELP      : To get this help message (but not execute anything)'
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE IDEN_SET( )
!!**********************************************************************
!!
!!  Purpose:
!!
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
!!
!!    This subroutine is coded for a PC using a FORTRAN 95 compiler.
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      CHARACTER(LEN=10) :: SDATE ! System date in the form YYYYMMDD
      CHARACTER(LEN=10) :: STIME ! System time in the form HHMMSS.SSS
!
!---- Executable code --------------------------------------------------
!
! *** User name
      USRNAM = 'Anonymous User'
!
! *** Program name and version number
      PRGNAM = 'HighImpact'
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
!
      SUBROUTINE INIT( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine initializes the global variables of the ECEM
!!    software package.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Jul 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
!---- Executable code --------------------------------------------
!
      VERBOSE = .FALSE.
!
! *** Keyword input file
!
      IKEY  = 9
      FNKEY = ' '
!
! *** Report file
!
      IRPT  = 10
      IRPT_ERR = IRPT
      FNRPT = ' '
      REPORT = .FALSE.
!
! *** HUMAN output file
!
      IHUM  = 11
      FNHUM = ' '
!
! *** Output results file
!
      IRES  = 12
      FNRES = ' '
!
! *** Problem title, solution nad analyte type
!
      PTITLE = ' '
!
! *** Counter type information
!
      NUM_LOC = 0
      NUM_TIM = 0
!
! *** Solution type information
!
      USE_MAX = .FALSE.
      USE_AVG = .FALSE.
      USE_SNG = .FALSE.
!
      NREAL   = 0
      NUM_REL = 0
!
      RETURN
      END SUBROUTINE
!
      INTEGER FUNCTION KEEP_YEAR( TMP_YEAR )
!!*****************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine compares a temporary year value in TMP_YEAR to see if it
!!    is in the list of desired years.  The index of the match is returned if
!!    a match is found, otherwise value of 0 is returned.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Jul 2004 : Version 1.0
!!
!!*****************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: TMP_YEAR ! Year value to check against the list of saved years
!
! *** Local variables
      INTEGER :: I ! Looping variable
!
!---- Executable code --------------------------------------------
!
      KEEP_YEAR = 0
      DO I = 1, NUM_TIM
        IF( TMP_YEAR .EQ. TIM_USE(I) ) THEN
          KEEP_YEAR = I
          RETURN
        END IF
      END DO
!
      RETURN
      END FUNCTION
!
      LOGICAL FUNCTION KEEP_LOC( TMP_LOC )
!!*****************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine compares a temporary location ID in TMP_LOC to see if it
!!    is in the list of desired locations.  A .TRUE. value is returned if a match
!!    is found, otherwise a .FALSE. value is returned.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Jul 2004 : Version 1.0
!!
!!*****************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*) :: TMP_LOC ! Location ID to check against the list of saved locations
!
! *** Local variables
      INTEGER :: I ! Looping variable
!
!---- Executable code --------------------------------------------
!
      KEEP_LOC = .FALSE.
      DO I = 1, NUM_LOC
        IF( TMP_LOC .EQ. LOC_USE(I) ) THEN
          KEEP_LOC = .TRUE.
          RETURN
        END IF
      END DO
!
      RETURN
      END FUNCTION
!
      SUBROUTINE OPEN_HUMAN( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the HUMAN data file for reading.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Jul 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'OPEN_HUMAN' ! Name of this subroutine
      LOGICAL :: THERE ! File inquire flag
      INTEGER :: IERF  ! Status variable for open statement
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the requested file exists
!
      IF( FNHUM .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The HUMAN data file name is blank'
        MESSAG(2) = 'Change the file name on the FILE keyword, HUMAN modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      INQUIRE(FILE=FNHUM,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested HUMAN data file was not found'
        MESSAG(2) = 'Change the file name on the FILE keyword, HUMAN modifier'
        MESSAG(3) = 'File: '//TRIM(FNHUM)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IHUM,FILE=FNHUM,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the HUMAN data file'
        MESSAG(2) = 'File name entered on the FILE keyword, HUMAN modifier'
        MESSAG(3) = 'File: '//TRIM(FNHUM)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
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
!!  History:
!!    Paul W. Eslinger : 25 Jul 2004 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Udate to TIIA, add copyright information
!!    Paul W. Eslinger : 29 Jun 2012 : Revise to a common callable routine
!!
!!  Call List Variables:
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in locating or opening the input keyword file
!!
!!********************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** External functions
      INTEGER, EXTERNAL :: NARGS ! Lahey utility subroutine
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      LOGICAL :: THERE ! File existence variable
      INTEGER :: IERF  ! Status variable for open statement
      INTEGER :: NUM_ARGS      ! Number of command line arguments
      INTEGER :: NUM_FNAM      ! Argument number for the keyword file name
      CHARACTER(LEN=5) :: HELP ! Character for checking for help
!
!---- First executable code --------------------------------------------
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
!     NARGS is a Lahey Fortran language extension
!     The command is argument number 1 for this call
      NUM_ARGS = NARGS( )
!
! *** Use the file name if it was entered on the command line
      IF( NUM_ARGS .GT. 1 ) THEN
!       GETARG is a Lahey Fortran language extension that returns the specified
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
 1010   FORMAT(/' Enter the control keyword file name > ')
        READ(*,*) FNKEY
      END IF
!
! *** Check if the requested file exists
!
      INQUIRE(FILE=FNKEY,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 1
        WRITE(*,1020) TRIM(FNKEY)
 1020   FORMAT(' The requested keyword file was not found'/ &
          ' File: ',A)
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IKEY,FILE=FNKEY,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 2
        WRITE(*,1030) TRIM(FNKEY)
 1030   FORMAT(' System error opening the input keyword file'/ &
               ' File: ',A)
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE OPEN_RESULTS( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain the
!!    extracted data values.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Jul 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Errors_Mod
      USE Iden_Mod
      USE Control_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      INTEGER :: IERF ! Status variable for open statement
      CHARACTER(LEN=12) :: CALLER = 'OPEN_RESULTS'
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the file name was entered
!
      IF( FNRES .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The output results data file name is blank'
        MESSAG(2) = 'Change the file name on the FILE keyword, RESULTS modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IRES,FILE=FNRES,STATUS='UNKNOWN',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the results data file'
        MESSAG(2) = 'File name entered on the FILE keyword, RESULTS modifier'
        MESSAG(3) = 'File: '//TRIM(FNRES)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Write a few identification items to the file
!
      WRITE(IRES,1000) 'Code Name:',    TRIM(PRGNAM)
      WRITE(IRES,1000) 'Code Version:', TRIM(PRGVER)
      WRITE(IRES,1000) 'Code Date:',    TRIM(PRGDAT)
      WRITE(IRES,1000) 'Run ID:',       TRIM(CRUNID)
      WRITE(IRES,1000) 'Run Title:',    TRIM(PTITLE)
      WRITE(IRES,1000) 'User Name:',    TRIM(USRNAM)
 1000 FORMAT('"',A,'"',:,',"',A,'"')
!
      IF( USE_MAX ) WRITE(IRES,1010) 'Solution type', 'MAXIMUM', TRIM(ANA_USE), TRIM(RES_USE), TRIM(SOL_USE)
      IF( USE_AVG ) WRITE(IRES,1010) 'Solution type', 'MEAN', TRIM(ANA_USE), TRIM(RES_USE), TRIM(SOL_USE)
 1010 FORMAT('"',A,':","',A,'","',A,'","',A,'","',A,'"')
      IF( USE_SNG ) WRITE(IRES,1020) 'Solution type', 'SINGLE =', NUM_REL, TRIM(ANA_USE), TRIM(RES_USE), TRIM(SOL_USE)
 1020 FORMAT('"',A,':","',A,1X,I0,'","',A,'","',A,'","',A,'"')
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PROCESS_HUMAN( IERR )
!!*****************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine compares NCHAR characters from two character strings
!!    and checks for a match that is not case sensitive.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Jul 2004 : Version 1.0
!!
!!*****************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined external functions
      INTEGER, EXTERNAL :: KEEP_YEAR
      LOGICAL, EXTERNAL :: KEEP_LOC
!
! *** Call list variables
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
      CHARACTER(LEN=13) :: CALLER = 'PROCESS_HUMAN' ! Name of this subroutine
!
      CHARACTER(LEN=13) :: STRING1, STRING2, STRING3, STRING4, STRING5
      INTEGER :: IREL     ! Realization index
      INTEGER :: ITIM     ! Time index
      INTEGER :: TMP_YEAR ! Temporary year variable
      INTEGER :: MATCH    ! Temporary matching variable
      REAL :: TMP_VAL     ! Temporary value
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Allocate a temporary vector for numerical values
!
      IF( NREAL .GT. 0 ) THEN
        ALLOCATE( TMP_DATA(NREAL), STAT= IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 1
          MESSAG(1) ='Error allocating TMP_DATA(NREAL)'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      ELSE
        IERR = 201
        MESSAG(1) = 'Number of HUMAN realizations must be greater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Read the header line
!
      READ(IHUM,*,END=9999) STRING1, STRING2, STRING3, STRING4, STRING5
!
! *** Check for the correct header line
!
      IF( .NOT.( STRING1.EQ.'Time' .AND. STRING2.EQ.'Location ID' .AND. STRING3.EQ.'Analyte ID' .AND. &
                 STRING4.EQ.'Analyte Type' .AND. STRING5.EQ.'Solution Type' ) ) THEN
        IERR = 2
        MESSAG(1) = 'The data file is not of type HUMAN details'
        MESSAG(2) = 'File: '//TRIM(FNHUM)
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Top of loop on reading and processing data
!       The order of the checks against requested data is intended to minimize the
!       time spent in the checks.
!
!     Set up to save off the maximum of different metrics
      REL_DATA = -HUGE(1.0)
      LOC_DATA = 'XXXXXX'
!
      MATCH = 0
   10 CONTINUE
        READ(IHUM,*,END=9998) TMP_YEAR, STRING1, STRING2, STRING3, STRING4, STRING5, (TMP_DATA(IREL),IREL=1,NREAL)
!        IF( VERBOSE) WRITE(*,*) TMP_YEAR, STRING1, STRING2, STRING3, STRING4, STRING5
!
!        TMP_VAL = TMP_DATA(NUM_REL)
!        WRITE(*,*) ' Data   ', TMP_YEAR, TRIM(STRING1), ' ', trim(string2), ' ', &
!                       trim(string3), ' ', trim(string4), ' ', trim(string5), tmp_val, tmp_data
!
! ***   Check on a requested analyte (ANA_ID)
        IF( STRING2 .NE. ANA_USE ) GO TO 10
!
! ***   Check on a requested analyte (result) type
        IF( STRING3 .NE. RES_USE ) GO TO 10
!
! ***   Check on a requested solution type
        IF( STRING4 .NE. SOL_USE ) GO TO 10
!
! ***   Check on a requested year
        ITIM = KEEP_YEAR( TMP_YEAR )
        IF( ITIM .LT. 1 ) GO TO 10
!
! ***   Check on a requested location
        IF( .NOT.KEEP_LOC( STRING1 ) ) GO TO 10
!
        MATCH = MATCH + 1
!
! ***   Process the desired solution for this data line
!
        IF( USE_MAX ) THEN
          TMP_VAL = -HUGE(1.0)
          DO IREL = 1, NREAL
            TMP_VAL = MAX( TMP_VAL, TMP_DATA(IREL) )
          END DO
          IF( TMP_VAL .GT. REL_DATA(ITIM) ) THEN
            REL_DATA(ITIM) = TMP_VAL
            LOC_DATA(ITIM) = TRIM(STRING1)
          END IF
        END IF
!
        IF( USE_AVG ) THEN
          TMP_VAL = SUM( TMP_DATA ) / NREAL
          IF( TMP_VAL .GT. REL_DATA(ITIM) ) THEN
            REL_DATA(ITIM) = TMP_VAL
            LOC_DATA(ITIM) = TRIM(STRING1)
          END IF
        END IF
!
        IF( USE_SNG ) THEN
          TMP_VAL = TMP_DATA(NUM_REL)
          IF( TMP_VAL .GT. REL_DATA(ITIM) ) THEN
!            WRITE(*,*) ' Bigger ', TMP_YEAR, TRIM(STRING1), ' ', trim(string2), ' ', &
!                       trim(string3), ' ', trim(string4), ' ', trim(string5), tmp_val
            REL_DATA(ITIM) = TMP_VAL
            LOC_DATA(ITIM) = TRIM(STRING1)
          END IF
        END IF
!
        GO TO 10
!
! *** Normal exit point
!
 9998 CONTINUE
!
! *** Write out the selected data
!
      IF( MATCH .GT. 0 ) THEN
        DO ITIM = 1, NUM_TIM
          IF( LOC_DATA(ITIM) .NE. 'XXXXXX' ) WRITE(IRES,1050) TIM_USE(ITIM), TRIM(LOC_DATA(ITIM)), REL_DATA(ITIM)
 1050     FORMAT(I0,',"',A,'",',1P,E12.5)
        END DO
      ELSE
        WRITE(IRES,2000) 'No matching data were identified'
 2000   FORMAT('"',A,'"')
      END IF
      RETURN
!
! *** Error exit point
!
 9999 CONTINUE
      IERR = 3
      MESSAG(1) = 'End-of-file reading the HUMAN results file'
      MESSAG(2) = 'File: '//TRIM(FNHUM)
      CALL PRTERR( IERR, CALLER, 2 )
      RETURN
!
      END SUBROUTINE

      SUBROUTINE READ_KEY( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and the keyword control
!!    information required to set variable dimensions.
!!
!!  Call List Variables:
!!
!!    IERR : Output integer error flag.
!!            0 = Normal execution
!!           >0 = Terminal error encountered
!!
!!  Auxiliary Routines:
!!
!!    CEXIST, PRTERR, and all RDBLK related routines
!!
!!  Notes:
!!
!!    When errors are encountered, a message is output to the standard
!!    output device and control is passed back to the calling routine.
!!
!!  History:
!!
!!    Paul W. Eslinger :  6 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Move REPORT to another routine
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined external functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'READ_KEY' ! Nmae of this subroutine
!
      CHARACTER :: TITLE*(LENCRD)       ! Title line from RDBLK
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary variable for file names
!
      REAL :: RTMP   ! Temporary real value
      INTEGER :: IDX ! Temporary index variable
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
!     Initialize for RDBLK
      ILINE = 0
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!----------------------------------------------------------------------C
!
   10 CONTINUE
!
      CALL RDBLK( IKEY, IRPT, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level RDBLK routine'
        MESSAG(2) = 'Examine the standard output messages for details'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
!----------------------------------------------------------------------------
        CASE( 'ANA_ID' ) ! ===> ANA_ID Keyword
          IF( NQUOTE .GT. 0 ) ANA_USE = QUOTE(1)
!
!----------------------------------------------------------------------------
        CASE( 'END' ) ! ===> END keyword
          CLOSE( IKEY )
          RETURN
!
!----------------------------------------------------------------------------
        CASE( 'FILE' ) ! ===> FILE keyword
!
          IF( CEXIST('HUMAN') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 3
              MESSAG(1) = 'The HUMAN modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNHUM = TMP_NAME
            END IF
          END IF
!
          IF( CEXIST('RESULTS') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 3
              MESSAG(1) = 'The RESULTS modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNRES = TMP_NAME
            END IF
          END IF
!
        CASE( 'LOC_ID' ) ! ===> LOC_ID Keyword
!         Count quote strings, allocate memory, store data
          NUM_LOC = NQUOTE
          IF( NUM_LOC .GT. 0 ) THEN
            ALLOCATE( LOC_USE(NUM_LOC) )
            DO IDX = 1, NUM_LOC
              LOC_USE(IDX) = QUOTE(IDX)(1:6)
            END DO
          END IF
!
!----------------------------------------------------------------------------
        CASE( 'REALIZAT' ) ! ===> REALIZATION Keyword
!
          IF( CEXIST('HUMAN') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              NREAL = RTMP
            ELSE
              IERR = 15
              MESSAG(1) = 'REALIZAT HUMAN modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 16
            MESSAG(1) = 'HUMAN modifier not entered on the REALIZAT keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!         Options are { MAXIMUM | MEAN | MEDIAN | SINGLE=NUM_REL}
          IF( CEXIST('MAXIMUM') ) USE_MAX = .TRUE.
          IF( CEXIST('MEAN') )    USE_AVG = .TRUE.
          IF( CEXIST('SINGLE') )  THEN
            USE_SNG = .TRUE.
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              NUM_REL = RTMP
            ELSE
              IERR = 16
              MESSAG(1) = 'REALIZAT SINGLE modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!----------------------------------------------------------------------------
        CASE( 'R_TYPE' ) ! ===> R_TYPE Keyword
          IF( NQUOTE .GT. 0 ) RES_USE = QUOTE(1)
!
!----------------------------------------------------------------------------
        CASE( 'S_TYPE' ) ! ===> S_TYPE Keyword
          IF( NQUOTE .GT. 0 ) SOL_USE = QUOTE(1)
!
!----------------------------------------------------------------------------
        CASE( 'TIME' ) ! ===> TIME Keyword
!         Count values, allocate memory, store data
          NUM_TIM = NVALUE
          IF( NUM_TIM .GT. 0 ) THEN
            ALLOCATE( TIM_USE(NUM_TIM) )
            DO IDX = 1, NUM_TIM
              TIM_USE(IDX) = VALUE(IDX)
            END DO
          END IF
!
!----------------------------------------------------------------------------
        CASE( 'TITLE' ) ! ===> TITLE Keyword
          PTITLE = QUOTE(1)
!
!----------------------------------------------------------------------------
        CASE( 'USER' ) ! ===> USER Keyword
          USRNAM = QUOTE(1)
!
!----------------------------------------------------------------------------
        CASE( 'VERBOSE' ) ! ===> VERBOSE Keyword
          VERBOSE = .TRUE.
!
!----------------------------------------------------------------------------
        CASE DEFAULT ! Ignore other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE
!
      SUBROUTINE READ_KEYS_REPORT( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the control keywords looking for the
!!    report file definition.  The expected keyword structure is
!!    the following:
!!      REPORT "File name"
!!
!!    The report file is opened once the name is found.
!!
!!  Auxiliary Routines:
!!
!!    PRTERR, and all RDBLK related routines
!!
!!  History:
!!
!!    Paul W. Eslinger :  5 Jun 2007 : Original source
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      CHARACTER(LEN=16) :: CALLER = 'READ_KEYS_REPORT' ! Name of this subroutine
      CHARACTER :: TITLE*(LENCRD)                      ! Title line from RDBLK
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
      ILINE = 0
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!----------------------------------------------------------------------C
!
   10 CONTINUE
!
      CALL RDBLK( IKEY, IRPT, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error in lower level RDBLK routine'
        WRITE(*,*) 'Message issued in routine ' // CALLER
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'END' ) ! ===> END keyword ------------------------------------------------
          WRITE(*,*) ' '
          WRITE(*,*) 'The REPORT file name was not identified in the keyword file'
          WRITE(*,*) 'The keyword structure expected was REPORT "File name"'
          WRITE(*,*) 'Unable to open the report file in subroutine '//CALLER
          IERR = 666
          RETURN
!
        CASE( 'REPORT' ) ! ===> REPORT keyword ----------------------------------------------
!
!         Check that exactly one quote string is entered
          IF( NQUOTE .NE. 1 ) THEN
            WRITE(*,*) ' '
            WRITE(*,*) 'The REPORT file keyword should have exactly one quote string'
            WRITE(*,*) 'Problem encountered in subroutine '//CALLER
            IERR = 666
            RETURN
          END IF
!
!         Open the report file
          FNRPT = QUOTE(1)
          IF( FNRPT .NE. ' ' ) THEN
            OPEN(IRPT,FILE=FNRPT,STATUS='UNKNOWN',IOSTAT=IERR)
            IRPT_ERR = IRPT
            REPORT = .TRUE.
          ELSE
            IERR = 666
          END IF
!
!         Error opening the report file
          IF( IERR .NE. 0 ) THEN
            WRITE(*,*) ' Unable to open the report file in subroutine '//CALLER
            WRITE(*,*) ' File '//TRIM(FNRPT)
            IERR = 666
            RETURN
          END IF
!
          REWIND( IKEY )
          RETURN
!
        CASE DEFAULT ! Ignore all other keywords ----------------------------------------
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE
!
      SUBROUTINE TELLTIME( MESSAGE, PLACE, LDATE, IRPT )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine prints the date and time and a message to the
!!    computer screen, the report file, or both locations.
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
!!    LDATE     If .true. output date with the time
!!    IRPT      Unit number for output file
!!
!!  History:
!!
!!    Paul W. Eslinger :  5 Jun 2007 : Original code
!!
!!**********************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: STRCOMP
!
! *** Call list variables
      CHARACTER(LEN=*) :: MESSAGE ! Character string to write
      CHARACTER(LEN=*) :: PLACE   ! Place to output the mesage
      LOGICAL :: LDATE            ! Only print date if LDATE=.TRUE.
      INTEGER :: IRPT             ! Unit number for output file
!
! *** Local variables
      CHARACTER(LEN=10) :: EDATE ! System date in the form YYYYMMDD
      CHARACTER(LEN=10) :: ETIME ! System time in the form HHMMSS.SSS
!
! *** Start of executable code
!
      CALL DATE_AND_TIME( EDATE, ETIME )
!
! *** Output to the requested places
!
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

