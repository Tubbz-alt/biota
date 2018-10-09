!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
!
MODULE Control_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This include deck contains control information for the entire problem as well as
!    providing allocation of work arrays for a variety of purposes.
!
!  History:
!    Paul W. Eslinger : 17 Aug 2004 : Original code
!
      CHARACTER(LEN=200) :: PTITLE  ! Title for this program run
!
      CHARACTER(LEN=6) :: ANA_USE  ! Analyte to process
      INTEGER :: ANA_IDX           ! Analyte index in ESD data
      CHARACTER(LEN=6) :: MED_USE  ! Media use to process
!
      CHARACTER(LEN=7) :: SOLUTION ! Solution type to process
      INTEGER :: IMEDIA            ! Media index
      INTEGER :: NUM_SNG           ! Realization number to extract (SINGLE option)
!
      LOGICAL :: USE_PLOT  ! Develop an output plot at a given year
      INTEGER :: PLOT_YEAR ! Year for output of the plot
!
      CHARACTER(LEN=10) :: SDATE ! Start date of processing
      CHARACTER(LEN=10) :: STIME ! Start time of processing
!
      LOGICAL :: REPORT  ! Flag whether the report file is open
      LOGICAL :: VERBOSE ! Flag for verbose outputs to the screen
!
      INTEGER :: NUM_ANA ! Number of analytes in this run
      INTEGER :: NUM_TIM ! Number of times in this run
      INTEGER :: NUM_LOC ! Number of locations in this run
!
      REAL, ALLOCATABLE :: CVEC(:)       ! Temporary concentration vector by realization
      REAL, ALLOCATABLE :: WVEC(:)       ! Work vector (length ESD_NREAL)
      REAL :: CONVERT_UNITS              ! Factor for conversion of data units
      CHARACTER(LEN=8) :: UNITS_INPUT    ! Input data units
      CHARACTER(LEN=8) :: UNITS_OUTPUT   ! Output data units
!
      INTEGER :: NUM_CASES ! Number of cases (used as the case counter as well)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE ESD_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information from the ESD file that will be
!    used in all the codes, in addition, it contains some information
!    defined in this code that depends on location, time, or analyte.
!
!  History:
!
!    Paul W. Eslinger : 10 Mar 2000 : Version 1.0
!    Paul W. Eslinger : 27 Feb 2003 : SAC Rev. 1
!
!  Variables:
!
      INTEGER :: ESD_NUM_ANA ! Number of analytes in the ESD file
      INTEGER :: ESD_NUM_LOC ! Number of locations in the ESD file
      INTEGER :: ESD_NUM_TIM ! Number of times in the ESD file
      INTEGER :: ESD_NUM_SPC ! Number of species in the ESD file
      INTEGER :: ESD_NREAL   ! Number of realizations in the ESD file
      INTEGER, PARAMETER :: ESD_NUM_SOI = 3 ! Number of upland soil types allowed
      INTEGER :: SOIL_IDX         ! Soil dependent index for write statements
      CHARACTER(LEN=4) :: SOIL_ID ! Soil type for write statements
!
      CHARACTER(LEN=72) :: ESD_TITLE ! Title in the ESD file
!
!     Type definition for ESD time data
!
      TYPE ESD_TIM_TYPE
        INTEGER :: TIME ! Times where data are stored
        LOGICAL :: COMP ! Flag whether this time is to be used
      END TYPE ESD_TIM_TYPE
      TYPE (ESD_TIM_TYPE), ALLOCATABLE :: ESD_TIM(:) ! The ESD location variable
!
!     Type definition for ESD location data
!
      TYPE ESD_LOC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for a location
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a location
        CHARACTER(LEN= 8) :: TYPE ! Location type (aquatic, riparian, or upland)
        LOGICAL :: COMP           ! Flag whether this location is to be used
        LOGICAL :: OUTPUT         ! Flag whether results are output for this location
        REAL :: EASTING           ! Easting coordinate in state plane projection
        REAL :: NORTHING          ! Northing coordinate in state plane projection
        INTEGER :: POP            ! Population
        INTEGER :: MEAT           ! Location index for meat
        INTEGER :: FISH           ! Location index for fish
        INTEGER :: BIRD           ! Location index for bird
      END TYPE ESD_LOC_TYPE
      TYPE (ESD_LOC_TYPE), ALLOCATABLE :: ESD_LOC(:) ! The ESD location variable
!
!     Type definition for ESD analyte data
!
      TYPE ESD_ANA_TYPE ! Type definition for ESD analyte data
        CHARACTER(LEN= 6) :: ID   ! Identification number for an analyte
        CHARACTER(LEN=72) :: NAME ! Descriptive name for an analyte
        CHARACTER(LEN= 2) :: TYPE ! Analyte type
!                                    OS = organic, stable      OR = organic, radioactive,
!                                    NS = nonorganic, stable   NR = nonorganic, radioactive,
        INTEGER :: ANATYP         ! Vector of analyte types for analysis:
!                                    1=radioactive, 2=carcinogen, 3=noncarcinogen
        LOGICAL :: COMP           ! Flag whether this analyte is to be used
        LOGICAL :: OUTPUT         ! Flag whether results are output for this analyte
      END TYPE ESD_ANA_TYPE
      TYPE (ESD_ANA_TYPE), ALLOCATABLE :: ESD_ANA(:) ! The ESD analyte variable
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
      INTEGER :: IESD               ! Unit number for the ESD data file
      CHARACTER(LEN=MAXFN) :: FNESD ! Name of the ESD data file
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
      INTEGER :: IMAP               ! Unit number for the concentration record number map file
      CHARACTER(LEN=MAXFN) :: FNMAP ! Name of the concentration record number map file
!
      INTEGER :: IPLT               ! Unit number for the optional plot file
      CHARACTER(LEN=MAXFN) :: FNPLT ! Name of the optional output plot file
!
      INTEGER :: ICON               ! Unit number for the concentration file
      CHARACTER(LEN=MAXFN), ALLOCATABLE :: FNCON(:) ! Name of the input concentration file
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

      PROGRAM HighMedia
!!***********************************************************************************************************
!!
!!             HighMedia - Extract highest media value from an ECDA File
!!                 Toolkit for Integrated Impacts Assessments (TIIA)
!!     Battelle, Pacific Northwest National Laboratories, Richland, Washington
!!
!!***********************************************************************************************************
!!
!!  Purpose:
!!    This code is designed to extract the largest media result over a set of
!!    specified locations and times for data contained in an ECDA file.
!!    For example, it can provide the highest groundwater concentration for a
!!    single analyte outside the Hanford core zone as a function of time.
!!
!! Reference:
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!  History:
!!    Carmen Arimescu  : 17 Aug 2004 : Original source
!!    Paul W. Eslinger : 19 Aug 2004 : Change to allow cases with runs
!!    Paul W. Eslinger : 27 Sep 2004 : Add output plot option
!!    Paul W. Eslinger :  5 Jun 2007 : Update for TIIA logic and copyright information
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!    Paul W. Eslinger : 13 Jul 2012 : Fix error in READ_CASE for the TIMES LIST option 
!!
!!***********************************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Iden_Mod
      USE ESD_Mod
      USE ECDA_Mod
      USE RDBLK_Mod, ONLY: ILINE
!
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: IERR          ! Integer error flag
      LOGICAL :: FINISHED      ! Finished processing cases
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
!-----------------------------------------------------------------------------
!     Program, run identification, and initialization
!-----------------------------------------------------------------------------
!
      CALL IDEN_SET( )
      CALL INIT( )
!
! *** Open the input file
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the keyword file'
        WRITE(*,*) 'Stop in '//PRGNAM
        STOP
      END IF
!
! *** Define the report file
      CALL TELLTIME( 'Keyword file: '//TRIM(FNKEY), 'SCREEN', .FALSE., IRPT )
      CALL TELLTIME( 'Extracting report file name', 'SCREEN', .FALSE., IRPT )
      CALL READ_KEYS_REPORT( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the report file'
        WRITE(*,*) 'Stop in '//PRGNAM
        STOP
      END IF
!
!     Start the banner page for the report file
      CALL BANNER_1( )
!
! *** Read the keywords to get the user defined and the ESD keyword file name
      CALL TELLTIME( 'Reading keywords - first pass', 'SCREEN', .FALSE., IRPT )
      CALL READ_KEY( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
!     Finish the banner page for the report file
      CALL BANNER_2( )
!
      CALL CHECK_KEY( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
!-----------------------------------------------------------------------------
!     Open and process the input environmental (ESD) keyword file
!-----------------------------------------------------------------------------
!
      IF( VERBOSE ) CALL TELLTIME( 'Opening ESD keyword File', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_ESD(IERR)
      IF( IERR .NE. 0 ) GOTO 999
!
! *** Read the environmental keywords to set dimensions
      CALL TELLTIME( 'Reading ESD Keywords - Pass #1', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_1( IERR )
      IF( IERR .NE. 0 ) GOTO 999
!
      CALL ESD_MEMORY( IERR )
      IF( IERR .NE. 0 ) GOTO 999
!
      CALL ESD_INIT( )
      IF( IERR .NE. 0 ) GOTO 999
!
! *** Read the second pass of environmental keywords
      CALL TELLTIME( 'Reading ESD Keywords - Pass #2', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_2( IERR )
      IF( IERR .NE. 0 ) GOTO 999
!
! *** Check problem dimension information
      CALL CHECK_ESD( IERR )
      IF( IERR .NE. 0 ) GOTO 999
!
! *** Output some information
      CALL ECHO_INIT( )
!
!-----------------------------------------------------------------------------
!    Loop on cases until an END card is read from the keyword file
!-----------------------------------------------------------------------------
!
      FINISHED = .FALSE.
      NUM_CASES = 0
!
! *** Line counter for RDBLK will be off, but start reading a keyword for the first case
      ILINE = 0
!
   10 CONTINUE
!
! ***   Reset data before the case
        IF( VERBOSE ) CALL TELLTIME( 'Initializing case variables', 'SCREEN', .FALSE., IRPT )
        CALL INIT_CASE( )
!
! ***   Read the definition for this case
        IF( VERBOSE ) CALL TELLTIME( 'Calling READ_CASE', 'SCREEN', .FALSE., IRPT )
        CALL READ_CASE( FINISHED, IERR )
        IF( IERR .NE. 0 ) GO TO 999
        IF( FINISHED ) GO TO 1000
        CALL TELLTIME( 'Case: '//TRIM(PTITLE), 'SCREEN', .FALSE., IRPT )
!
! ***   Check for errors
        IF( VERBOSE ) CALL TELLTIME( 'Calling CHECK_CASE', 'SCREEN', .FALSE., IRPT )
        CALL CHECK_CASE( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
! ***   Output the case definition
        IF( VERBOSE ) CALL TELLTIME( 'Calling ECHO_CASE', 'SCREEN', .FALSE., IRPT )
        CALL ECHO_CASE( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
! ***   Do the extraction for this case
        IF( VERBOSE ) CALL TELLTIME( 'Calling COMP_CASE', 'SCREEN', .FALSE., IRPT )
        CALL COMP_CASE( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Go attempt to read another case definition
!
        GO TO 10
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
!
      CALL TELLTIME( 'Abnormal Run Termination Due to Errors', 'SCREEN', .FALSE., IRPT )
      STOP
!
 1000 CONTINUE
!
! *** Normal completion of the program
!     Elapsed time message
!
      MESSAG(1) = 'Normal Termination'
      CALL PRTERR( IERR, PRGNAM, 1 )
!
      CALL TELLTIME( 'Normal Termination', 'SCREEN', .FALSE., IRPT )
      STOP
!
      END PROGRAM

      SUBROUTINE BANNER_1( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a banner page to the report file.
!!
!!  History:
!!    Carmen Arimescu  : 17 Aug 2004 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Udate to TIIA, add copyright information
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
!
      IMPLICIT NONE
!
!---- Executable code --------------------------------------------------
!
! *** Define the header
!
      WRITE(IRPT,1000)
 1000 FORMAT(/)
      WRITE(IRPT,1010) 'H     H IIIIIII  GGGGG  H     H M     M EEEEEEE DDDDDD  IIIIIII   AAAAA '
      WRITE(IRPT,1010) 'H     H    I    G     G H     H MM   MM E       D     D    I     A     A'
      WRITE(IRPT,1010) 'H     H    I    G       H     H M M M M E       D     D    I     A     A'
      WRITE(IRPT,1010) 'HHHHHHH    I    G   GGG HHHHHHH M  M  M EEEE    D     D    I     AAAAAAA'
      WRITE(IRPT,1010) 'H     H    I    G     G H     H M     M E       D     D    I     A     A'
      WRITE(IRPT,1010) 'H     H    I    G     G H     H M     M E       D     D    I     A     A'
      WRITE(IRPT,1010) 'H     H IIIIIII  GGGGG  H     H M     M EEEEEEE DDDDDD  IIIIIII  A     A'
 1010 FORMAT(1X,A)
!
      WRITE(IRPT,1020) PRGNAM, PRGVER, PRGDAT
 1020 FORMAT(//27X,A,' Version ',A/29X,'Modified on ',A)
!
      RETURN
      END SUBROUTINE

      SUBROUTINE BANNER_2( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints banner information to the report file.
!!
!!
!!  History:
!!    Carmen Arimescu : 17 Aug 2004 : Version 1.0
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
      USE Qa_Mod
!
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

      SUBROUTINE CHECK_CASE( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks a case definition.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Aug 2004 : Version 1.0
!!    Paul W. Eslinger : 27 Sep 2004 : Add output plot option
!!    Paul W. Eslinger :  5 Jun 2007 : Update comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'CHECK_CASE' ! Name of this subroutine
!
      INTEGER :: ILOC ! Looping indices
      INTEGER :: ITIM ! Time loop counter
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(//32('='),' Case Definition ',31('='))
!
! *** Problem title
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The problem title is not defined'
        MESSAG(2) = 'Change the TITLE keyword'
        MESSAG(3) = 'Problem with case number '
        WRITE(MESSAG(3)(27:),*) NUM_CASES
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Check on media request
!
      IMEDIA = 0
      SELECT CASE ( MED_USE )
        CASE( 'GWAT' )
          IMEDIA = IGWAT
        CASE( 'SEEP' )
          IMEDIA = ISEEP
        CASE( 'SWAT' )
          IMEDIA = ISWAT
        CASE( 'PWAT' )
          IMEDIA = IPWAT
        CASE( 'SEDI' )
          IMEDIA = ISEDI
        CASE( 'SORP' )
          IMEDIA = ISORP
        CASE( 'SODR' )
          IMEDIA = ISODR
        CASE( 'SOGW' )
          IMEDIA = ISOGW
        CASE( 'SOSW' )
          IMEDIA = ISOSW
        CASE( 'AIRC' )
          IMEDIA = IAIRC
        CASE( 'AIRD' )
          IMEDIA = IAIRD
        CASE DEFAULT
          IERR = 2
          MESSAG(1) = 'The desired MEDIA is undefined'
          MESSAG(2) = 'Change the MEDIA keyword'
          MESSAG(3) = 'Problem with case number '
          WRITE(MESSAG(3)(27:),*) NUM_CASES
          CALL PRTERR( IERR, CALLER, 3 )
      END SELECT
!
! *** Check on the requested solution
!
      SELECT CASE ( SOLUTION )
        CASE( 'MEAN' )
        CASE( 'MEDIAN' )
        CASE( 'MAXIMUM' )
        CASE( 'SINGLE' )
          IF( NUM_SNG.LT.1 .OR. NUM_SNG.GT.ESD_NREAL ) THEN
            IERR = 3
            MESSAG(1) = 'The desired solution is a single realization'
            MESSAG(2) = 'Change the REALIZAT keyword to enter a realization number'
            MESSAG(3) = 'Example: REALIZAT SINGLE=5'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        CASE DEFAULT
          IERR = 4
          MESSAG(1) = 'The desired solution must be defined with the REALIZAT keyword'
          MESSAG(2) = 'Modifiers include MEAN, MEDIAN, MAXIMUM, and SINGLE=N'
          MESSAG(3) = 'Problem with case number '
          WRITE(MESSAG(3)(27:),*) NUM_CASES
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
      END SELECT
!
! *** Analyte requested
!
      IF( ANA_USE .EQ. ' ' ) THEN
        IERR = 5
        MESSAG(1) = 'The desired analyte is undefined'
        MESSAG(2) = 'Change the ANALYTE keyword'
        MESSAG(3) = 'Problem with case number '
        WRITE(MESSAG(3)(27:),*) NUM_CASES
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Set the analyte index into ESD data
!
      CALL MATCH_ANA( ANA_USE, ANA_IDX )
      IF( ANA_IDX .LT. 1 ) THEN
        IERR = 6
        MESSAG(1) = 'The requested analyte was not found in the ESD information'
        MESSAG(2) = 'Analyte ID: '//ANA_USE
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!      write(*,*) 'Found index ',ANA_IDX,' for '//TRIM(ANA_USE) !pwe@
!
! *** Units conversion
!
      IF( CONVERT_UNITS .LE. 0.0 ) THEN
        IERR = 7
        MESSAG(1) = 'The units conversion factor is undefined or negative'
        MESSAG(2) = 'Change the UNITS keyword, FACTOR modifier'
        MESSAG(3) = 'Problem with case number '
        WRITE(MESSAG(3)(27:),*) NUM_CASES
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      IF( UNITS_OUTPUT .EQ. ' ' ) THEN
        IERR = 8
        MESSAG(1) = 'The units output label is undefined'
        MESSAG(2) = 'Change the UNITS keyword, OUTPUT modifier'
        MESSAG(3) = 'Problem with case number '
        WRITE(MESSAG(3)(27:),*) NUM_CASES
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Check for one or more requested locations
!
      NUM_LOC = 0
      DO ILOC = 1, ESD_NUM_LOC
        IF( ESD_LOC(ILOC)%COMP ) NUM_LOC = NUM_LOC + 1
      END DO
      IF( NUM_LOC .EQ. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'At least one location must be requested'
        MESSAG(2) = 'The LOCATION keyword is missing or poorly defined'
        MESSAG(3) = 'Problem with case number '
        WRITE(MESSAG(3)(27:),*) NUM_CASES
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Check on the requested times
!
      NUM_TIM = 0
      DO ITIM = 1, ESD_NUM_TIM
        IF( ESD_TIM(ITIM)%COMP ) NUM_TIM = NUM_TIM + 1
      END DO
      IF( NUM_TIM .LT. 1 ) THEN
        IERR = 10
        MESSAG(1) = 'The number of desired times is zero'
        MESSAG(2) = 'The TIMES keyword is missing or poorly defined'
        MESSAG(3) = 'Problem with case number '
        WRITE(MESSAG(3)(27:),*) NUM_CASES
      END IF
!
! *** Optional plot file
!
      IF( USE_PLOT ) THEN
!
        IF( FNPLT .EQ. ' ' ) THEN
          IERR = 11
          MESSAG(1) = 'The plot file name is missing'
          MESSAG(2) = 'Change the PLOT keyword, FILE modifier'
          MESSAG(3) = 'Problem with case number '
          WRITE(MESSAG(3)(27:),*) NUM_CASES
          CALL PRTERR( IERR, CALLER, 3 )
        END if
!
        IF( PLOT_YEAR .LT. 0 ) THEN
          IERR = 12
          MESSAG(1) = 'The plot year is invalid'
          MESSAG(2) = 'Change the PLOT keyword, YEAR modifier'
          MESSAG(3) = 'Problem with case number '
          WRITE(MESSAG(3)(27:),*) NUM_CASES
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
      END IF

      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_ESD( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine does a simple check on the ESD keyword file
!!    information to ensure that a valid problem is being posed in
!!    terms of the number of analytes, locations, and times.
!!
!!  History:
!!
!!    Carmen Arimescu  : 17 Aug 2004 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Update comments
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Terminal error encountered
!!
!!**********************************************************************
!
! *** Global variables
      USE Esd_Mod
      USE Errors_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number (0=no errors)
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'CHECK_ESD' ! Name of this subroutine
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Number of analytes in the ESD file
!
      IF( ESD_NUM_ANA .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'At least one analyte required in the ESD keyword file'
        MESSAG(2) = 'The environmental codes must be run to generate concentrations'
        CALL PRTERR( IERR, CALLER, 2 )
       END IF
!
! *** Number of locations in the ESD file
!
      IF( ESD_NUM_LOC .LT. 1 ) THEN
        IERR = 2
        MESSAG(1) = 'At least one location required in the ESD keyword file'
        MESSAG(2) = 'The environmental codes must be run to generate concentrations'
        CALL PRTERR( IERR, CALLER, 2 )
       END IF
!
! *** Number of times in the ESD file
!
      IF( ESD_NUM_TIM .LT. 1 ) THEN
        IERR = 3
        MESSAG(1) = 'At least one time required in the ESD keyword file'
        MESSAG(2) = 'The environmental codes must be run to generate concentrations'
        CALL PRTERR( IERR, CALLER, 2 )
       END IF
!
! *** Number of realizations in the ESD file
!
      IF( ESD_NREAL .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'At least one realization required in the ESD keyword file'
        MESSAG(2) = 'The environmental codes must be run to generate concentrations'
        CALL PRTERR( IERR, CALLER, 2 )
       END IF
!
! *** Check on the requested analyte
!
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_KEY( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the information from the initial keyword
!!    cards looking for problem definition problems.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Aug 2004 : Version 1.0
!!
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Terminal error encountered
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Errors_Mod
      USE Files_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR
!
! *** Local variables
!
      CHARACTER(LEN=9) :: CALLER = 'CHECK_KEY'
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Identification errors
!
      IF( USRNAM .EQ. 'Anonymous User' ) THEN
        IERR = 1
        MESSAG(1) = 'Error in the initial section of control keywords'
        MESSAG(2) = 'The user name must be entered'
        MESSAG(3) = 'Enter the name using the USER keyword'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** ESD Keyword file errors
!
      IF( FNESD .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'Error in the initial section of control keywords'
        MESSAG(2) = 'The ESD keyword file name must be entered'
        MESSAG(3) = 'Enter the name using the ESD modifier on the FILE keyword'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
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
      WRITE(*,*) '  '//TRIM(PRGNAM)//' file_name  : To define the control keyword file name'
      WRITE(*,*) '  '//TRIM(PRGNAM)//' -HELP      : To get this help message (but not execute anything)'
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE COMP_CASE( IERR )
!!*****************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine schedules computation for an extraction case.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Aug 2004 : Version 1.0
!!    Paul W. Eslinger : 27 Sep 2004 : Add plot file option
!!
!!*****************************************************************************
!
! *** Include global parameters, variables, and arrays
!
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Iden_Mod
      USE ESD_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: IERR          ! Integer error flag
!
! *** Local variables
!
      CHARACTER(LEN=9) :: CALLER = 'COMP_CASE'
      INTEGER :: ITIM          ! Time loop counter
      CHARACTER(LEN=6) :: CTMP ! Temporary output variable
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Open and read the ECDA map file
!
      IF( VERBOSE ) CALL TELLTIME( 'Calling ECDA_MAPREAD', 'SCREEN', .FALSE., IRPT )
      CALL ECDA_MAPREAD( FNMAP, IMAP, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Echo the ECDA header information to the report file
!
      IF( VERBOSE ) CALL TELLTIME( 'Calling ECDA_MAPECHO', 'SCREEN', .FALSE., IRPT )
      CALL ECDA_MAPECHO( IRPT )
!
! *** Determine which concentration file is needed and open it
!
      IF( VERBOSE ) CALL TELLTIME( 'Opening the ECDA file', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_CON( IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Save off the input data units and output them to the report file
!
      UNITS_INPUT = ECDA_UNITS(IMEDIA)
      WRITE(IRPT,1010) TRIM(UNITS_INPUT), TRIM(UNITS_OUTPUT), CONVERT_UNITS
 1010 FORMAT(/'ECDA file units = "',A,'", output units = "',A,'", conversion factor is ',1P,E12.5)
!
! *** Open the output data file
!
      IF( VERBOSE ) CALL TELLTIME( 'Opening results file', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_RESULTS( IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output data file
!
      IF( VERBOSE ) CALL TELLTIME( 'Opening plot file', 'SCREEN', .FALSE., IRPT )
      IF( USE_PLOT ) THEN
        CALL OPEN_PLOT( IERR )
        IF( IERR .NE. 0 ) THEN
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
! *** Loop over times to read and process concentration data
!
      IF( VERBOSE ) CALL TELLTIME( 'Starting to process data', 'SCREEN', .FALSE., IRPT )
      DO ITIM = 1, ESD_NUM_TIM
        IF( .NOT.ESD_TIM(ITIM)%COMP ) CYCLE
        WRITE(CTMP(1:),'(I0)') ESD_TIM(ITIM)%TIME
        IF( VERBOSE ) CALL TELLTIME( 'Reading data for year : '//TRIM(CTMP), 'SCREEN', .FALSE., IRPT )
        CALL FIND_MAX( ITIM, IERR )
        IF( IERR .NE. 0 ) THEN
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ECHO_CASE( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine outputs a case definition to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Aug 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE ECDA_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      INTEGER :: ILOC   ! Looping indices
      INTEGER :: ITIM   ! Time loop counter
      INTEGER :: IPRINT ! Print lines counter
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** File names
!
      WRITE(IRPT,1010) 'Results file         ', TRIM(FNRES)
      WRITE(IRPT,1010) 'Media Concentrations ', TRIM(FNCON(ANA_IDX))
      WRITE(IRPT,1010) 'Concentration Index  ', TRIM(FNMAP)
 1010 FORMAT(/A,' : File: ',A)
!
      WRITE(IRPT,1020) TRIM(MED_USE)
 1020 FORMAT(/'MEDIA = "',A,'"')
!
      WRITE(IRPT,1030) TRIM(ANA_USE)
 1030 FORMAT('Analyte requested = "',A,'"')
!
! *** Requested solution
!
      SELECT CASE ( SOLUTION )
        CASE( 'MEAN' )
          WRITE(IRPT,1160) 'Solution type is MEAN'
     1160 FORMAT(/A,:,1X,I0)
        CASE( 'MEDIAN' )
          WRITE(IRPT,1160) 'Solution type is MEDIAN'
        CASE( 'MAXIMUM' )
          WRITE(IRPT,1160) 'Solution type is MAXIMUM'
        CASE( 'SINGLE' )
          WRITE(IRPT,1160) 'Solution type is SINGLE using realization number', NUM_SNG
        CASE DEFAULT
      END SELECT
!
! *** Requested locations
!
      WRITE(IRPT,1100)
 1100 FORMAT(/'Location information'/ &
              'Index     ID   '/ &
              '------  --------')
      IPRINT = 0
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT.ESD_LOC(ILOC)%COMP ) CYCLE
        IPRINT = IPRINT + 1
        IF( .NOT.VERBOSE .AND. IPRINT.GT.5 ) CYCLE
        WRITE(IRPT,1500) ILOC, ESD_LOC(ILOC)%ID
 1500   FORMAT(1X,I5,'  "',A6,'"')
        IF( .NOT.VERBOSE .AND. IPRINT.EQ.5 .AND. ESD_NUM_LOC.GT.5 ) &
          WRITE(IRPT,*) '  Use the VERBOSE keyword to see more locations'
      END DO
      WRITE(IRPT,1120) NUM_LOC
 1120 FORMAT('A total of ',I0,' locations have been requested.')
!
! *** Requested times
!
      WRITE(IRPT,1130)
 1130 FORMAT(/'Time information'/ &
              'Index    Year  '/ &
              '------  ------')
      IPRINT = 0
      DO ITIM = 1, ESD_NUM_TIM
        IF( .NOT.ESD_TIM(ITIM)%COMP ) CYCLE
        IPRINT = IPRINT + 1
        IF( .NOT.VERBOSE .AND. IPRINT.GT.5 ) CYCLE
        WRITE(IRPT,1140) ITIM, ESD_TIM(ITIM)%TIME
 1140   FORMAT(1X,I5,2X,I0)
        IF( .NOT.VERBOSE .AND. IPRINT.EQ.5 .AND. ESD_NUM_TIM.GT.5 ) &
          WRITE(IRPT,*) '  Use the VERBOSE keyword to see more times'
      END DO
      WRITE(IRPT,1150) NUM_TIM
 1150 FORMAT('A total of ',I0,' times have been requested.')
!
! *** Plot file option
!
      IF( USE_PLOT ) THEN
        WRITE(IRPT,1170) PLOT_YEAR, TRIM(FNPLT)
 1170   FORMAT(/'A plot file for the year ',I0,' was requested'/ &
                'The plot is in file: ',A)
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ECHO_INIT( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles writing an echo of the initial
!!    information to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Aug 2004 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Update comments
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(//24('='),' Echo of the Problem Definition ',24('='))
!
! *** Problem Identification
!
      WRITE(IRPT,1010) USRNAM
 1010 FORMAT(/'User:  ',A)
!
! *** Files names used in the analysis
!
      WRITE(IRPT,1020) 'File for Input Keyword Data: ', TRIM(FNKEY)
      WRITE(IRPT,1020) 'File for the Report File:    ', TRIM(FNRPT)
      WRITE(IRPT,1020) 'File Name for ESD Keywords:  ', TRIM(FNESD)
 1020 FORMAT(A,1X,A)
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ESD_INIT( )
!!**********************************************************************
!!
!! Purpose:
!!
!!    This subroutine initializes all of the variables that had
!!    memory allocated by subroutine ESD_MEMORY.  This includes
!!    data from the ESD file and the associated compute-related
!!    information for this run.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Mar 2000 : Version 2.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE CONTROL_Mod
      USE ESD_Mod
      USE Ecda_Mod
      USE Errors_Mod
      USE Files_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR  ! Error flag, nonzero if no match
!
! *** Local variables
!
      INTEGER :: I
      CHARACTER(LEN=8) :: CALLER = 'ESD_INIT'
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
! *** Variables for the time slice data
!
      IF( ESD_NUM_TIM .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid value for ESD_NUM_TIM'
        MESSAG(2) = 'Suggest modifying the ESD TIME keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      DO I = 1, ESD_NUM_TIM
        ESD_TIM(I)%TIME = 0
        ESD_TIM(I)%COMP = .FALSE.
      END DO
!
! *** Variables for the location data
!
      IF( ESD_NUM_LOC .LT. 1 ) THEN
        IERR = 2
        MESSAG(1) = 'Invalid value for ESD_NUM_LOC'
        MESSAG(2) = 'Suggest modifying the ESD LOCATION keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      DO I = 1, ESD_NUM_LOC
        ESD_LOC(I)%ID = ' '
        ESD_LOC(I)%NAME = ' '
        ESD_LOC(I)%COMP = .FALSE.
        ESD_LOC(I)%EASTING  = 0.0
        ESD_LOC(I)%NORTHING = 0.0
      END DO
!
! *** Variables for the analyte data
!
      IF( ESD_NUM_ANA .LT. 1 ) THEN
        IERR = 3
        MESSAG(1) = 'Invalid value for ESD_NUM_ANA'
        MESSAG(2) = 'Suggest modifying the ESD ANALYTE keywords'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      DO I = 1, ESD_NUM_ANA
        ESD_ANA(I)%ID = ' '
        ESD_ANA(I)%NAME = ' '
        ESD_ANA(I)%COMP = .FALSE.
      END DO
!
! *** Vector of file names
!
      FNCON = ' '
!
      ICON = 21
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ESD_MEMORY( IERR )
!!**********************************************************************
!!
!! Purpose:
!!
!!    This subroutine allocates memory for the data read from the
!!    environmental settings definition file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Mar 2000 : Version 2.0
!!    Carmen Arimescu  : 24 Oct 2002 : SAC Rev.1
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
      USE Files_Mod
      USE ECDA_Mod
      USE ESD_Mod
      USE Control_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR  ! Error flag, nonzero if no error
!
! *** Local variables
!
      INTEGER :: IERA  ! Error status variable from the allocate action
      CHARACTER(LEN=10) :: CALLER = 'ESD_MEMORY' ! Name of this routine
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
! *** Check on the number of analytes
!
      IF( ESD_NUM_ANA .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'At least 1 analyte required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ESD_ANA(ESD_NUM_ANA), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error allocating memory for ESD_ANA'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( FNCON(ESD_NUM_ANA), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Error allocating memory for FNCON'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check on the number of locations
!
      IF( ESD_NUM_LOC .LT. 1 ) THEN
        IERR = 3
        MESSAG(1) = 'At least 1 location required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ESD_LOC(ESD_NUM_LOC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating memory for ESD_LOC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check on the number of times
!
      IF( ESD_NUM_TIM .LT. 1 ) THEN
        IERR = 5
        MESSAG(1) = 'At least 1 time required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ESD_TIM(ESD_NUM_TIM), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating memory for ESD_TIM'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! Concentration work vector
!
      ALLOCATE( CVEC(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) = 'Error allocating memory for CVEC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( WVEC(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating memory for WVEC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE FIND_MAX( ITIM, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the concentration data the ECDA file for
!!    all locations for a single time.  The maximum solution value over
!!    all locations is output to the results file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 18 Aug 2004 : Version 1.0
!!    Paul W. Eslinger : 27 Sep 2004 : Add optional plot file
!!    Paul W. Eslinger :  5 Jun 2007 : Update comments
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Control_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE Files_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Input time index
      INTEGER :: IERR             ! Output error flag
!
! *** Local variables
!
      CHARACTER(LEN=8) :: CALLER = 'FIND_MAX' ! Name of this subroutine
      REAL :: MAX_VALUE          ! Maximum value
      REAL :: TMP_VALUE          ! Value to compare to the maximum
      REAL :: MAX_EASTING        ! Easting of the maximum
      REAL :: MAX_NORTHING       ! Northing of the maximum
      CHARACTER(LEN=6) :: MAX_ID ! ID where maximum occurred
      REAL :: XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD
      INTEGER :: ILOC ! Location loop counter
      INTEGER :: IDX ! Time loop counter

      INTEGER :: TIME              ! Calendar year for concentration data
      CHARACTER(LEN=6) :: LOC_ID   ! Location ID
      CHARACTER(LEN=4) :: MED_ID   ! Media ID
      INTEGER :: IREL ! Realization looping index
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Error check on the time slice
!
      IF( ITIM.LT.1 .OR. ITIM.GT.ESD_NUM_TIM ) THEN
        IERR = 1
        MESSAG(1) = 'Bad time index for reading concentration data'
        MESSAG(2) = 'Valid range is 1 to '
        WRITE(MESSAG(2)(22:),*) ESD_NUM_TIM
        MESSAG(3) = 'Value entered was '
        WRITE(MESSAG(3)(20:),*) ITIM
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Loop over all requested locations and find the maximum
!
      MAX_VALUE    = -HUGE(1.0)
      MAX_EASTING  = 0.0
      MAX_NORTHING = 0.0
      MAX_ID       = 'XXXXXX'
!
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT.ESD_LOC(ILOC)%COMP ) CYCLE
!
!       Check whether media exists at this location
        CALL ECDA_RECNO_INDEX( ITIM, ILOC, IMEDIA, IDX, IERR )
        IF( IDX.LT.1 .OR. IERR.NE.0 ) CYCLE
!
!       Read the media concentration
        CALL ECDA_READ( IDX, TIME, LOC_ID, MED_ID, CVEC, ESD_NREAL, ICON, IERR )
        IF( IERR .EQ. 0 ) THEN
          DO IREL = 1, ESD_NREAL
            CVEC(IREL) = CVEC(IREL) * CONVERT_UNITS
          END DO
        ELSE
          IERR = 2
          MESSAG(1) = 'Error reading concentration data'
          MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Analyte =  '//ANA_USE
          MESSAG(4) = 'Time = '
          WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
!
!       Extract solution
        SELECT CASE ( SOLUTION )
          CASE( 'MEAN' )
            CALL USTAT( CVEC, ESD_NREAL, WVEC, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, IERR )
            TMP_VALUE = XAVG
            IF( TMP_VALUE .GT. MAX_VALUE ) THEN
              MAX_VALUE    = TMP_VALUE
              MAX_ID       = ESD_LOC(ILOC)%ID
              MAX_EASTING  = ESD_LOC(ILOC)%EASTING
              MAX_NORTHING = ESD_LOC(ILOC)%NORTHING
            END IF
          CASE( 'MEDIAN' )
            CALL USTAT( CVEC, ESD_NREAL, WVEC, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, IERR )
            TMP_VALUE = XMED
            IF( TMP_VALUE .GT. MAX_VALUE ) THEN
              MAX_VALUE    = TMP_VALUE
              MAX_ID       = ESD_LOC(ILOC)%ID
              MAX_EASTING  = ESD_LOC(ILOC)%EASTING
              MAX_NORTHING = ESD_LOC(ILOC)%NORTHING
            END IF
          CASE( 'MAXIMUM' )
            CALL USTAT( CVEC, ESD_NREAL, WVEC, XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD, IERR )
            TMP_VALUE = XMAX
            IF( TMP_VALUE .GT. MAX_VALUE ) THEN
              MAX_VALUE    = TMP_VALUE
              MAX_ID       = ESD_LOC(ILOC)%ID
              MAX_EASTING  = ESD_LOC(ILOC)%EASTING
              MAX_NORTHING = ESD_LOC(ILOC)%NORTHING
            END IF
          CASE( 'SINGLE' )
            TMP_VALUE = CVEC( NUM_SNG )
            IF( TMP_VALUE .GT. MAX_VALUE ) THEN
              MAX_VALUE    = TMP_VALUE
              MAX_ID       = ESD_LOC(ILOC)%ID
              MAX_EASTING  = ESD_LOC(ILOC)%EASTING
              MAX_NORTHING = ESD_LOC(ILOC)%NORTHING
            END IF
          CASE DEFAULT
            IERR = 3
            MESSAG(1) = 'Did not find the right solution'
            MESSAG(2) = 'Solution in question is '//SOLUTION
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
!
        END SELECT
!
!       Optional plot file output
        IF( USE_PLOT .AND. PLOT_YEAR.EQ.ESD_TIM(ITIM)%TIME ) THEN
          WRITE(IPLT,1020) TRIM(ESD_LOC(ILOC)%ID), ESD_LOC(ILOC)%EASTING, ESD_LOC(ILOC)%NORTHING, TMP_VALUE
 1020     FORMAT('"',A,'",',1P,E12.5,',',E12.5,',',E12.5)
        END IF
!
      END DO
!
! *** Output the final result
!
      WRITE(IRES,1010) ESD_TIM(ITIM)%TIME, MAX_VALUE, TRIM(MAX_ID), MAX_EASTING, MAX_NORTHING
 1010 FORMAT(I0,',',1P,E12.5,',"',A,'",',E12.5,',',E12.5)
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
!!    This subroutine is coded for a PC using a FORTRAN 90 compiler.
!!
!!  History:
!!
!!    Paul W. Eslinger :  8 Mar 2000 : Version 1.0
!!    Carmen Arimescu  : 17 Aug 2004 : Last update
!!    Paul W. Eslinger :  5 Jun 2007 : Last update
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
!
      USRNAM = 'Anonymous User'
!
! *** Program name and version number
!
      PRGNAM = 'HighMedia'
      PRGVER = '4.00.001'
!
! *** Program date (DD MMM YYYYY)
!
      PRGDAT = ' 5 Jun 2007'
!
! *** Get the date and time from the operating system
!
      CALL DATE_AND_TIME( SDATE, STIME )
!
! *** System time in the (CHARACTER) form hh:mm:ss
!
      SYSTIM = STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
!
! *** System date in the (CHARACTER) form mm-dd-yyyy
!
      SYSDAT = SDATE(5:6)//'/'//SDATE(7:8)//'/'//SDATE(1:4)
!
! *** Run identification number
!
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
!!    This subroutine initializes global variables
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Aug 2004 : Version 1.0
!!    Paul W. Eslinger : 27 Sep 2004 : Add output plot option
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
!
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
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
! *** ESD Keyword file
!
      IESD  = 11
      FNESD = ' '
!
! *** Optional plot file
!
      IPLT  = 16
!
! *** Number of cases
!
      NUM_CASES = 0
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE INIT_CASE( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine initializes variables specific to a case.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Aug 2004 : Version 1.0
!!    Paul W. Eslinger : 27 Sep 2004 : Add output plot option
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
!
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ESD_Mod
!
      IMPLICIT NONE
!
! *** Local variables
!
      INTEGER :: I ! Looping index
!
!---- Executable code --------------------------------------------
!
! *** Output results file
      FNRES = ' '
!
! *** Problem title, solution and analyte type
      PTITLE = ' '
!
! *** Counter type information
      NUM_LOC = 0
      NUM_TIM = 0
!
! *** Solution type information
      SOLUTION = ' '
      NUM_SNG = 0
!
      ANA_USE = ' '
      MED_USE = ' '
!
! *** Units data
      CONVERT_UNITS = -1.0E30
      UNITS_OUTPUT  = ' '
!
! *** Variables for the time slice data
      DO I = 1, ESD_NUM_TIM
        ESD_TIM(I)%COMP = .FALSE.
      END DO
!
! *** Variables for the location data
      DO I = 1, ESD_NUM_LOC
        ESD_LOC(I)%COMP = .FALSE.
      END DO
!
! *** Variables for the analyte data
      DO I = 1, ESD_NUM_ANA
        ESD_ANA(I)%COMP = .FALSE.
      END DO
!
! *** Optional Plot file
      USE_PLOT = .FALSE.
      PLOT_YEAR = -1
      CLOSE(IPLT)
!
      FNPLT = ' '
!
! *** Close the files
      CLOSE(ICON)
      CLOSE(IRES)
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE KEY_ESD_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and the environmental settings
!!    keyword control information required to set variable dimensions.
!!    It also rewinds the keyword file before exiting.
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
!!  History:
!!
!!    Carmen Arimescu : 17 Aug 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Rdblk_Mod
      USE ESD_Mod
      USE Errors_Mod
      USE Files_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR
!
! *** Local variable
!
      CHARACTER(LEN=LENCRD) :: TITLE
      LOGICAL, EXTERNAL :: CEXIST
      INTEGER :: IPERR
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_1'
!
!---- First executable code --------------------------------------------
!
! *** Initialize for RDBLK
!
      ILINE = 0
      IPERR = IRPT
!
! *** Top of loop on reading keyword cards
!
   10 CONTINUE
!
      CALL RDBLK( IESD, IPERR, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'ANALYTE' )  ! ===> ANALYTE keyword
          ESD_NUM_ANA = ESD_NUM_ANA + 1
!
        CASE( 'END' )      ! ===> END keyword
          REWIND(IESD)
          RETURN
!
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
          ESD_NUM_LOC = ESD_NUM_LOC + 1
!
        CASE( 'REALIZAT' ) ! ===> REALIZATION Keyword
         ESD_NREAL = VALUE(1)
!
        CASE( 'TIMES' )    ! ===> TIMES Keyword
          ESD_NUM_TIM = ESD_NUM_TIM + NVALUE
!
        CASE( 'TITLE' )    ! ===> TITLE Keyword
          ESD_TITLE = QUOTE(1)
!
        CASE DEFAULT       ! Ignore other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit the subroutine routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE
!
      SUBROUTINE KEY_ESD_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading the environmental settings
!!    keyword control information.  Some error checking is done on
!!    the existance of data items.  No error checking is done on
!!    validity of entered data.
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
!!  History:
!!
!!    Carmen Arimescu : 17 Aug 2004 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Update comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE ECDA_Mod
      USE Control_Mod
!
      IMPLICIT NONE
!
! *** User defined variables
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
!
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_2' ! Name of this subroutine
      REAL :: RTMP
      INTEGER :: IDX, IDXA
      INTEGER :: ITMP
      CHARACTER(LEN=LENCRD) :: TITLE
!
      CHARACTER(LEN=LENQQQ) :: TMP_ID, TMP_ANA
      CHARACTER(LEN=72) :: TMP_NAME
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
!
      ILINE = 0
!
! *** Reinitialize counters
!
      ESD_NUM_ANA = 0 ! Number of analytes in the ESD file
      ESD_NUM_LOC = 0 ! Number of locations in the ESD file
      ESD_NUM_TIM = 0 ! Number of times in the ESD file
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!----------------------------------------------------------------------C
!
   10 CONTINUE
!
      CALL RDBLK( IESD, IRPT, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'ANALYTE' ) ! ===> ANALYTE keyword
          ESD_NUM_ANA = ESD_NUM_ANA + 1
!
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%ID = TMP_ID
            ELSE
              IERR = 1
              MESSAG(1) = 'Analyte ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 2
            MESSAG(1) = 'ID modifier not entered on the ANALYTE keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('NAME    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%NAME = TMP_NAME
            ELSE
              IERR = 3
              MESSAG(1) = 'Analyte NAME modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 4
            MESSAG(1) = 'NAME modifier not entered on the ANALYTE keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('TYPE    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%TYPE = TMP_NAME
            ELSE
              IERR = 5
              MESSAG(1) = 'Analyte TYPE modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 6
            MESSAG(1) = 'TYPE modifier not entered on the ANALYTE keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
        CASE( 'END' )  ! ===> END keyword
          CLOSE( IESD )
          RETURN
!
        CASE( 'FILE' ) ! ===> FILE keyword
!
          IF( CEXIST('I_ECDA') ) THEN
!
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 7
                MESSAG(1) = 'NAME modifier missing quote string on FILE (C_ECDA) keyword'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
!              WRITE(*,*) 'The ECDA map file name is ', TRIM(TMP_ID)
              FNMAP = TMP_ID
            ELSE
              IERR = 8
              MESSAG(1) = 'NAME modifier not found on FILE card for type C_ECDA'
              MESSAG(4) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('C_ECDA') ) THEN
!
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 7
                MESSAG(1) = 'NAME modifier missing quote string on FILE (C_ECDA) keyword'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
            ELSE
              IERR = 9
              MESSAG(1) = 'NAME modifier not found on FILE card for type C_ECDA'
              MESSAG(4) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!
            IF( CEXIST('ANALYTE ') ) THEN
              CALL NXTQOT( IDX, TMP_ANA )
              CALL MATCH_ANA( TMP_ANA, IDXA )
!              write(*,*) 'Found analyte '//trim(TMP_ANA)//' with index ',IDXA,' and file '//TRIM(tmp_id) !PWE@
              IF( IDXA .GT. 0 ) THEN
                FNCON(IDXA) = TMP_ID
              ELSE
                IERR = 10
                MESSAG(1) = 'Error for the ANALYTE modifier on FILE C_ECDA keyword'
                MESSAG(2) = 'No quote string was entered with the analyte ID'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            ELSE
              IERR = 11
              MESSAG(1) = 'ANALYTE modifier not found on FILE card for type C_ECDA'
              MESSAG(4) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!
          END IF
!
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
!
          ESD_NUM_LOC = ESD_NUM_LOC + 1
!
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%ID = TMP_ID
            ELSE
              IERR = 12
              MESSAG(1) = 'Location ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 13
            MESSAG(1) = 'ID modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('NAME    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%NAME = TMP_NAME
            ELSE
              IERR = 14
              MESSAG(1) = 'Location NAME modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 15
            MESSAG(1) = 'NAME modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('EASTING ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%EASTING = RTMP
            ELSE
              IERR = 16
              MESSAG(1) = 'Location EASTING modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 17
            MESSAG(1) = 'EASTING modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('NORTHING') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
            ESD_LOC(ESD_NUM_LOC)%NORTHING = RTMP
            ELSE
              IERR = 18
              MESSAG(1) = 'Location NORTHING modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 19
            MESSAG(1) = 'NORTHING modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
        CASE( 'TIMES' ) ! ===> TIMES Keyword
          IF( NVALUE .GT. 0 ) THEN
            DO ITMP = 1, NVALUE
              ESD_NUM_TIM = ESD_NUM_TIM + 1
              ESD_TIM(ESD_NUM_TIM)%TIME = VALUE(ITMP)
            END DO
          ELSE
            IERR = 22
            MESSAG(1) = 'No numeric values found on the TIMES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
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
!
      SUBROUTINE MATCH_ANA( LABEL, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between an analyte name
!!    in the ESD module and an analyte label.  If found, the index
!!    identifies the storage location for the analyte data.  If not
!!    found, a negative index is returned.
!!
!!  Call List:
!!
!!    LABEL : Input  - Character - analyte label
!!    IDX   : Output - Integer   - index for data associated with LABEL
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Mar 2000 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE ESD_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IDX
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
!
! *** Local variables
!
      INTEGER :: IANA ! Looping variable
!
! *** Look for a match on the analyte names
!
      IDX = -1
      DO IANA = 1, ESD_NUM_ANA
        IF( ESD_ANA(IANA)%ID .EQ. LABEL ) THEN
          IDX = IANA
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_LOC( LABEL, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a location name
!!    in the ESD module and a location label.  If found, the index
!!    identifies the storage location for the location data.  If not
!!    found, a negative index is returned.
!!
!!  Call List:
!!
!!    LABEL : Input  - Character - location label
!!    IDX   : Output - Integer   - index for data associated with LABEL
!!
!!  History:
!!
!!    Paul W. Eslinger : 15 Mar 2000 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE ESD_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IDX
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
!
! *** Local variables
!
      INTEGER :: ILOC ! Looping variable
!
! *** Look for a match on the location names
!
      IDX = -1
      DO ILOC = 1, ESD_NUM_LOC
        IF( ESD_LOC(ILOC)%ID .EQ. LABEL ) THEN
          IDX = ILOC
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_TIM( TIME, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a time in the
!!    ESD module and an input time.  If found, the index identifies the
!!    storage location for the time data.  If not found, a negative
!!    index is returned.
!!
!!  Call List:
!!
!!    TIMEL : Input  - Integer - Time
!!    IDX   : Output - Integer - index for data associated with TIME
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Mar 2000 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE ESD_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IDX
      INTEGER :: TIME
!
! *** Local variables
!
      INTEGER :: ITIM ! Looping variable
!
! *** Look for a match on the analyte names
!
      IDX = -1
      DO ITIM = 1, ESD_NUM_TIM
        IF( ESD_TIM(ITIM)%TIME .EQ. TIME ) THEN
          IDX = ITIM
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE OPEN_CON( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the concentration data file and reads
!!    the header information.
!!
!!  History:
!!
!!    Carmen Arimescu : 17 Aug 2004 : Version 1.0
!!
!!  Call List Variables:
!!
!!    IERR   : Output integer error flag
!!             0 = No errors
!!            >0 = Terminal error
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Ecda_Mod
      USE Control_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR
!
! *** Local variables
!
      CHARACTER(LEN=8) :: CALLER = 'OPEN_CON'
      LOGICAL :: THERE
!
!---- First executable code --------------------------------------------
!
! *** Only look at analyte selected for this scenario
!
      CALL MATCH_ANA( ANA_USE, ANA_IDX )
      IF( ANA_IDX .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'The requested analyte was not found in the ESD information'
        MESSAG(2) = 'Analyte ID: '//ANA_USE
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      INQUIRE( FILE=FNCON(ANA_IDX), EXIST=THERE )
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested ECDA concentration file was not found'
        MESSAG(2) = 'Analyte ID: '//ANA_USE
        MESSAG(3) = 'File: '//TRIM(FNCON(ANA_IDX))
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Open the concentration data file
!
      CALL ECDA_OPEN( FNCON(ANA_IDX), ICON, IERR  )
      IF ( IERR .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the ECDA concentration file'
        MESSAG(2) = 'Analyte ID: ' // ANA_USE
        MESSAG(3) = 'File: ' // TRIM(FNCON(ANA_IDX))
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE OPEN_ESD( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the ESD keyword file for reading keywords.
!!
!!  History:
!!
!!    Carmen Arimescu : 17 Aug 2004 : Version 1.0
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in locating or opening the input keyword file
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Errors_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR
      INTEGER :: IERF ! Status variable for open statement
!
! *** Local variables
!
      LOGICAL THERE
      CHARACTER(LEN=8) :: CALLER = 'OPEN_ESD'
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the requested file exists
!
      IF( FNESD .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The ESD keyword file name is blank'
        MESSAG(2) = 'Change the file name on the FILE keyword, ESD modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      INQUIRE(FILE=FNESD,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested ESD keyword file was not found'
        MESSAG(2) = 'Change the file name on the FILE keyword, ESD modifier'
        MESSAG(3) = 'File: '//TRIM(FNESD)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IESD,FILE=FNESD,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the ESD keyword file'
        MESSAG(2) = 'File name entered on the FILE keyword, ESD modifier'
        MESSAG(3) = 'File: '//TRIM(FNESD)
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
!!    This subroutine performs two actions for the input keyword file:
!!      1. Obtain the file name
!!         a) First check the command line to see if the file
!!            name was provided or,
!!         b) query the user for the file name if it was not
!!            entered on the command line.
!!      2. Open the file for use by keyword reading subroutines
!!
!!  History:
!!    Carmen Arimescu  : 17 Aug 2004 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Udate to TIIA
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
      USE Files_Mod
      USE Iden_Mod
!
      IMPLICIT NONE
!
! *** External functions
      INTEGER, EXTERNAL :: NARGS ! Lahey utility subroutine
!
! *** Call list variables
      INTEGER :: IERR ! Error number (0=no errors)
!
! *** Local variables
!
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
      SUBROUTINE OPEN_PLOT( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain the
!!    optional plot values.
!!
!!  History:
!!
!!    Paul W. Eslinger : 27 Sep 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Errors_Mod
      USE Iden_Mod
      USE Control_Mod
      USE ECDA_mOD
      USE ESD_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR
!
! *** Local variables
!
      INTEGER :: IERF ! Status variable for open statement
      CHARACTER(LEN=9) :: CALLER = 'OPEN_PLOT'
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the file name was entered
!
      IF( FNPLT .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The output plot data file name is blank'
        MESSAG(2) = 'Change the file name on the PLOT keyword, FILE modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IPLT,FILE=FNPLT,STATUS='UNKNOWN',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the plot data file'
        MESSAG(2) = 'File name entered on the PLOT keyword, FILE modifier'
        MESSAG(3) = 'File: '//TRIM(FNPLT)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Write a few identification items to the file
!
      WRITE(IRES,1030) '"Location ID","Easting","Northing","Value"'
 1030 FORMAT(A)
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
!!    Carmen Arimescu : 17 Aug 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Errors_Mod
      USE Iden_Mod
      USE Control_Mod
      USE ECDA_mOD
      USE ESD_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR
!
! *** Local variables
!
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
      WRITE(IRES,1000) 'File Name:',    TRIM(FNCON(ANA_IDX))
!
      SELECT CASE ( SOLUTION )
!
        CASE( 'MEAN' )
          WRITE(IRES,1000) 'Solution type:","MEAN'
 1160     FORMAT('"',A,'"',:,',',I0)
!
        CASE( 'MEDIAN' )
          WRITE(IRES,1000) 'Solution type:","MEDIAN'
!
        CASE( 'MAXIMUM' )
          WRITE(IRES,1000) 'Solution type: ","MAXIMUM'
!
        CASE( 'SINGLE' )
          IF( NUM_SNG.LT.1 .OR. NUM_SNG.GT.ESD_NREAL ) THEN
            IERR = 2
            MESSAG(1) = 'The desired solution is a single realization'
            MESSAG(2) = 'Change the REALIZAT keyword to enter a realization number'
            MESSAG(3) = 'Example: REALIZAT SINGLE=5'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
          WRITE(IRES,1160) 'Solution type:","SINGLE',NUM_SNG
!
        CASE DEFAULT
          IERR = 3
          MESSAG(1) = 'The desired solution must be defined with the REALIZAT keyword'
          MESSAG(2) = 'Modifiers include MEAN, MEDIAN, MAXIMUM, and SINGLE=N'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
!
      END SELECT
!
      WRITE(IRES,1000) 'Analyte ID:', TRIM(ANA_USE)
      WRITE(IRES,1000) 'Media ID:',   TRIM(MED_USE)
!
 1000 FORMAT('"',A,'"',:,',"',A,'"')
!
      WRITE(IRES,1030) '"Year","Solution","Location ID","Easting","Northing"'
 1030 FORMAT(A)
!
      RETURN
      END SUBROUTINE

      SUBROUTINE READ_CASE( FINISHED, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and the keyword control
!!    information required to define a specific case
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
!!    Paul W. Eslinger : 17 Aug 2004 : Version 1.0
!!    Paul W. Eslinger : 27 Sep 2004 : Add output plot option
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ESD_Mod
!
      IMPLICIT NONE
!
! *** User defined external functions
!
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
!
      LOGICAL :: FINISHED ! Finished processing cases
      INTEGER :: IERR     ! Error number flag
!
! *** Local variables
!
      INTEGER :: IDX, IDXL, IDXT, ITMP   ! Temporary indeices
      CHARACTER :: TITLE*(LENCRD)
      CHARACTER(LEN=9) :: CALLER = 'READ_CASE'
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary file name
      REAL :: RTMP
      INTEGER :: ILOC ! Looping indices
      INTEGER :: ITIM ! Time loop counter
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
      NUM_CASES = NUM_CASES + 1
      FINISHED = .FALSE.
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
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
!----------------------------------------------------------------------------
        CASE( 'ANALYTE' ) ! ===> ANALYTE Keyword
          IF( NQUOTE .GT. 0 ) ANA_USE = QUOTE(1)
!
!----------------------------------------------------------------------------
        CASE( 'END' ) ! ===> END keyword
          CLOSE( IKEY )
          FINISHED = .TRUE.
          RETURN
!
!----------------------------------------------------------------------------
        CASE( 'ENDCASE' ) ! ===> ENDCASE keyword
          RETURN
!
!----------------------------------------------------------------------------
        CASE( 'FILE' ) ! ===> FILE keyword
!
          IF( CEXIST('RESULTS') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 3
              MESSAG(1) = 'The RESULTS modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Problem with case number '
              WRITE(MESSAG(3)(27:),*) NUM_CASES
              CALL PRTERR( IERR, CALLER, 3 )
            ELSE
              FNRES = TMP_NAME
            END IF
          END IF
!
!----------------------------------------------------------------------------
        CASE( 'PLOT' ) ! ===> PLOT keyword
!
          IF( CEXIST('FILE') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 3
              MESSAG(1) = 'The FILE modifier on the PLOT keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Problem with case number '
              WRITE(MESSAG(3)(27:),*) NUM_CASES
              CALL PRTERR( IERR, CALLER, 3 )
            ELSE
              FNPLT = TMP_NAME
            END IF
          END IF
!
          IF( CEXIST('YEAR') )  THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              PLOT_YEAR = RTMP
            ELSE
              IERR = 4
              MESSAG(1) = 'YEAR modifier missing value on the PLOT keyword'
              MESSAG(2) = 'Problem with case number '
              WRITE(MESSAG(2)(27:),*) NUM_CASES
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          USE_PLOT = .TRUE.
!
!----------------------------------------------------------------------------
        CASE( 'REALIZAT' ) ! ===> REALIZATION Keyword
!
!         Options are { MEAN | MEDIAN | SINGLE=NUM_SNG}
          IF( CEXIST('MEDIAN') )  SOLUTION = 'MEDIAN'
          IF( CEXIST('MEAN') )    SOLUTION = 'MEAN'
          IF( CEXIST('MAXIMUM') ) SOLUTION = 'MAXIMUM'
          IF( CEXIST('SINGLE') )  THEN
            SOLUTION = 'SINGLE'
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              NUM_SNG = RTMP
            ELSE
              IERR = 4
              MESSAG(1) = 'REALIZAT SINGLE modifier missing value'
              MESSAG(2) = 'Problem with case number '
              WRITE(MESSAG(2)(27:),*) NUM_CASES
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
!----------------------------------------------------------------------------
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
!
          IF( .NOT. (CEXIST('ALL') .OR. CEXIST('LIST')) ) THEN
            IERR = 1
            MESSAG(1) = 'The LOCATION keyword did not have the ALL'
            MESSAG(2) = 'or LIST modifier on it.'
            MESSAG(3) = 'Problem with case number '
            WRITE(MESSAG(3)(27:),*) NUM_CASES
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
          IF( CEXIST('ALL') ) THEN
            DO ILOC = 1, ESD_NUM_LOC
              ESD_LOC(ILOC)%COMP = .TRUE.
            END DO
          END IF
!
          IF( CEXIST('LIST') .AND. NQUOTE.GT.0 ) THEN
            DO ILOC = 1, NQUOTE
              TMP_NAME = QUOTE(ILOC)
              CALL MATCH_LOC( TMP_NAME, IDXL )
              IF( IDXL .GT. 0 ) THEN
                ESD_LOC(IDXL)%COMP = .TRUE.
              ELSE
                IERR = 2
                MESSAG(1) = 'The LOCATION keyword had an invalid ID'
                MESSAG(2) = 'ID: "' // TMP_NAME(1:6) // '"'
                MESSAG(3) = 'Problem with case number '
                WRITE(MESSAG(3)(27:),*) NUM_CASES
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END DO
          END IF
!
!----------------------------------------------------------------------------
        CASE( 'TIMES' ) ! ===> TIME keyword
!
          IF( .NOT. (CEXIST('ALL') .OR. CEXIST('LIST')) ) THEN
            IERR = 1
            MESSAG(1) = 'The TIMES keyword did not have the ALL'
            MESSAG(2) = 'or LIST modifier on it.'
            MESSAG(3) = 'Problem with case number '
            WRITE(MESSAG(3)(27:),*) NUM_CASES
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
          IF( CEXIST('ALL') ) THEN
            DO ITIM = 1, ESD_NUM_TIM
              ESD_TIM(ITIM)%COMP = .TRUE.
            END DO
          END IF
!
          IF( CEXIST('LIST') ) THEN
            DO ITIM = 1, NVALUE
!              RTMP = VALUE(ITIM)
              ITMP = VALUE(ITIM)
              CALL MATCH_TIM( ITMP, IDXT )
              IF( IDXT .GT. 0 ) THEN
                ESD_TIM(IDXT)%COMP = .TRUE.
              ELSE
                IERR = 3
                MESSAG(1) = 'Time requested is not in the master list'
                MESSAG(2) = 'Invalid time is '
                WRITE(MESSAG(2)(17:),*) ITMP
                MESSAG(3) = 'Problem with case number '
                WRITE(MESSAG(3)(27:),*) NUM_CASES
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
             END IF
            END DO
          END IF
!
!----------------------------------------------------------------------------
        CASE( 'MEDIA' ) ! ===> MEDIA Keyword
          IF( NQUOTE .GT. 0 ) MED_USE = QUOTE(1)
!
!----------------------------------------------------------------------------
        CASE( 'TITLE' ) ! ===> TITLE Keyword
          PTITLE = QUOTE(1)
!
!----------------------------------------------------------------------------
        CASE( 'UNITS' ) ! ===> USER Keyword
!
          IF( CEXIST('OUTPUT') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              UNITS_OUTPUT = TMP_NAME
            ELSE
              IERR = 5
              MESSAG(1) = 'UNITS OUTPUT modifier missing quote string'
              MESSAG(2) = 'Problem with case number '
              WRITE(MESSAG(2)(27:),*) NUM_CASES
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 6
            MESSAG(1) = 'OUTPUT modifier not entered on the UNITS keyword'
            MESSAG(2) = 'Problem with case number '
            WRITE(MESSAG(2)(27:),*) NUM_CASES
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('FACTOR') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              CONVERT_UNITS = RTMP
            ELSE
              IERR = 7
              MESSAG(1) = 'UNITS FACTOR modifier missing value'
              MESSAG(2) = 'Problem with case number '
              WRITE(MESSAG(2)(27:),*) NUM_CASES
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 8
            MESSAG(1) = 'FACTOR modifier not entered on the UNITS keyword'
            MESSAG(2) = 'Problem with case number '
            WRITE(MESSAG(2)(27:),*) NUM_CASES
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
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
!!    Carmen Arimescu  : 17 Aug 2004 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Move REPORT to another subroutine
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
      IMPLICIT NONE
!
! *** User defined external functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'READ_KEY' ! Name of this subroutine
      INTEGER :: IDX   ! Temporary index
      CHARACTER :: TITLE*(LENCRD)
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary file name
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
        MESSAG(1) = 'Error in lower level RDBLK routine'
        MESSAG(2) = 'Examine the standard output messages for details'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
!----------------------------------------------------------------------------
        CASE( 'END' ) ! ===> END keyword (not expected in the INIT section)
          IERR = 1
          MESSAG(1) = 'Unexpected END keyword encountered'
          MESSAG(2) = 'An ENDINIT keyword was expected'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
!
!----------------------------------------------------------------------------
        CASE( 'ENDINIT' ) ! ===> ENDINIT keyword
          RETURN
!
!----------------------------------------------------------------------------
        CASE( 'FILE' ) ! ===> FILE keyword
!
          IF( CEXIST('ESD') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 2
              MESSAG(1) = 'The ESD modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNESD = TMP_NAME
            END IF
          END IF
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
!!  Auxiliary Routines:
!!
!!    PRTERR, and all RDBLK related routines
!!
!!  History:
!!
!!    Paul W. Eslinger :  4 Jun 2007 : Original source
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
!
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      CHARACTER(LEN=16) :: CALLER = 'READ_KEYS_REPORT' ! Name of this subroutine
      CHARACTER(LEN=LENCRD) :: TITLE ! Temporary input line from RDBLK
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
!!    Paul W. Eslinger : 31 May 2007 : Original code
!!
!!**********************************************************************
!
! *** Global variables
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*) :: MESSAGE ! Character string to write
      CHARACTER(LEN=*) :: PLACE   ! Place to output the mesage
      LOGICAL :: LDATE            ! Only print date if LDATE=.TRUE.
      INTEGER :: IRPT             ! Unit number for output file
!
! *** External functions
!
      LOGICAL, EXTERNAL :: STRCOMP
!
! *** Local variables
!
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
!
      SUBROUTINE USTAT( X, N, UWORK, XMIN, XV05, XV10, XV25, XMED, &
        XV75, XV90, XV95, XMAX, XAVG, XSTD, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine will compute some summary statistics for
!!    the N values in the vector X.
!!
!!  Change History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!
!!  Auxiliary Routines Required:
!!
!!    SORT : A subroutine to do a sort on a REAL vector
!!
!!  Variable Descriptios:
!!
!!    Inputs
!!    ----------------------------------------------------
!!    X    : Vector of input values.
!!    N    : Number of values in the vector X.
!!    UWORK : A work vector with the same dimension as X.
!!
!!    Outputs
!!    ----------------------------------------------------
!!    XMIN : Minimum value in X.
!!    XV05 :  5% percentile of the values in X.
!!    XV10 : 10% percentile of the values in X.
!!    XV25 : 25% percentile of the values in X.
!!    XMED : 50% percentile of the values in X (median)
!!    XV75 : 75% percentile of the values in X.
!!    XV90 : 90% percentile of the values in X.
!!    XV95 : 95% percentile of the values in X.
!!    XMAX : Maximum value in X.
!!    XAVG : Average (mean) value in X.
!!    XSTD : Standard deviation of the values in X.
!!    IERR : Error flag.
!!             0 : Normal termination
!!            >0 : Error encountered
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      REAL, DIMENSION(*), INTENT(IN) :: X
      INTEGER :: N
      REAL, DIMENSION(N) :: UWORK
      REAL :: XMIN, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XMAX, XAVG, XSTD
      INTEGER :: IERR
!
! *** Local variables
!
      REAL(KIND=8) :: VSUM
      INTEGER :: IP05, IP10, IP25, IP75, IP90, IP95
      CHARACTER(LEN=5) :: CALLER = 'USTAT' ! Name of this routine
      INTEGER :: I ! Local looping variable
      INTEGER :: KFLAG ! Local sort type selection flag
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize and check for error on entry
!
      XMIN = 0.0
      XV05 = 0.0
      XV10 = 0.0
      XV25 = 0.0
      XMED = 0.0
      XV75 = 0.0
      XV90 = 0.0
      XV95 = 0.0
      XMAX = 0.0
!
      XAVG = 0.0
      XSTD = 0.0
!
! *** Error check on the sample size
!
      IERR = 0
      IF( N .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'Sample size less than 1'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Sort the data to find the median efficiently
!     Use the WORK array for all calculations
!
      DO I = 1, N
        UWORK(I) = X(I)
      END DO
!
      KFLAG = 1
      CALL SORT( UWORK, N, KFLAG, IERR )
!
! *** Set up indices for limits
!
      IP05 = 0.05*N
      IP05 = MAX( IP05, 1 )
      IP10 = 0.10*N
      IP10 = MAX( IP10, 1 )
      IP25 = 0.25*N
      IP25 = MAX( IP25, 1 )
      IP75 = 0.75*N
      IP75 = MAX( IP75, 1 )
      IP90 = 0.90*N
      IP90 = MAX( IP90, 1 )
      IP95 = 0.95*N
      IP95 = MAX( IP95, 1 )
!
! *** Store the percentile values
!
      XMIN = UWORK(1)
      XV05 = UWORK(IP05)
      XV10 = UWORK(IP10)
      XV25 = UWORK(IP25)
      IF( MOD(N,2) .EQ. 1 ) THEN
        XMED = UWORK(N/2+1)
      ELSE
        XMED = (UWORK(N/2)+UWORK(N/2+1)) / 2.0
      END IF
      XV75 = UWORK(IP75)
      XV90 = UWORK(IP90)
      XV95 = UWORK(IP95)
      XMAX = UWORK(N)
!
! *** Calculate the mean
!
      VSUM = 0.0
      DO I = 1, N
        VSUM = VSUM + UWORK(I)
      END DO
      XAVG =  VSUM / FLOAT(N)
!
! *** Calculate the standard deviation if there is more
!     than one data value
!
      IF( N .EQ. 1 ) THEN
        RETURN
      ELSE
        VSUM  = 0.0D0
        DO I = 1, N
          VSUM  = VSUM  + (UWORK(I)-XAVG)**2
        END DO
        XSTD = SQRT( VSUM/DBLE(N-1) )
      END IF
!
! *** Normal exit
!
      RETURN
      END SUBROUTINE

