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
!    Paul W. Eslinger : 21 Mar 2005 : Version 1.0
!    Paul W. Eslinger :  5 Jun 2007 : Update for TIIA and stochastic data
!
      CHARACTER(LEN=72) :: PTITLE ! Title for this program run
!
      CHARACTER(LEN=10) :: SDATE  ! Start date of processing (reused)
      CHARACTER(LEN=10) :: STIME  ! Start time of processing (reused)
      CHARACTER(LEN=10) :: EDATE  ! End date of processing
      CHARACTER(LEN=10) :: ETIME  ! End time of processing
!
      LOGICAL :: REPORT   ! Flag whether the report file is open
      LOGICAL :: VERBOSE  ! Flag whether verbose results are output
!
      INTEGER :: NREAL    ! Number of realizations to process
      INTEGER :: NUM_DATA ! Number of DATA cards to process
!
!     Type definition for process data
!
      CHARACTER(LEN= 6) :: DATA_LOC_ID    ! Identification number for a location
      CHARACTER(LEN= 6) :: DATA_ANA_ID    ! Identification number for an analyte
      CHARACTER(LEN= 6) :: DATA_SPC_ID    ! Identification number for a species
      CHARACTER(LEN= 4) :: DATA_STY_ID    ! Identification number for a soil
      INTEGER :: DATA_SPC_IDX             ! Index for the species data
      INTEGER :: DATA_STY_IDX             ! Index for the soil data
      INTEGER :: DATA_YEAR                ! Time (year) where data are valid
      REAL, ALLOCATABLE :: DATA_VALUES(:) ! Vector of data values for output
!
      LOGICAL :: DEBUG = .FALSE.      ! Hard coded debug flag
!
      REAL(KIND=8) :: SDSTOC ! Random seed for all stochastic variables
!
      LOGICAL :: VB_STOC_DEFN ! Logical flag for writing stochastic variable definition
      LOGICAL :: VB_STOC_STAT ! Logical flag for writing statistics for computed values
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Control_Mod

MODULE ESD_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information from the ESD file that will be
!    used in the HUMAN code, in addition, it contains some information
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
        INTEGER :: SECOND         ! Index for the second choice of concentrations
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
!     Type definition for ESD species data
!
      TYPE ESD_SPC_TYPE
        CHARACTER(LEN= 6) :: ID      ! Species ID
        CHARACTER(LEN=48) :: NAME    ! Species name
        CHARACTER(LEN= 8) :: HABITAT ! Species type (aquatic, riparian, or upland)
        LOGICAL :: COMP              ! Flag whether this species is to be used
      END TYPE ESD_SPC_TYPE
      TYPE(ESD_SPC_TYPE), ALLOCATABLE :: ESD_SPC(:) ! Variable structure for species information
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE ESD_Mod

MODULE Files_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This include deck contains file specific variable information.
!
!  History:
!
!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!    Paul W. Eslinger :  5 Jun 2007 : Update for TIIA
!
      INTEGER, PARAMETER :: MAXFN=200 ! Length of file names
!
      INTEGER :: IESD ! Unit number for the ESD keyword file
      CHARACTER(LEN=MAXFN) :: FNESD ! Name of the input ESD keyword file
!
      INTEGER :: IKEY ! Unit number for the keyword file
      CHARACTER(LEN=MAXFN) :: FNKEY ! Name of the input keyword file
!
      INTEGER :: IRPT ! Unit number for the report file
      CHARACTER(LEN=MAXFN) :: FNRPT ! Name of the output report file
!
      CHARACTER(LEN=MAXFN) :: FNMAP ! Name of the concentration record number map file
!
      INTEGER, ALLOCATABLE :: IFOD(:,:) ! Unit number for the food concentration files
      CHARACTER(LEN=MAXFN), ALLOCATABLE :: FNFOD(:,:) ! Names of food concentration files
!
      CHARACTER(LEN=MAXFN) :: FOODPATH ! Path for location of food files
      CHARACTER(LEN=MAXFN) :: FOODMAP  ! Name of the food map file
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Files_Mod

MODULE Iden_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains run and user identification information
!
!  History:
!
!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
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
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Iden_Mod

      PROGRAM FillFCDA
!!*************************************************************************************************
!!
!!                    FillFCDA - Food Concentration Data File Modifications
!!
!!                      Toolkit for Integrated Impacts Assessments (TIIA)
!!                  Data preparation for Environmental Concentration Data Files
!!            Battelle, Pacific Northwest National Laboratories, Richland, Washington
!!
!!*************************************************************************************************
!!
!!  Purpose:
!!    This program reads a keyword file of data and inserts concentrations of foods into binary
!!    food concentration data files.
!!
!!    This program must have been preceded by a run of the ECEM code from the TIIA (Version 1 )
!!    suite of codes.  The ECEM code creates the FOOD concentration data files which this program
!!    will modify.
!!
!! Reference:
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!
!!  History:
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Carmen Arimescu  : 18 Jan 2006 : Change call to FCDA_WRITE (SCR-1101)
!!    Paul W. Eslinger :  5 Jun 2007 : Change structure to handle stochastic data
!!    Paul W. Eslinger : 13 Sep 2007 : Modify stochastic data handling
!!    Paul W. Eslinger : 13 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!*************************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Errors_Mod
      USE Files_Mod
      USE Control_Mod
      USE ESD_Mod
      USE Stats_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Declare local variables
      CHARACTER(LEN=8) :: CALLER = 'FillFCDA' ! Name of this program
      INTEGER :: IERR ! Integer error flag
      INTEGER :: MAXTUSE ! Maximum number of entries in a statistical table
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
!------------------------------------------------------------------------------------
!     Program and run identification and initialization
!------------------------------------------------------------------------------------
      CALL IDEN_SET( )
      CALL INIT( )
!
!------------------------------------------------------------------------------------
!     Keyword file stuff
!------------------------------------------------------------------------------------
!
! *** Open the input file
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the keyword file'
        WRITE(*,*) 'Stop in '//CALLER
        STOP
      END IF
!
! *** Define the report file
      CALL TELLTIME( 'Extracting report file name', 'SCREEN', .FALSE., IRPT )
      CALL READ_KEYS_REPORT( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the report file'
        WRITE(*,*) 'Stop in '//CALLER
        STOP
      END IF
!
! *** Start writing the banner page to the report file
      CALL BANNER_1( )
!
! *** Read the first pass of keywords
      CALL TELLTIME( 'Reading FillFCDA keywords - Pass #1', 'SCREEN', .FALSE., IRPT )
      CALL KEY_FILL_1( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the report file'
        WRITE(*,*) 'Stop in '//CALLER
        STOP
      END IF
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Set up memory for stochastic variables and initialize other stochastic information
      CALL TELLTIME( 'Allocating Stochastic Memory', 'SCREEN', .FALSE., IRPT )
!     Use exactly one stochastic variable at a time
      INDSTO = 1
!
!     Determine the space needed to store the largest user defined stochastic definition
      CALL KEY_USER_DEF( MAXTUSE, IERR )
!
      CALL STOCH_MEMORY( INDSTO, MAXTUSE, NREAL, IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
!     Use the initialization routine and override stochastic output flags
      CALL INIT_STOCH( )
      BG_STOC_DEFN = VB_STOC_DEFN
      BG_STOC_STAT = VB_STOC_STAT
!
! *** Check the identification keywords
      CALL CHECK_FILL_1( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Finish writing the banner page to the report file
      CALL BANNER_2( )
!
! *** Elapsed time message
      CALL ELAPSE( 1, IRPT, IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Open the environmental settings definition keyword file
      CALL OPEN_ESD( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Read the first pass of environmental settings keywords to
!     collect array dimension information
      CALL TELLTIME( 'Reading ESD keywords - Pass #1', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_1( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Check the problem size definition ESD keywords
      CALL CHECK_ESD( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Allocate memory for the ESD variables
      CALL TELLTIME( 'Allocating Memory - ESD', 'SCREEN', .FALSE., IRPT )
      CALL ESD_MEMORY( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Initialize environmental settings data
      CALL ESD_INIT(  )
!
! *** Read the second pass of environmental keywords
!     Save time, analyte, location information, concentration file names
      CALL TELLTIME( 'Reading ESD keywords - Pass #2', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_2( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Check a couple of things from the FILLECDA data
      CALL CHECK_FILL_2( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Echo the data to the report file
      CALL ECHO( )
!
!------------------------------------------------------------------------------------
!     Food concentration file processing
!------------------------------------------------------------------------------------
!
! *** Determine which food files are needed
      CALL TELLTIME( 'Determine with Food Files are Needed', 'SCREEN', .FALSE., IRPT )
      CALL KEY_FILL_2( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Open all the food concentration files needed
      CALL TELLTIME( 'Opening Food Files', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_FOODS( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Write the data into the FCDA files
      CALL TELLTIME( 'Processing Data', 'SCREEN', .FALSE., IRPT )
      CALL PROCESS_DATA( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Normal completion of the program
      GO TO 1000
!
!-----------------------------------------------------------------------
!     Fatal errors trapped after report file is open come to this point
!-----------------------------------------------------------------------
!
  999 CONTINUE
      MESSAG(1) = 'Error encountered in a lower-level routine.'
      MESSAG(2) = 'Execution halted because of the above errors.'
      CALL PRTERR( IERR, CALLER, 2 )
!
! *** Elapsed time message
      CALL ELAPSE( 2, IRPT, IERR )
      WRITE(*,*) 'Abnormal Run Termination Due to Errors'
      STOP
!
 1000 CONTINUE
!
! *** Elapsed time message
      CALL ELAPSE( 2, IRPT, IERR )
!
      MESSAG(1) = 'Normal Termination'
      CALL PRTERR( IERR, CALLER, 1 )
      WRITE(*,*) 'Normal Termination'
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
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change for TIIA version
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
!---- Executable code --------------------------------------------------
!
! *** Define the header
!
      WRITE(IRPT,1000)
 1000 FORMAT(/)
!
      WRITE(IRPT,1010) 'FFFFFFF IIIIIII L       L       EEEEEEE  CCCCC  DDDD      AAAAA '
      WRITE(IRPT,1010) 'F          I    L       L       E       C     C D   DD   A     A'
      WRITE(IRPT,1010) 'F          I    L       L       E       C       D     D  A     A'
      WRITE(IRPT,1010) 'FFFFF      I    L       L       EEEEE   C       D     D  AAAAAAA'
      WRITE(IRPT,1010) 'F          I    L       L       E       C       D     D  A     A'
      WRITE(IRPT,1010) 'F          I    L       L       E       C     C D   DD   A     A'
      WRITE(IRPT,1010) 'F       IIIIIII LLLLLLL LLLLLLL E       CCCCC   DDDD     A     A'
 1010 FORMAT(8X,A)
!
      WRITE(IRPT,1020) PRGNAM, PRGVER, PRGDAT
 1020 FORMAT(//27X,A,' Version ',A/26X,'Last Modified on ',A)
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
!!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Qa_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
!---- Executable code --------------------------------------------
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
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
      USE Esd_Mod
      USE Errors_Mod
      USE Control_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Output integer error flag
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'CHECK_ESD'
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
! *** Number of realizations in the ESD file
!
      IF( ESD_NREAL .LT. 1 ) THEN
        IERR = 2
        MESSAG(1) = 'At least one realization required in the ESD keyword file'
        MESSAG(2) = 'The environmental codes must be run to generate concentrations'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Number of locations in the ESD file
!
      IF( ESD_NUM_LOC .LT. 1 ) THEN
         IERR = 3
         MESSAG(1) = 'At least one  location required in the ESD keyword file'
         MESSAG(2) = 'The environmental codes must be run to generate concentrations'
         CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Number of times in the ESD file
!
      IF( ESD_NUM_TIM .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'At least one time required in the ESD keyword file'
        MESSAG(2) = 'The environmental codes must be run to generate concentrations'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Number of species in the ESD file
!
      IF( ESD_NUM_SPC .LT. 1 ) THEN
        IERR = 5
        MESSAG(1) = 'No species defined in the ESD keyword file'
        MESSAG(2) = 'At least one species is required'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_FILL_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the information from the keyword cards
!!    looking for problem definition problems.
!!
!!  History:
!!
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
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
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'CHECK_FILL_1' ! Name of this routine
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Identification errors
!
      IF( USRNAM .EQ. 'Anonymous User' ) THEN
        IERR = 1
        MESSAG(1) = 'User name must be entered (Keyword USER)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'Problem title must be entered (Keyword TITLE)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
! *** Data card errors
!
      IF( NUM_DATA .LT. 1 ) THEN
        IERR = 3
        MESSAG(1) = 'No DATA cards were entered in the keyword file'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
! *** Number of realizations
!
      IF( NREAL .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'No REALIZATION card was entered in the keyword file'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_FILL_2( IERR )
!!****************************************************************************
!!
!! Purpose:
!!
!!    This subroutine checks some information from the FIllECDA keyword file.
!!
!!  History:
!!
!!    Paul W. Eslinger :  5 Jun 2007 : Original source
!!
!!****************************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE ESD_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error flag, nonzero if no error
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'CHECK_FILL_2' ! Name of this routine
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
! *** Check on the number of realizations
!
      IF( ESD_NREAL .NE. NREAL ) THEN
        IERR = 1
        MESSAG(1) = 'Mismatch on the realization keywords'
        MESSAG(2) = 'FillECDA set NREAL = '
        WRITE(MESSAG(2)(22:),'(I0)') NREAL
        MESSAG(3) = 'The ESD keyword file had ESD_NREAL set = '
        WRITE(MESSAG(3)(42:),'(I0)') ESD_NREAL
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
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
!!    Paul W. Eslinger :  5 Jun 2007 : Original source
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Iden_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
!---- Executable code ---------------------------------------------
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
      SUBROUTINE ECHO( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles writing an echo of the problem definition
!!    to the report file.
!!
!!  History:
!!
!!    Carmen Arimescu  :  4 Mar 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments TIIA version
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Esd_Mod
      USE FCDA_MOD
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Local variables
!
!---- Executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(//24('='),' Echo of the Problem Definition ',24('='))
!
! *** Problem Identification
!
      WRITE(IRPT,1010) PTITLE, USRNAM
 1010 FORMAT(/'Title: ',A/'User:  ',A)
!
      WRITE(IRPT,1020) NREAL, ESD_NREAL
 1020 FORMAT(I4,' Realizations requested in this run'/&
             I4,' Realizations available in the concentration files')
!
! *** Files names used in the analysis
!
      WRITE(IRPT,1030) 'File Name for this file', TRIM(FNRPT)
      WRITE(IRPT,1030) 'File Name for Input Keyword Data', TRIM(FNKEY)
 1030 FORMAT(/A/'File: ',A)
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ELAPSE( IFLG, IRPT, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes a time and date stamp to the report
!!    file at starting and end of the processing for this scenario.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!  Call List Variables:
!!
!!    IFLG  : Input integer flag
!!              1 = Starting time
!!              2 = Ending time
!!    IERR  : Output integer flag (0=no errors)
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: IFLG ! Flag for place (begining or end)
      INTEGER, INTENT(IN) :: IRPT ! Unit number for the report file
      INTEGER :: IERR ! Error flag indicator
!
! *** Local variables
!
      CHARACTER(LEN=5) :: CALLER = 'ELAPSE'
!
!---- Executable code --------------------------------------------------
!
      IERR   = 0
!
      SELECT CASE ( IFLG )
!
        CASE( 1 ) ! Starting problem execution
          CALL DATE_AND_TIME( SDATE, STIME )
          WRITE(IRPT,1000) 'Starting Problem Execution'
 1000     FORMAT(/A)
          WRITE(IRPT,1010) SDATE(5:6), SDATE(7:8), SDATE(1:4), &
            STIME(1:2), STIME(3:4), STIME(5:10)
 1010     FORMAT(2X,'Date: ',A,'/',A,'/',A/2X,'Time: ',A,':',A,':',A)
!
        CASE( 2 ) ! Ending problem execution
          CALL DATE_AND_TIME( EDATE, ETIME )
          WRITE(IRPT,1000) 'Ending Problem Execution'
          WRITE(IRPT,1010) EDATE(5:6), EDATE(7:8), EDATE(1:4), &
            ETIME(1:2), ETIME(3:4), ETIME(5:10)
!
        CASE DEFAULT ! Invalid branch
          IERR = 1
          MESSAG(1) = 'Invalid time selector (IFLG)'
          MESSAG(2) = 'Time selector must be 1 or 2'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
!
      END SELECT
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
!!    Paul W. Eslinger : 11 May 2000 : Version 2.0
!!    Paul W. Eslinger : 22 Oct 2003 : Add MEAT remote location logic
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
!
      USE ESD_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Control_Mod
      USE FCDA_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error flag, nonzero if no match
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'ESD_INIT' ! Name of this routine
      INTEGER :: I    ! Looping variable
      INTEGER :: ISPC ! Species looping variable
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
        MESSAG(2) = 'Suggest modifying the ESD TIME keyword, HUMAN modifier'
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
        MESSAG(2) = 'Suggest modifying the ESD LOCATION keyword, HUMAN modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      DO I = 1, ESD_NUM_LOC
        ESD_LOC(I)%ID = ' '
        ESD_LOC(I)%NAME = ' '
        ESD_LOC(I)%TYPE = ' '   
        ESD_LOC(I)%COMP   = .FALSE.
        ESD_LOC(I)%OUTPUT = .FALSE.
        ESD_LOC(I)%EASTING  = 0.0
        ESD_LOC(I)%NORTHING = 0.0
        ESD_LOC(I)%POP    = 0
        ESD_LOC(I)%SECOND = 0
        ESD_LOC(I)%MEAT   = 0
        ESD_LOC(I)%FISH   = 0
        ESD_LOC(I)%BIRD   = 0
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
        ESD_ANA(I)%COMP   = .FALSE.
        ESD_ANA(I)%OUTPUT = .FALSE.
        ESD_ANA(I)%ANATYP = 0
      END DO
!
! *** Variables for the species data
!
      DO ISPC = 1, ESD_NUM_SPC
        ESD_SPC(ISPC)%ID = ' '
        ESD_SPC(ISPC)%NAME = ' '
        ESD_SPC(ISPC)%HABITAT = ' '
        ESD_SPC(ISPC)%COMP = .FALSE.
      END DO
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
!!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
      USE Files_Mod
      USE ESD_Mod
      USE Control_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error flag, nonzero if no error
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'ESD_MEMORY' ! Name of this routine
      INTEGER :: IERA ! Error status variable from the allocate action
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
! *** ESD_ANA : Top-level for the analyte structure
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
! *** Food concentration files by analyte and species
!
      ALLOCATE( FNFOD(ESD_NUM_ANA,ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating memory for FNFOD(ESD_NUM_ANA,ESD_NUM_SPC)'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( IFOD(ESD_NUM_ANA,ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating memory for IFOD(ESD_NUM_ANA,ESD_NUM_SPC)'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF      
!
! *** Check on the number of locations
!
      IF( ESD_NUM_LOC .LT. 1 ) THEN
        IERR = 5
       MESSAG(1) = 'At least 1 location required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** ESD_LOC : Top-level of the location structure
!
      ALLOCATE( ESD_LOC(ESD_NUM_LOC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 6
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
        IERR = 7
        MESSAG(1) = 'At least 1 time required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** ESD_TIM : Top-level of the times structure
!
      ALLOCATE( ESD_TIM(ESD_NUM_TIM), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating memory for ESD_TIM'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** ESD_SPC : Top-level of the species structure
!
      ALLOCATE( ESD_SPC(ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Error allocating memory for ESD_SPC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      INTEGER FUNCTION GET_UNIT_NUMBER( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This functions finds the first unused unit number in the range
!!    7 to 1007 to use for a file operation.
!!
!!    A value of -1 is returned if the search was unsuccessful.
!!
!!  History:
!!
!!    Paul W. Eslinger : 27 Oct 2004 : Original source
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Local variables
      LOGICAL :: CONNECTED ! Logical flag whether unit number was connected
      INTEGER :: IFIL      ! Looping index for file numbers
!
!---- Executable code --------------------------------------------
!
      DO IFIL = 7, 1007
        INQUIRE( UNIT=IFIL, OPENED=CONNECTED )
        IF( .NOT. CONNECTED ) THEN
          GET_UNIT_NUMBER = IFIL
          RETURN
        END IF
      END DO
!
! *** Didn't find a valid unit number
!
      GET_UNIT_NUMBER = -1
!
      RETURN
      END FUNCTION
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
!!  History:
!!
!!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
!
! *** Force explicit typing of variables
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
      PRGNAM = 'FillFCDA'
      PRGVER = '4.00.001'
!
! *** Program date (DD MMM YYYYY)
      PRGDAT = '13 Jul 2012'
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
!!    This subroutine initializes some global variables of the FillFCDA
!!    computer code.
!!
!!  History:
!!
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change for stochastic variables
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE ESD_Mod
!
! *** Force explicit variable typing
      IMPLICIT NONE
!
!---- Executable code --------------------------------------------
!
! *** Keyword input file
!
      FNKEY = ' '
!
! *** Report file
!
      FNRPT = ' '
      REPORT = .FALSE.
!
! *** ESD keyword input file
!
      FNESD = ' '
!
! *** Input concentration record number map file
!
      FNMAP = ' '
!
! *** Variables for food consumption
!
      FOODPATH = ' '
      FOODMAP  = ' '
!
! *** Problem title
!
      PTITLE = ' '
!
! *** Initialize counters
!
      NREAL = 0
      NUM_DATA = 0
!
      ESD_NUM_LOC = 0
      ESD_NUM_TIM = 0
      ESD_NUM_ANA = 0
      ESD_NUM_SPC = 0
!
! *** Logical flags
      DEBUG   = .FALSE.
      VERBOSE = .FALSE.
!
      VB_STOC_DEFN = .FALSE.
      VB_STOC_STAT = .FALSE.
!
! *** Seed for random number generator
      SDSTOC = 0.0D0
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE KEY_ESD_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading the environmental settings for the
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
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
      USE ESD_Mod
!
! *** Force explicit variable typing
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_1'
      CHARACTER :: TITLE*(LENCRD) ! Title for RDBLK
!
!---- Executable code --------------------------------------------
!
! *** Initialize for RDBLK
!
      ILINE = 0
!
! *** Top of loop on reading keyword cards
!
   10 CONTINUE
!
      CALL RDBLK( IESD, IRPT, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'ANALYTE' ) ! ===> ANALYTE keyword
          ESD_NUM_ANA = ESD_NUM_ANA + 1
!
        CASE( 'END' ) ! ===> END keyword
          REWIND(IESD)
          RETURN
!
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
          ESD_NUM_LOC = ESD_NUM_LOC + 1
!
        CASE( 'REALIZAT' ) ! ===> REALIZATION Keyword
          ESD_NREAL = VALUE(1)
!
        CASE( 'SPECIES' ) ! ===> SPECIES Keyword
          ESD_NUM_SPC = ESD_NUM_SPC + 1
!
        CASE( 'TIMES' ) ! ===> TIMES Keyword
           ESD_NUM_TIM = ESD_NUM_TIM + NVALUE
!
        CASE( 'TITLE' ) ! ===> TITLE Keyword
          ESD_TITLE = QUOTE(1)
!
        CASE DEFAULT ! Ignore other keywords
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
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE Control_Mod
!
! *** Force explicit variable typing
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_2'
!
      INTEGER :: IDX, ITMP, ISPC
      CHARACTER(LEN=LENCRD) :: TITLE
      CHARACTER(LEN=LENQQQ) :: TMP_ID, TMP_NAME
!
!---- Executable code --------------------------------------------
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
      ESD_NUM_SPC = 0 ! Number of species in the ESD file
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!----------------------------------------------------------------------C
!
   10 CONTINUE
!
      CALL RDBLK( IESD, IRPT, TITLE, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'ANALYTE' ) ! ===> ANALYTE keyword
!
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
          IF( CEXIST('TYPE    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%TYPE = TMP_NAME
            ELSE
              IERR = 3
              MESSAG(1) = 'Analyte TYPE modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 4
            MESSAG(1) = 'TYPE modifier not entered on the ANALYTE keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('NAME    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%NAME = TMP_NAME
            ELSE
              IERR = 5
              MESSAG(1) = 'Analyte NAME modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 6
            MESSAG(1) = 'NAME modifier not entered on the ANALYTE keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
        CASE( 'END' ) ! ===> END keyword
          CLOSE( IESD )
          RETURN
!
!-----------------------------------------------------------------------
        CASE( 'LOCATION' ) ! LOCATION Keyword
!
          ESD_NUM_LOC = ESD_NUM_LOC + 1
!
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%ID = TMP_ID
            ELSE
              IERR = 13
              MESSAG(1) = 'Location ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 14
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
              IERR = 15
              MESSAG(1) = 'Location NAME modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 16
            MESSAG(1) = 'NAME modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('TYPE') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%TYPE = TMP_NAME
            ELSE
              IERR = 21
              MESSAG(1) = 'Location TYPE modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 22
            MESSAG(1) = 'TYPE modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'TIMES' ) ! TIMES Keyword
          IF( NVALUE .GT. 0 ) THEN
            DO ITMP = 1, NVALUE
              ESD_NUM_TIM = ESD_NUM_TIM + 1
              ESD_TIM(ESD_NUM_TIM)%TIME = VALUE(ITMP)
            END DO
          ELSE
            IERR = 33
            MESSAG(1) = 'No numeric values found on the TIMES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF          
!
!-----------------------------------------------------------------------
        CASE( 'SPECIES' ) ! ===> SPECIES Keyword
!
          ESD_NUM_SPC = ESD_NUM_SPC + 1
!
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
            ESD_SPC(ESD_NUM_SPC)%ID = TMP_ID
            ELSE
              IERR = 35
              MESSAG(1) = 'Species ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 36
            MESSAG(1) = 'ID modifier not entered on the SPECIES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!         Ensure the species ID is unique
          DO ISPC = 1, ESD_NUM_SPC-1
            IF( ESD_SPC(ESD_NUM_SPC)%ID .EQ. ESD_SPC(ISPC)%ID ) THEN
              IERR = 38
              MESSAG(1) = 'Multiple species have the same ID'
              MESSAG(2) = 'The species ID is "'//TRIM(ESD_SPC(ISPC)%ID)//'"'
              MESSAG(3) = 'Problem encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          END DO
!
          IF( CEXIST('NAME    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_SPC(ESD_NUM_SPC)%NAME = TMP_NAME
            ELSE
              IERR = 39
              MESSAG(1) = 'Species NAME modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 40
            MESSAG(1) = 'NAME modifier not entered on the SPECIES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('HABITAT') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              IF( TMP_ID.EQ.'AQUATIC' .OR. TMP_ID.EQ.'RIPARIAN' .or. TMP_ID.EQ.'UPLAND' ) THEN
                ESD_SPC(ESD_NUM_SPC)%HABITAT = TMP_ID
              ELSE
                IERR = 43
                MESSAG(1) = 'Invalid HABITAT string encountered'
                MESSAG(2) = 'Species ID is '// ESD_SPC(ESD_NUM_SPC)%ID
                MESSAG(3) = 'Problem in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            ELSE
              IERR = 44
              MESSAG(1) = 'No quote string with the HABITAT modifier on the SPECIES keyword'
              MESSAG(2) = 'Species ID is '// ESD_SPC(ESD_NUM_SPC)%ID
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 45
            MESSAG(1) = 'HABITAT modifier not entered on the SPECIES keyword'
            MESSAG(2) = 'Species ID is '// ESD_SPC(ESD_NUM_SPC)%ID
            MESSAG(3) = 'Problem in the ESD keyword file'
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
!
      SUBROUTINE KEY_FILL_1( IERR )
!!********************************************************************************
!!
!!  Purpose:
!!    This subroutine handles reading the FillFCDA keyword control
!!    information required to set variable dimensions.
!!
!!  Call List Variables:
!!    IERR : Output integer error flag.
!!            0 = Normal execution
!!           >0 = Terminal error encountered
!!
!!  Auxiliary Routines:
!!    CEXIST, PRTERR, and all RDBLK related routines
!!
!!  Notes:
!!    When errors are encountered, a message is output to the standard
!!    output device and control is passed back to the calling routine.
!!
!!  History:
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change structure to handle stochastic data
!!
!!********************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** User-defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'KEY_FILL_1'
!
      INTEGER :: IDX   ! Temporary index variable
      CHARACTER :: TITLE*(LENCRD)
      CHARACTER(LEN=LENQQQ) :: TMP_NAME
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
!
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
        CASE( 'DATA' ) ! ===> DATA Keyword
          NUM_DATA = NUM_DATA + 1
!
        CASE( 'DEBUG' ) ! ===> DEBUG Keyword
          DEBUG = .TRUE.
!
        CASE( 'END' ) ! ===> END keyword
          REWIND( IKEY )
          RETURN
!
        CASE( 'FILE' ) ! ===> FILE keyword
          IF( CEXIST('ESD     ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 14
              MESSAG(1) = 'The ESD modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNESD = TMP_NAME
            END IF
          END IF
!
        CASE( 'FOODS' ) ! ===> FOODS keyword
!
          IF( CEXIST('PATH') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 26
              MESSAG(1) = 'Path name (quote string) not found'
              MESSAG(2) = 'FOOD keyword, PATH modifier'
              MESSAG(3) = 'Problem in the FillFCDA keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              FOODPATH = TMP_NAME
            END IF
          ELSE
            IERR = 27
            MESSAG(1) = 'The PATH modifier is required on the FOOD keyword'
            MESSAG(2) = 'Problem in the FillFCDA keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('MAP') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 28
              MESSAG(1) = 'Map file name (quote string) not found'
              MESSAG(2) = 'FOOD keyword, MAP modifier'
              MESSAG(3) = 'Problem in the FillFCDA keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              FOODMAP = TMP_NAME
            END IF
          ELSE
            IERR = 29
            MESSAG(1) = 'The MAP modifier is required on the FOOD keyword'
            MESSAG(2) = 'Problem in the FillFCDA keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 'REALIZAT' ) ! ===> REALIZATION Keyword
          NREAL = VALUE(1)
!
        CASE( 'SEED' ) ! ===> SEED keyword
          SDSTOC = VALUE(1)
!
        CASE( 'TITLE' ) ! ===> TITLE Keyword
          PTITLE = QUOTE(1)
!
        CASE( 'USER' ) ! ===> USER Keyword
          USRNAM = QUOTE(1)
!
        CASE( 'VERBOSE' ) ! ===> VERBOSE Keyword
          VERBOSE = .TRUE.
          IF( CEXIST('STATISTI') ) VB_STOC_STAT = .TRUE.
          IF( CEXIST('DEFINITI') ) VB_STOC_DEFN = .TRUE.
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
      SUBROUTINE KEY_FILL_2( IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles defining the analytes and species for which food
!!    files are needed.
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
!!    Paul W. Eslinger :  5 Jun 2007 : New source
!!
!!********************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE FCDA_MOD
      USE Stats_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** User-defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'KEY_FILL_2' ! Name of this subroutine
      INTEGER :: IDX, IDXA, IDXS      ! Temporary index variables
      CHARACTER :: TITLE*(LENCRD)     ! Keyword line from RDBLK
      CHARACTER(LEN=LENQQQ) :: CQTMP  ! Temporary variable for quote strings
      CHARACTER(LEN=6) :: SHORT_ID    ! Temporary variable for quote strings
!
!---- Executable code --------------------------------------------
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
        CASE( 'DATA' ) ! ===> DATA Keyword
!
          IF( CEXIST('ANALYTE') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            SHORT_ID = CQTMP
            CALL MATCH_ANA( SHORT_ID, IDXA )
            IF( IDXA .GT. 0 ) THEN
              ESD_ANA(IDXA)%COMP = .TRUE.
!              IF( DEBUG ) WRITE(IRPT,*) 'Analyte = '//TRIM(SHORT_ID)
            ELSE
              IERR = 1
              MESSAG(1) = 'Analyte requested that is not in the master list'
              MESSAG(2) = 'Analyte ID is '// SHORT_ID
              MESSAG(3) = 'Problem in the FillECDA keyword file - DATA keyword'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 2
            MESSAG(1) = 'ANALYTE modifier not entered on the DATA keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('SPECIES') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            SHORT_ID = CQTMP
            CALL MATCH_SPC( SHORT_ID, IDXS )
            IF( IDXS .GT. 0 ) THEN
              ESD_SPC(IDXS)%COMP = .TRUE.
!              IF( DEBUG ) WRITE(IRPT,*) 'Species = '//TRIM(SHORT_ID)
            ELSE
              IERR = 3
              MESSAG(1) = 'Species requested that is not in the master list'
              MESSAG(2) = 'Species ID is '// SHORT_ID
              MESSAG(3) = 'Problem in the FillFCDA keyword file - DATA keyword'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 4
            MESSAG(1) = 'Species modifier not entered on the DATA keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
        CASE( 'END' ) ! ===> END keyword
          REWIND( IKEY )
          RETURN
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
      SUBROUTINE KEY_FILL_3( MOREDATA, STOCDATA, IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and storing the data from a
!!    single DATA keyword.
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
!!    Paul Eslinger    : 10 Jun 2002 : Version 2.0
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Carmen Arimescu  : 18 Jan 2006 : Fix error traps (return statements)
!!    Paul W. Eslinger :  5 Jun 2007 : Change structure to handle stochastic data
!!
!!********************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE FCDA_MOD
      USE Stats_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** User-defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      LOGICAL :: MOREDATA ! Return .True. if DATA found; .false. otherwise
      LOGICAL :: STOCDATA ! Return .True. if stochastic; .false. otherwise
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      CHARACTER(LEN=10) :: CALLER = 'KEY_FILL_3'
      INTEGER :: IDX   ! Temporary index variable
      INTEGER :: IDXA, IDXS  ! Temporary index variable
      REAL :: RTMP  ! Temporary real variable
      CHARACTER :: TITLE*(LENCRD)
      CHARACTER(LEN=LENQQQ) :: CQTMP    ! Temporary variable for quote strings
      CHARACTER(LEN=6) :: SHORT_ID
      CHARACTER(LEN=4) :: SHORT2_ID
!
!     Variables for decoding stochastic data
      CHARACTER(LEN=20) :: CTMP ! Temporary variable ID
      CHARACTER(LEN=72) :: CMES ! Temporary distribution label
      LOGICAL :: TRUNC          ! Logical flag for truncation test
!
!---- Executable code --------------------------------------------
!
      IERR = 0
      MOREDATA = .FALSE.
      STOCDATA = .FALSE.
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
        CASE( 'DATA' ) ! ===> DATA Keyword
!
          MOREDATA = .TRUE.
!
          IF( CEXIST('ANALYTE') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            SHORT_ID = CQTMP
            CALL MATCH_ANA( SHORT_ID, IDXA )
            IF( IDXA .GT. 0 ) THEN
              DATA_ANA_ID = SHORT_ID
            ELSE
              IERR = 1
              MESSAG(1) = 'Analyte requested that is not in the master list'
              MESSAG(2) = 'Analyte ID is '// SHORT_ID
              MESSAG(3) = 'Problem in the FillECDA keyword file - DATA keyword'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 2
            MESSAG(1) = 'ANALYTE modifier not entered on the DATA keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('LOCATION') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            SHORT_ID = CQTMP
            CALL MATCH_LOC( SHORT_ID, IDXA )
            IF( IDXA .GT. 0 ) THEN
              DATA_LOC_ID = SHORT_ID
            ELSE
              IERR = 3
              MESSAG(1) = 'Location requested that is not in the master list'
              MESSAG(2) = 'Location ID is '// SHORT_ID
              MESSAG(3) = 'Problem in the FillECDA keyword file - DATA keyword'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 4
            MESSAG(1) = 'LOCATION modifier not entered on the DATA keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('YEAR') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 5
              MESSAG(1) = 'Year value not found'
              MESSAG(2) = 'DATA keyword, YEAR modifier'
              MESSAG(3) = 'Problem in the FillECDA keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            DATA_YEAR = RTMP
          ELSE
            IERR = 6
            MESSAG(1) = 'YEAR modifier not entered on the DATA keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('SPECIES') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            SHORT_ID = CQTMP
            CALL MATCH_SPC( SHORT_ID, IDXS )
            IF( IDXS .GT. 0 ) THEN
              DATA_SPC_ID = SHORT_ID
              DATA_SPC_IDX = IDXS
            ELSE
              IERR = 1
              MESSAG(1) = 'Species requested that is not in the master list'
              MESSAG(2) = 'Species ID is '// SHORT_ID
              MESSAG(3) = 'Problem in the FillFCDA keyword file - DATA keyword'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 2
            MESSAG(1) = 'Species modifier not entered on the DATA keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('SOIL') ) THEN
            CALL NXTQOT( IDX, CQTMP )
            SHORT2_ID = CQTMP
            CALL MATCH_SOIL( SHORT2_ID, IDXA )
            IF( IDXA .GT. 0 ) THEN
              DATA_STY_IDX = IDXA
              DATA_STY_ID  = SHORT2_ID
            ELSE
              IERR = 7
              MESSAG(1) = 'Soil requested that is not in the master list'
              MESSAG(2) = 'Soil ID is '// SHORT2_ID
              MESSAG(3) = 'Problem in the FillFCDA keyword file - DATA keyword'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          END IF
!
!         Data values are entered explicitly
          IF( CEXIST('VALUES') ) THEN
            STOCDATA = .FALSE.
            IF( NVALUE .LT. NREAL+1 ) THEN
              IERR = 11
              MESSAG(1) = 'Not enough numerical data entries for the number of realizations'
              MESSAG(2) = 'DATA keyword, VALUES modifier'
              MESSAG(3) = 'Problem in the FillECDA keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 9
              MESSAG(1) = 'No numerical values found'
              MESSAG(2) = 'DATA keyword, VALUES modifier'
              MESSAG(3) = 'Problem in the FillECDA keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            DO IDXA = 1, NREAL
              DATA_VALUES(IDXA) = VALUE(IDX+IDXA-1)
            END DO
          END IF
!
!         Data values are entered as a statistical distribution
          IF( CEXIST('STOCHAST') ) THEN
            STOCDATA = .TRUE.
!
!           Find where the stochastic data start
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 12
              MESSAG(1) = 'No numerical values found'
              MESSAG(2) = 'DATA keyword, STOCHASTIC modifier'
              MESSAG(3) = 'Problem in the FillECDA keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
!
!           Store the quote strings and decode the numerical parameters for stochastic data
            CTMP = DATA_ANA_ID//DATA_LOC_ID//DATA_STY_ID
            CMES = 'Stochastic data for the year '
            WRITE(CMES(30:),'(I0)') DATA_YEAR
            IF( CEXIST('TRUNCATE') ) THEN
              TRUNC = .TRUE.
            ELSE
              TRUNC = .FALSE.
            END IF
!           ! Only save one stochastic distribution at a time (reset some counters)
            INDSTO = 0
            INDTBL = 0
            CALL SDECOD( CTMP, CMES, IDX, TRUNC, VALUE, NVALUE, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 9999
              MESSAG(1) = 'Error in lower level routine for variable ' // CTMP
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          END IF
!
!         Exit if data successfully found
          RETURN
!
        CASE( 'END' ) ! ===> END keyword
          RETURN
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
      SUBROUTINE KEY_USER_DEF( MAXTUSE, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles checking which DATA keywords have a user
!!    defined distribution and returns the maximum table size needed
!!    to store the data.
!!
!!    A minimum value of 1 is returned so the allocate statements will
!!    not have an error termination.
!!
!!  History:
!!
!!    Paul W. Eslinger : 13 Sep 2007 : Original code
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
!
      IMPLICIT NONE
!
! *** User-defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: MAXTUSE ! Maximum table dimension
      INTEGER :: IERR    ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'KEY_USER_DEF'
      INTEGER :: IDX   ! Temporary index variable
      REAL :: RTMP     ! Temporary real value
      INTEGER :: ITMP  ! Temporary integer variable
!
!     RDBLK Storage variables
      CHARACTER :: TITLE*(LENCRD)
!
!---- First executable code --------------------------------------------
!
      IERR = 0
      ILINE = 0
!
      MAXTUSE = 1
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
        CASE( 'DATA' ) ! ===> DATA Keyword
!
!         Only use this option for stochastic data
          IF( CEXIST('STOCHAST') ) THEN
!           Check for numerical data
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 1
              MESSAG(1) = 'No numerical data associated with the STOCHAST modifier'
              MESSAG(2) = 'Invalid construction of a DATA keyword'
              MESSAG(3) = 'Problem in the FillECDA keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
!
!           Only use distribution type 10 (user defined distribution)
            ITMP = RTMP
            IF( ITMP .EQ. 10 ) THEN
              IDX = IDX + 1
              IF( IDX .GT. NVALUE ) THEN
                IERR = 2
                MESSAG(1) = 'Not enough numerical data associated with the STOCHAST modifier'
                MESSAG(2) = 'Invalid construction of a DATA keyword'
                MESSAG(3) = 'Problem in the FillECDA keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
              ITMP = VALUE(IDX)
              MAXTUSE = MAX(MAXTUSE,ITMP)
            END IF
!
          END IF
!
        CASE( 'END' ) ! ===> END keyword
          REWIND( IKEY )
          RETURN
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
!!    This subroutine attempts to find a match between an analyte ID
!!    in the ESD module and an analyte label.  If found, the index
!!    identifies the storage location for the analyte data.  If not
!!    found, a negative index is returned.
!!
!!  Call List:
!!
!!    LABEL : Input  - Character - analyte ID
!!    IDX   : Output - Integer   - index for data associated with LABEL
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
      USE ESD_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IDX
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
!
! *** Local variables
      INTEGER :: IANA ! Looping variable
!
!---- Executable code --------------------------------------------
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
!!    LABEL : Input  - Character - laocation label
!!    IDX   : Output - Integer   - index for data associated with LABEL
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
      USE ESD_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IDX
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
!
! *** Local variables
      INTEGER :: ILOC ! Looping variable
!
!---- Executable code --------------------------------------------
!
      IDX = -1
!
!        WRITE(*,*) esd_num_loc
      DO ILOC = 1, ESD_NUM_LOC
!        WRITE(*,*) esd_loc(iloc)%id
        IF( ESD_LOC(ILOC)%ID .EQ. LABEL ) THEN
          IDX = ILOC
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_SOIL( LABEL, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a media ID
!!    in the control module and a media index.  If found, the index
!!    identifies the storage location for the soil data.  If not
!!    found, a negative index is returned.
!!
!!  Call List:
!!
!!    LABEL : Input  - Character - Media ID
!!    IDX   : Output - Integer   - index for data associated with LABEL
!!
!!  History:
!!
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Paul W. Eslinger : 23 Feb 2006 : Change variable names
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
      USE FCDA_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IDX
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
!
!---- Executable code --------------------------------------------
!
      IDX = -1
      IF( LABEL .EQ. 'SODR' ) IDX = IDXF_SODR             ! IDXF_SODR = 1
      IF( LABEL .EQ. 'SOGW' ) IDX = IDXF_SOGW             ! IDXF_SOGW = 2
      IF( LABEL .EQ. 'SOSW' ) IDX = IDXF_SOSW             ! IDXF_SOSW = 3

!      WRITE(*,*) label, ' idx=',idx
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_SPC( LABEL, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between an species ID
!!    in the ESD module and a species label.  If found, the index
!!    identifies the storage location for the species data.  If not
!!    found, a negative index is returned.
!!
!!  Call List:
!!
!!    LABEL : Input  - Character - species label
!!    IDX   : Output - Integer   - index for data associated with LABEL
!!
!!  History:
!!
!!    Paul W. Eslinger : 26 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
      USE ESD_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IDX
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
!
! *** Local variables
!
      INTEGER :: ISPC ! Looping variable
!
!---- Executable code --------------------------------------------
!
      IDX = -1
      DO ISPC = 1, ESD_NUM_SPC
!        WRITE(*,*) ISPC, ' : Checking ',LABEL,' against ', ESD_SPC(ISPC)%ID !PWE
        IF( ESD_SPC(ISPC)%ID .EQ. LABEL ) THEN
          IDX = ISPC
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
!!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!
!!**********************************************************************
!
! *** Global variables
      USE ESD_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IDX
      INTEGER :: TIME
!
! *** Local variables
      INTEGER :: ITIM ! Looping variable
!
!---- Executable code --------------------------------------------
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
      SUBROUTINE OPEN_ESD( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the ESD keyword file for reading keywords.
!!
!!  History:
!!
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
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
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** External functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR
      INTEGER :: IERF ! Status variable for open statement
!
! *** Local variables
!
      LOGICAL THERE
      CHARACTER(LEN=8) :: CALLER = 'OPEN_ESD'
!
!---- Executable code --------------------------------------------
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
! *** Get a unit number
!
      IESD = GET_UNIT_NUMBER(  )
      IF( IESD .LT. 7 ) THEN
        IERR = 3
        WRITE(*,*) 'Unable to get unit number for ESD keyword file'
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IESD,FILE=FNESD,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 4
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
      SUBROUTINE OPEN_FOODS( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the input food concentration files 
!!    used for consumption.
!!
!!  History:
!!
!!    Carmen Arimescu  : 24 Feb 2005 : SAC Rev. 1
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
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
      USE Errors_Mod
      USE Control_Mod
      USE FCDA_Mod
      USE ESD_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** External functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR ! Error indication
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'OPEN_FOODS' ! Name of this subroutine
      INTEGER :: IANA ! Looping variable for analytes
      INTEGER :: ISPC ! Looping variable for species
!
      CHARACTER(LEN=6) :: FCDA_ANA_ID   ! Analyte ID: First header line
      CHARACTER(LEN=6) :: FCDA_SPC_ID   ! Species ID: Second header line
      CHARACTER(LEN=8) :: FCDA_SPC_HAB  ! Species habitat
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      IF( DEBUG ) THEN
        WRITE(IRPT,'(A/)') 'Entering subroutine ' // CALLER
        WRITE(IRPT,'(A)') 'Opening all food files'
      END IF
!
! *** Get the index map data for all foods
!
      FOODMAP = TRIM(FOODPATH)//TRIM(FOODMAP)
      IF( DEBUG ) THEN
        WRITE(IRPT,'(A)') 'Food Path: '//TRIM(FOODPATH)
        WRITE(IRPT,'(A)') 'Food Map File: '//TRIM(FOODMAP)
      END IF
!
      CALL FCDA_MAPREAD( FOODMAP, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Error reading the food map data file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
      IF( DEBUG ) WRITE(IRPT,'(A)') 'Food map data file read successfully'
!
! *** Loop over analytes and species and only open the files represented in the data set
!
      WRITE(IRPT,'(A)') ' '
      WRITE(IRPT,'(A)') 'Listing of unit numbers and food concentration files'
!
!      IF( DEBUG ) WRITE(IRPT,1000) ESD_NUM_ANA, ESD_NUM_SPC
! 1000 FORMAT(/'ESD_NUM_ANA=',I0,' ESD_NUM_SPC=',I0)
!
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
!
        DO ISPC = 1, ESD_NUM_SPC
          IF( .NOT. ESD_SPC(ISPC)%COMP ) CYCLE
!
! ***     Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
! ***     Open the file
!
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 3
            MESSAG(1) = 'System error opening the FOOD concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'Species ID: ' // ESD_SPC(ISPC)%ID
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF

!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 4
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 5
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
        END DO
!
      END DO
!
      RETURN
      END SUBROUTINE OPEN_FOODS
!
      SUBROUTINE OPEN_KEY( IERR )
!!**********************************************************************
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
!!    Carmen Arimescu  : 21 Mar 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change comments for TIIA version
!!    Paul W. Eslinger : 29 Jun 2012 : Revise to a common callable routine
!!
!!  Call List Variables:
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in locating or opening the input keyword file
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Iden_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** External functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
      INTEGER, EXTERNAL :: NARGS ! Lahey utility subroutine
!
! *** Call list variables
      INTEGER :: IERR ! Output integer error flag
!                       0 = No errors; >0 = Terminal error
!
! *** Local variables
      INTEGER :: NUM_ARGS      ! Number of command line arguments
      INTEGER :: NUM_FNAM      ! Argument number for the keyword file name
      CHARACTER(LEN=5) :: HELP ! Character for checking for help
      LOGICAL :: THERE         ! File existence variable
      INTEGER :: IERF          ! Status variable for open statement
!
!---- Executable code --------------------------------------------
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
 1010   FORMAT(/' Enter the control keyword file name > ')
        READ(*,*) FNKEY
      END IF
!
! *** Check if the requested file exists
      INQUIRE(FILE=FNKEY,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 1
        WRITE(*,1020) TRIM(FNKEY)
 1020   FORMAT(' The requested keyword file was not found'/ &
          ' File: ',A)
        RETURN
      END IF
!
! *** Get a unit number
      IKEY = GET_UNIT_NUMBER(  )
      IF( IKEY .LT. 7 ) THEN
        IERR = 2
        WRITE(*,*) 'Unable to get unit number for the input keyword file'
        RETURN
      END IF
!
! *** Attempt to open the file
      OPEN(IKEY,FILE=FNKEY,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 3
        WRITE(*,1030) TRIM(FNKEY)
 1030   FORMAT(' System error opening the input keyword file'/ &
               ' File: ',A)
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PROCESS_DATA( IERR )
!!******************************************************&**************************
!!
!!  Purpose:
!!
!!    This subroutine handles processing the data and writing to the
!!    appropriate data file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!!    Carmen Arimescu  :  4 Mar 2005
!!    Carmen Arimescu  : 18 Jan 2006 : Update call to FCDA_WRITE to
!!                                     avoid Linux memory problem
!!    Paul W. Eslinger :  5 Jun 2007 : Change structure to handle stochastic data
!!
!!******************************************************&**************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Esd_Mod
      USE Errors_Mod
      USE FCDA_MOD
      USE Stats_Mod
      USE Rdblk_Mod
!
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Output integer error flag
!                       0 = No errors; >0 = Terminal error
!
! *** Local variables
!
      CHARACTER(LEN=12) :: CALLER = 'PROCESS_DATA'  ! Name of this routine
!
      INTEGER :: IANA ! Analyte index
      INTEGER :: ILOC ! Location index
      INTEGER :: ITIM ! Time index
      INTEGER :: ISPC ! Species index
      INTEGER :: IDX
!      INTEGER :: I    ! Looping index
!
!     Stuff from the data file to ensure data index matching
      CHARACTER(LEN=6) :: TMP_ID
      INTEGER :: NREC
!
      LOGICAL :: MOREDATA ! .True. if DATA found; .false. otherwise
      LOGICAL :: STOCDATA ! .True. if stochastic data; .false. otherwise
      INTEGER :: ICNT     ! Counter for DATA data
!
      INTEGER :: VIDX ! Local index for a stochastic variable
      CHARACTER(LEN=24) :: CTMP ! Temporary variable ID
!
!     Stuff from the data file to ensure data index matching
      REAL, ALLOCATABLE :: TMP_VALUES(:) ! Data values from file
      INTEGER ::           TMP_YEAR      ! Year from file
!
      INTEGER :: IERA ! Error status variable from the allocate action
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
      ILINE = 0 ! Initialize for RDBLK
      ICNT = 0
!
!     Allocate work space for values
      ALLOCATE( DATA_VALUES(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Error allocating memory for DATA_VALUES'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( TMP_VALUES(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error allocating memory for TMP_VALUES'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Loop over all data points
!
   10 CONTINUE
!
! ***   Read the keywords to get the FOOD data
        CALL KEY_FILL_3( MOREDATA, STOCDATA, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!
! ***   Determine whether there were any data to process
        IF( .NOT.MOREDATA ) RETURN
!
        ICNT = ICNT + 1
!
! ***   Match the data with the correct analyte data file
        CALL MATCH_ANA( DATA_ANA_ID, IANA )
        IF( IANA .LT. 1 ) THEN
          IERR = 3
          MESSAG(1) = 'Mismatch on the analyte ID'
          MESSAG(2) = 'Processing analyte with ID = ' // DATA_ANA_ID
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
! ***   Match the data with a location index
        CALL MATCH_LOC( DATA_LOC_ID, ILOC )
        IF( ILOC .LT. 1 ) THEN
          IERR = 4
          MESSAG(1) = 'Mismatch on the location ID'
          MESSAG(2) = 'Processing location with ID = ' // DATA_LOC_ID
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
! ***   Match the data with a time index
        CALL MATCH_TIM( DATA_YEAR, ITIM )
        IF( ITIM .LT. 1 ) THEN
          IERR = 5
          MESSAG(1) = 'Mismatch on the time index'
          MESSAG(2) = 'Processing time with value = '
          WRITE(MESSAG(2)(30:),'(I0)') DATA_YEAR
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
! ***   Match the data with a species index
        CALL MATCH_SPC( DATA_SPC_ID, ISPC )
        IF( ISPC .GT. 0 ) THEN
          ESD_SPC(ISPC)%COMP = .TRUE.
        ELSE
          IERR = 6
          MESSAG(1) = 'Species requested that is not in the master list'
          MESSAG(2) = 'Species ID is '// DATA_SPC_ID
          MESSAG(3) = 'Problem in the ECEM keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
!
! ***   Get the record index for the data location
!
        NREC = -1
        SELECT CASE ( ESD_LOC(ILOC)%TYPE )
!
          CASE( 'UPLAND' )
            IF( DATA_STY_IDX.LT.1 .OR. DATA_STY_IDX.GT.3 ) THEN
              IERR = 7
              MESSAG(1) = 'Soil index is not one of the values 1, 2, or 3'
              MESSAG(2) = 'Location ID (assumed to be upland) is '//TRIM(DATA_LOC_ID)
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, DATA_STY_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 8
              MESSAG(1) = 'Error in lower level routine - seeking UPLAND'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE( 'RIPARIAN' )
            CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC,  ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 9
              MESSAG(1) = 'Error in lower level routine - seeking RIPARIAN'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE( 'AQUATIC' )
            CALL FCDA_RECNO_AQ( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 10
              MESSAG(1) = 'Error in lower level routine - seeking AQUATIC'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
          CASE DEFAULT
            IERR = 11
            MESSAG(1) = 'Invalid location type'
            MESSAG(2) = 'Data had "'//TRIM(ESD_LOC(ILOC)%TYPE)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
!
        END SELECT
!
!       Check solution was available
        IF( NREC .LT. 0 ) THEN
          IERR = 12
          MESSAG(1) = 'NREC was unexpectedly less than zero'
          MESSAG(2) = 'Food data record index is not defined properly'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!       WRITE(*,*) ' NREC = ',NREC
!
! ***   Read the old data to ensure the correct data indices have been used
        CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, TMP_VALUES, NREAL, IFOD(IANA,ISPC), IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!
! ***   Check for a data match
        IF( TMP_YEAR .NE. DATA_YEAR ) THEN
          IERR = 13
          MESSAG(1) = 'Mismatch on times with the FCDA data file'
          MESSAG(2) = 'Expecting year '
          WRITE(MESSAG(2)(17:),'(I0)') DATA_YEAR
          MESSAG(3) = 'Obtained year '
          WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
          MESSAG(4) = 'Record number'
          WRITE(MESSAG(4)(17:),'(I0)') NREC
          MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
          CALL PRTERR( IERR, CALLER, 5 )
          RETURN
        END IF
!
        IF( TMP_ID .NE. DATA_LOC_ID ) THEN
          IERR = 14
          MESSAG(1) = 'Mismatch on locations with the FCDA data file'
          MESSAG(2) = 'Expecting ID ' // DATA_LOC_ID
          MESSAG(3) = 'Obtained ID  ' // TMP_ID
          MESSAG(4) = 'Record number'
          WRITE(MESSAG(4)(17:),'(I0)') NREC
          MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
          CALL PRTERR( IERR, CALLER, 5 )
          RETURN
        END IF
!
! ***   Generate stochastic values if this DATA entry is stochastic
        IF( STOCDATA ) THEN
!
          IF( SDSTOC .LE. 1.0D0 ) THEN
            IERR = 15
            MESSAG(1) = 'Random seed must be greater than 1.0D0 (SEED)'
            MESSAG(2) = 'Check if the SEED keyword is entered'
            MESSAG(3) = 'Problem encountered in the Human keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
          CTMP = DATA_ANA_ID//DATA_LOC_ID//DATA_STY_ID
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, DATA_VALUES, IRPT, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            MESSAG(2) = 'Variable is ' // TRIM(CTMP)
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        END IF
!
! ***   Write the data to the FCDA file
        CALL FCDA_WRITE( NREC, DATA_YEAR, DATA_LOC_ID, DATA_VALUES, NREAL, IFOD(IANA,ISPC), IERR )
!
        IF( DEBUG ) THEN
          WRITE(IRPT,2050) ICNT
 2050     FORMAT(/'DATA keyword number in the input file is ',I0)
          WRITE(IRPT,2010) TRIM(FNFOD(IANA,ISPC))
 2010     FORMAT('Food file = "',A,'"')
          WRITE(IRPT,2020) NREC
 2020     FORMAT('Record number is ',I0)
          WRITE(IRPT,2030)
 2030     FORMAT('Real, Old Data, New Data')
          DO IDX = 1, NREAL
            WRITE(IRPT,2040) IDX, TMP_VALUES(IDX), DATA_VALUES(IDX)
 2040       FORMAT(3X,I0,1X,1P,E14.7,1X,E14.7)
          END DO
        END IF
!
! ***   Go see if there is another DATA keyword to process
        GO TO 10
!
      END SUBROUTINE

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
! *** Force explicit typing of variables
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=16) :: CALLER = 'READ_KEYS_REPORT' ! Name of this subroutine
      CHARACTER(LEN=LENCRD) :: TITLE ! Temporary input line from RDBLK
!
!---- Executable code --------------------------------------------
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
!         Get a unit number
          IRPT = GET_UNIT_NUMBER(  )
          IF( IRPT .LT. 7 ) THEN
            WRITE(*,*) 'Unable to get a unit number for the report file'
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
! *** Force explicit typing of variables
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
!---- Executable code --------------------------------------------
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

