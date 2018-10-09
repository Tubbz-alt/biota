!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
MODULE Control_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains control variables
!
!  History:
!
!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!    Paul W. Eslinger :  4 Jun 2007 : Rerganize data and add infiltration file logic
!
      CHARACTER(LEN=200) :: PTITLE ! Title for this program run
!
      INTEGER :: NREAL      ! Number of realizations
      LOGICAL :: DEBUG_ECDA ! Flag for ECDA debug writes to the report file
!
      INTEGER :: NUM_ANA ! Number of analytes
      INTEGER :: NUM_LOC ! Number of locations
      INTEGER :: NUM_TIM ! Number of times
!
      LOGICAL :: ECHO_DILUTE ! Flag for echo of DILUTE details to the report file
      LOGICAL :: ECHO_KDSOIL ! Flag for echo of KDSOIL details to the report file
      LOGICAL :: ECHO_INFILT ! Flag for echo of INFILTRATION details to the report file
!
      LOGICAL :: CREATE_DILUTE ! Flag for creating a DILUTE data library
      LOGICAL :: CREATE_KDSOIL ! Flag for creating a KDSOIL data library
      LOGICAL :: CREATE_INFILT ! Flag for creating a INFILTRATION data library
!
      CHARACTER(LEN= 6), ALLOCATABLE :: ANA_ID(:)   ! Analyte IDs
      CHARACTER(LEN=72), ALLOCATABLE :: ANA_DESC(:) ! Analyte descriptions
      CHARACTER(LEN= 2), ALLOCATABLE :: ANA_TYPE(:) ! Analyte types
!
      INTEGER :: YEAR_START ! Year that the simulation starts
      INTEGER :: YEAR_STOP  ! Year that the simulation stops
      INTEGER :: YEAR_CLOSE ! Year of site closure
!
!     Type definition for ESD location data
!
      TYPE ESD_LOC_TYPE
        CHARACTER(LEN= 8) :: TYPE  ! Location type (AQUATIC, RIPARIAN, or UPLAND)
        CHARACTER(LEN= 7) :: LOCUS ! Location locus from the ESD file (HANFORD, FARSIDE)
        REAL :: EASTING  ! Easting coordinate in state plane projection
        REAL :: NORTHING ! Northing coordinate in state plane projection
        REAL :: AREA     ! Impact location area (sq. m)
        REAL :: MILE     ! Columbia River mile
        LOGICAL :: GWAT  ! GWAT - Media active flag
        LOGICAL :: SEEP  ! SEEP - Media active flag
        LOGICAL :: SWAT  ! SWAT - Media active flag
        LOGICAL :: PWAT  ! PWAT - Media active flag
        LOGICAL :: SEDI  ! SEDI - Media active flag
        LOGICAL :: SORP  ! SORP - Media active flag
        LOGICAL :: SODR  ! SODR - Media active flag
        LOGICAL :: SOGW  ! SOGW - Media active flag
        LOGICAL :: SOSW  ! SOSW - Media active flag
        LOGICAL :: AIRC  ! AIRC - Media active flag
        LOGICAL :: AIRD  ! AIRD - Media active flag
        CHARACTER(LEN= 6) :: ID_SWAT ! ID for a surface water location for irrigation
      END TYPE ESD_LOC_TYPE
      TYPE (ESD_LOC_TYPE), ALLOCATABLE :: ESD_LOC(:) ! The ESD location variable
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Dilute_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to Dilute Data
!
!  History:
!
!    Paul W. Eslinger : 29 Mar 2000 : Version 1.0
!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!
      INTEGER :: DF_NITR                ! Number of DILUTE realizations
      INTEGER :: DF_INDSTO              ! Number of stochastic DILUTE keywords
      INTEGER :: DF_INDTBL              ! Number of stochastic table entries for DILUTE keywords
      REAL(KIND=8) :: DF_SEED           ! Random number seed
      CHARACTER(LEN=200) :: DF_PTITLE   ! Problem title line from generating program
      CHARACTER(LEN= 10) :: DF_PRGNAM   ! Program name of generating program
      CHARACTER(LEN=  8) :: DF_PRGVER   ! Program version number of generating program
      CHARACTER(LEN= 12) :: DF_PRGDAT   ! Program date of generating program
      CHARACTER(LEN= 16) :: DF_USRNAM   ! User name from generating program
      CHARACTER(LEN= 14) :: DF_CRUNID   ! Run identification number from generating program
!
      CHARACTER(LEN= 20), ALLOCATABLE :: DF_UNITS(:) ! Units for dilution
      REAL, ALLOCATABLE :: DF_SOIL(:,:) ! (DF_INDSTO,DF_NITR) Array of stochastic dilutions
      CHARACTER(LEN= 20), ALLOCATABLE :: DF_ID(:) ! (DF_INDSTO) Stochastic IDs for the calculations
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Files_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This module contains file names and unit numbers
!
!  History:
!    Paul W. Eslinger : 29 Mar 2000 : Version 1.0
!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!    Paul W. Eslinger : 10 Aug 2006 : Add create flags for map and header files
!
      INTEGER, PARAMETER :: MAXFN = 200  ! Length of a file name
      CHARACTER(LEN=MAXFN) :: FNTMP      ! Temporary file name
      CHARACTER(LEN=MAXFN) :: FNRPT      ! Name of the report file
      CHARACTER(LEN=MAXFN) :: FNKEY      ! Name of the input keyword file
      CHARACTER(LEN=MAXFN) :: FN_ECDA    ! File name for the ECDA file
      CHARACTER(LEN=MAXFN) :: FN_MAP     ! File name for the ECDA record map data
!      CHARACTER(LEN=MAXFN) :: FN_HDR     ! File name for the SACView header data
!      LOGICAL :: CREATE_HDR              ! Header file creation flag
      LOGICAL :: CREATE_MAP              ! Map (index) file creation flag
!
      INTEGER :: IRPT       ! Unit number for the report file
      INTEGER :: IKEY       ! Unit number for the input keyword file
!      INTEGER :: UN_HDR     ! Unit number for the SACView header file
      INTEGER :: UN_MAP     ! Unit number for the ECDA map file
      INTEGER :: UN_ECDA    ! Unit number for the ECDA file
!
      INTEGER :: IKDS                   ! Unit number for the KdSoil data file
      CHARACTER(LEN=MAXFN) :: FN_KDSOIL ! File name for KdSoil data
!
      INTEGER :: IDIL                   ! Unit number for the Dilute data file
      CHARACTER(LEN=MAXFN) :: FN_DILUTE ! File name for Dilute data
!
      INTEGER :: INFL                   ! Unit number for the infiltration data file
      CHARACTER(LEN=MAXFN) :: FN_INFILT ! File name for infiltration data
!
!     File names for concentrations
      CHARACTER(LEN=MAXFN), ALLOCATABLE :: FN_CON(:) ! (Dimension by analyte)
      LOGICAL, ALLOCATABLE :: FN_CON_CREATE(:) ! (Dimension by analyte)
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
MODULE Iden_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains run and user identification information
!
!  History:
!
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
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
END MODULE

MODULE Infilt_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to infiltration data
!
!  History:
!
!    Paul W. Eslinger :  4 Jun 2007 : Original data
!
      INTEGER :: IN_NITR                ! Number of infiltration realizations
      INTEGER :: IN_INDSTO              ! Number of stochastic infiltration keywords
      INTEGER :: IN_INDTBL              ! Number of stochastic table entries for infiltration keywords
      REAL(KIND=8) :: IN_SEED           ! Random number seed
      CHARACTER(LEN=200) :: IN_PTITLE   ! Problem title line from generating program
      CHARACTER(LEN= 10) :: IN_PRGNAM   ! Program name of generating program
      CHARACTER(LEN=  8) :: IN_PRGVER   ! Program version number of generating program
      CHARACTER(LEN= 12) :: IN_PRGDAT   ! Program date of generating program
      CHARACTER(LEN= 16) :: IN_USRNAM   ! User name from generating program
      CHARACTER(LEN= 14) :: IN_CRUNID   ! Run identification number from generating program
!
      CHARACTER(LEN= 20), ALLOCATABLE :: IN_UNITS(:) ! Units for infiltration data
      REAL, ALLOCATABLE :: IN_STOC(:,:) ! (IN_INDSTO,UN_NITR) Array of stochastic infiltrations
      CHARACTER(LEN= 20), ALLOCATABLE :: IN_ID(:) ! (IN_INDSTO) Stochastic IDs for the calculations
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE KdSoil_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to KdSoil Data
!
!  History:
!
!    Paul W. Eslinger : 29 Mar 2000 : Version 1.0
!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!
      INTEGER :: KD_NITR                ! Number of KDSOIL realizations
      INTEGER :: KD_INDSTO              ! Number of stochastic KDSOIL keywords
      INTEGER :: KD_INDTBL              ! Number of stochastic table entries for KDSOIL keywords
      REAL(KIND=8) :: KD_SEED           ! Random number seed
      CHARACTER(LEN=200) :: KD_PTITLE   ! Problem title line from generating program
      CHARACTER(LEN= 10) :: KD_PRGNAM   ! Program name of generating program
      CHARACTER(LEN=  8) :: KD_PRGVER   ! Program version number of generating program
      CHARACTER(LEN= 12) :: KD_PRGDAT   ! Program date of generating program
      CHARACTER(LEN= 16) :: KD_USRNAM   ! User name from generating program
      CHARACTER(LEN= 14) :: KD_CRUNID   ! Run identification number from generating program
!
      REAL, ALLOCATABLE :: KD_SOIL(:,:) ! (KD_INDSTO,KD_NITR) Array of stochastic KD values
      CHARACTER(LEN= 20), ALLOCATABLE :: KD_ID(:) ! (KD_INDSTO) Stochastic IDs for the calculation
      CHARACTER(LEN= 20), ALLOCATABLE :: KD_UNITS(:) ! Units for KDSOIL   CA
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

      PROGRAM ECDA
!!**************************************************************************************************
!!
!!                                     ECDA
!!          Environmental Concentration Data Accumulator Utility Routines
!!              Battelle, Pacific Northwest National Laboratory
!!                           Richland, Washington
!!
!!**************************************************************************************************
!!
!!  ECDA is the top level routine for environmental concentration data file
!!  handling of the Toolkit for Integrated Impacts Assessments (TIIA).
!!
!!  This code does the following things
!!    1. Creates and initializes environmental concentration data (ECDA) files
!!    2. Writes a stochastic data library of soil-water Kd values
!!    3. Writes a stochastic data library of dilution factors for groundwater
!!       and river water interactions in the riparian zone.
!!    4. Writes a library of infiltration rate values for the vadose zone soil
!!
!! Reference:
!!
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!**************************************************************************************************
!!
!!  Language:
!!    This program is written in Fortran 95 (free source form)
!!
!!  Module History:
!!    Kelly Lessor & Paul W. Eslinger : 11 May 2000 : Version 1.0
!!    Paul W. Eslinger : 11 Oct 2002 : SAC Rev. 1 Version
!!    Paul W. Eslinger : 10 Jan 2003 : Add units to KDSOIL and DILUTE
!!                                     Generalize FILLECDA actions
!!    Paul W. Eslinger : 17 Jun 2003 : Revise IERR in ECDA_OPEN
!!    Paul W. Eslinger : 25 Jun 2003 : Enhance error checking on ESD
!!                                     location keywords
!!    Paul W. Eslinger : 26 Jun 2003 : Add easting and northing
!!    Paul W. Eslinger : 16 Oct 2003 : Upgrade error messages
!!    Paul W. Eslinger :  1 Sep 2004 : Upgrade error messages
!!    Paul W. Eslinger : 22 Sep 2004 : Add MILE to location information
!!    Paul W. Eslinger : 13 Oct 2004 : Add surface water irrigation information
!!    Paul W. Eslinger : 21 Mar 2005 : Output location area information
!!    Paul W. Eslinger :  5 Jan 2006 : Change concentration file error checks
!!                                     Add TELLTIME logic
!!    Paul W. Eslinger :  8 Sep 2006 : (SCR-1141) Add ECDA file size check
!!                                     Implement COMPUTE flag logic for files
!!    Paul W. Eslinger : 13 Sep 2006 : (SCR-1142) Update statistics routines
!!    Paul W. Eslinger : 18 Sep 2006 : (SCR-1145) Fix file size error check
!!    Paul W. Eslinger :  4 Jun 2007 : Add infiltration file generation logic
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!    Paul W. Eslinger : 11 Jul 2012 : Change LOCUS error detection to eliminate HANFORD
!!    Paul W. Eslinger :  7 Feb 2014 : (TIIA SCR-0004) Remove the SACVIEW header files
!!
!!**************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Iden_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Ecda_Mod
      USE Control_Mod
!
      IMPLICIT NONE
!
! *** Local variables
      CHARACTER(LEN=4) :: CALLER = 'ECDA' ! Name of this routine
      INTEGER :: IERR  ! Error number
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Program run identification, and initialization
      CALL IDEN_SET( )
      CALL INIT( )
!
! *** Open the ECDA report file (fixed name)
      CALL OPEN_REPORT( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
      CALL BANNER_1( )
!
! *** Open the Environmental Settings Definition (ESD) keyword file
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the keyword file'
        WRITE(*,*) 'Stop in '//CALLER
        STOP
      END IF
!
! *** Scan the ESD keyword file for controlling information
      CALL TELLTIME( 'Reading ESD keywords - Pass #1', 'SCREEN', .FALSE. )
      CALL SCAN_KEYS_1( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
! *** Check stuff on the initial set of keywords
      CALL CHECK_KEYS_1( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
! *** Allocate memory for ESD keyword information
      CALL TELLTIME( 'Allocating memory', 'SCREEN', .FALSE. )
      CALL ECDA_ALLOC( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
! *** Get the analyte definitions from the keyword file
!     (File name storage depends on analyte information)
      CALL TELLTIME( 'Reading ESD keywords - Pass #2', 'SCREEN', .FALSE. )
      CALL SCAN_KEYS_2( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
! *** Get the locations, times, and ECDA analyte-specific file
!     names from the ESD keyword file
      CALL TELLTIME( 'Reading ESD keywords - Pass #3', 'SCREEN', .FALSE. )
      CALL SCAN_KEYS_3( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
! *** Check stuff on the final set of ESD keywords
      CALL CHECK_KEYS_2( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
! *** Finish writing the banner page
      CALL BANNER_2( )
!
! *** Echo the problem definition to the report file
      CALL ECHO( )
!
! *** Process ECDA-type actions
!     (Record map index file and the concentration files)
      CALL TELLTIME( 'Starting ECDA actions', 'SCREEN', .FALSE. )
      CALL PROCESS_ECDA( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
! *** Define and write the header file
!      CALL TELLTIME( 'Processing the SACView header file', 'SCREEN', .FALSE. )
!      IF( CREATE_HDR ) THEN
!        CALL PROCESS_HEADER( IERR )
!        IF(IERR .NE. 0) THEN
!          IERR = 999
!          GO TO 9999
!        END IF
!      END IF
!
! *** Define and output the KDSOIL data library
      CALL TELLTIME( 'Processing KDSOIL actions', 'SCREEN', .FALSE. )
      CALL PROCESS_KDSOIL( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
! *** Define and output the DILUTE data library
      CALL TELLTIME( 'Processing DILUTE actions', 'SCREEN', .FALSE. )
      CALL PROCESS_DILUTE( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
! *** Define and output the DILUTE data library
      CALL TELLTIME( 'Processing INFILTRATION actions', 'SCREEN', .FALSE. )
      CALL PROCESS_INFILT( IERR )
      IF(IERR .NE. 0) THEN
        IERR = 999
        GO TO 9999
      END IF
!
! *** Branch point for all error terminations (as well as nominal termination)
 9999 CONTINUE
!
      IF( IERR .EQ. 0 ) THEN
        MESSAG(1) = 'Normal Termination'
        CALL PRTERR( IERR, CALLER, 1)
        WRITE(*,*) 'Normal Termination'
        STOP
      ELSE
        MESSAG(1) = 'Abnormal Run Termination Due to Errors'
        CALL PRTERR( IERR, CALLER, 1)
        WRITE(*,*) 'Abnormal Run Termination Due to Errors'
        STOP
      END IF
!
      END PROGRAM

      SUBROUTINE BANNER_1( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints an initial banner page to the report file.
!!
!!  History:
!!    Paul W. Eslinger : 25 Apr 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Add infiltration file generation logic
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod, ONLY: IRPT
!
      IMPLICIT NONE
!
!---- Executable code ---------------------------------------------------
!
      WRITE(IRPT,'(/)')
!
      WRITE(IRPT,7010) 'EEEEEEE  CCCCC  DDDDDD    AAA  '
      WRITE(IRPT,7010) 'E       C     C D     D  A   A '
      WRITE(IRPT,7010) 'E       C       D     D A     A'
      WRITE(IRPT,7010) 'EEEEE   C       D     D AAAAAAA'
      WRITE(IRPT,7010) 'E       C       D     D A     A'
      WRITE(IRPT,7010) 'E       C     C D     D A     A'
      WRITE(IRPT,7010) 'EEEEEEE  CCCCC  DDDDDD  A     A'
 7010 FORMAT(24X,A)
!
      WRITE(IRPT,7020) PRGNAM, PRGVER, PRGDAT
 7020 FORMAT(//25X,A,'   Version ',A/25X,'Last Modified on ',A)
!
      RETURN
      END SUBROUTINE

      SUBROUTINE BANNER_2( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints more banner information to the report file.
!!
!!  History:
!!    Paul W. Eslinger : 21 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global valriables
      USE Iden_Mod
      USE Qa_Mod
      USE Files_Mod, ONLY: IRPT
!
      IMPLICIT NONE
!
!---- First executable code --------------------------------------------
!
! *** Identification information
!
      WRITE(IRPT,1000) CRUNID, USRNAM
 1000 FORMAT(/10X,'Current Run ID = ',A,'   User Name = ',A)
!
      WRITE(IRPT,1010) SYSDAT, SYSTIM
 1010 FORMAT(17X,'System Date = ',A10,'  System Time = ',A8)
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

      SUBROUTINE CHECK_KEYS_1( IERR )
!!*******************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks the set of keywords read by SCAN_KEYS_1 for
!!    consistency errors.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  5 Jan 2005 : Change error messages
!!
!!*******************************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Errors_Mod
      USE Ecda_Mod
      USE Control_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: IERR  ! Error number indicator
!
! *** Local variables
!
      CHARACTER(LEN=12) :: CALLER = 'CHECK_KEYS_1'
      INTEGER :: ILEN  ! Temporary length variable
      INTEGER :: IMED  ! Temporary looping variable
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check on the problem title
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'Problem title is blank'
        MESSAG(2) = 'Use the TITLE keyword to enter a problem title'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Check on the user name
!
      IF( USRNAM .EQ. 'Anonymous User' ) THEN
        IERR = 2
        MESSAG(1) = 'The user name was not entered'
        MESSAG(2) = 'Use the USER keyword to enter a user name'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Check on the number of realizations
!
      IF( NREAL .LT. 1 ) THEN
        IERR = 3
        MESSAG(1) = 'Number of realizations must be at least 1'
        MESSAG(2) = 'Value = '
        WRITE(MESSAG(2)(9:),'(I0)') NREAL
        MESSAG(3) = 'Use the REALIZATION keyword to enter a value'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Check on the number of locations
!
      IF( NUM_LOC .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'At least one location must be defined'
        MESSAG(2) = 'Use the LOCATION keyword to enter a location definition'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Check on the number of times
!
      IF( NUM_TIM .LT. 1 ) THEN
        IERR = 5
        MESSAG(1) = 'At least one time must be entered'
        MESSAG(2) = 'Use the TIMES keyword to enter solution times'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Check on the number of analytes
!
      IF( NUM_ANA .LT. 1 ) THEN
        IERR = 6
        MESSAG(1) = 'At least one analyte must be defined'
        MESSAG(2) = 'Use the ANALYTE keyword to enter analyte definitions'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Check the period times
!
      IF( YEAR_START .LT. 0 ) THEN
        IERR = 7
        MESSAG(1) = 'Invalid start year for the simulation'
        MESSAG(2) = 'Check the modifier START on the PERIOD keyword'
        MESSAG(3) = 'Value = '
        WRITE(MESSAG(3)(9:),'(I0)') YEAR_START
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( YEAR_STOP .LT. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Invalid stop year for the simulation'
        MESSAG(2) = 'Check the modifier STOP on the PERIOD keyword'
        MESSAG(3) = 'Value = '
        WRITE(MESSAG(3)(9:),'(I0)') YEAR_STOP
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( YEAR_STOP .LT. YEAR_START ) THEN
        IERR = 9
        MESSAG(1) = 'Invalid stop and stop year combination for the simulation'
        MESSAG(2) = 'Check the modifiers START and STOP on the PERIOD keyword'
        MESSAG(3) = 'Start year = '
        WRITE(MESSAG(3)(14:),'(I0)') YEAR_START
        MESSAG(3) = TRIM(MESSAG(3)) // ' stop year ='
        ILEN = LEN_TRIM(MESSAG(3))
        WRITE(MESSAG(3)((ILEN+1):),'(I0)') YEAR_STOP
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( YEAR_CLOSE .LT. 0 ) THEN
        IERR = 10
        MESSAG(1) = 'Invalid site closure year for the simulation'
        MESSAG(2) = 'Check the modifier CLOSURE on the PERIOD keyword'
        MESSAG(3) = 'Value = '
        WRITE(MESSAG(3)(9:),'(I0)') YEAR_CLOSE
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Check for valid fill options
      DO IMED = 1, ECDA_NMED
!       FILL(IMED) must have a value of 1 (FIXED) or 2 (RANDOM)
        IF(.NOT.(FILL(IMED).EQ.1 .OR. FILL(IMED).EQ.2)) THEN
          IERR = 11
          MESSAG(1) = 'An internal fill media index is not properly defined'
          MESSAG(2) = 'Media with a problem is ' // ECDA_ID_MED(IMED)
          MESSAG(3) = 'The modifier FIXED or RANDOM must be used on the FILLECDA keyword'
          CALL PRTERR( IERR, CALLER, 3 )
        ELSE
          IF( FILL(IMED) .EQ. 2 ) THEN
!           Check for a valid value for the seed for the random number generator
            IF( FILL_VALUE(IMED).LT.1 .OR. FILL_VALUE(IMED).GT.2147483646.0d0) THEN
              IERR = 12
              MESSAG(1) = 'The stochastic seed for filling media with random values is invalid'
              MESSAG(2) = 'The value must be in the range 1 to 999999'
              MESSAG(3) = 'Change the value with the RANDOM modifier on the FILLECDA keyword'
              MESSAG(4) = 'Media with a problem is ' // ECDA_ID_MED(IMED)
              MESSAG(5) = 'Value: '
              WRITE(MESSAG(5)(8:),*) FILL_VALUE(IMED)
              CALL PRTERR( IERR, CALLER, 5 )
            END IF
          END IF
        END IF
      END DO
!
      RETURN
      END SUBROUTINE

      SUBROUTINE CHECK_KEYS_2( IERR )
!!*******************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks the keywords for additional consistency errors.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  5 Jan 2005 : Change file name checks
!!    Paul W. Eslinger : 10 Aug 2006 : (SCR-1141) Add check on ECDA file sizes
!!    Paul W. Eslinger : 2006 : (SCR-1141) Add check on ECDA file sizes
!!
!!*******************************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Ecda_Mod
      USE Control_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: IERR  ! Error number indicator
!
! *** Local variables
!
      CHARACTER(LEN=12) :: CALLER = 'CHECK_KEYS_2'
      CHARACTER(LEN=6) :: TMP_ID ! Temporary location ID
      INTEGER :: IANA  ! Analyte looping index
      INTEGER :: ITIM  ! Time looping index
      INTEGER :: ILOC  ! Location looping index
      INTEGER :: ILEN  ! Temporary length variable
      INTEGER :: LIDX  ! Temporary location index
!
      REAL :: FILE_SIZE ! Temporary calculation of file size
      REAL :: MAX_SIZE  ! Temporary file size maximum
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check the impact times
!
      DO ITIM = 1, NUM_TIM
        IF( ECDA_TIMES(ITIM).LT.YEAR_START .OR. ECDA_TIMES(ITIM).GT.YEAR_STOP ) THEN
          IERR = 1
          MESSAG(1) = 'Invalid impact time detected'
          MESSAG(2) = 'Invalid time is '
          WRITE(MESSAG(2)(17:),'(I0)') ECDA_TIMES(ITIM)
          MESSAG(3) = 'Valid range for times is '
          WRITE(MESSAG(3)(26:),'(I0)') YEAR_START
          MESSAG(3) = TRIM(MESSAG(3)) // ' to'
          ILEN = LEN_TRIM(MESSAG(3))
          WRITE(MESSAG(3)((ILEN+2):),'(I0)') YEAR_STOP
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
        IF( ITIM .GT. 1 ) THEN
          IF( ECDA_TIMES(ITIM) .LE. ECDA_TIMES(ITIM-1) ) THEN
            IERR = 2
            MESSAG(1) = 'Invalid impact time detected - values must be in increasing order'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0)') 'Invalid time is at index ', ITIM
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0)') 'Invalid time is ', ECDA_TIMES(ITIM)
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END IF
      END DO
!
! *** Check that all analytes have a name defined for the associated ECDA file
!
      DO IANA = 1, NUM_ANA
!        IF( FN_CON_CREATE(IANA) .AND. FN_CON(IANA).EQ.' ' ) THEN
        IF( FN_CON(IANA) .EQ. ' ' ) THEN
          IERR = 3
          MESSAG(1) = 'An ECDA file name is missing'
          MESSAG(2) = 'Analyte is ' // TRIM(ANA_ID(IANA))
          MESSAG(3) = 'Modify the FILE keyword, C_ECDA modifier'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
      END DO
!
! *** Irrigation data for those locations where irrigation is requested
!
      DO ILOC = 1, NUM_LOC
!      
        IF( ESD_LOC(ILOC)%SOSW ) THEN
!
!         Check that a surface water location was defined for locations with surface water irrigation
          TMP_ID = ESD_LOC(ILOC)%ID_SWAT
          CALL MATCH_LOC( TMP_ID, LIDX )
          IF( LIDX.LT.1 .OR. LIDX.GT.NUM_LOC ) THEN
            IERR = 4
            MESSAG(1) = 'Invalid surface water source for an irrigated location'
            MESSAG(2) = 'Irrigated location ID is '//TRIM(ECDA_ID_LOC(ILOC))
            MESSAG(3) = 'Water source location ID is '//TRIM(ESD_LOC(ILOC)%ID_SWAT)
            MESSAG(4) = 'A value of -Null- means the source water location was not defined using the IRIG_SWAT modifier'
            MESSAG(5) = 'Any other ID means the source water location was not defined as a location'
            CALL PRTERR( IERR, CALLER, 5 )
          END IF
!
!         Check that the surface water media is available at requested locations
          IF( LIDX.GE.1 .AND. LIDX.LE.NUM_LOC ) THEN
            IF( .NOT. ESD_LOC(LIDX)%SWAT ) THEN
              IERR = 5
              MESSAG(1) = 'Invalid surface water source for an irrigated location'
              MESSAG(2) = 'Irrigated location ID is '//TRIM(ECDA_ID_LOC(ILOC))
              MESSAG(3) = 'Water source location ID is '//TRIM(ESD_LOC(ILOC)%ID_SWAT)
              MESSAG(4) = 'Surface water is not being calculated at the water source location'
              CALL PRTERR( IERR, CALLER, 4 )
            END IF
          END IF
        END IF
!
        IF( ESD_LOC(ILOC)%SOGW ) THEN
!         Check that the groundwater media is defined for locations with groundwater irrigation
          IF( .NOT. ESD_LOC(ILOC)%GWAT ) THEN
            IERR = 6
            MESSAG(1) = 'Invalid data for an irrigated location'
            MESSAG(2) = 'Irrigated location ID is '//TRIM(ECDA_ID_LOC(ILOC))
            MESSAG(3) = 'Groundwater must be calculated at this location'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END IF
!
      END DO
!
! *** Check that the ECDA file sizes aren't bigger than allowed for 32 bit addressing
!
      MAX_SIZE  = 2147483647.0  ! Set to (2^31) - 1
      FILE_SIZE = REAL(ECDA_BLOCK)*REAL(4+6+4+4*NREAL)*REAL(NUM_TIM) + 12.0*REAL(4+6+4+4*NREAL)
      IF( FILE_SIZE .GT. MAX_SIZE ) THEN
        IERR = 7
        MESSAG(1) = 'Invalid file size for ECDA files - the file is too large'
        MESSAG(2) = 'Reduce the problem size by changine the number of locations,'
        MESSAG(3) = 'number of saved media, number of realizations, or the number'
        MESSAG(4) = 'of output times.'
        CALL PRTERR( IERR, CALLER, 4 )
      END IF
!
      RETURN
      END SUBROUTINE

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
!!    Paul W. Eslinger : 30 May 2007 : Original source
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
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
      SUBROUTINE ECDA_ALLOC( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine either allocates memory needed by the ECDA
!!    routines.  Some allocated variables are also initialized.
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger : 26 Jun 2003 : Add ESD_LOC
!!
!!***********************************************************************
!
! *** Global variables
!
      USE ECDA_Mod
      USE Errors_Mod
      USE Control_Mod
      USE Files_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
!
      INTEGER :: ISTAT ! System status variable
      CHARACTER(LEN=10) :: CALLER = 'ECDA_ALLOC'
      INTEGER :: IANA  ! Analyte looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
      ALLOCATE( ECDA_TIMES(NUM_TIM), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Error allocating ECDA_TIMES(NUM_TIM)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ECDA_ID_LOC(NUM_LOC), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error allocating ECDA_ID_LOC(NUM_LOC)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ECDA_NM_LOC(NUM_LOC), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating ECDA_NM_LOC(NUM_LOC)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ESD_LOC(NUM_LOC), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating ESD_LOC(NUM_LOC)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ECDA_LOC_MED(NUM_LOC,ECDA_NMED), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating ECDA_LOC_MED(NUM_LOC,ECDA_NMED)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Allocate memory for the number of analytes
!
      ALLOCATE( ANA_ID(NUM_ANA), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating ANA_ID(NUM_ANA)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ANA_TYPE(NUM_ANA), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) = 'Error allocating ANA_TYPE(NUM_ANA)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ANA_DESC(NUM_ANA), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating ANA_DESC(NUM_ANA)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( FN_CON(NUM_ANA), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Error allocating FN_CON(NUM_ANA)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( FN_CON_CREATE(NUM_ANA), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 10
        MESSAG(1) = 'Error allocating FN_CON_CREATE(NUM_ANA)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      DO IANA = 1, NUM_ANA
        FN_CON(IANA) = ' '
        FN_CON_CREATE(IANA) = .FALSE.
      END DO
!
      RETURN
      END
!
      SUBROUTINE ECDA_FILL( FN_ECDA, UN_ECDA, NREAL, DEBUG_ECDA, IRPT, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine writes user specified constants or random uniform(0,1) 
!!    values to all media in the ECDA files.  These initialization values 
!!    written just after the file is created.
!!
!!  History:
!!
!!    Paul W. Eslinger : 29 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 26 Mar 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** User-defined functions
      REAL, EXTERNAL :: U01 ! Uniform random number generator
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: FN_ECDA ! File name for the ECDA file
      INTEGER, INTENT(IN)  :: UN_ECDA ! Unit number for the ECDA file
      INTEGER, INTENT(IN)  :: NREAL   ! Number of realizations
      LOGICAL :: DEBUG_ECDA           ! Flag for debug writes to the report file
      INTEGER, INTENT(IN)  :: IRPT    ! Report file number
      INTEGER :: IERR                 ! Error Number
!
! *** Local variables
!
      CHARACTER(LEN=9) :: CALLER = 'ECDA_FILL' ! Name of this routine
!
      INTEGER :: IOS       ! I/O Status
      INTEGER :: NREC      ! Index variable for records
      INTEGER :: NREC_TEST ! Index variable for records
      INTEGER :: ITIM      ! Index variable for times
      INTEGER :: ILOC      ! Index variable for locations
      INTEGER :: IMED      ! Index variable for media
      INTEGER :: IREL      ! Index variable for realizations
!
      INTEGER :: TIME            ! Calendar year for concentration data
      CHARACTER(LEN=6) :: LOC_ID ! Location ID
      CHARACTER(LEN=2) :: MED_ID ! Media ID
!
      REAL, ALLOCATABLE :: CONC(:) ! Concentration vector
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Open the ECDA file
!
      CALL ECDA_OPEN( FN_ECDA, UN_ECDA, IERR )
      IF ( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error opening ECDA file'
        CALL PRTERR(IERR, CALLER, 1)
        RETURN
      END IF
!
! *** Set up the concentration vector space
!
      ALLOCATE( CONC(NREAL), STAT=IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) ='Error allocating CONC(NREAL)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      IF( DEBUG_ECDA ) THEN
        WRITE(IRPT,1000) CALLER
 1000   FORMAT(//'Entering routine ',A)
      END IF
!
! *** Write uniform values to every data record in the ECDA file
!
      NREC = ECDA_NHEAD
      DO ITIM = 1, ECDA_NTIMES
        DO ILOC = 1, ECDA_NLOCS
          DO IMED = 1, ECDA_NMED
            IF ( ECDA_LOC_MED(ILOC,IMED) .GT. 0 ) THEN
              NREC = NREC + 1
!
              IF( FILL(IMED) .EQ. 1 ) THEN
                DO IREL = 1, NREAL
                  CONC(IREL) = FILL_VALUE(IMED)
                END DO
              ELSE
                DO IREL = 1, NREAL
                  CONC(IREL) = U01( FILL_VALUE(IMED) )
                END DO
              END IF
!
              CALL ECDA_WRITE( NREC, ECDA_TIMES(ITIM), ECDA_ID_LOC(ILOC), ECDA_ID_MED(IMED), CONC, NREAL, UN_ECDA, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 2
                MESSAG(1) = 'An error occurred writing the data to the ECDA file.'
                MESSAG(2) = 'File name is ' // TRIM(FN_ECDA)
                CALL PRTERR(IERR, CALLER, 2 )
                RETURN
              END IF
!
!             This block is for use in debugging
              IF( DEBUG_ECDA ) THEN
                WRITE(IRPT,1010) 'Original  ', NREC, ECDA_TIMES(ITIM), ECDA_ID_LOC(ILOC), &
                  ECDA_ID_MED(IMED), (CONC(IREL),IREL=1,NREAL)
 1010           FORMAT(A,I6,1X,I5,1X,A,1X,A,1P,100(1X,E11.4))
                CALL ECDA_READ( NREC, TIME, LOC_ID, MED_ID, CONC, NREAL, UN_ECDA, IERR )
                IF( IERR .NE. 0 ) THEN
                  IERR = 3
                  MESSAG(1) = 'An error occurred reading data from the ECDA file.'
                  MESSAG(2) = 'File name is ' // TRIM(FN_ECDA)
                  CALL PRTERR(IERR, CALLER, 2 )
                  RETURN
                END IF
                WRITE(IRPT,1010) 'ECDA_READ ', NREC, TIME, LOC_ID, MED_ID, (CONC(IREL),IREL=1,NREAL)
                CALL ECDA_RECNO( TIME, LOC_ID, MED_ID, NREC_TEST, IERR )
                IF( NREC .EQ. NREC_TEST ) THEN
                  WRITE(IRPT,1020) 'Record numbers match ', NREC, NREC_TEST,' using ECDA_RECNO'
 1020             FORMAT(A,I7,' and ',I7,A)
                ELSE
                  WRITE(IRPT,1020) 'Record numbers don''t match ', NREC, NREC_TEST,' using ECDA_RECNO'
                END IF
                CALL ECDA_RECNO_INDEX( ITIM, ILOC, IMED, NREC_TEST, IERR )
                IF( NREC .EQ. NREC_TEST ) THEN
                  WRITE(IRPT,1020) 'Record numbers match ', NREC, NREC_TEST,' using ECDA_RECNO_INDEX'
                ELSE
                  WRITE(IRPT,1020) 'Record numbers don''t match ', NREC, NREC_TEST,' using ECDA_RECNO_INDEX'
                END IF
              END IF
!
            END IF
          END DO
        END DO
      END DO
!
      DEALLOCATE( CONC )
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ECHO( )
!!*****************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes some problem definition information to
!!    the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger : 26 Jun 2003 : Add location information
!!    Paul W. Eslinger : 22 Sep 2004 : Add mile information
!!    Paul W. Eslinger : 10 Aug 2006 : Add header file output
!!    Paul W. Eslinger :  4 Jun 2007 : Add infiltration file generation logic
!!
!!*****************************************************************************
!
! *** Global valriables
!
      USE Iden_Mod
      USE Qa_Mod
      USE Files_Mod
      USE Control_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** Local variables
!
      INTEGER :: IANA ! Analyte looping control
      INTEGER :: ILOC ! Location looping control
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(//24('='),' Echo of the Problem Definition ',24('='))
!
! *** Problem Identification
!
      WRITE(IRPT,1010) PTITLE, USRNAM
 1010 FORMAT(/'Title: ',A/'User:  ',A)
!
      WRITE(IRPT,1020) TRIM(FNKEY)
 1020 FORMAT(/'Processing based on keyword data'/&
              '  File = ',A)
!
      WRITE(IRPT,1030) NREAL,   ' Realizations used'
      WRITE(IRPT,1030) NUM_LOC, ' Locations used'
      WRITE(IRPT,1030) NUM_TIM, ' Times used'
 1030 FORMAT(/I0,A)
!
! *** Analyte names and types
!
      WRITE(IRPT,1040) NUM_ANA
 1040 FORMAT(/'Analye Information based on ',I0,' analytes')
      DO IANA = 1, NUM_ANA
        WRITE(IRPT,1050) IANA, ANA_ID(IANA), ANA_TYPE(IANA), TRIM(ANA_DESC(IANA))
 1050   FORMAT(3X,I3,' : ',A,' : ',A,' : ',A)
      END DO
!
      WRITE(IRPT,1060) YEAR_START, YEAR_STOP, YEAR_CLOSE
 1060 FORMAT(/'Year definitions from the PERIOD keyword'/ &
        5X,I0,' : Start year for the simulation'/ &
        5X,I0,' : Stop  year for the simulation'/ &
        5X,I0,' : Site closure year for the simulation')
!
! *** Header file
!
!      IF( CREATE_HDR ) THEN
!        WRITE(IRPT,1070) TRIM(FN_HDR)
! 1070   FORMAT(/'File Name for ECDA Header File'/'File: ',A)
!      ELSE
!        WRITE(IRPT,'(/A)') 'Header file will not be created'
!      END IF
!
! *** Map (index) file
!
      IF( CREATE_MAP ) THEN
        WRITE(IRPT,1080) 'File Name for ECDA Index (Map) File', TRIM(FN_MAP)
 1080   FORMAT(/A,:,/'File: ',A)
      ELSE
        WRITE(IRPT,1080) 'ECDA Index (Map) File will NOT be created or modified', TRIM(FN_MAP)
      END IF
!
! *** KD soil file
!
      IF( CREATE_KDSOIL ) THEN
        WRITE(IRPT,1080) 'File Name for KDSOIL File', TRIM(FN_KDSOIL)
      ELSE
        IF( FN_KDSOIL .NE. ' ' ) THEN
          WRITE(IRPT,1080) 'KSDOIL File will NOT be created or modified', TRIM(FN_KDSOIL)
        ELSE
          WRITE(IRPT,1080) 'KSDOIL File will NOT be created or modified'
        END IF
      END IF
!
! *** Dilution factors file
!
      IF( CREATE_DILUTE ) THEN
        WRITE(IRPT,1080) 'File Name for DILUTE File', TRIM(FN_DILUTE)
      ELSE
        IF( FN_DILUTE .NE. ' ' ) THEN
          WRITE(IRPT,1080) 'DILUTE File will NOT be created or modified', TRIM(FN_DILUTE)
        ELSE
          WRITE(IRPT,1080) 'DILUTE File will NOT be created or modified'
        END IF
      END IF
!
! *** Infiltration data file
!
      IF( CREATE_INFILT ) THEN
        WRITE(IRPT,1080) 'File Name for Infiltration File', TRIM(FN_INFILT)
      ELSE
        IF( FN_INFILT .NE. ' ' ) THEN
          WRITE(IRPT,1080) 'Infiltration File will NOT be created or modified', TRIM(FN_INFILT)
        ELSE
          WRITE(IRPT,1080) 'Infiltration File will NOT be created or modified'
        END IF
      END IF
!
! *** Concentration files for each analyte
!
      DO IANA = 1, NUM_ANA
        WRITE(IRPT,1090) TRIM(ANA_ID(IANA)), TRIM(FN_CON(IANA))
 1090   FORMAT(/'File Name for Media Concentrations for analyte with ID="',A,'"'/'File: ',A)
        IF( .NOT. FN_CON_CREATE(IANA) ) THEN
          WRITE(IRPT,'(A)') 'File will not be created or initialized'
        END IF
      END DO
!
! *** Location information for error checking
!
      WRITE(IRPT,1100) &
        '"Location ID",TYPE,LOCUS,Easting,Northing,Mile,Area,GWAT,SEEP,SWAT,PWAT,SEDI,SORP,SODR,SOGW,SOSW,AIRC,AIRD,IRG_SWAT,Name'
 1100 FORMAT(/A)
      DO ILOC = 1, NUM_LOC
        WRITE(IRPT,1110) TRIM(ECDA_ID_LOC(ILOC)), TRIM(ESD_LOC(ILOC)%TYPE), TRIM(ESD_LOC(ILOC)%LOCUS), &
          ESD_LOC(ILOC)%EASTING, ESD_LOC(ILOC)%NORTHING, ESD_LOC(ILOC)%MILE, ESD_LOC(ILOC)%AREA, ESD_LOC(ILOC)%GWAT, &
          ESD_LOC(ILOC)%SEEP, ESD_LOC(ILOC)%SWAT, ESD_LOC(ILOC)%PWAT, ESD_LOC(ILOC)%SEDI, ESD_LOC(ILOC)%SORP, ESD_LOC(ILOC)%SODR, &
          ESD_LOC(ILOC)%SOGW, ESD_LOC(ILOC)%SOSW, ESD_LOC(ILOC)%AIRC, ESD_LOC(ILOC)%AIRD, ESD_LOC(ILOC)%ID_SWAT, &
          TRIM(ECDA_NM_LOC(ILOC))
 1110   FORMAT('"',A,'","',A,'","',A,'",',1P,E14.7,',',E14.7,',',E14.7,',',E14.7,11(',',L1),',"',A,'","',A,'"')
      END DO
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
!!    This subroutine is operating system independent.
!!
!!  History:
!!
!!    Paul W. Eslinger : 24 Jan 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Last modification
!!    Paul W. Eslinger : 29 Jun 2012 : Revise to use common callable routine
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
!
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
      PRGNAM = 'ECDA'
      PRGVER = '4.00.002'
!
! *** Program date (DD MMM YYYYY)
      PRGDAT = ' 7 Feb 2014'
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
!!    This subroutine initializes global variables.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  9 Mar 2001 : Version 1.0.A
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Add infiltration logic
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Files_Mod
      USE Errors_Mod
      USE Control_Mod
      USE KdSoil_Mod
      USE Dilute_Mod
      USE Infilt_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: IMED ! Looping variable
!
!---- Executable code ---------------------------------------------------
!
! *** Keyword file
!
      IKEY  = 7
      FNKEY = ' '
!
! *** Report file
!
      IRPT  = 10
      FNRPT = 'ecda.rpt'
!
! *** ECDA map file
!
      UN_MAP  = 11
      FN_MAP = ' '
      CREATE_MAP = .FALSE.
!
! *** ECDA concentration files
!
      UN_ECDA  = 12
      FN_ECDA = ' '
!
! *** Error message file
!
      IRPT_ERR = IRPT
!
! *** Problem title
!
      PTITLE = ' '
!
! *** Debug flag
!
      DEBUG_ECDA = .FALSE.
!
! *** Variables controlling filling the files with fake data
!
      DO IMED = 1, ECDA_NMED
        FILL(IMED) = -1
        FILL_VALUE(IMED) = -1.0
      END DO
!
! *** Header file information
!
!      FN_HDR = ' '
!      UN_HDR = 13
!      CREATE_HDR = .FALSE.
!
! *** KDSOIL variables
!
      KD_INDSTO  = 0
      KD_INDTBL  = 0
      KD_NITR    = 0
      KD_SEED    = 0.0D0
!
      IKDS = 8
      FN_KDSOIL = ' '
      ECHO_KDSOIL = .FALSE.
      CREATE_KDSOIL = .FALSE.
!
! *** DILUTE variables
!
      DF_INDSTO  = 0
      DF_INDTBL  = 0
      DF_NITR    = 0
      DF_SEED    = 0.0D0
!
      IDIL = 9
      FN_DILUTE = ' '
      ECHO_DILUTE = .FALSE.
      CREATE_DILUTE = .FALSE.
!
! *** INFILTRATION variables
!
      IN_INDSTO  = 0
      IN_INDTBL  = 0
      IN_NITR    = 0
      IN_SEED    = 0.0D0
!
      INFL = 14
      FN_INFILT = ' '
      ECHO_INFILT = .FALSE.
      CREATE_INFILT = .FALSE.
!
! *** Simulation control times
!
      YEAR_START = -999
      YEAR_STOP  = -999
      YEAR_CLOSE = -999
!
! *** Number of realizations
!
      NREAL = -999
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_LOC( LABEL, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a location ID
!!    in the ECDA module and a location label.  If found, the index
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
!!    Paul W. Eslinger : 27 Jan 2006 : Version 1.0
!!    Paul W. Eslinger :  4 Jun 2007 : Revise comments
!!
!!  Notes:
!!
!!    This version of MATCH_LOC is different than the versions in the
!!    suite of impacts codes.
!!
!!**********************************************************************
!
! *** Global variables
      USE Control_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
      INTEGER :: IDX
!
! *** Local variables
      INTEGER :: ILOC ! Looping variable
!
! *** Look for a match on the location names
!
      IDX = -1
      DO ILOC = 1, NUM_LOC
        IF( ECDA_ID_LOC(ILOC) .EQ. LABEL ) THEN
          IDX = ILOC
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE OPEN_KEY( IERR )
!!*********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine performs two actions for the keyword file:
!!      1. Obtain the file name and store it in the variable FNKEY
!!         a) First check the command line to see if the file
!!            name was provided or,
!!         b) query the user for the file name if it was not
!!            entered on the command line.
!!      2. Open the file on unit number IKEY for use by other subroutines
!!
!!  Note:
!!
!!    This subroutine does not call PRTERR when an error occurs because
!!    PRTERR writes to the report file.  The report file is not opened
!!    until after the keywords are read.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!!    Paul W. Eslinger : 30 May 2007 : Change structure to handle stochastic data
!!    Paul W. Eslinger : 29 Jun 2012 : Revise to a common callable QA routine
!!
!!*********************************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Iden_Mod
      USE Errors_Mod
!
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: NARGS ! Lahey utility subroutine
!
! *** Call list variables
      INTEGER :: IERR ! Error number variable
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'OPEN_KEY'
!
      INTEGER :: NUM_ARGS      ! Number of command line arguments
      INTEGER :: NUM_FNAM      ! Argument number for the keyword file name
      CHARACTER(LEN=5) :: HELP ! Character for checking for help
      LOGICAL :: THERE         ! File existence variable
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
 1010   FORMAT(/' Enter the control keyword file name > ')
        READ(*,*) FNKEY
      END IF
!
! *** Check if the requested file exists
!
      INQUIRE(FILE=FNKEY,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 1
        MESSAG(1) = 'The requested keyword file was not found'
        MESSAG(2) = 'File: ' // TRIM(FNKEY)
        CALL PRTERR( IERR, CALLER, 2 )
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
      IERR = 2
      MESSAG(1) = 'System error encountered opening the input keyword file'
      MESSAG(2) = 'File: ' // TRIM(FNKEY)
      CALL PRTERR( IERR, CALLER, 2 )
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE OPEN_REPORT( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger :  9 Mar 2001 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Revise comments
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in opening the report file
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number variable
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Attempt to open the report file
!
      OPEN(IRPT,FILE=FNRPT,STATUS='UNKNOWN',ERR=999)
!
      RETURN
!
! *** Error opening the file
!
  999 CONTINUE
!
      IERR = 1
      WRITE(*,1000) TRIM(FNRPT)
 1000 FORMAT(' System error encountered opening the report file'/' File: ',A)
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PROCESS_ECDA( IERR )
!!*******************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine schedules all actions needed for the ECDA routines
!!    including the following actions:
!!      Generating the index map file
!!      Creating media concentration files for all analytes
!!      Filling all media concentration files with -1 values or, optionally
!!      filling all media concentration files with Uniform(0,1) data
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  9 Mar 2001 : Version 1.0.A (File change)
!!    Paul W. Eslinger : 31 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Revise comments
!!
!!*******************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Ecda_Mod
      USE Control_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'PROCESS_ECDA' ! Name of this subroutine
      INTEGER :: IANA  ! Analyte looping index
!
!---- Executable code ---------------------------------------------------
!
! *** Set up some of the ECDA variables
!
      ECDA_RECLEN = 4 + 6 + 4 + 4*NREAL ! Int, Char*6, Char*4, NREAL reals
                                        ! time, location, media, concs
      ECDA_PRGNAM = PRGNAM
      ECDA_PRGVER = PRGVER
      ECDA_PRGDAT = PRGDAT
      ECDA_USRNAM = USRNAM
      ECDA_CRUNID = CRUNID
      ECDA_NREAL  = NREAL
      ECDA_NTIMES = NUM_TIM
      ECDA_NLOCS  = NUM_LOC
      ECDA_PTITLE = PTITLE
!
! *** Record map file generation ---------------------------------------------------------
!
      IF( CREATE_MAP ) THEN
!
        IF( FN_MAP .EQ. ' ' ) THEN
          IERR = 1
          MESSAG(1) = 'Index (map) file name is not provided'
          MESSAG(2) = 'Unexpected logic error'
          MESSAG(3) = 'Check the FILE keyword with the I_ECDA modifier'
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
!
        CALL TELLTIME( 'Generating the ECDA map file', 'SCREEN', .FALSE. )
        CALL ECDA_MAPOUT( FN_MAP, UN_MAP, IERR )
        IF (IERR .NE. 0) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!
        WRITE(IRPT,1020) TRIM(FN_MAP)
 1020   FORMAT(/'Record number map generated and written'/'  File Name = ',A)
        CALL ECDA_MAPECHO( IRPT )
!
        IF( DEBUG_ECDA ) THEN
          CALL TEST_MAPREAD( FN_MAP, UN_MAP, IERR )
          IF (IERR .NE. 0) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
        END IF
!
      END IF
!
! *** Create ECDA media files ----------------------------------------------------------------------
!
      WRITE(IRPT,'(/A)') 'Creation of media files is as follows:'
!
      DO IANA = 1, NUM_ANA
!
        IF( FN_CON_CREATE(IANA) ) THEN
!
          FN_ECDA = FN_CON(IANA)
!          CALL DATE_AND_TIME( SDATE, STIME )
!
          ECDA_ANALYTE = ' '
          ECDA_ANALYTE = TRIM(ANA_ID(IANA))
!
!         Set the units depending on analyte type
!
          IF( ANA_TYPE(IANA)(2:2) .EQ. 'R' ) THEN
            ECDA_UNITS( 1) = 'Ci/m^3'
            ECDA_UNITS( 2) = 'Ci/m^3'
            ECDA_UNITS( 3) = 'Ci/m^3'
            ECDA_UNITS( 4) = 'Ci/m^3'
            ECDA_UNITS( 5) = 'Ci/kg'
            ECDA_UNITS( 6) = 'Ci/kg'
            ECDA_UNITS( 7) = 'Ci/kg'
            ECDA_UNITS( 8) = 'Ci/kg'
            ECDA_UNITS( 9) = 'Ci/kg'
            ECDA_UNITS(10) = 'Ci/m^3'
            ECDA_UNITS(11) = 'Ci/m^2/yr'
          ELSE
            ECDA_UNITS( 1) = 'kg/m^3'
            ECDA_UNITS( 2) = 'kg/m^3'
            ECDA_UNITS( 3) = 'kg/m^3'
            ECDA_UNITS( 4) = 'kg/m^3'
            ECDA_UNITS( 5) = 'kg/kg'
            ECDA_UNITS( 6) = 'kg/kg'
            ECDA_UNITS( 7) = 'kg/kg'
            ECDA_UNITS( 8) = 'kg/kg'
            ECDA_UNITS( 9) = 'kg/kg'
            ECDA_UNITS(10) = 'kg/m^3'
            ECDA_UNITS(11) = 'kg/m^2/yr'
          END IF
!
!         Create the file
!
          CALL TELLTIME( 'Creating media file for analyte: '//TRIM(ANA_ID(IANA)), 'BOTH', .FALSE. )
          CALL ECDA_CREATE( FN_ECDA, UN_ECDA, IERR )
          IF (IERR .NE. 0) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!         Fill the file with initial values
!
          CALL TELLTIME( 'Initializing media file for analyte: '//TRIM(ANA_ID(IANA)), 'BOTH', .FALSE. )
          CALL ECDA_FILL( FN_ECDA, UN_ECDA, NREAL, DEBUG_ECDA, IRPT, IERR )
          IF (IERR .NE. 0) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
        END IF ! File create by analyte
!
      END DO ! Analytes loop
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PROCESS_DILUTE( IERR )
!!*******************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine schedules all actions needed for processing DILUTE keywords
!!    including the following actions:
!!      Creating the output file for DILUTE values
!!      Writing stochastic DILUTE data to the output file
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Add infiltration file generation logic
!!
!!*******************************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Errors_Mod
      USE Control_Mod
      USE Iden_Mod
      USE DILUTE_Mod
      USE Stats_Mod, ONLY: WORK, BG_STOC_DEFN, BG_STOC_STAT
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error number
!
! *** Local variables
      CHARACTER(LEN=14) :: CALLER = 'PROCESS_DILUTE'
      CHARACTER(LEN=24) :: VNAME ! Temporary stochastic variable name
!
      INTEGER :: VIDX ! Stochastic variable index
      INTEGER :: IOS  ! System error number
      INTEGER :: ISTO ! Stochastic variable looping index
      INTEGER :: IREL ! Realization looping index
      LOGICAL :: TMPFLG1 ! Temporary logical flag
      LOGICAL :: TMPFLG2 ! Temporary logical flag
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Put data into the DILUTE variables
!
      DF_PTITLE = PTITLE
      DF_PRGNAM = PRGNAM
      DF_PRGVER = PRGVER
      DF_PRGDAT = PRGDAT
      DF_CRUNID = CRUNID
      DF_USRNAM = USRNAM
!
      DF_NITR = NREAL
!
! *** Exit if no DILUTE data are desired
!
      IF( .NOT.CREATE_DILUTE ) RETURN
!
      IF( FN_DILUTE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The DILUTE file name is missing'
        MESSAG(2) = 'Modify the DILUTE modifier on the FILE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      WRITE(IRPT,1030) TRIM(FN_DILUTE)
 1030 FORMAT(/'Processing DILUTE keywords into a data file'/ &
        '  File Name = ',A)
!
! *** Open the output file as a sequential ASCII file
!
      OPEN(UNIT=IDIL, IOSTAT=IOS, STATUS='UNKNOWN', FILE=FN_DILUTE)
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error opening output DILUTE file'
        MESSAG(2) = ' '
        WRITE(MESSAG(2), *) 'UNIT = ', IDIL, ' IOSTAT = ', IOS
        MESSAG(3) = 'FN_DILUTE = ' // TRIM(FN_DILUTE)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Write output file header
!
      WRITE(IDIL,1000,ERR=9999) 'DILUTE'
      WRITE(IDIL,1000,ERR=9999) TRIM(DF_PTITLE)
      WRITE(IDIL,1000,ERR=9999) TRIM(DF_PRGNAM)
      WRITE(IDIL,1000,ERR=9999) TRIM(DF_PRGVER)
      WRITE(IDIL,1000,ERR=9999) TRIM(DF_PRGDAT)
      WRITE(IDIL,1000,ERR=9999) TRIM(DF_CRUNID)
      WRITE(IDIL,1000,ERR=9999) TRIM(DF_USRNAM)
 1000 FORMAT('"',A,'"')
!
      WRITE(IDIL,1010,ERR=9999) DF_INDSTO
      WRITE(IDIL,1010,ERR=9999) DF_NITR
 1010 FORMAT(I0)
!
! *** Set up stochastic memory
!
      CALL STOCH_MEMORY( DF_INDSTO, DF_INDTBL, DF_NITR, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL INIT_STOCH( )
!
! *** Set up DILUTE memory
!
      ALLOCATE( DF_ID(DF_INDSTO), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating DF_ID(DF_INDSTO)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( DF_UNITS(DF_INDSTO), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating DF_UNITS(DF_INDSTO)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( DF_SOIL(DF_INDSTO,DF_NITR), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating DF_SOIL(DF_INDSTO,DF_NITR)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Read DILUTE keywords
!
      CALL SCAN_DATA_DILUTE( IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Process DILUTE keywords and write data to the output file
!
      IF( ECHO_DILUTE ) THEN
        TMPFLG1 = BG_STOC_DEFN
        TMPFLG2 = BG_STOC_STAT
        BG_STOC_DEFN = .TRUE.
        BG_STOC_STAT = .TRUE.
      END IF
!
      DO ISTO = 1, DF_INDSTO
!
        VNAME = DF_ID(ISTO)
        VIDX = ISTO
        CALL STONE( VNAME, VIDX, DF_NITR, DF_SEED, WORK, IRPT, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        DO IREL = 1, DF_NITR
          DF_SOIL(ISTO,IREL) = WORK(IREL)
        END DO
!
        WRITE(IDIL,1020,ERR=9999) ISTO, TRIM(DF_ID(ISTO)), TRIM(DF_UNITS(ISTO)),(DF_SOIL(ISTO,IREL),IREL=1,DF_NITR)
 1020   FORMAT(I0,',"',A,'","',A,'"',1P,100(:,',',E12.5))
!
      END DO
!
      IF( ECHO_DILUTE ) THEN
        BG_STOC_DEFN = TMPFLG1
        BG_STOC_STAT = TMPFLG2
      END IF
!
! *** Free the DILUTE memory variables
!
      DEALLOCATE( DF_ID,DF_UNITS, DF_SOIL )
!
! *** Free the memory for the stochastic variables
!
      CALL FREE_STOCH_DIM( 'TABLE', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL FREE_STOCH_DIM( 'WORK', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL FREE_STOCH_DIM( 'VARIABLE', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
!
! *** Abnormal exit: Error writing to the file
!
 9999 CONTINUE
!
      IERR = 5
      MESSAG(1) = 'Error writing to the DILUTE file'
      MESSAG(2) = 'File: ' // TRIM(FN_DILUTE)
      CALL PRTERR( IERR, CALLER, 2 )
      CLOSE( IDIL )
      RETURN
!
      END SUBROUTINE

      SUBROUTINE PROCESS_INFILT( IERR )
!!*******************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine schedules all actions needed for processing INFILTRA keywords
!!    including the following actions:
!!      Creating the output file for infiltration values
!!      Writing stochastic infiltration data to the output file
!!
!!  History:
!!
!!    Paul W. Eslinger :  4 Jun 2007 : Original source
!!
!!*******************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Errors_Mod
      USE Control_Mod
      USE Iden_Mod
      USE Infilt_Mod
      USE Stats_Mod, ONLY: WORK, BG_STOC_DEFN, BG_STOC_STAT
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error number
!
! *** Local variables
      CHARACTER(LEN=14) :: CALLER = 'PROCESS_INFILT' ! Name of this subroutine
      CHARACTER(LEN=24) :: VNAME ! Temporary stochastic variable name
      INTEGER :: VIDX ! Stochastic variable index
      INTEGER :: IOS  ! System error number
      INTEGER :: ISTO ! Stochastic variable looping index
      INTEGER :: IREL ! Realization looping index
      LOGICAL :: TMPFLG1 ! Temporary logical flag
      LOGICAL :: TMPFLG2 ! Temporary logical flag
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Put data into the infiltration variables
!
      IN_PTITLE = PTITLE
      IN_PRGNAM = PRGNAM
      IN_PRGVER = PRGVER
      IN_PRGDAT = PRGDAT
      IN_CRUNID = CRUNID
      IN_USRNAM = USRNAM
!
      IN_NITR = NREAL
!
! *** Exit if no infiltration data are desired
!
      IF( .NOT.CREATE_INFILT ) RETURN
!
      IF( FN_INFILT .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The INFILTRATION file name is missing'
        MESSAG(2) = 'Modify the INFILTRATION modifier on the FILE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      WRITE(IRPT,1030) TRIM(FN_INFILT)
 1030 FORMAT(/'Processing INFILTRATION keywords into a data file'/ &
        '  File Name = ',A)
!
! *** Open the output file as a sequential ASCII file
!
      OPEN(UNIT=INFL, IOSTAT=IOS, STATUS='UNKNOWN', FILE=FN_INFILT)
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error opening output INFILTRATION file'
        MESSAG(2) = ' '
        WRITE(MESSAG(2), *) 'UNIT = ', INFL, ' IOSTAT = ', IOS
        MESSAG(3) = 'FN_INFILT = ' // TRIM(FN_INFILT)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Write output file header
!
      WRITE(INFL,1000,ERR=9999) 'INFILT'
      WRITE(INFL,1000,ERR=9999) TRIM(IN_PTITLE)
      WRITE(INFL,1000,ERR=9999) TRIM(IN_PRGNAM)
      WRITE(INFL,1000,ERR=9999) TRIM(IN_PRGVER)
      WRITE(INFL,1000,ERR=9999) TRIM(IN_PRGDAT)
      WRITE(INFL,1000,ERR=9999) TRIM(IN_CRUNID)
      WRITE(INFL,1000,ERR=9999) TRIM(IN_USRNAM)
 1000 FORMAT('"',A,'"')
!
      WRITE(INFL,1010,ERR=9999) IN_INDSTO
      WRITE(INFL,1010,ERR=9999) IN_NITR
 1010 FORMAT(I0)
!
! *** Set up stochastic memory
!
      CALL STOCH_MEMORY( IN_INDSTO, IN_INDTBL, IN_NITR, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL INIT_STOCH( )
!
! *** Set up INFILTRATION memory
!
      ALLOCATE( IN_ID(IN_INDSTO), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating IN_ID(IN_INDSTO)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( IN_UNITS(IN_INDSTO), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating IN_UNITS(IN_INDSTO)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( IN_STOC(IN_INDSTO,IN_NITR), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating IN_STOC(IN_INDSTO,IN_NITR)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Read INFILTRATION keywords
!
      CALL SCAN_DATA_INFILT( IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Process INFILTRATION keywords and write data to the output file
!
      IF( ECHO_INFILT ) THEN
        TMPFLG1 = BG_STOC_DEFN
        TMPFLG2 = BG_STOC_STAT
        BG_STOC_DEFN = .TRUE.
        BG_STOC_STAT = .TRUE.
      END IF
!
      DO ISTO = 1, IN_INDSTO
!
        VNAME = IN_ID(ISTO)
        VIDX = ISTO
        CALL STONE( VNAME, VIDX, IN_NITR, IN_SEED, WORK, IRPT, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        DO IREL = 1, IN_NITR
          IN_STOC(ISTO,IREL) = WORK(IREL)
        END DO
!
        WRITE(INFL,1020,ERR=9999) ISTO, TRIM(IN_ID(ISTO)), TRIM(IN_UNITS(ISTO)),(IN_STOC(ISTO,IREL),IREL=1,IN_NITR)
 1020   FORMAT(I0,',"',A,'","',A,'"',1P,100(:,',',E12.5))
!
      END DO
!
      IF( ECHO_INFILT ) THEN
        BG_STOC_DEFN = TMPFLG1
        BG_STOC_STAT = TMPFLG2
      END IF
!
! *** Free the INFILTRATION memory variables
!
      DEALLOCATE( IN_ID,IN_UNITS, IN_STOC )
!
! *** Free the memory for the stochastic variables
!
      CALL FREE_STOCH_DIM( 'TABLE', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL FREE_STOCH_DIM( 'WORK', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL FREE_STOCH_DIM( 'VARIABLE', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
!
! *** Abnormal exit: Error writing to the file
!
 9999 CONTINUE
!
      IERR = 5
      MESSAG(1) = 'Error writing to the INFILTRATION file'
      MESSAG(2) = 'File: ' // TRIM(FN_INFILT)
      CALL PRTERR( IERR, CALLER, 2 )
      CLOSE( INFL )
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE PROCESS_KDSOIL( IERR )
!!*******************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine schedules all actions needed for processing KDSOIL keywords
!!    including the following actions:
!!      Creating the output file for KDSOIL values
!!      Writing stochastic KDSOIL data to the output file
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Add infiltration file generation logic
!!
!!*******************************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Errors_Mod
      USE Control_Mod
      USE Iden_Mod
      USE KdSoil_Mod
      USE Stats_Mod, ONLY: WORK, BG_STOC_DEFN, BG_STOC_STAT
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error number
!
! *** Local variables
      CHARACTER(LEN=14) :: CALLER = 'PROCESS_KDSOIL' ! Name of this subroutine
      CHARACTER(LEN=24) :: VNAME ! Temporary stochastic variable name
      INTEGER :: VIDX ! Stochastic variable index
      INTEGER :: IOS  ! System error number
      INTEGER :: ISTO ! Stochastic variable looping index
      INTEGER :: IREL ! Realization looping index
      LOGICAL :: TMPFLG1 ! Temporary logical flag
      LOGICAL :: TMPFLG2 ! Temporary logical flag
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Put data into the KDSOIL variables
!
      KD_PTITLE = PTITLE
      KD_PRGNAM = PRGNAM
      KD_PRGVER = PRGVER
      KD_PRGDAT = PRGDAT
      KD_CRUNID = CRUNID
      KD_USRNAM = USRNAM
!
      KD_NITR = NREAL
!
! *** Exit if no KDSOIL data are desired
!
      IF( .NOT.CREATE_KDSOIL ) RETURN
!
      IF( FN_KDSOIL .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The KDSOIL file name is missing'
        MESSAG(2) = 'Modify the KDSOIL modifier on the FILE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      WRITE(IRPT,1030) TRIM(FN_KDSOIL)
 1030 FORMAT(/'Processing KDSOIL keywords into a data file'/ &
        '  File Name = ',A)
!
! *** Open the output file as a sequential ASCII file
!
      OPEN(UNIT=IKDS, IOSTAT=IOS, STATUS='UNKNOWN', FILE=FN_KDSOIL)
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error opening output KDSOIL file'
        MESSAG(2) = ' '
        WRITE(MESSAG(2), *) 'UNIT = ', IKDS, ' IOSTAT = ', IOS
        MESSAG(3) = 'FN_KDSOIL = ' // TRIM(FN_KDSOIL)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Write output file header
!
      WRITE(IKDS,1000,ERR=9999) 'KDSOIL'
      WRITE(IKDS,1000,ERR=9999) TRIM(KD_PTITLE)
      WRITE(IKDS,1000,ERR=9999) TRIM(KD_PRGNAM)
      WRITE(IKDS,1000,ERR=9999) TRIM(KD_PRGVER)
      WRITE(IKDS,1000,ERR=9999) TRIM(KD_PRGDAT)
      WRITE(IKDS,1000,ERR=9999) TRIM(KD_CRUNID)
      WRITE(IKDS,1000,ERR=9999) TRIM(KD_USRNAM)
 1000 FORMAT('"',A,'"')
!
      WRITE(IKDS,1010,ERR=9999) KD_INDSTO
      WRITE(IKDS,1010,ERR=9999) KD_NITR
 1010 FORMAT(I0)
!
! *** Set up stochastic memory
!
      CALL STOCH_MEMORY( KD_INDSTO, KD_INDTBL, KD_NITR, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL INIT_STOCH( )
!
! *** Set up KDSOIL memory
!
      ALLOCATE( KD_ID(KD_INDSTO), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating KD_ID(KD_INDSTO)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
     ALLOCATE( KD_UNITS(KD_INDSTO), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating KD_UNITS(KD_INDSTO)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF

      ALLOCATE( KD_SOIL(KD_INDSTO,KD_NITR), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating KD_SOIL(KD_INDSTO,KD_NITR)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Read KDSOIL keywords
!
      CALL SCAN_DATA_KDSOIL( IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Process KDSOIL keywords and write data to the output file
!
      IF( ECHO_KDSOIL ) THEN
        TMPFLG1 = BG_STOC_DEFN
        TMPFLG2 = BG_STOC_STAT
        BG_STOC_DEFN = .TRUE.
        BG_STOC_STAT = .TRUE.
      END IF
!
      DO ISTO = 1, KD_INDSTO
!
        VNAME = KD_ID(ISTO)
        VIDX = ISTO
        CALL STONE( VNAME, VIDX, KD_NITR, KD_SEED, WORK, IRPT, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        DO IREL = 1, KD_NITR
          KD_SOIL(ISTO,IREL) = WORK(IREL)
        END DO
        WRITE(IKDS,1020,ERR=9999) ISTO, TRIM(KD_ID(ISTO)),TRIM(KD_UNITS(ISTO)),(KD_SOIL(ISTO,IREL),IREL=1,KD_NITR)
 1020   FORMAT(I0,',"',A,'","',A,'"',1P,100(:,',',E12.5))
!
      END DO
!
      IF( ECHO_KDSOIL ) THEN
        BG_STOC_DEFN = TMPFLG1
        BG_STOC_STAT = TMPFLG2
      END IF
!
! *** Free the KDSOIL memory variables
!
      DEALLOCATE( KD_ID, KD_UNITS, KD_SOIL )
!
! *** Free the memory for the stochastic variables
!
      CALL FREE_STOCH_DIM( 'TABLE', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL FREE_STOCH_DIM( 'WORK', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      CALL FREE_STOCH_DIM( 'VARIABLE', IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
!
! *** Abnormal exit: Error writing to the file
!
 9999 CONTINUE
!
      IERR = 6
      MESSAG(1) = 'Error writing to the KDSOIL file'
      MESSAG(2) = 'File: ' // TRIM(FN_KDSOIL)
      CALL PRTERR( IERR, CALLER, 2 )
      CLOSE( IKDS )
      RETURN
!
      END SUBROUTINE

      SUBROUTINE SCAN_DATA_DILUTE( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the entire set of keywords from the keyword
!!    file to extract and store the data defining the stochastic set of
!!    DILUTE variables.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Apr 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Jan 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Revise comments
!!
!!***********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Files_Mod
      USE Rdblk_Mod
      USE Iden_Mod
      USE Control_Mod
      USE DILUTE_Mod
      USE Stats_Mod, ONLY: INDSTO
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number
!
! *** Local variables
!
      CHARACTER(LEN=16) :: CALLER = 'SCAN_DATA_DILUTE' ! Name of this routine
      CHARACTER (LEN=LENCRD) :: CARD ! Input file line without the keyword
!
      INTEGER :: IDXQ   ! Index for quote strings
      REAL :: RTMP      ! Temporary real value
      CHARACTER(LEN=LENQQQ) :: TMPNAME ! Temporary quote string
      CHARACTER(LEN=20) :: CTMP     ! Temporary ID for stochastic variables
      CHARACTER(LEN=LENQQQ) :: CMES ! Temporary variable for quote strings
      INTEGER :: ISTAT              ! Temporary index to statistical distribution definition
      LOGICAL :: TRUNC              ! Truncation flag for use by SDECOD
!
! *** User defined functions
!
      LOGICAL, EXTERNAL :: CEXIST ! Routine for existence of keyword modifier
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize for RDBLK
!
      ILINE = 0    ! Line number of the next line to read from the keyword file
!
      REWIND(IKEY) ! File has already been read by other subroutines
      IERR = 0
!
!-----------------------------------------------------------------------
!                   Top of loop on reading keyword cards
!-----------------------------------------------------------------------
!
   10 CONTINUE
!
      CALL RDBLK( IKEY, IRPT, CARD, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE ( 'END' ) ! END Keyword (exit point from this subroutine)
          REWIND(IKEY)
          RETURN
!
        CASE( 'DILUTE' ) ! DILUTE Keyword
!
!       Force the user to enter at least one quote string
!
          IF( NQUOTE .LT. 2 ) THEN
            IERR = 1
            CTMP = QUOTE(1)
            MESSAG(1) = 'At least two quote strings required (DILUTE)'
            MESSAG(2) = 'Stochastic variable: ' // TRIM(CTMP)
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
!
          IF( CEXIST('ID') ) THEN
            CALL NXTQOT( IDXQ, TMPNAME ) ! The stochastic variable ID
            IF( IDXQ .LT. 1 ) THEN
              IERR = 2
              MESSAG(1) = 'No quote string associated with ID modifier on DILUTE keyword'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            ENDIF
            CTMP = TMPNAME
            CALL NXTVAL( ISTAT, RTMP) ! Statistical distribution index
            IF( ISTAT .LT. 1 ) THEN
              IERR = 3
              MESSAG(1) = 'No statistical distribution type entered'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            ENDIF
          ELSE
            IERR = 4
            MESSAG(1) = 'ID modifier missing on a DILUTE keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('LABEL') ) THEN
            CALL NXTQOT( IDXQ, CMES )
            IF( IDXQ .LT. 1 ) THEN
              IERR = 5
              MESSAG(1) = 'No quote string associated with LABEL modifier on DILUTE keyword'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            ENDIF
          ELSE
            CMES = 'No statistical distribution description entered'
          END IF

          IF( CEXIST('UNITS') ) THEN
            CALL NXTQOT( IDXQ, TMPNAME )
            IF( IDXQ .LT. 1 ) THEN
              IERR = 6
              MESSAG(1) = 'No quote string associated with UNITS modifier on DILUTE keyword'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            ENDIF
          ELSE
            IERR = 7
            MESSAG(1) = 'DILUTE UNITS modifier missing quote string'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('TRUNCATE') ) THEN
            TRUNC = .TRUE.
          ELSE
            TRUNC = .FALSE.
          END IF

          CALL SDECOD( CTMP, CMES, ISTAT, TRUNC, VALUE, NVALUE, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error for variable ' // CTMP
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DF_ID(INDSTO) = CTMP
          DF_UNITS(INDSTO)= TMPNAME
!
        CASE DEFAULT ! Other Keywords are ignored
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE

      SUBROUTINE SCAN_DATA_INFILT( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the entire set of keywords from the keyword
!!    file to extract and store the data defining the stochastic set of
!!    INFILTRATION DEFINE variables.
!!
!!  History:
!!
!!    Paul W. Eslinger :  4 Jun 2007 : Original source
!!
!!***********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Files_Mod
      USE Rdblk_Mod
      USE Iden_Mod
      USE Control_Mod
      USE Infilt_Mod
      USE Stats_Mod, ONLY: INDSTO
!
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST ! Routine for existence of keyword modifier
!
! *** Call list variables
      INTEGER :: IERR ! Error number
!
! *** Local variables
      CHARACTER(LEN=16) :: CALLER = 'SCAN_DATA_INFILT' ! Name of this routine
!
      CHARACTER (LEN=LENCRD) :: CARD ! Input file line without the keyword
!
      INTEGER :: IDXQ   ! Index for quote strings
      REAL :: RTMP      ! Temporary real value
      CHARACTER(LEN=LENQQQ) :: TMPNAME ! Temporary quote string
      CHARACTER(LEN=20) :: CTMP     ! Temporary ID for stochastic variables
      CHARACTER(LEN=LENQQQ) :: CMES ! Temporary variable for quote strings
      INTEGER :: ISTAT              ! Temporary index to statistical distribution definition
      LOGICAL :: TRUNC              ! Truncation flag for use by SDECOD
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize for RDBLK
!
      ILINE = 0    ! Line number of the next line to read from the keyword file
!
      REWIND(IKEY) ! File has already been read by other subroutines
      IERR = 0
!
!-----------------------------------------------------------------------
!                   Top of loop on reading keyword cards
!-----------------------------------------------------------------------
!
   10 CONTINUE
!
      CALL RDBLK( IKEY, IRPT, CARD, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE ( 'END' ) ! END Keyword (exit point from this subroutine)
          REWIND(IKEY)
          RETURN
!
        CASE( 'INFILTRA' ) ! INFILTRATION Keyword
!
          IF( CEXIST('DEFINE') ) THEN ! Only use the DEFINE for of the keyword
!
            IF( CEXIST('CLASS') ) THEN
              CALL NXTQOT( IDXQ, TMPNAME ) ! The stochastic variable ID
              IF( IDXQ .LT. 1 ) THEN
                IERR = 2
                MESSAG(1) = 'No quote string associated with CLASS modifier on INFILTRATION keyword'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              ENDIF
              CTMP = TMPNAME
              CALL NXTVAL( ISTAT, RTMP) ! Statistical distribution index
              IF( ISTAT .LT. 1 ) THEN
                IERR = 3
                MESSAG(1) = 'No statistical distribution type entered'
                MESSAG(2) = 'CLASS = '//TRIM(CTMP)
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              ENDIF
              IF( NQUOTE .GT. IDXQ ) THEN
                CMES = QUOTE(IDXQ+1)
              ELSE
                CMES = 'No statistical distribution description entered'
              END IF
            ELSE
              IERR = 4
              MESSAG(1) = 'CLASS modifier missing on a INFILTRATION DEFINE keyword'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
            IF( CEXIST('UNIT') ) THEN
              CALL NXTQOT( IDXQ, TMPNAME )
              IF( IDXQ .LT. 1 ) THEN
                IERR = 6
                MESSAG(1) = 'No quote string associated with UNIT modifier on INFILTRATION DEFINE keyword'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              ENDIF
            ELSE
              IERR = 7
              MESSAG(1) = 'INFILTRATION DEFINE UNIT modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!
            IF( CEXIST('TRUNCATE') ) THEN
              TRUNC = .TRUE.
            ELSE
              TRUNC = .FALSE.
            END IF

            CALL SDECOD( CTMP, CMES, ISTAT, TRUNC, VALUE, NVALUE, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error for variable ' // CTMP
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IN_ID(INDSTO) = CTMP
            IN_UNITS(INDSTO)= TMPNAME
!
          END IF
!
        CASE DEFAULT ! Other Keywords are ignored
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE
!
      SUBROUTINE SCAN_DATA_KDSOIL( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the entire set of keywords from the keyword
!!    file to extract and store the data defining the stochastic set of
!!    KDSOIL variables.
!!
!!  History:
!!
!!    Paul W. Eslinger : 25 Apr 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Jan 2003 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Revise comments
!!
!!***********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE ECDA_Mod
      USE Files_Mod
      USE Rdblk_Mod
      USE Iden_Mod
      USE Control_Mod
      USE KdSoil_Mod
      USE Stats_Mod, ONLY: INDSTO
!
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST ! Routine for existence of keyword modifier
!
! *** Call list variables
      INTEGER :: IERR ! Error number
!
! *** Local variables
!
      CHARACTER(LEN=16) :: CALLER = 'SCAN_DATA_KDSOIL' ! Name of this routine
!
      CHARACTER (LEN=LENCRD) :: CARD ! Input file line without the keyword
      INTEGER :: IDXQ   ! Index for quote strings
      REAL :: RTMP      ! Temporary real value
      CHARACTER(LEN=LENQQQ) :: TMPNAME ! Temporary quote string
!
      CHARACTER(LEN=20) :: CTMP     ! Temporary ID for stochastic variables
      CHARACTER(LEN=LENQQQ) :: CMES ! Temporary variable for quote strings
      INTEGER :: ISTAT              ! Temporary index to statistical distribution definition
      LOGICAL :: TRUNC              ! Truncation flag for use in SDECOD
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize for RDBLK
!
      ILINE = 0    ! Line number of the next line to read from the input file
!
      REWIND(IKEY) ! Rewind the file because it has been read earlier
      IERR = 0
!
!-----------------------------------------------------------------------
!                   Top of loop on reading keyword cards
!-----------------------------------------------------------------------
!
   10 CONTINUE
!
      CALL RDBLK( IKEY, IRPT, CARD, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE ( 'END' ) ! END Keyword (exit point from this subroutine)
          REWIND(IKEY)
          RETURN
!
        CASE( 'KDSOIL' ) ! KDSOIL Keyword
!
!         Force the user to enter at least one quote string
!
          IF( NQUOTE .LT. 2 ) THEN
            IERR = 1
            CTMP = QUOTE(1)
            MESSAG(1) = 'At least two quote strings required (KDSOIL)'
            MESSAG(2) = 'Stochastic variable: ' // TRIM(CTMP)
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
!
          IF( CEXIST('ID') ) THEN
            CALL NXTQOT( IDXQ, TMPNAME ) ! The stochastic variable ID
            IF( IDXQ .LT. 1 ) THEN
              IERR = 1
              MESSAG(1) = 'No quote string associated with ID modifier on KDSOIL keyword'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            ENDIF
            CTMP = TMPNAME
            CALL NXTVAL( ISTAT, RTMP) ! Statistical distribution index
            IF( ISTAT .LT. 1 ) THEN
              IERR = 3
              MESSAG(1) = 'No statistical distribution type entered'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            ENDIF
          ELSE
            IERR = 4
            MESSAG(1) = 'ID modifier missing on a KDSOIL keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('LABEL') ) THEN
            CALL NXTQOT( IDXQ, CMES )
            IF( IDXQ .LT. 1 ) THEN
              IERR = 5
              MESSAG(1) = 'No quote string associated with LABEL modifier on KDSOIL keyword'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            ENDIF
          ELSE
            CMES = 'No statistical distribution description entered'
          END IF

          IF( CEXIST('UNITS') ) THEN
            CALL NXTQOT( IDXQ, TMPNAME )
            IF( IDXQ .LT. 1 ) THEN
              IERR = 6
              MESSAG(1) = 'No quote string associated with UNITS modifier on KDSOIL keyword'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            ENDIF
          ELSE
            IERR = 7
            MESSAG(1) = 'KDSOIL UNITS modifier missing quote string'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('TRUNCATE') ) THEN
            TRUNC = .TRUE.
          ELSE
            TRUNC = .FALSE.
          END IF

          CALL SDECOD( CTMP, CMES, ISTAT, TRUNC, VALUE, NVALUE, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error for variable ' // CTMP
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          KD_ID(INDSTO) = CTMP
          KD_UNITS(INDSTO)= TMPNAME
!
        CASE DEFAULT ! Other Keywords are ignored
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE
!
      SUBROUTINE SCAN_KEYS_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the input keyword file to perform the
!!    following operations:
!!      => record the number of realizations requested,
!!      => record the number of ananlytes
!!      => count the number of locations
!!      => record the media which have been requested for each location
!!      => count the number of times
!!      => record the map file name
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Feb 2001 : Version 1.0
!!    Paul W. Eslinger :  9 Mar 2001 : Version 1.0.A (Report file
!!                                     opened prior to this routine)
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger : 10 Aug 2006 : Change Header file logic
!!    Paul W. Eslinger :  4 Jun 2007 : Add infiltration file generation logic
!!
!!***********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE ECDA_Mod
      USE Files_Mod
      USE Rdblk_Mod
      USE Iden_Mod
      USE Control_Mod
      USE KdSoil_Mod, ONLY: KD_INDSTO, KD_INDTBL
      USE Dilute_Mod, ONLY: DF_INDSTO, DF_INDTBL
      USE Infilt_Mod, ONLY: IN_INDSTO, IN_INDTBL
!
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST ! Routine for existence of keyword modifier
!
! *** Call list variables
      INTEGER :: IERR ! Error number
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'SCAN_KEYS_1'
!
      INTEGER :: IDXQ   ! Index for quote strings
      INTEGER :: IDXV   ! Index for real values
      INTEGER :: ITMP   ! Temporary integer variable
      REAL :: RTMP ! Temporary real value
      CHARACTER(LEN=LENCRD) :: CARD   ! Input line without the leading keyword
      CHARACTER(LEN=LENQQQ) :: TMPNAME ! Temporary quote string
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize for RDBLK
!
      ILINE = 0    ! Line number of the next line to read from the input file
!
! *** Initialize location, analyte and time counters
!
      NUM_ANA = 0
      NUM_LOC = 0
      NUM_TIM = 0
!
!-----------------------------------------------------------------------
!                   Top of loop on reading keyword cards
!-----------------------------------------------------------------------
!
      IERR = 0
!
   10 CONTINUE
      CALL RDBLK( IKEY, IRPT, CARD, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'ANALYTE' ) ! ANALYTE Keyword
          NUM_ANA = NUM_ANA + 1
!
        CASE( 'DEBUG' ) ! DEBUG Keyword
          IF( CEXIST('ECDA') ) DEBUG_ECDA = .TRUE.
!
        CASE( 'DILUTE' ) ! DILUTE Keyword
          DF_INDSTO = DF_INDSTO + 1
          ITMP = VALUE(1)
          IF( ITMP .EQ. 10 ) DF_INDTBL = DF_INDTBL + VALUE(2)
!
        CASE( 'ECHO' ) ! ECHO Keyword
          IF( CEXIST('KDSOIL') )   ECHO_KDSOIL = .TRUE.
          IF( CEXIST('DILUTE') )   ECHO_DILUTE = .TRUE.
          IF( CEXIST('INFILT') ) ECHO_INFILT = .TRUE.
!
        CASE ( 'END' ) ! END Keyword
          REWIND(IKEY)
          RETURN
!
        CASE ( 'FILE' ) ! FILE Keyword
!
          IF( CEXIST('I_ECDA') ) THEN
            IF( CEXIST('NAME') ) THEN
              CALL NXTQOT( IDXQ, TMPNAME )
              FN_MAP = TMPNAME
            END IF
            IF( CEXIST('CREATE') ) CREATE_MAP = .TRUE.
          END IF
!
!          IF( CEXIST('HEADER') ) THEN
!            IF( CEXIST('NAME') ) THEN
!              CALL NXTQOT( IDXQ, TMPNAME )
!              FN_HDR = TMPNAME
!            END IF
!            IF( CEXIST('CREATE') ) CREATE_HDR = .TRUE.
!          END IF
!
        CASE( 'INFILTRA' ) ! INFILTRATION (DEFINE) Keyword
          IF( CEXIST('DEFINE') ) THEN
            IN_INDSTO = IN_INDSTO + 1
            ITMP = VALUE(1)
            IF( ITMP .EQ. 10 ) IN_INDTBL = IN_INDTBL + VALUE(2)
          END IF
!
        CASE( 'KDSOIL' ) ! KDSOIL Keyword
          KD_INDSTO = KD_INDSTO + 1
          ITMP = VALUE(1)
          IF( ITMP .EQ. 10 ) KD_INDTBL = KD_INDTBL + VALUE(2)
!
        CASE( 'FILLECDA' ) ! FILLECDA Keyword
          IF( CEXIST('GWAT') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(IGWAT) = 1
              FILL_VALUE(IGWAT) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(IGWAT) = 2
              FILL_VALUE(IGWAT) = VALUE(1)
            END IF
          END IF
!
          IF( CEXIST('SEEP') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(ISEEP) = 1
              FILL_VALUE(ISEEP) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(ISEEP) = 2
              FILL_VALUE(ISEEP) = VALUE(1)
            END IF
          END IF
!
          IF( CEXIST('SWAT') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(ISWAT) = 1
              FILL_VALUE(ISWAT) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(ISWAT) = 2
              FILL_VALUE(ISWAT) = VALUE(1)
            END IF
          END IF
!
          IF( CEXIST('PWAT') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(IPWAT) = 1
              FILL_VALUE(IPWAT) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(IPWAT) = 2
              FILL_VALUE(IPWAT) = VALUE(1)
            END IF
          END IF
!
          IF( CEXIST('SEDI') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(ISEDI) = 1
              FILL_VALUE(ISEDI) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(ISEDI) = 2
              FILL_VALUE(ISEDI) = VALUE(1)
            END IF
          END IF
!
          IF( CEXIST('SORP') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(ISORP) = 1
              FILL_VALUE(ISORP) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(ISORP) = 2
              FILL_VALUE(ISORP) = VALUE(1)
            END IF
          END IF
!
          IF( CEXIST('SODR') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(ISODR) = 1
              FILL_VALUE(ISODR) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(ISODR) = 2
              FILL_VALUE(ISODR) = VALUE(1)
            END IF
          END IF
!
          IF( CEXIST('SOGW') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(ISOGW) = 1
              FILL_VALUE(ISOGW) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(ISOGW) = 2
              FILL_VALUE(ISOGW) = VALUE(1)
            END IF
          END IF
!
          IF( CEXIST('SOSW') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(ISOSW) = 1
              FILL_VALUE(ISOSW) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(ISOSW) = 2
              FILL_VALUE(ISOSW) = VALUE(1)
            END IF
          END IF
!
          IF( CEXIST('AIRC') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(IAIRC) = 1
              FILL_VALUE(IAIRC) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(IAIRC) = 2
              FILL_VALUE(IAIRC) = VALUE(1)
            END IF
          END IF
!
          IF( CEXIST('AIRD') ) THEN
            IF( CEXIST('FIXED') ) THEN
              FILL(IAIRD) = 1
              FILL_VALUE(IAIRD) = VALUE(1)
            END IF
            IF( CEXIST('RANDOM') ) THEN
              FILL(IAIRD) = 2
              FILL_VALUE(IAIRD) = VALUE(1)
            END IF
          END IF
!
        CASE( 'LOCATION' ) ! LOCATION Keyword
          NUM_LOC = NUM_LOC + 1
!
        CASE( 'PERIOD' ) ! PERIOD Keyword
          IF( CEXIST('START   ') ) THEN
            CALL NXTVAL( IDXV, RTMP )
            IF( IDXV .GT. 0 ) THEN
              YEAR_START = RTMP
            ELSE
              IERR = 1
              MESSAG(1) = 'PERIOD START modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 2
            MESSAG(1) = 'START modifier not entered on the PERIOD keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('STOP    ') ) THEN
            CALL NXTVAL( IDXV, RTMP )
            IF( IDXV .GT. 0 ) THEN
              YEAR_STOP = RTMP
            ELSE
              IERR = 3
              MESSAG(1) = 'PERIOD STOP modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 4
            MESSAG(1) = 'STOP modifier not entered on the PERIOD keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('CLOSURE ') ) THEN
            CALL NXTVAL( IDXV, RTMP )
            IF( IDXV .GT. 0 ) THEN
              YEAR_CLOSE = RTMP
            ELSE
              IERR = 5
              MESSAG(1) = 'PERIOD CLOSURE modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 6
            MESSAG(1) = 'CLOSURE modifier not entered on the PERIOD keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
        CASE( 'REALIZAT' ) ! REALIZATION Keyword
          IF( NVALUE .GT. 0 ) NREAL = VALUE(1)
!
        CASE( 'TIMES' ) ! TIMES Keyword
          NUM_TIM = NUM_TIM + NVALUE
!
        CASE( 'TITLE' ) ! TITLE Keyword
          PTITLE = QUOTE(1)
!
        CASE( 'USER' ) ! USER Keyword
          USRNAM = QUOTE(1)
!
        CASE DEFAULT ! Other Keyword
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE
!
      SUBROUTINE SCAN_KEYS_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the input keyword file to perform the
!!    following operations:
!!      => Define the analytes
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  4 Jun 2007 : Revise comments
!!
!!***********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Files_Mod
      USE Rdblk_Mod
      USE Iden_Mod
      USE Control_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST ! Routine for existence of keyword modifier
!
! *** Call list variables
      INTEGER :: IERR ! Error number
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'SCAN_KEYS_2'
!
      INTEGER :: IDXQ    ! Index for quote strings
      INTEGER :: IDX     ! Index
      INTEGER :: NUM_OLD ! Old number of analytes
      CHARACTER (LEN=LENCRD) :: CARD   ! Input line without the leading keyword
      CHARACTER(LEN=LENQQQ) :: TMPNAME ! Temporary quote string
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize for RDBLK
!
      ILINE = 0    ! Line number of the next line to read from the input file
!
! *** Initialize location, analyte and time counters
!
      NUM_OLD = NUM_ANA
      NUM_ANA = 0
!
!-----------------------------------------------------------------------
!                   Top of loop on reading keyword cards
!-----------------------------------------------------------------------
!
      IERR = 0
!
   10 CONTINUE
      CALL RDBLK( IKEY, IRPT, CARD, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'ANALYTE' ) ! ANALYTE Keyword
          NUM_ANA = NUM_ANA + 1
          IF( CEXIST('ID') ) THEN
            CALL NXTQOT( IDXQ, TMPNAME )
            ANA_ID(NUM_ANA) = TMPNAME
          ELSE
            IERR = 1
            MESSAG(1) = 'ANALYTE keyword missing ID modifier'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( CEXIST('TYPE') ) THEN
            CALL NXTQOT( IDXQ, TMPNAME )
            ANA_TYPE(NUM_ANA) = TMPNAME
          ELSE
            IERR = 2
            MESSAG(1) = 'ANALYTE keyword missing TYPE modifier'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( CEXIST('NAME') ) THEN
            CALL NXTQOT( IDX, TMPNAME )
            IF( IDX .GT. 0 ) THEN
              ANA_DESC(NUM_ANA) = TMPNAME
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
          IF( NUM_ANA .EQ. NUM_OLD ) THEN
            REWIND(IKEY)
            RETURN
          END IF
!
        CASE ( 'END' ) ! END Keyword
          REWIND(IKEY)
          RETURN
!
        CASE DEFAULT ! Other Keyword
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE
!
      SUBROUTINE SCAN_KEYS_3( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the input keyword file to perform the
!!    following operations:
!!      => Define the locations
!!      => Define the times
!!      => Define analyte-specific file names
!!    It also collects the data for the ECDA index map file
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger : 25 Jun 2003 : Upgrade LOCATION error messages
!!    Paul W. Eslinger : 26 Jun 2003 : Add easting and northing and media
!!    Paul W. Eslinger : 13 Oct 2004 : Add surface water irrigation
!!    Paul W. Eslinger : 21 Mar 2005 : Add location area
!!    Paul W. Eslinger :  4 Jun 2007 : Add infiltration file logic
!!
!!***********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Files_Mod
      USE Rdblk_Mod
      USE Iden_Mod
      USE Control_Mod
      USE ECDA_Mod
      USE Dilute_Mod
      USE KdSoil_Mod
      USE Infilt_Mod
!
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST ! Routine for existence of keyword modifier
!
! *** Call list variables
      INTEGER :: IERR ! Error number
!
! *** Local variables
!
      CHARACTER(LEN=11) :: CALLER = 'SCAN_KEYS_3'
!
      INTEGER :: IDXQ ! Index for quote strings
      INTEGER :: ITIM ! Temporary time index
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: NUM_R ! Record counter
      INTEGER :: NUM_M ! Media counter
      INTEGER :: MATCH ! Temporary index match
      INTEGER :: IDXR   ! Temporary index
      INTEGER :: IDX   ! Temporary index 
      REAL :: RTMP ! Temporary real value
      CHARACTER (LEN=LENCRD) :: CARD   ! Input line without the leading keyword
      CHARACTER(LEN=LENQQQ) :: TMPNAME ! Temporary quote string
!
!---- Executable code ---------------------------------------------------
!
! *** Initialize for RDBLK
!
      ILINE = 0    ! Line number of the next line to read from the input file
!
! *** Initialize location and time counters
!
      NUM_LOC = 0
      NUM_TIM = 0
!
! *** Initialize location, time, and record counters for this impact type
!
      NUM_R = ECDA_NHEAD
!
!-----------------------------------------------------------------------
!                   Top of loop on reading keyword cards
!-----------------------------------------------------------------------
!
      IERR = 0
!
   10 CONTINUE
      CALL RDBLK( IKEY, IRPT, CARD, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE ( 'END' ) ! END Keyword
          ECDA_BLOCK = NUM_R - ECDA_NHEAD
          REWIND(IKEY)
          RETURN
!
        CASE( 'FILE' ) ! FILE Keyword
!
          IF( CEXIST('C_ECDA') ) THEN
            IF( NQUOTE .NE. 2 ) THEN
              IERR = 1
              MESSAG(1) = 'Exactly 2 quote strings should have been entered'
              MESSAG(2) = 'Problem with the FILE keyword, C_ECDA modifier'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            IF( CEXIST('ANALYTE') ) THEN
              CALL NXTQOT( IDXQ, TMPNAME )
              MATCH = 0
              DO IANA = 1, NUM_ANA
                IF( ANA_ID(IANA) .EQ. TMPNAME ) THEN
                  MATCH = IANA
                  EXIT
                END IF
              END DO
!              WRITE(*,*) TRIM(tmpname), ' match=',match
              IF( CEXIST('NAME') .AND. MATCH.GT.0 ) THEN
                CALL NXTQOT( IDXQ, TMPNAME )
                FN_CON(MATCH) = TMPNAME
                IF( CEXIST('CREATE') ) FN_CON_CREATE(MATCH) = .TRUE.
              END IF
            ELSE
              IERR = 2
              MESSAG(1) = 'Missing the ANALYTE modifier'
              MESSAG(2) = 'Modify the FILE keyword, C_ECDA modifier'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('KDSOIL') ) THEN
            IF( CEXIST('CREATE') ) THEN
              CREATE_KDSOIL = .TRUE.
              IF( CEXIST('NAME') ) THEN
                CALL NXTQOT( IDXQ, TMPNAME )
                IF( IDXQ .GT. 0 ) THEN
                  FN_KDSOIL = TMPNAME
                ELSE
                  IERR = 3
                  MESSAG(1) = 'Missing file name for the KDSOIL file'
                  MESSAG(2) = 'Modify the FILE keyword, KDSOIL and NAME modifiers'
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              ELSE
                IERR = 4
                MESSAG(1) = 'Missing NAME modifier'
                MESSAG(2) = 'Modify the FILE keyword, KDSOIL modifiers'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
              IF( CEXIST('SEED') ) THEN
                CALL NXTVAL( IDXR, RTMP )
                IF( IDXR .GT. 0 ) THEN
                  KD_SEED = RTMP
                ELSE
                  IERR = 5
                  MESSAG(1) = 'Stochastic seed missing'
                  MESSAG(2) = 'FILE keyword, KDSOIL modifier'
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              ELSE
                IERR = 6
                MESSAG(1) = 'Stochastic seed missing'
                MESSAG(2) = 'FILE keyword, KDSOIL modifier'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            END IF
          END IF
!
          IF( CEXIST('DILUTE') ) THEN
            IF( CEXIST('CREATE') ) THEN
              CREATE_DILUTE = .TRUE.
              IF( CEXIST('NAME') ) THEN
                CALL NXTQOT( IDXQ, TMPNAME )
                IF( IDXQ .GT. 0 ) THEN
                  FN_DILUTE = TMPNAME
                ELSE
                  IERR = 7
                  MESSAG(1) = 'Missing file name for the DILUTE file'
                  MESSAG(2) = 'Modify the FILE keyword, DILUTE and NAME modifiers'
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              ELSE
                IERR = 8
                MESSAG(1) = 'Missing NAME modifier'
                MESSAG(2) = 'Modify the FILE keyword, DILUTE modifier'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
              IF( CEXIST('SEED') ) THEN
                CALL NXTVAL( IDXR, RTMP )
                IF( IDXR .GT. 0 ) THEN
                  DF_SEED = RTMP
                ELSE
                  IERR = 9
                  MESSAG(1) = 'Stochastic seed missing'
                  MESSAG(2) = 'FILE keyword, DILUTE modifier'
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              ELSE
                IERR = 10
                MESSAG(1) = 'Stochastic seed missing'
                MESSAG(2) = 'FILE keyword, DILUTE modifier'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            END IF
          END IF
!
          IF( CEXIST('INFILT') ) THEN
            IF( CEXIST('CREATE') ) THEN
              CREATE_INFILT = .TRUE.
              IF( CEXIST('NAME') ) THEN
                CALL NXTQOT( IDXQ, TMPNAME )
                IF( IDXQ .GT. 0 ) THEN
                  FN_INFILT = TMPNAME
                ELSE
                  IERR = 31
                  MESSAG(1) = 'Missing file name for the INFILTRATION file'
                  MESSAG(2) = 'Modify the FILE keyword, INFILT and NAME modifiers'
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              ELSE
                IERR = 32
                MESSAG(1) = 'Missing NAME modifier'
                MESSAG(2) = 'Modify the FILE keyword, INFILT modifier'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
              IF( CEXIST('SEED') ) THEN
                CALL NXTVAL( IDXR, RTMP )
                IF( IDXR .GT. 0 ) THEN
                  IN_SEED = RTMP
                ELSE
                  IERR = 33
                  MESSAG(1) = 'Stochastic seed missing'
                  MESSAG(2) = 'FILE keyword, INFILT modifier'
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              ELSE
                IERR = 34
                MESSAG(1) = 'Stochastic seed missing'
                MESSAG(2) = 'FILE keyword, INFILT modifier'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            END IF
          END IF
!
        CASE( 'LOCATION' ) ! LOCATION Keyword
          NUM_LOC = NUM_LOC + 1
!
!         Get the location ID
          IF( CEXIST('ID') ) THEN
            CALL NXTQOT( IDXQ, TMPNAME )
            ECDA_ID_LOC(NUM_LOC) = TMPNAME(1:6)
          ELSE
            IERR = 11
            MESSAG(1) = 'An identifier is required for each location.'
            MESSAG(2) = 'No identifier found for the following card.'
            MESSAG(3) = KNAME // CARD
            CALL PRTERR(IERR, CALLER, 3)
            RETURN
          END IF
!
!         Make sure the location ID is unique
          IF( NUM_LOC .GT. 1 ) THEN
            DO IDX = 1, NUM_LOC-1
              IF( ECDA_ID_LOC(NUM_LOC) .EQ. ECDA_ID_LOC(IDX) ) THEN
                IERR = 12
                MESSAG(1) = 'The location ID is not unique'
                MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
                MESSAG(3) = 'Location number is '
                WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
                MESSAG(4) = 'The location ID is case sensitive'
                CALL PRTERR( IERR, CALLER, 4 )
              RETURN
              END IF
            END DO
          END IF
!
!         Get the location name
          IF( CEXIST('NAME    ') ) THEN
            CALL NXTQOT( IDX, TMPNAME )
            IF( IDX .GT. 0 ) THEN
              ECDA_NM_LOC(NUM_LOC) = TMPNAME
            ELSE
              IERR = 13
              MESSAG(1) = 'Location NAME modifier missing quote string'
              MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
              MESSAG(3) = 'Location number is '
              WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 14
            MESSAG(1) = 'NAME modifier not entered on the LOCATION keyword'
            MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
            MESSAG(3) = 'Location number is '
            WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Get the location type
          IF( CEXIST('TYPE') ) THEN
            CALL NXTQOT( IDX, TMPNAME )
            IF( IDX .GT. 0 ) THEN
              CALL UPCASE( TMPNAME )
              ESD_LOC(NUM_LOC)%TYPE = TRIM(TMPNAME(1:8))
              IF( .NOT.(ESD_LOC(NUM_LOC)%TYPE.EQ.'UPLAND' .OR. ESD_LOC(NUM_LOC)%TYPE.EQ.'RIPARIAN' .OR. &
                ESD_LOC(NUM_LOC)%TYPE.EQ.'AQUATIC') ) THEN
                IERR = 15
                MESSAG(1) = 'Invalid TYPE entry on the LOCATION keyword - Modify the ESD keyword file'
                MESSAG(2) = 'Valid entries are "AQUATIC", "RIPARIAN", or "UPLAND"'
                MESSAG(3) = 'The current entry is "' // TRIM(ESD_LOC(NUM_LOC)%TYPE) // '"'
                MESSAG(4) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
                MESSAG(5) = 'Location number is '
                WRITE(MESSAG(5)(20:),'(I0)') NUM_LOC
                CALL PRTERR( IERR, CALLER, 5 )
                RETURN
              END IF
            ELSE
              IERR = 16
              MESSAG(1) = 'TYPE modifier missing quote string on the LOCATION keyword'
              MESSAG(2) = 'Modify the ESD keyword file'
              MESSAG(3) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
              MESSAG(4) = 'Location number is '
              WRITE(MESSAG(4)(20:),'(I0)') NUM_LOC
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
          ELSE
            IERR = 17
            MESSAG(1) = 'TYPE modifier not entered on the LOCATION keyword'
            MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
            MESSAG(3) = 'Location number is '
            WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Error check on the locus
          IF( CEXIST('LOCUS') ) THEN
            CALL NXTQOT( IDX, TMPNAME )
            IF( IDX .GT. 0 ) THEN
              CALL UPCASE( TMPNAME )
              ESD_LOC(NUM_LOC)%LOCUS = TMPNAME
!              IF( .NOT.(ESD_LOC(NUM_LOC)%LOCUS.EQ.'HANFORD' .OR. ESD_LOC(NUM_LOC)%LOCUS.EQ.'FARSIDE') ) THEN
!                IERR = 18
!                MESSAG(1) = 'Invalid LOCUS entry on the LOCATION keyword - Modify the ESD keyword file'
!                MESSAG(2) = 'Valid entries are "HANFORD" or "FARSIDE"'
!                MESSAG(3) = 'The current entry is "' // TRIM(ESD_LOC(NUM_LOC)%LOCUS) // '"'
!                MESSAG(4) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
!                MESSAG(5) = 'Location number is '
!                WRITE(MESSAG(5)(20:),'(I0)') NUM_LOC
!                CALL PRTERR( IERR, CALLER, 5 )
!                RETURN
!              END IF
            ELSE
              IERR = 19
              MESSAG(1) = 'Location LOCUS modifier missing quote string'
              MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
              MESSAG(3) = 'Location number is '
              WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
              MESSAG(4) = 'Location NAME = "'// TRIM(ECDA_NM_LOC(NUM_LOC)) // '"'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
          ELSE
            IERR = 20
            MESSAG(1) = 'LOCUS modifier not entered on the LOCATION keyword'
            MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
            MESSAG(3) = 'Location number is '
            WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
            MESSAG(4) = 'Location NAME = "'// TRIM(ECDA_NM_LOC(NUM_LOC)) // '"'
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
!         Error check on the EASTING modifier
          IF( CEXIST('EASTING ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(NUM_LOC)%EASTING = RTMP
            ELSE
              IERR = 21
              MESSAG(1) = 'Location EASTING modifier missing value'
              MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
              MESSAG(3) = 'Location number is '
              WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
              MESSAG(4) = 'Location NAME = "'// TRIM(ECDA_NM_LOC(NUM_LOC)) // '"'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
          ELSE
            IERR = 22
            MESSAG(1) = 'EASTING modifier not entered on the LOCATION keyword'
            MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
            MESSAG(3) = 'Location number is '
            WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
            MESSAG(4) = 'Location NAME = "'// TRIM(ECDA_NM_LOC(NUM_LOC)) // '"'
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
!         Error check on the NORTHING modifier
          IF( CEXIST('NORTHING ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(NUM_LOC)%NORTHING = RTMP
            ELSE
              IERR = 23
              MESSAG(1) = 'Location NORTHING modifier missing value'
              MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
              MESSAG(3) = 'Location number is '
              WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
              MESSAG(4) = 'Location NAME = "'// TRIM(ECDA_NM_LOC(NUM_LOC)) // '"'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
          ELSE
            IERR = 24
            MESSAG(1) = 'NORTHING modifier not entered on the LOCATION keyword'
            MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
            MESSAG(3) = 'Location number is '
            WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
            MESSAG(4) = 'Location NAME = "'// TRIM(ECDA_NM_LOC(NUM_LOC)) // '"'
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
!         Error check on the MILE modifier
          IF( CEXIST('MILE') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(NUM_LOC)%MILE = RTMP
            ELSE
              IERR = 25
              MESSAG(1) = 'Location MILE modifier missing value'
              MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
              MESSAG(3) = 'Location number is '
              WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
              MESSAG(4) = 'Location NAME = "'// TRIM(ECDA_NM_LOC(NUM_LOC)) // '"'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
          ELSE
            ESD_LOC(NUM_LOC)%MILE = -1.0
          END IF
!
!         Error check on the AREA modifier
          IF( CEXIST('AREA') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(NUM_LOC)%AREA = RTMP
            ELSE
              IERR = 26
              MESSAG(1) = 'Location AREA modifier missing value'
              MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
              MESSAG(3) = 'Location number is '
              WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
              MESSAG(4) = 'Location NAME = "'// TRIM(ECDA_NM_LOC(NUM_LOC)) // '"'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
          ELSE
            ESD_LOC(NUM_LOC)%AREA = -1.0
          END IF
!
!         Get the active media and set up map indices
          NUM_M = 0
          IF( CEXIST('GWAT') ) THEN
            ESD_LOC(NUM_LOC)%GWAT = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,IGWAT) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%GWAT = .FALSE.
            ECDA_LOC_MED(NUM_LOC,IGWAT) = -1
          END IF
!
          IF( CEXIST('SEEP') ) THEN
            ESD_LOC(NUM_LOC)%SEEP = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,ISEEP) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%SEEP = .FALSE.
            ECDA_LOC_MED(NUM_LOC,ISEEP) = -1
          END IF
!
          IF( CEXIST('SWAT') ) THEN
            ESD_LOC(NUM_LOC)%SWAT = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,ISWAT) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%SWAT = .FALSE.
            ECDA_LOC_MED(NUM_LOC,ISWAT) = -1
          END IF
!
          IF( CEXIST('PWAT') ) THEN
            ESD_LOC(NUM_LOC)%PWAT = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,IPWAT) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%PWAT = .FALSE.
            ECDA_LOC_MED(NUM_LOC,IPWAT) = -1
          END IF
!
          IF( CEXIST('SEDI') ) THEN
            ESD_LOC(NUM_LOC)%SEDI = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,ISEDI) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%SEDI = .FALSE.
            ECDA_LOC_MED(NUM_LOC,ISEDI) = -1
          END IF
!
          IF( CEXIST('SORP') ) THEN
            ESD_LOC(NUM_LOC)%SORP = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,ISORP) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%SORP = .FALSE.
            ECDA_LOC_MED(NUM_LOC,ISORP) = -1
          END IF
!
          IF( CEXIST('SODR') ) THEN
            ESD_LOC(NUM_LOC)%SODR = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,ISODR) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%SODR = .FALSE.
            ECDA_LOC_MED(NUM_LOC,ISODR) = -1
          END IF
!
          IF( CEXIST('SOGW') ) THEN
            ESD_LOC(NUM_LOC)%SOGW = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,ISOGW) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%SOGW = .FALSE.
            ECDA_LOC_MED(NUM_LOC,ISOGW) = -1
          END IF
!
          IF( CEXIST('SOSW') ) THEN
            ESD_LOC(NUM_LOC)%SOSW = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,ISOSW) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%SOSW = .FALSE.
            ECDA_LOC_MED(NUM_LOC,ISOSW) = -1
          END IF
!
          IF( CEXIST('AIRC') ) THEN
            ESD_LOC(NUM_LOC)%AIRC = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,IAIRC) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%AIRC = .FALSE.
            ECDA_LOC_MED(NUM_LOC,IAIRC) = -1
          END IF
!
          IF( CEXIST('AIRD') ) THEN
            ESD_LOC(NUM_LOC)%AIRD = .TRUE.
            NUM_M = NUM_M + 1
            NUM_R = NUM_R + 1
            ECDA_LOC_MED(NUM_LOC,IAIRD) = NUM_R
          ELSE
            ESD_LOC(NUM_LOC)%AIRD = .FALSE.
            ECDA_LOC_MED(NUM_LOC,IAIRD) = -1
          END IF
!
          IF (NUM_M .EQ. 0 ) THEN
            IERR = 27
            MESSAG(1) = 'At least one media is required at a location'
            MESSAG(2) = 'Location ID = "' // TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
            MESSAG(3) = 'Modify the LOCATION keyword to indicate the desired media solutions'
            MESSAG(4) = 'Valid media IDs are: GWAT SEEP SWAT PWAT SEDI SORP SODR SOGW SOSW AIRC AIRD'
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
          IF( CEXIST('IRG_SWAT') ) THEN
            CALL NXTQOT( IDX, TMPNAME )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(NUM_LOC)%ID_SWAT = TMPNAME(1:6)
            ELSE
              IERR = 28
              MESSAG(1) = 'Location IRG_SWAT modifier missing quote string'
              MESSAG(2) = 'Location ID = "'// TRIM(ECDA_ID_LOC(NUM_LOC)) // '"'
              MESSAG(3) = 'Location number is '
              WRITE(MESSAG(3)(20:),'(I0)') NUM_LOC
              MESSAG(4) = 'Location NAME = "'// TRIM(ECDA_NM_LOC(NUM_LOC)) // '"'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
          ELSE
            ESD_LOC(NUM_LOC)%ID_SWAT = '-Null-'
          END IF
!
        CASE( 'TIMES' ) ! TIMES Keyword
          DO ITIM = 1, NVALUE
            NUM_TIM = NUM_TIM + 1
            ECDA_TIMES(NUM_TIM) = VALUE(ITIM)
          END DO
!
        CASE DEFAULT ! Other Keyword
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE
!
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
!
      CHARACTER(LEN=*) :: MESSAGE ! Character string to write
      CHARACTER(LEN=*) :: PLACE   ! Place to output the mesage
      LOGICAL :: LDATE            ! Only print date if LDATE=.TRUE.
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
      SUBROUTINE TEST_MAPREAD( FN_MAP, UN_MAP, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine is used only in debug mode to test the reading
!!    and writing of map data.
!!
!!  History:
!!
!!    Paul W. Eslinger :  7 Oct 2002 : SAC Rev. 1
!!    Carmen Arimescu  :  7 Oct 2002 : SAC Rev. 1
!!
!!**********************************************************************
!
! *** Global variables
!
      USE ECDA_Mod
      USE Errors_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      CHARACTER(LEN=*), INTENT(IN) :: FN_MAP ! File name for the map file
      INTEGER, INTENT(IN)    :: UN_MAP       ! Unit number for the map file
      INTEGER, INTENT(INOUT) :: IERR         ! Error number
!
! *** Local variables
!
      CHARACTER(LEN=12) :: CALLER = 'TEST_MAPREAD' ! Name of this routine
!
      INTEGER :: ISTAT ! System status variable
      INTEGER :: IOS             ! I/O Status from open, read, or close of file
      INTEGER :: J, K            ! Index variables for writing
      CHARACTER(LEN=6) :: TMP_ID ! Temporary ID for error checking
!
!
      CHARACTER(LEN=200) :: TEST_ECDA_PTITLE ! Problem title line from generating program
      CHARACTER(LEN= 10) :: TEST_ECDA_PRGNAM ! Program name of generating program
      CHARACTER(LEN=  8) :: TEST_ECDA_PRGVER ! Program version number of generating program
      CHARACTER(LEN= 12) :: TEST_ECDA_PRGDAT ! Program date of generating program
      CHARACTER(LEN= 16) :: TEST_ECDA_USRNAM ! User name from generating program
      CHARACTER(LEN= 14) :: TEST_ECDA_CRUNID ! Run identification number from generating program
      INTEGER :: TEST_ECDA_BLOCK  ! Number of records with data in a time block
      INTEGER :: TEST_ECDA_RECLEN ! Record length of records in the data file
      INTEGER :: TEST_ECDA_NREAL  ! Number of realizations in the data file
      INTEGER :: TEST_ECDA_NTIMES ! Number of times at which the calculations are to be performed
      INTEGER :: TEST_ECDA_NLOCS  ! Number of locations at which the calculations are to be performed
      INTEGER :: TEST_NUM_TMP     ! Temporaly counter
!
      INTEGER, ALLOCATABLE :: TEST_ECDA_TIMES(:)            ! (TEST_ECDA_NTIMES) Times for the calculations
      CHARACTER(LEN=6), ALLOCATABLE :: TEST_ECDA_ID_LOC(:)  ! (TEST_ECDA_NLOCS)  Locations for the calculation
      INTEGER, ALLOCATABLE :: TEST_ECDA_LOC_MED(:,:)        ! (ECDA_NLOCS,ECDA_NMED) Record numbers for all
!                                                             location and media for the first time step
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Open the ECDA map file
!
      OPEN(UNIT=UN_MAP, IOSTAT=IOS, STATUS='OLD', FILE=FN_MAP )
      IF( IOS .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Error opening ECDA map file'
        WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
        MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_PTITLE
      IF( TEST_ECDA_PTITLE .NE. ECDA_PTITLE ) THEN
        IERR = 2
        MESSAG(1) = 'Error reading TEST_ESDA_PTITLE '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_PRGNAM
      IF( TEST_ECDA_PRGNAM .NE. ECDA_PRGNAM ) THEN
        IERR = 3
        MESSAG(1) = 'Error reading TEST_ESDA_PRGNAM '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_PRGVER
      IF( TEST_ECDA_PRGVER .NE. ECDA_PRGVER) THEN
        IERR = 4
        MESSAG(1) = 'Error reading TEST_ECDA_PRGVER '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_PRGDAT
      IF( TEST_ECDA_PRGDAT .NE. ECDA_PRGDAT ) THEN
        IERR = 5
        MESSAG(1) = 'Error reading TEST_ECDA_PRGDAT '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_USRNAM
      IF( TEST_ECDA_USRNAM .NE. ECDA_USRNAM ) THEN
        IERR = 6
        MESSAG(1) = 'Error reading TEST_ECDA_USRNAM '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_CRUNID
      IF( TEST_ECDA_CRUNID .NE. ECDA_CRUNID) THEN
        IERR = 7
        MESSAG(1) = 'Error reading TEST_ECDA_CRUNID '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_BLOCK
      IF( TEST_ECDA_BLOCK .NE. ECDA_BLOCK ) THEN
        IERR = 8
        MESSAG(1) = 'Error reading TEST_ECDA_BLOCK '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_RECLEN
      IF( TEST_ECDA_RECLEN .NE. ECDA_RECLEN ) THEN
        IERR = 9
        MESSAG(1) = 'Error reading TEST_ESDA_PRGNAM '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_NREAL
      IF( TEST_ECDA_NREAL .NE. ECDA_NREAL ) THEN
        IERR = 10
        MESSAG(1) = 'Error reading TEST_ECDA_RECLEN '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Read the time information
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_NTIMES

      IF( TEST_ECDA_NTIMES .NE. ECDA_NTIMES ) THEN
        IERR = 11
        MESSAG(1) = 'Error reading TEST_ECDA_NTIMES '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
      ALLOCATE( TEST_ECDA_TIMES(TEST_ECDA_NTIMES), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 12
        MESSAG(1) = 'Error allocating TEST_ECDA_TIMES(TEST_ECDA_NTIMES)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF

      DO J = 1, TEST_ECDA_NTIMES
        READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_TIMES(J)
        IF(ISTAT .NE. 0 ) THEN
          IERR = 13
          MESSAG(1) = 'Error reading TEST_ECDA_TIMES'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END DO
!
! *** Read the location information
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_NLOCS
      IF( TEST_ECDA_NLOCS .NE. ECDA_NLOCS ) THEN
        IERR = 14
        MESSAG(1) = 'Error reading TEST_ECDA_NLOCS '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
      ALLOCATE( TEST_ECDA_ID_LOC(TEST_ECDA_NLOCS), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 15
        MESSAG(1) = 'Error allocating TEST_ECDA_ID_LOC(TEST_ECDA_NLOCS)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
      DO J = 1, TEST_ECDA_NLOCS
        READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_ECDA_ID_LOC(J)
        IF(ISTAT .NE. 0 ) THEN
          IERR = 16
          MESSAG(1) = 'Error reading TEST_ECDA_ID_LOC'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END DO
!
! *** Read and discard the media information
!     These will be used by SACView but are hard-coded in this program
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TEST_NUM_TMP
      IF( TEST_NUM_TMP .NE. ECDA_NMED ) THEN
        IERR = 17
        MESSAG(1) = 'Error reading TEST_NUM_TMP '
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      DO J = 1, TEST_NUM_TMP
        READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TMP_ID
        IF(ISTAT .NE. 0 ) THEN
          IERR = 18
          MESSAG(1) = 'Error reading TMP_ID(TEST_NUM_TMP)'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END DO
!
! *** Read the map index data from the map file
!
      ALLOCATE( TEST_ECDA_LOC_MED(TEST_ECDA_NLOCS,ECDA_NMED), STAT= ISTAT )
       IF(ISTAT .NE. 0 ) THEN
        IERR = 19
        MESSAG(1) = 'Error allocating TEST_ECDA_LOC_MED(TEST_ECDA_NLOCS,ECDA_NMED)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF

      DO K = 1, TEST_ECDA_NLOCS
        READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=8000, FMT=*) TMP_ID, (TEST_ECDA_LOC_MED(K,J),J=1,ECDA_NMED)
        IF( TMP_ID .NE. ECDA_ID_LOC(K) ) THEN
          IERR = 20
          MESSAG(1) = 'Mismatch on header location ID and record index ID tag'
          MESSAG(2) = 'Location ID from header = ' // TRIM(ECDA_ID_LOC(K))
          MESSAG(3) = 'Record index ID tag was = ' // TRIM(TMP_ID)
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
        DO J=1,ECDA_NMED
          IF( TEST_ECDA_LOC_MED(K,J) .NE. ECDA_LOC_MED(K,J) ) THEN
            IERR = 21
            MESSAG(1) = 'Mismatch on record index'
            MESSAG(2) = 'Location index = '
            WRITE(MESSAG(2)(16:),*)K
            MESSAG(3) = 'Media index = '
            WRITE(MESSAG(3)(14:),*)J
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        END DO
      END DO
!
! *** Close the ECDA map file and return
!
      CLOSE( UNIT=UN_MAP, IOSTAT=IOS, ERR=9000 )
      RETURN
!
! *** Error Branch: The header lines were not read successfully
!
 7000 CONTINUE
      IERR = 22
      MESSAG(1) = 'Error reading header lines from the ECDA map file'
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** Error Branch: The map data were not read successfully
!
 8000 CONTINUE
      IERR = 23
      MESSAG(1) = 'Error reading record number lines from the ECDA map file'
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** Error Branch: The ECDA map file was not closed successfully
!
 9000 CONTINUE
      IERR = 24
      MESSAG(1) = 'Error closing ECDA map file'
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
      END SUBROUTINE

