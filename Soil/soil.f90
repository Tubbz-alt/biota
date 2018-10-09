!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
MODULE Control_Mod
!! Module Control_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  History:
!
!    Paul W. Eslinger : 10 Jul 2003 : Version 1.0
!    Paul W. Eslinger : 28 Jun 2005 : Allow soil type selections
!    Paul W. Eslinger : 28 Jul 2006 : Add flag for all realizations
!
      CHARACTER(LEN=200) :: PTITLE ! Title for this program run
      INTEGER :: NREAL       ! Number of realizations
      LOGICAL :: BUG_LOOP    ! Flag for debug writes on looping information
      LOGICAL :: BUG_CONC    ! Flag for debug writes on concentration data
      LOGICAL :: BUG_CONC_BQ ! Flag for debug writes on radioactive concentration data
!                              using Bq instead of Ci
      LOGICAL :: BUG_COMP    ! Flag for debug writes on computing soil concentrations
!
      LOGICAL :: REPORT ! Flag whether the report file is open
      LOGICAL :: EXECUT ! Flag whether the problem is to be executed (.false.=error check only)
!
      LOGICAL :: COMPUTE_SODR ! Flag whether the SODR solution is to be computed
      LOGICAL :: COMPUTE_SOGW ! Flag whether the SOGW solution is to be computed
      LOGICAL :: COMPUTE_SOSW ! Flag whether the SOSW solution is to be computed
      LOGICAL :: USE_ALL_REL  ! Flag whether all realizations are computed
!
      INTEGER :: SOI_NUM_ANA ! Number of analytes in this scenario
!
      INTEGER :: IRIG_START    ! Start year for irrigation
      REAL    :: IRIG_RATE     ! Irrigation rate (cm/yr)
      REAL    :: IRIG_NET_FRAC ! Fraction of irrigation rate that infiltrates
!
      REAL    :: THETA_IRG  ! Surface soil volumetric water content with irrigation (mL/cm^3)
      REAL    :: THETA_DRY  ! Surface soil volumetric water content without irrigation (mL/cm^3)
!
      INTEGER :: SEASON_START ! Start day for the growing season
      INTEGER :: SEASON_END   ! End day for the growing season
!
      LOGICAL, ALLOCATABLE :: REL_USE(:) ! Realization use logical vector
!
      REAL :: SOIL_DENSITY  ! Surface soil density (g/cm^3)
      REAL :: SOIL_DEPTH    ! Surface soil depth (cm)
!
      INTEGER :: YEAR_START ! Year that the simulation starts
      INTEGER :: YEAR_STOP  ! Year that the simulation stops
      INTEGER :: YEAR_CLOSE ! Year of site closure
!
!! End Module Control_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
MODULE ESD_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information from the ESD file that will be
!    used in the soil SOIL code, in addition, it contains some information
!    defined in this code that depends on location, time, or analyte.
!
!  History:
!
!    Paul W. Eslinger : 17 Mar 2003 : Version 1.0
!
      INTEGER :: ESD_NUM_ANA ! Number of analytes in the ESD file
      INTEGER :: ESD_NUM_LOC ! Number of locations in the ESD file
      INTEGER :: ESD_NUM_TIM ! Number of times in the ESD file
      INTEGER :: ESD_NREAL   ! Number of realizations in the ESD file
!
      CHARACTER(LEN=200) :: ESD_TITLE ! Title in the ESD file
!
!     Type definition for ESD time data
!
      TYPE ESD_TIM_TYPE
        INTEGER :: TIME ! Times where data are stored
      END TYPE ESD_TIM_TYPE
      TYPE (ESD_TIM_TYPE), ALLOCATABLE :: ESD_TIM(:) ! The ESD time variable for impacts
!
!     Type definition for ESD location data
!
      TYPE ESD_LOC_TYPE
        CHARACTER(LEN= 6) :: ID   ! ID for a location
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a location
        CHARACTER(LEN= 8) :: TYPE ! Location type (AQUATIC, RIPARIAN, or UPLAND)
        LOGICAL :: COMP           ! Compute flag for this location
        LOGICAL :: AIRD           ! Flag for air deposition values
        LOGICAL :: SODR           ! Flag for dry soil concentrations
        LOGICAL :: SOGW           ! Flag for soil concentrations using groundwater
        LOGICAL :: SOSW           ! Flag for soil concentrations using surface water
        LOGICAL :: GWAT           ! Flag for ground water concentrations
        LOGICAL :: SWAT           ! Flag for surface water concentrations
        INTEGER :: IX_SWAT        ! Index for the surface water irrigation location
        CHARACTER(LEN= 6) :: ID_SWAT ! ID for a surface water location for irrigation
      END TYPE ESD_LOC_TYPE
      TYPE (ESD_LOC_TYPE), ALLOCATABLE :: ESD_LOC(:) ! The ESD location variable for impacts
!
!     Type definition for ESD analyte data
!
      TYPE ESD_ANA_TYPE ! Type definition for ESD analyte data
        CHARACTER(LEN= 6) :: ID     ! Identification number for an analyte
        CHARACTER(LEN=72) :: NAME   ! Descriptive name for an analyte
        CHARACTER(LEN=2)  :: TYPE   ! Analyte type
        REAL              :: LAMBDA ! Decay term (1/yr)
        LOGICAL :: COMP             ! Flag whether this analyte is to be used
      END TYPE ESD_ANA_TYPE
      TYPE (ESD_ANA_TYPE), ALLOCATABLE :: ESD_ANA(:) ! The ESD analyte variable
!
      CHARACTER(LEN=20), ALLOCATABLE :: KD_MAP(:,:) ! Distribution coefficient map by location,
!                                                    and analyte (length of VLABEL)
      REAL, ALLOCATABLE :: KD(:,:,:) ! Distribution coefficient by location, analyte, and realization
!
      CHARACTER(LEN=20), ALLOCATABLE :: IN_MAP(:) ! Infiltration map by location (length of VLABEL)
      REAL, ALLOCATABLE :: INFILT(:,:) ! Infiltration value by location and realization (cm/yr)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
MODULE Files_Mod
!!! Module Files_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains file names and unit numbers
!
!  History:
!
!    Paul W. Eslinger : 19 Sep 2002 : Version 1.0
!    Paul W. Eslinger :  6 Jun 2007 : Update comments
!    Paul W. Eslinger :  7 Feb 2014 : (TIIA SCR-0003) Remove the "run" and "done" files
!
      INTEGER, PARAMETER :: MAXFN = 200 ! Length of a file name (same as LENQQQ in RDBLK)
      CHARACTER(LEN=MAXFN) :: FNTMP     ! Temporary file name
      CHARACTER(LEN=MAXFN) :: FNRPT     ! Name of the report file
      CHARACTER(LEN=MAXFN) :: FNKEY     ! Name of the input keyword file
!      CHARACTER(LEN=MAXFN) :: PATH_KEY  ! Path of the input keyword file name
      INTEGER :: IRPT                   ! Unit number for the report file
      INTEGER :: IKEY                   ! Unit number for the input keyword file
      INTEGER :: UN_ECDAMAP             ! Unit number for the ECDA map file
!
      CHARACTER(LEN=MAXFN) :: FN_MAP    ! File name for the concentration record map
!
      INTEGER :: IESD                   ! Unit number for the ESD keyword file
      CHARACTER(LEN=MAXFN) :: FN_ESD    ! Name of the input ESD keyword file
!
      INTEGER :: IKDS                   ! Unit number for the KdSoil data file
      CHARACTER(LEN=MAXFN) :: FN_KDSOIL ! File name for KdSoil data
!
      INTEGER :: IINF                   ! Unit number for the infiltration data file
      CHARACTER(LEN=MAXFN) :: FN_INFILT ! File name for infiltration data
!
!      INTEGER :: IDON                   ! Unit number for the "Done" file
!      CHARACTER(LEN=MAXFN) :: FN_DONE   ! File name for the "Done" file
!
!      INTEGER :: IRUN                   ! Unit number for the "Run" file
!      CHARACTER(LEN=MAXFN) :: FN_RUN    ! File name for the "Run" file
!
!     Unit number for the concentration files (only one open at a time)
      INTEGER :: ICON
!     Vector of file names by analyte for concentration data (ECDA files)
      CHARACTER(LEN=MAXFN), ALLOCATABLE :: FN_CON(:)
!
!!!! End Module Files_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
MODULE Iden_Mod
!++MODULE Iden_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains run and user identification information
!
!  History:
!
!    Paul W. Eslinger : 19 Sep 2002 : Version 1.0
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
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
MODULE Infilt_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information related to the infiltration data file
!
!  History:
!
!    Paul W. Eslinger :  9 Oct 2002 : Version 1.0
!
      CHARACTER(LEN=  8) :: IN_TYPE      ! The character string INFILT
      CHARACTER(LEN=200) :: IN_PTITLE    ! Program modification date from infiltration generator
      CHARACTER(LEN= 10) :: IN_PRGNAM    ! Program name from infiltration generator
      CHARACTER(LEN=  8) :: IN_PRGVER    ! Program version number from infiltration generator
      CHARACTER(LEN= 12) :: IN_PRGDAT    ! Program date from infiltration generator
      CHARACTER(LEN= 14) :: IN_CRUNID    ! Run identification number from infiltration generator
      CHARACTER(LEN= 16) :: IN_USRNAM    ! User name from infiltration generator
      INTEGER :: IN_NUM                  ! Number of infiltration stochastic variables
      INTEGER :: IN_NITR                 ! Number of infiltration stochastic iterations
      CHARACTER(LEN=20) :: IN_ID         ! Infiltration stochastic ID
      CHARACTER(LEN=20), ALLOCATABLE :: IN_UNITS(:)   ! Infiltration units by locations
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
MODULE Kdsoil_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information related to the KD (soil-water
!    distribution coefficient) data file
!
!  History:
!
!    Paul W. Eslinger : 28 Apr 2000 : Version 1.0
!    Paul W. Eslinger :  7 Oct 2002 : Revised for SAC Rev. 1
!
      CHARACTER(LEN=  8) :: KD_TYPE      ! The character string KDSOIL
      CHARACTER(LEN=200) :: KD_PTITLE    ! Program modification date from KD generator
      CHARACTER(LEN= 10) :: KD_PRGNAM    ! Program name from KD generator
      CHARACTER(LEN=  8) :: KD_PRGVER    ! Program version number from KD generator
      CHARACTER(LEN= 12) :: KD_PRGDAT    ! Program date from KD generator
      CHARACTER(LEN= 14) :: KD_CRUNID    ! Run identification number from KD generator
      CHARACTER(LEN= 16) :: KD_USRNAM    ! User name from KD generator
      INTEGER :: KD_NUM                  ! Number of KD stochastic variables
      INTEGER :: KD_NITR                 ! Number of KD stochastic iterations
      CHARACTER(LEN=20) :: KD_ID         ! KD stochastic ID
      CHARACTER(LEN=20), ALLOCATABLE :: KD_UNITS(:)   ! KD units by locations
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Soil_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information related to the time accumulation
!    of analytes in the soil
!
!  History:
!
!    Paul W. Eslinger :  8 Oct 2002 : Version 1.0
!    Paul W. Eslinger :  6 Jun 2007 : Update comments
!
!     The following arrays are to be dimensioned (ESD_NUM_TIM, ESD_NUM_REL)
!     and will contain data for all times and realizations at one location
      REAL, ALLOCATABLE :: AIRD(:,:) ! Air deposition
      REAL, ALLOCATABLE :: GWAT(:,:) ! Groundwater concentration
      REAL, ALLOCATABLE :: SWAT(:,:) ! Surface water concentration
      REAL, ALLOCATABLE :: SODR(:,:) ! Non-irrigated upland soil concentrations
      REAL, ALLOCATABLE :: SOGW(:,:) ! Groundwater irrigated upland soil concentrations
      REAL, ALLOCATABLE :: SOSW(:,:) ! Surface water irrigated upland soil concentrations
!
!     The following arrays are all dimensioned by the number of realizations
      REAL, ALLOCATABLE :: CVEC(:) ! Temporary concentration vector
      REAL, ALLOCATABLE :: ZERO(:) ! Temporary vector of zeros
!
      REAL, ALLOCATABLE :: MASS_SODR(:) ! Upper soil layer mass from air deposition
      REAL, ALLOCATABLE :: MASS_SOGW(:) ! Upper soil layer mass from groundwater irrigation
      REAL, ALLOCATABLE :: MASS_SOSW(:) ! Upper soi layer mass from surface water irrigation
!
      REAL, ALLOCATABLE :: DELTA_AIRD(:) ! Annual delta on air deposition
      REAL, ALLOCATABLE :: DELTA_GWAT(:) ! Annual delta on groundwater concentrations
      REAL, ALLOCATABLE :: DELTA_SWAT(:) ! Annual delta on surface water concentrations
!
      REAL, ALLOCATABLE :: SAVE_SODR(:) ! Saved value in growing season for air deposition only
      REAL, ALLOCATABLE :: SAVE_SOGW(:) ! Saved value in growing season for groundwater concentrations
      REAL, ALLOCATABLE :: SAVE_SOSW(:) ! Saved value in growing season for surface water concentrations
!
      REAL, ALLOCATABLE :: ANNUAL_AIRD(:) ! Annual value for air deposition
      REAL, ALLOCATABLE :: ANNUAL_GWAT(:) ! Annual value for groundwater concentrations
      REAL, ALLOCATABLE :: ANNUAL_SWAT(:) ! Annual value for surface water concentrations
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

      PROGRAM Soil
!!***********************************************************************************************************
!!
!!                                         Soil
!!                       Stochastic Soil Concentration Analysis
!!                   Toolkit for Integrated Impact Assessments (TIIA)
!!          Battelle, Pacific Northwest National Laboratories, Richland, Washington
!!
!!***********************************************************************************************************
!!
!!  Purpose:
!!    Soil is the top level routine for the upper soil layer environment of the TIIA, Version 1.
!!    This code calculates stochastic concentrations in soil using contamination from air deposition and 
!!    irrigation using groundwater or surface water.
!!
!! Reference:
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!***********************************************************************************************************
!!
!!  Module History:
!!    Paul W. Eslinger : 17 Oct 2002 : Version 1.0
!!    Paul W. Eslinger : 24 Jul 2003 : SCR 1013 - Fix ECDA label errors
!!                       Fix units on the KD variable, the irrigation rate units,
!!                       the radioactive decay rate units, and days per year
!!    Paul W. Eslinger : 25 Sep 2003 : SCR 1033 - Trap negative
!!                       concentrations in media data
!!    Paul W. Eslinger : 18 Dec 2003 : SCR 1044 - Set small negative
!!                       values to zero.  Change units on the infiltration data.
!!    Paul W. Eslinger : 29 Dec 2003 : Add irrigation net infiltration
!!    Paul W. Eslinger : 14 Dec 2004 : SCR-1071 Set AOVERL to annual rates
!!    Paul W. Eslinger : 31 May 2005 : SCR-1080 Set AIRD to annual rate
!!    Paul W. Eslinger : 28 Jun 2005 : SCR-1080 Allow soil type compute options
!!    Paul W. Eslinger : 28 Jul 2006 : SCR-1139 Fix LIST option problems
!!    Paul W. Eslinger :  6 Jun 2007 : Update for TIIA logic and copyright, fix
!!                                     error trap in ESD_INIT
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!    Paul W. Eslinger :  8 Apr 2013 : (TIIA SCR-0003) Remove the "run" and "done" files
!!    Paul W. Eslinger :  7 Feb 2014 : (TIIA SCR-0003) Remove the "run" and "done" files
!!
!!***********************************************************************************************************
!!
!!  General Notes:
!!
!!    1. This code can compute soil concentrations at one or more impact locations
!!       in a single run.
!!
!!    2. This code can compute soil concentrations for one or more analytes in a
!!       single run.
!!
!!    3. This code provides stochastic results.  It also provides deterministic
!!       results in that a run of one realization is allowed.
!!
!!    4. This program is a batch-type program.  The sole input to start a program
!!       run is the name of a simulation control file.
!!
!!    5. Even though Soil runs in a stand-alone mode, previous programs in the TIIA
!!       suite of codes must execute prior to the Soil code to generate the set of
!!       air, groundwater and surface water concentrations used for input.
!!
!!    6. This program is written in Fortran 95 (free format).
!!
!!***********************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Errors_Mod
      USE Files_Mod
      USE Control_Mod, ONLY: BUG_LOOP, REPORT, EXECUT
      USE Esd_Mod
      USE Ecda_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      CHARACTER(LEN=4) :: CALLER = 'Soil' ! Name of this routine
!
      INTEGER :: IERR  ! Error number
      INTEGER :: IANA  ! Analyte looping index
      LOGICAL :: THERE ! File existence variable
!
      CHARACTER(LEN=10) :: SDATE ! System date in the form YYYYMMDD
      CHARACTER(LEN=10) :: STIME ! System time in the form HHMMSS.SSS
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Program, run identification, and initialization
      CALL IDEN_SET( )
      CALL INIT( )
!
! *** Open the run signal file
!      OPEN(IRUN,FILE=FN_RUN)
!
! *** Make sure there is no "done" signal file
!      INQUIRE(FILE=FN_DONE,EXIST=THERE)
!      IF( THERE ) THEN
!        OPEN(IDON,FILE=FN_DONE)
!        CLOSE(IDON,STATUS='DELETE')
!      END IF
!
! *** Open the Soil keyword file
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the keyword file'
!        WRITE(*,*) 'Stop in '//CALLER
        GO TO 9999
      END IF
!
! *** Define the report file
      CALL TELLTIME( 'Extracting the report file name', 'SCREEN', .FALSE., IRPT )
      CALL READ_KEYS_REPORT( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the report file'
        WRITE(*,*) 'Stop in '//CALLER
        STOP
      END IF
!
! *** Print the initial portion of the banner page to the report file
      CALL BANNER_1( )
!
! *** Read the first pass of the Soil control keywords
      CALL TELLTIME( 'Reading SOIL Keywords - First Pass', 'SCREEN', .FALSE., IRPT )
      CALL KEY_SOIL_1( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Open the environmental settings definition keyword file
      IF( BUG_LOOP ) CALL TELLTIME( 'Opening the ESD keyword file', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_ESD( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Read the first pass of environmental keywords to collect
!     array dimension information
      CALL TELLTIME( 'Reading ESD Keywords - First Pass', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_1( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Set up the dynamic storage for the problem size identified
!     in the environmental keyword file
      IF( BUG_LOOP ) CALL TELLTIME( 'Allocating ESD Memory', 'SCREEN', .FALSE., IRPT )
      CALL ESD_MEMORY( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Initialize environmental settings data and the "use" information
      IF( BUG_LOOP ) CALL TELLTIME( 'Initializing ESD keyword data', 'SCREEN', .FALSE., IRPT )
      CALL ESD_INIT( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Read the second pass of environmental keywords
!     Save time, analyte, location information for impacts
!     Save concentration file names
      CALL TELLTIME( 'Reading ESD Keywords - Second Pass', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_2( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Read the rest of the Soil control keywords
      CALL TELLTIME( 'Reading SOIL Keywords - Second Pass', 'SCREEN', .FALSE., IRPT )
      CALL KEY_SOIL_2( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Perform error checking on keywords and the problem definition
      IF( BUG_LOOP ) CALL TELLTIME( 'Checking SOIL keyword data', 'SCREEN', .FALSE., IRPT )
      CALL CHECK_SOIL( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
      CALL BANNER_2( )
!
! *** Echo the problem definition to the report file
      IF( BUG_LOOP ) CALL TELLTIME( 'Echoing SOIL definitions to the report file', 'SCREEN', .FALSE., IRPT )
      CALL ECHO( )
!
!---- Start execution of the problem ----------------------------------------
!
      IF( .NOT. EXECUT ) THEN
        MESSAG(1) = 'Execution not requested'
        MESSAG(2) = 'Use the EXECUTE card'
        CALL PRTERR( 0, CALLER, 2 )
        GO TO 9999
      END IF
!
! *** Open and read the file containing the KDSOIL stochastic data
      CALL OPEN_KDSOIL( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Open and read the file containing the infiltration stochastic data
      CALL OPEN_INFILT( IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Get the record number map for the concentration data
!     The same map applies for all analytes
      IF( BUG_LOOP ) CALL TELLTIME( 'Opening concentration map data file', 'SCREEN', .FALSE., IRPT )
      CALL ECDA_MAPREAD( FN_MAP, UN_ECDAMAP, IERR )
      IF( IERR .NE. 0 ) GO TO 9999
!
! *** Echo the header lines from the map file to the report file
      CALL ECDA_MAPECHO( IRPT )
!
! *** The outer loop on processing soil is by analyte
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE ! Only process the requested analytes
        CALL TELLTIME ( 'Processing concentrations for '//ESD_ANA(IANA)%ID, 'BOTH', .FALSE., IRPT )
        CALL PROCESS_SOIL( IANA, IERR )
        IF( IERR .NE. 0 ) GO TO 9999
      END DO
!
! *** End of the calculations - final cleanup and messages
!
 9999 CONTINUE
!
! *** Write the "Done" file
!      IF( REPORT ) WRITE(IRPT,3030) 'FN_DONE = ', TRIM(FN_DONE)
! 3030 FORMAT(/1X,A,'"',A,'"')
!
!      OPEN(IDON,FILE=FN_DONE)
!      IF( IERR .NE. 0 ) THEN
!        WRITE(IDON,*) IERR, 'Error termination'
!      ELSE
!        WRITE(IDON,*) IERR, 'Normal termination'
!      END IF
!      CLOSE(IDON)
!
! *** Close the run file
!      CLOSE(IRUN,STATUS='DELETE')
!
! *** Final message on program termination
!
      CALL DATE_AND_TIME( SDATE, STIME )
!
      IF( REPORT ) THEN
        IF( IERR .NE. 0 ) THEN
          MESSAG(1) = 'Abnormal Run Termination Due to Errors'
          CALL PRTERR( IERR, CALLER, 1)
          WRITE(IRPT,'(4X,A)') 'Run Completed on '//SDATE(5:6)//'/'//SDATE(7:8)//'/'//SDATE(1:4)//&
            ' at '//STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
          WRITE(*,*) 'Error Termination'
          STOP
        ELSE
          MESSAG(1) = 'Normal Termination'
          CALL PRTERR( IERR, CALLER, 1)
          WRITE(IRPT,'(4X,A)') 'Run Completed on '//SDATE(5:6)//'/'//SDATE(7:8)//'/'//SDATE(1:4)//&
            ' at '//STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
          WRITE(*,*) 'Normal Run Termination'
          STOP
        END IF
      ELSE
        IF( IERR .NE. 0 ) THEN
          WRITE(*,*) 'Abnormal Run Termination Due to Errors'
          WRITE(*,'(4X,A)') 'Run Completed on '//SDATE(5:6)//'/'//SDATE(7:8)//'/'//SDATE(1:4)//&
            ' at '//STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
          WRITE(*,*) 'Error Termination'
          STOP
        ELSE
          WRITE(*,*) 'Normal Termination'
          WRITE(*,'(4X,A)') 'Run Completed on '//SDATE(5:6)//'/'//SDATE(7:8)//'/'//SDATE(1:4)//&
            ' at '//STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
          WRITE(*,*) 'Normal Run Termination'
          STOP
        END IF
      END IF
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
!!    Paul W. Eslinger : 21 Nov 2002 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Update for TIIA logic and copyright
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
      WRITE(IRPT,1000)
 1000 FORMAT(//)
!
      WRITE(IRPT,1010) '  SSS     OOOOO   IIIIIII  L      '
      WRITE(IRPT,1010) 'S    SS  O     O     I     L      '
      WRITE(IRPT,1010) ' S    S  O     O     I     L      '
      WRITE(IRPT,1010) '  SS     O     O     I     L      '
      WRITE(IRPT,1010) 'S   S    O     O     I     L      '
      WRITE(IRPT,1010) 'SS   S   O     O     I     L      '
      WRITE(IRPT,1010) '  SSS     OOOOO   IIIIIII  LLLLLLL'
 1010 FORMAT(22X,A)
!
      WRITE(IRPT,1020) TRIM(PRGNAM), TRIM(PRGVER), TRIM(PRGDAT)
 1020 FORMAT(//32X,A,1X,A/24X,'Last Modified on ',A)
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
!!    Paul W. Eslinger :  7 Oct 2002 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Update comments
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE QA_Mod
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

      SUBROUTINE CHECK_SOIL( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the information from the SOIL keyword
!!    cards looking for problem definition problems.  In addition,
!!    the indices for surface water irrigation locations are set
!!    during the error checks.
!!
!!  History:
!!
!!    Paul W. Eslinger : 31 Oct 2002 : Version 1.0
!!    Paul W. Eslinger : 29 Dec 2003 : Add irrigation net infiltration
!!    Paul W. Eslinger : 28 Jun 2005 : Add compute solution options
!!    Paul W. Eslinger :  6 Jun 2007 : Update comments
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
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'CHECK_SOIL' ! Name of this subroutine
!
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: ILOC ! Location looping index
      INTEGER :: ICNT ! Realization counting variable
      INTEGER :: IREL ! Realization looping variable
      INTEGER :: ILEN ! Temporary length variable
      INTEGER :: INDX ! Temporary index variable
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** User name
!
      IF( USRNAM .EQ. ' ' ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The user name is missing'
        MESSAG(2) = 'Use the USER keyword in the SOIL keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Title
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The problem title is missing'
        MESSAG(2) = 'Use the TITLE keyword in the SOIL keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Control errors
!
      IF( NREAL .LE. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'At least one realization must be run'
        MESSAG(2) = 'Modify the REALIZATION keyword in the ESD keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
       END IF
!
! *** Compute solution errors
!
      ICNT = 0
      IF( COMPUTE_SODR ) ICNT = ICNT + 1
      IF( COMPUTE_SOGW ) ICNT = ICNT + 1
      IF( COMPUTE_SOSW ) ICNT = ICNT + 1
      IF( ICNT .LT. 1 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'At least one soil type must be computed'
        MESSAG(2) = 'Modify the COMPUTE keyword in the SOIL keyword file'
        MESSAG(3) = 'Modifiers entered must be one or more of SODR, SOGW and SOSW'
        CALL PRTERR( IERR, CALLER, 3 )
       END IF
!
! *** Check on the number of activated realizations
!
      ICNT = 0
      DO IREL = 1, NREAL
        IF( REL_USE(IREL) ) ICNT = ICNT + 1
      END DO
      IF( ICNT .EQ. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'At least one realization is required in SOIL'
        MESSAG(2) = 'Modify the REALIZATION keyword in the SOIL keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Check on the growing season definition
!
      IF( SEASON_START.LT.1 .OR. SEASON_START.GT.365 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The growing season start date is invalid'
        MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
        MESSAG(3) = 'Value entered was '
        WRITE(MESSAG(3)(19:),'(I0)') SEASON_START
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( SEASON_END.LT.1 .OR. SEASON_END.GT.365 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The growing season end date is invalid'
        MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
        MESSAG(3) = 'Value entered was '
        WRITE(MESSAG(3)(19:),'(I0)') SEASON_END
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( SEASON_END .LT. SEASON_START ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The growing season end date is before the start date'
        MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Check the period times
!
      IF( YEAR_START .LT. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'Invalid start year for the simulation'
        MESSAG(2) = 'Check the modifier START on the ESD PERIOD keyword'
        MESSAG(3) = 'Value = '
        WRITE(MESSAG(3)(9:),'(I0)') YEAR_START
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( YEAR_STOP .LT. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'Invalid stop year for the simulation'
        MESSAG(2) = 'Check the modifier STOP on the ESD PERIOD keyword'
        MESSAG(3) = 'Value = '
        WRITE(MESSAG(3)(9:),'(I0)') YEAR_STOP
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( YEAR_STOP .LT. YEAR_START ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'Invalid stop and stop year combination for the simulation'
        MESSAG(2) = 'Check the modifiers START and STOP on the ESD PERIOD keyword'
        MESSAG(3) = 'Start year = '
        WRITE(MESSAG(3)(14:),'(I0)') YEAR_START
        MESSAG(3) = TRIM(MESSAG(3)) // ' stop year ='
        ILEN = LEN_TRIM(MESSAG(3))
        WRITE(MESSAG(3)((ILEN+1):),'(I0)') YEAR_STOP
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( YEAR_CLOSE .LT. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'Invalid site closure year for the simulation'
        MESSAG(2) = 'Check the modifier CLOSURE on the ESD PERIOD keyword'
        MESSAG(3) = 'Value = '
        WRITE(MESSAG(3)(9:),'(I0)') YEAR_CLOSE
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Irrigation related variables
!
      IF( IRIG_RATE .LT. 0.0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The irrigation rate is less than zero'
        MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
      IF( IRIG_NET_FRAC.LT.0.0 .OR. IRIG_NET_FRAC.GT.1.0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The irrigation net infiltration fraction is not in the range (0,1)'
        MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
      IF( THETA_IRG.LT.0.0 .OR. THETA_IRG.GT.1.0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The soil moisture fraction under irrigation is not in the range (0,1)'
        MESSAG(2) = 'Change the THETAIRG modifier on the IRRIGATE keyword in the ESD keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
      IF( THETA_DRY.LT.0.0 .OR. THETA_DRY.GT.1.0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The soil moisture fraction (dry conditions) is not in the range (0,1)'
        MESSAG(2) = 'Change the THETADRY modifier on the IRRIGATE keyword in the ESD keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
      IF( IRIG_START .LT. YEAR_START ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The first year of irrigation is before the start year'
        MESSAG(2) = 'Modify the IRRIGATE keyword in the ESD keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** File name errors
!
      IF( FN_KDSOIL .EQ. ' ' ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The soil-water KD file data file name is missing'
        MESSAG(2) = 'Use the KDSOIL modifier the ESD FILE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( FN_INFILT .EQ. ' ' ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The input infiltration data file name is missing'
        MESSAG(2) = 'Use the INFILT modifier on the ESD FILE Keyword'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** Check for all concentration files
!
      DO IANA = 1, ESD_NUM_ANA
        IF( ESD_ANA(IANA)%COMP ) THEN
          IF( FN_CON(IANA) .EQ. ' ' ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'The input ECDA concentration file name is missing'
            MESSAG(2) = 'Analyte ID = '//TRIM(ESD_ANA(IANA)%ID)
            MESSAG(3) = 'Check the C_ECDA modifier on the FILE keyword in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END IF
      END DO
!
! *** Analyte-related errors
!
      IF( SOI_NUM_ANA .EQ. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'At least one analyte must be requested'
        MESSAG(2) = 'Modify the ANALYTE keyword in the SOIL keyword file'
        CALL PRTERR( IERR, CALLER, 2 )
      END IF
!
! *** KDSOIL and infiltration mappings by impact type
!
      DO ILOC = 1, ESD_NUM_LOC
!
        IF( .NOT.ESD_LOC(ILOC)%COMP ) CYCLE
        IF( IN_MAP(ILOC) .EQ. ' ' ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'IN_MAP data missing'
          MESSAG(2) = 'Location = ' // ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Modify the SOIL LOCATION keyword'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
!
        DO IANA = 1, ESD_NUM_ANA
          IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
          IF( KD_MAP(ILOC,IANA) .EQ. ' ' ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'KD_MAP data missing'
            MESSAG(2) = 'Analyte = ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'Location = ' // ESD_LOC(ILOC)%ID
            MESSAG(4) = 'Modify the ESD KDSOIL keyword or the SOIL KDSOIL keyword'
            CALL PRTERR( IERR, CALLER, 4 )
          END IF
        END DO
!
      END DO
!
! *** Check on availability of data
!
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT.ESD_LOC(ILOC)%COMP ) CYCLE
        IF( (ESD_LOC(ILOC)%SODR .OR. ESD_LOC(ILOC)%SODR .OR. ESD_LOC(ILOC)%SODR) .AND. (.NOT.ESD_LOC(ILOC)%AIRD) ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'Soil calculations are requested at a location where air deposition was not computed'
          MESSAG(2) = 'Check the consistency of the solutions on the LOCATION keyword in the ESD keyword file'
          MESSAG(3) = 'Location ID with a problem is "'//TRIM(ESD_LOC(ILOC)%ID)//'"'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
      END DO
!
! *** Check on surface water irrigation mappings
!     Set the indices so don't have to do the mapping multiple times
!
      DO ILOC = 1, ESD_NUM_LOC
        IF( ESD_LOC(ILOC)%ID_SWAT .EQ. 'Null' ) CYCLE
        CALL MATCH_LOC( ESD_LOC(ILOC)%ID_SWAT, INDX )
        IF( INDX .LE. 0 ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'Surface water irrigation location is not in the master list'
          MESSAG(2) = 'Location and surface water IDs are: "'//TRIM(ESD_LOC(ILOC)%ID)//'" and "'//TRIM(ESD_LOC(ILOC)%ID_SWAT)//'"'
          MESSAG(3) = 'There is a problem with the IRG_SWAT modifier on the ESD LOCATION keyword'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
        ESD_LOC(ILOC)%IX_SWAT = INDX
      END DO
!
! *** Ending message if errors encountered
!
      IF( IERR .GT. 0 ) THEN
        MESSAG(1) = 'Ending consistency scan of inputs with '
        WRITE(MESSAG(1)(40:),'(I0)') IERR
        MESSAG(1) = TRIM(MESSAG(1)) // ' errors.'
        CALL PRTERR( IERR, CALLER, 1 )
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
!!    Paul W. Eslinger :  6 Jun 2007 : Original source
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
      SUBROUTINE COMPUTE_SOIL( ILOC, IANA, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine computes the soil concentrations for all times
!!    and realizations for a single location and a single analyte.
!!
!!  Assumptions:
!!
!!    1. The source term media (air, groundwater, and surface water)
!!       linearly interpolated between the years of saved data
!!       get the level used for any particular year
!!
!!    2. The concentrations at irrigated locations are the same as for
!!       the dry locations before the onset of irrigation.
!!
!!  History:
!!
!!    Paul W. Eslinger : 21 Oct 2002 : Version 1.0
!!    Paul W. Eslinger : 18 Dec 2003 : Set small concentrations to zero
!!    Paul W. Eslinger : 29 Dec 2003 : Add irrigation net infiltration
!!    Paul W. Eslinger :  2 Nov 2004 : SCR-1071 Set AOVERL to annual rates
!!    Paul W. Eslinger : 31 May 2005 : SCR-1080 Set AIRD to annual rate
!!    Paul W. Eslinger : 28 Jun 2005 : SCR-1080 Allow soil type selections
!!    Paul W. Eslinger :  6 Jun 2007 : Update comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Soil_Mod
      USE Kdsoil_Mod
      USE Infilt_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER, INTENT(IN) :: IANA ! Analyte index
      INTEGER :: IERR             ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'COMPUTE_SOIL' ! Name of this subroutine
      INTEGER :: ITIM ! Time solution looping index
      INTEGER :: IYRS ! Time passing looping index
      INTEGER :: IREL ! Realization looping variable
!
      REAL :: LAMBDA_LEACH ! Water leach term
!
      INTEGER :: IYRS_1 ! Year indices for looping (lower)
      INTEGER :: IYRS_2 ! Year indices for looping (upper)
!
      REAL :: AOVERL  ! Incoming mass over combined leaching and decay
      REAL :: LAMBDAT ! Combined leaching and decay multiplies by time
!
      REAL :: SMALL_CONC = 1.0E-26 ! Concentrations smaller than this will be set to zero
!
      REAL :: FY_BEFORE ! Fraction of year before irrigation
      REAL :: FY_DURING ! Fraction of year during irrigation
      REAL :: FY_AFTER  ! Fraction of year after irrigation
!
      REAL :: LAMBDA_LEACH_DRY ! Water leach term - dry period
      REAL :: LAMBDA_LEACH_WET ! Water leach term - irrigation period
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
      IF( BUG_COMP ) THEN
        WRITE(IRPT,1000) 'Entering '//CALLER, ILOC, ESD_LOC(ILOC)%ID, TRIM(ESD_LOC(ILOC)%NAME), &
          IANA, ESD_ANA(IANA)%ID, TRIM(ESD_ANA(IANA)%NAME), ESD_ANA(IANA)%LAMBDA
 1000   FORMAT(/A/&
        '    Location index = ',I0,' : "',A,'" : "',A,'"'/&
        '    Analyte index  = ',I0,' : "',A,'" : "',A,'"'/&
        '    Decay Factor   = ',1P,E12.5)
      END IF
!
! *** Fraction of year in each period
!
      FY_BEFORE = (SEASON_START-1)/365.0
      FY_DURING = (SEASON_END-SEASON_START+1.0)/365.0
      FY_AFTER  = (365.0-SEASON_END)/365.0
!
      IF( BUG_COMP ) THEN
        WRITE(IRPT,1010) SEASON_START, SEASON_END, IRIG_RATE, SOIL_DEPTH, SOIL_DENSITY, THETA_IRG, THETA_DRY, &
          FY_BEFORE, FY_DURING, FY_AFTER
 1010   FORMAT(/'Calculating the divisor and other information'/&
          4X,'SEASON_START     = ',I0/&
          4X,'SEASON_END       = ',I0/&
          4X,'IRIG_RATE        = ',1P,E12.5/&
          4X,'SOIL_DEPTH       = ',1P,E12.5/&
          4X,'SOIL_DENSITY     = ',E12.5/&
          4X,'THETA_IRG        = ',E12.5/&
          4X,'THETA_DRY        = ',E12.5/&
          4X,'FY_BEFORE        = ',E12.5/&
          4X,'FY_DURING        = ',E12.5/&
          4X,'FY_AFTER         = ',E12.5)
      END IF
!
! *** Initialize mass in the upper soil layer to zero (matrix assignment)
!
      MASS_SODR = ZERO
      MASS_SOGW = ZERO
      MASS_SOSW = ZERO
!
! *** Loop over all solution times
!
      DO ITIM = 1, ESD_NUM_TIM
!
! ***   Compute annual delta deposition and concentrations by realization (if they are needed)
!
        IF( ITIM .EQ. 1 ) THEN
!
          IF( ESD_LOC(ILOC)%AIRD ) THEN
            DO IREL = 1, ESD_NREAL
              IF( .NOT. REL_USE(IREL) ) CYCLE
              DELTA_AIRD(IREL)  = 0.0
              ANNUAL_AIRD(IREL) = AIRD(ITIM,IREL)
            END DO
          ELSE
            DELTA_AIRD  = ZERO
            ANNUAL_AIRD = ZERO
          END IF
!
          IF( ESD_LOC(ILOC)%GWAT .AND. COMPUTE_SOGW ) THEN
            DO IREL = 1, ESD_NREAL
              IF( .NOT. REL_USE(IREL) ) CYCLE
              DELTA_GWAT(IREL)  = 0.0
              ANNUAL_GWAT(IREL) = GWAT(ITIM,IREL)
            END DO
          ELSE
            DELTA_GWAT  = ZERO
            ANNUAL_GWAT = ZERO
          END IF
!
          IF( ESD_LOC(ILOC)%IX_SWAT.GT.0 .AND. COMPUTE_SOSW ) THEN
            DO IREL = 1, ESD_NREAL
              IF( .NOT. REL_USE(IREL) ) CYCLE
              DELTA_SWAT(IREL) = 0.0
              ANNUAL_SWAT(IREL) = SWAT(ITIM,IREL)
            END DO
          ELSE
            DELTA_SWAT  = ZERO
            ANNUAL_SWAT = ZERO
          END IF
!
        ELSE
!
          IF( ESD_LOC(ILOC)%AIRD ) THEN
            DO IREL = 1, ESD_NREAL
              IF( .NOT. REL_USE(IREL) ) CYCLE
              DELTA_AIRD(IREL)  = (AIRD(ITIM,IREL)-AIRD(ITIM-1,IREL)) / (ESD_TIM(ITIM)%TIME-ESD_TIM(ITIM-1)%TIME)
              ANNUAL_AIRD(IREL) = AIRD(ITIM-1,IREL) + DELTA_AIRD(IREL)
            END DO
          ELSE
            DELTA_AIRD  = ZERO
            ANNUAL_AIRD = ZERO
          END IF
!
          IF( ESD_LOC(ILOC)%GWAT .AND. COMPUTE_SOGW ) THEN
            DO IREL = 1, ESD_NREAL
              IF( .NOT. REL_USE(IREL) ) CYCLE
              DELTA_GWAT(IREL)  = (GWAT(ITIM,IREL)-GWAT(ITIM-1,IREL)) / (ESD_TIM(ITIM)%TIME-ESD_TIM(ITIM-1)%TIME)
              ANNUAL_GWAT(IREL) = GWAT(ITIM-1,IREL) + DELTA_GWAT(IREL)
            END DO
          ELSE
            DELTA_GWAT  = ZERO
            ANNUAL_GWAT = ZERO
          END IF
!
          IF( ESD_LOC(ILOC)%IX_SWAT.GT.0 .AND. COMPUTE_SOSW ) THEN
            DO IREL = 1, ESD_NREAL
              IF( .NOT. REL_USE(IREL) ) CYCLE
              DELTA_SWAT(IREL) = (SWAT(ITIM,IREL)-SWAT(ITIM-1,IREL)) / (ESD_TIM(ITIM)%TIME-ESD_TIM(ITIM-1)%TIME)
              ANNUAL_SWAT(IREL) = SWAT(ITIM-1,IREL) + DELTA_SWAT(IREL)
            END DO
          ELSE
            DELTA_SWAT  = ZERO
            ANNUAL_SWAT = ZERO
          END IF
!
        END IF
        IF( BUG_COMP ) THEN
          WRITE(IRPT,1020) ITIM, ESD_NUM_TIM, ESD_TIM(ITIM)%TIME
 1020     FORMAT(/'Time loop, index = ',I0,' of ',I0,' year = ',I0/&
            'Delta deposition and concentrations by realization'/&
            '   Index    Air Del      Air Ann       GW Del       GW Ann       SW Del       SW Ann       INFILT   IN_UNITS ',&
            'KD           KD_UNITS '/&   
            '   ----- ------------ ------------ ------------ ------------ ------------ ------------ ------------ -------- ',&
            '------------ ---------')
          DO IREL = 1, ESD_NREAL
            IF( .NOT. REL_USE(IREL) ) CYCLE
            WRITE(IRPT,1030) IREL, DELTA_AIRD(IREL), ANNUAL_AIRD(IREL), DELTA_GWAT(IREL), ANNUAL_GWAT(IREL), &
              DELTA_SWAT(IREL), ANNUAL_SWAT(IREL), INFILT(ILOC,IREL), TRIM(IN_UNITS(ILOC)), KD(ILOC,IANA,IREL), TRIM(KD_UNITS(ILOC))
 1030       FORMAT(4X,I4,1P,7(1X,E12.5),1X,A,4X,E12.5,1X,A)
          END DO
        END IF
!
! ****  Loop on years between solution times
!
        IF( ITIM .EQ. 1 ) THEN
          IYRS_1 = ESD_TIM(ITIM)%TIME
          IYRS_2 = IYRS_1
        ELSE
          IYRS_1 = ESD_TIM(ITIM-1)%TIME + 1
          IYRS_2 = IYRS_1 + ESD_TIM(ITIM)%TIME - ESD_TIM(ITIM-1)%TIME - 1
        END IF
        IF( BUG_COMP ) THEN
          WRITE(IRPT,1040) ITIM, ESD_TIM(ITIM)%TIME, IYRS_1, IYRS_2
 1040     FORMAT(/'Looping years calculation'/&
                  '    ITIM               = ',I0/&
                  '    ESD_TIM(ITIM)%TIME = ',I0/&
                  '    IYRS_1             = ',I0/&
                  '    IYRS_2             = ',I0)
           WRITE(IRPT,'(A)') 'Media, IYRS, IDAY, IREL, ANNUAL_AIR, ANNUAL_OTHER, LEACH RATE, MASS, AOVERL'
        END IF
!
        DO IYRS = IYRS_1, IYRS_2
!         IF( MOD(IYRS,100) .EQ. 1 ) WRITE(*,*) ITIM, IYRS
!
!         Compute soil concentrations by realization
!
          DO IREL = 1, ESD_NREAL
!
            IF( .NOT. REL_USE(IREL) ) CYCLE
!
!           Leaching terms (dry land conditions and irrigated conditions)
!
            LAMBDA_LEACH_DRY = INFILT(ILOC,IREL) / (SOIL_DEPTH*THETA_DRY*(1.0+1000.0*KD(ILOC,IANA,IREL)*SOIL_DENSITY/THETA_DRY))
            LAMBDA_LEACH_WET = (INFILT(ILOC,IREL) + IRIG_NET_FRAC*IRIG_RATE/FY_DURING) / &
              (SOIL_DEPTH*THETA_IRG*(1.0+1000.0*KD(ILOC,IANA,IREL)*SOIL_DENSITY/THETA_IRG))
            IF( BUG_COMP ) THEN
              WRITE(IRPT,1050) IREL, IANA, ESD_ANA(IANA)%ID, ESD_LOC(ILOC)%ID, KD(ILOC,IANA,IREL), THETA_DRY, LAMBDA_LEACH_DRY, &
                THETA_IRG, LAMBDA_LEACH_WET
 1050         FORMAT(/&
                'Realization      = ',I0/&
                'Analyte Index    = ',I0/&
                'Analyte ID       = ',A/&
                'Location ID      = ',A/&
                'KD               = ',1P,E12.5/&
                'Theta_Dry        = ',E12.5/&
                'LAMBDA_LEACH_DRY = ',E12.5/&
                'Theta_IRG        = ',E12.5/&
                'LAMBDA_LEACH_WET = ',E12.5)
            END IF
!
!           Dry locations (and irrigated locations before the onset of irrigation)
            IF( ESD_LOC(ILOC)%SODR .OR. IYRS.LT.IRIG_START) THEN
              LAMBDA_LEACH = LAMBDA_LEACH_DRY
              LAMBDAT = ESD_ANA(IANA)%LAMBDA + LAMBDA_LEACH
              AOVERL = ANNUAL_AIRD(IREL) / (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
              MASS_SODR(IREL) = AOVERL + EXP(-LAMBDAT)*(MASS_SODR(IREL)-AOVERL)
              SAVE_SODR(IREL) = MASS_SODR(IREL)
              IF( BUG_COMP ) WRITE(IRPT,1060) 'SODR', IYRS, 365, IREL, ANNUAL_AIRD(IREL), ANNUAL_AIRD(IREL), LAMBDA_LEACH, &
                MASS_SODR(IREL), AOVERL, 'All year'
 1060           FORMAT(A,',',I0,',',I0,',',I0,1P,5(:,',',1P,E12.5),',',A)
              IF( IYRS .LT. IRIG_START ) THEN
                MASS_SOGW(IREL) = MASS_SODR(IREL)
                SAVE_SOGW(IREL) = MASS_SOGW(IREL)
                MASS_SOSW(IREL) = MASS_SODR(IREL)
                SAVE_SOSW(IREL) = MASS_SOSW(IREL)
              END IF
            END IF
!
!           Locations with groundwater irrigation (after the onset of irrigation)
            IF( ESD_LOC(ILOC)%SOGW .AND. COMPUTE_SOGW ) THEN
              IF( IYRS .GE. IRIG_START ) THEN
!               Before the growing season
                LAMBDA_LEACH = LAMBDA_LEACH_DRY
                AOVERL = ANNUAL_AIRD(IREL) / (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                LAMBDAT = FY_BEFORE * (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                MASS_SOGW(IREL) = AOVERL + EXP(-LAMBDAT)*(MASS_SOGW(IREL)-AOVERL)
                IF( BUG_COMP ) WRITE(IRPT,1060) 'SOGW', IYRS, SEASON_START, IREL, ANNUAL_AIRD(IREL), ANNUAL_GWAT(IREL), &
                  LAMBDA_LEACH, MASS_SOGW(IREL), AOVERL, 'Before Growing Season'
!               During the growing season
                LAMBDA_LEACH = LAMBDA_LEACH_WET
                AOVERL = (ANNUAL_GWAT(IREL)*IRIG_RATE*0.01/FY_DURING + ANNUAL_AIRD(IREL)) / (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                LAMBDAT = FY_DURING * (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                MASS_SOGW(IREL) = AOVERL + EXP(-LAMBDAT)*(MASS_SOGW(IREL)-AOVERL)
                SAVE_SOGW(IREL) = MASS_SOGW(IREL)
                IF( BUG_COMP ) WRITE(IRPT,1060) 'SOGW', IYRS, SEASON_END, IREL, ANNUAL_AIRD(IREL), ANNUAL_GWAT(IREL), &
                  LAMBDA_LEACH, MASS_SOGW(IREL), AOVERL, 'End of Growing Season'
!               After the growing season
                LAMBDA_LEACH = LAMBDA_LEACH_DRY
                AOVERL = ANNUAL_AIRD(IREL) / (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                LAMBDAT = FY_AFTER * (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                MASS_SOGW(IREL) = AOVERL + EXP(-LAMBDAT)*(MASS_SOGW(IREL)-AOVERL)
                IF( BUG_COMP ) WRITE(IRPT,1060) 'SOGW', IYRS, 365, IREL, ANNUAL_AIRD(IREL), ANNUAL_GWAT(IREL), &
                  LAMBDA_LEACH, MASS_SOGW(IREL), AOVERL, 'End of year'
              END IF
            END IF
!
!           Locations with surface water irrigation (after the onset of irrigation)
            IF( ESD_LOC(ILOC)%SOSW .AND. COMPUTE_SOSW ) THEN
              IF( IYRS .GE. IRIG_START ) THEN
!               Before the growing season
                LAMBDA_LEACH = LAMBDA_LEACH_DRY
                AOVERL = ANNUAL_AIRD(IREL) / (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                LAMBDAT = FY_BEFORE * (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                MASS_SOSW(IREL) = AOVERL + EXP(-LAMBDAT)*(MASS_SOSW(IREL)-AOVERL)
                IF( BUG_COMP ) WRITE(IRPT,1060) 'SOSW', IYRS, SEASON_START, IREL, ANNUAL_AIRD(IREL), ANNUAL_SWAT(IREL), &
                  LAMBDA_LEACH, MASS_SOSW(IREL), AOVERL, 'Before Growing Season'
!               During the growing season
                LAMBDA_LEACH = LAMBDA_LEACH_WET
                AOVERL = (ANNUAL_SWAT(IREL)*IRIG_RATE*0.01/FY_DURING + ANNUAL_AIRD(IREL)) / (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                LAMBDAT = FY_DURING * (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                MASS_SOSW(IREL) = AOVERL + EXP(-LAMBDAT)*(MASS_SOSW(IREL)-AOVERL)
                SAVE_SOSW(IREL) = MASS_SOSW(IREL)
                IF( BUG_COMP ) WRITE(IRPT,1060) 'SOSW', IYRS, SEASON_END, IREL, ANNUAL_AIRD(IREL), ANNUAL_SWAT(IREL), &
                  LAMBDA_LEACH, MASS_SOSW(IREL), AOVERL, 'End of Growing Season'
!               After the growing season
                LAMBDA_LEACH = LAMBDA_LEACH_DRY
                AOVERL = ANNUAL_AIRD(IREL) / (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                LAMBDAT = FY_AFTER * (ESD_ANA(IANA)%LAMBDA+LAMBDA_LEACH)
                MASS_SOSW(IREL) = AOVERL + EXP(-LAMBDAT)*(MASS_SOSW(IREL)-AOVERL)
                IF( BUG_COMP ) WRITE(IRPT,1060) 'SOSW', IYRS, 365, IREL, ANNUAL_AIRD(IREL), ANNUAL_SWAT(IREL), &
                  LAMBDA_LEACH, MASS_SOSW(IREL), AOVERL, 'End of year'
              END IF
            END IF
          END DO ! IREL
!
! ***     Update the input concentrations
!
          IF( ESD_LOC(ILOC)%AIRD ) THEN
            DO IREL = 1, ESD_NREAL
              IF( .NOT. REL_USE(IREL) ) CYCLE
!              IF( BUG_COMP ) WRITE(IRPT,2000) 'AIRD', IYRS, IREL, ANNUAL_AIRD(IREL)
 2000         FORMAT('"',A,'",',I0,',',I0,',',1P,E12.5)
              ANNUAL_AIRD(IREL) = ANNUAL_AIRD(IREL) + DELTA_AIRD(IREL)
            END DO
          END IF
!
          IF( ESD_LOC(ILOC)%GWAT ) THEN
            DO IREL = 1, ESD_NREAL
              IF( .NOT. REL_USE(IREL) ) CYCLE
!              IF( BUG_COMP ) WRITE(IRPT,2000) 'GWAT', IYRS, IREL, ANNUAL_GWAT(IREL)
              ANNUAL_GWAT(IREL) = ANNUAL_GWAT(IREL) + DELTA_GWAT(IREL)
            END DO
          END IF
!
          IF( ESD_LOC(ILOC)%IX_SWAT .GT. 0 ) THEN
            DO IREL = 1, ESD_NREAL
              IF( .NOT. REL_USE(IREL) ) CYCLE
!              IF( BUG_COMP ) WRITE(IRPT,2000) 'SWAT', IYRS, IREL, ANNUAL_SWAT(IREL)
              ANNUAL_SWAT(IREL) = ANNUAL_SWAT(IREL) + DELTA_SWAT(IREL)
            END DO
          END IF
!
        END DO ! IYRS
!
! ***   Put the normalized computed concentration values back into the arrays
!
        IF( BUG_COMP ) WRITE(IRPT,2010)
 2010   FORMAT(/'Location, Analyte, Year, Solution, Concentration by Realization')
        DO IREL = 1, ESD_NREAL
          IF( .NOT. REL_USE(IREL) ) CYCLE
          IF( ESD_LOC(ILOC)%SODR ) THEN
            SODR(ITIM,IREL) = SAVE_SODR(IREL) / (SOIL_DEPTH*SOIL_DENSITY*10.0)
            IF( ABS(SODR(ITIM,IREL)) .LT. SMALL_CONC ) SODR(ITIM,IREL) = 0.0
          END IF
          IF( ESD_LOC(ILOC)%SOGW ) THEN
            SOGW(ITIM,IREL) = SAVE_SOGW(IREL) / (SOIL_DEPTH*SOIL_DENSITY*10.0)
            IF( ABS(SOGW(ITIM,IREL)) .LT. SMALL_CONC ) SOGW(ITIM,IREL) = 0.0
          END IF
          IF( ESD_LOC(ILOC)%SOSW ) THEN
            SOSW(ITIM,IREL) = SAVE_SOSW(IREL) / (SOIL_DEPTH*SOIL_DENSITY*10.0)
            IF( ABS(SOSW(ITIM,IREL)) .LT. SMALL_CONC ) SOSW(ITIM,IREL) = 0.0
          END IF
        END DO ! IREL
        IF( BUG_COMP ) THEN
          IF( COMPUTE_SODR ) WRITE(IRPT,2020) ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_TIM(ITIM)%TIME, 'SODR', &
                               (SODR(ITIM,IREL),IREL=1,ESD_NREAL)
          IF( COMPUTE_SOSW ) WRITE(IRPT,2020) ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_TIM(ITIM)%TIME, 'SOSW', &
                               (SOSW(ITIM,IREL),IREL=1,ESD_NREAL)
          IF( COMPUTE_SOGW ) WRITE(IRPT,2020) ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, ESD_TIM(ITIM)%TIME, 'SOGW', &
                               (SOGW(ITIM,IREL),IREL=1,ESD_NREAL)
 2020     FORMAT(A,',',A,',',I0,',',A,1P,100(:,',',E12.5))
        END IF
!
      END DO ! ITIM
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ECHO(  )
!!******************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles writing an echo of the problem definition
!!    to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 17 Mar 2003 : Version 1.0
!!    Paul W. Eslinger : 28 Jun 2005 : SCR-1080 Allow soil type selections
!!    Paul W. Eslinger :  6 Jun 2007 : Update comments
!!
!!******************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Esd_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: ILOC  ! Location looping index
      INTEGER :: IANA  ! Analyte looping index
      INTEGER :: ITIM  ! Time looping index
      INTEGER :: ICNT  ! Realization counting variable
      INTEGER :: IREL  ! Realization looping variable
      INTEGER :: NLINE ! Variable for printing realization activations
      INTEGER :: NTIME ! Variable for printing realization activations
      INTEGER :: NOVER ! Variable for printing realization activations
      INTEGER :: I1    ! Variable for printing realization activations
      INTEGER :: I2    ! Variable for printing realization activations
      INTEGER :: J     ! Variable for printing realization activations
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
      WRITE(IRPT,1020) ESD_NREAL, 'Realizations identified in the ESD keyword file'
 1020 FORMAT(I4,1X,A)
      ICNT = 0
      DO IREL = 1, NREAL
        IF( REL_USE(IREL) ) ICNT = ICNT + 1
      END DO
      WRITE(IRPT,1020) ICNT, 'Realizations utilized in this run'
!
      WRITE(IRPT,1025)
 1025 FORMAT(/'A map of realization activations (true/false) is:')
      I2 = 0
      NLINE = 20
      NTIME = NREAL / NLINE
      NOVER = NREAL - NLINE*NTIME
      DO IREL = 1, NTIME
        I1 = (IREL-1)*NLINE + 1
        I2 = I1 + NLINE - 1
        WRITE(IRPT,1026) I1, I2, (REL_USE(J),J=I1,I2)
 1026   FORMAT(1X,I3,'-',I3,25(2X,L1))
      END DO
      IF( NOVER .GT. 0 ) THEN
        I1 = I2 + 1
        I2 = NREAL
        WRITE(IRPT,1026) I1, I2, (REL_USE(J),J=I1,I2)
      ENDIF
!
      WRITE(IRPT,1210) YEAR_START, YEAR_STOP, YEAR_CLOSE, IRIG_START
 1210 FORMAT(/'Year definitionsd'/ &
        5X,I0,' : Start year for the simulation'/ &
        5X,I0,' : Stop  year for the simulation'/ &
        5X,I0,' : Site closure year for the simulation'/ &
        5X,I0,' : Start year for irrigation')
!
! *** Files names used in the analysis
!
      WRITE(IRPT,1030) 'File Name for SOIL Input Keyword Data', TRIM(FNKEY)
      WRITE(IRPT,1030) 'File Name for ESD Input Keyword Data', TRIM(FN_ESD)
      WRITE(IRPT,1030) 'File Name for Infiltration Data', TRIM(FN_INFILT)
      WRITE(IRPT,1030) 'File Name for KDSOIL Data', TRIM(FN_KDSOIL)
 1030 FORMAT(/A/'File: ',A)
!
! *** Concentration map and data files for each analyte
!
      WRITE(IRPT,1045) TRIM(FN_MAP)
 1045 FORMAT(/'File Name for Concentration Map File'/'File: ',A)
!
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
        WRITE(IRPT,1040) TRIM(ESD_ANA(IANA)%ID), TRIM(FN_CON(IANA))
 1040   FORMAT(/'File Name for Media Concentrations: Analyte ID="',A,'"'/'File: ',A)
      END DO
!
! *** Analyte names and types
!
      WRITE(IRPT,1050) ESD_NUM_ANA
 1050 FORMAT(/'Analye Information for ',I0,' analytes.')
      DO IANA = 1, ESD_NUM_ANA
        IF( ESD_ANA(IANA)%COMP ) THEN
          WRITE(IRPT,1060) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%NAME
 1060     FORMAT(3X,I3,' : ',A,' : ',A)
        ELSE
          WRITE(IRPT,1060) IANA, ESD_ANA(IANA)%ID, '===> Not used'
        END IF
      END DO
      WRITE(IRPT,1070) SOI_NUM_ANA
 1070 FORMAT('A total of ',I0,' analytes have been requested.')
!
! *** Output the set of time definitions
!
      WRITE(IRPT,*) ' '
      WRITE(IRPT,1300) ESD_NUM_TIM
 1300 FORMAT('Number of times is ',I0)
      WRITE(IRPT,'(A)') '  Index   Year'
      DO ITIM = 1, ESD_NUM_TIM
        WRITE(IRPT,'(1X,I6,1X,I6)') ITIM, ESD_TIM(ITIM)%TIME
      END DO
!
! *** Output the set of location definitions
!     Skip output indices where no solutions are requested
!
      WRITE(IRPT,*) ' '
      WRITE(IRPT,1320) ESD_NUM_LOC
 1320 FORMAT('Total number of locations defined in the ESD keyword file are ',I0/&
             'Activated locations for upland soil conputations are shown below')
      WRITE(IRPT,'(A)') '  Index      ID      SW Irrig   Name'
      WRITE(IRPT,'(A)') '  -----   --------   --------   ------------------------------------------------'
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT.ESD_LOC(ILOC)%COMP ) CYCLE
        IF( ESD_LOC(ILOC)%TYPE .NE. 'UPLAND' ) CYCLE
        IF( ESD_LOC(ILOC)%SOSW .AND. COMPUTE_SOSW ) THEN
          WRITE(IRPT,1322) ILOC, ESD_LOC(ILOC)%ID, ESD_LOC(ILOC)%ID_SWAT, TRIM(ESD_LOC(ILOC)%NAME)
 1322     FORMAT(1X,I6,' : "',A,'" : "',A,'" : "',A,'"')
        ELSE
          IF( COMPUTE_SODR .OR. COMPUTE_SOGW ) THEN
            WRITE(IRPT,1323) ILOC, ESD_LOC(ILOC)%ID, 'N/A     ', TRIM(ESD_LOC(ILOC)%NAME)
 1323       FORMAT(1X,I6,' : "',A,'" : ',A,' : "',A,'"')
          END IF
        END IF
      END DO
!
! *** KDSOIL and DILUTE mappings by impact type
!
      WRITE(IRPT,1180)
 1180 FORMAT(/'Soil-Water KD and Dilution Factor Mapping',/&
        '  Location         Analyte       KDSOIL ID String        INFILT ID String'/&
        '  ---------------  ------------  ----------------------  ----------------------')
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT.ESD_LOC(ILOC)%COMP ) CYCLE
        IF( ESD_LOC(ILOC)%TYPE .NE. 'UPLAND' ) CYCLE
        DO IANA = 1, ESD_NUM_ANA
          IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
          WRITE(IRPT,1190) ILOC, ESD_LOC(ILOC)%ID, IANA, ESD_ANA(IANA)%ID, KD_MAP(ILOC,IANA), IN_MAP(ILOC)
 1190     FORMAT(2X,I5,': "',A,'" ',I3,': "',A,'"  "',A,'"',:,'  "',A,'"')
        END DO
      END DO
!
! *** End of problem definition
!
      WRITE(IRPT,1200)
 1200 FORMAT(/24('='),' End of the Problem Definition ',25('='))
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ESD_INIT( IERR )
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
!!    Paul W. Eslinger : 17 Mar 2003 : Version 2.0
!!    Paul W. Eslinger :  6 Jun 2007 : Fix IERR logic
!!
!!**********************************************************************
!
! *** Global variables
      USE ESD_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Control_Mod
      USE Soil_Mod, ONLY: ZERO
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number, 0 = no errors
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'ESD_INIT' ! Name of this subroutine
!
      INTEGER :: I    ! Looping variable
      INTEGER :: IREL ! Realization looping variable
      INTEGER :: IANA ! Analyte looping variable
      INTEGER :: ILOC ! Location looping variable
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
! *** Variables for the time slice data
!
      DO I = 1, ESD_NUM_TIM
        ESD_TIM(I)%TIME = 0
      END DO
!
! *** Variables for the location data
!
      DO I = 1, ESD_NUM_LOC
        ESD_LOC(I)%ID = ' '
        ESD_LOC(I)%NAME = ' '
        ESD_LOC(I)%TYPE = ' '
        ESD_LOC(I)%COMP = .FALSE.
        ESD_LOC(I)%AIRD = .FALSE.
        ESD_LOC(I)%SODR = .FALSE.
        ESD_LOC(I)%SOGW = .FALSE.
        ESD_LOC(I)%SOSW = .FALSE.
        ESD_LOC(I)%GWAT = .FALSE.
        ESD_LOC(I)%SWAT = .FALSE.
        ESD_LOC(I)%ID_SWAT = 'Null'
        ESD_LOC(I)%IX_SWAT = 0
      END DO
!
! *** Variables for the analyte data
!
      IF( ESD_NUM_ANA .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid value for ESD_NUM_ANA'
        MESSAG(2) = 'Suggest modifying the ESD ANALYTE keywords'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      ICON = 15
      DO I = 1, ESD_NUM_ANA
        ESD_ANA(I)%ID   = ' '
        ESD_ANA(I)%NAME = ' '
        ESD_ANA(I)%COMP = .FALSE.
        ESD_ANA(I)%LAMBDA = 0.0
        FN_CON(I)  = ' '
      END DO
!
! *** Map and data for Soil Kd's and  infiltration factors
!
      DO ILOC = 1, ESD_NUM_LOC
        IN_MAP(ILOC) = ' '
        DO IREL = 1, NREAL
          INFILT(ILOC,IREL) = -1.0
        END DO
        DO IANA = 1, ESD_NUM_ANA
          KD_MAP(ILOC,IANA) = ' '
          DO IREL = 1, NREAL
            KD(ILOC,IANA,IREL) = -1.0
          END DO
        END DO
      END DO
!
! *** Realization use vector
!
      DO IREL = 1, ESD_NREAL
        REL_USE(IREL) = .FALSE.
        ZERO(IREL) = 0.0
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
!!    Paul W. Eslinger :  8 Oct 2002 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Update comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Files_Mod
      USE ESD_Mod
      USE Control_Mod
      USE Soil_Mod
      USE Infilt_Mod
      USE Kdsoil_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error flag, nonzero if no error
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'ESD_MEMORY' ! Name of this routine
      INTEGER :: IERA  ! Error status variable from the allocate action
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
      ALLOCATE( FN_CON(ESD_NUM_ANA), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating memory for FN_CON'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( ESD_NUM_LOC .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'At least 1 location required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Allocate for the number of locations
!
      ALLOCATE( ESD_LOC(ESD_NUM_LOC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating memory for ESD_LOC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( IN_MAP(ESD_NUM_LOC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating memory for IN_MAP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( IN_UNITS(ESD_NUM_LOC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) = 'Error allocating memory for IN_UNITS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( KD_UNITS(ESD_NUM_LOC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating memory for KD_UNITS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Allocation depending on locations and analytes
!
      ALLOCATE( KD_MAP(ESD_NUM_LOC,ESD_NUM_ANA), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Error allocating memory for KD_MAP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Allocate on the number of times
!
      IF( ESD_NUM_TIM .GT. 0 ) THEN
        ALLOCATE( ESD_TIM(ESD_NUM_TIM), STAT=IERA )
        IF( IERA .NE. 0 ) THEN
          IERR = 10
          MESSAG(1) = 'Error allocating memory for ESD_TIM'
          MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
      END IF
!
! *** Check on the number of realizations
!
      IF( ESD_NREAL .LT. 1 ) THEN
        IERR = 11
        MESSAG(1) = 'At least 1 ESD realization required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
     ALLOCATE( KD(ESD_NUM_LOC,ESD_NUM_ANA,ESD_NREAL), STAT=IERA )
     IF( IERA .NE. 0 ) THEN
       IERR = 12
       MESSAG(1) = 'Error allocating memory for KD'
       MESSAG(2) = 'System error status was '
       WRITE(MESSAG(2)(25:),*) IERA
       CALL PRTERR( IERR, CALLER, 2 )
       RETURN
     END IF
!
     ALLOCATE( INFILT(ESD_NUM_LOC,ESD_NREAL), STAT=IERA )
     IF( IERA .NE. 0 ) THEN
       IERR = 13
       MESSAG(1) = 'Error allocating memory for INFILT'
       MESSAG(2) = 'System error status was '
       WRITE(MESSAG(2)(25:),*) IERA
       CALL PRTERR( IERR, CALLER, 2 )
       RETURN
     END IF
!
! *** Allocate work space
!
      ALLOCATE( CVEC(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 14
        MESSAG(1) = 'Error allocating memory for CVEC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ZERO(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 15
        MESSAG(1) = 'Error allocating memory for ZERO'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( MASS_SODR(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 16
        MESSAG(1) = 'Error allocating memory for MASS_SODR'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( MASS_SOGW(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 17
        MESSAG(1) = 'Error allocating memory for MASS_SOGW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( MASS_SOSW(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 18
        MESSAG(1) = 'Error allocating memory for MASS_SOSW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DELTA_AIRD(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 19
        MESSAG(1) = 'Error allocating memory for DELTA_AIRD'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DELTA_GWAT(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 20
        MESSAG(1) = 'Error allocating memory for DELTA_GWAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DELTA_SWAT(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 21
        MESSAG(1) = 'Error allocating memory for DELTA_SWAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ANNUAL_AIRD(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 22
        MESSAG(1) = 'Error allocating memory for ANNUAL_AIRD'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ANNUAL_GWAT(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 23
        MESSAG(1) = 'Error allocating memory for ANNUAL_GWAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ANNUAL_SWAT(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 24
        MESSAG(1) = 'Error allocating memory for ANNUAL_SWAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SAVE_SOSW(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 25
        MESSAG(1) = 'Error allocating memory for SAVE_SOSW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SAVE_SOGW(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 26
        MESSAG(1) = 'Error allocating memory for SAVE_SOGW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SAVE_SODR(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 27
        MESSAG(1) = 'Error allocating memory for SAVE_SODR'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Allocate the realization use vector
!
      ALLOCATE( REL_USE(ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 28
        MESSAG(1) = 'Error allocating memory for REL_USE'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Concentration arrays for soil buildup
!
      ALLOCATE( AIRD(ESD_NUM_TIM,ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 29
        MESSAG(1) = 'Error allocating memory for AIRD'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( GWAT(ESD_NUM_TIM,ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 30
        MESSAG(1) = 'Error allocating memory for GWAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SWAT(ESD_NUM_TIM,ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 31
        MESSAG(1) = 'Error allocating memory for SWAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SODR(ESD_NUM_TIM,ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 32
        MESSAG(1) = 'Error allocating memory for SODR'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SOGW(ESD_NUM_TIM,ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 33
        MESSAG(1) = 'Error allocating memory for SOGW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SOSW(ESD_NUM_TIM,ESD_NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 34
        MESSAG(1) = 'Error allocating memory for SOSW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
!      SUBROUTINE GET_PATH( INFILE, PATH, PLEN, IERR )
!!********************************************************************
!!
!!  Purpose:
!!
!!    Extract the leading path (if any) from the input file name.
!!
!!  History:
!!
!!    David W. Engel   : 28 Jun 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Update comments
!!
!!********************************************************************
!
! *** Force explicit typing of all variables and functions
!      IMPLICIT NONE
!
! *** Call list variables
!      INTEGER :: PLEN             ! Returned length of path
!      INTEGER :: IERR             ! Returned error code
!      CHARACTER(LEN=*) :: INFILE  ! Input file name
!      CHARACTER(LEN=*) :: PATH    ! Output path
!
! *** Local variables
!      INTEGER :: FNLN ! File name length
!      INTEGER :: I    ! Looping index
!
!---- First executable code --------------------------------------------
!
!      IERR = 0
!      PATH = ' '
!
! *** Get the length of file name
!
!      FNLN = LEN_TRIM(INFILE)
!      IF( FNLN .EQ. 0 ) THEN
!        IERR = 1
!        RETURN
!      END IF
!
! *** Check for last occurance of path and file name delimiter (\ or /)
!
!      PLEN = 0
!      DO I = 1, FNLN
!        IF( INFILE(I:I).EQ.'\' .OR. INFILE(I:I).EQ.'/') PLEN = I
!      END DO
!
! *** Transfer path name
!
!      IF( PLEN .GT. 0 ) PATH(1:PLEN) = INFILE(1:PLEN)
!
!      RETURN
!      END SUBROUTINE
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
!!    Paul W. Eslinger : 21 Oct 2002 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Last update
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
      USRNAM = ' '
!
! *** Program name and version number
      PRGNAM = 'Soil'
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
      SYSDAT = SDATE(5:6)//'/'//SDATE(7:8)//'/'//SDATE(1:4)
!
! *** Run identification number
      CRUNID = SDATE(1:8)//STIME(1:6)
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE INIT(  )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine initializes global variables
!!
!!  History:
!!
!!    Paul W. Eslinger :  9 Oct 2002 : Version 1.0
!!    Paul W. Eslinger : 28 Jun 2005 : SCR-1080 Allow soil type selections
!!    Paul W. Eslinger : 28 Jul 2006 : SCR-1139 Fix LIST option problems
!!    Paul W. Eslinger :  6 Jun 2007 : Update comments
!!    Paul W. Eslinger :  7 Feb 2014 : (TIIA SCR-0003) Remove the "run" and "done" files
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Files_Mod
      USE Errors_Mod
      USE Control_Mod
      USE Esd_Mod
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
!---- Executable code ---------------------------------------------------
!
! *** Keyword file
!
      IKEY  = 7
      FNKEY = ' '
!      PATH_KEY = ' '
!
! *** Report file
!
      IRPT  = 8
      FNRPT = ' '
      IRPT_ERR = IRPT
      REPORT = .FALSE.
!
! *** ECDA map file
!
      UN_ECDAMAP  = 9
!
! *** Define the "Done" file name without the path
!     The path for this file will be added in routine OPEN_ESD
!
!      IDON = 10
!      IRUN = 11
!      FN_DONE = TRIM(PRGNAM)//'.Done'
!      FN_RUN  = TRIM(PRGNAM)//'.Run'
!
! *** Error message file
!
      IRPT_ERR = IRPT
!
! *** KDSOIL variables
!
      IKDS = 12
      FN_KDSOIL = ' '
!
! *** DILUTE variables
!
      IINF = 13
      FN_INFILT = ' '
!
! *** ESD keyword input file
!
      IESD  = 14
      FN_ESD = ' '
!
! *** Problem title
!
      PTITLE = ' '
!
! *** Debug and execute flags
!
      BUG_CONC    = .FALSE.
      BUG_CONC_BQ = .FALSE.
      BUG_COMP    = .FALSE.
      BUG_LOOP    = .FALSE.
!
      EXECUT = .FALSE.
!
      COMPUTE_SODR = .FALSE.
      COMPUTE_SOGW = .FALSE.
      COMPUTE_SOSW = .FALSE.
!
      USE_ALL_REL = .FALSE.
!
! *** Initialize some file names
!
      FN_MAP = ' '
!
! *** Initialize counters
!
      ESD_NUM_LOC = 0
      ESD_NUM_TIM = 0
!
! *** Irrigation values
!
      IRIG_START = -1
      IRIG_RATE = -1.0
      IRIG_NET_FRAC = -1.0
!
! *** Growing season dates
!
      SEASON_START = -1
      SEASON_END   = -1
!
! *** Soil values
!
      THETA_IRG = -1.0
      THETA_DRY = -1.0
      SOIL_DENSITY = 1.5
      SOIL_DEPTH   = 15.0
!
! *** Simulation control times
!
      YEAR_START = -999
      YEAR_STOP  = -999
      YEAR_CLOSE = -999
!
! *** Number of analytes
!
      SOI_NUM_ANA = 0
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
!!    Paul W. Eslinger : 19 Sep 2002 : Version 2.0
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments and error return
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
      USE Control_Mod
      USE ESD_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_1' ! Name of this subroutine
      CHARACTER*(LENCRD) :: TITLE ! Title line from RDBLK
!
!---- First executable code --------------------------------------------
!
      IERR = 0
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
        CASE( 'ANALYTE' ) ! ====================================> ANALYTE keyword
          ESD_NUM_ANA = ESD_NUM_ANA + 1
!
        CASE( 'END' ) ! ========================================> END keyword
          REWIND(IESD)
          RETURN
!
        CASE( 'LOCATION' ) ! ===================================> LOCATION keyword
          ESD_NUM_LOC = ESD_NUM_LOC + 1
!
        CASE( 'REALIZAT' ) ! ===================================> REALIZATION Keyword
          ESD_NREAL = VALUE(1)
          NREAL = ESD_NREAL
!
        CASE( 'TIMES' ) ! ======================================> TIMES Keyword
          ESD_NUM_TIM = ESD_NUM_TIM + NVALUE
!
        CASE( 'TITLE' ) ! ======================================> TITLE Keyword
          ESD_TITLE = QUOTE(1)
!
        CASE DEFAULT ! Ignore all other keywords
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
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine handles reading the environmental settings keyword control information.  
!!    Some error checking is done on the existance of data items.  No error checking is done 
!!    on the validity of entered data.
!!
!!  Call List Variables:
!!    IERR : Output integer error flag.
!!            0 = Normal execution
!!           >0 = Terminal error encountered
!!
!!  Auxiliary Routines:
!!    CEXIST, PRTERR, and all RDBLK related routines
!!
!!  History:
!!    Paul W. Eslinger : 17 Mar 2003 : Version 2.0
!!    Paul W. Eslinger : 31 May 2005 : SCR-1080 Set annual rates
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments and error return
!!    Paul W. Eslinger : 12 Jul 2012 : Remove requirement on ANALYTE HALFLIFE existence
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
      USE ESD_Mod
      USE Control_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_2' ! Name of this subroutine
!
      INTEGER :: IDX, ITMP, IDXA  ! Temporary index variables
      REAL :: RTMP                ! Temporary real variable
      CHARACTER*(LENCRD) :: TITLE ! Title line from RDBLK
!
!     Temporary character strings to match with a RDBLK quote string
      CHARACTER(LEN=LENQQQ) :: TMP_ID, TMP_ANA, LOC_ID_2, TMP_NAME
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
!-----------------------------------------------------------------------
!                Top of loop on reading keyword cards
!-----------------------------------------------------------------------
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
        CASE( 'ANALYTE' ) ! ============================================> ANALYTE keyword
          ESD_NUM_ANA = ESD_NUM_ANA + 1
!
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%ID = TMP_ID
            ELSE
              IERR = 1
              MESSAG(1) = 'Analyte ID modifier missing quote string'
              MESSAG(2) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 2
            MESSAG(1) = 'ID modifier not entered on the ANALYTE keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('NAME    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%NAME = TRIM(TMP_NAME(1:72))
            ELSE
              IERR = 3
              MESSAG(1) = 'Analyte NAME modifier missing quote string'
              MESSAG(2) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 4
            MESSAG(1) = 'NAME modifier not entered on the ANALYTE keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('TYPE    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_ANA(ESD_NUM_ANA)%TYPE = TMP_NAME(1:2)
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
          IF( CEXIST('HALFLIFE') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 7
              MESSAG(1) = 'Halflife not found'
              MESSAG(2) = 'ANALYTE keyword, HALFLIFE modifier'
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            IF( RTMP .GT. 0.0 ) THEN
!             lambda to have units 1/yr
              ESD_ANA(ESD_NUM_ANA)%LAMBDA = LOG(2.0) / RTMP
            ELSE
              ESD_ANA(ESD_NUM_ANA)%LAMBDA = 0.0
            END IF
          ELSE
!            HALFLIFE not required for nonradioactive analytes
!            IERR = 8
!            MESSAG(1) = 'The HALFLIFE modifier is required on the ANALYTE keyword'
!            MESSAG(2) = 'Problem in the ESD keyword file'
!            CALL PRTERR( IERR, CALLER, 2 )
!            RETURN
          END IF
!
        CASE( 'END' ) ! ================================================> END keyword
          CLOSE( IESD )
          RETURN
!
        CASE( 'FILE' ) ! ===============================================> FILE keyword
!
          IF( CEXIST('C_ECDA') ) THEN
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 9
                MESSAG(1) = 'NAME modifier missing quote string on FILE (C_ECDA) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            ELSE
              IERR = 10
              MESSAG(1) = 'NAME modifier not found on FILE card for type C_ECDA'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!            WRITE(*,*) ' Under C_ECDA logic'
            IF( CEXIST('ANALYTE ') ) THEN
              CALL NXTQOT( IDX, TMP_ANA )
              CALL MATCH_ANA( TMP_ANA, IDXA )
              IF( IDXA .GT. 0 ) THEN
                FN_CON(IDXA) = TMP_ID
              ELSE
                IERR = 11
                MESSAG(1) = 'Analyte requested is not yet entered with an ANALYTE keyword'
                MESSAG(2) = 'Analyte name is '//TRIM(TMP_ANA)
                MESSAG(3) = 'Put the FILE keywords for concentrations after the ANALYTE keywords'
                MESSAG(4) = 'In the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 4 )
                RETURN
              END IF
            ELSE
              IERR = 12
              MESSAG(1) = 'ANALYTE modifier not found on FILE card for type C_DAT'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('I_ECDA') ) THEN
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 13
                MESSAG(1) = 'NAME modifier missing quote string on FILE (I_ECDA) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              ELSE
                FN_MAP = TMP_ID
              END IF
            ELSE
              IERR = 14
              MESSAG(1) = 'NAME modifier not found on FILE card for type I_ECDA'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('KDSOIL  ') ) THEN
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 15
                MESSAG(1) = 'NAME modifier missing quote string on FILE (KDSOIL) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              ELSE
                FN_KDSOIL = TMP_ID
              END IF
            ELSE
              IERR = 16
              MESSAG(1) = 'NAME modifier not found on FILE card for type KDSOIL'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('INFILT') ) THEN
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 17
                MESSAG(1) = 'NAME modifier missing quote string on FILE (INFILT) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              ELSE
                FN_INFILT = TMP_ID
              END IF
            ELSE
              IERR = 18
              MESSAG(1) = 'NAME modifier not found on FILE card for type INFILT'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
        CASE( 'IRRIGATE' ) ! ===========================================> IRRIGATE keyword
!
          IF( CEXIST('NET') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 19
              MESSAG(1) = 'Irrigation net infiltration fraction not found'
              MESSAG(2) = 'IRRIGATE keyword, NET modifier'
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            IRIG_NET_FRAC = RTMP
          ELSE
            IERR = 20
            MESSAG(1) = 'The NET modifier is required on the IRRIGATE keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('START') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 21
              MESSAG(1) = 'Irrigation start year not found'
              MESSAG(2) = 'IRRIGATE keyword, START modifier'
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            IRIG_START = RTMP
          ELSE
            IERR = 22
            MESSAG(1) = 'The START modifier is required on the IRRIGATE keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('RATE') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 23
              MESSAG(1) = 'Irrigation rate not found'
              MESSAG(2) = 'IRRIGATE keyword, RATE modifier'
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            IRIG_RATE = RTMP
          ELSE
            IERR = 24
            MESSAG(1) = 'The RATE modifier is required on the IRRIGATE keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('SPRING') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 25
              MESSAG(1) = 'Growing season spring start day not found'
              MESSAG(2) = 'SEASON keyword, SPRING modifier'
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            SEASON_START = RTMP
          ELSE
            IERR = 26
            MESSAG(1) = 'The SPRING modifier is required on the SEASON keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('FALL') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 27
              MESSAG(1) = 'Growing season fall end day not found'
              MESSAG(2) = 'SEASON keyword, FALL modifier'
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            SEASON_END = RTMP
          ELSE
            IERR = 28
            MESSAG(1) = 'The FALL modifier is required on the SEASON keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('THETAIRG') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 29
              MESSAG(1) = 'Soil moisture content for irrigation period not found'
              MESSAG(2) = 'IRRIGATE keyword, THETAIRG modifier'
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            THETA_IRG = RTMP
          ELSE
            IERR = 30
            MESSAG(1) = 'The THETAIRG modifier is required on the IRRIGATE keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('THETADRY') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 31
              MESSAG(1) = 'Soil moisture content outside the irrigation period not found'
              MESSAG(2) = 'IRRIGATE keyword, THETADRY modifier'
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            THETA_DRY = RTMP
          ELSE
            IERR = 32
            MESSAG(1) = 'The THETADRY modifier is required on the IRRIGATE keyword'
            MESSAG(2) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
        CASE( 'LOCATION' ) ! ===========================================> LOCATION keyword
!
          ESD_NUM_LOC = ESD_NUM_LOC + 1
!
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%ID = TMP_ID
            ELSE
              IERR = 33
              MESSAG(1) = 'Location ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 34
            MESSAG(1) = 'ID modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('NAME    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%NAME = TRIM(TMP_NAME(1:72))
            ELSE
              IERR = 35
              MESSAG(1) = 'Location NAME modifier missing quote string'
              MESSAG(2) = 'Location ID = '//ESD_LOC(ESD_NUM_LOC)%ID
              MESSAG(3) = 'Modify the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 36
            MESSAG(1) = 'NAME modifier not entered on the LOCATION keyword'
            MESSAG(2) = 'Location ID = '//ESD_LOC(ESD_NUM_LOC)%ID
            MESSAG(3) = 'Modify the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
          IF( CEXIST('AIRD') ) ESD_LOC(ESD_NUM_LOC)%AIRD = .TRUE.
          IF( CEXIST('SODR') ) ESD_LOC(ESD_NUM_LOC)%SODR = .TRUE.
          IF( CEXIST('SOGW') ) ESD_LOC(ESD_NUM_LOC)%SOGW = .TRUE.
          IF( CEXIST('GWAT') ) ESD_LOC(ESD_NUM_LOC)%GWAT = .TRUE.
          IF( CEXIST('SWAT') ) ESD_LOC(ESD_NUM_LOC)%SWAT = .TRUE.
!
          IF( CEXIST('SOSW') ) THEN
            ESD_LOC(ESD_NUM_LOC)%SOSW = .TRUE.
            IF( CEXIST('IRG_SWAT') ) THEN
              CALL NXTQOT( IDX, LOC_ID_2 )
              IF( IDX .EQ. 0 ) THEN
                IERR = 37
                MESSAG(1) = 'The IRG_SWAT modifier on the LOCATION keyword did not'
                MESSAG(2) = 'have a quote string associated with it.'
                MESSAG(3) = 'Location ID is : '//ESD_LOC(ESD_NUM_LOC)%ID
                MESSAG(4) = 'Problem in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 4 )
                RETURN
              ELSE
                ESD_LOC(ESD_NUM_LOC)%ID_SWAT = TRIM(LOC_ID_2)
              END IF
            ELSE
              IERR = 38
              MESSAG(1) = 'The IRG_SWAT modifier was not present on LOCATION keyword'
              MESSAG(2) = 'that requested a soil irrigated by surface water.'
              MESSAG(3) = 'Location ID is : '//ESD_LOC(ESD_NUM_LOC)%ID
              MESSAG(4) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('TYPE') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%TYPE = TRIM(TMP_ID(1:8))
            ELSE
              IERR = 39
              MESSAG(1) = 'TYPE modifier missing quote string on the LOCATION keyword'
              MESSAG(2) = 'Location ID = '//ESD_LOC(ESD_NUM_LOC)%ID
              MESSAG(3) = 'Modify the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 40
            MESSAG(1) = 'TYPE modifier not entered on the LOCATION keyword'
            MESSAG(2) = 'Location ID = '//ESD_LOC(ESD_NUM_LOC)%ID
            MESSAG(3) = 'Modify the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
        CASE( 'PERIOD' ) !  ============================================> PERIOD Keyword
          IF( CEXIST('START   ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              YEAR_START = RTMP
            ELSE
              IERR = 41
              MESSAG(1) = 'PERIOD START modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 42
            MESSAG(1) = 'START modifier not entered on the PERIOD keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('STOP    ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              YEAR_STOP = RTMP
            ELSE
              IERR = 43
              MESSAG(1) = 'PERIOD STOP modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 44
            MESSAG(1) = 'STOP modifier not entered on the PERIOD keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('CLOSURE ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              YEAR_CLOSE = RTMP
            ELSE
              IERR = 45
              MESSAG(1) = 'PERIOD CLOSURE modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 46
            MESSAG(1) = 'CLOSURE modifier not entered on the PERIOD keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
        CASE( 'TIMES' ) ! ==============================================> TIMES Keyword
          IF( NVALUE .GT. 0 ) THEN
            DO ITMP = 1, NVALUE
              ESD_NUM_TIM = ESD_NUM_TIM + 1
              ESD_TIM(ESD_NUM_TIM)%TIME = VALUE(ITMP)
            END DO
          ELSE
            IERR = 47
            MESSAG(1) = 'No numeric values found on the TIMES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
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
!
      SUBROUTINE KEY_SOIL_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and the SOIL keyword control
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
!!    Paul W. Eslinger : 19 Sep 2002 : Version 1.0
!!    Paul W. Eslinger : 28 Jun 2005 : SCR-1080 Add COMPUTE keyword
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments and error return
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
      CHARACTER(LEN=10) :: CALLER = 'KEY_SOIL_1' ! Name of this subroutine
      INTEGER :: IDX              ! Temporary index variable
      CHARACTER*(LENCRD) :: TITLE ! Title line from RDBLK
      REAL :: RTMP                ! Temporary real variable
!
!---- First executable code --------------------------------------------
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
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'ANALYTE' ) ! ============================================> ANALYTE keyword
          SOI_NUM_ANA = SOI_NUM_ANA + NQUOTE
!
        CASE( 'COMPUTE' ) ! ============================================> COMPUTE keyword
          IF( CEXIST('SODR') ) COMPUTE_SODR = .TRUE.
          IF( CEXIST('SOGW') ) COMPUTE_SOGW = .TRUE.
          IF( CEXIST('SOSW') ) COMPUTE_SOSW = .TRUE.
!
        CASE( 'DEBUG' ) ! ==============================================> DEBUG keyword
          IF( CEXIST('LOOP') )     BUG_LOOP = .TRUE.
          IF( CEXIST('CONCENTR') ) THEN
            BUG_CONC = .TRUE.
            IF( CEXIST('BQ') ) BUG_CONC_BQ = .TRUE.
          END IF
          IF( CEXIST('COMPUTE') )  BUG_COMP = .TRUE.
!
        CASE( 'END' ) ! ================================================> END keyword
          REWIND( IKEY )
          RETURN
!
        CASE( 'EXECUTE' ) ! ============================================> EXECUTE keyword
          EXECUT = .TRUE.
!
        CASE( 'FILE' ) ! ===============================================> FILE keyword
          IF( CEXIST('ESD     ') ) THEN
            CALL NXTQOT( IDX, FN_ESD )
            IF( IDX .EQ. 0 ) THEN
              IERR = 2
              MESSAG(1) = 'The ESD modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
        CASE( 'SOIL' ) ! ===============================================> SOIL Keyword
!
          IF( CEXIST('DENSITY') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 15
              MESSAG(1) = 'Surface soil density value not found'
              MESSAG(2) = 'SOIL keyword, DENSITY modifier'
              MESSAG(3) = 'Problem in the SOIL keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            SOIL_DENSITY = RTMP
          END IF
!
          IF( CEXIST('DEPTH') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 16
              MESSAG(1) = 'Surface soil depth value not found'
              MESSAG(2) = 'SOIL keyword, DEPTH modifier'
              MESSAG(3) = 'Problem in the SOIL keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            SOIL_DEPTH = RTMP
          END IF
!
        CASE( 'TITLE' ) ! ==============================================> TITLE Keyword
          PTITLE = QUOTE(1)
!
        CASE( 'USER' ) ! ===============================================> USER Keyword
          USRNAM = QUOTE(1)
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
!
      SUBROUTINE KEY_SOIL_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and the SOIL keyword control
!!    information required to finish the scenario definition.
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
!!    Paul W. Eslinger : 10 Oct 2002 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments and error return
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined external functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Local variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'KEY_SOIL_2' ! Name of this subroutine
      INTEGER :: IANA  ! Temporary index variable
      INTEGER :: IDX   ! Temporary index variable
      INTEGER :: IDX2  ! Temporary index variable
      INTEGER :: IREL  ! Temporary realization index variable
      INTEGER :: IDXA  ! Temporary index for analytes
      INTEGER :: IDXL  ! Temporary index for locations
      INTEGER :: ILOC  ! Temporary index for locations
!
      CHARACTER(LEN=LENCRD) :: TITLE ! Keyword data line read by RDBLK routines
      CHARACTER(LEN=LENQQQ) :: TMP_ID, TMP_KD, TMP_AN ! Temporary variable for quote strings
      CHARACTER(LEN=6) :: SHORT_ID   ! Temporary ID string
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
!
      ILINE = 0
!
!-----------------------------------------------------------------------
!                Top of loop on reading keyword cards
!-----------------------------------------------------------------------
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
        CASE( 'ANALYTE' ) ! ============================================> ANALYTE keyword
!
          IF( NQUOTE .LT. 1 ) THEN
            IERR = 1
            MESSAG(1) = 'At least one ID is required on the ANALTYE keyword'
            MESSAG(2) = 'Modify the ANALYTE keyword in the SOIL keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          DO IANA = 1, NQUOTE
            CALL MATCH_ANA( QUOTE(IANA), IDXA )
            IF( IDXA .GT. 0 ) THEN
              ESD_ANA(IDXA)%COMP = .TRUE.
            ELSE
              IERR = 2
              MESSAG(1) = 'Analyte requested that is not in the master list'
              MESSAG(2) = 'Analyte name is '//TRIM(QUOTE(IANA))
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END DO
!
        CASE( 'END' ) ! ================================================> END keyword
          CLOSE( IKEY )
          RETURN
!
        CASE( 'KDSOIL' ) ! =============================================> KDSOIL keyword
!
          IF( NQUOTE .LT. 2 ) THEN
            IERR = 3
            MESSAG(1) = 'Invalid form for the KDSOIL keyword'
            MESSAG(2) = 'At least 2 quote strings are required'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('LOCATION') .AND. NQUOTE.LT.3 ) THEN
            IERR = 4
            MESSAG(1) = 'Invalid form for the KDSOIL keyword'
            MESSAG(2) = 'At least 3 quote strings are required'
            MESSAG(3) = 'when the LOCATION modifier is used.'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
          IF( CEXIST('ANALYTE') ) THEN
            CALL NXTQOT( IDX, TMP_AN )
            CALL MATCH_ANA( TMP_AN, IDXA )
            IF( IDXA .LE. 0 ) THEN
              IERR = 5
              MESSAG(1) = 'Analyte requested on KDSOIL keyword is not in the master list'
              MESSAG(2) = 'Analyte name is '//TMP_AN
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 6
            MESSAG(1) = 'Every KDSOIL keyword must have the ANALYTE modifier'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('KDSOIL') ) THEN
            CALL NXTQOT( IDX, TMP_KD )
            IF( IDX .LE. 0 ) THEN
              IERR = 7
              MESSAG(1) = 'KDSOIL modifier on the KDSOIL keyword is missing the ID string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 8
            MESSAG(1) = 'Every KDSOIL keyword must also have the KDSOIL modifier'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!         Check for a LOCATION or TERSE modifier
          IF( .NOT.(CEXIST('LOCATION') .OR. CEXIST('TERSE')) ) THEN
            IERR = 9
            MESSAG(1) = 'The KDSOIL keyword must have a LOCATION or TERSE modifier.'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!         Check for only one of LOCATION or TERSE
          IF( CEXIST('LOCATION') .AND. CEXIST('TERSE') ) THEN
            IERR = 10
            MESSAG(1) = 'The KDSOIL keyword must have a LOCATION or TERSE modifier.'
            MESSAG(2) = 'But, only one of the LOCATION or TERSE modifiers is alowed.'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('LOCATION') ) THEN ! Assign the KDSOIL value to one or more locations
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 11
              MESSAG(1) = 'The LOCATION modifier on the KDSOIL keyword'
              MESSAG(2) = 'must have an associated quote string.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            IDX = IDX - 1
            DO ILOC = 1, NQUOTE-2
              IDX = IDX + 1
              TMP_ID = QUOTE(IDX)
              CALL MATCH_LOC( TMP_ID, IDXL )
              IF( IDXL .LE. 0  ) THEN
                IERR = 12
                MESSAG(1) = 'Location requested on KDSOIL keyword is not in the master list'
                MESSAG(2) = 'Location ID is '//TMP_ID
                MESSAG(3) = 'Analyte ID is '//TMP_AN
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
              KD_MAP(IDXL,IDXA) = TRIM(TMP_KD)
            END DO
          END IF
!
          IF( CEXIST('TERSE') ) THEN ! Assign the same KDSOIL value to all locations
            DO ILOC = 1, ESD_NUM_LOC
              KD_MAP(ILOC,IDXA) = TRIM(TMP_KD)
            END DO
          END IF
!
        CASE( 'LOCATION' ) ! ===========================================> LOCATION keyword
!
          IF( CEXIST('ID') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              SHORT_ID = TMP_ID
              CALL MATCH_LOC( SHORT_ID, IDXL )
              IF( IDXL .LE. 0 ) THEN
                IERR = 13
                MESSAG(1) = 'Location requested is not in the master list'
                MESSAG(2) = 'Location ID: ' // TRIM(TMP_ID)
                MESSAG(3) = 'Problem in the SOIL keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
              ESD_LOC(IDXL)%COMP = .TRUE.
            ELSE
              IERR = 14
              MESSAG(1) = 'Location ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 15
            MESSAG(1) = 'ID modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('INFILT') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              IN_MAP(IDXL) = TMP_ID
            ELSE
              IERR = 16
              MESSAG(1) = 'Location INFILT modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 17
            MESSAG(1) = 'LOCATION keyword missing infiltration factor modifier INFILT'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
        CASE( 'REALIZAT' ) ! ===========================================> REALIZATION Keyword
!
!         Check for single option
          IF( CEXIST('ALL').AND.CEXIST('LIST') .OR. CEXIST('ALL').AND.CEXIST('RANGE') .OR. &
            CEXIST('LIST').AND.CEXIST('RANGE') ) THEN
            IERR = 18
            MESSAG(1) = 'Invalid options on the REALIZATION keyword'
            MESSAG(2) = 'Only one of ALL, LIST, or RANGE allowed'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!         ALL option
          IF( CEXIST('ALL') ) THEN
            USE_ALL_REL = .TRUE.
            DO IREL = 1, NREAL
              REL_USE(IREL) = .TRUE.
            END DO
          ENDIF
!
!         LIST option
          IF( CEXIST('LIST') ) THEN
            DO IREL = 1, NVALUE
              IDX = VALUE(IREL)
              IF( IDX.GE.1 .AND. IDX.LE.NREAL ) REL_USE(IDX) = .TRUE.
            END DO
          END IF
!
!         RANGE option
          IF( CEXIST('RANGE') ) THEN
            IDX  = VALUE(1)
            IDX2 = VALUE(2)
            IF( IDX.GT.0 .AND. IDX.LE.IDX2 .AND. IDX2.LE.NREAL ) THEN
              DO IREL = IDX, IDX2
                REL_USE(IREL) = .TRUE.
              END DO
            ELSE
              IERR = 19
              MESSAG(1) = 'Invalid range of realizations encountered for the'
              MESSAG(2) = 'RANGE option on the REALIZATION keyword'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
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
!!    Paul W. Eslinger : 19 Sep 2002 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Update comments
!!
!!**********************************************************************
!
! *** Global variables
      USE ESD_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IDX
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
!
! *** Local variables
      INTEGER :: IANA ! Looping variable
!
!---- First executable code --------------------------------------------
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
!!    LABEL : Input  - Character - Location label
!!    IDX   : Output - Integer   - Index for data associated with LABEL
!!
!!  History:
!!
!!    Paul W. Eslinger : 29 Oct 2002 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Update comments
!!
!!**********************************************************************
!
! *** Global variables
      USE ESD_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IDX
      CHARACTER(LEN=*), INTENT(IN) :: LABEL
!
! *** Local variables
      INTEGER :: ILOC ! Looping variable
!
!---- First executable code --------------------------------------------
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
      SUBROUTINE OPEN_ESD( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the ESD keyword file for reading keywords.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Sep 2002 : Version 2.0
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      USE Files_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'OPEN_ESD' ! Name of this subroutine
!
      INTEGER :: IERF  ! Status variable for open statement
      LOGICAL :: THERE ! Logical flag for inquire status
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the file name was defined
!
      IF( FN_ESD .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The ESD keyword file name is blank'
        MESSAG(2) = 'Change the file name on the FILE keyword, ESD modifier'
        MESSAG(3) = 'Problem exists in the SOIL keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Check if the requested file exists
!
      INQUIRE(FILE=FN_ESD,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested ESD keyword file was not found'
        MESSAG(2) = 'Change the file name on the FILE keyword, ESD modifier'
        MESSAG(3) = 'File: '//TRIM(FN_ESD)
        MESSAG(4) = 'Problem exists in the SOIL keyword file'
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IESD,FILE=FN_ESD,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the ESD keyword file'
        MESSAG(2) = 'File name entered on the FILE keyword, ESD modifier'
        MESSAG(3) = 'File: '//TRIM(FN_ESD)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE OPEN_INFILT( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the input file which contain the infiltration
!!    data and reads the data.
!!
!!  History:
!!
!!    Paul W. Eslinger :  7 Oct 2002 : Version 1.0
!!    Paul W. Eslinger : 18 Dec 2003 : Add units change logic
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Infilt_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'OPEN_INFILT' ! Name of this subroutine
!
      INTEGER :: ILOC ! Local location index
      INTEGER :: IREL ! Local realization index
      INTEGER :: I    ! Local looping variable
      INTEGER :: JTMP ! Local unused variable
!
      LOGICAL :: THERE ! Temporary logical variable
      INTEGER :: IERF  ! Temporary system error code
      CHARACTER(LEN=20) :: TMP_UNITS
!
      REAL, ALLOCATABLE :: INWORK(:) ! Temporary infiltration work vector
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the requested file exists
!
      IF( FN_INFILT .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The infiltration library file name is blank'
        MESSAG(2) = 'Change the file name on the ESD FILE keyword, INFILT modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      INQUIRE(FILE=FN_INFILT,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested infiltration data file was not found'
        MESSAG(2) = 'Change the file name on the ESD FILE keyword, INFILT modifier'
        MESSAG(3) = 'File: '//TRIM(FN_INFILT)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IINF,FILE=FN_INFILT,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the infiltration data file'
        MESSAG(2) = 'File name entered on the ESD FILE keyword, INFILT modifier'
        MESSAG(3) = 'File: '//TRIM(FN_INFILT)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read all of the header data
!
      READ(IINF,*,ERR=998,END=998) IN_TYPE   ! Type of data file
      READ(IINF,*,ERR=998,END=998) IN_PTITLE ! Program modification date from infiltration generator
      READ(IINF,*,ERR=998,END=998) IN_PRGNAM ! Program name from infiltration generator
      READ(IINF,*,ERR=998,END=998) IN_PRGVER ! Program version number from infiltration generator
      READ(IINF,*,ERR=998,END=998) IN_PRGDAT ! Program date from infiltration generator
      READ(IINF,*,ERR=998,END=998) IN_CRUNID ! Run identification number from infiltration generator
      READ(IINF,*,ERR=998,END=998) IN_USRNAM ! User name from infiltration generator
      READ(IINF,*,ERR=998,END=998) IN_NUM    ! Number of infiltration stochastic variables
      READ(IINF,*,ERR=998,END=998) IN_NITR   ! Number of infiltration stochastic iterations
!
! *** Do some error checking
!
      IF( IN_TYPE .NE. 'INFILT' ) THEN
        IERR = 4
        MESSAG(1) = 'Wrong data type in the infiltration file'
        MESSAG(2) = 'First entry in the file was not the string INFILT'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( IN_NITR .LT. NREAL ) THEN
        IERR = 5
        MESSAG(1) = 'Not enough realizations of data in the infiltration data file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Read the infiltration data and see if it matches with locations being used in this run
!
      ALLOCATE( INWORK(IN_NITR) )
!
      DO I = 1, IN_NUM
!
! ***   Read the stochastic realizations for one infiltration ID
!       The variable index (JTMP) is not used for anything in this code
!
        READ(IINF,*,ERR=999,END=999) JTMP, IN_ID, TMP_UNITS, (INWORK(IREL),IREL=1,IN_NITR)    
!
! ***   See if the data just read are needed
!
        DO ILOC = 1, ESD_NUM_LOC
          IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
          IF( .NOT.ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) CYCLE
          IF( IN_MAP(ILOC) .EQ. IN_ID ) THEN
          IN_UNITS(ILOC) = TMP_UNITS
          IN_UNITS(ILOC)(1:1) = 'c'
            IF( TMP_UNITS .NE. 'mm/yr' ) THEN
              IERR = 9
              MESSAG(1) = 'Infiltration data units were expected as mm/yr'
              MESSAG(2) = 'Data in the library used units of '//TRIM(TMP_UNITS)
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              INFILT(ILOC,IREL) = INWORK(IREL) / 10.0 ! The 10 changes from mm/yr to cm/yr
            END DO
          END IF
        END DO
!
      END DO
!
! *** Check that all required infiltration data are entered
!
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
        IF( .NOT.ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) CYCLE
        DO IREL = 1, NREAL
          IF( INFILT(ILOC,IREL) .LT. 0.0 ) THEN
            IERR = 6
            MESSAG(1) = 'Invalid infiltration value encountered, Realization = '
            WRITE(MESSAG(1)(55:),'(I0)') IREL
            MESSAG(2) = 'Location ID = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Invalid match with ID in the infiltration data library'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        END DO
      END DO
!
! *** Normal exit
!
      DEALLOCATE( INWORK )
      CLOSE( IINF )
      RETURN
!
! *** Error reading the header data
!
  998 CONTINUE
      IERR = 7
      MESSAG(1) = 'Error reading header data from the infiltration data file'
      CALL PRTERR( IERR, CALLER, 1 )
      RETURN
!
! *** Error reading the header data
!
  999 CONTINUE
      IERR = 8
      MESSAG(1) = 'Error reading data from the infiltration data file'
      CALL PRTERR( IERR, CALLER, 1 )
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE OPEN_KDSOIL( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the input file which contain the KD
!!    data and reads the data.
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Apr 2000 : Version 1.0
!!    Paul W. Eslinger : 17 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger :  2 Jul 2003 : Fix units on the KD variable
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
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
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Kdsoil_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'OPEN_KDSOIL' ! Name of this subroutine
!
      INTEGER :: IANA ! Local analyte index
      INTEGER :: ILOC ! Local location index
      INTEGER :: IREL ! Local realization index
      INTEGER :: I    ! Local looping variable
      INTEGER :: J    ! Local looping variable
!
      LOGICAL :: THERE ! Temporary logical variable
      INTEGER :: IERF  ! Temporary system error code
!
      CHARACTER(LEN=20) :: TMP_UNITS ! Temporary units label from file
      REAL, ALLOCATABLE :: KDWORK(:) ! Temporary KD work vector
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Check if the requested file exists
!
      IF( FN_KDSOIL .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The soil-water KD file name is blank'
        MESSAG(2) = 'Change the file name on the ESD FILE keyword, KDSOIL modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      INQUIRE(FILE=FN_KDSOIL,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested soil-water KD data file was not found'
        MESSAG(2) = 'Change the file name on the ESD FILE keyword, KDSOIL modifier'
        MESSAG(3) = 'File: '//TRIM(FN_KDSOIL)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Attempt to open the file
!
      OPEN(IKDS,FILE=FN_KDSOIL,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the KD data file'
        MESSAG(2) = 'File name entered on the ESD FILE keyword, KDSOIL modifier'
        MESSAG(3) = 'File: '//TRIM(FN_KDSOIL)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Read all of the header data
!
      READ(IKDS,*,ERR=998,END=998) KD_TYPE   ! Type of data file
      READ(IKDS,*,ERR=998,END=998) KD_PTITLE ! Program modification date from KD generator
      READ(IKDS,*,ERR=998,END=998) KD_PRGNAM ! Program name from KD generator
      READ(IKDS,*,ERR=998,END=998) KD_PRGVER ! Program version number from KD generator
      READ(IKDS,*,ERR=998,END=998) KD_PRGDAT ! Program date from KD generator
      READ(IKDS,*,ERR=998,END=998) KD_CRUNID ! Run identification number from KD generator
      READ(IKDS,*,ERR=998,END=998) KD_USRNAM ! User name from KD generator
      READ(IKDS,*,ERR=998,END=998) KD_NUM    ! Number of KD stochastic variables
      READ(IKDS,*,ERR=998,END=998) KD_NITR   ! Number of KD stochastic iterations
!
! *** Do some error checking
!
      IF( KD_TYPE .NE. 'KDSOIL' ) THEN
        IERR = 4
        MESSAG(1) = 'Wrong data type in the soil-water KD data file'
        MESSAG(2) = 'First entry in the file was not the string KDSOIL'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( KD_NITR .LT. NREAL ) THEN
        IERR = 5
        MESSAG(1) = 'Not enough realizations of data in the KD data file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Read the KD data and see if it matches with locations and anlaytes
!     we are using in this run
!
      ALLOCATE( KDWORK(KD_NITR) )
!
      DO I = 1, KD_NUM
!
! ***   Read the stochastic realizations for one KDSOIL ID
!
        READ(IKDS,*,ERR=999,END=999) KD_NUM, KD_ID, TMP_UNITS, (KDWORK(J),J=1,KD_NITR)
!
! ***   See if the data just read are needed
!
        DO ILOC = 1, ESD_NUM_LOC
          IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
          IF( .NOT.ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) CYCLE
          DO IANA = 1, ESD_NUM_ANA
            IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
            IF( KD_MAP(ILOC,IANA) .EQ. KD_ID ) THEN
              KD_UNITS(ILOC) = TMP_UNITS 
              DO IREL = 1, NREAL
                KD(ILOC,IANA,IREL) = KDWORK(IREL)
              END DO
            END IF
          END DO
        END DO
!
      END DO
!
! *** Check that all required KD's are entered
!
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
        IF( .NOT.ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) CYCLE
        DO IANA = 1, ESD_NUM_ANA
          IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
          DO IREL = 1, NREAL
            IF( KD(ILOC,IANA,IREL) .LT. 0.0 ) THEN
              IERR = 6
              MESSAG(1) = 'Invalid (negative) KD value encountered, Realization = '
              WRITE(MESSAG(1)(56:),'(I0)') IREL
              MESSAG(2) = 'Location ID = '//ESD_LOC(ILOC)%ID
              MESSAG(3) = 'Analyte ID = '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Option: Data not matched on a KDSOIL card'
              MESSAG(5) = 'Option: Invalid distribution on an ESD KDSOIL keyword'
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
        END DO
      END DO
!
! *** Normal exit
!
      DEALLOCATE( KDWORK )
      CLOSE( IKDS )
      RETURN
!
! *** Error reading the header data
!
  998 CONTINUE
      IERR = 7
      MESSAG(1) = 'Error reading header data from the KDSOIL data file'
      CALL PRTERR( IERR, CALLER, 1 )
      RETURN
!
! *** Error reading the header data
!
  999 CONTINUE
      IERR = 8
      MESSAG(1) = 'Error reading data from the KDSOIL data file'
      CALL PRTERR( IERR, CALLER, 1 )
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE OPEN_KEY( IERR )
!!*********************************************************************************
!!
!!  Purpose:
!!    This subroutine performs two actions for the keyword file:
!!      1. Obtain the file name and store it in the variable FNKEY
!!         a) First check the command line to see if the file
!!            name was provided or,
!!         b) query the user for the file name if it was not
!!            entered on the command line.
!!      2. Open the file on unit number IKEY for use by other subroutines
!!
!!  Note:
!!    This subroutine does not call PRTERR when an error occurs because
!!    PRTERR writes to the report file.  The report file is not opened
!!    until after the keywords are read.
!!
!!  History:
!!    Paul W. Eslinger : 10 Jun 2002 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Change from GETCL to GETARG function
!!    Paul W. Eslinger :  9 Jul 2012 : Revise to a common callable routine
!!    Paul W. Eslinger :  7 Feb 2014 : (TIIA SCR-0003) Remove the "run" and "done" files
!!
!!*********************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
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
!      INTEGER :: PLEN_KEY      ! Number of characters in the variable PATH_KEY
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
 1020   FORMAT(' The requested keyword file was not found'/' File: ',A)
        RETURN
      END IF
!
! *** Attempt to open the file
      OPEN(IKEY,FILE=FNKEY,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 2
        WRITE(*,1030) TRIM(FNKEY)
 1030   FORMAT(' System error opening the input keyword file'/' File: ',A)
        RETURN
      END IF
!
! *** Get the path from the SOIL keyword file for use in the "Done" file
!      CALL GET_PATH( FNKEY, PATH_KEY, PLEN_KEY, IERR )
!      IF( IERR .NE. 0  ) THEN
!        IERR = 3
!        WRITE(*,1040) TRIM(FNKEY)
! 1040   FORMAT(' Error extracting path from the SOIL keyword file name'/' File: ',A)
!        RETURN
!      END IF
!
!      IF( PLEN_KEY .GT. 0 ) THEN
!        FN_DONE = TRIM(PATH_KEY) // TRIM(FN_DONE)
!        FN_RUN  = TRIM(PATH_KEY) // TRIM(FN_RUN)
!      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PRINT_CONC( ILOC, IANA )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes the concentrations passed to the soil
!!    computations to the report file for a single analyte and location
!!    as a function of time and realization
!!
!!  History:
!!
!!    Paul W. Eslinger :  9 Oct 2002 : Version 1.0
!!    Paul W. Eslinger : 16 May 2005 : Add output data units
!!    Paul W. Eslinger : 28 Jun 2005 : SCR-1080 Allow soil type selections
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Soil_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER, INTENT(IN) :: IANA ! Analyte index
!
! *** Local variables
      INTEGER :: ITIM    ! Time solution looping index
      INTEGER :: IREL    ! Realization looping variable
      CHARACTER(LEN=10) :: CUNITS ! Units string for output
!
!---- Executable code ---------------------------------------------------
!
! *** Output the air deposition values
!
      WRITE(IRPT,1000) ILOC, ESD_LOC(ILOC)%ID, IANA, ESD_ANA(IANA)%ID
 1000 FORMAT(/'Concentrations passed to the soil model'/&
              '    ILOC             = ',I0/&
              '    ESD_LOC(ILOC)%ID = ',A/&
              '    IANA             = ',I0/&
              '    ESD_ANA(IANA)&ID = ',A)
!
      IF( COMPUTE_SODR .OR. COMPUTE_SOGW .OR. COMPUTE_SOSW ) THEN
        IF( ESD_ANA(IANA)%TYPE(2:2).EQ.'R' ) THEN
          CUNITS = 'Ci/m^2/day'
        ELSE
          CUNITS = 'Kg/m^2/day'
        END IF
        WRITE(IRPT,1010) TRIM(ESD_ANA(IANA)%ID)//' - AIRD', 'Deposition rate from air ('//TRIM(CUNITS)//')'
 1010   FORMAT(/A,1X,A)
        DO ITIM = 1, ESD_NUM_TIM
          WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, (AIRD(ITIM,IREL),IREL=1,ESD_NREAL)
 1020     FORMAT(1X,I0,1P,500(1X,E12.5))
        END DO
      END IF
!
      IF( COMPUTE_SOGW ) THEN
        IF( ESD_ANA(IANA)%TYPE(2:2).EQ.'R' ) THEN
          CUNITS = 'Ci/m^3'
        ELSE
          CUNITS = 'Kg/m^3'
        END IF
        WRITE(IRPT,1010) TRIM(ESD_ANA(IANA)%ID)//' - GWAT', 'Groundwater concentration ('//TRIM(CUNITS)//')'
        DO ITIM = 1, ESD_NUM_TIM
          WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, (GWAT(ITIM,IREL),IREL=1,ESD_NREAL)
        END DO
      END IF
!
      IF( COMPUTE_SOSW ) THEN
        IF( ESD_ANA(IANA)%TYPE(2:2).EQ.'R' ) THEN
          CUNITS = 'Ci/m^3'
        ELSE
          CUNITS = 'Kg/m^3'
        END IF
        WRITE(IRPT,1010) TRIM(ESD_ANA(IANA)%ID)//' - SWAT', 'Surface water concentration ('//TRIM(CUNITS)//')'
        DO ITIM = 1, ESD_NUM_TIM
          WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, (SWAT(ITIM,IREL),IREL=1,ESD_NREAL)
        END DO
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PRINT_SOIL( ILOC, IANA )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes the concentrations computed by the soil
!!    computations to the report file for a single analyte and location
!!    as a function of time and realization
!!
!!  History:
!!
!!    Paul W. Eslinger :  9 Oct 2002 : Version 1.0
!!    Paul W. Eslinger : 16 May 2005 : Add output data units
!!    Paul W. Eslinger : 28 Jun 2005 : SCR-1080 Allow soil type selections
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Soil_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER, INTENT(IN) :: IANA ! Analyte index
!
! *** Local variables
!
      INTEGER :: ITIM    ! Time solution looping index
      INTEGER :: IREL    ! Realization looping variable
      LOGICAL :: OUTPUT_BQ ! Flag to convert rads to Bq before output
!
      CHARACTER(LEN=5) :: CUNITS    = 'Kg/Kg' ! Temporary units label
      CHARACTER(LEN=5) :: CUNITS_CI = 'Ci/kg' ! Temporary units label
      CHARACTER(LEN=5) :: CUNITS_BQ = 'Bq/Kg' ! Temporary units label
!
!---- Executable code ---------------------------------------------------
!
      OUTPUT_BQ = .FALSE.
      IF( ESD_ANA(IANA)%TYPE(2:2).EQ.'R' .AND. BUG_CONC_BQ ) OUTPUT_BQ = .TRUE.
!
! *** Output the air deposition values
!
      WRITE(IRPT,1000) ILOC, ESD_LOC(ILOC)%ID, IANA, ESD_ANA(IANA)%ID
 1000 FORMAT(/'Concentrations computed by the soil model'/&
              '    ILOC             = ',I0/&
              '    ESD_LOC(ILOC)%ID = ',A/&
              '    IANA             = ',I0/&
              '    ESD_ANA(IANA)&ID = ',A)
!
      IF( ESD_LOC(ILOC)%SODR .AND. COMPUTE_SODR ) THEN
        WRITE(IRPT,1010) TRIM(ESD_ANA(IANA)%ID)//' - SODR', 'Dry soil (no irrigation) concentration',' Year Units  Value'
 1010   FORMAT(/A,1X,A/A)
        DO ITIM = 1, ESD_NUM_TIM
          IF( ESD_ANA(IANA)%TYPE(2:2) .EQ. 'R' ) THEN
            IF( OUTPUT_BQ ) THEN
              DO IREL = 1, ESD_NREAL
                CVEC(IREL) = SODR(ITIM,IREL) * 3.7E10
              END DO
              WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, CUNITS_BQ, (CVEC(IREL),IREL=1,ESD_NREAL)
 1020         FORMAT(1X,I0,1P,1X,A,500(1X,E12.5))
            ELSE
              WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, CUNITS_CI, (SODR(ITIM,IREL),IREL=1,ESD_NREAL)
            END IF
          ELSE
            WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, CUNITS, (SODR(ITIM,IREL),IREL=1,ESD_NREAL)
          END IF
        END DO
      END IF
!
      IF( ESD_LOC(ILOC)%SOGW .AND. COMPUTE_SOGW ) THEN
        WRITE(IRPT,1010) TRIM(ESD_ANA(IANA)%ID)//' - SOGW', 'Irrigated (groundwater) soil concentration',' Year Units  Value'
        DO ITIM = 1, ESD_NUM_TIM
          IF( ESD_ANA(IANA)%TYPE(2:2) .EQ. 'R' ) THEN
            IF( OUTPUT_BQ ) THEN
              DO IREL = 1, ESD_NREAL
                CVEC(IREL) = SOGW(ITIM,IREL) * 3.7E10
              END DO
              WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, CUNITS_BQ, (CVEC(IREL),IREL=1,ESD_NREAL)
            ELSE
              WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, CUNITS_CI, (SOGW(ITIM,IREL),IREL=1,ESD_NREAL)
            END IF
          ELSE
            WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, CUNITS, (SOGW(ITIM,IREL),IREL=1,ESD_NREAL)
          END IF
        END DO
      END IF
!
      IF( ESD_LOC(ILOC)%SOSW .AND. COMPUTE_SOSW ) THEN
        WRITE(IRPT,1010) TRIM(ESD_ANA(IANA)%ID)//' - SOSW', 'Irrigated (surface water) soil concentration',' Year Units  Value'
        DO ITIM = 1, ESD_NUM_TIM
          IF( ESD_ANA(IANA)%TYPE(2:2) .EQ. 'R' ) THEN
            IF( OUTPUT_BQ ) THEN
              DO IREL = 1, ESD_NREAL
                CVEC(IREL) = SOSW(ITIM,IREL) * 3.7E10
              END DO
              WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, CUNITS_BQ, (CVEC(IREL),IREL=1,ESD_NREAL)
            ELSE
              WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, CUNITS_CI, (SOSW(ITIM,IREL),IREL=1,ESD_NREAL)
            END IF
          ELSE
            WRITE(IRPT,1020) ESD_TIM(ITIM)%TIME, CUNITS, (SOSW(ITIM,IREL),IREL=1,ESD_NREAL)
          END IF
        END DO
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PROCESS_SOIL( IANA, IERR )
!!*************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads input concentration data for a single analyte and schedules
!!    the processing for all three soil concentrations for all locations, times, and
!!    realizations.  The looping order is location, then time, then realization.
!!
!!  History:
!!
!!    Paul W. Eslinger : 17 Oct 2002 : Version 1.0
!!    Paul W. Eslinger : 25 Sep 2003 : Trap negative concentrations
!!    Paul W. Eslinger : 12 Dec 2003 : Change negative concentration trap
!!    Paul W. Eslinger : 14 Dec 2004 : Add additional screen output message
!!    Paul W. Eslinger : 31 May 2005 : SCR-1080 Set AIRD to annual rate
!!    Paul W. Eslinger : 28 Jun 2005 : SCR-1080 Allow soil type selections
!!                                     Remove unnecessary read on SODR, SOGW and SOSW
!!    Paul W. Eslinger : 28 Jul 2006 : Add flag for all realizations
!!                                     Reactivate reads on SODR, SOGW and SOSW
!!    Paul W. Eslinger :  6 Jun 2007 : Revise comments and add IMPLICIT NONE
!!
!!*************************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Files_Mod
      USE Esd_Mod
      USE Ecda_Mod
      USE Control_Mod
      USE Soil_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: IANA ! Analyte index
      INTEGER :: IERR             ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'PROCESS_SOIL' ! Name of this subroutine
!
      INTEGER :: TIME            ! Calendar year from file
      CHARACTER(LEN=6) :: LOC_ID ! Location ID from file
      CHARACTER(LEN=4) :: MED_ID ! Media ID from file
      INTEGER :: IREL            ! Realization looping index
      INTEGER :: ILOC            ! Location looping index
      INTEGER :: ITIM            ! Time looping index
      INTEGER :: IDX             ! ECDA concentration record index
!
      LOGICAL :: THERE           ! Logical variable for file inquire
!
      CHARACTER(LEN=10) :: SDATE ! System date in the form YYYYMMDD
      CHARACTER(LEN=10) :: STIME ! System time in the form HHMMSS.SSS
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
      MESSAG(1) = 'Entering subroutine for analyte "' // TRIM(ESD_ANA(IANA)%ID) // '"'
      CALL DATE_AND_TIME( SDATE, STIME )
      MESSAG(2) = 'On '//SDATE(5:6)//'/'//SDATE(7:8)//'/'//SDATE(1:4)//' at '//STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
      CALL PRTERR( IERR, CALLER, 2 )
!
! *** See if the concentration data file exists
!
      INQUIRE( FILE=FN_CON(IANA), EXIST=THERE )
      IF( .NOT.THERE ) THEN
        IERR = 1
        MESSAG(1) = 'The requested ECDA concentration file was not found'
        MESSAG(2) = 'Analyte ID: '//ESD_ANA(IANA)%ID
        MESSAG(3) = 'File: '//TRIM(FN_CON(IANA))
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Open the concentration data file
!
      CALL ECDA_OPEN( FN_CON(IANA), ICON, IERR )
      IF ( IERR .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the ECDA concentration file'
        MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
        MESSAG(3) = 'File: ' // TRIM(FN_CON(IANA))
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Looping order is locations, realizations, and then times
!     Carry information for dry, groundwater irrigation, and surface water irrigation
!
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
        IF( .NOT.ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) CYCLE
        CALL TELLTIME ( 'Processing concentrations for '//ESD_ANA(IANA)%ID//' at '//ESD_LOC(ILOC)%ID, 'SCREEN', .FALSE., IRPT )
!
! ***   Read air deposition, groundwater concentrations, surface water concentrations, soil (dry)
!       concentrations, soil GW irrigated concentrations, and soil SW irrigated concentrations
!       for all times and realizations at this location
!
        DO ITIM = 1, ESD_NUM_TIM
!
          IF( ESD_LOC(ILOC)%AIRD ) THEN ! Read the air deposition
!
! ***       Get the record number index
            CALL ECDA_RECNO_INDEX( ITIM, ILOC, IAIRD, IDX, IERR )
            IF( IERR.NE.0 .OR. IDX.LE.0 ) THEN
              IERR = 3
              MESSAG(1) = 'Error in lower level routine for ECDA_RECNO_INDEX'
              MESSAG(2) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Location ID is ' // ESD_LOC(ILOC)%ID
              MESSAG(4) = 'Time index is '
              WRITE(MESSAG(4)(16:),*) ITIM
              MESSAG(5) = 'Media index is '
              WRITE(MESSAG(5)(17:),*) IAIRD
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
! ***       Read the data
            CALL ECDA_READ( IDX, TIME, LOC_ID, MED_ID, CVEC, ECDA_NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 4
              MESSAG(1) = 'Error reading air deposition data'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Time = '
              WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
!
! ***       Store the data in the use array and check for invalid values
            DO IREL = 1, ECDA_NREAL
              IF( CVEC(IREL) .LT. 0.0 ) THEN
                IERR = 5
                MESSAG(1) = 'Error reading air deposition data - negative value'
                MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
                MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
                MESSAG(4) = 'Time = '
                WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
                CALL PRTERR( IERR, CALLER, 4 )
                MESSAG(5) = 'Value = '
                WRITE(MESSAG(5)(8:),*) CVEC(IREL)
                RETURN
              END IF
              AIRD(ITIM,IREL) = CVEC(IREL)
            END DO
!
          ELSE ! Set air deposition to zero if not computed
!
            DO IREL = 1, ECDA_NREAL
              AIRD(ITIM,IREL) = 0.0
            END DO
!
          END IF
!
          IF( ESD_LOC(ILOC)%GWAT .AND. COMPUTE_SOGW ) THEN ! Read the groundwater concentrations
!
! ***       Get the record number index
            CALL ECDA_RECNO_INDEX( ITIM, ILOC, IGWAT, IDX, IERR )
            IF( IERR.NE.0 .OR. IDX.LE.0 ) THEN
              IERR = 5
              MESSAG(1) = 'Error in lower level routine for ECDA_RECNO_INDEX'
              MESSAG(2) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Location ID is ' // ESD_LOC(ILOC)%ID
              MESSAG(4) = 'Time index is '
              WRITE(MESSAG(4)(16:),*) ITIM
              MESSAG(5) = 'Media index is '
              WRITE(MESSAG(5)(17:),*) IGWAT
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
! ***       Read the data
            CALL ECDA_READ( IDX, TIME, LOC_ID, MED_ID, CVEC, ECDA_NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 6
              MESSAG(1) = 'Error reading groundwater concentration data'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Time = '
              WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
!
! ***       Store the data in the use array and check for invalid values
            DO IREL = 1, ECDA_NREAL
              IF( CVEC(IREL) .LT. 0.0 ) THEN
                IERR = 5
                MESSAG(1) = 'Error reading groundwater data - negative value'
                MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
                MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
                MESSAG(4) = 'Time = '
                WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
                CALL PRTERR( IERR, CALLER, 4 )
                MESSAG(5) = 'Value = '
                WRITE(MESSAG(5)(8:),*) CVEC(IREL)
                RETURN
              END IF
              GWAT(ITIM,IREL) = CVEC(IREL)
            END DO
!
          ELSE ! Set groundwater concentrations to zero if not computed
!
            DO IREL = 1, ECDA_NREAL
              GWAT(ITIM,IREL) = 0.0
            END DO
!
          END IF
!
          IF( ESD_LOC(ILOC)%IX_SWAT.GT.0 .AND. COMPUTE_SOSW ) THEN ! Read the surface water concentrations
!
! ***       Get the record number index
            CALL ECDA_RECNO_INDEX( ITIM, ESD_LOC(ILOC)%IX_SWAT, ISWAT, IDX, IERR )
            IF( IERR.NE.0 .OR. IDX.LE.0 ) THEN
              IERR = 7
              MESSAG(1) = 'Error in lower level routine for ECDA_RECNO_INDEX'
              MESSAG(2) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Location ID is ' // ESD_LOC(ESD_LOC(ILOC)%IX_SWAT)%ID
              MESSAG(4) = 'Time index is '
              WRITE(MESSAG(4)(16:),*) ITIM
              MESSAG(5) = 'Media index is '
              WRITE(MESSAG(5)(17:),*) ISWAT
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
! ***       Read the data
            CALL ECDA_READ( IDX, TIME, LOC_ID, MED_ID, CVEC, ECDA_NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 8
              MESSAG(1) = 'Error reading surface water concentration data'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Time = '
              WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
!
! ***       Store the data in the use array and check for invalid values
            DO IREL = 1, ECDA_NREAL
              IF( CVEC(IREL) .LT. 0.0 ) THEN
                IERR = 5
                MESSAG(1) = 'Error reading surface water concentration data - negative value'
                MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
                MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
                MESSAG(4) = 'Time = '
                WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
                CALL PRTERR( IERR, CALLER, 4 )
                MESSAG(5) = 'Value = '
                WRITE(MESSAG(5)(8:),*) CVEC(IREL)
                RETURN
              END IF
              SWAT(ITIM,IREL) = CVEC(IREL)
            END DO
!
          ELSE ! Set surface concentrations to zero if not computed
!
            DO IREL = 1, ECDA_NREAL
              SWAT(ITIM,IREL) = 0.0
            END DO
!
          END IF
!
          IF( .NOT.USE_ALL_REL ) THEN ! Read SOIL data from file if some realizations are not computed
!                                       They will be unchanged when the revised data are written out
!
            IF( ESD_LOC(ILOC)%SODR ) THEN ! Read the soil (dry) concentrations
!
! ***         Get the record number index
              CALL ECDA_RECNO_INDEX( ITIM, ILOC, ISODR, IDX, IERR )
              IF( IERR.NE.0 .OR. IDX.LE.0 ) THEN
                IERR = 9
                MESSAG(1) = 'Error in lower level routine for ECDA_RECNO_INDEX'
                MESSAG(2) = 'Analyte =  '//ESD_ANA(IANA)%ID
                MESSAG(3) = 'Location ID is ' // ESD_LOC(ILOC)%ID
                MESSAG(4) = 'Time index is '
                WRITE(MESSAG(4)(16:),*) ITIM
                MESSAG(5) = 'Media index is '
                WRITE(MESSAG(5)(17:),*) ISODR
                CALL PRTERR( IERR, CALLER, 5 )
                RETURN
              END IF
!
! ***         Read the data
              CALL ECDA_READ( IDX, TIME, LOC_ID, MED_ID, CVEC, ECDA_NREAL, ICON, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 10
                MESSAG(1) = 'Error reading soil (dry) concentration data'
                MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
                MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
                MESSAG(4) = 'Time = '
                WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
                CALL PRTERR( IERR, CALLER, 4 )
                RETURN
              END IF
!
! ***         Store the data in the use array
              DO IREL = 1, ECDA_NREAL
                SODR(ITIM,IREL) = CVEC(IREL)
              END DO
!
            END IF

            IF( ESD_LOC(ILOC)%SOGW ) THEN ! Read the soil (GW irrigation) concentrations
!
! ***         Get the record number index
              CALL ECDA_RECNO_INDEX( ITIM, ILOC, ISOGW, IDX, IERR )
              IF( IERR.NE.0 .OR. IDX.LE.0 ) THEN
                IERR = 11
                MESSAG(1) = 'Error in lower level routine for ECDA_RECNO_INDEX'
                MESSAG(2) = 'Analyte =  '//ESD_ANA(IANA)%ID
                MESSAG(3) = 'Location ID is ' // ESD_LOC(ILOC)%ID
                MESSAG(4) = 'Time index is '
                WRITE(MESSAG(4)(16:),*) ITIM
                MESSAG(5) = 'Media index is '
                WRITE(MESSAG(5)(17:),*) ISOGW
                CALL PRTERR( IERR, CALLER, 5 )
                RETURN
              END IF
!
! ***         Read the data
              CALL ECDA_READ( IDX, TIME, LOC_ID, MED_ID, CVEC, ECDA_NREAL, ICON, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 12
                MESSAG(1) = 'Error reading soil (GW irrigation) concentration data'
                MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
                MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
                MESSAG(4) = 'Time = '
                WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
                CALL PRTERR( IERR, CALLER, 4 )
                RETURN
              END IF
!
! ***         Store the data in the use array
              DO IREL = 1, ECDA_NREAL
                SOGW(ITIM,IREL) = CVEC(IREL)
              END DO
!
            END IF

            IF( ESD_LOC(ILOC)%SOSW ) THEN ! Read the soil (SW irrigation) concentrations
!
! ***         Get the record number index
              CALL ECDA_RECNO_INDEX( ITIM, ILOC, ISOSW, IDX, IERR )
              IF( IERR.NE.0 .OR. IDX.LE.0 ) THEN
                IERR = 13
                MESSAG(1) = 'Error in lower level routine for ECDA_RECNO_INDEX'
                MESSAG(2) = 'Analyte =  '//ESD_ANA(IANA)%ID
                MESSAG(3) = 'Location ID is ' // ESD_LOC(ILOC)%ID
                MESSAG(4) = 'Time index is '
                WRITE(MESSAG(4)(16:),*) ITIM
                MESSAG(5) = 'Media index is '
                WRITE(MESSAG(5)(17:),*) ISOSW
                CALL PRTERR( IERR, CALLER, 5 )
                RETURN
              END IF
!
! ***         Read the data
              CALL ECDA_READ( IDX, TIME, LOC_ID, MED_ID, CVEC, ECDA_NREAL, ICON, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 14
                MESSAG(1) = 'Error reading soil (SW irrigation) concentration data'
                MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
                MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
                MESSAG(4) = 'Time = '
                WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
                CALL PRTERR( IERR, CALLER, 4 )
                RETURN
              END IF
!
! ***         Store the data in the use array
              DO IREL = 1, ECDA_NREAL
                SOSW(ITIM,IREL) = CVEC(IREL)
              END DO
!
            END IF
!
          END IF
!
        END DO
!
! ***   Optionally output the input media concentrations
        IF( BUG_CONC ) CALL PRINT_CONC( ILOC, IANA )
!
! ***   Compute the soil concentrations from air and water concentrations
        CALL DATE_AND_TIME( SDATE, STIME )
        IF( BUG_LOOP ) WRITE(*,*) ' COMPUTE_SOIL : Location '//TRIM(ESD_LOC(ILOC)%ID)//' : Analyte '//TRIM(ESD_ANA(IANA)%ID)//&
          ' : '//SDATE(5:6)//'/'//SDATE(7:8)//'/'//SDATE(1:4)//' at '//STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
        CALL COMPUTE_SOIL( ILOC, IANA, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!
! ***   Output the computed soil concentrations
!
        IF( BUG_CONC ) CALL PRINT_SOIL( ILOC, IANA )
!
! ***   Write soil (dry) concentrations, soil GW irrigated concentrations, and soil SW irrigated
!       concentrations to the ECDA file for all times and realizations at this location
!
        DO ITIM = 1, ESD_NUM_TIM
!
          IF( ESD_LOC(ILOC)%SODR .AND. COMPUTE_SODR ) THEN ! Write the soil (dry) concentrations
!
! ***       Get the record number index
            CALL ECDA_RECNO_INDEX( ITIM, ILOC, ISODR, IDX, IERR )
            IF( IERR.NE.0 .OR. IDX.LE.0 ) THEN
              IERR = 15
              MESSAG(1) = 'Error in lower level routine for ECDA_RECNO_INDEX'
              MESSAG(2) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Location ID is ' // ESD_LOC(ILOC)%ID
              MESSAG(4) = 'Time index is '
              WRITE(MESSAG(4)(16:),*) ITIM
              MESSAG(5) = 'Media index is '
              WRITE(MESSAG(5)(17:),*) ISODR
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
! ***       Pull out the data to output
            DO IREL = 1, ECDA_NREAL
              CVEC(IREL) = SODR(ITIM,IREL)
            END DO
!
! ***       Write out the data
            TIME = ESD_TIM(ITIM)%TIME
            LOC_ID = ESD_LOC(ILOC)%ID
            MED_ID = 'SODR'
            CALL ECDA_WRITE( IDX, TIME, LOC_ID, MED_ID, CVEC, ECDA_NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 16
              MESSAG(1) = 'Error writing soil (dry) concentration data'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Time = '
              WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
!
          END IF
!
          IF( ESD_LOC(ILOC)%SOGW .AND. COMPUTE_SOGW ) THEN ! Write the soil (GW irrigation) concentrations
!
! ***       Get the record number index
            CALL ECDA_RECNO_INDEX( ITIM, ILOC, ISOGW, IDX, IERR )
            IF( IERR.NE.0 .OR. IDX.LE.0 ) THEN
              IERR = 17
              MESSAG(1) = 'Error in lower level routine for ECDA_RECNO_INDEX'
              MESSAG(2) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Location ID is ' // ESD_LOC(ILOC)%ID
              MESSAG(4) = 'Time index is '
              WRITE(MESSAG(4)(16:),*) ITIM
              MESSAG(5) = 'Media index is '
              WRITE(MESSAG(5)(17:),*) ISOGW
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
! ***       Pull out the data to output
            DO IREL = 1, ECDA_NREAL
              CVEC(IREL) = SOGW(ITIM,IREL)
            END DO
!
! ***       Write out the data
            TIME = ESD_TIM(ITIM)%TIME
            LOC_ID = ESD_LOC(ILOC)%ID
            MED_ID = 'SOGW'
            CALL ECDA_WRITE( IDX, TIME, LOC_ID, MED_ID, CVEC, ECDA_NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 18
              MESSAG(1) = 'Error WRITING soil (GW irrigation) concentration data'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Time = '
              WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
!
          END IF
!
          IF( ESD_LOC(ILOC)%SOSW .AND. COMPUTE_SOSW ) THEN ! Write the soil (SW irrigation) concentrations
!
! ***       Get the record number index
            CALL ECDA_RECNO_INDEX( ITIM, ILOC, ISOSW, IDX, IERR )
            IF( IERR.NE.0 .OR. IDX.LE.0 ) THEN
              IERR = 19
              MESSAG(1) = 'Error in lower level routine for ECDA_RECNO_INDEX'
              MESSAG(2) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(3) = 'Location ID is ' // ESD_LOC(ILOC)%ID
              MESSAG(4) = 'Time index is '
              WRITE(MESSAG(4)(16:),*) ITIM
              MESSAG(5) = 'Media index is '
              WRITE(MESSAG(5)(17:),*) ISOSW
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
! ***       Pull out the data to output
            DO IREL = 1, ECDA_NREAL
              CVEC(IREL) = SOSW(ITIM,IREL)
            END DO
!
! ***       Write out the data
            TIME = ESD_TIM(ITIM)%TIME
            LOC_ID = ESD_LOC(ILOC)%ID
            MED_ID = 'SOSW'
            CALL ECDA_WRITE( IDX, TIME, LOC_ID, MED_ID, CVEC, ECDA_NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 20
              MESSAG(1) = 'Error writing soil (SW irrigation) concentration data'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              MESSAG(4) = 'Time = '
              WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
!
          END IF
!
        END DO
!
      END DO
!
! *** Close the file
!
      CLOSE( ICON )
!
      RETURN
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
!!    Paul W. Eslinger :  6 Jun 2007 : Original source
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
!!    Paul W. Eslinger :  6 Jun 2007 : Original code
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
!---- First executable code --------------------------------------------
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

