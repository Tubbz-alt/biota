!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
!
MODULE Control_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This include deck contains control information for the entire problem as well as
!    providing allocation of work arrays for a variety of purposes.
!
!  History:
!
!    Paul W. Eslinger : 27 Oct 2004 : Original version
!    Carmen Arimescu  :  3 Mar 2006 : Revised
!    Carmen Arimescu  :  7 Apr 2006 : Remove OS_SLASH
!
      CHARACTER(LEN=72) :: PTITLE  ! Title for this program run (limited length for Tecplot passing)
!
      CHARACTER(LEN=10) :: SDATE ! Start date of processing
      CHARACTER(LEN=10) :: STIME ! Start time of processing
!
      LOGICAL :: REPORT  ! Flag whether the report file is open
      LOGICAL :: VERBOSE ! Flag for verbose outputs to the screen
!
      LOGICAL :: CAT_CULTURE ! Process CULTURE data
      LOGICAL :: CAT_ECEM    ! Process ECEM data
      LOGICAL :: CAT_ECDA    ! Process ECDA data
      LOGICAL :: CAT_FCDA    ! Process FCDA data
      LOGICAL :: CAT_HUMAN   ! Process HUMAN data
!
      LOGICAL :: LOC_UPLAND   ! Process UPLAND locations
      LOGICAL :: LOC_RIPARIAN ! Process RIPARIAN locations
      LOGICAL :: LOC_AQUATIC  ! Process AQUATIC locations
!
      LOGICAL :: USE_ALL_TIME ! Process all times slices
!
      INTEGER :: CUL_NUM_ANA ! Number of analytes in this run
      INTEGER :: CUL_NUM_TIM ! Number of times in this run
      INTEGER :: CUL_NUM_LOC ! Number of locations in this run 
!
      CHARACTER(LEN=6) :: MAP_USE  ! MAP to process
      CHARACTER(LEN=7) :: ANA_USE  ! Analyte to process
      CHARACTER(LEN=6) :: MED_USE  ! Media to process
      CHARACTER(LEN=8) :: ATY_USE  ! Analyte type to process
      CHARACTER(LEN=6) :: STY_USE  ! Soil type to process
      CHARACTER(LEN=7) :: SLT_USE  ! Solution type to process
      CHARACTER(LEN=6) :: SPC_USE  ! Species to process
      INTEGER          :: REL_USE  ! Realization number to process
!
      INTEGER :: NUM_CON ! Number of nodes in the connectivity file
      INTEGER :: NUM_COR ! Number of locations in the coordinate file
      INTEGER :: NREAL   ! Number of realizations in the details (or other) file
!
      REAL, ALLOCATABLE :: VBR(:) ! Data vector
!
      REAL :: CONVERT_UNITS               ! Factor for conversion of data units
      CHARACTER(LEN=10) :: UNITS_INPUT    ! Input data units
      CHARACTER(LEN=10) :: UNITS_OUTPUT   ! Output data units
      CHARACTER(LEN=72) :: LEGEND_TITLE   ! TITLE1 for the TECPLOT animation
      CHARACTER(LEN=13) :: LEGEND_CONTOUR ! This can be: Dose Levels, Risk, Concentration....
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Control_Mod

MODULE ECDA_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Purpose:
!
!   This module contains variables related to reading and writing Environmental Concentration Data
!   Accumulator (ECDA) files.  The media descriptions, IDs, and data file indices are all defined in
!   this module at compile time.
!
! History:
!
!   Kelly Lessor     : 20 Mar 2000 : Version 1.0
!   Paul W. Eslinger :  9 Oct 2002 : SAC Rev. 1 Version (together with Terri Miley)
!   Carmen Arimescu  : 22 Dec 2005 : ECDA_TIMES_TYPE, ECDA_LOC_TYPE
!   Paul W. Eslinger : 22 Jun 2012 : Fix typo on error trapping
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!===> Common data descriptions for the ECDA map and concentration files and the ECDA header file
!
      INTEGER, PARAMETER :: ECDA_NMED = 11 ! Number of media for which the calculations are to be performed
      INTEGER, PARAMETER :: ECDA_NHEAD=ECDA_NMED+1  ! Number of header records in an ECDA file
!
!===> Header information in the binary concentration file
!
      CHARACTER(LEN=6) :: ECDA_ANALYTE                      ! Analyte ID:  First header line
      CHARACTER(LEN=16), DIMENSION(ECDA_NMED) :: ECDA_UNITS ! Media units: Header lines 2 through ECDA_NMED+1
!
!===> Information in the ASCII record map file
!
      INTEGER :: ECDA_BLOCK  ! Number of records with data in a time block
      INTEGER :: ECDA_RECLEN ! Record length of records in the data file
      INTEGER :: ECDA_NREAL  ! Number of realizations in the data file
!
!     dimension CONC of size ECDA_NREAL
      REAL, ALLOCATABLE :: CONC(:)
!
      CHARACTER(LEN=200) :: ECDA_PTITLE ! Problem title line from generating program
      CHARACTER(LEN= 10) :: ECDA_PRGNAM ! Program name of generating program
      CHARACTER(LEN=  8) :: ECDA_PRGVER ! Program version number of generating program
      CHARACTER(LEN= 12) :: ECDA_PRGDAT ! Program date of generating program
      CHARACTER(LEN= 16) :: ECDA_USRNAM ! User name from generating program
      CHARACTER(LEN= 14) :: ECDA_CRUNID ! Run identification number from generating program
!
      INTEGER, ALLOCATABLE :: ECDA_LOC_MED(:,:)        ! (ECDA_NLOCS,ECDA_NMED) Record numbers for all
!
!     Media indices - This order must match with IDs and descriptions defined below
      INTEGER, PARAMETER :: IGWAT =  1 ! Index to groundwater media (computed by CFEST)
      INTEGER, PARAMETER :: ISEEP =  2 ! Index to seep water media (computed by RIPSAC)
      INTEGER, PARAMETER :: ISWAT =  3 ! Index to surface water media (computed by MASS2)
      INTEGER, PARAMETER :: IPWAT =  4 ! Index to pore water media (computed by MASS2)
      INTEGER, PARAMETER :: ISEDI =  5 ! Index to river bottom sediment media (computed by MASS2)
      INTEGER, PARAMETER :: ISORP =  6 ! Index to riparian zone soil media (computed by RIPSAC)
      INTEGER, PARAMETER :: ISODR =  7 ! Index to non-irrigated upland soil media (computed by SOIL)
      INTEGER, PARAMETER :: ISOGW =  8 ! Index to groundwater irrigated upland soil media (computed by SOIL)
      INTEGER, PARAMETER :: ISOSW =  9 ! Index to surface water irrigated upland soil media (computed by SOIL)
      INTEGER, PARAMETER :: IAIRC = 10 ! Index to air concentration media (computed by RATCHET)
      INTEGER, PARAMETER :: IAIRD = 11 ! Index to air deposition media (computed by RATCHET)
!
!     Media IDs - Order must match the media indices defined above
      CHARACTER(LEN=4), DIMENSION(ECDA_NMED), PARAMETER :: ECDA_ID_MED &
        = (/'GWAT','SEEP','SWAT','PWAT','SEDI','SORP','SODR','SOGW','SOSW','AIRC','AIRD'/)
!
      INTEGER :: MED_IDX ! Media index for use in accessing data
!
      TYPE ECDA_TIMES_TYPE
        INTEGER :: TIME ! Times where data are stored
        LOGICAL :: COMP ! Flag whether this time is to be used
      END TYPE ECDA_TIMES_TYPE
      TYPE (ECDA_TIMES_TYPE), ALLOCATABLE :: ECDA_TIMES(:) ! The ECDA time variable
      INTEGER :: ECDA_NTIMES     !Number of times at which the calculations are to be performed
!
!     Type definition for location data
!
      TYPE ECDA_LOC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for a location
        CHARACTER(LEN= 8) :: TYPE ! Location type (AQUATIC, RIPARIAN, or UPLAND)
        LOGICAL :: COMP           ! Flag whether this location is to be used
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a location
        REAL :: EASTING           ! Easting coordinate in state plane projection
        REAL :: NORTHING          ! Northing coordinate in state plane projection
        REAL :: MILE              ! Columbia river mile
      END TYPE ECDA_LOC_TYPE
      TYPE (ECDA_LOC_TYPE), ALLOCATABLE :: ECDA_LOC(:) ! The ECDA location variable
      INTEGER :: ECDA_NLOCS  ! Number of locations at which the calculations are to be performed
!
!     Type definition for analyte data
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE ECDA_Mod

MODULE Debug_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to initiating debug
!    outputs to the report file for a variety of calculations.
!
!   History:
!
!     Carmen Arimescu : 27 Oct 2004 : Version 1.0
!
      LOGICAL :: BGHUMAN  ! Debug flag for human data
      LOGICAL :: BGCULTURE! Debug flag for culture data
      LOGICAL :: BGECEM   ! Debug flag for ecological data
      LOGICAL :: BGECDA   ! Debug flag for concentration data
      LOGICAL :: BGFCDA   ! Debug flag for food data
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Debug_Mod

MODULE FCDA_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Purpose:
!
!   This module contains variables related to reading and writing FOOD Concentration Data (FCDA) files.
!
! History:
!
!    Paul W. Eslinger :  5 Mar 2003 : SAC Rev. 1
!    Carmen Arimescu  :  9 Jan 2005 :
!    Paul W. Eslinger :  4 Jun 2007 : Update comments
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!===> Common data descriptions for the FCDA map and concentration files and the FCDA header file
!
      INTEGER :: FCDA_NHEAD ! Number of header records in an FCDA binary file
!
!===> Header information in the ASCII record map file
!
      INTEGER :: FCDA_BLOCK_AQ ! Number of data records in a time block - aquatic species
      INTEGER :: FCDA_BLOCK_RP ! Number of data records in a time block - riparian species
      INTEGER :: FCDA_BLOCK_UP ! Number of data records in a time block - upland species
      INTEGER :: FCDA_RECLEN   ! Record length of records in the data file
      INTEGER :: FCDA_NREAL    ! Number of realizations in the map data file
      REAL, ALLOCATABLE :: CONC(:)
!
      CHARACTER(LEN=200) :: FCDA_PTITLE ! Problem title line from generating program
      CHARACTER(LEN= 10) :: FCDA_PRGNAM ! Program name of generating program
      CHARACTER(LEN=  8) :: FCDA_PRGVER ! Program version number of generating program
      CHARACTER(LEN= 12) :: FCDA_PRGDAT ! Program date of generating program
      CHARACTER(LEN= 16) :: FCDA_USRNAM ! User name from generating program
      CHARACTER(LEN= 14) :: FCDA_CRUNID ! Run identification number from generating program
!
      INTEGER, ALLOCATABLE :: FCDA_MAP_AQ(:)   ! Indices for food calculations by ESD location and aquatic species
      INTEGER, ALLOCATABLE :: FCDA_MAP_RP(:)   ! Indices for food calculations by ESD location and riparian species
      INTEGER, ALLOCATABLE :: FCDA_MAP_UP(:,:) ! Indices for food calculations by ESD location and soil type
!                                                for upland species
      INTEGER, PARAMETER :: ESD_NUM_SOI = 3    ! Number of upland soil types allowed
!
      INTEGER, PARAMETER :: IDX_SODR = 1 ! Index for upland food map for dry-land soil species
      INTEGER, PARAMETER :: IDX_SOGW = 2 ! Index for upland food map for groundwater irrigated soil species
      INTEGER, PARAMETER :: IDX_SOSW = 3 ! Index for upland food map for surfacewater irrigated soil species
!
      INTEGER :: STY_IDX ! SOIL index for use in accessing data  
      INTEGER :: SPC_IDX ! Species index for use in processing data 
!
!     Type definition for FCDA (and ECEM) time data
      TYPE FCDA_TIM_TYPE
        INTEGER :: TIME     ! Times where data are stored
        LOGICAL :: COMP     ! Flag whether time is used in the FCDA
        LOGICAL :: TEC_COMP ! Flag whether time is to be used in animation
      END TYPE FCDA_TIM_TYPE
      TYPE (FCDA_TIM_TYPE), ALLOCATABLE :: FCDA_TIM(:) ! The FCDA time variable
      INTEGER :: FCDA_NTIMES
!
!     Type definition for FCDA (and ECEM) location data
      TYPE FCDA_LOC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for a location
        CHARACTER(LEN= 8) :: TYPE ! Location type (AQUATIC, RIPARIAN, or UPLAND)
        LOGICAL :: COMP           ! Flag whether this location is to be used
        LOGICAL :: TEC_COMP       ! Flag whether this location is to be used (coordinates)
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a location
        REAL :: EASTING           ! Easting coordinate in state plane projection
        REAL :: NORTHING          ! Northing coordinate in state plane projection
        REAL :: MILE              ! Columbia river mile
      END TYPE FCDA_LOC_TYPE
      TYPE (FCDA_LOC_TYPE), ALLOCATABLE :: FCDA_LOC(:) ! The FCDA location variable
      INTEGER :: FCDA_NLOCS
!
!     Type definition for FCDA (and ECEM) species data
      TYPE FCDA_SPC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Species ID
        CHARACTER(LEN=48) :: NAME ! Species name
        LOGICAL :: COMP           ! Flag whether a species results are computed as a food
      END TYPE FCDA_SPC_TYPE
      TYPE(FCDA_SPC_TYPE), ALLOCATABLE :: FCDA_SPC(:) ! Variable structure for species information
      INTEGER :: FCDA_NSPC
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE FCDA_Mod

MODULE Files_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This include deck contains file specific variable information.
!
!  History:
!
!    Carmen Arimescu  : 10 Nov 2004
!    Carmen Arimescu  : 19 Apr 2006 : Add Map file for the CULTURE code
!
      INTEGER, PARAMETER :: MAXFN=200  ! Length of file names
!
      INTEGER :: IKEY                  ! Unit number for the keyword file
      CHARACTER(LEN=MAXFN) :: FNKEY    ! Name of the input keyword file
!
      INTEGER :: ICON                  ! Unit number for the concentration file
      CHARACTER(LEN=MAXFN) :: FNCON    ! Name of the input concentration file
!
      INTEGER :: IFOD                  ! Unit number for the food concentration files
      CHARACTER(LEN=MAXFN) :: FNFOD    ! Names of food concentration files
!
      CHARACTER(LEN=MAXFN) :: FOODPATH ! Path for location of food files
      CHARACTER(LEN=MAXFN) :: FOODMAP  ! Name of the food map file
!      
      INTEGER :: ICOR                  ! Unit number for the location coordinate file
      CHARACTER(LEN=MAXFN) ::  FNCOR   ! Name of the input location coordinate file
!
      INTEGER :: ICOV                  ! Unit number for the connectivity file
      CHARACTER(LEN=MAXFN) :: FNCOV    ! Name of the input connectivity file
!
      INTEGER :: IDET                  ! Unit number for the input detailed risks file
      CHARACTER(LEN=MAXFN) :: FNDET    ! Name of the input detailed risks file
!
      INTEGER :: IHED                  ! Unit number for the input header file
      CHARACTER(LEN=MAXFN) :: FNHED    ! Name of the input header file
!
      INTEGER :: IRPT                  ! Unit number for the report file
      CHARACTER(LEN=MAXFN) :: FNRPT    ! Name of the output report file
!
      INTEGER :: IRES                  ! Unit number for the tecplot file
      CHARACTER(LEN=MAXFN) :: FNRES    ! Name of the output tecplot file
!
      INTEGER :: IMAP                  ! Unit number for the concentration record number map file
      CHARACTER(LEN=MAXFN) :: FNMAP    ! Name of the concentration record number map file
!
      INTEGER :: ICMAP                 ! Unit number for the output map file for the CULTURE code
      CHARACTER(LEN=MAXFN) :: FNCMAP   ! Name of the output map file for the CULTURE code
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Files_Mod

MODULE CULTURE_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!     This module contains informations obtained from the CULTURE detail file.
!
!  History:
!     Carmen Arimescu : 19 May 2006
!
!     Type definition for time data
!
      TYPE CULTURE_TIM_TYPE
        INTEGER :: TIME ! Times where data are stored
        LOGICAL :: COMP ! Flag whether this time is to be used
      END TYPE CULTURE_TIM_TYPE
      TYPE (CULTURE_TIM_TYPE), ALLOCATABLE :: CULTURE_TIM(:) ! The culture time variable
      INTEGER :: CULTURE_NUM_TIM     ! Number of times used in the CULTURE code
!
!     Type definition for location data
!
      TYPE CULTURE_LOC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for a location
        CHARACTER(LEN= 8) :: TYPE ! Location type (AQUATIC, RIPARIAN, or UPLAND)
        LOGICAL :: COMP           ! Flag whether this location is to be used
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a location
        REAL :: EASTING           ! Easting coordinate in state plane projection
        REAL :: NORTHING          ! Northing coordinate in state plane projection
        REAL :: MILE              ! Columbia river mile
      END TYPE CULTURE_LOC_TYPE
      TYPE (CULTURE_LOC_TYPE), ALLOCATABLE :: CULTURE_LOC(:) ! The culture location variable
      INTEGER :: CULTURE_NUM_LOC     ! Number of locations used in the CULTURE code
!
      TYPE CULTURE_MAP_TYPE
        CHARACTER(LEN=6) :: ID     ! MAP identification string
        CHARACTER(LEN=72) :: NAME ! Title line for this map
        LOGICAL, DIMENSION(6) :: MEDIUM ! Flags for including media
        LOGICAL, POINTER :: MAP_ANA(:)    ! Analytes that will be included in the calculation
      END TYPE CULTURE_MAP_TYPE
      TYPE (CULTURE_MAP_TYPE), ALLOCATABLE :: CULTURE_MAP(:)  !MAPS
!
      INTEGER :: CULTURE_NUM_MAPS   ! Number of Maps created by CULTURE code
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE CULTURE_Mod

MODULE HUMAN_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!     This module contains informations obtained from the Human detail file.
!
!  History:
!     Carmen Arimescu : 22 Nov 2004
!
!     Type definition for time data
!
      TYPE HUMAN_TIM_TYPE
        INTEGER :: TIME ! Times where data are stored
        LOGICAL :: COMP ! Flag whether this time is to be used
      END TYPE HUMAN_TIM_TYPE
      TYPE (HUMAN_TIM_TYPE), ALLOCATABLE :: HUMAN_TIM(:) ! The human time variable
      INTEGER :: HUMAN_NUM_TIM     ! Number of human times
!
!     Type definition for location data
!
      TYPE HUMAN_LOC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for a location
        CHARACTER(LEN= 8) :: TYPE ! Location type (AQUATIC, RIPARIAN, or UPLAND)
        LOGICAL :: COMP           ! Flag whether this location is to be used
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a location
        REAL :: EASTING           ! Easting coordinate in state plane projection
        REAL :: NORTHING          ! Northing coordinate in state plane projection
        REAL :: MILE              ! Columbia river mile
      END TYPE HUMAN_LOC_TYPE
      TYPE (HUMAN_LOC_TYPE), ALLOCATABLE :: HUMAN_LOC(:) ! The human location variable
      INTEGER :: HUMAN_NUM_LOC     ! Number of human locations
!
!     Type definition for analyte data
!
      TYPE HUMAN_ANA_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for an analyte
        CHARACTER(LEN=72) :: NAME ! Descriptive name for an analyte
        CHARACTER(LEN= 2) :: TYPE ! Analyte type
!                                   'RAD' = Radioactive  'CON' = Concentration
!                                   'CAR' = Carcinogen   'HAZ' = Harzardous material
        LOGICAL :: COMP           ! Flag whether this analyte is to be used
      END TYPE HUMAN_ANA_TYPE
      TYPE (HUMAN_ANA_TYPE), ALLOCATABLE :: HUMAN_ANA(:) ! The human analyte variable
      INTEGER :: HUMAN_NUM_ANA ! Number of analytes
!
!     Type definition for solution data
!
      TYPE HUMAN_SLN_TYPE
        CHARACTER(LEN= 7) :: ID   ! Solution type Identification
        LOGICAL :: COMP           ! Flag whether this analyte is to be used
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a solution
      END TYPE HUMAN_SLN_TYPE
      TYPE (HUMAN_SLN_TYPE), ALLOCATABLE :: HUMAN_SLN(:) ! The HUMAN analyte variable
      INTEGER :: HUMAN_NUM_SLN ! Number of solution
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE HUMAN_Mod

MODULE ECEM_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!     This module contains informations obtained from the ECEM detail file.
!
!  History:
!     Carmen Arimescu : 7 Dec 2004
!
!     Type definition for time data
!
      TYPE ECEM_TIM_TYPE
        INTEGER :: TIME ! Times where data are stored
        LOGICAL :: COMP ! Flag whether this time is to be used
      END TYPE ECEM_TIM_TYPE
      TYPE (ECEM_TIM_TYPE), ALLOCATABLE :: ECEM_TIM(:) ! The ECEM time variable
      INTEGER :: ECEM_NUM_TIM     ! Number of ECEM times
!
!     Type definition for location data
!
      TYPE ECEM_LOC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for a location
        CHARACTER(LEN= 8) :: TYPE ! Location type (AQUATIC, RIPARIAN, or UPLAND)
        LOGICAL :: COMP           ! Flag whether this location is to be used
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a location
        REAL :: EASTING           ! Easting coordinate in state plane projection
        REAL :: NORTHING          ! Northing coordinate in state plane projection
        REAL :: MILE              ! Columbia river mile
      END TYPE ECEM_LOC_TYPE
      TYPE (ECEM_LOC_TYPE), ALLOCATABLE :: ECEM_LOC(:) ! The ECEM location variable
      INTEGER :: ECEM_NUM_LOC     ! Number of ECEM locations
!
!     Type definition for analyte data
!
      TYPE ECEM_ANA_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for an analyte
        CHARACTER(LEN=72) :: NAME ! Descriptive name for an analyte
        CHARACTER(LEN= 2) :: TYPE ! Analyte type
!
        LOGICAL :: COMP           ! Flag whether this analyte is to be used
      END TYPE ECEM_ANA_TYPE
      TYPE (ECEM_ANA_TYPE), ALLOCATABLE :: ECEM_ANA(:) ! The ECEM analyte variable
      INTEGER :: ECEM_NUM_ANA ! Number of analytes
!
!     Type definition for ECEM species data
!
      TYPE ECEM_SPC_TYPE
        CHARACTER(LEN= 6) :: ID   ! Species ID
        CHARACTER(LEN=48) :: NAME ! Species name
        CHARACTER(LEN= 2) :: TYPE ! Analyte type
        LOGICAL :: COMP      ! Flag whether a species results are computed
!
      END TYPE ECEM_SPC_TYPE
      TYPE(ECEM_SPC_TYPE), ALLOCATABLE :: ECEM_SPC(:) ! Variable structure for species information
      INTEGER :: ECEM_NUM_SPC ! Number of species in this run of ECEM
!
!     Type definition for solution data
!
      TYPE ECEM_SLN_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for an analyte
!
        LOGICAL :: COMP           ! Flag whether this analyte is to be used
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a solution
      END TYPE ECEM_SLN_TYPE
      TYPE (ECEM_SLN_TYPE), ALLOCATABLE :: ECEM_SLN(:) ! The ECEM analyte variable
      INTEGER :: ECEM_NUM_SLN ! Number of solution
!
!     Type definition for MEDIA
!
      TYPE ECEM_MEDIA_TYPE
        CHARACTER(LEN= 6) :: ID   ! Identification number for a MEDIA
!
        LOGICAL :: COMP           ! Flag whether this MEDIA is to be used
        CHARACTER(LEN=72) :: NAME ! Descriptive name for a MEDIA
      END TYPE ECEM_MEDIA_TYPE
      TYPE (ECEM_MEDIA_TYPE), ALLOCATABLE :: ECEM_MEDIA(:) ! The ECEM analyte variable
      INTEGER :: ECEM_NUM_MEDIA ! Number of solution
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE ECEM_Mod

 MODULE Iden_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains run and user identification information
!
!  History:
!
!    Paul W. Eslinger : 27 Oct 2004 : Original version
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
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Iden_Mod

      PROGRAM ANIMATE
!!**********************************************************************************************
!!
!!                             ANIMATE - Animation Preprocessor
!!                      Toolkit for Integrated Impacts Assessments (TIIA)
!!            Battelle, Pacific Northwest National Laboratories, Richland, Washington
!!
!!**********************************************************************************************
!!
!!  Purpose:
!!    ANIMATE is a utility code that extracts data and sets up input files for TecPlot
!!    for the purpose of making time-series animations.  Data are handled from five
!!    separate codes.  The codes (data types) are:
!!       CULTURE - Any map data (SAC rev. 1 code)
!!       ECDA    - Media concentrations in the binary ECDA files
!!       ECEM    - Any data written to the "DETAILS" file
!!       FCDA    - Food concentrations in the binary FCDA files
!!       HUMAN   - Any data written to the "DETAILS" file
!!
!!  History:
!!    Carmen Arimescu  : 21 Mar 2005 : Original source
!!    Carmen Arimescu  : 26 Apr 2006 : SCR-1120 - Added animation for Culture code maps
!!    Paul W. Eslinger :  4 Jun 2007 : Move REPORT keyword to another routine
!!                                     Revise for TIIA and copyright
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!! Reference:
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!**********************************************************************************************
!
! *** Include global parameters, variables, and arrays
!
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Iden_Mod
      USE Ecem_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** Declare local variables
      CHARACTER(LEN=7) :: CALLER = 'ANIMATE' ! Name of this program
      INTEGER :: IERR                        ! Integer error flag
!
!---- Executable code --------------------------------------------------------
!
      IERR = 0
!
!-----------------------------------------------------------------------------
! *** Program, run identification, and initialization
!-----------------------------------------------------------------------------
!
      CALL IDEN_SET( )
      CALL INIT( )
!
! *** Open the input file
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) GOTO 999
!
! *** Define the report file
      CALL TELLTIME( 'Extracting the report file name', 'SCREEN', .FALSE. )
      CALL READ_KEYS_REPORT( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the report file'
        WRITE(*,*) 'Stop in '//CALLER
        STOP
      END IF
!
! *** Print the opening banner page
      CALL BANNER_1(  )
!
! *** Read the first pass of the keywords to get the report file
!     open, user definition, and array dimension information collected
      CALL TELLTIME( 'Reading keywords', 'SCREEN', .FALSE. )
      CALL READ_KEY_1( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
      CALL BANNER_2( )
!
! *** Read the control information and process data for the specific category
!
!----------------------------------------------------------------------------------- 
!     Processing of CULTURE data
!-----------------------------------------------------------------------------------
!
      IF( CAT_CULTURE ) THEN
!
!       Do error checking on the initial keyword set
        IF( VERBOSE ) CALL TELLTIME( 'Check initial keywords', 'SCREEN', .FALSE. )
        CALL CHECK_INIT_CULTURE( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read all keywords in Culture except times (really need file names)
        IF( VERBOSE ) CALL TELLTIME( 'Reading Culture keywords - Pass #2', 'SCREEN', .FALSE. )
        CALL READ_KEY_CULTURE_1( IERR ) ! Everything CULTURE except times
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read SacView header file (Get lists of data)
        IF( VERBOSE ) CALL TELLTIME( 'Reading SacView header file', 'SCREEN', .FALSE. )
        CALL READ_CULTURE_SV_HEADER( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read and save TecPlot coordinate file (ID, Easting, Northing)
!       Check against what's in the header file list
!       Set compute flags in header file list
        IF( VERBOSE ) CALL TELLTIME( 'Reading TecPlot coordinate file', 'SCREEN', .FALSE. )
        IF( LOC_UPLAND ) CALL READ_UPLAND_COORDS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
        IF( LOC_RIPARIAN .OR. LOC_AQUATIC ) CALL READ_RIP_AQ_COORDS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read keywords in CULTURE for times
        IF( VERBOSE ) CALL TELLTIME( 'Reading CULTURE keywords - Pass #3', 'SCREEN', .FALSE. )
        CALL READ_KEY_CULTURE_2( IERR ) ! CULTURE times
        IF( IERR .NE. 0 ) GO TO 999
!
!       Echo some information to the report file
        CALL ECHO( )
!
!       Check problem definition
        IF( VERBOSE ) CALL TELLTIME( 'Checking CULTURE keywords', 'SCREEN', .FALSE. )
        CALL CHECK_CULTURE( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Open and read and process data
        IF( VERBOSE ) CALL TELLTIME( 'Process CULTURE MAP data file', 'SCREEN', .FALSE. )
        CALL PROCESS_CULTURE( IERR ) ! Open and read and process data
        IF( IERR .NE. 0 ) GO TO 999
!
      END IF
!
!------------------------------------------------------------------------------------
!     Processing of HUMAN data
!------------------------------------------------------------------------------------
!
      IF( CAT_HUMAN ) THEN
!
!       Do error checking on the initial keyword set
        IF( VERBOSE ) CALL TELLTIME( 'Check initial keywords', 'SCREEN', .FALSE. )
        CALL CHECK_INIT( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read all keywords in HUMAN except times (really need file names)
        IF( VERBOSE ) CALL TELLTIME( 'Reading HUMAN keywords - Pass #2', 'SCREEN', .FALSE. )
        CALL READ_KEY_HUMAN_1( IERR ) ! Everything HUMAN except times
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read SacView header file (Get lists of data)
        IF( VERBOSE ) CALL TELLTIME( 'Reading SacView header file', 'SCREEN', .FALSE. )
        CALL READ_HUMAN_SV_HEADER( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read and save TecPlot coordinate file (ID, Easting, Northing)
!       Check against what's in the header file list
!       Set compute flags in header file list
        IF( VERBOSE ) CALL TELLTIME( 'Reading TecPlot coordinate file', 'SCREEN', .FALSE. )
        IF( LOC_UPLAND ) CALL READ_UPLAND_COORDS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
        IF( LOC_RIPARIAN .OR. LOC_AQUATIC ) CALL READ_RIP_AQ_COORDS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read keywords in HUMAN for times
        IF( VERBOSE ) CALL TELLTIME( 'Reading HUMAN keywords - Pass #3', 'SCREEN', .FALSE. )
        CALL READ_KEY_HUMAN_2( IERR ) ! HUMAN times
        IF( IERR .NE. 0 ) GO TO 999
!
!       Echo some information to the report file
        CALL ECHO( )
!
!       Check problem definition
        IF( VERBOSE ) CALL TELLTIME( 'Checking HUMAN keywords', 'SCREEN', .FALSE. )
        CALL CHECK_HUMAN( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Open and read and process data
        IF( VERBOSE ) CALL TELLTIME( 'Process HUMAN detailed data file', 'SCREEN', .FALSE. )
        CALL PROCESS_HUMAN( IERR ) ! Open and read and process data
        IF( IERR .NE. 0 ) GO TO 999
!
      END IF
!
!-----------------------------------------------------------------------------------
!     Processing of ECDA data
!-----------------------------------------------------------------------------------
!
      IF( CAT_ECDA ) THEN
!
!       Do error checking on the initial keyword set
        IF( VERBOSE ) CALL TELLTIME( 'Check initial ECDA keywords', 'SCREEN', .FALSE. )
        CALL CHECK_INIT_ECDA( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read all keywords in HUMAN except times (really need file names)
        IF( VERBOSE ) CALL TELLTIME( 'Reading ECDA keywords - Pass #2', 'SCREEN', .FALSE. )
        CALL READ_KEY_ECDA_1( IERR ) ! Everything ECDA except times
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read THE MAP file (Get lists of data)
        IF( VERBOSE ) CALL TELLTIME( 'Reading ECDA map file', 'SCREEN', .FALSE. )
        CALL ECDA_MAPREAD( FNMAP, IMAP, IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read and save TecPlot coordinate file (ID, Easting, Northing)
!       Check against what's in the header file list
!       Set compute flags in header file list
        IF( VERBOSE ) CALL TELLTIME( 'Reading TecPlot coordinate file', 'SCREEN', .FALSE. )
        IF( LOC_UPLAND ) CALL READ_UPLAND_COORDS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
        IF( LOC_RIPARIAN .OR. LOC_AQUATIC ) CALL READ_RIP_AQ_COORDS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read keywords for times
        IF( VERBOSE ) CALL TELLTIME( 'Reading ECDA keywords - Pass #3', 'SCREEN', .FALSE. )
        CALL READ_KEY_ECDA_2( IERR ) ! times
        IF( IERR .NE. 0 ) GO TO 999
!
!       Echo some information to the report file
        CALL ECHO( )
!
!       Check problem definition
        IF( VERBOSE ) CALL TELLTIME( 'Checking ECDA keywords', 'SCREEN', .FALSE. )
        CALL CHECK_ECDA( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Open and read and process data
        IF( VERBOSE ) CALL TELLTIME( 'Process ECDA data file', 'SCREEN', .FALSE. )
        CALL PROCESS_ECDA( IERR ) ! Open and read and process data
        IF( IERR .NE. 0 ) GO TO 999
!
      END IF
!
!-----------------------------------------------------------------------------------
!     Processing of FCDA data
!-----------------------------------------------------------------------------------
!
      IF( CAT_FCDA ) THEN
!
!       Do error checking on the initial keyword set
        IF( VERBOSE ) CALL TELLTIME( 'Check initial FCDA keywords', 'SCREEN', .FALSE. )
        CALL CHECK_INIT_FCDA( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read all keywords in HUMAN except times (really need file names)
        IF( VERBOSE ) CALL TELLTIME( 'Reading FCDA keywords - Pass #2', 'SCREEN', .FALSE. )
        CALL READ_KEY_FCDA_1( IERR ) ! Everything FCDA except times
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read THE MAP file (Get lists of data)
        IF( VERBOSE ) CALL TELLTIME( 'Reading FCDA map file', 'SCREEN', .FALSE. )
        CALL FCDA_MAPREAD( FNMAP, IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read and save TecPlot coordinate file (ID, Easting, Northing)
!       Check against what's in the header file list
!       Set compute flags in header file list
        IF( VERBOSE ) CALL TELLTIME( 'Reading TecPlot coordinate file', 'SCREEN', .FALSE. )
        IF( LOC_UPLAND ) CALL READ_UPLAND_COORDS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
        IF( LOC_RIPARIAN .OR. LOC_AQUATIC ) CALL READ_RIP_AQ_COORDS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read keywords for times
        IF( VERBOSE ) CALL TELLTIME( 'Reading FCDA keywords - Pass #3', 'SCREEN', .FALSE. )
        CALL READ_KEY_FCDA_2( IERR ) ! times
        IF( IERR .NE. 0 ) GO TO 999
!
!       Echo some information to the report file
        CALL ECHO( )
!
!       Check problem definition
        IF( VERBOSE ) CALL TELLTIME( 'Checking FCDA keywords', 'SCREEN', .FALSE. )
        CALL CHECK_FCDA( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Open and read and process data
        IF( VERBOSE ) CALL TELLTIME( 'Process FCDA data file', 'SCREEN', .FALSE. )
        CALL PROCESS_FCDA( IERR ) ! Open and read and process data
        IF( IERR .NE. 0 ) GO TO 999
!
      END IF
!
!-----------------------------------------------------------------------------------
!     Processing of ECEM data
!-----------------------------------------------------------------------------------
      IF( CAT_ECEM ) THEN
!
!       Do error checking on the initial keyword set
        IF( VERBOSE ) CALL TELLTIME( 'Check initial keywords', 'SCREEN', .FALSE. )
        CALL CHECK_INIT( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read all keywords in ECEM except times (really need file names)
        IF( VERBOSE ) CALL TELLTIME( 'Reading ECEM keywords - Pass #2', 'SCREEN', .FALSE. )
        CALL READ_KEY_ECEM_1( IERR ) ! Everything ECEM except times
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read SacView header file (Get lists of data)
        IF( VERBOSE ) CALL TELLTIME( 'Reading SacView header file', 'SCREEN', .FALSE. )
        CALL READ_ECEM_SV_HEADER( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read and save TecPlot coordinate file (ID, Easting, Northing)
!       Check against what's in the header file list
!       Set compute flags in header file list
        IF( VERBOSE ) CALL TELLTIME( 'Reading TecPlot coordinate file', 'SCREEN', .FALSE. )
        IF( LOC_UPLAND ) CALL READ_UPLAND_COORDS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
        IF( LOC_RIPARIAN .OR. LOC_AQUATIC ) CALL READ_RIP_AQ_COORDS( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Read keywords in ECEM for times
        IF( VERBOSE ) CALL TELLTIME( 'Reading ECEM keywords - Pass #3', 'SCREEN', .FALSE. )
        CALL READ_KEY_ECEM_2( IERR ) ! ECEM times
        IF( IERR .NE. 0 ) GO TO 999
!
!       Echo some information to the report file
        CALL ECHO( )
!
!       Check problem definition
        IF( VERBOSE ) CALL TELLTIME( 'Checking ECEM keywords', 'SCREEN', .FALSE. )
        CALL CHECK_ECEM( IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
!       Open and read and process data
        IF( VERBOSE ) CALL TELLTIME( 'Process ECEM detailed data file', 'SCREEN', .FALSE. )
        CALL PROCESS_ECEM( IERR ) ! Open and read and process data
        IF( IERR .NE. 0 ) GO TO 999
!
      END IF
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
      MESSAG(1) = 'Error encountered in a lower-level routine.'
      MESSAG(2) = 'Execution halted because of the above errors.'
      CALL PRTERR( IERR, PRGNAM, 2 )
      CALL TELLTIME( 'Abnormal Run Termination Due to Errors', 'SCREEN', .FALSE. )
!
      STOP
!
 1000 CONTINUE
!
! *** Elapsed time message
!
      MESSAG(1) = 'Normal Termination'
      CALL PRTERR( IERR, PRGNAM, 1 )
!
      CALL TELLTIME( 'Normal Termination', 'SCREEN', .FALSE. )
      STOP
      END PROGRAM

      SUBROUTINE BANNER_1( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a banner page to the report file.
!!
!!  History:
!!    Carmen Arimnescu : 21 Mar 2005 : Original source
!!    Paul W. Eslinger :  4 Jun 2007 : Udate to TIIA, add copyright information
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
!
!---- Executable code --------------------------------------------------
!
      IMPLICIT NONE
!
! *** Define the header
      WRITE(IRPT,1000)
 1000 FORMAT(/)
!
      WRITE(IRPT,1010) ' AAAAA   N     N  IIIIIII  M     M   AAAAA   TTTTTTT  EEEEEEE'
      WRITE(IRPT,1010) 'A     A  NN    N     I     MM   MM  A     A     T     E      '
      WRITE(IRPT,1010) 'A     A  N N   N     I     M M M M  A     A     T     E      '
      WRITE(IRPT,1010) 'AAAAAAA  N  N  N     I     M  M  M  AAAAAAA     T     EEEEE  '
      WRITE(IRPT,1010) 'A     A  N   N N     I     M     M  A     A     T     E      '
      WRITE(IRPT,1010) 'A     A  N    NN     I     M     M  A     A     T     E      '
      WRITE(IRPT,1010) 'A     A  N     N  IIIIIII  M     M  A     A     T     EEEEEEE'
 1010 FORMAT(10X,A)
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
!!    Paul W. Eslinger : 27 Oct 2004 : Original source
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
!---- Executable code -------------------------------------------------------------
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

      SUBROUTINE CHECK_CULTURE( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks keyword definitions for a CULTURE data set.
!!
!!  History:
!!
!!    Carmen Arimescu : 19 APR 2006 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE CULTURE_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
!
      CHARACTER(LEN=13) :: CALLER = 'CHECK_CULTURE'
!
      INTEGER :: ITIM ! Time loop counter
      INTEGER :: ICNT ! Counting variable
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(/'Case definition ====================')
!
! *** Problem title
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The problem title is not defined'
        MESSAG(2) = 'Change the TITLE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Map requested
!
      IF( MAP_USE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'The desired MAP is undefined'
        MESSAG(2) = 'Change the MAP keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! **** Check on the title of the ouput tecplot file
!
      IF( LEGEND_TITLE .EQ. ' ' ) THEN
        IERR = 9
        MESSAG(1) = 'The legend title label is undefined'
        MESSAG(2) = 'Change the LEGEND keyword, TITLE modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( LEGEND_CONTOUR .EQ. ' ' ) THEN
        IERR = 11
        MESSAG(1) = 'The legend contour label is undefined'
        MESSAG(2) = 'Change the LEGEND keyword, CONTOUR modifier'
        CALL PRTERR( IERR, CALLER, 2 )
       RETURN
      END IF
 !
! *** Check for one or more output times defined
!
      WRITE(IRPT,2000)
 2000 FORMAT(/' Output times to be animated')
!
      ICNT = 0
      DO ITIM = 1, CULTURE_NUM_TIM
        IF( CULTURE_TIM(ITIM)%COMP ) THEN
          ICNT = ICNT + 1
          WRITE(IRPT,2010) ITIM, CULTURE_TIM(ITIM)%TIME
 2010     FORMAT(2X,I4,1X,I0)
        END IF
      END DO
!
      IF( ICNT .EQ. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Output time is undefined'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_HUMAN( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks keyword definitions for a HUMAN data set.
!!
!!  History:
!!
!!    Carmen Arimescu : 20 APR 2006 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Human_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
!
      CHARACTER(LEN=11) :: CALLER = 'CHECK_HUMAN'
!
      INTEGER :: ITIM ! Time loop counter
      INTEGER :: ICNT ! Counting variable
      INTEGER :: ICST
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(/'Case definition ====================')
!
! *** Problem title
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The problem title is not defined'
        MESSAG(2) = 'Change the TITLE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Analyte requested
!
      IF( ANA_USE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'The desired analyte is undefined'
        MESSAG(2) = 'Change the ANALYTE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Analyte type requested
!
      IF( ATY_USE .EQ. ' ' ) THEN
        IERR = 3
        MESSAG(1) = 'The desired analyte type is undefined'
        MESSAG(2) = 'Change the ANATYPE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Check for a valid analyte type
!
      ICNT = 0
      IF( ATY_USE .EQ. 'CONMEDIA' ) ICNT = ICNT + 1
      IF( ATY_USE .EQ. 'CONFOODS' ) ICNT = ICNT + 1
      IF( ATY_USE .EQ. 'RAD' ) ICNT = ICNT + 1
      IF( ATY_USE .EQ. 'CAR' ) ICNT = ICNT + 1
      IF( ATY_USE .EQ. 'HAZ' ) ICNT = ICNT + 1
      IF( ATY_USE .EQ. 'ALL' ) ICNT = ICNT + 1
      IF( ICNT .NE. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'ANALTYPE keyword modifier is not a correct value'
        MESSAG(2) = 'Allowed values are: CONMEDIA,CONFOODS,RAD,CAR,HAZ or ALL'
        MESSAG(3) = 'Value entered was ' // ATY_USE
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Solution type requested
!
      IF( SLT_USE .EQ. ' ' ) THEN
        IERR = 5
        MESSAG(1) = 'The desired solution type is undefined'
        MESSAG(2) = 'Change the SOLUTION keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Check for a valid solution
!
      ICST = 0
!
      IF( SLT_USE .EQ. 'DOSEING' ) ICST = ICST + 1   ! Ingestion dose
      IF( SLT_USE .EQ. 'DOSEINH' ) ICST = ICST + 1   ! Inhalation dose
      IF( SLT_USE .EQ. 'DOSEEXT' ) ICST = ICST + 1   ! External dose
      IF( SLT_USE .EQ. 'DOSEDER' ) ICST = ICST + 1   ! Dermal dose
      IF( SLT_USE .EQ. 'RISKING' ) ICST = ICST + 1   ! Ingestion risk
      IF( SLT_USE .EQ. 'RISKINH' ) ICST = ICST + 1   ! Inhalation risk
      IF( SLT_USE .EQ. 'RISKEXT' ) ICST = ICST + 1   ! External risk
      IF( SLT_USE .EQ. 'RISKDER' ) ICST = ICST + 1   ! Dermal risk
      IF( SLT_USE .EQ. 'HQING  ' ) ICST = ICST + 1   ! Ingestion hazard quotient
      IF( SLT_USE .EQ. 'HQINH  ' ) ICST = ICST + 1   ! Inhalation hazard quotient
      IF( SLT_USE .EQ. 'HQDER  ' ) ICST = ICST + 1   ! Dermal hazard quotient
      IF( SLT_USE .EQ. 'ANADOSE' ) ICST = ICST + 1   ! Analyte dose
      IF( SLT_USE .EQ. 'ANARISK' ) ICST = ICST + 1   ! Analyte risk
      IF( SLT_USE .EQ. 'ANAHQ  ' ) ICST = ICST + 1   ! Analyte hazard quotient
      IF( SLT_USE .EQ. 'SUMDOSE' ) ICST = ICST + 1   ! Dose summed over analyes
      IF( SLT_USE .EQ. 'SUMRISK' ) ICST = ICST + 1   ! Risk summed over analyes
      IF( SLT_USE .EQ. 'SUMHQ  ' ) ICST = ICST + 1   ! Hazard quotient summed over analyes
      IF( SLT_USE .EQ. 'POPDOSE' ) ICST = ICST + 1   ! Population dose (radioactive)
      IF( SLT_USE .EQ. 'POPRISK' ) ICST = ICST + 1   ! Population risk (radioactive)
      IF( SLT_USE .EQ. 'GWAT' ) ICST = ICST + 1   ! Concentration in ground water
      IF( SLT_USE .EQ. 'SWAT' ) ICST = ICST + 1   ! Concentration in surface water
      IF( SLT_USE .EQ. 'SEDI' ) ICST = ICST + 1   ! Concentration in sediment
      IF( SLT_USE .EQ. 'SEEP' ) ICST = ICST + 1   ! Concentration in seep water
      IF( SLT_USE .EQ. 'SORP' ) ICST = ICST + 1   ! Concentration in riparian zone soil
      IF( SLT_USE .EQ. 'SODR' ) ICST = ICST + 1   ! Concentration in non-irrigated upland soil
      IF( SLT_USE .EQ. 'SOGW' ) ICST = ICST + 1   ! Concentration in groundwater irrigated upland soil
      IF( SLT_USE .EQ. 'SOSW' ) ICST = ICST + 1   ! Concentration in surface water irrigated upland soil
      IF( SLT_USE .EQ. 'AIRC' ) ICST = ICST + 1   ! Concentration in air                                  
      IF( ICST .NE. 1 ) THEN
        IERR = 6
        MESSAG(1) = 'SOLUTION keyword modifier is not a correct value'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Units conversion
!
      IF( CONVERT_UNITS .LE. 0.0 ) THEN
        IERR = 7
        MESSAG(1) = 'The units conversion factor is undefined or negative'
        MESSAG(2) = 'Change the UNITS keyword, FACTOR modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( UNITS_OUTPUT .EQ. ' ' ) THEN
        IERR = 8
        MESSAG(1) = 'The units output label is undefined'
        MESSAG(2) = 'Change the UNITS keyword, OUTPUT modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! **** Check on the title of the ouput tecplot file
!
      IF( LEGEND_TITLE .EQ. ' ' ) THEN
        IERR = 9
        MESSAG(1) = 'The legend title label is undefined'
        MESSAG(2) = 'Change the LEGEND keyword, TITLE modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( LEGEND_CONTOUR .EQ. ' ' ) THEN
        IERR = 11
        MESSAG(1) = 'The legend contour label is undefined'
        MESSAG(2) = 'Change the LEGEND keyword, CONTOUR modifier'
        CALL PRTERR( IERR, CALLER, 2 )
       RETURN
      END IF
!
! *** Check for one or more output times defined
!
      WRITE(IRPT,2000)
 2000 FORMAT(/' Output times to be animated')
!
      ICNT = 0
      DO ITIM = 1, HUMAN_NUM_TIM
        IF( HUMAN_TIM(ITIM)%COMP ) THEN
          ICNT = ICNT + 1
          WRITE(IRPT,2010) ITIM, HUMAN_TIM(ITIM)%TIME
 2010     FORMAT(2X,I4,1X,I0)
        END IF
      END DO
!
      IF( ICNT .EQ. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Output time is  undefined'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_ECEM( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks keyword definitions for an ECEM data set.
!!
!!  History:
!!
!!    Carmen Arimescu : 8 Dec 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ECEM_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
!
      CHARACTER(LEN=10) :: CALLER = 'CHECK_ECEM'
!
      INTEGER :: ITIM ! Time loop counter
      INTEGER :: ICNT ! Counting variable
      INTEGER :: ICST
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(/'Case definition ====================')
!
! *** Problem title
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The problem title is not defined'
        MESSAG(2) = 'Change the TITLE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! **** Check on the title of the ouput tecplot file
!
      IF( LEGEND_TITLE .EQ. ' ' ) THEN
        IERR = 9
        MESSAG(1) = 'The legend title label is undefined'
        MESSAG(2) = 'Change the LEGEND keyword, TITLE modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( LEGEND_CONTOUR .EQ. ' ' ) THEN
        IERR = 11
        MESSAG(1) = 'The legend contour label is undefined'
        MESSAG(2) = 'Change the LEGEND keyword, CONTOUR modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Analyte requested
!
      IF( ANA_USE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'The desired analyte is undefined'
        MESSAG(2) = 'Change the ANALYTE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Soil type requested
!
      IF( STY_USE .EQ. ' ' ) THEN
        IERR = 5
        MESSAG(1) = 'The desired soil type is undefined'
        MESSAG(2) = 'Change the SOIL type keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Check for a valid soil
!
      ICST = 0
      IF( STY_USE .EQ. 'SODR' ) ICST = ICST + 1 ! riparian zone soil media (computed by RIPSAC)
      IF( STY_USE .EQ. 'SORP' ) ICST = ICST + 1 ! non-irrigated upland soil media (computed by SOIL)
      IF( STY_USE .EQ. 'SOGW' ) ICST = ICST + 1 ! groundwater irrigated upland soil media (computed by SOIL)
      IF( STY_USE .EQ. 'SOSW' ) ICST = ICST + 1 ! surface water irrigated upland soil media (computed by SOIL)
      IF( STY_USE .EQ. 'NONE' ) ICST = ICST + 1 ! none soil
!
      IF( ICST .NE. 1 ) THEN
        IERR = 6
        MESSAG(1) = 'SOILTYPE keyword modifier is not a correct value'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Species type requested
!
      IF( SPC_USE .EQ. ' ' ) THEN
        IERR = 3
        MESSAG(1) = 'The desired species is undefined'
        MESSAG(2) = 'Change the SPECIES keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Solution type requested
!
      IF( SLT_USE .EQ. ' ' ) THEN
        IERR = 5
        MESSAG(1) = 'The desired solution type is undefined'
        MESSAG(2) = 'Change the SOLUTION keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Check for a valid solution
!
      ICST = 0
      IF( SLT_USE .EQ. 'CONCEN' ) ICST = ICST + 1   !concentration
      IF( SLT_USE .EQ. 'BURDEN' ) ICST = ICST + 1   !body burden
      IF( SLT_USE .EQ. 'BMTISS' ) ICST = ICST + 1   !aquatic species tissue benchmark
      IF( SLT_USE .EQ. 'DOSRAD' ) ICST = ICST + 1   !radionuclide dose by nuclide
      IF( SLT_USE .EQ. 'SUMRAD' ) ICST = ICST + 1   !summed radionuclide dose
      IF( SLT_USE .EQ. 'DOSDER' ) ICST = ICST + 1   !dermal dose per analyte
      IF( SLT_USE .EQ. 'DOSING' ) ICST = ICST + 1   !ingestion dose by analyte
      IF( SLT_USE .EQ. 'DOSINH' ) ICST = ICST + 1   !inhalation dose by analyte
      IF( ICST .NE. 1 ) THEN
        IERR = 6
        MESSAG(1) = 'SOLUTION keyword modifier is not a correct value'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Units conversion
!
      IF( CONVERT_UNITS .LE. 0.0 ) THEN
        IERR = 7
        MESSAG(1) = 'The units conversion factor is undefined or negative'
        MESSAG(2) = 'Change the UNITS keyword, FACTOR modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( UNITS_OUTPUT .EQ. ' ' ) THEN
        IERR = 8
        MESSAG(1) = 'The units output label is undefined'
        MESSAG(2) = 'Change the UNITS keyword, OUTPUT modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check for one or more output times defined
!
      WRITE(IRPT,2000)
 2000 FORMAT(/' Output times to be animated')
!
      ICNT = 0
      DO ITIM = 1, ECEM_NUM_TIM
        IF( ECEM_TIM(ITIM)%COMP ) THEN
          ICNT = ICNT + 1
          WRITE(IRPT,2010) ITIM, ECEM_TIM(ITIM)%TIME
 2010     FORMAT(2X,I4,1X,I0)
        END IF
      END DO
!
      IF( ICNT .EQ. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Output time is  undefined'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_ECDA( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks keyword definitions for an ECDA data set.
!!
!!  History:
!!
!!    Carmen Arimescu : 8 Dec 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ECDA_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
!
      CHARACTER(LEN=10) :: CALLER = 'CHECK_ECDA'
!
      INTEGER :: ITIM ! Time loop counter
      INTEGER :: ICNT ! Counting variable
!      INTEGER :: ICST
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(/'Case definition ====================')
!
! *** Problem title
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The problem title is not defined'
        MESSAG(2) = 'Change the TITLE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! **** Check on the title of the ouput tecplot file
!
      IF( LEGEND_TITLE .EQ. ' ' ) THEN
        IERR = 9
        MESSAG(1) = 'The legend title label is undefined'
        MESSAG(2) = 'Change the LEGEND keyword, TITLE modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( LEGEND_CONTOUR .EQ. ' ' ) THEN
        IERR = 11
        MESSAG(1) = 'The legend contour label is undefined'
        MESSAG(2) = 'Change the LEGEND keyword, CONTOUR modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Analyte requested
!
      IF( ANA_USE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'The desired analyte is undefined'
        MESSAG(2) = 'Change the ANALYTE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Media type requested
!
      IF( MED_USE .EQ. ' ' ) THEN
        IERR = 5
        MESSAG(1) = 'The desired media type is undefined'
        MESSAG(2) = 'Change the MEDIA type keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Check for a valid MEDIA and assign the index for use in ECDA data access
!
      MED_IDX = 0
      IF( MED_USE .EQ. 'GWAT' ) MED_IDX = IGWAT ! Groundwater media (computed by CFEST)
      IF( MED_USE .EQ. 'SEEP' ) MED_IDX = ISEEP ! Seep water media (computed by RIPSAC)
      IF( MED_USE .EQ. 'SWAT' ) MED_IDX = ISWAT ! Surface water media (computed by MASS2)
      IF( MED_USE .EQ. 'PWAT' ) MED_IDX = IPWAT ! Pore water media (computed by MASS2)
      IF( MED_USE .EQ. 'SEDI' ) MED_IDX = ISEDI ! River bottom sediment media (computed by MASS2)
      IF( MED_USE .EQ. 'SORP' ) MED_IDX = ISORP ! Riparian zone soil media (computed by RIPSAC)
      IF( MED_USE .EQ. 'SODR' ) MED_IDX = ISODR ! Non-irrigated upland soil media (computed by SOIL)
      IF( MED_USE .EQ. 'SOGW' ) MED_IDX = ISOGW ! Groundwater irrigated upland soil media (computed by SOIL)
      IF( MED_USE .EQ. 'SOSW' ) MED_IDX = ISOSW ! Surface water irrigated upland soil media (computed by SOIL)
      IF( MED_USE .EQ. 'AIRC' ) MED_IDX = IAIRC ! Air concentration media (computed by RATCHET)
      IF( MED_USE .EQ. 'AIRD' ) MED_IDX = IAIRD ! Air deposition media (computed by RATCHET)
      IF( MED_IDX .EQ. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'MEDIA keyword modifier is not a correct value'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Units conversion
!
      IF( CONVERT_UNITS .LE. 0.0 ) THEN
        IERR = 7
        MESSAG(1) = 'The units conversion factor is undefined or negative'
        MESSAG(2) = 'Change the UNITS keyword, FACTOR modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( UNITS_OUTPUT .EQ. ' ' ) THEN
        IERR = 8
        MESSAG(1) = 'The units output label is undefined'
        MESSAG(2) = 'Change the UNITS keyword, OUTPUT modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check for one or more output times defined
!
      WRITE(IRPT,2000)
 2000 FORMAT(/' Output times to be animated')
!
      ICNT = 0
      DO ITIM = 1, ECDA_NTIMES
        IF( ECDA_TIMES(ITIM)%COMP ) THEN
          ICNT = ICNT + 1
          WRITE(IRPT,2010) ITIM, ECDA_TIMES(ITIM)%TIME
 2010     FORMAT(2X,I4,1X,I0)
        END IF
      END DO
!
      IF( ICNT .EQ. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Output time is  undefined'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_FCDA( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine checks keyword definitions for an FCDA data set.
!!
!!  History:
!!
!!    Carmen Arimescu : 10 Jan 2005 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE FCDA_Mod
!
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: COMPUTE_SPC_FCDA
!
! *** Call list variables
      INTEGER :: IERR ! Integer error flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'CHECK_FCDA'
!
      INTEGER :: ITIM ! Time loop counter
      INTEGER :: ICNT ! Counting variable
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(/'Case definition ====================')
!
! *** Problem title
!
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The problem title is not defined'
        MESSAG(2) = 'Change the TITLE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! **** Check on the title of the ouput tecplot file
!
      IF( LEGEND_TITLE .EQ. ' ' ) THEN
        IERR = 9
        MESSAG(1) = 'The legend title label is undefined'
        MESSAG(2) = 'Change the LEGEND keyword, TITLE modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( LEGEND_CONTOUR .EQ. ' ' ) THEN
        IERR = 11
        MESSAG(1) = 'The legend contour label is undefined'
        MESSAG(2) = 'Change the LEGEND keyword, CONTOUR modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Analyte requested
!
      IF( ANA_USE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'The desired analyte is undefined'
        MESSAG(2) = 'Change the ANALYTE keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( LOC_UPLAND)  THEN
!
! ***   Soil type requested
!
        IF( STY_USE .EQ. ' ' ) THEN
          IERR = 5
          MESSAG(1) = 'The desired soil type is undefined'
          MESSAG(2) = 'Change the SOIL type keyword'
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
!       Check for a valid SOIL and assign the index for use in FCDA data access
!
        STY_IDX = 0
        IF( STY_USE .EQ. 'SODR' ) STY_IDX = IDX_SODR  ! Dry-land soil species
        IF( STY_USE .EQ. 'SOGW' ) STY_IDX = IDX_SOGW  ! Groundwater irrigated soil species
        IF( STY_USE .EQ. 'SOSW' ) STY_IDX = IDX_SOSW  ! Surfacewater irrigated soil species
        IF( STY_IDX .EQ. 0 ) THEN
          IERR = 6
          MESSAG(1) = 'SOIL keyword modifier is not a correct value'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      END IF
!
! *** Species type requested
!
      IF( SPC_USE .EQ. ' ' ) THEN
        IERR = 7
        MESSAG(1) = 'The desired species is undefined'
        MESSAG(2) = 'Change the SPECIES keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( .NOT.COMPUTE_SPC_FCDA( SPC_USE ) ) THEN
        IERR = 8
        MESSAG(1) = 'The REQUESTED species ('//TRIM(SPC_USE)//') was not computed as a food'
        MESSAG(2) = 'Change the SPECIES keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Units conversion
!
      IF( CONVERT_UNITS .LE. 0.0 ) THEN
        IERR = 9
        MESSAG(1) = 'The units conversion factor is undefined or negative'
        MESSAG(2) = 'Change the UNITS keyword, FACTOR modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      IF( UNITS_OUTPUT .EQ. ' ' ) THEN
        IERR = 10
        MESSAG(1) = 'The units output label is undefined'
        MESSAG(2) = 'Change the UNITS keyword, OUTPUT modifier'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check for one or more output times defined
      WRITE(IRPT,2000)
 2000 FORMAT(/' Output times to be animated')
!
      ICNT = 0
      DO ITIM = 1, FCDA_NTIMES
        IF( FCDA_TIM(ITIM)%COMP .AND. FCDA_TIM(ITIM)%TEC_COMP ) THEN
          ICNT = ICNT + 1
          WRITE(IRPT,2010) ITIM, FCDA_TIM(ITIM)%TIME
 2010     FORMAT(2X,I4,1X,I0)
        END IF
      END DO
!
      IF( ICNT .EQ. 0 ) THEN
        IERR = 11
        MESSAG(1) = 'Output time is  undefined'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_INIT( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the initial keyword information
!!    looking for problem definition errors.
!!
!!  History:
!!
!!    Paul W. Eslinger : 27 Oct 2004 : Original source
!!    Carmen Arimescu  : 10 Nov 2004
!!    Carmen Arimescu  :  7 Apr 2006 : Remove OS_SLASH
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR ! Output integer error flag
!                         0 = No errors
!                        >0 = Terminal error encountered
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'CHECK_INIT'
      LOGICAL :: THERE   ! Temporary logical flag
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Identification errors
      IF( USRNAM .EQ. 'Anonymous User' ) THEN
        IERR = 1
        MESSAG(1) = 'User name must be entered (Keyword USER)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
! *** Problem title
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'A problem title must be entered (Keyword TITLE)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
! *** Check on the coordinate file name
      IF( FNCOR .EQ. ' ' ) THEN
        IERR = 3
        MESSAG(1) = 'A coordinate file name must be entered (Keyword FILE COORDIN)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNCOR,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 4
          MESSAG(1) = 'The coordinate file does not exist (Keyword FILE COORDIN)'
          MESSAG(2) = 'File: '//TRIM(FNCOR)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the connectivity file name
      IF( FNCOV .EQ. ' ' ) THEN
        IERR = 5
        MESSAG(1) = 'A conectivity file name must be entered (Keyword FILE CONNECT)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNCOV,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 6
          MESSAG(1) = 'The coordinate file does not exist (Keyword FILE CONECT)'
          MESSAG(2) = 'File: '//TRIM(FNCOV)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the header file name
!
      IF( FNHED .EQ. ' ' ) THEN
        IERR = 7
        MESSAG(1) = 'The output SACVIEW header file name is missing'
        MESSAG(2) = 'Use the HEADER modifier on the FILE Keyword'
        MESSAG(3) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Check on the detail file name
      IF( FNDET .EQ. ' ' ) THEN
        IERR = 8
        MESSAG(1) = 'A details file name must be entered (Keyword FILE DETAIL)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNDET,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 9
          MESSAG(1) = 'The details file does not exist (Keyword FILE DETAIL)'
          MESSAG(2) = 'File: '//TRIM(FNDET)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the results file name
      IF( FNRES .EQ. ' ' ) THEN
        IERR = 12
        MESSAG(1) = 'A results file name must be entered (Keyword FILE RESULTS)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_INIT_CULTURE( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the initial keyword information
!!    looking for problem definition errors.
!!
!!  History:
!!
!!    Paul W. Eslinger : 27 Oct 2004 : Original source
!!    Carmen Arimescu  : 20 Apr 2006
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR ! Output integer error flag
!                         0 = No errors
!                        >0 = Terminal error encountered
!
! *** Local variables
      CHARACTER(LEN=18) :: CALLER = 'CHECK_INIT_CULTURE'
      LOGICAL :: THERE   ! Temporary logical flag
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Identification errors
      IF( USRNAM .EQ. 'Anonymous User' ) THEN
        IERR = 1
        MESSAG(1) = 'User name must be entered (Keyword USER)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
! *** Problem title
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'A problem title must be entered (Keyword TITLE)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
! *** Check on the coordinate file name
      IF( FNCOR .EQ. ' ' ) THEN
        IERR = 3
        MESSAG(1) = 'A coordinate file name must be entered (Keyword FILE COORDIN)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNCOR,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 4
          MESSAG(1) = 'The coordinate file does not exist (Keyword FILE COORDIN)'
          MESSAG(2) = 'File: '//TRIM(FNCOR)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the connectivity file name
      IF( FNCOV .EQ. ' ' ) THEN
        IERR = 5
        MESSAG(1) = 'A conectivity file name must be entered (Keyword FILE CONNECT)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNCOV,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 6
          MESSAG(1) = 'The coordinate file does not exist (Keyword FILE CONECT)'
          MESSAG(2) = 'File: '//TRIM(FNCOV)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the header file name
!
      IF( FNHED .EQ. ' ' ) THEN
        IERR = 7
        MESSAG(1) = 'The output SACVIEW header file name is missing'
        MESSAG(2) = 'Use the HEADER modifier on the FILE Keyword'
        MESSAG(3) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Check on the Culture map file name
      IF( FNCMAP .EQ. ' ' ) THEN
        IERR = 10
        MESSAG(1) = 'A map file name must be entered (Keyword FILE MAP)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNCMAP,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR =11
          MESSAG(1) = 'The map file does not exist (Keyword FILE MAP)'
          MESSAG(2) = 'File: '//TRIM(FNCMAP)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the results file name
      IF( FNRES .EQ. ' ' ) THEN
        IERR = 12
        MESSAG(1) = 'A results file name must be entered (Keyword FILE RESULTS)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_INIT_ECDA( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the initial keyword information
!!    looking for problem definition errors.
!!
!!  History:
!!
!!    Paul W. Eslinger : 27 Oct 2004 : Original source
!!    Carmen Arimescu  :  3 Jan 2005
!!    Carmen Arimescu  :  7 Apr 2006 : Remove OS_SLASH
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR ! Output integer error flag
!                         0 = No errors
!                        >0 = Terminal error encountered
!
! *** Local variables
      CHARACTER(LEN=15) :: CALLER = 'CHECK_INIT_ECDA'
      LOGICAL :: THERE   ! Temporary logical flag
!
!---- Executable code --------------------------------------------
!
      IERR = 0
!
! *** Identification errors
      IF( USRNAM .EQ. 'Anonymous User' ) THEN
        IERR = 1
        MESSAG(1) = 'User name must be entered (Keyword USER)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
! *** Problem title
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'A problem title must be entered (Keyword TITLE)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
! *** Check on the coordinate file name
      IF( FNCOR .EQ. ' ' ) THEN
        IERR = 3
        MESSAG(1) = 'A coordinate file name must be entered (Keyword FILE COORDIN)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNCOR,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 4
          MESSAG(1) = 'The coordinate file does not exist (Keyword FILE COORDIN)'
          MESSAG(2) = 'File: '//TRIM(FNCOR)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the connectivity file name
      IF( FNCOV .EQ. ' ' ) THEN
        IERR = 5
        MESSAG(1) = 'A conectivity file name must be entered (Keyword FILE CONNECT)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNCOV,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 6
          MESSAG(1) = 'The coordinate file does not exist (Keyword FILE CONECT)'
          MESSAG(2) = 'File: '//TRIM(FNCOV)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the ECDA map file name
      IF( FNMAP .EQ. ' ' ) THEN
        IERR = 8
        MESSAG(1) = 'A ECDA map file name must be entered (Keyword FILE I_ECDA)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNMAP,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 9
          MESSAG(1) = 'The ECDA map file does not exist (Keyword FILE I_ECDA)'
          MESSAG(2) = 'File: '//TRIM(FNMAP)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the ECDA concentrations file name
      IF( FNCOV .EQ. ' ' ) THEN
        IERR = 8
        MESSAG(1) = 'A ECDA concentrations file name must be entered (Keyword FILE C_ECDA)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNCOV,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 9
          MESSAG(1) = 'The map file does not exist (Keyword FILE C_ECDA)'
          MESSAG(2) = 'File: '//TRIM(FNCOV)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the results file name
      IF( FNRES .EQ. ' ' ) THEN
        IERR = 10
        MESSAG(1) = 'A results file name must be entered (Keyword FILE RESULTS)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
      RETURN
      END SUBROUTINE
!
      SUBROUTINE CHECK_INIT_FCDA( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the initial keyword information
!!    looking for problem definition errors.
!!
!!  History:
!!
!!    Paul W. Eslinger : 27 Oct 2004 : Original source
!!    Carmen Arimescu  :  9 Jan 2005
!!    Carmen Arimescu  :  7 Apr 2006 : Remove OS_SLASH
!!    Paul W. Eslinger :  5 Jun 2007 : Update Error checks
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Output integer error flag
!                         0 = No errors
!                        >0 = Terminal error encountered
!
! *** Local variables
      CHARACTER(LEN=15) :: CALLER = 'CHECK_INIT_FCDA' ! Name of this subroutine
      LOGICAL :: THERE   ! Temporary logical flag
!
!---- Executable code --------------------------------------------------------------
!
      IERR = 0
!
! *** Identification errors
      IF( USRNAM .EQ. 'Anonymous User' ) THEN
        IERR = 1
        MESSAG(1) = 'User name must be entered (Keyword USER)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
! *** Problem title
      IF( PTITLE .EQ. ' ' ) THEN
        IERR = 2
        MESSAG(1) = 'A problem title must be entered (Keyword: TITLE)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
! *** Check on the coordinate file name
      IF( FNCOR .EQ. ' ' ) THEN
        IERR = 3
        MESSAG(1) = 'A coordinate file name must be entered (Keyword: FILE COORDIN)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNCOR,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 4
          MESSAG(1) = 'The coordinate file does not exist (Keyword: FILE COORDIN)'
          MESSAG(2) = 'File: '//TRIM(FNCOR)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the connectivity file name
      IF( FNCOV .EQ. ' ' ) THEN
        IERR = 5
        MESSAG(1) = 'A conectivity file name must be entered (Keyword: FILE CONNECT)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNCOV,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 6
          MESSAG(1) = 'The conectivity file does not exist (Keyword: FILE CONNECT)'
          MESSAG(2) = 'File: '//TRIM(FNCOV)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the FCDA map file name
      IF( FNMAP .EQ. ' ' ) THEN
        IERR = 7
        MESSAG(1) = 'A FCDA map file name must be entered (Keyword: FILE I_FCDA)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNMAP,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 8
          MESSAG(1) = 'The FCDA map file does not exist (Keyword: FILE I_FCDA)'
          MESSAG(2) = 'File: '//TRIM(FNMAP)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the FCDA concentrations file name
      IF( FNFOD .EQ. ' ' ) THEN
        IERR = 9
        MESSAG(1) = 'A FCDA concentrations file name must be entered (Keyword: FILE C_FCDA)'
        CALL PRTERR( IERR, CALLER, 1 )
      ELSE
        INQUIRE(FILE=FNFOD,EXIST=THERE)
        IF( .NOT.THERE ) THEN
          IERR = 10
          MESSAG(1) = 'The concentration file does not exist (Keyword: FILE C_ECDA)'
          MESSAG(2) = 'File: '//TRIM(FNFOD)
          CALL PRTERR( IERR, CALLER, 2 )
        END IF
      END IF
!
! *** Check on the results file name
      IF( FNRES .EQ. ' ' ) THEN
        IERR = 11
        MESSAG(1) = 'A results file name must be entered (Keyword: FILE RESULTS)'
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
!!    Carmen Arimnescu : 21 Mar 2005 : Original source
!!    Paul W. Eslinger :  5 Jun 2007 : Enhance outputs
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
!---- First executable code --------------------------------------------
!
      WRITE(IRPT,1000)
 1000 FORMAT(//24('='),' Echo of the Problem Definition ',24('='))
!
! *** Problem Identification
      WRITE(IRPT,1010) TRIM(PTITLE), TRIM(USRNAM)
 1010 FORMAT(/'Title: "',A,'"'/'User:  "',A,'"')
!
! *** Files names used in the analysis
      WRITE(IRPT,1030) 'File Name for this file', TRIM(FNRPT)
      WRITE(IRPT,1030) 'File Name for Input Keyword Data', TRIM(FNKEY)
      WRITE(IRPT,1030) 'File Name for Location Coordinates', TRIM(FNRES)
      WRITE(IRPT,1030) 'File Name for Tecplot connectivity', TRIM(FNRES)
      WRITE(IRPT,1030) 'File Name for Output Results', TRIM(FNRES)
 1030 FORMAT(/A/'File: "',A,'"')
!
! *** Echo the data type selected
      IF( CAT_CULTURE ) THEN
        WRITE(IRPT,1040) TRIM(MAP_USE), TRIM(FNCMAP)
 1040   FORMAT(/'The data type is map (CULTURE Code), map ID: ',A/&
                'File: "',A,'"')
      END IF
!
      IF( CAT_ECDA ) THEN
        WRITE(IRPT,1050) TRIM(MED_USE), TRIM(FNCON)
 1050   FORMAT(/'The data type is media concentrations (ECDA), media type: ',A/&
                'File: "',A,'"')
      END IF
!
      IF( CAT_ECEM ) THEN
        WRITE(IRPT,1060) TRIM(SLT_USE), TRIM(FNDET)
 1060   FORMAT(/'The data type is detailed outputs from the ECEM code, solution type: ',A/&
                'File: "',A,'"')
      END IF
!
      IF( CAT_FCDA ) THEN
        WRITE(IRPT,1070) TRIM(SPC_USE), TRIM(FNFOD)
 1070   FORMAT(/'The data type is food concentrations (FCDA) for species: ',A/&
                'File: "',A,'"')
      END IF
!
      IF( CAT_HUMAN ) THEN
        WRITE(IRPT,1080) TRIM(SLT_USE), TRIM(FNDET)
 1080   FORMAT(/'The data type is detailed outputs from the HUMAN code, solution type: ',A/&
                'File: "',A,'"')
      END IF
!
! *** Echo the location selection
      IF( LOC_UPLAND )   WRITE(IRPT,1090) 'UPLAND'
      IF( LOC_RIPARIAN ) WRITE(IRPT,1090) 'RIPARIAN'
      IF( LOC_AQUATIC )  WRITE(IRPT,1090) 'AQUATIC'
 1090 FORMAT('The location selection for the data was: ',A)
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
!!
!!**********************************************************************
!
      IMPLICIT NONE
!
! *** Local variables
!
      LOGICAL :: CONNECTED ! Logical flag whether unit number was connected
      INTEGER :: IFIL      ! Looping index for file numbers
!
! *** Executable code
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
!!    Paul W. Eslinger : 27 Oct 2004 : Original source
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
      PRGNAM = 'ANIMATE'
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
!!    This subroutine initializes global variables.
!!
!!  History:
!!
!!    Carmen Arimnescu : 21 Mar 2005 : Original source
!!    Carmen Arimescu  :  7 Apr 2006 : Remove OS_SLASH
!!
!!**********************************************************************
!
! *** Include global parameters, variables, and arrays
!
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Debug_Mod
      USE Human_Mod
      USE Culture_Mod
!
      IMPLICIT NONE
!
!---- Executable code --------------------------------------------
!
! *** Additional output information
      VERBOSE = .FALSE.
!
! *** Keyword input file
      FNKEY = ' '
!
! *** Report file
      FNRPT = ' '
      REPORT = .FALSE.
!
! *** Input scrach file
      FNHED = ' '
!
! *** Coordinate input file
      FNCOR = ' '
!
! *** Connectivity input file
      FNCOV = ' '
!
! *** Input detailed risks file
      FNDET = ' '
!
! *** Input map file from the Culture impact
      FNCMAP = ' '
!
! *** Input ECDA map file
      FNMAP = ' '
!
! *** Input ECDA concentrations file
      FNCON = ' '
!
! *** Input FCDA concentrations file
      FNFOD = ' '
!
! *** Input scrach file
      FNRES = ' '
!
! *** Problem title, solution nad analyte type
      PTITLE = ' '
!
! *** Solution type information
!
      ANA_USE = ' '    ! ANALYTE
      ATY_USE = ' '    ! ANALYTE TYPE
      SLT_USE = ' '    ! SOLUTION TYPE
      STY_USE = ' '    ! SOIL TYPE
      SPC_USE = ' '    ! SPECIES
      MED_USE = ' '    ! MEDIA
      MAP_USE = ' '    ! MAP
!
! *** Units data
!
      CONVERT_UNITS = -1.0E30
      UNITS_OUTPUT  = ' '
!
! *** Legend Data
!
     LEGEND_TITLE = ' '
     LEGEND_CONTOUR = ' '
!
! *** Variables for the time slice data
!
      USE_ALL_TIME = .FALSE.
!
! *** Solution type information
!
      CAT_CULTURE = .FALSE.
      CAT_ECEM    = .FALSE.
      CAT_ECDA    = .FALSE.
      CAT_FCDA    = .FALSE.
      CAT_HUMAN   = .FALSE.
!
      LOC_UPLAND    = .FALSE.
      LOC_RIPARIAN  = .FALSE.
      LOC_AQUATIC   = .FALSE.
!
! *** Data type information
!
      HUMAN_NUM_LOC    = 0
      HUMAN_NUM_TIM    = 0
      CULTURE_NUM_TIM  = 0
      CULTURE_NUM_LOC  = 0
      CULTURE_NUM_MAPS = 0
!
! *** Debug flags
!
      BGHUMAN   = .FALSE.
      BGCULTURE = .FALSE.
      BGECEM    = .FALSE.
      BGECDA    = .FALSE.
      BGFCDA    = .FALSE.
!      
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_LOC_CULTURE( LOC_ID, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a location name
!!    in the CULTURE module and a location label.  If found, the index
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
!!   Carmen Arimescu 19 Apr 2006 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE CULTURE_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IDX
      CHARACTER(LEN=6) :: LOC_ID
!
! *** Local variables
!
      INTEGER :: ILOC ! Looping variable
!
! *** Look for a match on the analyte names
!
      IDX = -1
      DO ILOC = 1, CULTURE_NUM_LOC
        IF( CULTURE_LOC(ILOC)%ID .EQ. LOC_ID ) THEN
          IDX = ILOC
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_LOC_HUMAN( LOC_ID, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a location name
!!    in the HUMAN module and a location label.  If found, the index
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
!!    Paul W. Eslinger : 13 Dec : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE HUMAN_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IDX
      CHARACTER(LEN=6) :: LOC_ID
!
! *** Local variables
!
      INTEGER :: ILOC ! Looping variable
!
! *** Look for a match on the analyte names
!
      IDX = -1
      DO ILOC = 1, HUMAN_NUM_LOC
        IF( HUMAN_LOC(ILOC)%ID .EQ. LOC_ID ) THEN
          IDX = ILOC
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_LOC_ECEM( LOC_ID, IDX )
!!**********************************************************************
!! Purpose:
!!
!!    This subroutine attempts to find a match between a location name
!!    in the ECEM module and a location label.  If found, the index
!!    identifies the storage location for the location data.  If not
!!    found, a negative index is returned.
!!
!!  Call List:
!!
!!    LABEL : Input  - Character - laocation label
!!    IDX   : Output - Integer   - index for data associated with LABEL
!!
!!
!!  History:
!!
!!    Paul W. Eslinger : 13 Dec : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE ECEM_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IDX
      CHARACTER(LEN=6) :: LOC_ID
!
! *** Local variables
!
      INTEGER :: ILOC ! Looping variable
!
! *** Look for a match on the analyte names
!
      IDX = -1
      DO ILOC = 1, ECEM_NUM_LOC
        IF( ECEM_LOC(ILOC)%ID .EQ. LOC_ID ) THEN
          IDX = ILOC
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_LOC_ECDA( LOC_ID, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a location name
!!    in the ECDA module and a location label.  If found, the index
!!    identifies the storage location for the location data.  If not
!!    found, a negative index is returned.
!!
!!  Call List:
!!
!!    LABEL : Input  - Character - laocation label
!!    IDX   : Output - Integer   - index for data associated with LABEL
!!
!!
!!  History:
!!
!!    Carmen Arimescu :  3 Jan 2005 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE ECDA_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IDX
      CHARACTER(LEN=6) :: LOC_ID
!
! *** Local variables
!
      INTEGER :: ILOC ! Looping variable
!
! *** Look for a match on the analyte names
!
      IDX = -1
      DO ILOC = 1, ECDA_NLOCS
        IF( ECDA_LOC(ILOC)%ID .EQ. LOC_ID ) THEN
          IDX = ILOC
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_LOC_FCDA( LOC_ID, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a location name
!!    in the FCDA module and a location label.  If found, the index
!!    identifies the storage location for the location data.  If not
!!    found, a negative index is returned.
!!
!!  Call List:
!!
!!    LABEL : Input  - Character - laocation label
!!    IDX   : Output - Integer   - index for data associated with LABEL
!!
!!
!!  History:
!!
!!    Carmen Arimescu :  3 Jan 2005 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE FCDA_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IDX
      CHARACTER(LEN=6) :: LOC_ID
!
! *** Local variables
!
      INTEGER :: ILOC ! Looping variable
!
! *** Look for a match on the analyte names
!
      IDX = -1
      DO ILOC = 1, FCDA_NLOCS
        IF( FCDA_LOC(ILOC)%ID .EQ. LOC_ID ) THEN
          IDX = ILOC
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      LOGICAL FUNCTION COMPUTE_SPC_FCDA( SPC_ID )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine determines whether a species identified by a
!!    species ID was computed as a food.
!!
!!  History:
!!
!!    Paul W. Eslinger :  5 Jun 2007 : Original source
!!
!!**********************************************************************
!
! *** Global variables
      USE FCDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=6) :: SPC_ID
!
! *** Local variables
      INTEGER :: I ! Looping variable
!
!---- First executable code --------------------------------------------
!
      COMPUTE_SPC_FCDA = .FALSE.
      DO I = 1, FCDA_NSPC
        IF( FCDA_SPC(I)%ID.EQ.SPC_ID .AND. FCDA_SPC(I)%COMP ) THEN
          COMPUTE_SPC_FCDA = .TRUE.
          RETURN
        END IF
      END DO
!
      RETURN
      END FUNCTION
!
      SUBROUTINE MATCH_TIM_CULTURE( TIME, IDX )
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
!!    Carmen Arimescu 19 Apr 2006 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE CULTURE_Mod
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
      DO ITIM = 1, CULTURE_NUM_TIM
        IF( CULTURE_TIM(ITIM)%TIME .EQ. TIME ) THEN
          IDX = ITIM
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_TIM_HUMAN( TIME, IDX )
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
      USE HUMAN_Mod
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
      DO ITIM = 1, HUMAN_NUM_TIM
        IF( HUMAN_TIM(ITIM)%TIME .EQ. TIME ) THEN
          IDX = ITIM
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_TIM_ECEM( TIME, IDX )
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
      USE ECEM_Mod
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
      DO ITIM = 1, ECEM_NUM_TIM
        IF( ECEM_TIM(ITIM)%TIME .EQ. TIME ) THEN
          IDX = ITIM
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_TIM_ECDA( TIME, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a time in the
!!    ECDA module and an input time.  If found, the index identifies the
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
!!    Carmen Arimescu :  3 Jan 2005 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE ECDA_Mod
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
      DO ITIM = 1, ECDA_NTIMES
        IF( ECDA_TIMES(ITIM)%TIME .EQ. TIME ) THEN
          IDX = ITIM
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE MATCH_TIM_FCDA( TIME, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between a time in the
!!    FCDA module and an input time.  If found, the index identifies the
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
!!    Carmen Arimescu :  9 Jan 2005 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE FCDA_Mod
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
      DO ITIM = 1, FCDA_NTIMES
        IF( FCDA_TIM(ITIM)%TIME .EQ. TIME ) THEN
          IDX = ITIM
          RETURN
        END IF
      END DO
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
!!      2. Open the file on unit number IKEY for later use
!!
!!  Note:
!!    This subroutine does not call PRTERR when an error occurs because
!!    PRTERR writes to the report file.  The report file is not opened
!!    until the keywords are read in subroutine KEY_BACK.
!!
!!  History:
!!    Carmen Arimnescu : 21 Mar 2005 : Original source
!!    Paul W. Eslinger : 29 Jun 2012 : Revise to a common callable routine
!!
!!********************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Iden_Mod
!
! *** Call list variables
      IMPLICIT NONE
!
      INTEGER :: IERR
!
! *** External functions
      INTEGER, EXTERNAL :: NARGS ! Lahey utility subroutine
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
      LOGICAL :: THERE         ! Logical file existence variable
      INTEGER :: IERF          ! Status variable for open statement
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
        WRITE(*,1030)
 1030   FORMAT(' System error getting a unit number for the input keyword file')
        RETURN
      END IF
!
! *** Attempt to open the file
      OPEN(IKEY,FILE=FNKEY,STATUS='OLD',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 3
        WRITE(*,1040) TRIM(FNKEY)
 1040   FORMAT(' System error opening the input keyword file'/ &
               ' File: ',A)
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PROCESS_ECDA( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This program creates a file ready to be use in TecPlot for
!!    animations. It operates on only 1 realization at a time.
!!    (CATEGORY ECDA)
!!
!!  History:
!!
!!    Carmen Arimescu : 4 Jan 2005 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE ECDA_Mod
      USE Rdblk_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
      INTEGER :: IERR ! Error flag
!
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
!
      CHARACTER(LEN=12) :: CALLER = 'PROCESS_ECDA'
      LOGICAL :: FIRST_YEAR ! Flag for processing the first year
      REAL, ALLOCATABLE :: DET_VALUE(:)
!
      INTEGER :: C_YEAR     ! Year from ECDA data file
      CHARACTER(LEN=6) :: C_LOC_ID ! Location from ECDA data file
      CHARACTER(LEN=4) :: C_MED_ID ! Media from ECDA data file
 !
      INTEGER :: I1, I2, I3  !Connectivity
      INTEGER :: I, IDX
!      CHARACTER(LEN=LENQQQ) :: HEADER
      INTEGER :: IOS ! Status variable for open statement
      INTEGER :: ITIM, ICNT, ILOC
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      ICON = GET_UNIT_NUMBER(  )
      IF( ICON .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the ECDA concentration file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the ECDA Details file
!
      CALL ECDA_OPEN( FNCON, ICON, IERR )
!
! *** Check for the correct analyte
!
      IF( ECDA_ANALYTE .NE. ANA_USE ) THEN
        IERR = 2
        MESSAG(1) = 'Error matching ECDA file with the requested analyte'
        MESSAG(2) = 'File: '//TRIM(FNCON)
        MESSAG(3) = 'Requested: '//TRIM(ANA_USE)
        MESSAG(4) = 'In File:   '//TRIM(ECDA_ANALYTE)
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Get a unit number for output file
!
      IRES = GET_UNIT_NUMBER(  )
      IF( IRES .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the output  file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output file
!
      OPEN(UNIT=IRES, IOSTAT=IOS, FILE=FNRES )
      IF( IOS .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Error opening output file'
        WRITE(MESSAG(2), *) 'UNIT = ', IRES, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNRES = ' // TRIM(FNRES)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Allocate space for the details vector
!
      FIRST_YEAR = .TRUE.
      ALLOCATE(DET_VALUE(NUM_COR))
      DET_VALUE = 0.0
!
!--------------------------------------------------------------------------------------
!     Upland data type processing
!--------------------------------------------------------------------------------------
!
      IF( LOC_UPLAND ) THEN
!       UPLAND Locations require a connectivity file
!
! ***   Get a unit number for connectivity file
!
        ICOV = GET_UNIT_NUMBER(  )
        IF( ICOV .LT. 7 ) THEN
          IERR = 1
          MESSAG(1) = 'Unable to get unit number for the connectivity file'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!       
! ***   Open the connectivity file
!
        OPEN(UNIT=ICOV, IOSTAT=IOS, STATUS='OLD', FILE=FNCOV )
        IF( IOS .NE. 0 ) THEN
          IERR = 2
          MESSAG(1) = 'Error opening connectivity file'
          WRITE(MESSAG(2), *) 'UNIT = ', ICOV, ' IOSTAT = ', IOS
          MESSAG(3) = 'FNCOV = ' // TRIM(FNCOV)
          CALL PRTERR(IERR, CALLER, 3)
          RETURN
        END IF
!
        NUM_CON=0
   10   CONTINUE
          READ(UNIT=ICOV,FMT=*, END=20) I1, I2, I3
          NUM_CON = NUM_CON + 1
          GO TO 10
!
   20   CONTINUE
!
        REWIND(ICOV)
!
! ***   Opening lines in the Tecplot output file
!
         WRITE(IRES,660) TRIM(LEGEND_TITLE)
         WRITE(IRES,770) TRIM(LEGEND_CONTOUR)
   660   FORMAT(2x,'TITLE     = "',A,'"')
   770   FORMAT(2x,'VARIABLES = "Easting","Northing","NODE","',A,'"')
!
   60   FORMAT(2x,'TITLE     = "',A,'"')
!
!       Reading detail file one year at a time
        DO ITIM = 1, ECDA_NTIMES
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. ECDA_TIMES(ITIM)%COMP ) CYCLE
!
          ICNT = 0
          DO ILOC = 1, ECDA_NLOCS
!
!           Check whether data for this location are included in the selected outputs
            IF( .NOT. ECDA_LOC(ILOC)%COMP ) CYCLE
!
!           Get the concentration data index
!
            CALL ECDA_RECNO_INDEX( ITIM, ILOC, MED_IDX, IDX, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine getting ECDA data index'
              MESSAG(2) = 'Location ID is '//ECDA_LOC(ILOC)%ID
              MESSAG(3) = 'Time index is '
              WRITE(MESSAG(3)(17:),*) ITIM
              MESSAG(4) = 'Media index is '
              WRITE(MESSAG(4)(17:),*) MED_IDX
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
!
            IF( IDX .LT. 0) THEN  ! PWE26
              IERR = 999
              MESSAG(1) = 'Error in lower level routine getting ECDA data index'
              MESSAG(2) = 'INDEX is '
              WRITE(MESSAG(2)(10:),*) IDX
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!
! ***       Read the values from the concentration file for a single (media, location, time)
            CALL ECDA_READ( IDX, C_YEAR, C_LOC_ID, C_MED_ID, CONC, ECDA_NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR(IERR, CALLER, 1)
              RETURN
            END IF
            IF( C_YEAR .NE. ECDA_TIMES(ITIM)%TIME ) THEN
              IERR = 3
              MESSAG(1) = 'The time are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( C_LOC_ID .NE. ECDA_LOC(ILOC)%ID ) THEN
              IERR = 4
              MESSAG(1) = 'The locations are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF              
            IF( C_MED_ID .NE. MED_USE ) THEN
              IERR = 5
              MESSAG(1) = 'The media are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
            ICNT = ICNT + 1
            DET_VALUE(ICNT) = CONC(REL_USE)
!
          END DO ! Locations
!
! ***     DIfferent actions for the first year that is output and later years
!
          IF( FIRST_YEAR ) THEN
!
!           Output two lines of information for the every year
            WRITE(IRES,50) ECDA_TIMES(ITIM)%TIME
   50       FORMAT(2X,'ZONE T="',I0,'"')
            WRITE(IRES,41) NUM_COR, NUM_CON   ! ECEM_NUM_LOC ==> NUM_LOC
   41       FORMAT(2x,'N=',I0,',',2X,'E=',I0,',F=FEPOINT ET=triangle')
!           Loop over the coordinates and output data
            ICNT = 0
            DO ILOC = 1, ECDA_NLOCS
              IF( .NOT. ECDA_LOC(ILOC)%COMP ) CYCLE
              ICNT = ICNT + 1
              WRITE(IRES,80) ECDA_LOC(ILOC)%EASTING, ECDA_LOC(ILOC)%NORTHING, ICNT, DET_VALUE(ICNT)*CONVERT_UNITS
   80         FORMAT(1P,E13.6,:,2X,E13.6,2X,0P,I0,2X,1P,E13.6)
            END DO
!
!           Read conectivity and write in the output file
            DO I = 1, NUM_CON
              READ(UNIT=ICOV, FMT=*) I1, I2, I3
              WRITE(IRES,'(I0,2(2X,I0))') I1, I2, I3
            ENDDO
            CLOSE( ICOV )
            FIRST_YEAR = .FALSE.
!
          ELSE ! After the first year
!      
!           Output two lines of information for the every year
            WRITE(IRES,51) ECDA_TIMES(ITIM)%TIME
   51       FORMAT(2X,'ZONE T="',I0,'"')
            WRITE(IRES,61) NUM_COR, NUM_CON   ! ECEM_NUM_LOC ==> NUM_LOC
   61       FORMAT(2X,'N=',I0,',',2X,'E=',I0,',F=FEPOINT ET=triangle, D=(1,2,3,FECONNECT)')
!           Loop over the coordinates and output data
            DO I = 1, NUM_COR
              WRITE(IRES,80) DET_VALUE(I)*CONVERT_UNITS
            END DO
!
          END IF ! First year check
!
        END DO ! Time loop
!
      END IF
!
!--------------------------------------------------------------------------------------
!     Riparian
!--------------------------------------------------------------------------------------
!
       IF( LOC_RIPARIAN )  THEN
!      IF( LOC_RIPARIAN .OR. LOC_AQUATIC ) THEN
!
! ***   Opening lines in the Tecplot output file
!
        WRITE(IRES,60) TRIM(PTITLE)
        WRITE(IRES,170) TRIM(UNITS_OUTPUT)
   170  FORMAT(2x,'VARIABLES = "MILE","',A,'"')
!
!       Reading details file one year at a time
        DO ITIM = 1, ECDA_NTIMES
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. ECDA_TIMES(ITIM)%COMP ) CYCLE
!
          ICNT = 0
          DO ILOC = 1, ECDA_NLOCS
!
!           Check whether data for this location are included in the selected outputs
            IF( .NOT. ECDA_LOC(ILOC)%COMP ) CYCLE
!
!           Get the concentration data index
!
            CALL ECDA_RECNO_INDEX( ITIM, ILOC, MED_IDX, IDX, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine getting ECDA data index'
              MESSAG(2) = 'Location ID is '//ECDA_LOC(ILOC)%ID
              MESSAG(3) = 'Time index is '
              WRITE(MESSAG(3)(17:),*) ITIM
              MESSAG(4) = 'Media index is '
              WRITE(MESSAG(4)(17:),*) MED_IDX
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
!
            IF( IDX .LT. 0) THEN  ! PWE26
              IERR = 999
              MESSAG(1) = 'Error in lower level routine getting ECDA data index'
              MESSAG(2) = 'INDEX is '
              WRITE(MESSAG(2)(10:),*) IDX
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!
! ***       Read the values from the concentration file for a single (media, location, time)
            CALL ECDA_READ( IDX, C_YEAR, C_LOC_ID, C_MED_ID, CONC, ECDA_NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR(IERR, CALLER, 1)
              RETURN
            END IF
            IF( C_YEAR .NE. ECDA_TIMES(ITIM)%TIME ) THEN
              IERR = 6
              MESSAG(1) = 'The time are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( C_LOC_ID .NE. ECDA_LOC(ILOC)%ID ) THEN
              IERR = 7
              MESSAG(1) = 'The locations are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF              
            IF( C_MED_ID .NE. MED_USE ) THEN
              IERR = 8
              MESSAG(1) = 'The media are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
            ICNT = ICNT + 1
            DET_VALUE(ICNT) = CONC(REL_USE)
!
          END DO ! Locations
!
!         Output information for the every year
          WRITE(IRES,51) ECDA_TIMES(ITIM)%TIME
!         Loop over the coordinates and output data
          ICNT = 0
          DO ILOC = 1, ECDA_NLOCS
            IF( .NOT. ECDA_LOC(ILOC)%COMP ) CYCLE
            ICNT = ICNT + 1
            WRITE(IRES,80) ECDA_LOC(ILOC)%MILE, DET_VALUE(ICNT)*CONVERT_UNITS
          END DO
!
        END DO
!
      ENDIF
!--------------------------------------------------------------------------------------
!     Aquatic data type processing
!--------------------------------------------------------------------------------------
!
      IF( LOC_AQUATIC ) THEN
!
! ***   Opening lines in the Tecplot output file
!
        WRITE(IRES,60) TRIM(PTITLE)
        WRITE(IRES,170) TRIM(UNITS_OUTPUT)
!   170  FORMAT(2x,'VARIABLES = "MILE","',A,'"')
!
!       Reading details file one year at a time
        DO ITIM = 1, ECDA_NTIMES
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. ECDA_TIMES(ITIM)%COMP ) CYCLE
!
          ICNT = 0
          DO ILOC = 1, ECDA_NLOCS
!
!           Check whether data for this location are included in the selected outputs
            IF( .NOT. ECDA_LOC(ILOC)%COMP ) CYCLE
!
!           Get the concentration data index
!
            CALL ECDA_RECNO_INDEX( ITIM, ILOC, MED_IDX, IDX, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine getting ECDA data index'
              MESSAG(2) = 'Location ID is '//ECDA_LOC(ILOC)%ID
              MESSAG(3) = 'Time index is '
              WRITE(MESSAG(3)(17:),*) ITIM
              MESSAG(4) = 'Media index is '
              WRITE(MESSAG(4)(17:),*) MED_IDX
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
!
            IF( IDX .LT. 0) THEN  ! PWE26
              IERR = 999
              MESSAG(1) = 'Error in lower level routine getting ECDA data index'
              MESSAG(2) = 'INDEX is '
              WRITE(MESSAG(2)(10:),*) IDX
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!
! ***       Read the values from the concentration file for a single (media, location, time)
            CALL ECDA_READ( IDX, C_YEAR, C_LOC_ID, C_MED_ID, CONC, ECDA_NREAL, ICON, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR(IERR, CALLER, 1)
              RETURN
            END IF
            IF( C_YEAR .NE. ECDA_TIMES(ITIM)%TIME ) THEN
              IERR = 3
              MESSAG(1) = 'The time are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( C_LOC_ID .NE. ECDA_LOC(ILOC)%ID ) THEN
              IERR = 4
              MESSAG(1) = 'The locations are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF              
            IF( C_MED_ID .NE. MED_USE ) THEN
              IERR = 5
              MESSAG(1) = 'The media are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
!
            ICNT = ICNT + 1
            DET_VALUE(ICNT) = CONC(REL_USE)
!
          END DO ! Locations
!
!         Output information for the every year
          WRITE(IRES,51) ECDA_TIMES(ITIM)%TIME
!         Loop over the coordinates and output data
          ICNT = 0
          DO ILOC = 1, ECDA_NLOCS
            IF( .NOT. ECDA_LOC(ILOC)%COMP ) CYCLE
            ICNT = ICNT + 1
            WRITE(IRES,80) ECDA_LOC(ILOC)%MILE, DET_VALUE(ICNT)*CONVERT_UNITS
          END DO
!
        END DO
!
      ENDIF
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE PROCESS_ECEM( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This program creates a data file to be used in TecPlot for
!!    animations.  It operates on only one realization at a time.
!!    (CATEGORY ECEM
!!
!!  History:
!!
!!    Carmen Arimescu : 7 DEC 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE ECEM_Mod
      USE Rdblk_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
      INTEGER :: IERR ! Error flag
!
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
!
      CHARACTER(LEN=12) :: CALLER = 'PROCESS_ECEM'
      LOGICAL :: FIRST_YEAR ! Flag for processing the first year
      REAL, ALLOCATABLE :: DET_VALUE(:)
!
      INTEGER :: I1, I2, I3  !Connectivity
      INTEGER :: I
      CHARACTER(LEN=LENQQQ) :: HEADER
      INTEGER :: IOS ! Status variable for open statement
      INTEGER :: ITIM, ICNT, ILOC
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      IDET = GET_UNIT_NUMBER(  )
      IF( IDET .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the detail file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the ECEM Details file
!
      OPEN(UNIT=IDET, IOSTAT=IOS, STATUS='OLD', FILE=FNDET )
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error opening detail  file'
        WRITE(MESSAG(2), *) 'UNIT = ', IDET, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNDET = ' // TRIM(FNDET)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
      READ(UNIT=IDET,FMT='(A)') HEADER
!
! *** Get a unit number for output file
!
      IRES = GET_UNIT_NUMBER(  )
      IF( IRES .LT. 7 ) THEN
        IERR = 3
        MESSAG(1) = 'Unable to get unit number for the output  file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output file
!
      OPEN(UNIT=IRES, IOSTAT=IOS, FILE=FNRES )
      IF( IOS .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error opening output file'
        WRITE(MESSAG(2), *) 'UNIT = ', IRES, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNRES = ' // TRIM(FNRES)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Allocate space for the details vector
!
      FIRST_YEAR = .TRUE.
      ALLOCATE(DET_VALUE(NUM_COR))
!
!--------------------------------------------------------------------------------------
!     Upland data type processing
!--------------------------------------------------------------------------------------
!
      IF( LOC_UPLAND ) THEN
!       UPLAND Locations require a connectivity file
!
! ***   Get a unit number for connectivity file
!
        ICOV = GET_UNIT_NUMBER(  )
        IF( ICOV .LT. 7 ) THEN
          IERR = 5
          MESSAG(1) = 'Unable to get unit number for the connectivity file'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!       
! ***   Open the connectivity file
!
        OPEN(UNIT=ICOV, IOSTAT=IOS, STATUS='OLD', FILE=FNCOV )
        IF( IOS .NE. 0 ) THEN
          IERR = 6
          MESSAG(1) = 'Error opening connectivity file'
          WRITE(MESSAG(2), *) 'UNIT = ', ICOV, ' IOSTAT = ', IOS
          MESSAG(3) = 'FNCOV = ' // TRIM(FNCOV)
          CALL PRTERR(IERR, CALLER, 3)
          RETURN
        END IF
!
        NUM_CON=0
   10   CONTINUE
          READ(UNIT=ICOV,FMT=*, END=20) I1, I2, I3
          NUM_CON = NUM_CON + 1
          GO TO 10
!
   20   CONTINUE
!
        REWIND(ICOV)
!
! ***   Opening lines in the Tecplot output file
!
         WRITE(IRES,660) TRIM(LEGEND_TITLE)
         WRITE(IRES,770) TRIM(LEGEND_CONTOUR)
   660   FORMAT(2x,'TITLE     = "',A,'"')
   770   FORMAT(2x,'VARIABLES = "Easting","Northing","NODE","',A,'"')
!
   60   FORMAT(2x,'TITLE     = "',A,'"')
!
!       Reading detail file one year at a time
        DO ITIM = 1, ECEM_NUM_TIM
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. ECEM_TIM(ITIM)%COMP ) CYCLE
!
! ***     Read the values from the details file for a single year
          CALL READ_ECEM_DET( ITIM, DET_VALUE, IERR )
          IF(IERR .NE.0) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR(IERR, CALLER, 1)
            RETURN
          END IF
!
! ***     DIfferent actions for the first year that is output and later years
!
          IF( FIRST_YEAR ) THEN
!
!           Output two lines of information for the every year
            WRITE(IRES,50) ECEM_TIM(ITIM)%TIME
   50       FORMAT(2X,'ZONE T="',I0,'"')
            WRITE(IRES,41) NUM_COR, NUM_CON   ! ECEM_NUM_LOC ==> NUM_LOC
   41       FORMAT(2x,'N=',I0,',',2X,'E=',I0,',F=FEPOINT ET=triangle')
!           Loop over the coordinates and output data
            ICNT = 0
            DO ILOC = 1, ECEM_NUM_LOC
              IF( .NOT. ECEM_LOC(ILOC)%COMP ) CYCLE
              ICNT = ICNT + 1
              WRITE(IRES,80) ECEM_LOC(ILOC)%EASTING, ECEM_LOC(ILOC)%NORTHING, ICNT, DET_VALUE(ICNT)*CONVERT_UNITS
   80         FORMAT(1P,E13.6,:,2X,E13.6,2X,0P,I0,2X,1P,E13.6)
            END DO
!
!           Read conectivity and write in the output file
            DO I = 1, NUM_CON
              READ(UNIT=ICOV, FMT=*) I1, I2, I3
              WRITE(IRES,'(I0,2(2X,I0))') I1, I2, I3
            ENDDO
            CLOSE( ICOV )
            FIRST_YEAR = .FALSE.
!
          ELSE ! After the first year
!      
!           Output two lines of information for the every year
            WRITE(IRES,51) ECEM_TIM(ITIM)%TIME
   51       FORMAT(2X,'ZONE T="',I0,'"')
            WRITE(IRES,61) NUM_COR, NUM_CON   ! ECEM_NUM_LOC ==> NUM_LOC
   61       FORMAT(2X,'N=',I0,',',2X,'E=',I0,',F=FEPOINT ET=triangle, D=(1,2,3,FECONNECT)')
!           Loop over the coordinates and output data
            DO I = 1, NUM_COR
              WRITE(IRES,80) DET_VALUE(I)*CONVERT_UNITS
            END DO
!
          END IF ! First year check
!
        END DO ! Time loop
!
      END IF
!
!--------------------------------------------------------------------------------------
!     Riparian data type processing
!--------------------------------------------------------------------------------------
!
       IF( LOC_RIPARIAN )THEN
!      IF( LOC_RIPARIAN .OR. LOC_AQUATIC ) THEN
!
! ***   Opening lines in the Tecplot output file
!
        WRITE(IRES,60) TRIM(PTITLE)
        WRITE(IRES,170) TRIM(UNITS_OUTPUT)
   170  FORMAT(2x,'VARIABLES = "MILE","',A,'"')
!
!       Reading details file one year at a time
        DO ITIM = 1, ECEM_NUM_TIM
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. ECEM_TIM(ITIM)%COMP ) CYCLE
!
! ***     Read the values from the details file for a single year
          CALL READ_ECEM_DET( ITIM, DET_VALUE, IERR )
          IF(IERR .NE.0) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR(IERR, CALLER, 1)
            RETURN
          END IF
!
!         Output information for the every year
          WRITE(IRES,51) ECEM_TIM(ITIM)%TIME
!         Loop over the coordinates and output data
          ICNT = 0
          DO ILOC = 1, ECEM_NUM_LOC
            IF( .NOT. ECEM_LOC(ILOC)%COMP ) CYCLE
            ICNT = ICNT + 1
            WRITE(IRES,80) ECEM_LOC(ILOC)%MILE, DET_VALUE(ICNT)*CONVERT_UNITS
          END DO
!
        END DO
!
      ENDIF
!
!--------------------------------------------------------------------------------------
!     Aquatic data type processing
!--------------------------------------------------------------------------------------
!
       IF( LOC_AQUATIC )THEN
!      IF( LOC_RIPARIAN .OR. LOC_AQUATIC ) THEN
!
! ***   Opening lines in the Tecplot output file
!
        WRITE(IRES,60) TRIM(PTITLE)
        WRITE(IRES,170) TRIM(UNITS_OUTPUT)
!   170  FORMAT(2x,'VARIABLES = "MILE,"',A,'"')
!
!       Reading details file one year at a time
        DO ITIM = 1, ECEM_NUM_TIM
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. ECEM_TIM(ITIM)%COMP ) CYCLE
!
! ***     Read the values from the details file for a single year
          CALL READ_ECEM_DET( ITIM, DET_VALUE, IERR )
          IF(IERR .NE.0) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR(IERR, CALLER, 1)
            RETURN
          END IF
!
!         Output information for the every year
          WRITE(IRES,51) ECEM_TIM(ITIM)%TIME
!         Loop over the coordinates and output data
          ICNT = 0
          DO ILOC = 1, ECEM_NUM_LOC
            IF( .NOT. ECEM_LOC(ILOC)%COMP ) CYCLE
            ICNT = ICNT + 1
            WRITE(IRES,80) ECEM_LOC(ILOC)%MILE, DET_VALUE(ICNT)*CONVERT_UNITS
          END DO
!
        END DO
!
      ENDIF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PROCESS_FCDA( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This program creates a file ready to be use in TecPlot for
!!    animations. It operates on only 1 realization at a time.
!!    (CATEGORY FCDA)
!!
!!  History:
!!
!!    Carmen Arimescu  : 11 Jan 2005 : Version 1.0
!!    Paul W. Eslinger :  5 Jun 2007 : Fix error traps
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE FCDA_Mod
      USE Rdblk_Mod
!
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR ! Error flag
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'PROCESS_FCDA' ! Name of this subroutine
!
      LOGICAL :: FIRST_YEAR ! Flag for processing the first year
      REAL, ALLOCATABLE :: DET_VALUE(:)
!
      INTEGER :: F_YEAR            ! Year from FCDA data file
      CHARACTER(LEN=6) :: F_LOC_ID ! Location from FCDA data file
 !
      CHARACTER(LEN=6) :: FCDA_ANA_ID   ! Analyte ID: First header line
      CHARACTER(LEN=6) :: FCDA_SPC_ID   ! Species ID: Second header line
      CHARACTER(LEN=8) :: FCDA_SPC_HAB  ! Species habitat
!
      INTEGER :: I1, I2, I3  !Connectivity
      INTEGER :: I, IDX
!      CHARACTER(LEN=LENQQQ) :: HEADER
      INTEGER :: IOS ! Status variable for open statement
      INTEGER :: ITIM, ICNT, ILOC
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number for the food file
      IFOD = GET_UNIT_NUMBER(  )
      IF( IFOD .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the FCDA concentration file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the FOOD file
      CALL FCDA_OPEN( FNFOD, IFOD, FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the FCDA concentration file'
        MESSAG(2) = 'File: ' // TRIM(FNFOD)
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     Check the data in the first two lines
!
      IF( FCDA_ANA_ID .NE. ANA_USE ) THEN
        IERR = 3
        MESSAG(1) = 'Error matching FCDA file with the requested analyte'
        MESSAG(2) = 'File: '//TRIM(FNFOD)
        MESSAG(3) = 'Requested: '//TRIM(ANA_USE)
        MESSAG(4) = 'In File:   '//TRIM(FCDA_ANA_ID)
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
      IF( FCDA_SPC_ID .NE. SPC_USE ) THEN
        IERR = 4
        MESSAG(1) = 'Error matching FCDA file with the requested species'
        MESSAG(2) = 'File: '//TRIM(FNFOD)
        MESSAG(3) = 'Requested: '//TRIM(SPC_USE)
        MESSAG(4) = 'In File:   '//TRIM(FCDA_SPC_ID)
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Get a unit number for the output file
!
      IRES = GET_UNIT_NUMBER(  )
      IF( IRES .LT. 7 ) THEN
        IERR = 5
        MESSAG(1) = 'Unable to get unit number for the output file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output file
!
      OPEN(UNIT=IRES, IOSTAT=IOS, FILE=FNRES )
      IF( IOS .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error opening output file'
        WRITE(MESSAG(2), *) 'UNIT = ', IRES, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNRES = ' // TRIM(FNRES)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Allocate space for the details vector
!
      FIRST_YEAR = .TRUE.
      ALLOCATE(DET_VALUE(NUM_COR))
      DET_VALUE = 0.0
!
!--------------------------------------------------------------------------------------
!     Upland data type processing
!--------------------------------------------------------------------------------------
!
      IF( LOC_UPLAND ) THEN  ! UPLAND Locations require a connectivity file
!
! ***   Get a unit number for connectivity file
        ICOV = GET_UNIT_NUMBER(  )
        IF( ICOV .LT. 7 ) THEN
          IERR = 7
          MESSAG(1) = 'Unable to get unit number for the connectivity file'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!       
! ***   Open the connectivity file
        OPEN(UNIT=ICOV, IOSTAT=IOS, STATUS='OLD', FILE=FNCOV )
        IF( IOS .NE. 0 ) THEN
          IERR = 8
          MESSAG(1) = 'Error opening connectivity file'
          WRITE(MESSAG(2), *) 'UNIT = ', ICOV, ' IOSTAT = ', IOS
          MESSAG(3) = 'FNCOV = ' // TRIM(FNCOV)
          CALL PRTERR(IERR, CALLER, 3)
          RETURN
        END IF
!
        NUM_CON=0
   10   CONTINUE
          READ(UNIT=ICOV,FMT=*,END=20) I1, I2, I3
          NUM_CON = NUM_CON + 1
          GO TO 10
!
   20   CONTINUE
!
        REWIND(ICOV)
!
! ***   Opening lines in the Tecplot output file
!
        WRITE(IRES,660) TRIM(LEGEND_TITLE)
        WRITE(IRES,770) TRIM(LEGEND_CONTOUR)
    660 FORMAT(2x,'TITLE     = "',A,'"')
    770 FORMAT(2x,'VARIABLES = "Easting","Northing","NODE","',A,'"')
!
   60   FORMAT(2x,'TITLE     = "',A,'"')
!
!       Reading detail file one year at a time
        DO ITIM = 1, FCDA_NTIMES
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. FCDA_TIM(ITIM)%COMP ) CYCLE
          IF( .NOT. FCDA_TIM(ITIM)%TEC_COMP ) CYCLE
!
! ***     Read the food file
!
          ICNT = 0
          DO ILOC = 1, FCDA_NLOCS
!
!           Check whether data for this location are included in the selected outputs
            IF( .NOT. FCDA_LOC(ILOC)%COMP ) CYCLE
            IF( .NOT. FCDA_LOC(ILOC)%TEC_COMP ) CYCLE
!
!           Get the concentration data index
            CALL FCDA_RECNO_UP( ITIM, ILOC, SPC_IDX, STY_IDX, IDX, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine getting FCDA data index'
              MESSAG(2) = 'Location ID is '//FCDA_LOC(ILOC)%ID
              MESSAG(3) = 'SPECIES ID is '// SPC_USE
              MESSAG(4) = 'Time index is '
              WRITE(MESSAG(4)(16:),*) ITIM
              MESSAG(5) = 'SOIL index is '
              WRITE(MESSAG(5)(16:),*) STY_IDX
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
!           Check whether data were available
            IF( IDX .LT. 0 ) THEN
              IERR = 9
              MESSAG(1) = 'IDX was unexpectedly less than zero - food data not available'
              MESSAG(2) = 'Location ID is '//FCDA_LOC(ILOC)%ID
              MESSAG(3) = 'SPECIES ID is '// SPC_USE
              MESSAG(4) = 'Time index is '
              WRITE(MESSAG(4)(16:),*) ITIM
              MESSAG(5) = 'SOIL index is '
              WRITE(MESSAG(5)(16:),*) STY_IDX
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
! ***       Read the values from the concentration file for a single (location, time)
            CALL FCDA_READ( IDX, F_YEAR, F_LOC_ID, CONC, FCDA_NREAL, IFOD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR(IERR, CALLER, 1)
              RETURN
            END IF
            IF( F_YEAR .NE. FCDA_TIM(ITIM)%TIME ) THEN
              IERR = 10
              MESSAG(1) = 'The time are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( F_LOC_ID .NE. FCDA_LOC(ILOC)%ID ) THEN
              IERR = 11
              MESSAG(1) = 'The locations are not correct values'
              MESSAG(2) = FCDA_LOC(ILOC)%ID // ' : Reqested location'
              MESSAG(3) = F_LOC_ID // ' : Obtained from FCDA file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF              
!
            ICNT = ICNT + 1
            DET_VALUE(ICNT) = CONC(REL_USE)
!
          END DO ! Locations
!
! ***     DIfferent actions for the first year that is output and later years
!
          IF( FIRST_YEAR ) THEN
!
!           Output two lines of information for the every year
            WRITE(IRES,50) FCDA_TIM(ITIM)%TIME
   50       FORMAT(2X,'ZONE T="',I0,'"')
            WRITE(IRES,41) NUM_COR, NUM_CON
   41       FORMAT(2x,'N=',I0,',',2X,'E=',I0,',F=FEPOINT ET=triangle')
!           Loop over the coordinates and output data
            ICNT = 0
            DO ILOC = 1, FCDA_NLOCS
              IF( .NOT. FCDA_LOC(ILOC)%COMP ) CYCLE
              IF( .NOT. FCDA_LOC(ILOC)%TEC_COMP ) CYCLE
              ICNT = ICNT + 1
              WRITE(IRES,80) FCDA_LOC(ILOC)%EASTING, FCDA_LOC(ILOC)%NORTHING, ICNT, DET_VALUE(ICNT)*CONVERT_UNITS
   80         FORMAT(1P,E13.6,:,2X,E13.6,2X,0P,I0,2X,1P,E13.6)
            END DO
!
!           Read conectivity and write in the output file
            DO I = 1, NUM_CON
              READ(UNIT=ICOV, FMT=*) I1, I2, I3
              WRITE(IRES,'(I0,2(2X,I0))') I1, I2, I3
            ENDDO
            CLOSE( ICOV )
            FIRST_YEAR = .FALSE.
!
          ELSE ! After the first year
!      
!           Output two lines of information for the every year
            WRITE(IRES,51) FCDA_TIM(ITIM)%TIME
   51       FORMAT(2X,'ZONE T="',I0,'"')
            WRITE(IRES,61) NUM_COR, NUM_CON
   61       FORMAT(2X,'N=',I0,',',2X,'E=',I0,',F=FEPOINT ET=triangle, D=(1,2,3,FECONNECT)')
!           Loop over the coordinates and output data
            DO I = 1, NUM_COR
              WRITE(IRES,80) DET_VALUE(I)*CONVERT_UNITS
            END DO
!
          END IF ! First year check
!
        END DO ! Time loop
!
      END IF
!
!--------------------------------------------------------------------------------------
!     Riparian data type processing
!--------------------------------------------------------------------------------------
!
      IF( LOC_RIPARIAN) THEN
!
! ***   Opening lines in the Tecplot output file
!
        WRITE(IRES,60) TRIM(PTITLE)
        WRITE(IRES,170) TRIM(UNITS_OUTPUT)
   170  FORMAT(2x,'VARIABLES = "MILE","',A,'"')
!
!       Reading details file one year at a time
        DO ITIM = 1, FCDA_NTIMES
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. FCDA_TIM(ITIM)%COMP ) CYCLE
          IF( .NOT. FCDA_TIM(ITIM)%TEC_COMP ) CYCLE
!
! ***     Read the food file
!
          ICNT = 0
          DO ILOC = 1, FCDA_NLOCS
!
!           Check whether data for this location are included in the selected outputs
            IF( .NOT. FCDA_LOC(ILOC)%COMP ) CYCLE
            IF( .NOT. FCDA_LOC(ILOC)%TEC_COMP ) CYCLE
!
!           Get the concentration data index
!
            CALL FCDA_RECNO_RP( ITIM, ILOC, SPC_IDX, IDX, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine getting FCDA data index'
              MESSAG(2) = 'Location ID is '//FCDA_LOC(ILOC)%ID
              MESSAG(2) = 'SPECIES ID is '//SPC_USE
              MESSAG(3) = 'Time index is '
              WRITE(MESSAG(3)(16:),*) ITIM
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
!
!           Check whether data were available
            IF( IDX .LT. 0 ) THEN
              IERR = 9
              MESSAG(1) = 'IDX was unexpectedly less than zero - food data not available'
              MESSAG(2) = 'Location ID is '//FCDA_LOC(ILOC)%ID
              MESSAG(3) = 'SPECIES ID is '// SPC_USE
              MESSAG(4) = 'Time index is '
              WRITE(MESSAG(4)(16:),*) ITIM
              MESSAG(5) = 'Species index is '
              WRITE(MESSAG(5)(19:),*) SPC_IDX
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
! ***       Read the values from the concentration file for a single (location, time)
            CALL FCDA_READ( IDX, F_YEAR, F_LOC_ID, CONC, FCDA_NREAL, IFOD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR(IERR, CALLER, 1)
              RETURN
            END IF
!
            IF( F_YEAR .NE. FCDA_TIM(ITIM)%TIME ) THEN
              IERR = 3
              MESSAG(1) = 'The time are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( F_LOC_ID .NE. FCDA_LOC(ILOC)%ID ) THEN
              IERR = 4
              MESSAG(1) = 'The locations are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF              
!
            ICNT = ICNT + 1
            DET_VALUE(ICNT) = CONC(REL_USE)
!
          END DO ! Locations
!
!         Output information for the every year
          WRITE(IRES,51) FCDA_TIM(ITIM)%TIME
!
!         Loop over the coordinates and output data
          ICNT = 0
          DO ILOC = 1,FCDA_NLOCS
            IF( .NOT. FCDA_LOC(ILOC)%COMP ) CYCLE
            IF( .NOT. FCDA_LOC(ILOC)%TEC_COMP ) CYCLE
            ICNT = ICNT + 1
            WRITE(IRES,80) FCDA_LOC(ILOC)%MILE, DET_VALUE(ICNT)*CONVERT_UNITS
          END DO
!
        END DO
!
      ENDIF
!
!--------------------------------------------------------------------------------------
!     Aquatic data type processing
!--------------------------------------------------------------------------------------
!
      IF( LOC_AQUATIC ) THEN
!
! ***   Opening lines in the Tecplot output file
!
        WRITE(IRES,60) TRIM(PTITLE)
        WRITE(IRES,180) TRIM(UNITS_OUTPUT)
   180  FORMAT(2x,'VARIABLES = "MILE","',A,'"')
!
!       Reading details file one year at a time
        DO ITIM = 1, FCDA_NTIMES
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. FCDA_TIM(ITIM)%COMP ) CYCLE
          IF( .NOT. FCDA_TIM(ITIM)%TEC_COMP ) CYCLE
!
! ***     Read the food file
!
          ICNT = 0
          DO ILOC = 1, FCDA_NLOCS
!
!           Check whether data for this location are included in the selected outputs
            IF( .NOT. FCDA_LOC(ILOC)%COMP ) CYCLE
            IF( .NOT. FCDA_LOC(ILOC)%TEC_COMP ) CYCLE
!
!           Get the concentration data index
!
            CALL FCDA_RECNO_AQ( ITIM, ILOC, SPC_IDX, IDX, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine getting FCDA data index'
              MESSAG(2) = 'Location ID is '//FCDA_LOC(ILOC)%ID
              MESSAG(2) = 'SPECIES ID is '//SPC_USE
              MESSAG(3) = 'Time index is '
              WRITE(MESSAG(3)(16:),*) ITIM
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
!
!           Check whether data were available
            IF( IDX .LT. 0 ) THEN
              IERR = 9
              MESSAG(1) = 'IDX was unexpectedly less than zero - food data not available'
              MESSAG(2) = 'Location ID is '//FCDA_LOC(ILOC)%ID
              MESSAG(3) = 'SPECIES ID is '// SPC_USE
              MESSAG(4) = 'Time index is '
              WRITE(MESSAG(4)(16:),*) ITIM
              MESSAG(5) = 'Species index is '
              WRITE(MESSAG(5)(19:),*) SPC_IDX
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
!
! ***       Read the values from the concentration file for a single (location, time)
            CALL FCDA_READ( IDX, F_YEAR, F_LOC_ID, CONC, FCDA_NREAL, IFOD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine'
              CALL PRTERR(IERR, CALLER, 1)
              RETURN
            END IF
            IF( F_YEAR .NE. FCDA_TIM(ITIM)%TIME ) THEN
              IERR = 3
              MESSAG(1) = 'The time are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            IF( F_LOC_ID .NE. FCDA_LOC(ILOC)%ID ) THEN
              IERR = 4
              MESSAG(1) = 'The locations are not correct values'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF              
!
            ICNT = ICNT + 1
            DET_VALUE(ICNT) = CONC(REL_USE)
!
          END DO ! Locations
!
!         Output information for the every year
          WRITE(IRES,51) FCDA_TIM(ITIM)%TIME
!         Loop over the coordinates and output data
          ICNT = 0
          DO ILOC = 1, FCDA_NLOCS
            IF( .NOT. FCDA_LOC(ILOC)%COMP ) CYCLE
            IF( .NOT. FCDA_LOC(ILOC)%TEC_COMP ) CYCLE
            ICNT = ICNT + 1
            WRITE(IRES,80) FCDA_LOC(ILOC)%MILE, DET_VALUE(ICNT)*CONVERT_UNITS
          END DO
!
        END DO
!
      ENDIF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PROCESS_HUMAN( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This program creates a data file to be used in TecPlot for
!!    animations.  It operates on only one realization at a time.
!!    (CATEGORY HUMAN)
!!
!!  History:
!!
!!    Carmen Arimescu : 29 NOV 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE HUMAN_Mod
      USE Rdblk_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
      INTEGER :: IERR ! Error flag
!
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
!
      CHARACTER(LEN=13) :: CALLER = 'PROCESS_HUMAN'
      LOGICAL :: FIRST_YEAR ! Flag for processing the first year
      REAL, ALLOCATABLE :: DET_VALUE(:)
!
      INTEGER :: I1, I2, I3  !Connectivity
      INTEGER :: I
      CHARACTER(LEN=LENQQQ) :: HEADER
      INTEGER :: IOS ! Status variable for open statement
      INTEGER :: ITIM, ICNT, ILOC
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      IDET = GET_UNIT_NUMBER(  )
      IF( IDET .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the detail file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the HUMAN Details file
!
      OPEN(UNIT=IDET, IOSTAT=IOS, STATUS='OLD', FILE=FNDET )
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error opening detail  file'
        WRITE(MESSAG(2), *) 'UNIT = ', IDET, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNDET = ' // TRIM(FNDET)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
      READ(UNIT=IDET,FMT='(A)') HEADER
!
! *** Get a unit number for output file
!
      IRES = GET_UNIT_NUMBER(  )
      IF( IRES .LT. 7 ) THEN
        IERR = 3
        MESSAG(1) = 'Unable to get unit number for the output  file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output file
!
      OPEN(UNIT=IRES, IOSTAT=IOS, FILE=FNRES )
      IF( IOS .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error opening output file'
        WRITE(MESSAG(2), *) 'UNIT = ', IRES, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNRES = ' // TRIM(FNRES)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Allocate space for the details vector
!
      FIRST_YEAR = .TRUE.
      ALLOCATE(DET_VALUE(NUM_COR))
!
!--------------------------------------------------------------------------------------
!     Upland data type processing
!--------------------------------------------------------------------------------------
!
      IF( LOC_UPLAND ) THEN
!       UPLAND Locations require a connectivity file
!
! ***   Get a unit number for connectivity file
!
        ICOV = GET_UNIT_NUMBER(  )
        IF( ICOV .LT. 7 ) THEN
          IERR = 5
          MESSAG(1) = 'Unable to get unit number for the connectivity file'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!       
! ***   Open the connectivity file
!
        OPEN(UNIT=ICOV, IOSTAT=IOS, STATUS='OLD', FILE=FNCOV )
        IF( IOS .NE. 0 ) THEN
          IERR = 6
          MESSAG(1) = 'Error opening connectivity file'
          WRITE(MESSAG(2), *) 'UNIT = ', ICOV, ' IOSTAT = ', IOS
          MESSAG(3) = 'FNCOV = ' // TRIM(FNCOV)
          CALL PRTERR(IERR, CALLER, 3)
          RETURN
        END IF
!
        NUM_CON=0
   10   CONTINUE
          READ(UNIT=ICOV,FMT=*, END=20) I1, I2, I3
          NUM_CON = NUM_CON + 1
          GO TO 10
!
   20   CONTINUE
!
        REWIND(ICOV)
!
         WRITE(IRES,660) TRIM(LEGEND_TITLE)
         WRITE(IRES,770) TRIM(LEGEND_CONTOUR)
    660   FORMAT(2x,'TITLE     = "',A,'"')
    770   FORMAT(2x,'VARIABLES = "Easting","Northing","NODE","',A,'"')
!
   60   FORMAT(2x,'TITLE     = "',A,'"')
!
!       Reading detail file one year at a time
        DO ITIM = 1, HUMAN_NUM_TIM
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. HUMAN_TIM(ITIM)%COMP ) CYCLE
!
! ***     Read the values from the details file for a single year
          CALL READ_HUMAN_DET( ITIM, DET_VALUE, IERR )
          IF(IERR .NE.0) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR(IERR, CALLER, 1)
            RETURN
          END IF
!
! ***     DIfferent actions for the first year that is output and later years
!
          IF( FIRST_YEAR ) THEN
!
!           Output two lines of information for the every year
            WRITE(IRES,50) HUMAN_TIM(ITIM)%TIME
   50       FORMAT(2X,'ZONE T="',I0,'"')
            WRITE(IRES,41) NUM_COR, NUM_CON   ! HUMAN_NUM_LOC ==> NUM_LOC
   41       FORMAT(2x,'N=',I0,',',2X,'E=',I0,',F=FEPOINT ET=triangle')
!
!           Loop over the coordinates and output data
            ICNT = 0
            DO ILOC = 1, HUMAN_NUM_LOC
              IF( .NOT. HUMAN_LOC(ILOC)%COMP ) CYCLE
              ICNT = ICNT + 1
              WRITE(IRES,80) HUMAN_LOC(ILOC)%EASTING, HUMAN_LOC(ILOC)%NORTHING, ICNT, DET_VALUE(ICNT)*CONVERT_UNITS
   80         FORMAT(1P,E13.6,:,2X,E13.6,2X,0P,I0,2X,1P,E13.6)
            END DO
!
!           Read conectivity and write in the output file
            DO I = 1, NUM_CON
              READ(UNIT=ICOV, FMT=*) I1, I2, I3
              WRITE(IRES,'(I0,2(2X,I0))') I1, I2, I3
            ENDDO
            CLOSE( ICOV )
            FIRST_YEAR = .FALSE.
!
          ELSE ! After the first year
!      
!           Output two lines of information for the every year
            WRITE(IRES,51) HUMAN_TIM(ITIM)%TIME
   51       FORMAT(2X,'ZONE T="',I0,'"')
            WRITE(IRES,61) NUM_COR, NUM_CON   ! HUMAN_NUM_LOC ==> NUM_LOC
   61       FORMAT(2X,'N=',I0,',',2X,'E=',I0,',F=FEPOINT ET=triangle, D=(1,2,3,FECONNECT)')
!           Loop over the coordinates and output data
            DO I = 1, NUM_COR
              WRITE(IRES,80) DET_VALUE(I)*CONVERT_UNITS
            END DO
!
          END IF ! First year check
!
        END DO ! Time loop
!
      END IF
!
!--------------------------------------------------------------------------------------
!     Riparian or Aquatic data type processing
!--------------------------------------------------------------------------------------
!
      IF( LOC_RIPARIAN .OR. LOC_AQUATIC ) THEN
!
! ***   Opening lines in the Tecplot output file
!
        WRITE(IRES,60) TRIM(PTITLE)
        WRITE(IRES,170) TRIM(UNITS_OUTPUT)
   170  FORMAT(2x,'VARIABLES = "MILE","',A,'"')
!
!       Reading details file one year at a time
        DO ITIM = 1, HUMAN_NUM_TIM
!
! ***     Read the values from the details file for a single year
          CALL READ_HUMAN_DET(ITIM, DET_VALUE, IERR )
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. HUMAN_TIM(ITIM)%COMP ) CYCLE
!
!         Output information for the every year
          WRITE(IRES,51) HUMAN_TIM(ITIM)%TIME
!         Loop over the coordinates and output data
          ICNT = 0
          DO ILOC = 1, HUMAN_NUM_LOC
            IF( .NOT. HUMAN_LOC(ILOC)%COMP ) CYCLE
            ICNT = ICNT + 1
            WRITE(IRES,80) HUMAN_LOC(ILOC)%MILE, DET_VALUE(ICNT)*CONVERT_UNITS
          END DO
!
        END DO
!
      ENDIF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE PROCESS_CULTURE( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This program creates a data file to be used in TecPlot for
!!    animations.  It operates on only one realization at a time.
!!    (CATEGORY HUMAN)
!!
!!  History:
!!
!!    Carmen Arimescu : 24 Apr 2006 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE CULTURE_Mod
      USE Rdblk_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
      INTEGER :: IERR ! Error flag
!
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
!
      CHARACTER(LEN=15) :: CALLER = 'PROCESS_CULTURE'
      LOGICAL :: FIRST_YEAR ! Flag for processing the first year
      REAL, ALLOCATABLE :: DET_VALUE(:)
!
      INTEGER :: I1, I2, I3  !Connectivity
      INTEGER :: I
      CHARACTER(LEN=LENQQQ) :: HEADER
      INTEGER :: IOS ! Status variable for open statement
      INTEGER :: ITIM, ICNT, ILOC
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      ICMAP = GET_UNIT_NUMBER(  )
      IF( ICMAP .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the map file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the CULTURE Details file
!
      OPEN(UNIT=ICMAP, IOSTAT=IOS, STATUS='OLD', FILE=FNCMAP )
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error opening the map file'
        WRITE(MESSAG(2), *) 'UNIT = ', ICMAP, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNDET = ' // TRIM(FNDET)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
      READ(UNIT=ICMAP,FMT='(A)') HEADER
!
! *** Get a unit number for output file
!
      IRES = GET_UNIT_NUMBER(  )
      IF( IRES .LT. 7 ) THEN
        IERR = 3
        MESSAG(1) = 'Unable to get unit number for the output  file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output file
!
      OPEN(UNIT=IRES, IOSTAT=IOS, FILE=FNRES )
      IF( IOS .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error opening output file'
        WRITE(MESSAG(2), *) 'UNIT = ', IRES, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNRES = ' // TRIM(FNRES)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Allocate space for the details vector
!
      FIRST_YEAR = .TRUE.
      ALLOCATE(DET_VALUE(NUM_COR))
!
!--------------------------------------------------------------------------------------
!     Upland data type processing
!--------------------------------------------------------------------------------------
!
      IF( LOC_UPLAND ) THEN
!       UPLAND Locations require a connectivity file
!
! ***   Get a unit number for connectivity file
!
        ICOV = GET_UNIT_NUMBER(  )
        IF( ICOV .LT. 7 ) THEN
          IERR = 5
          MESSAG(1) = 'Unable to get unit number for the connectivity file'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!       
! ***   Open the connectivity file
!
        OPEN(UNIT=ICOV, IOSTAT=IOS, STATUS='OLD', FILE=FNCOV )
        IF( IOS .NE. 0 ) THEN
          IERR = 6
          MESSAG(1) = 'Error opening connectivity file'
          WRITE(MESSAG(2), *) 'UNIT = ', ICOV, ' IOSTAT = ', IOS
          MESSAG(3) = 'FNCOV = ' // TRIM(FNCOV)
          CALL PRTERR(IERR, CALLER, 3)
          RETURN
        END IF
!
        NUM_CON=0
   10   CONTINUE
          READ(UNIT=ICOV,FMT=*, END=20) I1, I2, I3
          NUM_CON = NUM_CON + 1
          GO TO 10
!
   20   CONTINUE
!
        REWIND(ICOV)
!
         WRITE(IRES,660) TRIM(LEGEND_TITLE)
         WRITE(IRES,770) TRIM(LEGEND_CONTOUR)
    660   FORMAT(2x,'TITLE     = "',A,'"')
    770   FORMAT(2x,'VARIABLES = "Easting","Northing","NODE","',A,'"')
!
   60   FORMAT(2x,'TITLE     = "',A,'"')
!
!       Reading detail file one year at a time
        DO ITIM = 1, CULTURE_NUM_TIM
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. CULTURE_TIM(ITIM)%COMP ) CYCLE
!
! ***     Read the values from the details file for a single year
          CALL READ_CULTURE_MAP( ITIM, DET_VALUE, IERR )
          IF(IERR .NE.0) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine'
            CALL PRTERR(IERR, CALLER, 1)
            RETURN
          END IF
!
! ***     DIfferent actions for the first year that is output and later years
!
          IF( FIRST_YEAR ) THEN
!
!           Output two lines of information for the every year
            WRITE(IRES,50) CULTURE_TIM(ITIM)%TIME
   50       FORMAT(2X,'ZONE T="',I0,'"')
            WRITE(IRES,41) NUM_COR, NUM_CON   ! CULTURE_NUM_LOC ==> NUM_LOC
   41       FORMAT(2x,'N=',I0,',',2X,'E=',I0,',F=FEPOINT ET=triangle')
!
!           Loop over the coordinates and output data
            ICNT = 0
            DO ILOC = 1, CULTURE_NUM_LOC
              IF( .NOT. CULTURE_LOC(ILOC)%COMP ) CYCLE
              ICNT = ICNT + 1
              WRITE(IRES,80) CULTURE_LOC(ILOC)%EASTING, CULTURE_LOC(ILOC)%NORTHING, ICNT, DET_VALUE(ICNT)! *CONVERT_UNITS
   80         FORMAT(1P,E13.6,:,2X,E13.6,2X,0P,I0,2X,1P,E13.6)
            END DO
!
!           Read conectivity and write in the output file
            DO I = 1, NUM_CON
              READ(UNIT=ICOV, FMT=*) I1, I2, I3
              WRITE(IRES,'(I0,2(2X,I0))') I1, I2, I3
            ENDDO
            CLOSE( ICOV )
            FIRST_YEAR = .FALSE.
!
          ELSE ! After the first year
!      
!           Output two lines of information for the every year
            WRITE(IRES,51) CULTURE_TIM(ITIM)%TIME
   51       FORMAT(2X,'ZONE T="',I0,'"')
            WRITE(IRES,61) NUM_COR, NUM_CON   ! CULTURE_NUM_LOC ==> NUM_LOC
   61       FORMAT(2X,'N=',I0,',',2X,'E=',I0,',F=FEPOINT ET=triangle, D=(1,2,3,FECONNECT)')
!           Loop over the coordinates and output data
            DO I = 1, NUM_COR
              WRITE(IRES,80) DET_VALUE(I)! *CONVERT_UNITS
            END DO
!
          END IF ! First year check
!
        END DO ! Time loop
!
      END IF
!
!--------------------------------------------------------------------------------------
!     Riparian or Aquatic data type processing
!--------------------------------------------------------------------------------------
!
      IF( LOC_RIPARIAN .OR. LOC_AQUATIC ) THEN
!
! ***   Opening lines in the Tecplot output file
!
        WRITE(IRES,60) TRIM(PTITLE)
        WRITE(IRES,170) TRIM(UNITS_OUTPUT)
   170  FORMAT(2x,'VARIABLES = "MILE","',A,'"')
!
!       Reading details file one year at a time
        DO ITIM = 1, CULTURE_NUM_TIM
!
! ***     Read the values from the details file for a single year
          CALL READ_CULTURE_MAP(ITIM, DET_VALUE, IERR )
!
!         Check whether data for this year are included in the selected outputs
          IF( .NOT. CULTURE_TIM(ITIM)%COMP ) CYCLE
!
!         Output information for the every year
          WRITE(IRES,51) CULTURE_TIM(ITIM)%TIME
!         Loop over the coordinates and output data
          ICNT = 0
          DO ILOC = 1, CULTURE_NUM_LOC
            IF( .NOT. CULTURE_LOC(ILOC)%COMP ) CYCLE
            ICNT = ICNT + 1
            WRITE(IRES,80) CULTURE_LOC(ILOC)%MILE, DET_VALUE(ICNT) ! *CONVERT_UNITS
          END DO
!
        END DO
!
      ENDIF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE READ_ECEM_SV_HEADER( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens and reads the SACVIEW header file for data
!!    for the ECEM code.
!!
!!  History:
!!
!!    Carmen Arimescu : 7 Dec 2004 : Version 1.0
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
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE ECEM_Mod
      USE Rdblk_Mod
      USE Debug_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR
      INTEGER :: ISPC
!
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
!
      CHARACTER(LEN=19) :: CALLER = 'READ_ECEM_SV_HEADER' ! Name of this subroutine
      INTEGER :: IERA  ! Error indicator from the file system
      INTEGER :: IANA  ! Local analyte index
      INTEGER :: ILOC  ! Local location index
      INTEGER :: ITIM  ! Local time index
      INTEGER :: IMED  ! Local media index
      INTEGER :: MEDIA ! Number of media
!
      CHARACTER(LEN=20)  :: HDR_TYPE
      CHARACTER(LEN=8)   :: LABEL1
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      IHED = GET_UNIT_NUMBER(  )
      IF( IHED .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the header file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output header file
!
      OPEN(IHED,FILE=FNHED,STATUS='old',IOSTAT=IERA)
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the header file'
        MESSAG(2) = 'File: ' // TRIM(FNHED)
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** READ a few identification items to the file
!
      READ(IHED,*,ERR=9999) LABEL1, HDR_TYPE  !'type: ',     'ECEM'
!         IF(HDR_TYPE .NE. 'ECEM')  THEN
!           IERR = 3
!           MESSAG(1) = 'System error opening the header file'
!           MESSAG(2) = 'File: ' // TRIM(FNHED)
!           CALL PRTERR( IERR, CALLER, 2 )
!           RETURN
!         END IF
!
      READ(IHED,*,ERR=9999) LABEL1    !'title: ',    TRIM(PTITLE)
      READ(IHED,*,ERR=9999) LABEL1    !'user: ',     TRIM(USRNAM)
      READ(IHED,*,ERR=9999) LABEL1    !'name: ',     TRIM(PRGNAM)
      READ(IHED,*,ERR=9999) LABEL1    !'version: ',  TRIM(PRGVER)
      READ(IHED,*,ERR=9999) LABEL1    !'date: ',     TRIM(PRGDAT)
      READ(IHED,*,ERR=9999) LABEL1    !'id: ',       TRIM(CRUNID)
      READ(IHED,*,ERR=9999) LABEL1    !'envfile: ',  TRIM(FNESD)
! 1000 FORMAT(A,'"',A,'"')
!
! *** READ number of realizations
!
      READ(IHED,*,ERR=9999) LABEL1, NREAL
      IF( NREAL .GT. 0 ) THEN
        ALLOCATE( VBR(NREAL) )
      ELSE
        IERR = 3
        MESSAG(1) = 'Invalid number of realizations'
        MESSAG(2) = 'Problems reading the fheader file'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
 1010 FORMAT(A,I0)
!
! *** READ time information
!
      READ(IHED,*,ERR=9999) LABEL1, ECEM_NUM_TIM
      IF( BGECEM ) WRITE(*,*) LABEL1, ECEM_NUM_TIM
      ALLOCATE(ECEM_TIM(ECEM_NUM_TIM))
      DO ITIM = 1, ECEM_NUM_TIM
        READ(IHED,*,ERR=9999) ECEM_TIM(ITIM)%TIME
        ECEM_TIM(ITIM)%COMP = .FALSE.
        IF( BGECEM) WRITE(*,*) ECEM_TIM(ITIM)%TIME
      END DO
!
! *** READ location information
!
        READ(IHED,*,ERR=9999) LABEL1, ECEM_NUM_LOC
        IF( BGECEM ) WRITE(*,*) LABEL1, ECEM_NUM_LOC
        ALLOCATE(ECEM_LOC(ECEM_NUM_LOC))
        DO ILOC = 1, ECEM_NUM_LOC
          READ(IHED,*,ERR=9999) ECEM_LOC(ILOC)%ID, ECEM_LOC(ILOC)%NAME
          ECEM_LOC(ILOC)%COMP = .FALSE.
          IF( BGECEM) WRITE(*,*) TRIM(ECEM_LOC(ILOC)%ID), ' ', TRIM(ECEM_LOC(ILOC)%NAME)
        END DO
!
! *** READ analyte information
!
        READ(IHED,*,ERR=9999) LABEL1, ECEM_NUM_ANA
        ALLOCATE(ECEM_ANA(ECEM_NUM_ANA))
        DO IANA = 1, ECEM_NUM_ANA
          READ(IHED,*,ERR=9999) ECEM_ANA(IANA)%ID, ECEM_ANA(IANA)%TYPE, ECEM_ANA(IANA)%NAME
        END DO
!
! *** READ SPECIES information
!
        READ(IHED,*,ERR=9999) LABEL1, ECEM_NUM_SPC
        ALLOCATE(ECEM_SPC(ECEM_NUM_SPC))
        DO ISPC = 1, ECEM_NUM_SPC
          READ(IHED,*,ERR=9999) ECEM_SPC(ISPC)%ID, ECEM_SPC(ISPC)%TYPE, ECEM_SPC(ISPC)%NAME
        END DO
!
! *** READ media information
!
      READ(IHED,*,ERR=9999) LABEL1, MEDIA
      DO IMED = 1, MEDIA
        READ(IHED,*,ERR=9999) LABEL1
      END DO
!
! *** READ solution information
!
        READ(IHED,*,ERR=9999) LABEL1, ECEM_NUM_SLN
        ALLOCATE(ECEM_SLN(ECEM_NUM_SLN))
        DO ILOC = 1, ECEM_NUM_SLN
          READ(IHED,*,ERR=9999) ECEM_SLN(ILOC)%ID, ECEM_SLN(ILOC)%NAME
        END DO
!
! *** Normal exit
!
      CLOSE( IHED )
      RETURN
!
! *** Abnormal exit: Error reading from the file
!
 9999 CONTINUE
!
      CLOSE( IHED )
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE READ_ECEM_DET( ITIM, DET_VALUE, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the ECEM details file and returns a vector
!!    DET_VALUE of length NUM_COR with values for only the selected
!!    locations
!!
!!  History:
!!
!!    Carmen Arimescu : 8 Dec 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ECEM_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: ITIM ! Time index
      REAL, DIMENSION(*) :: DET_VALUE ! Vector of values to output
      INTEGER :: IERR ! Error flag
      INTEGER :: YEAR
      CHARACTER(LEN=6) :: LOC_ID
      CHARACTER(LEN=4) :: SOIL_TYPE
      CHARACTER(LEN=7) :: ANA_ID
      CHARACTER(LEN=6) :: SPC
      CHARACTER(LEN=7) :: SOL_TYPE
      CHARACTER(LEN=7) :: UNITS
      INTEGER :: IDXL
!
! *** Local variables
!
      INTEGER :: ICNT ! Local looping index
      CHARACTER(LEN=13) :: CALLER = 'READ_ECEM_DET' ! Name of this subroutine
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
      ICNT = 0
   10 CONTINUE
!
       READ(IDET,*,END=999) YEAR, LOC_ID, SOIL_TYPE, ANA_ID, SPC, SOL_TYPE, UNITS, VBR
!
!      Disscard data before the desired year
       IF( YEAR .LT. ECEM_TIM(ITIM)%TIME ) GO TO 10
!
!      Check whether we have moved beyond the desired data for a given year (ITIM index)
       IF( YEAR .GT. ECEM_TIM(ITIM)%TIME ) THEN
         BACKSPACE(IDET)
         IF( ICNT .NE. NUM_COR ) THEN
           IERR = 2
           MESSAG(1) = 'Mismatch in the data file'
           MESSAG(2) = 'Number of data values at a given year does not match'
           MESSAG(3) = 'the number of coordinates for the animation'
           MESSAG(4) = ' '
           WRITE(MESSAG(4)(1:),1000) ICNT, NUM_COR
 1000      FORMAT('Number found was ',I0,' number expected was ',I0)
           CALL PRTERR( IERR, CALLER, 4 )
           RETURN
         END IF
         RETURN
       END IF
!
!      Check for the correct soil type
       IF( SOIL_TYPE .NE. STY_USE ) GO TO 10
!
!      Check for the correct analyte
       IF( ANA_ID .NE. ANA_USE ) GO TO 10
!
!      Check for the correct species type
       IF( SPC .NE. SPC_USE ) GO TO 10
!
!      Check for the correct solution type
       IF( SOL_TYPE .NE. SLT_USE ) GO TO 10
!
!      Check for a selected location
!      Get the location index in the vector ECEM_LOC => iloc
       CALL MATCH_LOC_ECEM( LOC_ID, IDXL )    
       IF( IDXL .LT. 1 ) THEN
         IERR = 1
         MESSAG(1) = 'Location requested that is not in the master list'
         MESSAG(2) = 'Location name is '//LOC_ID
         CALL PRTERR( IERR, CALLER, 2 )
         RETURN
       END IF
       IF( .NOT.ECEM_LOC(IDXL)%COMP ) GO TO 10
!
!      Use this data line (pick out the single requested realization)
       ICNT = ICNT + 1
       IF( ICNT .GT. NUM_COR ) THEN
         IERR = 4
         MESSAG(1) = 'Data problem'
         MESSAG(2) = 'The data in the ECEM details file do not match with the'
         MESSAG(3) = 'number of coordinates.'
         CALL PRTERR( IERR, CALLER, 3 )
         RETURN
       END IF
       DET_VALUE(ICNT) = VBR(REL_USE)
       GO TO 10
!
! *** End of file
!
  999 CONTINUE
      IF( ICNT .NE. NUM_COR ) THEN
        IERR = 3
        MESSAG(1) = 'Mismatch in the data file'
        MESSAG(2) = 'Number of data values at a given year does not match'
        MESSAG(3) = 'the number of coordinates for the animation'
        MESSAG(4) = ' '
        WRITE(MESSAG(4)(1:),1000) ICNT, NUM_COR
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
      RETURN
      END SUBROUTINE
!
      SUBROUTINE READ_CULTURE_SV_HEADER( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens and reads the SACVIEW header file for
!!    data from the CULTURE code.
!!
!!  History:
!!
!!    Carmen Arimescu : 19 APR 2006 : Version 1.0
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
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE CULTURE_Mod
      USE Rdblk_Mod
      USE Debug_Mod
!
      IMPLICIT NONE
!
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
!
      INTEGER :: IERR
!
! *** Local variables
!
      INTEGER :: IERA ! Error indicator from the file system
      INTEGER :: ITIM ! Local time index
      INTEGER :: ILOC
      INTEGER :: IMAPS ! Local map index
      CHARACTER(LEN=20) :: HDR_TYPE
      CHARACTER(LEN=16)  :: LABEL1
!
      CHARACTER(LEN=22) :: CALLER = 'READ_CULTURE_SV_HEADER' ! Name of this subroutine
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      IHED = GET_UNIT_NUMBER(  )
      IF( IHED .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the header file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output header file
!
      OPEN(IHED,FILE=FNHED,STATUS='UNKNOWN',IOSTAT=IERA)
      IF( IERA .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'System error opening the SACVIEW header file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** READ a few identification items to the file
!
      READ(IHED,*,ERR=9999) LABEL1, HDR_TYPE !'type: ',     'Cultural'
      READ(IHED,*,ERR=9999) LABEL1  !'title: ',    TRIM(PTITLE)
      READ(IHED,*,ERR=9999) LABEL1  !'user: ',     TRIM(USRNAM)
      READ(IHED,*,ERR=9999) LABEL1  !'name: ',     TRIM(PRGNAM)
      READ(IHED,*,ERR=9999) LABEL1  !'version: ',  TRIM(PRGVER)
      READ(IHED,*,ERR=9999) LABEL1  !'date: ',     TRIM(PRGDAT)
      READ(IHED,*,ERR=9999) LABEL1  !'id: ',       TRIM(CRUNID)
      READ(IHED,*,ERR=9999) LABEL1  !'envfile: ',  TRIM(FNESD)
!
! *** READ number of realizations
!
      READ(IHED,*,ERR=9999) LABEL1, NREAL
      IF( NREAL .GT. 0 ) THEN
        ALLOCATE( VBR(NREAL) )
      ELSE
        IERR = 3
        MESSAG(1) = 'Invalid number of realizations'
        MESSAG(2) = 'Problems reading the fheader file'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
 1010 FORMAT(A,I0)
!
! *** READ time information
!
      READ(IHED,*,ERR=9999) LABEL1, CULTURE_NUM_TIM
!      IF( BGCULTURE ) WRITE(*,*) LABEL1, CULTURE_NUM_TIM
      ALLOCATE(CULTURE_TIM(CULTURE_NUM_TIM))
      DO ITIM = 1, CULTURE_NUM_TIM
        READ(IHED,*,ERR=9999) CULTURE_TIM(ITIM)%TIME
        CULTURE_TIM(ITIM)%COMP = .FALSE.
!        IF( BGCULTURE) WRITE(*,*) CULTURE_TIM(ITIM)%TIME
      END DO
!
! *** READ location information
!
      READ(IHED,*,ERR=9999) LABEL1, CULTURE_NUM_LOC !'locations: ', CUL_NUM_LOC
      IF( BGCULTURE ) WRITE(*,*) LABEL1, CULTURE_NUM_LOC
      ALLOCATE(CULTURE_LOC(CULTURE_NUM_LOC))
      DO ILOC = 1, CULTURE_NUM_LOC
        READ(IHED,*,ERR=9999) CULTURE_LOC(ILOC)%ID, CULTURE_LOC(ILOC)%NAME
        CULTURE_LOC(ILOC)%COMP = .FALSE.
        IF( BGCULTURE) WRITE(*,*) TRIM(CULTURE_LOC(ILOC)%ID), ' ', TRIM(CULTURE_LOC(ILOC)%NAME)
      END DO
!
! *** READ site information
!
       READ(IHED,*,ERR=9999) LABEL1  !'sites: ', NUM_SITE
       READ(IHED,*,ERR=9999) LABEL1  !'summary: ', TRIM(FNSIS)
       READ(IHED,*,ERR=9999) LABEL1  !'detail: ', TRIM(FNSID)
!
! *** READ area information
!
       READ(IHED,*,ERR=9999) LABEL1  !'areas: ', NUM_AREA
       READ(IHED,*,ERR=9999) LABEL1  !AREAS(IARE)%ID, TRIM(AREAS(IARE)%NAME)
       READ(IHED,*,ERR=9999) LABEL1  !'analyte: ', ICNT
       READ(IHED,*,ERR=9999) LABEL1  !ESD_ANA(IANA)%ID
       READ(IHED,*,ERR=9999) LABEL1  !'media: ', ICNT
       READ(IHED,*,ERR=9999) LABEL1  !'GWAT'
       READ(IHED,*,ERR=9999) LABEL1  !'summary: ', TRIM(FNARS)
       READ(IHED,*,ERR=9999) LABEL1  !'detail: ', TRIM(FNARD)
!
! *** READ map information
!
        READ(IHED,*,ERR=9999) LABEL1, CULTURE_NUM_MAPS  !'maps: ', NUM_MAPS
        ALLOCATE(CULTURE_MAP(CULTURE_NUM_MAPS))
        DO IMAPS = 1, CULTURE_NUM_MAPS
          READ(IHED,*,ERR=9999) CULTURE_MAP(IMAPS)%ID
        END DO
!
! *** READ analyte information
!
        READ(IHED,*,ERR=9999) LABEL1  !'analyte: ', ICNT
        READ(IHED,*,ERR=9999) LABEL1  !ESD_ANA(IANA)%ID
!
! *** READ media information
!
        READ(IHED,*,ERR=9999) LABEL1 !'media: ', ICNT
        READ(IHED,*,ERR=9999) LABEL1 !'MEDIA ID'
!        READ(IHED,*,ERR=9999) LABEL1 !'summary: ', TRIM(FNCMAP)
!
! *** Normal exit
!
      CLOSE( IHED )
      RETURN
!
! *** Abnormal exit: Error writing to the file
!
 9999 CONTINUE
!
      IERR = 2
      MESSAG(1) = 'Error writing to the SACVIEW header file'
      CALL PRTERR( IERR, CALLER, 1 )
      CLOSE( IHED )
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE READ_CULTURE_MAP( ITIM, DET_VALUE, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the HUMAN details file and returns a vector
!!    DET_VALUE of length NUM_COR with values for only the selected
!!    locations
!!
!!  History:
!!
!!    Carmen Arimescu : 24 Apr : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE CULTURE_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: ITIM ! Time index
      REAL, DIMENSION(*) :: DET_VALUE ! Vector of values to output
      INTEGER :: IERR ! Error flag
      INTEGER :: YEAR
      CHARACTER(LEN=6) :: LOC_ID
      CHARACTER(LEN=9) :: MAP_ID
      REAL :: EASTING
      REAL :: NORTHING
      INTEGER :: IDXL
!
! *** Local variables
!
      INTEGER :: ICNT ! Local looping index
      CHARACTER(LEN=16) :: CALLER = 'READ_CULTURE_MAP' ! Name of this subroutine
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
      ICNT = 0
   10 CONTINUE
!
       READ(ICMAP,*,END=999) YEAR, MAP_ID, LOC_ID, EASTING, NORTHING, VBR(REL_USE)
!
!      Disscard data before the desired year
       IF( YEAR .LT. CULTURE_TIM(ITIM)%TIME ) GO TO 10
!
!      Check whether we have moved beyond the desired data for a given year (ITIM index)
       IF( YEAR .GT. CULTURE_TIM(ITIM)%TIME ) THEN
         BACKSPACE(ICMAP)
         IF( ICNT .NE. NUM_COR ) THEN
           IERR = 2
           MESSAG(1) = 'Mismatch in the data file'
           MESSAG(2) = 'Number of data values at a given year does not match'
           MESSAG(3) = 'the number of coordinates for the animation'
           MESSAG(4) = ' '
           WRITE(MESSAG(4)(1:),1000) ICNT, NUM_COR
 1000      FORMAT('Number found was ',I0,' number expected was ',I0)
           CALL PRTERR( IERR, CALLER, 4 )
           RETURN
         END IF
         RETURN
       END IF
!
!      Check for the correct analyte
       IF( MAP_ID .NE. MAP_USE ) GO TO 10
!
!      Check for a selected location
!      Get the location index in the vector HUMAN_LOC => iloc
       CALL MATCH_LOC_CULTURE( LOC_ID, IDXL )
       IF( IDXL .LT. 1 ) THEN
         IERR = 1
         MESSAG(1) = 'Location requested that is not in the master list'
         MESSAG(2) = 'Location name is '//LOC_ID
         CALL PRTERR( IERR, CALLER, 2 )
         RETURN
       END IF
       IF( .NOT.CULTURE_LOC(IDXL)%COMP ) GO TO 10
!
!      Use this data line (pick out the single requested realization)
       ICNT = ICNT + 1
       DET_VALUE(ICNT) = VBR(REL_USE)
       GO TO 10
!
! *** End of file
!
  999 CONTINUE
      IF( ICNT .NE. NUM_COR ) THEN
        IERR = 3
        MESSAG(1) = 'Mismatch in the data file'
        MESSAG(2) = 'Number of data values at a given year does not match'
        MESSAG(3) = 'the number of coordinates for the animation'
        MESSAG(4) = ' '
        WRITE(MESSAG(4)(1:),1000) ICNT, NUM_COR
        CALL PRTERR( IERR, CALLER, 4 )
      END IF
      RETURN
      END SUBROUTINE
!
      SUBROUTINE READ_HUMAN_DET( ITIM, DET_VALUE, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the HUMAN details file and returns a vector
!!    DET_VALUE of length NUM_COR with values for only the selected
!!    locations
!!
!!  History:
!!
!!    Carmen Arimescu : 29 Nov 2004 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Files_Mod
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE HUMAN_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: ITIM ! Time index
      REAL, DIMENSION(*) :: DET_VALUE ! Vector of values to output
      INTEGER :: IERR ! Error flag
      INTEGER :: YEAR
      CHARACTER(LEN=6) :: LOC_ID
      CHARACTER(LEN=7) :: ANA_ID
      CHARACTER(LEN=8) :: ANA_TYPE
      CHARACTER(LEN=7) :: SOL_TYPE
      CHARACTER(LEN=5) :: UNITS
      INTEGER :: IDXL
!
! *** Local variables
!
      INTEGER :: ICNT ! Local looping index
      CHARACTER(LEN=14) :: CALLER = 'READ_HUMAN_DET' ! Name of this subroutine
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
      ICNT = 0
   10 CONTINUE
!
       READ(IDET,*,END=999) YEAR, LOC_ID, ANA_ID, ANA_TYPE, SOL_TYPE, UNITS, VBR
!
!      Discard data before the desired year
       IF( YEAR .LT. HUMAN_TIM(ITIM)%TIME ) GO TO 10
!
!      Check whether we have moved beyond the desired data for a given year (ITIM index)
       IF( YEAR .GT. HUMAN_TIM(ITIM)%TIME ) THEN
         BACKSPACE(IDET)
         IF( ICNT .NE. NUM_COR ) THEN
           IERR = 2
           MESSAG(1) = 'Mismatch in the data file'
           MESSAG(2) = 'Number of data values at a given year does not match'
           MESSAG(3) = 'the number of coordinates for the animation'
           MESSAG(4) = ' '
           WRITE(MESSAG(4)(1:),1000) ICNT, NUM_COR
 1000      FORMAT('Number found was ',I0,' number expected was ',I0)
           CALL PRTERR( IERR, CALLER, 4 )
           RETURN
         END IF
         RETURN
       END IF
!
!      Check for the correct analyte
       IF( ANA_ID .NE. ANA_USE ) GO TO 10
!
!      Check for the correct analyte type
       IF( ANA_TYPE .NE. ATY_USE ) GO TO 10
!
!      Check for the correct solution type
       IF( SOL_TYPE .NE. SLT_USE ) GO TO 10
!
!      Check for a selected location
!      Get the location index in the vector HUMAN_LOC => iloc
       CALL MATCH_LOC_HUMAN( LOC_ID, IDXL )
       IF( IDXL .LT. 1 ) THEN
         IERR = 1
         MESSAG(1) = 'Location requested that is not in the master list'
         MESSAG(2) = 'Location name is '//LOC_ID
         CALL PRTERR( IERR, CALLER, 2 )
         RETURN
       END IF
       IF( .NOT.HUMAN_LOC(IDXL)%COMP ) GO TO 10
!
!      Use this data line (pick out the single requested realization)
       ICNT = ICNT + 1
       DET_VALUE(ICNT) = VBR(REL_USE)
       GO TO 10
!
! *** End of file
!
  999 CONTINUE
      IF( ICNT .NE. NUM_COR ) THEN
        IERR = 3
        MESSAG(1) = 'Mismatch in the data file'
        MESSAG(2) = 'Number of data values at a given year does not match'
        MESSAG(3) = 'the number of coordinates for the animation'
        MESSAG(4) = ' '
        WRITE(MESSAG(4)(1:),1000) ICNT, NUM_COR
        CALL PRTERR( IERR, CALLER, 4 )
      END IF
      RETURN
      END SUBROUTINE
!
      SUBROUTINE READ_HUMAN_SV_HEADER( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens and reads the SACVIEW header file for
!!    data from the HUMAN code.
!!
!!  History:
!!
!!    Carmen Arimescu : 18 Nov : Version 1.0
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
      USE Iden_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Files_Mod
      USE HUMAN_Mod
      USE Rdblk_Mod
      USE Debug_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER :: IERR
!
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
!
      INTEGER :: IERA ! Error indicator from the file system
      INTEGER :: IANA ! Local analyte index
      INTEGER :: ILOC ! Local location index
      INTEGER :: ITIM ! Local time index
      CHARACTER(LEN=20) :: HDR_TYPE
      CHARACTER(LEN=16)  :: LABEL1
!
      CHARACTER(LEN=20) :: CALLER = 'READ_HUMAN_SV_HEADER' ! Name of this subroutine
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      IHED = GET_UNIT_NUMBER(  )
      IF( IHED .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the header file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output header file
!
      OPEN(IHED,FILE=FNHED,STATUS='old',IOSTAT=IERA)
      IF( IERA .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the header file'
        MESSAG(2) = 'File: ' // TRIM(FNHED)
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** READ a few identification items to the file
!
      READ(IHED,*,ERR=9999) LABEL1, HDR_TYPE  !'type: ',     'Human'
      READ(IHED,*,ERR=9999) LABEL1    !'title: ',    TRIM(PTITLE)
      READ(IHED,*,ERR=9999) LABEL1    !'user: ',     TRIM(USRNAM)
      READ(IHED,*,ERR=9999) LABEL1    !'name: ',     TRIM(PRGNAM)
      READ(IHED,*,ERR=9999) LABEL1    !'version: ',  TRIM(PRGVER)
      READ(IHED,*,ERR=9999) LABEL1    !'date: ',     TRIM(PRGDAT)
      READ(IHED,*,ERR=9999) LABEL1    !'id: ',       TRIM(CRUNID)
      READ(IHED,*,ERR=9999) LABEL1    !'envfile: ',  TRIM(FNESD)
!
! *** READ number of realizations
!
      READ(IHED,*,ERR=9999) LABEL1, NREAL
      IF( NREAL .GT. 0 ) THEN
        ALLOCATE( VBR(NREAL) )
      ELSE
        IERR = 3
        MESSAG(1) = 'Invalid number of realizations'
        MESSAG(2) = 'Problems reading the fheader file'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
 1010 FORMAT(A,I0)
!
! *** READ time information
!
      READ(IHED,*,ERR=9999) LABEL1, HUMAN_NUM_TIM
      IF( BGHUMAN ) WRITE(*,*) LABEL1, HUMAN_NUM_TIM
      ALLOCATE(HUMAN_TIM(HUMAN_NUM_TIM))
      DO ITIM = 1, HUMAN_NUM_TIM
        READ(IHED,*,ERR=9999) HUMAN_TIM(ITIM)%TIME
        HUMAN_TIM(ITIM)%COMP = .FALSE.
        IF( BGHUMAN) WRITE(*,*) HUMAN_TIM(ITIM)%TIME
      END DO
!
! *** READ location information
!
        READ(IHED,*,ERR=9999) LABEL1, HUMAN_NUM_LOC
        IF( BGHUMAN ) WRITE(*,*) LABEL1, HUMAN_NUM_LOC
        ALLOCATE(HUMAN_LOC(HUMAN_NUM_LOC))
        DO ILOC = 1, HUMAN_NUM_LOC
          READ(IHED,*,ERR=9999) HUMAN_LOC(ILOC)%ID, HUMAN_LOC(ILOC)%NAME
          HUMAN_LOC(ILOC)%COMP = .FALSE.
          IF( BGHUMAN) WRITE(*,*) TRIM(HUMAN_LOC(ILOC)%ID), ' ', TRIM(HUMAN_LOC(ILOC)%NAME)
        END DO
!
! *** READ analyte information
!
        READ(IHED,*,ERR=9999) LABEL1, HUMAN_NUM_ANA
        ALLOCATE(HUMAN_ANA(HUMAN_NUM_ANA))
        DO IANA = 1, HUMAN_NUM_ANA
          READ(IHED,*,ERR=9999) HUMAN_ANA(IANA)%ID, HUMAN_ANA(IANA)%TYPE, HUMAN_ANA(IANA)%NAME
        END DO
!
! *** READ solution information
!
        READ(IHED,*,ERR=9999) LABEL1, HUMAN_NUM_SLN
        ALLOCATE(HUMAN_SLN(HUMAN_NUM_SLN))
        DO ILOC = 1, HUMAN_NUM_SLN
          READ(IHED,*,ERR=9999) HUMAN_SLN(ILOC)%ID, HUMAN_SLN(ILOC)%NAME
        END DO
!
! *** Normal exit
!
      CLOSE( IHED )
      RETURN
!
! *** Abnormal exit: Error reading from the file
!
 9999 CONTINUE
!
      CLOSE( IHED )
      RETURN
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
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
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
      SUBROUTINE READ_KEY_1( IERR )
!!****************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and storing the top-level keyword
!!    control information.
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
!!    Carmen Arimescu  :  2 Nov 2004 : Original source
!!    Carmen Arimescu  : 27 Dec 2004 : Modified for ECDA case
!!    Paul W. Eslinger :  4 Jun 2007 : Move REPORT keyword to another routine
!!
!!****************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Debug_Mod
!
      IMPLICIT NONE
!
! *** User-defined functions
      LOGICAL, EXTERNAL :: CEXIST
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'READ_KEY_1' ! Name of this subroutine
!
      INTEGER :: IDX                    ! Temporary index variable
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary storage for a quote string
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
      ILINE = 0
!
!----------------------------------------------------------------------
!                Top of loop on reading keyword cards
!----------------------------------------------------------------------
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
!------ CATEGORY Keyword -------------------------------------------------
        CASE( 'CATEGORY' )
          IF( CEXIST('CULTURE') ) CAT_CULTURE = .TRUE.
          IF( CEXIST('ECEM') )    CAT_ECEM    = .TRUE.
          IF( CEXIST('ECDA') )    CAT_ECDA    = .TRUE.
          IF( CEXIST('FCDA') )    CAT_FCDA    = .TRUE.
          IF( CEXIST('HUMAN') )   CAT_HUMAN   = .TRUE.
!
!------ DEBUG Keyword -------------------------------------------------
        CASE( 'DEBUG' )
          IF( CEXIST('CULTURE') ) BGCULTURE = .TRUE.
          IF( CEXIST('ECEM') )    BGECEM    = .TRUE.
          IF( CEXIST('ECDA') )    BGECDA    = .TRUE.
          IF( CEXIST('FCDA') )    BGFCDA    = .TRUE.
          IF( CEXIST('HUMAN') )   BGHUMAN   = .TRUE.
!
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          REWIND( IKEY )
          RETURN
!
!-------LOCATION Keyword---------------------------------------------------
        CASE( 'LOCATION' )
          IDX = 0
          IF( CEXIST('UPLAND') )   THEN
            IDX = IDX + 1
            LOC_UPLAND   = .TRUE.
          END IF
          IF( CEXIST('RIPARIAN') ) THEN
            IDX = IDX + 1
            LOC_RIPARIAN = .TRUE.
          END IF
          IF( CEXIST('AQUATIC') )  THEN
            IDX = IDX + 1
            LOC_AQUATIC  = .TRUE.
          END IF
          IF( IDX .NE. 1 ) THEN
            IERR = 3
            MESSAG(1) = 'Only one of the UPLAND, AQUATIC or RIPARIAN modifiers can be'
            MESSAG(2) = 'used on the LOCATION keyword.'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!------ FILE Keyword -----------------------------------------------------
        CASE( 'FILE' )   ! ===> FILE keyword
!         Detail modifier
          IF( CEXIST('DETAIL') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 1
              MESSAG(1) = 'The DETAIL modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNDET = TMP_NAME
            END IF
          END IF
!
!         MAP modifier
          IF( CEXIST('MAP') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 1
              MESSAG(1) = 'The MAP modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNCMAP = TMP_NAME
            END IF
          END IF
!
!         Header modifier
          IF( CEXIST('HEADER') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 2
              MESSAG(1) = 'The HEADER modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNHED = TMP_NAME
            END IF
          END IF
!
!         ECDA concentration file modifier
          IF( CEXIST('C_ECDA') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 2
              MESSAG(1) = 'The C_ECDA modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNCON = TMP_NAME
            END IF
          END IF
!
!         ECDA map index file modifier
          IF( CEXIST('I_ECDA') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 2
              MESSAG(1) = 'The I_ECDA modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNMAP = TMP_NAME
            END IF
          END IF
!
!         FCDA concentration file modifier
          IF( CEXIST('C_FCDA') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 2
              MESSAG(1) = 'The C_FCDA modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNFOD = TMP_NAME
            END IF
          END IF
!
!         FCDA map index file modifier
          IF( CEXIST('I_FCDA') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 2
              MESSAG(1) = 'The I_FCDA modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNMAP = TMP_NAME
            END IF
          END IF
!
          IF( CEXIST('COORDIN') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 3
              MESSAG(1) = 'The COORDIN modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNCOR = TMP_NAME
            END IF
          END IF
!
          IF( CEXIST('CONNECT') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 4
              MESSAG(1) = 'The CONECT modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNCOV = TMP_NAME
            END IF
          END IF
!
          IF( CEXIST('RESULTS') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 5
              MESSAG(1) = 'The RESULTS modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
            ELSE
              FNRES = TMP_NAME
            END IF
          END IF
!
!------ REALIZAT Keyword ----------------------------------------------------!
        CASE( 'PICK_REL' ) ! ===> REALIZATION Keyword
          REL_USE = VALUE(1)
!
!------ TITLE Keyword ----------------------------------------------------
        CASE( 'TITLE' )
          IF( NQUOTE .GE. 1 ) PTITLE = QUOTE(1)
!
!------ USER Keyword -----------------------------------------------------
        CASE( 'USER' )
          IF( NQUOTE .GE. 1 ) USRNAM = QUOTE(1)
!
!------ VERBOSE Keyword --------------------------------------------------
        CASE( 'VERBOSE' )
          VERBOSE = .TRUE.
!
!------ Undefined Keyword ------------------------------------------------
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
      SUBROUTINE READ_RIP_AQ_COORDS( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens a file containing the coordinates
!!    (easting,northing) and river mile associated with locations
!!    in the riparian zone or in the river.  The locations entered
!!    in this file define the locations for riparian zone (or aquatic)
!!    animations.  Data values need to be entered for this entire
!!    set of coordinates to allow animation of riparian or aquatic
!!    data sets.
!!
!!  Call List Variables:
!!
!!    IERR : Output integer error flag.
!!            0 = Normal execution
!!           >0 = Terminal error encountered
!!
!!  History:
!!
!!    Carmen Arimescu : 18 Mar 2005 : Original source
!!    Carmen Arimescu : 26 Apr 2006 : Added the CULTURE category
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
      USE HUMAN_Mod
      USE ECEM_MOD
      USE ECDA_Mod
      USE FCDA_Mod
      USE CULTURE_Mod
!
      IMPLICIT NONE
!
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
!
      INTEGER :: IERR ! Error number flag
!
      CHARACTER(LEN=11) :: CALLER = 'READ_RIP_AQ_COORDS'
!
      INTEGER :: IOS             ! I/O Status from open, read, or close of file
      CHARACTER(LEN=LENQQQ) :: HEADER
      INTEGER :: IDXL
      REAL :: EASTING
      REAL :: NORTHING
      REAL :: RIVER_MILE
      CHARACTER(LEN=6) :: COORD_LOC_ID
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      ICOR = GET_UNIT_NUMBER(  )
      IF( ICOR .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the coordinate file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the coordinate file
!
      OPEN(UNIT=ICOR, IOSTAT=IOS, STATUS='OLD', FILE=FNCOR )
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error opening coordinate file'
        WRITE(MESSAG(2), *) 'UNIT = ', ICOR, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNCOR = ' // TRIM(FNCOR)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
      READ(UNIT=ICOR,FMT='(A)') HEADER
!
     NUM_COR = 0
  10 CONTINUE
       READ(UNIT=ICOR,FMT=*,END=20) COORD_LOC_ID, EASTING, NORTHING, RIVER_MILE
!
!
       IF( CAT_CULTURE) THEN
         CALL MATCH_LOC_CULTURE(COORD_LOC_ID,IDXL)
         IF( IDXL .GT. 0 ) THEN
           NUM_COR = NUM_COR + 1
           CULTURE_LOC(IDXL)%COMP     = .TRUE.
           CULTURE_LOC(IDXL)%EASTING  = EASTING
           CULTURE_LOC(IDXL)%NORTHING = NORTHING
           CULTURE_LOC(IDXL)%MILE     = RIVER_MILE
         ELSE
           IERR = 3
           MESSAG(1) = 'Location requested is not in the master list'
           MESSAG(2) = 'Invalid location id is '
           WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
           CALL PRTERR( IERR, CALLER, 2 )
           RETURN
         END IF
!
         GO TO 10
       END IF
!
       IF( CAT_HUMAN) THEN
         CALL MATCH_LOC_HUMAN(COORD_LOC_ID,IDXL)
         IF( IDXL .GT. 0 ) THEN
           NUM_COR = NUM_COR + 1
           HUMAN_LOC(IDXL)%COMP     = .TRUE.
           HUMAN_LOC(IDXL)%EASTING  = EASTING
           HUMAN_LOC(IDXL)%NORTHING = NORTHING
           HUMAN_LOC(IDXL)%MILE     = RIVER_MILE
         ELSE
           IERR = 3
           MESSAG(1) = 'Location requested is not in the master list'
           MESSAG(2) = 'Invalid location id is '
           WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
           CALL PRTERR( IERR, CALLER, 2 )
           RETURN
         END IF
!
         GO TO 10
       END IF
!
       IF( CAT_ECEM) THEN
         CALL MATCH_LOC_ECEM(COORD_LOC_ID,IDXL)
         IF( IDXL .GT. 0 ) THEN
           NUM_COR = NUM_COR + 1
           ECEM_LOC(IDXL)%COMP     = .TRUE.
           ECEM_LOC(IDXL)%EASTING  = EASTING
           ECEM_LOC(IDXL)%NORTHING = NORTHING
           ECEM_LOC(IDXL)%MILE     = RIVER_MILE
         ELSE
           IERR = 3
           MESSAG(1) = 'Location requested is not in the master list'
           MESSAG(2) = 'Invalid location id is '
           WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
           CALL PRTERR( IERR, CALLER, 2 )
           RETURN
         END IF
!
         GO TO 10
        END IF
 !
       IF( CAT_ECDA) THEN
         CALL MATCH_LOC_ECDA(COORD_LOC_ID,IDXL)
         IF( IDXL .GT. 0 ) THEN
           NUM_COR = NUM_COR + 1
           ECDA_LOC(IDXL)%COMP     = .TRUE.
           ECDA_LOC(IDXL)%EASTING  = EASTING
           ECDA_LOC(IDXL)%NORTHING = NORTHING
           ECDA_LOC(IDXL)%MILE     = RIVER_MILE
         ELSE
           IERR = 3
           MESSAG(1) = 'Location requested is not in the master list'
           MESSAG(2) = 'Invalid location id is '
           WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
           CALL PRTERR( IERR, CALLER, 2 )
           RETURN
         END IF
!
         GO TO 10
        END IF
!
       IF( CAT_FCDA ) THEN
         CALL MATCH_LOC_FCDA(COORD_LOC_ID,IDXL)
         IF( IDXL .GT. 0 ) THEN
           IF (.NOT. FCDA_LOC(IDXL)%COMP)  THEN
             IERR = 4
             MESSAG(1) = 'Location requested is not in the master list'
             MESSAG(2) = 'Invalid location id is '
             WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
             CALL PRTERR( IERR, CALLER, 2 )
             RETURN
           END IF
           NUM_COR = NUM_COR + 1
           FCDA_LOC(IDXL)%TEC_COMP = .TRUE.   ! coord RIP
           FCDA_LOC(IDXL)%EASTING  = EASTING
           FCDA_LOC(IDXL)%NORTHING = NORTHING
           FCDA_LOC(IDXL)%MILE     = RIVER_MILE
         ELSE
           IERR = 5
           MESSAG(1) = 'Location requested is not in the master list'
           MESSAG(2) = 'Invalid location id is '
           WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
           CALL PRTERR( IERR, CALLER, 2 )
           RETURN
         END IF
         GO TO 10
       END IF
   20 CONTINUE
      CLOSE(ICOR)
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE READ_UPLAND_COORDS( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens a file containing the upland coordinates
!!    (easting,northing) that define the Tecplot connectivity.  Thus,
!!    data values need to be entered for this set of coordinates to
!!    allow animation of upland-based data sets.
!!
!!  Call List Variables:
!!
!!    IERR : Output integer error flag.
!!            0 = Normal execution
!!           >0 = Terminal error encountered
!!
!!  History:
!!
!!    Carmen Arimescu : Nov 23 2004 : Original source
!!    Carmen Arimescu : 26 Apr 2006 : Added the CULTURE category
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
      USE HUMAN_Mod
      USE ECEM_Mod
      USE ECDA_Mod
      USE FCDA_Mod
      USE CULTURE_Mod
!
      IMPLICIT NONE
!
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
!
      INTEGER :: IERR ! Error number flag
!
      CHARACTER(LEN=18) :: CALLER = 'READ_UPLAND_COORDS'
!
      INTEGER :: IOS             ! I/O Status from open, read, or close of file
      CHARACTER(LEN=LENQQQ) :: HEADER
      INTEGER :: IDXL
      REAL :: EASTING
      REAL :: NORTHING
      CHARACTER(LEN=6) :: COORD_LOC_ID
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      ICOR = GET_UNIT_NUMBER(  )
      IF( ICOR .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the coordinate file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the coordinate file
!
      OPEN(UNIT=ICOR, IOSTAT=IOS, STATUS='OLD', FILE=FNCOR )
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error opening coordinate file'
        WRITE(MESSAG(2), *) 'UNIT = ', ICOR, ' IOSTAT = ', IOS
        MESSAG(3) = 'FNCOR = ' // TRIM(FNCOR)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
      READ(UNIT=ICOR,FMT='(A)') HEADER
!
     NUM_COR = 0
  10 CONTINUE
       READ(UNIT=ICOR,FMT=*,END=20) COORD_LOC_ID, EASTING, NORTHING
!
!
       IF( CAT_CULTURE ) THEN
         CALL MATCH_LOC_CULTURE(COORD_LOC_ID,IDXL)
         IF( IDXL .GT. 0 ) THEN
           NUM_COR = NUM_COR + 1
           CULTURE_LOC(IDXL)%COMP     = .TRUE.
           CULTURE_LOC(IDXL)%EASTING  = EASTING
           CULTURE_LOC(IDXL)%NORTHING = NORTHING
         ELSE
           IERR = 3
           MESSAG(1) = 'Location requested is not in the master list'
           MESSAG(2) = 'Invalid location id is '
           WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
           CALL PRTERR( IERR, CALLER, 2 )
           RETURN
         END IF
         GO TO 10
       END IF
!
       IF( CAT_HUMAN ) THEN
         CALL MATCH_LOC_HUMAN(COORD_LOC_ID,IDXL)
         IF( IDXL .GT. 0 ) THEN
           NUM_COR = NUM_COR + 1
           HUMAN_LOC(IDXL)%COMP     = .TRUE.
           HUMAN_LOC(IDXL)%EASTING  = EASTING
           HUMAN_LOC(IDXL)%NORTHING = NORTHING
         ELSE
           IERR = 3
           MESSAG(1) = 'Location requested is not in the master list'
           MESSAG(2) = 'Invalid location id is '
           WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
           CALL PRTERR( IERR, CALLER, 2 )
           RETURN
         END IF
         GO TO 10
       END IF
!
       IF( CAT_ECEM ) THEN
         CALL MATCH_LOC_ECEM(COORD_LOC_ID,IDXL)
         IF( IDXL .GT. 0 ) THEN
           NUM_COR = NUM_COR + 1
           ECEM_LOC(IDXL)%COMP     = .TRUE.
           ECEM_LOC(IDXL)%EASTING  = EASTING
           ECEM_LOC(IDXL)%NORTHING = NORTHING
         ELSE
           IERR = 4
           MESSAG(1) = 'Location requested is not in the master list'
           MESSAG(2) = 'Invalid location id is '
           WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
           CALL PRTERR( IERR, CALLER, 2 )
           RETURN
         END IF
         GO TO 10
       END IF
!
       IF( CAT_ECDA ) THEN
         CALL MATCH_LOC_ECDA(COORD_LOC_ID,IDXL)
         IF( IDXL .GT. 0 ) THEN
           NUM_COR = NUM_COR + 1
             ECDA_LOC(IDXL)%COMP     = .TRUE.
           ECDA_LOC(IDXL)%EASTING  = EASTING
           ECDA_LOC(IDXL)%NORTHING = NORTHING
         ELSE
           IERR = 5
           MESSAG(1) = 'Location requested is not in the master list'
           MESSAG(2) = 'Invalid location id is '
           WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
           CALL PRTERR( IERR, CALLER, 2 )
           RETURN
         END IF
         GO TO 10
       END IF
!
       IF( CAT_FCDA ) THEN
         CALL MATCH_LOC_FCDA(COORD_LOC_ID,IDXL)
         IF( IDXL .GT. 0 ) THEN
           IF (.NOT. FCDA_LOC(IDXL)%COMP)  THEN
             IERR = 6
             MESSAG(1) = 'Location requested is not in the master list'
             MESSAG(2) = 'Invalid location id is '
             WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
             CALL PRTERR( IERR, CALLER, 2 )
             RETURN
           END IF
           NUM_COR = NUM_COR + 1
           FCDA_LOC(IDXL)%TEC_COMP = .TRUE.   ! coord UPLAND
           FCDA_LOC(IDXL)%EASTING  = EASTING
           FCDA_LOC(IDXL)%NORTHING = NORTHING
         ELSE
           IERR = 7
           MESSAG(1) = 'Location requested is not in the master list'
           MESSAG(2) = 'Invalid location id is '
           WRITE(MESSAG(2)(17:),*) COORD_LOC_ID
           CALL PRTERR( IERR, CALLER, 2 )
           RETURN
!           END IF
         END IF
         GO TO 10
       END IF
!
   20 CONTINUE
!
      CLOSE(ICOR)
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE READ_KEY_ECEM_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading ECEM keyword control information
!!    with the exception of time.
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
!!    Carmen Arimescu : 8 DEC 2004 : Original source
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
      IMPLICIT NONE
!
! *** User-defined functions
      LOGICAL, EXTERNAL :: CEXIST ! Part of the RDBLK suite of keyword routines
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=15) :: CALLER = 'READ_KEY_ECEM_1'
!
      INTEGER :: IDX   ! Temporary index variable
      REAL :: RTMP     ! Temporary real variable
!
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary storage for a quote string
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
      ILINE = 0
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!------------------------------------------------------------------------
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
!-------SOILTYPE Keyword---------------------------------------------------
        CASE( 'SOILTYPE' ) ! ===> SOILTYPE Keyword
          IF( NQUOTE .GT. 0 ) STY_USE = QUOTE(1)
!
!-------ANALYTE Keyword---------------------------------------------------
        CASE( 'ANALYTE' ) ! ===> ANALYTE Keyword
          IF( NQUOTE .GT. 0 ) ANA_USE = QUOTE(1)
!
!-------SPECIES Keyword---------------------------------------------------
        CASE( 'SPECIES' ) ! ===> SPECIES Keyword
          IF( NQUOTE .GT. 0 ) SPC_USE = QUOTE(1)
!
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          REWIND( IKEY )
          RETURN
!
!------ PICK_REL Keyword---------------------------------------------------
        CASE( 'PICK_REL' )
          IF( NVALUE .GT. 0 ) REL_USE = VALUE(1)
!
!-------SOLUTION Keyword---------------------------------------------------
        CASE( 'SOLUTION' ) ! ===> SOLUTION Keyword
          IF( NQUOTE .GT. 0 ) SLT_USE = QUOTE(1)
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
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 6
            MESSAG(1) = 'OUTPUT modifier not entered on the UNITS keyword'
            CALL PRTERR( IERR, CALLER, 1 )
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
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 8
            MESSAG(1) = 'FACTOR modifier not entered on the UNITS keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!------ LEGEND keyword -----------------------------------------------------
        CASE( 'LEGEND' ) ! ===> USER Keyword
!
          IF( CEXIST('TITLE') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              LEGEND_TITLE = TMP_NAME
            ELSE
              IERR = 9
              MESSAG(1) = 'LEGEND_TITLE modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 10
            MESSAG(1) = 'TITLE modifier not entered on the LEGEND keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!
          IF( CEXIST('CONTOUR') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              LEGEND_CONTOUR = TMP_NAME
            ELSE
              IERR = 13
              MESSAG(1) = 'LEGEND_CONTOUR modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 14
            MESSAG(1) = 'CONTOUR modifier not entered on the LEGEND keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!------ Undefined Keyword ------------------------------------------------
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
      SUBROUTINE READ_KEY_ECEM_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading time information from the
!!    ECEM keyword information.
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
!!    Carmen Arimescu : 8 DEC 2004 : Original source
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ECEM_Mod
!
      IMPLICIT NONE
!
! *** User-defined functions
      LOGICAL, EXTERNAL :: CEXIST ! Part of the RDBLK suite of keyword routines
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=15) :: CALLER = 'READ_KEY_ECEM_2'
!
      INTEGER :: ITIM
      INTEGER :: IDXT
      INTEGER :: TEMPTIME
!
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
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
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          CLOSE( IKEY )
          RETURN
!
!-------TIME keyword---------------------------------------------------------------------
        CASE( 'TIMES' ) ! ===> TIME keyword
!
          IF( .NOT. (CEXIST('ALL') .OR. CEXIST('LIST')) ) THEN
            IERR = 1
            MESSAG(1) = 'The TIMES keyword did not have the ALL'
            MESSAG(2) = 'or LIST modifier on it.'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('ALL') ) THEN
            DO ITIM = 1, ECEM_NUM_TIM
              ECEM_TIM(ITIM)%COMP = .TRUE.
            END DO
          END IF
!
          IF( CEXIST('LIST') ) THEN
            IF( NVALUE .GT. 0 ) THEN
              DO ITIM = 1, NVALUE
                TEMPTIME = VALUE(ITIM)
                CALL MATCH_TIM_ECEM( TEMPTIME, IDXT )
                IF( IDXT .GT. 0 ) THEN
                  ECEM_TIM(IDXT)%COMP = .TRUE.
                ELSE
                  IERR = 3
                  MESSAG(1) = 'Time requested is not in the master list'
                  MESSAG(2) = 'Invalid time is '
                  WRITE(MESSAG(2)(17:),*) TEMPTIME
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              END DO
            ELSE
              IERR = 4
              MESSAG(1) = 'Expected one or more times in the list'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!------ Undefined Keyword ------------------------------------------------
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
      SUBROUTINE READ_KEY_ECDA_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading ECDA keyword control information
!!    with the exception of time.
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
!!    Carmen Arimescu : 8 DEC 2004 : Original source
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** User-defined functions
      LOGICAL, EXTERNAL :: CEXIST ! Part of the RDBLK suite of keyword routines
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=15) :: CALLER = 'READ_KEY_ECDA_1'
!
      INTEGER :: IDX   ! Temporary index variable
      REAL :: RTMP
!
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary storage for a quote string
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
      ILINE = 0
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!------------------------------------------------------------------------
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
!-------ANALYTE Keyword---------------------------------------------------
        CASE( 'ANALYTE' ) ! ===> ANALYTE Keyword
          IF( NQUOTE .GT. 0 ) ANA_USE = QUOTE(1)
!
!-------MEDIA Keyword---------------------------------------------------
        CASE( 'MEDIA' ) ! ===> MEDIA Keyword
          IF( NQUOTE .GT. 0 ) MED_USE = QUOTE(1)
!
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          REWIND( IKEY )
          RETURN
!
!------ PICK_REL Keyword---------------------------------------------------
        CASE( 'PICK_REL' )
          IF( NVALUE .GT. 0 ) REL_USE = VALUE(1)
!
!-------USER Keyword-------------------------------------------------------
        CASE( 'UNITS' ) ! ===> USER Keyword
!
          IF( CEXIST('OUTPUT') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              UNITS_OUTPUT = TMP_NAME
            ELSE
              IERR = 5
              MESSAG(1) = 'UNITS OUTPUT modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 6
            MESSAG(1) = 'OUTPUT modifier not entered on the UNITS keyword'
            CALL PRTERR( IERR, CALLER, 1 )
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
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 8
            MESSAG(1) = 'FACTOR modifier not entered on the UNITS keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!------ LEGEND keyword -----------------------------------------------------
        CASE( 'LEGEND' ) ! ===> USER Keyword
!
          IF( CEXIST('TITLE') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              LEGEND_TITLE = TMP_NAME
            ELSE
              IERR = 9
              MESSAG(1) = 'LEGEND_TITLE modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 10
            MESSAG(1) = 'TITLE modifier not entered on the LEGEND keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!
          IF( CEXIST('CONTOUR') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              LEGEND_CONTOUR = TMP_NAME
            ELSE
              IERR = 13
              MESSAG(1) = 'LEGEND_CONTOUR modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 14
            MESSAG(1) = 'CONTOUR modifier not entered on the LEGEND keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!------ Undefined Keyword ------------------------------------------------
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
      SUBROUTINE READ_KEY_ECDA_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading time information from the
!!    ECDA keyword information.
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
!!    Carmen Arimescu : 8 DEC 2004 : Original source
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
      USE ECDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      CHARACTER(LEN=15) :: CALLER = 'READ_KEY_ECDA_2'
!
      INTEGER :: ITIM
      INTEGER :: IDXT
      INTEGER :: TEMPTIME
!
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
!
! *** User-defined functions
!
      LOGICAL, EXTERNAL :: CEXIST ! Part of the RDBLK suite of keyword routines
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
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
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          CLOSE( IKEY )
          RETURN
!
!-------TIME keyword---------------------------------------------------------------------
        CASE( 'TIMES' ) ! ===> TIME keyword
!
          IF( .NOT. (CEXIST('ALL') .OR. CEXIST('LIST')) ) THEN
            IERR = 1
            MESSAG(1) = 'The TIMES keyword did not have the ALL'
            MESSAG(2) = 'or LIST modifier on it.'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('ALL') ) THEN
            DO ITIM = 1, ECDA_NTIMES
              ECDA_TIMES(ITIM)%COMP = .TRUE.
            END DO
          END IF
!
          IF( CEXIST('LIST') ) THEN
            IF( NVALUE .GT. 0 ) THEN
              DO ITIM = 1, NVALUE
                TEMPTIME = VALUE(ITIM)
                CALL MATCH_TIM_ECDA( TEMPTIME, IDXT )
                IF( IDXT .GT. 0 ) THEN
                  ECDA_TIMES(IDXT)%COMP = .TRUE.
                ELSE
                  IERR = 3
                  MESSAG(1) = 'Time requested is not in the master list'
                  MESSAG(2) = 'Invalid time is '
                  WRITE(MESSAG(2)(17:),*) TEMPTIME
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              END DO
            ELSE
              IERR = 4
              MESSAG(1) = 'Expected one or more times in the list'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!------ Undefined Keyword ------------------------------------------------
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
      SUBROUTINE READ_KEY_FCDA_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading FCDA keyword control information
!!    with the exception of time.
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
!!    Carmen Arimescu : 10 Jan 2005 : Original source
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
      USE FCDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      CHARACTER(LEN=15) :: CALLER = 'READ_KEY_FCDA_1'
!
      INTEGER :: IDX   ! Temporary index variable
      REAL :: RTMP
!
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary storage for a quote string
!
! *** User-defined functions
!
      LOGICAL, EXTERNAL :: CEXIST ! Part of the RDBLK suite of keyword routines
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
      ILINE = 0
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!------------------------------------------------------------------------
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
!-------ANALYTE Keyword---------------------------------------------------
        CASE( 'ANALYTE' ) ! ===> ANALYTE Keyword
          IF( NQUOTE .GT. 0 ) ANA_USE = QUOTE(1)
!
!-------SOIL Keyword---------------------------------------------------
        CASE( 'SOIL' ) ! ===> SOIL Keyword
          IF( NQUOTE .GT. 0 ) STY_USE = QUOTE(1)
!
!-------SPECIES Keyword---------------------------------------------------
        CASE( 'SPECIES' ) ! ===> SPECIES Keyword
          IF( NQUOTE .GT. 0 ) SPC_USE = QUOTE(1)
!
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          REWIND( IKEY )
          RETURN
!
!------ PICK_REL Keyword---------------------------------------------------
        CASE( 'PICK_REL' )
          IF( NVALUE .GT. 0 ) REL_USE = VALUE(1)
!
!-------USER Keyword-------------------------------------------------------
        CASE( 'UNITS' ) ! ===> USER Keyword
!
          IF( CEXIST('OUTPUT') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              UNITS_OUTPUT = TMP_NAME
            ELSE
              IERR = 5
              MESSAG(1) = 'UNITS OUTPUT modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 6
            MESSAG(1) = 'OUTPUT modifier not entered on the UNITS keyword'
            CALL PRTERR( IERR, CALLER, 1 )
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
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 8
            MESSAG(1) = 'FACTOR modifier not entered on the UNITS keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!------ LEGEND keyword -----------------------------------------------------
        CASE( 'LEGEND' ) ! ===> USER Keyword
!
          IF( CEXIST('TITLE') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              LEGEND_TITLE = TMP_NAME
            ELSE
              IERR = 9
              MESSAG(1) = 'LEGEND_TITLE modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 10
            MESSAG(1) = 'TITLE modifier not entered on the LEGEND keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!
          IF( CEXIST('CONTOUR') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              LEGEND_CONTOUR = TMP_NAME
            ELSE
              IERR = 13
              MESSAG(1) = 'LEGEND_CONTOUR modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 14
            MESSAG(1) = 'CONTOUR modifier not entered on the LEGEND keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!------ Undefined Keyword ------------------------------------------------
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
      SUBROUTINE READ_KEY_FCDA_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading time information from the
!!    FCDA keyword information.
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
!!    Carmen Arimescu : 10 Jan 2005 : Original source
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
      USE FCDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      CHARACTER(LEN=15) :: CALLER = 'READ_KEY_FCDA_2'
!
      INTEGER :: ITIM
      INTEGER :: IDXT
      INTEGER :: TEMPTIME
!
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
!
! *** User-defined functions
!
      LOGICAL, EXTERNAL :: CEXIST ! Part of the RDBLK suite of keyword routines
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
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
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          CLOSE( IKEY )
          RETURN
!
!-------TIME keyword---------------------------------------------------------------------
        CASE( 'TIMES' ) ! ===> TIME keyword
!
          IF( .NOT. (CEXIST('ALL') .OR. CEXIST('LIST')) ) THEN
            IERR = 1
            MESSAG(1) = 'The TIMES keyword did not have the ALL'
            MESSAG(2) = 'or LIST modifier on it.'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('ALL') ) THEN
            DO ITIM = 1, FCDA_NTIMES
              FCDA_TIM(ITIM)%TEC_COMP = .TRUE.   ! user times
            END DO
          END IF
!
          IF( CEXIST('LIST') ) THEN
            IF( NVALUE .GT. 0 ) THEN
              DO ITIM = 1, NVALUE
                TEMPTIME = VALUE(ITIM)
                CALL MATCH_TIM_FCDA( TEMPTIME, IDXT )
                IF( IDXT .GT. 0 ) THEN
                  FCDA_TIM(IDXT)%TEC_COMP = .TRUE.   ! user times
                ELSE
                  IERR = 3
                  MESSAG(1) = 'Time requested is not in the master list'
                  MESSAG(2) = 'Invalid time is '
                  WRITE(MESSAG(2)(17:),*) TEMPTIME
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              END DO
            ELSE
              IERR = 4
              MESSAG(1) = 'Expected one or more times in the list'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!------ Undefined Keyword ------------------------------------------------
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
      SUBROUTINE READ_KEY_HUMAN_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading HUMAN keyword control information
!!    with the exception of time.
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
!!    Carmen Arimescu : 18 Nov 2004 : Original source
!!    Carmen Arimescu :  7 Mar 2006 : Add a new keyword (LEGEND)
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
! *** Call list variables
!
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      CHARACTER(LEN=16) :: CALLER = 'READ_KEY_HUMAN_1'
!
      INTEGER :: IDX   ! Temporary index variable
!                        report file name as the first entry
      REAL :: RTMP
!
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary storage for a quote string
!
! *** User-defined functions
!
      LOGICAL, EXTERNAL :: CEXIST ! Part of the RDBLK suite of keyword routines
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
      ILINE = 0
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!------------------------------------------------------------------------
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
!------ ANALYTE Keyword --------------------------------------------------
        CASE( 'ANALYTE' ) ! ===> ANALYTE Keyword
          IF( NQUOTE .GT. 0 ) ANA_USE = QUOTE(1)
!
!------ ANALTYPE Keyword -------------------------------------------------
        CASE( 'ANALTYPE' ) ! ===> ANALTYPE Keyword
          IF( NQUOTE .GT. 0 ) ATY_USE = QUOTE(1)
!
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          REWIND( IKEY )
          RETURN
!
!------ PICK_REL Keyword --------------------------------------------------
        CASE( 'PICK_REL' )
          IF( NVALUE .GT. 0 ) REL_USE = VALUE(1)
!
!------ SOLNTYPE Keyword --------------------------------------------------
        CASE( 'SOLUTION' ) ! ===> SOLUTION Keyword
          IF( NQUOTE .GT. 0 ) SLT_USE = QUOTE(1)
!
!------ UNITS keyword -----------------------------------------------------
        CASE( 'UNITS' ) ! ===> USER Keyword
!
          IF( CEXIST('OUTPUT') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              UNITS_OUTPUT = TMP_NAME
            ELSE
              IERR = 5
              MESSAG(1) = 'UNITS OUTPUT modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 6
            MESSAG(1) = 'OUTPUT modifier not entered on the UNITS keyword'
            CALL PRTERR( IERR, CALLER, 1 )
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
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 8
            MESSAG(1) = 'FACTOR modifier not entered on the UNITS keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!------ LEGEND keyword -----------------------------------------------------
        CASE( 'LEGEND' ) ! ===> USER Keyword
!
          IF( CEXIST('TITLE') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              LEGEND_TITLE = TMP_NAME
            ELSE
              IERR = 9
              MESSAG(1) = 'LEGEND_TITLE modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 10
            MESSAG(1) = 'TITLE modifier not entered on the LEGEND keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!
          IF( CEXIST('CONTOUR') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              LEGEND_CONTOUR = TMP_NAME
            ELSE
              IERR = 13
              MESSAG(1) = 'LEGEND_CONTOUR modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 14
            MESSAG(1) = 'CONTOUR modifier not entered on the LEGEND keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!------ Undefined Keyword ------------------------------------------------
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
      SUBROUTINE READ_KEY_HUMAN_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading time information from the
!!    HUMAN keyword information.
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
!!    Carmen Arimescu : 16 NOV 2004 : Original source
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
      USE Human_Mod
!
      IMPLICIT NONE
!
! *** User-defined functions
!
      LOGICAL, EXTERNAL :: CEXIST ! Part of the RDBLK suite of keyword routines
!
! *** Call list variables
!
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      CHARACTER(LEN=16) :: CALLER = 'READ_KEY_HUMAN_2'
!
      INTEGER :: ITIM
      INTEGER :: IDXT
      INTEGER :: TEMPTIME
!
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
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
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          CLOSE( IKEY )
          RETURN
!
!------ TIMES keyword ----------------------------------------------------
        CASE( 'TIMES' ) ! ===> TIME keyword
!
          IF( .NOT. (CEXIST('ALL') .OR. CEXIST('LIST')) ) THEN
            IERR = 1
            MESSAG(1) = 'The TIMES keyword did not have the ALL'
            MESSAG(2) = 'or LIST modifier on it.'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('ALL') ) THEN
            DO ITIM = 1, HUMAN_NUM_TIM
              HUMAN_TIM(ITIM)%COMP = .TRUE.
            END DO
          END IF
!
          IF( CEXIST('LIST') ) THEN
            IF( NVALUE .GT. 0 ) THEN
              DO ITIM = 1, NVALUE
                TEMPTIME = VALUE(ITIM)
                CALL MATCH_TIM_HUMAN( TEMPTIME, IDXT )
                IF( IDXT .GT. 0 ) THEN
                  HUMAN_TIM(IDXT)%COMP = .TRUE.
                ELSE
                  IERR = 3
                  MESSAG(1) = 'Time requested is not in the master list'
                  MESSAG(2) = 'Invalid time is '
                  WRITE(MESSAG(2)(17:),*) TEMPTIME
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              END DO
            ELSE
              IERR = 4
              MESSAG(1) = 'Expected one or more times in the list'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!------ Undefined Keyword ------------------------------------------------
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
      SUBROUTINE READ_KEY_CULTURE_1( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading CULTURE keyword control information
!!    with the exception of time.
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
!!    Carmen Arimescu : 19 APR 2006 : Original source
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
      USE CULTURE_MOD
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      CHARACTER(LEN=18) :: CALLER = 'READ_KEY_CULTURE_1'
!
      INTEGER :: IDX   ! Temporary index variable
!                        report file name as the first entry
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary storage for a quote string
!
! *** User-defined functions
!
      LOGICAL, EXTERNAL :: CEXIST ! Part of the RDBLK suite of keyword routines
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
      ILINE = 0
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!------------------------------------------------------------------------
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
!------ ANALYTE Keyword --------------------------------------------------
        CASE( 'MAP' ) ! ===> MAP Keyword
          IF( NQUOTE .GT. 0 ) MAP_USE = QUOTE(1)
!
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          REWIND( IKEY )
          RETURN
!
!------ PICK_REL Keyword --------------------------------------------------
        CASE( 'PICK_REL' )
          IF( NVALUE .GT. 0 ) REL_USE = VALUE(1)
!
!------ LEGEND keyword -----------------------------------------------------
        CASE( 'LEGEND' ) ! ===> USER Keyword
!
          IF( CEXIST('TITLE') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              LEGEND_TITLE = TMP_NAME
            ELSE
              IERR = 9
              MESSAG(1) = 'LEGEND_TITLE modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 10
            MESSAG(1) = 'TITLE modifier not entered on the LEGEND keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!
          IF( CEXIST('CONTOUR') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              LEGEND_CONTOUR = TMP_NAME
            ELSE
              IERR = 13
              MESSAG(1) = 'LEGEND_CONTOUR modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 14
            MESSAG(1) = 'CONTOUR modifier not entered on the LEGEND keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!------ Undefined Keyword ------------------------------------------------
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
      SUBROUTINE READ_KEY_CULTURE_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading time information from the
!!    CULTURE keyword information.
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
!!    Carmen Arimescu : 20 APR 2006 : Original source
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
      USE CULTURE_Mod
!
      IMPLICIT NONE
!
! *** User-defined functions
!
      LOGICAL, EXTERNAL :: CEXIST ! Part of the RDBLK suite of keyword routines
!
! *** Call list variables
!
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
!
      CHARACTER(LEN=18) :: CALLER = 'READ_KEY_CULTURE_2'
!
      INTEGER :: ITIM
      INTEGER :: IDXT
      INTEGER :: TEMPTIME
!
      CHARACTER :: TITLE*(LENCRD)       ! Temporary storage of the keyword line
!
!---- First executable code --------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK and error checking
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
!------ END Keyword ------------------------------------------------------
        CASE( 'END' )
          CLOSE( IKEY )
          RETURN
!
!------ TIMES keyword ----------------------------------------------------
        CASE( 'TIMES' ) ! ===> TIME keyword
!
          IF( .NOT. (CEXIST('ALL') .OR. CEXIST('LIST')) ) THEN
            IERR = 1
            MESSAG(1) = 'The TIMES keyword did not have the ALL'
            MESSAG(2) = 'or LIST modifier on it.'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('ALL') ) THEN
            DO ITIM = 1, CULTURE_NUM_TIM
              CULTURE_TIM(ITIM)%COMP = .TRUE.
            END DO
          END IF
!
          IF( CEXIST('LIST') ) THEN
            IF( NVALUE .GT. 0 ) THEN
              DO ITIM = 1, NVALUE
                TEMPTIME = VALUE(ITIM)
                CALL MATCH_TIM_CULTURE( TEMPTIME, IDXT )
                IF( IDXT .GT. 0 ) THEN
                  CULTURE_TIM(IDXT)%COMP = .TRUE.
                ELSE
                  IERR = 3
                  MESSAG(1) = 'Time requested is not in the master list'
                  MESSAG(2) = 'Invalid time is '
                  WRITE(MESSAG(2)(17:),*) TEMPTIME
                  CALL PRTERR( IERR, CALLER, 2 )
                  RETURN
                END IF
              END DO
            ELSE
              IERR = 4
              MESSAG(1) = 'Expected one or more times in the list'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!------ Undefined Keyword ------------------------------------------------
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
!!
!!**********************************************************************
!
      USE Files_Mod
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
      SUBROUTINE ECDA_MAPREAD( FN_MAP, UN_MAP, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine reads an ASCII ECDA map file.  It allocates
!!    memory as it works its way through the information in the file.
!!    Error checking is performed to ensure that the allocate
!!    statements have a chance of successful completion.
!!
!!  History:
!!
!!    Kelly Lessor     : 17 May 2000 : Version 1.0
!!    Paul W. Eslinger : 21 Oct 2002 : SAC Rev. 1
!!    Carmen Arimescu  : 13 Jan 2005 : Changed for ANIMATE
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
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
!
      CHARACTER(LEN=*), INTENT(IN) :: FN_MAP ! File name for the map file
      INTEGER :: UN_MAP       ! Unit number for the map file
      INTEGER :: IERR         ! Error number
!
! *** Local variables
!
      CHARACTER(LEN=12) :: CALLER = 'ECDA_MAPREAD' ! Name of this routine
!
      INTEGER :: IOS             ! I/O Status from open, read, or close of file
      INTEGER :: J, K            ! Index variables for writing
      INTEGER :: NUM_TMP         ! Temporary counter variable
      CHARACTER(LEN=6) :: TMP_ID ! Temporary ID for error checking
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
      UN_MAP = GET_UNIT_NUMBER(  )
      IF( UN_MAP .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'System error getting a unit number for the ECDA map file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the ECDA map file
!
      OPEN(UNIT=UN_MAP, IOSTAT=IOS, STATUS='OLD', FILE=FN_MAP )
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error opening ECDA map file'
        WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
        MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
! *** Read the header lines from the map file
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_PTITLE
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_PRGNAM
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_PRGVER
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_PRGDAT
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_USRNAM
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_CRUNID
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_BLOCK
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_RECLEN
!
! *** Read the realizations information
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_NREAL
      IF(ECDA_NREAL .GT. 0 ) THEN
        ALLOCATE( CONC(ECDA_NREAL) )
      ELSE
        IERR = 3
        MESSAG(1) = 'Invalid number of realizations'
        MESSAG(2) = 'Problems reading the header file'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
 1010 FORMAT(A,I0)
!
! *** Read the time information
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_NTIMES
      IF( ECDA_NTIMES .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) ='ECDA_NTIMES must be greater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ECDA_TIMES(ECDA_NTIMES), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) ='Error allocating ECDA_TIMES(ECDA_NTIMES)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      DO J = 1, ECDA_NTIMES
        READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_TIMES(J)%TIME
        ECDA_TIMES(J)%COMP = .FALSE.
      END DO
!
! *** Read the location information
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_NLOCS
      IF( ECDA_NLOCS .LT. 1 ) THEN
        IERR = 6
        MESSAG(1) ='ECDA_NLOCS must be greater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
      ALLOCATE( ECDA_LOC(ECDA_NLOCS), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) ='Error allocating ECDA_LOC(ECDA_NLOCS)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
      DO J = 1, ECDA_NLOCS
        READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) ECDA_LOC(J)%ID
        ECDA_LOC(J)%COMP = .FALSE.
      END DO
!
! *** Read and discard the media information
!     These will be used by SACView but are hard-coded in this program
!
      READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) NUM_TMP
      IF( NUM_TMP .LT. 1 ) THEN
        IERR = 8
        MESSAG(1) ='Number of media must be greater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
      DO J = 1, NUM_TMP
        READ(UNIT=UN_MAP, IOSTAT=IOS, ERR=7000, FMT=*) TMP_ID
      END DO
!
! *** Read the map index data from the map file
!
      ALLOCATE( ECDA_LOC_MED(ECDA_NLOCS,ECDA_NMED), STAT= IOS )
      IF( IOS .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) ='Error allocating ECDA_LOC_MED(ECDA_NLOCS,ECDA_NMED)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
      DO K = 1, ECDA_NLOCS
        READ(UN_MAP,*,ERR=8000) TMP_ID, (ECDA_LOC_MED(K,J),J=1,ECDA_NMED)
        IF( TMP_ID .NE. ECDA_LOC(K)%ID ) THEN
          IERR = 10
          MESSAG(1) = 'Mismatch on header location ID and record index ID tag'
          MESSAG(2) = 'Location ID from header = ' // TRIM(ECDA_LOC(K)%ID)
          MESSAG(3) = 'Record index ID tag was = ' // TRIM(TMP_ID)
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
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
      IERR = 11
      MESSAG(1) = 'Error reading header lines from the ECDA map file'
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** Error Branch: The map data were not read successfully
!
 8000 CONTINUE
      IERR = 12
      MESSAG(1) = 'Error reading record number lines from the ECDA map file'
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** Error Branch: The ECDA map file was not closed successfully
!
 9000 CONTINUE
      IERR = 13
      MESSAG(1) = 'Error closing ECDA map file'
      WRITE(MESSAG(2), *) 'UNIT = ', UN_MAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FN_MAP = ' // TRIM(FN_MAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ECDA_OPEN( FN_ECDA, UN_ECDA, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine opens an ECDA file and reads the header lines.
!!
!!  History:
!!
!!    Kelly Lessor     : 27 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  9 Oct 2002 : SAC Rev. 1
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
! *** Call list variables
!
      CHARACTER(LEN=*), INTENT(IN) :: FN_ECDA ! File name for the ECDA file
      INTEGER, INTENT(IN)  :: UN_ECDA ! Unit number for the ECDA file
      INTEGER, INTENT(OUT) :: IERR    ! Error Number
!
! *** Local variables
!
      CHARACTER(LEN=9) :: CALLER = 'ECDA_OPEN' ! Name of this routine
      INTEGER :: IOS ! I/O Status
      INTEGER :: J   ! Index variable for writing
!
!---- Executable code ---------------------------------------------------
!
! *** Open the ECDA file as read-only
!
      OPEN( UNIT=UN_ECDA, IOSTAT=IOS, ERR=1000, FILE=FN_ECDA, STATUS='OLD', ACCESS='DIRECT', ACTION='READ', RECL=ECDA_RECLEN)
!
! *** Read the header lines from the ECDA file
!
      READ(UNIT=UN_ECDA, IOSTAT=IOS, REC=1, ERR=2000) ECDA_ANALYTE
      DO J = 1, ECDA_NMED
        READ(UNIT=UN_ECDA, IOSTAT=IOS, REC=(J+1), ERR=2000) ECDA_UNITS(J)
      END DO
!
! *** Normal exit
!
      RETURN
!
! *** An error occurred opening the ECDA file
!
 1000 CONTINUE
!
      IERR = 1
      MESSAG(1) = 'An error occurred opening the ECDA file'
      MESSAG(2) = 'File name is ' // TRIM(FN_ECDA)
      MESSAG(3) = ' '
      WRITE(MESSAG(3), FMT='(A,I0)') 'IOSTAT = ', IOS
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** An error occurred reading the header lines from the ECDA file
!
 2000 CONTINUE
!
      IERR = 2
      MESSAG(1) = 'An error occurred reading the header lines from the ECDA file'
      MESSAG(2) = 'File name is ' // TRIM(FN_ECDA)
      MESSAG(3) = ' '
      WRITE(MESSAG(3), FMT='(A,I0)') 'IOSTAT = ', IOS
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE ECDA_READ( NREC, TIME, LOC_ID, MED_ID, CONC, NREAL, ICON, IERR )
!!*******************************************************************************
!!
!!  Purpose:
!!
!!    This routine reads a single data record from an open ECDA file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 29 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!
!!********************************************************************************
!
! *** Global variables
!
      USE Errors_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NREC  ! Record number
      INTEGER :: TIME              ! Calendar year for concentration data
      CHARACTER(LEN=6) :: LOC_ID   ! Location ID
      CHARACTER(LEN=4) :: MED_ID   ! Media ID
      REAL, DIMENSION(*) :: CONC   ! Concentration vector of length NREAL
      INTEGER, INTENT(IN) :: NREAL ! Number of realizations to write
      INTEGER, INTENT(IN) :: ICON  ! Fortran unit number to use for output
      INTEGER :: IERR              ! Error number indicator
!
! *** Local variables
!
      CHARACTER(LEN=9) :: CALLER = 'ECDA_READ'
      INTEGER :: IOS  ! System error number
      INTEGER :: IREL ! Realization looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
      IF( NREAL .LE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Number of realizations must be greater than zero'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      READ( UNIT=ICON, IOSTAT=IOS, REC=NREC ) TIME, LOC_ID, MED_ID, (CONC(IREL),IREL=1,NREAL)
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error reading from the ECDA concentration file'
        MESSAG(2) = ' '
        WRITE(MESSAG(2),'(A,I3,A,I5)') 'UNIT = ',ICON, ' IOSTAT = ',IOS
        CALL PRTERR(IERR, CALLER, 2)
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE ECDA_RECNO_INDEX( ITIM, ILOC, IMED, NREC, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine returns a record number in the data file given the
!!    indices for time, location ID, and media ID.
!!
!!  History:
!!
!!    Paul W. Eslinger : 29 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 10 Oct 2002 : SAC Rev. 1
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
      USE Ecda_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER, INTENT(IN) :: IMED ! Media index
      INTEGER :: NREC             ! Record number
      INTEGER :: IERR             ! Error number indicator
!
! *** Local variables
!
      CHARACTER(LEN=16) :: CALLER = 'ECDA_RECNO_INDEX'
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check the time index
!
      IF( ITIM.LT.1 .OR. ITIM.GT.ECDA_NTIMES ) THEN
        IERR = 1
        MESSAG(1) = 'Time index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ITIM
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ECDA_NTIMES
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Check the location index
!
      IF( ILOC.LT.1 .OR. ILOC.GT.ECDA_NLOCS ) THEN
        IERR = 2
        MESSAG(1) = 'Location index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ILOC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ECDA_NLOCS
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Check the media index
!
      IF( IMED.LT.1 .OR. IMED.GT.ECDA_NMED ) THEN
        IERR = 3
        MESSAG(1) = 'Media index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) IMED
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ECDA_NMED
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Compute the data index
!
      NREC = ECDA_LOC_MED(ILOC,IMED)
      IF( NREC .LT. 0 ) RETURN
!
      NREC = (ITIM-1)*ECDA_BLOCK + NREC
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE FCDA_MAPREAD( FNMAP, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads an ASCII FCDA map file.
!!
!!  Limitations:
!!
!!    This subroutine can only be used in the context of code that has
!!    read and stored time, location, and species information from the
!!    ESD keyword file.
!!
!!  Auxiliary Routines:
!!
!!    GET_UNIT_NUMBER - Gets a unit number to use when writing the
!!                      file.  The unit is closed at the end of the
!!                      writing activities.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Mar 2003 : SAC Rev. 1
!!    Carmen Arimescu  : 10 Jan 2005 : modified for the animation program
!!
!!**********************************************************************
!
! *** Global variables
!
      USE FCDA_Mod
      USE Errors_Mod
      USE Control_Mod, ONLY: SPC_USE
!
      IMPLICIT NONE
!
! *** External functions
!
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
!
      CHARACTER(LEN=*), INTENT(IN) :: FNMAP ! Name of the map file
      INTEGER, INTENT(INOUT) :: IERR ! Error Number
!
! *** Local variables
!
      CHARACTER(LEN=12) :: CALLER = 'FCDA_MAPREAD' ! Name of this routine
!
      INTEGER :: IMAP  ! Unit number for the record number map file
      INTEGER :: IOS   ! I/O Status from open, write, or close of file
      INTEGER :: IERA  ! Error status variable from the allocate action
      INTEGER :: J, K  ! Index variables for writing
      LOGICAL :: THERE ! Temporary logical variable
!
      CHARACTER(LEN=24) :: CTMP            ! Temporary character variable
      CHARACTER(LEN=6) :: TMP_ID           ! Temporary identification variable
      INTEGER :: ISTAT                     ! System status variable
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
!--------------------------------------------------------------------------
!     File handling
!--------------------------------------------------------------------------
!
! *** Get a unit number for the map file
      IMAP = GET_UNIT_NUMBER( )
      IF( IMAP .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Could not obtain a free unit number for the FCDA map file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Check if the requested file exists
      INQUIRE(FILE=FNMAP,EXIST=THERE)
      IF( .NOT.THERE ) THEN
        IERR = 2
        MESSAG(1) = 'The requested FCDA map file was not found'
        MESSAG(2) = 'File: ' // TRIM(FNMAP)
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Open the FCDA map file as a sequential ASCII file
      OPEN(UNIT=IMAP,FILE=FNMAP,IOSTAT=IOS,STATUS='OLD')
      IF( IOS .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error opening the FCDA map file'
        MESSAG(2) = ' '
        MESSAG(3) = 'FNMAP = ' // TRIM(FNMAP)
        CALL PRTERR(IERR, CALLER, 3)
        RETURN
      END IF
!
!--------------------------------------------------------------------------
!     Header-type variables
!--------------------------------------------------------------------------
!
! *** Read the character header lines
      READ(IMAP,*,ERR=7000) FCDA_PTITLE
      READ(IMAP,*,ERR=7000) FCDA_PRGNAM
      READ(IMAP,*,ERR=7000) FCDA_PRGVER
      READ(IMAP,*,ERR=7000) FCDA_PRGDAT
      READ(IMAP,*,ERR=7000) FCDA_USRNAM
      READ(IMAP,*,ERR=7000) FCDA_CRUNID
!
! *** Read block size for the file
      READ(IMAP,*,ERR=7000) FCDA_BLOCK_AQ
      READ(IMAP,*,ERR=7000) FCDA_BLOCK_RP
      READ(IMAP,*,ERR=7000) FCDA_BLOCK_UP
!
! *** Read record length for the file
      READ(IMAP,*,ERR=7000) FCDA_RECLEN
!
! *** Read number of realizations
      READ(IMAP,*,ERR=7000) FCDA_NREAL
      IF(FCDA_NREAL .GT. 0 ) THEN
        ALLOCATE( CONC(FCDA_NREAL) )
      ELSE
        IERR = 4
        MESSAG(1) = 'Invalid number of realizations'
        MESSAG(2) = 'Problems reading the MAP file'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      ENDIF
!
! *** Read number of header lines
      READ(IMAP,*,ERR=7000) FCDA_NHEAD
!
!--------------------------------------------------------------------------
!     Time information
!--------------------------------------------------------------------------
!
      READ(IMAP,*,ERR=7000) FCDA_NTIMES
      IF( FCDA_NTIMES .LT. 1 ) THEN
        IERR = 5
        MESSAG(1) = 'FCDA_NTIMES must be grater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!     Allocate the space to store data
      ALLOCATE( FCDA_TIM(FCDA_NTIMES), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating FCDA_TIM(FCDA_NTIMES)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!     Read the data
      DO J = 1, FCDA_NTIMES
        READ(IMAP,*,ERR=7000) FCDA_TIM(J)%TIME, FCDA_TIM(J)%COMP
        FCDA_TIM(J)%TEC_COMP = .FALSE.
      END DO
!
!--------------------------------------------------------------------------
!     Location information
!--------------------------------------------------------------------------
!
      READ(IMAP,*,ERR=7000) FCDA_NLOCS
      IF( FCDA_NLOCS .LT. 1 ) THEN
        IERR = 7
        MESSAG(1) = 'FCDA_NLOCS must be grater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!     Allocate the space to store data
      ALLOCATE( FCDA_LOC(FCDA_NLOCS), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating FCDA_LOC(FCDA_LOCS)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
! CA
!     Read the data
      DO J = 1, FCDA_NLOCS
        READ(IMAP,*,ERR=7000) FCDA_LOC(J)%ID, FCDA_LOC(J)%COMP, FCDA_LOC(J)%NAME
        FCDA_LOC(J)%TEC_COMP = .FALSE.
      END DO
!
!--------------------------------------------------------------------------
!     Species information
!--------------------------------------------------------------------------
!
      READ(IMAP,*,ERR=7000) FCDA_NSPC
      IF( FCDA_NSPC .LT. 1 ) THEN
        IERR = 9
        MESSAG(1) = 'FCDA_NSPC must be grater than 0'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!     Allocate the space to store data
      ALLOCATE( FCDA_SPC(FCDA_NSPC), STAT= ISTAT )
      IF(ISTAT .NE. 0 ) THEN
        IERR = 10
        MESSAG(1) = 'Error allocating FCDA_SPC(FCDA_NSPC)'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
!     Read the data
      SPC_IDX = 0
      DO J = 1, FCDA_NSPC
        READ(IMAP,*,ERR=7000) FCDA_SPC(J)%ID, FCDA_SPC(J)%COMP, FCDA_SPC(J)%NAME
        IF( SPC_USE .EQ. FCDA_SPC(J)%ID ) SPC_IDX = J
      END DO
      IF( SPC_IDX .EQ. 0 ) THEN
        IERR = 101
        MESSAG(1) = 'Input species was not found in the master species list'
        MESSAG(2) = 'Input selection was '//TRIM(SPC_USE)
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!--------------------------------------------------------------------------
!     Map information
!--------------------------------------------------------------------------
!
!     Allocate space for aquatic species map
      ALLOCATE( FCDA_MAP_AQ(FCDA_NLOCS), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 11
        MESSAG(1) = 'Error allocating memory for FCDA_MAP_AQ'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!     Set the entire vector to -1
      FCDA_MAP_AQ = -1
!
!     Allocate space for riparian species map
      ALLOCATE( FCDA_MAP_RP(FCDA_NLOCS), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 12
        MESSAG(1) = 'Error allocating memory for FCDA_MAP_RP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!     Set the entire array to -1
      FCDA_MAP_RP = -1
!
!     Allocate space for upland species map
      ALLOCATE( FCDA_MAP_UP(FCDA_NLOCS,ESD_NUM_SOI), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 13
        MESSAG(1) = 'Error allocating memory for FCDA_MAP_UP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!     Set the entire array to -1
      FCDA_MAP_UP = -1
!
! *** Read map index data for aquatic species
      READ(IMAP,*,ERR=8000) CTMP
      IF( CTMP .NE. 'Map: Aquatic Species' ) THEN
        IERR = 14
        MESSAG(1) = 'Data mismatch in the map file'
        MESSAG(2) = 'Expecting the string "Map: Aquatic Species"'
        MESSAG(3) = 'Obtained the string: "' // TRIM(CTMP) // '"'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
      DO K = 1, FCDA_NLOCS
        READ(IMAP,*,ERR=8000) TMP_ID, FCDA_MAP_AQ(K)
        IF( FCDA_LOC(K)%ID .NE. TMP_ID ) THEN
          IERR = 15
          MESSAG(1) = 'Mismatch on locations in the FCDA map file and the'
          MESSAG(2) = 'Locations in the map record set - aquatic'
          MESSAG(3) = 'FCDA: "' // FCDA_LOC(K)%ID // '"'
          MESSAG(4) = 'MAP:  "' // TMP_ID // '"'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END DO
!
! *** Read map index data for riparian species
      READ(IMAP,*,ERR=8000) CTMP
      IF( CTMP .NE. 'Map: Riparian Species' ) THEN
        IERR = 16
        MESSAG(1) = 'Data mismatch in the map file'
        MESSAG(2) = 'Expecting the string "Map: Riparian Species"'
        MESSAG(3) = 'Obtained the string: "' // TRIM(CTMP) // '"'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
      DO K = 1, FCDA_NLOCS
        READ(IMAP,*,ERR=8000) TMP_ID, FCDA_MAP_RP(K)
        IF( FCDA_LOC(K)%ID .NE. TMP_ID ) THEN
          IERR = 17
          MESSAG(1) = 'Mismatch on locations in the FCDA map file and the'
          MESSAG(2) = 'Locations in the map record set - riparian'
          MESSAG(3) = 'FCDA: "' // FCDA_LOC(K)%ID // '"'
          MESSAG(4) = 'MAP:  "' // TMP_ID // '"'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END DO
!
! *** Read map index data for upland species
      READ(IMAP,*,ERR=8000) CTMP
      IF( CTMP .NE. 'Map: Upland Species' ) THEN
        IERR = 18
        MESSAG(1) = 'Data mismatch in the map file'
        MESSAG(2) = 'Expecting the string "Map: Upland Species"'
        MESSAG(3) = 'Obtained the string: "' // TRIM(CTMP) // '"'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
      DO K = 1, FCDA_NLOCS
        READ(IMAP,*,ERR=8000) TMP_ID, FCDA_MAP_UP(K,1)
        IF( FCDA_LOC(K)%ID .NE. TMP_ID ) THEN
          IERR = 19
          MESSAG(1) = 'Mismatch on locations in the FCDA map file and the'
          MESSAG(2) = 'Locations in the map record set - upland'
          MESSAG(3) = 'FCDA: "' // FCDA_LOC(K)%ID // '"'
          MESSAG(4) = 'MAP:  "' // TMP_ID // '"'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        READ(IMAP,*,ERR=8000) TMP_ID, FCDA_MAP_UP(K,2)
        IF( FCDA_LOC(K)%ID .NE. TMP_ID ) THEN
          IERR = 20
          MESSAG(1) = 'Mismatch on locations in the FCDA map file and the'
          MESSAG(2) = 'locations in the map record set - upland'
          MESSAG(3) = 'FCDA: "' // FCDA_LOC(K)%ID // '"'
          MESSAG(4) = 'MAP:  "' // TMP_ID // '"'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
        READ(IMAP,*,ERR=8000) TMP_ID, FCDA_MAP_UP(K,3)
        IF( FCDA_LOC(K)%ID .NE. TMP_ID ) THEN
          IERR = 21
          MESSAG(1) = 'Mismatch on locations in the FCDA map file and the'
          MESSAG(2) = 'locations in the map record set - upland'
          MESSAG(3) = 'FCDA: "' // FCDA_LOC(K)%ID // '"'
          MESSAG(4) = 'MAP:  "' // TMP_ID // '"'
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END DO
!
! *** Close the FCDA map file and return (normal exit point)
!
      CLOSE(IMAP)
      RETURN
!
! *** Error branch: The header lines were not written successfully
!
 7000 CONTINUE
      IERR = 22
      MESSAG(1) = 'Error reading header lines from the FCDA map file'
      MESSAG(2) = ' '
      WRITE(MESSAG(2), *) 'UNIT = ', IMAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FNMAP = ' // TRIM(FNMAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
! *** Error branch: The map data were not read successfully
!
 8000 CONTINUE
      IERR = 23
      MESSAG(1) = 'Error reading record number data from the FCDA map file'
      MESSAG(2) = ' '
      WRITE(MESSAG(2), *) 'UNIT = ', IMAP, ' IOSTAT = ', IOS
      MESSAG(3) = 'FNMAP = ' // TRIM(FNMAP)
      CALL PRTERR(IERR, CALLER, 3)
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE FCDA_OPEN( FN_FCDA, UN_FCDA, FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!   This routine reads a single data record from an open FCDA file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Mar 2003 : SAC Rev. 1
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
      USE FCDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      CHARACTER(LEN=*), INTENT(IN) :: FN_FCDA   ! File name for the FCDA file
      INTEGER, INTENT(IN) :: UN_FCDA            ! Unit number for the FCDA file
      CHARACTER(LEN=*) :: FCDA_ANA_ID           ! Analyte ID:  First header line
      CHARACTER(LEN=*) :: FCDA_SPC_ID           ! Species ID:  Second header line
      CHARACTER(LEN=*) :: FCDA_SPC_HAB          ! Species habitat: Third header line
      INTEGER :: IERR                           ! Error Number
!
! *** Local variables
!
      CHARACTER(LEN=9) :: CALLER = 'FCDA_OPEN' ! Name of this routine
!
!---- Executable code ---------------------------------------------------
!
! *** Open the FCDA file
!
      OPEN(UN_FCDA, ERR=1000, FILE=FN_FCDA, STATUS='OLD', ACCESS='DIRECT', RECL=FCDA_RECLEN)
!
! *** Read the header lines from the FCDA file
!
      READ(UN_FCDA, REC=1, ERR=2000) FCDA_ANA_ID
      READ(UN_FCDA, REC=2, ERR=2000) FCDA_SPC_ID
      READ(UN_FCDA, REC=3, ERR=2000) FCDA_SPC_HAB
!
! *** Normal exit
!
      RETURN
!
! *** An error occurred opening the file
!
 1000 CONTINUE
      IERR = 1
      MESSAG(1) = 'An error occurred opening the FCDA file'
      MESSAG(2) = 'File name is ' // TRIM(FN_FCDA)
      CALL PRTERR( IERR, CALLER, 2 )
      RETURN
!
! *** An error occurred reading the header lines from the file
!
 2000 CONTINUE
      IERR = 2
      MESSAG(1) = 'An error occurred reading the header lines from the FCDA file'
      MESSAG(2) = 'File name is ' // TRIM(FN_FCDA)
      CALL PRTERR( IERR, CALLER, 2 )
      RETURN
!
      END SUBROUTINE
!
      SUBROUTINE FCDA_READ( NREC, TIME, LOC_ID, CONC, NREAL, IFOD, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine reads a single data record from an open FCDA file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Mar 2003 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NREC  ! Record number
      INTEGER :: TIME              ! Calendar year for concentration data
      CHARACTER(LEN=*) :: LOC_ID   ! Location ID
      REAL, DIMENSION(*) :: CONC   ! Concentration vector of length NREAL
      INTEGER, INTENT(IN) :: NREAL ! Number of realizations to read
      INTEGER, INTENT(IN) :: IFOD  ! Fortran unit number to use for input
      INTEGER :: IERR              ! Error number indicator
!
! *** Local variables
!
      CHARACTER(LEN=9) :: CALLER = 'FCDA_READ'
      INTEGER :: IOS  ! System error number
      INTEGER :: IREL ! Realization looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
      IF( NREAL .LE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Number of realizations must be greater than zero'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      READ( IFOD, IOSTAT=IOS, REC=NREC ) TIME, LOC_ID, (CONC(IREL),IREL=1,NREAL)
!      READ( IFOD, REC=NREC ) TIME, LOC_ID, (CONC(IREL),IREL=1,NREAL)
      IF( IOS .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error reading from the FCDA concentration file'
        MESSAG(2) = ' '
        WRITE(MESSAG(2),'(A,I3,A,I5)') 'UNIT = ',IFOD, ' IOSTAT = ',IOS
        CALL PRTERR(IERR, CALLER, 2)
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE FCDA_RECNO_AQ( ITIM, ILOC, ISPC, NREC, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine returns a record number in a FCDA binary data file
!!    given the indices for time, location for aquatic species.  All of
!!    the indices are defined relative to the data in the ESD keyword
!!    file.  This subroutine is valid only for aquatic species and
!!    locations.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Mar 2003 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
      USE FCDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER :: ITIM ! Time index
      INTEGER :: ILOC ! Location index
      INTEGER :: ISPC ! Species index
      INTEGER :: NREC             ! Record number (output)
      INTEGER :: IERR             ! Error number indicator (output)
!
! *** Local variables
!
      CHARACTER(LEN=13) :: CALLER = 'FCDA_RECNO_AQ'
      INTEGER :: NTIM ! Local time block counter
      INTEGER :: JTIM ! Time looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check the time index
!
      IF( ITIM.LT.1 .OR. ITIM.GT.FCDA_NTIMES ) THEN
        IERR = 1
        MESSAG(1) = 'Time index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ITIM
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) FCDA_NTIMES
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** The time must be computed in ECEM
!
      IF( .NOT. FCDA_TIM(ITIM)%COMP ) THEN
        NREC = -1
        RETURN
      END IF
!
! *** Check the location index
!
      IF( ILOC.LT.1 .OR. ILOC.GT.FCDA_NLOCS ) THEN
        IERR = 2
        MESSAG(1) = 'Location index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ILOC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) FCDA_NLOCS
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** The location must be computed in ECEM
!
      IF( .NOT. FCDA_LOC(ILOC)%COMP ) THEN
        NREC = -1
        RETURN
      END IF
!
! *** Check the species index
!
      IF( ISPC.LT.1 .OR. ISPC.GT. FCDA_NSPC) THEN
        IERR = 3
        MESSAG(1) = 'Species index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ISPC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) FCDA_NSPC
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** The species must be computed and output as a food in ECEM
!
      IF( .NOT. FCDA_SPC(ISPC)%COMP ) THEN
        NREC = -1
        RETURN
      END IF
!
! *** Compute the data index
!
      NREC = FCDA_MAP_AQ(ILOC)
      IF( NREC .LT. 0 ) RETURN
!
! *** Adjust the counter for skipped times in ECEM
      NTIM = 0
      DO JTIM = 1, (ITIM-1)
        IF( FCDA_TIM(JTIM)%COMP ) NTIM = NTIM + 1
      END DO
      NREC = NTIM*FCDA_BLOCK_AQ + NREC
!      NREC = (ITIM-1)*FCDA_BLOCK_AQ + NREC
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE FCDA_RECNO_RP( ITIM, ILOC, ISPC, NREC, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine returns a record number in a FCDA binary data file
!!    given the indices for time, location for riparian species.
!!    All of the indices are defined relative to the data in the ESD
!!    keyword file. This subroutine is valid only for riparian species and
!!    locations.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Mar 2003 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
      USE FCDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER, INTENT(IN) :: ISPC ! Species index
      INTEGER :: NREC             ! Record number (output)
      INTEGER :: IERR             ! Error number indicator (output)
!
! *** Local variables
!
      CHARACTER(LEN=13) :: CALLER = 'FCDA_RECNO_RP'
      INTEGER :: NTIM ! Local time block counter
      INTEGER :: JTIM ! Time looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check the time index
!
      IF( ITIM.LT.1 .OR. ITIM.GT.FCDA_NTIMES ) THEN
        IERR = 1
        MESSAG(1) = 'Time index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ITIM
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) FCDA_NTIMES
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** The time must be computed in ECEM
!
      IF( .NOT. FCDA_TIM(ITIM)%COMP ) THEN
        NREC = -1
        RETURN
      END IF
!
! *** Check the location index
!
      IF( ILOC.LT.1 .OR. ILOC.GT.FCDA_NLOCS ) THEN
        IERR = 2
        MESSAG(1) = 'Location index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ILOC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) FCDA_NLOCS
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** The location must be computed in ECEM
!
      IF( .NOT. FCDA_LOC(ILOC)%COMP ) THEN
        NREC = -1
        RETURN
      END IF
!
! *** Check the species index
!
      IF( ISPC.LT.1 .OR. ISPC.GT.FCDA_NSPC ) THEN
        IERR = 3
        MESSAG(1) = 'Species index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ISPC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) FCDA_NSPC
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** The species must be computed and output as a food in ECEM
!
      IF( .NOT. FCDA_SPC(ISPC)%COMP ) THEN
        NREC = -1
        RETURN
      END IF
!
! *** Compute the data index
!
      NREC = FCDA_MAP_RP(ILOC)
      IF( NREC .LT. 0 ) RETURN
!
! *** Adjust the counter for skipped times in ECEM
      NTIM = 0
      DO JTIM = 1, (ITIM-1)
        IF( FCDA_TIM(JTIM)%COMP ) NTIM = NTIM + 1
      END DO
      NREC = NTIM*FCDA_BLOCK_RP + NREC
!      NREC = (ITIM-1)*FCDA_BLOCK_RP + NREC
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE FCDA_RECNO_UP( ITIM, ILOC, ISPC, ISOI, NREC, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This routine returns a record number in a FCDA binary data file
!!    given the indices for time, location, and soil type for upland
!!    species.  All of the indices are defined relative to the data in
!!    the ESD keyword file.  This routine is valid only for upland
!!    species and locations.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 Mar 2003 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
!
      USE Errors_Mod
      USE FCDA_Mod
!
      IMPLICIT NONE
!
! *** Call list variables
!
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER, INTENT(IN) :: ISOI ! Soil type index
      INTEGER, INTENT(IN) :: ISPC ! Species index
      INTEGER :: NREC             ! Record number (output)
      INTEGER :: IERR             ! Error number indicator (output)
!
! *** Local variables
!
      CHARACTER(LEN=13) :: CALLER = 'FCDA_RECNO_UP'
      INTEGER :: NTIM ! Local time block counter
      INTEGER :: JTIM ! Time looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check the time index
!
      IF( ITIM.LT.1 .OR. ITIM.GT.FCDA_NTIMES ) THEN
        IERR = 1
        MESSAG(1) = 'Time index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ITIM
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) FCDA_NTIMES
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** The time must be computed in ECEM
!
      IF( .NOT. FCDA_TIM(ITIM)%COMP ) THEN
        NREC = -1
        RETURN
      END IF
!
! *** Check the location index
!
      IF( ILOC.LT.1 .OR. ILOC.GT.FCDA_NLOCS ) THEN
        IERR = 2
        MESSAG(1) = 'Location index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ILOC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) FCDA_NLOCS
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** The location must be computed in ECEM
!
      IF( .NOT. FCDA_LOC(ILOC)%COMP ) THEN
        NREC = -1
        RETURN
      END IF
!
! *** Check the species index
!
      IF( ISPC.LT.1 .OR. ISPC.GT.FCDA_NSPC ) THEN
        IERR = 3
        MESSAG(1) = 'Species index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ISPC
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) FCDA_NSPC
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** The species must be computed and output as a food in ECEM
!
      IF( .NOT. FCDA_SPC(ISPC)%COMP ) THEN
        NREC = -1
        RETURN
      END IF
!
! *** Check the soil type index
!
      IF( ISOI.LT.1 .OR. ISOI.GT.ESD_NUM_SOI ) THEN
        IERR = 4
        MESSAG(1) = 'Soil index out of range'
        MESSAG(2) = 'Requested index was '
        WRITE(MESSAG(2)(21:),*) ISOI
        MESSAG(3) = 'Valid range is 1 to '
        WRITE(MESSAG(3)(21:),*) ESD_NUM_SOI
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Compute the data index
!
      NREC = FCDA_MAP_UP(ILOC,ISOI)
      IF( NREC .LT. 0 ) RETURN
!
! *** Adjust the counter for skipped times in ECEM
      NTIM = 0
      DO JTIM = 1, (ITIM-1)
        IF( FCDA_TIM(JTIM)%COMP ) NTIM = NTIM + 1
      END DO
      NREC = NTIM*FCDA_BLOCK_UP + NREC
!      NREC = (ITIM-1)*FCDA_BLOCK_UP + NREC
!
      RETURN
      END SUBROUTINE
