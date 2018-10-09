!----------------------------------------------------
!   Copyright, Battelle Memorial Institute, 2007.
!----------------------------------------------------
!
MODULE Param_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    Define constants used in the exposure equations
!
!  History:
!
!    Paul W. Eslinger : 30 Mar 2000 : Version 1.0
!    Paul W. Eslinger :  7 Feb 2002 : Remove CF5 (unused in code)
!    Paul W. Eslinger : 28 Feb 2006 : Remove BIG, CF2, CF6 (unused in code)
!    Paul W. Eslinger : 30 Oct 2006 : SCR-1148 - Move averaging time to Control_Mod
!
      REAL, PARAMETER :: CF1 = 1.0E-6  ! Conversion factor: 1.0E-6 kg/mg
      REAL, PARAMETER :: CF3 = 1.0E-3  ! Conversion factor: 1.0E-3 L/cm^3
      REAL, PARAMETER :: CF4 = 24.0    ! Conversion factor: 24 hr/day
      REAL, PARAMETER :: CF7 = 0.001   ! Conversion factor: 0.001 milligram/microgram
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Concen_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains concentrations in abiotic media and in food products
!
!  History:
!
!    Paul W. Eslinger : 30 Mar 2000 : Version 1.0
!    Paul W. Eslinger :  2 Feb 2003 : Upgrade for SAC Rev. 1 food sources
!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!    Paul W. Eslinger : 15 Feb 2006 : Add sweat lodge pathway
!
      REAL, ALLOCATABLE :: GWAT(:,:) ! Concentration by analyte and realization in ground water
      REAL, ALLOCATABLE :: SWAT(:,:) ! Concentration by analyte and realization in surface water
      REAL, ALLOCATABLE :: SEDI(:,:) ! Concentration by analyte and realization in sediment
      REAL, ALLOCATABLE :: SEEP(:,:) ! Concentration by analyte and realization in seep water
      REAL, ALLOCATABLE :: SORP(:,:) ! Concentration by analyte and realization in riparian zone soil
      REAL, ALLOCATABLE :: SODR(:,:) ! Concentration by analyte and realization in non-irrigated upland soil
      REAL, ALLOCATABLE :: SOGW(:,:) ! Concentration by analyte and realization in groundwater irrigated upland soil
      REAL, ALLOCATABLE :: SOSW(:,:) ! Concentration by analyte and realization in surface water irrigated upland soil
      REAL, ALLOCATABLE :: AIRC(:,:) ! Concentration by analyte and realization in air
!
      REAL, ALLOCATABLE :: CFISH(:,:)    ! Concentration by analyte and realization in fish
      REAL, ALLOCATABLE :: CFISH_2(:,:)  ! Concentration by analyte and realization in fish_2
      REAL, ALLOCATABLE :: CFISH_3(:,:)  ! Concentration by analyte and realization in fish_3
      REAL, ALLOCATABLE :: CLEAFVEG(:,:) ! Concentration by analyte and realization in leaf vegetables
      REAL, ALLOCATABLE :: CROOTVEG(:,:) ! Concentration by analyte and realization in root vegetables
      REAL, ALLOCATABLE :: CMEAT(:,:)    ! Concentration by analyte and realization in meat
      REAL, ALLOCATABLE :: CBIRD(:,:)    ! Concentration by analyte and realization in birds
      REAL, ALLOCATABLE :: CMILK(:,:)    ! Concentration by analyte and realization in milk
      REAL, ALLOCATABLE :: CFRUIT(:,:)   ! Concentration by analyte and realization in fruit
      REAL, ALLOCATABLE :: CGRAIN(:,:)   ! Concentration by analyte and realization in grain
      REAL, ALLOCATABLE :: CEGGS(:,:)    ! Concentration by analyte and realization in eggs
      REAL, ALLOCATABLE :: CSOIL(:,:)    ! Concentration by analyte and realization in soil
      REAL, ALLOCATABLE :: CSWEAT(:,:)   ! Concentration by analyte and realization for water in the sweat lodge
      REAL, ALLOCATABLE :: CSHOWER(:,:)  ! Concentration by analyte and realization water in shower water
!
      REAL, ALLOCATABLE :: CVEC(:) ! Concentration work vector
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Control_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains control and intermediate results information
!
!  History:
!
!    Paul W. Eslinger : 26 Jan 2001 : Version 1.0
!    Paul W. Eslinger :  4 Sep 2002 : Add DET_CONC
!    Carmen Arimescu  :  2 Feb 2003 : SAC Rev. 1
!    Paul W. Eslinger : 23 Oct 2003 : Add the FOODSOURCE option
!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!    Paul W. Eslinger : 28 Feb 2006 : Allow all analyte types in a given run, add output labels
!    Paul W. Eslinger : 30 Oct 2006 : SCR-1148 - Change averaging time
!
      LOGICAL :: EXECUT    ! .FALSE. = Perform error checking only,
                           ! .TRUE.  = Run the entire code
      LOGICAL :: REPORT    ! Flag whether the report file is open
!
      CHARACTER(LEN=72) :: PTITLE ! Title for this program run
!
      CHARACTER(LEN=10) :: SDATE ! Start date of processing
      CHARACTER(LEN=10) :: STIME ! Start time of processing
      CHARACTER(LEN=10) :: EDATE ! End date of processing
      CHARACTER(LEN=10) :: ETIME ! End time of processing
!
      LOGICAL :: USE_LEAFVEG ! Flag indicating nonzero ingestion of leaf vegetables
      LOGICAL :: USE_ROOTVEG ! Flag indicating nonzero ingestion of root vegetables
      LOGICAL :: USE_GRAIN   ! Flag indicating nonzero ingestion of grains
      LOGICAL :: USE_FRUIT   ! Flag indicating nonzero ingestion of fruits
      LOGICAL :: USE_BIRD    ! Flag indicating nonzero ingestion of birds
      LOGICAL :: USE_MEAT    ! Flag indicating nonzero ingestion of meat
      LOGICAL :: USE_MILK    ! Flag indicating nonzero ingestion of milk
      LOGICAL :: USE_FISH    ! Flag indicating nonzero ingestion of fish
      LOGICAL :: USE_FISH_2  ! Flag indicating nonzero ingestion of fish_2
      LOGICAL :: USE_FISH_3  ! Flag indicating nonzero ingestion of fish_3
      LOGICAL :: USE_EGGS    ! Flag indicating nonzero ingestion of eggs
      LOGICAL :: USE_SOIL    ! Flag indicating nonzero ingestion of soil
      LOGICAL :: USE_SEDI    ! Flag indicating nonzero ingestion of sediment
!
      CHARACTER(LEN=80) :: LABEL_SURFACE        ! Food label for  ingestion of surface water
      CHARACTER(LEN=80) :: LABEL_GROUND         ! Food label for  ingestion of ground water
      CHARACTER(LEN=80) :: LABEL_SEEP           ! Food label for  ingestion of seep water
      CHARACTER(LEN=80) :: LABEL_SOIL_CHILD     ! Food label for ingestion of soil
      CHARACTER(LEN=80) :: LABEL_SOIL_ADULT     ! Food label for ingestion of soil
      CHARACTER(LEN=80) :: LABEL_SEDIMENT_CHILD ! Food label for ingestion of sediment
      CHARACTER(LEN=80) :: LABEL_SEDIMENT_ADULT ! Food label for ingestion of sediment
!
      CHARACTER(LEN=80) :: LABEL_LEAFVEG ! Food label for ingestion of leaf vegetables
      CHARACTER(LEN=80) :: LABEL_ROOTVEG ! Food label for ingestion of root vegetables
      CHARACTER(LEN=80) :: LABEL_GRAIN   ! Food label for ingestion of grains
      CHARACTER(LEN=80) :: LABEL_FRUIT   ! Food label for ingestion of fruits
      CHARACTER(LEN=80) :: LABEL_BIRD    ! Food label for ingestion of birds
      CHARACTER(LEN=80) :: LABEL_MEAT    ! Food label for ingestion of meat
      CHARACTER(LEN=80) :: LABEL_MILK    ! Food label for ingestion of milk
      CHARACTER(LEN=80) :: LABEL_FISH    ! Food label for ingestion of fish
      CHARACTER(LEN=80) :: LABEL_FISH_2  ! Food label for ingestion of fish_2
      CHARACTER(LEN=80) :: LABEL_FISH_3  ! Food label for ingestion of fish_3
      CHARACTER(LEN=80) :: LABEL_EGGS    ! Food label for ingestion of eggs
!
      INTEGER :: LEAFVEG_FIDX ! ESD Species index for food type: leaf vegetables
      INTEGER :: ROOTVEG_FIDX ! ESD Species index for food type: root vegetables
      INTEGER :: GRAIN_FIDX   ! ESD Species index for food type: grains
      INTEGER :: FRUIT_FIDX   ! ESD Species index for food type: fruit
      INTEGER :: BIRD_FIDX    ! ESD Species index for food type: birds
      INTEGER :: MEAT_FIDX    ! ESD Species index for food type: meat
      INTEGER :: MILK_FIDX    ! ESD Species index for food type: milk
      INTEGER :: FISH_FIDX    ! ESD Species index for food type: fish
      INTEGER :: FISH_FIDX_2  ! ESD Species index for food type: fish_2
      INTEGER :: FISH_FIDX_3  ! ESD Species index for food type: fish_3
      INTEGER :: EGGS_FIDX    ! ESD Species index for food type: eggs
!
      INTEGER :: NUMCAR ! Number of carcinogenic analytes
      INTEGER :: NUMHAZ ! Number of hazardous analytes
      INTEGER :: NUMRAD ! Number of radioactive analytes
!
      INTEGER :: HUM_NUM_ANA ! Number of analytes in this scenario
      INTEGER :: HUM_NUM_LOC ! Number of locations in this scenario
      INTEGER :: HUM_NUM_TIM ! Number of times in this scenario
!
      INTEGER :: NREAL       ! Number of realizations
!
      LOGICAL :: STA_USE      ! True if any result statistics are output
      LOGICAL :: STA_CONC     ! Flag controlling output of media concentration statistics
      LOGICAL :: STA_FOOD     ! Flag controlling output of food concentration statistics
      LOGICAL :: STA_SUMDOSE  ! Flag controlling output of dose statistics summed over analytes
      LOGICAL :: STA_SUMRISK  ! Flag controlling output of dose statistics summed over analytes
      LOGICAL :: STA_POPDOSE  ! Flag controlling output of population dose statistics
      LOGICAL :: STA_POPRISK  ! Flag controlling output of population risk statistics
      LOGICAL :: STA_ANARISK  ! Flag controlling output of dose statistics at the analyte level
      LOGICAL :: STA_ANADOSE  ! Flag controlling output of risk statistics at the analyte level
      LOGICAL :: STA_PATHDOSE ! Flag controlling output of dose statistics at the exposure pathway level
      LOGICAL :: STA_PATHRISK ! Flag controlling output of risk statistics at the exposure pathway level
!
      LOGICAL :: DET_USE      ! True if any detailed risk values are output
      LOGICAL :: DET_CONC     ! Flag controlling output of media concentration detailed data
      LOGICAL :: DET_FOOD     ! Flag controlling output of food concentration detailed data
      LOGICAL :: DET_SUMDOSE  ! Flag controlling output of detailed doses summed over analytes
      LOGICAL :: DET_SUMRISK  ! Flag controlling output of detailed risks (or hazard quotients) summed over analytes
      LOGICAL :: DET_POPDOSE  ! Flag controlling output of population dose detailed data
      LOGICAL :: DET_POPRISK  ! Flag controlling output of population risk detailed data
      LOGICAL :: DET_ANADOSE  ! Flag controlling output of detailed dose data at the analyte level
      LOGICAL :: DET_ANARISK  ! Flag controlling output of detailed risk data at the analyte level
      LOGICAL :: DET_PATHDOSE ! Flag controlling output of detailed dose data at the pathway level
      LOGICAL :: DET_PATHRISK ! Flag controlling output of detailed risk data at the pathway level
!
!     Shortcut data to replace (and override) all entries on LOCATION keywords
      LOGICAL :: SOURCE_BIRD   ! All BIRD food comes from the same location
      LOGICAL :: SOURCE_FISH   ! All FISH food comes from the same location
      LOGICAL :: SOURCE_FISH_2 ! All FISH_2 food comes from the same location
      LOGICAL :: SOURCE_FISH_3 ! All FISH_3 food comes from the same location
      LOGICAL :: SOURCE_MEAT   ! All MEAT food comes from the same location
      CHARACTER(LEN=6) :: SOURCE_BIRD_ID   ! Location ID for all BIRD food
      CHARACTER(LEN=6) :: SOURCE_FISH_ID   ! Location ID for all FISH food
      CHARACTER(LEN=6) :: SOURCE_FISH_ID_2 ! Location ID for all FISH food
      CHARACTER(LEN=6) :: SOURCE_FISH_ID_3 ! Location ID for all FISH food
      CHARACTER(LEN=6) :: SOURCE_MEAT_ID   ! Location ID for all MEAT food
!
      CHARACTER(LEN=4) :: SWEAT_SOURCE  ! Source for water for sweat lodge
      CHARACTER(LEN=4) :: SHOWER_SOURCE ! Source for water for showering
!
      CHARACTER(LEN=6) :: FOOD_UNIT ! String for units for foods
      CHARACTER(LEN=6) :: MILK_UNIT ! String for units for milk
!
      REAL :: AVGTIME_CAR  ! Averaging time for carcinogens (days using 365.25 days/yr)
      REAL :: AVGTIME_HAZ  ! Averaging time for hazardous (days using 365.25 days/yr)
!
      REAL, ALLOCATABLE :: CONVERT_UNITS(:,:) ! Units conversion (multiplier)
!                                               Dimensions are analytes & media
      CHARACTER(LEN=12), ALLOCATABLE :: CONVERT_LABELS(:,:,:) ! Units conversion (multiplier)
!                                               Dimensions are analytes, media, stage
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Debug_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to initiating debug
!    outputs to the report file for a variety of calculations.
!
!  History:
!
!    Paul W. Eslinger : 10 Apr 2000 : Version 1.0
!
      LOGICAL :: BGCONC ! Debug flag for concentration data
      LOGICAL :: BGFOOD ! Debug flag for food concentration data
      LOGICAL :: BGRAD  ! Debug flag for radiological risk computations
      LOGICAL :: BGHAZ  ! Debug flag for hazardous risk computations
      LOGICAL :: BGCAR  ! Debug flag for carcinogenic risk computations
      LOGICAL :: BGVERB ! Debug flag for more verbose outputs
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

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
!
      INTEGER, PARAMETER :: ESD_NUM_SOI = 3 ! Number of upland soil types allowed
      INTEGER :: SOIL_IDX                   ! Soil dependent index for write statements
      CHARACTER(LEN=4) :: SOIL_ID           ! Soil type for write statements
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
        INTEGER :: THIRD          ! Index for the third choice of concentrations
        INTEGER :: MEAT           ! Location index for meat
        INTEGER :: FISH           ! Location index for fish
        INTEGER :: FISH_2         ! Location index for fish_2
        INTEGER :: FISH_3         ! Location index for fish_3
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
        LOGICAL :: ANATYP_CAR     ! Flag for carcinogenic analyte for analysis
        LOGICAL :: ANATYP_HAZ     ! Flag for hazardous analyte for analysis
        LOGICAL :: ANATYP_RAD     ! Flag for radioactive analyte for analysis
        LOGICAL :: COMP           ! Flag whether this analyte is to be used
        LOGICAL :: OUTPUT         ! Flag whether results are output for this analyte
        LOGICAL :: VERBOSE        ! Flag whether verbose results are output for this analyte
        LOGICAL :: TERSE          ! Flag whether terse results are output for this analyte
        LOGICAL :: RFDING         ! Flag whether ingestion doses are calculated for hazardous analytes
        LOGICAL :: RFDINH         ! Flag whether inhalation doses are calculated for hazardous analytes
        LOGICAL :: SFING          ! Flag whether ingestion doses are calculated for carcinogenic analytes
        LOGICAL :: SFINH          ! Flag whether inhalation doses are calculated for carcinogenic analytes
      END TYPE ESD_ANA_TYPE
      TYPE (ESD_ANA_TYPE), ALLOCATABLE :: ESD_ANA(:) ! The ESD analyte variable
!
!     Type definition for ESD species data
!
      TYPE ESD_SPC_TYPE
        CHARACTER(LEN= 6) :: ID      ! Species ID
        CHARACTER(LEN=48) :: NAME    ! Species name
        CHARACTER(LEN= 8) :: HABITAT ! Species type (aquatic, riparian, or upland)
      END TYPE ESD_SPC_TYPE
      TYPE(ESD_SPC_TYPE), ALLOCATABLE :: ESD_SPC(:) ! Variable structure for species information
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE ESD_Mod

MODULE Files_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This module contains file-related information.
!
!  History:
!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!    Carmen Arimescu  : 20 Feb 2003 : SAC Rev 1.0
!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0004) Remove SACVIEW header files
!
      INTEGER, PARAMETER :: MAXFN=200 ! Set equal to LENQQQ in RDBLKD_Mod
!
      INTEGER :: IESD ! Unit number for the ESD keyword file
      CHARACTER(LEN=MAXFN) :: FNESD ! Name of the input ESD keyword file
!
      INTEGER :: IKEY ! Unit number for the HUMAN keyword file
      CHARACTER(LEN=MAXFN) :: FNKEY ! Name of the input HUMAN keyword file
!
      INTEGER :: IRPT ! Unit number for the output report file
      CHARACTER(LEN=MAXFN) :: FNRPT ! Name of the output output report file
!
      INTEGER :: IDET ! Unit number for the output detailed risks file
      CHARACTER(LEN=MAXFN) :: FNDET ! Name of the output detailed risks file
!
      INTEGER :: ISUM ! Unit number for the output summary stastistics file
      CHARACTER(LEN=MAXFN) :: FNSUM ! Name of the output summary statistics file
!
      INTEGER :: IMAP ! Unit number for the concentration record number map file
      CHARACTER(LEN=MAXFN) :: FNMAP ! Name of the concentration record number map file
!
      INTEGER, ALLOCATABLE :: ICON(:) ! Unit number for the concentration file
      CHARACTER(LEN=MAXFN), ALLOCATABLE :: FNCON(:) ! Name of the input concentration file
!
      INTEGER :: IVLU ! Unit number for the output generated statistical values
      CHARACTER(LEN=MAXFN) :: FNVAL ! Name of the output generated statistical values
!
      INTEGER, ALLOCATABLE :: IFOD(:,:) ! Unit number for the food concentration files
      CHARACTER(LEN=MAXFN), ALLOCATABLE :: FNFOD(:,:) ! Names of food concentration files
!
      CHARACTER(LEN=MAXFN) :: FOODPATH ! Path for location of food files
      CHARACTER(LEN=MAXFN) :: FOODMAP  ! Name of the food map file
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

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
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Results_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information on the computed dose, risk, or hazard quotient
!
!  History:
!
!    Paul W. Eslinger : 30 Mar 2000 : Version 1.0
!    Paul W. Eslinger : 28 Feb 2006 : Change population dose calcs
!                                     Change to calculating both risk and dose outputs
!
      REAL, ALLOCATABLE :: DOSERAD(:) ! Radioactive dose results for a single location (summed over analytes)
      REAL, ALLOCATABLE :: RISKRAD(:) ! Radioactive risk results for a single location (summed over analytes)
!
      REAL, ALLOCATABLE :: DOSECAR(:) ! Carcinogenic dose results for a single location (summed over analytes)
      REAL, ALLOCATABLE :: RISKCAR(:) ! Carcinogenic risk results for a single location (summed over analytes)
!
      REAL, ALLOCATABLE :: RISKHAZ(:) ! Hazardous risk results for a single location (summed over analytes)
      REAL, ALLOCATABLE :: DOSEHAZ(:) ! Hazardous dose results for a single location (summed over analytes)
!
      REAL, ALLOCATABLE :: DOSEINC(:) ! Incremental dose for an analyte and location
      REAL, ALLOCATABLE :: RISKINC(:) ! Incremental risk (or hazard quotient) for an analyte and location
!
      REAL, ALLOCATABLE :: POPDOSE(:) ! Population dose over all locations and nuclides
      REAL, ALLOCATABLE :: POPRISK(:) ! Population risk over all locations and nuclides
!
      REAL, ALLOCATABLE :: ING_INC_DOS(:) ! Incremental dose for an analyte and location from ingestion
      REAL, ALLOCATABLE :: INH_INC_DOS(:) ! Incremental dose for an analyte and location from inhalation
      REAL, ALLOCATABLE :: EXT_INC_DOS(:) ! Incremental dose for an analyte and location from external exposure
!
      REAL, ALLOCATABLE :: ING_INC_RSK(:) ! Incremental risk for an analyte and location from ingestion
      REAL, ALLOCATABLE :: INH_INC_RSK(:) ! Incremental risk for an analyte and location from inhalation
      REAL, ALLOCATABLE :: EXT_INC_RSK(:) ! Incremental risk for an analyte and location from external exposure
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Seeds_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!   History:
!
!     Paul W. Eslinger : 16 Mar 2000 : Version 1.0
!
      REAL(KIND=8) :: SDSTOC ! Random seed for all stochastic variables
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Stocvars_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains definitions for variables that are stochastic
!    within the code
!
!   History:
!
!     Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!     Paul W. Eslinger : 22 Oct 2003 : Remove unused variables
!     Paul W. Eslinger : 27 Feb 2006 : Add showering pathway
!                                      And radionculide risk factors
!
!     Variables changing only by realization
      REAL, ALLOCATABLE :: AFSED(:)       ! Soil adherence factor for sediment
      REAL, ALLOCATABLE :: AFSOIL(:)      ! Soil adherence factor for soil
      REAL, ALLOCATABLE :: CFSWEAT(:)     ! Concentration factor for sweat lodge (water in air)
      REAL, ALLOCATABLE :: CFSHOWER(:)    ! Concentration factor for showering (water in air)
      REAL, ALLOCATABLE :: ML(:)          ! Mass loading of soil in air
      REAL, ALLOCATABLE :: IRGRATE(:)     ! Irrigation rate
      REAL, ALLOCATABLE :: SHIELDSOIL(:)  ! Soil shielding factor
      REAL, ALLOCATABLE :: SHIELDSED(:)   ! Sediment shielding factor
      REAL, ALLOCATABLE :: WORK(:)        ! Work vector of length number of realizations
!
!     Type definition for body data changing only by realization
!
      TYPE BODY_TYPE
        REAL :: BWADULT  ! Body weight for an adult
        REAL :: BWCHILD  ! Body weight for a child
        REAL :: SASOIL   ! Body surface area exposed to soil
        REAL :: SASED    ! Body surface area exposed to sediment
        REAL :: SASEEP   ! Body surface area exposed to seeps
        REAL :: SASWIM   ! Body surface area exposed to surface water (swim)
        REAL :: SASWEAT  ! Body surface area exposed to sweat lodge water
        REAL :: SASHOWER ! Body surface area exposed to shower water
      END TYPE BODY_TYPE
      TYPE (BODY_TYPE), ALLOCATABLE :: BODY(:) ! The body variables
!
!     Type definition for exposure data changing only by realization
!
      TYPE EXPOSURE_TYPE
        REAL :: ED         ! Exposure duration
        REAL :: EDCHILD    ! Exposure duration for a child
        REAL :: EDADULT    ! Exposure duration for an adult
        REAL :: EFBOAT     ! Exposure frequency for boating
        REAL :: EFSURFACE  ! Exposure frequency for surface water activities
        REAL :: EFAIR      ! Exposure frequency for air
        REAL :: EFGROUND   ! Exposure frequency for ground water activities
        REAL :: EFSED      ! Exposure frequency for sediment
        REAL :: EFSEEP     ! Exposure frequency for seeps
        REAL :: EFSOIL     ! Exposure frequency for soil
        REAL :: EFSWIM     ! Exposure frequency for swimming
        REAL :: EFSWEAT    ! Exposure frequency for sweat lodge items
        REAL :: EFSHOWER   ! Exposure frequency for showering
        REAL :: EFLEAFVEG  ! Exposure frequency for eating leaf vegetables
        REAL :: EFROOTVEG  ! Exposure frequency for eating root vegetables
        REAL :: EFMEAT     ! Exposure frequency for eating meat
        REAL :: EFMILK     ! Exposure frequency for eating milk
        REAL :: EFBIRD     ! Exposure frequency for eating birds
        REAL :: EFFISH     ! Exposure frequency for eating fish
        REAL :: EFFISH_2   ! Exposure frequency for eating fish_2
        REAL :: EFFISH_3   ! Exposure frequency for eating fish_3
        REAL :: EFFRUIT    ! Exposure frequency for eating fruit
        REAL :: EFGRAIN    ! Exposure frequency for eating grain
        REAL :: EFEGGS     ! Exposure frequency for eating eggs
        REAL :: ETBOAT     ! Exposure time for boating
        REAL :: ETSURFACE  ! Exposure time for surface water activities
        REAL :: ETAIR      ! Exposure time for air
        REAL :: ETSED      ! Exposure time for sediment
        REAL :: ETSEEP     ! Exposure time for seep water
        REAL :: ETSOIL     ! Exposure time for soil
        REAL :: ETSWIM     ! Exposure time for swimming
        REAL :: ETSWEAT    ! Exposure time for sweat lodge
        REAL :: ETSHOWER   ! Exposure time for showering
      END TYPE EXPOSURE_TYPE
      TYPE (EXPOSURE_TYPE), ALLOCATABLE :: EXPOS(:) ! The exposure variables
!
!     Type definition for ingestion and inhalation data changing only by realization
!
      TYPE INGEST_TYPE
        REAL :: IRSOILADULT   ! Ingestion rate of soil for an adult
        REAL :: IRSOILCHILD   ! Ingestion rate of soil for a child
        REAL :: IRSEDADULT    ! Ingestion rate of sediment for an adult
        REAL :: IRSEDCHILD    ! Ingestion rate of sediment for a child
        REAL :: IRSURFACE     ! Ingestion rate of surface water
        REAL :: IRGROUND      ! Ingestion rate of ground water
        REAL :: IRSEEP        ! Ingestion rate of seep water
        REAL :: IRFISH        ! Ingestion rate of fish
        REAL :: IRFISH_2      ! Ingestion rate of fish_2
        REAL :: IRFISH_3      ! Ingestion rate of fish_3
        REAL :: IRLEAFVEG     ! Ingestion rate of leafy vegetables
        REAL :: IRROOTVEG     ! Ingestion rate of root vegetables
        REAL :: IRMEAT        ! Ingestion rate of meat
        REAL :: IRMILK        ! Ingestion rate of milk
        REAL :: IRBIRD        ! Ingestion rate of birds
        REAL :: IRFRUIT       ! Ingestion rate of fruit
        REAL :: IRGRAIN       ! Ingestion rate of grain
        REAL :: IREGGS        ! Ingestion rate of eggs
        REAL :: IRATE         ! Inhalation rate
      END TYPE INGEST_TYPE
      TYPE (INGEST_TYPE), ALLOCATABLE :: INGEST(:) ! The ingestion variables
!
!     Variables changing by realization and analyte
!
      REAL, ALLOCATABLE :: DFING(:,:)   ! Dose factor for ingestion      (rem/pCi)
      REAL, ALLOCATABLE :: DFINH(:,:)   ! Dose factor for inhalation     (rem/pCi)
      REAL, ALLOCATABLE :: DFSOIL(:,:)  ! Dose factor for soil exposure  (rem/hr per pCi/kg)
      REAL, ALLOCATABLE :: DFSWIM(:,:)  ! Dose factor for swimming       (rem/hr per pCi/L)
      REAL, ALLOCATABLE :: DFBOAT(:,:)  ! Dose factor for boating        (rem/hr per pCi/L)
!
      REAL, ALLOCATABLE :: RFING(:,:)   ! Risk factor for ingestion      (risk/pCi)
      REAL, ALLOCATABLE :: RFINH(:,:)   ! Risk factor for inhalation     (risk/pCi)
      REAL, ALLOCATABLE :: RFSOIL(:,:)  ! Risk factor for soil exposure  (risk/hr per pCi/kg)
      REAL, ALLOCATABLE :: RFSWIM(:,:)  ! Risk factor for swimming       (risk/hr per pCi/L)
      REAL, ALLOCATABLE :: RFBOAT(:,:)  ! Risk factor for boating        (risk/hr per pCi/L)
!
      REAL, ALLOCATABLE :: RFDING(:,:)  ! Hazardous reference dose for ingestion
      REAL, ALLOCATABLE :: RFDINH(:,:)  ! Hazardous reference dose for inhalation
!
      REAL, ALLOCATABLE :: SFING(:,:)   ! Carcinogenic slope factor for ingestion
      REAL, ALLOCATABLE :: SFINH(:,:)   ! Carcinogenic slope factor for inhalation
!
      REAL, ALLOCATABLE :: ABSORP(:,:)  ! Skin absorption factor
      REAL, ALLOCATABLE :: KP(:,:)      ! Skin permeability factor
      REAL, ALLOCATABLE :: VF(:,:)      ! Analyte volatilation factor in air (L/m^3)
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

      PROGRAM HUMAN
!!***********************************************************************************************************
!!
!!                             HUMAN - Stochastic Human Risk Analysis
!!                        Toolkit for Integrated Impacts Assessments (TIIA)
!!
!!              Battelle, Pacific Northwest National Laboratories, Richland, Washington
!!
!!***********************************************************************************************************
!!
!!  HUMAN is the top level routine for the human analysis portion of the TIIA, Vesion 1.  This code performs
!!  a stochastic analysis of the risk to humans from a variety of exposure pathways given a descrption of
!!  contaminated media and food products.
!!
!!  The HUMAN code draws heavily from the version initially developed for the Columbia River Comprehensive
!!  Impact Assessment project.  That version had a development team of Paul W. Eslinger, Terri B. Miley
!!  and Bruce A. Napier.
!!
!! Reference:
!!
!!   P.W. Eslinger and T.B. Miley.  June 2012.  User Instructions for the Computer Codes of the
!!   Toolkit for Integrated Impact Assessments (TIIA), Version 1.  PNWD-4357.  Battelle Memorial
!!   Institute, Columbus, Ohio.
!!
!!***********************************************************************************************************
!!
!!  Module History:
!!
!!    Paul W. Eslinger :  3 Oct 1996 : Version 1.0 : CRCIA Version
!!    Paul W. Eslinger : 21 Apr 2000 : Version 2.0 : SAC Version
!!    Paul W. Eslinger : 26 Sep 2000 : Version 2.00.B : Add animal water intake in food
!!                                     concentration calculations
!!    Paul W. Eslinger : 25 Jun 2001 : Version 2.00.C : Error trap negative concentrations
!!    Paul W. Eslinger : 30 Jul 2001 : Version 2.00.D : Terse KDSOIL
!!    Paul W. Eslinger :  7 Aug 2001 : Version 2.00.D : Upgrade trap on negative concentrations
!!    Paul W. Eslinger : 12 Dec 2001 : Version 2.00.E.0 : Add output units, upgrade header file for SACVIEW
!!    Paul W. Eslinger :  4 Feb 2002 : Version 2.00.E.1 : Rework upgrade on header file for SACVIEW
!!    Paul W. Eslinger :  7 Feb 2002 : Version 2.00.E.2 : Removeunused conversion factor CF5
!!    Paul W. Eslinger :  4 Sep 2002 : Version 2.00.E.8 : Add optional concentration output to the
!!                                     details file and in report file
!!    Paul W. Eslinger : 29 Jan 2003 : SAC Rev. 1 Version
!!    Paul W. Eslinger : 13 Jun 2003 : SCR-1003 - Force TYPE to upper case on ESD ANALYTE keyword
!!    Paul W. Eslinger : 26 Jun 2003 : Allow runs with no food ingestion
!!    Paul W. Eslinger : 14 Oct 2003 : Revise error messages, and remove OTHERMED option on the
!!                                     LOCATION keyword, fix debug writes
!!    Paul W. Eslinger : 23 Oct 2003 : SCR-1041 - Allow remote meat locations add TELLTIME logic to
!!                                     screen outputs, remove vestigal variables, add compact food
!!                                     source logic, upgrade error messages
!!    Paul W. Eslinger :  8 Dec 2003 : SCR-1052 - Change sweat source to 4 characters in length
!!    Paul W. Eslinger : 21 Sep 2004 : Set CSWEAT concentrations
!!    Paul W. Eslinger : 23 Sep 2004 : Add EFFRUIT, EFEGGS and EFGRAIN generation
!!    Paul W. Eslinger : 24 Jan 2005 : Change HQING and HQINH to RFDING and RFDINH
!!    Paul W. Eslinger :  3 Mar 2005 : SCR-1073 Add ETGROUND
!!    Paul W. Eslinger : 22 Mar 2005 : Update comments
!!    Paul W. Eslinger :  1 Apr 2005 : SCR-1077 - Change INGESTIO keyword options, update stochastic
!!                                     routines, shange type references to separate vectors
!!    Paul W. Eslinger : 13 May 2005 : Add third location option
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!    Paul W. Eslinger :  1 Dec 2005 : Fix output labels in FOOD_READ
!!    Paul W. Eslinger :  2 Dec 2005 : Rearrange debug outputs in the three impacts subroutines
!!    Paul W. Eslinger : 28 Feb 2006 : SCR-1107 - Add dermal pathway to sweat lodge
!!                                     Add explicit shower exposure, remove ETGROUND
!!                                     Allow all analyte types in a given run
!!                                     Revise output labels and population dose calculations
!!                                     Calculate radioactive risk from direct inputs (FGR-13) rather
!!                                     than using dose to risk conversions.
!!                                     Make SACVIEW header file an optional file
!!    Paul W. Eslinger : 13 Apr 2006 : SCR-1119 - Fix output flags
!!    Paul W. Eslinger : 18 Sep 2006 : SCR-1143 - Fix food labels in FOOD_READ
!!                                     Check for negative concentrations in FOOD_READ
!!    Paul W. Eslinger : 30 Oct 2006 : SCR-1148 - Fix averaging times for multiple analytes
!!    Paul W. Eslinger :  5 Jun 2007 : Update for TIIA logic and copyright information
!!                                     Upgrade REPORT keyword logic, upgrade comments
!!                                     Change stochastic variable to length 24
!!    Paul W. Eslinger : 28 Jun 2012 : Update comments and fix indexing bug for FISH_3 in FOOD_READ
!!    Paul W. Eslinger :  9 Jul 2012 : Update to standard copyright and QA disclaimers
!!    Paul W. Eslinger : 15 Jul 2012 : Fix header line in summary statistics file
!!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0004) Remove SACVIEW header files
!!
!!***********************************************************************************************************
!!
!!  General Notes:
!!
!!    1. This code is applies one definition of human lifestyle parameters in a single run.
!!       The code applies the same definition to multiple times and locations while handling
!!       multiple contaminants.
!!
!!    2. This code provides stochastic results.  It also provides deterministic results in that
!!       a run of one realization is allowed.
!!
!!    3. This program is written in free format Fortran 95
!!
!!    4. This program is a console-initiated program.  The sole input upon starting a program
!!       run is the name of a simulation control file.
!!
!!    5. Even though HUMAN runs in a stand-alone mode, previous programs in the SAC, Rev. 1,
!!       suite of codes must execute prior to the HUMAN code to generate the library of
!!       environmental media and food concentrations used to calculate the impacts.
!!
!!***********************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Files_Mod
      USE Control_Mod
      USE Debug_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Stats_Mod, ONLY: INDSTO, INDTBL, BG_STOC_VALU
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'HUMAN' ! Name of this routine
!
      INTEGER :: IERR ! Error flag variable
      INTEGER :: ITIM ! Time looping index
      INTEGER :: ILOC ! Location looping index
!
      CHARACTER(LEN=7) :: CYEAR ! Character year
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Program, run identification, and initialization
      CALL IDEN_SET( )
      CALL INIT_1( )
!
! *** Open the HUMAN keyword file
      CALL OPEN_KEY( IERR )
      IF( IERR .NE. 0 ) THEN
        WRITE(*,*) 'Error opening the HUMAN keyword file'
        WRITE(*,*) 'Stop in '//CALLER
        STOP
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
! *** Read the first pass of HUMAN keywords
!     Collect static and array dimension information
      CALL TELLTIME( 'Reading HUMAN keywords - Pass #1', 'SCREEN', .TRUE., IRPT )
      CALL KEY_HUMAN_1( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
!     Print the opening banner page
      CALL BANNER_1(  )
!
! *** Check the identification keywords
      CALL CHECK_IDEN( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Write a more extensive banner to the report file
      CALL BANNER_2( )
!
! *** Open the environmental settings definition keyword file
      CALL OPEN_ESD( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Read the first pass of environmental settings keywords to
!     collect array dimension information
      CALL TELLTIME( 'Reading ESD keywords   - Pass #1', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_1( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Check the problem size definition ESD keywords
      CALL CHECK_ESD( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Set up memory for stochastic variables and initialize
!     other stochastic information
      IF( BGVERB ) CALL TELLTIME( 'Allocating Stochastic Memory', 'SCREEN', .FALSE., IRPT )
      CALL STOCH_MEMORY( INDSTO, INDTBL, NREAL, IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
      CALL INIT_STOCH( )
!
! *** Set up the dynamic storage for the problem size identified
!     in the environmental keyword file
      IF( BGVERB ) CALL TELLTIME( 'Allocating ESD Memory', 'SCREEN', .FALSE., IRPT )
      CALL ESD_MEMORY( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Initialize environmental settings data and the "use" information
      CALL ESD_INIT(  )
!
! *** Read the second pass of environmental keywords
!     Save time, analyte, location information for human
!     Save concentration file names
      CALL TELLTIME( 'Reading ESD keywords   - Pass #2', 'SCREEN', .FALSE., IRPT )
      CALL KEY_ESD_2( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Set up the dynamic memory for all other arrays
      IF( BGVERB ) CALL TELLTIME( 'Allocating HUMAN Memory', 'SCREEN', .FALSE., IRPT )
      CALL HUM_MEMORY( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Read the rest of the HUMAN keywords to define the problem
      CALL TELLTIME( 'Reading HUMAN keywords - Pass #2', 'SCREEN', .FALSE., IRPT )
      CALL KEY_HUMAN_2( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Perform error checking on keywords and the problem definition
      CALL CHECK_HUMAN( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Determine which foods are eaten in this scenario
      IF( BGVERB ) CALL TELLTIME( 'Determine which foods are eaten', 'SCREEN', .FALSE., IRPT )
      CALL EAT_FOODS( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Echo most of the problem definition to the report file
      CALL ECHO_1(  )
!
!---- Start execution of the problem ------------------------------------------------------
!
      IF( .NOT. EXECUT ) THEN
        MESSAG(1) = 'Execution not requested (use the EXECUTE card)'
        CALL PRTERR( IERR, CALLER, 1 )
        GO TO 1000
      END IF
!
! *** Elapsed time message
      CALL ELAPSE( 1, IRPT, IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
      IF( BGVERB ) CALL TELLTIME( 'File Handling', 'SCREEN', .FALSE., IRPT )
!
! *** Open the output summary statistics file and write an info line
      IF( STA_USE ) THEN
        CALL OPEN_SUMMARY( IERR )
        IF( IERR .NE. 0 ) GO TO 999
      END IF
!
! *** Open the output detailed risk information file
      IF( DET_USE ) THEN
        CALL OPEN_DETAIL( IERR )
        IF( IERR .NE. 0 ) GO TO 999
      END IF
!
! *** Open the output file for generated values
      IF( BG_STOC_VALU ) THEN
        CALL OPEN_VALUE( IERR )
        IF( IERR .NE. 0 ) GO TO 999
      END IF
!
! *** Generate stochastic information for all realizations
      CALL TELLTIME( 'Generating Stochastic Variables', 'SCREEN', .FALSE., IRPT )
      CALL STOGEN( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Get the record number map for concentration data
      IF( BGVERB ) CALL TELLTIME( 'Reading ECDA map file', 'SCREEN', .FALSE., IRPT )
      CALL ECDA_MAPREAD( FNMAP, IMAP, IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Open all of the ECDA concentration files needed
      IF( BGVERB ) CALL TELLTIME( 'Opening ECDA concentration files', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_CON( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Echo more of the problem definition to the report file
      CALL ECHO_2(  )
!
! *** Open all the food concentration files needed
      IF( BGVERB ) CALL TELLTIME( 'Opening food concentration files', 'SCREEN', .FALSE., IRPT )
      CALL OPEN_FOODS( IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Report the units conversion values
      IF( BGVERB ) CALL TELLTIME( 'Writing units conversion data', 'SCREEN', .FALSE., IRPT )
      CALL REPORT_UNITS( )
!
!---- Loop over all times and locations ---------------------------------------------------
!
      DO ITIM = 1, ESD_NUM_TIM
!
        IF( .NOT.ESD_TIM(ITIM)%COMP ) CYCLE
!
        CYEAR = ' '
        WRITE(CYEAR(1:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL TELLTIME( 'Computing year '//TRIM(CYEAR), 'SCREEN', .FALSE., IRPT )
!
! ***   Initialize population dose for this time slice
        IF( BGVERB ) CALL TELLTIME( 'CALL INIT_POPDOSE', 'SCREEN', .FALSE., IRPT )
        CALL INIT_POPDOSE( )
!
        DO ILOC = 1, ESD_NUM_LOC
!
          IF( .NOT.ESD_LOC(ILOC)%COMP ) CYCLE
!
! ***     Read the concentration data for all analytes and
!         all realizations for this time slice and this location
          IF( BGVERB ) CALL TELLTIME( 'CALL CON_DATA', 'SCREEN', .FALSE., IRPT )
          CALL CON_DATA( ITIM, ILOC, IERR )
          IF( IERR .NE. 0 ) GO TO 999
!
! ***     Calculate food concentrations for a given time and location
          IF( BGVERB ) CALL TELLTIME( 'CALL FOOD_READ', 'SCREEN', .FALSE., IRPT )
          CALL FOOD_READ( ITIM, ILOC, IERR )
          IF( IERR .NE. 0 ) GO TO 999
!
! ***     Process all of the radioactive analytes at this location
          IF( NUMRAD .GT. 0 ) THEN
            IF( BGVERB ) CALL TELLTIME( 'CALL RAD_DOSE', 'SCREEN', .FALSE., IRPT )
            CALL RAD_DOSE( ITIM, ILOC, IERR )
            IF( IERR .NE. 0 ) GO TO 999
          END IF
!
! ***     Process all of the carcinogenic analytes at this location
          IF( NUMCAR .GT. 0 ) THEN
            IF( BGVERB ) CALL TELLTIME( 'CALL CAR_DOSE', 'SCREEN', .FALSE., IRPT )
            CALL CAR_DOSE( ITIM, ILOC, IERR )
            IF( IERR .NE. 0 ) GO TO 999
          END IF
!
! ***     Output summed risk for RAD and CAR if requested  'COMBIN'  "ALL' 'Risk'
          IF( (DET_SUMRISK.OR.STA_SUMRISK) .AND. (NUMCAR.GT.0 .OR. NUMRAD.GT.0) ) THEN
            IF( BGVERB ) CALL TELLTIME( 'CALL SUM_RISKS', 'SCREEN', .FALSE., IRPT )
            CALL SUM_RISKS( ITIM, ILOC, IERR )
            IF( IERR .NE. 0 ) GO TO 999
          END IF
!
! ***     Process all of the hazardous analytes at this location
          IF( NUMHAZ .GT. 0 ) THEN
            IF( BGVERB ) CALL TELLTIME( 'CALL HAZ_DOSE', 'SCREEN', .FALSE., IRPT )
            CALL HAZ_DOSE( ITIM, ILOC, IERR )
            IF( IERR .NE. 0 ) GO TO 999
          END IF
!
        END DO
!
! ***   Finish population dose for this time slice
        IF( BGVERB ) CALL TELLTIME( 'CALL FINISH_POPDOSE', 'SCREEN', .FALSE., IRPT )
        CALL FINISH_POPDOSE( ITIM, IERR )
        IF( IERR .NE. 0 ) GO TO 999
!
      END DO
!
! *** Elapsed time message
      CALL ELAPSE( 2, IRPT, IERR )
      IF( IERR .NE. 0 ) GO TO 999
!
! *** Normal completion of the program
      GO TO 1000
!
!-----------------------------------------------------------------------------------------------
!     Fatal errors trapped after the report file is available return
!     to this point for termination (errors trapped before the report
!     file is available terminate at the point of the error trap).
!-----------------------------------------------------------------------------------------------
!
  999 CONTINUE
      IERR = 999
      IF( REPORT ) THEN
        MESSAG(1) = 'Error encountered in a lower-level routine.'
        MESSAG(2) = 'Execution halted because of the above errors.'
        CALL PRTERR( IERR, CALLER, 2 )
      ELSE
        WRITE(*,*) 'Error encountered in a lower-level routine.'
        WRITE(*,*) 'Execution halted because of the above errors.'
        WRITE(*,*) 'Program stop in ' // TRIM(CALLER)
      END IF
      CALL TELLTIME( 'Error Termination', 'SCREEN', .TRUE., IRPT )
      STOP
!
 1000 CONTINUE
      MESSAG(1) = 'Normal Termination'
      CALL PRTERR( IERR, CALLER, 1 )
      CALL TELLTIME( 'Normal Termination', 'SCREEN', .TRUE., IRPT )
      STOP
!
      END PROGRAM

      SUBROUTINE BANNER_1( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints the first portion of a banner page to the open report file.
!!
!!  History:
!!    Paul W. Eslinger : 20 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 23 Oct 2003 : SAC Rev. 1
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
      WRITE(IRPT,1010) 'H     H  U     U  M     M   AAAAA   N     N'
      WRITE(IRPT,1010) 'H     H  U     U  MM   MM  A     A  NN    N'
      WRITE(IRPT,1010) 'H     H  U     U  M M M M  A     A  N N   N'
      WRITE(IRPT,1010) 'HHHHHHH  U     U  M  M  M  AAAAAAA  N  N  N'
      WRITE(IRPT,1010) 'H     H  U     U  M     M  A     A  N   N N'
      WRITE(IRPT,1010) 'H     H  U     U  M     M  A     A  N    NN'
      WRITE(IRPT,1010) 'H     H   UUUUU   M     M  A     A  N     N'
 1010 FORMAT(18X,A)
!
      WRITE(IRPT,1020) PRGNAM, PRGVER, PRGDAT
 1020 FORMAT(//34X,A8,1X,A8/26X,'Last Modified on ',A12)
!
      RETURN
      END SUBROUTINE

      SUBROUTINE BANNER_2( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints additional banner information to the report file.
!!
!!  History:
!!    Paul W. Eslinger :  9 May 2000 : Version 2.0
!!    Paul W. Eslinger : 29 Jan 2003 : Version 3.0 : SAC Rev. 1
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

      SUBROUTINE CAR_DOSE( ITIM, ILOC, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine computes the dose and risk from all of the carcinogenic analytes for a single
!!    location for a single time and outputs the results.
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Oct 1996 : Version 1.0
!!    Paul W. Eslinger : 10 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Sep 2002 : Change debug formats
!!    Paul W. Eslinger : 24 Feb 2003 : SAC REV.1
!!    Paul W. Eslinger : 21 Sep 2004 : Set CSWEAT concentrations
!!    Paul W. Eslinger :  3 Mar 2005 : SCR-1073 Add ETGROUND
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!    Paul W. Eslinger :  2 Dec 2005 : Rearrange debug outputs
!!    Paul W. Eslinger : 28 Feb 2006 : SCR-1107 - Add dermal to sweat lodge
!!                                     Add explicit shower exposure
!!                                     Expand to both dose and risk solutions
!!    Paul W. Eslinger : 13 Apr 2006 : SCR-1119 - Fix output flags
!!    Paul W. Eslinger : 30 Oct 2006 : SCR-1148 Fix averaging time variables
!!    Paul W. Eslinger : 15 Jul 2012 : Modify output format
!!
!!**************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Concen_Mod
      USE Results_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Stocvars_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: PRINTRISK
      REAL, EXTERNAL :: PERCENT
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'CAR_DOSE' ! Name of this subroutine
      CHARACTER(LEN=80) :: CTMP ! Local character variable
      INTEGER :: LEN_CTMP       ! Variable length of CTMP
!
      REAL :: XMIN ! Statistics - minimum
      REAL :: XV01 ! Statistics -  1st percentile
      REAL :: XV05 ! Statistics -  5th percentile
      REAL :: XV10 ! Statistics - 10th percentile
      REAL :: XV25 ! Statistics - 25th percentile
      REAL :: XMED ! Statistics - median
      REAL :: XV75 ! Statistics - 75th percentile
      REAL :: XV90 ! Statistics - 90th percentile
      REAL :: XV95 ! Statistics - 95th percentile
      REAL :: XV99 ! Statistics - 99th percentile
      REAL :: XMAX ! Statistics - maximum
      REAL :: XAVG ! Statistics - average
      REAL :: XSTD ! Statistics - standard deviation
!
      INTEGER :: IREL    ! Realization looping index
      INTEGER :: IANA    ! Analyte looping index
      INTEGER :: RACTION ! Risk action flag for the print routine
!
      REAL :: TMP_ING_WATER          ! Water hazard increment
      REAL :: TMP_ING_SWAT           ! Water hazard increment - surface water
      REAL :: TMP_ING_GWAT           ! Water hazard increment - ground water
      REAL :: TMP_ING_SEEP           ! Water hazard increment - seeps
      REAL :: TMP_ING_SOIL           ! Soil hazard increment
      REAL :: TMP_ING_FOOD           ! Food hazard increment
      REAL :: TMP_ING_LEAFVEG        ! Leafy vegetable hazard increment
      REAL :: TMP_ING_ROOTVEG        ! Root vegetable hazard increment
      REAL :: TMP_ING_MEAT           ! Meat hazard increment
      REAL :: TMP_ING_MILK           ! Milk hazard increment
      REAL :: TMP_ING_BIRD           ! Bird hazard increment
      REAL :: TMP_ING_FRUIT          ! Fruit hazard increment
      REAL :: TMP_ING_GRAIN          ! Grain hazard increment
      REAL :: TMP_ING_EGGS           ! Eggs hazard increment
      REAL :: TMP_ING_FISH           ! Fish hazard increment
      REAL :: TMP_ING_FISH_2         ! Fish_2 hazard increment
      REAL :: TMP_ING_FISH_3         ! Fish_3 hazard increment
      REAL :: TMP_ING_SEDIMENT       ! Sediment hazard increment
      REAL :: TMP_ING_SEDIMENT_CHILD ! Sediment hazard increment - child
      REAL :: TMP_ING_SEDIMENT_ADULT ! Sediment hazard increment - adult
      REAL :: TMP_ING_SOIL_CHILD     ! Soil hazard increment - child
      REAL :: TMP_ING_SOIL_ADULT     ! Soil hazard increment - child
      REAL :: TMP_ING_DIRT           ! Soil/sediment hazard increment
!
      REAL :: TMP_INH_SOIL   ! Hazard increment from air mass loading of soil
      REAL :: TMP_INH_SEEP   ! Hazard increment from seep water in air
      REAL :: TMP_INH_SWAT   ! Hazard increment from surface water in air
      REAL :: TMP_INH_SWEAT  ! Hazard increment from sweat lodge (water in air)
      REAL :: TMP_INH_SHOWER ! Hazard increment from shower (water in air)
      REAL :: TMP_INH_AIRC   ! Hazard increment from farfield air concentrations
!
      REAL :: TMP_CON_INH_SOIL   ! Concentration for air mass loading of soil
      REAL :: TMP_CON_INH_SEEP   ! Concentration for seep water in air
      REAL :: TMP_CON_INH_SWAT   ! Concentration for surface water in air
      REAL :: TMP_CON_INH_SHOWER ! Concentration for water in a shower
      REAL :: TMP_CON_INH_SWEAT  ! Concentration for sweat lodge air
      REAL :: TMP_CON_INH_AIRC   ! Concentration for farfield air concentrations
!
      REAL :: TMP_DER_SOIL   ! Hazard increment from dermal contact with soil
      REAL :: TMP_DER_SEDI   ! Hazard increment from dermal contact with sediment
      REAL :: TMP_DER_SEEP   ! Hazard increment from dermal contact with seep water
      REAL :: TMP_DER_SWEAT  ! Hazard increment from dermal contact with sweat lodge water
      REAL :: TMP_DER_SHOWER ! Hazard increment from dermal contact with shower water
      REAL :: TMP_DER_SWIM   ! Hazard increment from dermal contact with swimming water
!
      LOGICAL :: ANYRISK ! Flag if any carcinogenic risk is requested
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
      RACTION = 1
!
      IF( BGCAR ) THEN
        WRITE(IRPT,1000) 'Entering ' // CALLER
 1000   FORMAT(/A)
      END IF
!
! *** Check on the validity of ITIM
!
      IF( ITIM.LT.1 .OR. ITIM.GT.ESD_NUM_TIM ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid time index'
        MESSAG(2) = 'Value = '
        WRITE(MESSAG(2)(9:),*) ITIM
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check on the validity of ILOC
!
      IF( ILOC.LT.1 .OR. ILOC.GT.ESD_NUM_LOC ) THEN
        IERR = 2
        MESSAG(1) = 'Invalid location index'
        MESSAG(2) = 'Value = '
        WRITE(MESSAG(2)(9:),*) ILOC
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** More debug writes
!
      IF( BGCAR ) THEN
!
        CTMP = 'Location index '
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ILOC
        CTMP = TRIM(CTMP) // ' : '//ESD_LOC(ILOC)%ID
        WRITE(IRPT,1010) TRIM(CTMP)
 1010   FORMAT(A)
!
        CTMP = 'Time index '
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+1):),'(I0)') ITIM
        CTMP = TRIM(CTMP) // ' : Calendar year'
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ESD_TIM(ITIM)%TIME
        WRITE(IRPT,1010) TRIM(CTMP)
!
        WRITE(IRPT,1020) NREAL, 'Numer of realizations to be processed'
 1020   FORMAT(3X,I0,' : ',A)
!
        WRITE(IRPT,1030) AVGTIME_CAR, 'AVGTIME_CAR'
        WRITE(IRPT,1030) CF3, 'CF3'
        WRITE(IRPT,1030) CF4, 'CF4'
        WRITE(IRPT,1030) CF7, 'CF7'
 1030   FORMAT(3X,1P,E12.5,' : ',A)
!
      END IF
!
! *** Initialize the carcinogenic dose and risk to zero for this location
!
      FORALL(IREL=1:NREAL) DOSECAR(IREL) = 0.0
      FORALL(IREL=1:NREAL) RISKCAR(IREL) = 0.0
!
! *** Fill the soil concentrations to match the location and food types
!
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'AQUATIC' ) FORALL(IREL=1:NREAL,IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = 0.0
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) FORALL(IREL=1:NREAL,IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SORP(IANA,IREL)
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
        IF( SOIL_IDX .EQ. 1 ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SODR(IANA,IREL) ! Dry location
        IF( SOIL_IDX .EQ. 2 ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SOGW(IANA,IREL) ! Groundwater irrigation
        IF( SOIL_IDX .EQ. 3 ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SOSW(IANA,IREL) ! Surface water irrig
      END IF
!
! *** Fill the sweat lodge water concentrations
!       Default to zero if the sweat lodge option is not used
      CSWEAT = 0.0
      IF( SWEAT_SOURCE .EQ. 'GWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSWEAT(IANA,IREL) = GWAT(IANA,IREL)
      IF( SWEAT_SOURCE .EQ. 'SWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSWEAT(IANA,IREL) = SWAT(IANA,IREL)
      IF( SWEAT_SOURCE .EQ. 'SEEP' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSWEAT(IANA,IREL) = SEEP(IANA,IREL)
!
! *** Fill the shower water concentrations
!       Default to zero if the showering option is not used
      CSHOWER = 0.0
      IF( SHOWER_SOURCE .EQ. 'GWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSHOWER(IANA,IREL) = GWAT(IANA,IREL)
      IF( SHOWER_SOURCE .EQ. 'SWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSHOWER(IANA,IREL) = SWAT(IANA,IREL)
      IF( SHOWER_SOURCE .EQ. 'SEEP' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSHOWER(IANA,IREL) = SEEP(IANA,IREL)
!
! *** Loop over analytes
!
      ANYRISK = .FALSE.
      DO IANA = 1, ESD_NUM_ANA
!
        IF( .NOT.ESD_ANA(IANA)%COMP )       CYCLE ! Skip all analytes that are not requested
        IF( .NOT.ESD_ANA(IANA)%ANATYP_CAR ) CYCLE ! Skip all analytes that are no carcinogenic
!
!       Flag for output of the risk sum over analytes
        IF( ESD_ANA(IANA)%SFING .OR. ESD_ANA(IANA)%SFINH ) ANYRISK = .TRUE.
!
        DO IREL = 1, NREAL
!
! ***     Ingestion - water
          TMP_ING_SWAT = SWAT(IANA,IREL) * INGEST(IREL)%IRSURFACE  * EXPOS(IREL)%ED * EXPOS(IREL)%EFSURFACE  * CF7 /&
              (AVGTIME_CAR * BODY(IREL)%BWADULT )
          TMP_ING_GWAT = GWAT(IANA,IREL) * INGEST(IREL)%IRGROUND * EXPOS(IREL)%ED * EXPOS(IREL)%EFGROUND * CF7 /&
              (AVGTIME_CAR * BODY(IREL)%BWADULT )
          TMP_ING_SEEP = SEEP(IANA,IREL) * INGEST(IREL)%IRSEEP   * EXPOS(IREL)%ED * EXPOS(IREL)%EFSEEP   * CF7 /&
              (AVGTIME_CAR * BODY(IREL)%BWADULT )
          TMP_ING_WATER = TMP_ING_SWAT + TMP_ING_GWAT + TMP_ING_SEEP
!
! ***     Ingestion - soil
          TMP_ING_SOIL_CHILD = CSOIL(IANA,IREL) * INGEST(IREL)%IRSOILCHILD * EXPOS(IREL)%EDCHILD * EXPOS(IREL)%EFSOIL * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWCHILD)
          TMP_ING_SOIL_ADULT = CSOIL(IANA,IREL) * INGEST(IREL)%IRSOILADULT * EXPOS(IREL)%EDADULT * EXPOS(IREL)%EFSOIL * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_SOIL  = TMP_ING_SOIL_CHILD + TMP_ING_SOIL_ADULT
!
! ***     Ingestion - sediment
          TMP_ING_SEDIMENT_CHILD = SEDI(IANA,IREL) * INGEST(IREL)%IRSEDCHILD * EXPOS(IREL)%EDCHILD * EXPOS(IREL)%EFSED * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWCHILD)
          TMP_ING_SEDIMENT_ADULT = SEDI(IANA,IREL) * INGEST(IREL)%IRSEDADULT * EXPOS(IREL)%EDADULT * EXPOS(IREL)%EFSED * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_SEDIMENT = TMP_ING_SEDIMENT_CHILD + TMP_ING_SEDIMENT_ADULT
!
          TMP_ING_DIRT = TMP_ING_SOIL + TMP_ING_SEDIMENT
!
! ***     Ingestion - food
          TMP_ING_LEAFVEG = CLEAFVEG(IANA,IREL) * INGEST(IREL)%IRLEAFVEG * EXPOS(IREL)%ED * EXPOS(IREL)%EFLEAFVEG * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_ROOTVEG = CROOTVEG(IANA,IREL) * INGEST(IREL)%IRROOTVEG * EXPOS(IREL)%ED * EXPOS(IREL)%EFROOTVEG * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_MEAT    = CMEAT(IANA,IREL)    * INGEST(IREL)%IRMEAT    * EXPOS(IREL)%ED * EXPOS(IREL)%EFMEAT    * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_MILK    = CMILK(IANA,IREL)    * INGEST(IREL)%IRMILK    * EXPOS(IREL)%ED * EXPOS(IREL)%EFMILK    * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_BIRD    = CBIRD(IANA,IREL)    * INGEST(IREL)%IRBIRD    * EXPOS(IREL)%ED * EXPOS(IREL)%EFBIRD    * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_FRUIT   = CFRUIT(IANA,IREL)   * INGEST(IREL)%IRFRUIT   * EXPOS(IREL)%ED * EXPOS(IREL)%EFFRUIT   * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_GRAIN   = CGRAIN(IANA,IREL)   * INGEST(IREL)%IRGRAIN   * EXPOS(IREL)%ED * EXPOS(IREL)%EFGRAIN   * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_EGGS    = CEGGS(IANA,IREL)    * INGEST(IREL)%IREGGS    * EXPOS(IREL)%ED * EXPOS(IREL)%EFEGGS    * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_FISH    = CFISH(IANA,IREL)    * INGEST(IREL)%IRFISH    * EXPOS(IREL)%ED * EXPOS(IREL)%EFFISH    * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_FISH_2  = CFISH_2(IANA,IREL)  * INGEST(IREL)%IRFISH_2  * EXPOS(IREL)%ED * EXPOS(IREL)%EFFISH_2  * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_FISH_3  = CFISH_3(IANA,IREL)  * INGEST(IREL)%IRFISH_3  * EXPOS(IREL)%ED * EXPOS(IREL)%EFFISH_3  * CF7 /&
            (AVGTIME_CAR*BODY(IREL)%BWADULT)
          TMP_ING_FOOD  = TMP_ING_LEAFVEG + TMP_ING_ROOTVEG + TMP_ING_MEAT + TMP_ING_MILK + TMP_ING_BIRD + TMP_ING_FRUIT + &
              TMP_ING_GRAIN + TMP_ING_EGGS + TMP_ING_FISH + TMP_ING_FISH_2 + TMP_ING_FISH_3
!
! ***     Total ingestion
          ING_INC_DOS(IREL) = TMP_ING_WATER + TMP_ING_DIRT + TMP_ING_FOOD
!
! ***     Inhalation - soil
          TMP_INH_SOIL = CSOIL(IANA,IREL) * ML(IREL) * EXPOS(IREL)%ETSOIL* EXPOS(IREL)%EFSOIL * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_CAR * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_SOIL = CSOIL(IANA,IREL) * ML(IREL)
!
! ***     Inhalation - seep water (outdoor)
          TMP_INH_SEEP = SEEP(IANA,IREL) * VF(IANA,IREL) * EXPOS(IREL)%ETSEEP * EXPOS(IREL)%EFSEEP * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_CAR * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_SEEP = SEEP(IANA,IREL) * VF(IANA,IREL)
!
! ***     Inhalation - surface water (outdoor)
          TMP_INH_SWAT = SWAT(IANA,IREL) * VF(IANA,IREL) * EXPOS(IREL)%ETSURFACE * EXPOS(IREL)%EFSURFACE * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_CAR * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_SWAT = SWAT(IANA,IREL) * VF(IANA,IREL)
!
! ***     Inhalation - showering water (indoor)
          TMP_INH_SHOWER = CSHOWER(IANA,IREL) * CFSHOWER(IREL) * EXPOS(IREL)%ETSHOWER * EXPOS(IREL)%EFSHOWER * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_CAR * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_SHOWER = CSHOWER(IANA,IREL) * CFSHOWER(IREL)
!
! ***     Inhalation - sweat lodge water
          TMP_INH_SWEAT = CSWEAT(IANA,IREL) * CFSWEAT(IREL) * EXPOS(IREL)%ETSWEAT * EXPOS(IREL)%EFSWEAT * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_CAR * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_SWEAT = CSWEAT(IANA,IREL) * CFSWEAT(IREL)
!
! ***     Inhalation - air
          TMP_INH_AIRC = AIRC(IANA,IREL) * EXPOS(IREL)%ETAIR * EXPOS(IREL)%EFAIR * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_CAR * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_AIRC = AIRC(IANA,IREL)
!
! ***     Total inhalation dose
          INH_INC_DOS(IREL) = TMP_INH_SOIL + TMP_INH_SEEP + TMP_INH_SWAT + TMP_INH_SHOWER + TMP_INH_SWEAT + TMP_INH_AIRC
!
! ***     Dermal - soil
          TMP_DER_SOIL = CSOIL(IANA,IREL) * AFSOIL(IREL) * ABSORP(IANA,IREL) * BODY(IREL)%SASOIL * EXPOS(IREL)%EFSOIL * &
              CF1 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT *AVGTIME_CAR )
!
! ***     Dermal - sediment
          TMP_DER_SEDI = SEDI(IANA,IREL) * AFSED(IREL) * ABSORP(IANA,IREL) * BODY(IREL)%SASED * EXPOS(IREL)%EFSED * &
            CF1 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT *AVGTIME_CAR )
!
! ***     Dermal - seep
          TMP_DER_SEEP = SEEP(IANA,IREL) * KP(IANA,IREL) * BODY(IREL)%SASEEP * EXPOS(IREL)%ETSEEP * EXPOS(IREL)%EFSEEP * &
            CF3 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT *AVGTIME_CAR )
!
! ***     Dermal - swimming
          TMP_DER_SWIM = SWAT(IANA,IREL) * KP(IANA,IREL) * BODY(IREL)%SASWIM * EXPOS(IREL)%ETSWIM * EXPOS(IREL)%EFSWIM * &
            CF3 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT *AVGTIME_CAR )
!
! ***     Dermal - sweat lodge
          TMP_DER_SWEAT = CSWEAT(IANA,IREL) * KP(IANA,IREL) * BODY(IREL)%SASWEAT * EXPOS(IREL)%ETSWEAT * EXPOS(IREL)%EFSWEAT * &
            CF3 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT *AVGTIME_CAR )
!
! ***     Dermal - showering
          TMP_DER_SHOWER = CSHOWER(IANA,IREL) * KP(IANA,IREL) * BODY(IREL)%SASHOWER * EXPOS(IREL)%ETSHOWER * EXPOS(IREL)%EFSHOWER* &
            CF3 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT *AVGTIME_CAR )
!
! ***     Total dermal dose
          EXT_INC_DOS(IREL) = TMP_DER_SEDI + TMP_DER_SEEP + TMP_DER_SOIL + TMP_DER_SWIM + TMP_DER_SWEAT + TMP_DER_SHOWER
!
! ***     Dose and risk increments for this analyte (check for solutions requested for risk)
          DOSEINC(IREL) = ING_INC_DOS(IREL) + INH_INC_DOS(IREL) + EXT_INC_DOS(IREL)
!
          RISKINC(IREL) = 0.0
          IF( ESD_ANA(IANA)%SFING ) RISKINC(IREL) = RISKINC(IREL) + ING_INC_DOS(IREL)*SFING(IANA,IREL) + &
                                                    EXT_INC_DOS(IREL)*SFING(IANA,IREL)
          IF( ESD_ANA(IANA)%SFINH ) RISKINC(IREL) = RISKINC(IREL) + INH_INC_DOS(IREL)*SFINH(IANA,IREL)
!
! ***     Increment the risk variable for all carcinogenic analytes
          DOSECAR(IREL) = DOSECAR(IREL) + DOSEINC(IREL)
          RISKCAR(IREL) = RISKCAR(IREL) + RISKINC(IREL)
!
          IF( BGCAR .OR. ESD_ANA(IANA)%VERBOSE .OR. ESD_ANA(IANA)%TERSE ) THEN
            WRITE(IRPT,1000) 'Processing Location    : ' // ESD_LOC(ILOC)%ID // ' : "' // TRIM(ESD_LOC(ILOC)%NAME) // '"'
            WRITE(IRPT,1010) 'Processing Analyte     : ' // ESD_ANA(IANA)%ID // ' : "' // TRIM(ESD_ANA(IANA)%NAME) // '"'
            CTMP = 'Processing realization : '
            WRITE(CTMP(26:),'(I0)') IREL
            WRITE(IRPT,1010) TRIM(CTMP)
          END IF
!
          IF( BGCAR .OR. ESD_ANA(IANA)%VERBOSE ) THEN
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Risk increments for the ingestion pathway for (carcinogenic) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Pathway           Dose                  Risk              Ingestion           '//&
              ' Concentration        Exposure Freq.       Food Definition'
            WRITE(IRPT,1010) '   ----------------- --------------------- ----------------- --------------------'//&
              ' -------------------- -------------------  -------------------------------------------'
            WRITE(IRPT,1040) 'Surface water   ', TMP_ING_SWAT,           'mg/kg-day', &
              PRINTRISK(TMP_ING_SWAT,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),           'Risk', &
              INGEST(IREL)%IRSURFACE,   'L/day  ', SWAT(IANA,IREL),     'ug/L   ', EXPOS(IREL)%EFSURFACE, 'day/yr  '//LABEL_SURFACE
            WRITE(IRPT,1040) 'Ground water    ', TMP_ING_GWAT,           'mg/kg-day', &
              PRINTRISK(TMP_ING_GWAT,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),           'Risk', &
              INGEST(IREL)%IRGROUND,    'L/day  ', GWAT(IANA,IREL),     'ug/L   ', EXPOS(IREL)%EFGROUND,  'day/yr  '//LABEL_GROUND
            WRITE(IRPT,1040) 'Seep water      ', TMP_ING_SEEP,           'mg/kg-day', &
              PRINTRISK(TMP_ING_SEEP,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),           'Risk', &
              INGEST(IREL)%IRSEEP,      'L/day  ', SEEP(IANA,IREL),     'ug/L   ', EXPOS(IREL)%EFSEEP,    'day/yr  '//LABEL_SEEP
            WRITE(IRPT,1040) 'Soil (child)    ', TMP_ING_SOIL_CHILD,     'mg/kg-day', &
              PRINTRISK(TMP_ING_SOIL_CHILD,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),     'Risk', &
              INGEST(IREL)%IRSOILCHILD, 'kg/day ', CSOIL(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFSOIL,    'day/yr  '//&
              LABEL_SOIL_CHILD
            WRITE(IRPT,1040) 'Soil (adult)    ', TMP_ING_SOIL_ADULT,     'mg/kg-day', &
              PRINTRISK(TMP_ING_SOIL_ADULT,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),     'Risk', &
              INGEST(IREL)%IRSOILADULT, 'kg/day ', CSOIL(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFSOIL,    'day/yr  '//&
              LABEL_SOIL_ADULT
            WRITE(IRPT,1040) 'Sediment (child)', TMP_ING_SEDIMENT_CHILD, 'mg/kg-day', &
              PRINTRISK(TMP_ING_SEDIMENT_CHILD,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION), 'Risk', &
              INGEST(IREL)%IRSEDCHILD,  'kg/day ', SEDI(IANA,IREL),     'ug/kg  ', EXPOS(IREL)%EFSED,     'day/yr  '//&
              LABEL_SEDIMENT_CHILD
            WRITE(IRPT,1040) 'Sediment (adult)', TMP_ING_SEDIMENT_ADULT, 'mg/kg-day', &
              PRINTRISK(TMP_ING_SEDIMENT_ADULT,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION), 'Risk', &
              INGEST(IREL)%IRSEDADULT,  'kg/day ', SEDI(IANA,IREL),     'ug/kg  ', EXPOS(IREL)%EFSED,     'day/yr  '//&
              LABEL_SEDIMENT_ADULT
            WRITE(IRPT,1040) 'Leafy Vegetables', TMP_ING_LEAFVEG,        'mg/kg-day', &
              PRINTRISK(TMP_ING_LEAFVEG,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),        'Risk', &
              INGEST(IREL)%IRLEAFVEG,   'kg/day ', CLEAFVEG(IANA,IREL), 'ug/kg  ', EXPOS(IREL)%EFLEAFVEG, 'day/yr  '//LABEL_LEAFVEG
            WRITE(IRPT,1040) 'Root Vegetables ', TMP_ING_ROOTVEG,        'mg/kg-day', &
              PRINTRISK(TMP_ING_ROOTVEG,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),        'Risk', &
              INGEST(IREL)%IRROOTVEG,   'kg/day ', CROOTVEG(IANA,IREL), 'ug/kg  ', EXPOS(IREL)%EFROOTVEG, 'day/yr  '//LABEL_ROOTVEG
            WRITE(IRPT,1040) 'Meat            ', TMP_ING_MEAT,           'mg/kg-day', &
              PRINTRISK(TMP_ING_MEAT,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),           'Risk', &
              INGEST(IREL)%IRMEAT,      'kg/day ', CMEAT(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFMEAT,    'day/yr  '//LABEL_MEAT
            WRITE(IRPT,1040) 'Milk            ', TMP_ING_MILK,           'mg/kg-day', &
              PRINTRISK(TMP_ING_MILK,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),           'Risk', &
              INGEST(IREL)%IRMILK,      'L/day  ', CMILK(IANA,IREL),    'ug/L   ', EXPOS(IREL)%EFMILK,    'day/yr  '//LABEL_MILK
            WRITE(IRPT,1040) 'Bird            ', TMP_ING_BIRD,           'mg/kg-day', &
              PRINTRISK(TMP_ING_BIRD,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),           'Risk', &
              INGEST(IREL)%IRBIRD,      'kg/day ', CBIRD(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFBIRD,    'day/yr  '//LABEL_BIRD
            WRITE(IRPT,1040) 'Fruit           ', TMP_ING_FRUIT,          'mg/kg-day', &
              PRINTRISK(TMP_ING_FRUIT,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),          'Risk', &
              INGEST(IREL)%IRFRUIT,     'kg/day ', CFRUIT(IANA,IREL),   'ug/kg  ', EXPOS(IREL)%EFFRUIT,   'day/yr  '//LABEL_FRUIT
            WRITE(IRPT,1040) 'Grain           ', TMP_ING_GRAIN,          'mg/kg-day', &
              PRINTRISK(TMP_ING_GRAIN,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),          'Risk', &
              INGEST(IREL)%IRGRAIN,     'kg/day ', CGRAIN(IANA,IREL),   'ug/kg  ', EXPOS(IREL)%EFGRAIN,   'day/yr  '//LABEL_GRAIN
            WRITE(IRPT,1040) 'Eggs            ', TMP_ING_EGGS,           'mg/kg-day', &
              PRINTRISK(TMP_ING_EGGS,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),           'Risk', &
              INGEST(IREL)%IREGGS,      'kg/day ', CEGGS(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFEGGS,    'day/yr  '//LABEL_EGGS
            WRITE(IRPT,1040) 'Fish            ', TMP_ING_FISH,           'mg/kg-day', &
              PRINTRISK(TMP_ING_FISH,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),           'Risk', &
              INGEST(IREL)%IRFISH,      'kg/day ', CFISH(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFFISH,    'day/yr  '//LABEL_FISH
            WRITE(IRPT,1040) 'Fish_2          ', TMP_ING_FISH_2,         'mg/kg-day', &
              PRINTRISK(TMP_ING_FISH_2,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),         'Risk', &
              INGEST(IREL)%IRFISH_2,    'kg/day ', CFISH_2(IANA,IREL),  'ug/kg  ', EXPOS(IREL)%EFFISH_2,  'day/yr  '//LABEL_FISH_2
            WRITE(IRPT,1040) 'Fish_3          ', TMP_ING_FISH_3,         'mg/kg-day', &
              PRINTRISK(TMP_ING_FISH_3,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),         'Risk', &
              INGEST(IREL)%IRFISH_3,    'kg/day ', CFISH_3(IANA,IREL),  'ug/kg  ', EXPOS(IREL)%EFFISH_3,  'day/yr  '//LABEL_FISH_3
 1040       FORMAT(3X,A,1X,1P,E12.5,1X,A,8(1X,E12.5,1X,A))
!
            WRITE(IRPT,1010) ' '
            CTMP = 'Inhalation rate (m^3/day) is '
            WRITE(CTMP(30:),'(1P,E12.5)') INGEST(IREL)%IRATE
            WRITE(IRPT,1010) 'Risk increments for the inhalation pathway for (carcinogenic) '//TRIM(ESD_ANA(IANA)%ID)//&
              '   ['//TRIM(CTMP)//']'
            WRITE(IRPT,1010) '   Pathway                    Dose                   Risk              Concentration       '//&
              ' Exposure Time       Exposure Frequency   Loading or Volatile'
            WRITE(IRPT,1010) '   -------------------------- ---------------------- ----------------- ------------------- '//&
              ' ------------------- -------------------- ------------------- '
            WRITE(IRPT,1040) 'Inhalation - Soil         ', TMP_INH_SOIL  , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,SFINH(IANA,IREL),ESD_ANA(IANA)%SFINH,RACTION),   'Risk', &
              TMP_CON_INH_SOIL,   'ug/m^3 ',&
              EXPOS(IREL)%EFSOIL,   'day/yr', EXPOS(IREL)%ETSOIL,   'hr/day', ML(IREL),       'kg/m^3'
            WRITE(IRPT,1040) 'Inhalation - Seep water   ', TMP_INH_SEEP  , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SEEP,SFINH(IANA,IREL),ESD_ANA(IANA)%SFINH,RACTION),   'Risk', &
              TMP_CON_INH_SEEP,   'ug/m^3 ',&
              EXPOS(IREL)%EFSEEP,   'day/yr', EXPOS(IREL)%ETSEEP,   'hr/day', VF(IANA,IREL),  'L/m^3'
            WRITE(IRPT,1040) 'Inhalation - Surface water', TMP_INH_SWAT  , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SWAT,SFINH(IANA,IREL),ESD_ANA(IANA)%SFINH,RACTION),   'Risk', &
              TMP_CON_INH_SWAT,   'ug/m^3 ',&
              EXPOS(IREL)%EFSURFACE, 'day/yr', EXPOS(IREL)%ETSURFACE,  'hr/day', VF(IANA,IREL),  'L/m^3'
            WRITE(IRPT,1040) 'Inhalation - Shower water ', TMP_INH_SHOWER, 'mg/kg-day', &
              PRINTRISK(TMP_INH_SHOWER,SFINH(IANA,IREL),ESD_ANA(IANA)%SFINH,RACTION), 'Risk', &
              TMP_CON_INH_SHOWER, 'ug/m^3 ',&
              EXPOS(IREL)%EFSHOWER, 'day/yr', EXPOS(IREL)%ETSHOWER, 'hr/day', CFSHOWER(IREL),  'L/m^3'
            WRITE(IRPT,1040) 'Inhalation - Sweat lodge  ', TMP_INH_SWEAT , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SWEAT,SFINH(IANA,IREL),ESD_ANA(IANA)%SFINH,RACTION), 'Risk', &
              TMP_CON_INH_SWEAT,  'ug/m^3 ', &
              EXPOS(IREL)%EFSWEAT,  'day/yr', EXPOS(IREL)%ETSWEAT,  'hr/day', CFSWEAT(IREL),  'L/m^3'
            WRITE(IRPT,1040) 'Inhalation - Air concent. ', TMP_INH_AIRC  , 'mg/kg-day', &
              PRINTRISK(TMP_INH_AIRC,SFINH(IANA,IREL),ESD_ANA(IANA)%SFINH,RACTION),   'Risk', &
              TMP_CON_INH_AIRC,   'ug/m^3 ',&
              EXPOS(IREL)%EFAIR,    'day/yr', EXPOS(IREL)%ETAIR,    'hr/day'
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Risk increments for the (soil) dermal pathway for (carcinogenic) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Pathway               Dose                   Risk             Adherence Factor        '//&
              ' Absorption         Concentration      Surface area      Exposure Freq.'
            WRITE(IRPT,1010) '   -------------------   ---------------------  ---------------- ------------------------'//&
              ' ------------------ ------------------ ----------------- -----------------'
            WRITE(IRPT,1040) 'Dermal - Soil       ', TMP_DER_SOIL, 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION), 'Risk', AFSOIL(IREL), 'mg/cm^2-day', &
              ABSORP(IANA,IREL), 'none ', CSOIL(IANA,IREL), 'ug/kg', BODY(IREL)%SASOIL, 'cm^2', EXPOS(IREL)%EFSOIL, 'day/yr'
            WRITE(IRPT,1040) 'Dermal - Sediment   ', TMP_DER_SEDI, 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION), 'Risk', AFSED(IREL),  'mg/cm^2-day', &
              ABSORP(IANA,IREL), 'none ', SEDI(IANA,IREL),  'ug/kg', BODY(IREL)%SASED,  'cm^2', EXPOS(IREL)%EFSED,  'day/yr'
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Risk increments for the (water) dermal pathway for (carcinogenic) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Pathway               Dose                   Risk             Permeability      '//&
              ' Concentration      Surface area      Exposure Duration   Exposure Frequency'
            WRITE(IRPT,1010) '   -------------------   ---------------------  ---------------- ------------------'//&
              ' ------------------ ----------------- ------------------- -------------------'
            WRITE(IRPT,1040) 'Dermal - seep water ', TMP_DER_SEEP , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION), 'Risk', KP(IANA,IREL),'cm/hr', &
              SEEP(IANA,IREL), 'ug/L ',    BODY(IREL)%SASEEP, 'cm^2', EXPOS(IREL)%ETSEEP,  'hr/day', EXPOS(IREL)%EFSEEP, 'day/yr'
            WRITE(IRPT,1040) 'Dermal - swimming   ', TMP_DER_SWIM , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION), 'Risk', KP(IANA,IREL),'cm/hr', &
              SWAT(IANA,IREL), 'ug/L ',    BODY(IREL)%SASWIM,'cm^2',  EXPOS(IREL)%ETSWIM,  'hr/day',  EXPOS(IREL)%EFSWIM, 'day/yr'
            WRITE(IRPT,1040) 'Dermal - sweat lodge', TMP_DER_SWEAT, 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION), 'Risk', KP(IANA,IREL),'cm/hr', &
              CSWEAT(IANA,IREL), 'ug/L ',  BODY(IREL)%SASWEAT,'cm^2', EXPOS(IREL)%ETSWEAT, 'hr/day', EXPOS(IREL)%EFSWEAT, 'day/yr'
          END IF
!
          IF( BGCAR .OR. ESD_ANA(IANA)%VERBOSE .OR. ESD_ANA(IANA)%TERSE ) THEN
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Summary of dose and risk increments for (carcinogenic) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Dose (mg/kg-day) Risk (risk)  Percent (dose) Percent (risk) Pathway'
            WRITE(IRPT,1010) '   ---------------- ------------ -------------- -------------- ----------------------'
            WRITE(IRPT,1055) TMP_ING_FOOD,  PRINTRISK(TMP_ING_FOOD,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION) ,&
              PERCENT(TMP_ING_FOOD,DOSEINC(IREL)), &
              PERCENT(PRINTRISK(TMP_ING_FOOD,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION) ,&
              RISKINC(IREL)), 'Food ingestion'
            WRITE(IRPT,1055) TMP_ING_DIRT,  PRINTRISK(TMP_ING_DIRT,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION) ,&
              PERCENT(TMP_ING_DIRT,DOSEINC(IREL)), &
              PERCENT(PRINTRISK(TMP_ING_DIRT,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION) ,&
              RISKINC(IREL)), 'Soil/sediment ingestion'
            WRITE(IRPT,1055) TMP_ING_WATER, PRINTRISK(TMP_ING_WATER,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),&
              PERCENT(TMP_ING_WATER,DOSEINC(IREL)),&
              PERCENT(PRINTRISK(TMP_ING_WATER,SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),&
              RISKINC(IREL)),'Water ingestion'
            WRITE(IRPT,1055) ING_INC_DOS(IREL), PRINTRISK(ING_INC_DOS(IREL),SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),&
              PERCENT(ING_INC_DOS(IREL),DOSEINC(IREL)),&
              PERCENT(PRINTRISK(ING_INC_DOS(IREL),SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),&
              RISKINC(IREL)),'All ingestion'
            WRITE(IRPT,1055) INH_INC_DOS(IREL), PRINTRISK(INH_INC_DOS(IREL),SFINH(IANA,IREL),ESD_ANA(IANA)%SFINH,RACTION),&
              PERCENT(INH_INC_DOS(IREL),DOSEINC(IREL)),&
              PERCENT(PRINTRISK(INH_INC_DOS(IREL),SFINH(IANA,IREL),ESD_ANA(IANA)%SFINH,RACTION),&
              RISKINC(IREL)),'Inhalation'
            WRITE(IRPT,1055) EXT_INC_DOS(IREL), PRINTRISK(EXT_INC_DOS(IREL),SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),&
              PERCENT(EXT_INC_DOS(IREL),DOSEINC(IREL)),&
              PERCENT(PRINTRISK(EXT_INC_DOS(IREL),SFING(IANA,IREL),ESD_ANA(IANA)%SFING,RACTION),&
              RISKINC(IREL)),'Dermal'
            WRITE(IRPT,1056) DOSEINC(IREL), RISKINC(IREL), 'Total'
 1055       FORMAT(3X,1P,E12.5,5X,E12.5,1X,E12.5,3X,E12.5,3X,A)
 1056       FORMAT(3X,1P,E12.5,1X,12X,1X,E12.5,1X,12X,1X,A)
          END IF
!
          IF( BGCAR .OR. ESD_ANA(IANA)%VERBOSE ) THEN
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Exposure durations'
            WRITE(IRPT,1030) EXPOS(IREL)%ED,      'Duration (years)'
            WRITE(IRPT,1030) EXPOS(IREL)%EDCHILD, 'Duration - child (years)'
            WRITE(IRPT,1030) EXPOS(IREL)%EDADULT, 'Duration - adult (years)'
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Slope factors for (carcinogenic) '//TRIM(ESD_ANA(IANA)%ID)
            IF( ESD_ANA(IANA)%SFING ) THEN
              WRITE(IRPT,1030) SFING(IANA,IREL),  'Ingestion  (risk per mg(intake)/(kg(bodyweight)/day))'
            ELSE
              WRITE(IRPT,1010) 'No risk for ingestion is requested'
            END IF
!
            IF( ESD_ANA(IANA)%SFINH ) THEN
              WRITE(IRPT,1030) SFINH(IANA,IREL),  'Inhalation (risk per mg(intake)/(kg(bodyweight)/day))'
            ELSE
              WRITE(IRPT,1010) 'No risk for inhalation is requested'
            END IF
!
          END IF
!
        END DO ! Realization loop
!
! ***   Compute summary statistics on the dose increments by pathway
        IF( STA_PATHDOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
!
!         Output ingestion dose summary statistics
          CALL USTAT( ING_INC_DOS, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable ING_INC_DOS'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'DOSEING', 'mg/kg-day', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
!         Output inhalation dose summary statistics
          CALL USTAT( INH_INC_DOS, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable INH_INC_DOS'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'DOSEINH', 'mg/kg-day', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
!         Output dermal dose summary statistics
          CALL USTAT( EXT_INC_DOS, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable EXT_INC_D'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'DOSEDER', 'mg/kg-day', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
        END IF
!
        IF( STA_PATHRISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
!
!         Output ingestion risk summary statistics
          IF( ESD_ANA(IANA)%SFING ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = ING_INC_DOS(IREL) * SFING(IANA,IREL)
            END DO
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable ING_INC*SFING -> CVEC'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'RISKING', 'Risk', &
              XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
!
          IF( ESD_ANA(IANA)%SFINH ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = INH_INC_DOS(IREL) * SFINH(IANA,IREL)
            END DO
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable INH_INC*SFINH -> CVEC'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'RISKINH', 'Risk', &
              XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
!
!         Output dermal risk summary statistics
          IF( ESD_ANA(IANA)%SFING ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = EXT_INC_DOS(IREL) * SFING(IANA,IREL)
            END DO
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable EXT_INC*SFING -> CVEC'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'RISKDER', 'Risk', &
              XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
!
        END IF
!
! ***   Compute summary statistics on the dose and risk for this analyte
        IF( STA_ANADOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL USTAT( DOSEINC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable RISKINC'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'ANADOSE', 'mg/kg-day', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
        END IF
!
        IF( STA_ANARISK .AND. (ESD_ANA(IANA)%SFING .OR. ESD_ANA(IANA)%SFING) &
          .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL USTAT( RISKINC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable RISKINC'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'ANARISK', 'Risk', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
        END IF
!
! ***   Output detailed values on the dose increments by pathway
        IF( DET_PATHDOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'DOSEING', 'mg/kg-day', &
            ING_INC_DOS, NREAL)
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'DOSEINH', 'mg/kg-day', &
            INH_INC_DOS, NREAL)
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'DOSEDER', 'mg/kg-day', &
            EXT_INC_DOS, NREAL)
        END IF
!
! ***   Output detailed values on the risk increments by pathway
        IF( DET_PATHRISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
!
!         Output the ingestion pathway risk
          IF( ESD_ANA(IANA)%SFING ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = ING_INC_DOS(IREL) * SFING(IANA,IREL)
            END DO
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'RISKING', 'Risk', CVEC, NREAL )
          END IF
!
!         Output the inhalation pathway risk
          IF( ESD_ANA(IANA)%SFINH ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = INH_INC_DOS(IREL) * SFINH(IANA,IREL)
            END DO
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'RISKINH', 'Risk', CVEC, NREAL )
          END IF
!
!         Output the dermal pathway risk
          IF( ESD_ANA(IANA)%SFING ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = EXT_INC_DOS(IREL) * SFING(IANA,IREL)
            END DO
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'RISKDER', 'Risk', CVEC, NREAL )
          END IF
!
        END IF
!
! ***   Output detailed dose values on this analyte
        IF( DET_ANADOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'ANADOSE', 'mg/kg-day', DOSEINC, NREAL)
        END IF
!
! ***   Output detailed risk values on this analyte
        IF( DET_ANARISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          IF( ESD_ANA(IANA)%SFINH .OR. ESD_ANA(IANA)%SFING ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CAR', 'ANARISK', 'Risk', RISKINC, NREAL )
        END IF
!
      END DO
!
! *** Compute combined summary statistics
!
!     Ouput summary statistics on risk summed over analytes
      IF( STA_SUMRISK .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
        IF( ANYRISK ) THEN
          CALL USTAT( RISKCAR, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable RISKCAR'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'COMBIN', 'CAR', 'SUMRISK', 'Risk', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
        END IF
      END IF
!
! *** Output combined detailed values
!
!     Output detailed values for risk summed over analytes
      IF( DET_SUMRISK .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
        IF( ANYRISK ) THEN
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'COMBIN', 'CAR', 'SUMRISK', 'Risk', RISKCAR, NREAL )
        END IF
      END IF
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
!!    A check is also made whether enough realizations were run in
!!    the ESD simulation to support this run of the HUMAN code.
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Apr 2000 : Version 1.0
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
      USE Control_Mod, ONLY: NREAL
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
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
! *** Number of species in the ESD file
!
      IF( ESD_NUM_SPC .LT. 1 ) THEN
        MESSAG(1) = 'No species defined in the ESD keyword file'
        MESSAG(2) = 'Check that no species are needed'
        MESSAG(3) = 'One fake species is defined'
        CALL PRTERR( IERR, CALLER, 3 )
        ESD_NUM_SPC = 1
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
      IF( ESD_NREAL .LT. NREAL ) THEN
        IERR = 5
        MESSAG(1) = 'More realizations requested than are in the ESD keyword file'
        MESSAG(2) = 'ESD Realizations = '
        WRITE(MESSAG(2)(20:),'(I0)') ESD_NREAL
        MESSAG(3) = 'Requested in this run = '
        WRITE(MESSAG(3)(25:),'(I0)') NREAL
        CALL PRTERR( IERR, CALLER, 3 )
       END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE CHECK_HUMAN( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the information from the keyword cards looking for definition problems.
!!    The error checking for stochastic variables is done at the point of generation instead of in
!!    this routine.
!!
!!  History:
!!
!!    Paul W. Eslinger : 17 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 17 May 2001 : Last modification
!!    Paul W. Eslinger : 23 Oct 2003 : Add short food source locations
!!    Paul W. Eslinger :  2 Feb 2006 : Add shower water source.  Add warnings on slope factors and
!!                                     reference doses.  Make SACVIEW header file optional.
!!    Paul W. Eslinger : 30 Oct 2006 : SCR-1148 - Fix averaging times for multiple analytes
!!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0004) Remove SACVIEW header files
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Terminal error encountered
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Seeds_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Stats_Mod, ONLY: BG_STOC_VALU
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'CHECK_HUMAN'
      INTEGER :: LOCTOT ! Total number of locations in this scenario
      INTEGER :: I      ! Looping index
      INTEGER :: IANA   ! Looping index
      INTEGER :: NUMTIM ! Number of requested times
      LOGICAL :: UPLAND ! Flag that upland locations are used
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** File name errors
!
      IF( FNSUM.EQ.' ' .AND. STA_USE ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The output summary statistics file name is missing'
        MESSAG(2) = 'Use the STATS modifier on the FILE Keyword'
        MESSAG(3) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( FNDET.EQ.' ' .AND. DET_USE ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The output detailed risks file name is missing'
        MESSAG(2) = 'Use the DETAIL modifier on the FILE Keyword'
        MESSAG(3) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( FNMAP .EQ. ' ' ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The input ECDA index file name is missing'
        MESSAG(2) = 'Use the I_ECDA modifier on the FILE Keyword'
        MESSAG(3) = 'Problem encountered in the ESD keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      IF( BG_STOC_VALU .AND. (FNVAL.EQ.' ') ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'Name of output stochastic generated data file not entered'
        MESSAG(2) = 'Enter it using the FILE keyword'
        MESSAG(3) = 'Use the VALUES modifier'
        MESSAG(4) = 'Problem in the HUMAN keyword file'
        CALL PRTERR( IERR, CALLER, 4 )
      END IF
!
! ***  Check for all concentration files
!
      DO IANA = 1, ESD_NUM_ANA
        IF( ESD_ANA(IANA)%COMP ) THEN
          IF( FNCON(IANA) .EQ. ' ' ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'The input ECDA concentration file name is missing'
            MESSAG(2) = 'Analyte ID = '//TRIM(ESD_ANA(IANA)%ID)
            MESSAG(3) = 'Problem encountered in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END IF
      END DO
!
! *** Control errors
!
      IF( NREAL .LE. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'At least one realization must be run (REALIZAT)'
        MESSAG(2) = 'Check the REALIZATION keyword'
        MESSAG(3) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
       END IF
!
      IF( SDSTOC .LE. 1.0D0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'Random seed must be greater than 1.0D0 (SEED)'
        MESSAG(2) = 'Check if the SEED keyword is entered'
        MESSAG(3) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
       END IF
!
! *** Analyte-related errors
!
      IF( HUM_NUM_ANA .EQ. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'At least one analyte must be requested (ANALYTE)'
        MESSAG(2) = 'Check if an ANALYTE keyword entry exists'
        MESSAG(3) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Analyte definitions
!
      DO IANA = 1, ESD_NUM_ANA
!
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
!
        IF( ESD_ANA(IANA)%ANATYP_HAZ ) THEN
          IF( .NOT.(ESD_ANA(IANA)%RFDING.OR.ESD_ANA(IANA)%RFDINH) ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'The hazardous anayte '//TRIM(ESD_ANA(IANA)%ID)//' : '//TRIM(ESD_ANA(IANA)%NAME)
            MESSAG(2) = 'Does not have a reference dose for ingestion or inhalation'
            MESSAG(3) = 'Check the ANALYTE keyword in the Human keyword file for correct definitions'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END IF
!
        IF( ESD_ANA(IANA)%ANATYP_CAR ) THEN
          IF( .NOT.(ESD_ANA(IANA)%SFING.OR.ESD_ANA(IANA)%SFINH) ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'The carcinogenic anayte '//TRIM(ESD_ANA(IANA)%ID)//' : '//TRIM(ESD_ANA(IANA)%NAME)
            MESSAG(2) = 'Does not have a slope factor for ingestion or inhalation'
            MESSAG(3) = 'Check the ANALYTE keyword in the Human keyword file for correct definitions'
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
        END IF
!
      END DO
!
! *** Averaging times
!
      IF( NUMHAZ .GT. 0 ) THEN
        IF( AVGTIME_HAZ.LT.365.0 .OR. AVGTIME_HAZ.GT.36525.0 ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'The averaging time is not properly specified for hazardous analytes'
          MESSAG(2) = 'Check the HAZARDOU modifier on the AVERAGE keyword'
          MESSAG(3) = 'Problem encountered in the Human keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
      END IF
!
      IF( NUMCAR .GT. 0 ) THEN
        IF( AVGTIME_CAR.LT.365.0 .OR. AVGTIME_CAR.GT.36525.0 ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'The averaging time is not properly specified for carcinogenic analytes'
          MESSAG(2) = 'Check the CARCINOG modifier on the AVERAGE keyword'
          MESSAG(3) = 'Problem encountered in the Human keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
      END IF
!
! *** Location-related errors
!
      UPLAND = .FALSE.
      LOCTOT = 0
      DO I = 1, ESD_NUM_LOC
        IF( ESD_LOC(I)%COMP ) THEN
          LOCTOT = LOCTOT + 1
          IF( ESD_LOC(I)%TYPE .EQ. 'AQUATIC' ) THEN
            IERR = IERR + 1
            MESSAG(1) = 'Primary locations must be of type RIPARIAN or UPLAND'
            MESSAG(2) = 'Humans cannot live at aquatic locations'
            MESSAG(3) = 'Problem with location ' // ESD_LOC(I)%ID
            CALL PRTERR( IERR, CALLER, 3 )
          END IF
          IF( ESD_LOC(I)%TYPE .EQ. 'UPLAND' ) UPLAND = .TRUE.
        END IF
      END DO
      IF( LOCTOT .EQ. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'At least one location must be requested (location)'
        MESSAG(2) = 'Check if the LOCATION keyword exists'
        MESSAG(3) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
      IF( LOCTOT .NE. HUM_NUM_LOC ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The number of activated HUMAN locations is ill-defined'
        MESSAG(2) = ' '
        WRITE(MESSAG(2)(1:),'(I0)') HUM_NUM_LOC
        MESSAG(2)(8:) = ': Number of HUMAN location cards that were entered'
        MESSAG(3) = ' '
        WRITE(MESSAG(3)(1:),'(I0)') LOCTOT
        MESSAG(3)(8:) = ': Number of activated HUMAN locations from the ESD keyword file'
        MESSAG(4) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 4 )
      END IF
!
! *** Time-related errors
!
      NUMTIM = 0
      DO I = 1, ESD_NUM_TIM
        IF( ESD_TIM(I)%COMP ) NUMTIM = NUMTIM + 1
      END DO
      IF( NUMTIM .EQ. 0 ) THEN
        IERR = IERR + 1
        MESSAG(1) = 'At least one time must be requested (location)'
        MESSAG(2) = 'Check the if the TIMES keyword exists'
        MESSAG(3) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
! *** Ending message if errors encountered
!
      IF( IERR .GT. 0 ) THEN
        MESSAG(1) = 'Ending consistency scan of inputs with III errors.'
        WRITE(MESSAG(1)(40:42),'(I3)') IERR
        CALL PRTERR( 0, CALLER, 1 )
      END IF
!
! *** Check that a soil type is defined (only needed for upland locations)
!
      IF( UPLAND ) THEN
        IF( SOIL_IDX.LT.1 .OR. SOIL_IDX.GT.3 ) THEN
          IERR = IERR + 1
          MESSAG(1) = 'The soil type index has an invalid value (for upland locations)'
          MESSAG(2) = 'Enter a FOODSOIL keyword with one of the modifiers SODR, SOGW, or SOSW'
          MESSAG(3) = 'Problem encountered in the Human keyword file'
          CALL PRTERR( IERR, CALLER, 3 )
        END IF
      END IF
!
! *** Check that a valid shower water source is defined
!
      IF( .NOT.(SHOWER_SOURCE.EQ.'GWAT' .OR. SHOWER_SOURCE.EQ.'SWAT' .OR. SHOWER_SOURCE.EQ.'SEEP'.OR. SHOWER_SOURCE.EQ.'NONE')) THEN
        IERR = IERR + 1
        MESSAG(1) = 'The shower water source is invalid'
        MESSAG(2) = 'Enter a SHOWER keyword with one of the modifiers GROUND, SURFACE, SEEP or NONE'
        MESSAG(3) = 'Problem encountered in the Human keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE CHECK_IDEN( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the information from the keyword cards
!!    looking for problem definition problems.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Oct 2000 : Version 1.0
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
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'CHECK_IDEN'
!
!---- Executable code -------------------------------------------------------------
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
      IF( NREAL .LT. 1 ) THEN
        IERR = 3
        MESSAG(1) = 'Number of realizations requested is less than 1 (Keyword REALIZAT)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
      IF( HUM_NUM_ANA .LT. 1 ) THEN
        IERR = 4
        MESSAG(1) = 'Number of requested analytes is less than 1 (Keyword ANALYTE)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
      IF( HUM_NUM_LOC .LT. 1 ) THEN
        IERR = 5
        MESSAG(1) = 'Number of requested locations is less than 1 (Keyword LOCATION)'
        CALL PRTERR( IERR, CALLER, 1 )
      END IF
!
      IF( HUM_NUM_TIM .LT. 1 ) THEN
        IERR = 6
        MESSAG(1) = 'Number of requested times is less than 1 (Keyword TIMES)'
        CALL PRTERR( IERR, CALLER, 1 )
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
!---- Executable code -------------------------------------------------------------
!
      WRITE(*,*) ' '
      WRITE(*,*) 'A single (optional) command line argument is allowed.  Enter'
      WRITE(*,*) '  '//TRIM(PRGNAM)//'            : To be prompted for the control keyword file name'
      WRITE(*,*) '  '//TRIM(PRGNAM)//' file_name  : To define the control keyword file name'
      WRITE(*,*) '  '//TRIM(PRGNAM)//' -HELP      : To get this help message (but not execute anything)'
!
      RETURN
      END SUBROUTINE

      SUBROUTINE CON_DATA( ITIM, ILOC, IERR )
!!******************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine reads the concentration data from their respective data files for
!!    one time, one location, all selected analytes, and all media.
!!
!!  Notes:
!!
!!    1. The record number for a particular media applies to all the analytes, so only
!!       needs computing once
!!
!!    2. If there are no concentration data in the ECDA file, set the concentrations to
!!       zero for that particular media
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Apr 2000 : Version 1.0
!!    Paul W. Eslinger : 25 Jun 2001 : Version 1.0 : Add error trap negative concentrations
!!    Paul W. Eslinger :  7 Aug 2001 : Version 1.0 : Upgrade error trap
!!    Paul W. Eslinger :  4 Sep 2002 : Add optional concentration output to the details file
!!                                     and upgrade output formats with years
!!    Paul W. Eslinger : 24 Sep 2003 : Add option on read of soil or sediment concentrations
!!    Paul W. Eslinger : 28 Feb 2006 : Add output statistics logic
!!    Paul W. Eslinger : 14 Sep 2006 : Change GET_INDEX call list
!!
!!******************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Control_Mod
      USE Concen_Mod
      USE Debug_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Files_Mod
      USE ECDA_Mod
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
!
      CHARACTER(LEN=8)  :: CALLER = 'CON_DATA'
!
      CHARACTER(LEN=80) :: CTMP ! Local character variable
      INTEGER :: LEN_CTMP       ! Variable length of CTMP
!
      INTEGER :: IANA  ! Analyte looping index
      INTEGER :: IREL  ! Realization looping index
!
      INTEGER :: IDXGWAT ! Groundwater concentration data record number
      INTEGER :: IDXSEEP ! Seep water concentration data record number
      INTEGER :: IDXSWAT ! Surface water concentration data record number
      INTEGER :: IDXSEDI ! Sediment concentration data record number
      INTEGER :: IDXSORP ! Riparian zone concentration data record number
      INTEGER :: IDXSODR ! Non-irrigated upland soil concentration data record number
      INTEGER :: IDXSOGW ! Groundwater irrigated upland soil concentration data record number
      INTEGER :: IDXSOSW ! Surface water irrigated upland soil concentration data record number
      INTEGER :: IDXAIRC ! Air concentration data record number
!
      INTEGER :: TIME            ! Calendar year from file
      CHARACTER(LEN=6) :: LOC_ID ! Location ID from file
      CHARACTER(LEN=4) :: MED_ID ! Media ID from file
      INTEGER :: IPLACE          ! Place index for GET_INDEX
!
      REAL :: SMALL = 0.5 ! Small concentration value (pCi/L pCi/kg ug/kg or ug/L)
!
      REAL :: XMIN ! Statistics - minimum
      REAL :: XV01 ! Statistics -  1st percentile
      REAL :: XV05 ! Statistics -  5th percentile
      REAL :: XV10 ! Statistics - 10th percentile
      REAL :: XV25 ! Statistics - 25th percentile
      REAL :: XMED ! Statistics - median
      REAL :: XV75 ! Statistics - 75th percentile
      REAL :: XV90 ! Statistics - 90th percentile
      REAL :: XV95 ! Statistics - 95th percentile
      REAL :: XV99 ! Statistics - 99th percentile
      REAL :: XMAX ! Statistics - maximum
      REAL :: XAVG ! Statistics - average
      REAL :: XSTD ! Statistics - standard deviation
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
      IF( BGCONC ) THEN
!
        WRITE(IRPT,1000) 'Entering ' // CALLER
 1000   FORMAT(/A)
!
!        CTMP = 'Location index XXXXX : '//ESD_LOC(ILOC)%ID
!        WRITE(CTMP(16:20),'(I5)') ILOC
        CTMP = 'Location index '
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ILOC
        CTMP = TRIM(CTMP) // ' : '//ESD_LOC(ILOC)%ID
        WRITE(IRPT,1010) TRIM(CTMP)
 1010   FORMAT(A)
!
        CTMP = 'Time index '
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ITIM
        CTMP = TRIM(CTMP) // ' : Calendar year'
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ESD_TIM(ITIM)%TIME
        WRITE(IRPT,1010) TRIM(CTMP)
!
        WRITE(IRPT,1020) 'NREAL (Number of realizations)', NREAL
 1020   FORMAT(A,' : ',I0)
!
      END IF
!
! *** Error check on the location index
!
      IF( ILOC.LT.1 .OR. ILOC.GT.ESD_NUM_LOC ) THEN
        IERR = 1
        MESSAG(1) = 'Bad location index for reading concentration data'
        MESSAG(2) = 'Valid range is 1 to '
        WRITE(MESSAG(2)(22:),*) ESD_NUM_LOC
        MESSAG(3) = 'Value entered was '
        WRITE(MESSAG(3)(20:),*) ILOC
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Error check on the time slice
!
      IF( ITIM.LT.1 .OR. ITIM.GT.ESD_NUM_TIM ) THEN
        IERR = 2
        MESSAG(1) = 'Bad time index for reading concentration data'
        MESSAG(2) = 'Valid range is 1 to '
        WRITE(MESSAG(2)(22:),*) ESD_NUM_TIM
        MESSAG(3) = 'Value entered was '
        WRITE(MESSAG(3)(20:),*) ITIM
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Get the concentration index for groundwater
!
      CALL GET_INDEX( ITIM, ILOC, IGWAT, IDXGWAT, IPLACE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Invalid ECDA index for groundwater'
        MESSAG(2) = 'Check whether groundwater was calculated'
        MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(4) = 'Year = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Get the concentration index for surface water
!
      CALL GET_INDEX( ITIM, ILOC, ISWAT, IDXSWAT, IPLACE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Invalid ECDA index for surface water'
        MESSAG(2) = 'Check whether surface water was calculated'
        MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(4) = 'Year = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Get the concentration index for seep water
!
      CALL GET_INDEX( ITIM, ILOC, ISEEP, IDXSEEP, IPLACE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Invalid ECDA index for seep water'
        MESSAG(2) = 'Check whether seep water was calculated'
        MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(4) = 'Year = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Get the concentration index for sediment
!
      CALL GET_INDEX( ITIM, ILOC, ISEDI, IDXSEDI, IPLACE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) = 'Invalid ECDA index for sediment'
        MESSAG(2) = 'Check whether sediment was calculated'
        MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(4) = 'Year = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Get the concentration index for riparian zone soil
!
      CALL GET_INDEX( ITIM, ILOC, ISORP, IDXSORP, IPLACE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Invalid ECDA index for riparian zone soil'
        MESSAG(2) = 'Check whether riparian soil was calculated'
        MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(4) = 'Year = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Get the concentration index for non-irrigated upland soil
!
      CALL GET_INDEX( ITIM, ILOC, ISODR, IDXSODR, IPLACE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Invalid ECDA index for non-irrigated upland soil'
        MESSAG(2) = 'Check whether non-irrigated upland soil was calculated'
        MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(4) = 'Year = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Get the concentration index for groundwater irrigated upland soil
!
      CALL GET_INDEX( ITIM, ILOC, ISOGW, IDXSOGW, IPLACE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 10
        MESSAG(1) = 'Invalid ECDA index for groundwater irrigated upland soil'
        MESSAG(2) = 'Check whether groundwater irrigated upland soil was calculated'
        MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(4) = 'Year = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Get the concentration index for surface water irrigated upland soil
!
      CALL GET_INDEX( ITIM, ILOC, ISOSW, IDXSOSW, IPLACE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 11
        MESSAG(1) = 'Invalid ECDA index for surface water irrigated upland soil'
        MESSAG(2) = 'Check whether surface water irrigated upland soil was calculated'
        MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(4) = 'Year = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Get the concentration index for air concentration
!
      CALL GET_INDEX( ITIM, ILOC, IAIRC, IDXAIRC, IPLACE, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 12
        MESSAG(1) = 'Invalid ECDA index for air concentration'
        MESSAG(2) = 'Check whether air concentration was calculated'
        MESSAG(3) = 'Location = '//ESD_LOC(ILOC)%ID
        MESSAG(4) = 'Year = '
        WRITE(MESSAG(4)(8:),'(I0)') ESD_TIM(ITIM)%TIME
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Loop over all analytes and read data for the ones requested
!
      DO IANA = 1, ESD_NUM_ANA
!
        IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE ! Skip analytes not requested
!
!       Groundwater concentration data
!
        IF( IDXGWAT .GT. 0 ) THEN
          CALL ECDA_READ( IDXGWAT, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
          IF( IERR .EQ. 0 ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = CVEC(IREL) * CONVERT_UNITS(IANA,IGWAT)
              GWAT(IANA,IREL) = CVEC(IREL)
              IF( GWAT(IANA,IREL) .LT. 0.0 ) THEN
                IF( ABS(GWAT(IANA,IREL)) .GT. SMALL ) THEN
                  IERR = 13
                  MESSAG(1) = 'Negative ground water concentration encountered'
                  MESSAG(2) = 'Location     = '//ESD_LOC(ILOC)%ID // '  Analyte     = '//ESD_ANA(IANA)%ID
                  MESSAG(3) = 'Year         = '
                  WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
                  MESSAG(4) = 'Realization  = '
                  WRITE(MESSAG(4)(15:),'(I0)') IREL
                  MESSAG(5) = 'Scaled Value = '
                  WRITE(MESSAG(5)(15:),'(1P,E12.5)') GWAT(IANA,IREL)
                  CALL PRTERR( IERR, CALLER, 5 )
                  RETURN
                ELSE
                  GWAT(IANA,IREL) = 0.0
                END IF
              END IF
            END DO
            IF( DET_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
              CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
              'CONMEDIA', 'GWAT', CONVERT_LABELS(IANA,IGWAT,2), CVEC, NREAL)
!
            IF( STA_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
              CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error detected for variable GWAT'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONMEDIA', &
                'GWAT', CONVERT_LABELS(IANA,IGWAT,2), XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
            END IF
          ELSE
              IERR = 14
              MESSAG(1) = 'Error reading groundwater concentration data'
              MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
              MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            DO IREL = 1, NREAL
              GWAT(IANA,IREL) = 0.0
            END DO
          END IF
!
!       Surface water concentration data
!
        IF( IDXSWAT .GT. 0 ) THEN
          CALL ECDA_READ( IDXSWAT, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
          IF( IERR .EQ. 0 ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = CVEC(IREL) * CONVERT_UNITS(IANA,ISWAT)
              SWAT(IANA,IREL) = CVEC(IREL)
              IF( SWAT(IANA,IREL) .LT. 0.0 ) THEN
                IF( ABS(SWAT(IANA,IREL)) .GT. SMALL ) THEN
                  IERR = 15
                  MESSAG(1) = 'Negative surface water concentration encountered'
                  MESSAG(2) = 'Location     = '//ESD_LOC(ILOC)%ID // '  Analyte     = '//ESD_ANA(IANA)%ID
                  MESSAG(3) = 'Year         = '
                  WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
                  MESSAG(4) = 'Realization  = '
                  WRITE(MESSAG(4)(15:),'(I0)') IREL
                  MESSAG(5) = 'Scaled Value = '
                  WRITE(MESSAG(5)(15:),'(1P,E12.5)') SWAT(IANA,IREL)
                  CALL PRTERR( IERR, CALLER, 5 )
                  RETURN
                ELSE
                  SWAT(IANA,IREL) = 0.0
                END IF
              END IF
            END DO
            IF( DET_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
              CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
              'CONMEDIA', 'SWAT', CONVERT_LABELS(IANA,ISWAT,2), CVEC, NREAL)
!
            IF( STA_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
              CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error detected for variable SWAT'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONMEDIA', &
                'SWAT', CONVERT_LABELS(IANA,ISWAT,2), XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
            END IF
          ELSE
            IERR = 16
            MESSAG(1) = 'Error reading surface water concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        ELSE
          DO IREL = 1, NREAL
            SWAT(IANA,IREL) = 0.0
          END DO
        END IF
!
!       Seep water concentration data
!
        IF( IDXSEEP .GT. 0 ) THEN
          CALL ECDA_READ( IDXSEEP, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
          IF( IERR .EQ. 0 ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = CVEC(IREL) * CONVERT_UNITS(IANA,ISEEP)
              SEEP(IANA,IREL) = CVEC(IREL)
              IF( SEEP(IANA,IREL) .LT. 0.0 ) THEN
                IF( ABS(SEEP(IANA,IREL)) .GT. SMALL ) THEN
                  IERR = 17
                  MESSAG(1) = 'Negative seep water concentration encountered'
                  MESSAG(2) = 'Location     = '//ESD_LOC(ILOC)%ID // '  Analyte     = '//ESD_ANA(IANA)%ID
                  MESSAG(3) = 'Year         = '
                  WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
                  MESSAG(4) = 'Realization  = '
                  WRITE(MESSAG(4)(15:),'(I0)') IREL
                  MESSAG(5) = 'Scaled Value = '
                  WRITE(MESSAG(5)(15:),'(1P,E12.5)') SEEP(IANA,IREL)
                  CALL PRTERR( IERR, CALLER, 5 )
                  RETURN
                ELSE
                  SEEP(IANA,IREL) = 0.0
                END IF
              END IF
            END DO
            IF( DET_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
              CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
              'CONMEDIA', 'SEEP', CONVERT_LABELS(IANA,ISEEP,2), CVEC, NREAL)
!
            IF( STA_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
              CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error detected for variable SEEP'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONMEDIA', &
                'SEEP', CONVERT_LABELS(IANA,ISEEP,2), XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
            END IF
          ELSE
            IERR = 18
            MESSAG(1) = 'Error reading seep water concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        ELSE
          DO IREL = 1, NREAL
            SEEP(IANA,IREL) = 0.0
          END DO
        END IF
!
!       Sediment concentration data
!
        IF( IDXSEDI.GT.0 .AND. USE_SEDI ) THEN
          CALL ECDA_READ( IDXSEDI, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
          IF( IERR .EQ. 0 ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = CVEC(IREL) * CONVERT_UNITS(IANA,ISEDI)
              SEDI(IANA,IREL) = CVEC(IREL)
              IF( SEDI(IANA,IREL) .LT. 0.0 ) THEN
                IF( ABS(SEDI(IANA,IREL)) .GT. SMALL ) THEN
                  IERR = 19
                  MESSAG(1) = 'Negative sediment concentration encountered'
                  MESSAG(2) = 'Location     = '//ESD_LOC(ILOC)%ID // '  Analyte     = '//ESD_ANA(IANA)%ID
                  MESSAG(3) = 'Year         = '
                  WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
                  MESSAG(4) = 'Realization  = '
                  WRITE(MESSAG(4)(15:),'(I0)') IREL
                  MESSAG(5) = 'Scaled Value = '
                  WRITE(MESSAG(5)(15:),'(1P,E12.5)') SEDI(IANA,IREL)
                  CALL PRTERR( IERR, CALLER, 5 )
                  RETURN
                ELSE
                  SEDI(IANA,IREL) = 0.0
                END IF
              END IF
            END DO
            IF( DET_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
              CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
              'CONMEDIA', 'SEDI', CONVERT_LABELS(IANA,ISEDI,2), CVEC, NREAL)
!
            IF( STA_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
              CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error detected for variable SEDI'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONMEDIA', &
                'SEDI', CONVERT_LABELS(IANA,ISEDI,2), XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
            END IF
          ELSE
            IERR = 20
            MESSAG(1) = 'Error reading sediment concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        ELSE
          DO IREL = 1, NREAL
            SEDI(IANA,IREL) = 0.0
          END DO
        END IF
!
!       Riparian zone soil concentration data
!
        IF( IDXSORP.GT.0 .AND. USE_SOIL ) THEN
          CALL ECDA_READ( IDXSORP, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
          IF( IERR .EQ. 0 ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = CVEC(IREL) * CONVERT_UNITS(IANA,ISORP)
              SORP(IANA,IREL) = CVEC(IREL)
              IF( SORP(IANA,IREL) .LT. 0.0 ) THEN
                IF( ABS(SORP(IANA,IREL)) .GT. SMALL ) THEN
                  IERR = 21
                  MESSAG(1) = 'Negative Soil concentration encountered'
                  MESSAG(2) = 'Location     = '//ESD_LOC(ILOC)%ID // '  Analyte     = '//ESD_ANA(IANA)%ID
                  MESSAG(3) = 'Year         = '
                  WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
                  MESSAG(4) = 'Realization  = '
                  WRITE(MESSAG(4)(15:),'(I0)') IREL
                  MESSAG(5) = 'Scaled Value = '
                  WRITE(MESSAG(5)(15:),'(1P,E12.5)') SORP(IANA,IREL)
                  CALL PRTERR( IERR, CALLER, 5 )
                  RETURN
                ELSE
                  SORP(IANA,IREL) = 0.0
                END IF
              END IF
            END DO
            IF( DET_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
              CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
              'CONMEDIA', 'SORP', CONVERT_LABELS(IANA,ISORP,2), CVEC, NREAL)
!
            IF( STA_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
              CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error detected for variable SORP'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONMEDIA', &
                'SORP', CONVERT_LABELS(IANA,ISORP,2), XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
            END IF
          ELSE
            IERR = 22
            MESSAG(1) = 'Error reading soil concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        ELSE
          DO IREL = 1, NREAL
            SORP(IANA,IREL) = 0.0
          END DO
        END IF
!
!       Non-irrigated upland soil concentration data
!
        IF( IDXSODR.GT.0 .AND. USE_SOIL ) THEN
          CALL ECDA_READ( IDXSODR, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
          IF( IERR .EQ. 0 ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = CVEC(IREL) * CONVERT_UNITS(IANA,ISODR)
              SODR(IANA,IREL) = CVEC(IREL)
              IF( SODR(IANA,IREL) .LT. 0.0 ) THEN
                IF( ABS(SODR(IANA,IREL)) .GT. SMALL ) THEN
                  IERR = 23
                  MESSAG(1) = 'Negative Soil concentration encountered'
                  MESSAG(2) = 'Location     = '//ESD_LOC(ILOC)%ID // '  Analyte     = '//ESD_ANA(IANA)%ID
                  MESSAG(3) = 'Year         = '
                  WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
                  MESSAG(4) = 'Realization  = '
                  WRITE(MESSAG(4)(15:),'(I0)') IREL
                  MESSAG(5) = 'Scaled Value = '
                  WRITE(MESSAG(5)(15:),'(1P,E12.5)') SODR(IANA,IREL)
                  CALL PRTERR( IERR, CALLER, 5 )
                  RETURN
                ELSE
                  SODR(IANA,IREL) = 0.0
                END IF
              END IF
            END DO
            IF( DET_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
              CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
              'CONMEDIA', 'SODR', CONVERT_LABELS(IANA,ISODR,2), CVEC, NREAL)
!
            IF( STA_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
              CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error detected for variable SODR'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONMEDIA', &
                'SODR', CONVERT_LABELS(IANA,ISODR,2), XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
            END IF
          ELSE
            IERR = 24
            MESSAG(1) = 'Error reading soil concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        ELSE
          DO IREL = 1, NREAL
            SODR(IANA,IREL) = 0.0
          END DO
        END IF
!
!        Groundwater irrigated upland soil concentration data
!
        IF( IDXSOGW.GT.0 .AND. USE_SOIL ) THEN
          CALL ECDA_READ( IDXSOGW, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
          IF( IERR .EQ. 0 ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = CVEC(IREL) * CONVERT_UNITS(IANA,ISOGW)
              SOGW(IANA,IREL) = CVEC(IREL)
              IF( SOGW(IANA,IREL) .LT. 0.0 ) THEN
                IF( ABS(SOGW(IANA,IREL)) .GT. SMALL ) THEN
                  IERR = 25
                  MESSAG(1) = 'Negative Soil concentration encountered'
                  MESSAG(2) = 'Location     = '//ESD_LOC(ILOC)%ID // '  Analyte     = '//ESD_ANA(IANA)%ID
                  MESSAG(3) = 'Year         = '
                  WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
                  MESSAG(4) = 'Realization  = '
                  WRITE(MESSAG(4)(15:),'(I0)') IREL
                  MESSAG(5) = 'Scaled Value = '
                  WRITE(MESSAG(5)(15:),'(1P,E12.5)') SOGW(IANA,IREL)
                  CALL PRTERR( IERR, CALLER, 5 )
                  RETURN
                ELSE
                  SOGW(IANA,IREL) = 0.0
                END IF
              END IF
            END DO
            IF( DET_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
              CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
              'CONMEDIA', 'SOGW', CONVERT_LABELS(IANA,ISOGW,2), CVEC, NREAL)
!
            IF( STA_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
              CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error detected for variable SOGW'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONMEDIA', &
                'SOGW', CONVERT_LABELS(IANA,ISOGW,2), XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
            END IF
          ELSE
            IERR = 26
            MESSAG(1) = 'Error reading soil concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        ELSE
          DO IREL = 1, NREAL
            SOGW(IANA,IREL) = 0.0
          END DO
        END IF
!
!       Surface water irrigated upland soil concentration data
!
        IF( IDXSOSW.GT.0 .AND. USE_SOIL ) THEN
          CALL ECDA_READ( IDXSOSW, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
          IF( IERR .EQ. 0 ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = CVEC(IREL) * CONVERT_UNITS(IANA,ISOSW)
              SOSW(IANA,IREL) = CVEC(IREL)
              IF( SOSW(IANA,IREL) .LT. 0.0 ) THEN
                IF( ABS(SOSW(IANA,IREL)) .GT. SMALL ) THEN
                  IERR = 27
                  MESSAG(1) = 'Negative soil concentration encountered'
                  MESSAG(2) = 'Location     = '//ESD_LOC(ILOC)%ID // '  Analyte     = '//ESD_ANA(IANA)%ID
                  MESSAG(3) = 'Year         = '
                  WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
                  MESSAG(4) = 'Realization  = '
                  WRITE(MESSAG(4)(15:),'(I0)') IREL
                  MESSAG(5) = 'Scaled Value = '
                  WRITE(MESSAG(5)(15:),'(1P,E12.5)') SOSW(IANA,IREL)
                  CALL PRTERR( IERR, CALLER, 5 )
                  RETURN
                ELSE
                  SOSW(IANA,IREL) = 0.0
                END IF
              END IF
            END DO
            IF( DET_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
              CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
              'CONMEDIA', 'SOSW', CONVERT_LABELS(IANA,ISOSW,2), CVEC, NREAL)
!
            IF( STA_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
              CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error detected for variable SOSW'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONMEDIA', &
                'SOSW', CONVERT_LABELS(IANA,ISOSW,2), XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
            END IF
          ELSE
            IERR = 28
            MESSAG(1) = 'Error reading soil concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        ELSE
          DO IREL = 1, NREAL
            SOSW(IANA,IREL) = 0.0
          END DO
        END IF
!
!       Air concentration data
!
        IF( IDXAIRC .GT. 0 ) THEN
          CALL ECDA_READ( IDXAIRC, TIME, LOC_ID, MED_ID, CVEC, NREAL, ICON(IANA), IERR )
          IF( IERR .EQ. 0 ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = CVEC(IREL) * CONVERT_UNITS(IANA,IAIRC)
              AIRC(IANA,IREL) = CVEC(IREL)
              IF( AIRC(IANA,IREL) .LT. 0.0 ) THEN
                IF( ABS(AIRC(IANA,IREL)) .GT. SMALL ) THEN
                  IERR = 29
                  MESSAG(1) = 'Negative air concentration encountered'
                  MESSAG(2) = 'Location     = '//ESD_LOC(ILOC)%ID // '  Analyte     = '//ESD_ANA(IANA)%ID
                  MESSAG(3) = 'Year         = '
                  WRITE(MESSAG(3)(15:),'(I0)') ESD_TIM(ITIM)%TIME
                  MESSAG(4) = 'Realization  = '
                  WRITE(MESSAG(4)(15:),'(I0)') IREL
                  MESSAG(5) = 'Scaled Value = '
                  WRITE(MESSAG(5)(15:),'(1P,E12.5)') AIRC(IANA,IREL)
                  CALL PRTERR( IERR, CALLER, 5 )
                  RETURN
                ELSE
                  AIRC(IANA,IREL) = 0.0
                END IF
              END IF
            END DO
            IF( DET_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
              CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
              'CONMEDIA', 'AIRC', CONVERT_LABELS(IANA,IAIRC,2), CVEC, NREAL)
!
            IF( STA_CONC .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
              CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
              IF( IERR .NE. 0 ) THEN
                IERR = 999
                MESSAG(1) = 'Error detected for variable AIRC'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              END IF
              CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONMEDIA', &
                'AIRC', CONVERT_LABELS(IANA,IAIRC,2), XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
            END IF
          ELSE
            IERR = 30
            MESSAG(1) = 'Error reading air concentration data'
            MESSAG(2) = 'Location = '//ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Analyte =  '//ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
        ELSE
          DO IREL = 1, NREAL
            AIRC(IANA,IREL) = 0.0
          END DO
        END IF
!
! ***   Debug write on concentration data
!
        IF( BGCONC ) THEN
          WRITE(IRPT,1010) ' '
          WRITE(IRPT,1010) 'Concentrations by media after units conversion (soil and sediment output only if used)'
          WRITE(IRPT,1030) IANA,ESD_ANA(IANA)%ID,ILOC,ESD_LOC(ILOC)%ID,ITIM,ESD_TIM(ITIM)%TIME,'GWAT',&
            IDXGWAT,(GWAT(IANA,IREL),IREL=1,NREAL)
          WRITE(IRPT,1030) IANA,ESD_ANA(IANA)%ID,ILOC,ESD_LOC(ILOC)%ID,ITIM,ESD_TIM(ITIM)%TIME,'SEEP',&
            IDXSEEP,(SEEP(IANA,IREL),IREL=1,NREAL)
          WRITE(IRPT,1030) IANA,ESD_ANA(IANA)%ID,ILOC,ESD_LOC(ILOC)%ID,ITIM,ESD_TIM(ITIM)%TIME,'SWAT',&
            IDXSWAT,(SWAT(IANA,IREL),IREL=1,NREAL)
          IF( USE_SEDI ) WRITE(IRPT,1030) IANA,ESD_ANA(IANA)%ID,ILOC,ESD_LOC(ILOC)%ID,ITIM,ESD_TIM(ITIM)%TIME,'SEDI',&
            IDXSEDI,(SEDI(IANA,IREL),IREL=1,NREAL)
          IF( USE_SOIL ) THEN
            WRITE(IRPT,1030) IANA,ESD_ANA(IANA)%ID,ILOC,ESD_LOC(ILOC)%ID,ITIM,ESD_TIM(ITIM)%TIME,'SORP',&
              IDXSORP,(SORP(IANA,IREL),IREL=1,NREAL)
            WRITE(IRPT,1030) IANA,ESD_ANA(IANA)%ID,ILOC,ESD_LOC(ILOC)%ID,ITIM,ESD_TIM(ITIM)%TIME,'SODR',&
              IDXSODR,(SODR(IANA,IREL),IREL=1,NREAL)
            WRITE(IRPT,1030) IANA,ESD_ANA(IANA)%ID,ILOC,ESD_LOC(ILOC)%ID,ITIM,ESD_TIM(ITIM)%TIME,'SOGW',&
              IDXSOGW,(SOGW(IANA,IREL),IREL=1,NREAL)
            WRITE(IRPT,1030) IANA,ESD_ANA(IANA)%ID,ILOC,ESD_LOC(ILOC)%ID,ITIM,ESD_TIM(ITIM)%TIME,'SOSW',&
              IDXSOSW,(SOSW(IANA,IREL),IREL=1,NREAL)
          END IF
          WRITE(IRPT,1030) IANA,ESD_ANA(IANA)%ID,ILOC,ESD_LOC(ILOC)%ID,ITIM,ESD_TIM(ITIM)%TIME,'AIRC',&
            IDXAIRC,(AIRC(IANA,IREL),IREL=1,NREAL)
 1030     FORMAT(3X,I0,1X,A, 1X,I0,1X,A, 1X,I0,1X,I0, 1X,A,1X,I0, 1P,100(1X,E12.5))
        END IF
!
      END DO
!
      RETURN
      END

      SUBROUTINE EAT_FOODS( IERR )
!!******************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine scans the information from the keyword cards and determines which
!!    foods are used for this scenario.
!!
!!    This subroutine also applies the food source logic for the shortcut definition for
!!    BIRD, FISH, or MEAT concentrations.
!!
!!  History:
!!
!!    Paul W. Eslinger : 18 Feb 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 23 OCt 2003 : Add food source logic
!!    Paul W. Eslinger : 31 Mar 2005 : Change logic on use flags for foods
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Terminal error encountered
!!
!!******************************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Stats_Mod
      USE ESD_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN= 9) :: CALLER = 'EAT_FOODS'
      CHARACTER(LEN=16) :: VNAME    ! Local ID for accessing stochastic info
      INTEGER :: VIDX ! Local index for a stochastic variable
      INTEGER :: ILOC ! Local index for a location
      INTEGER :: INDX ! Temporary index for
!
      CHARACTER(LEN= 8) :: TMP_TYPE ! Temporary location type
      INTEGER :: TMP_ILOC           ! Temporary location index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check if root vegetables are being consumed -----------------------------
!
      IF( USE_ROOTVEG ) THEN
!
        VIDX = 0
        VNAME = 'IRROOTVEG'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 1
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_ROOTVEG = .FALSE.
        END IF
      END IF
!
! *** Check if leafy vegetables are being consumed ----------------------------
!
      IF( USE_LEAFVEG ) THEN
!
        VIDX = 0
        VNAME = 'IRLEAFVEG'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 2
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_LEAFVEG = .FALSE.
        END IF
!
      END IF
!
! *** Check if fish is being consumed -----------------------------------------
!
      IF( USE_FISH ) THEN
!
        VIDX = 0
        VNAME = 'IRFISH'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 3
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_FISH = .FALSE.
        END IF
!
      END IF
!
!     Food source logic for FISH concentrations (same source for all locations)
!     Error check on the source locations defined on the LOCATION keywords
!
      IF( USE_FISH ) THEN
        IF( SOURCE_FISH ) THEN ! Set all of the source locations
          CALL MATCH_LOC( SOURCE_FISH_ID, INDX )
          IF( INDX .LE. 0 ) THEN
            IERR = 4
            MESSAG(1) = 'Location requested as the source of all FISH concentrations is not in the master list'
            MESSAG(2) = 'Location ID is "'//TRIM(SOURCE_FISH_ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          FORALL(ILOC=1:ESD_NUM_LOC) ESD_LOC(ILOC)%FISH = INDX
        ELSE
          DO ILOC = 1, ESD_NUM_LOC ! Check all of the source locations
            IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
            IF( ESD_LOC(ILOC)%FISH.LT.1 .OR.ESD_LOC(ILOC)%FISH.GT.ESD_NUM_LOC ) THEN
              IERR = 5
              MESSAG(1) = 'Location with ID "'//TRIM(ESD_LOC(ILOC)%ID)//'" does not have a source'
              MESSAG(2) = 'for fish concentrations identified.'
              MESSAG(3) = 'Change the LOCATION keyword or the FOODSOURCE keyword.'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END DO
          IF( IERR .NE. 0 ) RETURN
        END IF
      END IF
!
! *** Check if fish_2 is being consumed -----------------------------------------
!
      IF( USE_FISH_2 ) THEN
!
        VIDX = 0
        VNAME = 'IRFISH_2'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 16
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_FISH_2 = .FALSE.
        END IF
!
      END IF
!
!     Food source logic for FISH_2 concentrations (same source for all locations)
!     Error check on the source locations defined on the LOCATION keywords
!
      IF( USE_FISH_2 ) THEN
        IF( SOURCE_FISH_2 ) THEN ! Set all of the source locations
          CALL MATCH_LOC( SOURCE_FISH_ID_2, INDX )
          IF( INDX .LE. 0 ) THEN
            IERR = 17
            MESSAG(1) = 'Location requested as the source of all FISH_2 concentrations is not in the master list'
            MESSAG(2) = 'Location ID is "'//TRIM(SOURCE_FISH_ID_2)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          FORALL(ILOC=1:ESD_NUM_LOC) ESD_LOC(ILOC)%FISH_2 = INDX
        ELSE
          DO ILOC = 1, ESD_NUM_LOC ! Check all of the source locations
            IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
            IF( ESD_LOC(ILOC)%FISH_2.LT.1 .OR.ESD_LOC(ILOC)%FISH_2.GT.ESD_NUM_LOC ) THEN
              IERR = 18
              MESSAG(1) = 'Location with ID "'//TRIM(ESD_LOC(ILOC)%ID)//'" does not have a source'
              MESSAG(2) = 'for FISH_2 concentrations identified.'
              MESSAG(3) = 'Change the LOCATION keyword or the FOODSOURCE keyword.'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END DO
          IF( IERR .NE. 0 ) RETURN
        END IF
      END IF
!
! *** Check if fish_3 is being consumed -----------------------------------------
!
      IF( USE_FISH_3 ) THEN
!
        VIDX = 0
        VNAME = 'IRFISH_3'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 19
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_FISH_3 = .FALSE.
        END IF
!
      END IF
!
!     Food source logic for FISH_3 concentrations (same source for all locations)
!     Error check on the source locations defined on the LOCATION keywords
!
      IF( USE_FISH_3 ) THEN
        IF( SOURCE_FISH_3 ) THEN ! Set all of the source locations
          CALL MATCH_LOC( SOURCE_FISH_ID_3, INDX )
          IF( INDX .LE. 0 ) THEN
            IERR = 20
            MESSAG(1) = 'Location requested as the source of all FISH_3 concentrations is not in the master list'
            MESSAG(2) = 'Location ID is "'//TRIM(SOURCE_FISH_ID_3)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          FORALL(ILOC=1:ESD_NUM_LOC) ESD_LOC(ILOC)%FISH_3 = INDX
        ELSE
          DO ILOC = 1, ESD_NUM_LOC ! Check all of the source locations
            IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
            IF( ESD_LOC(ILOC)%FISH_3.LT.1 .OR.ESD_LOC(ILOC)%FISH_3.GT.ESD_NUM_LOC ) THEN
              IERR = 21
              MESSAG(1) = 'Location with ID "'//TRIM(ESD_LOC(ILOC)%ID)//'" does not have a source'
              MESSAG(2) = 'for FISH_3 concentrations identified.'
              MESSAG(3) = 'Change the LOCATION keyword or the FOODSOURCE keyword.'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END DO
          IF( IERR .NE. 0 ) RETURN
        END IF
      END IF
!
! *** Check if meat is being consumed -----------------------------------------
!
      IF( USE_MEAT ) THEN
!
        VIDX = 0
        VNAME = 'IRMEAT'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 6
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_MEAT = .FALSE.
        END IF
!
      END IF
!
!     Food source logic for meat concentrations (same source for all lcoations)
!     Error check on the source locations defined on the LOCATION keywords
!
      IF( USE_MEAT ) THEN
        IF( SOURCE_MEAT ) THEN ! Set all of the source locations
          CALL MATCH_LOC( SOURCE_MEAT_ID, INDX )
          IF( INDX .LE. 0 ) THEN
            IERR = 7
            MESSAG(1) = 'Location requested as the source of all MEAT concentrations is not in the master list'
            MESSAG(2) = 'Location ID is "'//TRIM(SOURCE_MEAT_ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          FORALL(ILOC=1:ESD_NUM_LOC) ESD_LOC(ILOC)%MEAT = INDX
        ELSE
          DO ILOC = 1, ESD_NUM_LOC ! Check all of the source locations
            IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
            IF( ESD_LOC(ILOC)%MEAT.LT.1 .OR.ESD_LOC(ILOC)%MEAT.GT.ESD_NUM_LOC ) THEN
              IERR = 8
              MESSAG(1) = 'Location with ID "'//TRIM(ESD_LOC(ILOC)%ID)//'" does not have a source'
              MESSAG(2) = 'for MEAT concentrations identified.'
              MESSAG(3) = 'Change the LOCATION keyword or the FOODSOURCE keyword.'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END DO
          IF( IERR .NE. 0 ) RETURN
        END IF
      END IF
!
! *** Check if birds are being consumed ---------------------------------------
!
      IF( USE_BIRD ) THEN
!
        VIDX = 0
        VNAME = 'IRBIRD'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_BIRD = .FALSE.
        END IF
!
      END IF
!
!     Food source logic for BIRD concentrations (same source for all lcoations)
!     Error check on the source locations defined on the LOCATION keywords
!
      IF( USE_BIRD ) THEN
        IF( SOURCE_BIRD ) THEN ! Set all of the source locations
          CALL MATCH_LOC( SOURCE_BIRD_ID, INDX )
          IF( INDX .LE. 0 ) THEN
            IERR = 10
            MESSAG(1) = 'Location requested as the source of all BIRD concentrations is not in the master list'
            MESSAG(2) = 'Location ID is "'//TRIM(SOURCE_BIRD_ID)//'"'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          FORALL(ILOC=1:ESD_NUM_LOC) ESD_LOC(ILOC)%BIRD = INDX
        ELSE
          DO ILOC = 1, ESD_NUM_LOC ! Check all of the source locations
            IF( .NOT. ESD_LOC(ILOC)%COMP ) CYCLE
            IF( ESD_LOC(ILOC)%BIRD.LT.1 .OR.ESD_LOC(ILOC)%BIRD.GT.ESD_NUM_LOC ) THEN
              IERR = 11
              MESSAG(1) = 'Location with ID "'//TRIM(ESD_LOC(ILOC)%ID)//'" does not have a source'
              MESSAG(2) = 'for BIRD concentrations identified.'
              MESSAG(3) = 'Change the LOCATION keyword or the FOODSOURCE keyword.'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
          END DO
          IF( IERR .NE. 0 ) RETURN
        END IF
      END IF
!
! *** Check if milk is being consumed -----------------------------------------
!
      IF( USE_MILK ) THEN
!
        VIDX = 0
        VNAME = 'IRMILK'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 12
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_MILK = .FALSE.
        END IF
!
      END IF
!
! *** Check if eggs are being consumed ----------------------------------------
!
      IF( USE_EGGS ) THEN
!
        VIDX = 0
        VNAME = 'IREGGS'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 13
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_EGGS = .FALSE.
        END IF
!
      END IF
!
! *** Check if fruit is being consumed ----------------------------------------
!
      IF( USE_FRUIT ) THEN
!
        VIDX = 0
        VNAME = 'IRFRUIT'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 14
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_FRUIT = .FALSE.
        END IF
!
      END IF
!
! *** Check if grain is being consumed ----------------------------------------
!
      IF( USE_GRAIN ) THEN
!
        VIDX = 0
        VNAME = 'IRGRAIN'
        CALL FSINDX( VNAME, VIDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 15
          MESSAG(1) = 'Error detected in lower level routine'
          MESSAG(2) = 'Variable: '//VNAME
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
        END IF
!
        IF( VTYPE(VIDX) .EQ. 1 ) THEN ! Distribution type 1 is a constant
          IF( VPARMS(VIDX,1) .EQ. 0.0 ) USE_GRAIN = .FALSE.
        END IF
!
      END IF
!
! *** Check that all primary locations are of same location type for some food groups
!       This enforces the assumption that many foods must be local to the primary
!       location because the food reading routine only assignes one type per code run.
!
      IF( USE_ROOTVEG .OR. USE_LEAFVEG .OR. USE_MILK .OR. USE_EGGS .OR. USE_FRUIT .OR. USE_GRAIN ) THEN
        TMP_TYPE = ' '
        DO ILOC = 1, ESD_NUM_LOC
          IF( .NOT.ESD_LOC(ILOC)%COMP ) CYCLE
          IF( TMP_TYPE .EQ. ' ' ) THEN
            TMP_TYPE = ESD_LOC(ILOC)%TYPE
            TMP_ILOC = ILOC
            CYCLE
          ELSE
            IF( TMP_TYPE .NE. ESD_LOC(ILOC)%TYPE ) THEN
              IERR = 101
              MESSAG(1) = 'All primary locations must be of the same location type for several food groups.'
              MESSAG(2) = 'The following food groups: ROOTVEG, LEAFVEG, MILK, EGGS, FRUIT, and GRAIN, are'
              MESSAG(3) = 'only defined for local consumption and there is a problem with assigned locations.'
              MESSAG(4) = 'Locations "'// ESD_LOC(TMP_ILOC)%ID //'" and "'// ESD_LOC(ILOC)%ID //'" do not match in types.'
              CALL PRTERR( IERR, CALLER, 4 )
              RETURN
            END IF
          END IF
        END DO
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE ECHO_1( )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles writing most of an echo of the problem definition to the report file.
!!    The definition of stochastic variables is deferred to subroutine STOGEN.
!!
!!  Auxiliary Routines:
!!
!!    LOCIDX, PRTERR
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 30 Jul 2001 : Debug output for KDSOIL stuff
!!    Paul W. Eslinger :  8 Feb 2002 : Upgrade file name outputs
!!    Carmen Arimescu  : 12 Mar 2003 : SAC Rev.1
!!    Paul W. Eslinger : 22 Oct 2003 : Update formats
!!    Paul W. Eslinger : 23 Oct 2003 : Add location information
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!    Paul W. Eslinger : 28 Feb 2006 : SCR-1107 Add shower water options
!!                                     Add food labels for later output
!!    Paul W. Eslinger : 14 Sep 2006 : SCR-1143 Split into two subroutines
!!    Paul W. Eslinger : 30 Oct 2006 : SCR-1148 Fix averaging time variables
!!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0004) Remove SACVIEW header files
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Seeds_Mod
      USE Debug_Mod
      USE Esd_Mod
      USE Stats_Mod, ONLY: BG_STOC_VALU, BG_STOC_DEFN, BG_STOC_STAT
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: ITIM ! Time looping index
!
!---- Executable code -------------------------------------------------------------
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
      WRITE(IRPT,1030) 'File Name for Input Keyword Data', TRIM(FNKEY)
      IF( STA_USE ) WRITE(IRPT,1030) 'File Name for Summary Risk Data', TRIM(FNSUM)
      IF( DET_USE ) WRITE(IRPT,1030) 'File Name for Detailed Risk Data', TRIM(FNDET)
      IF( BG_STOC_VALU  ) WRITE(IRPT,1030) 'File Name for Generated Stochastic Values', TRIM(FNVAL)
 1030 FORMAT(/A/'File: ',A)
!
!     Concentration files for each analyte
!
      DO IANA = 1, ESD_NUM_ANA
        IF( ESD_ANA(IANA)%COMP ) THEN
          WRITE(IRPT,1035) TRIM(ESD_ANA(IANA)%ID), TRIM(FNCON(IANA))
 1035     FORMAT(/'File Name for Media Concentrations for analyte with ID="',A,'"'/'File: ',A)
        END IF
      END DO
!
! *** Debug flags
!
      WRITE(IRPT,1040)
 1040 FORMAT(/'Debug Flag Information')
!
      IF( BGCONC ) THEN
        WRITE(IRPT,1050) 'Yes', 'Concentration Data'
 1050   FORMAT(3X,A3,' : ',A)
      ELSE
        WRITE(IRPT,1050) 'No ', 'Concentration Data'
      END IF
!
      IF( BGRAD ) THEN
        WRITE(IRPT,1050) 'Yes', 'Radiological Risk'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Radiological Risk'
      END IF
!
      IF( BGCAR ) THEN
        WRITE(IRPT,1050) 'Yes', 'Carcinogenic Risk'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Carcinogenic Risk'
      END IF
!
      IF( BGHAZ ) THEN
        WRITE(IRPT,1050) 'Yes', 'Hazard Quotient'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Hazard Quotient'
      END IF
!
      IF( BG_STOC_VALU ) THEN
        WRITE(IRPT,1050) 'Yes', 'Stochastic Calculations'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Stochastic Calculations'
      END IF
!
      IF( BGFOOD ) THEN
        WRITE(IRPT,1050) 'Yes', 'Food concentrations'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Food concentrations'
      END IF
!
! *** Data Output Options
!
      WRITE(IRPT,1250)
 1250 FORMAT(/'Output Data Options Information')
!
      IF( DET_SUMDOSE ) THEN
        WRITE(IRPT,1050) 'Yes', 'Dose details summed over analytes of similar type'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Dose details summed over analytes of similar type'
      END IF
!
      IF( DET_SUMRISK ) THEN
        WRITE(IRPT,1050) 'Yes', 'Risk (or hazard quotient) details summed over analytes of similar type'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Risk (or hazard quotient) details summed over analytes of similar type'
      END IF
!
      IF( STA_SUMDOSE ) THEN
        WRITE(IRPT,1050) 'Yes', 'Dose statistics summed over analytes of similar type'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Dose statistics summed over analytes of similar type'
      END IF
!
      IF( STA_SUMRISK ) THEN
        WRITE(IRPT,1050) 'Yes', 'Risk (or hazard quotient) statistics summed over analytes of similar type'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Risk (or hazard quotient) statistics summed over analytes of similar type'
      END IF
!
      IF( DET_POPDOSE ) THEN
        WRITE(IRPT,1050) 'Yes', 'Combined Population Dose Details'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Combined Population Dose Details'
      END IF
!
      IF( STA_POPDOSE ) THEN
        WRITE(IRPT,1050) 'Yes', 'Combined Population Dose Statistics'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Combined Population Dose Statistics'
      END IF
!
      IF( DET_POPRISK ) THEN
        WRITE(IRPT,1050) 'Yes', 'Combined Population Risk Details'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Combined Population Risk Details'
      END IF
!
      IF( STA_POPRISK ) THEN
        WRITE(IRPT,1050) 'Yes', 'Combined Population Risk Statistics'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Combined Population Risk Statistics'
      END IF
!
      IF( DET_ANADOSE ) THEN
        WRITE(IRPT,1050) 'Yes', 'Analyte Detailed Dose Data'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Analyte Detailed Dose Data'
      END IF
!
      IF( DET_ANARISK ) THEN
        WRITE(IRPT,1050) 'Yes', 'Analyte Detailed Risk Data'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Analyte Detailed Risk Data'
      END IF
!
      IF( STA_ANADOSE ) THEN
        WRITE(IRPT,1050) 'Yes', 'Analyte Dose Statistics'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Analyte Dose Statistics'
      END IF
!
      IF( STA_ANARISK ) THEN
        WRITE(IRPT,1050) 'Yes', 'Analyte Risk Statistics'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Analyte Risk Statistics'
      END IF
!
      IF( DET_PATHDOSE ) THEN
        WRITE(IRPT,1050) 'Yes', 'Exposure Pathway Level Dose Details'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Exposure Pathway Level Dose Details'
      END IF
!
      IF( DET_PATHRISK ) THEN
        WRITE(IRPT,1050) 'Yes', 'Exposure Pathway Level Risk (or Hazard quotient) Details'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Exposure Pathway Level Risk (or Hazard quotient) Details'
      END IF
!
      IF( STA_PATHDOSE ) THEN
        WRITE(IRPT,1050) 'Yes', 'Exposure Pathway Level Dose Statistics'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Exposure Pathway Level Dose Statistics'
      END IF
!
      IF( STA_PATHRISK ) THEN
        WRITE(IRPT,1050) 'Yes', 'Exposure Pathway Level Risk (or Hazard quotient) Statistics'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Exposure Pathway Level Risk (or Hazard quotient) Statistics'
      END IF
!
      IF( DET_CONC ) THEN
        WRITE(IRPT,1050) 'Yes', 'Media Level Concentrations'
      ELSE
        WRITE(IRPT,1050) 'No ', 'Media Level Concentrations'
      END IF
!
! *** Averaging time (days)
!
      IF( NUMHAZ .GT. 0 ) WRITE(IRPT,1240) 'hazardous', AVGTIME_HAZ
      IF( NUMCAR .GT. 0 ) WRITE(IRPT,1240) 'carcinogenic', AVGTIME_CAR
 1240 FORMAT(/'Averaging time for ',A,' analytes'/'   Value = ',F9.2,'   Units are days')
!
! *** Sweat lodge water options
!
      SELECT CASE ( SWEAT_SOURCE )
        CASE( 'SWAT' )
          WRITE(IRPT,1230) 'Sweat lodge option is used', 'Surface water'
 1230     FORMAT(/A,:,/'  Water source is ',A)
        CASE( 'SEEP' )
          WRITE(IRPT,1230) 'Sweat lodge option is used', 'Seep water'
        CASE( 'GWAT' )
          WRITE(IRPT,1230) 'Sweat lodge option is used', 'Ground water'
        CASE DEFAULT
          WRITE(IRPT,1230) 'Sweat lodge option is not used'
      END SELECT
!
! *** Shower water selection options
!
      SELECT CASE ( SHOWER_SOURCE )
        CASE( 'SWAT' )
          WRITE(IRPT,1235) 'Shower water selection is used', 'Surface water'
 1235     FORMAT(/A,:,' : Water source is ',A)
        CASE( 'SEEP' )
          WRITE(IRPT,1235) 'Shower water selection is used', 'Seep water'
        CASE( 'GWAT' )
          WRITE(IRPT,1235) 'Shower water selection is used', 'Ground water'
        CASE DEFAULT
          WRITE(IRPT,1235) 'Shower water is not used'
      END SELECT
!
! *** Analyte names and types
!
      WRITE(IRPT,1100) ESD_NUM_ANA
 1100 FORMAT(/'Analye Information based on ',I0,' possible analytes.')
      DO IANA = 1, ESD_NUM_ANA
        IF( ESD_ANA(IANA)%COMP ) THEN
          WRITE(IRPT,1120) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, ESD_ANA(IANA)%ANATYP_CAR, ESD_ANA(IANA)%ANATYP_HAZ, &
            ESD_ANA(IANA)%ANATYP_RAD, ESD_ANA(IANA)%NAME
 1120     FORMAT(3X,I3,' : ',A,' : ',A,' : Carcinogen ',L1,' : Hazardous ',L1,' : Radioactive ',L1,' : ',A,' : ',A)
        END IF
      END DO
      WRITE(IRPT,1130) HUM_NUM_ANA
 1130 FORMAT('A total of ',I0,' analytes have been requested.')
!
! *** Soil type selection for for upland food
!
      IF (SOIL_ID .EQ. 'NONE') THEN
        WRITE(IRPT,1195)
        WRITE(IRPT,*) 'Soil type   : None'
      END IF
      IF (SOIL_ID .EQ. 'SOGW') THEN
        WRITE(IRPT,1195)
        WRITE(IRPT,*) 'SOGW   : Groundwater irrigation'
      END IF
      IF (SOIL_ID .EQ. 'SOSW') THEN
        WRITE(IRPT,1195)
        WRITE(IRPT,*) 'SOSW   : Surface water irrigation'
      END IF
      IF (SOIL_ID .EQ. 'SODR') THEN
        WRITE(IRPT,1195)
        WRITE(IRPT,*) 'SODR   : No irrigation'
      END IF
 1195 FORMAT(/'Soil type selection for for upland food')
!
! *** Food information
!
      WRITE(IRPT,1150)
 1150 FORMAT(/'FOOD Information')
      IF( USE_MILK ) THEN
        WRITE(IRPT,1140) 'MILK', TRIM(ESD_SPC(MILK_FIDX)%ID), TRIM(ESD_SPC(MILK_FIDX)%NAME)
 1140   FORMAT(3X,A,'    : Species "',A,'"',:,'  "',A,'"')
        LABEL_MILK = 'ID: '//ESD_SPC(MILK_FIDX)%ID//'  Name: '//TRIM(ESD_SPC(MILK_FIDX)%NAME)
      ELSE
        WRITE(IRPT,1175) 'MILK'
 1145   FORMAT(3X,A,' : Not consumed')
 1193   FORMAT(3X,A,'  : Not consumed')
 1185   FORMAT(3X,A,'   : Not consumed')
 1175   FORMAT(3X,A,'    : Not consumed')
        LABEL_MILK = 'Not consumed'
      END IF
!
      IF( USE_BIRD ) THEN
        WRITE(IRPT,1140) 'BIRD', TRIM(ESD_SPC(BIRD_FIDX)%ID), TRIM(ESD_SPC(BIRD_FIDX)%NAME)
        LABEL_BIRD = 'ID: '//ESD_SPC(BIRD_FIDX)%ID//'  Name: '//TRIM(ESD_SPC(BIRD_FIDX)%NAME)
      ELSE
        WRITE(IRPT,1175) 'BIRD'
        LABEL_BIRD = 'Not consumed'
      END IF
!
      IF( USE_GRAIN ) THEN
        WRITE(IRPT,1165) 'GRAIN', TRIM(ESD_SPC(GRAIN_FIDX)%ID), TRIM(ESD_SPC(GRAIN_FIDX)%NAME)
 1165   FORMAT(3X,A,'   : Species "',A,'"',:,'  "',A,'"')
        LABEL_GRAIN = 'ID: '//ESD_SPC(GRAIN_FIDX)%ID//'  Name: '//TRIM(ESD_SPC(GRAIN_FIDX)%NAME)
      ELSE
        WRITE(IRPT,1185) 'GRAIN'
        LABEL_GRAIN = 'Not consumed'
      END IF
!
      IF( USE_FRUIT ) THEN
        WRITE(IRPT,1165) 'FRUIT', TRIM(ESD_SPC(FRUIT_FIDX)%ID), TRIM(ESD_SPC(FRUIT_FIDX)%NAME)
        LABEL_FRUIT = 'ID: '//ESD_SPC(FRUIT_FIDX)%ID//'  Name: '//TRIM(ESD_SPC(FRUIT_FIDX)%NAME)
      ELSE
        WRITE(IRPT,1185) 'FRUIT'
        LABEL_FRUIT = 'Not consumed'
      END IF
!
      IF( USE_EGGS ) THEN
        WRITE(IRPT,1140) 'EGGS', TRIM(ESD_SPC(EGGS_FIDX)%ID), TRIM(ESD_SPC(EGGS_FIDX)%NAME)
        LABEL_EGGS = 'ID: '//ESD_SPC(EGGS_FIDX)%ID//'  Name: '//TRIM(ESD_SPC(EGGS_FIDX)%NAME)
      ELSE
        WRITE(IRPT,1175) 'EGGS'
        LABEL_EGGS = 'Not consumed'
      END IF
!
      IF( USE_FISH ) THEN
        WRITE(IRPT,1140) 'FISH', TRIM(ESD_SPC(FISH_FIDX)%ID), TRIM(ESD_SPC(FISH_FIDX)%NAME)
        LABEL_FISH = 'ID: '//ESD_SPC(FISH_FIDX)%ID//'  Name: '//TRIM(ESD_SPC(FISH_FIDX)%NAME)
      ELSE
        WRITE(IRPT,1175) 'FISH'
        LABEL_FISH = 'Not consumed'
      END IF
!
      IF( USE_FISH_2 ) THEN
        WRITE(IRPT,1166) 'FISH_2', TRIM(ESD_SPC(FISH_FIDX_2)%ID), TRIM(ESD_SPC(FISH_FIDX_2)%NAME)
 1166   FORMAT(3X,A,'  : Species "',A,'"',:,'  "',A,'"')
        LABEL_FISH_2 = 'ID: '//ESD_SPC(FISH_FIDX_2)%ID//'  Name: '//TRIM(ESD_SPC(FISH_FIDX_2)%NAME)
      ELSE
        WRITE(IRPT,1193) 'FISH_2'
        LABEL_FISH_2 = 'Not consumed'
      END IF
!
      IF( USE_FISH_3 ) THEN
        WRITE(IRPT,1166) 'FISH_3', TRIM(ESD_SPC(FISH_FIDX_3)%ID), TRIM(ESD_SPC(FISH_FIDX_3)%NAME)
        LABEL_FISH_3 = 'ID: '//ESD_SPC(FISH_FIDX_3)%ID//'  Name: '//TRIM(ESD_SPC(FISH_FIDX_3)%NAME)
      ELSE
        WRITE(IRPT,1193) 'FISH_3'
        LABEL_FISH_3 = 'Not consumed'
      END IF
!
      IF( USE_MEAT ) THEN
        WRITE(IRPT,1140) 'MEAT', TRIM(ESD_SPC(MEAT_FIDX)%ID), TRIM(ESD_SPC(MEAT_FIDX)%NAME)
        LABEL_MEAT = 'ID: '//ESD_SPC(MEAT_FIDX)%ID//'  Name: '//TRIM(ESD_SPC(MEAT_FIDX)%NAME)
      ELSE
        WRITE(IRPT,1175) 'MEAT'
        LABEL_MEAT = 'Not consumed'
      END IF
!
      IF( USE_LEAFVEG ) THEN
        WRITE(IRPT,1160) 'LEAFVEG', TRIM(ESD_SPC(LEAFVEG_FIDX)%ID), TRIM(ESD_SPC(LEAFVEG_FIDX)%NAME)
        LABEL_LEAFVEG = 'ID: '//ESD_SPC(LEAFVEG_FIDX)%ID//'  Name: '//TRIM(ESD_SPC(LEAFVEG_FIDX)%NAME)
      ELSE
        WRITE(IRPT,1145) 'LEAFVEG'
        LABEL_LEAFVEG = 'Not consumed'
      END IF
!
      IF( USE_ROOTVEG ) THEN
        WRITE(IRPT,1160) 'ROOTVEG', TRIM(ESD_SPC(ROOTVEG_FIDX)%ID), TRIM(ESD_SPC(ROOTVEG_FIDX)%NAME)
 1160   FORMAT(3X,A,' : Species "',A,'"',:,'  "',A,'"')
        LABEL_ROOTVEG = 'ID: '//ESD_SPC(ROOTVEG_FIDX)%ID//'  Name: '//TRIM(ESD_SPC(ROOTVEG_FIDX)%NAME)
      ELSE
        WRITE(IRPT,1145) 'ROOTVEG'
        LABEL_ROOTVEG = 'Not consumed'
      END IF
!
! *** Time slices
!
      WRITE(IRPT,1170) ESD_NUM_TIM
 1170 FORMAT(/'Time Slices Requested: (Index, Calendar Year) based on ',I0,' possible times.')
      DO ITIM = 1, ESD_NUM_TIM
        IF( ESD_TIM(ITIM)%COMP ) THEN
          WRITE(IRPT,1180) ITIM, ESD_TIM(ITIM)%TIME, ' : Included in this scenario'
 1180     FORMAT(3X,I4,' : ',I5,A)
        END IF
      END DO
      WRITE(IRPT,1190) HUM_NUM_TIM
 1190 FORMAT('A total of ',I0,' times have been requested.')
!
      WRITE(IRPT,1320) HUM_NUM_LOC
 1320 FORMAT(/'A total of ',I0,' locations have been requested.')
!
      WRITE(IRPT,1220)
 1220 FORMAT(/24('='),' End of the Problem Definition ',25('='))
!
      RETURN
      END SUBROUTINE

      SUBROUTINE ECHO_2( )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine finishes writing an echo of the problem definition to the report file.
!!
!!  Auxiliary Routines:
!!
!!    LOCIDX, PRTERR
!!
!!  History:
!!
!!    Paul W. Eslinger : 14 Sep 2006 : SCR-1143 Modify location outputs
!!    Paul W. Eslinger : 30 Oct 2006 : SCR-1148 Fix averaging time variables
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Seeds_Mod
      USE Debug_Mod
      USE Esd_Mod
      USE Stats_Mod, ONLY: BG_STOC_VALU, BG_STOC_DEFN, BG_STOC_STAT
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: ILOC ! Location looping index
      CHARACTER(LEN=6) :: CLBIRD, CLMEAT             ! Local character strings for food ID's
      CHARACTER(LEN=6) :: CLFISH, CLFISH_2, CLFISH_3 ! Local character strings for food ID's
      CHARACTER(LEN=6) :: CLPRIMID, CLSECID, CLTHDID ! Local character strings for location ID's
      CHARACTER(LEN=6) :: CLGWAT, CLSODR, CLSOGW, CLSOSW, CLSEEP, CLSORP, CLSWAT, CLSEDI, CLAIRC ! Media
!
!---- Executable code -------------------------------------------------------------
!
! *** Location information
!
      WRITE(IRPT,1300)
 1300 FORMAT(/'Locations Requested and associated source locations for media and foods.'/ &
              '  Index PRIMARY  SECOND   THIRD      GWAT     SODR     SOGW     SOSW  ',&
              '   SEEP     SORP     SWAT     SEDI     AIRC     BIRD     MEAT     FISH    FISH_2   FISH_3'/&
              '  ----- -------- -------- -------- -------- -------- -------- --------',&
              ' -------- -------- -------- -------- -------- -------- -------- -------- -------- --------')
      DO ILOC = 1, ESD_NUM_LOC
        IF( .NOT.ESD_LOC(ILOC)%COMP ) CYCLE
        CALL GET_LOC_LABELS( ILOC, CLPRIMID, CLSECID, CLTHDID, CLGWAT, CLSODR, CLSOGW, CLSOSW, CLSEEP, &
          CLSORP, CLSWAT, CLSEDI, CLAIRC, CLBIRD, CLMEAT, CLFISH, CLFISH_2, CLFISH_3 )
        WRITE(IRPT,1310) ILOC, CLPRIMID, CLSECID, CLTHDID, CLGWAT, CLSODR, CLSOGW, CLSOSW, CLSEEP, &
          CLSORP, CLSWAT, CLSEDI, CLAIRC, CLBIRD, CLMEAT, CLFISH, CLFISH_2, CLFISH_3
 1310   FORMAT(3X,I4,' "',16(A,'" "'),A,'"')
      END DO
!
      RETURN
      END SUBROUTINE

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
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!
!!  Call List Variables:
!!
!!    IFLG  : Input integer flag
!!              1 = Starting time
!!              2 = EndinG time
!!    IERR  : Output integer flag (0=no errors)
!!
!!**********************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IFLG ! Flag for place (begining or end)
      INTEGER :: IRPT ! Unit number for the report file
      INTEGER :: IERR ! Error flag indicator
!
! *** Local variables
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
 1010     FORMAT(4X,'Date: ',A,'/',A,'/',A/4X,'Time: ',A,':',A,':',A)
          RETURN
!
        CASE( 2 ) ! Ending problem execution
          CALL DATE_AND_TIME( EDATE, ETIME )
          WRITE(IRPT,1000) 'Ending Problem Execution'
          WRITE(IRPT,1010) EDATE(5:6), EDATE(7:8), EDATE(1:4), &
            ETIME(1:2), ETIME(3:4), ETIME(5:10)
          RETURN
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
      END SUBROUTINE

      SUBROUTINE ESD_INIT( )
!!*****************************************************************************************
!!
!! Purpose:
!!
!!    This subroutine initializes all of the variables that had memory allocated by
!!    subroutine ESD_MEMORY.  This includes data from the ESD file and the associated
!!    compute-related information for this run.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 May 2000 : Version 2.0
!!    Paul W. Eslinger : 22 Oct 2003 : Add MEAT remote location logic
!!    Paul W. Eslinger : 17 Feb 2006 : SCR-1107  Add more options on solution selections
!!
!!*****************************************************************************************
!
! *** Global variables
      USE ESD_Mod
      USE Errors_Mod
      USE Files_Mod
      USE Control_Mod
      USE Stocvars_Mod
      USE ECDA_Mod, ONLY: ECDA_NMED
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error flag, nonzero if no match
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'ESD_INIT'
      INTEGER :: I    ! Looping variable
      INTEGER :: IANA ! Analyte looping variable
      INTEGER :: IMED ! Media looping variable
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
        ESD_LOC(I)%THIRD  = 0
        ESD_LOC(I)%MEAT   = 0
        ESD_LOC(I)%FISH   = 0
        ESD_LOC(I)%FISH_2 = 0
        ESD_LOC(I)%FISH_3 = 0
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
!
        ESD_ANA(I)%ID = ' '
        ESD_ANA(I)%NAME = ' '
        ESD_ANA(I)%COMP    = .FALSE.
        ESD_ANA(I)%OUTPUT  = .FALSE.
        ESD_ANA(I)%VERBOSE = .FALSE.
        ESD_ANA(I)%TERSE   = .FALSE.
        ESD_ANA(I)%RFDING  = .FALSE.
        ESD_ANA(I)%RFDINH  = .FALSE.
        ESD_ANA(I)%SFING   = .FALSE.
        ESD_ANA(I)%SFINH   = .FALSE.
        ESD_ANA(I)%ANATYP_CAR = .FALSE.
        ESD_ANA(I)%ANATYP_HAZ = .FALSE.
        ESD_ANA(I)%ANATYP_RAD = .FALSE.
!
        FNCON(I) = ' '
!
      END DO
!
! *** Variables for the species data
!
      DO ISPC = 1, ESD_NUM_SPC
        ESD_SPC(ISPC)%ID = ' '
        ESD_SPC(ISPC)%NAME = ' '
        ESD_SPC(ISPC)%HABITAT = ' '
      END DO
!
! *** Units conversion data
!
      DO IANA = 1, ESD_NUM_ANA
        DO IMED = 1, ECDA_NMED
          CONVERT_UNITS(IANA,IMED) = -1.0E30
          CONVERT_LABELS(IANA,IMED,1) = 'Bad Data'
          CONVERT_LABELS(IANA,IMED,2) = 'Bad Data'
        END DO
      END DO
!
      RETURN
      END SUBROUTINE

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
!!
!!**********************************************************************
!
! *** Global variables
      USE Control_Mod, ONLY: CONVERT_UNITS, CONVERT_LABELS
      USE ECDA_Mod, ONLY: ECDA_NMED
      USE Errors_Mod
      USE Files_Mod
      USE ESD_Mod
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
      ALLOCATE( FNCON(ESD_NUM_ANA), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating memory for FNCON'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ICON(ESD_NUM_ANA), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating memory for ICON'
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
! *** Check on the number of locations
!
      IF( ESD_NUM_SPC .LT. 1 ) THEN
        IERR = 5
        MESSAG(1) = 'At least 1 location required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE(ESD_SPC (ESD_NUM_SPC), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating memory for ESD_SPC'
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
! *** Check on the number of times
!
      IF( ESD_NUM_TIM .LT. 1 ) THEN
        IERR = 8
        MESSAG(1) = 'At least 1 time required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( ESD_TIM(ESD_NUM_TIM), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Error allocating memory for ESD_TIM'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check on the number of realizations
!
      IF( ESD_NREAL .LT. 1 ) THEN
        IERR = 10
        MESSAG(1) = 'At least 1 ESD realization required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Units conversion factors
!
      ALLOCATE( CONVERT_UNITS(ESD_NUM_ANA,ECDA_NMED), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 79
        MESSAG(1) = 'Error allocating memory for CONVERT_UNITS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CONVERT_LABELS(ESD_NUM_ANA,ECDA_NMED,2), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 80
        MESSAG(1) = 'Error allocating memory for CONVERT_LABELS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE FINISH_POPDOSE( ITIM, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine finishes the population dose calculation for
!!    a single time slice.
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 17 Feb 2006 : Fix output units
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Control_Mod
      USE Results_Mod
      USE Errors_Mod
      USE Stocvars_Mod
      USE Esd_Mod
      USE Files_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error indicator
      INTEGER, INTENT(IN) :: ITIM ! Time loop variable
!
! *** Local variables
      CHARACTER(LEN=14) :: CALLER = 'FINISH_POPDOSE'
      REAL :: XMIN ! Statistics - minimum
      REAL :: XV01 ! Statistics -  1st percentile
      REAL :: XV05 ! Statistics -  5th percentile
      REAL :: XV10 ! Statistics - 10th percentile
      REAL :: XV25 ! Statistics - 25th percentile
      REAL :: XMED ! Statistics - median
      REAL :: XV75 ! Statistics - 75th percentile
      REAL :: XV90 ! Statistics - 90th percentile
      REAL :: XV95 ! Statistics - 95th percentile
      REAL :: XV99 ! Statistics - 99th percentile
      REAL :: XMAX ! Statistics - maximum
      REAL :: XAVG ! Statistics - average
      REAL :: XSTD ! Statistics - standard deviation
!
!---- Executable code -------------------------------------------------------------
!
! *** Compute combined summary statistics
!
      IF( STA_POPDOSE ) THEN
        CALL USTAT( POPDOSE, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error detected for variable POPDOSE'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, 'POPDOSE', 'POPDOSE', 'RAD', 'POPDOSE', 'Person-Rem',&
          XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
      END IF
!
      IF( STA_POPRISK ) THEN
        CALL USTAT( POPRISK, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error detected for variable POPRISK'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, 'POPRISK', 'POPRISK', 'RAD', 'POPRISK', 'LCF',&
          XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
      END IF
!
! *** Output combined detailed values
!
      IF( DET_POPDOSE ) THEN
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, 'POPDOSE', 'POPDOSE', 'RAD', 'POPDOSE', 'Person-Rem', POPDOSE, NREAL )
      END IF
!
      IF( DET_POPRISK ) THEN
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, 'POPRISK', 'POPRISK', 'RAD', 'POPRISK', 'LCF', POPRISK, NREAL )
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE FOOD_READ( ITIM, ILOC, IERR )
!!***********************************************************************************************************************
!!
!!  Purpose:
!!
!!    This routine reads food files (nominally written by the ECEM code) for a single time and a single location.
!!
!!    The concentration for a food type is set to zero rather than reading data from the food concentration file
!!    if the scenario has a zero consumption rate for that food.
!!
!!  History:
!!
!!    Carmen Arimescu :  25 Feb 2003 : Version 1.0
!!    Paul W. Eslinger : 26 Jun 2003 : Allow runs with no foods
!!    Paul W. Eslinger : 14 Oct 2003 : Fix debug writes
!!    Paul W. Eslinger : 23 Oct 2003 : Add MEAT remote location logic
!!                                     Upgrade error checking
!!    Paul W. Eslinger : 12 May 2005 : Add output units
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!    Paul W. Eslinger :  1 Dec 2005 : Fix output units labels
!!    Paul W. Eslinger : 28 Feb 2006 : Add food statistics outputs
!!    Paul W. Eslinger : 15 Sep 2006 : Fix food details and statistics labels
!!    Paul W. Eslinger : 15 Sep 2006 : Add error check for negative concentrations
!!    Paul W. Eslinger :  5 Jun 2007 : Change FCDA routines to get ESD parameters from the call list rather than a module
!!    Paul W. Eslinger : 28 Jun 2012 : Fix bug in call to FCDA_RECNO_AQ for FISH_3
!!
!!***********************************************************************************************************************
!
! *** Global parameters, variables, and arrays
!
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Concen_Mod
      USE Stocvars_Mod
      USE Errors_Mod
      USE ESD_Mod
!
! *** Call list variables
!
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER :: IERR ! Error flag
!
! *** Local variables
!
      CHARACTER(LEN=9) :: CALLER = 'FOOD_READ'
      CHARACTER(LEN=80) :: CTMP ! Local character variable
      INTEGER :: LEN_CTMP       ! Variable length of CTMP
      INTEGER :: NREC ! Record number for use in a binary food file
      INTEGER :: TMP_YEAR ! Year associated with a FCDA data record
      CHARACTER(LEN=6) :: TMP_ID ! Location ID associated with a FCDA data record
!
      REAL :: XMIN ! Statistics - minimum
      REAL :: XV01 ! Statistics -  1st percentile
      REAL :: XV05 ! Statistics -  5th percentile
      REAL :: XV10 ! Statistics - 10th percentile
      REAL :: XV25 ! Statistics - 25th percentile
      REAL :: XMED ! Statistics - median
      REAL :: XV75 ! Statistics - 75th percentile
      REAL :: XV90 ! Statistics - 90th percentile
      REAL :: XV95 ! Statistics - 95th percentile
      REAL :: XV99 ! Statistics - 99th percentile
      REAL :: XMAX ! Statistics - maximum
      REAL :: XAVG ! Statistics - average
      REAL :: XSTD ! Statistics - standard deviation
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
      IF( BGFOOD ) THEN
        CALL TELLTIME( 'Entering '//CALLER, 'SCREEN', .FALSE., IRPT )
        WRITE(IRPT,1000) 'Entering ' // CALLER
 1000   FORMAT(/A)
        CTMP = 'Location index '
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ILOC
        CTMP = TRIM(CTMP) // ' : '//ESD_LOC(ILOC)%ID
        WRITE(IRPT,1010) TRIM(CTMP)
 1010   FORMAT(A)
!
        CTMP = 'Time index '
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ITIM
        CTMP = TRIM(CTMP) // ' : Calendar year'
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ESD_TIM(ITIM)%TIME
        WRITE(IRPT,1010) TRIM(CTMP)
!
        WRITE(IRPT,1020) 'NREAL (Number of realizations)', NREAL
 1020   FORMAT(A,' : ',I0)
!
      END IF
!
!
! *** Loop over analytes
!
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
!
! ***   Define the food units for output labeling purposes
!
        IF (ESD_ANA(IANA)%TYPE(2:2) .EQ. 'R' )    THEN
          FOOD_UNIT = 'pCi/kg'
          MILK_UNIT = 'pCi/L'
        ELSE
          FOOD_UNIT = 'ug/kg'
          MILK_UNIT = 'ug/L'
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: FISH
! -------------------------------------------------------------------------------------------
!
        IF( USE_FISH ) THEN
!
          ISPC = FISH_FIDX
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
           IERR = 1
           MESSAG(1) = 'Species index for fish food is invalid'
           CALL PRTERR( IERR, CALLER, 1 )
           RETURN
          END IF
!
          IF( ESD_LOC(ILOC)%FISH.LT.1 .OR. ESD_LOC(ILOC)%FISH.GT.ESD_NUM_LOC ) THEN
           IERR = 2
           MESSAG(1) = 'The location index for FISH was not set properly'
           MESSAG(2) = 'Check the FISH modifier on the HUMAN INGESTION keyword'
           MESSAG(3) = 'Location ID = "' // ESD_LOC(ILOC)%ID // '"'
           CALL PRTERR( IERR, CALLER, 3 )
           RETURN
          END IF
!
          NREC = -1
!
!         Get the fish concentration index from the specific FISH location
          IF( ESD_LOC(ESD_LOC(ILOC)%FISH)%TYPE .EQ. 'AQUATIC' ) THEN
            CALL FCDA_RECNO_AQ( ITIM, ESD_LOC(ILOC)%FISH, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking FISH'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 3
            MESSAG(1) = 'The specified location for FISH was not AQUATIC'
            MESSAG(2) = 'The location was "'//TRIM(ESD_LOC(ESD_LOC(ILOC)%FISH)%ID)
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!         Check whether a FISH solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 4
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking FISH'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
            MESSAG(5) = ' '
            WRITE(MESSAG(5),'(A,I0,1X,A)') 'Fish Index=', ESD_LOC(ILOC)%FISH, ESD_LOC(ESD_LOC(ILOC)%FISH)%ID
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine - seeking FISH'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 5
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ESD_LOC(ILOC)%FISH)%ID ) THEN
            IERR = 6
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ESD_LOC(ILOC)%FISH)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 7
              MESSAG(1) = 'Negative food concentration in the food file for food type : FISH'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CFISH(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, &
              ESD_ANA(IANA)%ID, 'CONFOODS', 'FISH', FOOD_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable FISH'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'FISH', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
!
        ELSE
          FORALL (IREL=1:NREAL) CFISH(IANA,IREL) = 0.0
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: FISH_2
! -------------------------------------------------------------------------------------------
!
        IF( USE_FISH_2 ) THEN
!
          ISPC = FISH_FIDX_2
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
           IERR = 8
           MESSAG(1) = 'Species index for fish_2 food is invalid'
           CALL PRTERR( IERR, CALLER, 1 )
           RETURN
          END IF
!
          IF( ESD_LOC(ILOC)%FISH_2.LT.1 .OR. ESD_LOC(ILOC)%FISH_2.GT.ESD_NUM_LOC ) THEN
           IERR = 9
           MESSAG(1) = 'The location index for FISH_2 was not set properly'
           MESSAG(2) = 'Check the FISH_2 modifier on the HUMAN INGESTION keyword'
           MESSAG(3) = 'Location ID = "' // ESD_LOC(ILOC)%ID // '"'
           CALL PRTERR( IERR, CALLER, 3 )
           RETURN
          END IF
!
          NREC = -1
!
!         Get the fish concentration index from the specific FISH_2 location
          IF( ESD_LOC(ESD_LOC(ILOC)%FISH_2)%TYPE .EQ. 'AQUATIC' ) THEN
            CALL FCDA_RECNO_AQ( ITIM, ESD_LOC(ILOC)%FISH_2, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking FISH_2'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 10
            MESSAG(1) = 'The specified location for FISH_2 was not AQUATIC'
            MESSAG(2) = 'The location was "'//TRIM(ESD_LOC(ESD_LOC(ILOC)%FISH_2)%ID)
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!         Check whether a FISH_2 solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 11
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking FISH_2'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
            MESSAG(5) = ' '
            WRITE(MESSAG(5),'(A,I0,1X,A)') 'Fish_2 Index=', ESD_LOC(ILOC)%FISH_2, ESD_LOC(ESD_LOC(ILOC)%FISH_2)%ID
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine - seeking FISH_2'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 12
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ESD_LOC(ILOC)%FISH_2)%ID ) THEN
            IERR = 13
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ESD_LOC(ILOC)%FISH_2)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 14
              MESSAG(1) = 'Negative food concentration in the food file for food type : FISH_2'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CFISH_2(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food  concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, &
              ESD_ANA(IANA)%ID, 'CONFOODS', 'FISH_2', FOOD_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable FISH_2'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'FISH_2', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
        ELSE
          FORALL (IREL=1:NREAL) CFISH_2(IANA,IREL) = 0.0
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: FISH_3
! -------------------------------------------------------------------------------------------
!
        IF( USE_FISH_3 ) THEN
!
          ISPC = FISH_FIDX_3
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
           IERR = 15
           MESSAG(1) = 'Species index for fish_3 food is invalid'
           CALL PRTERR( IERR, CALLER, 1 )
           RETURN
          END IF
!
          IF( ESD_LOC(ILOC)%FISH_3.LT.1 .OR. ESD_LOC(ILOC)%FISH_3.GT.ESD_NUM_LOC ) THEN
           IERR = 16
           MESSAG(1) = 'The location index for FISH_3 was not set properly'
           MESSAG(2) = 'Check the FISH_3 modifier on the HUMAN INGESTION keyword'
           MESSAG(3) = 'Location ID = "' // ESD_LOC(ILOC)%ID // '"'
           CALL PRTERR( IERR, CALLER, 3 )
           RETURN
          END IF
!
          NREC = -1
!
!         Get the fish concentration index from the specific FISH_3 location
          IF( ESD_LOC(ESD_LOC(ILOC)%FISH_3)%TYPE .EQ. 'AQUATIC' ) THEN
            CALL FCDA_RECNO_AQ( ITIM, ESD_LOC(ILOC)%FISH_3, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 17999
              MESSAG(1) = 'Error in lower level routine - seeking FISH_3'
              MESSAG(2) = 'The location was "'//TRIM(ESD_LOC(ESD_LOC(ILOC)%FISH_3)%ID)//'"'
              MESSAG(3) = 'The analyte was "'//TRIM(ESD_ANA(IANA)%ID)//'"'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 17
            MESSAG(1) = 'The specified location for FISH_3 was not AQUATIC'
            MESSAG(2) = 'The location was "'//TRIM(ESD_LOC(ESD_LOC(ILOC)%FISH_3)%ID)
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!         Check whether a FISH_3 solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 18
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking FISH_3'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
            MESSAG(5) = ' '
            WRITE(MESSAG(5),'(A,I0,1X,A)') 'Fish_3 Index=', ESD_LOC(ILOC)%FISH_3, ESD_LOC(ESD_LOC(ILOC)%FISH_3)%ID
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 18999
            MESSAG(1) = 'Error in lower level routine - seeking FISH_3'
            MESSAG(2) = 'The location was "'//TRIM(ESD_LOC(ESD_LOC(ILOC)%FISH_3)%ID)//'"'
            MESSAG(3) = 'The analyte was "'//TRIM(ESD_ANA(IANA)%ID)//'"'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 19
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ESD_LOC(ILOC)%FISH_3)%ID ) THEN
            IERR = 20
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ESD_LOC(ILOC)%FISH_3)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 21
              MESSAG(1) = 'Negative food concentration in the food file for food type : FISH_3'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CFISH_3(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food  concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, &
              ESD_ANA(IANA)%ID, 'CONFOODS', 'FISH_3', FOOD_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable FISH_3'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'FISH_3', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
        ELSE
          FORALL (IREL=1:NREAL) CFISH_3(IANA,IREL) = 0.0
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: Leafy Vegetables
! -------------------------------------------------------------------------------------------
!
        IF( USE_LEAFVEG ) THEN
!
          ISPC = LEAFVEG_FIDX
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
            IERR = 22
            MESSAG(1) = 'Species index for leafy vegetable food is invalid'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          NREC = -1
!
!         When the primary location is riparian use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) THEN
            CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC,  ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking leafy vegetables'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!        When the primary location is upland use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
            CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX,  ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking leafy vegetables'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         Check whether a leafy vegetable solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 23
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking leafy vegetables'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,A,I0)') 'ITIM=', ITIM, ' Year=', ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine - seeking leafy vegetables'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 24
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ILOC)%ID ) THEN
            IERR = 25
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 26
              MESSAG(1) = 'Negative food concentration in the food file for food type : LEAFVEG'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CLEAFVEG(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food  concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
            'CONFOODS', 'LEAFVEG', FOOD_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable LEAFVEG'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'LEAFVEG', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
        ELSE
          FORALL (IREL=1:NREAL) CLEAFVEG(IANA,IREL) = 0.0
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: Root Vegetables
! -------------------------------------------------------------------------------------------
!
        IF( USE_ROOTVEG ) THEN
!
          ISPC = ROOTVEG_FIDX
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
            IERR = 27
            MESSAG(1) = 'Species index for root vegetables food is invalid'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          NREC = -1
!
!         When the primary location is riparian use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) THEN
            CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC,  ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking root vegetables'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         When the primary location is upland use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
            CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX,  ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking root vegetables'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         Check whether a root vegetable solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 28
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking root vegetables'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine - seeking root vegetables'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 29
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ILOC)%ID ) THEN
            IERR = 30
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 31
              MESSAG(1) = 'Negative food concentration in the food file for food type : ROOTVEG'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CROOTVEG(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food  concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
            'CONFOODS', 'ROOTVEG', FOOD_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable ROOTVEG'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'ROOTVEG', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
        ELSE
          FORALL (IREL=1:NREAL) CROOTVEG(IANA,IREL) = 0.0
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: Grain
! -------------------------------------------------------------------------------------------
!
        IF( USE_GRAIN ) THEN
!
          ISPC = GRAIN_FIDX
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
            IERR = 32
            MESSAG(1) = 'Species index for grain food is invalid'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          NREC = -1
!
!         When the primary location is riparian use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) THEN
            CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC,  ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking grain'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         When the primary location is upland use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
            CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX,  ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking grain'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         Check whether a grain solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 33
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking grain'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine - seeking grain'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 34
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ILOC)%ID ) THEN
            IERR = 35
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 36
              MESSAG(1) = 'Negative food concentration in the food file for food type : GRAIN'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CGRAIN(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food  concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
            'CONFOODS', 'GRAIN', FOOD_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable GRAIN'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'GRAIN', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
        ELSE
          FORALL (IREL=1:NREAL) CGRAIN(IANA,IREL) = 0.0
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: Fruit
! -------------------------------------------------------------------------------------------
!
        IF( USE_FRUIT ) THEN
!
          ISPC = FRUIT_FIDX
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
            IERR = 37
            MESSAG(1) = 'Species index for fruit food is invalid'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          NREC = -1
!
!         When the primary location is riparian use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) THEN
            CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking fruit'
              MESSAG(2) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
!         When the primary location is upland use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
            CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking fruit'
              MESSAG(2) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
!         Check whether a fruit solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 38
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking fruit'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine - seeking fruit'
            MESSAG(2) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 39
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ILOC)%ID ) THEN
            IERR = 40
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 41
              MESSAG(1) = 'Negative food concentration in the food file for food type : FRUIT'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CFRUIT(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food  concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
            'CONFOODS', 'FRUIT', FOOD_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable FRUIT'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'FRUIT', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
        ELSE
          FORALL (IREL=1:NREAL) CFRUIT(IANA,IREL) = 0.0
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: Meat
! -------------------------------------------------------------------------------------------
!
        IF( USE_MEAT ) THEN
!
          ISPC = MEAT_FIDX
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
            IERR = 42
            MESSAG(1) = 'Species index for meat food is invalid'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( ESD_LOC(ILOC)%MEAT.LT.1 .OR. ESD_LOC(ILOC)%MEAT.GT.ESD_NUM_LOC ) THEN
           IERR = 43
           MESSAG(1) = 'The location index for MEAT was not set properly'
           MESSAG(2) = 'Check the MEAT modifier on the HUMAN location keyword'
           MESSAG(3) = 'Location ID = "' // ESD_LOC(ILOC)%ID // '"'
           CALL PRTERR( IERR, CALLER, 3 )
           RETURN
          END IF
!
          NREC = -1
!
!         When the meat location is riparian
          IF( ESD_LOC(ESD_LOC(ILOC)%MEAT)%TYPE .EQ. 'RIPARIAN' ) THEN
            CALL FCDA_RECNO_RP( ITIM, ESD_LOC(ILOC)%MEAT, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking meat'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         When the meat location is upland
          IF( ESD_LOC(ESD_LOC(ILOC)%MEAT)%TYPE .EQ. 'UPLAND' ) THEN
            CALL FCDA_RECNO_UP( ITIM, ESD_LOC(ILOC)%MEAT,ISPC,SOIL_IDX,ESD_NUM_TIM,ESD_NUM_LOC,ESD_NUM_SPC,ESD_NUM_SOI, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking meat'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         Check whether a meat solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 44
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking meat'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=',  ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=',  ISPC, ESD_SPC(ISPC)%ID
            MESSAG(5) = ' '
            WRITE(MESSAG(5),'(A,I0,1X,A)') 'Meat Index=', ESD_LOC(ILOC)%MEAT, &
              'Meat location ID="'//ESD_LOC(ESD_LOC(ILOC)%MEAT)%ID//'"'
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine - seeking meat'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 45
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ESD_LOC(ILOC)%MEAT)%ID ) THEN
            IERR = 46
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ESD_LOC(ILOC)%MEAT)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 47
              MESSAG(1) = 'Negative food concentration in the food file for food type : MEAT'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CMEAT(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food  concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
            'CONFOODS', 'MEAT', FOOD_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable MEAT'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'MEAT', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
        ELSE
          FORALL (IREL=1:NREAL) CMEAT(IANA,IREL) = 0.0
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: BIRD
! -------------------------------------------------------------------------------------------
!
        IF( USE_BIRD ) THEN
!
          ISPC = BIRD_FIDX
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
            IERR = 48
            MESSAG(1) = 'Species index for bird food is invalid'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( ESD_LOC(ILOC)%BIRD.LT.1 .OR. ESD_LOC(ILOC)%BIRD.GT.ESD_NUM_LOC ) THEN
           IERR = 49
           MESSAG(1) = 'The location index for BIRD was not set properly'
           MESSAG(2) = 'Check the BIRD modifier on the HUMAN location keyword'
           MESSAG(3) = 'Location ID = "' // ESD_LOC(ILOC)%ID // '"'
           CALL PRTERR( IERR, CALLER, 3 )
           RETURN
          END IF
!
          NREC = -1
!
!         Get the bird concentration from the specific BIRD location
          IF( ESD_LOC(ESD_LOC(ILOC)%BIRD)%TYPE .EQ. 'RIPARIAN' ) THEN
            CALL FCDA_RECNO_RP( ITIM, ESD_LOC(ILOC)%BIRD, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking BIRD'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
          IF( ESD_LOC(ESD_LOC(ILOC)%BIRD)%TYPE .EQ. 'UPLAND' ) THEN
            CALL FCDA_RECNO_UP( ITIM, ESD_LOC(ILOC)%BIRD,ISPC,SOIL_IDX,ESD_NUM_TIM,ESD_NUM_LOC,ESD_NUM_SPC,ESD_NUM_SOI, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking BIRD'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         Check whether a bird solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 50
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking bird'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=',  ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=',  ISPC, ESD_SPC(ISPC)%ID
            MESSAG(5) = ' '
            WRITE(MESSAG(5),'(A,I0,1X,A)') 'Bird Index=', ESD_LOC(ILOC)%BIRD, &
              'Bird location ID="'//ESD_LOC(ESD_LOC(ILOC)%BIRD)%ID//'"'
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine - seeking bird'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 51
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ESD_LOC(ILOC)%BIRD)%ID ) THEN
            IERR = 52
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ESD_LOC(ILOC)%BIRD)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 53
              MESSAG(1) = 'Negative food concentration in the food file for food type : BIRD'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CBIRD(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food  concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, &
              ESD_ANA(IANA)%ID, 'CONFOODS', 'BIRD', FOOD_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable BIRD'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'BIRD', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
        ELSE
          FORALL (IREL=1:NREAL) CBIRD(IANA,IREL) = 0.0
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: Milk
! -------------------------------------------------------------------------------------------
!
        IF( USE_MILK ) THEN
!
          ISPC = MILK_FIDX
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
            IERR = 54
            MESSAG(1) = 'Species index for milk food is invalid'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          NREC = -1
!
!         When the primary location is riparian use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) THEN
            CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking milk'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         When the primary location is upland use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
            CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking milk'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         Check whether a milk solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 55
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking milk'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine - seeking milk'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 56
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ILOC)%ID ) THEN
            IERR = 57
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 58
              MESSAG(1) = 'Negative food concentration in the food file for food type : MILK'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CMILK(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food  concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
            'CONFOODS', 'MILK', MILK_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable MILK'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'MILK', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
        ELSE
          FORALL (IREL=1:NREAL) CMILK(IANA,IREL) = 0.0
        END IF
!
! -------------------------------------------------------------------------------------------
!       Food: Eggs
! -------------------------------------------------------------------------------------------
!
        IF( USE_EGGS ) THEN
!
          ISPC = EGGS_FIDX
          IF( ISPC.LE.0 .OR. ISPC.GT.ESD_NUM_SPC ) THEN
            IERR = 59
            MESSAG(1) = 'Species index for eggs food is invalid'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          NREC = -1
!
!         When the primary location is riparian use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) THEN
            CALL FCDA_RECNO_RP( ITIM, ILOC, ISPC, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking eggs'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         When the primary location is upland use local data
          IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
            CALL FCDA_RECNO_UP( ITIM, ILOC, ISPC, SOIL_IDX, ESD_NUM_TIM, ESD_NUM_LOC, ESD_NUM_SPC, ESD_NUM_SOI, NREC, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error in lower level routine - seeking eggs'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!         Check whether a eggs solution was available
          IF( NREC .LT. 0 ) THEN
            IERR = 60
            MESSAG(1) = 'NREC was unexpectedly less than zero - seeking eggs'
            MESSAG(2) = ' '
            WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
            MESSAG(3) = ' '
            WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
            MESSAG(4) = ' '
            WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
!
!         Actually read the food concentration data
          CALL FCDA_READ( NREC, TMP_YEAR, TMP_ID, CVEC, NREAL, IFOD(IANA,ISPC), IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine - seeking eggs'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          IF( TMP_YEAR .NE. ESD_TIM(ITIM)%TIME ) THEN
            IERR = 61
            MESSAG(1) = 'Mismatch on times with the FCDA data file'
            MESSAG(2) = 'Expecting year '
            WRITE(MESSAG(2)(17:),'(I0)') ESD_TIM(ITIM)%TIME
            MESSAG(3) = 'Obtained year '
            WRITE(MESSAG(3)(17:),'(I0)') TMP_YEAR
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          IF( TMP_ID .NE. ESD_LOC(ILOC)%ID ) THEN
            IERR = 62
            MESSAG(1) = 'Mismatch on locations with the FCDA data file'
            MESSAG(2) = 'Expecting ID ' // ESD_LOC(ILOC)%ID
            MESSAG(3) = 'Obtained ID  ' // TMP_ID
            MESSAG(4) = 'Record number'
            WRITE(MESSAG(4)(17:),'(I0)') NREC
            MESSAG(5) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 5 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            IF( CVEC(IREL) .LT. 0.0 ) THEN
              IERR = 63
              MESSAG(1) = 'Negative food concentration in the food file for food type : EGGS'
              MESSAG(2) = ' '
              WRITE(MESSAG(2),'(A,I0,1X,I0)') 'ITIM=', ITIM, ESD_TIM(ITIM)%TIME
              MESSAG(3) = ' '
              WRITE(MESSAG(3),'(A,I0,1X,A)') 'ILOC=', ILOC, ESD_LOC(ILOC)%ID
              MESSAG(4) = ' '
              WRITE(MESSAG(4),'(A,I0,1X,A)') 'ISPC=', ISPC, ESD_SPC(ISPC)%ID
              MESSAG(5) = ' '
              WRITE(MESSAG(5),'(A,I0,1X,A,1P,E12.5)') 'Realization = ',IREL,'Value = ', CVEC(IREL)
              CALL PRTERR( IERR, CALLER, 5 )
              RETURN
            END IF
          END DO
!
          FORALL (IREL=1:NREAL) CEGGS(IANA,IREL) = CVEC(IREL)
!
!         Optional output on food  concentrations
          IF( DET_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, &
            'CONFOODS', 'EGGS', FOOD_UNIT, CVEC, NREAL )
!
          IF( STA_FOOD .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable EGGS'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'CONFOODS', 'EGGS', &
              FOOD_UNIT, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
        ELSE
          FORALL (IREL=1:NREAL) CEGGS(IANA,IREL) = 0.0
        END IF
!
! ***   Debug write on food concentration data
!
        IF( BGFOOD ) THEN
!
          IF( USE_BIRD ) THEN
            WRITE(IRPT,1035) 'BIRD', FOOD_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(BIRD_FIDX)%ID, ESD_LOC(ESD_LOC(ILOC)%BIRD)%ID, (CBIRD(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1035) 'BIRD', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
          IF( USE_FISH ) THEN
            WRITE(IRPT,1035) 'FISH', FOOD_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(FISH_FIDX)%ID, ESD_LOC(ESD_LOC(ILOC)%FISH)%ID, (CFISH(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1035) 'FISH', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
          IF( USE_FISH_2 ) THEN
            WRITE(IRPT,1032) 'FISH_2', FOOD_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(FISH_FIDX_2)%ID, ESD_LOC(ESD_LOC(ILOC)%FISH_2)%ID, (CFISH_2(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1032) 'FISH_2', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
          IF( USE_FISH_3 ) THEN
            WRITE(IRPT,1032) 'FISH_3', FOOD_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(FISH_FIDX_3)%ID, ESD_LOC(ESD_LOC(ILOC)%FISH_3)%ID, (CFISH_3(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1032) 'FISH_3', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
          IF( USE_EGGS ) THEN
            WRITE(IRPT,1035) 'EGGS', FOOD_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(EGGS_FIDX)%ID, ESD_LOC(ILOC)%ID, (CEGGS(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1035) 'EGGS', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
          IF( USE_GRAIN ) THEN
            WRITE(IRPT,1040) 'GRAIN', FOOD_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(GRAIN_FIDX)%ID, ESD_LOC(ILOC)%ID, (CGRAIN(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1040) 'GRAIN', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
          IF( USE_FRUIT ) THEN
            WRITE(IRPT,1040) 'FRUIT', FOOD_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(FRUIT_FIDX)%ID, ESD_LOC(ILOC)%ID, (CFRUIT(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1040) 'FRUIT', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
          IF( USE_MEAT ) THEN
            WRITE(IRPT,1035) 'MEAT', FOOD_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(MEAT_FIDX)%ID, ESD_LOC(ESD_LOC(ILOC)%MEAT)%ID, (CMEAT(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1035) 'MEAT', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
          IF( USE_MILK ) THEN
            WRITE(IRPT,1035) 'MILK', MILK_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(MILK_FIDX)%ID, ESD_LOC(ILOC)%ID, (CMILK(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1035) 'MILK', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
          IF( USE_LEAFVEG ) THEN
            WRITE(IRPT,1030) 'LEAFVEG', FOOD_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(LEAFVEG_FIDX)%ID, ESD_LOC(ILOC)%ID, (CLEAFVEG(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1030) 'LEAFVEG', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
          IF( USE_ROOTVEG ) THEN
            WRITE(IRPT,1030) 'ROOTVEG', FOOD_UNIT, &
              ESD_ANA(IANA)%ID, ESD_SPC(ROOTVEG_FIDX)%ID, ESD_LOC(ILOC)%ID, (CROOTVEG(IANA,IREL),IREL=1,NREAL)
          ELSE
            WRITE(IRPT,1030) 'ROOTVEG', ESD_ANA(IANA)%ID, 'Not used'
          END IF
!
 1035     FORMAT(3X,A,:,4X,A,:,1X,A,1X,A,1X,A,1P,100(1X,E12.5))
 1040     FORMAT(3X,A,:,3X,A,:,1X,A,1X,A,1X,A,1P,100(1X,E12.5))
 1032     FORMAT(3X,A,:,2X,A,:,1X,A,1X,A,1X,A,1P,100(1X,E12.5))
 1030     FORMAT(3X,A,:,1X,A,:,1X,A,1X,A,1X,A,1P,100(1X,E12.5))
        END IF
      END DO
!
      RETURN
      END SUBROUTINE

      SUBROUTINE GET_INDEX( ITIM, ILOC, IMED, IDX, IPLACE, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine gets the record number in an ECDA concetration
!!    file associated with:
!!      time index, ITIM
!!      location index, ILOC
!!      media index, IMED
!!    The index is output in the variable IDX
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Apr 2000 : Version 1.0
!!    Paul W. Eslinger : 22 Sep 2004 : Add the secondary location index
!!    Paul W. Eslinger : 13 May 2005 : Add the third location index
!!    Paul W. Eslinger : 14 Sep 2006 : Add IPLACE indicator
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_mod
      USE Ecda_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Time loop index
      INTEGER, INTENT(IN) :: ILOC ! Location loop index
      INTEGER, INTENT(IN) :: IMED ! Media index
!
      INTEGER :: IDX    ! Output record number index
      INTEGER :: IPLACE ! Output index for source place (primary=1,second=2,third=3)
      INTEGER :: IERR   ! Error number variable
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'GET_INDEX'
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
      IPLACE = 0
!
! *** Get the primary location index
!
      CALL ECDA_RECNO_INDEX( ITIM, ILOC, IMED, IDX, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 999
        MESSAG(1) = 'Error in lower level routine for primary index'
        MESSAG(2) = 'Location ID is '//ESD_LOC(ILOC)%ID
        MESSAG(3) = 'Time index is '
        WRITE(MESSAG(3)(16:),*) ITIM
        MESSAG(4) = 'Media index is '
        WRITE(MESSAG(4)(17:),*) IMED
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
      IF( IDX .GT. 0 ) THEN
        IPLACE = 1
        RETURN
      END IF
!
! *** Check for a secondary location index if no primary data
!
      IF( ESD_LOC(ILOC)%SECOND .GT. 0 ) THEN
        CALL ECDA_RECNO_INDEX( ITIM, ESD_LOC(ILOC)%SECOND, IMED, IDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine for secondary index'
          MESSAG(2) = 'Location ID is '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Time index is '
          WRITE(MESSAG(3)(16:),*) ITIM
          MESSAG(4) = 'Media index is '
          WRITE(MESSAG(4)(17:),*) IMED
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
      IF( IDX .GT. 0 ) THEN
        IPLACE = 2
        RETURN
      END IF
!
! *** Check for a third location index if no primary data
!
      IF( ESD_LOC(ILOC)%THIRD .GT. 0 ) THEN
        CALL ECDA_RECNO_INDEX( ITIM, ESD_LOC(ILOC)%THIRD, IMED, IDX, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error in lower level routine for tertiary index'
          MESSAG(2) = 'Location ID is '//ESD_LOC(ILOC)%ID
          MESSAG(3) = 'Time index is '
          WRITE(MESSAG(3)(16:),*) ITIM
          MESSAG(4) = 'Media index is '
          WRITE(MESSAG(4)(17:),*) IMED
          CALL PRTERR( IERR, CALLER, 4 )
          RETURN
        END IF
      END IF
      IF( IDX .GT. 0 ) IPLACE = 3
!
      RETURN
      END SUBROUTINE

      SUBROUTINE GET_LOC_LABELS( ILOC, CLPRIMID, CLSECID, CLTHDID, CLGWAT, CLSODR, CLSOGW, CLSOSW, &
        CLSEEP, CLSORP, CLSWAT, CLSEDI, CLAIRC, CLBIRD, CLMEAT, CLFISH, CLFISH_2, CLFISH_3 )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine determines the location ID associated with foods and media for output
!!    to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 14 Sep 2006 : (SCR-1143) Original code
!!
!!**************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE ECDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: ILOC ! Location looping index
!
      CHARACTER(LEN=6) :: CLBIRD, CLMEAT             ! Local character strings for food ID's
      CHARACTER(LEN=6) :: CLFISH, CLFISH_2, CLFISH_3 ! Local character strings for food ID's
      CHARACTER(LEN=6) :: CLPRIMID, CLSECID, CLTHDID ! Local character strings for location ID's
      CHARACTER(LEN=6) :: CLGWAT, CLSODR, CLSOGW, CLSOSW, CLSEEP, CLSORP, CLSWAT, CLSEDI, CLAIRC ! Media
!
      INTEGER :: IPLACE ! Place index for GET_INDEX
!
      INTEGER :: IDXGWAT ! Groundwater concentration data record number
      INTEGER :: IDXSEEP ! Seep water concentration data record number
      INTEGER :: IDXSWAT ! Surface water concentration data record number
      INTEGER :: IDXSEDI ! Sediment concentration data record number
      INTEGER :: IDXSORP ! Riparian zone concentration data record number
      INTEGER :: IDXSODR ! Non-irrigated upland soil concentration data record number
      INTEGER :: IDXSOGW ! Groundwater irrigated upland soil concentration data record number
      INTEGER :: IDXSOSW ! Surface water irrigated upland soil concentration data record number
      INTEGER :: IDXAIRC ! Air concentration data record number
!
! *** Local variables
      INTEGER :: IDX  ! Temporary index
      INTEGER :: IERR ! Error flag
      INTEGER :: ITMP ! Temporary time index
!
!---- Executable code ---------------------------------------------------
!
      ITMP = 1
!
!     Primary location ID
      CLPRIMID = ESD_LOC(ILOC)%ID
!
!     Secondary location ID
      IDX = ESD_LOC(ILOC)%SECOND
      IF( IDX .GT. 0 ) THEN
        CLSECID = ESD_LOC(IDX)%ID
      ELSE
        CLSECID = '-None-'
      END IF
!
!     Tertiary location ID
      IDX = ESD_LOC(ILOC)%THIRD
      IF( IDX .GT. 0 ) THEN
        CLTHDID = ESD_LOC(IDX)%ID
      ELSE
        CLTHDID = '-None-'
      END IF
!
!     Bird location
      IF( USE_BIRD ) THEN
        CLBIRD = ESD_LOC(ESD_LOC(ILOC)%BIRD)%ID
      ELSE
        CLBIRD = '-None-'
      END IF
!
!     Fish location
      IF( USE_FISH ) THEN
        CLFISH = ESD_LOC(ESD_LOC(ILOC)%FISH)%ID
      ELSE
        CLFISH = '-None-'
      END IF
!
!     Fish_2 location
      IF( USE_FISH_2 ) THEN
        CLFISH_2 = ESD_LOC(ESD_LOC(ILOC)%FISH_2)%ID
      ELSE
        CLFISH_2 = '-None-'
      END IF
!
!     Fish_3 location
      IF( USE_FISH_3 ) THEN
        CLFISH_3 = ESD_LOC(ESD_LOC(ILOC)%FISH_3)%ID
      ELSE
        CLFISH_3 = '-None-'
      END IF
!
!     Meat location
      IF( USE_MEAT ) THEN
        CLMEAT = ESD_LOC(ESD_LOC(ILOC)%MEAT)%ID
      ELSE
        CLMEAT = '-None-'
      END IF
!
!     Ground water location
      CALL GET_INDEX( ITMP, ILOC, IGWAT, IDXGWAT, IPLACE, IERR )
      IF( IDXGWAT .GT. 0 ) THEN
        CLGWAT = 'Error-'
        IF( IPLACE .EQ. 1 ) CLGWAT = ESD_LOC(ILOC)%ID
        IF( IPLACE .EQ. 2 ) CLGWAT = ESD_LOC(ESD_LOC(ILOC)%SECOND)%ID
        IF( IPLACE .EQ. 3 ) CLGWAT = ESD_LOC(ESD_LOC(ILOC)%THIRD)%ID
      ELSE
        CLGWAT = '-None-'
      END IF
!
!     Soil (dry) location
      CALL GET_INDEX( ITMP, ILOC, ISODR, IDXSODR, IPLACE, IERR )
      IF( IDXSODR .GT. 0 ) THEN
        CLSOGW = 'Error-'
        IF( IPLACE .EQ. 1 ) CLSODR = ESD_LOC(ILOC)%ID
        IF( IPLACE .EQ. 2 ) CLSODR = ESD_LOC(ESD_LOC(ILOC)%SECOND)%ID
        IF( IPLACE .EQ. 3 ) CLSODR = ESD_LOC(ESD_LOC(ILOC)%THIRD)%ID
      ELSE
        CLSODR = '-None-'
      END IF
!
!     Soil (groundwater irrigation) location
      CALL GET_INDEX( ITMP, ILOC, ISOGW, IDXSOGW, IPLACE, IERR )
      IF( IDXSOGW .GT. 0 ) THEN
        CLSOGW = 'Error-'
        IF( IPLACE .EQ. 1 ) CLSOGW = ESD_LOC(ILOC)%ID
        IF( IPLACE .EQ. 2 ) CLSOGW = ESD_LOC(ESD_LOC(ILOC)%SECOND)%ID
        IF( IPLACE .EQ. 3 ) CLSOGW = ESD_LOC(ESD_LOC(ILOC)%THIRD)%ID
      ELSE
        CLSOGW = '-None-'
      END IF
!
!     Soil (surface water irrigation) location
      CALL GET_INDEX( ITMP, ILOC, ISOSW, IDXSOSW, IPLACE, IERR )
      IF( IDXSOSW .GT. 0 ) THEN
        CLSOSW = 'Error-'
        IF( IPLACE .EQ. 1 ) CLSOSW = ESD_LOC(ILOC)%ID
        IF( IPLACE .EQ. 2 ) CLSOSW = ESD_LOC(ESD_LOC(ILOC)%SECOND)%ID
        IF( IPLACE .EQ. 3 ) CLSOSW = ESD_LOC(ESD_LOC(ILOC)%THIRD)%ID
      ELSE
        CLSOSW = '-None-'
      END IF
!
!     Seep location
      CALL GET_INDEX( ITMP, ILOC, ISEEP, IDXSEEP, IPLACE, IERR )
      IF( IDXSEEP .GT. 0 ) THEN
        CLSEEP = 'Error-'
        IF( IPLACE .EQ. 1 ) CLSEEP = ESD_LOC(ILOC)%ID
        IF( IPLACE .EQ. 2 ) CLSEEP = ESD_LOC(ESD_LOC(ILOC)%SECOND)%ID
        IF( IPLACE .EQ. 3 ) CLSEEP = ESD_LOC(ESD_LOC(ILOC)%THIRD)%ID
      ELSE
        CLSEEP = '-None-'
      END IF
!
!     Riparian soil location
      CALL GET_INDEX( ITMP, ILOC, ISORP, IDXSORP, IPLACE, IERR )
      IF( IDXSORP .GT. 0 ) THEN
        CLSORP = 'Error-'
        IF( IPLACE .EQ. 1 ) CLSORP = ESD_LOC(ILOC)%ID
        IF( IPLACE .EQ. 2 ) CLSORP = ESD_LOC(ESD_LOC(ILOC)%SECOND)%ID
        IF( IPLACE .EQ. 3 ) CLSORP = ESD_LOC(ESD_LOC(ILOC)%THIRD)%ID
      ELSE
        CLSORP = '-None-'
      END IF
!
!     Surface water location
      CALL GET_INDEX( ITMP, ILOC, ISWAT, IDXSWAT, IPLACE, IERR )
      IF( IDXSWAT .GT. 0 ) THEN
        CLSWAT = 'Error-'
        IF( IPLACE .EQ. 1 ) CLSWAT = ESD_LOC(ILOC)%ID
        IF( IPLACE .EQ. 2 ) CLSWAT = ESD_LOC(ESD_LOC(ILOC)%SECOND)%ID
        IF( IPLACE .EQ. 3 ) CLSWAT = ESD_LOC(ESD_LOC(ILOC)%THIRD)%ID
      ELSE
        CLSWAT = '-None-'
      END IF
!
!     Sediment location
      CALL GET_INDEX( ITMP, ILOC, ISEDI, IDXSEDI, IPLACE, IERR )
      IF( IDXSEDI .GT. 0 ) THEN
        CLSEDI = 'Error-'
        IF( IPLACE .EQ. 1 ) CLSEDI = ESD_LOC(ILOC)%ID
        IF( IPLACE .EQ. 2 ) CLSEDI = ESD_LOC(ESD_LOC(ILOC)%SECOND)%ID
        IF( IPLACE .EQ. 3 ) CLSEDI = ESD_LOC(ESD_LOC(ILOC)%THIRD)%ID
      ELSE
        CLSEDI = '-None-'
      END IF
!
!     Air concentration location
      CALL GET_INDEX( ITMP, ILOC, IAIRC, IDXAIRC, IPLACE, IERR )
      IF( IDXAIRC .GT. 0 ) THEN
        CLAIRC = 'Error-'
        IF( IPLACE .EQ. 1 ) CLAIRC = ESD_LOC(ILOC)%ID
        IF( IPLACE .EQ. 2 ) CLAIRC = ESD_LOC(ESD_LOC(ILOC)%SECOND)%ID
        IF( IPLACE .EQ. 3 ) CLAIRC = ESD_LOC(ESD_LOC(ILOC)%THIRD)%ID
      ELSE
        CLAIRC = '-None-'
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE HAZ_DOSE( ITIM, ILOC, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine computes the risk from hazardous analytes for all analytes in a single
!!    location for a single time and outputs the results.
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Oct 1996 : Version 1.0
!!    Paul W. Eslinger : 10 Apr 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Sep 2002 : Change debug formats
!!    Paul W. Eslinger : 15 Mar 2004 : Fix ingestion dose logic
!!    Paul W. Eslinger : 21 Sep 2004 : Set CSWEAT concentrations
!!    Paul W. Eslinger :  3 Mar 2005 : SCR-1073 Add ETGROUND
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!    Paul W. Eslinger :  2 Dec 2005 : Rearrange debug outputs
!!    Paul W. Eslinger : 28 Feb 2006 : SCR-1107 - Add dermal to sweat lodge
!!                                     Add explicit shower exposure
!!                                     Expand to both dose and risk solutions
!!    Paul W. Eslinger : 13 Apr 2006 : SCR-1119 - Fix output flags
!!    Paul W. Eslinger : 30 Oct 2006 : SCR-1148 Fix averaging time variables
!!
!!**************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Param_Mod
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Concen_Mod
      USE Stocvars_Mod
      USE Results_Mod
      USE Errors_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: PRINTRISK
      REAL, EXTERNAL :: PERCENT
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER :: IERR             ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'HAZ_DOSE'
      CHARACTER(LEN=160) :: CTMP ! Local character variable
      INTEGER :: LEN_CTMP         ! Length (nonblanks) of CTMP
!
      REAL :: XMIN ! Statistics - minimum
      REAL :: XV01 ! Statistics -  1st percentile
      REAL :: XV05 ! Statistics -  5th percentile
      REAL :: XV10 ! Statistics - 10th percentile
      REAL :: XV25 ! Statistics - 25th percentile
      REAL :: XMED ! Statistics - median
      REAL :: XV75 ! Statistics - 75th percentile
      REAL :: XV90 ! Statistics - 90th percentile
      REAL :: XV95 ! Statistics - 95th percentile
      REAL :: XV99 ! Statistics - 99th percentile
      REAL :: XMAX ! Statistics - maximum
      REAL :: XAVG ! Statistics - average
      REAL :: XSTD ! Statistics - standard deviation
!
      INTEGER :: IREL ! Realization looping variable
      INTEGER :: IANA ! Analyte looping variable
      INTEGER :: RACTION ! Risk action flag for the print routine
!
      REAL :: TMP_ING_WATER          ! Water hazard increment
      REAL :: TMP_ING_SWAT           ! Water hazard increment - surface water
      REAL :: TMP_ING_GWAT           ! Water hazard increment - ground water
      REAL :: TMP_ING_SEEP           ! Water hazard increment - seeps
      REAL :: TMP_ING_SOIL           ! Soil hazard increment
      REAL :: TMP_ING_FOOD           ! Food hazard increment
      REAL :: TMP_ING_LEAFVEG        ! Leafy vegetable hazard increment
      REAL :: TMP_ING_ROOTVEG        ! Root vegetable hazard increment
      REAL :: TMP_ING_MEAT           ! Meat hazard increment
      REAL :: TMP_ING_MILK           ! Milk hazard increment
      REAL :: TMP_ING_BIRD           ! Bird hazard increment
      REAL :: TMP_ING_FRUIT          ! Fruit hazard increment
      REAL :: TMP_ING_GRAIN          ! Grain hazard increment
      REAL :: TMP_ING_EGGS           ! Eggs hazard increment
      REAL :: TMP_ING_FISH           ! Fish hazard increment
      REAL :: TMP_ING_FISH_2         ! Fish_3 hazard increment
      REAL :: TMP_ING_FISH_3         ! Fish_3 hazard increment
      REAL :: TMP_ING_SEDIMENT       ! Sediment hazard increment
      REAL :: TMP_ING_SEDIMENT_CHILD ! Sediment hazard increment - child
      REAL :: TMP_ING_SEDIMENT_ADULT ! Sediment hazard increment - adult
      REAL :: TMP_ING_SOIL_CHILD     ! Soil hazard increment - child
      REAL :: TMP_ING_SOIL_ADULT     ! Soil hazard increment - child
      REAL :: TMP_ING_DIRT           ! Soil/sediment hazard increment
!
      REAL :: TMP_INH_SOIL   ! Hazard increment from air mass loading of soil
      REAL :: TMP_INH_SEEP   ! Hazard increment from seep water in air
      REAL :: TMP_INH_SWAT   ! Hazard increment from surface water in air
      REAL :: TMP_INH_SWEAT  ! Hazard increment from sweat lodge (water in air)
      REAL :: TMP_INH_SHOWER ! Hazard increment from shower (water in air)
      REAL :: TMP_INH_AIRC   ! Hazard increment from farfield air concentrations
!
      REAL :: TMP_CON_INH_SOIL   ! Concentration for air mass loading of soil
      REAL :: TMP_CON_INH_SEEP   ! Concentration for seep water in air
      REAL :: TMP_CON_INH_SWAT   ! Concentration for surface water in air
      REAL :: TMP_CON_INH_SHOWER ! Concentration for water in a shower
      REAL :: TMP_CON_INH_SWEAT  ! Concentration for sweat lodge air
      REAL :: TMP_CON_INH_AIRC   ! Concentration for farfield air concentrations
!
      REAL :: TMP_DER_SOIL   ! Hazard increment from dermal contact with soil
      REAL :: TMP_DER_SEDI   ! Hazard increment from dermal contact with sediment
      REAL :: TMP_DER_SEEP   ! Hazard increment from dermal contact with seep water
      REAL :: TMP_DER_SWIM   ! Hazard increment from dermal contact with swimming water
      REAL :: TMP_DER_SWEAT  ! Hazard increment from dermal contact with sweat lodge water
      REAL :: TMP_DER_SHOWER ! Hazard increment from dermal contact with shower water
!
      LOGICAL :: ANYRISK ! Flag if any hazardour risk is requested
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
      RACTION = 2
!
      IF( BGHAZ ) THEN
        WRITE(IRPT,1000) 'Entering ' // CALLER
 1000   FORMAT(/A)
      END IF
!
! *** Check on the validity of ITIM
!
      IF( ITIM.LT.1 .OR. ITIM.GT.ESD_NUM_TIM ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid time index'
        MESSAG(2) = 'Value = '
        WRITE(MESSAG(2)(9:),*) ITIM
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Check on the validity of ILOC
!
      IF( ILOC.LT.1 .OR. ILOC.GT.ESD_NUM_LOC ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid location index'
        MESSAG(2) = 'Value = '
        WRITE(MESSAG(2)(9:),*) ILOC
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** More debug writes
!
      IF( BGHAZ ) THEN
!
        CTMP = 'Location index '
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ILOC
        CTMP = TRIM(CTMP) // ' : '//ESD_LOC(ILOC)%ID
        WRITE(IRPT,1010) TRIM(CTMP)
 1010   FORMAT(A)
!
        CTMP = 'Time index '
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+1):),'(I0)') ITIM
        CTMP = TRIM(CTMP) // ' : Calendar year'
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ESD_TIM(ITIM)%TIME
        WRITE(IRPT,1010) TRIM(CTMP)
!
        WRITE(IRPT,1020) NREAL, 'Numer of realizations to be processed'
 1020   FORMAT(3X,I0,' : ',A)
!
        WRITE(IRPT,1030) AVGTIME_HAZ, 'AVGTIME_HAZ'
        WRITE(IRPT,1030) CF3, 'CF3'
        WRITE(IRPT,1030) CF4, 'CF4'
        WRITE(IRPT,1030) CF7, 'CF7'
 1030   FORMAT(3X,1P,E12.5,' : ',A)
!
      END IF
!
! *** Initialize the hazardous dose and risk to zero for this location
!
      FORALL (IREL=1:NREAL) RISKHAZ(IREL) = 0.0
      FORALL (IREL=1:NREAL) DOSEHAZ(IREL) = 0.0
!
! *** Fill the soil concentrations to match the location and food types
!
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'AQUATIC' ) FORALL (IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = 0.0
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) FORALL (IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SORP(IANA,IREL)
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
        IF( SOIL_IDX .EQ. 1 ) FORALL (IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SODR(IANA,IREL) !Dry location
        IF( SOIL_IDX .EQ. 2 ) FORALL (IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SOGW(IANA,IREL) !Groundwater irrigation
        IF( SOIL_IDX .EQ. 3 ) FORALL (IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SOSW(IANA,IREL) !Surface water irrigation
      END IF
!
! *** Fill the sweat lodge water concentrations
!       Default to zero if the sweat lodge option is not used
      CSWEAT = 0.0
      IF( SWEAT_SOURCE .EQ. 'GWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSWEAT(IANA,IREL) = GWAT(IANA,IREL)
      IF( SWEAT_SOURCE .EQ. 'SWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSWEAT(IANA,IREL) = SWAT(IANA,IREL)
      IF( SWEAT_SOURCE .EQ. 'SEEP' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSWEAT(IANA,IREL) = SEEP(IANA,IREL)
!
! *** Fill the shower water concentrations
!       Default to zero if the showering option is not used
      CSHOWER = 0.0
      IF( SHOWER_SOURCE .EQ. 'GWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSHOWER(IANA,IREL) = GWAT(IANA,IREL)
      IF( SHOWER_SOURCE .EQ. 'SWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSHOWER(IANA,IREL) = SWAT(IANA,IREL)
      IF( SHOWER_SOURCE .EQ. 'SEEP' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSHOWER(IANA,IREL) = SEEP(IANA,IREL)
!
! *** Loop over analytes
!
      ANYRISK = .FALSE.
      DO IANA = 1, ESD_NUM_ANA
!
        IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE     ! Skip analytes not requested
        IF( .NOT.ESD_ANA(IANA)%ANATYP_HAZ ) CYCLE ! Only use hazardous analytes
!
!       Flag for output of the risk sum over analytes
        IF( ESD_ANA(IANA)%RFDING .OR. ESD_ANA(IANA)%RFDINH ) ANYRISK = .TRUE.
!
        DO IREL = 1, NREAL
!
! ***     Ingestion - water
          TMP_ING_SWAT = SWAT(IANA,IREL) * INGEST(IREL)%IRSURFACE  * EXPOS(IREL)%ED * EXPOS(IREL)%EFSURFACE * CF7 / &
            (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_GWAT = GWAT(IANA,IREL) * INGEST(IREL)%IRGROUND   * EXPOS(IREL)%ED * EXPOS(IREL)%EFGROUND  * CF7 / &
            (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_SEEP = SEEP(IANA,IREL) * INGEST(IREL)%IRSEEP     * EXPOS(IREL)%ED * EXPOS(IREL)%EFSEEP    * CF7 / &
            (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_WATER = TMP_ING_SWAT + TMP_ING_GWAT + TMP_ING_SEEP
!
! ***     Ingestion - soil
          TMP_ING_SOIL_CHILD = CSOIL(IANA,IREL) * INGEST(IREL)%IRSOILCHILD * EXPOS(IREL)%EDCHILD * EXPOS(IREL)%EFSOIL * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWCHILD)
          TMP_ING_SOIL_ADULT = CSOIL(IANA,IREL) * INGEST(IREL)%IRSOILADULT * EXPOS(IREL)%EDADULT * EXPOS(IREL)%EFSOIL * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_SOIL  = TMP_ING_SOIL_CHILD + TMP_ING_SOIL_ADULT
!
! ***     Ingestion - sediment
          TMP_ING_SEDIMENT_CHILD = SEDI(IANA,IREL) * INGEST(IREL)%IRSEDCHILD * EXPOS(IREL)%EDCHILD * EXPOS(IREL)%EFSED * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWCHILD)
          TMP_ING_SEDIMENT_ADULT = SEDI(IANA,IREL) * INGEST(IREL)%IRSEDADULT * EXPOS(IREL)%EDADULT * EXPOS(IREL)%EFSED * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_SEDIMENT = TMP_ING_SEDIMENT_CHILD + TMP_ING_SEDIMENT_ADULT
!
          TMP_ING_DIRT = TMP_ING_SOIL + TMP_ING_SEDIMENT
!
! ***     Ingestion - food
          TMP_ING_LEAFVEG = CLEAFVEG(IANA,IREL) * INGEST(IREL)%IRLEAFVEG * EXPOS(IREL)%ED  * EXPOS(IREL)%EFLEAFVEG * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_ROOTVEG = CROOTVEG(IANA,IREL)  * INGEST(IREL)%IRROOTVEG  * EXPOS(IREL)%ED  * EXPOS(IREL)%EFROOTVEG  * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_MEAT    = CMEAT(IANA,IREL)  * INGEST(IREL)%IRMEAT  * EXPOS(IREL)%ED  * EXPOS(IREL)%EFMEAT   * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_MILK    = CMILK(IANA,IREL)  * INGEST(IREL)%IRMILK  * EXPOS(IREL)%ED  * EXPOS(IREL)%EFMILK   * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_BIRD    = CBIRD(IANA,IREL)  * INGEST(IREL)%IRBIRD  * EXPOS(IREL)%ED  * EXPOS(IREL)%EFBIRD   * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_FRUIT   = CFRUIT(IANA,IREL)  * INGEST(IREL)%IRFRUIT  * EXPOS(IREL)%ED  * EXPOS(IREL)%EFFRUIT  * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_GRAIN   = CGRAIN(IANA,IREL)  * INGEST(IREL)%IRGRAIN  * EXPOS(IREL)%ED  * EXPOS(IREL)%EFGRAIN  * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_EGGS    = CEGGS(IANA,IREL)  * INGEST(IREL)%IREGGS  * EXPOS(IREL)%ED  * EXPOS(IREL)%EFEGGS  * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_FISH    = CFISH(IANA,IREL)  * INGEST(IREL)%IRFISH  * EXPOS(IREL)%ED  * EXPOS(IREL)%EFFISH   * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_FISH_2  = CFISH_2(IANA,IREL)* INGEST(IREL)%IRFISH_2* EXPOS(IREL)%ED  * EXPOS(IREL)%EFFISH_2 * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_FISH_3  = CFISH_3(IANA,IREL)* INGEST(IREL)%IRFISH_3* EXPOS(IREL)%ED  * EXPOS(IREL)%EFFISH_3 * CF7 / &
              (AVGTIME_HAZ*BODY(IREL)%BWADULT)
          TMP_ING_FOOD  = TMP_ING_LEAFVEG + TMP_ING_ROOTVEG + TMP_ING_MEAT + TMP_ING_MILK + TMP_ING_BIRD + TMP_ING_FRUIT + &
              TMP_ING_GRAIN + TMP_ING_EGGS + TMP_ING_FISH + TMP_ING_FISH_2 + TMP_ING_FISH_3
!
! ***     Total ingestion dose
          ING_INC_DOS(IREL) = TMP_ING_WATER + TMP_ING_DIRT + TMP_ING_FOOD
!
! ***     Inhalation - soil
          TMP_INH_SOIL = CSOIL(IANA,IREL) * ML(IREL) * EXPOS(IREL)%ETSOIL* EXPOS(IREL)%EFSOIL * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE  * CF7 / (AVGTIME_HAZ * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_SOIL = CSOIL(IANA,IREL) * ML(IREL)
!
! ***     Inhalation - seep water (outdoor)
          TMP_INH_SEEP = SEEP(IANA,IREL) * VF(IANA,IREL) * EXPOS(IREL)%ETSEEP * EXPOS(IREL)%EFSEEP * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_HAZ * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_SEEP = SEEP(IANA,IREL) * VF(IANA,IREL)
!
! ***     Inhalation - surface water (outdoor)
          TMP_INH_SWAT = SWAT(IANA,IREL) * VF(IANA,IREL) * EXPOS(IREL)%ETSURFACE * EXPOS(IREL)%EFSURFACE * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_HAZ * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_SWAT = SWAT(IANA,IREL) * VF(IANA,IREL)
!
! ***     Inhalation - showering water (indoor)
          TMP_INH_SHOWER = CSHOWER(IANA,IREL) * CFSHOWER(IREL) * EXPOS(IREL)%ETSHOWER * EXPOS(IREL)%EFSHOWER * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_HAZ * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_SHOWER = CSHOWER(IANA,IREL) * CFSHOWER(IREL)
!
! ***     Inhalation - sweat lodge
          TMP_INH_SWEAT = CSWEAT(IANA,IREL) * CFSWEAT(IREL) * EXPOS(IREL)%ETSWEAT * EXPOS(IREL)%EFSWEAT * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_HAZ * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_SWEAT = CSWEAT(IANA,IREL) * CFSWEAT(IREL)
!
! ***     Inhalation - air
          TMP_INH_AIRC = AIRC(IANA,IREL) * EXPOS(IREL)%ETAIR * EXPOS(IREL)%EFAIR * EXPOS(IREL)%ED * &
              INGEST(IREL)%IRATE * CF7 / (AVGTIME_HAZ * BODY(IREL)%BWADULT * CF4 )
          TMP_CON_INH_AIRC = AIRC(IANA,IREL)
!
! ***     Total inhalation dose
          INH_INC_DOS(IREL) = TMP_INH_SOIL + TMP_INH_SEEP + TMP_INH_SWAT + TMP_INH_SHOWER + TMP_INH_SWEAT + TMP_INH_AIRC
!
! ***     Dermal - soil
          TMP_DER_SOIL = CSOIL(IANA,IREL) * AFSOIL(IREL) * ABSORP(IANA,IREL) * BODY(IREL)%SASOIL * EXPOS(IREL)%EFSOIL * &
              CF1 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT*AVGTIME_HAZ )
!
! ***     Dermal - sediment
          TMP_DER_SEDI = SEDI(IANA,IREL) * AFSED(IREL) * ABSORP(IANA,IREL) * BODY(IREL)%SASED * EXPOS(IREL)%EFSED * &
            CF1 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT*AVGTIME_HAZ )
!
! ***     Dermal - seep
          TMP_DER_SEEP = SEEP(IANA,IREL) * KP(IANA,IREL) * BODY(IREL)%SASEEP * EXPOS(IREL)%ETSEEP * EXPOS(IREL)%EFSEEP * &
            CF3 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT*AVGTIME_HAZ )
!
! ***     Dermal - swimming
          TMP_DER_SWIM = SWAT(IANA,IREL) * KP(IANA,IREL) * BODY(IREL)%SASWIM * EXPOS(IREL)%ETSWIM * EXPOS(IREL)%EFSWIM * &
            CF3 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT*AVGTIME_HAZ )
!
! ***     Dermal - sweat lodge
          TMP_DER_SWEAT = CSWEAT(IANA,IREL) * KP(IANA,IREL) * BODY(IREL)%SASWEAT * EXPOS(IREL)%ETSWEAT * EXPOS(IREL)%EFSWEAT * &
            CF3 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT*AVGTIME_HAZ )
!
! ***     Dermal - showering
          TMP_DER_SHOWER = CSHOWER(IANA,IREL) * KP(IANA,IREL) * BODY(IREL)%SASHOWER * EXPOS(IREL)%ETSHOWER * EXPOS(IREL)%EFSHOWER* &
            CF3 * EXPOS(IREL)%ED * CF7 / ( BODY(IREL)%BWADULT*AVGTIME_HAZ )
!
! ***     Total dermal dose
          EXT_INC_DOS(IREL) = TMP_DER_SOIL + TMP_DER_SEDI + TMP_DER_SEEP + TMP_DER_SWIM + TMP_DER_SWEAT + TMP_DER_SHOWER
!
! ***     Dose and hazard quotient increment for this analyte
          DOSEINC(IREL) = ING_INC_DOS(IREL) + INH_INC_DOS(IREL) + EXT_INC_DOS(IREL)
!
          RISKINC(IREL) = 0.0
          IF(ESD_ANA(IANA)%RFDING) RISKINC(IREL) = RISKINC(IREL) + ING_INC_DOS(IREL)/RFDING(IANA,IREL) + &
                                                   EXT_INC_DOS(IREL)/RFDING(IANA,IREL)
          IF(ESD_ANA(IANA)%RFDINH) RISKINC(IREL) = RISKINC(IREL) + INH_INC_DOS(IREL)/RFDINH(IANA,IREL)
!
! ***     Increment the dose and risk for all analytes
          DOSEHAZ(IREL) = DOSEHAZ(IREL) + DOSEINC(IREL)
          IF(ESD_ANA(IANA)%RFDING) RISKHAZ(IREL) = RISKHAZ(IREL) + ING_INC_DOS(IREL)/RFDING(IANA,IREL) + &
                                                   EXT_INC_DOS(IREL)/RFDING(IANA,IREL)
          IF(ESD_ANA(IANA)%RFDINH) RISKHAZ(IREL) = RISKHAZ(IREL) + INH_INC_DOS(IREL)/RFDINH(IANA,IREL)
!
          IF( BGHAZ .OR. ESD_ANA(IANA)%VERBOSE .OR. ESD_ANA(IANA)%TERSE ) THEN
            WRITE(IRPT,1000) 'Processing Location    : ' // ESD_LOC(ILOC)%ID // ' : "' // TRIM(ESD_LOC(ILOC)%NAME) // '"'
            WRITE(IRPT,1010) 'Processing Analyte     : ' // ESD_ANA(IANA)%ID // ' : "' // TRIM(ESD_ANA(IANA)%NAME) // '"'
            CTMP = 'Processing realization : '
            WRITE(CTMP(26:),'(I0)') IREL
            WRITE(IRPT,1010) TRIM(CTMP)
          END IF
!
          IF( BGHAZ .OR. ESD_ANA(IANA)%VERBOSE ) THEN
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Dose and hazard quotient increments - ingestion pathway for (hazardous) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Pathway           Dose                  Hazard Quotient   Ingestion           '//&
              ' Concentration        Exposure Freq.       Food Definition'
            WRITE(IRPT,1010) '   ----------------- --------------------- ----------------- --------------------'//&
              ' -------------------- -------------------  -------------------------------------------'
!
            WRITE(IRPT,1040) 'Surface water   ', TMP_ING_SWAT,           'mg/kg-day', &
              PRINTRISK(TMP_ING_SWAT,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),           'none', &
              INGEST(IREL)%IRSURFACE,   'L/day  ', SWAT(IANA,IREL),     'ug/L   ', EXPOS(IREL)%EFSURFACE, 'day/yr  '//LABEL_SURFACE
            WRITE(IRPT,1040) 'Ground water    ', TMP_ING_GWAT,           'mg/kg-day', &
              PRINTRISK(TMP_ING_GWAT,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),           'none', &
              INGEST(IREL)%IRGROUND,    'L/day  ', GWAT(IANA,IREL),     'ug/L   ', EXPOS(IREL)%EFGROUND,  'day/yr  '//LABEL_GROUND
            WRITE(IRPT,1040) 'Seep water      ', TMP_ING_SEEP,           'mg/kg-day', &
              PRINTRISK(TMP_ING_SEEP,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),           'none', &
              INGEST(IREL)%IRSEEP,      'L/day  ', SEEP(IANA,IREL),     'ug/L   ', EXPOS(IREL)%EFSEEP,    'day/yr  '//LABEL_SEEP
            WRITE(IRPT,1040) 'Soil (child)    ', TMP_ING_SOIL_CHILD,     'mg/kg-day', &
              PRINTRISK(TMP_ING_SOIL_CHILD,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),     'none', &
              INGEST(IREL)%IRSOILCHILD, 'kg/day ', CSOIL(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFSOIL,    'day/yr  '//&
              LABEL_SOIL_CHILD
            WRITE(IRPT,1040) 'Soil (adult)    ', TMP_ING_SOIL_ADULT,     'mg/kg-day', &
              PRINTRISK(TMP_ING_SOIL_ADULT,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),     'none', &
              INGEST(IREL)%IRSOILADULT, 'kg/day ', CSOIL(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFSOIL,    'day/yr  '//&
              LABEL_SOIL_ADULT
            WRITE(IRPT,1040) 'Sediment (child)', TMP_ING_SEDIMENT_CHILD, 'mg/kg-day', &
              PRINTRISK(TMP_ING_SEDIMENT_CHILD,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION), 'none', &
              INGEST(IREL)%IRSEDCHILD,  'kg/day ', SEDI(IANA,IREL),     'ug/kg  ', EXPOS(IREL)%EFSED,     'day/yr  '//&
              LABEL_SEDIMENT_CHILD
            WRITE(IRPT,1040) 'Sediment (adult)', TMP_ING_SEDIMENT_ADULT, 'mg/kg-day', &
              PRINTRISK(TMP_ING_SEDIMENT_ADULT,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION), 'none', &
              INGEST(IREL)%IRSEDADULT,  'kg/day ', SEDI(IANA,IREL),     'ug/kg  ', EXPOS(IREL)%EFSED,     'day/yr  '//&
              LABEL_SEDIMENT_ADULT
            WRITE(IRPT,1040) 'Leafy Vegetables', TMP_ING_LEAFVEG,        'mg/kg-day', &
              PRINTRISK(TMP_ING_LEAFVEG,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),        'none', &
              INGEST(IREL)%IRLEAFVEG,   'kg/day ', CLEAFVEG(IANA,IREL), 'ug/kg  ', EXPOS(IREL)%EFLEAFVEG, 'day/yr  '//LABEL_LEAFVEG
            WRITE(IRPT,1040) 'Root Vegetables ', TMP_ING_ROOTVEG,        'mg/kg-day', &
              PRINTRISK(TMP_ING_ROOTVEG,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),        'none', &
              INGEST(IREL)%IRROOTVEG,   'kg/day ', CROOTVEG(IANA,IREL), 'ug/kg  ', EXPOS(IREL)%EFROOTVEG, 'day/yr  '//LABEL_ROOTVEG
            WRITE(IRPT,1040) 'Meat            ', TMP_ING_MEAT,           'mg/kg-day', &
              PRINTRISK(TMP_ING_MEAT,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),           'none', &
              INGEST(IREL)%IRMEAT,      'kg/day ', CMEAT(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFMEAT,    'day/yr  '//LABEL_MEAT
            WRITE(IRPT,1040) 'Milk            ', TMP_ING_MILK,           'mg/kg-day', &
              PRINTRISK(TMP_ING_MILK,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),           'none', &
              INGEST(IREL)%IRMILK,      'L/day  ', CMILK(IANA,IREL),    'ug/L   ', EXPOS(IREL)%EFMILK,    'day/yr  '//LABEL_MILK
            WRITE(IRPT,1040) 'Bird            ', TMP_ING_BIRD,           'mg/kg-day', &
              PRINTRISK(TMP_ING_BIRD,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),           'none', &
              INGEST(IREL)%IRBIRD,      'kg/day ', CBIRD(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFBIRD,    'day/yr  '//LABEL_BIRD
            WRITE(IRPT,1040) 'Fruit           ', TMP_ING_FRUIT,          'mg/kg-day', &
              PRINTRISK(TMP_ING_FRUIT,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),          'none', &
              INGEST(IREL)%IRFRUIT,     'kg/day ', CFRUIT(IANA,IREL),   'ug/kg  ', EXPOS(IREL)%EFFRUIT,   'day/yr  '//LABEL_FRUIT
            WRITE(IRPT,1040) 'Grain           ', TMP_ING_GRAIN,          'mg/kg-day', &
              PRINTRISK(TMP_ING_GRAIN,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),          'none', &
              INGEST(IREL)%IRGRAIN,     'kg/day ', CGRAIN(IANA,IREL),   'ug/kg  ', EXPOS(IREL)%EFGRAIN,   'day/yr  '//LABEL_GRAIN
            WRITE(IRPT,1040) 'Eggs            ', TMP_ING_EGGS,           'mg/kg-day', &
              PRINTRISK(TMP_ING_EGGS,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),           'none', &
              INGEST(IREL)%IREGGS,      'kg/day ', CEGGS(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFEGGS,    'day/yr  '//LABEL_EGGS
            WRITE(IRPT,1040) 'Fish            ', TMP_ING_FISH,           'mg/kg-day', &
              PRINTRISK(TMP_ING_FISH,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),           'none', &
              INGEST(IREL)%IRFISH,      'kg/day ', CFISH(IANA,IREL),    'ug/kg  ', EXPOS(IREL)%EFFISH,    'day/yr  '//LABEL_FISH
            WRITE(IRPT,1040) 'Fish_2          ', TMP_ING_FISH_2,         'mg/kg-day', &
              PRINTRISK(TMP_ING_FISH_2,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),         'none', &
              INGEST(IREL)%IRFISH_2,    'kg/day ', CFISH_2(IANA,IREL),  'ug/kg  ', EXPOS(IREL)%EFFISH_2,  'day/yr  '//LABEL_FISH_2
            WRITE(IRPT,1040) 'Fish_3          ', TMP_ING_FISH_3,         'mg/kg-day', &
              PRINTRISK(TMP_ING_FISH_3,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),         'none', &
              INGEST(IREL)%IRFISH_3,    'kg/day ', CFISH_3(IANA,IREL),  'ug/kg  ', EXPOS(IREL)%EFFISH_3,  'day/yr  '//LABEL_FISH_3
 1040       FORMAT(3X,A,1X,1P,E12.5,1X,A,8(1X,E12.5,1X,A))
!
            WRITE(IRPT,1010) ' '
            CTMP = 'Inhalation rate (m^3/day) is '
            WRITE(CTMP(30:),'(1P,E12.5)') INGEST(IREL)%IRATE
            WRITE(IRPT,1010) 'Dose and hazard quotient increments - inhalation pathway for (hazardous) '//TRIM(ESD_ANA(IANA)%ID)//&
              '   ['//TRIM(CTMP)//']'
            WRITE(IRPT,1010) '   Pathway                    Dose                   Hazard Quotient   Concentration       '//&
              ' Exposure Time       Exposure Frequency   Loading or Volatile'
            WRITE(IRPT,1010) '   -------------------------- ---------------------- ----------------- ------------------- '//&
              ' ------------------- -------------------- ------------------- '
            WRITE(IRPT,1040) 'Inhalation - Soil         ', TMP_INH_SOIL  , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,RFDINH(IANA,IREL),ESD_ANA(IANA)%RFDINH,RACTION),   'none', &
              TMP_CON_INH_SOIL,   'ug/m^3 ',&
              EXPOS(IREL)%EFSOIL,   'day/yr', EXPOS(IREL)%ETSOIL,   'hr/day', ML(IREL),       'kg/m^3'
            WRITE(IRPT,1040) 'Inhalation - Seep water   ', TMP_INH_SEEP  , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SEEP,RFDINH(IANA,IREL),ESD_ANA(IANA)%RFDINH,RACTION),   'none', &
              TMP_CON_INH_SEEP,   'ug/m^3 ',&
              EXPOS(IREL)%EFSEEP,   'day/yr', EXPOS(IREL)%ETSEEP,   'hr/day', VF(IANA,IREL),  'L/m^3'
            WRITE(IRPT,1040) 'Inhalation - Surface water', TMP_INH_SWAT  , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SWAT,RFDINH(IANA,IREL),ESD_ANA(IANA)%RFDINH,RACTION),   'none', &
              TMP_CON_INH_SWAT,   'ug/m^3 ',&
              EXPOS(IREL)%EFSURFACE, 'day/yr', EXPOS(IREL)%ETSURFACE,  'hr/day', VF(IANA,IREL),  'L/m^3'
            WRITE(IRPT,1040) 'Inhalation - Shower water ', TMP_INH_SHOWER, 'mg/kg-day', &
              PRINTRISK(TMP_INH_SHOWER,RFDINH(IANA,IREL),ESD_ANA(IANA)%RFDINH,RACTION), 'none', &
              TMP_CON_INH_SHOWER, 'ug/m^3 ',&
              EXPOS(IREL)%EFSHOWER, 'day/yr', EXPOS(IREL)%ETSHOWER, 'hr/day', CFSHOWER(IREL),  'L/m^3'
            WRITE(IRPT,1040) 'Inhalation - Sweat lodge  ', TMP_INH_SWEAT , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SWEAT,RFDINH(IANA,IREL),ESD_ANA(IANA)%RFDINH,RACTION), 'none', &
              TMP_CON_INH_SWEAT,  'ug/m^3 ', &
              EXPOS(IREL)%EFSWEAT,  'day/yr', EXPOS(IREL)%ETSWEAT,  'hr/day', CFSWEAT(IREL),  'L/m^3'
            WRITE(IRPT,1040) 'Inhalation - Air concent. ', TMP_INH_AIRC  , 'mg/kg-day', &
              PRINTRISK(TMP_INH_AIRC,RFDINH(IANA,IREL),ESD_ANA(IANA)%RFDINH,RACTION),   'none', &
              TMP_CON_INH_AIRC,   'ug/m^3 ',&
              EXPOS(IREL)%EFAIR,    'day/yr', EXPOS(IREL)%ETAIR,    'hr/day'
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Dose and hazard quotient increments - (soil) dermal pathway for (hazardous) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Pathway               Dose                   Hazard Quotient  Adherence Factor        '//&
              ' Absorption         Concentration      Surface area      Exposure Freq.'
            WRITE(IRPT,1010) '   -------------------   ---------------------  ---------------- ------------------------'//&
              ' ------------------ ------------------ ----------------- -----------------'
            WRITE(IRPT,1040) 'Dermal - Soil       ', TMP_DER_SOIL, 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION), 'none', AFSOIL(IREL), 'mg/cm^2-day', &
              ABSORP(IANA,IREL), 'none ', CSOIL(IANA,IREL), 'ug/kg', BODY(IREL)%SASOIL, 'cm^2', EXPOS(IREL)%EFSOIL, 'day/yr'
            WRITE(IRPT,1040) 'Dermal - Sediment   ', TMP_DER_SEDI, 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION), 'none', AFSED(IREL),  'mg/cm^2-day', &
              ABSORP(IANA,IREL), 'none ', SEDI(IANA,IREL),  'ug/kg', BODY(IREL)%SASED,  'cm^2', EXPOS(IREL)%EFSED,  'day/yr'
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Dose and hazard quotient increments - (water) dermal pathway for (hazardous) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Pathway               Dose                   Hazard Quotient  Permeability      '//&
              ' Concentration      Surface area      Exposure Duration   Exposure Frequency'
            WRITE(IRPT,1010) '   -------------------   ---------------------  ---------------- ------------------'//&
              ' ------------------ ----------------- ------------------- -------------------'
            WRITE(IRPT,1040) 'Dermal - seep water ', TMP_DER_SEEP , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION), 'none', KP(IANA,IREL),'cm/hr', &
              SEEP(IANA,IREL), 'ug/L ',    BODY(IREL)%SASEEP, 'cm^2', EXPOS(IREL)%ETSEEP,  'hr/day', EXPOS(IREL)%EFSEEP, 'day/yr'
            WRITE(IRPT,1040) 'Dermal - swimming   ', TMP_DER_SWIM , 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION), 'none', KP(IANA,IREL),'cm/hr', &
              SWAT(IANA,IREL), 'ug/L ',    BODY(IREL)%SASWIM,'cm^2',  EXPOS(IREL)%ETSWIM,  'hr/day',  EXPOS(IREL)%EFSWIM, 'day/yr'
            WRITE(IRPT,1040) 'Dermal - sweat lodge', TMP_DER_SWEAT, 'mg/kg-day', &
              PRINTRISK(TMP_INH_SOIL,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION), 'none', KP(IANA,IREL),'cm/hr', &
              CSWEAT(IANA,IREL), 'ug/L ',  BODY(IREL)%SASWEAT,'cm^2', EXPOS(IREL)%ETSWEAT, 'hr/day', EXPOS(IREL)%EFSWEAT, 'day/yr'
          END IF
!
          IF( BGHAZ .OR. ESD_ANA(IANA)%VERBOSE .OR. ESD_ANA(IANA)%TERSE ) THEN
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Summary of dose and hazard quotient increments for (hazardous) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Dose (mg/kg-day) Haz Quotient Percent (dose) Percent (HQ)   Pathway'
            WRITE(IRPT,1010) '   ---------------- ------------ -------------- -------------- ----------------------'
            WRITE(IRPT,1055) TMP_ING_FOOD,  PRINTRISK(TMP_ING_FOOD,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION) ,&
              PERCENT(TMP_ING_FOOD,DOSEINC(IREL)), &
              PERCENT(PRINTRISK(TMP_ING_FOOD,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION) ,RISKINC(IREL)), &
              'Food ingestion'
            WRITE(IRPT,1055) TMP_ING_DIRT,  PRINTRISK(TMP_ING_DIRT,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION) ,&
              PERCENT(TMP_ING_DIRT,DOSEINC(IREL)), &
              PERCENT(PRINTRISK(TMP_ING_DIRT,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION) ,RISKINC(IREL)), &
              'Soil/sediment ingestion'
            WRITE(IRPT,1055) TMP_ING_WATER, PRINTRISK(TMP_ING_WATER,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),&
              PERCENT(TMP_ING_WATER,DOSEINC(IREL)),&
              PERCENT(PRINTRISK(TMP_ING_WATER,RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),RISKINC(IREL)),&
              'Water ingestion'
            WRITE(IRPT,1055) ING_INC_DOS(IREL), PRINTRISK(ING_INC_DOS(IREL),RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),&
              PERCENT(ING_INC_DOS(IREL),DOSEINC(IREL)),&
              PERCENT(PRINTRISK(ING_INC_DOS(IREL),RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),RISKINC(IREL)),&
              'All ingestion'
            WRITE(IRPT,1055) INH_INC_DOS(IREL), PRINTRISK(INH_INC_DOS(IREL),RFDINH(IANA,IREL),ESD_ANA(IANA)%RFDINH,RACTION),&
              PERCENT(INH_INC_DOS(IREL),DOSEINC(IREL)),&
              PERCENT(PRINTRISK(INH_INC_DOS(IREL),RFDINH(IANA,IREL),ESD_ANA(IANA)%RFDINH,RACTION),RISKINC(IREL)),&
              'Inhalation'
            WRITE(IRPT,1055) EXT_INC_DOS(IREL), PRINTRISK(EXT_INC_DOS(IREL),RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),&
              PERCENT(EXT_INC_DOS(IREL),DOSEINC(IREL)),&
              PERCENT(PRINTRISK(EXT_INC_DOS(IREL),RFDING(IANA,IREL),ESD_ANA(IANA)%RFDING,RACTION),RISKINC(IREL)),&
              'Dermal'
            WRITE(IRPT,1056) DOSEINC(IREL), RISKINC(IREL), 'Total'
 1055       FORMAT(3X,1P,E12.5,5X,E12.5,1X,E12.5,3X,E12.5,3X,A)
 1056       FORMAT(3X,1P,E12.5,1X,3X,1X,E12.5,1X,21X,9X,A)
!
          END IF
!
          IF( BGHAZ .OR. ESD_ANA(IANA)%VERBOSE ) THEN
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Reference dose factors for (hazardous) '//TRIM(ESD_ANA(IANA)%ID)
!
            IF( ESD_ANA(IANA)%RFDING ) THEN
              WRITE(IRPT,1030) RFDING(IANA,IREL),  'Ingestion  (mg(intake)/(kg(bodyweight) day))'
            ELSE
              WRITE(IRPT,1010) 'No hazard quotient calculations for ingestion are requested'
            END IF
  !
            IF( ESD_ANA(IANA)%RFDINH ) THEN
              WRITE(IRPT,1030) RFDINH(IANA,IREL),  'Inhalation (mg(intake)/(kg(bodyweight) day))'
            ELSE
              WRITE(IRPT,1010) 'No hazard quotient calculations for inhalation are requested'
            END IF
!
          END IF
!
        END DO ! Realization loop
!
! ***   Compute summary statistics on the dose and hazard quotient increments by pathway
!
        IF( STA_PATHDOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
!
!         Output ingestion dose summary statistics
          CALL USTAT( ING_INC_DOS, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable ING_INC_DOS -> DOSEING'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'DOSEING', 'mg/kg-day', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
!         Output inhalation dose summary statistics
          CALL USTAT( INH_INC_DOS, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable INH_INC_DOS -> DOSEINH'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'DOSEINH', 'mg/kg-day', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
!         Output dermal dose summary statistics
          CALL USTAT( EXT_INC_DOS, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable EXT_INC_DOS -> DOSEDER'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'DOSEDER', 'mg/kg-day', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
        END IF
!
        IF( STA_PATHRISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
!
!         Output ingestion hazard quotient summary statistics
          IF( ESD_ANA(IANA)%RFDING ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = ING_INC_DOS(IREL) / RFDING(IANA,IREL)
            END DO
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable ING_INC*SFING -> CVEC'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'HQING', 'Unitless', &
              XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
!
!         Output inhalation hazard quotient summary statistics
          IF( ESD_ANA(IANA)%RFDINH ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = INH_INC_DOS(IREL) / RFDINH(IANA,IREL)
            END DO
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable INH_INC*SFINH -> CVEC'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'HQINH', 'Unitless', &
              XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
!
!         Output dermal hazard quotient summary statistics
          IF( ESD_ANA(IANA)%RFDING ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = EXT_INC_DOS(IREL) / RFDING(IANA,IREL)
            END DO
            CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
            IF( IERR .NE. 0 ) THEN
              IERR = 999
              MESSAG(1) = 'Error detected for variable EXT_INC*SFING -> CVEC'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'HQDER', 'Unitless', &
              XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
          END IF
!
        END IF
!
! ***   Compute summary statistics on the dose and hazard quotient for this analyte
!
        IF( STA_ANADOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL USTAT( DOSEINC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable DOSEINC -> ANADOSE'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'ANADOSE', 'mg/kg-day', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
        END IF
!
        IF( STA_ANARISK .AND. (ESD_ANA(IANA)%RFDING .OR. ESD_ANA(IANA)%RFDING) &
          .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL USTAT( RISKINC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable RISKINC -> ANAHQ'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'ANAHQ', 'Unitless', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
        END IF
!
! ***   Output detailed values on the dose increments by pathway
!
        IF( DET_PATHDOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'DOSEING', 'mg/kg-day', &
            ING_INC_DOS, NREAL)
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'DOSEINH', 'mg/kg-day', &
            INH_INC_DOS, NREAL)
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'DOSEDER', 'mg/kg-day', &
            EXT_INC_DOS, NREAL)
        END IF
!
! ***   Output detailed values on the risk increments by pathway
        IF( DET_PATHRISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
!
!         Output the ingestion pathway risk
          IF( ESD_ANA(IANA)%SFING ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = ING_INC_DOS(IREL) / RFDING(IANA,IREL)
            END DO
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'HQING', 'Unitless', CVEC, NREAL )
          END IF
!
!         Output the inhalation pathway risk
          IF( ESD_ANA(IANA)%SFINH ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = INH_INC_DOS(IREL) / RFDINH(IANA,IREL)
            END DO
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'HQINH', 'Unitless', CVEC, NREAL )
          END IF
!
!         Output the dermal pathway risk
          IF( ESD_ANA(IANA)%SFING ) THEN
            DO IREL = 1, NREAL
              CVEC(IREL) = EXT_INC_DOS(IREL) / RFDING(IANA,IREL)
            END DO
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'HQDER', 'Unitless', CVEC, NREAL )
          END IF
!
        END IF
!
! ***   Output detailed dose values on this analyte
        IF( DET_ANADOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'ANADOSE', 'mg/kg-day', DOSEINC, NREAL)
        END IF
!
! ***   Output detailed hazard quotient values on this analyte
        IF( DET_ANARISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          IF( ESD_ANA(IANA)%RFDINH .OR. ESD_ANA(IANA)%RFDING ) &
            CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'HAZ', 'ANAHQ', 'Unitless', RISKINC, NREAL )
        END IF
!
      END DO
!
! *** Compute combined summary statistics
!
!     Ouput summary statistics on risk summed over analytes
      IF( STA_SUMRISK .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
        IF( ANYRISK ) THEN
          CALL USTAT( RISKHAZ, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable RISKHAZ -> SUMHQ'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'COMBIN', 'HAZ', 'SUMHQ', 'Unitless', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
        END IF
      END IF
!
! *** Output combined detailed values
!
!     Output detailed values for risk summed over analytes
      IF( DET_SUMRISK .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
        IF( ANYRISK ) THEN
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'COMBIN', 'HAZ', 'SUMHQ', 'Unitless', RISKHAZ, NREAL )
        END IF
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE HUM_MEMORY( IERR )
!!******************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine allocates memory for stochastic variables that depend on the number
!!    of realizations or on the combination of number of analytes and realizations.
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Mar 2000 : Version 2.0
!!    Paul W. Eslinger : 22 Oct 2003 : Remove unused variables
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!    Paul W. Eslinger : 28 Feb 2006 : Add showering pathway, initialize RISKRAD and RISKCAR
!!                                     Add radionuclide risk factors
!!
!!******************************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Control_Mod
      USE Stocvars_Mod
      USE Esd_Mod
      USE Results_Mod
      USE Concen_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR  ! Error flag, nonzero if no error
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'HUM_MEMORY' ! Name of this routine
      INTEGER :: IERA  ! Error status variable from the allocate action
!
!---- Executable code --------------------------------------------------
!
      IERR = 0
!
! *** Check on the number of realizations
!
      IF( NREAL .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'At least 1 realization required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Check on the number of analytes
!
      IF( ESD_NUM_ANA .LT. 1 ) THEN
        IERR = 2
        MESSAG(1) = 'At least 1 analyte required to allocate memory'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Allocate all memory that depends just on realizations
!
      ALLOCATE( AFSED(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating memory for AFSED'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( AFSOIL(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating memory for AFSOIL'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CFSWEAT(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating memory for CFSWEAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CFSHOWER(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating memory for CFSHOWER'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( BODY(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating memory for structure BODY'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( EXPOS(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Error allocating memory for structure EXPOS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( INGEST(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 10
        MESSAG(1) = 'Error allocating memory for structure INGEST'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ML(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 11
        MESSAG(1) = 'Error allocating memory for ML'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( IRGRATE(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 12
        MESSAG(1) = 'Error allocating memory for IRGRATE'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SHIELDSOIL(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 13
        MESSAG(1) = 'Error allocating memory for SHIELDSOIL'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SHIELDSED(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 14
        MESSAG(1) = 'Error allocating memory for SHIELDSED'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( WORK(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 15
        MESSAG(1) = 'Error allocating memory for WORK'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RISKRAD(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 16
        MESSAG(1) = 'Error allocating memory for RISKRAD'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      RISKRAD = 0.0 !Matrix assignment to zero
!
      ALLOCATE( DOSERAD(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 17
        MESSAG(1) = 'Error allocating memory for DOSERAD'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RISKCAR(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 18
        MESSAG(1) = 'Error allocating memory for RISKCAR'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
      RISKCAR = 0.0 !Matrix assignment to zero
!
      ALLOCATE( DOSECAR(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 19
        MESSAG(1) = 'Error allocating memory for RISKCAR'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RISKHAZ(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 20
        MESSAG(1) = 'Error allocating memory for RISKHAZ'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DOSEHAZ(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 21
        MESSAG(1) = 'Error allocating memory for DOSEHAZ'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DOSEINC(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 23
        MESSAG(1) = 'Error allocating memory for DOSEINC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RISKINC(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 24
        MESSAG(1) = 'Error allocating memory for RISKINC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ING_INC_DOS(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 22
        MESSAG(1) = 'Error allocating memory for ING_INC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( INH_INC_DOS(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 25
        MESSAG(1) = 'Error allocating memory for INH_INC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( EXT_INC_DOS(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 26
        MESSAG(1) = 'Error allocating memory for EXT_INC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( POPDOSE(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 27
        MESSAG(1) = 'Error allocating memory for POPDOSE'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( POPRISK(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 28
        MESSAG(1) = 'Error allocating memory for POPRISK'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CVEC(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 29
        MESSAG(1) = 'Error allocating memory for CVEC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Allocate memory that depends on realizations and analytes
!
      ALLOCATE( RFDING(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 30
        MESSAG(1) = 'Error allocating memory for RFDING'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RFDINH(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 31
        MESSAG(1) = 'Error allocating memory for RFDINH'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SFING(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 32
        MESSAG(1) = 'Error allocating memory for SFING'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SFINH(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 33
        MESSAG(1) = 'Error allocating memory for SFINH'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DFSOIL(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 34
        MESSAG(1) = 'Error allocating memory for DFSOIL'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DFSWIM(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 35
        MESSAG(1) = 'Error allocating memory for DFSWIM'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DFBOAT(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 36
        MESSAG(1) = 'Error allocating memory for DFBOAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DFING(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 37
        MESSAG(1) = 'Error allocating memory for DFING'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( DFINH(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 38
        MESSAG(1) = 'Error allocating memory for DFINH'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ABSORP(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 39
        MESSAG(1) = 'Error allocating memory for ABSORP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( KP(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 40
        MESSAG(1) = 'Error allocating memory for KP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( VF(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 41
        MESSAG(1) = 'Error allocating memory for VF'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Concentrations in foods depending on analyte and realization
!
      ALLOCATE( CFISH(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 42
        MESSAG(1) = 'Error allocating memory for CFISH'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CFISH_2(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 43
        MESSAG(1) = 'Error allocating memory for CFISH_2'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CFISH_3(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 44
        MESSAG(1) = 'Error allocating memory for CFISH_3'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CLEAFVEG(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 45
        MESSAG(1) = 'Error allocating memory for CLEAFVEG'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CROOTVEG(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 46
        MESSAG(1) = 'Error allocating memory for CROOTVEG'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CMEAT(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 47
        MESSAG(1) = 'Error allocating memory for CMEAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CBIRD(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 48
        MESSAG(1) = 'Error allocating memory for CBIRD'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CSWEAT(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 49
        MESSAG(1) = 'Error allocating memory for CSWEAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CSHOWER(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 50
        MESSAG(1) = 'Error allocating memory for CSHOWER'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CMILK(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 51
        MESSAG(1) = 'Error allocating memory for CMILK'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CFRUIT(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 52
        MESSAG(1) = 'Error allocating memory for CFRUIT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CGRAIN(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 53
        MESSAG(1) = 'Error allocating memory for CGRAIN'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CEGGS(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 54
        MESSAG(1) = 'Error allocating memory for CEGGS'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CSOIL(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 55
        MESSAG(1) = 'Error allocating memory for CSOIL'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Concentrations in media by analyte and realization
!
      ALLOCATE( GWAT(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 56
        MESSAG(1) = 'Error allocating memory for GWAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SWAT(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 57
        MESSAG(1) = 'Error allocating memory for SWAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SEEP(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 58
        MESSAG(1) = 'Error allocating memory for SEEP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SEDI(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 59
        MESSAG(1) = 'Error allocating memory for SEDI'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SORP(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 60
        MESSAG(1) = 'Error allocating memory for SORP'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SODR(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 61
        MESSAG(1) = 'Error allocating memory for SODR'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SOSW(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 62
        MESSAG(1) = 'Error allocating memory for SOSW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SOGW(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 63
        MESSAG(1) = 'Error allocating memory for SOGW'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
     ALLOCATE( AIRC(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 64
        MESSAG(1) = 'Error allocating memory for AIRC'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RFSOIL(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 65
        MESSAG(1) = 'Error allocating memory for RFSOIL'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RFSWIM(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 66
        MESSAG(1) = 'Error allocating memory for RFSWIM'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RFBOAT(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 67
        MESSAG(1) = 'Error allocating memory for RFBOAT'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RFING(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 68
        MESSAG(1) = 'Error allocating memory for RFING'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RFINH(ESD_NUM_ANA,NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 69
        MESSAG(1) = 'Error allocating memory for RFINH'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( ING_INC_RSK(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 70
        MESSAG(1) = 'Error allocating memory for ING_INC_RSK'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( INH_INC_RSK(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 71
        MESSAG(1) = 'Error allocating memory for INH_INC_RSK'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( EXT_INC_RSK(NREAL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 72
        MESSAG(1) = 'Error allocating memory for EXT_INC_RSK'
        MESSAG(2) = 'System error status was '
        WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

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
!!    Paul W. Eslinger :  8 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 26 Sep 2005 : Update comments and code tags
!!    Paul W. Eslinger :  5 Jun 2007 : Last update
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
      PRGNAM = 'Human'
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

      SUBROUTINE INIT_1( )
!!******************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine initializes global variables of the HUMAN
!!    code that do not depend on array dimensioning.
!!
!!  History:
!!
!!    Paul W. Eslinger : 10 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 26 Jan 2001 : Version 1.0.B
!!    Paul W. Eslinger : 23 Oct 2003 : Add food source variables
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways and another debug
!!    Paul W. Eslinger : 28 Feb 2006 : Add media output labels
!!    Paul W. Eslinger : 30 Oct 2006 : SCR-1148 Fix averaging time variables
!!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0004) Remove SACVIEW header files
!!
!!******************************************************************************************
!
! *** Global parameters, variables, and arrays
!
!      USE Param_Mod
      USE Files_Mod
      USE Stats_Mod
      USE Seeds_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Concen_Mod
      USE ESD_Mod
      USE Errors_Mod
!
!---- Executable code -------------------------------------------------------------
!
! *** Human keyword input file
!
      FNKEY = ' '
!
! *** Report file
      FNRPT = ' '
      REPORT = .FALSE.
!
! *** ESD keyword input file
      FNESD = ' '
!
! *** Output summary statistics risk file
      FNSUM = ' '
      STA_USE = .FALSE.
      STA_SUMDOSE  = .FALSE.
      STA_SUMRISK  = .FALSE.
      STA_POPDOSE  = .FALSE.
      STA_POPRISK  = .FALSE.
      STA_ANADOSE  = .FALSE.
      STA_ANARISK  = .FALSE.
      STA_PATHDOSE = .FALSE.
      STA_PATHRISK = .FALSE.
      STA_CONC     = .FALSE.
      STA_FOOD     = .FALSE.
!
! *** Output detailed risks file
      FNDET = ' '
      DET_USE = .FALSE.
      DET_SUMDOSE  = .FALSE.
      DET_SUMRISK  = .FALSE.
      DET_POPDOSE  = .FALSE.
      DET_POPRISK  = .FALSE.
      DET_ANADOSE  = .FALSE.
      DET_ANARISK  = .FALSE.
      DET_PATHDOSE = .FALSE.
      DET_PATHRISK = .FALSE.
      DET_CONC     = .FALSE.
      DET_FOOD     = .FALSE.
!
! *** Food file
      FNFOD = ' '
!
! *** Input concentration record number map file
      FNMAP = ' '
!
! *** Output file of generated statistical data
      FNVAL = ' '
!
! *** Seed for random number generator
      SDSTOC = 0.0D0
!
! *** Debug flags
      BGFOOD = .FALSE.
      BGCONC = .FALSE.
      BGRAD  = .FALSE.
      BGHAZ  = .FALSE.
      BGCAR  = .FALSE.
      BGVERB = .FALSE.
!
! *** Problem execution and output flags
      EXECUT = .FALSE.
!
! *** Averaging time (days)
      AVGTIME_CAR = -999.0
      AVGTIME_CAR = -999.0
!
! *** Things from the ESD keyword file
      ESD_NUM_ANA = 0    ! Number of analytes in the ESD file
      ESD_NUM_LOC = 0    ! Number of locations in the ESD file
      ESD_NUM_TIM = 0    ! Number of times in the ESD file
      ESD_NUM_SPC = 0    ! Number of species
      ESD_NREAL   = 0    ! Number of realizations in the ESD file
      ESD_TITLE   = ' ' ! Title in the ESD file
!
! *** Things from the HUMAN keyword file
      HUM_NUM_ANA = 0 ! Number of analytes in the human file
      HUM_NUM_LOC = 0 ! Number of locations in the human file
      HUM_NUM_TIM = 0 ! Number of times in the human file
      NREAL       = 0 ! Number of realizations in the human file
      PTITLE = ' '
!
! *** Variables for food consumption
      FOODPATH = ' '
      FOODMAP  = ' '
!
! *** Analyte information
      NUMRAD = 0
      NUMCAR = 0
      NUMHAZ = 0
!
! *** Sweat lodge and showering water sources
      SWEAT_SOURCE  = 'XXXX'
      SHOWER_SOURCE = 'XXXX'
!
! *** Set invalid food indices for identifying species
!     Turn off food pathways
!
      LEAFVEG_FIDX = -1
      ROOTVEG_FIDX = -1
      GRAIN_FIDX   = -1
      FRUIT_FIDX   = -1
      BIRD_FIDX    = -1
      MEAT_FIDX    = -1
      MILK_FIDX    = -1
      FISH_FIDX    = -1
      FISH_FIDX_2  = -1
      FISH_FIDX_3  = -1
      EGGS_FIDX    = -1
!
      USE_LEAFVEG = .FALSE.
      USE_ROOTVEG = .FALSE.
      USE_GRAIN   = .FALSE.
      USE_FRUIT   = .FALSE.
      USE_BIRD    = .FALSE.
      USE_MEAT    = .FALSE.
      USE_MILK    = .FALSE.
      USE_FISH    = .FALSE.
      USE_FISH_2  = .FALSE.
      USE_FISH_3  = .FALSE.
      USE_EGGS    = .FALSE.
      USE_SOIL    = .FALSE.
      USE_SEDI    = .FALSE.
!
! *** Soil ID
      SOIL_ID = 'None'
      SOIL_IDX = -1
!
! *** Shortcut variables for defining food sources (same for all locations)
      SOURCE_BIRD   = .FALSE.
      SOURCE_FISH   = .FALSE.
      SOURCE_FISH_2 = .FALSE.
      SOURCE_FISH_3 = .FALSE.
      SOURCE_MEAT   = .FALSE.
      SOURCE_BIRD_ID   = ' '
      SOURCE_FISH_ID   = ' '
      SOURCE_FISH_ID_2 = ' '
      SOURCE_FISH_ID_3 = ' '
      SOURCE_MEAT_ID   = ' '
!
! *** Labels for output
      LABEL_SURFACE  = 'Not consumed'
      LABEL_GROUND   = 'Not consumed'
      LABEL_SEEP     = 'Not consumed'
      LABEL_SOIL_CHILD     = 'Not consumed'
      LABEL_SOIL_ADULT     = 'Not consumed'
      LABEL_SEDIMENT_CHILD = 'Not consumed'
      LABEL_SEDIMENT_ADULT = 'Not consumed'
!
      RETURN
      END SUBROUTINE

      SUBROUTINE INIT_POPDOSE( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine initializes the population dose vector for
!!    a single time slice.
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Mar 2000 : Version 1.0
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Control_Mod
      USE Results_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: IREL ! Realization looping index
!
!---- Executable code -------------------------------------------------------------
!
      DO IREL = 1, NREAL
        POPDOSE(IREL) = 0.0
        POPRISK(IREL) = 0.0
      END DO
!
      RETURN
      END SUBROUTINE

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
!!    Paul W. Eslinger : 13 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  5 Jun 2007 : Revise comments and error return
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
      USE ESD_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number indicator
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'KEY_ESD_1' ! Name of this subroutine
      CHARACTER*(LENCRD) :: TITLE ! Title line from RDBLK
!
!---- Executable code -------------------------------------------------------------
!
! *** Initialize for RDBLK
!
      ILINE = 0
      IERR  = 0
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
        CASE( 'SPECIES' ) ! ===> SPECIES keyword
          ESD_NUM_SPC = ESD_NUM_SPC + 1
!
        CASE( 'TIMES' ) ! ===> TIMES Keyword
          ESD_NUM_TIM = ESD_NUM_TIM + NVALUE
!
        CASE( 'TITLE' ) ! ===> TITLE Keyword
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
!!    Paul W. Eslinger : 10 Mar 2000  : Version 2.0
!!    Carmen Arimescu  :  3 Feb 2003  : SAC Rev.1
!!    Paul W. Eslinger :  5 Jun 2007 : Revise comments
!!
!!**********************************************************************
!
! *** Global variables
      USE Rdblk_Mod
      USE Files_Mod
      USE Errors_Mod
      USE ESD_Mod
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
      REAL :: RTMP                ! Temporary real variable
      INTEGER :: IDX, ITMP, IDXA  ! Temporary index variables
      CHARACTER*(LENCRD) :: TITLE ! Title line from RDBLK
!
!     Temporary character strings to match with a RDBLK quote string
      CHARACTER(LEN=LENQQQ) :: TMP_ID, TMP_ANA, TMP_NAME
!
!---- Executable code -------------------------------------------------------------
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
      ESD_NUM_SPC = 0 ! Number of species in the ESD file
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
        MESSAG(1) = 'Error in lower level RDBLK routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
!-----------------------------------------------------------------------
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
              ESD_ANA(ESD_NUM_ANA)%NAME = TRIM(TMP_NAME(1:72))
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
              CALL UPCASE( TMP_NAME )
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
!-----------------------------------------------------------------------
        CASE( 'END' ) ! ===> END keyword
          CLOSE( IESD )
          RETURN
!
!-----------------------------------------------------------------------
        CASE( 'FILE' ) ! ===> FILE keyword
!
          IF( CEXIST('C_ECDA') ) THEN
!
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 7
                MESSAG(1) = 'NAME modifier missing quote string on FILE (C_ECDA) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            ELSE
              IERR = 8
              MESSAG(1) = 'NAME modifier not found on FILE card for type C_ECDA'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!
            IF( CEXIST('ANALYTE ') ) THEN
              CALL NXTQOT( IDX, TMP_ANA )
              CALL MATCH_ANA( TMP_ANA, IDXA )
              IF( IDXA .GT. 0 ) THEN
                FNCON(IDXA) = TMP_ID
              ELSE
                IERR = 7
                MESSAG(1) = 'Analyte requested is not yet entered with an ANALYTE keyword'
                MESSAG(2) = 'Analyte name is '//TRIM(TMP_ANA)
                MESSAG(3) = 'Put the FILE keywords for concentrations after the ANALYTE keywords'
                MESSAG(4) = 'In the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 4 )
                RETURN
              END IF
            ELSE
              IERR = 8
              MESSAG(1) = 'ANALYTE modifier not found on FILE card for type C_ECDA'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!
          END IF
!
          IF( CEXIST('I_ECDA') ) THEN
!
            IF( CEXIST('NAME    ') ) THEN
              CALL NXTQOT( IDX, TMP_ID )
              IF( IDX .LE. 0 ) THEN
                IERR = 9
                MESSAG(1) = 'NAME modifier missing quote string on FILE (I_ECDA) keyword'
                MESSAG(2) = 'Error encountered in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 1 )
                RETURN
              ELSE
                FNMAP = TMP_ID
              END IF
            ELSE
              IERR = 10
              MESSAG(1) = 'NAME modifier not found on FILE card for type I_ECDA'
              MESSAG(2) = 'Error encountered in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
!
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
!
          ESD_NUM_LOC = ESD_NUM_LOC + 1
!
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%ID = TMP_ID
            ELSE
              IERR = 11
              MESSAG(1) = 'Location ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 12
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
              IERR = 13
              MESSAG(1) = 'Location NAME modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 14
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
              IERR = 15
              MESSAG(1) = 'Location EASTING modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 16
            MESSAG(1) = 'EASTING modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('NORTHING ') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%NORTHING = RTMP
            ELSE
              IERR = 17
              MESSAG(1) = 'Location NORTHING modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 18
            MESSAG(1) = 'NORTHING modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('TYPE') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
              CALL UPCASE( TMP_ID )
              ESD_LOC(ESD_NUM_LOC)%TYPE = TRIM(TMP_ID(1:8))
            ELSE
              IERR = 19
              MESSAG(1) = 'TYPE modifier missing quote string on the LOCATION keyword'
              MESSAG(2) = 'Modify the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 20
            MESSAG(1) = 'TYPE modifier not entered on the LOCATION keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('POP') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .GT. 0 ) THEN
              ESD_LOC(ESD_NUM_LOC)%POP = RTMP
            ELSE
              IERR = 21
              MESSAG(1) = 'Location POP modifier missing value'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'SPECIES' ) ! ===> SPECIES keyword
!
          ESD_NUM_SPC = ESD_NUM_SPC + 1
!
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .GT. 0 ) THEN
            ESD_SPC(ESD_NUM_SPC)%ID = TMP_ID
            ELSE
              IERR = 23
              MESSAG(1) = 'Species ID modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 24
            MESSAG(1) = 'ID modifier not entered on the SPECIES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
          IF( CEXIST('NAME    ') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .GT. 0 ) THEN
              ESD_SPC(ESD_NUM_SPC)%NAME = TMP_NAME
            ELSE
              IERR = 25
              MESSAG(1) = 'Species NAME modifier missing quote string'
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
          ELSE
            IERR = 26
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
                IERR = 27
                MESSAG(1) = 'Invalid HABITAT string encountered'
                MESSAG(2) = 'Species ID is '// ESD_SPC(ESD_NUM_SPC)%ID
                MESSAG(3) = 'Problem in the ESD keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            ELSE
              IERR = 28
              MESSAG(1) = 'No quote string with the HABITAT modifier on the SPECIES keyword'
              MESSAG(2) = 'Species ID is '// ESD_SPC(ESD_NUM_SPC)%ID
              MESSAG(3) = 'Problem in the ESD keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
          ELSE
            IERR = 29
            MESSAG(1) = 'HABITAT modifier not entered on the SPECIES keyword'
            MESSAG(2) = 'Species ID is '// ESD_SPC(ESD_NUM_SPC)%ID
            MESSAG(3) = 'Problem in the ESD keyword file'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'TIMES' ) ! ===> TIMES Keyword
          IF( NVALUE .GT. 0 ) THEN
            DO ITMP = 1, NVALUE
              ESD_NUM_TIM = ESD_NUM_TIM + 1
              ESD_TIM(ESD_NUM_TIM)%TIME = VALUE(ITMP)
            END DO
          ELSE
            IERR = 30
            MESSAG(1) = 'No numeric values found on the TIMES keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE DEFAULT ! Ignore all other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE

      SUBROUTINE KEY_HUMAN_1( IERR )
!!***************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading the HUMAN keyword file for control
!!    information and data required to set variable dimensions.
!!
!!  Call List Variables:
!!
!!    IERR : Output integer error flag.
!!            0 = Normal execution
!!           >0 = Terminal error encountered
!!
!!  Auxiliary Routines:
!!
!!    CEXIST, PRTERR, and all RDBLK
!!    related routines
!!
!!  Notes:
!!
!!    When errors are encountered, a message is output to the standard
!!    output device and control is passed back to the calling routine.
!!
!!  History:
!!
!!    Paul W. Eslinger :  8 Mar 2000 : Version 2.0
!!    Paul W. Eslinger : 26 Sep 2005 : Move DEBUG from KEY_HUMAN_2
!!    Paul W. Eslinger :  5 Jun 2007 : Move REPORT to another routine
!!                                     Revise comments
!!
!!***************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Debug_Mod
      USE Errors_Mod
      USE Stats_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined external functions
      LOGICAL, EXTERNAL :: CEXIST
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'KEY_HUMAN_1'
      INTEGER :: IDX   ! Temporary index variable
      INTEGER :: ITMP  ! Temporary index variable
      CHARACTER*(LENCRD) :: TITLE ! Title line from RDBLK
      CHARACTER(LEN=LENQQQ) :: TMP_ID ! Temporary string to use with NXTQOT
!
!---- Executable code -------------------------------------------------------------
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
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
!-----------------------------------------------------------------------
        CASE( 'ANALYTE' ) ! ===> ANALYTE keyword
          HUM_NUM_ANA = HUM_NUM_ANA + 1
!
!-----------------------------------------------------------------------
        CASE( 'DEBUG' ) ! ===> DEBUG keyword
          IF( CEXIST('CARCINOG') ) BGCAR  = .TRUE.
          IF( CEXIST('CONCENTR') ) BGCONC = .TRUE.
          IF( CEXIST('FOOD    ') ) BGFOOD = .TRUE.
          IF( CEXIST('HAZARDOU') ) BGHAZ  = .TRUE.
          IF( CEXIST('RADIONUC') ) BGRAD  = .TRUE.
          IF( CEXIST('VERBOSE ') ) BGVERB = .TRUE.
          IF( CEXIST('STOCHAST') ) BG_STOC_VALU = .TRUE.
          IF( CEXIST('STATISTI') ) BG_STOC_STAT = .TRUE.
          IF( CEXIST('DEFINITI') ) BG_STOC_DEFN = .TRUE.
!
!-----------------------------------------------------------------------
        CASE( 'DETAILS ' ) ! ===> DETAILS keyword
          IF( CEXIST('SUMDOSE') ) THEN
            DET_SUMDOSE = .TRUE.
            DET_USE = .TRUE.
          END IF
          IF( CEXIST('SUMRISK') ) THEN
            DET_SUMRISK = .TRUE.
            DET_USE = .TRUE.
          END IF
          IF( CEXIST('ANADOSE') ) THEN
            DET_ANADOSE = .TRUE.
            DET_USE = .TRUE.
          END IF
          IF( CEXIST('ANARISK') ) THEN
            DET_ANARISK = .TRUE.
            DET_USE = .TRUE.
          END IF
          IF( CEXIST('PATHDOSE') ) THEN
            DET_PATHDOSE = .TRUE.
            DET_USE = .TRUE.
          END IF
          IF( CEXIST('PATHRISK') ) THEN
            DET_PATHRISK = .TRUE.
            DET_USE = .TRUE.
          END IF
          IF( CEXIST('POPDOSE') ) THEN
            DET_POPDOSE = .TRUE.
            DET_USE = .TRUE.
          END IF
          IF( CEXIST('POPRISK') ) THEN
            DET_POPRISK = .TRUE.
            DET_USE = .TRUE.
          END IF
          IF( CEXIST('CONCEN') ) THEN
            DET_CONC = .TRUE.
            DET_USE = .TRUE.
          END IF
          IF( CEXIST('FOODS') ) THEN
            DET_FOOD = .TRUE.
            DET_USE = .TRUE.
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'END' ) ! ===> END keyword
          REWIND( IKEY )
          RETURN
!
!-----------------------------------------------------------------------
        CASE( 'FILE' ) ! ===> FILE keyword
          IF( CEXIST('ESD     ') ) THEN
            CALL NXTQOT( IDX, FNESD )
            IF( IDX .EQ. 0 ) THEN
              IERR = 2
              MESSAG(1) = 'The ESD modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'FOODSOUR' ) ! ===> FOODSOURCE keyword
!
          IF( CEXIST('BIRD') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .EQ. 0 ) THEN
              IERR = 3
              MESSAG(1) = 'The BIRD modifier on the FOODSOURCE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SOURCE_BIRD = .TRUE.
            SOURCE_BIRD_ID = TMP_ID
          END IF
!
          IF( CEXIST('FISH') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .EQ. 0 ) THEN
              IERR = 3
              MESSAG(1) = 'The FISH modifier on the FOODSOURCE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SOURCE_FISH = .TRUE.
            SOURCE_FISH_ID = TMP_ID
          END IF
!
          IF( CEXIST('FISH_2') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .EQ. 0 ) THEN
              IERR = 4
              MESSAG(1) = 'The FISH_2 modifier on the FOODSOURCE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SOURCE_FISH_2 = .TRUE.
            SOURCE_FISH_ID_2 = TMP_ID
          END IF
!
          IF( CEXIST('FISH_3') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .EQ. 0 ) THEN
              IERR = 4
              MESSAG(1) = 'The FISH_3 modifier on the FOODSOURCE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SOURCE_FISH_3 = .TRUE.
            SOURCE_FISH_ID_3 = TMP_ID
          END IF
!
          IF( CEXIST('MEAT') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .EQ. 0 ) THEN
              IERR = 3
              MESSAG(1) = 'The MEAT modifier on the FOODSOURCE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SOURCE_MEAT = .TRUE.
            SOURCE_MEAT_ID = TMP_ID
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
          HUM_NUM_LOC = HUM_NUM_LOC + 1
!
!-----------------------------------------------------------------------
        CASE( 'REALIZAT' ) ! ===> REALIZATION Keyword
          NREAL = VALUE(1)
!
!-----------------------------------------------------------------------
        CASE( 'STATISTI' ) ! ===> STATISTICS keyword
          IF( CEXIST('SUMDOSE') ) THEN
            STA_SUMDOSE = .TRUE.
            STA_USE = .TRUE.
          END IF
          IF( CEXIST('SUMRISK') ) THEN
            STA_SUMRISK = .TRUE.
            STA_USE = .TRUE.
          END IF
          IF( CEXIST('ANADOSE') ) THEN
            STA_ANADOSE = .TRUE.
            STA_USE = .TRUE.
          END IF
          IF( CEXIST('ANARISK') ) THEN
            STA_ANARISK = .TRUE.
            STA_USE = .TRUE.
          END IF
          IF( CEXIST('PATHDOSE') ) THEN
            STA_PATHDOSE = .TRUE.
            STA_USE = .TRUE.
          END IF
          IF( CEXIST('PATHRISK') ) THEN
            STA_PATHRISK = .TRUE.
            STA_USE = .TRUE.
          END IF
          IF( CEXIST('POPDOSE') ) THEN
            STA_POPDOSE = .TRUE.
            STA_USE = .TRUE.
          END IF
          IF( CEXIST('POPRISK') ) THEN
            STA_POPRISK = .TRUE.
            STA_USE = .TRUE.
          END IF
          IF( CEXIST('CONCEN') ) THEN
            STA_CONC = .TRUE.
            STA_USE = .TRUE.
          END IF
          IF( CEXIST('FOODS') ) THEN
            STA_FOOD = .TRUE.
            STA_USE = .TRUE.
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'STOCHAST' ) ! ===> STOCHASTIC keyword
          INDSTO = INDSTO + 1
          ITMP = VALUE(1)
          IF( ITMP .EQ. 10 ) INDTBL = INDTBL + VALUE(2)
!
!-----------------------------------------------------------------------
        CASE( 'TIMES' ) ! ===> TIME keyword
          HUM_NUM_TIM = HUM_NUM_TIM + NVALUE
!
!-----------------------------------------------------------------------
        CASE( 'TITLE' ) ! ===> TITLE Keyword
          PTITLE = QUOTE(1)
!
!-----------------------------------------------------------------------
        CASE( 'USER' ) ! ===> USER Keyword
          USRNAM = QUOTE(1)
!
!-----------------------------------------------------------------------
        CASE DEFAULT ! Ignore all other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE

      SUBROUTINE KEY_HUMAN_2( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles reading and storing information from
!!    the HUMAN keyword control file that wasn't read by KEY_HUMAN_1.
!!
!!  Call List Variables:
!!
!!    IERR : Output integer error flag.
!!            0 = Normal execution
!!           >0 = Terminal error encountered
!!
!!  Auxiliary Routines:
!!
!!    CEXIST, PRTERR, SDECOD, and all RDBLK
!!    related routines
!!
!!  Notes:
!!
!!    1. All errors are reported to the report file through the use
!!       of subroutine PRTERR.
!!
!!    2. Don't have to check on dimensions for analytes because
!!       memory was allocated based on the number of input analytes
!!
!!    3. Don't have to check on dimensions for stochastic variables
!!       because memory was allocated based on the total number of
!!       stochastic keywords
!!
!!  History:
!!
!!    Paul W. Eslinger : 16 Mar 2000 : Version 2.0
!!    Paul W. Eslinger : 22 Nov 2000 : Version 2.00.1 : Add POP modifier
!!      to the LOCATION keyword - optional overwrite of ESD data
!!    Carmen Arimescu  :  3 Feb 2003 : SAC Rev.1 : Add new keywords and
!!                        change LOCATION keyword
!!    Paul W. Eslinger : 22 Oct 2003 : Add MEAT option on LOCATION keyword
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!                                     Move DEBUG to KEY_HUMAN_1
!!    Paul W. Eslinger : 15 Feb 2006 : Add separate showering definition
!!    Paul W. Eslinger : 14 Sep 2006 : Allow FOODS keyword for FOOD keyword
!!    Paul W. Eslinger : 30 Oct 2006 : SCR-1148 Fix averaging time variables
!!    Paul W. Eslinger :  5 Jun 2007 : Revise comments
!!    Paul W. Eslinger :  7 Feb 2014 : (SCR-0004) Remove SACVIEW header files
!!
!!**********************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Seeds_Mod
      USE Errors_Mod
      USE Stats_Mod
      USE Esd_Mod
      USE FCDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: CEXIST
      LOGICAL, EXTERNAL :: STRCOMP
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=11) :: CALLER = 'KEY_HUMAN_2'
!
      INTEGER :: ONE ! The number 1
!
      CHARACTER(LEN=LENCRD) :: TITLE ! Title line for keyword
!
      CHARACTER(LEN=16) :: CTMP    ! Temporary strings for stochastic variables
      CHARACTER(LEN=72) :: CMES    ! Temporary strings for stochastic variables
      CHARACTER(LEN=6) :: SHORT_ID ! Temporary ID string
!
      CHARACTER(LEN=LENQQQ) :: TMP_NAME ! Temporary variable for file names
      CHARACTER(LEN=LENQQQ) :: TMP_ID
      CHARACTER(LEN=LENQQQ) :: LOC_ID_2 ! Temporary location ID
!
      REAL :: RTMP           ! Temporary real value
      INTEGER :: TEMPTIME    ! Temporary time variable
      INTEGER :: GOOD_SWEAT  ! SWEAT keyword temporary variable
      INTEGER :: GOOD_SHOWER ! SHOWER keyword temporary variable
!
      LOGICAL :: TRUNC ! Logical flag for truncation test
      INTEGER :: IDXL  ! Temporary index for locations
      INTEGER :: IDX2  ! Temporary index for secondary locations
      INTEGER :: IDXA  ! Temporary index for analytes
      INTEGER :: IDXT  ! Temporary index for times
      INTEGER :: ITIM  ! Temporary looping index for times
      INTEGER :: IDX   ! Temporary index for RDBLK routines
      INTEGER :: ISPC  ! Temporary index species
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
      ILINE = 0
!
! *** Indicator of location for stochastics used in SDECOD
      ONE = 1
!
! *** Reset counters set in KEY_HUMAN_1 for dimensioning purposes
      INDSTO = 0
      INDTBL = 0
!
! *** Rewind the keyword file after it was used by KEY_HUMAN_1
!
      REWIND( IKEY )
!
      HUM_NUM_LOC = 0
      HUM_NUM_TIM = 0
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
!-----------------------------------------------------------------------
        CASE( 'ANALYTE' ) ! ===> ANALYTE keyword
!
          IDXA = -1
          IF( CEXIST('ID      ') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            CALL MATCH_ANA( TMP_ID, IDXA )
            IF( IDXA .GT. 0 ) THEN
              ESD_ANA(IDXA)%COMP = .TRUE.
            ELSE
              IERR = 1
              MESSAG(1) = 'Analyte requested that is not in the master list'
              MESSAG(2) = 'Analyte name is '//TMP_ID
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 2
            MESSAG(1) = 'ID modifier not entered on the ANALYTE keyword'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!         Analyte type (1 of 3 types required)
          IF( CEXIST('RADIOACT') ) THEN
            ESD_ANA(IDXA)%ANATYP_RAD = .TRUE.
            NUMRAD = NUMRAD + 1
          END IF
          IF( CEXIST('CARCINOG') ) THEN
            ESD_ANA(IDXA)%ANATYP_CAR = .TRUE.
            NUMCAR = NUMCAR + 1
          END IF
          IF( CEXIST('HAZARDOU') ) THEN
            ESD_ANA(IDXA)%ANATYP_HAZ = .TRUE.
            NUMHAZ =  NUMHAZ + 1
          END IF
          IF( (.NOT.ESD_ANA(IDXA)%ANATYP_CAR) .AND. (.NOT.ESD_ANA(IDXA)%ANATYP_HAZ) .AND.(.NOT.ESD_ANA(IDXA)%ANATYP_RAD) ) THEN
            IERR = 3
            MESSAG(1) = 'Type not set properly on ANALYTE Card for '//TRIM(QUOTE(1))
            MESSAG(2) = 'One or more of the modifiers CARCINOG, HAZARDOU, or RADIOACT'
            MESSAG(3) = 'are required.  Change the ANALYTE keyword.'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
! ***     Check for optional output flags
          IF( CEXIST('OUTPUT' ) ) ESD_ANA(IDXA)%OUTPUT  = .TRUE.
          IF( CEXIST('VERBOSE') ) ESD_ANA(IDXA)%VERBOSE = .TRUE.
          IF( CEXIST('TERSE'  ) ) ESD_ANA(IDXA)%TERSE   = .TRUE.
          IF( CEXIST('RFDING' ) ) ESD_ANA(IDXA)%RFDING  = .TRUE.
          IF( CEXIST('RFDINH' ) ) ESD_ANA(IDXA)%RFDINH  = .TRUE.
          IF( CEXIST('SFING'  ) ) ESD_ANA(IDXA)%SFING   = .TRUE.
          IF( CEXIST('SFINH'  ) ) ESD_ANA(IDXA)%SFINH   = .TRUE.
!
!-----------------------------------------------------------------------
        CASE( 'AVERAGE' ) ! ===> AVERAGE keyword
!
          IF( CEXIST('CARCINOG') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 92
              MESSAG(1) = 'The CARCINOG modifier on the AVERAGE keyword did not'
              MESSAG(2) = 'have a numerical value associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            AVGTIME_CAR = RTMP * 365.25  ! Convert to days before storing
          END IF
!
          IF( CEXIST('HAZARDOU') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 93
              MESSAG(1) = 'The HAZARDOU modifier on the AVERAGE keyword did not'
              MESSAG(2) = 'have a numerical value associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            AVGTIME_HAZ = RTMP * 365.25  ! Convert to days before storing
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'END' ) ! ===> END keyword
          CLOSE( IKEY )
          RETURN
!
!-----------------------------------------------------------------------
        CASE( 'EXECUTE' ) ! ===> EXECUTE keyword
          EXECUT = .TRUE.
!
!-----------------------------------------------------------------------
        CASE( 'FILE' ) ! ===> FILE keyword
!
          IF( CEXIST('DETAIL  ') ) THEN
            CALL NXTQOT( IDX, FNDET )
            IF( IDX .EQ. 0 ) THEN
              IERR = 4
              MESSAG(1) = 'The DETAIL modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('STATS   ') ) THEN
            CALL NXTQOT( IDX, FNSUM )
            IF( IDX .EQ. 0 ) THEN
              IERR = 6
              MESSAG(1) = 'The STATS modifier on the FILE keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          END IF
!
          IF( CEXIST('VALUES  ') ) THEN
            CALL NXTQOT( IDX, FNVAL )
            IF( IDX .EQ. 0 ) THEN
              IERR = 7
              MESSAG(1) = 'File name (quote string) not found'
              MESSAG(2) = 'FILE keyword, VALUES modifier'
              MESSAG(3) = 'Problem in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              BG_STOC_VALU = .TRUE.
            END IF
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'INGESTIO' ) ! ===> INGESTION keyword
!
          IF( CEXIST('FISH') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 8
              MESSAG(1) = 'Quote string not found for the FISH modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                FISH_FIDX = ISPC
                USE_FISH = .TRUE.
              ELSE
                IERR = 9
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for FISH) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          ELSE
            IERR = 999
            MESSAG(1) = 'Missing the FISH modifier on the INGESTION keyword'
            MESSAG(2) = 'Error in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('FISH_2') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 40
              MESSAG(1) = 'Quote string not found for the FISH_2 modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                FISH_FIDX_2 = ISPC
                USE_FISH_2 = .TRUE.
              ELSE
                IERR = 41
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for FISH_2) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          IF( CEXIST('FISH_3') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 42
              MESSAG(1) = 'Quote string not found for the FISH_3 modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                FISH_FIDX_3 = ISPC
                USE_FISH_3 = .TRUE.
              ELSE
                IERR = 43
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for FISH_3) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          IF( CEXIST('LEAFVEG') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 10
              MESSAG(1) = 'Quote string not found for the LEAFVEG modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                LEAFVEG_FIDX = ISPC
                USE_LEAFVEG = .TRUE.
              ELSE
                IERR = 11
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for LEAFVEG) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          ELSE
            IERR = 999
            MESSAG(1) = 'Missing the LEAFVEG modifier on the INGESTION keyword'
            MESSAG(2) = 'Error in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('ROOTVEG') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 12
              MESSAG(1) = 'Quote string not found for the ROOTVEG modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                ROOTVEG_FIDX = ISPC
                USE_ROOTVEG = .TRUE.
              ELSE
                IERR = 13
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for ROOTVEG) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          ELSE
            IERR = 999
            MESSAG(1) = 'Missing the ROOTVEG modifier on the INGESTION keyword'
            MESSAG(2) = 'Error in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('GRAIN') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 14
              MESSAG(1) = 'Quote string not found for the GRAIN modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                GRAIN_FIDX = ISPC
                USE_GRAIN = .TRUE.
              ELSE
                IERR = 15
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for GRAIN) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          ELSE
            IERR = 999
            MESSAG(1) = 'Missing the GRAIN modifier on the INGESTION keyword'
            MESSAG(2) = 'Error in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('FRUIT') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 16
              MESSAG(1) = 'Quote string not found for the FRUIT modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                FRUIT_FIDX = ISPC
                USE_FRUIT = .TRUE.
              ELSE
                IERR = 17
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for FRUIT) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          ELSE
            IERR = 999
            MESSAG(1) = 'Missing the FRUIT modifier on the INGESTION keyword'
            MESSAG(2) = 'Error in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('MEAT') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 18
              MESSAG(1) = 'Quote string not found for the MEAT modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                MEAT_FIDX = ISPC
                USE_MEAT = .TRUE.
              ELSE
                IERR = 19
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for MEAT) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          ELSE
            IERR = 999
            MESSAG(1) = 'Missing the MEAT modifier on the INGESTION keyword'
            MESSAG(2) = 'Error in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('MILK') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 20
              MESSAG(1) = 'Quote string not found for the MILK modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                MILK_FIDX = ISPC
                USE_MILK = .TRUE.
              ELSE
                IERR = 21
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for MILK) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          ELSE
            IERR = 999
            MESSAG(1) = 'Missing the MILK modifier on the INGESTION keyword'
            MESSAG(2) = 'Error in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('BIRD') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 22
              MESSAG(1) = 'Quote string not found for the BIRD modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                BIRD_FIDX = ISPC
                USE_BIRD = .TRUE.
              ELSE
                IERR = 23
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for BIRD) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          ELSE
            IERR = 999
            MESSAG(1) = 'Missing the BIRD modifier on the INGESTION keyword'
            MESSAG(2) = 'Error in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('EGGS') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            IF( IDX .LT. 1 ) THEN
              IERR = 24
              MESSAG(1) = 'Quote string not found for the EGGS modfier'
              MESSAG(2) = 'Error on the INGESTION keyword in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
            SHORT_ID = TMP_ID(1:6)
            IF( .NOT. STRCOMP(SHORT_ID,'NONE',4) ) THEN
              CALL MATCH_SPC( SHORT_ID, ISPC )
              IF( ISPC .GT. 0 ) THEN
                EGGS_FIDX = ISPC
                USE_EGGS = .TRUE.
              ELSE
                IERR = 25
                MESSAG(1) = 'Requested species is not in the master list'
                MESSAG(2) = 'Species ID entered as a food (for EGGS) was '//SHORT_ID
                MESSAG(3) = 'Error on the INGESTION keyword in the HUMAN keyword file'
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          ELSE
            IERR = 999
            MESSAG(1) = 'Missing the EGGS modifier on the INGESTION keyword'
            MESSAG(2) = 'Error in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
          CASE( 'FOOD','FOODS' ) ! ===> FOOD or FOODS keywordS
!
          IF( CEXIST('PATH') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 26
              MESSAG(1) = 'Path name (quote string) not found'
              MESSAG(2) = 'FOOD (FOODS) keyword, PATH modifier'
              MESSAG(3) = 'Problem in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              FOODPATH = TMP_NAME
            END IF
          ELSE
            IERR = 27
            MESSAG(1) = 'The PATH modifier is required on the FOOD (FOODS) keyword'
            MESSAG(2) = 'Problem in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('MAP') ) THEN
            CALL NXTQOT( IDX, TMP_NAME )
            IF( IDX .EQ. 0 ) THEN
              IERR = 28
              MESSAG(1) = 'Map file name (quote string) not found'
              MESSAG(2) = 'FOOD (FOODS) keyword, MAP modifier'
              MESSAG(3) = 'Problem in the HUMAN keyword file'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              FOODMAP = TMP_NAME
            END IF
          ELSE
            IERR = 29
            MESSAG(1) = 'The MAP modifier is required on the FOOD (FOODS) keyword'
            MESSAG(2) = 'Problem in the HUMAN keyword file'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'LOCATION' ) ! ===> LOCATION keyword
!
          HUM_NUM_LOC = HUM_NUM_LOC + 1
!
          IF( CEXIST('PRIMARY') ) THEN
            CALL NXTQOT( IDX, TMP_ID )
            CALL MATCH_LOC( TMP_ID, IDXL )
            IF( IDXL .GT. 0 ) THEN
              ESD_LOC(IDXL)%COMP = .TRUE.
            ELSE
              IERR = 30
              MESSAG(1) = 'Location requested that is not in the master list'
              MESSAG(2) = 'Location name is '//TMP_ID
              CALL PRTERR( IERR, CALLER, 2 )
              RETURN
            END IF
          ELSE
            IERR = 31
            MESSAG(1) = 'Every LOCATION card must have the PRIMARY modifier'
            MESSAG(2) = 'Location card number XXXX is missing the modifier'
            WRITE(MESSAG(2)(22:25),'(I4)') HUM_NUM_LOC
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
          IF( CEXIST('POP') ) THEN
            CALL NXTVAL( IDX, RTMP )
            IF( IDX .EQ. 0 ) THEN
              IERR = 32
              MESSAG(1) = 'The POP modifier on the LOCATION keyword did not'
              MESSAG(2) = 'have a numerical value associated with it.'
              MESSAG(3) = 'Location card PRIMARY ID is : '//TRIM(TMP_ID)
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              ESD_LOC(IDXL)%POP = RTMP
            END IF
          END IF
!
          IF( CEXIST('BIRD') ) THEN
            IF( SOURCE_BIRD ) THEN
              MESSAG(1) = 'The BIRD location on the LOCATION keyword for location "'//TRIM(ESD_LOC(IDXL)%ID)//'" is being ignored'
              MESSAG(2) = 'The BIRD concentration comes from location "'//TRIM(SOURCE_BIRD_ID)//'" for all output locations'
              MESSAG(3) = 'Modify or remove the FOODSOURCE keyword to change the source ID for BIRD concentrations'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
            CALL NXTQOT( IDX, LOC_ID_2 )
            IF( IDX .EQ. 0 ) THEN
              IERR = 33
              MESSAG(1) = 'The BIRD modifier on the LOCATION keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Location card PRIMARY ID is : '//TRIM(TMP_ID)
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              CALL MATCH_LOC( LOC_ID_2, IDX2 )
              IF( IDX2 .GT. 0 ) THEN
                ESD_LOC(IDXL)%BIRD = IDX2
              ELSE
                IERR = 34
                MESSAG(1) = 'BIRD location is not in the master list'
                MESSAG(2) = 'Location and BIRDary IDs are: '//TRIM(TMP_ID)//' and '//TRIM(LOC_ID_2)
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            END IF
          END IF
!
          IF( CEXIST('FISH') ) THEN
            IF( SOURCE_FISH ) THEN
              MESSAG(1) = 'The FISH location on the LOCATION keyword for location "'//TRIM(ESD_LOC(IDXL)%ID)//'" is being ignored'
              MESSAG(2) = 'The FISH concentration comes from location "'//TRIM(SOURCE_FISH_ID)//'" for all output locations'
              MESSAG(3) = 'Modify or remove the FOODSOURCE keyword to change the source ID for FISH concentrations'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
            CALL NXTQOT( IDX, LOC_ID_2 )
            IF( IDX .EQ. 0 ) THEN
              IERR = 33
              MESSAG(1) = 'The FISH modifier on the LOCATION keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Location card PRIMARY ID is : '//TRIM(TMP_ID)
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              CALL MATCH_LOC( LOC_ID_2, IDX2 )
              IF( IDX2 .GT. 0 ) THEN
                ESD_LOC(IDXL)%FISH = IDX2
              ELSE
                IERR = 34
                MESSAG(1) = 'FISH location is not in the master list'
                MESSAG(2) = 'Location and FISH IDs are: '//TRIM(TMP_ID)//' and '//TRIM(LOC_ID_2)
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            END IF
          END IF
!
          IF( CEXIST('FISH_2') ) THEN
            IF( SOURCE_FISH_2 ) THEN
              MESSAG(1) = 'The FISH_2 location on the LOCATION keyword for location "'//TRIM(ESD_LOC(IDXL)%ID)//'" is being ignored'
              MESSAG(2) = 'The FISH_2 concentration comes from location "'//TRIM(SOURCE_FISH_ID_2)//'" for all output locations'
              MESSAG(3) = 'Modify or remove the FOODSOURCE keyword to change the source ID for FISH_2 concentrations'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
            CALL NXTQOT( IDX, LOC_ID_2 )
            IF( IDX .EQ. 0 ) THEN
              IERR = 43
              MESSAG(1) = 'The FISH_2 modifier on the LOCATION keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Location card PRIMARY ID is : '//TRIM(TMP_ID)
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              CALL MATCH_LOC( LOC_ID_2, IDX2 )
              IF( IDX2 .GT. 0 ) THEN
                ESD_LOC(IDXL)%FISH_2 = IDX2
              ELSE
                IERR = 44
                MESSAG(1) = 'FISH_2 location is not in the master list'
                MESSAG(2) = 'Location and FISH_2 IDs are: '//TRIM(TMP_ID)//' and '//TRIM(LOC_ID_2)
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            END IF
          END IF
!
          IF( CEXIST('FISH_3') ) THEN
            IF( SOURCE_FISH_3 ) THEN
              MESSAG(1) = 'The FISH_3 location on the LOCATION keyword for location "'//TRIM(ESD_LOC(IDXL)%ID)//'" is being ignored'
              MESSAG(2) = 'The FISH_3 concentration comes from location "'//TRIM(SOURCE_FISH_ID_3)//'" for all output locations'
              MESSAG(3) = 'Modify or remove the FOODSOURCE keyword to change the source ID for FISH_3 concentrations'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
            CALL NXTQOT( IDX, LOC_ID_2 )
            IF( IDX .EQ. 0 ) THEN
              IERR = 45
              MESSAG(1) = 'The FISH_3 modifier on the LOCATION keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Location card PRIMARY ID is : '//TRIM(TMP_ID)
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              CALL MATCH_LOC( LOC_ID_2, IDX2 )
              IF( IDX2 .GT. 0 ) THEN
                ESD_LOC(IDXL)%FISH_3 = IDX2
              ELSE
                IERR = 46
                MESSAG(1) = 'FISH_3 location is not in the master list'
                MESSAG(2) = 'Location and FISH_3 IDs are: '//TRIM(TMP_ID)//' and '//TRIM(LOC_ID_2)
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            END IF
          END IF
!
          IF( CEXIST('MEAT') ) THEN
            IF( SOURCE_MEAT ) THEN
              MESSAG(1) = 'The MEAT location on the LOCATION keyword for location "'//TRIM(ESD_LOC(IDXL)%ID)//'" is being ignored'
              MESSAG(2) = 'The MEAT concentration comes from location "'//TRIM(SOURCE_MEAT_ID)//'" for all output locations'
              MESSAG(3) = 'Modify or remove the FOODSOURCE keyword to change the source ID for MEAT concentrations'
              CALL PRTERR( IERR, CALLER, 3 )
            END IF
            CALL NXTQOT( IDX, LOC_ID_2 )
            IF( IDX .EQ. 0 ) THEN
              IERR = 33
              MESSAG(1) = 'The MEAT modifier on the LOCATION keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Location card PRIMARY ID is : '//TRIM(TMP_ID)
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              CALL MATCH_LOC( LOC_ID_2, IDX2 )
              IF( IDX2 .GT. 0 ) THEN
                ESD_LOC(IDXL)%MEAT = IDX2
              ELSE
                IERR = 34
                MESSAG(1) = 'MEAT is not in the master list'
                MESSAG(2) = 'Location and MEAT IDs are: '//TRIM(TMP_ID)//' and '//TRIM(LOC_ID_2)
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            END IF
          END IF
!
          IF( CEXIST('SECOND') ) THEN
            CALL NXTQOT( IDX, LOC_ID_2 )
            IF( IDX .EQ. 0 ) THEN
              IERR = 35
              MESSAG(1) = 'The SECOND modifier on the HUMAN LOCATION keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Location card PRIMARY ID is : '//TRIM(ESD_LOC(IDXL)%ID)
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              CALL MATCH_LOC( LOC_ID_2, IDX2 )
              IF( IDX2 .GT. 0 ) THEN
                ESD_LOC(IDXL)%SECOND = IDX2
              ELSE
                IERR = 36
                MESSAG(1) = 'Problem with a LOCATION keyword in the HUMAN keyword file'
                MESSAG(2) = 'Secondary location is not in the master list'
                MESSAG(3) = 'Location and secondary IDs are: '//TRIM(ESD_LOC(IDXL)%ID)//' and '//TRIM(LOC_ID_2)
                CALL PRTERR( IERR, CALLER, 3 )
                RETURN
              END IF
            END IF
          END IF
!
          IF( CEXIST('THIRD') ) THEN
            CALL NXTQOT( IDX, LOC_ID_2 )
            IF( IDX .EQ. 0 ) THEN
              IERR = 35
              MESSAG(1) = 'The THIRD modifier on the HUMAN LOCATION keyword did not'
              MESSAG(2) = 'have a quote string associated with it.'
              MESSAG(3) = 'Location card PRIMARY ID is : '//TRIM(ESD_LOC(IDXL)%ID)
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            ELSE
              CALL MATCH_LOC( LOC_ID_2, IDX2 )
              IF( IDX2 .GT. 0 ) THEN
                ESD_LOC(IDXL)%THIRD = IDX2
              ELSE
                IERR = 36
                MESSAG(1) = 'Problem with a LOCATION keyword in the HUMAN keyword file'
                MESSAG(2) = 'Third location is not in the master list'
                MESSAG(3) = 'Location and third IDs are: '//TRIM(ESD_LOC(IDXL)%ID)//' and '//TRIM(LOC_ID_2)
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            END IF
          END IF
!
! ***     Check for optional detailed output (food and analyte details)
!
          IF( CEXIST('OUTPUT') ) ESD_LOC(IDXL)%OUTPUT = .TRUE.
!
!-----------------------------------------------------------------------
        CASE( 'FOODSOIL' ) ! ===> FOODSOIL keyword
!         Tie the indices to the index assumptions in FCDA_Mod
          IF( CEXIST('SODR') ) THEN
            SOIL_IDX = IDXF_SODR
            SOIL_ID = 'SODR'
          END IF
          IF( CEXIST('SOGW') ) THEN
            SOIL_IDX = IDXF_SOGW
            SOIL_ID = 'SOGW'
          END IF
          IF( CEXIST('SOSW') ) THEN
            SOIL_IDX = IDXF_SOSW
            SOIL_ID = 'SOSW'
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'SEED' ) ! ===> SEED keyword
          SDSTOC = VALUE(1)
!
!-----------------------------------------------------------------------
        CASE( 'STOCHAST' ) ! ===> STOCHAST keyword
!
!         Force the user to enter at least one quote string
          IF( NQUOTE .LT. 1 ) THEN
            IERR = 37
            CTMP = QUOTE(1)
            MESSAG(1) = 'One or two quote strings required (STOCHASTIC)'
            MESSAG(2) = 'Stochastic variable: ' // TRIM(CTMP)
            CALL PRTERR( IERR, CALLER, 2 )
          END IF
!
!         Store the quote strings and decode the numerical parameters
!
          CTMP = QUOTE(1)
          CMES = QUOTE(2)
          IF( CEXIST('TRUNCATE') ) THEN
            TRUNC = .TRUE.
          ELSE
            TRUNC = .FALSE.
          END IF
          CALL SDECOD( CTMP, CMES, ONE, TRUNC, VALUE, NVALUE, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 9999
            MESSAG(1) = 'Error for variable ' // CTMP
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'SWEAT' ) ! ===> SWEAT keyword
          SWEAT_SOURCE = 'XXXX'
          GOOD_SWEAT = 0
          IF( CEXIST('GROUND') ) THEN
            SWEAT_SOURCE = 'GWAT'
            GOOD_SWEAT = GOOD_SWEAT + 1
          END IF
          IF( CEXIST('SURFACE') ) THEN
            SWEAT_SOURCE = 'SWAT'
            GOOD_SWEAT = GOOD_SWEAT + 1
          END IF
          IF( CEXIST('SEEP') ) THEN
            SWEAT_SOURCE = 'SEEP'
            GOOD_SWEAT = GOOD_SWEAT + 1
          END IF
          IF( CEXIST('NONE') ) THEN
            SWEAT_SOURCE = 'XXXX'
            GOOD_SWEAT = GOOD_SWEAT + 1
          END IF
          IF( GOOD_SWEAT .NE. 1 ) THEN
            IERR = 38
            MESSAG(1) = 'The SWEAT keyword does not have a valid water source'
            MESSAG(2) = 'Valid options are one of the following modifiers: GROUND, SURFACE, SEEP or NONE'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'SHOWER' ) ! ===> SHOWER keyword
          SHOWER_SOURCE = 'XXXX'
          GOOD_SHOWER = 0
          IF( CEXIST('GROUND') ) THEN
            SHOWER_SOURCE = 'GWAT'
            GOOD_SHOWER = GOOD_SHOWER + 1
          END IF
          IF( CEXIST('SURFACE') ) THEN
            SHOWER_SOURCE = 'SWAT'
            GOOD_SHOWER = GOOD_SHOWER + 1
          END IF
          IF( CEXIST('SEEP') ) THEN
            SHOWER_SOURCE = 'SEEP'
            GOOD_SHOWER = GOOD_SHOWER + 1
          END IF
          IF( CEXIST('NONE') ) THEN
            SHOWER_SOURCE = 'NONE'
            GOOD_SHOWER = GOOD_SHOWER + 1
          END IF
          IF( GOOD_SHOWER .NE. 1 ) THEN
            IERR = 40
            MESSAG(1) = 'The SHOWER keyword does not have a valid water source'
            MESSAG(2) = 'Valid options are one of the following modifiers: GROUND, SURFACE, SEEP or NONE'
            CALL PRTERR( IERR, CALLER, 2 )
            RETURN
          END IF
!
!-----------------------------------------------------------------------
        CASE( 'TIMES' ) ! ===> TIME keyword
          IF( CEXIST('ALL') ) THEN
            DO ITIM = 1, ESD_NUM_TIM
              ESD_TIM(ITIM)%COMP = .TRUE.
            END DO
          ELSE
            DO ITIM = 1, NVALUE
              TEMPTIME = VALUE(ITIM)
              CALL MATCH_TIM( TEMPTIME, IDXT )
              IF( IDXT .GT. 0 ) THEN
                ESD_TIM(IDXT)%COMP = .TRUE.
                HUM_NUM_TIM = HUM_NUM_TIM + 1
              ELSE
                IERR = 39
                MESSAG(1) = 'Time requested is not in the master list'
                MESSAG(2) = 'Invalid time is '
                WRITE(MESSAG(2)(17:),*) TEMPTIME
                CALL PRTERR( IERR, CALLER, 2 )
                RETURN
              END IF
            END DO
          END IF
!
!-----------------------------------------------------------------------
        CASE DEFAULT ! Ignore all other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
!
      GO TO 10
!
      END SUBROUTINE

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

      SUBROUTINE MATCH_SPC( LABEL, IDX )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine attempts to find a match between an specie ID
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
!!    Paul W. Eslinger :  7 Feb 2003 : SAC Rev. 1
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
      INTEGER :: ISPC ! Looping variable
!
!---- Executable code -------------------------------------------------------------
!
      IDX = -1
      DO ISPC = 1, ESD_NUM_SPC
        IF( ESD_SPC(ISPC)%ID .EQ. LABEL ) THEN
          IDX = ISPC
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE

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
      USE ESD_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IDX
      INTEGER :: TIME
!
! *** Local variables
      INTEGER :: ITIM ! Looping variable
!
!---- Executable code -------------------------------------------------------------
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
!!    Paul W. Eslinger : 20 Mar 2000 : Version 1.0
!!    Carmen Arimescu  : 28 Feb 2003 : SAC Rev. 1
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
      USE Files_Mod
      USE Errors_Mod
      USE Esd_Mod
      USE Ecda_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR
      INTEGER :: IANA ! Looping variable for analytes
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'OPEN_CON'
      LOGICAL :: THERE ! Local file exist flag
!
!---- Executable code -------------------------------------------------------------
!
! *** Loop over all possible analytes
!
      DO IANA = 1, ESD_NUM_ANA
!
! ***   Only look at analytes selected for this scenario
!
        IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
!
        INQUIRE( FILE=FNCON(IANA), EXIST=THERE )
        IF( .NOT.THERE ) THEN
          IERR = 1
          MESSAG(1) = 'The requested ECDA concentration file was not found'
          MESSAG(2) = 'Analyte ID: '//ESD_ANA(IANA)%ID
          MESSAG(3) = 'File: '//TRIM(FNCON(IANA))
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
!
! ***   Get a unit number
!
        ICON(IANA) = GET_UNIT_NUMBER(  )
        IF( ICON(IANA) .LT. 7 ) THEN
          IERR = 2
          MESSAG(1) = 'Unable to get unit number for ECDA concentration file'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
!
! ***   Open the concentration data file
!
        CALL ECDA_OPEN( FNCON(IANA), ICON(IANA), IERR )
        IF ( IERR .NE. 0 ) THEN
          IERR = 3
          MESSAG(1) = 'System error opening the ECDA concentration file'
          MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
          MESSAG(3) = 'File: ' // TRIM(FNCON(IANA))
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
!
! ***   Set the units conversion for this analyte
!
        CALL SET_UNITS( IANA, IERR )
        IF ( IERR .NE. 0 ) THEN
          IERR = 4
          MESSAG(1) = 'Error in lower level routine'
          MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
          MESSAG(3) = 'File: ' // TRIM(FNCON(IANA))
          CALL PRTERR( IERR, CALLER, 3 )
          RETURN
        END IF
!
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE OPEN_DETAIL( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain the
!!    detailed risks calculations by analyte and realization for each
!!    location.
!!
!!  History:
!!
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
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
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      INTEGER :: IERF ! Status variable for open statement
      CHARACTER(LEN=11) :: CALLER = 'OPEN_DETAIL'
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      IDET = GET_UNIT_NUMBER(  )
      IF( IDET .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for details file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Attempt to open the output risks file
!
      OPEN(IDET,FILE=FNDET,STATUS='UNKNOWN',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the detailed risk file'
        MESSAG(2) = 'File name entered on the FILE keyword, STATS modifier'
        MESSAG(3) = 'File: '//TRIM(FNDET)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Write an identification line
!
      WRITE(IDET,1000,IOSTAT=IERF) &
        '"Time","Location ID","Analyte ID","Analyte Type","Solution Type","Units","Values by realization"'
 1000 FORMAT(A)
      IF( IERF .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'System error writing the detailed data file header line'
        MESSAG(2) = 'System error number is '
        WRITE(MESSAG(2)(24:),*) IERF
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE OPEN_ESD( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the ESD keyword file for reading keywords.
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Mar 2000 : Version 2.0
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
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR
      INTEGER :: IERF ! Status variable for open statement
!
! *** Local variables
      LOGICAL THERE
      CHARACTER(LEN=8) :: CALLER = 'OPEN_ESD'
!
!---- Executable code -------------------------------------------------------------
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
        MESSAG(1) = 'Unable to get unit number for ESD keyword file'
        CALL PRTERR( IERR, CALLER, 1 )
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

      SUBROUTINE OPEN_FOODS( IERR )
!!*********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the input food concentration files
!!    used for consumption.
!!
!!  Call List Variables:
!!
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in opening the file
!!
!!  History:
!!
!!    Carmen Arimescu  : 19 Feb 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 17 Apr 2003 : Change error reporting
!!    Paul W. Eslinger : 26 Jun 2003 : Allow runs with no foods
!!    Paul W. Eslinger : 23 Oct 2003 : Change output formats
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!    Paul W. Eslinger :  5 Jun 2007 : Change FCDA routines to get ESD parameters
!!                                     through the call list rather than a module
!!
!!*********************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Errors_Mod
      USE Control_Mod
      USE FCDA_Mod
      USE ESD_Mod
      USE Debug_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR ! Error indication
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'OPEN_FOODS' ! Name of this subroutine
!
      INTEGER :: IANA ! Looping variable for analytes
      INTEGER :: ISPC ! Looping variable for species
      INTEGER :: J    ! Generic looping index
!
      CHARACTER(LEN=6) :: FCDA_ANA_ID   ! Analyte ID: First header line
      CHARACTER(LEN=6) :: FCDA_SPC_ID   ! Species ID: Second header line
      CHARACTER(LEN=8) :: FCDA_SPC_HAB  ! Species habitat
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
      IF( BGFOOD ) THEN
        WRITE(IRPT,'(A/)') 'Entering subroutine ' // CALLER
        WRITE(IRPT,'(A)') 'Opening all food files'
      END IF
!
! *** Exit if no foods are needed
!
      IF( .NOT.(USE_LEAFVEG .OR. USE_ROOTVEG .OR. USE_GRAIN .OR. USE_FRUIT .OR. &
                USE_BIRD    .OR. USE_MEAT    .OR. USE_MILK  .OR. USE_FISH .OR. &
                USE_FISH_2  .OR. USE_FISH_3  .OR. USE_EGGS) ) RETURN
!
      WRITE(IRPT,'(/A)') 'Information on food files used in this run'
!
! *** Get the index map data for all foods
!
      FOODMAP = TRIM(FOODPATH)//TRIM(FOODMAP)
      IF( BGFOOD ) THEN
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
!
! *** Further error checks on time data
      DO J = 1, ESD_NUM_TIM
        IF( FCDA_TIM(J)%TIME .NE. ESD_TIM(J)%TIME ) THEN
          IERR = 2
          MESSAG(1) = 'Mismatch on times in the FCDA map file and the'
          MESSAG(2) = 'times in the ESD keyword file'
          MESSAG(3) = 'FCDA: '
          WRITE(MESSAG(3)(7:),'(I0)') FCDA_TIM(J)%TIME
          MESSAG(4) = 'ESD: '
          WRITE(MESSAG(4)(7:),'(I0)') ESD_TIM(J)%TIME
          CALL PRTERR( IERR, CALLER, 4 )
        END IF
      END DO
!
! *** Further error checks on location data
      DO J = 1, ESD_NUM_LOC
        IF( FCDA_LOC(J)%ID .NE. ESD_LOC(J)%ID ) THEN
          IERR = 3
          MESSAG(1) = 'Mismatch on locations in the FCDA map file and the'
          MESSAG(2) = 'locations in the ESD keyword file'
          MESSAG(3) = 'FCDA: "' // FCDA_LOC(J)%ID // '"'
          MESSAG(4) = 'ESD:  "' // ESD_LOC(J)%ID // '"'
          CALL PRTERR( IERR, CALLER, 4 )
        END IF
      END DO
!
! *** Further error checks on species data
      DO J = 1, ESD_NUM_SPC
        IF( FCDA_SPC(J)%ID .NE. ESD_SPC(J)%ID ) THEN
          IERR = 4
          MESSAG(1) = 'Mismatch on species in the FCDA map file and the'
          MESSAG(2) = 'species in the ESD keyword file'
          MESSAG(3) = 'FCDA: "' // FCDA_SPC(J)%ID // '"'
          MESSAG(4) = 'ESD:  "' // ESD_SPC(J)%ID // '"'
          CALL PRTERR( IERR, CALLER, 4 )
        END IF
      END DO
!
      IF( IERR .NE. 0 ) RETURN
!
      IF( BGFOOD ) WRITE(IRPT,'(A)') 'Food map data file read successfully'
!
! *** Loop over analytes and food species and open the files
!
      DO IANA = 1, ESD_NUM_ANA
        IF( .NOT. ESD_ANA(IANA)%COMP ) CYCLE
!
! -------------------------------------------------
! ***   Food: FISH
! -------------------------------------------------
!
        ISPC = FISH_FIDX
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 5
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = FISH'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: FISH      Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 6
            MESSAG(1) = 'System error opening the FOOD concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 7
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 8
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
! -------------------------------------------------
! ***   Food: FISH_2
! -------------------------------------------------
!
        ISPC = FISH_FIDX_2
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 9
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = FISH_2'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: FISH_2    Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 10
            MESSAG(1) = 'System error opening the FOOD_2 concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 11
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 12
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
! -------------------------------------------------
! ***   Food: FISH_3
! -------------------------------------------------
!
        ISPC = FISH_FIDX_3
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 13
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = FISH_3'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: FISH_3    Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 14
            MESSAG(1) = 'System error opening the FOOD_3 concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 15
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 16
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
! -------------------------------------------------
! ***   Food: LEAFVEG
! -------------------------------------------------
!
        ISPC = LEAFVEG_FIDX
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 17
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = LEAFVEG'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: LEAFVEG   Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF ( IERR .NE. 0 ) THEN
            IERR = 18
            MESSAG(1) = 'System error opening the FOOD concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 19
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 20
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
! -------------------------------------------------
! ***   Food: ROOTVEG
! -------------------------------------------------
!
        ISPC = ROOTVEG_FIDX
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 21
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = ROOTVEG'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: ROOTVEG   Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF ( IERR .NE. 0 ) THEN
            IERR = 22
            MESSAG(1) = 'System error opening the FOOD concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 23
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 24
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
! -------------------------------------------------
! ***   Food: GRAIN
! -------------------------------------------------
!
        ISPC = GRAIN_FIDX
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 25
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = GRAIN'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: GRAIN     Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF ( IERR .NE. 0 ) THEN
            IERR = 26
            MESSAG(1) = 'System error opening the FOOD concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 27
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 28
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
! -------------------------------------------------
! ***   Food: FRUIT
! -------------------------------------------------
!
        ISPC = FRUIT_FIDX
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 29
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = FRUIT'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: FRUIT     Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF ( IERR .NE. 0 ) THEN
            IERR = 30
            MESSAG(1) = 'System error opening the FOOD concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 31
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 32
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
! -------------------------------------------------
! ***   Food: MEAT
! -------------------------------------------------
!
        ISPC = MEAT_FIDX
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 33
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = MEAT'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: MEAT      Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF ( IERR .NE. 0 ) THEN
            IERR = 34
            MESSAG(1) = 'System error opening the FOOD concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 35
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 36
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
! -------------------------------------------------
! ***   Food: MILK
! -------------------------------------------------
!
        ISPC = MILK_FIDX
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 37
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = MILK'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: MILK      Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF ( IERR .NE. 0 ) THEN
            IERR = 38
            MESSAG(1) = 'System error opening the FOOD concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 39
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 40
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
! -------------------------------------------------
! ***   Food: BIRD
! -------------------------------------------------
!
        ISPC = BIRD_FIDX
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 41
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = BIRD'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: BIRD      Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF ( IERR .NE. 0 ) THEN
            IERR = 42
            MESSAG(1) = 'System error opening the FOOD concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 43
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 44
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
! -------------------------------------------------
! ***   Food: EGGS
! -------------------------------------------------
!
        ISPC = EGGS_FIDX
        IF( ISPC .GT. 0 ) THEN
!
!         Get a unit number
          IFOD(IANA,ISPC) = GET_UNIT_NUMBER( )
          IF( IFOD(IANA,ISPC) .LT. 7 ) THEN
            IERR = 45
            MESSAG(1) = 'Unable to get unit number for food concentration file'
            MESSAG(2) = 'Food type = EGGS'
            MESSAG(3) = 'Analyte ID = ' // ESD_ANA(IANA)%ID
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          ENDIF
!
!         Create the name of the food concentration data file
          FNFOD(IANA,ISPC) = TRIM(FOODPATH) // 'Food_' // TRIM(ESD_ANA(IANA)%ID) // &
            '_' // TRIM(ESD_SPC(ISPC)%ID) // '.fod'
          WRITE(IRPT,'(A,I3,A,A,A)') 'Food: EGGS      Unit: ',IFOD(IANA,ISPC), '  Analyte: "'//ESD_ANA(IANA)%ID, &
             '"  Species: "'//ESD_SPC(ISPC)%ID, '"  File: "'//TRIM(FNFOD(IANA,ISPC))//'"'
!
!         Open the file
          CALL FCDA_OPEN( FNFOD(IANA,ISPC), IFOD(IANA,ISPC), FCDA_ANA_ID, FCDA_SPC_ID, FCDA_SPC_HAB, IERR )
          IF ( IERR .NE. 0 ) THEN
            IERR = 46
            MESSAG(1) = 'System error opening the FOOD concentration file'
            MESSAG(2) = 'Analyte ID: ' // ESD_ANA(IANA)%ID
            MESSAG(3) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Check the data in the first two lines
          IF( FCDA_ANA_ID .NE. ESD_ANA(IANA)%ID ) THEN
            IERR = 47
            MESSAG(1) = 'Mismatch on the analyte ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_ANA(IANA)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_ANA_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
          IF( FCDA_SPC_ID .NE. ESD_SPC(ISPC)%ID ) THEN
            IERR = 48
            MESSAG(1) = 'Mismatch on the species ID from the FCDA file'
            MESSAG(2) = 'Data sent to file is  "' // ESD_SPC(ISPC)%ID // '"'
            MESSAG(3) = 'Data from the file is "' // FCDA_SPC_ID // '"'
            MESSAG(4) = 'File: ' // TRIM(FNFOD(IANA,ISPC))
            CALL PRTERR( IERR, CALLER, 4 )
            RETURN
          END IF
        END IF
!
      END DO
!
      RETURN
      END SUBROUTINE

      SUBROUTINE OPEN_KEY( IERR )
!!********************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine performs two actions for the Human keyword file:
!!      1. Obtain the file name
!!         a) First check the command line to see if the file
!!            name was provided or,
!!         b) query the user for the file name if it was not
!!            entered on the command line.
!!      2. Open the file for use by keyword reading subroutines
!!
!!  History:
!!
!!    Paul W. Eslinger : 26 Sep 1996 : Version 1.0
!!    Paul W. Eslinger : 25 Apr 2000 : Version 2.0
!!    Paul W. Eslinger : 31 Mar 2005 : Change to NARGS, GETARG from GETCL
!!    Paul W. Eslinger :  5 Jun 2007 : Udate to TIIA, add copyright information
!!    Paul W. Eslinger :  9 Jul 2012 : Revise to a common callable routine
!!
!!  Call List Variables:
!!
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
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
      INTEGER, EXTERNAL :: NARGS ! Lahey utility subroutine
!
! *** Call list variables
      INTEGER :: IERR ! Error number (0=no errors)
!
! *** Local variables
      LOGICAL :: THERE ! File existence variable
      INTEGER :: IERF  ! Status variable for open statement
      INTEGER :: NUM_ARGS      ! Number of command line arguments
      INTEGER :: NUM_FNAM      ! Argument number for the keyword file name
      CHARACTER(LEN=5) :: HELP ! Character for checking for help
!
!---- Executable code -------------------------------------------------------------
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
! *** Get a unit number
!
      IKEY = GET_UNIT_NUMBER(  )
      IF( IKEY .LT. 7 ) THEN
        IERR = 2
        WRITE(*,1030) TRIM(FNKEY)
 1030   FORMAT(' Unable to get unit number for the HUMAN keyword file'/ &
               ' File: ',A)
        RETURN
      END IF
!
! *** Attempt to open the file
!
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

      SUBROUTINE OPEN_SUMMARY( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine opens the output file which will contain summary statistics data.
!!
!!  History:
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 15 Jul 2012 : Fix header line (1% and 99%)
!!
!!  Call List Variables:
!!    IERR  : Output integer error flag
!!            0 = No errors
!!           >0 = Error in opening the file
!!
!!**************************************************************************************************
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
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Local variables
      CHARACTER(LEN=12) :: CALLER = 'OPEN_SUMMARY' ! Name of this subroutine
      INTEGER :: IERA ! Error indicator from the file system
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Check on the file name
!
      IF( FNSUM .EQ. ' ' ) THEN
        IERR = 1
        MESSAG(1) = 'The output summary statistics file name is missing'
        MESSAG(2) = 'Use the STATS modifier on the FILE Keyword'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Get a unit number
!
      ISUM = GET_UNIT_NUMBER(  )
      IF( ISUM .LT. 7 ) THEN
        IERR = 2
        MESSAG(1) = 'Unable to get unit number for the summary statistics file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Open the output summary statistics file
!
      OPEN(ISUM,FILE=FNSUM,STATUS='UNKNOWN',IOSTAT=IERA)
      IF( IERA .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'System error opening the summary statistics file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Write an identification line
!
      WRITE(ISUM,1000,IOSTAT=IERA) &
        '"Time","Location ID","Analyte ID","Analyte Type","Solution Type","Units",'//&
        '"Minimum","1st Percentile","5th Percentile","10th Percentile","25th Percentile",'//&
        '"Median","75th Percentile","90th Percentile","95th Percentile","99th Percentile",'//&
        '"Maximum","Mean","Standard Deviation"'
 1000 FORMAT(A)
      IF( IERA .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'System error writing the summary statistics file header line'
        MESSAG(2) = 'System error number is '
        WRITE(MESSAG(2)(24:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE OPEN_VALUE( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine opens the output file which will contain the
!!    generated values for each stochastic variable.
!!
!!  History:
!!
!!    Paul W. Eslinger : 26 Sep 2000 : Version 1.0
!!    Paul W. Eslinger : 11 Apr 2003 : SAC Rev. 1
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
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER, EXTERNAL :: GET_UNIT_NUMBER
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'OPEN_VALUE'
      INTEGER :: IERF ! Status variable for open statement
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Get a unit number
!
      IVLU = GET_UNIT_NUMBER(  )
      IF( IVLU .LT. 7 ) THEN
        IERR = 1
        MESSAG(1) = 'Unable to get unit number for the stochastic data file'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Attempt to the file
!
      OPEN(IVLU,FILE=FNVAL,STATUS='UNKNOWN',IOSTAT=IERF)
      IF( IERF .NE. 0  ) THEN
        IERR = 2
        MESSAG(1) = 'System error opening the generated stochastic data file'
        MESSAG(2) = 'File name entered on the FILE keyword, VALUE modifier'
        MESSAG(3) = 'File: '//TRIM(FNVAL)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

      REAL FUNCTION PERCENT( VALUE, TOTAL )
!!*******************************************************************************
!!
!!  Purpose:
!!
!!    This function returns the value of (VALUE/TOTAL) as a percent.  If TOTAL
!!    is near zero the value HUGE(1.0) is returned.
!!
!!  History:
!!    Paul W. Eslinger : 28 Feb 2006 : Original source
!!
!!*******************************************************************************
!
! *** Call list variables
      REAL(KIND=4), INTENT(IN) :: VALUE
      REAL(KIND=4), INTENT(IN) :: TOTAL
!
! *** Local variables
      REAL(KIND=4), PARAMETER :: SMALL = 1.0E-25
!
!---- Executable code -------------------------------------------------------------
!
      IF( ABS(TOTAL) .GT. SMALL ) THEN
        PERCENT = 100.0 * VALUE / TOTAL
      ELSE
        PERCENT = HUGE(1.0)
      END IF
!
      RETURN
      END FUNCTION

      REAL FUNCTION PRINTRISK( DOSEVAL, RISKFACT, RISKFLAG, RACTION )
!!*******************************************************************************
!!
!!  Purpose:
!!
!!    This function returns the value of a hazardous or carcinogenic risk to
!!    print.  Carcinogenic risk uses RACTION=1, hazardous risk ises RACTION=2
!!
!!    If RACTION is 1
!!      If RISKFLAG is .TRUE. the value DOSEVAL*RISKFACT is returned.
!!      Otherwise the value of 0.0 is returned.
!!
!!    If RACTION is 2
!!      If RISKFLAG is .TRUE. the value DOSEVAL*RISKFACT is returned.
!!      Otherwise the value of 0.0 is returned.
!!
!!    If RACTION is not 1 or 2
!!      The value of HUGE(1.0) is returned
!!
!!  History:
!!    Paul W. Eslinger : 28 Feb 2006 : Original source
!!
!!*******************************************************************************
!
! *** Call list variables
      REAL(KIND=4), INTENT(IN) :: DOSEVAL
      REAL(KIND=4), INTENT(IN) :: RISKFACT
      LOGICAL, INTENT(IN) :: RISKFLAG
      INTEGER, INTENT(IN) :: RACTION
!
!---- Executable code -------------------------------------------------------------
!
!     For carcinogenic risk (slope factors are multipliers)
      IF( RACTION .EQ. 1 ) THEN
        PRINTRISK = 0.0
        IF( RISKFLAG ) PRINTRISK = DOSEVAL * RISKFACT
        RETURN
      END IF
!
!     For hazardous risk (the reference dose values are divisors)
      IF( RACTION .EQ. 2 ) THEN
        PRINTRISK = 0.0
        IF( RISKFLAG ) PRINTRISK = DOSEVAL / RISKFACT
        RETURN
      END IF
!
!     Return an outlandish value for an input error
      PRINTRISK = HUGE(1.0)
!
      RETURN
      END FUNCTION

      SUBROUTINE RAD_DOSE( ITIM, ILOC, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine computes the dose, risk, and poplulation dose or risk increment from all
!!    radioactive analytes at a single location at a single time.
!!
!!  History:
!!
!!    Paul W. Eslinger :  3 Oct 1996 : Version 1.0
!!    Paul W. Eslinger : 10 Apr 2000 : Version 2.0
!!    Paul W. Eslinger :  4 Sep 2002 : Change debug formats
!!    Carmen Arimescu  : 25 Feb 2003 : SAC Rev.1
!!    Paul W. Eslinger : 21 Sep 2004 : Set CSWEAT concentrations
!!    Paul W. Eslinger :  3 Mar 2005 : SCR-1073 Add ETGROUND
!!    Paul W. Eslinger : 13 May 2005 : Restructure debug outputs
!!    Paul W. Eslinger : 19 May 2005 : Split out air increments for debugs
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!    Paul W. Eslinger :  2 Dec 2005 : Rearrange debug outputs
!!    Paul W. Eslinger : 28 Feb 2006 : Implement SCR-1107.  Revise the VERBOSE outputs to provide
!!      more information.  Remove drinking water calculations, Modify population dose calculations
!!      Output both dose and risk information, revise the output selection options.  Change to
!!      calculate risk from direct inputs rather than using dose to risk conversions.
!!
!!**************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Param_Mod
      USE Files_Mod
      USE Seeds_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Concen_Mod
      USE Stocvars_Mod
      USE Results_Mod
      USE Errors_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: PERCENT
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER :: IERR             ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=9)   :: CALLER = 'RAD_DOSE'
      CHARACTER(LEN=160) :: CTMP ! Temporary character variable
      INTEGER :: LEN_CTMP        ! Length (nonblank) of CTMP
!
      REAL :: XMIN ! Statistics - minimum
      REAL :: XV01 ! Statistics -  1st percentile
      REAL :: XV05 ! Statistics -  5th percentile
      REAL :: XV10 ! Statistics - 10th percentile
      REAL :: XV25 ! Statistics - 25th percentile
      REAL :: XMED ! Statistics - median
      REAL :: XV75 ! Statistics - 75th percentile
      REAL :: XV90 ! Statistics - 90th percentile
      REAL :: XV95 ! Statistics - 95th percentile
      REAL :: XV99 ! Statistics - 99th percentile
      REAL :: XMAX ! Statistics - maximum
      REAL :: XAVG ! Statistics - average
      REAL :: XSTD ! Statistics - standard deviation
!
      INTEGER :: IREL ! Realization looping index
      INTEGER :: IANA ! Analyte looping index
!
      REAL :: TMP_ING_WATER_D          ! Water dose increment
      REAL :: TMP_ING_SWAT_D           ! Water dose increment - surface water
      REAL :: TMP_ING_GWAT_D           ! Water dose increment - ground water
      REAL :: TMP_ING_SEEP_D           ! Water dose increment - seeps
      REAL :: TMP_ING_SOIL_D           ! Soil dose increment
      REAL :: TMP_ING_FOOD_D           ! Food dose increment
      REAL :: TMP_ING_LEAFVEG_D        ! Leafy vegetable dose increment
      REAL :: TMP_ING_ROOTVEG_D        ! Root vegetable dose increment
      REAL :: TMP_ING_MEAT_D           ! Meat dose increment
      REAL :: TMP_ING_MILK_D           ! Milk dose increment
      REAL :: TMP_ING_BIRD_D           ! Bird dose increment
      REAL :: TMP_ING_FRUIT_D          ! Fruit dose increment
      REAL :: TMP_ING_GRAIN_D          ! Grain dose increment
      REAL :: TMP_ING_EGGS_D           ! Eggs dose increment
      REAL :: TMP_ING_FISH_D           ! Fish dose increment
      REAL :: TMP_ING_FISH_2_D         ! Fish_3 dose increment
      REAL :: TMP_ING_FISH_3_D         ! Fish_3 dose increment
      REAL :: TMP_ING_SEDIMENT_D       ! Sediment dose increment
      REAL :: TMP_ING_SEDIMENT_CHILD_D ! Sediment dose increment - child
      REAL :: TMP_ING_SEDIMENT_ADULT_D ! Sediment dose increment - adult
      REAL :: TMP_ING_SOIL_CHILD_D     ! Soil dose increment - child
      REAL :: TMP_ING_SOIL_ADULT_D     ! Soil dose increment - child
      REAL :: TMP_ING_DIRT_D           ! Soil/sediment dose increment
!
      REAL :: TMP_ING_WATER_R          ! Water risk increment
      REAL :: TMP_ING_SWAT_R           ! Water risk increment - surface water
      REAL :: TMP_ING_GWAT_R           ! Water risk increment - ground water
      REAL :: TMP_ING_SEEP_R           ! Water risk increment - seeps
      REAL :: TMP_ING_SOIL_R           ! Soil risk increment
      REAL :: TMP_ING_FOOD_R           ! Food risk increment
      REAL :: TMP_ING_LEAFVEG_R        ! Leafy vegetable risk increment
      REAL :: TMP_ING_ROOTVEG_R        ! Root vegetable risk increment
      REAL :: TMP_ING_MEAT_R           ! Meat risk increment
      REAL :: TMP_ING_MILK_R           ! Milk risk increment
      REAL :: TMP_ING_BIRD_R           ! Bird risk increment
      REAL :: TMP_ING_FRUIT_R          ! Fruit risk increment
      REAL :: TMP_ING_GRAIN_R          ! Grain risk increment
      REAL :: TMP_ING_EGGS_R           ! Eggs risk increment
      REAL :: TMP_ING_FISH_R           ! Fish risk increment
      REAL :: TMP_ING_FISH_2_R         ! Fish_3 risk increment
      REAL :: TMP_ING_FISH_3_R         ! Fish_3 risk increment
      REAL :: TMP_ING_SEDIMENT_R       ! Sediment risk increment
      REAL :: TMP_ING_SEDIMENT_CHILD_R ! Sediment risk increment - child
      REAL :: TMP_ING_SEDIMENT_ADULT_R ! Sediment risk increment - adult
      REAL :: TMP_ING_SOIL_CHILD_R     ! Soil risk increment - child
      REAL :: TMP_ING_SOIL_ADULT_R     ! Soil risk increment - child
      REAL :: TMP_ING_DIRT_R           ! Soil/sediment risk increment
!
      REAL :: TMP_INH_SOIL_D   ! Dose increment from air mass loading of soil
      REAL :: TMP_INH_SEEP_D   ! Dose increment from seep water in air
      REAL :: TMP_INH_SWAT_D   ! Dose increment from surface water in air
      REAL :: TMP_INH_SHOWER_D ! Dose increment from shower water in air
      REAL :: TMP_INH_SWEAT_D  ! Dose increment from sweat lodge air
      REAL :: TMP_INH_AIRC_D   ! Dose increment from farfield air concentrations
!
      REAL :: TMP_INH_SOIL_R   ! Risk increment from air mass loading of soil
      REAL :: TMP_INH_SEEP_R   ! Risk increment from seep water in air
      REAL :: TMP_INH_SWAT_R   ! Risk increment from surface water in air
      REAL :: TMP_INH_SHOWER_R ! Risk increment from shower water in air
      REAL :: TMP_INH_SWEAT_R  ! Risk increment from sweat lodge air
      REAL :: TMP_INH_AIRC_R   ! Risk increment from farfield air concentrations
!
      REAL :: TMP_EXT_SWIM_D   ! Dose increment from external exposure while swimming
      REAL :: TMP_EXT_BOAT_D   ! Dose increment from external exposure while boating
      REAL :: TMP_EXT_SOIL_D   ! Dose increment from external exposure from soil
      REAL :: TMP_EXT_SEDI_D   ! Dose increment from external exposure from sedimet
!
      REAL :: TMP_EXT_SWIM_R   ! Risk increment from external exposure while swimming
      REAL :: TMP_EXT_BOAT_R   ! Risk increment from external exposure while boating
      REAL :: TMP_EXT_SOIL_R   ! Risk increment from external exposure from soil
      REAL :: TMP_EXT_SEDI_R   ! Risk increment from external exposure from sedimet
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
      IF( BGRAD ) THEN
!
        WRITE(IRPT,1000) 'Entering ' // CALLER
 1000   FORMAT(/A)
!
        CTMP = 'Location index '
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ILOC
        CTMP = TRIM(CTMP) // ' : '//ESD_LOC(ILOC)%ID // ' : "' //TRIM(ESD_LOC(ILOC)%NAME) // '"'
        WRITE(IRPT,1010) TRIM(CTMP)
 1010   FORMAT(A)
!
        CTMP = 'Time index '
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ITIM
        CTMP = TRIM(CTMP) // ' : Calendar year'
        LEN_CTMP = LEN_TRIM( CTMP )
        WRITE(CTMP((LEN_CTMP+2):),'(I0)') ESD_TIM(ITIM)%TIME
        WRITE(IRPT,1010) TRIM(CTMP)
!
        WRITE(IRPT,1020) NREAL, 'Numer of realizations to be processed'
 1020   FORMAT(3X,I0,' : ',A)
!
!        WRITE(IRPT,1030) CF4, 'CF4'
 1030   FORMAT(3X,1P,E12.5,' : ',A)
!
      END IF
!
! *** Initialize the combined risks and doses to zero for this location
!
      DO IREL = 1, NREAL
        DOSERAD(IREL) = 0.0
        RISKRAD(IREL) = 0.0
      END DO
!
! *** Fill the soil concentrations to match the location and food types
!
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'AQUATIC' )  FORALL (IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = 0.0
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'RIPARIAN' ) FORALL (IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SORP(IANA,IREL)
      IF( ESD_LOC(ILOC)%TYPE .EQ. 'UPLAND' ) THEN
        IF( SOIL_IDX .EQ. 1 ) FORALL (IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SODR(IANA,IREL) ! Dry location
        IF( SOIL_IDX .EQ. 2 ) FORALL (IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SOGW(IANA,IREL) ! Groundwater irrigation
        IF( SOIL_IDX .EQ. 3 ) FORALL (IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSOIL(IANA,IREL) = SOSW(IANA,IREL) ! Surfacewater irrigation
      END IF
!
! *** Fill the sweat lodge water concentrations
!       Default to zero if the sweat lodge option is not used
      CSWEAT = 0.0
      IF( SWEAT_SOURCE .EQ. 'GWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSWEAT(IANA,IREL) = GWAT(IANA,IREL)
      IF( SWEAT_SOURCE .EQ. 'SWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSWEAT(IANA,IREL) = SWAT(IANA,IREL)
      IF( SWEAT_SOURCE .EQ. 'SEEP' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSWEAT(IANA,IREL) = SEEP(IANA,IREL)
!
! *** Fill the shower water concentrations
!       Default to zero if the showering option is not used
      CSHOWER = 0.0
      IF( SHOWER_SOURCE .EQ. 'GWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSHOWER(IANA,IREL) = GWAT(IANA,IREL)
      IF( SHOWER_SOURCE .EQ. 'SWAT' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSHOWER(IANA,IREL) = SWAT(IANA,IREL)
      IF( SHOWER_SOURCE .EQ. 'SEEP' ) FORALL(IREL=1:NREAL, IANA=1:ESD_NUM_ANA) CSHOWER(IANA,IREL) = SEEP(IANA,IREL)
!
! *** Loop over analytes and process those that are radioactive
      DO IANA = 1, ESD_NUM_ANA
!
        IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE       ! Skip all analytes that are not requested
        IF( .NOT.ESD_ANA(IANA)%ANATYP_RAD ) CYCLE ! Skip all analytes that are not radioactive
!
!       Loop over all realizations for this analyte
        DO IREL = 1, NREAL
!
!         ----------------------------------- Ingestion -----------------------------------
!
! ***     Ingestion - water
          TMP_ING_SWAT_D  = SWAT(IANA,IREL) * INGEST(IREL)%IRSURFACE * EXPOS(IREL)%ED * EXPOS(IREL)%EFSURFACE * DFING(IANA,IREL)
          TMP_ING_GWAT_D  = GWAT(IANA,IREL) * INGEST(IREL)%IRGROUND  * EXPOS(IREL)%ED * EXPOS(IREL)%EFGROUND  * DFING(IANA,IREL)
          TMP_ING_SEEP_D  = SEEP(IANA,IREL) * INGEST(IREL)%IRSEEP    * EXPOS(IREL)%ED * EXPOS(IREL)%EFSEEP    * DFING(IANA,IREL)
          TMP_ING_SWAT_R  = SWAT(IANA,IREL) * INGEST(IREL)%IRSURFACE * EXPOS(IREL)%ED * EXPOS(IREL)%EFSURFACE * RFING(IANA,IREL)
          TMP_ING_GWAT_R  = GWAT(IANA,IREL) * INGEST(IREL)%IRGROUND  * EXPOS(IREL)%ED * EXPOS(IREL)%EFGROUND  * RFING(IANA,IREL)
          TMP_ING_SEEP_R  = SEEP(IANA,IREL) * INGEST(IREL)%IRSEEP    * EXPOS(IREL)%ED * EXPOS(IREL)%EFSEEP    * RFING(IANA,IREL)
!
          TMP_ING_WATER_D = TMP_ING_SWAT_D + TMP_ING_GWAT_D + TMP_ING_SEEP_D
          TMP_ING_WATER_R = TMP_ING_SWAT_R + TMP_ING_GWAT_R + TMP_ING_SEEP_R
!
! ***     Ingestion - soil
          TMP_ING_SOIL_CHILD_D = CSOIL(IANA,IREL) * INGEST(IREL)%IRSOILCHILD * EXPOS(IREL)%EDCHILD * &
                                 EXPOS(IREL)%EFSOIL * DFING(IANA,IREL)
          TMP_ING_SOIL_ADULT_D = CSOIL(IANA,IREL) * INGEST(IREL)%IRSOILADULT * EXPOS(IREL)%EDADULT * &
                                 EXPOS(IREL)%EFSOIL * DFING(IANA,IREL)
          TMP_ING_SOIL_CHILD_R = CSOIL(IANA,IREL) * INGEST(IREL)%IRSOILCHILD * EXPOS(IREL)%EDCHILD * &
                                 EXPOS(IREL)%EFSOIL * RFING(IANA,IREL)
          TMP_ING_SOIL_ADULT_R = CSOIL(IANA,IREL) * INGEST(IREL)%IRSOILADULT * EXPOS(IREL)%EDADULT * &
                                 EXPOS(IREL)%EFSOIL * RFING(IANA,IREL)
!
          TMP_ING_SOIL_D  = TMP_ING_SOIL_CHILD_D + TMP_ING_SOIL_ADULT_D
          TMP_ING_SOIL_R  = TMP_ING_SOIL_CHILD_R + TMP_ING_SOIL_ADULT_R
!
! ***     Ingestion - sediment
          TMP_ING_SEDIMENT_CHILD_D = SEDI(IANA,IREL) * INGEST(IREL)%IRSEDCHILD * EXPOS(IREL)%EDCHILD * &
                                     EXPOS(IREL)%EFSED * DFING(IANA,IREL)
          TMP_ING_SEDIMENT_ADULT_D = SEDI(IANA,IREL) * INGEST(IREL)%IRSEDADULT * EXPOS(IREL)%EDADULT * &
                                     EXPOS(IREL)%EFSED * DFING(IANA,IREL)
          TMP_ING_SEDIMENT_CHILD_R = SEDI(IANA,IREL) * INGEST(IREL)%IRSEDCHILD * EXPOS(IREL)%EDCHILD * &
                                     EXPOS(IREL)%EFSED * RFING(IANA,IREL)
          TMP_ING_SEDIMENT_ADULT_R = SEDI(IANA,IREL) * INGEST(IREL)%IRSEDADULT * EXPOS(IREL)%EDADULT * &
                                     EXPOS(IREL)%EFSED * RFING(IANA,IREL)
          TMP_ING_SEDIMENT_D = TMP_ING_SEDIMENT_CHILD_D + TMP_ING_SEDIMENT_ADULT_D
          TMP_ING_SEDIMENT_R = TMP_ING_SEDIMENT_CHILD_R + TMP_ING_SEDIMENT_ADULT_R
!
          TMP_ING_DIRT_D = TMP_ING_SOIL_D + TMP_ING_SEDIMENT_D
          TMP_ING_DIRT_R = TMP_ING_SOIL_R + TMP_ING_SEDIMENT_R
!
! ***     Ingestion - food
          TMP_ING_LEAFVEG_D = CLEAFVEG(IANA,IREL) * INGEST(IREL)%IRLEAFVEG * EXPOS(IREL)%ED * EXPOS(IREL)%EFLEAFVEG * &
                              DFING(IANA,IREL)
          TMP_ING_ROOTVEG_D = CROOTVEG(IANA,IREL) * INGEST(IREL)%IRROOTVEG * EXPOS(IREL)%ED * EXPOS(IREL)%EFROOTVEG * &
                              DFING(IANA,IREL)
          TMP_ING_MEAT_D    = CMEAT(IANA,IREL)    * INGEST(IREL)%IRMEAT    * EXPOS(IREL)%ED * EXPOS(IREL)%EFMEAT    * &
                              DFING(IANA,IREL)
          TMP_ING_MILK_D    = CMILK(IANA,IREL)    * INGEST(IREL)%IRMILK    * EXPOS(IREL)%ED * EXPOS(IREL)%EFMILK    * &
                              DFING(IANA,IREL)
          TMP_ING_BIRD_D    = CBIRD(IANA,IREL)    * INGEST(IREL)%IRBIRD    * EXPOS(IREL)%ED * EXPOS(IREL)%EFBIRD    * &
                              DFING(IANA,IREL)
          TMP_ING_FRUIT_D   = CFRUIT(IANA,IREL)   * INGEST(IREL)%IRFRUIT   * EXPOS(IREL)%ED * EXPOS(IREL)%EFFRUIT   * &
                              DFING(IANA,IREL)
          TMP_ING_GRAIN_D   = CGRAIN(IANA,IREL)   * INGEST(IREL)%IRGRAIN   * EXPOS(IREL)%ED * EXPOS(IREL)%EFGRAIN   * &
                              DFING(IANA,IREL)
          TMP_ING_EGGS_D    = CEGGS(IANA,IREL)    * INGEST(IREL)%IREGGS    * EXPOS(IREL)%ED * EXPOS(IREL)%EFEGGS    * &
                              DFING(IANA,IREL)
          TMP_ING_FISH_D    = CFISH(IANA,IREL)    * INGEST(IREL)%IRFISH    * EXPOS(IREL)%ED * EXPOS(IREL)%EFFISH    * &
                              DFING(IANA,IREL)
          TMP_ING_FISH_2_D  = CFISH_2(IANA,IREL)  * INGEST(IREL)%IRFISH_2  * EXPOS(IREL)%ED * EXPOS(IREL)%EFFISH_2  * &
                              DFING(IANA,IREL)
          TMP_ING_FISH_3_D  = CFISH_3(IANA,IREL)  * INGEST(IREL)%IRFISH_3  * EXPOS(IREL)%ED * EXPOS(IREL)%EFFISH_3  * &
                              DFING(IANA,IREL)
          TMP_ING_LEAFVEG_R = CLEAFVEG(IANA,IREL) * INGEST(IREL)%IRLEAFVEG * EXPOS(IREL)%ED * EXPOS(IREL)%EFLEAFVEG * &
                              RFING(IANA,IREL)
          TMP_ING_ROOTVEG_R = CROOTVEG(IANA,IREL) * INGEST(IREL)%IRROOTVEG * EXPOS(IREL)%ED * EXPOS(IREL)%EFROOTVEG * &
                              RFING(IANA,IREL)
          TMP_ING_MEAT_R    = CMEAT(IANA,IREL)    * INGEST(IREL)%IRMEAT    * EXPOS(IREL)%ED * EXPOS(IREL)%EFMEAT    * &
                              RFING(IANA,IREL)
          TMP_ING_MILK_R    = CMILK(IANA,IREL)    * INGEST(IREL)%IRMILK    * EXPOS(IREL)%ED * EXPOS(IREL)%EFMILK    * &
                              RFING(IANA,IREL)
          TMP_ING_BIRD_R    = CBIRD(IANA,IREL)    * INGEST(IREL)%IRBIRD    * EXPOS(IREL)%ED * EXPOS(IREL)%EFBIRD    * &
                              RFING(IANA,IREL)
          TMP_ING_FRUIT_R   = CFRUIT(IANA,IREL)   * INGEST(IREL)%IRFRUIT   * EXPOS(IREL)%ED * EXPOS(IREL)%EFFRUIT   * &
                              RFING(IANA,IREL)
          TMP_ING_GRAIN_R   = CGRAIN(IANA,IREL)   * INGEST(IREL)%IRGRAIN   * EXPOS(IREL)%ED * EXPOS(IREL)%EFGRAIN   * &
                              RFING(IANA,IREL)
          TMP_ING_EGGS_R    = CEGGS(IANA,IREL)    * INGEST(IREL)%IREGGS    * EXPOS(IREL)%ED * EXPOS(IREL)%EFEGGS    * &
                              RFING(IANA,IREL)
          TMP_ING_FISH_R    = CFISH(IANA,IREL)    * INGEST(IREL)%IRFISH    * EXPOS(IREL)%ED * EXPOS(IREL)%EFFISH    * &
                              RFING(IANA,IREL)
          TMP_ING_FISH_2_R  = CFISH_2(IANA,IREL)  * INGEST(IREL)%IRFISH_2  * EXPOS(IREL)%ED * EXPOS(IREL)%EFFISH_2  * &
                              RFING(IANA,IREL)
          TMP_ING_FISH_3_R  = CFISH_3(IANA,IREL)  * INGEST(IREL)%IRFISH_3  * EXPOS(IREL)%ED * EXPOS(IREL)%EFFISH_3  * &
                              RFING(IANA,IREL)
!
          TMP_ING_FOOD_D    = TMP_ING_LEAFVEG_D + TMP_ING_ROOTVEG_D + TMP_ING_MEAT_D + TMP_ING_MILK_D + TMP_ING_BIRD_D + &
                              TMP_ING_FRUIT_D + TMP_ING_GRAIN_D + TMP_ING_EGGS_D + TMP_ING_FISH_D + TMP_ING_FISH_2_D + &
                              TMP_ING_FISH_3_D
          TMP_ING_FOOD_R    = TMP_ING_LEAFVEG_R + TMP_ING_ROOTVEG_R + TMP_ING_MEAT_R + TMP_ING_MILK_R + TMP_ING_BIRD_R + &
                              TMP_ING_FRUIT_R + TMP_ING_GRAIN_R + TMP_ING_EGGS_R + TMP_ING_FISH_R + TMP_ING_FISH_2_R + &
                              TMP_ING_FISH_3_R
!
! ***     Ingestion - total pathway dose and risk
          ING_INC_DOS(IREL) = TMP_ING_WATER_D + TMP_ING_DIRT_D + TMP_ING_FOOD_D
          ING_INC_RSK(IREL) = TMP_ING_WATER_R + TMP_ING_DIRT_R + TMP_ING_FOOD_R
!
!         ----------------------------------- Inhalation -----------------------------------
!
! ***     Inhalation - soil particulates
          TMP_INH_SOIL_D = CSOIL(IANA,IREL) * ML(IREL) * EXPOS(IREL)%ETSOIL* EXPOS(IREL)%EFSOIL * &
                           EXPOS(IREL)%ED * INGEST(IREL)%IRATE * DFINH(IANA,IREL) / CF4
          TMP_INH_SOIL_R = CSOIL(IANA,IREL) * ML(IREL) * EXPOS(IREL)%ETSOIL* EXPOS(IREL)%EFSOIL * &
                           EXPOS(IREL)%ED * INGEST(IREL)%IRATE * RFINH(IANA,IREL) / CF4
!
! ***     Inhalation - seep water (outdoor)
          TMP_INH_SEEP_D = SEEP(IANA,IREL) * VF(IANA,IREL) * EXPOS(IREL)%ETSEEP * EXPOS(IREL)%EFSEEP * &
                           EXPOS(IREL)%ED * INGEST(IREL)%IRATE * DFINH(IANA,IREL) / CF4
          TMP_INH_SEEP_R = SEEP(IANA,IREL) * VF(IANA,IREL) * EXPOS(IREL)%ETSEEP * EXPOS(IREL)%EFSEEP * &
                           EXPOS(IREL)%ED * INGEST(IREL)%IRATE * RFINH(IANA,IREL) / CF4
!
! ***     Inhalation - surface water (outdoor)
          TMP_INH_SWAT_D = SWAT(IANA,IREL) * VF(IANA,IREL) * EXPOS(IREL)%ETSURFACE * EXPOS(IREL)%EFSURFACE * &
                           EXPOS(IREL)%ED * INGEST(IREL)%IRATE * DFINH(IANA,IREL) / CF4
          TMP_INH_SWAT_R = SWAT(IANA,IREL) * VF(IANA,IREL) * EXPOS(IREL)%ETSURFACE * EXPOS(IREL)%EFSURFACE * &
                           EXPOS(IREL)%ED * INGEST(IREL)%IRATE * RFINH(IANA,IREL) / CF4
!
! ***     Inhalation - showering
          TMP_INH_SHOWER_D = CSHOWER(IANA,IREL) * CFSHOWER(IREL) * EXPOS(IREL)%ETSHOWER * EXPOS(IREL)%EFSHOWER * &
                             EXPOS(IREL)%ED * INGEST(IREL)%IRATE * DFINH(IANA,IREL) / CF4
          TMP_INH_SHOWER_R = CSHOWER(IANA,IREL) * CFSHOWER(IREL) * EXPOS(IREL)%ETSHOWER * EXPOS(IREL)%EFSHOWER * &
                             EXPOS(IREL)%ED * INGEST(IREL)%IRATE * RFINH(IANA,IREL) / CF4
!
! ***     Inhalation - air
          TMP_INH_AIRC_D = AIRC(IANA,IREL) * EXPOS(IREL)%ETAIR * EXPOS(IREL)%EFAIR * &
                           EXPOS(IREL)%ED * INGEST(IREL)%IRATE * DFINH(IANA,IREL) / CF4
          TMP_INH_AIRC_R = AIRC(IANA,IREL) * EXPOS(IREL)%ETAIR * EXPOS(IREL)%EFAIR * &
                           EXPOS(IREL)%ED * INGEST(IREL)%IRATE * RFINH(IANA,IREL) / CF4
!
! ***     Inhalation - sweat lodge
          TMP_INH_SWEAT_D = CSWEAT(IANA,IREL) * CFSWEAT(IREL) * EXPOS(IREL)%ETSWEAT * EXPOS(IREL)%EFSWEAT * &
                            EXPOS(IREL)%ED * INGEST(IREL)%IRATE * DFINH(IANA,IREL) / CF4
          TMP_INH_SWEAT_R = CSWEAT(IANA,IREL) * CFSWEAT(IREL) * EXPOS(IREL)%ETSWEAT * EXPOS(IREL)%EFSWEAT * &
                            EXPOS(IREL)%ED * INGEST(IREL)%IRATE * RFINH(IANA,IREL) / CF4
!
! ***     Inhalation - total pathway dose and risk
          INH_INC_DOS(IREL) = TMP_INH_SOIL_D + TMP_INH_SEEP_D + TMP_INH_SWAT_D + TMP_INH_SHOWER_D + TMP_INH_AIRC_D + TMP_INH_SWEAT_D
          INH_INC_RSK(IREL) = TMP_INH_SOIL_R + TMP_INH_SEEP_R + TMP_INH_SWAT_R + TMP_INH_SHOWER_R + TMP_INH_AIRC_R + TMP_INH_SWEAT_R
!
!         ----------------------------------- External -----------------------------------
!
! ***     External - swimming
          TMP_EXT_SWIM_D = SWAT(IANA,IREL) * EXPOS(IREL)%ETSWIM * EXPOS(IREL)%EFSWIM * EXPOS(IREL)%ED * DFSWIM(IANA,IREL)
          TMP_EXT_SWIM_R = SWAT(IANA,IREL) * EXPOS(IREL)%ETSWIM * EXPOS(IREL)%EFSWIM * EXPOS(IREL)%ED * RFSWIM(IANA,IREL)
!
! ***     External - boating
          TMP_EXT_BOAT_D = SWAT(IANA,IREL) * EXPOS(IREL)%ETBOAT * EXPOS(IREL)%EFBOAT * EXPOS(IREL)%ED * DFBOAT(IANA,IREL)
          TMP_EXT_BOAT_R = SWAT(IANA,IREL) * EXPOS(IREL)%ETBOAT * EXPOS(IREL)%EFBOAT * EXPOS(IREL)%ED * RFBOAT(IANA,IREL)
!
! ***     External - soil
          TMP_EXT_SOIL_D = CSOIL(IANA,IREL) * SHIELDSOIL(IREL) * EXPOS(IREL)%ETSOIL* EXPOS(IREL)%EFSOIL * &
                           EXPOS(IREL)%ED * DFSOIL(IANA,IREL)
          TMP_EXT_SOIL_R = CSOIL(IANA,IREL) * SHIELDSOIL(IREL) * EXPOS(IREL)%ETSOIL* EXPOS(IREL)%EFSOIL * &
                           EXPOS(IREL)%ED * RFSOIL(IANA,IREL)
!
! ***     External - sediment
          TMP_EXT_SEDI_D = SEDI(IANA,IREL) * SHIELDSED(IREL) * EXPOS(IREL)%ETSED * EXPOS(IREL)%EFSED * &
                           EXPOS(IREL)%ED * DFSOIL(IANA,IREL)
          TMP_EXT_SEDI_R = SEDI(IANA,IREL) * SHIELDSED(IREL) * EXPOS(IREL)%ETSED * EXPOS(IREL)%EFSED * &
                           EXPOS(IREL)%ED * RFSOIL(IANA,IREL)
!
! ***     External - total pathway dose and risk
          EXT_INC_DOS(IREL) = TMP_EXT_SWIM_D + TMP_EXT_BOAT_D + TMP_EXT_SOIL_D + TMP_EXT_SEDI_D
          EXT_INC_RSK(IREL) = TMP_EXT_SWIM_R + TMP_EXT_BOAT_R + TMP_EXT_SOIL_R + TMP_EXT_SEDI_R
!
!         ----------------------------------- Sum of All Things -----------------------------------
!
! ***     Dose and risk increment for this analyte (sum inhalation, ingestion and external pathways)
          DOSEINC(IREL) = ING_INC_DOS(IREL) + INH_INC_DOS(IREL) + EXT_INC_DOS(IREL)
          RISKINC(IREL) = ING_INC_RSK(IREL) + INH_INC_RSK(IREL) + EXT_INC_RSK(IREL)
!
! ***     Increment the dose and risk for all radioactive analytes
          DOSERAD(IREL) = DOSERAD(IREL) + DOSEINC(IREL)
          RISKRAD(IREL) = RISKRAD(IREL) + RISKINC(IREL)
!
! ***     Population dose and population risk increments for this analyte for this location
          POPDOSE(IREL) = POPDOSE(IREL) + DOSEINC(IREL)*ESD_LOC(ILOC)%POP
          POPRISK(IREL) = POPRISK(IREL) + RISKINC(IREL)*ESD_LOC(ILOC)%POP
!
          IF( BGRAD .OR. ESD_ANA(IANA)%VERBOSE .OR. ESD_ANA(IANA)%TERSE ) THEN
            WRITE(IRPT,1000) 'Processing Location    : ' // ESD_LOC(ILOC)%ID // ' : "' // TRIM(ESD_LOC(ILOC)%NAME) // '"'
            WRITE(IRPT,1010) 'Processing Analyte     : ' // ESD_ANA(IANA)%ID // ' : "' // TRIM(ESD_ANA(IANA)%NAME) // '"'
            CTMP = 'Processing realization : '
            WRITE(CTMP(26:),'(I0)') IREL
            WRITE(IRPT,1010) TRIM(CTMP)
          END IF
!
          IF( BGRAD .OR. ESD_ANA(IANA)%VERBOSE ) THEN
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Details for the ingestion exposure pathway for (radioactive) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Pathway             Dose             Risk              Ingestion           '//&
              ' Concentration        Exposure Freq.      Food Definition'
            WRITE(IRPT,1010) '   ------------------- ---------------- ----------------- --------------------'//&
              ' -------------------- ------------------- -------------------------------------------'
            WRITE(IRPT,1040) 'Surface water     ', TMP_ING_SWAT_D,           'Rem', TMP_ING_SWAT_R,          'Risk', &
              INGEST(IREL)%IRSURFACE,   'L/day  ', SWAT(IANA,IREL),     'pCi/L  ', EXPOS(IREL)%EFSURFACE, 'day/yr  '//LABEL_SURFACE
            WRITE(IRPT,1040) 'Ground water      ', TMP_ING_GWAT_D,           'Rem', TMP_ING_GWAT_R,          'Risk', &
              INGEST(IREL)%IRGROUND,    'L/day  ', GWAT(IANA,IREL),     'pCi/L  ', EXPOS(IREL)%EFGROUND,  'day/yr  '//LABEL_GROUND
            WRITE(IRPT,1040) 'Seep water        ', TMP_ING_SEEP_D,           'Rem', TMP_ING_SEEP_R,          'Risk', &
              INGEST(IREL)%IRSEEP,      'L/day  ', SEEP(IANA,IREL),     'pCi/L  ', EXPOS(IREL)%EFSEEP,    'day/yr  '//LABEL_SEEP
            WRITE(IRPT,1040) 'Soil (child)      ', TMP_ING_SOIL_CHILD_D,     'Rem', TMP_ING_SOIL_CHILD_R,    'Risk',&
              INGEST(IREL)%IRSOILCHILD, 'kg/day ', CSOIL(IANA,IREL),    'pCi/kg ', EXPOS(IREL)%EFSOIL,    'day/yr  '//&
              LABEL_SOIL_CHILD
            WRITE(IRPT,1040) 'Soil (adult)      ', TMP_ING_SOIL_ADULT_D,     'Rem', TMP_ING_SOIL_ADULT_R,    'Risk',&
              INGEST(IREL)%IRSOILADULT, 'kg/day ', CSOIL(IANA,IREL),    'pCi/kg ', EXPOS(IREL)%EFSOIL,    'day/yr  '//&
              LABEL_SOIL_ADULT
            WRITE(IRPT,1040) 'Sediment (child)  ', TMP_ING_SEDIMENT_CHILD_D, 'Rem', TMP_ING_SEDIMENT_CHILD_R,'Risk',&
              INGEST(IREL)%IRSEDCHILD,  'kg/day ', SEDI(IANA,IREL),     'pCi/kg ', EXPOS(IREL)%EFSED,     'day/yr  '//&
              LABEL_SEDIMENT_CHILD
            WRITE(IRPT,1040) 'Sediment (adult)  ', TMP_ING_SEDIMENT_ADULT_D, 'Rem', TMP_ING_SEDIMENT_ADULT_R,'Risk',&
              INGEST(IREL)%IRSEDADULT,  'kg/day ', SEDI(IANA,IREL),     'pCi/kg ', EXPOS(IREL)%EFSED,     'day/yr  '//&
              LABEL_SEDIMENT_ADULT
            WRITE(IRPT,1040) 'Leafy Vegetables  ', TMP_ING_LEAFVEG_D,        'Rem', TMP_ING_LEAFVEG_R,       'Risk', &
              INGEST(IREL)%IRLEAFVEG,   'kg/day ', CLEAFVEG(IANA,IREL), 'pCi/kg ', EXPOS(IREL)%EFLEAFVEG, 'day/yr  '//LABEL_LEAFVEG
            WRITE(IRPT,1040) 'Root Vegetables   ', TMP_ING_ROOTVEG_D,        'Rem', TMP_ING_ROOTVEG_R,       'Risk', &
              INGEST(IREL)%IRROOTVEG,   'kg/day ', CROOTVEG(IANA,IREL), 'pCi/kg ', EXPOS(IREL)%EFROOTVEG, 'day/yr  '//LABEL_ROOTVEG
            WRITE(IRPT,1040) 'Meat              ', TMP_ING_MEAT_D,           'Rem', TMP_ING_MEAT_R,          'Risk', &
              INGEST(IREL)%IRMEAT,      'kg/day ', CMEAT(IANA,IREL),    'pCi/kg ', EXPOS(IREL)%EFMEAT,    'day/yr  '//LABEL_MEAT
            WRITE(IRPT,1040) 'Milk              ', TMP_ING_MILK_D,           'Rem', TMP_ING_MILK_R,          'Risk', &
              INGEST(IREL)%IRMILK,      'L/day  ', CMILK(IANA,IREL),    'pCi/L  ', EXPOS(IREL)%EFMILK,    'day/yr  '//LABEL_MILK
            WRITE(IRPT,1040) 'Bird              ', TMP_ING_BIRD_D,           'Rem', TMP_ING_BIRD_R,          'Risk', &
              INGEST(IREL)%IRBIRD,      'kg/day ', CBIRD(IANA,IREL),    'pCi/kg ', EXPOS(IREL)%EFBIRD,    'day/yr  '//LABEL_BIRD
            WRITE(IRPT,1040) 'Fruit             ', TMP_ING_FRUIT_D,          'Rem', TMP_ING_FRUIT_R,         'Risk', &
              INGEST(IREL)%IRFRUIT,     'kg/day ', CFRUIT(IANA,IREL),   'pCi/kg ', EXPOS(IREL)%EFFRUIT,   'day/yr  '//LABEL_FRUIT
            WRITE(IRPT,1040) 'Grain             ', TMP_ING_GRAIN_D,          'Rem', TMP_ING_GRAIN_R,         'Risk', &
              INGEST(IREL)%IRGRAIN,     'kg/day ', CGRAIN(IANA,IREL),   'pCi/kg ', EXPOS(IREL)%EFGRAIN,   'day/yr  '//LABEL_GRAIN
            WRITE(IRPT,1040) 'Eggs              ', TMP_ING_EGGS_D,           'Rem', TMP_ING_EGGS_R,          'Risk', &
              INGEST(IREL)%IREGGS,      'kg/day ', CEGGS(IANA,IREL),    'pCi/kg ', EXPOS(IREL)%EFEGGS,    'day/yr  '//LABEL_EGGS
            WRITE(IRPT,1040) 'Fish              ', TMP_ING_FISH_D,           'Rem', TMP_ING_FISH_R,          'Risk', &
              INGEST(IREL)%IRFISH,      'kg/day ', CFISH(IANA,IREL),    'pCi/kg ', EXPOS(IREL)%EFFISH,    'day/yr  '//LABEL_FISH
            WRITE(IRPT,1040) 'Fish_2            ', TMP_ING_FISH_2_D,         'Rem', TMP_ING_FISH_2_R,        'Risk', &
              INGEST(IREL)%IRFISH_2,    'kg/day ', CFISH_2(IANA,IREL),  'pCi/kg ', EXPOS(IREL)%EFFISH_2,  'day/yr  '//LABEL_FISH_2
            WRITE(IRPT,1040) 'Fish_3            ', TMP_ING_FISH_3_D,         'Rem', TMP_ING_FISH_3_R,        'Risk', &
              INGEST(IREL)%IRFISH_3,    'kg/day ', CFISH_3(IANA,IREL),  'pCi/kg ', EXPOS(IREL)%EFFISH_3,  'day/yr  '//LABEL_FISH_3
 1040       FORMAT(3X,A,1X,1P,E12.5,1X,A,8(1X,E12.5,1X,A))
!
            WRITE(IRPT,1010) ' '
            CTMP = 'Inhalation rate (m^3/day) is '
            WRITE(CTMP(30:),'(1P,E12.5)') INGEST(IREL)%IRATE
            WRITE(IRPT,1010) 'Details for the inhalation exposure pathway for (radioactive) '//TRIM(ESD_ANA(IANA)%ID)//&
              '   ['//TRIM(CTMP)//']'
            WRITE(IRPT,1010) '   Pathway                    Dose             Risk              Concentration      '//&
              ' Exposure Time       Exposure Frequency  Loading or Volatile'
            WRITE(IRPT,1010) '   -------------------------- ---------------- ----------------- -------------------'//&
              ' ------------------- ------------------- ------------------- '
            WRITE(IRPT,1051) 'Inhalation - soil         ', TMP_INH_SOIL_D,   'Rem', TMP_INH_SOIL_R,   'Risk', &
              CSOIL(IANA,IREL),   'pCi/kg', EXPOS(IREL)%ETSOIL,   'hr/day', EXPOS(IREL)%EFSOIL,   'day/yr', ML(IREL),       'kg/m^3'
            WRITE(IRPT,1051) 'Inhalation - seep water   ', TMP_INH_SEEP_D,   'Rem', TMP_INH_SEEP_R,   'Risk', &
              SEEP(IANA,IREL),    'pCi/L ', EXPOS(IREL)%ETSEEP,   'hr/day', EXPOS(IREL)%EFSEEP,   'day/yr', VF(IANA,IREL),  'L/m^3'
            WRITE(IRPT,1051) 'Inhalation - surface water', TMP_INH_SWAT_D,   'Rem', TMP_INH_SWAT_R,   'Risk', &
              SWAT(IANA,IREL),    'pCi/L ', EXPOS(IREL)%ETSURFACE, 'hr/day', EXPOS(IREL)%EFSURFACE, 'day/yr', VF(IANA,IREL), 'L/m^3'
            WRITE(IRPT,1051) 'Inhalation - showering    ', TMP_INH_SHOWER_D, 'Rem', TMP_INH_SHOWER_R, 'Risk', &
              CSHOWER(IANA,IREL), 'pCi/L ', EXPOS(IREL)%ETSHOWER, 'hr/day', EXPOS(IREL)%EFSHOWER, 'day/yr', CFSHOWER(IREL), 'L/m^3'
            WRITE(IRPT,1051) 'Inhalation - sweat lodge  ', TMP_INH_SWEAT_D,  'Rem', TMP_INH_SWEAT_R,  'Risk', &
              CSWEAT(IANA,IREL),  'pCi/L ', EXPOS(IREL)%ETSWEAT,  'hr/day', EXPOS(IREL)%EFSWEAT,  'day/yr', CFSWEAT(IREL),  'L/m^3'
            WRITE(IRPT,1051) 'Inhalation - air          ', TMP_INH_AIRC_D,   'Rem', TMP_INH_AIRC_R,   'Risk', &
              AIRC(IANA,IREL),    'pCi/L ', EXPOS(IREL)%ETAIR,    'hr/day', EXPOS(IREL)%EFAIR,    'day/yr'
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Details for the external exposure pathway for (radioactive) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Pathway  Dose             Risk              Concentration       Exposure Time      '//&
              ' Exposure Frequency  Shielding Factor  '
            WRITE(IRPT,1010) '   -------- ---------------- ----------------- ------------------- -------------------'//&
              ' ------------------- ---------------------'
            WRITE(IRPT,1051) 'Swimming', TMP_EXT_SWIM_D, 'Rem', TMP_EXT_SWIM_R, 'Risk', SWAT(IANA,IREL),  'pCi/L ', &
              EXPOS(IREL)%ETSWIM, 'hr/day', EXPOS(IREL)%EFSWIM, 'day/yr'
            WRITE(IRPT,1051) 'Boating ', TMP_EXT_BOAT_D, 'Rem', TMP_EXT_BOAT_R, 'Risk', SWAT(IANA,IREL),  'pCi/L ', &
              EXPOS(IREL)%ETBOAT, 'hr/day', EXPOS(IREL)%EFBOAT, 'day/yr'
            WRITE(IRPT,1051) 'Soil    ', TMP_EXT_SOIL_D, 'Rem', TMP_EXT_SOIL_R, 'Risk', CSOIL(IANA,IREL), 'pCi/kg',&
              EXPOS(IREL)%ETSOIL, 'hr/day', EXPOS(IREL)%EFSOIL, 'day/yr', SHIELDSOIL(IREL), 'unitless'
            WRITE(IRPT,1051) 'Sediment', TMP_EXT_SEDI_D, 'Rem', TMP_EXT_SEDI_R, 'Risk', SEDI(IANA,IREL),  'pCi/kg',&
              EXPOS(IREL)%ETSED,  'hr/day', EXPOS(IREL)%EFSOIL, 'day/yr', SHIELDSED(IREL),  'unitless'
 1051       FORMAT(3X,A,1X,1P,E12.5,1X,A, 6(1X,E12.5,1X,A))
!
          END IF
!
          IF( BGRAD .OR. ESD_ANA(IANA)%VERBOSE .OR. ESD_ANA(IANA)%TERSE ) THEN
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Summary of dose and risk increments for (radioactive) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '   Dose (rem)   Percent      Risk         Percent      Pathway'
            WRITE(IRPT,1010) '   ------------ ------------ ------------ ------------ ----------------------'
            WRITE(IRPT,1055) TMP_ING_FOOD_D,    PERCENT(TMP_ING_FOOD_D,DOSEINC(IREL)),    TMP_ING_FOOD_R,    &
              PERCENT(TMP_ING_FOOD_R,RISKINC(IREL)),    'Food ingestion'
            WRITE(IRPT,1055) TMP_ING_DIRT_D,    PERCENT(TMP_ING_DIRT_D,DOSEINC(IREL)),    TMP_ING_DIRT_R,    &
              PERCENT(TMP_ING_DIRT_R,RISKINC(IREL)),    'Soil/sediment ingestion'
            WRITE(IRPT,1055) TMP_ING_WATER_D,   PERCENT(TMP_ING_WATER_D,DOSEINC(IREL)),   TMP_ING_WATER_R,   &
              PERCENT(TMP_ING_WATER_R,RISKINC(IREL)),   'Water ingestion'
            WRITE(IRPT,1055) ING_INC_DOS(IREL), PERCENT(ING_INC_DOS(IREL),DOSEINC(IREL)), ING_INC_RSK(IREL), &
              PERCENT(ING_INC_RSK(IREL),RISKINC(IREL)), 'All ingestion'
            WRITE(IRPT,1055) INH_INC_DOS(IREL), PERCENT(INH_INC_DOS(IREL),DOSEINC(IREL)), INH_INC_RSK(IREL), &
              PERCENT(INH_INC_RSK(IREL),RISKINC(IREL)), 'Inhalation'
            WRITE(IRPT,1055) EXT_INC_DOS(IREL), PERCENT(EXT_INC_DOS(IREL),DOSEINC(IREL)), EXT_INC_RSK(IREL), &
              PERCENT(EXT_INC_RSK(IREL),RISKINC(IREL)), 'External'
            WRITE(IRPT,1056) DOSEINC(IREL), RISKINC(IREL), 'Total'
 1055       FORMAT(3X,1P,E12.5,1X,E12.5,1X,E12.5,1X,E12.5,1X,A)
 1056       FORMAT(3X,1P,E12.5,1X,12X,1X,E12.5,1X,12X,1X,A)
          END IF
!
          IF( BGRAD .OR. ESD_ANA(IANA)%VERBOSE ) THEN
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Exposure durations'
            WRITE(IRPT,1030) EXPOS(IREL)%ED,      'Duration (years)'
            WRITE(IRPT,1030) EXPOS(IREL)%EDCHILD, 'Duration - child (years)'
            WRITE(IRPT,1030) EXPOS(IREL)%EDADULT, 'Duration - adult (years)'
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Conversion factors for (radioactive) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1010) '                 Dose                           Risk'
            WRITE(IRPT,1010) '                 ------------------------------ -------------------------------'
            WRITE(IRPT,1058) 'Ingestion    ', DFING(IANA,IREL),  'rem/pCi          ', RFING(IANA,IREL),  'risk/pCi'
            WRITE(IRPT,1058) 'Inhalation   ', DFINH(IANA,IREL),  'rem/pCi          ', RFINH(IANA,IREL),  'risk/pCi'
            WRITE(IRPT,1058) 'Boating      ', DFBOAT(IANA,IREL), 'rem/hr per pCi/L ', RFBOAT(IANA,IREL), 'risk/hr per pCi/L'
            WRITE(IRPT,1058) 'Swimming     ', DFSWIM(IANA,IREL), 'rem/hr per pCi/L ', RFSWIM(IANA,IREL), 'risk/hr per pCi/L'
            WRITE(IRPT,1058) 'Soil exposure', DFSOIL(IANA,IREL), 'rem/hr per pCi/kg', RFSOIL(IANA,IREL), 'risk/hr per pCi/kg'
 1058       FORMAT(3X,A,1P,1X,E12.5,1X,A,1X,E12.5,1X,A)
!
          END IF
!
          IF( BGRAD ) THEN
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Miscellaneous values '
            WRITE(IRPT,1050) ESD_LOC(ILOC)%POP, 'Population at this location (number of people)'
 1050       FORMAT(3X,I0,' : ',A)
!
            WRITE(IRPT,1010) ' '
            WRITE(IRPT,1010) 'Miscellaneous dose and risk values for (radioactive) '//TRIM(ESD_ANA(IANA)%ID)
            WRITE(IRPT,1030) DOSERAD(IREL),   'Dose from all analytes at this location - combined to this point'
            WRITE(IRPT,1030) RISKRAD(IREL),   'Risk from all analytes at this location - combined to this point'
            WRITE(IRPT,1030) POPDOSE(IREL),   'Population dose - combined to this point - all locations (person-rem)'
            WRITE(IRPT,1030) POPRISK(IREL),   'Population risk - combined to this point - all locations (risk)'
!
          END IF
!
        END DO ! Realization loop
!
! ***   Compute and output summary statistics on the dose increments by pathway for this analyte
!
        IF( STA_PATHDOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
!
!         Ingestion
          CALL USTAT( ING_INC_DOS, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable ING_INC'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'DOSEING', 'Rem',&
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
!         Inhalation
          CALL USTAT( INH_INC_DOS, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable INH_INC'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'DOSEINH', 'Rem',&
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
!         External
          CALL USTAT( EXT_INC_DOS, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable EXT_INC'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'DOSEEXT', 'Rem',&
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
        END IF
!
! ***   Compute and output summary statistics on the risk increments by pathway for this analyte
!
        IF( STA_PATHRISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
!
!         Ingestion
          CALL USTAT( ING_INC_RSK, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable ING_INC -> CVEC'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'RISKING', 'Risk',&
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
!         Inhalation
          CALL USTAT( INH_INC_RSK, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable INH_INC -> CVEC'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'RISKINH', 'Risk',&
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
!         External
          CALL USTAT( EXT_INC_RSK, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable EXT_INC -> CVEC'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'RISKEXT', 'Risk',&
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!
        END IF
!
! ***   Compute and output summary statistics on the analyte-level dose
!
        IF( STA_ANADOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL USTAT( DOSEINC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable DOSEINC'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'ANADOSE', 'Rem',&
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
        END IF
!
! ***   Compute and output summary statistics on the analyte-level risk
!
        IF( STA_ANARISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL USTAT( RISKINC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error detected for variable RISKINC'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'ANARISK', 'Risk',&
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
        END IF
!
! ***   Output detailed values on the dose increments by pathway
!
        IF( DET_PATHDOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
!         Ingestion
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'DOSEING', 'Rem', ING_INC_DOS, NREAL )
!         Inhalation
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'DOSEINH', 'Rem', INH_INC_DOS, NREAL )
!         External
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'DOSEEXT', 'Rem', EXT_INC_DOS, NREAL )
!
        END IF
!
! ***   Output detailed values on the risk increments by pathway
!
        IF( DET_PATHRISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
!         Ingestion
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'RISKING', 'Risk', ING_INC_RSK, NREAL )
!         Inhalation
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'RISKINH', 'Risk', INH_INC_RSK, NREAL )
!         External
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'RISKEXT', 'Risk', EXT_INC_RSK, NREAL )
!
        END IF
!
! ***   Output detailed values on the dose and risk for this analyte
!
        IF( DET_ANADOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'ANADOSE', 'Rem', DOSEINC, NREAL )
        END IF
!
        IF( DET_ANARISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
          CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, ESD_ANA(IANA)%ID, 'RAD', 'ANARISK', 'Risk', RISKINC, NREAL )
        END IF
!
      END DO ! Analyte loop
!
!
! *** Compute and output summary statistics on total (sum over analytes) dose and risk at this location
!
      IF( STA_SUMDOSE .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
        CALL USTAT( DOSERAD, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error detected for variable RISKRAD'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'COMBIN', 'RAD', 'SUMDOSE', 'Rem', &
          XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
      END IF
!
      IF( STA_SUMRISK .AND. ESD_ANA(IANA)%OUTPUT .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
        CALL USTAT( RISKRAD, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error detected for variable RISKRAD'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'COMBIN', 'RAD', 'SUMDOSE', 'Risk', &
          XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
      END IF
!
! *** Output detailed values on total (sum over analytes) dose and risk at this location
!
      IF( DET_SUMDOSE .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'COMBIN', 'RAD', 'SUMDOSE', 'Rem', DOSERAD, NREAL )
      END IF
!
      IF( DET_SUMRISK .AND. ESD_LOC(ILOC)%OUTPUT ) THEN
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'COMBIN', 'RAD', 'SUMRISK', 'Risk', RISKRAD, NREAL )
      END IF
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
!---- Executable code -------------------------------------------------------------
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

      SUBROUTINE REPORT_UNITS( )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine handles writing an echo of the units conversion
!!    factors to the report file.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 May 2000 : Version 1.0
!!    Paul W. Eslinger : 21 Feb 2003 : Version 1.0
!!
!!**********************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Esd_Mod
      USE ECDA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: IANA ! Analyte looping index
!
!---- Executable code --------------------------------------------
!
! *** Analyte names and types
!
      WRITE(IRPT,1000)
 1000 FORMAT(/'Concentration data units conversion factors')
      DO IANA = 1, ESD_NUM_ANA
        IF( ESD_ANA(IANA)%COMP ) THEN
          WRITE(IRPT,*) ' '
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Ground water ', CONVERT_UNITS(IANA,IGWAT), &
            CONVERT_LABELS(IANA,IGWAT,1), CONVERT_LABELS(IANA,IGWAT,2)
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Surface water', CONVERT_UNITS(IANA,ISWAT), &
            CONVERT_LABELS(IANA,ISWAT,1), CONVERT_LABELS(IANA,ISWAT,2)
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Seep water   ', CONVERT_UNITS(IANA,ISEEP), &
            CONVERT_LABELS(IANA,ISEEP,1), CONVERT_LABELS(IANA,ISEEP,2)
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Sediment     ', CONVERT_UNITS(IANA,ISEDI), &
            CONVERT_LABELS(IANA,ISEDI,1), CONVERT_LABELS(IANA,ISEDI,2)
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Pore water   ', CONVERT_UNITS(IANA,IPWAT), &
            CONVERT_LABELS(IANA,IPWAT,1), CONVERT_LABELS(IANA,IPWAT,2)
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Soil-Riparian', CONVERT_UNITS(IANA,ISORP), &
            CONVERT_LABELS(IANA,ISORP,1), CONVERT_LABELS(IANA,ISORP,2)
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Soil-Dry Land', CONVERT_UNITS(IANA,ISODR), &
            CONVERT_LABELS(IANA,ISODR,1), CONVERT_LABELS(IANA,ISODR,2)
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Soil-GW Irrig', CONVERT_UNITS(IANA,ISOGW), &
            CONVERT_LABELS(IANA,ISOGW,1), CONVERT_LABELS(IANA,ISOGW,2)
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Soil-SW Irrig', CONVERT_UNITS(IANA,ISOSW), &
            CONVERT_LABELS(IANA,ISOSW,1), CONVERT_LABELS(IANA,ISOSW,2)
          WRITE(IRPT,1010) IANA, ESD_ANA(IANA)%ID, ESD_ANA(IANA)%TYPE, 'Air Concen.  ', CONVERT_UNITS(IANA,IAIRC), &
            CONVERT_LABELS(IANA,IAIRC,1), CONVERT_LABELS(IANA,IAIRC,2)
 1010     FORMAT(3X,I2,' : ',A,' : ',A,' : ',A,' : ',1P,E12.5,' : From ',A,' to ',A)
        END IF
      END DO
!
      RETURN
      END SUBROUTINE

      SUBROUTINE SET_UNITS( IANA, IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine sets the units conversion factor for all of the
!!    media for one analyte.
!!
!!    This subroutine needs to be immediately preceeded by a call to
!!    OPEN_ECDA so the header variables are correctly set in the
!!    ECDA module.
!!
!!  History:
!!
!!    Paul W. Eslinger : 11 May 2000 : Version 1.0
!!    Paul W. Eslinger : 21 Feb 2003 : SAC Rev. 1
!!
!!**********************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Control_Mod
      USE Ecda_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: IANA ! Looping variable for analytes
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=9) :: CALLER = 'SET_UNITS'
!
!---- Executable code -------------------------------------------------------------
!
! *** Check on a valid analyte index
!
      IF( IANA.LT.1 .OR. IANA.GT.ESD_NUM_ANA ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid analyte index'
        MESSAG(2) = 'Valid values are 1 to '
        WRITE(MESSAG(2)(23:),'(I4)') ESD_NUM_ANA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Set conversion factor based on type of analyte
!
      SELECT CASE ( ESD_ANA(IANA)%TYPE(2:2) )
!
        CASE( 'R' )
!
!         Radioactive - Groundwater
          IF( ECDA_UNITS(IGWAT) == 'Ci/m^3' ) THEN
            CONVERT_UNITS(IANA,IGWAT) = 1.0E9
            CONVERT_LABELS(IANA,IGWAT,1) = TRIM(ECDA_UNITS(IGWAT))
            CONVERT_LABELS(IANA,IGWAT,2) = 'pCi/L'
          ELSE
            IERR = 2
            MESSAG(1) = 'Unexpected units for ground water in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(IGWAT))
            MESSAG(3) = 'Units expected were ' // 'Ci/m^3'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Radioactive - Seep water
          IF( ECDA_UNITS(ISEEP) == 'Ci/m^3' ) THEN
            CONVERT_UNITS(IANA,ISEEP) = 1.0E9
            CONVERT_LABELS(IANA,ISEEP,1) = TRIM(ECDA_UNITS(ISEEP))
            CONVERT_LABELS(IANA,ISEEP,2) = 'pCi/L'
          ELSE
            IERR = 3
            MESSAG(1) = 'Unexpected units for seep water in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISEEP))
            MESSAG(3) = 'Units expected were ' // 'Ci/m^3'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Radioactive - surface water
          IF( ECDA_UNITS(ISWAT) == 'Ci/m^3' ) THEN
            CONVERT_UNITS(IANA,ISWAT) = 1.0E9
            CONVERT_LABELS(IANA,ISWAT,1) = TRIM(ECDA_UNITS(ISWAT))
            CONVERT_LABELS(IANA,ISWAT,2) = 'pCi/L'
          ELSE
            IERR = 4
            MESSAG(1) = 'Unexpected units for surface water in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISWAT))
            MESSAG(3) = 'Units expected were ' // 'Ci/m^3'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Radioactive - pore water
          IF( ECDA_UNITS(IPWAT) == 'Ci/m^3' ) THEN
            CONVERT_UNITS(IANA,IPWAT) = 1.0E9
            CONVERT_LABELS(IANA,IPWAT,1) = TRIM(ECDA_UNITS(IPWAT))
            CONVERT_LABELS(IANA,IPWAT,2) = 'pCi/L'
          ELSE
            IERR = 5
            MESSAG(1) = 'Unexpected units for pore water in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(IPWAT))
            MESSAG(3) = 'Units expected were ' // 'Ci/m^3'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Radioactive - sediment
          IF( ECDA_UNITS(ISEDI) == 'Ci/kg'  ) THEN
            CONVERT_UNITS(IANA,ISEDI) = 1.0E12
            CONVERT_LABELS(IANA,ISEDI,1) = TRIM(ECDA_UNITS(ISEDI))
            CONVERT_LABELS(IANA,ISEDI,2) = 'pCi/kg'
          ELSE
            IERR = 6
            MESSAG(1) = 'Unexpected units for sediment in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISEDI))
            MESSAG(3) = 'Units expected were ' // 'Ci/kg'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Radioactive - riparian soil
          IF( ECDA_UNITS(ISORP) == 'Ci/kg'  ) THEN
            CONVERT_UNITS(IANA,ISORP) = 1.0E12
            CONVERT_LABELS(IANA,ISORP,1) = TRIM(ECDA_UNITS(ISORP))
            CONVERT_LABELS(IANA,ISORP,2) = 'pCi/kg'
          ELSE
            IERR = 7
            MESSAG(1) = 'Unexpected units for riparian soil in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISORP))
            MESSAG(3) = 'Units expected were ' // 'Ci/kg'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Radioactive - dry soil
          IF( ECDA_UNITS(ISODR) == 'Ci/kg'  ) THEN
            CONVERT_UNITS(IANA,ISODR) = 1.0E12
            CONVERT_LABELS(IANA,ISODR,1) = TRIM(ECDA_UNITS(ISODR))
            CONVERT_LABELS(IANA,ISODR,2) = 'pCi/kg'
          ELSE
            IERR = 8
            MESSAG(1) = 'Unexpected units for dry upland soil in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISODR))
            MESSAG(3) = 'Units expected were ' // 'Ci/kg'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Radioactive - groundwater irrigated soil
          IF( ECDA_UNITS(ISOGW) == 'Ci/kg'  ) THEN
            CONVERT_UNITS(IANA,ISOGW) = 1.0E12
            CONVERT_LABELS(IANA,ISOGW,1) = TRIM(ECDA_UNITS(ISOGW))
            CONVERT_LABELS(IANA,ISOGW,2) = 'pCi/kg'
          ELSE
            IERR = 9
            MESSAG(1) = 'Unexpected units for groundwater irrigated soil in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISOGW))
            MESSAG(3) = 'Units expected were ' // 'Ci/kg'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Radioactive - surface water irrigated soil
          IF( ECDA_UNITS(ISOSW) == 'Ci/kg'  ) THEN
            CONVERT_UNITS(IANA,ISOSW) = 1.0E12
            CONVERT_LABELS(IANA,ISOSW,1) = TRIM(ECDA_UNITS(ISOSW))
            CONVERT_LABELS(IANA,ISOSW,2) = 'pCi/kg'
          ELSE
            IERR = 10
            MESSAG(1) = 'Unexpected units for surface water irrigated soil in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISOSW))
            MESSAG(3) = 'Units expected were ' // 'Ci/kg'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Radioactive - air concentrations
          IF( ECDA_UNITS(IAIRC) == 'Ci/m^3'  ) THEN
            CONVERT_UNITS(IANA,IAIRC) = 1.0E12
            CONVERT_LABELS(IANA,IAIRC,1) = TRIM(ECDA_UNITS(IAIRC))
            CONVERT_LABELS(IANA,IAIRC,2) = 'pCi/m^3'
          ELSE
            IERR = 11
            MESSAG(1) = 'Unexpected units for air concentrations in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(IAIRC))
            MESSAG(3) = 'Units expected were ' // 'Ci/m^3'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
        CASE( 'S' )
!
!         Stable - groundwater
          IF( ECDA_UNITS(IGWAT) == 'kg/m^3' ) THEN
            CONVERT_UNITS(IANA,IGWAT) = 1.0E6
            CONVERT_LABELS(IANA,IGWAT,1) = TRIM(ECDA_UNITS(IGWAT))
            CONVERT_LABELS(IANA,IGWAT,2) = 'ug/L'
          ELSE
            IERR = 13
            MESSAG(1) = 'Unexpected units for groundwater in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(IGWAT))
            MESSAG(3) = 'Units expected were ' // 'kg/m^3'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Stable - seep
          IF( ECDA_UNITS(ISEEP) == 'kg/m^3' ) THEN
            CONVERT_UNITS(IANA,ISEEP) = 1.0E6
            CONVERT_LABELS(IANA,ISEEP,1) = TRIM(ECDA_UNITS(ISEEP))
            CONVERT_LABELS(IANA,ISEEP,2) = 'ug/L'
          ELSE
            IERR = 14
            MESSAG(1) = 'Unexpected units for seep water in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISEEP))
            MESSAG(3) = 'Units expected were ' // 'kg/m^3'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Stable - surface water
          IF( ECDA_UNITS(ISWAT) == 'kg/m^3' ) THEN
            CONVERT_UNITS(IANA,ISWAT) = 1.0E6
            CONVERT_LABELS(IANA,ISWAT,1) = TRIM(ECDA_UNITS(ISWAT))
            CONVERT_LABELS(IANA,ISWAT,2) = 'ug/L'
          ELSE
            IERR = 15
            MESSAG(1) = 'Unexpected units for surface water in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISWAT))
            MESSAG(3) = 'Units expected were ' // 'kg/m^3'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Stable - pore water
          IF( ECDA_UNITS(IPWAT) == 'kg/m^3' ) THEN
            CONVERT_UNITS(IANA,IPWAT) = 1.0E6
            CONVERT_LABELS(IANA,IPWAT,1) = TRIM(ECDA_UNITS(IPWAT))
            CONVERT_LABELS(IANA,IPWAT,2) = 'ug/L'
          ELSE
            IERR = 16
            MESSAG(1) = 'Unexpected units for pore water in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(IPWAT))
            MESSAG(3) = 'Units expected were ' // 'kg/m^3'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Stable - sediment
          IF( ECDA_UNITS(ISEDI) == 'kg/kg'  ) THEN
            CONVERT_UNITS(IANA,ISEDI) = 1.0E9
            CONVERT_LABELS(IANA,ISEDI,1) = TRIM(ECDA_UNITS(ISEDI))
            CONVERT_LABELS(IANA,ISEDI,2) = 'ug/kg'
          ELSE
            IERR = 17
            MESSAG(1) = 'Unexpected units for sediment in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISEDI))
            MESSAG(3) = 'Units expected were ' // 'kg/kg'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Stable - riparian soil
          IF( ECDA_UNITS(ISORP) == 'kg/kg'  ) THEN
            CONVERT_UNITS(IANA,ISORP) = 1.0E9
            CONVERT_LABELS(IANA,ISORP,1) = TRIM(ECDA_UNITS(ISORP))
            CONVERT_LABELS(IANA,ISORP,2) = 'ug/kg'
          ELSE
            IERR = 18
            MESSAG(1) = 'Unexpected units for soil in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISORP))
            MESSAG(3) = 'Units expected were ' // 'kg/kg'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Stable - dry soil
          IF( ECDA_UNITS(ISODR) == 'kg/kg'  ) THEN
            CONVERT_UNITS(IANA,ISODR) = 1.0E9
            CONVERT_LABELS(IANA,ISODR,1) = TRIM(ECDA_UNITS(ISODR))
            CONVERT_LABELS(IANA,ISODR,2) = 'ug/kg'
          ELSE
            IERR = 19
            MESSAG(1) = 'Unexpected units for dry upland soil in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISODR))
            MESSAG(3) = 'Units expected were ' // 'kg/kg'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Stable - groundwater irrigated soil
          IF( ECDA_UNITS(ISOGW) == 'kg/kg'  ) THEN
            CONVERT_UNITS(IANA,ISOGW) = 1.0E9
            CONVERT_LABELS(IANA,ISOGW,1) = TRIM(ECDA_UNITS(ISOGW))
            CONVERT_LABELS(IANA,ISOGW,2) = 'ug/kg'
          ELSE
            IERR = 20
            MESSAG(1) = 'Unexpected units for groundwater irrigated soil in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISOGW))
            MESSAG(3) = 'Units expected were ' // 'kg/kg'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Stable - surface water irrigated soil
          IF( ECDA_UNITS(ISOSW) == 'kg/kg'  ) THEN
            CONVERT_UNITS(IANA,ISOSW) = 1.0E9
            CONVERT_LABELS(IANA,ISOSW,1) = TRIM(ECDA_UNITS(ISOSW))
            CONVERT_LABELS(IANA,ISOSW,2) = 'ug/kg'
          ELSE
            IERR = 21
            MESSAG(1) = 'Unexpected units for surface water irrigated soil in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(ISOSW))
            MESSAG(3) = 'Units expected were ' // 'kg/kg'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
!         Stable - air concentration
          IF( ECDA_UNITS(IAIRC) == 'kg/m^3'  ) THEN
            CONVERT_UNITS(IANA,IAIRC) = 1.0E9
            CONVERT_LABELS(IANA,IAIRC,1) = TRIM(ECDA_UNITS(IAIRC))
            CONVERT_LABELS(IANA,IAIRC,2) = 'ug/m^3'
          ELSE
            IERR = 22
            MESSAG(1) = 'Unexpected units for air concentrations in the concentration file'
            MESSAG(2) = 'Units in the file are ' // TRIM(ECDA_UNITS(IAIRC))
            MESSAG(3) = 'Units expected were ' // 'kg/m^3'
            CALL PRTERR( IERR, CALLER, 3 )
            RETURN
          END IF
!
        CASE DEFAULT
          IERR = 24
          MESSAG(1) = 'Invalid analyte type (type was not R or S in position 2)'
          MESSAG(2) = 'Analyte in question is '//ESD_ANA(IANA)%ID
          CALL PRTERR( IERR, CALLER, 2 )
          RETURN
!
      END SELECT
!
      RETURN
      END SUBROUTINE

      SUBROUTINE STOGEN( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine schedules generation of all realizations
!!    for all stochastic variables.
!!
!!  History:
!!
!!    Paul W. Eslinger : 20 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 31 Mar 2000 : Version 2.0
!!    Paul W. Eslinger : 22 Oct 2003 : Remove call to STOGENBT
!!    Paul W. Eslinger :  1 Apr 2005 : Add more screen outputs
!!    Paul W. Eslinger : 26 Sep 2005 : Rearrange error messages
!!
!!  Call List Variables:
!!
!!    IERR   : Output integer error flag
!!             0 = No errors
!!            >0 = Terminal error in lower level routine or
!!                Terminal error setting data
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Files_Mod
      USE Debug_Mod
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'STOGEN'
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Dose factors for radioactive analytes
!     Slope factors for carcinogenic analytes
!     Reference doses for hazardous analytes
!
      IF( BGVERB ) CALL TELLTIME( 'Generating dose factors, slope factors, reference doses', 'SCREEN', .FALSE., IRPT )
      CALL STOGENDF( IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Exposure frequencies and times
!
      IF( BGVERB ) CALL TELLTIME( 'Generating exposure frequencies and times', 'SCREEN', .FALSE., IRPT )
      CALL STOGENEF( IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Individual definitions
!     Ingestion rates
!
      IF( BGVERB ) CALL TELLTIME( 'Generating individual definitionas and ingestion rates', 'SCREEN', .FALSE., IRPT )
      CALL STOGENIN( IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Soil, water and analyte volatilization
!
      IF( BGVERB ) CALL TELLTIME( 'Generating soil, water and analyte volatilization', 'SCREEN', .FALSE., IRPT )
      CALL STOGENSW( IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Coefficients and factors for carcinogenic and hazardous analytes
!
      CALL STOGENKS( IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE

      SUBROUTINE STOGENDF( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates all realizations of the dose or risk factors for all analytes.
!!
!!  History:
!!
!!    Paul W. Eslinger : 26 Sep 1996 : Version 1.0
!!    Paul W. Eslinger : 31 Mar 2000 : Version 2.0
!!    Paul W. Eslinger : 24 Jan 2005 : Change HQING and HQINH to RFDING and RFDINH
!!    Paul W. Eslinger : 28 Feb 2006 : Make RFDING, RFDINH, SFING and SFINH optional inputs
!!                                     Add direct radionuclide risk factors
!!    Paul W. Eslinger :  5 Jun 2007 : Change stochastic variable to length 24
!!
!!  Call List Variables:
!!
!!    IERR   : Output integer error flag
!!             0 = No errors
!!            >0 = Terminal error in lower level routine or
!!                Terminal error setting data
!!
!!**************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Files_Mod
      USE Debug_Mod
      USE Seeds_Mod
      USE Control_Mod
      USE Stocvars_Mod
      USE Errors_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error indicator
!
! *** Local variables
!
      CHARACTER(LEN=8) :: CALLER = 'STOGENDF'
      INTEGER :: VIDX ! Variable index
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: IREL ! Realization looping index
      CHARACTER(LEN=24) :: CTMP ! Temporary variable label
!
!---- Executable code -------------------------------------------------------------
!
      IERR   = 0
      MESSAG(1) = 'Error in lower level routine'
!
! *** The following quantities depend on which analyte is selected
!
      DO IANA = 1, ESD_NUM_ANA
!
        IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
!
!-----------------------------------------------------------------------
!      Generate Dose and Risk Factors for Radioactive Analytes
!-----------------------------------------------------------------------
!
        IF( ESD_ANA(IANA)%ANATYP_RAD ) THEN
!
! ***     Dose Factor - External - Soil
!
          CTMP = ESD_ANA(IANA)%ID//'DFSOIL'
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            DFSOIL(IANA,IREL) = WORK(IREL)
          END DO
!
! ***     Dose Factor - External - Swimming
!
          CTMP = ESD_ANA(IANA)%ID//'DFSWIM'
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            DFSWIM(IANA,IREL) = WORK(IREL)
          END DO
!
! ***     Dose Factor - External - Boating
!
          CTMP = ESD_ANA(IANA)%ID//'DFBOAT'
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            DFBOAT(IANA,IREL) = WORK(IREL)
          END DO
!
! ***     Dose Factor - Ingestion
!
          CTMP = ESD_ANA(IANA)%ID//'DFING'
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            DFING(IANA,IREL) = WORK(IREL)
          END DO
!
! ***     Dose Factor - Inhalation
!
          CTMP = ESD_ANA(IANA)%ID//'DFINH'
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            DFINH(IANA,IREL) = WORK(IREL)
          END DO
!
! ***     Risk Factor - External - Soil
!
          CTMP = ESD_ANA(IANA)%ID//'RFSOIL'
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            RFSOIL(IANA,IREL) = WORK(IREL)
          END DO
!
! ***     Risk Factor - External - Swimming
!
          CTMP = ESD_ANA(IANA)%ID//'RFSWIM'
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            RFSWIM(IANA,IREL) = WORK(IREL)
          END DO
!
! ***     Risk Factor - External - Boating
!
          CTMP = ESD_ANA(IANA)%ID//'RFBOAT'
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            RFBOAT(IANA,IREL) = WORK(IREL)
          END DO
!
! ***     Risk Factor - Ingestion
!
          CTMP = ESD_ANA(IANA)%ID//'RFING'
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            RFING(IANA,IREL) = WORK(IREL)
          END DO
!
! ***     Risk Factor - Inhalation
!
          CTMP = ESD_ANA(IANA)%ID//'RFINH'
          VIDX = 0
          CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
          IF( IERR .NE. 0 ) THEN
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
          DO IREL = 1, NREAL
            RFINH(IANA,IREL) = WORK(IREL)
          END DO
!
        END IF
!
!-----------------------------------------------------------------------
!             Generate Slope Factors for Carcinogenic Analytes
!-----------------------------------------------------------------------
!
        IF( ESD_ANA(IANA)%ANATYP_CAR ) THEN
!
! ***     Ingestion slope factor
!
          IF( ESD_ANA(IANA)%SFING ) THEN
            CTMP = ESD_ANA(IANA)%ID//'SFING'
            VIDX = 0
            CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              SFING(IANA,IREL) = WORK(IREL)
            END DO
          END IF
!
! ***     Inhalation slope factor
!
          IF( ESD_ANA(IANA)%SFINH ) THEN
            CTMP = ESD_ANA(IANA)%ID//'SFINH'
            VIDX = 0
            CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              SFINH(IANA,IREL) = WORK(IREL)
            END DO
          END IF
!
        END IF
!
!-----------------------------------------------------------------------
!            Generate Reference Doses for Hazardous Analytes
!-----------------------------------------------------------------------
!
        IF( ESD_ANA(IANA)%ANATYP_HAZ ) THEN
!
! ***     Ingestion reference dose
!
          IF( ESD_ANA(IANA)%RFDING ) THEN
            CTMP = ESD_ANA(IANA)%ID//'RFDING'
            VIDX = 0
            CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RFDING(IANA,IREL) = WORK(IREL)
            END DO
          END IF
!
! ***     Inhalation reference dose
!
          IF( ESD_ANA(IANA)%RFDINH ) THEN
            CTMP = ESD_ANA(IANA)%ID//'RFDINH'
            VIDX = 0
            CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
            IF( IERR .NE. 0 ) THEN
              CALL PRTERR( IERR, CALLER, 1 )
              RETURN
            END IF
            DO IREL = 1, NREAL
              RFDINH(IANA,IREL) = WORK(IREL)
            END DO
          END IF
!
        END IF
!
      END DO
!
      RETURN
      END SUBROUTINE

      SUBROUTINE STOGENEF( IERR )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates all realizations of all exposure
!!    frequency variables.
!!
!!  History:
!!
!!    Paul W. Eslinger : 20 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 31 Mar 2000 : Version 2.0
!!    Paul W. Eslinger : 25 Sep 2000 : Version 2.00.B : Add animal intake
!!    Paul W. Eslinger : 25 Feb 2003 : SAC Rev. 1
!!    Paul W. Eslinger : 23 Sep 2004 : Add EFFRUIT and EFEGGS
!!    Paul W. Eslinger :  2 Mar 2005 : Add ETGROUND
!!    Paul W. Eslinger : 31 Mar 2005 : Update PRTSTAT
!!    Paul W. Eslinger :  1 Apr 2005 : Change to using a work vector
!!    Paul W. Eslinger : 13 May 2005 : Clean up work vector
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!    Paul W. Eslinger : 26 Sep 2005 : Add additional error checks
!!    Paul W. Eslinger : 28 Feb 2006 : Add showering logic, remove ETGROUND
!!
!!  Call List Variables:
!!
!!    IERR   : Output integer error flag
!!             0 = No errors
!!            >0 = Terminal error in lower level routine or
!!                Terminal error setting data
!!
!!**********************************************************************
!
! *** Global parameters, variables, and arrays
      USE Files_Mod
      USE Debug_Mod
      USE Seeds_Mod
      USE Control_Mod
      USE Stocvars_Mod
      USE Errors_Mod
      USE Stats_Mod, ONLY: BG_STOC_VALU, BG_STOC_DEFN, BG_STOC_STAT, RWORK
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER ! Name of this subroutine
      LOGICAL :: BAD  ! Found at least one bad value
      INTEGER :: VIDX ! Variable index
      INTEGER :: IREL ! Realization looping index
!
      REAL :: XMIN ! Statistics - minimum
      REAL :: XV01 ! Statistics -  1st percentile
      REAL :: XV05 ! Statistics -  5th percentile
      REAL :: XV10 ! Statistics - 10th percentile
      REAL :: XV25 ! Statistics - 25th percentile
      REAL :: XMED ! Statistics - median
      REAL :: XV75 ! Statistics - 75th percentile
      REAL :: XV90 ! Statistics - 90th percentile
      REAL :: XV95 ! Statistics - 95th percentile
      REAL :: XV99 ! Statistics - 99th percentile
      REAL :: XMAX ! Statistics - maximum
      REAL :: XAVG ! Statistics - average
      REAL :: XSTD ! Statistics - standard deviation
!
!---- Executable code -------------------------------------------------------------
!
      CALLER = 'STOGENEF'
      IERR   = 0
      BAD = .FALSE.
!
! *** Exposure frequency - Boating
!
      VIDX = 0
      CALL STONE( 'EFBOAT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFBOAT = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 1
            MESSAG(1) = 'Invalid data for EFBOAT'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure time - Boating
!
      VIDX = 0
      CALL STONE( 'ETBOAT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%ETBOAT = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.24.0 ) THEN
            BAD = .TRUE.
            IERR = 2
            MESSAG(1) = 'Invalid data for ETBOAT'
            MESSAG(2) = 'Valid range is 0 to 24'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - surface water
!
      VIDX = 0
      CALL STONE( 'EFSURFACE', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFSURFACE = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 3
            MESSAG(1) = 'Invalid data for EFSURFACE'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure time - surface water
!
      VIDX = 0
      CALL STONE( 'ETSURFACE', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%ETSURFACE = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.24.0 ) THEN
            BAD = .TRUE.
            IERR = 4
            MESSAG(1) = 'Invalid data for ETSURFACE'
            MESSAG(2) = 'Valid range is 0 to 24'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - air submersion
!
      VIDX = 0
      CALL STONE( 'EFAIR', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFAIR = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 5
            MESSAG(1) = 'Invalid data for EFAIR'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure time - air submersion
!
      VIDX = 0
      CALL STONE( 'ETAIR', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%ETAIR = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.24.0 ) THEN
            BAD = .TRUE.
            IERR = 6
            MESSAG(1) = 'Invalid data for ETAIR'
            MESSAG(2) = 'Valid range is 0 to 24'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - Sediments
!
      VIDX = 0
      CALL STONE( 'EFSED', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFSED = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 7
            MESSAG(1) = 'Invalid data for EFSED'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure time - Sediments
!
      VIDX = 0
      CALL STONE( 'ETSED', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%ETSED = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.24.0 ) THEN
            BAD = .TRUE.
            IERR = 8
            MESSAG(1) = 'Invalid data for ETSED'
            MESSAG(2) = 'Valid range is 0 to 24'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - Seep water
!
      VIDX = 0
      CALL STONE( 'EFSEEP', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFSEEP = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 9
            MESSAG(1) = 'Invalid data for EFSEEP'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure time - Seep water
!
      VIDX = 0
      CALL STONE( 'ETSEEP', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%ETSEEP = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.24.0 ) THEN
            BAD = .TRUE.
            IERR = 10
            MESSAG(1) = 'Invalid data for ETSEEP'
            MESSAG(2) = 'Valid range is 0 to 24'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - Soil
!
      VIDX = 0
      CALL STONE( 'EFSOIL', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFSOIL = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 11
            MESSAG(1) = 'Invalid data for EFSOIL'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure time - Soil
!
      VIDX = 0
      CALL STONE( 'ETSOIL', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%ETSOIL = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.24.0 ) THEN
            BAD = .TRUE.
            IERR = 12
            MESSAG(1) = 'Invalid data for ETSOIL'
            MESSAG(2) = 'Valid range is 0 to 24'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - Ground water
!
      VIDX = 0
      CALL STONE( 'EFGROUND', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFGROUND = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 13
            MESSAG(1) = 'Invalid data for EFGROUND'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - Swimming
!
      VIDX = 0
      CALL STONE( 'EFSWIM', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFSWIM = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 15
            MESSAG(1) = 'Invalid data for EFSWIM'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure time - Swimming
!
      VIDX = 0
      CALL STONE( 'ETSWIM', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%ETSWIM = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.24.0 ) THEN
            BAD = .TRUE.
            IERR = 16
            MESSAG(1) = 'Invalid data for ETSWIM'
            MESSAG(2) = 'Valid range is 0 to 24'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure duration - Adult
!
      VIDX = 0
      CALL STONE( 'EDADULT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EDADULT = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.125.0 ) THEN
            BAD = .TRUE.
            IERR = 17
            MESSAG(1) = 'Invalid data for EDADULT'
            MESSAG(2) = 'Valid range is 0 to 125'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure duration - Child
!
      VIDX = 0
      CALL STONE( 'EDCHILD', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EDCHILD = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.21.0 ) THEN
            BAD = .TRUE.
            IERR = 18
            MESSAG(1) = 'Invalid data for EDCHILD'
            MESSAG(2) = 'Valid range is 0 to 21'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure duration is the sum of child and adult exposure duration
!
      DO IREL = 1, NREAL
        EXPOS(IREL)%ED = EXPOS(IREL)%EDCHILD + EXPOS(IREL)%EDADULT
        IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.125.0 ) THEN
          IERR = 19
          BAD = .TRUE.
          MESSAG(1) = 'Invalid data for exposure duration (sum of EDCHILD and EDADULT)'
          MESSAG(2) = 'Valid range is 0 to 125'
          MESSAG(3) = 'Value generated was '
          WRITE(MESSAG(4)(21:),*) EXPOS(IREL)%ED
          CALL PRTERR( IERR, CALLER, 3 )
          EXIT
        END IF
        RWORK(IREL) = EXPOS(IREL)%ED
      END DO
!
      IF( BG_STOC_STAT ) THEN
        CALL USTAT( RWORK, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, &
          XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 9999
          MESSAG(1) = 'Error detected in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          BAD = .TRUE.
        ELSE
          CALL PRTSTAT( 'ED', 'Exposure duration (sum of child and adult)', &
            XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IRPT )
        END IF
      END IF
!
      IF( BG_STOC_VALU ) THEN
        WRITE(IVLU,1010) 'ED', (EXPOS(IREL)%ED,IREL=1,NREAL)
 1010   FORMAT('"',A,'"',1P,1000(:,',',E12.5))
      END IF
!
! *** Exposure time - sweat lodge
!
      IF( SWEAT_SOURCE .NE. 'XXXX' ) THEN
        VIDX = 0
        CALL STONE( 'ETSWEAT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          BAD = .TRUE.
        ELSE
          DO IREL = 1, NREAL
            EXPOS(IREL)%ETSWEAT = WORK(IREL)
            IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.24.0 ) THEN
              BAD = .TRUE.
              IERR = 20
              MESSAG(1) = 'Invalid data for ETSWEAT'
              MESSAG(2) = 'Valid range is 0 to 24'
              MESSAG(3) = 'Value generated was '
              WRITE(MESSAG(3)(21:),*) WORK(IREL)
              CALL PRTERR( IERR, CALLER, 3 )
              EXIT
            END IF
          END DO
        END IF
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%ETSWEAT = 0.0
        END DO
      END IF
!
! *** Exposure frequency - sweat lodge
!
      IF( SWEAT_SOURCE .NE. 'XXXX' ) THEN
        VIDX = 0
        CALL STONE( 'EFSWEAT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          BAD = .TRUE.
        ELSE
          DO IREL = 1, NREAL
            EXPOS(IREL)%EFSWEAT = WORK(IREL)
            IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
              BAD = .TRUE.
              IERR = 21
              MESSAG(1) = 'Invalid data for EFSWEAT'
              MESSAG(2) = 'Valid range is 0 to 366'
              MESSAG(3) = 'Value generated was '
              WRITE(MESSAG(3)(21:),*) WORK(IREL)
              CALL PRTERR( IERR, CALLER, 3 )
              EXIT
            END IF
          END DO
        END IF
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFSWEAT = 0.0
        END DO
      END IF
!
! *** Exposure time - showering
!
      VIDX = 0
      CALL STONE( 'ETSHOWER', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%ETSHOWER = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.24.0 ) THEN
            BAD = .TRUE.
            IERR = 20
            MESSAG(1) = 'Invalid data for ETSHOWER'
            MESSAG(2) = 'Valid range is 0 to 24'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - showering
!
      VIDX = 0
      CALL STONE( 'EFSHOWER', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFSHOWER = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 21
            MESSAG(1) = 'Invalid data for EFSHOWER'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - Leafy vegetables
!
      VIDX = 0
      CALL STONE( 'EFLEAFVEG', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFLEAFVEG = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 22
            MESSAG(1) = 'Invalid data for EFLEAFVEG'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - root vegetables
!
      VIDX = 0
      CALL STONE( 'EFROOTVEG', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFROOTVEG = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 23
            MESSAG(1) = 'Invalid data for EFROOTVEG'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - meat
!
      VIDX = 0
      CALL STONE( 'EFMEAT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFMEAT = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 24
            MESSAG(1) = 'Invalid data for EFMEAT'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - milk
!
      VIDX = 0
      CALL STONE( 'EFMILK', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFMILK = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 25
            MESSAG(1) = 'Invalid data for EFMILK'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - bird
!
      VIDX = 0
      CALL STONE( 'EFBIRD', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFBIRD = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 26
            MESSAG(1) = 'Invalid data for EFBIRD'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - fish
!
      VIDX = 0
      CALL STONE( 'EFFISH', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFFISH = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 27
            MESSAG(1) = 'Invalid data for EFFISH'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - fish_2
!
      IF( USE_FISH_2 ) THEN
        VIDX = 0
        CALL STONE( 'EFFISH_2', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          BAD = .TRUE.
        ELSE
          DO IREL = 1, NREAL
            EXPOS(IREL)%EFFISH_2 = WORK(IREL)
            IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
              BAD = .TRUE.
              IERR = 28
              MESSAG(1) = 'Invalid data for EFFISH_2'
              MESSAG(2) = 'Valid range is 0 to 366'
              MESSAG(3) = 'Value generated was '
              WRITE(MESSAG(3)(21:),*) WORK(IREL)
              CALL PRTERR( IERR, CALLER, 3 )
              EXIT
            END IF
          END DO
        END IF
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFFISH_2 = 0.0
        END DO
      END IF
!
! *** Exposure frequency - fish_3
!
      IF( USE_FISH_3 ) THEN
        VIDX = 0
        CALL STONE( 'EFFISH_3', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          BAD = .TRUE.
        ELSE
          DO IREL = 1, NREAL
            EXPOS(IREL)%EFFISH_3 = WORK(IREL)
            IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
              BAD = .TRUE.
              IERR = 29
              MESSAG(1) = 'Invalid data for EFFISH_3'
              MESSAG(2) = 'Valid range is 0 to 366'
              MESSAG(3) = 'Value generated was '
              WRITE(MESSAG(3)(21:),*) WORK(IREL)
              CALL PRTERR( IERR, CALLER, 3 )
              EXIT
            END IF
          END DO
        END IF
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFFISH_3 = 0.0
        END DO
      END IF
!
! *** Exposure frequency - fruit
!
      VIDX = 0
      CALL STONE( 'EFFRUIT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
       DO IREL = 1, NREAL
          EXPOS(IREL)%EFFRUIT = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 30
            MESSAG(1) = 'Invalid data for EFFRUIT'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - eggs
!
      VIDX = 0
      CALL STONE( 'EFEGGS', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFEGGS = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 31
            MESSAG(1) = 'Invalid data for EFEGGS'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Exposure frequency - grain
!
      VIDX = 0
      CALL STONE( 'EFGRAIN', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          EXPOS(IREL)%EFGRAIN = WORK(IREL)
          IF( WORK(IREL).LT.0.0 .OR. WORK(IREL).GT.366.0 ) THEN
            BAD = .TRUE.
            IERR = 32
            MESSAG(1) = 'Invalid data for EFGRAIN'
            MESSAG(2) = 'Valid range is 0 to 366'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Make sure error flag is set for bad data
!
      IF( BAD ) IERR = 666
!
      RETURN
      END SUBROUTINE

      SUBROUTINE STOGENIN( IERR )
!!*************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates all realizations of the variables
!!    defining individuals.
!!
!!  History:
!!
!!    Paul W. Eslinger : 26 Sep 1996 : Version 1.0
!!    Paul W. Eslinger : 31 Mar 2000 : Version 2.0
!!    Paul W. Eslinger : 24 Sep 2003 : Add option on using soil or sediment
!!    Paul W. Eslinger :  1 Apr 2005 : Change to using a work vector
!!    Paul W. Eslinger : 26 Sep 2005 : Add two additional fish pathways
!!                                     Update error checking
!!    Paul W. Eslinger : 28 Feb 2006 : SCR-1107 - Change error checks on surface areas
!!                                     Add showering and output labels
!!
!!  Call List Variables:
!!
!!    IERR   : Output integer error flag
!!             0 = No errors
!!            >0 = Terminal error in lower level routine or
!!                Terminal error setting data
!!
!!*************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Files_Mod
      USE Debug_Mod
      USE Seeds_Mod
      USE Control_Mod
      USE Stocvars_Mod
      USE Errors_Mod
      USE ECDA_Mod
      USE ESD_Mod
      USE Stats_Mod, ONLY: VPARMS, VTYPE
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'STOGENIN' ! Name of this subroutine
      INTEGER :: VIDX ! Variable index
      INTEGER :: IREL ! Realization looping index
      LOGICAL :: BAD  ! Found at least one bad value
!
!---- Executable code -------------------------------------------------------------
!
      IERR   = 0
      BAD = .FALSE.
!
! *** Body weight - child
!
      VIDX = 0
      CALL STONE( 'BWCHILD', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          BODY(IREL)%BWCHILD = WORK(IREL)
          IF( WORK(IREL).LE.0.0 .OR. WORK(IREL).GT.250.0 ) THEN
            BAD = .TRUE.
            IERR = 1
            MESSAG(1) = 'Invalid data for BWCHILD'
            MESSAG(2) = 'Valid range is 0 to 250'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Body weight - adult
!
      VIDX = 0
      CALL STONE( 'BWADULT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          BODY(IREL)%BWADULT = WORK(IREL)
          IF( WORK(IREL).LE.0.0 .OR. WORK(IREL).GT.500.0 ) THEN
            BAD = .TRUE.
            IERR = 2
            MESSAG(1) = 'Invalid data for BWADULT'
            MESSAG(2) = 'Valid range is 0 to 500'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Body surface area - soil
!
      VIDX = 0
      CALL STONE( 'SASOIL', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          BODY(IREL)%SASOIL = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.1000000.0 ) THEN
            BAD = .TRUE.
            IERR = 3
            MESSAG(1) = 'Invalid data for SASOIL'
            MESSAG(2) = 'Valid range is 0 to 1000000'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Body surface area - sediments
!
      VIDX = 0
      CALL STONE( 'SASED', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          BODY(IREL)%SASED = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.1000000.0 ) THEN
            BAD = .TRUE.
            IERR = 4
            MESSAG(1) = 'Invalid data for SASED'
            MESSAG(2) = 'Valid range is 0 to 1000000'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Body surface area - seep water
!
      VIDX = 0
      CALL STONE( 'SASEEP', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          BODY(IREL)%SASEEP = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.1000000.0 ) THEN
            BAD = .TRUE.
            IERR = 5
            MESSAG(1) = 'Invalid data for SASEEP'
            MESSAG(2) = 'Valid range is 0 to 1000000'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Body surface area - surface water
!
      VIDX = 0
      CALL STONE( 'SASWIM', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          BODY(IREL)%SASWIM = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.1000000.0 ) THEN
            BAD = .TRUE.
            IERR = 6
            MESSAG(1) = 'Invalid data for SASWIM'
            MESSAG(2) = 'Valid range is 0 to 1000000'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Body surface area - sweat lodge
!
      IF( SWEAT_SOURCE .NE. 'XXXX' ) THEN
        VIDX = 0
        CALL STONE( 'SASWEAT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          BAD = .TRUE.
        ELSE
          DO IREL = 1, NREAL
            BODY(IREL)%SASWEAT = WORK(IREL)
            IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.1000000.0 ) THEN
              BAD = .TRUE.
              IERR = 7
              MESSAG(1) = 'Invalid data for SASWEAT'
              MESSAG(2) = 'Valid range is 0 to 1000000'
              MESSAG(3) = 'Value generated was '
              WRITE(MESSAG(3)(21:),*) WORK(IREL)
              CALL PRTERR( IERR, CALLER, 3 )
              EXIT
            END IF
          END DO
        END IF
      ELSE
        DO IREL = 1, NREAL
          BODY(IREL)%SASWEAT = 0.0
        END DO
      END IF
!
! *** Body surface area - showering
!
      VIDX = 0
      CALL STONE( 'SASHOWER', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          BODY(IREL)%SASHOWER = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.1000000.0 ) THEN
            BAD = .TRUE.
            IERR = 7
            MESSAG(1) = 'Invalid data for SASHOWER'
            MESSAG(2) = 'Valid range is 0 to 1000000'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Soil ingestion rate - child
!
      USE_SOIL = .FALSE.
!
      VIDX = 0
      CALL STONE( 'IRSOILCHILD', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
       ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRSOILCHILD = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 9
            MESSAG(1) = 'Invalid data for IRSOILCHILD'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
          IF( WORK(IREL) .GT. 0.0 ) THEN
            IF( SOIL_IDX.GE.1 .AND. SOIL_IDX.LE.3 ) THEN
              LABEL_SOIL_CHILD = 'ID: '//ECDA_ID_MED(SOIL_IDX+6)//'    Name: '//ECDA_DESC_MED(SOIL_IDX+6)
            ELSE
              LABEL_SOIL_CHILD = 'ID: '//'Bad data - consumption mismatch'
            END IF
          END IF
        END DO
      END IF
!
      IF( .NOT.(VTYPE(VIDX).EQ.1 .AND. VPARMS(VIDX,1).EQ.0.0) ) USE_SOIL = .TRUE.
!
! *** Soil ingestion rate - adult
!
      VIDX = 0
      CALL STONE( 'IRSOILADULT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRSOILADULT = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 10
            MESSAG(1) = 'Invalid data for IRSOILADULT'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
          IF( WORK(IREL) .GT. 0.0 ) THEN
            IF( SOIL_IDX.GE.1 .AND. SOIL_IDX.LE.3 ) THEN
              LABEL_SOIL_ADULT = 'ID: '//ECDA_ID_MED(SOIL_IDX+6)//'    Name: '//ECDA_DESC_MED(SOIL_IDX+6)
            ELSE
              LABEL_SOIL_ADULT = 'ID: '//'Bad data - consumption mismatch'
            END IF
          END IF
        END DO
      END IF
!
      IF( .NOT.(VTYPE(VIDX).EQ.1 .AND. VPARMS(VIDX,1).EQ.0.0) ) USE_SOIL = .TRUE.
!
! *** Sediment ingestion rate - Child
!
      USE_SEDI = .FALSE.
!
      VIDX = 0
      CALL STONE( 'IRSEDCHILD', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRSEDCHILD = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 11
            MESSAG(1) = 'Invalid data for IRSEDCHILD'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
          IF( WORK(IREL) .GT. 0.0 ) LABEL_SEDIMENT_CHILD = 'ID: '//ECDA_ID_MED(ISEDI)//'    Name: '//ECDA_DESC_MED(ISEDI)
        END DO
      END IF
!
      IF( .NOT.(VTYPE(VIDX).EQ.1 .AND. VPARMS(VIDX,1).EQ.0.0) ) USE_SEDI = .TRUE.
!
! *** Sediment ingestion rate - Adult
!
      VIDX = 0
      CALL STONE( 'IRSEDADULT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRSEDADULT = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 12
            MESSAG(1) = 'Invalid data for IRSEDLADULT'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
          IF( WORK(IREL) .GT. 0.0 ) LABEL_SEDIMENT_ADULT = 'ID: '//ECDA_ID_MED(ISEDI)//'    Name: '//ECDA_DESC_MED(ISEDI)
        END DO
      END IF
!
      IF( .NOT.(VTYPE(VIDX).EQ.1 .AND. VPARMS(VIDX,1).EQ.0.0) ) USE_SEDI = .TRUE.
!
! *** Surface water ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRSURFACE', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRSURFACE = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 13
            MESSAG(1) = 'Invalid data for IRSURFACE'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
          IF( WORK(IREL) .GT. 0.0 ) LABEL_SURFACE = 'ID: '//ECDA_ID_MED(ISWAT)//'    Name: '//ECDA_DESC_MED(ISWAT)
        END DO
      END IF
!
! *** Ground water ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRGROUND', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRGROUND = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 14
            MESSAG(1) = 'Invalid data for IRGROUND'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
          IF( WORK(IREL) .GT. 0.0 ) LABEL_GROUND = 'ID: '//ECDA_ID_MED(IGWAT)//'    Name: '//ECDA_DESC_MED(IGWAT)
        END DO
      END IF
!
! *** Seep water ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRSEEP', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRSEEP = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 15
            MESSAG(1) = 'Invalid data for IRSEEP'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
          IF( WORK(IREL) .GT. 0.0 ) LABEL_SEEP = 'ID: '//ECDA_ID_MED(ISEEP)//'    Name: '//ECDA_DESC_MED(ISEEP)
        END DO
      END IF
!
! *** Fish ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRFISH', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRFISH = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 16
            MESSAG(1) = 'Invalid data for IRFISH'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Fish_2 ingestion rate
!
      IF( USE_FISH_2 ) THEN
        VIDX = 0
        CALL STONE( 'IRFISH_2', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          BAD = .TRUE.
        ELSE
          DO IREL = 1, NREAL
            INGEST(IREL)%IRFISH_2 = WORK(IREL)
            IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
              BAD = .TRUE.
              IERR = 17
              MESSAG(1) = 'Invalid data for IRFISH_2'
              MESSAG(2) = 'Valid range is 0 to 10'
              MESSAG(3) = 'Value generated was '
              WRITE(MESSAG(3)(21:),*) WORK(IREL)
              CALL PRTERR( IERR, CALLER, 3 )
              EXIT
            END IF
          END DO
        END IF
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRFISH_2 = 0.0
        END DO
      END IF
!
! *** Fish_3 ingestion rate
!
      IF( USE_FISH_3 ) THEN
        VIDX = 0
        CALL STONE( 'IRFISH_3', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          MESSAG(1) = 'Error in lower level routine'
          CALL PRTERR( IERR, CALLER, 1 )
          BAD = .TRUE.
        ELSE
          DO IREL = 1, NREAL
            INGEST(IREL)%IRFISH_3 = WORK(IREL)
            IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
              BAD = .TRUE.
              IERR = 18
              MESSAG(1) = 'Invalid data for IRFISH_3'
              MESSAG(2) = 'Valid range is 0 to 10'
              MESSAG(3) = 'Value generated was '
              WRITE(MESSAG(3)(21:),*) WORK(IREL)
              CALL PRTERR( IERR, CALLER, 3 )
              EXIT
            END IF
          END DO
        END IF
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRFISH_3 = 0.0
        END DO
      END IF
!
! *** Leafy plants ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRLEAFVEG', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRLEAFVEG = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 19
            MESSAG(1) = 'Invalid data for IRLEAFVEG'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Root plants ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRROOTVEG', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRROOTVEG = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 20
            MESSAG(1) = 'Invalid data for IRROOTVEG'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Meat ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRMEAT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRMEAT = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 21
            MESSAG(1) = 'Invalid data for IRMEAT'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Birds ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRBIRD', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRBIRD = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 22
            MESSAG(1) = 'Invalid data for IRBIRD'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Milk ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRMILK', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRMILK = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 23
            MESSAG(1) = 'Invalid data for IRMILK'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** FRUIT ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRFRUIT', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRFRUIT = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 24
            MESSAG(1) = 'Invalid data for IRFRUIT'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** GRAIN ingestion rate
!
      VIDX = 0
      CALL STONE( 'IRGRAIN', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRGRAIN = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 25
            MESSAG(1) = 'Invalid data for IRGRAIN'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** EGGS ingestion rate
!
      VIDX = 0
      CALL STONE( 'IREGGS', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IREGGS = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.10.0 ) THEN
            BAD = .TRUE.
            IERR = 26
            MESSAG(1) = 'Invalid data for IREGGS'
            MESSAG(2) = 'Valid range is 0 to 10'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Inhalation rate
!
      VIDX = 0
      CALL STONE( 'IRATE', VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        MESSAG(1) = 'Error in lower level routine'
        CALL PRTERR( IERR, CALLER, 1 )
        BAD = .TRUE.
      ELSE
        DO IREL = 1, NREAL
          INGEST(IREL)%IRATE = WORK(IREL)
          IF( WORK(IREL) .LT. 0.0 .OR. WORK(IREL).GT.1000.0 ) THEN
            BAD = .TRUE.
            IERR = 27
            MESSAG(1) = 'Invalid data for IRATE'
            MESSAG(2) = 'Valid range is 0 to 1000'
            MESSAG(3) = 'Value generated was '
            WRITE(MESSAG(3)(21:),*) WORK(IREL)
            CALL PRTERR( IERR, CALLER, 3 )
            EXIT
          END IF
        END DO
      END IF
!
! *** Make sure error flag is set for bad data
!
      IF( BAD ) IERR = 666
!
      RETURN
      END SUBROUTINE

      SUBROUTINE STOGENKS( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine generates all realizations of the permeability coefficients and absorption 
!!    factors for non-radioactive analytes.
!!
!!  History:
!!    Paul W. Eslinger : 26 Sep 1996 : Version 1.0
!!    Paul W. Eslinger : 31 Mar 2000 : Version 2.0
!!    Paul W. Eslinger :  5 Jun 2007 : Change stochastic variable to length 24
!!
!!  Call List Variables:
!!    IERR   : Output integer error flag
!!             0 = No errors
!!            >0 = Terminal error in lower level routine or
!!                Terminal error setting data
!!
!!**************************************************************************************************
!
! *** Include global parameters, variables, and arrays
      USE Files_Mod
      USE Debug_Mod
      USE Seeds_Mod
      USE Control_Mod
      USE Stocvars_Mod
      USE Errors_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error indicator
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'STOGENKS'
      INTEGER :: IANA ! Analyte looping index
      INTEGER :: IREL ! Realization looping index
!
      INTEGER :: VIDX ! Variable index
      CHARACTER(LEN=24) :: CTMP ! Temporary variable label
!
!---- Executable code -------------------------------------------------------------
!
      MESSAG(1) = 'Error in lower level routine'
      IERR = 0
!
      DO IANA = 1, ESD_NUM_ANA
!
        IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
!
! ***   Skip radioactive analytes
!
        IF( ESD_ANA(IANA)%ANATYP_RAD ) CYCLE
!
! ***   Permeability coefficient
!
        CTMP = ESD_ANA(IANA)%ID//'KP'
        VIDX = 0
        CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        DO IREL = 1, NREAL
          KP(IANA,IREL) = WORK(IREL)
        END DO
!
! ***   Absorption coefficient for dermal
!
        CTMP = ESD_ANA(IANA)%ID//'ABSORP'
        VIDX = 0
        CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        DO IREL = 1, NREAL
          ABSORP(IANA,IREL) = WORK(IREL)
        END DO
!
      END DO
!
      RETURN
      END SUBROUTINE

      SUBROUTINE STOGENSW( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine generates all realizations of the
!!    soil, water, and volatilization variables.
!!
!!  History:
!!
!!    Paul W. Eslinger : 26 Sep 1996 : Version 1.0
!!    Paul W. Eslinger : 31 Mar 2000 : Version 2.0
!!    Paul W. Eslinger : 15 Feb 2006 : Add showering term
!!    Paul W. Eslinger :  5 Jun 2007 : Change stochastic variable to length 24
!!
!!  Call List Variables:
!!
!!    IERR   : Output integer error flag
!!             0 = No errors
!!            >0 = Terminal error in lower level routine or
!!                Terminal error setting data
!!
!!**************************************************************************************************
!
! *** Include global parameters, variables, and arrays
!
      USE Files_Mod
      USE Debug_Mod
      USE Seeds_Mod
      USE Control_Mod
      USE Stocvars_Mod
      USE Errors_Mod
      USE Esd_Mod
!      USE Stats_Mod, ONLY: BG_STOC_VALU, BG_STOC_DEFN, BG_STOC_STAT
!
      INTEGER :: VIDX ! Variable index
      CHARACTER(LEN=24) :: CTMP ! Temporary variable label
      CHARACTER(LEN=8) :: CALLER = 'STOGENSW'
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
      MESSAG(1) = 'Error in lower level routine'
!
! *** Soil shielding factor
!
      VIDX = 0
      CALL STONE( 'SHIELDSOIL', VIDX, NREAL, SDSTOC, SHIELDSOIL, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Sediment shielding factor
!
      VIDX = 0
      CALL STONE( 'SHIELDSED', VIDX, NREAL, SDSTOC, SHIELDSED, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Mass loading of soil in air
!
      VIDX = 0
      CALL STONE( 'ML', VIDX, NREAL, SDSTOC, ML, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Concentration factor for sweat lodge in air
!
      IF( SWEAT_SOURCE .NE. 'XXXX' ) THEN
        VIDX = 0
        CALL STONE( 'CFSWEAT', VIDX, NREAL, SDSTOC, CFSWEAT, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
      ELSE
        DO IREL = 1, NREAL
          CFSWEAT(IREL)= 0.0
        END DO
      END IF
!
! *** Concentration factor for showering (water in the air)
!
      VIDX = 0
      CALL STONE( 'CFSHOWER', VIDX, NREAL, SDSTOC, CFSHOWER, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Adherence factor for soil
!
      VIDX = 0
      CALL STONE( 'AFSOIL', VIDX, NREAL, SDSTOC, AFSOIL, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Adherence factor for sediments
!
      VIDX = 0
      CALL STONE( 'AFSED', VIDX, NREAL, SDSTOC, AFSED, IVLU, IERR )
      IF( IERR .NE. 0 ) THEN
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Volatilization factor by analyte
!
      DO IANA = 1, ESD_NUM_ANA
!
        IF( .NOT.ESD_ANA(IANA)%COMP ) CYCLE
!
        CTMP = ESD_ANA(IANA)%ID//'VF'
        VIDX = 0
        CALL STONE( CTMP, VIDX, NREAL, SDSTOC, WORK, IVLU, IERR )
        IF( IERR .NE. 0 ) THEN
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        DO IREL = 1, NREAL
          VF(IANA,IREL) = WORK(IREL)
        END DO
!
      END DO
!
      RETURN
      END SUBROUTINE

      SUBROUTINE SUM_RISKS( ITIM, ILOC, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine computes the summed risks from radioactive and carcinogenic analytes for a
!!    single location at a single time and outputs them as detailed data, summary statistics, or
!!    both.
!!
!!  History:
!!
!!    Paul W. Eslinger : 28 Feb 2006 : (SCR-1107) Original code
!!
!!**************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Files_Mod
      USE Control_Mod
      USE Concen_Mod
      USE Stocvars_Mod
      USE Results_Mod
      USE Errors_Mod
      USE Esd_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: ITIM ! Time index
      INTEGER, INTENT(IN) :: ILOC ! Location index
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=10)   :: CALLER = 'SUM_RISKS'
!
      REAL :: XMIN ! Statistics - minimum
      REAL :: XV01 ! Statistics -  1st percentile
      REAL :: XV05 ! Statistics -  5th percentile
      REAL :: XV10 ! Statistics - 10th percentile
      REAL :: XV25 ! Statistics - 25th percentile
      REAL :: XMED ! Statistics - median
      REAL :: XV75 ! Statistics - 75th percentile
      REAL :: XV90 ! Statistics - 90th percentile
      REAL :: XV95 ! Statistics - 95th percentile
      REAL :: XV99 ! Statistics - 95th percentile
      REAL :: XMAX ! Statistics - maximum
      REAL :: XAVG ! Statistics - average
      REAL :: XSTD ! Statistics - standard deviation
!
      INTEGER :: IREL ! Realization looping index
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Do not output anything unless the output flag for this location is activated
      IF( .NOT.ESD_LOC(ILOC)%OUTPUT ) RETURN
!
! *** Generate the sum of radioactive and carcinogenic risk by realization
      DO IREL = 1, NREAL
        CVEC(IREL) = RISKRAD(IREL) + RISKCAR(IREL)
      END DO
!
! *** Compute and output summary statistics on total risk at this location
      IF( STA_SUMRISK ) THEN
        CALL USTAT( CVEC, NREAL, WORK, XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD, IERR )
        IF( IERR .NE. 0 ) THEN
          IERR = 999
          MESSAG(1) = 'Error detected for variable CVEC -> RISKRAD + RISKCAR'
          CALL PRTERR( IERR, CALLER, 1 )
          RETURN
        END IF
        CALL WRITE_STATISTICS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'COMBIN', 'ALL', 'SUMRISK', 'Risk', &
          XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
      END IF
!
! *** Output detailed values on total risk at this location
      IF( DET_SUMRISK ) THEN
        CALL WRITE_DETAILS( ESD_TIM(ITIM)%TIME, ESD_LOC(ILOC)%ID, 'COMBIN', 'ALL', 'SUMRISK', 'Risk', CVEC, NREAL )
      END IF
!
      RETURN
      END SUBROUTINE

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
!---- Executable code -------------------------------------------------------------
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

      SUBROUTINE WRITE_DETAILS( TIME, LOC_ID, ANA_ID, R_TYPE, S_TYPE, UNITS, RISK, NREAL )
!!***************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes detailed risk values to a file
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 12 Dec 2001 : Version 2.00.E.0 (Add data units)
!!
!!***************************************************************************************
!
! *** Global variables
      USE Files_Mod, ONLY: IDET
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: TIME ! Time (celndar year) for this result
      CHARACTER(LEN=*), INTENT(IN) :: LOC_ID ! Location ID
      CHARACTER(LEN=*), INTENT(IN) :: ANA_ID ! Analyte ID
      CHARACTER(LEN=*), INTENT(IN) :: R_TYPE ! Risk type
      CHARACTER(LEN=*), INTENT(IN) :: S_TYPE ! Solution type
      CHARACTER(LEN=*), INTENT(IN) :: UNITS  ! Data units
      INTEGER, INTENT(IN) :: NREAL ! Number of realizations
      REAL, INTENT(IN), DIMENSION(NREAL) :: RISK ! Vector of risk values
!
! *** Local variables
      INTEGER :: I ! Looping index
!
!---- Executable code -------------------------------------------------------------
!
      WRITE(IDET,1000) TIME, TRIM(LOC_ID), TRIM(ANA_ID), TRIM(R_TYPE), TRIM(S_TYPE), TRIM(UNITS), (RISK(I),I=1,NREAL)
 1000 FORMAT(I0,',"',A,'","',A,'","',A,'","',A,'","',A,'"',1P,1000(:,',',E12.5))
!
      RETURN
      END SUBROUTINE

      SUBROUTINE WRITE_STATISTICS( TIME, LOC_ID, ANA_ID, A_TYPE, S_TYPE, UNITS, XMIN, XV01, XV05, &
        XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD )
!!***************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine writes summary risk values to a file
!!
!!  History:
!!
!!    Paul W. Eslinger : 30 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 12 Dec 2001 : Version 2.00.E.0 (Add data units)
!!    Paul W. Eslinger :  5 Jun 2007 : Add 1st and 99th percentiles
!!
!!***************************************************************************************
!
! *** Global variables
      USE Files_Mod, ONLY: ISUM
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: TIME ! Time (celndar year) for this result
      CHARACTER(LEN=*), INTENT(IN) :: LOC_ID ! Location ID
      CHARACTER(LEN=*), INTENT(IN) :: ANA_ID ! Analyte ID
      CHARACTER(LEN=*), INTENT(IN) :: A_TYPE ! Analyte type
      CHARACTER(LEN=*), INTENT(IN) :: S_TYPE ! Solution type
      CHARACTER(LEN=*), INTENT(IN) :: UNITS  ! Data units
      REAL, INTENT(IN) :: XMIN ! Statistics - minimum
      REAL, INTENT(IN) :: XV01 ! Statistics -  1st percentile
      REAL, INTENT(IN) :: XV05 ! Statistics -  5th percentile
      REAL, INTENT(IN) :: XV10 ! Statistics - 10th percentile
      REAL, INTENT(IN) :: XV25 ! Statistics - 25th percentile
      REAL, INTENT(IN) :: XMED ! Statistics - median
      REAL, INTENT(IN) :: XV75 ! Statistics - 75th percentile
      REAL, INTENT(IN) :: XV90 ! Statistics - 90th percentile
      REAL, INTENT(IN) :: XV95 ! Statistics - 95th percentile
      REAL, INTENT(IN) :: XV99 ! Statistics - 99th percentile
      REAL, INTENT(IN) :: XMAX ! Statistics - maximum
      REAL, INTENT(IN) :: XAVG ! Statistics - average
      REAL, INTENT(IN) :: XSTD ! Statistics - standard deviation
!
!---- Executable code -------------------------------------------------------------
!
      WRITE(ISUM,1000) TIME, TRIM(LOC_ID), TRIM(ANA_ID), TRIM(A_TYPE), TRIM(S_TYPE), TRIM(UNITS), &
        XMIN, XV01, XV05, XV10, XV25, XMED, XV75, XV90, XV95, XV99, XMAX, XAVG, XSTD
 1000 FORMAT(I0,',"',A,'","',A,'","',A,'","',A,'","',A,'"',1P,13(',',E12.5))
!
      RETURN
      END SUBROUTINE
!

